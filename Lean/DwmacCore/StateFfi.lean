import DwmacCore.Command
import DwmacCore.WindowKind
import DwmacCore.CliCmd

/-!
# The state machine, exported to C

Swift drives the model through text-based calls, so adding an op or a command doesn't change the C API:

* `dwmac_lean_state_exec state input`: run an event or a command (see `parseInput`), encoded as tab-separated
  fields. Lists separate items with `\x1e` and pairs with `\x1f`. Returns the new state and the reply (see
  `Reply.render`). Input that doesn't parse leaves the state alone, with a failed reply
* `dwmac_lean_state_preview state input`: the reply `exec` would give, without changing the state
* `dwmac_lean_state_snapshot state`: the state, one record per line (see `snapshot`)
* `dwmac_lean_state_query state input`: answer a question that doesn't change the state (see `query`)
* `dwmac_lean_state_layout state showAll`: the layout's effects, one per line (see `Effect.render`)

Whatever the input, the result is `State.exec` of some input (or the state unchanged), so `reachable_exec_inv` holds
for it.
-/

namespace DwmacCore.Model

private def items (s : String) : List String := (s.splitOn "\x1e").filter (· ≠ "")
private def pair (s : String) : Option (String × String) :=
  match s.splitOn "\x1f" with
  | [a, b] => some (a, b)
  | _ => none

def parseForced (s : String) : Option (List (WsName × MonId)) :=
  (items s).mapM fun i => do let (ws, mon) ← pair i; return (WsName.parse ws, ← mon.toNat?)

def parseScratchpads (s : String) : Option (List Scratchpad) :=
  (items s).mapM fun i => do let (name, app) ← pair i; return { name, app }

private def parseBool : String → Option Bool
  | "0" => some false
  | "1" => some true
  | _ => none

private def b (v : Bool) : String := if v then "1" else "0"

/-- `x,y,w,h` -/
def parseRect (s : String) : Option IRect :=
  match s.splitOn "," with
  | [x, y, w, h] => return ⟨← x.toInt?, ← y.toInt?, ← w.toNat?, ← h.toNat?⟩
  | _ => none

def IRect.render (r : IRect) : String := s!"{r.x},{r.y},{r.w},{r.h}"

/-- Items of `mon rect visible padded gapH gapV name`, separated by `\x1f` -/
def parseGeometry (s : String) : Option (List (MonId × Geom)) :=
  (items s).mapM fun i =>
    match i.splitOn "\x1f" with
    | [mon, rect, visible, padded, gapH, gapV, name] => do
      let g : Geom := ⟨← parseRect rect, ← parseRect visible, ← parseRect padded, ← gapH.toNat?, ← gapV.toNat?, name⟩
      return (← mon.toNat?, g)
    | _ => none

def parseLayout : String → Option WsLayout
  | "master-stack" => some .masterStack
  | "floating" => some .floating
  | _ => none

def WsLayout.render : WsLayout → String
  | .masterStack => "master-stack"
  | .floating => "floating"

def parseOrientation : String → Option Orientation
  | "h" => some .h
  | "v" => some .v
  | _ => none

def _root_.DwmacCore.Orientation.render : Orientation → String
  | .h => "h"
  | .v => "v"

/-- `-` for none -/
private def parseOpt {α} (f : String → Option α) (s : String) : Option (Option α) :=
  if s == "-" then some none else (f s).map some

def parseMfactChange (kind amount : String) : Option MfactChange := do
  let n ← amount.toNat?
  match kind with
  | "set" => some (.set n)
  | "add" => some (.add n)
  | "sub" => some (.sub n)
  | _ => none

def parseOp (s : String) : Option State.Op :=
  match s.splitOn "\t" with
  | ["focusWindow", id] => return State.Op.focusWindow (← id.toNat?)
  | ["focusWorkspace", ws] => return State.Op.focusWorkspace (.parse ws)
  | ["summonWorkspace", ws] => return State.Op.summonWorkspace (.parse ws)
  | ["focusMonitor", mon] => return State.Op.focusMonitor (← mon.toNat?)
  | ["moveWorkspaceToMonitor", mon] => return State.Op.moveWorkspaceToMonitor (← mon.toNat?)
  | ["showWorkspace", mon, ws] => return State.Op.showWorkspace (← mon.toNat?) (.parse ws)
  | ["moveWindowToWorkspace", id, ws, follow] =>
    return State.Op.moveWindowToWorkspace (← id.toNat?) (.parse ws) (← parseBool follow)
  | ["placeWindow", id, ws, index, floating] =>
    return State.Op.placeWindow (← id.toNat?) (.parse ws) (← index.toNat?) (if floating == "-" then none else parseBool floating)
  | ["setFloating", id, floating] => return State.Op.setFloating (← id.toNat?) (← parseBool floating)
  | ["toggleScratchpad", name] => return State.Op.toggleScratchpad name
  | ["windowAppeared", id, app, ws, floating, index] =>
    return State.Op.windowAppeared (← id.toNat?) app (.parse ws) (← parseBool floating) (← index.toNat?)
  | ["windowClosed", id] => return State.Op.windowClosed (← id.toNat?)
  | ["monitorsChanged", mons, forced] => return State.Op.monitorsChanged (← (items mons).mapM String.toNat?) (← parseForced forced)
  | ["configChanged", sps, forced] => return State.Op.configChanged (← parseScratchpads sps) (← parseForced forced)
  | ["setLayout", ws, layout, orientation] =>
    return State.Op.setLayout (.parse ws) (← parseOpt parseLayout layout) (← parseOpt parseOrientation orientation)
  | ["mfact", ws, kind, amount] => return State.Op.mfact (.parse ws) (← parseMfactChange kind amount)
  | ["setFullscreen", id, on, noOuterGaps] =>
    return State.Op.setFullscreen (← id.toNat?) (← parseBool on) (← parseBool noOuterGaps)
  | ["setParked", id, parked] => return State.Op.setParked (← id.toNat?) (← parseBool parked)
  | ["geometryChanged", geometry] => return State.Op.geometryChanged (← parseGeometry geometry)
  | ["settingsChanged", layout, mfact, masterAtStop, center, persistent] =>
    return State.Op.settingsChanged
      { defaultLayout := ← parseLayout layout, defaultMfactPermille := ← mfact.toNat?,
        masterAtStop := ← parseBool masterAtStop, centerFloating := ← parseBool center,
        persistent := (items persistent).map WsName.parse }
  | _ => none

private def parseTarget (s : String) : Option Target :=
  if s == "-" then some .focused
  else match s.toList with
    | 'w' :: id => return .window (← (String.ofList id).toNat?) false
    | 'e' :: id => return .window (← (String.ofList id).toNat?) true
    | 's' :: ws => return .workspace (.parse (String.ofList ws))
    | _ => none

private def parseNextPrev : String → Option NextPrev
  | "next" => some .next
  | "prev" => some .prev
  | _ => none

/-- `name <ws> - -` or `next|prev - <wrap> <among: -, or a list>` -/
private def parseWsTo (kind name wrap among : String) : Option WsTo :=
  if kind == "name" then some (.name (.parse name))
  else
    let among := if among == "-" then none else some ((items among).map WsName.parse)
    return .rel (← parseNextPrev kind) (← parseBool wrap) among

private def parseLayoutDesc : String → Option LayoutDesc
  | "master-stack" => some .masterStack
  | "h_master-stack" => some .hMasterStack
  | "v_master-stack" => some .vMasterStack
  | "horizontal" => some .horizontal
  | "vertical" => some .vertical
  | "tiling" => some .tiling
  | "floating" => some .floating
  | _ => none

def parseCmd : List String → Option Cmd
  | ["focus", t, "window", id] => return .focus (← parseTarget t) (.window (← id.toNat?))
  | ["focus", t, "index", n] => return .focus (← parseTarget t) (.index (← n.toNat?))
  | ["focus", t, dir, boundary] =>
    return .focus (← parseTarget t) (.rel (← parseNextPrev dir) (← match boundary with
      | "stop" => some .stop
      | "fail" => some .fail
      | "wrap" => some .wrap
      | _ => none))
  | ["focusBackAndForth"] => some .focusBackAndForth
  | ["focusMasterOrBack"] => some .focusMasterOrBack
  | ["focusMonitor", mon] => return .focusMonitor (← mon.toNat?)
  | ["workspace", t, kind, name, wrap, among, autoBackAndForth, failIfNoop] =>
    return .workspace (← parseTarget t) (← parseWsTo kind name wrap among) (← parseBool autoBackAndForth)
      (← parseBool failIfNoop)
  | ["workspaceBackAndForth"] => some .workspaceBackAndForth
  | ["summonWorkspace", ws, failIfNoop] => return .summonWorkspace (.parse ws) (← parseBool failIfNoop)
  | ["moveWorkspaceToMonitor", t, mon] => return .moveWorkspaceToMonitor (← parseTarget t) (← mon.toNat?)
  | ["moveNodeToWorkspace", t, kind, name, wrap, among, focusFollows, failIfNoop] =>
    return .moveNodeToWorkspace (← parseTarget t) (← parseWsTo kind name wrap among) (← parseBool focusFollows)
      (← parseBool failIfNoop)
  | ["moveNodeToMonitor", t, mon, dir, focusFollows, failIfNoop] =>
    return .moveNodeToMonitor (← parseTarget t) (← mon.toNat?)
      (← match dir with
        | "-" => some none
        | "h1" => some (some (.h, true))
        | "h0" => some (some (.h, false))
        | "v1" => some (some (.v, true))
        | "v0" => some (some (.v, false))
        | _ => none)
      (← parseBool focusFollows) (← parseBool failIfNoop)
  | ["moveNodeToMaster"] => some .moveNodeToMaster
  | ["rotate"] => some .rotate
  | ["syncFocus", native, now] => return .syncFocus (if native == "-" then none else native.toNat?) (← now.toNat?)
  | ["swap", t, dir, wrap, swapFocus] =>
    return .swap (← parseTarget t) (← parseNextPrev dir) (← parseBool wrap) (← parseBool swapFocus)
  | ["scratchpad", name] => some (.scratchpad name)
  | ["layout", t, descs] => return .layout (← parseTarget t) (← (items descs).mapM parseLayoutDesc)
  | ["mfact", kind, amount] => return .mfact (← parseMfactChange kind amount)
  | ["fullscreen", t, tog, noOuterGaps, failIfNoop] =>
    return .fullscreen (← parseTarget t)
      (← match tog with
        | "on" => some .on
        | "off" => some .off
        | "toggle" => some .toggle
        | _ => none)
      (← parseBool noOuterGaps) (← parseBool failIfNoop)
  | _ => none

/-- `cmd <command fields>` (see `parseCmd`), else an op (see `parseOp`) -/
def parseInput (s : String) : Option State.Input :=
  match s.splitOn "\t" with
  | "cmd" :: fields => (parseCmd fields).map .cmd
  | _ => (parseOp s).map .op

def CmdEffect.render : CmdEffect → String
  | .launch app => s!"effect\tlaunch\t{app}"
  | .prepareSummon id => s!"effect\tprepareSummon\t{id}"
  | .nativeFocus id => s!"effect\tnativeFocus\t{id}"
  | .recheck ms => s!"effect\trecheck\t{ms}"

/-- `ok <0/1>`, then `out <line>`, `err <line>` and `effect <kind> <arg>` records -/
def Reply.render (r : Reply) : String :=
  "\n".intercalate (s!"ok\t{b r.ok}" :: r.out.map (s!"out\t{·}") ++ r.err.map (s!"err\t{·}") ++
    r.effects.map (·.render))

def WsProps.render (p : WsProps) : String := s!"{p.layout.render}\t{p.orientation.render}\t{p.mfactPermille}"

/-- One record per line, fields separated by tabs:
`focus <ws> <window id or ->`, then `screen <monitor> <ws>` per monitor (main first), then
`window <id> <ws> <floating 0/1> <fullscreen: -, or no outer gaps 0/1> <parked 0/1>` per window in tiling order,
then `mru <ids, most recent first, comma separated>`, `prev <ws or -> <window id or ->` (the focus before the last
change), `assigned <ws\x1fmonitor items>` (the connected monitor each workspace was last shown on), then
`wsdefault <layout> <orientation> <mfact permille>` and
`ws <name> <layout> <orientation> <mfact permille>` per workspace that differs from it -/
def State.snapshot (s : State) : String :=
  let focus := s!"focus\t{s.focusWs.render}\t{(s.focusWin.map toString).getD "-"}"
  let screens := s.screens.map fun (m, ws) => s!"screen\t{m}\t{ws.render}"
  let windows := s.windows.map fun w =>
    s!"window\t{w.id}\t{w.ws.render}\t{b w.floating}\t{(w.fullscreen.map b).getD "-"}\t{b w.parked}"
  let mru := s!"mru\t{",".intercalate (s.mru.map toString)}"
  let prev := match s.prevFocus with
    | some (ws, win) => s!"prev\t{ws.render}\t{(win.map toString).getD "-"}"
    | none => "prev\t-\t-"
  let lastShown := s.assigned.foldl (fun acc (ws, m) =>
    if s.monitors.contains m && !acc.any (·.1 == ws) then acc ++ [(ws, m)] else acc) []
  let assigned := s!"assigned\t{"\x1e".intercalate (lastShown.map fun (ws, m) => s!"{ws.render}\x1f{m}")}"
  let wsDefault := s!"wsdefault\t{(s.props (.named "")).render}"
  let wsProps := s.wsProps.map fun (ws, p) => s!"ws\t{ws.render}\t{p.render}"
  "\n".intercalate (focus :: screens ++ windows ++ [mru, prev, assigned, wsDefault] ++ wsProps)

def Corner.render : Corner → String
  | .bottomLeft => "bottom-left"
  | .bottomRight => "bottom-right"

/-- `unhide <id> <monitor rect> <restore>`, `tile <id> <rect> <virtual rect> <fullscreen>`,
`float <id> <monitor> <visible rect> <center>`, `hide <id> <corner> <visible rect>`. Rects are `x,y,w,h` -/
def Effect.render : Effect → String
  | .unhide id r restore => s!"unhide\t{id}\t{r.render}\t{b restore}"
  | .tile id r v full => s!"tile\t{id}\t{r.render}\t{v.render}\t{b full}"
  | .float id mon r center => s!"float\t{id}\t{mon}\t{r.render}\t{b center}"
  | .hide id c r => s!"hide\t{id}\t{c.render}\t{r.render}"

@[export dwmac_lean_state_init]
def stateInitFfi (scratchpads : String) : State := State.init ((parseScratchpads scratchpads).getD [])

/-- `cli <DWMAC_WINDOW_ID or -> <DWMAC_WORKSPACE or -> <stdin lines, \x1e separated> <args, \x1f separated>`: parse a
command Lean runs, resolve it against the state, and run it -/
def State.execCli (s : State) (fields : List String) : State × Reply :=
  match fields with
  | [windowId, workspace, stdin, raw] =>
    let env : Env := { windowId := if windowId == "-" then none else windowId.toNat?,
                       workspace := if workspace == "-" then none else some workspace, stdin := items stdin }
    match Cli.parseArgs (if raw.isEmpty then [] else raw.splitOn "\x1f") with
    | .cmd kind f => match s.cliCmd env kind f with
      | .ok c => s.exec (.cmd c)
      | .error e => (s, .fail e)
    | .help text => (s, { out := text.splitOn "\n" })
    | .fail msg => (s, .fail msg)
  | _ => (s, .fail "dwmac bug: Lean can't parse a cli input")

def State.execText (s : State) (input : String) : State × Reply :=
  match input.splitOn "\t" with
  | "cli" :: fields => s.execCli fields
  | _ =>
  match parseInput input with
  | some i => s.exec i
  | none => (s, .fail s!"dwmac bug: Lean can't parse '{input}'")

@[export dwmac_lean_state_exec]
def stateExecFfi (s : State) (input : String) : State × String :=
  let (s, r) := s.execText input
  (s, r.render)

@[export dwmac_lean_state_preview]
def statePreviewFfi (s : State) (input : String) : String := (s.execText input).2.render

/-! ## Queries -/

private def parseButton : String → Option WindowKind.Button
  | "-" => some .absent
  | "0" => some .disabled
  | "1" => some .enabled
  | _ => none

private def parseOptBool : String → Option (Option Bool)
  | "-" => some none
  | v => (parseBool v).map some

private def optStr (v : String) : Option String := if v == "-" then none else some v

/-- `<bundle id> <accessory 0/1> <level or -> <subrole or -> <title> <identifier or -> <close> <fullscreen> <zoom>
<minimize> <focused> <main> <app focused 0/1>`. Buttons: `-` missing, `0` not enabled, `1` enabled; focused and
main: `-` unknown, `0`, `1` -/
def parseFacts : List String → Option WindowKind.Facts
  | [app, accessory, level, subrole, title, identifier, close, fullscreen, zoom, minimize, focused, main, appFocused] => do
    let level ← if level == "-" then pure none else some <$> level.toInt?
    return {
      app, title, level
      accessory := ← parseBool accessory
      subrole := optStr subrole
      identifier := optStr identifier
      close := ← parseButton close
      fullscreen := ← parseButton fullscreen
      zoom := ← parseButton zoom
      minimize := ← parseButton minimize
      focused := ← parseOptBool focused
      main := ← parseOptBool main
      appFocused := ← parseBool appFocused }
  | _ => none

def _root_.DwmacCore.WindowKind.Kind.render : WindowKind.Kind → String
  | .window => "window"
  | .dialog => "dialog"
  | .popup => "popup"

/-- `windowKind <facts>` (see `parseFacts`): `<window/dialog/popup> <is dialog 0/1>`.
`parse <command string>` and `parseArgs <args, \x1f separated>`: the parsed command (`Cli.Parsed.render`), then
`args <args, \x1f separated>`, the arguments it was split into. The empty string for input that doesn't parse -/
def State.query (_s : State) (input : String) : String :=
  let args (l : List String) := "\nargs\t" ++ "\x1f".intercalate l
  match input.splitOn "\t" with
  | "windowKind" :: facts => match parseFacts facts with
    | some f => s!"{(WindowKind.kind f).render}\t{b (WindowKind.isDialog f)}"
    | none => ""
  | ["parse", raw] => let (p, l) := Cli.parseString raw; p.render ++ args l
  | ["parseArgs", raw] =>
    let l := if raw.isEmpty then [] else raw.splitOn "\x1f"
    (Cli.parseArgs l).render ++ args l
  | _ => ""

@[export dwmac_lean_state_query]
def stateQueryFfi (s : State) (input : String) : String := s.query input

@[export dwmac_lean_state_snapshot]
def stateSnapshotFfi (s : State) : String := s.snapshot

@[export dwmac_lean_state_layout]
def stateLayoutFfi (s : State) (showAll : UInt8) : String := "\n".intercalate ((s.layout (showAll != 0)).map (·.render))

end DwmacCore.Model
