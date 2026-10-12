import DwmacCore.Layout

/-!
# The state machine, exported to C

Swift drives the model through two text-based calls, so adding an op doesn't change the C API:

* `dwmac_lean_state_step state op`: apply one op, encoded as tab-separated fields (see `parseOp`). Lists separate
  items with `\x1e` and pairs with `\x1f`. An op that doesn't parse leaves the state alone
* `dwmac_lean_state_snapshot state`: the state, one record per line (see `snapshot`)
* `dwmac_lean_state_layout state showAll`: the layout's effects, one per line (see `Effect.render`)

Whatever the input, the result is `State.step` of some op (or the state unchanged), so `reachable_inv` holds for it.
-/

namespace DwmacCore.Model

def homePrefix := "scratchpad:"

/-- Swift's workspace name: `scratchpad:x` is a home, a number (as Swift would print it) is `num` -/
def WsName.parse (s : String) : WsName :=
  if s.startsWith homePrefix then .home (s.drop homePrefix.length).toString
  else match s.toNat? with
    | some n => if toString n == s then .num n else .named s
    | none => .named s

def WsName.render : WsName → String
  | .num n => toString n
  | .named s => s
  | .home s => homePrefix ++ s

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

/-- Items of `mon rect visible padded gapH gapV`, separated by `\x1f` -/
def parseGeometry (s : String) : Option (List (MonId × Geom)) :=
  (items s).mapM fun i =>
    match i.splitOn "\x1f" with
    | [mon, rect, visible, padded, gapH, gapV] => do
      let g : Geom := ⟨← parseRect rect, ← parseRect visible, ← parseRect padded, ← gapH.toNat?, ← gapV.toNat?⟩
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
  | ["layoutConfigChanged", layout, mfact, masterAtStop, center] =>
    return State.Op.layoutConfigChanged
      { defaultLayout := ← parseLayout layout, defaultMfactPermille := ← mfact.toNat?,
        masterAtStop := ← parseBool masterAtStop, centerFloating := ← parseBool center }
  | _ => none

def WsProps.render (p : WsProps) : String := s!"{p.layout.render}\t{p.orientation.render}\t{p.mfactPermille}"

/-- One record per line, fields separated by tabs:
`focus <ws> <window id or ->`, then `screen <monitor> <ws>` per monitor (main first), then
`window <id> <ws> <floating 0/1> <fullscreen: -, or no outer gaps 0/1>` per window in tiling order, then
`mru <ids, most recent first, comma separated>`, then `wsdefault <layout> <orientation> <mfact permille>` and
`ws <name> <layout> <orientation> <mfact permille>` per workspace that differs from it -/
def State.snapshot (s : State) : String :=
  let focus := s!"focus\t{s.focusWs.render}\t{(s.focusWin.map toString).getD "-"}"
  let screens := s.screens.map fun (m, ws) => s!"screen\t{m}\t{ws.render}"
  let windows := s.windows.map fun w =>
    s!"window\t{w.id}\t{w.ws.render}\t{b w.floating}\t{(w.fullscreen.map b).getD "-"}"
  let mru := s!"mru\t{",".intercalate (s.mru.map toString)}"
  let wsDefault := s!"wsdefault\t{(s.props (.named "")).render}"
  let wsProps := s.wsProps.map fun (ws, p) => s!"ws\t{ws.render}\t{p.render}"
  "\n".intercalate (focus :: screens ++ windows ++ [mru, wsDefault] ++ wsProps)

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

@[export dwmac_lean_state_step]
def stateStepFfi (s : State) (op : String) : State :=
  match parseOp op with
  | some op => s.step op
  | none => s

@[export dwmac_lean_state_snapshot]
def stateSnapshotFfi (s : State) : String := s.snapshot

@[export dwmac_lean_state_layout]
def stateLayoutFfi (s : State) (showAll : UInt8) : String := "\n".intercalate ((s.layout (showAll != 0)).map (·.render))

end DwmacCore.Model
