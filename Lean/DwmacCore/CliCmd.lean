import DwmacCore.Command
import DwmacCore.Cli

/-!
# From a parsed command line to a command

The commands Lean runs (`focus`, `workspace`, `swap`, ...) take their parsed fields (Cli.lean), the environment a
command runs in (`DWMAC_WINDOW_ID`, `DWMAC_WORKSPACE`, stdin), and the state: the target window or workspace and
monitor targets (`focus-monitor left`, `move-node-to-monitor 'LG.*'`) are resolved here, against the monitors' geometry.
-/

namespace DwmacCore.Model
open Cli

/-- What a command runs with besides its arguments -/
structure Env where
  /-- `DWMAC_WINDOW_ID` -/
  windowId : Option WinId := none
  /-- `DWMAC_WORKSPACE` -/
  workspace : Option String := none
  /-- stdin's lines, for `--stdin` -/
  stdin : List String := []
deriving Repr, Inhabited

/-- The commands Lean runs, by name -/
def leanRunKinds : List String := ["focus", "focus-back-and-forth", "focus-master-or-back", "focus-monitor",
  "fullscreen", "layout", "mfact", "move-node-to-master", "move-node-to-monitor", "move-node-to-workspace",
  "move-workspace-to-monitor", "rotate", "scratchpad", "summon-workspace", "swap", "workspace",
  "workspace-back-and-forth"]

/-- `--window-id`/`--workspace`, then `DWMAC_WINDOW_ID`/`DWMAC_WORKSPACE`, else focus -/
def target (f : Fields) (env : Env) : Target :=
  match (f.str? "windowId").bind String.toNat?, f.str? "workspaceName", env.windowId, env.workspace with
  | some id, _, _, _ => .window id false
  | none, some ws, _, _ => .workspace (.parse ws)
  | none, none, some id, _ => .window id true
  | none, none, none, some ws => .workspace (.parse ws)
  | none, none, none, none => .focused

/-! ## Monitors -/

namespace State

/-- Connected monitors with their geometry, left to right, then top to bottom -/
def sortedMonitors (s : State) : List (MonId × Geom) :=
  let ms := s.monitors.filterMap fun m => (s.geomOf m).map (m, ·)
  ms.mergeSort fun a b => a.2.rect.x < b.2.rect.x || (a.2.rect.x == b.2.rect.x && a.2.rect.y ≤ b.2.rect.y)

def mainMonitor (s : State) : Option MonId := s.monitors.head?

/-- Side by side (`h`) when their vertical extents overlap, else one above the other (`v`) -/
def relation (a b : Geom) : Orientation :=
  let a0 := a.rect.y
  let a1 := a.rect.y + a.rect.h
  let b0 := b.rect.y
  let b1 := b.rect.y + b.rect.h
  if a.rect.h == 0 || b.rect.h == 0 then .h else if a0 < b1 && b0 < a1 then .h else .v

def wrapIndex (i : Int) (n : Nat) : Nat := if n == 0 then 0 else ((i % n + n) % n).toNat

/-- A `<monitor-pattern>`: a 1-based sequence number, `main`, `secondary` (with exactly two monitors), or a regex
matched against monitor names -/
def resolvePattern (s : State) (pattern : String) : Option MonId :=
  let sorted := s.sortedMonitors
  match Cli.parseInt pattern with
  | some n => if n ≥ 1 then (sorted[(n - 1).toNat]?).map (·.1) else none
  | none =>
    if pattern == "main" then s.mainMonitor
    else if pattern == "secondary" then
      if sorted.length == 2 then (sorted.find? (some ·.1 != s.mainMonitor)).map (·.1) else none
    else (sorted.find? fun (_, g) => Regex.search pattern g.name).map (·.1)

/-- The monitor a `focus-monitor`-style target names, seen from `cur` -/
def resolveMonitor (s : State) (cur : MonId) (f : Fields) : Except String MonId := do
  let sorted := s.sortedMonitors
  let wrap := f.bool "wrapAround"
  let pick (l : List (MonId × Geom)) (i : Int) : Option MonId :=
    if wrap then (l[wrapIndex i l.length]?).map (·.1) else if i < 0 then none else (l[i.toNat]?).map (·.1)
  match f.str? "target" with
  | some dir =>
    if Cli.directions.contains dir then
      let some curGeom := s.geomOf cur | throw "Should never happen. Can't find the current monitor"
      let orientation : Orientation := if dir == "up" || dir == "down" then .v else .h
      let inDirection := sorted.filter fun (m, g) => m == cur || relation g curGeom == orientation
      let some i := inDirection.findIdx? (·.1 == cur) | throw "Should never happen. Can't find the current monitor"
      let offset : Int := if dir == "down" || dir == "right" then 1 else -1
      match pick inDirection (i + offset) with
      | some m => return m
      | none => throw s!"No monitors in direction {dir}"
    else
      let some i := sorted.findIdx? (·.1 == cur) | throw "Can't find current monitor"
      match pick sorted (if dir == "next" then (i : Int) + 1 else (i : Int) - 1) with
      | some m => return m
      | none => throw "Can't find target monitor"
  | none =>
    match (f.strs "patterns").findSome? s.resolvePattern with
    | some m => return m
    | none => throw "None of the monitors match the pattern(s)"

end State

/-! ## Values -/

/-- `x` in thousandths, rounded half away from zero, if `x` is a decimal number (`0.5`, `-.25`, `1e-1`) -/
def permille (x : String) : Option Int := Cli.scaled x 3

def layoutDesc : String → Option LayoutDesc
  | "master-stack" => some .masterStack
  | "h_master-stack" => some .hMasterStack
  | "v_master-stack" => some .vMasterStack
  | "horizontal" => some .horizontal
  | "vertical" => some .vertical
  | "tiling" => some .tiling
  | "floating" => some .floating
  | _ => none

/-- Stdin's workspace names: one per line, trimmed -/
def stdinWorkspaces (env : Env) : List WsName :=
  (env.stdin.map (·.trimAscii.toString)).filter (!·.isEmpty) |>.map WsName.parse

def wsTo (f : Fields) (env : Env) : WsTo :=
  match f.str? "target" with
  | some "next" | some "prev" =>
    let dir : NextPrev := if f.str? "target" == some "next" then .next else .prev
    .rel dir (f.has "_wrapAround") (if f.get? "explicitStdinFlag" == some (.bool true) then some (stdinWorkspaces env) else none)
  | some t => .name (.parse ((t.drop 1).toString))
  | none => .name (.parse "")

/-- The command a parsed command line asks for -/
def State.cliCmd (s : State) (env : Env) (kind : String) (f : Fields) : Except String Cmd := do
  let t := target f env
  match kind with
  | "focus" =>
    let to : FocusTo := match (f.str? "windowId").bind String.toNat?, (f.str? "index").bind String.toNat? with
      | some id, _ => .window id
      | none, some n => .index n
      | none, none =>
        let dir : NextPrev := if f.str? "nextPrev" == some "next" then .next else .prev
        -- Without --boundaries-action, focus wraps around the workspace
        let boundary : Boundary := match f.str? "rawBoundariesAction" with
          | some "stop" => .stop
          | some "fail" => .fail
          | _ => .wrap
        .rel dir boundary
    return .focus t to
  | "focus-back-and-forth" => return .focusBackAndForth
  | "focus-master-or-back" => return .focusMasterOrBack
  | "focus-monitor" =>
    let (ws, _) ← s.resolve t
    return .focusMonitor (← s.resolveMonitor (s.workspaceMonitor ws) f)
  | "fullscreen" =>
    let tog : Toggle := match f.str? "toggle" with | some "on" => .on | some "off" => .off | _ => .toggle
    return .fullscreen t tog (f.bool "noOuterGaps") (f.bool "failIfNoop")
  | "layout" => return .layout t ((f.strs "toggleBetween").filterMap layoutDesc)
  | "mfact" =>
    let raw := (f.str? "amount").getD ""
    let (kind, value) :=
      if raw.startsWith "+" then ("add", (raw.drop 1).toString)
      else if raw.startsWith "-" then ("sub", (raw.drop 1).toString) else ("set", raw)
    let some p := permille value | throw "<number> argument must be a number"
    let p := p.toNat -- Lean keeps mfact between 0.05 and 0.95
    return .mfact (match kind with | "add" => .add p | "sub" => .sub p | _ => .set p)
  | "move-node-to-master" => return .moveNodeToMaster
  | "move-node-to-monitor" =>
    match ← s.resolve t with
    | (_, none) => throw noWindowIsFocused
    | (_, some id) =>
      let cur := s.workspaceMonitor (((s.window? id).map (·.ws)).getD s.focusWs)
      let mon ← s.resolveMonitor cur f
      let dir : Option (Orientation × Bool) := match f.str? "target" with
        | some "left" => some (.h, false)
        | some "right" => some (.h, true)
        | some "up" => some (.v, false)
        | some "down" => some (.v, true)
        | _ => none
      return .moveNodeToMonitor t mon dir (f.bool "focusFollowsWindow") (f.bool "failIfNoop")
  | "move-node-to-workspace" =>
    return .moveNodeToWorkspace t (wsTo f env) (f.bool "focusFollowsWindow") (f.bool "failIfNoop")
  | "move-workspace-to-monitor" =>
    let (ws, _) ← s.resolve t
    return .moveWorkspaceToMonitor t (← s.resolveMonitor (s.workspaceMonitor ws) f)
  | "rotate" => return .rotate
  | "scratchpad" => return .scratchpad ((f.str? "name").getD "")
  | "summon-workspace" => return .summonWorkspace (.parse ((f.str? "target").getD "")) (f.bool "failIfNoop")
  | "swap" =>
    let dir : NextPrev := if f.str? "target" == some "next" then .next else .prev
    return .swap t dir (f.bool "wrapAround") (f.bool "swapFocus")
  | "workspace" =>
    return .workspace t (wsTo f env) (f.bool "_autoBackAndForth") (f.bool "failIfNoop")
  | "workspace-back-and-forth" => return .workspaceBackAndForth
  | k => throw s!"dwmac bug: '{k}' isn't run by Lean"

end DwmacCore.Model
