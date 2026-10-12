import DwmacCore.Model

/-!
# The state machine, exported to C

Swift drives the model through two text-based calls, so adding an op doesn't change the C API:

* `dwmac_lean_state_step state op`: apply one op, encoded as tab-separated fields (see `parseOp`). Lists separate
  items with `\x1e` and pairs with `\x1f`. An op that doesn't parse leaves the state alone
* `dwmac_lean_state_snapshot state`: the state, one record per line (see `snapshot`)

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
  | _ => none

/-- One record per line, fields separated by tabs:
`focus <ws> <window id or ->`, then `screen <monitor> <ws>` per monitor (main first), then
`window <id> <ws> <floating 0/1>` per window in tiling order -/
def State.snapshot (s : State) : String :=
  let focus := s!"focus\t{s.focusWs.render}\t{(s.focusWin.map toString).getD "-"}"
  let screens := s.screens.map fun (m, ws) => s!"screen\t{m}\t{ws.render}"
  let windows := s.windows.map fun w => s!"window\t{w.id}\t{w.ws.render}\t{if w.floating then 1 else 0}"
  "\n".intercalate (focus :: screens ++ windows)

@[export dwmac_lean_state_init]
def stateInitFfi (scratchpads : String) : State := State.init ((parseScratchpads scratchpads).getD [])

@[export dwmac_lean_state_step]
def stateStepFfi (s : State) (op : String) : State :=
  match parseOp op with
  | some op => s.step op
  | none => s

@[export dwmac_lean_state_snapshot]
def stateSnapshotFfi (s : State) : String := s.snapshot

end DwmacCore.Model
