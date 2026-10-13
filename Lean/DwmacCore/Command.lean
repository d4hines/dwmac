import DwmacCore.Layout

/-!
# Commands

dwmac's CLI commands, as functions of the state. `plan` turns a command into the model ops it amounts to and the
reply (exit code, output, errors) to give once they're applied. Swift parses the arguments, resolves what only it
can (monitor patterns and directions, stdin), and prints the reply.

Because a command is only ever a list of ops, `exec_inv` comes for free: commands preserve the invariant, so
`home_never_shown` and `layout_never_shows_home` hold for every state commands and events can reach.

`exec` also remembers the focus before the last change, for `focus-back-and-forth` and `workspace-back-and-forth`.
-/

namespace DwmacCore.Model

/-- What a command acts on: `--window-id` (or `DWMAC_WINDOW_ID`, if `fromEnv`), `--workspace` (or
`DWMAC_WORKSPACE`), else the focus -/
inductive Target where
  | focused
  | window (id : WinId) (fromEnv : Bool)
  | workspace (ws : WsName)
deriving DecidableEq, Repr

inductive NextPrev where
  | next
  | prev
deriving DecidableEq, Repr

/-- `focus --boundaries-action` -/
inductive Boundary where
  | stop
  | fail
  | wrap
deriving DecidableEq, Repr

inductive FocusTo where
  | window (id : WinId)
  | index (n : Nat)
  | rel (dir : NextPrev) (boundary : Boundary)
deriving DecidableEq, Repr

/-- A workspace by name, or the next/previous one: among the workspaces on the target's monitor, or among `among`
(read from stdin) -/
inductive WsTo where
  | name (ws : WsName)
  | rel (dir : NextPrev) (wrap : Bool) (among : Option (List WsName))
deriving DecidableEq, Repr

inductive LayoutDesc where
  | masterStack
  | hMasterStack
  | vMasterStack
  | horizontal
  | vertical
  | tiling
  | floating
deriving DecidableEq, Repr

inductive Toggle where
  | on
  | off
  | toggle
deriving DecidableEq, Repr

inductive Cmd where
  | focus (t : Target) (to : FocusTo)
  | focusBackAndForth
  | focusMasterOrBack
  /-- `focus-monitor`, with the monitor resolved by Swift -/
  | focusMonitor (mon : MonId)
  | workspace (t : Target) (to : WsTo) (autoBackAndForth failIfNoop : Bool)
  | workspaceBackAndForth
  | summonWorkspace (ws : WsName) (failIfNoop : Bool)
  | moveWorkspaceToMonitor (t : Target) (mon : MonId)
  | moveNodeToWorkspace (t : Target) (to : WsTo) (focusFollows failIfNoop : Bool)
  /-- `dir`: the direction it was given as, if any (orientation, and whether it's right/down). Moving right or down
  along the target workspace's orientation puts the window first -/
  | moveNodeToMonitor (t : Target) (mon : MonId) (dir : Option (Orientation × Bool)) (focusFollows failIfNoop : Bool)
  | moveNodeToMaster
  | swap (t : Target) (dir : NextPrev) (wrap swapFocus : Bool)
  | scratchpad (name : String)
  | layout (t : Target) (toggleBetween : List LayoutDesc)
  | mfact (change : MfactChange)
  | fullscreen (t : Target) (tog : Toggle) (noOuterGaps failIfNoop : Bool)
deriving Repr

/-- What Swift does besides laying out windows -/
inductive CmdEffect where
  /-- Launch the app (or make it open a window) -/
  | launch (app : AppId)
  /-- A scratchpad window is about to be summoned: resize it while it's still hidden -/
  | prepareSummon (id : WinId)
deriving DecidableEq, Repr

structure Reply where
  ok : Bool := true
  out : List String := []
  err : List String := []
  effects : List CmdEffect := []
deriving DecidableEq, Repr

def Reply.fail (msg : String) : Reply := { ok := false, err := [msg] }

def noWindowIsFocused := "No window is focused"
def noopTip := "Tip: use --fail-if-noop to exit with non-zero code"

/-- A command, planned: the ops to apply, and the reply, given the state after them -/
structure Plan where
  ops : List State.Op := []
  reply : State → Reply := fun _ => {}

def Plan.fail (msg : String) : Plan := { reply := fun _ => .fail msg }
def Plan.quietFail : Plan := { reply := fun _ => { ok := false } }
def Plan.noop (msg : String) (failIfNoop : Bool) : Plan :=
  { reply := fun _ => { ok := !failIfNoop, err := if failIfNoop then [] else [s!"{msg} {noopTip}"] } }

namespace State

/-! ## Queries -/

/-- `ws`'s windows that are laid out (not parked by macOS), in order -/
def wsWindows (s : State) (ws : WsName) : List Win := s.windows.filter fun w => w.ws == ws && !w.parked

def monitorName (s : State) (mon : MonId) : String := ((s.geomOf mon).map (·.name)).getD s!"{mon}"

/-- Workspaces dwmac knows of: persistent ones, ones with windows, shown ones, and the focused one -/
def knownWorkspaces (s : State) : List WsName :=
  ((s.settings.persistent ++ s.windows.map (·.ws) ++ s.visible ++ [s.focusWs]).filter (!·.isHome)).eraseDups

/-- The digit and non-digit runs of a name: `a10b` is `a`, 10, `b` -/
def segments (name : String) : List (String ⊕ Nat) :=
  let runs := name.toList.splitBy fun a b => a.isDigit == b.isDigit
  runs.map fun r => if r.all Char.isDigit then .inr (String.ofList r).toNat! else .inl (String.ofList r)

/-- Natural order: numbers numerically and before words, then shorter first (like Swift's `StringLogicalSegments`) -/
def segmentsLt : List (String ⊕ Nat) → List (String ⊕ Nat) → Bool
  | [], [] => false
  | [], _ :: _ => true
  | _ :: _, [] => false
  | a :: as, b :: bs =>
    match a, b with
    | .inr x, .inr y => x < y || (x == y && segmentsLt as bs)
    | .inl x, .inl y => x < y || (x == y && segmentsLt as bs)
    | .inr _, .inl _ => true
    | .inl _, .inr _ => false

def wsLe (a b : WsName) : Bool := !segmentsLt (segments b.render) (segments a.render)

/-- The next or previous workspace from `cur`: on `cur`'s monitor, in natural order, or in `among`'s order -/
def nextPrevWs (s : State) (cur : WsName) (dir : NextPrev) (wrap : Bool) (among : Option (List WsName)) :
    Option WsName :=
  let mon := s.workspaceMonitor cur
  let list := match among with
    | some l => l
    | none => ((s.knownWorkspaces.filter (s.workspaceMonitor · == mon)) ++ [cur]).eraseDups.mergeSort wsLe
  let i := (list.idxOf? cur).getD 0
  match dir, wrap with
  | .next, false => list[i + 1]?
  | .prev, false => if i = 0 then none else list[i - 1]?
  | .next, true => if list.isEmpty then none else list[(i + 1) % list.length]?
  | .prev, true => if list.isEmpty then none else list[(i + list.length - 1) % list.length]?

/-- The workspace and window a command acts on -/
def resolve (s : State) : Target → Except String (WsName × Option WinId)
  | .focused => .ok (s.focusWs, s.focusWin)
  | .window id fromEnv =>
    match s.window? id with
    | some w => .ok (w.ws, some id)
    | none =>
      .error s!"Invalid <window-id> {id} {if fromEnv then "specified in DWMAC_WINDOW_ID env variable" else "passed to --window-id"}"
  | .workspace ws => .ok (ws, s.mruWin ws)

def isFloating (s : State) (id : WinId) : Bool := ((s.window? id).map (·.floating)).getD false

def fits (s : State) (ws : WsName) (id : WinId) : LayoutDesc → Bool
  | .horizontal => !s.isFloating id && (s.props ws).orientation == .h
  | .vertical => !s.isFloating id && (s.props ws).orientation == .v
  | .tiling => !s.isFloating id
  | .floating => s.isFloating id
  | .masterStack => !s.isFloating id && (s.props ws).layout == .masterStack
  | .hMasterStack => !s.isFloating id && (s.props ws).layout == .masterStack && (s.props ws).orientation == .h
  | .vMasterStack => !s.isFloating id && (s.props ws).layout == .masterStack && (s.props ws).orientation == .v

/-! ## Plans -/

/-- Focus `id`; the reply says whether it got focus -/
def focusPlan (id : WinId) : Plan := { ops := [.focusWindow id], reply := fun t => { ok := t.focusWin == some id } }

def focusWsPlan (ws : WsName) : Plan := { ops := [.focusWorkspace ws], reply := fun t => { ok := t.focusWs == ws } }

/-- Move `id` to `ws`, as its `index`th window -/
def moveWindowPlan (s : State) (id : WinId) (ws : WsName) (index : Nat) (focusFollows failIfNoop : Bool) : Plan :=
  match s.window? id with
  | none => .fail s!"Can't find window with ID {id}"
  | some w =>
    if w.ws == ws then .noop s!"Window '{id}' already belongs to workspace '{ws.render}'." failIfNoop
    else if (s.homeOf w.app).isSome && s.homeOf w.app != some ws then
      .fail s!"Window '{id}' belongs to a scratchpad. Use 'scratchpad' instead"
    else if ws.isHome && (s.homeOf w.app).isNone then .fail s!"'{ws.render}' is a scratchpad's hidden home"
    else
      { ops := if (s.homeOf w.app).isSome then [.moveWindowToWorkspace id ws false]
               else .placeWindow id ws index none :: if focusFollows then [.focusWindow id] else []
        reply := fun t => { ok := !focusFollows || t.focusWin == some id } }

def focus (s : State) (t : Target) (to : FocusTo) : Plan :=
  match s.resolve t, to with
  | .error e, _ => .fail e
  | .ok _, .window id => if (s.window? id).isSome then focusPlan id else .fail s!"Can't find window with ID {id}"
  | .ok (ws, _), .index n =>
    match (s.wsWindows ws)[n]? with
    | some w => focusPlan w.id
    | none => .fail s!"Can't find window with index {n}"
  | .ok (ws, win), .rel dir boundary =>
    let wins := s.wsWindows ws
    match wins.findIdx? (some ·.id == win) with
    | none => .quietFail
    | some i =>
      let j : Int := if dir == .next then i + 1 else i - 1
      if 0 ≤ j && j < wins.length then
        match wins[j.toNat]? with
        | some w => focusPlan w.id
        | none => .quietFail
      else
        match boundary with
        | .stop => {}
        | .fail => .quietFail
        | .wrap =>
          match wins[((j + wins.length) % wins.length).toNat]? with
          | some w => focusPlan w.id
          | none => .quietFail

def focusBackAndForth (s : State) : Plan :=
  let closed := Plan.fail "Prev window has been closed"
  match s.prevFocus with
  | none => closed
  | some (ws, win) =>
    -- If the window has moved away from the workspace, prefer the workspace (like Swift's FrozenFocus.live)
    let win := win.filter (s.hasWindow ws)
    if (ws, win.orElse fun _ => s.mruWin ws) == (s.focusWs, s.focusWin) then closed
    else match win with
      | some id => focusPlan id
      | none => focusWsPlan ws

def focusMasterOrBack (s : State) : Plan :=
  match s.focusWin.bind s.window? with
  | none => .quietFail
  | some cur =>
    match (s.wsWindows cur.ws).find? (!·.floating) with
    | none => .quietFail
    | some master =>
      if cur.id == master.id then
        match (s.lastNonMaster.find? (·.1 == cur.ws)).map (·.2) with
        | some id => if s.hasWindow cur.ws id then focusPlan id else .quietFail
        | none => .quietFail
      else
        let p := focusPlan master.id
        { p with ops := .setLastNonMaster cur.ws cur.id :: p.ops }

def workspace (s : State) (t : Target) (to : WsTo) (autoBackAndForth failIfNoop : Bool) : Plan :=
  match s.resolve t with
  | .error e => .fail e
  | .ok (cur, _) =>
    let name := match to with
      | .name ws => some ws
      | .rel dir wrap among => s.nextPrevWs cur dir wrap among
    match name, to with
    | none, _ => .quietFail
    | some ws, to =>
      if ws == cur then
        match to, autoBackAndForth, s.prevWs with
        | .name _, true, some prev => { ops := [.focusWorkspace prev] }
        | .name _, true, none => .quietFail
        | _, _, _ => .noop s!"Workspace '{ws.render}' is already focused." failIfNoop
      else focusWsPlan ws

def summonWorkspace (s : State) (ws : WsName) (failIfNoop : Bool) : Plan :=
  let mon := s.workspaceMonitor s.focusWs
  if ws.isHome then .fail s!"'{ws.render}' is a scratchpad's hidden home and is never shown. Use 'scratchpad' instead"
  else if s.monitorOf ws == some mon then .noop s!"Workspace '{ws.render}' is already visible on the focused monitor." failIfNoop
  else if !s.allowedOn ws mon then
    .fail s!"Can't move workspace '{ws.render}' to monitor '{s.monitorName mon}'. workspace-to-monitor-force-assignment doesn't allow it"
  else { ops := [.summonWorkspace ws], reply := fun t => { ok := t.focusWs == ws } }

def moveWorkspaceToMonitor (s : State) (t : Target) (mon : MonId) : Plan :=
  match s.resolve t with
  | .error e => .fail e
  | .ok (ws, _) =>
    if s.workspaceMonitor ws == mon then {}
    else
      -- Lean shows a stub on the previous monitor
      { ops := [.showWorkspace mon ws]
        reply := fun t =>
          if t.monitorOf ws == some mon then {}
          else .fail s!"Can't move workspace '{ws.render}' to monitor '{s.monitorName mon}'. workspace-to-monitor-force-assignment doesn't allow it" }

def moveNodeToWorkspace (s : State) (t : Target) (to : WsTo) (focusFollows failIfNoop : Bool) : Plan :=
  match s.resolve t with
  | .error e => .fail e
  | .ok (_, none) => .fail noWindowIsFocused
  | .ok (_, some id) =>
    let cur := ((s.window? id).map (·.ws)).getD s.focusWs
    let target := match to with
      | .name ws => some ws
      | .rel dir wrap among => s.nextPrevWs cur dir wrap among
    match target with
    | none => .fail "Can't resolve next or prev workspace"
    | some ws => s.moveWindowPlan id ws s.windows.length focusFollows failIfNoop

def moveNodeToMonitor (s : State) (t : Target) (mon : MonId) (dir : Option (Orientation × Bool))
    (focusFollows failIfNoop : Bool) : Plan :=
  match s.resolve t with
  | .error e => .fail e
  | .ok (_, none) => .fail noWindowIsFocused
  | .ok (_, some id) =>
    if ((s.window? id).map (·.parked)).getD false then
      .fail s!"Window {id} is not part of tree (minimized or hidden)"
    else match (s.screens.find? (·.1 == mon)).map (·.2) with
      | none => .fail s!"Monitor {mon} isn't connected"
      | some ws =>
        let first := dir.any fun (o, positive) => positive && (s.props ws).orientation == o
        s.moveWindowPlan id ws (if first then 0 else s.windows.length) focusFollows failIfNoop

def moveNodeToMaster (s : State) : Plan :=
  match s.focusWin.bind s.window? with
  | none => .quietFail
  | some w =>
    if w.floating then .quietFail
    else
      let others := s.windows.filter fun x => x.ws == w.ws && x.id != w.id
      let i := (others.findIdx? fun x => !x.floating && !x.parked).getD 0
      { ops := [.placeWindow w.id w.ws i none] }

def swap (s : State) (t : Target) (dir : NextPrev) (wrap swapFocus : Bool) : Plan :=
  match s.resolve t with
  | .error e => .fail e
  | .ok (_, none) => .fail noWindowIsFocused
  | .ok (_, some id) =>
    let ws := ((s.window? id).map (·.ws)).getD s.focusWs
    let tiled := (s.wsWindows ws).filter (!·.floating)
    match tiled.findIdx? (·.id == id) with
    | none => .quietFail
    | some i =>
      let j : Int := if dir == .next then i + 1 else i - 1
      let j := if 0 ≤ j && j < tiled.length then some j.toNat
        else if wrap then some ((j + tiled.length) % tiled.length).toNat else none
      match j.bind (tiled[·]?) with
      | none => .quietFail
      | some other =>
        -- Positions among all of the workspace's windows
        let all := s.windows.filter (·.ws == ws)
        let pa := (all.findIdx? (·.id == id)).getD 0
        let pb := (all.findIdx? (·.id == other.id)).getD 0
        let (a, b, pa, pb) := if pa < pb then (id, other.id, pa, pb) else (other.id, id, pb, pa)
        let p := if swapFocus then focusPlan other.id else {}
        { p with ops := [.placeWindow b ws pa none, .placeWindow a ws pb none] ++ p.ops }

def scratchpad (s : State) (name : String) : Plan :=
  match s.scratchpads.find? (·.name == name) with
  | none => .fail s!"Scratchpad '{name}' isn't declared. Declare it in the config with [scratchpad.{name}]"
  | some sp =>
    let wins := s.windows.filter fun w => w.app == sp.app && !w.parked
    if wins.isEmpty then
      -- Lean summons its first window when it appears
      { ops := [.toggleScratchpad name], reply := fun _ => { effects := [.launch sp.app] } }
    else
      let prepare := if wins.any (·.ws == s.focusWs) then [] else (s.primary wins).toList.map .prepareSummon
      { ops := [.toggleScratchpad name], reply := fun _ => { effects := prepare } }

def layoutCmd (s : State) (t : Target) (toggleBetween : List LayoutDesc) : Plan :=
  match s.resolve t with
  | .error e => .fail e
  | .ok (_, none) => .fail noWindowIsFocused
  | .ok (_, some id) =>
    let ws := ((s.window? id).map (·.ws)).getD s.focusWs
    match toggleBetween.find? (!s.fits ws id ·) |>.orElse fun _ => toggleBetween.head? with
    | none => .quietFail
    | some d =>
      if s.fits ws id d then .quietFail
      else
        let op : Op := match d with
          | .hMasterStack => .setLayout ws (some .masterStack) (some .h)
          | .vMasterStack => .setLayout ws (some .masterStack) (some .v)
          | .masterStack => .setLayout ws (some .masterStack) none
          | .horizontal => .setLayout ws none (some .h)
          | .vertical => .setLayout ws none (some .v)
          | .tiling => .setFloating id false
          | .floating => .setFloating id true
        { ops := [op] }

def mfactPlan (s : State) (change : MfactChange) : Plan :=
  match s.focusWin.bind s.window? with
  | none => .quietFail
  | some w => if (s.props w.ws).layout != .masterStack then .quietFail else { ops := [.mfact w.ws change] }

def fullscreen (s : State) (t : Target) (tog : Toggle) (noOuterGaps failIfNoop : Bool) : Plan :=
  match s.resolve t with
  | .error e => .fail e
  | .ok (_, none) => .fail noWindowIsFocused
  | .ok (_, some id) =>
    let isOn := ((s.window? id).bind (·.fullscreen)).isSome
    let on := match tog with
      | .on => true
      | .off => false
      | .toggle => !isOn
    if on == isOn then .noop (if on then "Already fullscreen." else "Already not fullscreen.") failIfNoop
    -- Lean also makes it its workspace's most recent window, the one shown fullscreen
    else { ops := [.setFullscreen id on noOuterGaps] }

def plan (s : State) : Cmd → Plan
  | .focus t to => s.focus t to
  | .focusBackAndForth => s.focusBackAndForth
  | .focusMasterOrBack => s.focusMasterOrBack
  | .focusMonitor mon =>
    { ops := [.focusMonitor mon], reply := fun t => { ok := t.monitorOf t.focusWs == some mon } }
  | .workspace t to autoBackAndForth failIfNoop => s.workspace t to autoBackAndForth failIfNoop
  | .workspaceBackAndForth =>
    match s.prevWs with
    | some ws => { ops := [.focusWorkspace ws] }
    | none => .quietFail
  | .summonWorkspace ws failIfNoop => s.summonWorkspace ws failIfNoop
  | .moveWorkspaceToMonitor t mon => s.moveWorkspaceToMonitor t mon
  | .moveNodeToWorkspace t to focusFollows failIfNoop => s.moveNodeToWorkspace t to focusFollows failIfNoop
  | .moveNodeToMonitor t mon dir focusFollows failIfNoop => s.moveNodeToMonitor t mon dir focusFollows failIfNoop
  | .moveNodeToMaster => s.moveNodeToMaster
  | .swap t dir wrap swapFocus => s.swap t dir wrap swapFocus
  | .scratchpad name => s.scratchpad name
  | .layout t toggleBetween => s.layoutCmd t toggleBetween
  | .mfact change => s.mfactPlan change
  | .fullscreen t tog noOuterGaps failIfNoop => s.fullscreen t tog noOuterGaps failIfNoop

/-! ## Running -/

/-- An event (from Swift) or a command -/
inductive Input where
  | op (op : Op)
  | cmd (cmd : Cmd)
deriving Repr

/-- After going from `s` to `t`: remember `s`'s focus if it changed -/
def notePrev (t s : State) : State :=
  { t with
    prevFocus := if t.focusWs == s.focusWs && t.focusWin == s.focusWin then t.prevFocus else some (s.focusWs, s.focusWin)
    prevWs := if t.focusWs == s.focusWs then t.prevWs else some s.focusWs }

def exec (s : State) (i : Input) : State × Reply :=
  let p : Plan := match i with
    | .op op => { ops := [op] }
    | .cmd c => s.plan c
  let t := (p.ops.foldl step s).notePrev s
  (t, p.reply t)

end State

/-! ## Proofs -/

open State

theorem Inv.notePrev {s t : State} (h : Inv t) : Inv (t.notePrev s) := by
  obtain ⟨⟨a, b, c, d⟩, ⟨e, f⟩, ⟨g, i⟩, j⟩ := h
  exact ⟨⟨a, b, c, d⟩, ⟨e, f⟩, ⟨g, i⟩, j⟩

theorem exec_inv {s : State} (h : Inv s) (i : Input) : Inv (s.exec i).1 :=
  Inv.notePrev (foldl_invariant Inv step (fun _ op h => step_inv h op) _ s h)

/-- The state after `inputs` -/
def State.execAll (s : State) (inputs : List Input) : State := inputs.foldl (fun s i => (s.exec i).1) s

/-- The invariant holds in every state commands and events can reach -/
theorem reachable_exec_inv (scratchpads : List Scratchpad) (inputs : List Input) :
    Inv ((init scratchpads).execAll inputs) :=
  foldl_invariant Inv _ (fun _ i h => exec_inv h i) inputs _ (init_inv scratchpads)

/-- **No command shows a scratchpad home** -/
theorem home_never_shown_exec (scratchpads : List Scratchpad) (inputs : List Input) :
    ∀ ws ∈ ((init scratchpads).execAll inputs).visible, ws.isHome = false :=
  (reachable_exec_inv scratchpads inputs).screens.noHome

/-- **A hidden scratchpad stays hidden**, whatever commands and events led there -/
theorem layout_never_shows_home_exec (scratchpads : List Scratchpad) (inputs : List Input) :
    let s := (init scratchpads).execAll inputs
    ∀ e ∈ s.layout false, e.shows = true → ∀ w ∈ s.windows, w.id = e.target → w.ws.isHome = false :=
  State.layout_hides_homes (reachable_exec_inv scratchpads inputs)

end DwmacCore.Model
