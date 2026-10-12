/-!
# The window manager's state machine

An abstract model of what dwmac tracks: monitors and the workspace each one shows, windows and the workspace each
one belongs to, focus, and scratchpads. Pixels, AX calls and macOS's own quirks are out of scope; the layout of a
workspace is `MasterStack`'s job.

Every command and event is an `Op`. `step` applies one in three stages:

1. the op itself (`raw`), which only has to keep the structure intact: each monitor shows one workspace, no
   workspace is on two monitors, no scratchpad home is shown, and windows sit only in their own scratchpad's home
2. `fixFocus`, which makes focus point at a shown workspace, and at one of its windows if it has any (like Swift's
   `FrozenFocus.live`)
3. `hideUnfocused`, which sends every scratchpad that doesn't have focus back to its home

`ModelProofs.lean` proves `step` preserves `Inv`, which includes the property the scratchpad fix was about: a
scratchpad home is never shown, and a scratchpad window outside its home is on the focused workspace and its
scratchpad has focus (so macOS keeps it on top).
-/

namespace DwmacCore.Model

/-- Workspace names. Swift spells a home `scratchpad:<name>`; a numeric name is `num` -/
inductive WsName where
  | num (n : Nat)
  | named (s : String)
  /-- The hidden workspace a scratchpad's windows live in while it isn't summoned -/
  | home (scratchpad : String)
deriving DecidableEq, Repr, Inhabited

namespace WsName

def isHome : WsName → Bool
  | home _ => true
  | _ => false

def numOf : WsName → Nat
  | num n => n
  | _ => 0

end WsName

abbrev WinId := Nat
abbrev MonId := Nat
abbrev AppId := String

structure Win where
  id : WinId
  app : AppId
  ws : WsName
  floating : Bool
deriving DecidableEq, Repr, Inhabited

structure Scratchpad where
  name : String
  app : AppId
deriving DecidableEq, Repr, Inhabited

structure State where
  /-- From the config; no op changes them -/
  scratchpads : List Scratchpad
  /-- Each monitor and the workspace it shows. The first monitor is the main one -/
  screens : List (MonId × WsName)
  /-- In tiling order -/
  windows : List Win
  focusWs : WsName
  focusWin : Option WinId
  /-- Most recently focused first. May hold closed windows -/
  mru : List WinId
  /-- The monitor each workspace was last shown on, most recent first -/
  assigned : List (WsName × MonId)
  /-- Scratchpads launched by `toggleScratchpad`, and the workspace to summon their first window to -/
  pending : List (String × WsName)
deriving Repr, Inhabited

namespace State

def visible (s : State) : List WsName := s.screens.map (·.2)
def monitors (s : State) : List MonId := s.screens.map (·.1)

def scratchpadOf (s : State) (app : AppId) : Option Scratchpad := s.scratchpads.find? (·.app == app)
/-- The home of the scratchpad that owns `app`'s windows, if any -/
def homeOf (s : State) (app : AppId) : Option WsName := (s.scratchpadOf app).map (.home ·.name)

def window? (s : State) (id : WinId) : Option Win := s.windows.find? (·.id == id)
def focusedApp (s : State) : Option AppId := s.focusWin.bind fun id => (s.window? id).map (·.app)

def monitorOf (s : State) (ws : WsName) : Option MonId := (s.screens.find? (·.2 == ws)).map (·.1)

/-- The monitor a workspace is (or would be) shown on: its current one, else the last one, else the main one -/
def workspaceMonitor (s : State) (ws : WsName) : MonId :=
  match s.monitorOf ws with
  | some m => m
  | none =>
    match s.assigned.find? (fun p => p.1 == ws && s.monitors.contains p.2) with
    | some p => p.2
    | none => s.monitors.head?.getD 0

/-- Whether `id` is a window of `ws` -/
def hasWindow (s : State) (ws : WsName) (id : WinId) : Bool := s.windows.any (fun w => w.id == id && w.ws == ws)

/-- The window of `ws` to focus: the most recently focused one, else the first -/
def mruWin (s : State) (ws : WsName) : Option WinId :=
  match s.mru.find? (s.hasWindow ws) with
  | some id => some id
  | none => (s.windows.find? (·.ws == ws)).map (·.id)

/-! ## Stub workspaces -/

/-- The lowest `n ≥ 1` such that workspace `n` isn't shown -/
def freshNum (vis : List WsName) : Nat :=
  match (List.range (vis.length + 1)).find? (fun n => !vis.contains (.num (n + 1))) with
  | some n => n + 1
  | none => (vis.map WsName.numOf).sum + 1 -- Unreachable: one of `vis.length + 1` numbers is free

/-- A workspace to show on `mon` when its workspace moves elsewhere: one last shown there, else a numbered one -/
def stub (s : State) (mon : MonId) : WsName :=
  match (s.assigned.filter (·.2 == mon)).find? (fun p => !p.1.isHome && !s.visible.contains p.1) with
  | some p => p.1
  | none => .num (freshNum s.visible)

/-! ## Primitive transitions -/

def replaceAt (l : List (MonId × WsName)) (m : MonId) (w : WsName) : List (MonId × WsName) :=
  l.map fun p => if p.1 = m then (p.1, w) else p

def replaceWs (l : List (MonId × WsName)) (old new : WsName) : List (MonId × WsName) :=
  l.map fun p => if p.2 = old then (p.1, new) else p

/-- Take `ws` off its monitor, if it's shown, putting a stub there -/
def vacate (s : State) (ws : WsName) : State :=
  match s.monitorOf ws with
  | some m => let st := s.stub m; { s with screens := replaceWs s.screens ws st, assigned := (st, m) :: s.assigned }
  | none => s

/-- Show `ws` on `mon`. If it's shown elsewhere, that monitor gets a stub. Scratchpad homes are never shown -/
def setActive (s : State) (mon : MonId) (ws : WsName) : State :=
  if ws.isHome || !s.monitors.contains mon || s.monitorOf ws == some mon then s
  else
    let s := s.vacate ws
    { s with screens := replaceAt s.screens mon ws, assigned := (ws, mon) :: s.assigned }

/-- Focus `ws` (and `win`), showing `ws` on its monitor -/
def focusOn (s : State) (ws : WsName) (win : Option WinId) : State :=
  let s := s.setActive (s.workspaceMonitor ws) ws
  { s with focusWs := ws, focusWin := win, mru := match win with | some id => id :: s.mru.erase id | none => s.mru }

/-- Move every window of `app` to `ws`, floating -/
def summon (s : State) (app : AppId) (ws : WsName) : List Win :=
  s.windows.map fun w => if w.app = app then { w with ws, floating := true } else w

/-- Send every window of `app` to its scratchpad's home -/
def sendHome (s : State) (app : AppId) : List Win :=
  s.windows.map fun w =>
    match s.homeOf w.app with
    | some h => if w.app = app then { w with ws := h, floating := true } else w
    | none => w

/-- The most recently focused of `wins` -/
def primary (s : State) (wins : List Win) : Option WinId :=
  match s.mru.find? (fun id => wins.any (·.id == id)) with
  | some id => some id
  | none => wins.getLast?.map (·.id)

/-! ## Ops -/

inductive Op where
  /-- `focus --window-id`, or macOS focusing a window (a click, Cmd-Tab, the Dock) -/
  | focusWindow (id : WinId)
  | focusWorkspace (ws : WsName)
  | summonWorkspace (ws : WsName)
  | focusMonitor (mon : MonId)
  | moveWorkspaceToMonitor (mon : MonId)
  | moveWindowToWorkspace (id : WinId) (ws : WsName) (focusFollows : Bool)
  | toggleScratchpad (name : String)
  | windowAppeared (id : WinId) (app : AppId)
  | windowClosed (id : WinId)
  | monitorsChanged (mons : List MonId)
deriving Repr

def focusWorkspace (s : State) (ws : WsName) : State :=
  if ws.isHome then s else s.focusOn ws (s.mruWin ws)

def focusWindow (s : State) (id : WinId) : State :=
  match s.window? id with
  | none => s
  | some w =>
    if w.ws.isHome then
      -- Focusing a hidden scratchpad window summons it; the home itself is never focused
      if s.focusWs.isHome then s else { s with windows := s.summon w.app s.focusWs }.focusOn s.focusWs (some id)
    else s.focusOn w.ws (some id)

def toggleScratchpad (s : State) (name : String) : State :=
  match s.scratchpads.find? (·.name == name) with
  | none => s
  | some sp =>
    let wins := s.windows.filter (·.app == sp.app)
    let here := wins.filter (·.ws == s.focusWs)
    if wins.isEmpty then
      -- Launch the app. Its first window will be summoned when it appears
      { s with pending := (name, s.focusWs) :: s.pending.filter (·.1 != name) }
    else if s.focusWs.isHome then s
    else if here.isEmpty then
      { s with windows := s.summon sp.app s.focusWs }.focusOn s.focusWs (s.primary wins)
    else if s.focusWin.any (fun f => here.any (·.id == f)) then
      let s := { s with windows := s.sendHome sp.app }
      s.focusOn s.focusWs (s.mruWin s.focusWs)
    else s.focusOn s.focusWs (s.primary here)

def windowAppeared (s : State) (id : WinId) (app : AppId) : State :=
  if s.windows.any (·.id == id) then s
  else
    match s.scratchpadOf app with
    | none => { s with windows := s.windows ++ [({ id, app, ws := s.focusWs, floating := false } : Win)] }
    | some sp =>
      match s.pending.find? (·.1 == sp.name) with
      | some (_, ws) =>
        if ws.isHome then s
        else
          { s with windows := s.windows ++ [({ id, app, ws, floating := true } : Win)], pending := s.pending.filter (·.1 != sp.name) }
            |>.focusOn ws (some id)
      | none =>
        -- Join already summoned siblings, else stay hidden
        let ws := ((s.windows.find? (fun w => w.app == app && !w.ws.isHome)).map (·.ws)).getD (.home sp.name)
        { s with windows := s.windows ++ [({ id, app, ws, floating := true } : Win)] }

def windowClosed (s : State) (id : WinId) : State :=
  { s with windows := s.windows.filter (·.id != id), mru := s.mru.erase id }

def moveWindowToWorkspace (s : State) (id : WinId) (ws : WsName) (focusFollows : Bool) : State :=
  match s.window? id with
  | none => s
  | some w =>
    if w.ws = ws then s
    else if (s.homeOf w.app).isSome then
      -- Scratchpads are summoned, not moved. Moving one to its home hides it
      if s.homeOf w.app = some ws then { s with windows := s.sendHome w.app } else s
    else if ws.isHome then s -- Only a scratchpad's own windows go in its home
    else
      let s := { s with windows := s.windows.filter (·.id != id) ++ [{ w with ws }] }
      if focusFollows then s.focusOn ws (some id) else s

/-- Add `mon`, showing a stub, unless it's already there -/
def addScreen (s : State) (mon : MonId) : State :=
  if s.monitors.contains mon then s
  else
    let st := s.stub mon
    { s with screens := s.screens ++ [(mon, st)], assigned := (st, mon) :: s.assigned }

/-- Monitors still connected keep their workspace; new ones get stubs -/
def monitorsChanged (s : State) (mons : List MonId) : State :=
  let s' := mons.foldl addScreen { s with screens := s.screens.filter (mons.contains ·.1) }
  if s'.screens.isEmpty then s else s'

def raw (s : State) : Op → State
  | .focusWindow id => s.focusWindow id
  | .focusWorkspace ws => s.focusWorkspace ws
  | .summonWorkspace ws =>
    if ws.isHome then s else (s.setActive (s.workspaceMonitor s.focusWs) ws).focusWorkspace ws
  | .focusMonitor mon =>
    match s.screens.find? (·.1 == mon) with
    | some (_, ws) => s.focusWorkspace ws
    | none => s
  | .moveWorkspaceToMonitor mon => s.setActive mon s.focusWs
  | .moveWindowToWorkspace id ws focusFollows => s.moveWindowToWorkspace id ws focusFollows
  | .toggleScratchpad name => s.toggleScratchpad name
  | .windowAppeared id app => s.windowAppeared id app
  | .windowClosed id => s.windowClosed id
  | .monitorsChanged mons => s.monitorsChanged mons

/-! ## Normalization -/

/-- Whether focus points at a window of the focused workspace -/
def focusWinValid (s : State) : Bool :=
  match s.focusWin with
  | some id => s.hasWindow s.focusWs id
  | none => false

/-- Focus a shown workspace: the main monitor's, if the focused one isn't shown -/
def fixFocusWs (s : State) : State :=
  if s.visible.contains s.focusWs then s
  else match s.screens with
    | [] => s
    | (_, ws) :: _ => { s with focusWs := ws }

/-- Focus the workspace's most recent window, unless focus already points at one of its windows -/
def fixFocusWin (s : State) : State :=
  if s.focusWinValid then s else { s with focusWin := s.mruWin s.focusWs }

def fixFocus (s : State) : State := s.fixFocusWs.fixFocusWin

/-- Scratchpads that don't have focus go home -/
def hideUnfocused (s : State) : State :=
  { s with windows := s.windows.map fun w =>
      match s.homeOf w.app with
      | none => w
      | some h =>
        if w.ws = s.focusWs ∧ s.focusedApp = some w.app then { w with floating := true }
        else { w with ws := h, floating := true } }

def step (s : State) (op : Op) : State := (s.raw op).fixFocus.hideUnfocused

/-- dwmac at startup: one monitor showing workspace 1, no windows -/
def init (scratchpads : List Scratchpad) : State :=
  { scratchpads, screens := [(0, .num 1)], windows := [], focusWs := .num 1, focusWin := none, mru := [],
    assigned := [(.num 1, 0)], pending := [] }

end State

end DwmacCore.Model
