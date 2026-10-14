import DwmacCore.MasterStack
import DwmacCore.Keys

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

`proofs/DwmacProofs/Model.lean` proves `step` preserves `Inv`, which includes the property the scratchpad fix was
about: a scratchpad home is never shown, and a scratchpad window outside its home is on the focused workspace and
its scratchpad has focus (so macOS keeps it on top).

The state also holds what the layout needs (`Layout.lean` turns it into frames): each workspace's layout, mfact and
monocle (`fullscreen`), and the monitors' geometry.
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

def homePrefix := "scratchpad:"

/-- Swift's workspace name: `scratchpad:x` is a home, a number (as Swift would print it) is `num` -/
def parse (s : String) : WsName :=
  if s.startsWith homePrefix then .home (s.drop homePrefix.length).toString
  else match s.toNat? with
    | some n => if toString n == s then .num n else .named s
    | none => .named s

def render : WsName → String
  | .num n => toString n
  | .named s => s
  | .home s => homePrefix ++ s

end WsName

abbrev WinId := Nat
abbrev MonId := Nat
abbrev AppId := String

structure Win where
  id : WinId
  app : AppId
  ws : WsName
  floating : Bool
  /-- macOS took it off the layout (native fullscreen, or its app is hidden). It still belongs to its workspace -/
  parked : Bool := false
  /-- `center-floating-windows` centered it since it last became floating: it stays where the user puts it -/
  centered : Bool := false
deriving DecidableEq, Repr, Inhabited

/-- A rect in global coordinates: (0, 0) is the main monitor's top left corner, y grows downwards -/
structure IRect where
  x : Int
  y : Int
  w : Nat
  h : Nat
deriving DecidableEq, Repr, Inhabited

/-- A monitor's geometry, with the config's gaps resolved for it -/
structure Geom where
  rect : IRect
  /-- Without the menu bar and the Dock -/
  visible : IRect
  /-- `visible` without the outer gaps: where windows are tiled -/
  padded : IRect
  gapH : Nat
  gapV : Nat
  /-- For messages -/
  name : String := ""
deriving DecidableEq, Repr, Inhabited

inductive WsLayout where
  | masterStack
  | floating
deriving DecidableEq, Repr, Inhabited

structure WsProps where
  layout : WsLayout
  orientation : Orientation
  mfactPermille : Nat
  /-- dwmac's `fullscreen` (not macOS's), xmonad's `Full`: every tiled window covers the workspace, and the focused
  one is in front. `some noOuterGaps` -/
  monocle : Option Bool := none
deriving DecidableEq, Repr, Inhabited

/-- Settings from the config that the invariant doesn't care about -/
structure Settings where
  defaultLayout : WsLayout := .masterStack
  defaultMfactPermille : Nat := 500
  masterAtStop : Bool := false
  centerFloating : Bool := false
  /-- `persistent-workspaces`: listed by `workspace next`/`prev` even when empty -/
  persistent : List WsName := []
deriving DecidableEq, Repr, Inhabited

inductive MfactChange where
  | set (permille : Nat)
  | add (permille : Nat)
  | sub (permille : Nat)
deriving DecidableEq, Repr

structure Scratchpad where
  name : String
  app : AppId
deriving DecidableEq, Repr, Inhabited

structure State where
  /-- From the config -/
  scratchpads : List Scratchpad
  /-- `workspace-to-monitor-force-assignment`, resolved against the connected monitors: a workspace listed here is
  only ever shown on its monitor -/
  forced : List (WsName × MonId)
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
  /-- Workspaces whose layout differs from the config's default -/
  wsProps : List (WsName × WsProps) := []
  geometry : List (MonId × Geom) := []
  settings : Settings := {}
  /-- What `focus-back-and-forth` and `workspace-back-and-forth` go back to: the focus before the last change -/
  prevFocus : Option (WsName × Option WinId) := none
  prevWs : Option WsName := none
  /-- `focus-master-or-back`: the window each workspace's master was focused from -/
  lastNonMaster : List (WsName × WinId) := []
  /-- The window macOS last reported as focused (see `syncFocus` in Command.lean) -/
  seenFocus : Option WinId := none
  /-- The window dwmac last asked macOS to focus, and when (ms) -/
  focusAsked : Option (WinId × Nat) := none
  /-- The config's key bindings and the active mode -/
  keymap : Keys.Keymap := {}
deriving Repr, Inhabited

namespace State

def visible (s : State) : List WsName := s.screens.map (·.2)
def monitors (s : State) : List MonId := s.screens.map (·.1)

def scratchpadOf (s : State) (app : AppId) : Option Scratchpad := s.scratchpads.find? (·.app == app)
/-- The home of the scratchpad that owns `app`'s windows, if any -/
def homeOf (s : State) (app : AppId) : Option WsName := (s.scratchpadOf app).map (.home ·.name)

def window? (s : State) (id : WinId) : Option Win := s.windows.find? (·.id == id)
def focusedApp (s : State) : Option AppId := s.focusWin.bind fun id => (s.window? id).map (·.app)

def props (s : State) (ws : WsName) : WsProps :=
  match s.wsProps.find? (·.1 == ws) with
  | some p => p.2
  | none => { layout := s.settings.defaultLayout, orientation := .h, mfactPermille := s.settings.defaultMfactPermille }

def setProps (s : State) (ws : WsName) (p : WsProps) : State :=
  { s with wsProps := (ws, p) :: s.wsProps.filter (·.1 != ws) }

def geomOf (s : State) (mon : MonId) : Option Geom := (s.geometry.find? (·.1 == mon)).map (·.2)

def monitorOf (s : State) (ws : WsName) : Option MonId := (s.screens.find? (·.2 == ws)).map (·.1)

def forcedOf (s : State) (ws : WsName) : Option MonId := (s.forced.find? (·.1 == ws)).map (·.2)
/-- Whether the force assignment lets `ws` be shown on `mon` -/
def allowedOn (s : State) (ws : WsName) (mon : MonId) : Bool := (s.forcedOf ws).all (· == mon)

/-- The monitor a workspace is (or would be) shown on: its forced one, else its current one, else the last one,
else the main one -/
def workspaceMonitor (s : State) (ws : WsName) : MonId :=
  match s.forcedOf ws with
  | some m => m
  | none =>
    match s.monitorOf ws with
    | some m => m
    | none =>
      match s.assigned.find? (fun p => p.1 == ws && s.monitors.contains p.2) with
      | some p => p.2
      | none => s.monitors.head?.getD 0

/-- Whether `id` is a window of `ws` -/
def hasWindow (s : State) (ws : WsName) (id : WinId) : Bool := s.windows.any (fun w => w.id == id && w.ws == ws)

/-- Whether `id` is a window of `ws` that macOS shows (not parked) -/
def hasShownWindow (s : State) (ws : WsName) (id : WinId) : Bool :=
  s.windows.any (fun w => w.id == id && w.ws == ws && !w.parked)

/-- The window of `ws` to focus: the most recently focused one, else the first. Windows macOS shows come first: a
hidden app's window, or one in native fullscreen, only if they're all like that -/
def mruWin (s : State) (ws : WsName) : Option WinId :=
  match s.mru.find? (s.hasShownWindow ws) with
  | some id => some id
  | none =>
  match s.windows.find? (fun w => w.ws == ws && !w.parked) with
  | some w => some w.id
  | none =>
  match s.mru.find? (s.hasWindow ws) with
  | some id => some id
  | none => (s.windows.find? (·.ws == ws)).map (·.id)

/-! ## Stub workspaces -/

/-- Whether `ws` can be shown on `mon` in place of what's there now -/
def stubOk (s : State) (mon : MonId) (ws : WsName) : Bool := !ws.isHome && !s.visible.contains ws && s.allowedOn ws mon

/-- The lowest `n ≥ 1` such that workspace `n` can be shown on `mon` -/
def freshNum (s : State) (mon : MonId) : Nat :=
  let bound := s.visible.length + s.forced.length + 1
  match (List.range bound).find? (fun n => s.stubOk mon (.num (n + 1))) with
  | some n => n + 1
  -- Unreachable (one of `bound` numbers is free), but this one is free by construction
  | none => ((s.visible ++ s.forced.map (·.1)).map WsName.numOf).sum + 1

/-- A workspace to show on `mon` when its workspace moves elsewhere: one last shown there, else a numbered one -/
def stub (s : State) (mon : MonId) : WsName :=
  match (s.assigned.filter (·.2 == mon)).find? (fun p => s.stubOk mon p.1) with
  | some p => p.1
  | none => .num (s.freshNum mon)

/-! ## Primitive transitions -/

def replaceAt (l : List (MonId × WsName)) (m : MonId) (w : WsName) : List (MonId × WsName) :=
  l.map fun p => if p.1 = m then (p.1, w) else p

/-- Take `ws` off its monitor, if it's shown, putting a stub there -/
def vacate (s : State) (ws : WsName) : State :=
  match s.monitorOf ws with
  | some m => let st := s.stub m; { s with screens := replaceAt s.screens m st, assigned := (st, m) :: s.assigned }
  | none => s

/-- Show `ws` on `mon`. If it's shown elsewhere, that monitor gets a stub. Scratchpad homes are never shown, and
forced workspaces only on their monitor -/
def setActive (s : State) (mon : MonId) (ws : WsName) : State :=
  if ws.isHome || !s.monitors.contains mon || !s.allowedOn ws mon || s.monitorOf ws == some mon then s
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

/-- Insert `w` so that it's the `i`th window of its workspace (or its last, if there are fewer) -/
def insertInWs (w : Win) : List Win → Nat → List Win
  | [], _ => [w]
  | x :: t, i =>
    if x.ws = w.ws then
      match i with
      | 0 => w :: x :: t
      | i + 1 => x :: insertInWs w t i
    else x :: insertInWs w t i

/-! ## Ops -/

inductive Op where
  /-- `focus --window-id`, or macOS focusing a window (a click, Cmd-Tab, the Dock) -/
  | focusWindow (id : WinId)
  | focusWorkspace (ws : WsName)
  | summonWorkspace (ws : WsName)
  | focusMonitor (mon : MonId)
  | moveWorkspaceToMonitor (mon : MonId)
  /-- Show `ws` on `mon` without focusing it -/
  | showWorkspace (mon : MonId) (ws : WsName)
  | moveWindowToWorkspace (id : WinId) (ws : WsName) (focusFollows : Bool)
  /-- Put a window at `index` in `ws` (swap, move-node-to-master, dragging with the mouse, restoring windows) -/
  | placeWindow (id : WinId) (ws : WsName) (index : Nat) (floating : Option Bool)
  | setFloating (id : WinId) (floating : Bool)
  | toggleScratchpad (name : String)
  /-- A new window. Swift picks where it would go (`ws`, `floating`, `index`); scratchpads override that -/
  | windowAppeared (id : WinId) (app : AppId) (ws : WsName) (floating : Bool) (index : Nat)
  | windowClosed (id : WinId)
  /-- The connected monitors, main first, and the force assignment resolved against them -/
  | monitorsChanged (mons : List MonId) (forced : List (WsName × MonId))
  | configChanged (scratchpads : List Scratchpad) (forced : List (WsName × MonId))
  /-- `layout`: change `ws`'s layout and/or orientation -/
  | setLayout (ws : WsName) (layout : Option WsLayout) (orientation : Option Orientation)
  | mfact (ws : WsName) (change : MfactChange)
  /-- `fullscreen`: `ws`'s tiled windows each cover it (`some noOuterGaps`), or are tiled again (`none`) -/
  | setMonocle (ws : WsName) (on : Option Bool)
  /-- macOS took the window off the layout (native fullscreen, hidden app), or gave it back -/
  | setParked (id : WinId) (parked : Bool)
  /-- Swift centered a floating window, as the layout asked (`center-floating-windows`) -/
  | centered (id : WinId)
  | geometryChanged (geometry : List (MonId × Geom))
  | settingsChanged (cfg : Settings)
  /-- `focus-master-or-back` leaves `id` for `ws`'s master -/
  | setLastNonMaster (ws : WsName) (id : WinId)
  /-- What `syncFocus` knows about macOS's focus -/
  | setFocusSync (seen : Option WinId) (asked : Option (WinId × Nat))
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

def windowAppeared (s : State) (id : WinId) (app : AppId) (ws : WsName) (floating : Bool) (index : Nat) : State :=
  if s.windows.any (·.id == id) then s
  else
    match s.scratchpadOf app with
    | none =>
      let ws := if ws.isHome then s.focusWs else ws
      { s with windows := insertInWs { id, app, ws, floating } s.windows index }
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

def placeWindow (s : State) (id : WinId) (ws : WsName) (index : Nat) (floating : Option Bool) : State :=
  match s.window? id with
  | none => s
  | some w =>
    -- A scratchpad window can only be reordered where it is, and stays floating
    let isScratchpad := (s.homeOf w.app).isSome
    if isScratchpad && w.ws != ws || ws.isHome && !isScratchpad then s
    else
      let floating := isScratchpad || floating.getD w.floating
      let centered := w.centered && floating
      { s with windows := insertInWs { w with ws, floating, centered, parked := false } (s.windows.filter (·.id != id)) index }

def setFloating (s : State) (id : WinId) (floating : Bool) : State :=
  { s with windows := s.windows.map fun w =>
      if w.id = id && (s.homeOf w.app).isNone then { w with floating, centered := w.centered && floating } else w }

/-- Add `mon`, showing a stub, unless it's already there -/
def addScreen (s : State) (mon : MonId) : State :=
  if s.monitors.contains mon then s
  else
    let st := s.stub mon
    { s with screens := s.screens ++ [(mon, st)], assigned := (st, mon) :: s.assigned }

/-- Drop disconnected monitors, and those whose workspace the new force assignment doesn't allow there -/
def keepScreens (s : State) (mons : List MonId) (forced : List (WsName × MonId)) : State :=
  let s := { s with forced }
  { s with screens := s.screens.filter fun p => mons.contains p.1 && s.allowedOn p.2 p.1 }

/-- The connected monitor `ws` is now forced onto, if it isn't shown -/
def forcedHere (t : State) (ws : WsName) : Option MonId :=
  (t.forcedOf ws).filter fun m => t.monitors.contains m && !t.visible.contains ws && !ws.isHome

def followStep (t : State) (ws : WsName) : State :=
  match t.forcedHere ws with
  | some m => t.setActive m ws
  | none => t

/-- Workspaces `prev` showed that are now forced onto a connected monitor go there, and the focused one keeps focus,
rather than leaving the screen -/
def followForced (prev s : State) : State :=
  let t := (prev.visible.filter (· != prev.focusWs)).foldl followStep s
  if (t.forcedHere prev.focusWs).isSome then t.focusOn prev.focusWs prev.focusWin else t

/-- Monitors still connected keep their workspace (if it's still allowed there); the rest get stubs. A shown
workspace that's now forced onto another connected monitor moves there -/
def monitorsChanged (s : State) (mons : List MonId) (forced : List (WsName × MonId)) : State :=
  let s' := mons.foldl addScreen (s.keepScreens mons forced)
  if s'.screens.isEmpty then s else s.followForced s'

/-- Windows left in the home of a scratchpad that no longer owns them go to the focused workspace -/
def evictFromHomes (s : State) : List Win :=
  s.windows.map fun w => if w.ws.isHome && s.homeOf w.app != some w.ws then { w with ws := s.focusWs } else w

def configChanged (s : State) (scratchpads : List Scratchpad) (forced : List (WsName × MonId)) : State :=
  let s := { s with scratchpads }
  { s with windows := s.evictFromHomes }.monitorsChanged s.monitors forced

/-- Change the windows matching `p`, keeping their id, app and workspace -/
def mapWins (s : State) (p : Win → Bool) (f : Win → Win) : List Win :=
  s.windows.map fun w => if p w then { f w with id := w.id, app := w.app, ws := w.ws } else w

def mfact (s : State) (ws : WsName) (change : MfactChange) : State :=
  let p := s.props ws
  let m := match change with
    | .set v => v
    | .add v => p.mfactPermille + v
    | .sub v => p.mfactPermille - v
  s.setProps ws { p with mfactPermille := max 50 (min 950 m) }

def setMonocle (s : State) (ws : WsName) (on : Option Bool) : State := s.setProps ws { s.props ws with monocle := on }

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
  | .showWorkspace mon ws => s.setActive mon ws
  | .moveWindowToWorkspace id ws focusFollows => s.moveWindowToWorkspace id ws focusFollows
  | .placeWindow id ws index floating => s.placeWindow id ws index floating
  | .setFloating id floating => s.setFloating id floating
  | .toggleScratchpad name => s.toggleScratchpad name
  | .windowAppeared id app ws floating index => s.windowAppeared id app ws floating index
  | .windowClosed id => s.windowClosed id
  | .monitorsChanged mons forced => s.monitorsChanged mons forced
  | .configChanged scratchpads forced => s.configChanged scratchpads forced
  | .setLayout ws layout orientation =>
    let p := s.props ws
    s.setProps ws { p with layout := layout.getD p.layout, orientation := orientation.getD p.orientation }
  | .mfact ws change => s.mfact ws change
  | .setMonocle ws on => s.setMonocle ws on
  | .setParked id parked =>
    -- A window macOS takes away loses focus (`fixFocus` picks another); macOS reports it if it keeps focus
    { s with windows := s.mapWins (·.id == id) ({ · with parked }),
             focusWin := if parked && s.focusWin == some id then none else s.focusWin }
  | .centered id => { s with windows := s.mapWins (·.id == id) ({ · with centered := true }) }
  | .geometryChanged geometry => { s with geometry }
  | .settingsChanged settings => { s with settings }
  | .setLastNonMaster ws id => { s with lastNonMaster := (ws, id) :: s.lastNonMaster.filter (·.1 != ws) }
  | .setFocusSync seen asked => { s with seenFocus := seen, focusAsked := asked }

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
  { scratchpads, forced := [], screens := [(0, .num 1)], windows := [], focusWs := .num 1, focusWin := none, mru := [],
    assigned := [(.num 1, 0)], pending := [] }

end State

end DwmacCore.Model
