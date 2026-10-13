import DwmacCore.StateFfi

/-! Commands, checked at build time (ported from Swift's command tests) -/

namespace DwmacCore.Model.CommandTest
open State

def kitty : Scratchpad := ⟨"kitty", "net.kovidgoyal.kitty"⟩
def go (inputs : List Input) : State := (init [kitty]).execAll inputs
/-- The reply to `c`, after `inputs` -/
def reply (inputs : List Input) (c : Cmd) : Reply := ((go inputs).exec (.cmd c)).2
/-- The focused window after each command, starting from `inputs` -/
def focuses (inputs : List Input) (cmds : List Cmd) : List (Option WinId) :=
  (cmds.foldl (fun (s, l) c => let t := (s.exec (.cmd c)).1; (t, l ++ [t.focusWin])) (go inputs, [])).2

def a : WsName := .named "a"
def b : WsName := .named "b"
/-- A new window, last in `ws` -/
def win (id : WinId) (ws : WsName := a) (app : AppId := "app") : Input := .op (.windowAppeared id app ws false 99)
def floatWin (id : WinId) (ws : WsName := a) : Input := .op (.windowAppeared id "app" ws true 99)
def focusW (id : WinId) : Input := .op (.focusWindow id)
def order (s : State) (ws : WsName) : List WinId := (s.windows.filter (·.ws == ws)).map (·.id)
def wsOf (s : State) (id : WinId) : Option WsName := (s.window? id).map (·.ws)

def next (b : Boundary := .wrap) : Cmd := .focus .focused (.rel .next b)
def prev (b : Boundary := .wrap) : Cmd := .focus .focused (.rel .prev b)

/-! ## focus -/

-- Next and previous, in the workspace's order
#guard focuses [win 1, win 2, win 3, win 4, focusW 1] [next, next, next, prev, prev, prev] ==
  [some 2, some 3, some 4, some 3, some 2, some 1]
-- Floating windows are included; past the end it wraps by default
#guard focuses [win 1, win 2, floatWin 3, focusW 1] [next, next, next] == [some 2, some 3, some 1]
#guard focuses [win 1, win 2, win 3, focusW 3] [next, prev] == [some 1, some 3]
-- --boundaries-action: stop succeeds without moving, fail fails, wrap wraps
#guard
  let s := [win 1, win 2, focusW 1]
  (reply s (prev .stop)).ok && !(reply s (prev .fail)).ok && focuses s [prev .stop, prev .fail, prev .wrap] == [some 1, some 1, some 2]
#guard
  let s := [win 1, win 2, focusW 2]
  (reply s (next .stop)).ok && !(reply s (next .fail)).ok && focuses s [next .stop, next .fail, next .wrap] == [some 2, some 2, some 1]
-- By index, and by window id
#guard focuses [win 1, win 2, win 3, focusW 1] [.focus .focused (.index 1), .focus .focused (.window 3)] == [some 2, some 3]
#guard (reply [win 1] (.focus .focused (.index 5))).err == ["Can't find window with index 5"]
#guard (reply [win 1] (.focus .focused (.window 9))).err == ["Can't find window with ID 9"]
#guard (reply [win 1] (.focus (.window 9 false) (.rel .next .wrap))).err == ["Invalid <window-id> 9 passed to --window-id"]
#guard (reply [win 1] (.focus (.window 9 true) (.rel .next .wrap))).err ==
  ["Invalid <window-id> 9 specified in DWMAC_WINDOW_ID env variable"]

/-! ## focus-back-and-forth, focus-master-or-back -/

#guard focuses [win 1, win 2, focusW 1, focusW 2] [.focusBackAndForth, .focusBackAndForth] == [some 1, some 2]
-- Across workspaces
#guard focuses [win 1, win 2 b, focusW 1, focusW 2] [.focusBackAndForth] == [some 1]
-- A closed window: back to its workspace
#guard
  let s := go [win 1, win 2, win 3 b, focusW 2, focusW 3, .op (.windowClosed 2), .cmd .focusBackAndForth]
  s.focusWs == a && s.focusWin == some 1
#guard (reply [] .focusBackAndForth).err == ["Prev window has been closed"]

-- Master, back, and again from another window
#guard focuses [win 1, win 2, win 3, focusW 2] [.focusMasterOrBack, .focusMasterOrBack] == [some 1, some 2]
#guard focuses [win 1, win 2, win 3, focusW 2, .cmd .focusMasterOrBack, .cmd .focusMasterOrBack, focusW 3]
  [.focusMasterOrBack, .focusMasterOrBack] == [some 1, some 3]
-- Focused the master without the command: nowhere to go back to
#guard !(reply [win 1, win 2, focusW 1] .focusMasterOrBack).ok

/-! ## workspace, workspace-back-and-forth -/

def wsCmd (ws : WsName) (autoBackAndForth := false) (failIfNoop := false) : Cmd :=
  .workspace .focused (.name ws) autoBackAndForth failIfNoop
def wsNext (wrap := true) (among : Option (List WsName) := none) : Cmd := .workspace .focused (.rel .next wrap among) false false

#guard (go [win 1, .cmd (wsCmd b)]).focusWs == b
-- Already there: a tip, or a failure with --fail-if-noop
#guard
  let r := reply [] (wsCmd (.num 1))
  r.ok && r.err == ["Workspace '1' is already focused. Tip: use --fail-if-noop to exit with non-zero code"]
#guard reply [] (wsCmd (.num 1) (failIfNoop := true)) == { ok := false }
-- --auto-back-and-forth goes back instead
#guard (go [.cmd (wsCmd b), .cmd (wsCmd b true)]).focusWs == .num 1
#guard (go [.cmd (wsCmd b), .cmd (wsCmd (.num 3)), .cmd .workspaceBackAndForth]).focusWs == b
-- Next and previous: the workspaces on the monitor in natural order (numbers first, numerically), skipping homes.
-- Workspace 1 is forgotten once it's empty and no longer shown
#guard
  let s := [win 1 (.num 10), win 2 (.num 2), win 3 b, win 4 (.home "kitty") kitty.app]
  focuses s [wsNext, wsNext, wsNext, wsNext] == [some 2, some 1, some 3, some 2]
#guard (go [win 2 (.num 2), .cmd (wsNext (wrap := false)), .cmd (wsNext (wrap := false))]).focusWs == .num 2
#guard !(reply [win 2 (.num 2), .cmd (wsNext (wrap := false))] (wsNext (wrap := false))).ok
-- Persistent workspaces are listed even when empty
#guard (go [.op (.settingsChanged { persistent := [.num 1, .num 4] }), .cmd wsNext]).focusWs == .num 4
-- --stdin: in the given order
#guard (go [.cmd (wsNext (among := some [b, .num 1, a]))]).focusWs == a

/-! ## summon-workspace, move-workspace-to-monitor -/

def twoMonitors : List Input :=
  [.op (.monitorsChanged [0, 1] [(.num 5, 1)]),
   .op (.geometryChanged [(0, { rect := ⟨0, 0, 1, 1⟩, visible := ⟨0, 0, 1, 1⟩, padded := ⟨0, 0, 1, 1⟩, gapH := 0, gapV := 0, name := "Built-in" }),
     (1, { rect := ⟨1, 0, 1, 1⟩, visible := ⟨1, 0, 1, 1⟩, padded := ⟨1, 0, 1, 1⟩, gapH := 0, gapV := 0, name := "LG" })])]

#guard (reply [] (.summonWorkspace (.home "kitty") false)).err ==
  ["'scratchpad:kitty' is a scratchpad's hidden home and is never shown. Use 'scratchpad' instead"]
#guard (reply [] (.summonWorkspace (.num 1) true)) == { ok := false }
#guard (reply twoMonitors (.summonWorkspace (.num 5) false)).err ==
  ["Can't move workspace '5' to monitor 'Built-in'. workspace-to-monitor-force-assignment doesn't allow it"]
-- Summoning the other monitor's workspace swaps in a stub there
#guard
  let s := go (twoMonitors ++ [.cmd (.summonWorkspace (.num 2) false)])
  s.focusWs == .num 2 && s.screens.contains (0, .num 2) && !(s.screens.contains (1, .num 2))

#guard (go (twoMonitors ++ [.cmd (.moveWorkspaceToMonitor .focused 1)])).screens.contains (1, .num 1)
#guard (reply (twoMonitors ++ [.cmd (wsCmd (.num 5))]) (.moveWorkspaceToMonitor .focused 0)).err ==
  ["Can't move workspace '5' to monitor 'Built-in'. workspace-to-monitor-force-assignment doesn't allow it"]

/-! ## move-node-to-workspace, move-node-to-monitor -/

def moveTo (ws : WsName) (focusFollows := false) (failIfNoop := false) : Cmd :=
  .moveNodeToWorkspace .focused (.name ws) focusFollows failIfNoop

#guard
  let s := go [win 1, win 2, focusW 2, .cmd (moveTo b)]
  order s a == [1] && order s b == [2] && s.focusWs == a && s.focusWin == some 1
#guard (go [floatWin 1, focusW 1, .cmd (moveTo b)]).windows.all (·.floating)
#guard (go [win 1, focusW 1, .cmd (moveTo b (focusFollows := true))]).focusWs == b
#guard (reply [win 1, focusW 1] (moveTo a)).err ==
  ["Window '1' already belongs to workspace 'a'. Tip: use --fail-if-noop to exit with non-zero code"]
#guard (reply [.cmd (wsCmd b)] (moveTo a)).err == ["No window is focused"]
-- Scratchpad windows are summoned, not moved; moving one home hides it
#guard
  let s := [win 1, win 2 a kitty.app, .cmd (.scratchpad "kitty")]
  (reply s (moveTo b)).err == ["Window '2' belongs to a scratchpad. Use 'scratchpad' instead"]
    && wsOf (go (s ++ [.cmd (moveTo (.home "kitty"))])) 2 == some (.home "kitty")
#guard (reply [win 1, focusW 1] (moveTo (.home "kitty"))).err == ["'scratchpad:kitty' is a scratchpad's hidden home"]

-- To the other monitor's workspace: last, or first when moving right along its orientation
#guard
  let s := twoMonitors ++ [win 1 (.num 2), win 2 (.num 1), focusW 2]
  order (go (s ++ [.cmd (.moveNodeToMonitor .focused 1 none false false)])) (.num 2) == [1, 2]
    && order (go (s ++ [.cmd (.moveNodeToMonitor .focused 1 (some (.h, true)) false false)])) (.num 2) == [2, 1]
    && order (go (s ++ [.cmd (.moveNodeToMonitor .focused 1 (some (.v, true)) false false)])) (.num 2) == [1, 2]

/-! ## move-node-to-master, swap -/

#guard order (go [win 1, win 2, focusW 2, .cmd .moveNodeToMaster]) a == [2, 1]
#guard order (go [win 1, win 2, focusW 1, .cmd .moveNodeToMaster]) a == [1, 2]
-- Floating windows go before the master, and can't become it
#guard order (go [floatWin 1, win 2, win 3, focusW 3, .cmd .moveNodeToMaster]) a == [1, 3, 2]
#guard !(reply [floatWin 1, win 2, focusW 1] .moveNodeToMaster).ok

-- rotate: the last tiled window becomes master and is focused; repeated, every window takes a turn
#guard
  let s := go [win 1, win 2, win 3, focusW 1, .cmd .rotate]
  order s a == [3, 1, 2] && s.focusWin == some 3
#guard order (go [win 1, win 2, win 3, focusW 1, .cmd .rotate, .cmd .rotate, .cmd .rotate]) a == [1, 2, 3]
-- Floating windows (overlays, summoned scratchpads) are skipped, wherever they are, and stay put
#guard
  let s := go [floatWin 1, win 2, win 3, floatWin 4, focusW 2, .cmd .rotate]
  order s a == [1, 3, 2, 4] && s.focusWin == some 3
-- Parked windows (native fullscreen, hidden app) are skipped too
#guard order (go [win 1, win 2, win 3, .op (.setParked 3 true), focusW 1, .cmd .rotate]) a == [2, 1, 3]
-- One tiled window: it's focused
#guard (go [win 1, floatWin 2, focusW 2, .cmd .rotate]).focusWin == some 1
-- No tiled window: nothing to do
#guard !(reply [floatWin 1, focusW 1] .rotate).ok && (reply [floatWin 1, focusW 1] .rotate).err.isEmpty
-- Only the focused workspace rotates
#guard
  let s := go [win 1, win 2, win 3 b, win 4 b, focusW 1, .cmd .rotate]
  order s a == [2, 1] && order s b == [3, 4]

def swapCmd (dir : NextPrev) (wrap := false) (swapFocus := false) : Cmd := .swap .focused dir wrap swapFocus

#guard
  let s := [win 1, win 2, win 3, focusW 1]
  order (go (s ++ [.cmd (swapCmd .next)])) a == [2, 1, 3]
    && order (go (s ++ [.cmd (swapCmd .next), .cmd (swapCmd .next)])) a == [2, 3, 1]
    && order (go (s ++ [.cmd (swapCmd .next), .cmd (swapCmd .next), .cmd (swapCmd .prev)])) a == [2, 1, 3]
    && (go (s ++ [.cmd (swapCmd .next), .cmd (swapCmd .next)])).focusWin == some 1
#guard
  let s := [win 1, win 2, win 3, focusW 1]
  !(reply s (swapCmd .prev)).ok && order (go (s ++ [.cmd (swapCmd .prev true)])) a == [3, 2, 1]
    && order (go (s ++ [.cmd (swapCmd .prev true), .cmd (swapCmd .next true)])) a == [1, 2, 3]
#guard
  let s := go [win 1, win 2, win 3, focusW 2, .cmd (swapCmd .next (swapFocus := true))]
  order s a == [1, 3, 2] && s.focusWin == some 3

/-! ## syncFocus: Lean's focus and macOS's -/

def sync (n : Option WinId) (now : Nat) : Input := .cmd (.syncFocus n now)
def kittyWin (id : WinId) : Input := .op (.windowAppeared id kitty.app a false 99)
/-- Windows 1 and 2 tiled on `a`, kitty's window 9 hidden; macOS and Lean agree on 1 -/
def agreed : List Input := [win 1, win 2, kittyWin 9, focusW 1, sync (some 1) 0]

#guard (go agreed).seenFocus == some 1 && (go agreed).focusAsked == none
-- A click (macOS moved on its own): Lean follows
#guard (go (agreed ++ [sync (some 2) 10])).focusWin == some 2
-- A command moves Lean: dwmac asks macOS to follow, then waits for it to land
#guard (reply (agreed ++ [focusW 2]) (.syncFocus (some 1) 10)).effects == [.nativeFocus 2, .recheck focusGraceMs]
#guard
  let s := go (agreed ++ [focusW 2, sync (some 1) 10, sync (some 1) 100])
  s.focusWin == some 2 && s.focusAsked == some (2, 10)
#guard
  let s := go (agreed ++ [focusW 2, sync (some 1) 10, sync (some 2) 100])
  s.focusWin == some 2 && s.focusAsked == none && s.seenFocus == some 2
-- The request didn't land in time: Lean follows macOS
#guard (go (agreed ++ [focusW 2, sync (some 1) 10, sync (some 1) (10 + focusGraceMs)])).focusWin == some 1
-- Popups, apps without windows, and unknown windows change nothing
#guard (go (agreed ++ [focusW 2, sync none 10])).focusWin == some 2
#guard (go (agreed ++ [focusW 2, sync (some 77) 10])).focusAsked == none
-- Cmd-Tab to a hidden scratchpad's window summons it
#guard
  let s := go (agreed ++ [sync (some 9) 10])
  s.focusWin == some 9 && wsOf s 9 == some a

-- The bug: toggling kitty and rotating faster than macOS activates apps. Each command's request is sent, but they
-- land out of order and Chrome (1) ends up in front while Lean still has kitty focused, on screen behind Chrome.
-- After the grace period, Lean follows macOS: Chrome focused, kitty hidden again
#guard
  let spam := agreed ++ [.cmd (.scratchpad "kitty"), sync (some 1) 10, .cmd .rotate, sync (some 1) 20,
    .cmd (.scratchpad "kitty"), sync (some 1) 30]
  let s := go spam
  -- Lean has kitty focused and on screen, waiting for macOS
  s.focusWin == some 9 && wsOf s 9 == some a && s.focusAsked == some (9, 30)
    -- macOS keeps reporting Chrome: Lean waits out the grace period, then follows
    && (go (spam ++ [sync (some 1) 100])).focusWin == some 9
    && (go (spam ++ [sync (some 1) 100, sync (some 1) (30 + focusGraceMs)])).focusWin == some 1
    && wsOf (go (spam ++ [sync (some 1) 100, sync (some 1) (30 + focusGraceMs)])) 9 == some (.home "kitty")

/-! ## scratchpad -/

#guard (reply [] (.scratchpad "nope")).err == ["Scratchpad 'nope' isn't declared. Declare it in the config with [scratchpad.nope]"]
-- No windows: launch the app
#guard (reply [] (.scratchpad "kitty")).effects == [.launch kitty.app]
-- A hidden window: resize it before it's summoned. Dismissing doesn't
#guard (reply [win 1, win 2 a kitty.app, focusW 1] (.scratchpad "kitty")).effects == [.prepareSummon 2]
#guard (reply [win 1, win 2 a kitty.app, focusW 1, .cmd (.scratchpad "kitty")] (.scratchpad "kitty")).effects == []
-- Summoned from another workspace; a new window joins its summoned siblings, floating
#guard
  let s := go [win 1 a kitty.app, .cmd (.scratchpad "kitty"), win 2 b kitty.app]
  wsOf s 1 == some (.num 1) && wsOf s 2 == some (.num 1) && s.isFloating 2 && s.focusWin == some 1
-- next and prev never land in a home
#guard
  let s := go [win 1 (.num 1) kitty.app, win 2 b, .cmd wsNext]
  !s.focusWs.isHome

/-! ## layout, mfact, fullscreen -/

def layoutCmd (descs : List LayoutDesc) : Cmd := .layout .focused descs

#guard
  let s := [win 1, focusW 1]
  (go (s ++ [.cmd (layoutCmd [.tiling, .floating])])).isFloating 1
    && !(go (s ++ [.cmd (layoutCmd [.tiling, .floating]), .cmd (layoutCmd [.tiling, .floating])])).isFloating 1
#guard ((go [win 1, focusW 1, .cmd (layoutCmd [.vMasterStack])]).props a).orientation == .v
-- Already that layout: fails
#guard !(reply [win 1, focusW 1] (layoutCmd [.hMasterStack])).ok
#guard (reply [.cmd (wsCmd b)] (layoutCmd [.floating])).err == ["No window is focused"]

def mfactAfter (change : MfactChange) : Nat := ((go [win 1, focusW 1, .cmd (.mfact change)]).props a).mfactPermille
#guard mfactAfter (.set 700) == 700 && mfactAfter (.add 100) == 600 && mfactAfter (.sub 100) == 400
#guard mfactAfter (.set 1500) == 950 && mfactAfter (.set 0) == 50
#guard !(reply [win 1, focusW 1, .op (.setLayout a (some .floating) none)] (.mfact (.add 100))).ok

def fullscreenCmd (tog : Toggle) (failIfNoop := false) : Cmd := .fullscreen .focused tog false failIfNoop
#guard
  let s := [win 1, win 2, focusW 1]
  ((go (s ++ [.cmd (fullscreenCmd .toggle)])).window? 1).bind (·.fullscreen) == some false
    && ((go (s ++ [.cmd (fullscreenCmd .toggle), .cmd (fullscreenCmd .toggle)])).window? 1).bind (·.fullscreen) == none
#guard (reply [win 1, focusW 1] (fullscreenCmd .off)).err ==
  ["Already not fullscreen. Tip: use --fail-if-noop to exit with non-zero code"]
#guard reply [win 1, focusW 1] (fullscreenCmd .off true) == { ok := false }

/-! ## The text protocol -/

#guard
  let (s, r) := (go [win 1, win 2, focusW 1]).execText "cmd\tfocus\t-\tnext\twrap"
  s.focusWin == some 2 && r.render == "ok\t1"
#guard ((go []).execText "cmd\tscratchpad\tnope").2.render ==
  "ok\t0\nerr\tScratchpad 'nope' isn't declared. Declare it in the config with [scratchpad.nope]"
#guard ((go []).execText "cmd\tscratchpad\tkitty").2.render == "ok\t1\neffect\tlaunch\tnet.kovidgoyal.kitty"
#guard ((go [win 1, win 2]).execText "cmd\tworkspace\t-\tnext\t-\t1\tb\x1ea\t0\t0").1.focusWs == a
#guard
  let s := (go [win 1, win 2 b, focusW 1, focusW 2]).snapshot
  (s.splitOn "\n").contains "prev\ta\t1"

end DwmacCore.Model.CommandTest

/-! ## Bugs found in other window managers' reports (scratchpad invariant mining) -/

namespace DwmacCore.Model.CommandTest
open State

-- A window that's hidden (its app hidden, or native fullscreen) loses focus to one that's shown, and `focus next`
-- still works. AeroSpace #1614, #18
#guard (go [win 1, win 2, focusW 1, .op (.setParked 1 true)]).focusWin == some 2
#guard (reply [win 1, win 2, win 3, focusW 1, .op (.setParked 1 true)] next).ok
-- Closing the focused window doesn't hand focus to a hidden one
#guard (go [win 1, win 2, win 3, focusW 2, focusW 1, .op (.setParked 2 true), .op (.windowClosed 1)]).focusWin == some 3
-- When every window is hidden, focus stays (macOS reports a native fullscreen window as focused)
#guard (go [win 1, focusW 1, .op (.setParked 1 true)]).focusWin == some 1

-- A force assignment (new monitor, or config reload) moves the focused workspace to its monitor, instead of taking it
-- off screen. AeroSpace #520, #2173; i3 #2936
def W : WsName := .named "W"
#guard let s := go [win 1 W, focusW 1, .op (.monitorsChanged [0, 1] [(W, 1)])]
  s.focusWs == W && s.focusWin == some 1 && s.screens.lookup 1 == some W
#guard let s := go [.op (.monitorsChanged [0, 1] []), win 1 W, focusW 1, .op (.configChanged [kitty] [(W, 1)])]
  s.focusWs == W && s.focusWin == some 1 && s.screens.lookup 1 == some W
#guard let s := go [.op (.monitorsChanged [0, 1] []), win 1 (.num 1), focusW 1, .op (.configChanged [kitty] [(.num 1, 1)])]
  s.focusWs == .num 1 && s.screens.lookup 1 == some (.num 1)
-- A workspace that was shown, not focused, follows its new assignment too
#guard let s := go [.op (.monitorsChanged [0, 1] []), win 1 W, focusW 1, .op (.showWorkspace 1 b), win 2 b, focusW 2,
    .op (.configChanged [kitty] [(W, 1)])]
  s.screens.lookup 1 == some W
-- Unplugging a monitor doesn't push its workspace onto the main one
#guard let s := go [.op (.monitorsChanged [0, 1] []), win 1 (.num 1), .op (.showWorkspace 1 b), win 2 b, focusW 1,
    .op (.monitorsChanged [0] [])]
  s.screens == [(0, .num 1)]

-- Closing the focused window: macOS focuses another window of the same app, on a hidden workspace. Lean keeps
-- the user's workspace and asks macOS to focus its pick. AeroSpace #1097, #571, #854
def twoSafaris : List Input := [.op (.monitorsChanged [0, 1] []), win 1 (.num 1) "safari", win 2 (.num 3) "safari",
  win 5 (.num 1) "term", focusW 1, sync (some 1) 0]
#guard let s := go (twoSafaris ++ [.op (.windowClosed 1)])
  let (t, r) := s.exec (sync (some 2) 10)
  t.focusWs == .num 1 && t.focusWin == some 5 && r.effects.contains (.nativeFocus 5)
-- A click on that window, with nothing closed, still goes there
#guard let (t, _) := (go twoSafaris).exec (sync (some 2) 10)
  t.focusWs == .num 3 && t.focusWin == some 2
-- If macOS won't focus Lean's pick, Lean follows it a grace period later
#guard let s := go (twoSafaris ++ [.op (.windowClosed 1), sync (some 2) 10])
  (s.exec (sync (some 2) (10 + focusGraceMs))).1.focusWin == some 2

end DwmacCore.Model.CommandTest
