import DwmacCore.StateFfi

/-! Scenarios, checked at build time -/

namespace DwmacCore.Model.Test
open State

def kitty : Scratchpad := ⟨"kitty", "net.kovidgoyal.kitty"⟩
def run (ops : List Op) : State := ops.foldl step (init [kitty])
def wsOf (s : State) (id : WinId) : Option WsName := (s.window? id).map (·.ws)

-- A hidden kitty window: focusing it (Cmd-Tab) summons it onto the focused workspace instead of showing its home
#guard
  let s := run [.windowAppeared 1 "code" (.num 1) false 0, .windowAppeared 2 kitty.app (.num 1) false 0, .focusWindow 2]
  s.visible == [.num 1] && wsOf s 2 == some (.num 1) && s.focusWin == some 2

-- ...and focusing another window sends it home again
#guard
  let s := run [.windowAppeared 1 "code" (.num 1) false 0, .windowAppeared 2 kitty.app (.num 1) false 0, .focusWindow 2, .focusWindow 1]
  wsOf s 2 == some (.home "kitty") && s.focusWin == some 1

-- Asking for a home directly is refused
#guard (run [.focusWorkspace (.home "kitty")]).visible == [.num 1]
#guard (run [.summonWorkspace (.home "kitty")]).visible == [.num 1]

-- Toggling twice summons, then hides
#guard
  let s := run [.windowAppeared 2 kitty.app (.num 1) false 0, .toggleScratchpad "kitty"]
  wsOf s 2 == some (.num 1) && s.focusWin == some 2
#guard
  let s := run [.windowAppeared 1 "code" (.num 1) false 0, .windowAppeared 2 kitty.app (.num 1) false 0, .toggleScratchpad "kitty",
    .toggleScratchpad "kitty"]
  wsOf s 2 == some (.home "kitty") && s.focusWin == some 1

-- Launching a scratchpad summons its first window when it appears
#guard
  let s := run [.toggleScratchpad "kitty", .windowAppeared 2 kitty.app (.num 1) false 0]
  wsOf s 2 == some (.num 1) && s.focusWin == some 2 && s.pending.isEmpty

-- Two monitors: moving the focused workspace to the other monitor leaves a stub behind
#guard
  let s := run [.monitorsChanged [0, 1] [], .moveWorkspaceToMonitor 1]
  s.screens == [(0, .num 3), (1, .num 1)] || s.screens == [(0, .num 2), (1, .num 1)]

-- Unplugging the monitor with focus moves focus to one still connected
#guard
  let s := run [.monitorsChanged [0, 1] [], .focusMonitor 1, .monitorsChanged [0] []]
  s.screens == [(0, .num 1)] && s.focusWs == .num 1

-- A window can't be moved into a home, and a scratchpad window can't be moved out
#guard (wsOf (run [.windowAppeared 1 "code" (.num 1) false 0, .moveWindowToWorkspace 1 (.home "kitty") false]) 1) == some (.num 1)
#guard
  let s := run [.windowAppeared 2 kitty.app (.num 1) false 0, .toggleScratchpad "kitty", .moveWindowToWorkspace 2 (.num 3) true]
  wsOf s 2 == some (.num 1)

-- Force assignment: workspace 5 lives on monitor 1, so it's never a stub on monitor 0, and focusing it shows it there
#guard
  let s := run [.monitorsChanged [0, 1] [(.num 5, 1)], .focusWorkspace (.num 5)]
  s.screens.contains (1, .num 5) && s.focusWs == .num 5
#guard
  let s := run [.monitorsChanged [0, 1] [(.num 1, 0), (.num 2, 0)], .focusMonitor 1, .moveWorkspaceToMonitor 0]
  s.screens.all (fun p => p.2 != .num 1 || p.1 == 0)

-- Placing: a window becomes master (index 0); a window can't be placed in a home
#guard
  let s := run [.windowAppeared 1 "a" (.num 1) false 0, .windowAppeared 2 "b" (.num 1) false 9, .placeWindow 2 (.num 1) 0 none]
  s.windows.map (·.id) == [2, 1]
#guard (wsOf (run [.windowAppeared 1 "a" (.num 1) false 0, .placeWindow 1 (.home "kitty") 0 none]) 1) == some (.num 1)

-- Master-stack tiles (ported from Swift's VerifiedLayoutTest): the remainder pixel goes to the first stack window
def tiles (w h n : Nat) (o : Orientation := .h) (pos : MasterPosition := .start) : List Rect :=
  masterStack { w, h, gapH := 10, gapV := 10, mfactPermille := 670, orientation := o, masterPosition := pos } n
#guard tiles 1512 945 4 == [⟨0, 0, 1006, 945⟩, ⟨1016, 0, 496, 309⟩, ⟨1016, 319, 496, 308⟩, ⟨1016, 637, 496, 308⟩]
#guard tiles 1512 945 2 (pos := .stop) == [⟨506, 0, 1006, 945⟩, ⟨0, 0, 496, 945⟩]
#guard tiles 1000 600 3 (o := .v) == [⟨0, 0, 1000, 395⟩, ⟨0, 405, 495, 195⟩, ⟨505, 405, 495, 195⟩]
#guard tiles 1512 945 0 == [] && tiles 1512 945 1 == [⟨0, 0, 1512, 945⟩]
-- Gaps that don't fit are dropped: tiles still never leave the workspace
#guard (tiles 100 100 50).length == 50 && (tiles 100 100 50).all fun r => r.x + r.w ≤ 100 && r.y + r.h ≤ 100

-- Layout: one 1920x1080 monitor, 10pt gaps
def geom : Geom := let r : IRect := ⟨0, 0, 1920, 1080⟩; ⟨r, r, r, 10, 10, "Built-in"⟩
def laidOut (ops : List Op) : List Effect := (run (.geometryChanged [(0, geom)] :: ops)).layout false

-- Two windows side by side; kitty's hidden window and workspace 2's window go to the corner
#guard
  laidOut [.windowAppeared 1 "a" (.num 1) false 0, .windowAppeared 2 "b" (.num 1) false 9,
    .windowAppeared 3 kitty.app (.num 1) false 0, .windowAppeared 4 "c" (.num 2) false 0] ==
  [.unhide 1 geom.rect false, .unhide 2 geom.rect false, .tile 1 ⟨0, 0, 955, 1079⟩ geom.padded false,
    .tile 2 ⟨965, 0, 955, 1079⟩ geom.padded false, .hide 3 .bottomRight geom.visible, .hide 4 .bottomRight geom.visible]

-- Monocle covers the workspace with every tiled window, whichever is focused; floating windows float; off tiles again
#guard
  let ops : List Op := [.windowAppeared 1 "a" (.num 1) false 0, .windowAppeared 2 "b" (.num 1) false 9,
    .windowAppeared 3 "c" (.num 1) true 9, .setMonocle (.num 1) (some false)]
  let fx := laidOut ops
  fx.contains (.tile 1 geom.padded geom.padded true) && fx.contains (.tile 2 geom.padded geom.padded true)
    && fx.contains (.float 3 0 geom.visible false)
    && laidOut (ops ++ [.focusWindow 1]) == fx && laidOut (ops ++ [.focusWindow 2]) == fx
    && (laidOut (ops ++ [.setMonocle (.num 1) none])).contains (.tile 1 ⟨0, 0, 955, 1079⟩ geom.padded false)
-- Parked ones only unhide
#guard (laidOut [.windowAppeared 1 "a" (.num 1) true 0]) == [.unhide 1 geom.rect true, .float 1 0 geom.visible false]
#guard (laidOut [.windowAppeared 1 "a" (.num 1) false 0, .setParked 1 true]) == [.unhide 1 geom.rect false]
#guard (laidOut [.windowAppeared 1 "a" (.num 1) false 0, .setLayout (.num 1) (some .floating) none]) == [.unhide 1 geom.rect false]

-- With a monitor to the right, windows hide in the bottom left corner instead
#guard
  let right : Geom := let r : IRect := ⟨1920, 0, 1920, 1080⟩; ⟨r, r, r, 0, 0, "Right"⟩
  (run [.geometryChanged [(0, geom), (1, right)]]).hideCorner geom == .bottomLeft

-- Dropping a scratchpad from the config frees its windows
#guard
  let s := run [.windowAppeared 2 kitty.app (.num 1) false 0, .configChanged [] []]
  wsOf s 2 == some (.num 1)

end DwmacCore.Model.Test

namespace DwmacCore.Model.Test
open State

-- The text protocol Swift speaks
#guard WsName.parse "scratchpad:kitty" == .home "kitty" && WsName.parse "5" == .num 5 && WsName.parse "05" == .named "05"
  && WsName.parse "S" == .named "S"
def runText (inputs : List String) : State := inputs.foldl (fun s i => (s.execText i).1) (stateInitFfi "")
#guard
  let s := runText ["windowAppeared\t7\tcode\t1\t0\t0", "monitorsChanged\t0\x1e1\t5\x1f1"]
  s.snapshot == "focus\t1\t7\nscreen\t0\t1\nscreen\t1\t2\nwindow\t7\t1\t0\t-\t0\nmru\t\nprev\t1\t-\nassigned\t2\x1f1\x1e1\x1f0\nwsdefault\tmaster-stack\th\t500"
#guard
  let s := runText ["geometryChanged\t0\x1f0,0,1920,1080\x1f0,25,1920,1055\x1f0,25,1920,1055\x1f10\x1f10\x1fBuilt-in",
    "windowAppeared\t7\tcode\t1\t0\t0", "mfact\t1\tadd\t100", "setLayout\t1\t-\tv"]
  stateLayoutFfi s 0 == "unhide\t7\t0,0,1920,1080\t0\ntile\t7\t0,25,1920,1054\t0,25,1920,1055\t0"
    && (s.props (.num 1)).mfactPermille == 600 && (s.props (.num 1)).orientation == .v
#guard ((init []).execText "nonsense").1.snapshot == (init []).snapshot

end DwmacCore.Model.Test

namespace DwmacCore.Model.Test
open State

-- center-floating-windows: a floating window is centered until Swift reports it did, then stays where the user puts
-- it; floating again after tiling centers it again
def centering : List Op := [.settingsChanged { centerFloating := true }, .windowAppeared 1 "a" (.num 1) true 0]
def centers (ops : List Op) : Bool := (laidOut (centering ++ ops)).any (·.centers)
#guard centers []
#guard !centers [.centered 1]
#guard !centers [.centered 1, .focusWorkspace (.num 2), .focusWorkspace (.num 1)]
#guard !centers [.centered 1, .setFloating 1 false]
#guard centers [.centered 1, .setFloating 1 false, .setFloating 1 true]
#guard !(laidOut [.windowAppeared 1 "a" (.num 1) true 0]).any (·.centers)

end DwmacCore.Model.Test
