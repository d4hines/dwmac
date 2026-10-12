import DwmacCore.Model
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
#guard
  let s := ["windowAppeared\t7\tcode\t1\t0\t0", "monitorsChanged\t0\x1e1\t5\x1f1"].foldl stateStepFfi (stateInitFfi "")
  s.snapshot == "focus\t1\t7\nscreen\t0\t1\nscreen\t1\t2\nwindow\t7\t1\t0"
#guard (stateStepFfi (init []) "nonsense").snapshot == (init []).snapshot

end DwmacCore.Model.Test
