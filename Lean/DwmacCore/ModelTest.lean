import DwmacCore.Model

/-! Scenarios, checked at build time -/

namespace DwmacCore.Model.Test
open State

def kitty : Scratchpad := ⟨"kitty", "net.kovidgoyal.kitty"⟩
def run (ops : List Op) : State := ops.foldl step (init [kitty])
def wsOf (s : State) (id : WinId) : Option WsName := (s.window? id).map (·.ws)

-- A hidden kitty window: focusing it (Cmd-Tab) summons it onto the focused workspace instead of showing its home
#guard
  let s := run [.windowAppeared 1 "code", .windowAppeared 2 kitty.app, .focusWindow 2]
  s.visible == [.num 1] && wsOf s 2 == some (.num 1) && s.focusWin == some 2

-- ...and focusing another window sends it home again
#guard
  let s := run [.windowAppeared 1 "code", .windowAppeared 2 kitty.app, .focusWindow 2, .focusWindow 1]
  wsOf s 2 == some (.home "kitty") && s.focusWin == some 1

-- Asking for a home directly is refused
#guard (run [.focusWorkspace (.home "kitty")]).visible == [.num 1]
#guard (run [.summonWorkspace (.home "kitty")]).visible == [.num 1]

-- Toggling twice summons, then hides
#guard
  let s := run [.windowAppeared 2 kitty.app, .toggleScratchpad "kitty"]
  wsOf s 2 == some (.num 1) && s.focusWin == some 2
#guard
  let s := run [.windowAppeared 1 "code", .windowAppeared 2 kitty.app, .toggleScratchpad "kitty",
    .toggleScratchpad "kitty"]
  wsOf s 2 == some (.home "kitty") && s.focusWin == some 1

-- Launching a scratchpad summons its first window when it appears
#guard
  let s := run [.toggleScratchpad "kitty", .windowAppeared 2 kitty.app]
  wsOf s 2 == some (.num 1) && s.focusWin == some 2 && s.pending.isEmpty

-- Two monitors: moving the focused workspace to the other monitor leaves a stub behind
#guard
  let s := run [.monitorsChanged [0, 1], .moveWorkspaceToMonitor 1]
  s.screens == [(0, .num 3), (1, .num 1)] || s.screens == [(0, .num 2), (1, .num 1)]

-- Unplugging the monitor with focus moves focus to one still connected
#guard
  let s := run [.monitorsChanged [0, 1], .focusMonitor 1, .monitorsChanged [0]]
  s.screens == [(0, .num 1)] && s.focusWs == .num 1

-- A window can't be moved into a home, and a scratchpad window can't be moved out
#guard (wsOf (run [.windowAppeared 1 "code", .moveWindowToWorkspace 1 (.home "kitty") false]) 1) == some (.num 1)
#guard
  let s := run [.windowAppeared 2 kitty.app, .toggleScratchpad "kitty", .moveWindowToWorkspace 2 (.num 3) true]
  wsOf s 2 == some (.num 1)

end DwmacCore.Model.Test
