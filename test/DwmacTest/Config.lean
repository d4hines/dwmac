import DwmacCore.Config

/-! Ported from the Swift config tests -/

namespace DwmacCore.Config.Test

def testEnv : List (String × String) := [("PATH", "DWMAC_TEST_PATH"), ("DWMAC_INHERITED_TEST_ENV", "inherited")]
def run (src : String) : Config × List String :=
  match parse testEnv src with
  | .ok r => r
  | .error e => ({}, ["SYNTAX: " ++ e])
def errs (src : String) : List String := (run src).2
def cfg (src : String) : Config := (run src).1
def main (src : String) : List Binding := ((cfg src).modes.lookup "main").getD []

#guard errs "config-version = 0" == ["config-version: Must be in [1, 2] range"]
#guard errs "exec-on-workspace-change = ['', 1]" ==
  ["exec-on-workspace-change[1]: Expected type is 'string'. But actual type is 'integer'"]
#guard errs "config-version = 2\npersistent-workspaces = ['a', 'a']" ==
  ["persistent-workspaces: Contains duplicated workspace names"]
#guard errs "persistent-workspaces = ['a']" ==
  ["persistent-workspaces: This config option is only available since 'config-version = 2'"]
#guard errs "unknownKey = true" == ["unknownKey: Unknown top-level key"]
#guard errs "[gaps]\n  unknownKey = true" == ["gaps.unknownKey: Unknown key"]
#guard errs "start-at-login = 'true'" == ["start-at-login: Expected type is 'bool'. But actual type is 'string'"]
#guard errs "true" == ["SYNTAX: Unexpected end of file in a key-value pair (at line 1, column 5)"]
#guard errs "default-mfact = 1" == ["default-mfact: Expected type is 'floating-point number'. But actual type is 'integer'"]
#guard (cfg "default-mfact = 0.67").mfact == "0.67"

-- Bindings
#guard main "[mode.main.binding]\n  lcmd-a = 'focus next'\n  rcmd-a = 'focus prev'\n  cmd-a = 'focus next'" ==
  [{ raw := "cmd-a", keyCode := 0, modifiers := Keys.command, desc := "cmd-a", commands := ["focus next"] },
   { raw := "lcmd-a", keyCode := 0, modifiers := Keys.command ||| Keys.lCommand, desc := "lcmd-a", commands := ["focus next"] },
   { raw := "rcmd-a", keyCode := 0, modifiers := Keys.command ||| Keys.rCommand, desc := "rcmd-a", commands := ["focus prev"] }]
#guard match errs "[mode.main.binding]\n  alt-a = 'list-apps'" with
  | [e] => e == "mode.main.binding.alt-a: Command 'list-apps' cannot be used in config"
  | _ => false
#guard errs "mode.main = {}" == [] && main "mode.main = {}" == []
#guard main "[mode.main.binding]\n  alt-h = 'focus next'" ==
  [{ raw := "alt-h", keyCode := 4, modifiers := Keys.option, desc := "alt-h", commands := ["focus next"] }]
#guard errs "[mode.foo.binding]\n  alt-h = 'focus next'" == ["mode: Please specify 'main' mode"]
#guard errs "[mode.main.binding]\n  alt-hh = 'focus next'\n  aalt-j = 'focus next'\n  alt-k = 'focus prev'" ==
  ["mode.main.binding.aalt-j: Can't parse modifiers in 'aalt-j' binding",
   "mode.main.binding.alt-hh: Can't parse the key in 'alt-hh' binding"]
#guard (main "[mode.main.binding]\n  alt-hh = 'focus next'\n  aalt-j = 'focus next'\n  alt-k = 'focus prev'").map (·.desc) ==
  ["alt-k"]
#guard errs "[mode.main.binding]\n  alt-shift-j = 'focus next'\n  shift-alt-j = 'focus prev'" ==
  ["mode.main.binding.shift-alt-j: 'alt-shift-j' Binding redeclaration"]
#guard errs "[mode.main.binding]\n  alt-j = ['macos-native-fullscreen', 'focus next']" ==
  ["mode.main.binding.alt-j: macos-native-* commands are only allowed to be the last commands in the list"]
#guard errs "[mode.main.binding]\n  alt-j = 1" ==
  ["mode.main.binding.alt-j: Expected types are 'string' or 'array'. But actual type is 'integer'"]
#guard (cfg "[mode.main.binding]\n  alt-1 = 'workspace 1'\n  alt-2 = 'workspace 2'\n  alt-3 = ['workspace 3']\n  alt-4 = ['workspace 4', 'focus next']").persistent.mergeSort ==
  ["1", "2", "3", "4"]
-- mod
#guard main "mod = 'cmd-alt'\n[mode.main.binding]\n  mod-e = 'focus next'" ==
  [{ raw := "mod-e", keyCode := 14, modifiers := Keys.command ||| Keys.option, desc := "alt-cmd-e", commands := ["focus next"] }]
#guard (main "mod = 'cmd-alt-ctrl-shift'\n[mode.main.binding]\n  mod-e = 'focus next'").map (·.desc) == ["alt-ctrl-cmd-shift-e"]
#guard errs "[mode.main.binding]\n  mod-e = 'focus next'" == ["mode.main.binding.mod-e: Can't parse modifiers in 'mod-e' binding"]

-- Monitors
#guard let src := "[workspace-to-monitor-force-assignment]
    workspace_name_1 = 1
    workspace_name_2 = 'main'
    workspace_name_3 = 'secondary'
    workspace_name_4 = 'built-in'
    workspace_name_5 = '^built-in retina display$'
    workspace_name_6 = ['secondary', 1]
    7 = \"foo\"
    w7 = ['', 'main']
    w8 = 0
    workspace_name_x = '2'"
  (cfg src).forced == [("7", ["foo"]), ("w7", ["main"]), ("w8", []), ("workspace_name_1", ["1"]),
    ("workspace_name_2", ["main"]), ("workspace_name_3", ["secondary"]), ("workspace_name_4", ["built-in"]),
    ("workspace_name_5", ["^built-in retina display$"]), ("workspace_name_6", ["secondary", "1"]),
    ("workspace_name_x", ["2"])] &&
  errs src == ["workspace-to-monitor-force-assignment.w7[0]: Empty string is an illegal monitor description",
    "workspace-to-monitor-force-assignment.w8: Monitor sequence numbers uses 1-based indexing. Values less than 1 are illegal"]

-- Callbacks
#guard let src := "[[on-window-detected]] # 0
    check-further-callbacks = true
    run = ['layout floating', 'move-node-to-workspace W']
[[on-window-detected]] # 1
    if.app-id = 'com.apple.systempreferences'
    run = []
[[on-window-detected]] # 2
[[on-window-detected]] # 3
    run = ['move-node-to-workspace S', 'layout tiling']"
  (cfg src).detected == [{ checkFurther := true, run := ["layout floating", "move-node-to-workspace W"] },
    { appId := some "com.apple.systempreferences" }, { run := ["move-node-to-workspace S", "layout tiling"] }] &&
  errs src == ["on-window-detected[2]: 'run' is mandatory key"]
#guard errs "[[on-window-detected]]\n  if.app-name-regex-substring = '^system settings$'\n  run = []" == []
#guard (errs "[[on-window-detected]]\n  if.window-title-regex-substring = '('\n  run = []").length == 1

-- Gaps
#guard let src := "[gaps]
    inner.horizontal = 10
    inner.vertical = [{ monitor.\"main\" = 1 }, { monitor.\"secondary\" = 2 }, 5]
    outer.left = 12
    outer.bottom = 13
    outer.top = [{ monitor.\"built-in\" = 3 }, { monitor.\"secondary\" = 4 }, 6]
    outer.right = [{ monitor.2 = 7 }, 8]"
  errs src == [] &&
  (cfg src).gaps == [("inner.horizontal", { default := 10 }), ("inner.vertical", { default := 5, perMonitor := [("main", 1), ("secondary", 2)] }),
    ("outer.bottom", { default := 13 }), ("outer.left", { default := 12 }), ("outer.right", { default := 8, perMonitor := [("2", 7)] }),
    ("outer.top", { default := 6, perMonitor := [("built-in", 3), ("secondary", 4)] })]
#guard errs "[gaps]\n  inner.horizontal = [true]\n  inner.vertical = [{ foo.main = 1 }, { monitor = { foo = 2, bar = 3 } }, 1]" ==
  ["gaps.inner.horizontal: The last item in the array must be of type Int",
   "gaps.inner.vertical[0]: The table is expected to have a single key 'monitor'",
   "gaps.inner.vertical[1].monitor: The table is expected to have a single key"]

-- Key mapping
#guard let src := "[key-mapping.key-notation-to-key-code]\n  q = 'q'\n  unicorn = 'u'\n[mode.main.binding]\n  alt-unicorn = 'workspace wonderland'"
  errs src == [] && (main src).map (·.keyCode) == [32] && (cfg src).customKeys == [("q", 12), ("unicorn", 32)]
#guard errs "[key-mapping.key-notation-to-key-code]\n  q = 'qw'\n  ' f' = 'f'" ==
  ["key-mapping.key-notation-to-key-code: ' f' is invalid key notation",
   "key-mapping.key-notation-to-key-code.q: 'qw' is invalid key code"]
#guard errs "key-mapping.preset = 'dvorak'" == [] && (cfg "key-mapping.preset = 'dvorak'").keys.lookup "quote" == some 12
#guard (cfg "key-mapping.preset = 'colemak'").keys.lookup "f" == some 14

-- Env vars
def interp (s : String) (vars : List (String × String) := []) : Option String := (interpolate s vars).toOption
#guard interp "echo ${foo}" [("foo", "bar")] == some "echo bar"
#guard interp "echo $foo" == some "echo $foo"
#guard interp "echo $$foo" == some "echo $$foo"
#guard interp "echo $${foo}" [("foo", "bar")] == some "echo $bar"
#guard interp "echo $" == some "echo $"
#guard interp "echo ${foo" == none && interp "echo ${foo{bar}" == none && interp "echo ${foo$bar}" == none
#guard interp "echo ${foo}" == none
#guard errs "exec.inherit-env-vars = false" == [] && (cfg "exec.inherit-env-vars = false").env testEnv == []
#guard (cfg "exec.inherit-env-vars = true").env testEnv == testEnv
#guard (cfg "[exec.env-vars]\nFOO = 'BAR'").env testEnv == testEnv ++ [("FOO", "BAR")]
#guard (cfg "").env testEnv == [("DWMAC_INHERITED_TEST_ENV", "inherited"), ("PATH", "/opt/homebrew/bin:/opt/homebrew/sbin:DWMAC_TEST_PATH")]
#guard errs "[exec.env-vars]\nFOO = '${BAR}'\nBAR = '${FOO}'" ==
  ["exec.env-vars.BAR: Env variable 'FOO' isn't presented in Dwmac.app env vars, or not available for interpolation (because it's mutated)",
   "exec.env-vars.FOO: Env variable 'BAR' isn't presented in Dwmac.app env vars, or not available for interpolation (because it's mutated)"]
#guard errs "[exec.env-vars]\nPWD = ''" == ["exec.env-vars.PWD: Changing 'PWD' is not allowed"]

-- Options
#guard (cfg "attach-below = true").attachBelow && !(cfg "").attachBelow
#guard (cfg "center-floating-windows = true").centerFloating
#guard (cfg "default-root-container-layout = 'floating'").layout == "floating"
#guard errs "default-root-container-layout = 'tiles'" == ["default-root-container-layout: Can't parse layout 'tiles'"]
#guard (cfg "master-position = 'right'").masterPosition == "right"
#guard errs "master-position = 'up'" == ["master-position: Can't parse master position 'up'"]
#guard errs "[scratchpad.a]\nwidth = 0.5\n[scratchpad.b]\napp-id = 'x'\nheight = 2\n[scratchpad.c]\napp-id = 'x'" ==
  ["scratchpad.a: 'app-id' is mandatory key", "scratchpad.b.height: Must be in (0, 1] range"]
#guard errs "[scratchpad.b]\napp-id = 'x'\n[scratchpad.c]\napp-id = 'x'" == ["scratchpad.c: app-id 'x' is already owned by scratchpad 'b'"]
#guard (cfg "[scratchpad.k]\napp-id = 'x'\nwidth = 1").scratchpads == [{ name := "k", appId := "x", width := "1" }]

end DwmacCore.Config.Test
