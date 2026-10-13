import DwmacCore.StateFfi

/-! Key presses, from the config's text to the commands a key runs -/

namespace DwmacCore.Model.KeymapTest
open State

def cfg := "[mode.main.binding]
alt-j = 'focus next'
alt-shift-j = 'rotate'
alt-semicolon = 'mode service'
lcmd-a = ['workspace 1', 'focus next']
[mode.service.binding]
esc = ['reload-config', 'mode main']"

/-- The keymap lines Swift forwards: the config's `mode` and `binding` lines -/
def keymapText : String :=
  match Config.parse [] cfg with
  | .ok (c, _) => "\x1f".intercalate ((c.render []).filter fun l => l.startsWith "mode\t" || l.startsWith "binding\t")
  | .error _ => ""

def go (inputs : List String) : State × Reply :=
  inputs.foldl (fun (s, _) i => s.execText i) (init [], {})
def reply (inputs : List String) : List String := (go (s!"keymap\t{keymapText}" :: inputs)).2.out

def alt := Keys.option
def shift := Keys.shift
-- j is key code 38, a is 0, escape 53
#guard reply [s!"key\t38\t{alt}"] == ["alt-j", "focus next"]
-- Extra modifiers: the binding with the most of them wins
#guard reply [s!"key\t38\t{alt ||| shift}"] == ["alt-shift-j", "rotate"]
#guard reply [s!"key\t38\t{alt ||| shift ||| Keys.control}"] == ["alt-shift-j", "rotate"]
#guard reply [s!"key\t38\t{shift}"] == []
#guard reply [s!"key\t38\t0"] == []
-- Left and right modifiers
#guard reply [s!"key\t0\t{Keys.command ||| Keys.lCommand}"] == ["lcmd-a", "workspace 1", "focus next"]
#guard reply [s!"key\t0\t{Keys.command ||| Keys.rCommand}"] == []
-- Modes
#guard reply ["setMode\tservice"] == ["changed"]
#guard reply ["setMode\tmain"] == []
#guard reply ["setMode\tservice", s!"key\t38\t{alt}"] == []
#guard reply ["setMode\tservice", "key\t53\t0"] == ["esc", "reload-config", "mode main"]
-- Disabled: no mode, nothing triggers
#guard reply ["setMode\t", s!"key\t38\t{alt}"] == []
-- A new config keeps the active mode
#guard reply ["setMode\tservice", s!"keymap\t{keymapText}", "key\t53\t0"] == ["esc", "reload-config", "mode main"]
-- trigger-binding
#guard reply ["trigger\tmain\talt-shift-j"] == ["rotate"]
#guard (go [s!"keymap\t{keymapText}", "trigger\tmain\tnope"]).2.err == ["Binding 'nope' is not presented in mode 'main'"]
#guard (go [s!"keymap\t{keymapText}", "trigger\tfoo\tx"]).2.err == ["Mode 'foo' doesn't exist. Available modes: main,service"]

end DwmacCore.Model.KeymapTest
