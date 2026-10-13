/-!
# Keys

The keys a binding can name, by layout preset (`key-mapping.preset`), with their macOS virtual key codes, and the
modifiers. `dvorak` and `colemak` name keys by what they type on those layouts.
-/

namespace DwmacCore.Keys

/-- Key names on a QWERTY keyboard and their key codes -/
def qwerty : List (String × Nat) :=
  [("sectionSign", 10), ("0", 29), ("1", 18), ("2", 19), ("3", 20), ("4", 21), ("5", 23), ("6", 22), ("7", 26), ("8", 28), ("9", 25), ("minus", 27), ("equal", 24), ("q", 12), ("w", 13), ("e", 14), ("r", 15), ("t", 17), ("y", 16), ("u", 32), ("i", 34), ("o", 31), ("p", 35), ("leftSquareBracket", 33), ("rightSquareBracket", 30), ("backslash", 42), ("a", 0), ("s", 1), ("d", 2), ("f", 3), ("g", 5), ("h", 4), ("j", 38), ("k", 40), ("l", 37), ("semicolon", 41), ("quote", 39), ("z", 6), ("x", 7), ("c", 8), ("v", 9), ("b", 11), ("n", 45), ("m", 46), ("comma", 43), ("period", 47), ("slash", 44), ("keypad0", 82), ("keypad1", 83), ("keypad2", 84), ("keypad3", 85), ("keypad4", 86), ("keypad5", 87), ("keypad6", 88), ("keypad7", 89), ("keypad8", 91), ("keypad9", 92), ("keypadClear", 71), ("keypadDecimalMark", 65), ("keypadDivide", 75), ("keypadEnter", 76), ("keypadEqual", 81), ("keypadMinus", 78), ("keypadMultiply", 67), ("keypadPlus", 69), ("pageUp", 116), ("pageDown", 121), ("home", 115), ("end", 119), ("forwardDelete", 117), ("f1", 122), ("f2", 120), ("f3", 99), ("f4", 118), ("f5", 96), ("f6", 97), ("f7", 98), ("f8", 100), ("f9", 101), ("f10", 109), ("f11", 103), ("f12", 111), ("f13", 105), ("f14", 107), ("f15", 113), ("f16", 106), ("f17", 64), ("f18", 79), ("f19", 80), ("f20", 90), ("backtick", 50), ("space", 49), ("enter", 36), ("esc", 53), ("backspace", 51), ("tab", 48), ("left", 123), ("down", 125), ("up", 126), ("right", 124)]

def dvorakOverrides : List (String × Nat) :=
  [("leftSquareBracket", 27), ("rightSquareBracket", 24), ("quote", 12), ("comma", 13), ("period", 14), ("p", 15), ("y", 17), ("f", 16), ("g", 32), ("c", 34), ("r", 31), ("l", 35), ("slash", 33), ("equal", 30), ("backslash", 42), ("a", 0), ("o", 1), ("e", 2), ("u", 3), ("i", 5), ("d", 4), ("h", 38), ("t", 40), ("n", 37), ("s", 41), ("minus", 39), ("semicolon", 6), ("q", 7), ("j", 8), ("k", 9), ("x", 11), ("b", 45), ("m", 46), ("w", 43), ("v", 47), ("z", 44)]

def colemakOverrides : List (String × Nat) :=
  [("q", 12), ("w", 13), ("f", 14), ("p", 15), ("g", 17), ("j", 16), ("l", 32), ("u", 34), ("y", 31), ("semicolon", 35), ("leftSquareBracket", 33), ("rightSquareBracket", 30), ("backslash", 42), ("a", 0), ("r", 1), ("s", 2), ("t", 3), ("d", 5), ("h", 4), ("n", 38), ("e", 40), ("i", 37), ("o", 41), ("quote", 39), ("z", 6), ("x", 7), ("c", 8), ("v", 9), ("b", 11), ("k", 45), ("m", 46), ("comma", 43), ("period", 47), ("slash", 44)]

def presets := ["qwerty", "dvorak", "colemak"]

def preset (name : String) : List (String × Nat) :=
  let over := if name == "dvorak" then dvorakOverrides else if name == "colemak" then colemakOverrides else []
  over ++ qwerty.filter fun (k, _) => !over.any (·.1 == k)

/-- Each key code's name, as binding errors show it -/
def names : List (Nat × String) :=
  [(0, "a"), (11, "b"), (8, "c"), (2, "d"), (14, "e"), (3, "f"), (5, "g"), (4, "h"), (34, "i"), (38, "j"), (40, "k"), (37, "l"), (46, "m"), (45, "n"), (31, "o"), (35, "p"), (12, "q"), (15, "r"), (1, "s"), (17, "t"), (32, "u"), (9, "v"), (13, "w"), (7, "x"), (16, "y"), (6, "z"), (29, "0"), (18, "1"), (19, "2"), (20, "3"), (21, "4"), (23, "5"), (22, "6"), (26, "7"), (28, "8"), (25, "9"), (47, "period"), (39, "quote"), (33, "leftSquareBracket"), (30, "rightSquareBracket"), (41, "semicolon"), (44, "slash"), (42, "backslash"), (43, "comma"), (24, "equal"), (50, "backtick"), (27, "minus"), (49, "space"), (48, "tab"), (36, "enter"), (116, "pageUp"), (121, "pageDown"), (115, "home"), (119, "end"), (123, "left"), (125, "down"), (126, "up"), (124, "right"), (53, "esc"), (51, "backspace"), (10, "sectionSign"), (122, "f1"), (120, "f2"), (99, "f3"), (118, "f4"), (96, "f5"), (97, "f6"), (98, "f7"), (100, "f8"), (101, "f9"), (109, "f10"), (103, "f11"), (111, "f12"), (105, "f13"), (107, "f14"), (113, "f15"), (106, "f16"), (64, "f17"), (79, "f18"), (80, "f19"), (90, "f20"), (82, "keypad0"), (83, "keypad1"), (84, "keypad2"), (85, "keypad3"), (86, "keypad4"), (87, "keypad5"), (88, "keypad6"), (89, "keypad7"), (91, "keypad8"), (92, "keypad9"), (71, "keypadClear"), (65, "keypadDecimalMark"), (75, "keypadDivide"), (76, "keypadEnter"), (81, "keypadEqual"), (78, "keypadMinus"), (67, "keypadMultiply"), (69, "keypadPlus"), (55, "cmd"), (54, "rCmd"), (58, "alt"), (61, "rAlt"), (59, "ctrl"), (62, "rCtrl"), (56, "shift"), (60, "rShift"), (63, "function"), (57, "capsLock"), (117, "forwardDelete"), (114, "help"), (72, "volumeUp"), (73, "volumeDown"), (74, "mute")]

/-! ## Modifiers, as `NSEvent.ModifierFlags` -/

def shift : Nat := 0x20000
def control : Nat := 0x40000
def option : Nat := 0x80000
def command : Nat := 0x100000
def lControl : Nat := 0x01
def lShift : Nat := 0x02
def rShift : Nat := 0x04
def lCommand : Nat := 0x08
def rCommand : Nat := 0x10
def lOption : Nat := 0x20
def rOption : Nat := 0x40
def rControl : Nat := 0x2000

def modifierFlags : String → Option Nat
  | "shift" => some shift
  | "alt" => some option
  | "ctrl" => some control
  | "cmd" => some command
  | "lshift" => some (shift ||| lShift)
  | "rshift" => some (shift ||| rShift)
  | "lalt" => some (option ||| lOption)
  | "ralt" => some (option ||| rOption)
  | "lctrl" => some (control ||| lControl)
  | "rctrl" => some (control ||| rControl)
  | "lcmd" => some (command ||| lCommand)
  | "rcmd" => some (command ||| rCommand)
  | _ => none

def has (flags bit : Nat) : Bool := flags &&& bit == bit

/-- `alt-ctrl-cmd-shift` order, with the side when there is one -/
def modifiersName (f : Nat) : String :=
  let one (l r both : Nat) (name : String) : List String :=
    if has f l then ["l" ++ name] else if has f r then ["r" ++ name] else if has f both then [name] else []
  "-".intercalate (one lOption rOption option "alt" ++ one lControl rControl control "ctrl" ++
    one lCommand rCommand command "cmd" ++ one lShift rShift shift "shift")

/-- A key binding: the key and modifiers that trigger it (macOS key code and `NSEvent.ModifierFlags`), and the
commands it runs -/
structure Binding where
  /-- As written in the config, e.g. `alt-shift-j` -/
  raw : String
  keyCode : Nat
  modifiers : Nat
  /-- The modifiers and key, normalized: `alt-shift-j` and `shift-alt-j` are the same binding -/
  desc : String
  commands : List String
deriving Repr, BEq, Inhabited

/-! ## The keymap -/

/-- The bindings of every mode, and the active mode: `none` while dwmac is disabled -/
structure Keymap where
  modes : List (String × List Binding) := []
  mode : Option String := some "main"
deriving Repr, Inhabited

def bits (n : Nat) : Nat := (Nat.toDigits 2 n).count '1'

/-- Whether `b` is triggered by key `code` with modifiers `flags` held: extra modifiers are allowed -/
def Binding.triggeredBy (b : Binding) (code flags : Nat) : Bool := b.keyCode == code && has flags b.modifiers

/-- Of the bindings `code` with `flags` triggers, the one with the most modifiers (the first, on a tie) -/
def best (bs : List Binding) (code flags : Nat) : Option Binding :=
  (bs.filter (·.triggeredBy code flags)).foldl
    (fun acc b => match acc with
      | some a => if bits b.modifiers > bits a.modifiers then some b else some a
      | none => some b) none

/-- The binding a key press triggers in the active mode -/
def Keymap.lookup (k : Keymap) (code flags : Nat) : Option Binding :=
  (k.mode.bind fun m => k.modes.lookup m).bind (best · code flags)

end DwmacCore.Keys
