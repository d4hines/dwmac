import DwmacCore.WindowKind

/-! Window, dialog or popup (the AX dumps in test/axDumps check the same rules through Swift) -/

namespace DwmacCore.WindowKind.Test

/-- A standard window with all four title bar buttons enabled -/
def std (app : String) : Facts :=
  { app, accessory := false, level := some 0, subrole := some "AXStandardWindow", title := "t", identifier := none,
    close := .enabled, fullscreen := .enabled, zoom := .enabled, minimize := .enabled,
    focused := some true, main := some true, appFocused := true }

#guard kind (std "com.apple.Safari") == .window
-- No fullscreen button: a dialog (About this Mac, Calculator), unless the app can hide its title bar
#guard kind { std "com.apple.calculator" with fullscreen := .absent } == .dialog
#guard kind { std "net.kovidgoyal.kitty" with fullscreen := .absent } == .window
-- Not a standard window: a dialog (the file picker)
#guard kind { std "com.apple.TextEdit" with subrole := some "AXDialog" } == .dialog
-- No buttons, unfocused, untitled: a popup (the keyboard layout switcher)
def switcher : Facts :=
  { std "com.apple.TextInputSwitcher" with
    subrole := some "AXDialog", title := "", close := .absent, fullscreen := .absent, zoom := .absent,
    minimize := .absent, focused := some false, main := some false, appFocused := false }
#guard kind switcher == .popup
-- Above normal windows in Chrome: a popup (tooltips)
#guard kind { std "com.google.Chrome" with level := some 3 } == .popup

end DwmacCore.WindowKind.Test
