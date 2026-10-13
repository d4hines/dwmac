/-!
# Window, dialog or popup

macOS reports many things as windows: real windows, dialogs, and popups that aren't windows to a human (tooltips,
context menus, the keyboard layout switcher). dwmac tiles windows, floats dialogs and ignores popups. Swift reads a
window's accessibility attributes into `Facts`; this decides. The rules are heuristics, accumulated app by app (each
comment names the windows a rule is for), and checked against AX dumps of real windows (test/axDumps).
-/

namespace DwmacCore.WindowKind

/-- A title bar button: missing, present but not (known to be) enabled, or enabled -/
inductive Button where
  | absent
  | disabled
  | enabled
deriving DecidableEq, Repr

/-- What the heuristics look at -/
structure Facts where
  /-- Bundle id, `""` if unknown -/
  app : String
  /-- The app's activation policy is `accessory` (no Dock icon) -/
  accessory : Bool
  /-- macOS window level: 0 for a normal window, `none` if unknown -/
  level : Option Int
  subrole : Option String
  /-- `""` if none -/
  title : String
  identifier : Option String
  close : Button
  fullscreen : Button
  zoom : Button
  minimize : Button
  focused : Option Bool
  main : Option Bool
  /-- The app reports this window as its focused one -/
  appFocused : Bool
deriving DecidableEq, Repr

inductive Kind where
  | window
  | dialog
  /-- Not even a real window -/
  | popup
deriving DecidableEq, Repr

def standard := "AXStandardWindow"

def isFirefox (app : String) : Bool :=
  ["org.mozilla.firefox", "org.mozilla.firefoxdeveloperedition", "org.mozilla.nightly", "app.zen-browser.zen"].contains app

def isVscode (app : String) : Bool := ["com.microsoft.VSCode", "com.vscodium"].contains app

/-- Apps whose real windows may lack an enabled fullscreen button -/
def noFullscreenButton : List String := [
  "org.gimp.gimp-2.10", -- Gimp doesn't show it
  -- Dragging a tab out of a Chrome window: Chrome enables the button only after the mouse is released
  "com.google.Chrome",
  "com.apple.ActivityMonitor",
  -- Terminals, browsers and editors that can hide their title bar
  "org.alacritty", "net.kovidgoyal.kitty", "com.github.wez.wezterm", "org.qutebrowser.qutebrowser",
  "com.googlecode.iterm2", "org.gnu.Emacs",
  "com.valvesoftware.steam.helper"]

/-- A dialog floats. Many windows have no title on startup, so the title isn't relied on -/
def isDialog (f : Facts) : Bool :=
  if f.app == "com.1password.1password" && f.level != some 0 then true
  else if f.app == "com.apple.iphonesimulator" then true
  -- Not a standard window: Chrome's find bar, the login screen, Xcode's "Build succeeded", IntelliJ's tooltips and
  -- menus, the native file picker. Minimized windows and windows of a hidden app are "AXDialog" too.
  -- qutebrowser's regular window is a dialog when its decorations are off
  else if f.subrole != some standard && f.app != "org.qutebrowser.qutebrowser" then true
  -- Firefox's Picture in Picture window has no minimize button
  else if isFirefox f.app && f.minimize != .enabled then true
  else if f.app == "com.apple.PhotoBooth" then true
  else if f.app == "com.mitchellh.ghostty" then f.fullscreen != .enabled && f.close == .enabled
  -- Windows without a (working) fullscreen button aren't designed to be big: IntelliJ's dialogs, Finder's copy
  -- dialog, System Settings, About this Mac, Calculator, login dialogs, screen recorders
  else f.fullscreen != .enabled && !noFullscreenButton.contains f.app && !isVscode f.app

/-- Before Firefox-specific rules (dwmac 0.18.3) -/
def isWindowOld (f : Facts) : Bool :=
  let noButtons := f.close == .absent && f.fullscreen == .absent && f.zoom == .absent && f.minimize == .absent
  let unfocused := f.focused == some false && f.main == some false && !f.appFocused
  -- Weird windows without buttons: the keyboard layout switcher (macOS 14), IntelliJ's and Telegram's context menus,
  -- the purple "pill" screen sharing indicator (titled "Window")
  if noButtons && unfocused && f.subrole != some standard && (f.title == "" || f.title == "Window") then false
  else f.subrole == some standard
    || f.subrole == some "AXDialog" -- the native file picker
    || f.subrole == some "AXFloatingWindow" -- Telegram's image viewer
    || (f.app == "com.apple.finder" && f.subrole == some "Quick Look")

/-- Whether it's a window at all, rather than a popup -/
def isWindow (f : Facts) : Bool :=
  -- Above normal windows: a popup. Rolled out only for apps with AX dumps
  if f.level != some 0 && (isFirefox f.app || ["com.tinyspeck.slackmacgap", "com.google.Chrome", "com.brave.Browser",
      "com.timpler.screenstudio", "pl.maketheweb.cleanshotx", "com.googlecode.iterm2"].contains f.app) then false
  -- Ghostty manages its Quick Terminal itself
  else if f.app == "com.mitchellh.ghostty" && f.identifier == some "com.mitchellh.ghostty.quickTerminal" then false
  else if f.app == "com.apple.dt.Xcode" && f.identifier == some "open_quickly" then false
  else if f.app == "com.googlecode.iterm2" && f.fullscreen == .absent then false
  else if f.accessory && f.close == .absent && f.app != "com.valvesoftware.steam.helper" then false
  else if !isFirefox f.app then isWindowOld f
  -- Firefox: anything with a button, focused, or standard. Its non-native video fullscreen is "AXUnknown"
  else f.close != .absent || f.fullscreen != .absent || f.zoom != .absent || f.minimize != .absent
    || f.focused == some true || f.main == some true || f.appFocused || f.subrole == some standard

def kind (f : Facts) : Kind :=
  if !isWindow f then .popup else if isDialog f then .dialog else .window

end DwmacCore.WindowKind
