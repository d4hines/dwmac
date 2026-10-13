import DwmacCore.Regex

/-!
# The command line

Every command, from the CLI, a key binding or a callback, is parsed here into its kind and its fields: flags and
arguments by name, with the values given. The parser is a loop over the arguments with a spec per command (its
flags, its positional arguments, which options conflict), then checks across fields. Errors are reported the way
dwmac always has, one `ERROR:` line each, and `-h`/`--help` gives the command's usage.
-/

namespace DwmacCore.Cli

/-- A field's value -/
inductive V where
  | bool (b : Bool)
  | str (s : String)
  | strs (l : List String)
deriving Repr, BEq, Inhabited

/-- Fields set while parsing, most recent first. A field that's absent has its default -/
abbrev Fields := List (String × V)

namespace Fields
def get? (f : Fields) (k : String) : Option V := (f.find? (·.1 == k)).map (·.2)
def set (f : Fields) (k : String) (v : V) : Fields := (k, v) :: f.filter (·.1 != k)
def erase (f : Fields) (k : String) : Fields := f.filter (·.1 != k)
def has (f : Fields) (k : String) : Bool := (f.get? k).isSome
def bool (f : Fields) (k : String) : Bool := match f.get? k with | some (.bool b) => b | _ => false
def str? (f : Fields) (k : String) : Option String := match f.get? k with | some (.str s) => some s | _ => none
def strs (f : Fields) (k : String) : List String := match f.get? k with | some (.strs l) => l | _ => []
end Fields

inductive Parsed where
  | cmd (kind : String) (fields : Fields)
  | help (text : String)
  | fail (msg : String)
deriving Repr, BEq, Inhabited

/-- Each error's first line gets `ERROR: `, the rest are indented under it -/
def joinErrors (errors : List String) : String :=
  "\n".intercalate (errors.map fun e =>
    "\n".intercalate ((e.splitOn "\n").zipIdx.map fun (line, i) => (if i == 0 then "ERROR: " else "       ") ++ line))

def quoted (s : String) : String := "'" ++ s ++ "'"
def union (values : List String) : String := "(" ++ "|".intercalate values ++ ")"

/-! ## Values -/

def enumValue (values : List String) (raw : String) : Except String String :=
  if values.contains raw then .ok raw else .error s!"Can't parse '{raw}'.\nPossible values: {union values}"

def nextPrev := ["next", "prev"]
def directions := ["left", "down", "up", "right"]

def reservedWorkspaceNames := ["focused", "non-focused", "visible", "invisible", "non-visible", "active", "non-active",
  "inactive", "back-and-forth", "back_and_forth", "previous", "prev", "next", "monitor", "workspace", "monitors",
  "workspaces", "all", "none", "mouse", "target"]

def isWhitespace (c : Char) : Bool := c.isWhitespace || c.val == 0x85 || c.val == 0xA0 || c.val == 0x2028

def workspaceName (raw : String) : Except String String :=
  if reservedWorkspaceNames.contains raw then .error s!"'{raw}' is a reserved workspace name"
  else if raw.isEmpty then .error "Empty workspace name is forbidden"
  else if raw.contains ',' then .error "Workspace names are not allowed to contain comma"
  else if raw.startsWith "_" then .error "Workspace names starting with underscore are reserved for future use"
  else if raw.startsWith "-" then .error "Workspace names starting with dash are disallowed"
  else if raw.any isWhitespace then .error "Whitespace characters are forbidden in workspace names"
  else .ok raw

/-- What Swift's `UInt32(_:)` accepts: digits, with an optional `+` -/
def parseUInt32 (s : String) : Option Nat :=
  let digits := if s.startsWith "+" then (s.drop 1).toString else s
  if digits.isEmpty || !digits.all Char.isDigit then none
  else digits.toNat?.filter (· < 2 ^ 32)

def parseInt32 (s : String) : Option Int :=
  let (neg, digits) :=
    if s.startsWith "-" then (true, (s.drop 1).toString)
    else if s.startsWith "+" then (false, (s.drop 1).toString) else (false, s)
  if digits.isEmpty || !digits.all Char.isDigit then none
  else (digits.toNat?.map fun n => if neg then -(n : Int) else n).filter fun i => -2 ^ 31 ≤ i && i < 2 ^ 31

/-- What Swift's `Int(_:)` accepts -/
def parseInt (s : String) : Option Int :=
  let (neg, digits) :=
    if s.startsWith "-" then (true, (s.drop 1).toString)
    else if s.startsWith "+" then (false, (s.drop 1).toString) else (false, s)
  if digits.isEmpty || !digits.all Char.isDigit then none
  else digits.toNat?.map fun n => if neg then -(n : Int) else n

/-- What Swift's `Double(_:)` accepts: a decimal (`1`, `.5`, `2.`, `1e-3`), `inf`/`infinity`/`nan`, or a hex float;
with an optional sign -/
def isDouble (s : String) : Bool :=
  let body := if s.startsWith "-" || s.startsWith "+" then (s.drop 1).toString else s
  let lower := body.toLower
  if ["inf", "infinity", "nan"].contains lower then true
  else if lower.startsWith "0x" then
    let rest := (lower.drop 2).toString
    let (mant, exp) := match rest.splitOn "p" with
      | [m] => (m, none)
      | [m, e] => (m, some e)
      | _ => ("", none)
    let hexDigit (c : Char) := c.isDigit || ('a' ≤ c && c ≤ 'f')
    let mantOk := match mant.splitOn "." with
      | [i] => !i.isEmpty && i.all hexDigit
      | [i, f] => !(i.isEmpty && f.isEmpty) && i.all hexDigit && f.all hexDigit
      | _ => false
    let expOk := exp.all fun e =>
      let e := if e.startsWith "-" || e.startsWith "+" then (e.drop 1).toString else e
      !e.isEmpty && e.all Char.isDigit
    mantOk && expOk
  else
    let (mant, exp) := match lower.splitOn "e" with
      | [m] => (m, none)
      | [m, e] => (m, some e)
      | _ => ("", none)
    let mantOk := match mant.splitOn "." with
      | [i] => !i.isEmpty && i.all Char.isDigit
      | [i, f] => !(i.isEmpty && f.isEmpty) && i.all Char.isDigit && f.all Char.isDigit
      | _ => false
    let expOk := exp.all fun e =>
      let e := if e.startsWith "-" || e.startsWith "+" then (e.drop 1).toString else e
      !e.isEmpty && e.all Char.isDigit
    mantOk && expOk

/-- `x * 10^places`, rounded half away from zero, if `x` is a decimal number (`0.5`, `-.25`, `1e-1`) -/
def scaled (x : String) (places : Nat) : Option Int := do
  let (neg, body) :=
    if x.startsWith "-" then (true, (x.drop 1).toString)
    else if x.startsWith "+" then (false, (x.drop 1).toString) else (false, x)
  let (mant, exp) ← match (body.toLower.splitOn "e") with
    | [m] => some (m, 0)
    | [m, e] => (Cli.parseInt e).map (m, ·)
    | _ => none
  let (intPart, frac) ← match mant.splitOn "." with
    | [i] => some (i, "")
    | [i, f] => some (i, f)
    | _ => none
  if (intPart.isEmpty && frac.isEmpty) || !intPart.all Char.isDigit || !frac.all Char.isDigit then none
  -- x = digits / 10^frac.length * 10^exp, so x * 10^places = digits * 10^(exp + places) / 10^frac.length
  let digits := (intPart ++ frac).toNat?.getD 0
  let shift : Int := exp + places - frac.length
  let (num, den) : Nat × Nat := if shift ≥ 0 then (digits * 10 ^ shift.toNat, 1) else (digits, 10 ^ (-shift).toNat)
  let rounded := (2 * num + den) / (2 * den)
  return if neg then -(rounded : Int) else rounded

/-- `<monitor-pattern>`: a 1-based sequence number, `main`, `secondary`, or a regex matched against monitor names -/
def monitorDescription (raw : String) : Except String String :=
  match parseInt raw with
  | some n => if n ≥ 1 then .ok raw else .error "Monitor sequence numbers uses 1-based indexing. Values less than 1 are illegal"
  | none =>
    if raw == "main" || raw == "secondary" then .ok raw
    else if raw.isEmpty then .error "Empty string is an illegal monitor description"
    else match Regex.parse raw with
      | .ok _ => .ok raw
      | .error e => .error s!"Can't parse '{raw}' regex. {e}"

/-! ## Format strings (`--format`) -/

/-- `%{var}` interpolation: `v:<var>` and `l:<literal>` tokens, or why the format doesn't parse -/
def formatTokens (s : String) : Except String (List String) := do
  -- Modes: literal text, after `%`, inside `%{...}`
  let rec go : List Char → Nat → String → String → List String → Except String (List String)
    | [], 0, lit, _, acc => .ok (acc ++ ["l:" ++ lit])
    | [], 1, lit, _, acc => .ok (acc ++ ["l:" ++ lit ++ "%"])
    | [], _, _, _, _ => .error "Unbalanced curly braces"
    | c :: cs, 0, lit, v, acc => if c == '%' then go cs 1 lit v acc else go cs 0 (lit.push c) v acc
    | c :: cs, 1, lit, _, acc =>
      if c == '{' then go cs 2 "" "" (acc ++ ["l:" ++ lit])
      else if c == '%' then go cs 1 (lit.push '%') "" acc
      else go cs 0 ((lit.push '%').push c) "" acc
    | c :: cs, _, lit, v, acc =>
      if c == '}' then go cs 0 lit "" (acc ++ ["v:" ++ v])
      else if c == '{' then .error s!"Can't parse '{v}\{' inside interpolation (Open curly brace is invalid character)"
      else if c == '%' then .error s!"Can't parse '{v}%' inside interpolation ('%' is disallowed character)"
      else go cs 2 lit (v.push c) acc
  let tokens ← go s.toList 0 "" "" []
  return tokens.filter (· != "l:")

def windowVars := ["window-id", "window-is-fullscreen", "window-title", "window-layout", "window-parent-container-layout"]
def workspaceVars := ["workspace", "workspace-is-focused", "workspace-is-visible", "workspace-root-container-layout"]
def appVars := ["app-bundle-id", "app-name", "app-pid", "app-exec-path", "app-bundle-path"]
def monitorVars := ["monitor-id", "monitor-appkit-nsscreen-screens-id", "monitor-name", "monitor-is-main"]
def plainVars := ["right-padding", "newline", "tab"]

/-- The `%{...}` variables of an object kind -/
def formatVars : String → List String
  | "app" => appVars
  | "monitor" => monitorVars
  | "workspace" => workspaceVars ++ monitorVars
  | _ => windowVars ++ workspaceVars ++ monitorVars ++ appVars

def jsonFormatError (tokens : List String) : Option String :=
  tokens.findSome? fun t =>
    if t == "v:right-padding" then some "%{right-padding} interpolation variable is not allowed when --json is used"
    else if t.startsWith "l:" && (t.drop 2).any (· != ' ') then
      some "Only interpolation variables and spaces are allowed in '--format' when '--json' is used"
    else none

/-! ## Parsers -/

/-- A flag's or argument's result: the new fields, or an error, and how many more arguments it consumed -/
structure Step where
  result : Except String Fields
  advance : Nat

/-- After the flag `super` at `args[i - 1]` -/
abbrev Flag := (super : String) → (args : Array String) → (i : Nat) → Fields → Step
/-- At `args[i]` -/
abbrev Pos := (args : Array String) → (i : Nat) → Fields → Step

structure PosArg where
  parse : Pos
  /-- The placeholder, if the argument is mandatory -/
  mandatory : Option String := none

def mand (parse : Pos) (placeholder : String) : PosArg := { parse, mandatory := some placeholder }
def opt (parse : Pos) : PosArg := { parse }

def nonFlagArg? (args : Array String) (i : Nat) : Option String := args[i]?.filter (!·.startsWith "-")
def nonFlagArgs (args : Array String) (i : Nat) : List String :=
  ((args.toList.drop i).takeWhile (!·.startsWith "-"))

def Step.ok (f : Fields) (advance : Nat) : Step := ⟨.ok f, advance⟩
def Step.err (msg : String) (advance : Nat) : Step := ⟨.error msg, advance⟩

def trueFlag (k : String) : Flag := fun _ _ _ f => .ok (f.set k (.bool true)) 0
def falseFlag (k : String) : Flag := fun _ _ _ f => .ok (f.set k (.bool false)) 0
/-- `--flag` or `--flag no` -/
def boolFlag (k : String) : Flag := fun _ args i f =>
  if args[i]? == some "no" then .ok (f.set k (.bool false)) 1 else .ok (f.set k (.bool true)) 0

def uint32Flag (k : String) : Flag := fun super args i f =>
  match nonFlagArg? args i with
  | some arg => match parseUInt32 arg with
    | some _ => .ok (f.set k (.str arg)) 1
    | none => .err s!"Can't parse '{arg}'. It must be a positive number" 1
  | none => .err s!"'{super}' must be followed by mandatory UInt32" 0

def windowIdFlag := uint32Flag "windowId"

def workspaceFlag : Flag := fun super args i f =>
  match nonFlagArg? args i with
  | some arg => ⟨(workspaceName arg).map fun ws => f.set "workspaceName" (.str ws), 1⟩
  | none => .err s!"'{super}' must be followed by mandatory workspace name" 0

def valueFlag (k placeholder : String) (check : String → Option String := fun _ => none) : Flag := fun _ args i f =>
  match nonFlagArg? args i with
  | some arg => match check arg with
    | none => .ok (f.set k (.str arg)) 1
    | some type => .err s!"Failed to convert '{arg}' to '{type}'" 1
  | none => .err s!"'{placeholder}' is mandatory" 0

def enumFlag (k : String) (values : List String) (placeholder : String) : Flag := fun _ args i f =>
  match nonFlagArg? args i with
  | some arg => ⟨(enumValue values arg).map fun v => f.set k (.str v), 1⟩
  | none => .err s!"{placeholder} is mandatory" 0

def formatFlag (kind : String) : Flag := fun _ args i f =>
  match nonFlagArg? args i with
  | some arg => match formatTokens arg with
    | .ok tokens => .ok (f.set "_format" (.strs tokens)) 1
    | .error e => .err s!"Failed to parse <output-format>. {e}" 1
  | none =>
    let values := "\n".intercalate ((formatVars kind ++ plainVars).map ("  " ++ ·))
    .err s!"<output-format> is mandatory. Possible values:\n{values}" 0

def monitorIdsFlag (k : String) : Flag := fun _ args i f =>
  let ids := nonFlagArgs args i
  let possible := "<monitor> possible values: (<monitor-id>|focused|mouse|all)"
  if ids.isEmpty then .err s!"<monitor>... is mandatory. {possible}" 0
  else match ids.zipIdx.find? fun (m, _) => (parseInt m).isNone && !["mouse", "all", "focused"].contains m with
    | some (m, n) => .err s!"Can't parse monitor ID '{m}'. {possible}" (n + 1)
    | none => .ok (f.set k (.strs ids)) ids.length

def workspacesFlag : Flag := fun _ args i f =>
  let names := nonFlagArgs args i
  let possible := "<workspace> possible values: (<workspace-name>|focused|visible)"
  if names.isEmpty then .err s!"<workspace>... is mandatory. {possible}" 0
  else match names.zipIdx.findSome? fun (w, n) =>
      if w == "visible" || w == "focused" then none
      else match workspaceName w with | .ok _ => none | .error e => some (e, n) with
    | some (e, n) => .err e (n + 1)
    | none => .ok (f.set "workspaces" (.strs names)) names.length

def enumArg (k : String) (values : List String) : Pos := fun args i f =>
  ⟨(enumValue values args[i]!).map fun v => f.set k (.str v), 1⟩

def strArg (k : String) : Pos := fun args i f => .ok (f.set k (.str args[i]!)) 1

def workspaceTargetArg : Pos := fun args i f =>
  let arg := args[i]!
  if nextPrev.contains arg then .ok (f.set "target" (.str arg)) 1
  else ⟨(workspaceName arg).map fun ws => f.set "target" (.str ("=" ++ ws)), 1⟩

def monitorTargetArg : Pos := fun args i f =>
  let arg := args[i]!
  if nextPrev.contains arg || directions.contains arg then .ok (f.set "target" (.str arg)) 1
  else
    let patterns := nonFlagArgs args i
    match patterns.findSome? fun p => match monitorDescription p with | .ok _ => none | .error e => some e with
    | some e => .err e patterns.length
    | none => .ok (f.set "patterns" (.strs patterns)) patterns.length

def toggleArg : Pos := fun args i f =>
  let arg := args[i]!
  if arg == "on" || arg == "off" then .ok (f.set "toggle" (.str arg)) 1
  else .err s!"Can't parse '{arg}'. Possible values: on|off" 1

/-- `[+|-]<number>` -/
def amountArg (k : String) : Pos := fun args i f =>
  let arg := args[i]!
  -- Like Swift: drop one `+`, then one `-`, and parse the rest as a Double
  let body := if arg.startsWith "+" then (arg.drop 1).toString else arg
  let body := if body.startsWith "-" then (body.drop 1).toString else body
  if isDouble body then .ok (f.set k (.str arg)) 1 else .err "<number> argument must be a number" 1

def layouts := ["master-stack", "horizontal", "vertical", "h_master-stack", "v_master-stack", "tiling", "floating"]

def layoutArgs : Pos := fun args i f =>
  let values := nonFlagArgs args i
  match values.zipIdx.find? fun (v, _) => !layouts.contains v with
  | some (v, n) => .err s!"Can't parse '{v}'\nPossible values: {union layouts}" (n + 1)
  | none => .ok (f.set "toggleBetween" (.strs values)) values.length

def volumeArg : Pos := fun args i f =>
  match args[i]! with
  | "up" | "down" | "mute-toggle" | "mute-off" | "mute-on" => .ok (f.set "action" (.str args[i]!)) 1
  | "set" => match args[i + 1]? with
    | none => .err "set argument must be followed by <number>" 1
    | some n => match parseInt n with
      | none => .err s!"Can't parse number '{n}'" 2
      | some v =>
        if v < 0 || v > 100 then .err s!"{v} must be in range from 0 to 100" 2
        else .ok (f.set "action" (.str s!"set {v}")) 2
  | arg => .err s!"Unknown argument '{arg}'. Possible values: (up|down|mute-toggle|mute-on|mute-off|set)" 1

/-! ## Commands -/

structure Spec where
  kind : String
  help : String
  /-- Query commands can't be used in the config -/
  allowInConfig : Bool := true
  flags : List (String × Flag) := []
  pos : List PosArg := []
  conflicts : List (List String) := []
  /-- Checks and normalization across fields -/
  post : Fields → Except String Fields := pure
  /-- `-5` is a number, not a flag -/
  negativeNumbers : Bool := false

def check (ok : Bool) (msg : String) : Except String Unit := if ok then pure () else throw msg

def windowIdHelp := "[--window-id <window-id>]"

def help (lines : List String) : String := "\n".intercalate lines

def monitorTargetPlaceholder := union (directions ++ nextPrev ++ ["<monitor-pattern>"])
def isRelativeTarget (f : Fields) : Bool := nextPrev.contains ((f.str? "target").getD "")

def jsonCheck (f : Fields) : Except String Fields :=
  match f.bool "json", jsonFormatError (f.strs "_format") with
  | true, some msg => throw msg
  | _, _ => pure f

/-- The specs, by command name -/
def specs : List Spec := [
  { kind := "close", help := "USAGE: close [-h|--help] [--quit-if-last-window] [--window-id <window-id>]",
    flags := [("--quit-if-last-window", trueFlag "quitIfLastWindow"), ("--window-id", windowIdFlag)] },
  { kind := "close-all-windows-but-current", help := "USAGE: close-all-windows-but-current [-h|--help] [--quit-if-last-window]",
    flags := [("--quit-if-last-window", trueFlag "quitIfLastWindow")] },
  { kind := "config", allowInConfig := false,
    help := help ["USAGE: config [-h|--help] --get <name> [--json] [--keys]", "   OR: config [-h|--help] --major-keys",
      "   OR: config [-h|--help] --all-keys", "   OR: config [-h|--help] --config-path"],
    flags := [("--json", trueFlag "json"), ("--keys", trueFlag "keys"), ("--major-keys", trueFlag "majorKeys"),
      ("--all-keys", trueFlag "allKeys"), ("--config-path", trueFlag "configPath"),
      ("--get", valueFlag "keyNameToGet" "<name>")],
    post := fun f => do
      -- In the order of Swift's Set: not deterministic there, so only the count matters for tests
      let modes := [("--get", f.has "keyNameToGet"), ("--major-keys", f.bool "majorKeys"),
        ("--all-keys", f.bool "allKeys"), ("--config-path", f.bool "configPath")].filter (·.2) |>.map (·.1)
      match modes.length with
      | 0 => throw "Mandatory flag is not specified (--get|--major-keys|--all-keys|--config-path)"
      | 1 => pure ()
      | _ => throw s!"Conflicting flags are specified: {", ".intercalate modes}"
      check (!f.bool "keys" || f.has "keyNameToGet") "--keys flag requires --get flag"
      check (!f.bool "json" || f.has "keyNameToGet") "--json flag requires --get flag"
      return f },
  { kind := "debug-windows", allowInConfig := false, help := "USAGE: debug-windows [-h|--help] [--window-id <window-id>]",
    flags := [("--window-id", windowIdFlag)] },
  { kind := "enable",
    help := help ["USAGE: enable [-h|--help] toggle", "   OR: enable [-h|--help] on [--fail-if-noop]",
      "   OR: enable [-h|--help] off [--fail-if-noop]"],
    flags := [("--fail-if-noop", trueFlag "failIfNoop")],
    pos := [mand (enumArg "targetState" ["on", "off", "toggle"]) "(on|off|toggle)"],
    post := fun f => do
      check (!(f.str? "targetState" == some "toggle" && f.bool "failIfNoop")) "--fail-if-noop is incompatible with 'toggle' argument"
      return f },
  { kind := "focus",
    help := help ["USAGE: focus [-h|--help] [--ignore-floating]",
      "             [--boundaries <boundary>] [--boundaries-action <action>]", "             (next|prev)",
      "   OR: focus [-h|--help] --window-id <window-id>", "   OR: focus [-h|--help] --index <index>"],
    flags := [("--ignore-floating", falseFlag "floatingAsTiling"),
      ("--boundaries", enumFlag "rawBoundaries" ["workspace", "all-monitors-outer-frame"] "<boundary>"),
      ("--boundaries-action", enumFlag "rawBoundariesAction"
        ["stop", "fail", "wrap-around-the-workspace", "wrap-around-all-monitors"] "<action>"),
      ("--window-id", windowIdFlag), ("--index", uint32Flag "index")],
    pos := [opt (enumArg "nextPrev" nextPrev)],
    post := fun f => do
      let boundaries := (f.str? "rawBoundaries").getD "workspace"
      let action := (f.str? "rawBoundariesAction").getD "stop"
      check (!(boundaries == "workspace" && action == "wrap-around-all-monitors"))
        s!"{boundaries} and {action} is an invalid combination of values"
      check (f.has "nextPrev" || f.has "windowId" || f.has "index")
        "Mandatory argument is missing. (next|prev), --window-id or --index is required"
      check (!f.has "windowId" || f.all (·.1 == "windowId")) "--window-id is incompatible with other options"
      check (!f.has "index" || f.all (·.1 == "index")) "--index is incompatible with other options"
      check (!f.has "nextPrev" || boundaries == "workspace") "(next|prev) only supports --boundaries workspace"
      return f },
  { kind := "focus-back-and-forth", help := "USAGE: focus-back-and-forth [-h|--help]" },
  { kind := "focus-master-or-back",
    help := "USAGE: focus-master-or-back\n\nSwitches focus between the master window and the previously focused window within the same workspace." },
  { kind := "focus-monitor",
    help := help ["USAGE: focus-monitor [-h|--help] [--wrap-around] (left|down|up|right)",
      "   OR: focus-monitor [-h|--help] [--wrap-around] (next|prev)", "   OR: focus-monitor [-h|--help] <monitor-pattern>..."],
    flags := [("--wrap-around", trueFlag "wrapAround")],
    pos := [mand (monitorTargetArg) monitorTargetPlaceholder],
    post := fun f => do
      check (!f.bool "wrapAround" || !f.has "patterns") "--wrap-around is incompatible with <monitor-pattern> argument"
      return f },
  { kind := "fullscreen",
    help := help ["USAGE: fullscreen [-h|--help]     [--window-id <window-id>] [--no-outer-gaps]",
      "   OR: fullscreen [-h|--help] on  [--window-id <window-id>] [--no-outer-gaps] [--fail-if-noop]",
      "   OR: fullscreen [-h|--help] off [--window-id <window-id>] [--fail-if-noop]"],
    flags := [("--no-outer-gaps", trueFlag "noOuterGaps"), ("--fail-if-noop", trueFlag "failIfNoop"), ("--window-id", windowIdFlag)],
    pos := [opt (toggleArg)],
    post := fun f => do
      check (!(f.str? "toggle" == some "off" && f.bool "noOuterGaps")) "--no-outer-gaps is incompatible with 'off' argument"
      check (!f.bool "failIfNoop" || f.has "toggle") "--fail-if-noop requires 'on' or 'off' argument"
      return f },
  { kind := "layout",
    help := help ["USAGE: layout [-h|--help] [--window-id <window-id>]",
      "              (h_master-stack|v_master-stack|master-stack|horizontal|vertical|tiling|floating)..."],
    flags := [("--window-id", windowIdFlag)],
    pos := [mand (layoutArgs) (union layouts)] },
  { kind := "list-apps", allowInConfig := false,
    help := "USAGE: list-apps [-h|--help] [--macos-native-hidden [no]] [--format <output-format>] [--count] [--json]",
    flags := [("--macos-native-hidden", boolFlag "macosHidden"), ("--format", formatFlag "app"),
      ("--count", trueFlag "outputOnlyCount"), ("--json", trueFlag "json")],
    conflicts := [["--count", "--format"], ["--count", "--json"]], post := jsonCheck },
  { kind := "list-exec-env-vars", help := "USAGE: list-exec-env-vars [-h|--help]" },
  { kind := "list-modes", allowInConfig := false, help := "USAGE: list-modes [-h|--help] [--current] [--count] [--json]",
    flags := [("--count", trueFlag "outputOnlyCount"), ("--current", trueFlag "current"), ("--json", trueFlag "json")],
    conflicts := [["--count", "--current"], ["--count", "--json"]] },
  { kind := "list-monitors", allowInConfig := false,
    help := "USAGE: list-monitors [-h|--help] [--focused [no]] [--mouse [no]] [--format <output-format>] [--count] [--json]",
    flags := [("--focused", boolFlag "focused"), ("--mouse", boolFlag "mouse"), ("--format", formatFlag "monitor"),
      ("--count", trueFlag "outputOnlyCount"), ("--json", trueFlag "json")],
    conflicts := [["--count", "--format"], ["--count", "--json"]], post := jsonCheck },
  { kind := "list-windows", allowInConfig := false,
    help := help ["USAGE: list-windows [-h|--help] (--workspace <workspace>...|--monitor <monitor>...)",
      "                    [--monitor <monitor>...] [--workspace <workspace>...]",
      "                    [--pid <pid>] [--app-bundle-id <app-bundle-id>] [--format <output-format>]",
      "                    [--count] [--json]",
      "   OR: list-windows [-h|--help] --all [--format <output-format>] [--count] [--json]",
      "   OR: list-windows [-h|--help] --focused [--format <output-format>] [--count] [--json]"],
    flags := [("--all", trueFlag "all"), ("--focused", trueFlag "focused"), ("--monitor", monitorIdsFlag "monitors"),
      ("--workspace", workspacesFlag),
      ("--pid", valueFlag "pidFilter" "<pid>" fun s => if (parseInt32 s).isSome then none else some "Int32"),
      ("--app-bundle-id", valueFlag "appIdFilter" "<app-bundle-id>"), ("--format", formatFlag "window"),
      ("--count", trueFlag "outputOnlyCount"), ("--json", trueFlag "json")],
    conflicts := [["--all", "--focused", "--workspace"], ["--all", "--focused", "--monitor"], ["--count", "--format"],
      ["--count", "--json"]],
    post := fun f => do
      let filters := ["focused", "monitors", "workspaces", "pidFilter", "appIdFilter"]
      check (f.bool "focused" || f.bool "all" || f.has "monitors" || f.has "workspaces")
        "Mandatory option is not specified (--focused|--all|--monitor|--workspace)"
      check (!f.bool "all" || !filters.any f.has)
        "--all conflicts with \"filtering\" flags. Please use '--monitor all' instead of '--all' alias"
      check (!f.bool "focused" || !(filters.erase "focused").any f.has) "--focused conflicts with other \"filtering\" flags"
      let f := if f.bool "all" then (f.erase "all").set "monitors" (.strs ["all"]) else f
      jsonCheck f },
  { kind := "list-workspaces", allowInConfig := false,
    help := help ["USAGE: list-workspaces [-h|--help] --monitor <monitor>... [--visible [no]] [--empty [no]] [--format <output-format>] [--count] [--json]",
      "   OR: list-workspaces [-h|--help] --all [--format <output-format>] [--count] [--json]",
      "   OR: list-workspaces [-h|--help] --focused [--format <output-format>] [--count] [--json]"],
    flags := [("--focused", trueFlag "focused"), ("--all", trueFlag "all"), ("--visible", boolFlag "visible"),
      ("--empty", boolFlag "empty"), ("--monitor", monitorIdsFlag "onMonitors"), ("--format", formatFlag "workspace"),
      ("--count", trueFlag "outputOnlyCount"), ("--json", trueFlag "json")],
    conflicts := [["--all", "--focused", "--monitor"], ["--count", "--format"], ["--count", "--json"]],
    post := fun f => do
      let filters := ["onMonitors", "visible", "empty"]
      check (f.bool "all" || f.bool "focused" || f.has "onMonitors") "Mandatory option is not specified (--all|--focused|--monitor)"
      check (!f.bool "all" || !filters.any f.has) "--all conflicts with any other \"filtering\" options"
      check (!f.bool "focused" || !filters.any f.has) "--focused conflicts with all other \"filtering\" options"
      let f := if f.bool "all" then (f.erase "all").set "onMonitors" (.strs ["all"]) else f
      let f := if f.bool "focused" then ((f.erase "focused").set "onMonitors" (.strs ["focused"])).set "visible" (.bool true) else f
      jsonCheck f },
  { kind := "macos-native-fullscreen",
    help := help ["USAGE: macos-native-fullscreen [-h|--help] [--window-id <window-id>]",
      "   OR: macos-native-fullscreen [-h|--help] [--window-id <window-id>] [--fail-if-noop] on",
      "   OR: macos-native-fullscreen [-h|--help] [--window-id <window-id>] [--fail-if-noop] off"],
    flags := [("--fail-if-noop", trueFlag "failIfNoop"), ("--window-id", windowIdFlag)],
    pos := [opt (toggleArg)],
    post := fun f => do
      check (!f.bool "failIfNoop" || f.has "toggle") "--fail-if-noop requires 'on' or 'off' argument"
      return f },
  { kind := "macos-native-minimize", help := "USAGE: macos-native-minimize [-h|--help] [--window-id <window-id>]",
    flags := [("--window-id", windowIdFlag)] },
  { kind := "mfact", help := "Usage: mfact [+|-]<number>", negativeNumbers := true,
    pos := [mand (amountArg "amount") "[+|-]<number>"] },
  { kind := "mode", help := "USAGE: mode [-h|--help] <binding-mode>",
    pos := [mand (strArg "targetMode") "<binding-mode>"] },
  { kind := "move", help := "USAGE: move [-h|--help] [--window-id <window-id>] (left|down|up|right)",
    flags := [("--window-id", windowIdFlag),
      ("--boundaries", enumFlag "rawBoundaries" ["workspace", "all-monitors-outer-frame"] "<boundary>"),
      ("--boundaries-action", enumFlag "rawBoundariesAction" ["stop", "fail", "create-implicit-container"] "<action>")],
    pos := [mand (enumArg "direction" directions) (union directions)] },
  { kind := "move-mouse", help := "USAGE: move-mouse [-h|--help] [--fail-if-noop] <mouse-position>",
    flags := [("--fail-if-noop", trueFlag "failIfNoop")],
    pos := [mand (enumArg "mouseTarget" ["monitor-lazy-center", "monitor-force-center", "window-lazy-center", "window-force-center"]) "<mouse-position>"],
    post := fun f => do
      check (!f.bool "failIfNoop" || ["window-lazy-center", "monitor-lazy-center"].contains ((f.str? "mouseTarget").getD ""))
        "--fail-if-noop is only compatible with window-lazy-center or monitor-lazy-center"
      return f },
  { kind := "move-node-to-master", help := "USAGE: move-node-to-master [-h|--help]" },
  { kind := "move-node-to-monitor",
    help := help ["USAGE: move-node-to-monitor [-h|--help] [--window-id <window-id>] [--focus-follows-window]",
      "                            [--wrap-around] (left|down|up|right|next|prev)",
      "   OR: move-node-to-monitor [-h|--help] [--window-id <window-id>] [--focus-follows-window]",
      "                            [--fail-if-noop] <monitor-pattern>..."],
    flags := [("--wrap-around", trueFlag "wrapAround"), ("--window-id", windowIdFlag),
      ("--focus-follows-window", trueFlag "focusFollowsWindow"), ("--fail-if-noop", trueFlag "failIfNoop")],
    pos := [mand (monitorTargetArg) monitorTargetPlaceholder],
    post := fun f => do
      check (!f.bool "wrapAround" || !f.has "patterns") "--wrap-around is incompatible with <monitor-pattern> argument"
      check (!f.bool "failIfNoop" || f.has "patterns") s!"--fail-if-noop is incompatible with {union (directions ++ nextPrev)}"
      return f },
  { kind := "move-node-to-workspace",
    help := help ["USAGE: move-node-to-workspace [-h|--help] [--focus-follows-window] [--wrap-around]",
      "                              [--stdin|--no-stdin]", "                              (next|prev)",
      "   OR: move-node-to-workspace [-h|--help] [--focus-follows-window] [--fail-if-noop]",
      "                              [--window-id <window-id>] <workspace-name>"],
    flags := [("--wrap-around", trueFlag "_wrapAround"), ("--fail-if-noop", trueFlag "failIfNoop"),
      ("--window-id", windowIdFlag), ("--focus-follows-window", trueFlag "focusFollowsWindow"),
      ("--stdin", trueFlag "explicitStdinFlag"), ("--no-stdin", falseFlag "explicitStdinFlag")],
    pos := [mand (workspaceTargetArg) "(<workspace-name>|next|prev)"],
    conflicts := [["--stdin", "--no-stdin"]],
    post := fun f => do
      let rel := isRelativeTarget f
      check (!f.has "_wrapAround" || rel) "--wrapAround requires using (prev|next) argument"
      check (!(f.bool "failIfNoop" && rel)) "--fail-if-noop is incompatible with (next|prev)"
      check (!(f.has "windowId" && rel)) "--window-id is incompatible with (next|prev)"
      check (!f.has "explicitStdinFlag" || rel) "--stdin and --no-stdin require using (next|prev) argument"
      return f },
  { kind := "move-workspace-to-monitor",
    help := help ["USAGE: move-workspace-to-monitor [-h|--help] [--workspace <workspace>] [--wrap-around] (left|down|up|right)",
      "   OR: move-workspace-to-monitor [-h|--help] [--workspace <workspace>] [--wrap-around] (next|prev)",
      "   OR: move-workspace-to-monitor [-h|--help] [--workspace <workspace>] <monitor-pattern>..."],
    flags := [("--wrap-around", trueFlag "wrapAround"), ("--workspace", workspaceFlag)],
    pos := [mand (monitorTargetArg) monitorTargetPlaceholder],
    post := fun f => do
      check (!f.bool "wrapAround" || !f.has "patterns") "--wrap-around is incompatible with <monitor-pattern> argument"
      return f },
  { kind := "reload-config", help := "USAGE: reload-config [-h|--help] [--no-gui] [--dry-run]",
    flags := [("--no-gui", trueFlag "noGui"), ("--dry-run", trueFlag "dryRun")] },
  { kind := "resize",
    help := "USAGE: resize [-h|--help] [--window-id <window-id>] (smart|smart-opposite|width|height) [+|-]<number>",
    negativeNumbers := true, flags := [("--window-id", windowIdFlag)],
    pos := [mand (enumArg "dimension" ["width", "height", "smart", "smart-opposite"]) "(smart|smart-opposite|width|height)", mand (amountArg "units") "[+|-]<number>"] },
  { kind := "rotate", help := "USAGE: rotate [-h|--help]" },
  { kind := "scratchpad", help := "USAGE: scratchpad [-h|--help] <scratchpad>",
    pos := [mand (strArg "name") "<scratchpad>"] },
  { kind := "summon-workspace", help := "USAGE: summon-workspace [-h|--help] [--fail-if-noop] <workspace>",
    flags := [("--fail-if-noop", trueFlag "failIfNoop")],
    pos := [mand (fun args i f => ⟨(workspaceName args[i]!).map fun ws => f.set "target" (.str ws), 1⟩) "<workspace>"] },
  { kind := "swap",
    help := help ["USAGE: swap [-h|--help] [--window-id <window-id>] [--swap-focus]", "            [--wrap-around]",
      "            (next|prev)"],
    flags := [("--swap-focus", trueFlag "swapFocus"), ("--wrap-around", trueFlag "wrapAround"), ("--window-id", windowIdFlag)],
    pos := [mand (enumArg "target" nextPrev) (union nextPrev)] },
  { kind := "trigger-binding", help := "USAGE: trigger-binding [-h|--help] <binding> --mode <mode-id>",
    flags := [("--mode", valueFlag "_mode" "<mode-id>")],
    pos := [mand (strArg "binding") "<binding>"],
    post := fun f => do check (f.has "_mode") "--mode flag is mandatory"; return f },
  { kind := "volume",
    help := help ["USAGE: volume [-h|--help] (up|down) [--no-gui]", "   OR: volume [-h|--help] (mute-toggle|mute-off|mute-on) [--no-gui]",
      "   OR: volume [-h|--help] set <number> [--no-gui]"],
    flags := [("--no-gui", falseFlag "gui")],
    pos := [mand (volumeArg) "(up|down|mute-toggle|mute-on|mute-off|set)"] },
  { kind := "workspace",
    help := help ["USAGE: workspace [-h|--help] [--auto-back-and-forth] [--fail-if-noop] <workspace-name>",
      "   OR: workspace [-h|--help] [--wrap-around] [--stdin|--no-stdin] (next|prev)"],
    flags := [("--auto-back-and-forth", trueFlag "_autoBackAndForth"), ("--wrap-around", trueFlag "_wrapAround"),
      ("--fail-if-noop", trueFlag "failIfNoop"), ("--stdin", trueFlag "explicitStdinFlag"),
      ("--no-stdin", falseFlag "explicitStdinFlag")],
    pos := [mand (workspaceTargetArg) "(<workspace-name>|next|prev)"],
    conflicts := [["--stdin", "--no-stdin"]],
    post := fun f => do
      let rel := isRelativeTarget f
      check (!f.has "_wrapAround" || rel) "--wrapAround requires using (next|prev) argument"
      check (!(f.has "_autoBackAndForth" && rel)) "--auto-back-and-forth is incompatible with (next|prev)"
      check (!(f.bool "failIfNoop" && rel)) "--fail-if-noop is incompatible with (next|prev)"
      check (!(f.bool "_autoBackAndForth" && f.bool "failIfNoop")) "--fail-if-noop is incompatible with --auto-back-and-forth"
      check (!f.has "explicitStdinFlag" || rel) "--stdin and --no-stdin require using (next|prev) argument"
      return f },
  { kind := "workspace-back-and-forth", help := "USAGE: workspace-back-and-forth [-h|--help]" }]

/-- Deprecated names, and their spec. `move-workspace-to-display` skips the cross-field checks, as it always did -/
def aliases : List (String × Spec) :=
  (match specs.find? (·.kind == "move") with | some s => [("move-through", s)] | none => []) ++
  (match specs.find? (·.kind == "move-workspace-to-monitor") with
    | some s => [("move-workspace-to-display", { s with post := pure })] | none => [])

def specOf? (name : String) : Option Spec :=
  (specs.find? (·.kind == name)).orElse fun _ => (aliases.find? (·.1 == name)).map (·.2)

/-- `-5` given to `resize` or `mfact` -/
def isNegativeNumber (arg : String) : Bool :=
  match arg.toList with
  | '-' :: c :: _ => c.isDigit
  | _ => false

/-- Parse a command's arguments against its spec -/
def parseWith (spec : Spec) (args : Array String) : Parsed := Id.run do
  let mut fields : Fields := []
  let mut errors : List String := []
  let mut pos := 0
  let mut options : List String := []
  let mut i := 0
  let mut fuel := args.size + 1
  while i < args.size && fuel > 0 do
    fuel := fuel - 1
    let arg := args[i]!
    if arg == "-h" || arg == "--help" then return .help spec.help
    else if arg.startsWith "-" && !(spec.negativeNumbers && isNegativeNumber arg) then
      match spec.flags.find? (·.1 == arg) with
      | some (_, flag) =>
        i := i + 1
        if options.contains arg then errors := errors ++ [s!"Duplicated option {quoted arg}"]
        else options := options ++ [arg]
        let step := flag arg args i fields
        i := i + step.advance
        match step.result with
        | .ok f => fields := f
        | .error e => errors := errors ++ [e]
      | none =>
        errors := errors ++ [s!"Unknown flag {quoted arg}"]
        break
    else match spec.pos[pos]? with
      | some p =>
        let step := p.parse args i fields
        i := i + step.advance
        match step.result with
        | .ok f => fields := f
        | .error e => errors := errors ++ [e]
        pos := pos + 1
      | none =>
        errors := errors ++ [s!"Unknown argument {quoted arg}"]
        break
  for p in spec.pos.drop pos do
    if let some placeholder := p.mandatory then errors := errors ++ [s!"Argument {quoted placeholder} is mandatory"]
  for conflict in spec.conflicts do
    let both := conflict.filter options.contains
    if both.length > 1 then
      let sorted := (both.toArray.qsort (· < ·)).toList
      errors := errors ++ [s!"Conflicting options: {", ".intercalate sorted}"]
      break
  if !errors.isEmpty then return .fail (joinErrors errors)
  -- Swift reported cross-field checks without the `ERROR:` prefix
  match spec.post fields with
  | .ok f => return .cmd spec.kind f
  | .error e => return .fail e

/-- Parse a command line split into arguments -/
def parseArgs (args : List String) : Parsed :=
  match args with
  | [] | "" :: _ => .fail "Can't parse empty string command"
  | name :: rest =>
    match specOf? name with
    | none => .fail s!"Unrecognized subcommand '{name}'"
    | some spec =>
      let rest := if spec.kind == "list-windows" then rest.map fun a => if a == "--app-id" then "--app-bundle-id" else a else rest
      parseWith spec rest.toArray

/-! ## Splitting a command string (key bindings, callbacks) -/

/-- `"  foo   bar "` is `["foo", "bar"]`, `"foo 'bar baz'"` is `["foo", "bar baz"]` -/
def splitArgs (s : String) : Except String (List String) :=
  let rec go : List Char → Option (Option Char) → String → List String → Except String (List String)
    -- `none`: between arguments; `some q`: in an argument, quoted with `q` if any
    | [], none, _, acc => .ok acc
    | [], some none, arg, acc => .ok (acc ++ [arg])
    | [], some (some q), _, _ => .error s!"Last quote {q} isn't closed"
    | c :: cs, none, arg, acc =>
      if c == '"' || c == '\'' then go cs (some (some c)) arg acc
      else if !isWhitespace c then go cs (some none) (arg.push c) acc
      else go cs none arg acc
    | c :: cs, some q, arg, acc =>
      if q == some c then go cs none "" (acc ++ [arg])
      else if q.isNone && isWhitespace c then go cs none "" (acc ++ [arg])
      else if q.isNone && (c == '"' || c == '\'') then .error s!"Unexpected quote {c} in argument '{arg}'"
      else go cs (some q) (arg.push c) acc
  go s.toList none "" []

/-- A command as written in the config, and the arguments it was split into: `exec-and-forget` takes the rest of the
line as its bash script -/
def parseString (raw : String) : Parsed × List String :=
  if raw.startsWith "exec-and-forget" then
    (.cmd "exec-and-forget" [("bashScript", .str (raw.drop "exec-and-forget".length).toString)], [])
  else match splitArgs raw with
    | .ok args => (parseArgs args, args)
    | .error e => (.fail e, [])

/-! ## The text protocol -/

/-- Tabs and newlines in strings travel as `\x1c` and `\x1d` -/
def escape (s : String) : String := (s.replace "\t" "\x1c").replace "\n" "\x1d"
def unescape (s : String) : String := (s.replace "\x1c" "\t").replace "\x1d" "\n"

def V.render : V → String
  | .bool b => if b then "b1" else "b0"
  | .str s => "s" ++ escape s
  | .strs l => "l" ++ "\x1e".intercalate (l.map escape)

/-- `cmd <kind>` then `field <name> <value>` lines (value: `b0`/`b1`, `s<string>`, or `l<items>`); `help <text>`; or
`fail <message>`. Newlines in help and messages are kept as `\x1f` -/
def Parsed.render : Parsed → String
  | .cmd kind fields => "\n".intercalate (s!"cmd\t{kind}" :: fields.reverse.map fun (k, v) => s!"field\t{k}\t{v.render}")
  | .help text => "help\t" ++ text.replace "\n" "\x1f"
  | .fail msg => "fail\t" ++ msg.replace "\n" "\x1f"

end DwmacCore.Cli
