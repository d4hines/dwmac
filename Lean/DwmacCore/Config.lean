import DwmacCore.Toml
import DwmacCore.Cli
import DwmacCore.Keys

/-!
# The config

`~/.dwmac.toml` (or `~/.config/dwmac/dwmac.toml`), parsed (`Toml`) and interpreted: every option, binding, callback
and scratchpad, with dwmac's error messages. Each error names the key it's about (`gaps.inner.vertical: ...`).
Keys are checked in sorted order, as dwmac always did; errors don't stop the rest of the config being read.
-/

namespace DwmacCore.Config

open Toml (Value)

export Keys (Binding)

/-- An `on-window-detected` callback -/
structure Detected where
  appId : Option String := none
  appNameRegex : Option String := none
  titleRegex : Option String := none
  workspace : Option String := none
  duringStartup : Option Bool := none
  checkFurther : Bool := false
  run : List String := []
deriving Repr, BEq, Inhabited

/-- A gap: a value, or values for monitors matching patterns (the first that matches) and a default -/
structure Dyn where
  default : Int := 0
  perMonitor : List (String × Int) := []
deriving Repr, BEq, Inhabited

structure Scratchpad where
  name : String
  appId : String
  /-- Fractions of the monitor's visible rect, as written -/
  width : String := "0.9"
  height : String := "0.9"
deriving Repr, BEq, Inhabited

def gapNames := ["inner.horizontal", "inner.vertical", "outer.bottom", "outer.left", "outer.right", "outer.top"]

structure Config where
  version : Nat := 1
  afterStartup : List String := []
  onFocusChanged : List String := []
  onFocusedMonitorChanged : List String := []
  onModeChanged : List String := []
  layout : String := "master-stack"
  orientation : String := "auto"
  masterPosition : String := "left"
  startAtLogin : Bool := false
  autoUnhide : Bool := false
  attachBelow : Bool := false
  centerFloating : Bool := false
  persistent : List String := []
  execOnWorkspaceChange : List String := []
  /-- As written -/
  mfact : String := "0.5"
  mod : String := ""
  preset : String := "qwerty"
  customKeys : List (String × Nat) := []
  /-- `exec`, if given: whether to inherit dwmac's environment, and the variables it sets (interpolated) -/
  exec : Option (Bool × List (String × String)) := none
  gaps : List (String × Dyn) := []
  forced : List (String × List String) := []
  modes : List (String × List Binding) := []
  detected : List Detected := []
  scratchpads : List Scratchpad := []
deriving Repr, Inhabited

/-- The environment `exec-and-forget` runs commands in, given dwmac's -/
def Config.env (c : Config) (env : List (String × String)) : List (String × String) :=
  let merge (base over : List (String × String)) := base.filter (fun (k, _) => !over.any (·.1 == k)) ++ over
  match c.exec with
  | none => merge env [("PATH", "/opt/homebrew/bin:/opt/homebrew/sbin:" ++ ((List.lookup "PATH" env).getD ""))]
  | some (inherit, over) => merge (if inherit then env else []) over

/-! ## Reading values -/

/-- Errors so far, in order -/
abbrev M := StateM (Array String)

/-- Where a value is: `gaps.inner.vertical`, `on-window-detected[0].run` -/
abbrev Path := String

def Path.key (p : Path) (k : String) : Path := if p.isEmpty then k else p ++ "." ++ k
def Path.idx (p : Path) (i : Nat) : Path := p ++ s!"[{i}]"

def err (p : Path) (msg : String) : M Unit := modify (·.push (if p.isEmpty then msg else s!"{p}: {msg}"))

def typeErr (expected : List String) (actual : Value) : String :=
  match expected with
  | [e] => s!"Expected type is '{e}'. But actual type is '{actual.typeName}'"
  | es => s!"Expected types are {" or ".intercalate (es.map fun e => s!"'{e}'")}. But actual type is '{actual.typeName}'"

/-- The value's error, under its path -/
def orErr {α} (p : Path) : Except String α → M (Option α)
  | .ok a => return some a
  | .error e => do err p e; return none

def str (p : Path) (v : Value) : M (Option String) := orErr p (v.str?.elim (.error (typeErr ["string"] v)) .ok)
def bool (p : Path) (v : Value) : M (Option Bool) := orErr p (v.bool?.elim (.error (typeErr ["bool"] v)) .ok)
def int (p : Path) (v : Value) : M (Option Int) := orErr p (v.int?.elim (.error (typeErr ["integer"] v)) .ok)

def table (p : Path) (v : Value) : M (Option (List (String × Value))) :=
  orErr p (v.table?.elim (.error (typeErr ["table"] v)) .ok)

/-- Keys in sorted order -/
def sorted (t : List (String × Value)) : List (String × Value) :=
  t.toArray.qsort (fun a b => a.1 < b.1) |>.toList

/-- Read each key of a table with the reader for it; other keys are errors -/
def fields {α} (p : Path) (t : List (String × Value)) (init : α) (readers : List (String × (Path → Value → α → M α)))
    (topLevel := false) : M α := do
  let mut acc := init
  for (k, v) in sorted t do
    match List.lookup k readers with
    | some read => acc ← read (p.key k) v acc
    | none => err (p.key k) (if topLevel then "Unknown top-level key" else "Unknown key")
  return acc

def arrayOfStrings (p : Path) (v : Value) : Except String (List String) :=
  match v with
  | .array items => items.zipIdx.mapM fun (x, i) => match x with
    | .str s => .ok s
    | x => .error s!"{p.idx i}: {typeErr ["string"] x}"
  | v => .error s!"{p}: {typeErr ["array"] v}"

/-- Like `orErr`, for errors that already carry their path -/
def orErrRaw {α} : Except String α → M (Option α)
  | .ok a => return some a
  | .error e => do modify (·.push e); return none

/-! ## Commands -/

def nativeKinds := ["macos-native-minimize", "macos-native-fullscreen"]

/-- A command as the config gives it: it must parse and be allowed in the config -/
def command (raw : String) : Except String (String × Cli.Fields) :=
  match (Cli.parseString raw).1 with
  | .cmd kind fields =>
    if ((Cli.specOf? kind).map (·.allowInConfig)).getD true then .ok (kind, fields)
    else .error s!"Command '{kind}' cannot be used in config"
  | .help text => .error text
  | .fail msg => .error msg

/-- A command or a list of commands. The macOS native commands may only come last -/
def commands (v : Value) : Except String (List String) :=
  match v with
  | .str s => do let _ ← command s; return [s]
  | .array items => do
    -- An item that isn't a string is parsed as its type error, as dwmac always did
    let raws := items.map fun x => x.str?.getD (typeErr ["string"] x)
    let parsed ← raws.mapM command
    if parsed.dropLast.any (nativeKinds.contains ·.1) then
      throw "macos-native-* commands are only allowed to be the last commands in the list"
    return raws
  | v => .error (typeErr ["string", "array"] v)

/-! ## Options -/

def monitorDescription (v : Value) : Except String String :=
  match v with
  | .str s => Cli.monitorDescription s
  | .int i => Cli.monitorDescription (toString i)
  | v => .error (typeErr ["string", "integer"] v)

/-- A gap: an integer, or `[{ monitor.<pattern> = n }, ..., default]` -/
def dyn (p : Path) (v : Value) : M Dyn := do
  match v with
  | .int i => return { default := i }
  | .array items =>
    match items.getLast? with
    | none => err p "The array must not be empty"; return {}
    | some last =>
      match last with
      | .int d =>
        if items.length == 1 then err p "The array must contain at least one monitor pattern"; return {}
        let mut rules := #[]
        for (x, i) in items.dropLast.zipIdx do
          let p := p.idx i
          let single (p : Path) (x : Value) (expected : Option String) : M (Option (Path × String × Value)) := do
            let msg := match expected with
              | some k => s!"The table is expected to have a single key '{k}'"
              | none => "The table is expected to have a single key"
            match x with
            | .table [(k, v)] =>
              if expected.all (· == k) then return some (p.key k, k, v) else err p msg; return none
            | .table _ => err p msg; return none
            | x => err p (typeErr ["table"] x); return none
          let some (p, _, inner) ← single p x (some "monitor") | continue
          let some (p, pattern, value) ← single p inner none | continue
          let some desc ← orErr p (Cli.monitorDescription pattern) | continue
          match value with
          | .int n => rules := rules.push (desc, n)
          | value => err p s!"Expected type is 'Int'. But actual type is '{value.typeName}'"
        return { default := d, perMonitor := rules.toList }
      | _ => err p "The last item in the array must be of type Int"; return {}
  | v => err p s!"Unsupported type: {v.typeName}, expected: Int or array"; return {}

def gaps (p : Path) (v : Value) (c : Config) : M Config := do
  let some t ← table p v | return c
  let side (prefix_ : String) (names : List String) : Path → Value → List (String × Dyn) → M (List (String × Dyn)) :=
    fun p v acc => do
      let some t ← table p v | return acc
      fields p t acc (names.map fun n => (n, fun p v acc => do
        let d ← dyn p v
        return (acc.filter (·.1 != s!"{prefix_}.{n}")) ++ [(s!"{prefix_}.{n}", d)]))
  let g ← fields p t c.gaps [("inner", side "inner" ["vertical", "horizontal"]),
    ("outer", side "outer" ["left", "bottom", "top", "right"])]
  return { c with gaps := g }

/-- `${VAR}` in an env var's value: the tokens, `.inl` literal or `.inr` variable -/
def interpolationTokens (s : String) : Except String (List (String ⊕ String)) := Id.run do
  -- 0: literal, 1: after `$`, 2: inside `${`
  let mut mode := 0
  let mut var := ""
  let mut lit := ""
  let mut out : Array (String ⊕ String) := #[]
  for c? in ((s.toList.map some) ++ [none] : List (Option Char)) do
    match mode, c? with
    | 0, some '$' => mode := 1
    | 0, some c => lit := lit.push c
    | 0, none => out := out.push (.inl lit)
    | 1, some '{' => mode := 2; var := ""; out := out.push (.inl lit); lit := ""
    | 1, some '$' => lit := lit.push '$'
    | 1, c? =>
      lit := lit.push '$'
      match c? with
      | some c => lit := lit.push c
      | none => out := out.push (.inl lit)
      mode := 0
    | _, some '}' => out := out.push (.inr var); mode := 0
    | _, some '{' => return .error s!"Can't parse '{var}\{' inside interpolation (Open curly brace is invalid character)"
    | _, some '$' => return .error s!"Can't parse '{var}$' inside interpolation ('$' is disallowed character)"
    | _, some c => var := var.push c
    | _, none => return .error "Unbalanced curly braces"
  return .ok (out.toList.filter (· != .inl ""))

def interpolate (s : String) (vars : List (String × String)) : Except (List String) String := do
  let tokens ← (interpolationTokens s).mapError ([·])
  let parts : List (Except String String) := tokens.map fun
    | .inl l => .ok l
    | .inr v => match List.lookup v vars with
      | some x => .ok x
      | none => .error s!"Env variable '{v}' isn't presented in Dwmac.app env vars, or not available for interpolation (because it's mutated)"
  let errors := parts.filterMap fun | .error e => some e | _ => none
  if errors.isEmpty then return String.join (parts.filterMap fun | .ok s => some s | _ => none) else throw errors

def exec (env : List (String × String)) (p : Path) (v : Value) (c : Config) : M Config := do
  let some t ← table p v | return c
  let (inherit, over) ← fields p t (true, ([] : List (String × String))) [
    ("inherit-env-vars", fun p v (_, o) => do return ((← bool p v).getD true, o)),
    ("env-vars", fun p v (i, _) => do
      let some t := v.table? | err p (typeErr ["array"] v); return (i, [])
      let mutated := t.map (·.1)
      let mut out := []
      for (k, x) in sorted t do
        let p := p.key k
        if k == "PWD" then err p "Changing 'PWD' is not allowed"
        let some raw ← str p x | continue
        let vars := env.filter (fun (k', _) => !mutated.contains k' || k' == k)
        match interpolate raw vars with
        | .ok s => out := out ++ [(k, s)]
        | .error es => for e in es do err p e
      return (i, out))]
  return { c with exec := some (inherit, over) }

def scratchpads (p : Path) (v : Value) (c : Config) : M Config := do
  let some t ← table p v | return c
  let mut out : Array Scratchpad := #[]
  for (name, x) in sorted t do
    let p := p.key name
    let (sp, mine) := (do
      let some t ← table p x | return { name, appId := "" }
      let fraction (p : Path) (v : Value) : M (Option String) := do
        let some f ← orErr p (v.float?.elim (.error (typeErr ["floating-point number", "integer"] v)) .ok) | return none
        match Cli.scaled f 9 with
        | some m => if m > 0 && m ≤ 1000000000 then return some f else err p "Must be in (0, 1] range"; return none
        | none => err p "Must be in (0, 1] range"; return none
      fields p t { name, appId := "" } [
        ("app-id", fun p v sp => do return { sp with appId := (← str p v).getD sp.appId }),
        ("width", fun p v sp => do return { sp with width := (← fraction p v).getD sp.width }),
        ("height", fun p v sp => do return { sp with height := (← fraction p v).getD sp.height })] : M Scratchpad).run #[]
    let mut mine := mine
    if sp.appId.isEmpty then mine := mine.push s!"{p}: 'app-id' is mandatory key"
    else if let some other := out.find? (·.appId == sp.appId) then
      mine := mine.push s!"{p}: app-id '{sp.appId}' is already owned by scratchpad '{other.name}'"
    if mine.isEmpty then out := out.push sp else modify (· ++ mine)
  return { c with scratchpads := out.toList }

def forced (p : Path) (v : Value) (c : Config) : M Config := do
  let some t ← table p v | return c
  let mut out := #[]
  for (ws, x) in sorted t do
    let p := p.key ws
    let descs ← match x with
      | .array items => items.zipIdx.filterMapM fun (d, i) => orErr (p.idx i) (monitorDescription d)
      | d => do pure (← orErr p (monitorDescription d)).toList
    out := out.push (ws, descs)
  return { c with forced := out.toList }

def detected (p : Path) (v : Value) (c : Config) : M Config := do
  let some items := v.array? | err p (typeErr ["array"] v); return c
  let mut out := #[]
  for (x, i) in items.zipIdx do
    let p := p.idx i
    let regex (p : Path) (v : Value) : M (Option String) := do
      let some s ← str p v | return none
      orErr p ((Regex.parse s).map (fun _ => s) |>.mapError fun e => s!"Can't parse '{s}' regex. {e}")
    let ((d, hasRun), mine) := (do
      let some t ← table p x | return ({}, false)
      fields p t (({} : Detected), false) [
        ("if", fun p v (d, r) => do
          let some t ← table p v | return (d, r)
          let d ← fields p t d [
            ("app-id", fun p v d => do return { d with appId := (← str p v).orElse fun _ => d.appId }),
            ("workspace", fun p v d => do return { d with workspace := (← str p v).orElse fun _ => d.workspace }),
            ("app-name-regex-substring", fun p v d => do
              return { d with appNameRegex := (← regex p v).orElse fun _ => d.appNameRegex }),
            ("window-title-regex-substring", fun p v d => do
              return { d with titleRegex := (← regex p v).orElse fun _ => d.titleRegex }),
            ("during-dwmac-startup", fun p v d => do
              return { d with duringStartup := (← bool p v).orElse fun _ => d.duringStartup })]
          return (d, r)),
        ("check-further-callbacks", fun p v (d, r) => do
          return ({ d with checkFurther := (← bool p v).getD d.checkFurther }, r)),
        ("run", fun p v (d, r) => do
          match ← orErr p (commands v) with
          | some run => return ({ d with run }, true)
          | none => return (d, r))] : M (Detected × Bool)).run #[]
    let mine := if hasRun then mine else mine.push s!"{p}: 'run' is mandatory key"
    if mine.isEmpty then out := out.push d else modify (· ++ mine)
  return { c with detected := out.toList }

def keyMapping (p : Path) (v : Value) (c : Config) : M Config := do
  let some t ← table p v | return c
  fields p t c [
    ("preset", fun p v c => do
      let some s ← str p v | return c
      if Keys.presets.contains s then return { c with preset := s }
      err p s!"Can't parse '{s}'.\nPossible values: (qwerty|dvorak|colemak)"; return c),
    ("key-notation-to-key-code", fun p v c => do
      let some t ← table p v | return c
      let mut keys := c.customKeys
      for (k, x) in sorted t do
        if k.toList.any Char.isWhitespace || k.contains '-' then err p s!"'{k}' is invalid key notation"
        else
          let p := p.key k
          let some name ← str p x | continue
          match List.lookup name Keys.qwerty with
          | some code => keys := (keys.filter (·.1 != k)) ++ [(k, code)]
          | none => err p s!"'{name}' is invalid key code"
      return { c with customKeys := keys })]

/-- The keys a binding can name, for the config's key mapping -/
def Config.keys (c : Config) : List (String × Nat) :=
  let preset := Keys.preset c.preset
  preset.filter (fun (k, _) => !c.customKeys.any (·.1 == k)) ++ c.customKeys

def binding (keys : List (String × Nat)) (mod : String) (p : Path) (raw : String) (v : Value) : M (Option Binding) := do
  let eff := if mod.isEmpty then raw else raw.replace "mod-" (mod ++ "-")
  let parts := (eff.splitOn "-").filter (!·.isEmpty)
  let mods := parts.dropLast.mapM Keys.modifierFlags
  let some mods := mods | err p s!"Can't parse modifiers in '{eff}' binding"; return none
  let some code := parts.getLast?.bind (List.lookup · keys) | err p s!"Can't parse the key in '{eff}' binding"; return none
  let some cmds ← orErr p (commands v) | return none
  let flags := mods.foldl (· ||| ·) 0
  let keyName := (List.lookup code Keys.names).getD ""
  let modName := Keys.modifiersName flags
  return some { raw, keyCode := code, modifiers := flags, commands := cmds,
                desc := if modName.isEmpty then keyName else modName ++ "-" ++ keyName }

def modes (p : Path) (v : Value) (c : Config) : M Config := do
  let some t ← table p v | return c
  let keys := c.keys
  let mut out := #[]
  for (name, x) in sorted t do
    let p := p.key name
    let bindings ← (do
      let some t ← table p x | return []
      fields p t [] [("binding", fun p v _ => do
        let some t ← table p v | return []
        let mut bs : List Binding := []
        for (raw, cmd) in sorted t do
          let p := p.key raw
          let some b ← binding keys c.mod p raw cmd | continue
          if bs.any (·.desc == b.desc) then err p s!"'{b.desc}' Binding redeclaration"
          bs := bs.filter (·.desc != b.desc) ++ [b]
        return bs)] : M (List Binding))
    out := out.push (name, bindings)
  if !out.any (·.1 == "main") then err p "Please specify 'main' mode"
  return { c with modes := out.toList }

/-- The workspaces `workspace <name>` and `move-node-to-workspace <name>` bindings name, and the force-assigned ones:
listed by `workspace next` even when empty, unless `persistent-workspaces` lists them (config version 2) -/
def Config.impliedPersistent (c : Config) : List String :=
  let fromBindings := c.modes.flatMap fun (_, bs) => bs.flatMap fun b => b.commands.filterMap fun raw =>
    match command raw with
    | .ok (kind, fields) =>
      if kind == "workspace" || kind == "move-node-to-workspace" then
        (fields.str? "target").bind fun t => if t.startsWith "=" then some (t.drop 1).toString else none
      else none
    | .error _ => none
  (fromBindings ++ c.forced.map (·.1)).eraseDups

def simple {α} (read : Path → Value → M (Option α)) (set : Config → α → Config) : Path → Value → Config → M Config :=
  fun p v c => do return (← read p v).elim c (set c)

def cmds (set : Config → List String → Config) : Path → Value → Config → M Config :=
  simple (fun p v => orErr p (commands v)) set

def enumStr (what : String) (values : List String) (set : Config → String → Config) : Path → Value → Config → M Config :=
  fun p v c => do
    let some s ← str p v | return c
    if values.contains s then return set c s else err p s!"Can't parse {what} '{s}'"; return c

/-- Interpret a parsed config. `env` is dwmac's environment, for `exec.env-vars` interpolation -/
def interpret (env : List (String × String)) (t : List (String × Value)) : Config × List String :=
  let go : M Config := do
    let c ← fields "" t ({} : Config) (topLevel := true) [
      ("config-version", fun p v c => do
        let some i ← int p v | return c
        if 1 ≤ i && i ≤ 2 then return { c with version := i.toNat } else err p "Must be in [1, 2] range"; return c),
      ("after-login-command", fun p v c => do
        if v.array? == some [] then return c
        err p "after-login-command is deprecated since Dwmac 0.19.0. https://github.com/hillyu/Dwmac/issues/1482"
        return c),
      ("after-startup-command", cmds fun c l => { c with afterStartup := l }),
      ("on-focus-changed", cmds fun c l => { c with onFocusChanged := l }),
      ("on-mode-changed", cmds fun c l => { c with onModeChanged := l }),
      ("on-focused-monitor-changed", cmds fun c l => { c with onFocusedMonitorChanged := l }),
      ("default-root-container-layout", fun p v c => do
        let some s ← str p v | return c
        if s == "master-stack" || s == "floating" then return { c with layout := s }
        err p s!"Can't parse layout '{s}'"; return c),
      ("default-root-container-orientation",
        enumStr "default container orientation" ["horizontal", "vertical", "auto"] fun c s => { c with orientation := s }),
      ("master-position", enumStr "master position" ["left", "right"] fun c s => { c with masterPosition := s }),
      ("start-at-login", simple bool fun c b => { c with startAtLogin := b }),
      ("automatically-unhide-macos-hidden-apps", simple bool fun c b => { c with autoUnhide := b }),
      ("persistent-workspaces", fun p v c => do
        let some l ← orErrRaw (arrayOfStrings p v) | return c
        if l.eraseDups.length != l.length then err p "Contains duplicated workspace names"; return c
        return { c with persistent := l }),
      ("exec-on-workspace-change", fun p v c => do
        return (← orErrRaw (arrayOfStrings p v)).elim c fun l => { c with execOnWorkspaceChange := l }),
      ("default-mfact", fun p v c => do
        match v with
        | .float f => return { c with mfact := f }
        | v => err p (typeErr ["floating-point number"] v); return c),
      ("attach-below", simple bool fun c b => { c with attachBelow := b }),
      ("center-floating-windows", simple bool fun c b => { c with centerFloating := b }),
      ("exec", exec env),
      ("key-mapping", fun _ _ c => pure c),
      ("mode", fun _ _ c => pure c),
      ("gaps", gaps),
      ("workspace-to-monitor-force-assignment", forced),
      ("on-window-detected", detected),
      ("scratchpad", scratchpads),
      ("mod", simple str fun c s => { c with mod := s }),
      ("non-empty-workspaces-root-containers-layout-on-startup", fun p v c => do
        let some s ← str p v | return c
        if s != "smart" then
          err p "'non-empty-workspaces-root-containers-layout-on-startup' is deprecated. Please drop it from your config"
        return c),
      ("indent-for-nested-containers-with-the-same-orientation", fun p _ c => do
        err p "Deprecated. Please drop it from the config. See https://github.com/hillyu/Dwmac/issues/96"; return c)]
    let c ← match List.lookup "key-mapping" t with
      | some v => keyMapping "key-mapping" v c
      | none => pure c
    let c ← match List.lookup "mode" t with
      | some v => modes "mode" v c
      | none => pure c
    if c.version ≤ 1 then
      if (List.lookup "persistent-workspaces" t).isSome then
        err "persistent-workspaces" "This config option is only available since 'config-version = 2'"
      return { c with persistent := c.impliedPersistent }
    return c
  let (c, errors) := go.run #[]
  (c, errors.toList)

/-- Parse and interpret a config file. A syntax error is the only error, with the default config -/
def parse (env : List (String × String)) (src : String) : Except String (Config × List String) :=
  (Toml.parse src).map (interpret env)

/-! ## The text protocol

One item per line, fields separated by tabs; lists by `\x1e`, pairs by `\x1f`; tabs and newlines in strings travel
as `\x1c` and `\x1d`. An optional string is empty (none) or `s<value>`. `error <message>` lines come first, or a
single `syntax <message>` -/

def esc := Cli.escape
def list (l : List String) : String := "\x1e".intercalate (l.map esc)
def opt (o : Option String) : String := o.elim "" (fun s => "s" ++ esc s)
def flag (b : Bool) : String := if b then "1" else "0"
def line (fields : List String) : String := "\t".intercalate fields

def Config.render (c : Config) (env : List (String × String)) : List String :=
  [line ["version", toString c.version]] ++
  [("afterStartup", c.afterStartup), ("onFocusChanged", c.onFocusChanged),
    ("onFocusedMonitorChanged", c.onFocusedMonitorChanged), ("onModeChanged", c.onModeChanged)].map
    (fun (k, l) => line ["cmds", k, list l]) ++
  [("layout", c.layout), ("orientation", c.orientation), ("masterPosition", c.masterPosition), ("mfact", c.mfact)].map
    (fun (k, v) => line ["opt", k, esc v]) ++
  [("startAtLogin", c.startAtLogin), ("autoUnhide", c.autoUnhide), ("attachBelow", c.attachBelow),
    ("centerFloating", c.centerFloating)].map (fun (k, v) => line ["flag", k, flag v]) ++
  [line ["persistent", list c.persistent], line ["execOnWorkspaceChange", list c.execOnWorkspaceChange]] ++
  (c.env env).map (fun (k, v) => line ["env", esc k, esc v]) ++
  c.gaps.map (fun (k, d) => line ["gap", k, toString d.default,
    "\x1e".intercalate (d.perMonitor.map fun (m, v) => esc m ++ "\x1f" ++ toString v)]) ++
  c.forced.map (fun (ws, ds) => line ["forced", esc ws, list ds]) ++
  c.scratchpads.map (fun sp => line ["scratchpad", esc sp.name, esc sp.appId, sp.width, sp.height]) ++
  c.modes.flatMap (fun (m, bs) => line ["mode", esc m] ::
    bs.map fun b => line ["binding", esc m, esc b.raw, toString b.keyCode, toString b.modifiers, esc b.desc,
      list b.commands]) ++
  c.detected.map (fun d => line ["detected", opt d.appId, opt d.appNameRegex, opt d.titleRegex, opt d.workspace,
    d.duringStartup.elim "" flag, flag d.checkFurther, list d.run])

/-- `config <env: k\x1fv items, \x1e separated> <toml>` (both escaped): the interpreted config -/
def query (envRaw src : String) : String :=
  let env := if envRaw.isEmpty then [] else (envRaw.splitOn "\x1e").filterMap fun kv =>
    match kv.splitOn "\x1f" with
    | [k, v] => some (Cli.unescape k, Cli.unescape v)
    | _ => none
  match parse env (Cli.unescape src) with
  | .error e => line ["syntax", esc e]
  | .ok (c, errors) => "\n".intercalate (errors.map (fun e => line ["error", esc e]) ++ c.render env)

end DwmacCore.Config
