/-!
# TOML

dwmac's config is a TOML file. This parses TOML 1.0 into a tree of values: strings (basic, literal and multi-line),
integers (decimal, hex, octal, binary), floats, booleans, arrays, tables (`[a.b]`, dotted keys, inline tables) and
arrays of tables (`[[a]]`). Dates and times are kept as their text: dwmac has no option that takes one, so they only
appear in type errors. Floats are kept as their text too (without `_`), for whoever reads them to convert.

A table keeps its keys in the order they were defined.
-/

namespace DwmacCore.Toml

inductive Value where
  | str (s : String)
  | int (i : Int)
  | float (text : String)
  | bool (b : Bool)
  | datetime (text : String)
  | array (items : List Value)
  | table (entries : List (String × Value))
deriving Repr, Inhabited, BEq

/-- The type's name in error messages, as dwmac always printed it -/
def Value.typeName : Value → String
  | .str _ => "string"
  | .int _ => "integer"
  | .float _ => "floating-point number"
  | .bool _ => "bool"
  | .datetime t => if t.contains ':' then (if t.contains '-' then "date-time" else "time") else "date"
  | .array _ => "array"
  | .table _ => "table"

def Value.str? : Value → Option String | .str s => some s | _ => none
def Value.int? : Value → Option Int | .int i => some i | _ => none
def Value.bool? : Value → Option Bool | .bool b => some b | _ => none
def Value.array? : Value → Option (List Value) | .array a => some a | _ => none
def Value.table? : Value → Option (List (String × Value)) | .table t => some t | _ => none
/-- A float's text; an integer counts -/
def Value.float? : Value → Option String
  | .float t => some t
  | .int i => some (toString i)
  | _ => none

/-! ## Parsing -/

structure Pos where
  line : Nat := 1
  col : Nat := 1

structure St where
  rest : List Char
  pos : Pos := {}

abbrev P := StateT St (Except String)

private def fail {α} (msg : String) : P α := do
  let p := (← get).pos
  throw s!"{msg} (at line {p.line}, column {p.col})"

private def peek : P (Option Char) := do return (← get).rest.head?
private def peek2 : P (Option Char) := do return (← get).rest.drop 1 |>.head?

private def adv : P Unit := modify fun s =>
  match s.rest with
  | '\n' :: r => { rest := r, pos := { line := s.pos.line + 1, col := 1 } }
  | _ :: r => { rest := r, pos := { s.pos with col := s.pos.col + 1 } }
  | [] => s

private def next (what : String) : P Char := do
  match ← peek with
  | some c => adv; return c
  | none => fail s!"Unexpected end of file in {what}"

private def startsWith (s : String) : P Bool := do return (← get).rest.take s.length == s.toList

private def skip (n : Nat) : P Unit := do for _ in [0:n] do adv

/-- Spaces and tabs -/
private partial def ws : P Unit := do
  match ← peek with
  | some ' ' | some '\t' => adv; ws
  | _ => pure ()

private partial def comment : P Unit := do
  if (← peek) == some '#' then
    let rec go : P Unit := do
      match ← peek with
      | none | some '\n' => pure ()
      | some _ => adv; go
    go

/-- Whitespace, newlines and comments, as allowed inside arrays -/
private partial def wsNlc : P Unit := do
  ws; comment
  match ← peek with
  | some '\n' => adv; wsNlc
  | some '\r' => if (← peek2) == some '\n' then adv; adv; wsNlc else pure ()
  | _ => pure ()

/-- The end of a line: optional whitespace and comment, then a newline or the end of the file -/
private def eol : P Unit := do
  ws; comment
  match ← peek with
  | none => pure ()
  | some '\n' => adv
  | some '\r' => if (← peek2) == some '\n' then adv; adv else fail "Expected a newline"
  | some c => fail s!"Expected a newline, found '{c}'"

private def isBare (c : Char) : Bool := c.isAlphanum || c == '_' || c == '-'

private def hexVal (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else if 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

private def unicodeEscape (n : Nat) : P Char := do
  let mut v := 0
  for _ in [0:n] do
    match hexVal (← next "a unicode escape") with
    | some d => v := v * 16 + d
    | none => fail "Invalid unicode escape"
  if v.isValidChar then return Char.ofNat v else fail "Invalid unicode scalar in escape"

private def escape : P Char := do
  match ← next "an escape sequence" with
  | 'b' => return '\x08'
  | 't' => return '\t'
  | 'n' => return '\n'
  | 'f' => return '\x0c'
  | 'r' => return '\r'
  | 'e' => return '\x1b'
  | '"' => return '"'
  | '\\' => return '\\'
  | 'u' => unicodeEscape 4
  | 'U' => unicodeEscape 8
  | c => fail s!"Invalid escape sequence '\\{c}'"

/-- `"..."`, after the opening quote -/
private partial def basicString (acc : String) : P String := do
  match ← next "a string" with
  | '"' => return acc
  | '\\' => basicString (acc.push (← escape))
  | '\n' => fail "Unterminated string"
  | c => basicString (acc.push c)

/-- `'...'`, after the opening quote -/
private partial def literalString (acc : String) : P String := do
  match ← next "a string" with
  | '\'' => return acc
  | '\n' => fail "Unterminated string"
  | c => literalString (acc.push c)

/-- After a line-ending backslash in a multi-line basic string: skip whitespace and newlines -/
private partial def skipWsNl : P Unit := do
  match ← peek with
  | some ' ' | some '\t' | some '\n' | some '\r' => adv; skipWsNl
  | _ => pure ()

/-- `"""..."""` or `'''...'''`, after the opening quotes. Up to two quotes may end the content -/
private partial def multiline (q : Char) (basic : Bool) (acc : String) : P String := do
  let qqq := String.mk [q, q, q]
  if ← startsWith qqq then
    skip 3
    -- `""""` and `"""""`: the extra quotes belong to the content
    let mut acc := acc
    for _ in [0:2] do
      if (← peek) == some q then adv; acc := acc.push q
    return acc
  match ← next "a multi-line string" with
  | '\\' =>
    if !basic then multiline q basic (acc.push '\\')
    else
      -- A line-ending backslash trims the newline and the whitespace after it
      let r := (← get).rest
      let trailing := r.takeWhile (fun c => c == ' ' || c == '\t')
      match r.drop trailing.length with
      | '\n' :: _ | '\r' :: '\n' :: _ => skipWsNl; multiline q basic acc
      | _ => multiline q basic (acc.push (← escape))
  | c => multiline q basic (acc.push c)

private def string : P String := do
  if ← startsWith "\"\"\"" then
    skip 3
    -- A newline right after the opening quotes is trimmed
    if ← startsWith "\r\n" then skip 2 else if (← peek) == some '\n' then adv
    multiline '"' true ""
  else if ← startsWith "'''" then
    skip 3
    if ← startsWith "\r\n" then skip 2 else if (← peek) == some '\n' then adv
    multiline '\'' false ""
  else if (← peek) == some '"' then adv; basicString ""
  else adv; literalString ""

private partial def bareKey (acc : String) : P String := do
  match ← peek with
  | some c => if isBare c then adv; bareKey (acc.push c) else return acc
  | none => return acc

private def simpleKey : P String := do
  match ← peek with
  | some '"' | some '\'' =>
    if (← startsWith "\"\"\"") || (← startsWith "'''") then fail "Multi-line strings can't be keys"
    string
  | some c => if isBare c then bareKey "" else fail s!"Expected a key, found '{c}'"
  | none => fail "Expected a key, found the end of the file"

/-- `a.b."c"`: dotted keys, with optional whitespace around the dots -/
private partial def key : P (List String) := do
  let k ← simpleKey
  ws
  if (← peek) == some '.' then adv; ws; return k :: (← key) else return [k]

/-- The characters of a number, date or keyword: up to whitespace or a delimiter -/
private partial def token (acc : String) : P String := do
  match ← peek with
  | some c =>
    if c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' || c == ']' || c == '}' || c == '#' then return acc
    -- A date-time may have a space between its date and time: `1979-05-27 07:32:00`
    else adv; token (acc.push c)
  | none => return acc

private def digitsOk (s : String) (ok : Char → Bool) : Bool :=
  let parts := s.splitOn "_"
  !s.isEmpty && parts.all fun p => !p.isEmpty && p.all ok

private def parseRadix (s : String) (radix : Nat) (ok : Char → Bool) : Option Int :=
  if !digitsOk s ok then none
  else some (Int.ofNat ((s.toList.filter (· != '_')).foldl (fun n c => n * radix + (hexVal c).getD 0) 0))

private def isDateLike (t : String) : Bool :=
  let cs := t.toList
  (cs.length ≥ 5 && (cs.take 4).all Char.isDigit && cs[4]? == some '-') ||
  (cs.length ≥ 3 && (cs.take 2).all Char.isDigit && cs[2]? == some ':')

/-- A number, boolean, `inf`/`nan`, or a date/time -/
private def scalar : P Value := do
  let t ← token ""
  -- A date followed by a space and a time
  let t ← if isDateLike t && t.length == 10 && (← peek) == some ' ' then do
      let r := (← get).rest.drop 1
      if (r.take 2).all Char.isDigit && r[2]? == some ':' then adv; pure (t ++ " " ++ (← token "")) else pure t
    else pure t
  if t == "true" then return .bool true
  if t == "false" then return .bool false
  if t.isEmpty then fail "Expected a value"
  if isDateLike t then return .datetime t
  let (neg, body) :=
    if t.startsWith "-" then (true, (t.drop 1).toString)
    else if t.startsWith "+" then (false, (t.drop 1).toString) else (false, t)
  if body == "inf" || body == "nan" then return .float t
  if !neg && !t.startsWith "+" then
    for (pre, radix, ok) in [("0x", 16, fun c => (hexVal c).isSome), ("0o", 8, fun c => '0' ≤ c && c ≤ '7'),
        ("0b", 2, fun c => c == '0' || c == '1')] do
      if t.startsWith pre then
        match parseRadix (t.drop 2).toString radix ok with
        | some i => return .int i
        | none => fail s!"Invalid number '{t}'"
  -- Decimal: no leading zeros
  let leadingZero (s : String) := s.length > 1 && s.startsWith "0" && (s.get ⟨1⟩).isDigit
  if digitsOk body Char.isDigit then
    if leadingZero body then fail s!"Leading zeros are not allowed: '{t}'"
    let n := (body.toList.filter (· != '_')).foldl (fun n c => n * 10 + (c.toNat - '0'.toNat)) 0
    return .int (if neg then -Int.ofNat n else Int.ofNat n)
  -- Float: int part, then a fraction and/or an exponent
  let (mant, exp) := match body.splitOn "e", body.splitOn "E" with
    | [m, e], _ => (m, some e)
    | _, [m, e] => (m, some e)
    | _, _ => (body, none)
  let (ip, frac) := match mant.splitOn "." with
    | [i, f] => (i, some f)
    | _ => (mant, none)
  let expOk := exp.all fun e =>
    let e := if e.startsWith "+" || e.startsWith "-" then (e.drop 1).toString else e
    digitsOk e Char.isDigit
  if (frac.isSome || exp.isSome) && digitsOk ip Char.isDigit && !leadingZero ip && frac.all (digitsOk · Char.isDigit)
      && expOk && (mant.splitOn ".").length ≤ 2 then
    return .float (String.mk (t.toList.filter (· != '_')))
  fail s!"Invalid value '{t}'"

/-- Set `path` to `v` in `t`: tables along the way are created, an existing key is an error -/
partial def insertAt (t : List (String × Value)) (path : List String) (v : Value) :
    Except String (List (String × Value)) :=
  match path with
  | [] => .ok t
  | [k] => if t.any (·.1 == k) then .error s!"Key '{k}' is defined twice" else .ok (t ++ [(k, v)])
  | k :: ks =>
    match t.find? (·.1 == k) with
    | none => do return t ++ [(k, .table (← insertAt [] ks v))]
    | some (_, .table sub) => do
      let sub ← insertAt sub ks v
      return t.map fun (k', x) => if k' == k then (k', .table sub) else (k', x)
    | some _ => .error s!"Key '{k}' is not a table"

mutual
  private partial def value : P Value := do
    match ← peek with
    | some '"' | some '\'' => return .str (← string)
    | some '[' => adv; array []
    | some '{' => adv; ws; inlineTable []
    | _ => scalar

  private partial def array (acc : List Value) : P Value := do
    wsNlc
    if (← peek) == some ']' then adv; return .array acc.reverse
    let v ← value
    wsNlc
    match ← next "an array" with
    | ',' => array (v :: acc)
    | ']' => return .array (v :: acc).reverse
    | c => fail s!"Expected ',' or ']' in an array, found '{c}'"

  private partial def inlineTable (acc : List (String × Value)) : P Value := do
    if acc.isEmpty && (← peek) == some '}' then adv; return .table []
    let k ← key
    ws
    if (← next "an inline table") != '=' then fail "Expected '=' after a key"
    ws
    let v ← value
    let acc ← match insertAt acc k v with
      | .ok t => pure t
      | .error e => fail e
    ws
    match ← next "an inline table" with
    | ',' => ws; inlineTable acc
    | '}' => return .table acc
    | c => fail s!"Expected ',' or '}' in an inline table, found '{c}'"
end

/-- Apply `f` to the table at `path`, creating tables on the way. An array of tables on the way means its last
table, as for `[a.b]` after `[[a]]` -/
partial def modifyAt (t : List (String × Value)) (path : List String)
    (f : List (String × Value) → Except String (List (String × Value))) : Except String (List (String × Value)) :=
  match path with
  | [] => f t
  | k :: ks =>
    let set (x : Value) := if t.any (·.1 == k) then t.map fun (k', y) => if k' == k then (k', x) else (k', y)
      else t ++ [(k, x)]
    match t.find? (·.1 == k) with
    | none => do return set (.table (← modifyAt [] ks f))
    | some (_, .table sub) => do return set (.table (← modifyAt sub ks f))
    | some (_, .array items) =>
      match items.getLast? with
      | some (.table last) => do
        let last ← modifyAt last ks f
        return set (.array (items.dropLast ++ [.table last]))
      | _ => .error s!"Key '{k}' is not a table"
    | some _ => .error s!"Key '{k}' is not a table"

private structure Doc where
  root : List (String × Value) := []
  /-- The table that `key = value` lines go in -/
  current : List String := []
  /-- `[a.b]` headers seen, which can't repeat -/
  headers : List (List String) := []

private partial def document (d : Doc) : P Doc := do
  wsNlc
  match ← peek with
  | none => return d
  | some '[' =>
    adv
    let isArray := (← peek) == some '['
    if isArray then adv
    ws
    let path ← key
    ws
    if (← next "a table header") != ']' then fail "Expected ']' after a table header"
    if isArray then
      if (← next "a table header") != ']' then fail "Expected ']]' after an array of tables header"
    eol
    let r : Except String Doc :=
      if isArray then
        let parent := path.dropLast
        let k := path.getLast!
        (modifyAt d.root parent fun t =>
          match t.find? (·.1 == k) with
          | none => .ok (t ++ [(k, .array [.table []])])
          | some (_, .array items) =>
            if items.all (· matches .table _) then .ok (t.map fun (k', x) => if k' == k then (k', .array (items ++ [.table []])) else (k', x))
            else .error s!"Key '{k}' is not an array of tables"
          | some _ => .error s!"Key '{k}' is not an array of tables").map fun root =>
          -- A new element: headers under it may repeat those under the previous one
          { root, current := path, headers := d.headers.filter (fun h => !(path.isPrefixOf h)) }
      else if d.headers.contains path then .error s!"Table '{".".intercalate path}' is defined twice"
      else (modifyAt d.root path .ok).map fun root => { root, current := path, headers := path :: d.headers }
    match r with
    | .ok d => document d
    | .error e => fail e
  | some _ =>
    let k ← key
    ws
    if (← next "a key-value pair") != '=' then fail "Expected '=' after a key"
    ws
    let v ← value
    eol
    match modifyAt d.root d.current (insertAt · k v) with
    | .ok root => document { d with root }
    | .error e => fail e

/-- Parse a TOML document into its top-level table, or say what's wrong and where -/
def parse (src : String) : Except String (List (String × Value)) :=
  (document {} |>.run' { rest := src.toList }).map (·.root)

end DwmacCore.Toml
