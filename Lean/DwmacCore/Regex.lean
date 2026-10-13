/-!
# Regular expressions

The config and some commands take regular expressions: monitor patterns (`focus-monitor 'LG.*'`,
`workspace-to-monitor-force-assignment`), and window title and app name matchers (`on-window-detected`). dwmac only
ever asks whether a regex matches somewhere in a string, ignoring case. This supports the usual syntax: literals and
escapes, `.`, classes (`[a-z]`, `[^0-9]`, `\d`, `\w`, `\s` and their negations), anchors `^` and `$`, groups
(`(...)`, `(?:...)`), alternation `|`, and greedy quantifiers `*`, `+`, `?`, `{n}`, `{n,}`, `{n,m}`. Matching
backtracks over the parsed expression.
-/

namespace DwmacCore.Regex

inductive ClassItem where
  | char (c : Char)
  | range (lo hi : Char)
  /-- `\d`, `\w`, `\s`, or their negation -/
  | named (name : Char) (negated : Bool)
deriving Repr, Inhabited, BEq

inductive Re where
  | empty
  | char (c : Char)
  | any
  | cls (items : List ClassItem) (negated : Bool)
  | start
  | stop
  | seq (a b : Re)
  | alt (a b : Re)
  /-- `min` to `max` (`none`: unbounded) repetitions, greedy -/
  | rep (r : Re) (min : Nat) (max : Option Nat)
deriving Repr, Inhabited

def isDigit (c : Char) : Bool := c.isDigit
def isWord (c : Char) : Bool := c.isAlphanum || c == '_'
def isSpace (c : Char) : Bool := c == ' ' || c == '\t' || c == '\n' || c == '\r' || c.val == 11 || c.val == 12

def namedClass (name : Char) (c : Char) : Bool :=
  match name with
  | 'd' => isDigit c
  | 'w' => isWord c
  | 's' => isSpace c
  | _ => false

def ClassItem.matches (ci : Bool) (c : Char) : ClassItem → Bool
  | .char x => if ci then x.toLower == c.toLower else x == c
  | .range lo hi =>
    let inRange (d : Char) := lo.val ≤ d.val && d.val ≤ hi.val
    inRange c || (ci && (inRange c.toLower || inRange c.toUpper))
  | .named n neg => namedClass n c != neg

/-! ## Parsing -/

abbrev Parser := StateT (List Char) (Except String)

private def peek : Parser (Option Char) := do return (← get).head?
private def next : Parser Char := do
  match ← get with
  | c :: rest => set rest; return c
  | [] => throw "unexpected end of pattern"

private def escapeChar (c : Char) : Char :=
  match c with
  | 'n' => '\n'
  | 't' => '\t'
  | 'r' => '\r'
  | c => c

/-- After `\` -/
private def parseEscape : Parser Re := do
  let c ← next
  match c with
  | 'd' | 'w' | 's' => return .cls [.named c false] false
  | 'D' => return .cls [.named 'd' true] false
  | 'W' => return .cls [.named 'w' true] false
  | 'S' => return .cls [.named 's' true] false
  | c => return .char (escapeChar c)

/-- After `[` -/
private partial def parseClass : Parser Re := do
  let negated ← do
    if (← peek) == some '^' then let _ ← next; pure true else pure false
  let rec items (acc : List ClassItem) (first : Bool) : Parser (List ClassItem) := do
    match ← next with
    | ']' => if first then items (acc ++ [.char ']']) false else return acc
    | '\\' =>
      let c ← next
      let item := match c with
        | 'd' | 'w' | 's' => ClassItem.named c false
        | 'D' => .named 'd' true
        | 'W' => .named 'w' true
        | 'S' => .named 's' true
        | c => .char (escapeChar c)
      items (acc ++ [item]) false
    | c =>
      match ← get with
      | '-' :: hi :: rest =>
        if hi == ']' then items (acc ++ [.char c]) false
        else
          set rest
          if hi.val < c.val then throw s!"invalid range {c}-{hi}"
          items (acc ++ [.range c hi]) false
      | _ => items (acc ++ [.char c]) false
  return .cls (← items [] true) negated

private partial def parseNat (acc : Option Nat) : Parser (Option Nat) := do
  match ← peek with
  | some c => if c.isDigit then let _ ← next; parseNat (some ((acc.getD 0) * 10 + (c.val - '0'.val).toNat)) else return acc
  | none => return acc

mutual
private partial def parseAlt : Parser Re := do
  let first ← parseSeq
  if (← peek) == some '|' then
    let _ ← next
    return .alt first (← parseAlt)
  else return first

private partial def parseSeq : Parser Re := do
  match ← peek with
  | none | some '|' | some ')' => return .empty
  | _ =>
    let a ← parseQuantified
    let b ← parseSeq
    return match b with
      | .empty => a
      | b => .seq a b

private partial def parseQuantified : Parser Re := do
  let atom ← parseAtom
  quantify atom

private partial def quantify (atom : Re) : Parser Re := do
  match ← peek with
  | some '*' => let _ ← next; skipLazy; quantify (.rep atom 0 none)
  | some '+' => let _ ← next; skipLazy; quantify (.rep atom 1 none)
  | some '?' => let _ ← next; skipLazy; quantify (.rep atom 0 (some 1))
  | some '{' =>
    let saved ← get
    let _ ← next
    match ← parseNat none with
    | none => set saved; return atom
    | some lo =>
      match ← next with
      | '}' => skipLazy; quantify (.rep atom lo (some lo))
      | ',' =>
        let hi ← parseNat none
        if (← next) != '}' then throw "unterminated {"
        if hi.any (· < lo) then throw "invalid repetition range"
        skipLazy; quantify (.rep atom lo hi)
      | _ => throw "invalid repetition"
  | _ => return atom

/-- A trailing `?` (lazy) or `+` (possessive) only changes how a match is found, not whether there is one -/
private partial def skipLazy : Parser Unit := do
  if (← peek) == some '?' || (← peek) == some '+' then let _ ← next

private partial def parseAtom : Parser Re := do
  match ← next with
  | '.' => return .any
  | '^' => return .start
  | '$' => return .stop
  | '\\' => parseEscape
  | '[' => parseClass
  | '(' =>
    if (← get).take 2 == ['?', ':'] then let _ ← next; let _ ← next
    let r ← parseAlt
    if (← next) != ')' then throw "unbalanced parenthesis"
    return r
  | ')' => throw "unbalanced parenthesis"
  | '*' | '+' | '?' => throw "quantifier without operand"
  | c => return .char c
end

/-- Parse a pattern, or say why it isn't one -/
def parse (pattern : String) : Except String Re := do
  let (r, rest) ← parseAlt.run pattern.toList
  if !rest.isEmpty then throw "unbalanced parenthesis"
  return r

/-! ## Matching -/

/-- Match `r` against `s` at position `i` (`s` as an array), calling `k` with every position a match can end at,
greediest first, until `k` succeeds -/
partial def matchAt (ci : Bool) (s : Array Char) (r : Re) (i : Nat) (k : Nat → Bool) : Bool :=
  match r with
  | .empty => k i
  | .char c => i < s.size && (if ci then s[i]!.toLower == c.toLower else s[i]! == c) && k (i + 1)
  | .any => i < s.size && s[i]! != '\n' && k (i + 1)
  | .cls items neg => i < s.size && (items.any (·.matches ci s[i]!) != neg) && k (i + 1)
  | .start => i == 0 && k i
  | .stop => i == s.size && k i
  | .seq a b => matchAt ci s a i fun j => matchAt ci s b j k
  | .alt a b => matchAt ci s a i k || matchAt ci s b i k
  | .rep r min max =>
    -- Greedy: one more repetition first. An empty one only counts towards `min`, so this can't loop forever
    let rec go (n i : Nat) : Bool :=
      (max.all (n < ·) && matchAt ci s r i fun j => (j != i || n < min) && go (n + 1) j) || (n ≥ min && k i)
    go 0 i

/-- Whether `r` matches somewhere in `s`, ignoring case -/
def Re.containsMatch (r : Re) (s : String) : Bool :=
  let chars := s.toList.toArray
  (List.range (chars.size + 1)).any fun i => matchAt true chars r i fun _ => true

/-- Whether `pattern` matches somewhere in `s`, ignoring case. An invalid pattern matches nothing -/
def search (pattern s : String) : Bool :=
  match parse pattern with
  | .ok r => r.containsMatch s
  | .error _ => false

end DwmacCore.Regex
