import DwmacCore.Toml

namespace DwmacCore.Toml.Test
open Value

def ok (src : String) : List (String × Value) := (parse src).toOption.getD [("FAILED", .bool false)]
def err (src : String) : String := match parse src with | .ok _ => "" | .error e => e

#guard ok "a = 1\nb = 'x'\nc = \"y\\tz\"\nd = true" ==
  [("a", int 1), ("b", str "x"), ("c", str "y\tz"), ("d", bool true)]
#guard ok "x = -1_000\ny = 0xff\nz = 0o17\nw = 0b101\nf = 0.67\ng = 1e3\nh = -2.5E-2\ni = inf" ==
  [("x", int (-1000)), ("y", int 255), ("z", int 15), ("w", int 5), ("f", float "0.67"), ("g", float "1e3"),
   ("h", float "-2.5E-2"), ("i", float "inf")]
-- Tables, dotted keys, comments
#guard ok "# c\n[gaps]\ninner.horizontal = 10 # c\nouter.left =   10\n[mode.main.binding]\nalt-j = 'focus next'" ==
  [("gaps", table [("inner", table [("horizontal", int 10)]), ("outer", table [("left", int 10)])]),
   ("mode", table [("main", table [("binding", table [("alt-j", str "focus next")])])])]
-- Arrays across lines, with comments and a trailing comma; inline tables
#guard ok "a = [\n  'x', # one\n  'y',\n]\nb = [{ monitor.main = 1 }, 2]\nc = []" ==
  [("a", array [str "x", str "y"]), ("b", array [table [("monitor", table [("main", int 1)])], int 2]),
   ("c", array [])]
-- Arrays of tables, and a sub-table of the last one
#guard ok "[[w]]\nif.app-id = 'a'\nrun = 'x'\n[[w]]\nrun = ['y', 'z']\n[w.if]\napp-id = 'b'" ==
  [("w", array [table [("if", table [("app-id", str "a")]), ("run", str "x")],
    table [("run", array [str "y", str "z"]), ("if", table [("app-id", str "b")])]])]
-- Quoted keys, multi-line strings
#guard ok "\"a b\" = 1\n'c.d' = 2\ns = \"\"\"\nline1\nline2\"\"\"\nt = '''x\\y'''\nu = \"\"\"a \\\n   b\"\"\"" ==
  [("a b", int 1), ("c.d", int 2), ("s", str "line1\nline2"), ("t", str "x\\y"), ("u", str "a b")]
#guard ok "u = \"\\u00e9\"\nd = 1979-05-27T07:32:00Z\ne = 1979-05-27 07:32:00\nt = 07:32:00" ==
  [("u", str "é"), ("d", datetime "1979-05-27T07:32:00Z"), ("e", datetime "1979-05-27 07:32:00"),
   ("t", datetime "07:32:00")]
#guard ok "" == [] && ok "\n\n# only a comment\n" == []
#guard ok "a = 1\r\nb = 2\r\n" == [("a", int 1), ("b", int 2)]

-- Errors say where
#guard err "true" == "Unexpected end of file in a key-value pair (at line 1, column 5)"
#guard err "a = 1\na = 2" == "Key 'a' is defined twice (at line 2, column 6)"
#guard err "[a]\n[a]" == "Table 'a' is defined twice (at line 2, column 4)"
#guard err "a = 1\n[a]" == "Key 'a' is not a table (at line 2, column 4)"
#guard err "a = 'x" == "Unexpected end of file in a string (at line 1, column 7)"
#guard err "a = 01" == "Leading zeros are not allowed: '01' (at line 1, column 7)"
#guard err "a = 1 b = 2" == "Expected a newline, found 'b' (at line 1, column 7)"
#guard err "a = [1 2]" == "Expected ',' or ']' in an array, found '2' (at line 1, column 9)"
#guard err "a = \"\\q\"" == "Invalid escape sequence '\\q' (at line 1, column 8)"
#guard (err "a = nope").startsWith "Invalid value 'nope'"

end DwmacCore.Toml.Test
