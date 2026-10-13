import DwmacCore.Cli

/-! The command line: parse results and the exact error messages (ported from the Swift parser's tests) -/

namespace DwmacCore.Cli.Test

def p (raw : String) : Parsed := (parseString raw).1
def err (raw : String) : Option String := match p raw with | .fail m => some m | _ => none
def ok (raw : String) : Bool := match p raw with | .cmd _ _ => true | _ => false
def fields (raw : String) : Fields := match p raw with | .cmd _ f => f | _ => []

-- The loop: unknown, duplicated and conflicting options, mandatory arguments
#guard err "workspace my mail" == some "ERROR: Unknown argument 'mail'"
#guard err "workspace 'my mail'" == some "ERROR: Whitespace characters are forbidden in workspace names"
#guard err "workspace" == some "ERROR: Argument '(<workspace-name>|next|prev)' is mandatory"
#guard err "summon-workspace" == some "ERROR: Argument '<workspace>' is mandatory"
#guard err "scratchpad" == some "ERROR: Argument '<scratchpad>' is mandatory"
#guard err "focus --boundaries workspace --boundaries workspace next" == some "ERROR: Duplicated option '--boundaries'"
#guard err "list-modes --json --count" == some "ERROR: Conflicting options: --count, --json"
#guard err "list-modes --current --count" == some "ERROR: Conflicting options: --count, --current"
#guard err "foo" == some "Unrecognized subcommand 'foo'"
#guard err "" == some "Can't parse empty string command"
#guard (p "workspace --help") == .help "USAGE: workspace [-h|--help] [--auto-back-and-forth] [--fail-if-noop] <workspace-name>\n   OR: workspace [-h|--help] [--wrap-around] [--stdin|--no-stdin] (next|prev)"

-- workspace
#guard fields "workspace next" == [("target", .str "next")]
#guard fields "workspace --auto-back-and-forth W" == [("target", .str "=W"), ("_autoBackAndForth", .bool true)]
#guard err "workspace --wrap-around W" == some "--wrapAround requires using (next|prev) argument"
#guard err "workspace --auto-back-and-forth next" == some "--auto-back-and-forth is incompatible with (next|prev)"
#guard ok "workspace next --wrap-around"
#guard err "workspace --stdin foo" == some "--stdin and --no-stdin require using (next|prev) argument"
#guard (fields "workspace --stdin next").get? "explicitStdinFlag" == some (.bool true)
#guard (fields "workspace --no-stdin next").get? "explicitStdinFlag" == some (.bool false)

-- focus
#guard ((err "focus --boundaries left").map (·.contains "Possible values")) == some true
#guard ok "focus --boundaries workspace next"
#guard err "focus --window-id 42 --ignore-floating" == some "--window-id is incompatible with other options"
#guard err "focus --boundaries all-monitors-outer-frame next" == some "(next|prev) only supports --boundaries workspace"
#guard err "focus" == some "Mandatory argument is missing. (next|prev), --window-id or --index is required"

-- layout
#guard (fields "layout tiling floating").get? "toggleBetween" == some (.strs ["tiling", "floating"])
#guard err "layout foo" == some "ERROR: Can't parse 'foo'\n       Possible values: (master-stack|horizontal|vertical|h_master-stack|v_master-stack|tiling|floating)"

-- resize and mfact: negative numbers aren't flags
#guard (fields "resize smart -10").get? "units" == some (.str "-10")
#guard ok "resize smart +0.5" && ok "resize width 10" && ok "mfact -0.05" && ok "mfact 0.5"
#guard err "resize s 10" == some "ERROR: Can't parse 's'.\n       Possible values: (width|height|smart|smart-opposite)"
#guard err "resize smart foo" == some "ERROR: <number> argument must be a number"

-- move-node-to-monitor
#guard fields "move-node-to-monitor next" == [("target", .str "next")]
#guard ok "move-node-to-monitor --fail-if-noop main"
#guard err "move-node-to-monitor --fail-if-noop next" == some "--fail-if-noop is incompatible with (left|down|up|right|next|prev)"
#guard err "move-node-to-monitor --fail-if-noop left" == some "--fail-if-noop is incompatible with (left|down|up|right|next|prev)"
#guard ((err "focus-monitor '('").map (·.startsWith "ERROR: Can't parse '(' regex")) == some true

-- list-*
#guard ok "list-monitors" && ok "list-monitors --focused" && ok "list-monitors --count"
#guard err "list-monitors --format %{monitor-id} --count" == some "ERROR: Conflicting options: --count, --format"
#guard ok "list-apps --macos-native-hidden" && ok "list-apps --macos-native-hidden no" && ok "list-apps --count"
#guard (fields "list-apps --macos-native-hidden no").get? "macosHidden" == some (.bool false)
#guard err "list-apps --format %{app-bundle-id} --count" == some "ERROR: Conflicting options: --count, --format"
#guard ok "list-workspaces --all" && !ok "list-workspaces --all --visible" && !ok "list-workspaces --focused --visible"
#guard !ok "list-workspaces --focused --all" && !ok "list-workspaces --visible" && ok "list-workspaces --visible --monitor 2"
#guard ok "list-workspaces --monitor focused" && !ok "list-workspaces --focused --monitor 2"
#guard err "list-workspaces --empty" == some "Mandatory option is not specified (--all|--focused|--monitor)"
#guard err "list-workspaces --all --focused --monitor mouse" == some "ERROR: Conflicting options: --all, --focused, --monitor"
#guard err "list-windows --pid 1" == some "Mandatory option is not specified (--focused|--all|--monitor|--workspace)"
#guard ok "list-windows --workspace M --pid 1" && ok "list-windows --all" && ok "list-windows --monitor mouse"
#guard err "list-windows --pid 1 --focused" == some "--focused conflicts with other \"filtering\" flags"
#guard err "list-windows --pid 1 --all" == some "--all conflicts with \"filtering\" flags. Please use '--monitor all' instead of '--all' alias"
#guard err "list-windows --all --workspace M" == some "ERROR: Conflicting options: --all, --workspace"
#guard err "list-windows --all --focused --monitor mouse --workspace focused" == some "ERROR: Conflicting options: --all, --focused, --workspace"
#guard err "list-windows --all --format '%{right-padding}' --json" == some "%{right-padding} interpolation variable is not allowed when --json is used"
#guard err "list-windows --all --format '%{window-title} |' --json" == some "Only interpolation variables and spaces are allowed in '--format' when '--json' is used"
#guard ok "list-windows --all --format '%{window-title}' --json"
-- --all is normalized to --monitor all
#guard fields "list-windows --all" == [("monitors", .strs ["all"])]
#guard (fields "list-windows --all --format '%{window-id}%{right-padding} | x'").get? "_format"
  == some (.strs ["v:window-id", "v:right-padding", "l: | x"])

-- Splitting command strings
#guard (splitArgs "  foo   bar ").toOption == some ["foo", "bar"] && (splitArgs "foo 'bar baz'").toOption == some ["foo", "bar baz"]
#guard (match splitArgs "foo 'bar" with | .error e => e | .ok _ => "") == "Last quote ' isn't closed"
#guard (parseString "exec-and-forget open -a Kitty").1 == .cmd "exec-and-forget" [("bashScript", .str " open -a Kitty")]

end DwmacCore.Cli.Test
