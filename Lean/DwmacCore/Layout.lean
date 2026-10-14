import DwmacCore.Model

/-!
# The layout, as effects

`State.layout` says where every window goes, as a list of effects for Swift to carry out with AX calls:

* windows of shown workspaces are unhidden, then tiled (`MasterStack`) or floated on their monitor
* windows of the other workspaces are hidden in a corner of their workspace's monitor

Swift only performs them. proofs/DwmacProofs/Layout.lean ties the layout to the model's proofs: a window of a
workspace that isn't shown is only ever hidden, so (with `home_never_shown`) a scratchpad's hidden windows stay
hidden.
-/

namespace DwmacCore.Model

inductive Corner where
  | bottomLeft
  | bottomRight
deriving DecidableEq, Repr, Inhabited

inductive Effect where
  /-- Bring the window back from its corner. `restore`: put it back where it was (floating windows; the layout
  places the others). `monitor` is its workspace monitor's rect -/
  | unhide (id : WinId) (monitor : IRect) (restore : Bool)
  /-- Set the window's frame. `virtual` is the workspace's rect, which mouse resizing works in -/
  | tile (id : WinId) (rect virtual : IRect) (fullscreen : Bool)
  /-- Keep a floating window on `mon` (whose visible rect is `visible`), centering it if `center`: once after it
  becomes floating (Swift then reports `Op.centered`), so it stays where the user drags or `move`s it -/
  | float (id : WinId) (mon : MonId) (visible : IRect) (center : Bool)
  /-- Move the window off screen, into `corner` of `visible` -/
  | hide (id : WinId) (corner : Corner) (visible : IRect)
deriving DecidableEq, Repr, Inhabited

namespace Effect

def target : Effect → WinId
  | unhide id .. | tile id .. | float id .. | hide id .. => id

/-- Whether the effect centers the window -/
def centers : Effect → Bool
  | float _ _ _ center => center
  | _ => false

/-- Whether the effect puts the window on screen -/
def shows : Effect → Bool
  | hide .. => false
  | _ => true

end Effect

namespace State

/-- The corner of `g` to hide windows in: the one fewer of the probes around it land on another monitor (a window
hidden there would peek out onto it). Probes right next to the corner count most -/
def hideCorner (s : State) (g : Geom) : Corner :=
  let r := g.rect
  let maxX := r.x + r.w
  let maxY := r.y + r.h
  let xOff : Int := r.w / 10
  let yOff : Int := r.h / 10
  let contains (m : IRect) (p : Int × Int) : Nat :=
    if m.x ≤ p.1 ∧ p.1 < m.x + m.w ∧ m.y ≤ p.2 ∧ p.2 < m.y + m.h then 1 else 0
  let score (p1 p2 p3 : Int × Int) : Nat :=
    (s.geometry.map fun (_, m) => contains m.rect p1 + contains m.rect p2 + 10 * contains m.rect p3).sum
  if score (r.x - 2, maxY - yOff) (r.x + xOff, maxY + 2) (r.x - 2, maxY + 2) <
      score (maxX + 2, maxY - yOff) (maxX - xOff, maxY + 2) (maxX + 2, maxY + 2)
  then .bottomLeft else .bottomRight

/-- Where `ws`'s windows go when it's shown on `mon` -/
def placeWs (s : State) (ws : WsName) (mon : MonId) (g : Geom) : List Effect :=
  let wins := s.windows.filter (·.ws == ws)
  let p := s.props ws
  let tiled := if p.layout == .masterStack then wins.filter (fun w => !w.floating && !w.parked) else []
  -- One point shorter than the workspace, as dwmac always did
  let tiles := masterStack
    { w := g.padded.w, h := g.padded.h - 1, gapH := g.gapH, gapV := g.gapV, mfactPermille := p.mfactPermille,
      orientation := p.orientation, masterPosition := if s.settings.masterAtStop then .stop else .start }
    tiled.length
  -- Monocle: every tiled window covers the workspace; macOS keeps the focused one in front
  let tileFx := (tiled.zip tiles).map fun (w, r) => match p.monocle with
    | some noOuterGaps => .tile w.id (if noOuterGaps then g.visible else g.padded) g.padded true
    | none => .tile w.id ⟨g.padded.x + r.x, g.padded.y + r.y, r.w, r.h⟩ g.padded false
  let floatFx := (wins.filter (fun w => w.floating && !w.parked)).map fun w =>
    .float w.id mon g.visible (s.settings.centerFloating && !w.centered)
  wins.map (fun w => .unhide w.id g.rect (w.floating && !w.parked)) ++ tileFx ++ floatFx

/-- Hide `ws`'s windows in a corner of its monitor -/
def hideWs (s : State) (ws : WsName) : List Effect :=
  match s.geomOf (s.workspaceMonitor ws) with
  | none => []
  | some g => (s.windows.filter (·.ws == ws)).map fun w => .hide w.id (s.hideCorner g) g.visible

/-- Workspaces that have windows, without duplicates -/
def windowWorkspaces (s : State) : List WsName := (s.windows.map (·.ws)).eraseDups

/-- The effects that make the screen match the state. Shown workspaces go first, so windows appear before others
disappear. With `showAll` (dwmac is disabled), every workspace is laid out on its monitor and nothing is hidden -/
def layout (s : State) (showAll : Bool) : List Effect :=
  if showAll then
    s.windowWorkspaces.flatMap fun ws =>
      let mon := s.workspaceMonitor ws
      match s.geomOf mon with
      | some g => s.placeWs ws mon g
      | none => []
  else
    (s.screens.flatMap fun (mon, ws) =>
      match s.geomOf mon with
      | some g => s.placeWs ws mon g
      | none => [])
    ++ (s.windowWorkspaces.filter (!s.visible.contains ·)).flatMap s.hideWs

end State

end DwmacCore.Model
