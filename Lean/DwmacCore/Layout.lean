import DwmacCore.ModelProofs

/-!
# The layout, as effects

`State.layout` says where every window goes, as a list of effects for Swift to carry out with AX calls:

* windows of shown workspaces are unhidden, then tiled (`MasterStack`) or floated on their monitor
* windows of the other workspaces are hidden in a corner of their workspace's monitor

Swift only performs them. The theorem at the bottom ties the layout to the model's proofs: a window of a workspace
that isn't shown is only ever hidden, so (with `home_never_shown`) a scratchpad's hidden windows stay hidden.
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
  /-- Keep a floating window on `mon` (whose visible rect is `visible`), centering it if `center` -/
  | float (id : WinId) (mon : MonId) (visible : IRect) (center : Bool)
  /-- Move the window off screen, into `corner` of `visible` -/
  | hide (id : WinId) (corner : Corner) (visible : IRect)
deriving DecidableEq, Repr, Inhabited

namespace Effect

def target : Effect → WinId
  | unhide id .. | tile id .. | float id .. | hide id .. => id

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

/-- The window shown fullscreen on `ws`, if any: its most recent window, if it's fullscreen -/
def fullscreenOn (s : State) (ws : WsName) : Option (WinId × Bool) :=
  (s.mruWin ws).bind fun id => (s.window? id).bind fun w => if w.parked then none else w.fullscreen.map (id, ·)

/-- Where `ws`'s windows go when it's shown on `mon` -/
def placeWs (s : State) (ws : WsName) (mon : MonId) (g : Geom) : List Effect :=
  let wins := s.windows.filter (·.ws == ws)
  let p := s.props ws
  let full := s.fullscreenOn ws
  let fullRect (noOuterGaps : Bool) := if noOuterGaps then g.visible else g.padded
  let tiled := if p.layout == .masterStack then wins.filter (fun w => !w.floating && !w.parked) else []
  -- One point shorter than the workspace, as dwmac always did
  let tiles := masterStack
    { w := g.padded.w, h := g.padded.h - 1, gapH := g.gapH, gapV := g.gapV, mfactPermille := p.mfactPermille,
      orientation := p.orientation, masterPosition := if s.layoutCfg.masterAtStop then .stop else .start }
    tiled.length
  let place (w : Win) (r : Option Rect) : Option Effect :=
    match full with
    | some (id, noOuterGaps) => if id == w.id then some (.tile w.id (fullRect noOuterGaps) g.padded true) else none
    | none => none
    <|> r.map fun r => .tile w.id ⟨g.padded.x + r.x, g.padded.y + r.y, r.w, r.h⟩ g.padded false
  let tileFx := (tiled.zip tiles).filterMap fun (w, r) => place w (some r)
  let floatFx := (wins.filter (fun w => w.floating && !w.parked)).map fun w =>
    (place w none).getD (.float w.id mon g.visible s.layoutCfg.centerFloating)
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

/-! ## Proofs -/

/-- Every effect of `placeWs ws` is about a window of `ws` -/
theorem placeWs_target (s : State) (ws : WsName) (mon : MonId) (g : Geom) :
    ∀ e ∈ s.placeWs ws mon g, ∃ w ∈ s.windows, w.id = e.target ∧ w.ws = ws := by
  intro e he
  simp only [placeWs, List.mem_append, List.mem_map, List.mem_filterMap] at he
  rcases he with (⟨w, hw, rfl⟩ | ⟨⟨w, r⟩, hwr, he⟩) | ⟨w, hw, rfl⟩
  · simp only [List.mem_filter, beq_iff_eq] at hw
    exact ⟨w, hw.1, rfl, hw.2⟩
  · have hw := (List.of_mem_zip hwr).1
    split at hw
    · simp only [List.mem_filter, beq_iff_eq] at hw
      refine ⟨w, hw.1.1, ?_, hw.1.2⟩
      revert he; split <;> (try split) <;> intro he <;> simp at he <;> (try obtain ⟨_, rfl⟩ := he) <;> (try subst he) <;> rfl
    · simp at hw
  · simp only [List.mem_filter, beq_iff_eq] at hw
    refine ⟨w, hw.1.1, ?_, hw.1.2⟩
    split <;> (try split) <;> simp [Effect.target]

theorem hideWs_hides (s : State) (ws : WsName) : ∀ e ∈ s.hideWs ws, e.shows = false := by
  intro e he
  simp only [hideWs] at he
  split at he
  · simp at he
  · simp only [List.mem_map] at he
    obtain ⟨_, _, rfl⟩ := he
    rfl

/-- **Only shown workspaces' windows are put on screen**: unless dwmac is disabled, every effect that shows a window
is about a window of a shown workspace -/
theorem layout_shows_visible (s : State) :
    ∀ e ∈ s.layout false, e.shows = true → ∃ w ∈ s.windows, w.id = e.target ∧ w.ws ∈ s.visible := by
  intro e he hs
  simp only [layout, Bool.false_eq_true, ↓reduceIte, List.mem_append, List.mem_flatMap] at he
  rcases he with ⟨⟨mon, ws⟩, hscr, he⟩ | ⟨ws, _, he⟩
  · split at he
    · obtain ⟨w, hw, hid, rfl⟩ := s.placeWs_target ws mon _ e he
      exact ⟨w, hw, hid, List.mem_map.mpr ⟨(mon, w.ws), hscr, rfl⟩⟩
    · simp at he
  · rw [s.hideWs_hides ws e he] at hs; contradiction

end State

/-- **A hidden scratchpad stays hidden**: in every state dwmac can reach, the layout never puts a window that's in a
scratchpad's home on screen -/
theorem layout_never_shows_home (scratchpads : List Scratchpad) (ops : List State.Op) :
    let s := ops.foldl State.step (State.init scratchpads)
    ∀ e ∈ s.layout false, e.shows = true → ∀ w ∈ s.windows, w.id = e.target → w.ws.isHome = false := by
  intro s e he hs w hw hid
  obtain ⟨w', hw', hid', hvis⟩ := s.layout_shows_visible e he hs
  have := nodup_map_inj (reachable_inv scratchpads ops).windows.idsNodup hw hw' (hid.trans hid'.symm)
  subst this
  exact home_never_shown scratchpads ops _ hvis

end DwmacCore.Model
