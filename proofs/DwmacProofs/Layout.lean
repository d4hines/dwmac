import DwmacCore.Layout
import DwmacProofs.Model
import DwmacProofs.MasterStack

/-! # The layout only shows windows of shown workspaces, so never a scratchpad's home -/

namespace DwmacCore.Model

namespace State

/-- Every effect of `placeWs ws` is about a window of `ws` -/
theorem placeWs_target (s : State) (ws : WsName) (mon : MonId) (g : Geom) :
    ∀ e ∈ s.placeWs ws mon g, ∃ w ∈ s.windows, w.id = e.target ∧ w.ws = ws := by
  intro e he
  simp only [placeWs, List.mem_append, List.mem_map] at he
  rcases he with (⟨w, hw, rfl⟩ | ⟨⟨w, r⟩, hwr, rfl⟩) | ⟨w, hw, rfl⟩
  · simp only [List.mem_filter, beq_iff_eq] at hw
    exact ⟨w, hw.1, rfl, hw.2⟩
  · have hw := (List.of_mem_zip hwr).1
    split at hw
    · simp only [List.mem_filter, beq_iff_eq] at hw
      refine ⟨w, hw.1.1, ?_, hw.1.2⟩
      split <;> simp [Effect.target]
    · simp at hw
  · simp only [List.mem_filter, beq_iff_eq] at hw
    exact ⟨w, hw.1.1, rfl, hw.1.2⟩

theorem hideWs_hides (s : State) (ws : WsName) : ∀ e ∈ s.hideWs ws, e.shows = false := by
  intro e he
  simp only [hideWs] at he
  split at he
  · simp at he
  · simp only [List.mem_map] at he
    obtain ⟨_, _, rfl⟩ := he
    rfl

theorem hideWs_centers (s : State) (ws : WsName) : ∀ e ∈ s.hideWs ws, e.centers = false := by
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

/-- Every effect of `placeWs` that centers a window is about a window `center-floating-windows` hasn't centered yet -/
theorem placeWs_centers (s : State) (ws : WsName) (mon : MonId) (g : Geom) :
    ∀ e ∈ s.placeWs ws mon g, e.centers = true → ∃ w ∈ s.windows, w.id = e.target ∧ w.centered = false := by
  intro e he hc
  obtain ⟨_, _, _, rfl⟩ : ∃ id m v, e = .float id m v true := by cases e <;> simp_all [Effect.centers]
  simp only [placeWs, List.mem_append, List.mem_map] at he
  rcases he with (⟨w, _, he⟩ | ⟨⟨w, r⟩, _, he⟩) | ⟨w, hw, he⟩
  · simp at he
  · revert he; split <;> simp
  · simp only [List.mem_filter] at hw
    refine ⟨w, hw.1.1, ?_⟩
    simp_all [Effect.target]

/-- **`center-floating-windows` centers a window once**: every effect of the layout that centers a window is about a
window that isn't marked centered. Swift marks it (`Op.centered`) once it has centered it, so it then stays where the
user drags or `move`s it, until it stops floating -/
theorem layout_centers_uncentered (s : State) (showAll : Bool) :
    ∀ e ∈ s.layout showAll, e.centers = true → ∃ w ∈ s.windows, w.id = e.target ∧ w.centered = false := by
  intro e he hc
  unfold layout at he
  split at he
  · simp only [List.mem_flatMap] at he
    obtain ⟨ws, _, he⟩ := he
    split at he
    · exact s.placeWs_centers ws _ _ e he hc
    · simp at he
  · simp only [List.mem_append, List.mem_flatMap] at he
    rcases he with ⟨⟨mon, ws⟩, _, he⟩ | ⟨ws, _, he⟩
    · split at he
      · exact s.placeWs_centers ws mon _ e he hc
      · simp at he
    · rw [hideWs_centers s ws e he] at hc; contradiction

end State

/-- After Swift reports it centered window `id`, the layout doesn't center it again -/
theorem State.centered_not_recentered (s : State) (id : WinId) (showAll : Bool) :
    ∀ e ∈ (s.raw (.centered id)).layout showAll, e.target = id → e.centers = false := by
  intro e he hid
  cases hc : e.centers
  · rfl
  · obtain ⟨w, hw, hwid, hwc⟩ := State.layout_centers_uncentered _ showAll e he hc
    simp only [State.raw, State.mapWins, List.mem_map] at hw
    obtain ⟨w0, _, rfl⟩ := hw
    split at hwc
    · simp at hwc
    · rename_i hne; simp_all

/-- In a state with the invariant, the layout never puts a window that's in a scratchpad's home on screen -/
theorem State.layout_hides_homes {s : State} (hinv : Inv s) :
    ∀ e ∈ s.layout false, e.shows = true → ∀ w ∈ s.windows, w.id = e.target → w.ws.isHome = false := by
  intro e he hs w hw hid
  obtain ⟨w', hw', hid', hvis⟩ := s.layout_shows_visible e he hs
  have := nodup_map_inj hinv.windows.idsNodup hw hw' (hid.trans hid'.symm)
  subst this
  exact hinv.screens.noHome _ hvis

/-- **A hidden scratchpad stays hidden**: in every state dwmac can reach, the layout never puts a window that's in a
scratchpad's home on screen -/
theorem layout_never_shows_home (scratchpads : List Scratchpad) (ops : List State.Op) :
    let s := ops.foldl State.step (State.init scratchpads)
    ∀ e ∈ s.layout false, e.shows = true → ∀ w ∈ s.windows, w.id = e.target → w.ws.isHome = false :=
  State.layout_hides_homes (reachable_inv scratchpads ops)

namespace State

/-- **A workspace's layout doesn't depend on focus**: so in monocle, `rotate` and `focus` only change which window
macOS keeps in front, and every window keeps covering the workspace -/
theorem placeWs_focus_irrelevant (s : State) (ws : WsName) (mon : MonId) (g : Geom) (fws : WsName)
    (fw : Option WinId) (mru : List WinId) :
    ({ s with focusWs := fws, focusWin := fw, mru } : State).placeWs ws mon g = s.placeWs ws mon g := rfl

/-- **Monocle covers the workspace with every tiled window** (dwmac's `fullscreen`, xmonad's `Full`) -/
theorem placeWs_monocle (s : State) (ws : WsName) (mon : MonId) (g : Geom) (noOuterGaps : Bool)
    (hm : (s.props ws).monocle = some noOuterGaps) (hl : (s.props ws).layout = .masterStack)
    (w : Win) (hw : w ∈ s.windows) (hws : w.ws = ws) (hf : w.floating = false) (hp : w.parked = false) :
    .tile w.id (if noOuterGaps then g.visible else g.padded) g.padded true ∈ s.placeWs ws mon g := by
  simp only [placeWs, hl, hm, List.mem_append, List.mem_map]
  left; right
  have hmem : w ∈ s.windows.filter (fun a => !a.floating && !a.parked && a.ws == ws) := by simp [hw, hws, hf, hp]
  obtain ⟨i, hi, hwi⟩ := List.getElem_of_mem hmem
  refine ⟨_, List.getElem_mem (l := List.zip _ _) (n := i) (by simpa [DwmacCore.masterStack_length] using hi), ?_⟩
  simp [hwi]

end State

end DwmacCore.Model
