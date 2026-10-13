import DwmacCore.Layout
import DwmacProofs.Model

/-! # The layout only shows windows of shown workspaces, so never a scratchpad's home -/

namespace DwmacCore.Model

namespace State

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

end DwmacCore.Model
