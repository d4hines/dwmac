import DwmacCore.Command
import DwmacProofs.Layout

/-! # Commands keep the invariant, so no command shows a scratchpad's home -/

namespace DwmacCore.Model

open State

theorem Inv.notePrev {s t : State} (h : Inv t) : Inv (t.notePrev s) := by
  obtain ⟨⟨a, b, c, d⟩, ⟨e, f⟩, ⟨g, i⟩, j⟩ := h
  exact ⟨⟨a, b, c, d⟩, ⟨e, f⟩, ⟨g, i⟩, j⟩

theorem exec_inv {s : State} (h : Inv s) (i : Input) : Inv (s.exec i).1 :=
  Inv.notePrev (foldl_invariant Inv step (fun _ op h => step_inv h op) _ s h)

/-- The invariant holds in every state commands and events can reach -/
theorem reachable_exec_inv (scratchpads : List Scratchpad) (inputs : List Input) :
    Inv ((init scratchpads).execAll inputs) :=
  foldl_invariant Inv _ (fun _ i h => exec_inv h i) inputs _ (init_inv scratchpads)

/-- **No command shows a scratchpad home** -/
theorem home_never_shown_exec (scratchpads : List Scratchpad) (inputs : List Input) :
    ∀ ws ∈ ((init scratchpads).execAll inputs).visible, ws.isHome = false :=
  (reachable_exec_inv scratchpads inputs).screens.noHome

/-- **A hidden scratchpad stays hidden**, whatever commands and events led there -/
theorem layout_never_shows_home_exec (scratchpads : List Scratchpad) (inputs : List Input) :
    let s := (init scratchpads).execAll inputs
    ∀ e ∈ s.layout false, e.shows = true → ∀ w ∈ s.windows, w.id = e.target → w.ws.isHome = false :=
  State.layout_hides_homes (reachable_exec_inv scratchpads inputs)

end DwmacCore.Model
