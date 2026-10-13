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

/-- **Lean's focus catches up with macOS's**: when `syncFocus` waits for a request to land, or makes one, then a grace
period later (with macOS still showing `n`) it stops waiting and follows macOS, whatever closed meanwhile. So once macOS's focus stops changing,
Lean's agrees with it within two reports, and a summoned scratchpad has macOS's focus too, keeping it on top -/
theorem focusMove_converges (lean seen : Option WinId) (asked : Option (WinId × Nat)) (gone gone' : Bool)
    (n t t' : Nat) (hasked : ∀ a t₀, asked = some (a, t₀) → t₀ ≤ t) (ht : t + focusGraceMs ≤ t') :
    match focusMove lean seen asked gone n t with
    | .push l => focusMove lean (some n) (some (l, t)) gone' n t' = .adopt
    | .wait _ => focusMove lean seen asked gone' n t' = .adopt
    | _ => True := by
  have late : ¬ t' < t + focusGraceMs := by omega
  unfold focusMove
  cases gone
  all_goals
    rcases asked with _ | ⟨a, t₀⟩ <;> rcases lean with _ | l
    · by_cases hs : seen = some n <;> simp [hs]
    · by_cases hln : l = n
      · simp [hln]
      · by_cases hs : seen = some n <;> simp [hln, hs, late]
    · by_cases hs : (seen == some n || a == n) = true <;> simp [hs]
    · have := hasked a t₀ rfl
      by_cases hln : l = n
      · simp [hln]
      · by_cases hla : l = a
        · subst hla
          by_cases htt : t < t₀ + focusGraceMs
          · simp [hln, htt, show ¬ t' < t₀ + focusGraceMs by omega]
          · simp [hln, htt]
        · by_cases hs : (seen == some n || a == n) = true <;> simp [hln, hla, hs, late]

end DwmacCore.Model
