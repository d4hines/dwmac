import DwmacCore.Keys

/-! # A key press triggers the most specific binding of the active mode -/

namespace DwmacCore.Keys

def step (acc : Option Binding) (b : Binding) : Option Binding :=
  match acc with
  | some a => if bits b.modifiers > bits a.modifiers then some b else some a
  | none => some b

theorem foldl_step (l : List Binding) (acc : Option Binding) (r : Binding) (h : l.foldl step acc = some r) :
    (r ∈ l ∨ acc = some r) ∧ (∀ x ∈ l, bits x.modifiers ≤ bits r.modifiers) ∧
      (∀ a, acc = some a → bits a.modifiers ≤ bits r.modifiers) := by
  induction l generalizing acc with
  | nil => simp at h; subst h; simp
  | cons b l ih =>
    simp only [List.foldl_cons] at h
    obtain ⟨hmem, hall, hacc⟩ := ih _ h
    have hb := hacc
    refine ⟨?_, ?_, ?_⟩
    · rcases hmem with hm | hm
      · exact .inl (List.mem_cons_of_mem _ hm)
      · cases acc with
        | none => simp [step] at hm; exact .inl (hm ▸ List.mem_cons_self ..)
        | some a =>
          simp only [step] at hm
          split at hm
          · simp at hm; exact .inl (hm ▸ List.mem_cons_self ..)
          · exact .inr hm
    · intro x hx
      rcases List.mem_cons.mp hx with rfl | hx
      · cases acc with
        | none => exact hb _ rfl
        | some a =>
          simp only [step] at hb
          split at hb
          · exact hb _ rfl
          · have := hb a rfl; omega
      · exact hall x hx
    · intro a ha
      subst ha
      simp only [step] at hb
      split at hb
      · have := hb _ rfl; omega
      · exact hb a rfl

theorem best_spec (bs : List Binding) (code flags : Nat) (b : Binding) (h : best bs code flags = some b) :
    b ∈ bs ∧ b.triggeredBy code flags ∧
      ∀ b' ∈ bs, b'.triggeredBy code flags → bits b'.modifiers ≤ bits b.modifiers := by
  have h' : (bs.filter (·.triggeredBy code flags)).foldl step none = some b := by
    exact h
  obtain ⟨hmem, hall, _⟩ := foldl_step _ none b h'
  have hmem : b ∈ bs.filter (·.triggeredBy code flags) := by simpa using hmem
  rw [List.mem_filter] at hmem
  exact ⟨hmem.1, hmem.2, fun b' hb' ht => hall b' (List.mem_filter.mpr ⟨hb', ht⟩)⟩

/-- **A key press triggers the most specific binding**: what `lookup` finds is a binding of the active mode for that
key, whose modifiers are all held, and no other such binding has more modifiers. So `alt-shift-j` wins over `alt-j`
while shift is held, and nothing triggers while dwmac is disabled (no active mode) -/
theorem Keymap.lookup_spec (k : Keymap) (code flags : Nat) (b : Binding) (h : k.lookup code flags = some b) :
    ∃ m bs, k.mode = some m ∧ k.modes.lookup m = some bs ∧ b ∈ bs ∧ b.keyCode = code ∧
      has flags b.modifiers = true ∧
      ∀ b' ∈ bs, b'.triggeredBy code flags → bits b'.modifiers ≤ bits b.modifiers := by
  unfold Keymap.lookup at h
  cases hm : k.mode with
  | none => simp [hm] at h
  | some m =>
    cases hbs : k.modes.lookup m with
    | none => simp [hm, hbs] at h
    | some bs =>
      simp only [hm, hbs, Option.bind_some] at h
      obtain ⟨hmem, ht, hmax⟩ := best_spec bs code flags b h
      simp only [Binding.triggeredBy, Bool.and_eq_true, beq_iff_eq] at ht
      exact ⟨m, bs, rfl, hbs, hmem, ht.1, ht.2, hmax⟩

end DwmacCore.Keys
