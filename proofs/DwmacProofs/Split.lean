import DwmacCore.Split

/-! # `split`'s segments are disjoint, and within the length -/

namespace DwmacCore

namespace Span

/-- The spans don't share a pixel -/
def Disjoint (a b : Span) : Prop := a.stop ≤ b.off ∨ b.stop ≤ a.off

/-- The span lies within `[0, total)` -/
def Within (s : Span) (total : Nat) : Prop := s.stop ≤ total

theorem Disjoint.symm {a b : Span} (h : a.Disjoint b) : b.Disjoint a := Or.symm h

end Span

theorem effectiveGap_fits (total gap n : Nat) : (n - 1) * effectiveGap total gap n ≤ total := by
  unfold effectiveGap; split <;> simp_all

theorem segOff_succ (q r gap i : Nat) : segOff q r gap (i + 1) = segOff q r gap i + segLen q r i + gap := by
  unfold segOff segLen
  rw [Nat.succ_mul]
  split <;> omega

theorem segOff_mono (q r gap : Nat) {i j : Nat} (h : i ≤ j) : segOff q r gap i ≤ segOff q r gap j := by
  induction h with
  | refl => exact Nat.le_refl _
  | step _ ih => rw [segOff_succ]; omega

@[simp] theorem split_length (total gap n : Nat) : (split total gap n).length = n := by
  simp [split]

/-- Segments come in order, at least a gap apart, so no two of them overlap -/
theorem split_pairwise_disjoint (total gap n : Nat) : (split total gap n).Pairwise Span.Disjoint := by
  unfold split
  simp only [List.pairwise_map]
  refine List.pairwise_lt_range.imp fun {i j} hij => Or.inl ?_
  simp only [Span.stop]
  have h := segOff_mono ((total - (n - 1) * effectiveGap total gap n) / n)
    ((total - (n - 1) * effectiveGap total gap n) % n) (effectiveGap total gap n) (Nat.succ_le_of_lt hij)
  rw [segOff_succ] at h
  omega

/-- Every segment lies within `[0, total)` -/
theorem split_within (total gap n : Nat) : ∀ s ∈ split total gap n, s.Within total := by
  intro s hs
  simp only [split, List.mem_map, List.mem_range] at hs
  obtain ⟨i, hi, rfl⟩ := hs
  simp only [Span.Within, Span.stop]
  -- Segment `i` ends where segment `i + 1` starts (minus the gap), and segment `n` would start at `total + gap`
  generalize hg : effectiveGap total gap n = g
  have fits : (n - 1) * g ≤ total := hg ▸ effectiveGap_fits total gap n
  generalize hav : total - (n - 1) * g = avail
  have h := segOff_mono (avail / n) (avail % n) g (Nat.succ_le_of_lt hi)
  rw [segOff_succ] at h
  have hn : segOff (avail / n) (avail % n) g n = avail + n * g := by
    have hr : avail % n < n := Nat.mod_lt _ (by omega)
    have := Nat.div_add_mod avail n
    unfold segOff
    rw [Nat.mul_add, Nat.min_eq_right (Nat.le_of_lt hr)]
    omega
  have hng : n * g = (n - 1) * g + g := by
    rw [← Nat.succ_mul, Nat.succ_eq_add_one, Nat.sub_add_cancel (by omega)]
  omega

end DwmacCore
