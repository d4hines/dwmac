/-!
# Splitting a length into segments

The one-dimensional core of every tiling layout: cut `total` pixels into `n` segments separated by `gap`
pixels. Lengths are whole pixels (macOS reports window frames in whole points); the remainder of the division
goes to the first segments, one pixel each, so the segments always fill `total` exactly.

If the gaps alone don't fit (`(n - 1) * gap > total`), they are dropped rather than pushing segments outside
`total`. That keeps every theorem below unconditional.
-/

namespace DwmacCore

/-- A segment of a line: `off` is its start, `len` its length. It covers `[off, off + len)` -/
structure Span where
  off : Nat
  len : Nat
deriving Repr, DecidableEq, Inhabited

namespace Span

def stop (s : Span) : Nat := s.off + s.len

/-- The spans don't share a pixel -/
def Disjoint (a b : Span) : Prop := a.stop ≤ b.off ∨ b.stop ≤ a.off

/-- The span lies within `[0, total)` -/
def Within (s : Span) (total : Nat) : Prop := s.stop ≤ total

theorem Disjoint.symm {a b : Span} (h : a.Disjoint b) : b.Disjoint a := Or.symm h

end Span

/-- The gap actually used: dropped if the `n - 1` gaps don't fit in `total` -/
def effectiveGap (total gap n : Nat) : Nat := if (n - 1) * gap ≤ total then gap else 0

theorem effectiveGap_fits (total gap n : Nat) : (n - 1) * effectiveGap total gap n ≤ total := by
  unfold effectiveGap; split <;> simp_all

/-- Start of segment `i`, given each segment is `q` pixels plus one for the first `r` -/
def segOff (q r gap i : Nat) : Nat := i * (q + gap) + min i r

def segLen (q r i : Nat) : Nat := q + if i < r then 1 else 0

theorem segOff_succ (q r gap i : Nat) : segOff q r gap (i + 1) = segOff q r gap i + segLen q r i + gap := by
  unfold segOff segLen
  rw [Nat.succ_mul]
  split <;> omega

theorem segOff_mono (q r gap : Nat) {i j : Nat} (h : i ≤ j) : segOff q r gap i ≤ segOff q r gap j := by
  induction h with
  | refl => exact Nat.le_refl _
  | step _ ih => rw [segOff_succ]; omega

/-- Split `total` into `n` segments separated by `gap` (see the module doc) -/
def split (total gap n : Nat) : List Span :=
  let g := effectiveGap total gap n
  let avail := total - (n - 1) * g
  (List.range n).map fun i => ⟨segOff (avail / n) (avail % n) g i, segLen (avail / n) (avail % n) i⟩

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
