import DwmacCore.MasterStack
import DwmacProofs.Split

/-! # Master-stack tiles: one per window, disjoint, within the workspace -/

namespace DwmacCore

namespace Rect

/-- The rects don't share a pixel: they are apart horizontally or vertically -/
def Disjoint (a b : Rect) : Prop := a.xSpan.Disjoint b.xSpan ∨ a.ySpan.Disjoint b.ySpan

/-- The rect lies within a `w` x `h` workspace -/
def Within (r : Rect) (w h : Nat) : Prop := r.xSpan.Within w ∧ r.ySpan.Within h

end Rect

/-- Two tiles in (main, cross) form are apart along one axis -/
def SpanPairDisjoint (a b : Span × Span) : Prop := a.1.Disjoint b.1 ∨ a.2.Disjoint b.2

def SpanPairWithin (main cross : Nat) (a : Span × Span) : Prop := a.1.Within main ∧ a.2.Within cross

theorem masterStackMain_spec (total gap permille : Nat) (masterAtStop : Bool) :
    (masterStackMain total gap permille masterAtStop).1.Disjoint (masterStackMain total gap permille masterAtStop).2 ∧
    (masterStackMain total gap permille masterAtStop).1.Within total ∧
    (masterStackMain total gap permille masterAtStop).2.Within total := by
  simp only [masterStackMain]
  generalize hg : (if gap ≤ total then gap else 0) = g
  have g_le : g ≤ total := by subst hg; split <;> omega
  have m_le : (total - g) * min permille 1000 / 1000 ≤ total - g := by
    calc (total - g) * min permille 1000 / 1000
        ≤ (total - g) * 1000 / 1000 := Nat.div_le_div_right (Nat.mul_le_mul_left _ (Nat.min_le_right _ _))
      _ = total - g := Nat.mul_div_cancel _ (by decide)
  generalize (total - g) * min permille 1000 / 1000 = m at m_le
  cases masterAtStop <;> simp [Span.Disjoint, Span.Within, Span.stop] <;> omega

@[simp] theorem masterStackSpans_length (main cross gapMain gapCross permille : Nat) (masterAtStop : Bool) (n : Nat) :
    (masterStackSpans main cross gapMain gapCross permille masterAtStop n).length = n := by
  match n with
  | 0 | 1 => rfl
  | n + 2 => simp [masterStackSpans]

theorem masterStackSpans_pairwise_disjoint (main cross gapMain gapCross permille : Nat) (masterAtStop : Bool) (n : Nat) :
    (masterStackSpans main cross gapMain gapCross permille masterAtStop n).Pairwise SpanPairDisjoint := by
  match n with
  | 0 | 1 => simp [masterStackSpans]
  | n + 2 =>
    have hmain := masterStackMain_spec main gapMain permille masterAtStop
    simp only [masterStackSpans, List.pairwise_cons, List.mem_map, List.pairwise_map]
    refine ⟨?_, ?_⟩
    · -- Master is apart from every stack tile along the main axis
      rintro _ ⟨c, _, rfl⟩
      exact Or.inl hmain.1
    · -- Stack tiles share the main axis span and are apart along the cross axis
      exact (split_pairwise_disjoint cross gapCross (n + 1)).imp Or.inr

theorem masterStackSpans_within (main cross gapMain gapCross permille : Nat) (masterAtStop : Bool) (n : Nat) :
    ∀ t ∈ masterStackSpans main cross gapMain gapCross permille masterAtStop n, SpanPairWithin main cross t := by
  match n with
  | 0 => simp [masterStackSpans]
  | 1 => simp [masterStackSpans, SpanPairWithin, Span.Within, Span.stop]
  | n + 2 =>
    have hmain := masterStackMain_spec main gapMain permille masterAtStop
    simp only [masterStackSpans, List.mem_cons, List.mem_map]
    rintro t (rfl | ⟨c, hc, rfl⟩)
    · exact ⟨hmain.2.1, by simp [Span.Within, Span.stop]⟩
    · exact ⟨hmain.2.2, split_within cross gapCross (n + 1) c hc⟩

/-! ## The layout theorems -/

/-- One tile per window -/
@[simp] theorem masterStack_length (p : Params) (n : Nat) : (masterStack p n).length = n := by
  unfold masterStack; split <;> simp

/-- No two tiles overlap -/
theorem masterStack_pairwise_disjoint (p : Params) (n : Nat) : (masterStack p n).Pairwise Rect.Disjoint := by
  unfold masterStack
  split <;> simp only [List.pairwise_map]
  · exact (masterStackSpans_pairwise_disjoint ..).imp id
  · exact (masterStackSpans_pairwise_disjoint ..).imp Or.symm

/-- Every tile stays within the workspace -/
theorem masterStack_within (p : Params) (n : Nat) : ∀ r ∈ masterStack p n, r.Within p.w p.h := by
  unfold masterStack
  split <;> simp only [List.mem_map] <;> rintro r ⟨t, ht, rfl⟩
  · exact masterStackSpans_within _ _ _ _ _ _ _ t ht
  · exact (masterStackSpans_within _ _ _ _ _ _ _ t ht).symm

end DwmacCore
