import DwmacCore.Split

/-!
# Master-stack layout

dwm's (and xmonad's "Tall") tiling: one master window takes `mfact` of the main axis, the others share the rest,
stacked along the cross axis. With a horizontal orientation, master is a column on the left (or right) and the
stack is a column of rows; with a vertical one, master is a row on top and the stack a row of columns.

Rects are relative to the workspace's top left corner, so the layout only needs the workspace's size. The theorems
at the bottom are the ones the window manager relies on: one tile per window, no two tiles overlap, and every tile
stays within the workspace.
-/

namespace DwmacCore

/-- A rect relative to the workspace's top left corner, in whole points -/
structure Rect where
  x : Nat
  y : Nat
  w : Nat
  h : Nat
deriving Repr, DecidableEq, Inhabited

namespace Rect

def xSpan (r : Rect) : Span := ⟨r.x, r.w⟩
def ySpan (r : Rect) : Span := ⟨r.y, r.h⟩
def ofSpans (xs ys : Span) : Rect := ⟨xs.off, ys.off, xs.len, ys.len⟩

/-- The rects don't share a pixel: they are apart horizontally or vertically -/
def Disjoint (a b : Rect) : Prop := a.xSpan.Disjoint b.xSpan ∨ a.ySpan.Disjoint b.ySpan

/-- The rect lies within a `w` x `h` workspace -/
def Within (r : Rect) (w h : Nat) : Prop := r.xSpan.Within w ∧ r.ySpan.Within h

end Rect

inductive Orientation where
  /-- Master is a column, the stack a column of rows -/
  | h
  /-- Master is a row on top, the stack a row of columns -/
  | v
deriving Repr, DecidableEq, Inhabited

/-- Where master goes along the main axis. Only the horizontal orientation supports `right` (like dwmac) -/
inductive MasterPosition where
  | start
  | stop
deriving Repr, DecidableEq, Inhabited

structure Params where
  /-- Workspace size -/
  w : Nat
  h : Nat
  /-- Gap between columns (`gapH`) and between rows (`gapV`) -/
  gapH : Nat
  gapV : Nat
  /-- Master's share of the main axis, in thousandths. Values above 1000 count as 1000 -/
  mfactPermille : Nat
  orientation : Orientation
  masterPosition : MasterPosition
deriving Repr, Inhabited

/-- Master's and the stack's spans along the main axis: `mfact` of what's left after the gap, rest to the stack -/
def masterStackMain (total gap permille : Nat) (masterAtStop : Bool) : Span × Span :=
  let g := if gap ≤ total then gap else 0
  let m := (total - g) * min permille 1000 / 1000
  let s := total - g - m
  if masterAtStop then (⟨s + g, m⟩, ⟨0, s⟩) else (⟨0, m⟩, ⟨m + g, s⟩)

/-- Tiles as (main axis span, cross axis span), master first -/
def masterStackSpans (main cross gapMain gapCross permille : Nat) (masterAtStop : Bool) : Nat → List (Span × Span)
  | 0 => []
  | 1 => [(⟨0, main⟩, ⟨0, cross⟩)]
  | n + 2 =>
    let (m, s) := masterStackMain main gapMain permille masterAtStop
    (m, ⟨0, cross⟩) :: (split cross gapCross (n + 1)).map (s, ·)

/-- The tiles of `n` windows, master first -/
def masterStack (p : Params) (n : Nat) : List Rect :=
  match p.orientation with
  | .h => (masterStackSpans p.w p.h p.gapH p.gapV p.mfactPermille (p.masterPosition == .stop) n).map
      fun (m, c) => Rect.ofSpans m c
  | .v => (masterStackSpans p.h p.w p.gapV p.gapH p.mfactPermille false n).map
      fun (m, c) => Rect.ofSpans c m

/-! ## Proofs -/

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
