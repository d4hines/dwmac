import DwmacCore.Split

/-!
# Master-stack layout

dwm's (and xmonad's "Tall") tiling: one master window takes `mfact` of the main axis, the others share the rest,
stacked along the cross axis. With a horizontal orientation, master is a column on the left (or right) and the
stack is a column of rows; with a vertical one, master is a row on top and the stack a row of columns.

Rects are relative to the workspace's top left corner, so the layout only needs the workspace's size.
proofs/DwmacProofs/MasterStack.lean proves what the window manager relies on: one tile per window, no two tiles
overlap, and every tile stays within the workspace.
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

end DwmacCore
