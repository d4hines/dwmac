/-!
# Splitting a length into segments

The one-dimensional core of every tiling layout: cut `total` pixels into `n` segments separated by `gap`
pixels. Lengths are whole pixels (macOS reports window frames in whole points); the remainder of the division
goes to the first segments, one pixel each, so the segments always fill `total` exactly.

If the gaps alone don't fit (`(n - 1) * gap > total`), they are dropped rather than pushing segments outside
`total`. That keeps every theorem about it (proofs/DwmacProofs/Split.lean) unconditional.
-/

namespace DwmacCore

/-- A segment of a line: `off` is its start, `len` its length. It covers `[off, off + len)` -/
structure Span where
  off : Nat
  len : Nat
deriving Repr, DecidableEq, Inhabited

namespace Span

def stop (s : Span) : Nat := s.off + s.len

end Span

/-- The gap actually used: dropped if the `n - 1` gaps don't fit in `total` -/
def effectiveGap (total gap n : Nat) : Nat := if (n - 1) * gap ≤ total then gap else 0

/-- Start of segment `i`, given each segment is `q` pixels plus one for the first `r` -/
def segOff (q r gap i : Nat) : Nat := i * (q + gap) + min i r

def segLen (q r i : Nat) : Nat := q + if i < r then 1 else 0

/-- Split `total` into `n` segments separated by `gap` (see the module doc) -/
def split (total gap n : Nat) : List Span :=
  let g := effectiveGap total gap n
  let avail := total - (n - 1) * g
  (List.range n).map fun i => ⟨segOff (avail / n) (avail % n) g i, segLen (avail / n) (avail % n) i⟩

end DwmacCore
