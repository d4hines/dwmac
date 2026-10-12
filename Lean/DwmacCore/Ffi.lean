import DwmacCore.MasterStack

/-!
# C exports

Plain C types in, a `ByteArray` out, so the C shim (Sources/CDwmacCore/shim.c) can hand Swift a flat array of
rects without exposing Lean objects. Tiles are encoded as `x, y, w, h`, each a little-endian `uint32`.
Every value fits: tiles stay within the workspace (`masterStack_within`), whose size came in as `uint32`s.
-/

namespace DwmacCore

def ByteArray.pushUInt32LE (a : ByteArray) (v : Nat) : ByteArray :=
  let v := v.toUInt32
  a.push v.toUInt8 |>.push (v >>> 8).toUInt8 |>.push (v >>> 16).toUInt8 |>.push (v >>> 24).toUInt8

def encodeRects (rects : List Rect) : ByteArray :=
  rects.foldl (init := ByteArray.emptyWithCapacity (16 * rects.length)) fun a r =>
    ByteArray.pushUInt32LE (ByteArray.pushUInt32LE (ByteArray.pushUInt32LE (ByteArray.pushUInt32LE a r.x) r.y) r.w) r.h

@[export dwmac_lean_master_stack]
def masterStackFfi (w h gapH gapV mfactPermille : UInt32) (vertical masterAtStop : UInt8) (n : UInt32) : ByteArray :=
  encodeRects <| masterStack
    { w := w.toNat, h := h.toNat, gapH := gapH.toNat, gapV := gapV.toNat, mfactPermille := mfactPermille.toNat
      orientation := if vertical != 0 then .v else .h
      masterPosition := if masterAtStop != 0 then .stop else .start }
    n.toNat

end DwmacCore
