import DwmacCore.Model

/-!
# The state machine's invariant

`Inv` is what holds in every state dwmac can reach (`reachable_inv`). Its parts:

* `ScreensOk`: monitors are distinct, no workspace is on two monitors, and **no scratchpad home is shown**
* `WindowsOk`: window ids are distinct, and a home holds only its own scratchpad's windows
* `FocusOk`: the focused workspace is shown, and the focused window belongs to it
* `ScratchpadsOk`: scratchpad windows float, and each is in its home, or on the focused workspace with its
  scratchpad focused. macOS keeps the focused app's windows on top, so a scratchpad is topmost or hidden
-/

namespace DwmacCore.Model
open State

structure ScreensOk (s : State) : Prop where
  monsNodup : s.monitors.Nodup
  visNodup : s.visible.Nodup
  noHome : ∀ w ∈ s.visible, w.isHome = false
  forcedOk : ∀ p ∈ s.screens, s.allowedOn p.2 p.1 = true

structure WindowsOk (s : State) : Prop where
  idsNodup : (s.windows.map (·.id)).Nodup
  homesOwned : ∀ w ∈ s.windows, w.ws.isHome = true → s.homeOf w.app = some w.ws

structure FocusOk (s : State) : Prop where
  shown : s.focusWs ∈ s.visible
  win : ∀ id, s.focusWin = some id → ∃ w ∈ s.windows, w.id = id ∧ w.ws = s.focusWs

def ScratchpadsOk (s : State) : Prop :=
  ∀ w ∈ s.windows, ∀ h, s.homeOf w.app = some h →
    w.floating = true ∧ (w.ws = h ∨ (w.ws = s.focusWs ∧ s.focusedApp = some w.app))

structure Inv (s : State) : Prop where
  screens : ScreensOk s
  windows : WindowsOk s
  focus : FocusOk s
  scratchpads : ScratchpadsOk s

/-! ## List lemmas -/

theorem nodup_map_inj {α β} {f : α → β} : ∀ {l : List α}, (l.map f).Nodup → ∀ {a b}, a ∈ l → b ∈ l → f a = f b → a = b
  | [], _, _, _, ha, _, _ => absurd ha List.not_mem_nil
  | x :: l, h, a, b, ha, hb, hab => by
    simp only [List.map_cons, List.nodup_cons, List.mem_map] at h
    simp only [List.mem_cons] at ha hb
    rcases ha with rfl | ha <;> rcases hb with rfl | hb
    · rfl
    · exact absurd ⟨b, hb, hab.symm⟩ h.1
    · exact absurd ⟨a, ha, hab⟩ h.1
    · exact nodup_map_inj h.2 ha hb hab

theorem le_sum_of_mem : ∀ {l : List Nat} {n : Nat}, n ∈ l → n ≤ l.sum
  | _ :: _, _, .head _ => by simp
  | _ :: _, _, .tail _ h => by have := le_sum_of_mem h; simp; omega

theorem mem_of_find?_some {α} {p : α → Bool} {l : List α} {a : α} (h : l.find? p = some a) : a ∈ l ∧ p a = true :=
  ⟨List.mem_of_find?_eq_some h, List.find?_some h⟩

/-! ## Lookups -/

theorem hasWindow_spec {s : State} {ws : WsName} {id : WinId} (h : s.hasWindow ws id = true) :
    ∃ w ∈ s.windows, w.id = id ∧ w.ws = ws := by
  simpa [hasWindow] using h

theorem mruWin_spec {s : State} {ws : WsName} {id : WinId} (h : s.mruWin ws = some id) :
    ∃ w ∈ s.windows, w.id = id ∧ w.ws = ws := by
  unfold mruWin at h
  split at h
  · rename_i hf
    cases h
    obtain ⟨w, hw, hc⟩ := List.any_eq_true.mp (mem_of_find?_some hf).2
    simp only [Bool.and_eq_true, beq_iff_eq] at hc
    exact ⟨w, hw, hc.1.1, hc.1.2⟩
  split at h
  · rename_i w hf
    cases h
    have := mem_of_find?_some hf
    simp only [Bool.and_eq_true, beq_iff_eq] at this
    exact ⟨w, this.1, rfl, this.2.1⟩
  split at h
  · rename_i hf
    cases h
    exact hasWindow_spec (mem_of_find?_some hf).2
  · obtain ⟨w, hw, rfl⟩ := Option.map_eq_some_iff.mp h
    have := mem_of_find?_some hw
    exact ⟨w, this.1, rfl, by simpa using this.2⟩

theorem window?_of_mem {s : State} (hw : WindowsOk s) {w : Win} (hm : w ∈ s.windows) : s.window? w.id = some w := by
  unfold window?
  cases hf : s.windows.find? (·.id == w.id) with
  | none => exact absurd hm (by simpa using List.find?_eq_none.mp hf w)
  | some x =>
    have := mem_of_find?_some hf
    rw [nodup_map_inj hw.idsNodup this.1 hm (by simpa using this.2)]

/-! ## fixFocus -/

@[simp] theorem fixFocusWs_screens (s : State) : s.fixFocusWs.screens = s.screens := by
  unfold fixFocusWs; split <;> (try split) <;> rfl
@[simp] theorem fixFocusWs_windows (s : State) : s.fixFocusWs.windows = s.windows := by
  unfold fixFocusWs; split <;> (try split) <;> rfl
@[simp] theorem fixFocusWs_scratchpads (s : State) : s.fixFocusWs.scratchpads = s.scratchpads := by
  unfold fixFocusWs; split <;> (try split) <;> rfl
@[simp] theorem fixFocusWs_forced (s : State) : s.fixFocusWs.forced = s.forced := by
  unfold fixFocusWs; split <;> (try split) <;> rfl
@[simp] theorem fixFocusWin_forced (s : State) : s.fixFocusWin.forced = s.forced := by
  unfold fixFocusWin; split <;> rfl
@[simp] theorem fixFocusWin_screens (s : State) : s.fixFocusWin.screens = s.screens := by
  unfold fixFocusWin; split <;> rfl
@[simp] theorem fixFocusWin_windows (s : State) : s.fixFocusWin.windows = s.windows := by
  unfold fixFocusWin; split <;> rfl
@[simp] theorem fixFocusWin_scratchpads (s : State) : s.fixFocusWin.scratchpads = s.scratchpads := by
  unfold fixFocusWin; split <;> rfl
@[simp] theorem fixFocusWin_focusWs (s : State) : s.fixFocusWin.focusWs = s.focusWs := by
  unfold fixFocusWin; split <;> rfl

theorem fixFocusWs_shown (s : State) (hne : s.screens ≠ []) : s.fixFocusWs.focusWs ∈ s.visible := by
  unfold fixFocusWs
  split
  · rename_i h; simpa using h
  · match hsc : s.screens, hne with
    | (m, ws) :: rest, _ => simp [visible, hsc]

theorem fixFocusWin_win (s : State) :
    ∀ id, s.fixFocusWin.focusWin = some id → ∃ w ∈ s.windows, w.id = id ∧ w.ws = s.focusWs := by
  unfold fixFocusWin
  split
  · intro id hid
    rename_i hv
    simp only [focusWinValid, hid] at hv
    exact hasWindow_spec hv
  · exact fun id hid => mruWin_spec hid

theorem fixFocus_focusOk (s : State) (hne : s.screens ≠ []) : FocusOk s.fixFocus := by
  have := fixFocusWin_win s.fixFocusWs
  simp only [fixFocusWs_windows] at this
  exact ⟨by simpa [fixFocus, visible] using fixFocusWs_shown s hne, by simpa [fixFocus] using this⟩

@[simp] theorem fixFocus_screens (s : State) : s.fixFocus.screens = s.screens := by simp [fixFocus]
@[simp] theorem fixFocus_windows (s : State) : s.fixFocus.windows = s.windows := by simp [fixFocus]
@[simp] theorem fixFocus_scratchpads (s : State) : s.fixFocus.scratchpads = s.scratchpads := by simp [fixFocus]
@[simp] theorem fixFocus_forced (s : State) : s.fixFocus.forced = s.forced := by simp [fixFocus]

/-! ## hideUnfocused -/

/-- What `hideUnfocused` does to one window -/
def hideOne (s : State) (w : Win) : Win :=
  match s.homeOf w.app with
  | none => w
  | some h =>
    if w.ws = s.focusWs ∧ s.focusedApp = some w.app then { w with floating := true }
    else { w with ws := h, floating := true }

theorem hideUnfocused_eq (s : State) : s.hideUnfocused = { s with windows := s.windows.map (hideOne s) } := rfl

@[simp] theorem hideOne_id (s : State) (w : Win) : (hideOne s w).id = w.id := by
  unfold hideOne; split <;> (try split) <;> rfl

@[simp] theorem hideOne_app (s : State) (w : Win) : (hideOne s w).app = w.app := by
  unfold hideOne; split <;> (try split) <;> rfl

theorem hideOne_none {s : State} {w : Win} (hh : s.homeOf w.app = none) : hideOne s w = w := by
  simp [hideOne, hh]

theorem hideOne_some {s : State} {w : Win} {h : WsName} (hh : s.homeOf w.app = some h) :
    hideOne s w = if w.ws = s.focusWs ∧ s.focusedApp = some w.app then { w with floating := true }
      else { w with ws := h, floating := true } := by
  simp [hideOne, hh]

theorem hideUnfocused_focusedApp (s : State) : s.hideUnfocused.focusedApp = s.focusedApp := by
  simp only [hideUnfocused_eq, focusedApp, window?, List.find?_map]
  cases s.focusWin with
  | none => rfl
  | some id =>
    simp only [Option.bind_some, Option.map_map]
    have h1 : ((fun w : Win => w.id == id) ∘ hideOne s) = (fun w => w.id == id) := funext fun w => by simp
    have h2 : ((fun w : Win => w.app) ∘ hideOne s) = (fun w => w.app) := funext fun w => by simp
    rw [h1, h2]

theorem hideUnfocused_inv (s : State) (hs : ScreensOk s) (hw : WindowsOk s) (hf : FocusOk s) :
    Inv s.hideUnfocused := by
  rw [hideUnfocused_eq]
  refine ⟨⟨hs.monsNodup, hs.visNodup, hs.noHome, hs.forcedOk⟩, ⟨?_, ?_⟩, ⟨hf.shown, ?_⟩, ?_⟩
  · -- Ids don't change
    have : ((fun w : Win => w.id) ∘ hideOne s) = (fun w => w.id) := funext fun w => by simp
    simpa [List.map_map, this] using hw.idsNodup
  · -- A window that ends up in a home is in its own
    simp only [List.mem_map]
    rintro _ ⟨w, hm, rfl⟩ hhome
    show s.homeOf (hideOne s w).app = some (hideOne s w).ws
    rw [hideOne_app]
    cases hh : s.homeOf w.app with
    | none =>
      rw [hideOne_none hh] at hhome
      exact absurd (hw.homesOwned w hm hhome) (by simp [hh])
    | some h =>
      rw [hideOne_some hh] at hhome ⊢
      by_cases hc : w.ws = s.focusWs ∧ s.focusedApp = some w.app
      · rw [if_pos hc] at hhome ⊢
        exact hh ▸ hw.homesOwned w hm hhome
      · rw [if_neg hc]
  · -- The focused window stays put
    intro id hid
    obtain ⟨w, hm, rfl, hws⟩ := hf.win id hid
    refine ⟨hideOne s w, List.mem_map_of_mem hm, hideOne_id .., ?_⟩
    show (hideOne s w).ws = s.focusWs
    have happ : s.focusedApp = some w.app := by
      have hid' : s.focusWin = some w.id := hid
      simp only [focusedApp, hid', Option.bind_some, window?_of_mem hw hm, Option.map_some]
    cases hh : s.homeOf w.app with
    | none => rw [hideOne_none hh]; exact hws
    | some h => rw [hideOne_some hh, if_pos ⟨hws, happ⟩]; exact hws
  · -- Scratchpad windows end up home, or summoned with focus
    intro w' hm' h hh
    obtain ⟨w, _, rfl⟩ := List.mem_map.mp hm'
    have happ := hideUnfocused_focusedApp s
    rw [hideUnfocused_eq] at happ
    rw [happ, hideOne_app]
    rw [hideOne_app] at hh
    have hh : s.homeOf w.app = some h := hh
    rw [hideOne_some hh]
    by_cases hc : w.ws = s.focusWs ∧ s.focusedApp = some w.app
    · rw [if_pos hc]; exact ⟨rfl, Or.inr hc⟩
    · rw [if_neg hc]; exact ⟨rfl, Or.inl rfl⟩

/-! ## Screens -/

theorem allowedOn_congr {s t : State} (h : t.forced = s.forced) : t.allowedOn = s.allowedOn := by
  funext ws mon; simp [allowedOn, forcedOf, h]

theorem ScreensOk.congr {s t : State} (hs : ScreensOk s) (h : t.screens = s.screens) (hf : t.forced = s.forced) :
    ScreensOk t := by
  obtain ⟨a, b, c, d⟩ := hs
  exact ⟨by simpa [monitors, h] using a, by simpa [visible, h] using b, by simpa [visible, h] using c,
    by rw [h, allowedOn_congr hf]; exact d⟩

theorem replaceAt_fst (l : List (MonId × WsName)) (m : MonId) (w : WsName) :
    (replaceAt l m w).map (·.1) = l.map (·.1) := by
  simp only [replaceAt, List.map_map]; congr 1; funext p; simp only [Function.comp]; split <;> rfl

theorem mem_replaceAt_pair {l : List (MonId × WsName)} {m : MonId} {w : WsName} {p : MonId × WsName}
    (h : p ∈ replaceAt l m w) : (p ∈ l ∧ p.1 ≠ m) ∨ p = (m, w) := by
  simp only [replaceAt, List.mem_map] at h
  obtain ⟨q, hq, rfl⟩ := h
  by_cases hqm : q.1 = m
  · simp [hqm]
  · simp only [hqm, if_false]; exact Or.inl ⟨hq, hqm⟩

theorem mem_replaceAt {l : List (MonId × WsName)} {m : MonId} {w x : WsName} (h : x ∈ (replaceAt l m w).map (·.2)) :
    x ∈ l.map (·.2) ∨ x = w := by
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp h
  rcases mem_replaceAt_pair hp with ⟨hp, _⟩ | rfl
  · exact Or.inl (List.mem_map_of_mem hp)
  · exact Or.inr rfl

theorem replaceAt_of_not_mem : ∀ {l : List (MonId × WsName)} {m : MonId} (w : WsName),
    m ∉ l.map (·.1) → replaceAt l m w = l
  | [], _, _, _ => rfl
  | p :: t, m, w, h => by
    simp only [List.map_cons, List.mem_cons, not_or] at h
    have ih := replaceAt_of_not_mem w h.2
    simp only [replaceAt] at ih ⊢
    simp only [List.map_cons, ih, Ne.symm h.1, if_false]

theorem replaceAt_nodup : ∀ {l : List (MonId × WsName)} {m : MonId} {w : WsName},
    (l.map (·.1)).Nodup → (l.map (·.2)).Nodup → w ∉ l.map (·.2) → ((replaceAt l m w).map (·.2)).Nodup
  | [], _, _, _, _, _ => by simp [replaceAt]
  | p :: t, m, w, hf, hl, hw => by
    simp only [List.map_cons, List.nodup_cons, List.mem_cons, not_or] at hf hl hw
    by_cases hpm : p.1 = m
    · have ht : replaceAt t m w = t := replaceAt_of_not_mem w (hpm ▸ hf.1)
      have : replaceAt (p :: t) m w = (p.1, w) :: t := by
        simp only [replaceAt] at ht ⊢; simp only [List.map_cons, hpm, if_true, ht]
      rw [this, List.map_cons, List.nodup_cons]
      exact ⟨hw.2, hl.2⟩
    · have : replaceAt (p :: t) m w = p :: replaceAt t m w := by simp [replaceAt, hpm]
      rw [this, List.map_cons, List.nodup_cons]
      refine ⟨fun hy => ?_, replaceAt_nodup hf.2 hl.2 hw.2⟩
      rcases mem_replaceAt hy with hy | hy
      · exact hl.1 hy
      · exact hw.1 hy.symm

theorem stubOk_spec {s : State} {mon : MonId} {ws : WsName} (h : s.stubOk mon ws = true) :
    ws.isHome = false ∧ ws ∉ s.visible ∧ s.allowedOn ws mon = true := by
  simp only [stubOk, Bool.and_eq_true, Bool.not_eq_true'] at h
  exact ⟨h.1.1, by simpa using h.1.2, h.2⟩

theorem freshNum_spec (s : State) (mon : MonId) : s.stubOk mon (.num (s.freshNum mon)) = true := by
  unfold freshNum
  dsimp only
  split
  · rename_i hf; exact (mem_of_find?_some hf).2
  · generalize hN : ((s.visible ++ s.forced.map (·.1)).map WsName.numOf).sum = N
    have big : ∀ w ∈ s.visible ++ s.forced.map (·.1), w ≠ .num (N + 1) := by
      rintro w hw rfl
      have := le_sum_of_mem (List.mem_map_of_mem (f := WsName.numOf) hw)
      simp only [WsName.numOf] at this; omega
    have hvis : WsName.num (N + 1) ∉ s.visible := fun h => big _ (List.mem_append_left _ h) rfl
    have hforced : s.forcedOf (.num (N + 1)) = none := by
      simp only [forcedOf, Option.map_eq_none_iff, List.find?_eq_none]
      intro q hq hqe
      exact big q.1 (List.mem_append_right _ (List.mem_map_of_mem hq)) (by simpa using hqe)
    simp [stubOk, WsName.isHome, hvis, allowedOn, hforced]

theorem stub_spec (s : State) (m : MonId) : s.stubOk m (s.stub m) = true := by
  unfold stub
  split
  · rename_i hf; exact (mem_of_find?_some hf).2
  · exact freshNum_spec s m

theorem monitorOf_none {s : State} {ws : WsName} (h : s.monitorOf ws = none) : ws ∉ s.visible := by
  simp only [monitorOf, Option.map_eq_none_iff, List.find?_eq_none] at h
  simp only [visible, List.mem_map, not_exists, not_and]
  intro p hp hpw
  exact h p hp (by simp [hpw])

theorem monitorOf_some {s : State} {ws : WsName} {m : MonId} (h : s.monitorOf ws = some m) : (m, ws) ∈ s.screens := by
  simp only [monitorOf, Option.map_eq_some_iff] at h
  obtain ⟨p, hp, rfl⟩ := h
  have := mem_of_find?_some hp
  have hw : p.2 = ws := by simpa using this.2
  rw [← hw]; exact this.1

@[simp] theorem vacate_windows (s : State) (ws : WsName) : (s.vacate ws).windows = s.windows := by
  unfold vacate; split <;> rfl
@[simp] theorem vacate_scratchpads (s : State) (ws : WsName) : (s.vacate ws).scratchpads = s.scratchpads := by
  unfold vacate; split <;> rfl
@[simp] theorem vacate_forced (s : State) (ws : WsName) : (s.vacate ws).forced = s.forced := by
  unfold vacate; split <;> rfl

theorem vacate_ok {s : State} (hs : ScreensOk s) (ws : WsName) :
    ScreensOk (s.vacate ws) ∧ ws ∉ (s.vacate ws).visible ∧ (s.vacate ws).screens.length = s.screens.length := by
  unfold vacate
  split
  · rename_i m hm
    have hin := monitorOf_some hm
    obtain ⟨sth, stv, sta⟩ := stubOk_spec (stub_spec s m)
    refine ⟨⟨?_, ?_, ?_, ?_⟩, ?_, by simp [replaceAt]⟩
    · simpa [monitors, replaceAt_fst] using hs.monsNodup
    · exact replaceAt_nodup hs.monsNodup hs.visNodup stv
    · intro x hx
      rcases mem_replaceAt hx with hx | rfl
      · exact hs.noHome x hx
      · exact sth
    · intro p hp
      rcases mem_replaceAt_pair hp with ⟨hp, _⟩ | rfl
      · exact hs.forcedOk p hp
      · exact sta
    · intro hx
      obtain ⟨p, hp, hpw⟩ := List.mem_map.mp hx
      rcases mem_replaceAt_pair hp with ⟨hp, hpm⟩ | rfl
      · exact hpm (congrArg Prod.fst (nodup_map_inj hs.visNodup hp hin hpw))
      · have hv : ws ∈ s.visible := List.mem_map_of_mem (f := (·.2)) hin
        have hpw : s.stub m = ws := hpw
        exact stv (hpw ▸ hv)
  · exact ⟨hs, monitorOf_none ‹_›, rfl⟩

theorem setActive_ok {s : State} (hs : ScreensOk s) (mon : MonId) (ws : WsName) :
    ScreensOk (s.setActive mon ws) ∧ (s.setActive mon ws).screens.length = s.screens.length := by
  unfold setActive
  split
  · exact ⟨hs, rfl⟩
  · rename_i hc
    have hhome : ws.isHome = false := by simp_all
    have hallow : s.allowedOn ws mon = true := by simp_all
    obtain ⟨hv, hws, hlen⟩ := vacate_ok hs ws
    refine ⟨⟨?_, ?_, ?_, ?_⟩, by simp [replaceAt, hlen]⟩
    · simpa [monitors, replaceAt_fst] using hv.monsNodup
    · exact replaceAt_nodup hv.monsNodup hv.visNodup hws
    · intro x hx
      rcases mem_replaceAt hx with hx | rfl
      · exact hv.noHome x hx
      · exact hhome
    · intro p hp
      rcases mem_replaceAt_pair hp with ⟨hp, _⟩ | rfl
      · exact hv.forcedOk p hp
      · show (s.vacate ws).allowedOn ws mon = true
        rw [allowedOn_congr (vacate_forced s ws)]; exact hallow

@[simp] theorem setActive_windows (s : State) (mon : MonId) (ws : WsName) : (s.setActive mon ws).windows = s.windows := by
  unfold setActive; split <;> simp
@[simp] theorem setActive_scratchpads (s : State) (mon : MonId) (ws : WsName) :
    (s.setActive mon ws).scratchpads = s.scratchpads := by
  unfold setActive; split <;> simp
@[simp] theorem setActive_forced (s : State) (mon : MonId) (ws : WsName) : (s.setActive mon ws).forced = s.forced := by
  unfold setActive; split <;> simp

/-! ## Windows -/

theorem homeOf_congr {s t : State} (h : t.scratchpads = s.scratchpads) : t.homeOf = s.homeOf := by
  funext app; simp [homeOf, scratchpadOf, h]

theorem WindowsOk.congr {s t : State} (h : WindowsOk s) (hw : t.windows = s.windows)
    (hsp : t.scratchpads = s.scratchpads) : WindowsOk t :=
  ⟨hw ▸ h.idsNodup, by rw [hw, homeOf_congr hsp]; exact h.homesOwned⟩

/-- Windows map to windows with the same id: ids stay distinct -/
theorem idsNodup_map {l : List Win} {f : Win → Win} (hf : ∀ w, (f w).id = w.id) (h : (l.map (·.id)).Nodup) :
    ((l.map f).map (·.id)).Nodup := by
  have : ((fun w : Win => w.id) ∘ f) = (fun w => w.id) := funext hf
  simpa [List.map_map, this] using h

theorem idsNodup_filter {l : List Win} (p : Win → Bool) (h : (l.map (·.id)).Nodup) :
    ((l.filter p).map (·.id)).Nodup :=
  h.sublist ((List.filter_sublist).map _)

theorem idsNodup_append {l : List Win} {w : Win} (h : (l.map (·.id)).Nodup) (hw : w.id ∉ l.map (·.id)) :
    ((l ++ [w]).map (·.id)).Nodup := by
  simp only [List.map_append, List.map_cons, List.map_nil, List.nodup_append, List.nodup_cons, List.not_mem_nil,
    not_false_eq_true, List.nodup_nil, and_self, List.mem_cons, List.mem_map, or_false, true_and]
  refine ⟨h, ?_⟩
  rintro _ ⟨x, hx, rfl⟩ _ rfl hxw
  exact hw (List.mem_map.mpr ⟨x, hx, hxw⟩)

theorem summon_ok {s : State} (hw : WindowsOk s) {app : AppId} {ws : WsName} (hws : ws.isHome = false) :
    WindowsOk { s with windows := s.summon app ws } := by
  refine ⟨idsNodup_map (fun w => by split <;> rfl) hw.idsNodup, ?_⟩
  simp only [summon, List.mem_map]
  rintro _ ⟨w, hm, rfl⟩ hhome
  split at hhome
  · rw [hws] at hhome; contradiction
  · rename_i hne; simp only [hne, if_false]; exact hw.homesOwned w hm hhome

theorem sendHome_ok {s : State} (hw : WindowsOk s) (app : AppId) : WindowsOk { s with windows := s.sendHome app } := by
  have hid : ∀ w, ((match s.homeOf w.app with
      | some h => if w.app = app then { w with ws := h, floating := true } else w
      | none => w) : Win).id = w.id := fun w => by split <;> (try split) <;> rfl
  refine ⟨idsNodup_map hid hw.idsNodup, ?_⟩
  simp only [sendHome, List.mem_map]
  rintro _ ⟨w, hm, rfl⟩ hhome
  show s.homeOf _ = _
  cases hh : s.homeOf w.app with
  | none => simp only [hh] at hhome ⊢; exact hh ▸ hw.homesOwned w hm hhome
  | some h =>
    simp only [hh] at hhome ⊢
    by_cases ha : w.app = app
    · rw [if_pos ha]; exact hh
    · simp only [ha, if_false] at hhome ⊢; exact hh ▸ hw.homesOwned w hm hhome

theorem append_ok {s : State} (hw : WindowsOk s) {w : Win} (hid : s.windows.any (·.id == w.id) = false)
    (hhome : w.ws.isHome = true → s.homeOf w.app = some w.ws) : WindowsOk { s with windows := s.windows ++ [w] } := by
  refine ⟨idsNodup_append hw.idsNodup (by simpa using hid), ?_⟩
  intro x hx
  simp only [List.mem_append, List.mem_cons, List.not_mem_nil, or_false] at hx
  rcases hx with hx | rfl
  · exact hw.homesOwned x hx
  · exact hhome

theorem filter_ok {s : State} (hw : WindowsOk s) (p : Win → Bool) : WindowsOk { s with windows := s.windows.filter p } :=
  ⟨idsNodup_filter p hw.idsNodup, fun w hm => hw.homesOwned w (List.mem_filter.mp hm).1⟩

/-! ## Raw ops keep the structure -/

/-- What every raw op preserves -/
structure Struct (s : State) : Prop where
  screens : ScreensOk s
  windows : WindowsOk s
  ne : s.screens ≠ []

theorem Struct.congr {s t : State} (h : Struct s) (h1 : t.screens = s.screens) (h2 : t.windows = s.windows)
    (h3 : t.scratchpads = s.scratchpads) (h4 : t.forced = s.forced) : Struct t :=
  ⟨h.screens.congr h1 h4, h.windows.congr h2 h3, h1 ▸ h.ne⟩

theorem Struct.withWindows {s : State} (h : Struct s) {l : List Win} (hw : WindowsOk { s with windows := l }) :
    Struct { s with windows := l } :=
  ⟨h.screens.congr rfl rfl, hw, h.ne⟩

theorem Struct.setActive {s : State} (h : Struct s) (mon : MonId) (ws : WsName) : Struct (s.setActive mon ws) := by
  obtain ⟨hs, hlen⟩ := setActive_ok h.screens mon ws
  refine ⟨hs, h.windows.congr (by simp) (by simp), ?_⟩
  intro he; rw [he] at hlen; exact h.ne (List.length_eq_zero_iff.mp hlen.symm)

theorem Struct.focusOn {s : State} (h : Struct s) (ws : WsName) (win : Option WinId) : Struct (s.focusOn ws win) :=
  (h.setActive _ ws).congr rfl rfl rfl rfl

theorem Struct.focusWorkspace {s : State} (h : Struct s) (ws : WsName) : Struct (s.focusWorkspace ws) := by
  unfold State.focusWorkspace; split
  · exact h
  · exact h.focusOn ..

theorem Inv.struct {s : State} (h : Inv s) : Struct s :=
  ⟨h.screens, h.windows, fun he => by have := h.focus.shown; simp [visible, he] at this⟩

theorem Inv.focusWs_not_home {s : State} (h : Inv s) : s.focusWs.isHome = false :=
  h.screens.noHome _ h.focus.shown

theorem window?_spec {s : State} {id : WinId} {w : Win} (h : s.window? id = some w) : w ∈ s.windows ∧ w.id = id := by
  have := mem_of_find?_some h
  exact ⟨this.1, by simpa using this.2⟩

theorem focusWindow_struct {s : State} (h : Struct s) (id : WinId) : Struct (s.focusWindow id) := by
  unfold State.focusWindow
  split
  · exact h
  · split
    · split
      · exact h
      · rename_i hfh
        exact (h.withWindows (summon_ok h.windows (by simpa using hfh))).focusOn ..
    · exact h.focusOn ..

theorem toggleScratchpad_struct {s : State} (h : Struct s) (name : String) : Struct (s.toggleScratchpad name) := by
  unfold State.toggleScratchpad
  split
  · exact h
  · simp only
    split
    · refine h.congr rfl rfl rfl rfl
    · split
      · exact h
      · rename_i hfh
        split
        · exact (h.withWindows (summon_ok h.windows (by simpa using hfh))).focusOn ..
        · split
          · exact (h.withWindows (sendHome_ok h.windows _)).focusOn ..
          · exact h.focusOn ..

theorem insertInWs_perm (w : Win) : ∀ (l : List Win) (i : Nat), (insertInWs w l i).Perm (w :: l)
  | [], _ => .refl _
  | x :: t, i => by
    unfold insertInWs
    split
    · split
      · exact .refl _
      · exact ((insertInWs_perm w t _).cons x).trans (.swap w x t)
    · exact ((insertInWs_perm w t i).cons x).trans (.swap w x t)

theorem insert_ok {s : State} (hw : WindowsOk s) {w : Win} (i : Nat) (hid : w.id ∉ s.windows.map (·.id))
    (hhome : w.ws.isHome = true → s.homeOf w.app = some w.ws) :
    WindowsOk { s with windows := insertInWs w s.windows i } := by
  have hp := insertInWs_perm w s.windows i
  refine ⟨(hp.map _).nodup_iff.mpr ?_, fun x hx => ?_⟩
  · simp only [List.map_cons, List.nodup_cons]; exact ⟨hid, hw.idsNodup⟩
  · rcases List.mem_cons.mp (hp.mem_iff.mp hx) with rfl | hx
    · exact hhome
    · exact hw.homesOwned x hx

theorem windowAppeared_struct {s : State} (h : Struct s) (hf : s.focusWs.isHome = false) (id : WinId) (app : AppId)
    (ws : WsName) (floating : Bool) (index : Nat) : Struct (s.windowAppeared id app ws floating index) := by
  unfold State.windowAppeared
  split
  · exact h
  · rename_i hid
    have hid : s.windows.any (·.id == id) = false := by simpa using hid
    split
    · refine h.withWindows (insert_ok h.windows index (by simpa using hid) ?_)
      intro hh; simp only at hh; split at hh
      · rw [hf] at hh; contradiction
      · simp_all
    · rename_i sp hsp
      have hhome : s.homeOf app = some (.home sp.name) := by simp [homeOf, hsp]
      split
      · rename_i ws _
        split
        · exact h
        · rename_i hws
          have hw := append_ok (w := ({ id, app, ws, floating := true } : Win)) h.windows hid
            (fun hh => absurd hh hws)
          refine Struct.focusOn ?_ ws (some id)
          exact (h.withWindows hw).congr rfl rfl rfl rfl
      · refine h.withWindows (append_ok h.windows hid ?_)
        simp only
        cases hfind : s.windows.find? (fun w => w.app == app && !w.ws.isHome) with
        | none => intro; simp [hhome]
        | some w =>
          have := (mem_of_find?_some hfind).2
          simp only [Bool.and_eq_true, Bool.not_eq_true'] at this
          simp [this.2]

theorem windowClosed_struct {s : State} (h : Struct s) (id : WinId) : Struct (s.windowClosed id) :=
  (h.withWindows (filter_ok h.windows _)).congr rfl rfl rfl rfl

theorem moveWindowToWorkspace_struct {s : State} (h : Struct s) (id : WinId) (ws : WsName) (focusFollows : Bool) :
    Struct (s.moveWindowToWorkspace id ws focusFollows) := by
  unfold State.moveWindowToWorkspace
  split
  · exact h
  · rename_i w hw
    obtain ⟨hm, rfl⟩ := window?_spec hw
    split
    · exact h
    · split
      · split
        · exact h.withWindows (sendHome_ok h.windows _)
        · exact h
      · split
        · exact h
        · rename_i hws
          have hok : WindowsOk { s with windows := s.windows.filter (·.id != w.id) ++ [{ w with ws }] } := by
            have hf := filter_ok h.windows (·.id != w.id)
            have := append_ok (w := { w with ws }) hf (by simp) (fun hh => absurd hh hws)
            exact this
          have hs := h.withWindows hok
          split
          · exact hs.focusOn ..
          · exact hs

theorem addScreen_ok {s : State} (hs : ScreensOk s) (mon : MonId) : ScreensOk (s.addScreen mon) := by
  unfold State.addScreen
  split
  · exact hs
  · rename_i hmon
    have hst := stubOk_spec (stub_spec s mon)
    have hst : s.stub mon ∉ s.visible ∧ (s.stub mon).isHome = false ∧ s.allowedOn (s.stub mon) mon = true :=
      ⟨hst.2.1, hst.1, hst.2.2⟩
    refine ⟨?_, ?_, ?_, ?_⟩
    · simp only [monitors, List.map_append, List.map_cons, List.map_nil]
      exact List.nodup_append.mpr ⟨hs.monsNodup, (by simp),
        fun a ha b hb hab => by simp at hb; subst hb hab; simp_all [monitors]⟩
    · simp only [visible, List.map_append, List.map_cons, List.map_nil]
      exact List.nodup_append.mpr ⟨hs.visNodup, (by simp),
        fun a ha b hb hab => by simp at hb; subst hb hab; exact hst.1 ha⟩
    · intro x hx
      simp only [visible, List.map_append, List.mem_append, List.map_cons, List.map_nil, List.mem_singleton] at hx
      rcases hx with hx | rfl
      · exact hs.noHome x hx
      · exact hst.2.1
    · intro p hp
      simp only [List.mem_append, List.mem_singleton] at hp
      rcases hp with hp | rfl
      · exact hs.forcedOk p hp
      · exact hst.2.2

theorem addScreen_forced (s : State) (mon : MonId) : (s.addScreen mon).forced = s.forced := by
  unfold State.addScreen; split <;> rfl

theorem addScreen_windows (s : State) (mon : MonId) : (s.addScreen mon).windows = s.windows := by
  unfold State.addScreen; split <;> rfl
theorem addScreen_scratchpads (s : State) (mon : MonId) : (s.addScreen mon).scratchpads = s.scratchpads := by
  unfold State.addScreen; split <;> rfl

theorem foldl_invariant {α β} (P : α → Prop) (f : α → β → α) (hf : ∀ a b, P a → P (f a b)) :
    ∀ (l : List β) (a : α), P a → P (l.foldl f a)
  | [], _, h => h
  | b :: l, a, h => foldl_invariant P f hf l (f a b) (hf a b h)

theorem keepScreens_ok {s : State} (hs : ScreensOk s) (mons : List MonId) (forced : List (WsName × MonId)) :
    ScreensOk (s.keepScreens mons forced) := by
  refine ⟨?_, ?_, ?_, ?_⟩
  · exact hs.monsNodup.sublist ((List.filter_sublist).map _)
  · exact hs.visNodup.sublist ((List.filter_sublist).map _)
  · intro x hx
    simp only [keepScreens, visible, List.mem_map, List.mem_filter] at hx
    obtain ⟨p, ⟨hp, _⟩, rfl⟩ := hx
    exact hs.noHome _ (List.mem_map_of_mem hp)
  · intro p hp
    simp only [keepScreens, List.mem_filter, Bool.and_eq_true] at hp
    exact hp.2.2

theorem Struct.followForced {s t : State} (h : Struct t) : Struct (s.followForced t) := by
  unfold State.followForced
  simp only
  have P := foldl_invariant Struct followStep
    (fun a b ha => by
      unfold followStep
      split
      · exact ha.setActive ..
      · exact ha)
  have P := P (s.visible.filter (· != s.focusWs)) t h
  split
  · exact P.focusOn ..
  · exact P

theorem monitorsChanged_struct {s : State} (h : Struct s) (mons : List MonId) (forced : List (WsName × MonId)) :
    Struct (s.monitorsChanged mons forced) := by
  unfold State.monitorsChanged
  simp only
  generalize hs' : mons.foldl State.addScreen (s.keepScreens mons forced) = s'
  split
  · exact h
  · rename_i hne
    have P := foldl_invariant
      (fun t : State => ScreensOk t ∧ t.windows = s.windows ∧ t.scratchpads = s.scratchpads) State.addScreen
      (fun a b ⟨ha, hw, hsp⟩ => ⟨addScreen_ok ha b, by rw [addScreen_windows, hw], by rw [addScreen_scratchpads, hsp]⟩)
      mons _ ⟨keepScreens_ok h.screens mons forced, rfl, rfl⟩
    rw [hs'] at P
    exact Struct.followForced ⟨P.1, h.windows.congr P.2.1 P.2.2, by simpa using hne⟩

theorem placeWindow_struct {s : State} (h : Struct s) (id : WinId) (ws : WsName) (index : Nat)
    (floating : Option Bool) : Struct (s.placeWindow id ws index floating) := by
  unfold State.placeWindow
  split
  · exact h
  · rename_i w hw
    obtain ⟨hm, rfl⟩ := window?_spec hw
    simp only
    split
    · exact h
    · rename_i hg
      refine h.withWindows (insert_ok (filter_ok h.windows (·.id != w.id)) index (by simp) ?_)
      intro hh
      simp only at hh ⊢
      cases hs : (s.homeOf w.app).isSome
      · simp_all
      · have hws : w.ws = ws := by simp_all
        subst hws
        exact h.windows.homesOwned w hm hh

theorem setFloating_struct {s : State} (h : Struct s) (id : WinId) (floating : Bool) :
    Struct (s.setFloating id floating) := by
  have hf : ∀ w : Win, (if w.id = id && (s.homeOf w.app).isNone then { w with floating, centered := w.centered && floating } else w).id = w.id ∧
      (if w.id = id && (s.homeOf w.app).isNone then { w with floating, centered := w.centered && floating } else w).ws = w.ws ∧
      (if w.id = id && (s.homeOf w.app).isNone then { w with floating, centered := w.centered && floating } else w).app = w.app :=
    fun w => by split <;> exact ⟨rfl, rfl, rfl⟩
  refine h.withWindows ⟨idsNodup_map (fun w => (hf w).1) h.windows.idsNodup, ?_⟩
  simp only [List.mem_map]
  rintro _ ⟨w, hm, rfl⟩ hhome
  rw [(hf w).2.1] at hhome ⊢
  rw [(hf w).2.2]
  exact h.windows.homesOwned w hm hhome

theorem configChanged_struct {s : State} (h : Struct s) (hf : s.focusWs.isHome = false)
    (scratchpads : List Scratchpad) (forced : List (WsName × MonId)) :
    Struct (s.configChanged scratchpads forced) := by
  unfold State.configChanged
  simp only
  apply monitorsChanged_struct
  refine ⟨h.screens.congr rfl rfl, ⟨?_, ?_⟩, h.ne⟩
  · refine idsNodup_map (fun w => ?_) h.windows.idsNodup
    split <;> rfl
  · simp only [State.evictFromHomes, List.mem_map]
    rintro _ ⟨w, _, rfl⟩ hhome
    split
    · rename_i hc; rw [if_pos hc] at hhome; rw [hf] at hhome; contradiction
    · rename_i hc
      rw [if_neg hc] at hhome
      simp only [Bool.and_eq_true, bne_iff_ne, ne_eq, not_and, Decidable.not_not] at hc
      exact hc hhome

theorem mapWins_struct {s : State} (h : Struct s) (p : Win → Bool) (f : Win → Win) :
    Struct { s with windows := s.mapWins p f } := by
  have hf : ∀ w : Win, (if p w then { f w with id := w.id, app := w.app, ws := w.ws } else w).id = w.id ∧
      (if p w then { f w with id := w.id, app := w.app, ws := w.ws } else w).ws = w.ws ∧
      (if p w then { f w with id := w.id, app := w.app, ws := w.ws } else w).app = w.app :=
    fun w => by split <;> exact ⟨rfl, rfl, rfl⟩
  refine h.withWindows ⟨idsNodup_map (fun w => (hf w).1) h.windows.idsNodup, ?_⟩
  simp only [State.mapWins, List.mem_map]
  rintro _ ⟨w, hm, rfl⟩ hhome
  rw [(hf w).2.1] at hhome ⊢
  rw [(hf w).2.2]
  exact h.windows.homesOwned w hm hhome

theorem Struct.setProps {s : State} (h : Struct s) (ws : WsName) (p : WsProps) : Struct (s.setProps ws p) :=
  h.congr rfl rfl rfl rfl

theorem raw_struct {s : State} (hinv : Inv s) (op : Op) : Struct (s.raw op) := by
  have h := hinv.struct
  cases op with
  | focusWindow id => exact focusWindow_struct h id
  | focusWorkspace ws => exact h.focusWorkspace ws
  | summonWorkspace ws =>
    simp only [State.raw]; split
    · exact h
    · exact (h.setActive _ ws).focusWorkspace ws
  | focusMonitor mon =>
    simp only [State.raw]; split
    · exact h.focusWorkspace _
    · exact h
  | moveWorkspaceToMonitor mon => exact h.setActive mon _
  | showWorkspace mon ws => exact h.setActive mon ws
  | moveWindowToWorkspace id ws focusFollows => exact moveWindowToWorkspace_struct h id ws focusFollows
  | placeWindow id ws index floating => exact placeWindow_struct h id ws index floating
  | setFloating id floating => exact setFloating_struct h id floating
  | toggleScratchpad name => exact toggleScratchpad_struct h name
  | windowAppeared id app ws floating index =>
    exact windowAppeared_struct h hinv.focusWs_not_home id app ws floating index
  | windowClosed id => exact windowClosed_struct h id
  | monitorsChanged mons forced => exact monitorsChanged_struct h mons forced
  | configChanged scratchpads forced => exact configChanged_struct h hinv.focusWs_not_home scratchpads forced
  | setLayout ws layout orientation => exact h.setProps ..
  | mfact ws change => exact h.setProps ..
  | setMonocle ws on => exact h.setProps ..
  | setParked id parked => exact (mapWins_struct h _ _).congr rfl rfl rfl rfl
  | centered id => exact mapWins_struct h _ _
  | geometryChanged geometry => exact h.congr rfl rfl rfl rfl
  | settingsChanged cfg => exact h.congr rfl rfl rfl rfl
  | setLastNonMaster ws id => exact h.congr rfl rfl rfl rfl
  | setFocusSync seen asked => exact h.congr rfl rfl rfl rfl

/-! ## The theorems -/

/-- Every op preserves the invariant -/
theorem step_inv {s : State} (h : Inv s) (op : Op) : Inv (s.step op) := by
  have hr := raw_struct h op
  exact hideUnfocused_inv _ (hr.screens.congr (by simp) (by simp)) (hr.windows.congr (by simp) (by simp))
    (fixFocus_focusOk _ hr.ne)

theorem init_inv (scratchpads : List Scratchpad) : Inv (init scratchpads) := by
  refine ⟨⟨by simp [init, monitors], by simp [init, visible], by simp [init, visible, WsName.isHome],
      by simp [init, allowedOn, forcedOf]⟩,
    ⟨by simp [init], by simp [init]⟩, ⟨by simp [init, visible], by simp [init]⟩, by simp [ScratchpadsOk, init]⟩

/-- The invariant holds in every state dwmac can reach -/
theorem reachable_inv (scratchpads : List Scratchpad) (ops : List Op) : Inv (ops.foldl step (init scratchpads)) :=
  foldl_invariant Inv step (fun _ op h => step_inv h op) ops _ (init_inv scratchpads)

/-- **A scratchpad home is never shown** -/
theorem home_never_shown (scratchpads : List Scratchpad) (ops : List Op) :
    ∀ ws ∈ (ops.foldl step (init scratchpads)).visible, ws.isHome = false :=
  (reachable_inv scratchpads ops).screens.noHome

/-- **A scratchpad is topmost or hidden**: each of its windows is in its home, or on the focused workspace with
the scratchpad focused -/
theorem scratchpad_topmost_or_hidden (scratchpads : List Scratchpad) (ops : List Op) :
    let s := ops.foldl step (init scratchpads)
    ∀ w ∈ s.windows, ∀ h, s.homeOf w.app = some h → w.ws = h ∨ (w.ws = s.focusWs ∧ s.focusedApp = some w.app) :=
  fun w hw h hh => ((reachable_inv scratchpads ops).scratchpads w hw h hh).2

end DwmacCore.Model
