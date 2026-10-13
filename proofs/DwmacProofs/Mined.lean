import DwmacProofs.Model

/-!
# Invariants from other window managers' bug reports

Properties that AeroSpace, i3 and dwm users found broken in those window managers, and dwmac's model had broken too
(scenarios in test/DwmacTest/Command.lean)
-/

namespace DwmacCore.Model
open State

/-! ## Focus prefers windows macOS shows (AeroSpace #1614, #18) -/

/-- **A shown window is preferred**: when `ws` has a window macOS shows (not a hidden app's, not in native
fullscreen), the window dwmac picks to focus there is one -/
theorem mruWin_shown {s : State} {ws : WsName} {id : WinId} (h : s.mruWin ws = some id)
    (hw : ∃ w ∈ s.windows, w.ws = ws ∧ w.parked = false) :
    ∃ w ∈ s.windows, w.id = id ∧ w.ws = ws ∧ w.parked = false := by
  unfold mruWin at h
  split at h
  · rename_i hf
    cases h
    obtain ⟨w, hm, hc⟩ := List.any_eq_true.mp (mem_of_find?_some hf).2
    simp only [Bool.and_eq_true, beq_iff_eq, Bool.not_eq_true'] at hc
    exact ⟨w, hm, hc.1.1, hc.1.2, hc.2⟩
  split at h
  · rename_i w hf
    cases h
    have := mem_of_find?_some hf
    simp only [Bool.and_eq_true, beq_iff_eq, Bool.not_eq_true'] at this
    exact ⟨w, this.1, rfl, this.2⟩
  · rename_i hnone
    obtain ⟨w, hm, hws, hp⟩ := hw
    have := List.find?_eq_none.mp hnone w hm
    simp [hws, hp] at this

theorem hideUnfocused_mem {s : State} {w : Win} (h : w ∈ s.windows) :
    ∃ w' ∈ s.hideUnfocused.windows, w'.id = w.id ∧ w'.parked = w.parked := by
  rw [hideUnfocused_eq]
  refine ⟨hideOne s w, List.mem_map_of_mem h, hideOne_id s w, ?_⟩
  unfold hideOne; split <;> (try split) <;> rfl

/-- **A window macOS takes away loses focus**: when the focused window's app is hidden or it goes native fullscreen,
focus moves to a window of the workspace that macOS still shows, if there is one. (If macOS then reports the
fullscreen window focused, dwmac follows: that's a choice macOS made, not one dwmac makes for it) -/
theorem setParked_moves_focus {s : State} (hinv : Inv s) {id : WinId} (hf : s.focusWin = some id)
    (hw : ∃ w ∈ s.windows, w.ws = s.focusWs ∧ w.id ≠ id ∧ w.parked = false) :
    ∃ w ∈ (s.step (.setParked id true)).windows, (s.step (.setParked id true)).focusWin = some w.id ∧
      w.parked = false := by
  let r := s.raw (.setParked id true)
  have hr : r = { s with windows := s.mapWins (·.id == id) ({ · with parked := true }), focusWin := none } := by
    simp [r, State.raw, hf]
  -- The workspace stays shown, so only the window is chosen again
  have hws : r.fixFocusWs = r := by
    have : r.focusWs ∈ r.visible := by
      rw [hr]; simpa [visible] using hinv.focus.shown
    unfold fixFocusWs
    rw [if_pos (List.elem_iff.mpr this)]
  have hwin : r.fixFocusWin.focusWin = r.mruWin s.focusWs := by
    simp [fixFocusWin, focusWinValid, hr]
  obtain ⟨w, hm, hwsw, hne, hp⟩ := hw
  have hm' : w ∈ r.windows := by
    rw [hr]; simp only [mapWins, List.mem_map]
    exact ⟨w, hm, by simp [hne]⟩
  have hmru : ∃ id', r.mruWin s.focusWs = some id' := by
    unfold mruWin; split
    · exact ⟨_, rfl⟩
    split
    · exact ⟨_, rfl⟩
    · rename_i hnone
      have := List.find?_eq_none.mp hnone w hm'
      simp [hwsw, hp] at this
  obtain ⟨id', hid'⟩ := hmru
  obtain ⟨v, hv, rfl, _, hvp⟩ := mruWin_shown hid' ⟨w, hm', hwsw, hp⟩
  have hfix : (s.raw (.setParked id true)).fixFocus = r.fixFocusWin := by
    simp only [fixFocus]; rw [show s.raw (.setParked id true) = r from rfl, hws]
  obtain ⟨v', hv', hid, hp'⟩ := hideUnfocused_mem (s := r.fixFocusWin) (by rw [fixFocusWin_windows]; exact hv)
  refine ⟨v', ?_, ?_, hp' ▸ hvp⟩
  · simpa [step, hfix] using hv'
  · simp only [step, hfix, hideUnfocused_eq]; rw [hwin, hid', hid]

/-! ## A forced workspace follows its assignment (AeroSpace #520, #2173; i3 #2936) -/

theorem mem_replaceAt_snd : ∀ {l : List (MonId × WsName)} {m : MonId} (w : WsName),
    m ∈ l.map (·.1) → w ∈ (replaceAt l m w).map (·.2)
  | [], _, _, h => by simp at h
  | p :: t, m, w, h => by
    simp only [replaceAt, List.map_cons, List.mem_cons, List.map_map] at h ⊢
    by_cases hp : p.1 = m
    · left; simp [hp]
    · right
      rcases h with h | h
      · exact absurd h.symm hp
      · have := mem_replaceAt_snd (l := t) w h
        simpa [replaceAt, List.map_map, Function.comp_def] using this

theorem vacate_monitors (s : State) (ws : WsName) : (s.vacate ws).monitors = s.monitors := by
  unfold vacate monitors; split
  · simp only; exact replaceAt_fst ..
  · rfl

/-- `setActive` shows the workspace, when it may be shown there -/
theorem setActive_shows {s : State} {mon : MonId} {ws : WsName} (hhome : ws.isHome = false)
    (hmon : mon ∈ s.monitors) (hallow : s.allowedOn ws mon = true) : ws ∈ (s.setActive mon ws).visible := by
  unfold setActive
  split
  · rename_i hc
    simp only [hhome, Bool.false_or, Bool.or_eq_true, Bool.not_eq_true', List.contains_iff_mem] at hc
    rcases hc with (hc | hc) | hc
    · exact absurd hmon (by simpa using hc)
    · simp [hallow] at hc
    · have := monitorOf_some (beq_iff_eq.mp hc)
      exact List.mem_map.mpr ⟨_, this, rfl⟩
  · simp only [visible]
    apply mem_replaceAt_snd
    have := vacate_monitors s ws
    simp only [monitors] at this hmon
    rw [this]; exact hmon

theorem followForced_focus (prev t : State) (hws : t.focusWs = prev.focusWs) :
    (prev.followForced t).focusWs = prev.focusWs := by
  unfold followForced
  simp only
  have P := foldl_invariant (fun u : State => u.focusWs = prev.focusWs)
    followStep
    (fun a b ha => by
      unfold followStep
      split
      · unfold setActive; split
        · exact ha
        · simpa [vacate] using (by unfold vacate; split <;> exact ha : (a.vacate b).focusWs = prev.focusWs)
      · exact ha)
    (prev.visible.filter (· != prev.focusWs)) t hws
  split
  · rfl
  · exact P

theorem setActive_monitors (s : State) (mon : MonId) (ws : WsName) : (s.setActive mon ws).monitors = s.monitors := by
  unfold setActive; split
  · rfl
  · simp only [monitors]; rw [replaceAt_fst]; exact vacate_monitors s ws

theorem addScreen_monitors_sub (t : State) (mon : MonId) : ∀ x ∈ t.monitors, x ∈ (t.addScreen mon).monitors := by
  intro x hx; unfold addScreen; split
  · exact hx
  · simp [monitors] at hx ⊢; exact .inl hx

theorem foldl_addScreen_monitors : ∀ (mons : List MonId) (t : State), ∀ m ∈ mons, m ∈ (mons.foldl addScreen t).monitors
  | [], _, _, h => by simp at h
  | b :: l, t, m, h => by
    simp only [List.foldl_cons]
    rcases List.mem_cons.mp h with rfl | h
    · -- `m` is a monitor of `t.addScreen m`, and monitors only grow
      have hm : m ∈ (t.addScreen m).monitors := by
        unfold addScreen; split
        · rename_i hc; simpa using hc
        · simp [monitors]
      clear h
      induction l generalizing t with
      | nil => exact hm
      | cons c l ih =>
        simp only [List.foldl_cons]
        exact foldl_mono l _ _ (addScreen_monitors_sub _ c m hm)
    · exact foldl_addScreen_monitors l _ m h
where
  foldl_mono : ∀ (l : List MonId) (t : State) (m : MonId), m ∈ t.monitors → m ∈ (l.foldl addScreen t).monitors
    | [], _, _, h => h
    | c :: l, t, m, h => foldl_mono l _ m (addScreen_monitors_sub t c m h)

/-- **Focus follows a force assignment**: when monitors change or the config is reloaded, and the focused workspace is
now forced onto a connected monitor, it stays focused and shown (there), rather than leaving the screen -/
theorem monitorsChanged_keeps_forced_focus (s : State) (mons : List MonId) (forced : List (WsName × MonId)) (m : MonId)
    (hm : m ∈ mons) (hf : ({ s with forced } : State).forcedOf s.focusWs = some m) (hhome : s.focusWs.isHome = false) :
    (s.monitorsChanged mons forced).focusWs = s.focusWs ∧ s.focusWs ∈ (s.monitorsChanged mons forced).visible := by
  unfold monitorsChanged
  simp only
  generalize hs' : mons.foldl addScreen (s.keepScreens mons forced) = s'
  -- `s'` has every monitor, `forced`, and `s`'s focus
  have hmon : m ∈ s'.monitors := hs' ▸ foldl_addScreen_monitors mons _ m hm
  have hP := foldl_invariant (fun u : State => u.forced = forced ∧ u.focusWs = s.focusWs) addScreen
    (fun a b ⟨h1, h2⟩ => by unfold addScreen; split <;> exact ⟨h1, h2⟩) mons (s.keepScreens mons forced) ⟨rfl, rfl⟩
  rw [hs'] at hP
  have hne : s'.screens ≠ [] := by
    intro he; simp [monitors, he] at hmon
  simp only [hne, List.isEmpty_iff, if_false]
  refine ⟨followForced_focus s s' hP.2, ?_⟩
  unfold followForced
  simp only
  -- The other workspaces moving keeps the monitors, the assignment and the focus
  have hQ := foldl_invariant (fun u : State => u.monitors = s'.monitors ∧ u.forced = forced)
    followStep
    (fun a b ⟨h1, h2⟩ => by
      unfold followStep; split
      · exact ⟨(setActive_monitors ..).trans h1, (setActive_forced ..).trans h2⟩
      · exact ⟨h1, h2⟩)
    (s.visible.filter (· != s.focusWs)) s' ⟨rfl, hP.1⟩
  generalize (s.visible.filter (· != s.focusWs)).foldl followStep s' = u at hQ ⊢
  have hfu : u.forcedOf s.focusWs = some m := by
    simpa [forcedOf, hQ.2] using hf
  have hmu : m ∈ u.monitors := hQ.1 ▸ hmon
  split
  · rename_i hsome
    simp only [focusOn, visible] at hsome ⊢
    have := setActive_shows (s := u) hhome hmu (by simp [allowedOn, hfu])
    simpa [workspaceMonitor, hfu, visible] using this
  · rename_i hnone
    simp only [forcedHere, hfu] at hnone
    by_cases hv : s.focusWs ∈ u.visible
    · exact hv
    · exfalso; apply hnone; simp [Option.filter, hmu, hv, hhome]
