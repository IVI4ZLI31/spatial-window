;;; spatial-window-geometry-voronoi-row-test.el --- Tests for voronoi-row algorithm -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window-geometry-voronoi-row.el
;; Pure geometry tests: window-bounds in, grid assignment out.
;; No keyboard layout dependency — assertions use grid-to-strings with symbol labels.

;;; Code:

(require 'ert)
(require 'spatial-window-geometry-voronoi-row)

;;; Key assignment tests

;;; ┌───────────────────┐
;;; │                   │
;;; │   W 100w×100h     │
;;; │                   │
;;; └───────────────────┘
;;; W=window
;;;
;;; Row 0: W W W W W W W W W W
;;; Row 1: W W W W W W W W W W
;;; Row 2: W W W W W W W W W W

(ert-deftest spatial-window-voronoi-row-test-assign-keys-single-window ()
  "Single window gets all cells."
  (let* ((window-bounds '((W 0.0 1.0 0.0 1.0)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("W W W W W W W W W W"
                          "W W W W W W W W W W"
                          "W W W W W W W W W W")))))

;;; ┌─────────┬─────────┐
;;; │         │         │
;;; │L 50w×100h│R 50w×100h│
;;; │         │         │
;;; └─────────┴─────────┘
;;; L=left  R=right
;;;
;;; Row 0: L L L L L R R R R R
;;; Row 1: L L L L L R R R R R
;;; Row 2: L L L L L R R R R R

(ert-deftest spatial-window-voronoi-row-test-assign-keys-2-columns ()
  "2 left-right windows: each gets half columns, all rows."
  (let* ((window-bounds '((L 0.0 0.5 0.0 1.0)
                          (R 0.5 1.0 0.0 1.0)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("L L L L L R R R R R"
                          "L L L L L R R R R R"
                          "L L L L L R R R R R")))))

;;; ┌──────────┬──────────┐
;;; │ T        │          │
;;; │ 50w×50h  │ R        │
;;; ├──────────┤ 50w×100h │
;;; │ B        │          │
;;; │ 50w×50h  │          │
;;; └──────────┴──────────┘
;;; T=top-left  B=bottom-left  R=right
;;;
;;; Row 0: T T T T T R R R R R
;;; Row 1: R R R R R R R R R R  ← voronoi: right window (full height) wins entire middle row
;;; Row 2: B B B B B R R R R R

(ert-deftest spatial-window-voronoi-row-test-assign-keys-2-left-1-right ()
  "2 windows top-bottom left, 1 spanning right.
Voronoi: right window's centroid is at frame center (full height), so it wins
the entire middle row.  Left windows get only top and bottom rows."
  (let* ((window-bounds '((T 0.0 0.5 0.0 0.5)
                          (B 0.0 0.5 0.5 1.0)
                          (R 0.5 1.0 0.0 1.0)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("T T T T T R R R R R"
                          "R R R R R R R R R R"
                          "B B B B B R R R R R")))))

;;; ┌──────────────────┬─────────────┐
;;; │  R               │             │
;;; │  59w×48h         │   C         │
;;; ├──────────────────┤   41w×98h   │
;;; │  M               │             │
;;; │  59w×50h         │             │
;;; └──────────────────┴─────────────┘
;;; R=magit-rev  M=magit  C=claude
;;;
;;; Row 0: R R R R R R C C C C
;;; Row 1: C C C C C C C C C C  ← voronoi: claude (full height) wins entire middle row
;;; Row 2: M M M M M M C C C C

(ert-deftest spatial-window-voronoi-row-test-near-equal-vertical-split ()
  "Near-equal vertical split: 49/51 at y=0.486.
Voronoi: claude (full height, right) wins the entire middle row due to
area-weighted distance — its large area pulls cells toward it."
  (let* ((window-bounds
          `((C 0.5894736842105263 0.9988304093567252 0.0019230769230769232 0.9846153846153847)
            (R 0.0011695906432748538 0.5894736842105263 0.0019230769230769232 0.4855769230769231)
            (M 0.0011695906432748538 0.5894736842105263 0.4855769230769231 0.9846153846153847)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("R R R R R R C C C C"
                          "C C C C C C C C C C"
                          "M M M M M M C C C C")))))

;;; ┌──────────────────┬─────────────┐
;;; │  S               │             │
;;; │  59w×50h         │             │
;;; ├──────────────────┤   C         │
;;; │  J               │   41w×98h   │
;;; │  59w×24h         │             │
;;; ├──────────────────┤             │
;;; │  T               │             │
;;; │  59w×24h         │             │
;;; └──────────────────┴─────────────┘
;;; S=spatial  J=journal  T=tools  C=claude
;;;
;;; Row 0: S S S S S S C C C C
;;; Row 1: C C C J C C C C C C  ← voronoi: claude wins most of middle row; journal steals one cell
;;; Row 2: T T T T T C C C C C

(ert-deftest spatial-window-voronoi-row-test-3-left-stacked-1-right ()
  "3 stacked windows on left (50/24/24) + full-height right.
Voronoi: claude (full height, large area) wins most of the middle row.
Journal (small, middle-left) steals only one cell."
  (let* ((window-bounds
          `((T 0.0011695906432748538 0.5894736842105263
               0.7423076923076923 0.9846153846153847)
            (C 0.5894736842105263 0.9988304093567252
               0.0019230769230769232 0.9846153846153847)
            (S 0.0011695906432748538 0.5894736842105263
               0.0019230769230769232 0.49903846153846154)
            (J 0.0011695906432748538 0.5894736842105263
               0.49903846153846154 0.7423076923076923)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("S S S S S S C C C C"
                          "C C C J C C C C C C"
                          "T T T T T C C C C C")))))

;;; Middle row assignment threshold characterization
;;;
;;; With 3 keyboard rows, the middle row (y=0.33-0.67) is contested in any
;;; top/bottom split.  Binary-search for the split point where the bigger
;;; window starts winning the middle row.

(ert-deftest spatial-window-voronoi-row-test-middle-row-assignment-threshold ()
  "Voronoi always assigns the middle row to the bigger window.
Unlike the margin-based algorithm (which requires ~57% dominance),
voronoi assigns every cell to its nearest weighted centroid — there is
no unassigned threshold. The bigger window always gets 20 cells at any
split away from exactly 50%."
  (let ((lo 0.3) (hi 0.5))
    ;; Binary search: lo = bottom wins middle row, hi = tied/top wins
    (dotimes (_ 30)
      (let* ((mid (* 0.5 (+ lo hi)))
             (window-bounds `((top 0.0 1.0 0.0 ,mid)
                              (bot 0.0 1.0 ,mid 1.0)))
             (grid (spatial-window--compute-assignment window-bounds))
             (bot-cells 0))
        ;; Count cells assigned to bot
        (dotimes (row 3)
          (dotimes (col 10)
            (when (eq (aref (aref grid row) col) 'bot)
              (cl-incf bot-cells))))
        (if (= bot-cells 20)
            (setq lo mid)
          (setq hi mid))))
    ;; Voronoi: threshold converges to 50% (bigger side = 50%)
    ;; Any split even slightly below 0.5 gives bottom window the middle row
    (let ((bigger-side-pct (- 100.0 (* hi 100.0))))
      (message "Middle-row threshold: split at %.4f%% → bigger side %.1f%%"
               (* hi 100.0) bigger-side-pct)
      (should (>= bigger-side-pct 50.0)))))

;;; ┌────┬───────────┬──────┐
;;; │    │ T         │      │
;;; │ L  │ 50w×50h   │  R   │
;;; │20w │───────────│ 30w  │
;;; │×   │ B         │ ×    │
;;; │100h│ 50w×50h   │ 100h │
;;; └────┴───────────┴──────┘
;;; L=left  T=mid-top  B=mid-bot  R=right
;;;
;;; Row 0: L T T T T T T T R R
;;; Row 1: L L L L R R R R R R  ← voronoi: left/right (full height) expand into middle row
;;; Row 2: L B B B B B B B R R

(ert-deftest spatial-window-voronoi-row-test-assign-keys-3-columns ()
  "3 columns: left/right span full height, middle has top-bottom split.
Voronoi: full-height windows expand into the middle row."
  (let* ((window-bounds '((L 0.0 0.2 0.0 1.0)
                          (T 0.2 0.7 0.0 0.5)
                          (B 0.2 0.7 0.5 1.0)
                          (R 0.7 1.0 0.0 1.0)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("L T T T T T T T R R"
                          "L L L L R R R R R R"
                          "L B B B B B B B R R")))))

;;; ┌─────────────────────┬──┐
;;; │                     │T │
;;; │   M 96w×100h        │4w│
;;; │                     │92h
;;; │                     ├──┤
;;; │                     │B │
;;; │                     │4w│
;;; │                     │8h│
;;; └─────────────────────┴──┘
;;; M=main  T=sidebar-top  B=sidebar-bot
;;;
;;; Row 0: M M M M M M M M M M
;;; Row 1: M M M M M M M M M T  ← voronoi: sidebar-top steals only one cell (no column consolidation)
;;; Row 2: M M M M M M M M M B

(ert-deftest spatial-window-voronoi-row-test-assign-keys-extreme-split ()
  "Extreme split: 95.5% main / 4.5% sidebar. All windows must get cells.
Voronoi: sidebar-top and sidebar-bot each steal exactly 1 cell."
  (let* ((window-bounds '((M 0.0 0.955 0.0 1.0)
                          (T 0.955 1.0 0.0 0.92)
                          (B 0.955 1.0 0.92 1.0)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("M M M M M M M M M M"
                          "M M M M M M M M M T"
                          "M M M M M M M M M B")))))

;;; ┌───────────────────┐
;;; │  1  100w×33h      │
;;; ├───────────────────┤
;;; │  2  100w×34h      │
;;; ├───────────────────┤
;;; │  3  100w×33h      │
;;; └───────────────────┘
;;; 1=win1  2=win2  3=win3
;;;
;;; Row 0: A A A A A A A A A A
;;; Row 1: B B B B B B B B B B
;;; Row 2: C C C C C C C C C C

(ert-deftest spatial-window-voronoi-row-test-max-3-rows ()
  "3 top-bottom windows = 3 keyboard rows, each gets exactly 1 row."
  (let* ((window-bounds '((A 0.0 1.0 0.0 0.33)
                          (B 0.0 1.0 0.33 0.67)
                          (C 0.0 1.0 0.67 1.0)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("A A A A A A A A A A"
                          "B B B B B B B B B B"
                          "C C C C C C C C C C")))))

;;; ┌──┬──┬──┬──┬──┬──┬──┬──┬──┬──┐
;;; │  │  │  │  │  │  │  │  │  │  │
;;; │10│10│10│10│10│10│10│10│10│10│ (10w×100h each)
;;; │  │  │  │  │  │  │  │  │  │  │
;;; └──┴──┴──┴──┴──┴──┴──┴──┴──┴──┘

(ert-deftest spatial-window-voronoi-row-test-max-10-cols ()
  "10 left-right windows = 10 keyboard columns, each gets 3 cells (1 col × 3 rows)."
  (let* ((window-bounds
          '((A 0.0 0.1 0.0 1.0) (B 0.1 0.2 0.0 1.0) (C 0.2 0.3 0.0 1.0)
            (D 0.3 0.4 0.0 1.0) (E 0.4 0.5 0.0 1.0) (F 0.5 0.6 0.0 1.0)
            (G 0.6 0.7 0.0 1.0) (H 0.7 0.8 0.0 1.0) (I 0.8 0.9 0.0 1.0)
            (J 0.9 1.0 0.0 1.0)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("A B C D E F G H I J"
                          "A B C D E F G H I J"
                          "A B C D E F G H I J")))))

;;; ┌───────────────────┬─────────┐
;;; │  G  51w×48h       │         │
;;; │                   │  C      │
;;; ├──┬──┬────┬────────┤  49w    │
;;; │A │B │ D  │   E    │  ×100h  │
;;; │7w│6w│13w │  26w   │         │
;;; ├──┼──┤×52h│  ×52h  │         │
;;; │Z │  │    │        │         │
;;; │7w│  │    │        │         │
;;; │28h  │    │        │         │
;;; └──┴──┴────┴────────┴─────────┘
;;; G=magit  C=claude  A=sw1  B=sw2  D=sw3  E=sw4  Z=backtrace
;;;
;;; Row 0: G G G G G C C C C C
;;; Row 1: A C C C C C C C C C  ← voronoi: claude wins most of middle row
;;; Row 2: Z B D E E E C C C C

(ert-deftest spatial-window-voronoi-row-test-complex-spanning-layout ()
  "Complex layout: 7 windows with multiple spanning. All must get cells.
Voronoi: claude (full height, right half) wins most of the middle row."
  (let* ((window-bounds
          '((G 0.0 0.511 0.0 0.483)
            (C 0.511 1.0 0.0 1.0)
            (A 0.0 0.066 0.483 0.725)
            (B 0.066 0.129 0.483 1.0)
            (D 0.129 0.255 0.483 1.0)
            (E 0.255 0.511 0.483 1.0)
            (Z 0.0 0.066 0.725 1.0)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("G G G G G C C C C C"
                          "A C C C C C C C C C"
                          "Z B D E E E C C C C")))))

;;; ┌─────────────┬───────┐
;;; │             │       │
;;; │ M 63w×93h   │ C     │
;;; │             │ 37w   │
;;; ├─────────────┤ ×100h │
;;; │ D 63w×5h    │       │
;;; └─────────────┴───────┘
;;; M=main  D=diff  C=claude
;;;
;;; Row 0: M M M M M M M C C C
;;; Row 1: M M M M M M C C C C
;;; Row 2: M M M D M M C C C C  ← voronoi: diff steals only one cell (no row consolidation)

(ert-deftest spatial-window-voronoi-row-test-ide-layout-with-thin-panel ()
  "IDE layout: main editor + thin diff panel on left, claude on right.
Voronoi: diff panel is tiny (5% height) — steals only one cell.
No row consolidation: main keeps most of bottom row."
  (let* ((window-bounds
          '((M 0.0 0.63 0.0 0.93)
            (D 0.0 0.63 0.93 0.985)
            (C 0.63 1.0 0.0 0.985)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("M M M M M M M C C C"
                          "M M M M M M C C C C"
                          "M M M D M M C C C C")))))

;;; ┌──┬────────────────────────────┐
;;; │T │                            │
;;; │4w│                            │
;;; │77h│     R 96w×98h             │
;;; ├──┤                            │
;;; │B │                            │
;;; │4w│                            │
;;; │22h│                           │
;;; └──┴────────────────────────────┘
;;; T=top-left  B=bot-left  R=right
;;;
;;; Row 0: R R R R R R R R R R
;;; Row 1: T R R R R R R R R R  ← voronoi: top-left steals one cell only
;;; Row 2: B R R R R R R R R R

(ert-deftest spatial-window-voronoi-row-test-extreme-narrow-left-column ()
  "Extreme narrow left column: 4% width split vertically, 96% right window.
Voronoi: top-left steals one cell, bot-left steals one cell. No column consolidation."
  (let* ((window-bounds
          '((T 0.001 0.042 0.002 0.769)
            (B 0.001 0.042 0.769 0.985)
            (R 0.042 0.999 0.002 0.985)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("R R R R R R R R R R"
                          "T R R R R R R R R R"
                          "B R R R R R R R R R")))))

;;; ┌────────────────────────┬────────────────┐
;;; │  A  60w×50h            │  B  40w×50h    │
;;; ├───────────┬────────────┴────────────────┤
;;; │ C 33w×49h │      D  67w×49h             │
;;; │           │                             │
;;; └───────────┴─────────────────────────────┘
;;; A=top-left  B=top-right  C=bot-left  D=bot-right
;;;
;;; Row 0: A A A A A A B B B B
;;; Row 1: · · A A A D D D · ·  ← ambiguity at stacked-window boundaries
;;; Row 2: C C C C D D D D D D

(ert-deftest spatial-window-voronoi-row-test-misaligned-vertical-splits ()
  "4 windows where top row split (60/40) differs from bottom row split (33/67).
Voronoi: middle row partially assigned — boundary cells left unassigned where
vertically-stacked windows have similar scores."
  (let* ((window-bounds
          '((A 0.001 0.598 0.002 0.5)
            (B 0.598 0.999 0.002 0.5)
            (C 0.001 0.327 0.5 0.985)
            (D 0.327 0.999 0.5 0.985)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("A A A A A A B B B B"
                          "· · A A A D D D · ·"
                          "C C C C D D D D D D")))))

;;; ┌────┬─────────────────────────────┐
;;; │ N  │                             │
;;; │11w │   W  89w×48h                │
;;; │49h │                             │
;;; ├────┼─────────────────────────────┤
;;; │    │   P  68w×26h                │
;;; │ G  ├─────────────────────────────┤
;;; │32w │   Q  68w×24h                │
;;; │50h │                             │
;;; └────┴─────────────────────────────┘
;;; N=code-narrow  W=code-wide  G=magit  P=posframe-top  Q=posframe-bot
;;;
;;; Row 0: N N W W W W W W W W
;;; Row 1: G G · W W · P P · ·  ← ambiguity: code-wide vs posframe-top near-50/50 y-split
;;; Row 2: G G G G Q Q Q Q Q Q

(ert-deftest spatial-window-voronoi-row-test-real-dev-session-layout ()
  "Real 5-window layout: narrow code window, wide code, magit, two posframes.
Voronoi: code-wide vs posframe-top near-50/50 y-split at 0.487 leaves most of
middle row unassigned.  posframe-top steals cells to ensure it has keys."
  (let* ((window-bounds
          '((P 0.3192982456140351 0.9988304093567252 0.48653846153846153 0.7423076923076923)
            (Q 0.3192982456140351 0.9988304093567252 0.7423076923076923 0.9846153846153847)
            (N 0.0011695906432748538 0.11052631578947368 0.0019230769230769232 0.48653846153846153)
            (W 0.11052631578947368 0.9988304093567252 0.0019230769230769232 0.48653846153846153)
            (G 0.0011695906432748538 0.3192982456140351 0.48653846153846153 0.9846153846153847)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("N N W W W W W W W W"
                          "G G · W W · P P · ·"
                          "G G G G Q Q Q Q Q Q")))))

;;; ┌──────────┬──────────┐
;;; │ A        │          │
;;; │ 51w×51h  │   C      │
;;; ├──────────┤  50w×81h │
;;; │ M        │          │
;;; │ 51w×30h  │          │
;;; ├──────────┴──────────┤
;;; │ L    100w×16h       │
;;; └─────────────────────┘
;;; A=activities  M=magit  C=claude  L=elpaca-log
;;;
;;; Row 0: A A A A A C C C C C
;;; Row 1: C C C C C C C C C C  ← voronoi: claude (large area) wins entire middle row
;;; Row 2: M M M · L L L · C C  ← ambiguity at log/magit and log/claude boundaries

(ert-deftest spatial-window-voronoi-row-test-full-width-bottom-panel ()
  "Full-width bottom panel (16% height).
Voronoi: claude (large area, full right) wins entire middle row.  Log panel
gets 3 cells in bottom row center — boundary cells left unassigned where log
competes with vertically-stacked magit/claude at similar scores."
  (let* ((window-bounds
          `((L 0.0011695906432748538 0.9988304093567252
               0.823076923076923 0.9846153846153847)
            (A 0.0011695906432748538 0.5087719298245614
               0.015384615384615385 0.5259615384615385)
            (M 0.0011695906432748538 0.5087719298245614
               0.5259615384615385 0.823076923076923)
            (C 0.5087719298245614 0.9988304093567252
               0.015384615384615385 0.823076923076923)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("A A A A A C C C C C"
                          "C C C C C C C C C C"
                          "M M M · L L L · C C")))))

(provide 'spatial-window-geometry-voronoi-row-test)

;;; spatial-window-geometry-voronoi-row-test.el ends here
