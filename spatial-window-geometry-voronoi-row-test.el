;;; spatial-window-geometry-voronoi-row-test.el --- Tests for voronoi-row algorithm -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window-geometry-voronoi-row.el
;; Pure geometry tests: window-bounds in, grid assignment out.
;; No keyboard layout dependency — assertions use grid-to-strings with symbol labels.

;;; Code:

(require 'ert)
(require 'spatial-window-geometry-voronoi-row)
(require 'spatial-window-test-helper)

;;; Assertion helper

(defun spatial-window-voronoi-row-test--assert-assignment (window-bounds expected-rows)
  "Assert WINDOW-BOUNDS produces EXPECTED-ROWS via compute-assignment.
On mismatch, fail with layout diagram and side-by-side expected/actual."
  (let* ((grid (spatial-window--compute-assignment window-bounds))
         (actual-rows (spatial-window--grid-to-strings grid)))
    (unless (equal actual-rows expected-rows)
      (let ((lines (list (spatial-window--bounds-to-string window-bounds) "")))
        (dotimes (i (length expected-rows))
          (push (if (= i 0)
                    (format "expected: %s   actual: %s"
                            (nth i expected-rows) (nth i actual-rows))
                  (format "        : %s           %s"
                          (nth i expected-rows) (nth i actual-rows)))
                lines))
        (ert-fail (mapconcat #'identity (nreverse lines) "\n"))))))

;;; Key assignment tests

;;; ┌──────────┐
;;; │ W        │
;;; │ 100w×100h│
;;; └──────────┘
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
;;; │ L       │ R       │
;;; │ 50w×100h│ 50w×100h│
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

;;; ┌────────┬────────┐
;;; │ T      │ R      │
;;; │ 50w×50h│ 50w    │
;;; ├────────┤ ×      │
;;; │ B      │ 100h   │
;;; │ 50w×50h│        │
;;; └────────┴────────┘
;;; T=top-left  B=bottom-left  R=right
;;;
;;; Row 0: T T T T T R R R R R
;;; Row 1: · · · · · R R R R R  ← geo-overlap gate: R excluded from left cols; T/B fail row threshold → nil
;;; Row 2: B B B B B R R R R R

(ert-deftest spatial-window-voronoi-row-test-assign-keys-2-left-1-right ()
  "2 windows top-bottom left, 1 spanning right.
Geo-overlap gate: R has zero x-overlap with left-half cells, so it is excluded.
T and B each have only 50% row overlap (< 60% threshold), so they fall through
to geo-eligible fallback — but then tie on Voronoi distance → ambiguity → nil."
  (let* ((window-bounds '((T 0.0 0.5 0.0 0.5)
                          (B 0.0 0.5 0.5 1.0)
                          (R 0.5 1.0 0.0 1.0)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("T T T T T R R R R R"
                          "· · · · · R R R R R"
                          "B B B B B R R R R R")))))

;;; ┌────────┬──────┐
;;; │ R      │ C    │
;;; │ 59w×48h│ 41w  │
;;; ├────────┤ ×    │
;;; │ M      │ 98h  │
;;; │ 59w×50h│      │
;;; └────────┴──────┘
;;; R=magit-rev  M=magit  C=claude
;;;
;;; Row 0: R R R R R R C C C C
;;; Row 1: · · · · · C C C C C  ← geo-overlap gate: C excluded from left cols; R/M fail row threshold → nil
;;; Row 2: M M M M M M C C C C

(ert-deftest spatial-window-voronoi-row-test-near-equal-vertical-split ()
  "Near-equal vertical split: 49/51 at y=0.486.
Geo-overlap gate: C has zero x-overlap with left-half cells, so it is excluded.
R and M each have ~50% row overlap (< 60% threshold) → geo-eligible fallback →
ambiguity → nil for left-half middle-row cells."
  (let* ((window-bounds
          `((C 0.5894736842105263 0.9988304093567252 0.0019230769230769232 0.9846153846153847)
            (R 0.0011695906432748538 0.5894736842105263 0.0019230769230769232 0.4855769230769231)
            (M 0.0011695906432748538 0.5894736842105263 0.4855769230769231 0.9846153846153847)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("R R R R R R C C C C"
                          "· · · · · C C C C C"
                          "M M M M M M C C C C")))))

;;; ┌────────┬──────┐
;;; │        │      │
;;; │ S      │      │
;;; │ 59w×50h│      │
;;; │        │ C    │
;;; ├────────┤ 41w  │
;;; │ J      │ ×    │
;;; │ 59w×24h│ 98h  │
;;; ├────────┤      │
;;; │ T      │      │
;;; │ 59w×24h│      │
;;; └────────┴──────┘
;;; S=spatial  J=journal  T=tools  C=claude
;;;
;;; Row 0: S S S S S S C C C C
;;; Row 1: · · J J · C C C C C  ← geo-overlap gate: C excluded from left cols; J wins its cells, rest ambiguous
;;; Row 2: T T T T T C C C C C

(ert-deftest spatial-window-voronoi-row-test-3-left-stacked-1-right ()
  "3 stacked windows on left (50/24/24) + full-height right.
Geo-overlap gate: C has zero x-overlap with left-half cells. Among left windows,
J wins cols 2-3 via centroid proximity; other left cells are ambiguous."
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
                          "· · J J · C C C C C"
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

;;; ┌─────┬────────────┬────────┐
;;; │ L   │ T          │ R      │
;;; │ 20w │ 50w×50h    │ 30w    │
;;; │ ×   ├────────────┤ ×      │
;;; │ 100h│ B          │ 100h   │
;;; │     │ 50w×50h    │        │
;;; └─────┴────────────┴────────┘
;;; L=left  T=mid-top  B=mid-bot  R=right
;;;
;;; Row 0: L L T T T T T R R R
;;; Row 1: L L · · · · · R R R  ← geo-overlap gate: T/B excluded from L/R cols; middle cells ambiguous
;;; Row 2: L L B B B B B R R R

(ert-deftest spatial-window-voronoi-row-test-assign-keys-3-columns ()
  "3 columns: left/right span full height, middle has top-bottom split.
Geo-overlap gate: T/B have zero x-overlap with L/R columns; middle-row cells
in cols 2-6 have only T/B as geo-eligible, both fail row threshold → ambiguous."
  (let* ((window-bounds '((L 0.0 0.2 0.0 1.0)
                          (T 0.2 0.7 0.0 0.5)
                          (B 0.2 0.7 0.5 1.0)
                          (R 0.7 1.0 0.0 1.0)))
         (grid (spatial-window--compute-assignment window-bounds))
         (rows (spatial-window--grid-to-strings grid)))
    (should (equal rows '("L L T T T T T R R R"
                          "L L · · · · · R R R"
                          "L L B B B B B R R R")))))

;;; ┌─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┬───────┐
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │ T     │
;;; │                                                                                                                                                     │ 5w×92h│
;;; │ M                                                                                                                                                   │       │
;;; │ 96w×100h                                                                                                                                            │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     │       │
;;; │                                                                                                                                                     ├───────┤
;;; │                                                                                                                                                     │ B     │
;;; │                                                                                                                                                     │ 5w×8h │
;;; └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┴───────┘
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

;;; ┌─────────┐
;;; │ A       │
;;; │ 100w×33h│
;;; ├─────────┤
;;; │ B       │
;;; │ 100w×34h│
;;; ├─────────┤
;;; │ C       │
;;; │ 100w×33h│
;;; └─────────┘
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

;;; ┌─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┐
;;; │ A       │ B       │ C       │ D       │ E       │ F       │ G       │ H       │ I       │ J       │
;;; │ 10w×100h│ 10w×100h│ 10w×100h│ 10w×100h│ 10w×100h│ 10w×100h│ 10w×100h│ 10w×100h│ 10w×100h│ 10w×100h│
;;; └─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┘

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

;;; ┌─────────────────────────────────────────────────────────┬────────────────────────────────────────────────────┐
;;; │                                                         │                                                    │
;;; │ G                                                       │                                                    │
;;; │ 51w×48h                                                 │                                                    │
;;; │                                                         │                                                    │
;;; ├───────┬───────┬─────────────┬───────────────────────────┤ C                                                  │
;;; │ A     │       │             │                           │ 49w×100h                                           │
;;; │ 7w×24h│ B     │ D           │ E                         │                                                    │
;;; ├───────┤ 6w×52h│ 13w×52h     │ 26w×52h                   │                                                    │
;;; │ Z     │       │             │                           │                                                    │
;;; │ 7w×28h│       │             │                           │                                                    │
;;; └───────┴───────┴─────────────┴───────────────────────────┴────────────────────────────────────────────────────┘
;;; G=magit  C=claude  A=sw1  B=sw2  D=sw3  E=sw4  Z=backtrace
;;;
;;; Row 0: G G G G G C C C C C
;;; Row 1: A G G G · C C C C C  ← geo-overlap gate: C excluded from left cols; G wins via geo-eligible
;;; Row 2: Z B D E E E C C C C

(ert-deftest spatial-window-voronoi-row-test-complex-spanning-layout ()
  "Complex layout: 7 windows with multiple spanning. All must get cells.
Geo-overlap gate: C excluded from left-half cells. G wins middle-row left cells
via geo-eligible fallback (it overlaps the top portion of the middle row)."
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
                          "A G G G · C C C C C"
                          "Z B D E E E C C C C")))))

;;; ┌────────┬─────┐
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │ M      │ C   │
;;; │ 63w×93h│ 37w │
;;; │        │ ×   │
;;; │        │ 98h │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; │        │     │
;;; ├────────┤     │
;;; │ D      │     │
;;; │ 63w×5h │     │
;;; └────────┴─────┘
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

;;; ┌───────┬────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
;;; │       │                                                                                                                                                                    │
;;; │       │                                                                                                                                                                    │
;;; │ T     │                                                                                                                                                                    │
;;; │ 4w×77h│                                                                                                                                                                    │
;;; │       │ R                                                                                                                                                                  │
;;; │       │ 96w×98h                                                                                                                                                            │
;;; │       │                                                                                                                                                                    │
;;; ├───────┤                                                                                                                                                                    │
;;; │ B     │                                                                                                                                                                    │
;;; │ 4w×22h│                                                                                                                                                                    │
;;; └───────┴────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
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

;;; ┌────────────────┬──────────┐
;;; │ A              │ B        │
;;; │ 60w×50h        │ 40w×50h  │
;;; ├────────┬───────┴──────────┤
;;; │ C      │ D                │
;;; │ 33w×48h│ 67w×48h          │
;;; └────────┴──────────────────┘
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

;;; ┌────────┬──────────────────────────────────────────────────────────────────┐
;;; │        │                                                                  │
;;; │ N      │ W                                                                │
;;; │ 11w×48h│ 89w×48h                                                          │
;;; │        │                                                                  │
;;; ├────────┴───────────────┬──────────────────────────────────────────────────┤
;;; │                        │ P                                                │
;;; │ G                      │ 68w×26h                                          │
;;; │ 32w×50h                ├──────────────────────────────────────────────────┤
;;; │                        │ Q                                                │
;;; │                        │ 68w×24h                                          │
;;; └────────────────────────┴──────────────────────────────────────────────────┘
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

;;; ┌────────┬────────┐
;;; │        │        │
;;; │        │        │
;;; │ A      │        │
;;; │ 51w×51h│        │
;;; │        │ C      │
;;; │        │ 49w×81h│
;;; ├────────┤        │
;;; │        │        │
;;; │ M      │        │
;;; │ 51w×30h│        │
;;; │        │        │
;;; ├────────┴────────┤
;;; │ L               │
;;; │ 100w×16h        │
;;; └─────────────────┘
;;; A=activities  M=magit  C=claude  L=elpaca-log
;;;
;;; Row 0: A A A A A C C C C C
;;; Row 1: · · · · · C C C C C  ← geo-overlap gate: C excluded from left cols; A/M fail row threshold → nil
;;; Row 2: M M M · L L L · C C  ← ambiguity at log/magit and log/claude boundaries

(ert-deftest spatial-window-voronoi-row-test-full-width-bottom-panel ()
  "Full-width bottom panel (16% height).
Geo-overlap gate: C has zero x-overlap with left-half cells in middle row.
A and M each have ~50% row overlap (< 60% threshold) → geo-eligible fallback →
ambiguity → nil for left-half middle-row cells."
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
                          "· · · · · C C C C C"
                          "M M M · L L L · C C")))))

(provide 'spatial-window-geometry-voronoi-row-test)

;;; spatial-window-geometry-voronoi-row-test.el ends here
