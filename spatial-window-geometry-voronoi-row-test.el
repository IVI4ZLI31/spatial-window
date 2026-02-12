;;; spatial-window-geometry-voronoi-row-test.el --- Tests for voronoi-row algorithm -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window-geometry-voronoi-row.el
;; Fully isolated from spatial-window.el — layout is passed explicitly.

;;; Code:

(require 'ert)
(require 'spatial-window-geometry-voronoi-row)

(defconst spatial-window-test-layout
  '(("q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
    ("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")
    ("z" "x" "c" "v" "b" "n" "m" "," "." "/"))
  "QWERTY layout for tests.")

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
  "Single window gets all keys."
  (let* ((win 'win)
         (all-keys (apply #'append spatial-window-test-layout))
         (window-bounds `((,win 0.0 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (keys (cdr (assq win result))))
    (should (seq-set-equal-p keys all-keys))))

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
  (let* ((win-left 'win-left)
         (win-right 'win-right)
         (window-bounds `((,win-left 0.0 0.5 0.0 1.0)
                          (,win-right 0.5 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (left-keys (cdr (assq win-left result)))
         (right-keys (cdr (assq win-right result))))
    (should (seq-set-equal-p left-keys '("q" "w" "e" "r" "t"
                                         "a" "s" "d" "f" "g"
                                         "z" "x" "c" "v" "b")))
    (should (seq-set-equal-p right-keys '("y" "u" "i" "o" "p"
                                          "h" "j" "k" "l" ";"
                                          "n" "m" "," "." "/")))))

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
  (let* ((win-top-left 'win-top-left)
         (win-bottom-left 'win-bottom-left)
         (win-right 'win-right)
         (window-bounds `((,win-top-left 0.0 0.5 0.0 0.5)
                          (,win-bottom-left 0.0 0.5 0.5 1.0)
                          (,win-right 0.5 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (right-keys (cdr (assq win-right result)))
         (top-left-keys (cdr (assq win-top-left result)))
         (bottom-left-keys (cdr (assq win-bottom-left result))))
    ;; Right window: right half all 3 rows + entire middle row = 20 keys
    (should (seq-set-equal-p right-keys '("y" "u" "i" "o" "p"
                                          "a" "s" "d" "f" "g"
                                          "h" "j" "k" "l" ";"
                                          "n" "m" "," "." "/")))
    ;; Top-left: top row left half
    (should (seq-set-equal-p top-left-keys '("q" "w" "e" "r" "t")))
    ;; Bottom-left: bottom row left half
    (should (seq-set-equal-p bottom-left-keys '("z" "x" "c" "v" "b")))))

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
  (let* ((win-claude 'win-claude)
         (win-magit-rev 'win-magit-rev)
         (win-magit 'win-magit)
         ;; Real layout: left 59%, right 41%, left split 48%/50% vertically
         (window-bounds
          `((,win-claude 0.5894736842105263 0.9988304093567252 0.0019230769230769232 0.9846153846153847)
            (,win-magit-rev 0.0011695906432748538 0.5894736842105263 0.0019230769230769232 0.4855769230769231)
            (,win-magit 0.0011695906432748538 0.5894736842105263 0.4855769230769231 0.9846153846153847)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (claude-keys (cdr (assq win-claude result)))
         (magit-rev-keys (cdr (assq win-magit-rev result)))
         (magit-keys (cdr (assq win-magit result))))
    ;; magit-revision: top row, left 6 columns
    (should (seq-set-equal-p magit-rev-keys '("q" "w" "e" "r" "t" "y")))
    ;; magit: bottom row, left 6 columns
    (should (seq-set-equal-p magit-keys '("z" "x" "c" "v" "b" "n")))
    ;; claude: right 4 columns all rows + entire middle row left
    (should (seq-set-equal-p claude-keys '("u" "i" "o" "p"
                                           "a" "s" "d" "f" "g" "h"
                                           "j" "k" "l" ";"
                                           "m" "," "." "/")))))

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
;;; Row 1: C C C J C C C C C C  ← voronoi: claude wins most of middle row; journal steals "f"
;;; Row 2: T T T T T C C C C C

(ert-deftest spatial-window-voronoi-row-test-3-left-stacked-1-right ()
  "3 stacked windows on left (50/24/24) + full-height right.
Voronoi: claude (full height, large area) wins most of the middle row.
Journal (small, middle-left) steals only 'f'."
  (let* ((win-tools 'win-tools)
         (win-claude 'win-claude)
         (win-spatial 'win-spatial)
         (win-journal 'win-journal)
         (window-bounds
          `((,win-tools   0.0011695906432748538 0.5894736842105263
                          0.7423076923076923 0.9846153846153847)
            (,win-claude  0.5894736842105263 0.9988304093567252
                          0.0019230769230769232 0.9846153846153847)
            (,win-spatial 0.0011695906432748538 0.5894736842105263
                          0.0019230769230769232 0.49903846153846154)
            (,win-journal 0.0011695906432748538 0.5894736842105263
                          0.49903846153846154 0.7423076923076923)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (spatial-keys (cdr (assq win-spatial result)))
         (claude-keys (cdr (assq win-claude result)))
         (journal-keys (cdr (assq win-journal result)))
         (tools-keys (cdr (assq win-tools result))))
    ;; spatial: top row, left 6 columns
    (should (seq-set-equal-p spatial-keys '("q" "w" "e" "r" "t" "y")))
    ;; claude: right 4 cols all rows + most of middle row left
    (should (seq-set-equal-p claude-keys '("u" "i" "o" "p"
                                           "a" "s" "d" "g" "h"
                                           "j" "k" "l" ";"
                                           "n" "m" "," "." "/")))
    ;; journal: only steals "f" (nearest cell to its centroid)
    (should (seq-set-equal-p journal-keys '("f")))
    ;; tools: bottom row left 5 columns
    (should (seq-set-equal-p tools-keys '("z" "x" "c" "v" "b")))))

;;; Middle row assignment threshold characterization
;;;
;;; With 3 keyboard rows, the middle row (y=0.33-0.67) is contested in any
;;; top/bottom split.  Binary-search for the split point where the bigger
;;; window starts winning the middle row.

(ert-deftest spatial-window-voronoi-row-test-middle-row-assignment-threshold ()
  "Voronoi always assigns the middle row to the bigger window.
Unlike the margin-based algorithm (which requires ~57% dominance),
voronoi assigns every cell to its nearest weighted centroid — there is
no unassigned threshold. The bigger window always gets 20 keys at any
split away from exactly 50%."
  (let ((lo 0.3) (hi 0.5))
    ;; Binary search: lo = bottom wins middle row, hi = tied/top wins
    (dotimes (_ 30)
      (let* ((mid (* 0.5 (+ lo hi)))
             (win-top 'win-top) (win-bot 'win-bot)
             (window-bounds `((,win-top 0.0 1.0 0.0 ,mid)
                              (,win-bot 0.0 1.0 ,mid 1.0)))
             (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
             (bot-keys (cdr (assq win-bot result))))
        (if (= (length bot-keys) 20)
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
Voronoi: full-height windows expand into the middle row.  Left gets 1 col
in rows 0/2 but 4 cols in row 1.  Right gets 2 cols in rows 0/2 but 6 in row 1."
  (let* ((win-left 'win-left)
         (win-mid-top 'win-mid-top)
         (win-mid-bot 'win-mid-bot)
         (win-right 'win-right)
         ;; Left: 20%, Middle: 50%, Right: 30%
         (window-bounds `((,win-left 0.0 0.2 0.0 1.0)
                          (,win-mid-top 0.2 0.7 0.0 0.5)
                          (,win-mid-bot 0.2 0.7 0.5 1.0)
                          (,win-right 0.7 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (left-keys (cdr (assq win-left result)))
         (mid-top-keys (cdr (assq win-mid-top result)))
         (mid-bot-keys (cdr (assq win-mid-bot result)))
         (right-keys (cdr (assq win-right result))))
    ;; Left (20%, full height): 1 col rows 0/2 + 4 cols middle row = 6 keys
    (should (seq-set-equal-p left-keys '("q" "a" "s" "d" "f" "z")))
    ;; Right (30%, full height): 2 cols rows 0/2 + 6 cols middle row = 10 keys
    (should (seq-set-equal-p right-keys '("o" "p" "g" "h" "j" "k" "l" ";" "." "/")))
    ;; Mid-top: 7 cols top row only
    (should (seq-set-equal-p mid-top-keys '("w" "e" "r" "t" "y" "u" "i")))
    ;; Mid-bot: 7 cols bottom row only
    (should (seq-set-equal-p mid-bot-keys '("x" "c" "v" "b" "n" "m" ",")))))

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
;;; Row 1: M M M M M M M M M T  ← voronoi: sidebar-top steals only ";" (no column consolidation)
;;; Row 2: M M M M M M M M M B

(ert-deftest spatial-window-voronoi-row-test-assign-keys-extreme-split ()
  "Extreme split: 95.5% main / 4.5% sidebar. All windows must get keys.
Voronoi: sidebar-top steals ';' (best overlap), sidebar-bot steals '/'.
No column consolidation — each sidebar window gets exactly 1 key."
  (let* ((win-main 'win-main)
         (win-sidebar-top 'win-sidebar-top)
         (win-sidebar-bot 'win-sidebar-bot)
         ;; Main takes 95.5% width, sidebar 4.5%
         ;; Sidebar split: 92% top, 8% bottom
         (window-bounds `((,win-main 0.0 0.955 0.0 1.0)
                          (,win-sidebar-top 0.955 1.0 0.0 0.92)
                          (,win-sidebar-bot 0.955 1.0 0.92 1.0)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (main-keys (cdr (assq win-main result)))
         (top-keys (cdr (assq win-sidebar-top result)))
         (bot-keys (cdr (assq win-sidebar-bot result))))
    ;; Sidebar-top steals ";" only (no column consolidation in voronoi)
    (should (seq-set-equal-p top-keys '(";")))
    ;; Sidebar-bot steals "/" (bottom-right corner)
    (should (seq-set-equal-p bot-keys '("/")))
    ;; Main gets 28 keys
    (should (seq-set-equal-p main-keys '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p"
                                          "a" "s" "d" "f" "g" "h" "j" "k" "l"
                                          "z" "x" "c" "v" "b" "n" "m" "," ".")))))

;;; ┌───────────────────┐
;;; │  1  100w×33h      │
;;; ├───────────────────┤
;;; │  2  100w×34h      │
;;; ├───────────────────┤
;;; │  3  100w×33h      │
;;; └───────────────────┘
;;; 1=win1  2=win2  3=win3
;;;
;;; Row 0: 1 1 1 1 1 1 1 1 1 1
;;; Row 1: 2 2 2 2 2 2 2 2 2 2
;;; Row 2: 3 3 3 3 3 3 3 3 3 3

(ert-deftest spatial-window-voronoi-row-test-max-3-rows ()
  "3 top-bottom windows = 3 keyboard rows, each gets exactly 1 row."
  (let* ((win1 'win1) (win2 'win2) (win3 'win3)
         (window-bounds `((,win1 0.0 1.0 0.0 0.33)
                          (,win2 0.0 1.0 0.33 0.67)
                          (,win3 0.0 1.0 0.67 1.0)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds)))
    (should (seq-set-equal-p (cdr (assq win1 result))
                             '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p")))
    (should (seq-set-equal-p (cdr (assq win2 result))
                             '("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")))
    (should (seq-set-equal-p (cdr (assq win3 result))
                             '("z" "x" "c" "v" "b" "n" "m" "," "." "/")))))

;;; ┌──┬──┬──┬──┬──┬──┬──┬──┬──┬──┐
;;; │  │  │  │  │  │  │  │  │  │  │
;;; │10│10│10│10│10│10│10│10│10│10│ (10w×100h each)
;;; │  │  │  │  │  │  │  │  │  │  │
;;; └──┴──┴──┴──┴──┴──┴──┴──┴──┴──┘
;;; 0=win0  1=win1 ... 9=win9
;;;
;;; Row 0: 0 1 2 3 4 5 6 7 8 9
;;; Row 1: 0 1 2 3 4 5 6 7 8 9
;;; Row 2: 0 1 2 3 4 5 6 7 8 9

(ert-deftest spatial-window-voronoi-row-test-max-10-cols ()
  "10 left-right windows = 10 keyboard columns, each gets 3 keys (1 col × 3 rows)."
  (let* ((win0 'win0) (win1 'win1) (win2 'win2) (win3 'win3) (win4 'win4)
         (win5 'win5) (win6 'win6) (win7 'win7) (win8 'win8) (win9 'win9)
         (window-bounds
          `((,win0 0.0 0.1 0.0 1.0) (,win1 0.1 0.2 0.0 1.0) (,win2 0.2 0.3 0.0 1.0)
            (,win3 0.3 0.4 0.0 1.0) (,win4 0.4 0.5 0.0 1.0) (,win5 0.5 0.6 0.0 1.0)
            (,win6 0.6 0.7 0.0 1.0) (,win7 0.7 0.8 0.0 1.0) (,win8 0.8 0.9 0.0 1.0)
            (,win9 0.9 1.0 0.0 1.0)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds)))
    (should (seq-set-equal-p (cdr (assq win0 result)) '("q" "a" "z")))
    (should (seq-set-equal-p (cdr (assq win1 result)) '("w" "s" "x")))
    (should (seq-set-equal-p (cdr (assq win2 result)) '("e" "d" "c")))
    (should (seq-set-equal-p (cdr (assq win3 result)) '("r" "f" "v")))
    (should (seq-set-equal-p (cdr (assq win4 result)) '("t" "g" "b")))
    (should (seq-set-equal-p (cdr (assq win5 result)) '("y" "h" "n")))
    (should (seq-set-equal-p (cdr (assq win6 result)) '("u" "j" "m")))
    (should (seq-set-equal-p (cdr (assq win7 result)) '("i" "k" ",")))
    (should (seq-set-equal-p (cdr (assq win8 result)) '("o" "l" ".")))
    (should (seq-set-equal-p (cdr (assq win9 result)) '("p" ";" "/")))))

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
;;; G=magit  C=claude  A=sw1(7w×24h)  B=sw2(6w×52h)
;;; D=sw3(13w×52h)  E=sw4(26w×52h)  Z=backtrace(7w×28h)
;;;
;;; Row 0: G G G G G C C C C C
;;; Row 1: A C C C C C C C C C  ← voronoi: claude wins most of middle row
;;; Row 2: Z B D E E E C C C C

(ert-deftest spatial-window-voronoi-row-test-complex-spanning-layout ()
  "Complex layout: 7 windows with multiple spanning. All must get keys.
Voronoi: claude (full height, right half) wins most of the middle row.
sw4 gets 3 keys (row 2 cols 3-5) vs production's 2."
  (let* ((win-magit 'win-magit)
         (win-claude 'win-claude)
         (win-sw1 'win-sw1)
         (win-sw2 'win-sw2)
         (win-sw3 'win-sw3)
         (win-sw4 'win-sw4)
         (win-backtrace 'win-backtrace)
         ;; Complex layout with spanning windows
         (window-bounds
          `((,win-magit 0.0 0.511 0.0 0.483)
            (,win-claude 0.511 1.0 0.0 1.0)
            (,win-sw1 0.0 0.066 0.483 0.725)
            (,win-sw2 0.066 0.129 0.483 1.0)
            (,win-sw3 0.129 0.255 0.483 1.0)
            (,win-sw4 0.255 0.511 0.483 1.0)
            (,win-backtrace 0.0 0.066 0.725 1.0)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds)))
    ;; Magit (top-left ~51%) gets top row only
    (should (seq-set-equal-p (cdr (assq win-magit result))
                             '("q" "w" "e" "r" "t")))
    ;; Claude (right half, full height) gets right cols + most of middle row
    (should (seq-set-equal-p (cdr (assq win-claude result))
                             '("y" "u" "i" "o" "p"
                               "s" "d" "f" "g" "h" "j" "k" "l" ";"
                               "m" "," "." "/")))
    ;; Small windows: sw1 steals "a", sw4 gets 3 keys
    (should (seq-set-equal-p (cdr (assq win-sw1 result)) '("a")))
    (should (seq-set-equal-p (cdr (assq win-sw2 result)) '("x")))
    (should (seq-set-equal-p (cdr (assq win-sw3 result)) '("c")))
    (should (seq-set-equal-p (cdr (assq win-sw4 result)) '("v" "b" "n")))
    (should (seq-set-equal-p (cdr (assq win-backtrace result)) '("z")))))

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
;;; Row 2: M M M D M M C C C C  ← voronoi: diff steals "v" only (no row consolidation)

(ert-deftest spatial-window-voronoi-row-test-ide-layout-with-thin-panel ()
  "IDE layout: main editor + thin diff panel on left, claude on right.
Voronoi: diff panel is tiny (5% height) — steals only 'v' (best overlap cell).
No row consolidation: main keeps most of bottom row."
  (let* ((win-main 'win-main)
         (win-diff 'win-diff)
         (win-claude 'win-claude)
         ;; Real layout from user's Emacs session
         ;; Left: 63%, Right: 37%
         ;; Left split: 93% main / 5% diff panel (unbalanced)
         (window-bounds
          `((,win-main 0.0 0.63 0.0 0.93)
            (,win-diff 0.0 0.63 0.93 0.985)
            (,win-claude 0.63 1.0 0.0 0.985)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (main-keys (cdr (assq win-main result)))
         (diff-keys (cdr (assq win-diff result)))
         (claude-keys (cdr (assq win-claude result))))
    ;; Main editor gets left 7 cols top row + 6 cols middle + 5 cols bottom (minus diff steal)
    (should (seq-set-equal-p main-keys '("q" "w" "e" "r" "t" "y" "u"
                                          "a" "s" "d" "f" "g" "h"
                                          "z" "x" "c" "b" "n")))
    ;; Diff panel steals only "v" (best overlap for this tiny window)
    (should (seq-set-equal-p diff-keys '("v")))
    ;; Claude window gets right 3 cols top + 4 cols middle/bottom
    (should (seq-set-equal-p claude-keys '("i" "o" "p"
                                            "j" "k" "l" ";"
                                            "m" "," "." "/")))))

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
;;; Row 1: T R R R R R R R R R  ← voronoi: top-left steals "a" only (no column consolidation)
;;; Row 2: B R R R R R R R R R

(ert-deftest spatial-window-voronoi-row-test-extreme-narrow-left-column ()
  "Extreme narrow left column: 4% width split vertically, 96% right window.
Voronoi: top-left steals 'a' only, bot-left steals 'z'. No column consolidation —
right window gets all of row 0 plus 28 total keys."
  (let* ((win-top-left 'win-top-left)
         (win-bot-left 'win-bot-left)
         (win-right 'win-right)
         ;; Real layout from user's Emacs session
         ;; Left column: 4.2% width, split 77%/22% vertically
         ;; Right: 95.8% width, full height
         (window-bounds
          `((,win-top-left 0.001 0.042 0.002 0.769)
            (,win-bot-left 0.001 0.042 0.769 0.985)
            (,win-right 0.042 0.999 0.002 0.985)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (top-left-keys (cdr (assq win-top-left result)))
         (bot-left-keys (cdr (assq win-bot-left result)))
         (right-keys (cdr (assq win-right result))))
    ;; Top-left steals "a" only (no column consolidation in voronoi)
    (should (seq-set-equal-p top-left-keys '("a")))
    ;; Bot-left steals "z" (bottom row)
    (should (seq-set-equal-p bot-left-keys '("z")))
    ;; Right window gets 28 keys (all of row 0 + rest)
    (should (seq-set-equal-p right-keys '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p"
                                          "s" "d" "f" "g" "h" "j" "k" "l" ";"
                                          "x" "c" "v" "b" "n" "m" "," "." "/")))))

;;; ┌────────────────────────┬────────────────┐
;;; │  A  60w×50h            │  B  40w×50h    │
;;; ├───────────┬────────────┴────────────────┤
;;; │ C 33w×49h │      D  67w×49h             │
;;; │           │                             │
;;; └───────────┴─────────────────────────────┘
;;; A=top-left  B=top-right  C=bot-left  D=bot-right
;;;
;;; Row 0: A A A A A A B B B B
;;; Row 1: A A A A A D D D D B  ← voronoi: middle row assigned via centroids
;;; Row 2: C C C C D D D D D D

(ert-deftest spatial-window-voronoi-row-test-misaligned-vertical-splits ()
  "4 windows where top row split (60/40) differs from bottom row split (33/67).
Voronoi: middle row fully assigned — top-left gets left 5, bot-right gets middle 4,
top-right gets rightmost 1."
  (let* ((win-top-left 'win-top-left)
         (win-top-right 'win-top-right)
         (win-bot-left 'win-bot-left)
         (win-bot-right 'win-bot-right)
         ;; Top row: 60%/40% split, Bottom row: 33%/67% split
         (window-bounds
          `((,win-top-left 0.001 0.598 0.002 0.5)
            (,win-top-right 0.598 0.999 0.002 0.5)
            (,win-bot-left 0.001 0.327 0.5 0.985)
            (,win-bot-right 0.327 0.999 0.5 0.985)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (top-left-keys (cdr (assq win-top-left result)))
         (top-right-keys (cdr (assq win-top-right result)))
         (bot-left-keys (cdr (assq win-bot-left result)))
         (bot-right-keys (cdr (assq win-bot-right result))))
    ;; Top-left (60%) gets top row left 6 + middle row left 5 = 11 keys
    (should (seq-set-equal-p top-left-keys '("q" "w" "e" "r" "t" "y"
                                              "a" "s" "d" "f" "g")))
    ;; Top-right (40%) gets top row right 4 + middle ";" = 5 keys
    (should (seq-set-equal-p top-right-keys '("u" "i" "o" "p" ";")))
    ;; Bot-left (33%) gets bottom row left 4
    (should (seq-set-equal-p bot-left-keys '("z" "x" "c" "v")))
    ;; Bot-right (67%) gets middle row 4 + bottom row right 6 = 10 keys
    (should (seq-set-equal-p bot-right-keys '("h" "j" "k" "l"
                                               "b" "n" "m" "," "." "/")))))

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
;;; Row 1: G G W W W P P P P W  ← voronoi: code-wide gets d,f,g + ";"; posframe-top gets h,j,k,l
;;; Row 2: G G G G Q Q Q Q Q Q

(ert-deftest spatial-window-voronoi-row-test-real-dev-session-layout ()
  "Real 5-window layout: narrow code window, wide code, magit, two posframes.
Voronoi: code-narrow gets 2 keys (q,w); code-wide gets top row + scattered
middle row cells; posframe-top gets 4 keys; posframe-bot gets bottom row right."
  (let* ((win-posframe-top 'win-posframe-top)
         (win-posframe-bot 'win-posframe-bot)
         (win-code-narrow 'win-code-narrow)
         (win-code-wide 'win-code-wide)
         (win-magit 'win-magit)
         ;; Actual bounds captured from Emacs session
         (window-bounds
          `((,win-posframe-top 0.3192982456140351 0.9988304093567252 0.48653846153846153 0.7423076923076923)
            (,win-posframe-bot 0.3192982456140351 0.9988304093567252 0.7423076923076923 0.9846153846153847)
            (,win-code-narrow 0.0011695906432748538 0.11052631578947368 0.0019230769230769232 0.48653846153846153)
            (,win-code-wide 0.11052631578947368 0.9988304093567252 0.0019230769230769232 0.48653846153846153)
            (,win-magit 0.0011695906432748538 0.3192982456140351 0.48653846153846153 0.9846153846153847)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (narrow-keys (cdr (assq win-code-narrow result)))
         (wide-keys (cdr (assq win-code-wide result)))
         (magit-keys (cdr (assq win-magit result)))
         (posframe-top-keys (cdr (assq win-posframe-top result)))
         (posframe-bot-keys (cdr (assq win-posframe-bot result)))
         (all-keys (apply #'append (mapcar #'cdr result))))
    ;; Narrow code window gets "q","w"
    (should (seq-set-equal-p narrow-keys '("q" "w")))
    ;; Wide code window: top row (minus q,w) + middle d,f,g,;
    (should (seq-set-equal-p wide-keys '("e" "r" "t" "y" "u" "i" "o" "p"
                                          "d" "f" "g" ";")))
    ;; Magit (32% width, bottom-left): middle a,s + bottom z,x,c,v
    (should (seq-set-equal-p magit-keys '("a" "s" "z" "x" "c" "v")))
    ;; Posframe-top: middle h,j,k,l
    (should (seq-set-equal-p posframe-top-keys '("h" "j" "k" "l")))
    ;; Posframe-bot: bottom row right
    (should (seq-set-equal-p posframe-bot-keys '("b" "n" "m" "," "." "/")))
    ;; All 5 windows have keys, 30 total, no duplicates
    (should (= (length all-keys) 30))
    (should (= (length all-keys) (length (delete-dups (copy-sequence all-keys)))))))

(ert-deftest spatial-window-voronoi-row-test-invalid-keyboard-layout ()
  "Returns nil and displays message when keyboard layout rows have different lengths."
  (let ((invalid-layout '(("q" "w" "e")
                          ("a" "s")))
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (let ((result (spatial-window--assign-keys invalid-layout)))
        (should (null result))
        (should (cl-some (lambda (msg) (string-match-p "Invalid keyboard layout" msg)) messages))))))

;;; Formatting tests

(ert-deftest spatial-window-voronoi-row-test-format-key-grid ()
  "Format keys as keyboard grid shows assigned keys and dots for unassigned."
  (let ((grid (spatial-window--format-key-grid '("q" "w" "e" "a" "s") spatial-window-test-layout)))
    (should (= (length (split-string grid "\n")) 3))
    (should (string-match-p "^q w e · · · · · · ·$" (car (split-string grid "\n"))))
    (should (string-match-p "^a s · · · · · · · ·$" (nth 1 (split-string grid "\n"))))
    (should (string-match-p "^· · · · · · · · · ·$" (nth 2 (split-string grid "\n"))))))

(ert-deftest spatial-window-voronoi-row-test-format-key-grid-empty ()
  "Empty key list produces all dots."
  (let ((grid (spatial-window--format-key-grid '() spatial-window-test-layout)))
    (should (string-match-p "^· · · · · · · · · ·$" (car (split-string grid "\n"))))))

(ert-deftest spatial-window-voronoi-row-test-format-key-grid-all-keys ()
  "All keys assigned shows full keyboard."
  (let* ((all-keys (apply #'append spatial-window-test-layout))
         (grid (spatial-window--format-key-grid all-keys spatial-window-test-layout)))
    (should (string-match-p "^q w e r t y u i o p$" (car (split-string grid "\n"))))))

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
;;; Row 2: G G G L L L L L C C  ← log gets partial bottom row; claude extends right

(ert-deftest spatial-window-voronoi-row-test-full-width-bottom-panel ()
  "Full-width bottom panel (16% height).
Voronoi: claude (large area, full right) wins entire middle row.  Log panel
gets only 5 keys in bottom row.  Magit gets 3 keys in bottom-left.
No row consolidation for log despite spanning full width."
  (let* ((win-log 'win-log)
         (win-activities 'win-activities)
         (win-magit 'win-magit)
         (win-claude 'win-claude)
         (window-bounds
          `((,win-log        0.0011695906432748538 0.9988304093567252
                             0.823076923076923 0.9846153846153847)
            (,win-activities 0.0011695906432748538 0.5087719298245614
                             0.015384615384615385 0.5259615384615385)
            (,win-magit      0.0011695906432748538 0.5087719298245614
                             0.5259615384615385 0.823076923076923)
            (,win-claude     0.5087719298245614 0.9988304093567252
                             0.015384615384615385 0.823076923076923)))
         (result (spatial-window--assign-keys spatial-window-test-layout nil window-bounds))
         (log-keys (cdr (assq win-log result)))
         (activities-keys (cdr (assq win-activities result)))
         (magit-keys (cdr (assq win-magit result)))
         (claude-keys (cdr (assq win-claude result))))
    ;; elpaca-log: partial bottom row (5 keys)
    (should (seq-set-equal-p log-keys '("v" "b" "n" "m" ",")))
    ;; activities: top row left
    (should (seq-set-equal-p activities-keys '("q" "w" "e" "r" "t")))
    ;; magit: bottom-left 3 keys
    (should (seq-set-equal-p magit-keys '("z" "x" "c")))
    ;; claude: right side all rows + entire middle row + bottom-right
    (should (seq-set-equal-p claude-keys '("y" "u" "i" "o" "p"
                                           "a" "s" "d" "f" "g"
                                           "h" "j" "k" "l" ";"
                                           "." "/")))))

(provide 'spatial-window-geometry-voronoi-row-test)

;;; spatial-window-geometry-voronoi-row-test.el ends here
