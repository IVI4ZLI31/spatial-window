;;; spatial-window-geometry-test.el --- Tests for spatial-window-geometry -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window-geometry.el

;;; Code:

(require 'ert)
(require 'spatial-window-geometry)
(require 'spatial-window)  ; for spatial-window--get-layout

;;; Boundary computation tests

(ert-deftest spatial-window-test-compute-boundaries-equal-split ()
  "Equal 50/50 split divides keys evenly."
  (let ((bounds (spatial-window--compute-boundaries '(0.5 0.5) 10)))
    (should (equal bounds '((0 . 4) (5 . 9))))))

(ert-deftest spatial-window-test-compute-boundaries-unequal-split ()
  "30/70 split gives proportional keys."
  (let ((bounds (spatial-window--compute-boundaries '(0.3 0.7) 10)))
    (should (equal bounds '((0 . 2) (3 . 9))))))

(ert-deftest spatial-window-test-compute-boundaries-minimum-1-key ()
  "Each cell gets at least 1 key even with tiny percentages."
  (let ((bounds (spatial-window--compute-boundaries '(0.95 0.05) 10)))
    ;; Both cells must have at least 1 key
    (should (>= (1+ (- (cdr (nth 0 bounds)) (car (nth 0 bounds)))) 1))
    (should (>= (1+ (- (cdr (nth 1 bounds)) (car (nth 1 bounds)))) 1))
    ;; Boundaries should cover all keys 0-9
    (should (= (car (nth 0 bounds)) 0))
    (should (= (cdr (nth 1 bounds)) 9))))

(ert-deftest spatial-window-test-compute-boundaries-too-many-cells ()
  "Returns nil when more cells than keys."
  (should (null (spatial-window--compute-boundaries '(0.25 0.25 0.25 0.25) 3))))

;;; Boundary lookup tests

(ert-deftest spatial-window-test-boundary-lookup ()
  "Key index maps to correct grid cell."
  (let ((bounds '((0 . 4) (5 . 9))))
    (should (= (spatial-window--boundary-lookup 0 bounds) 0))
    (should (= (spatial-window--boundary-lookup 4 bounds) 0))
    (should (= (spatial-window--boundary-lookup 5 bounds) 1))
    (should (= (spatial-window--boundary-lookup 9 bounds) 1))))

;;; Key assignment tests

(ert-deftest spatial-window-test-assign-keys-single-window ()
  "Single window gets all keys."
  (let* ((win 'win)
         (info-grid `(((:window ,win))))
         (cell-pcts '((1.0) . (1.0)))
         (result (spatial-window--assign-keys nil info-grid cell-pcts))
         (keys (cdr (assq win result))))
    (should (= (length keys) 30))))

(ert-deftest spatial-window-test-assign-keys-2-columns ()
  "2 left-right windows: each gets half columns, all rows."
  (let* ((win-left 'win-left)
         (win-right 'win-right)
         (info-grid `(((:window ,win-left)
                       (:window ,win-right))))
         (cell-pcts '((0.5 0.5) . (1.0)))
         (result (spatial-window--assign-keys nil info-grid cell-pcts))
         (left-keys (cdr (assq win-left result)))
         (right-keys (cdr (assq win-right result))))
    (should (seq-set-equal-p left-keys '("q" "w" "e" "r" "t"
                                         "a" "s" "d" "f" "g"
                                         "z" "x" "c" "v" "b")))
    (should (seq-set-equal-p right-keys '("y" "u" "i" "o" "p"
                                          "h" "j" "k" "l" ";"
                                          "n" "m" "," "." "/")))))

(ert-deftest spatial-window-test-assign-keys-2-left-1-right ()
  "2 windows top-bottom left, 1 spanning right: right gets all 3 rows."
  (let* ((win-top-left 'win-top-left)
         (win-bottom-left 'win-bottom-left)
         (win-right 'win-right)
         (info-grid `(((:window ,win-top-left)
                       (:window ,win-right))
                      ((:window ,win-bottom-left)
                       (:window ,win-right))))
         (cell-pcts '((0.5 0.5) . (0.5 0.5)))
         (result (spatial-window--assign-keys nil info-grid cell-pcts))
         (right-keys (cdr (assq win-right result)))
         (top-left-keys (cdr (assq win-top-left result)))
         (bottom-left-keys (cdr (assq win-bottom-left result)))
         (left-keys (append top-left-keys bottom-left-keys))
         (middle-row '("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")))
    ;; Right window: all 3 rows, right half = 15 keys
    (should (= (length right-keys) 15))
    ;; Right window includes middle row (h, j, k, l, ;)
    (should (seq-set-equal-p (seq-intersection right-keys middle-row)
                             '("h" "j" "k" "l" ";")))
    ;; Left windows exclude middle row entirely
    (should (null (seq-intersection left-keys middle-row)))
    ;; Top-left: top row left half
    (should (seq-set-equal-p top-left-keys '("q" "w" "e" "r" "t")))
    ;; Bottom-left: bottom row left half
    (should (seq-set-equal-p bottom-left-keys '("z" "x" "c" "v" "b")))))

(ert-deftest spatial-window-test-assign-keys-unbalanced-split ()
  "75/25 vertical split: larger window gets middle row, smaller gets bottom only."
  (let* ((win-top 'win-top)
         (win-bottom 'win-bottom)
         (info-grid `(((:window ,win-top))
                      ((:window ,win-bottom))))
         (cell-pcts '((1.0) . (0.75 0.25)))
         (result (spatial-window--assign-keys nil info-grid cell-pcts))
         (top-keys (cdr (assq win-top result)))
         (bottom-keys (cdr (assq win-bottom result)))
         (middle-row '("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")))
    ;; Top window (75%) should get top AND middle rows = 20 keys
    (should (= (length top-keys) 20))
    ;; Top window includes middle row
    (should (seq-set-equal-p (seq-intersection top-keys middle-row) middle-row))
    ;; Bottom window (25%) gets only bottom row = 10 keys
    (should (= (length bottom-keys) 10))
    ;; Bottom window should NOT have middle row
    (should (null (seq-intersection bottom-keys middle-row)))))

(ert-deftest spatial-window-test-assign-keys-3-columns ()
  "3 columns: left/right span full height, middle has top-bottom split."
  (let* ((win-left 'win-left)
         (win-mid-top 'win-mid-top)
         (win-mid-bot 'win-mid-bot)
         (win-right 'win-right)
         (info-grid `(((:window ,win-left)
                       (:window ,win-mid-top)
                       (:window ,win-right))
                      ((:window ,win-left)
                       (:window ,win-mid-bot)
                       (:window ,win-right))))
         (cell-pcts '((0.18 0.57 0.25) . (0.5 0.5)))
         (result (spatial-window--assign-keys nil info-grid cell-pcts))
         (left-keys (cdr (assq win-left result)))
         (mid-top-keys (cdr (assq win-mid-top result)))
         (mid-bot-keys (cdr (assq win-mid-bot result)))
         (right-keys (cdr (assq win-right result))))
    (should (seq-set-equal-p left-keys '("q" "w" "a" "s" "z" "x")))
    (should (seq-set-equal-p right-keys '("i" "o" "p" "k" "l" ";" "," "." "/")))
    ;; Mid-top: cols 2-6, top row only (middle row skipped for 2-way split)
    (should (seq-set-equal-p mid-top-keys '("e" "r" "t" "y" "u")))
    ;; Mid-bot: cols 2-6, bottom row only
    (should (seq-set-equal-p mid-bot-keys '("c" "v" "b" "n" "m")))))

(ert-deftest spatial-window-test-assign-keys-extreme-split ()
  "User's config: 95.5%/4.2% with 92%/6% sidebar split. All windows get keys."
  (let* ((win-main 'win-main)
         (win-sidebar-top 'win-sidebar-top)
         (win-sidebar-bot 'win-sidebar-bot)
         (info-grid `(((:window ,win-main)
                       (:window ,win-main))
                      ((:window ,win-sidebar-top)
                       (:window ,win-sidebar-bot))))
         (cell-pcts '((0.9554 0.0446) . (0.9807 0.0193)))
         (result (spatial-window--assign-keys nil info-grid cell-pcts))
         (main-keys (cdr (assq win-main result)))
         (top-keys (cdr (assq win-sidebar-top result)))
         (bot-keys (cdr (assq win-sidebar-bot result))))
    (should (>= (length main-keys) 1))
    (should (>= (length top-keys) 1))
    (should (>= (length bot-keys) 1))))

(ert-deftest spatial-window-test-max-3-rows ()
  "3 top-bottom windows = 3 keyboard rows, each gets exactly 1 row."
  (let* ((win1 'win1) (win2 'win2) (win3 'win3)
         (info-grid `(((:window ,win1))
                      ((:window ,win2))
                      ((:window ,win3))))
         (cell-pcts '((1.0) . (0.33 0.34 0.33)))
         (result (spatial-window--assign-keys nil info-grid cell-pcts)))
    (should (= (length (cdr (assq win1 result))) 10))
    (should (= (length (cdr (assq win2 result))) 10))
    (should (= (length (cdr (assq win3 result))) 10))))

(ert-deftest spatial-window-test-max-10-cols ()
  "10 left-right windows = 10 keyboard columns, each gets 3 keys (1 col × 3 rows)."
  (let* ((wins (cl-loop for i below 10 collect (intern (format "win%d" i))))
         (info-grid `(,(cl-loop for w in wins collect `(:window ,w))))
         (cell-pcts (cons (make-list 10 0.1) '(1.0)))
         (result (spatial-window--assign-keys nil info-grid cell-pcts)))
    (dolist (w wins)
      (should (= (length (cdr (assq w result))) 3)))))

(ert-deftest spatial-window-test-complex-spanning-layout ()
  "Complex layout: 3 rows × 5 cols with multiple spanning windows."
  (let* ((win-magit 'win-magit)
         (win-claude 'win-claude)
         (win-sw1 'win-sw1)
         (win-sw2 'win-sw2)
         (win-sw3 'win-sw3)
         (win-sw4 'win-sw4)
         (win-backtrace 'win-backtrace)
         (info-grid
          `(((:window ,win-magit)
             (:window ,win-magit)
             (:window ,win-magit)
             (:window ,win-magit)
             (:window ,win-claude))
            ((:window ,win-sw1)
             (:window ,win-sw2)
             (:window ,win-sw3)
             (:window ,win-sw4)
             (:window ,win-claude))
            ((:window ,win-backtrace)
             (:window ,win-sw2)
             (:window ,win-sw3)
             (:window ,win-sw4)
             (:window ,win-claude))))
         (cell-pcts '((0.066 0.063 0.126 0.253 0.489) . (0.483 0.242 0.256)))
         (result (spatial-window--assign-keys nil info-grid cell-pcts)))
    ;; ALL 7 windows MUST have at least 1 key
    (should (>= (length (cdr (assq win-magit result))) 1))
    (should (>= (length (cdr (assq win-claude result))) 1))
    (should (>= (length (cdr (assq win-sw1 result))) 1))
    (should (>= (length (cdr (assq win-sw2 result))) 1))
    (should (>= (length (cdr (assq win-sw3 result))) 1))
    (should (>= (length (cdr (assq win-sw4 result))) 1))
    (should (>= (length (cdr (assq win-backtrace result))) 1))))

(ert-deftest spatial-window-test-invalid-keyboard-layout ()
  "Returns nil and displays message when keyboard layout rows have different lengths."
  (let ((invalid-layout '(("q" "w" "e")
                          ("a" "s")))
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (let ((result (spatial-window--assign-keys nil nil nil invalid-layout)))
        (should (null result))
        (should (cl-some (lambda (msg) (string-match-p "Invalid keyboard layout" msg)) messages))))))

(ert-deftest spatial-window-test-too-many-windows-message ()
  "Displays appropriate message when too many windows."
  (let* ((wins (cl-loop for i below 4 collect (intern (format "win%d" i))))
         (info-grid (mapcar (lambda (w) `((:window ,w))) wins))
         (cell-pcts '((1.0) . (0.25 0.25 0.25 0.25)))
         (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (let ((result (spatial-window--assign-keys nil info-grid cell-pcts)))
        (should (null result))
        (should (cl-some (lambda (msg) (string-match-p "rows" msg)) messages))
        (should (cl-some (lambda (msg) (string-match-p "4 found of 3 max" msg)) messages))))))

;;; Formatting tests

(ert-deftest spatial-window-test-format-key-grid ()
  "Format keys as keyboard grid shows assigned keys and dots for unassigned."
  (let ((grid (spatial-window--format-key-grid '("q" "w" "e" "a" "s"))))
    (should (= (length (split-string grid "\n")) 3))
    (should (string-match-p "^q w e · · · · · · ·$" (car (split-string grid "\n"))))
    (should (string-match-p "^a s · · · · · · · ·$" (nth 1 (split-string grid "\n"))))
    (should (string-match-p "^· · · · · · · · · ·$" (nth 2 (split-string grid "\n"))))))

(ert-deftest spatial-window-test-format-key-grid-empty ()
  "Empty key list produces all dots."
  (let ((grid (spatial-window--format-key-grid '())))
    (should (string-match-p "^· · · · · · · · · ·$" (car (split-string grid "\n"))))))

(ert-deftest spatial-window-test-format-key-grid-all-keys ()
  "All keys assigned shows full keyboard."
  (let* ((all-keys (apply #'append (spatial-window--get-layout)))
         (grid (spatial-window--format-key-grid all-keys)))
    (should (string-match-p "^q w e r t y u i o p$" (car (split-string grid "\n"))))))

(provide 'spatial-window-geometry-test)

;;; spatial-window-geometry-test.el ends here
