;;; spatial-window-test-helper.el --- Grid assertion helper for spatial-window tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Assert key assignments against ASCII grid diagrams.

;;; Code:

(require 'ert)
(require 'seq)

(defun spatial-window-test-assert-grid (result abbrevs grid-string kbd-layout)
  "Assert RESULT matches the expected key assignment in GRID-STRING.

RESULT is an alist from `spatial-window--assign-keys',
  e.g. ((win-left \"q\" \"w\" ...) (win-right \"y\" \"u\" ...)).
ABBREVS is an alist mapping grid letter string to window symbol,
  e.g. ((\"L\" . win-left) (\"R\" . win-right)).
GRID-STRING is a multi-line string with one line per keyboard row,
  tokens separated by spaces.  \"·\" means unassigned.
KBD-LAYOUT is the keyboard layout (list of rows of key strings)."
  (let* ((grid-rows (spatial-window-test--parse-grid grid-string))
         (expected (spatial-window-test--build-expected grid-rows abbrevs kbd-layout))
         (all-grid-keys (spatial-window-test--all-grid-keys grid-rows kbd-layout)))
    ;; Assert each window's keys match
    (dolist (pair abbrevs)
      (let* ((abbr (car pair))
             (win (cdr pair))
             (actual-keys (cdr (assq win result)))
             (expected-keys (cdr (assq win expected))))
        (should
         (let ((result (seq-set-equal-p actual-keys expected-keys)))
           (unless result
             (ert-fail
              (format "Window %s (%s):\n  expected: %S\n  actual:   %S\n  missing:  %S\n  extra:    %S"
                      win abbr
                      (sort (copy-sequence expected-keys) #'string<)
                      (sort (copy-sequence actual-keys) #'string<)
                      (sort (seq-difference expected-keys actual-keys) #'string<)
                      (sort (seq-difference actual-keys expected-keys) #'string<))))
           result))))
    ;; Assert no unexpected keys in result beyond what the grid covers
    (let ((unexpected nil))
      (dolist (entry result)
        (let ((win (car entry))
              (keys (cdr entry)))
          (dolist (k keys)
            (unless (member k all-grid-keys)
              (push (cons win k) unexpected)))))
      (when unexpected
        (ert-fail
         (format "Keys assigned but not in grid: %S" unexpected))))))

(defun spatial-window-test--parse-grid (grid-string)
  "Parse GRID-STRING into list of lists of tokens.
Strip leading/trailing newlines, trim whitespace per line, split by spaces."
  (let* ((trimmed (string-trim grid-string))
         (lines (split-string trimmed "\n")))
    (mapcar (lambda (line)
              (split-string (string-trim line)))
            lines)))

(defun spatial-window-test--build-expected (grid-rows abbrevs kbd-layout)
  "Build expected key alist from GRID-ROWS, ABBREVS, and KBD-LAYOUT.
Returns alist of (window-symbol . (key1 key2 ...))."
  (let ((expected (make-hash-table :test 'eq)))
    (cl-loop for grid-row in grid-rows
             for kbd-row in kbd-layout
             do (cl-loop for token in grid-row
                         for key in kbd-row
                         unless (equal token "·")
                         do (let ((win (cdr (assoc token abbrevs))))
                              (unless win
                                (error "Grid token %S not found in abbrevs" token))
                              (push key (gethash win expected)))))
    (let ((alist nil))
      (maphash (lambda (win keys)
                 (push (cons win (nreverse keys)) alist))
               expected)
      alist)))

(defun spatial-window-test--all-grid-keys (grid-rows kbd-layout)
  "Return list of all keys covered by non-· tokens in GRID-ROWS."
  (let ((keys nil))
    (cl-loop for grid-row in grid-rows
             for kbd-row in kbd-layout
             do (cl-loop for token in grid-row
                         for key in kbd-row
                         unless (equal token "·")
                         do (push key keys)))
    keys))

(provide 'spatial-window-test-helper)

;;; spatial-window-test-helper.el ends here
