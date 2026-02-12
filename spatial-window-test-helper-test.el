;;; spatial-window-test-helper-test.el --- Tests for spatial-window-test-helper -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window-test-helper.el

;;; Code:

(require 'ert)
(require 'spatial-window-test-helper)

(ert-deftest spatial-window-test-bts-single-window ()
  "Single window renders as simple box with label and dimensions."
  (should (string= (spatial-window--bounds-to-string
                     '((A 0.0 1.0 0.0 1.0)))
                    (concat "┌──────────┐\n"
                            "│ A        │\n"
                            "│ 100w×100h│\n"
                            "└──────────┘"))))

(ert-deftest spatial-window-test-bts-two-columns ()
  "Vertical split renders two side-by-side boxes."
  (should (string= (spatial-window--bounds-to-string
                     '((L 0.0 0.5 0.0 1.0) (R 0.5 1.0 0.0 1.0)))
                    (concat "┌─────────┬─────────┐\n"
                            "│ L       │ R       │\n"
                            "│ 50w×100h│ 50w×100h│\n"
                            "└─────────┴─────────┘"))))

(ert-deftest spatial-window-test-bts-two-rows ()
  "Horizontal split renders two stacked boxes."
  (should (string= (spatial-window--bounds-to-string
                     '((T 0.0 1.0 0.0 0.5) (B 0.0 1.0 0.5 1.0)))
                    (concat "┌─────────┐\n"
                            "│ T       │\n"
                            "│ 100w×50h│\n"
                            "├─────────┤\n"
                            "│ B       │\n"
                            "│ 100w×50h│\n"
                            "└─────────┘"))))

(ert-deftest spatial-window-test-bts-2x2-grid ()
  "Four equal quadrants render with cross junction."
  (should (string= (spatial-window--bounds-to-string
                     '((TL 0.0 0.5 0.0 0.5) (TR 0.5 1.0 0.0 0.5)
                       (BL 0.0 0.5 0.5 1.0) (BR 0.5 1.0 0.5 1.0)))
                    (concat "┌────────┬────────┐\n"
                            "│ TL     │ TR     │\n"
                            "│ 50w×50h│ 50w×50h│\n"
                            "├────────┼────────┤\n"
                            "│ BL     │ BR     │\n"
                            "│ 50w×50h│ 50w×50h│\n"
                            "└────────┴────────┘"))))

(ert-deftest spatial-window-test-bts-column-spanning ()
  "Top window spans both columns, separator only in bottom half."
  (should (string= (spatial-window--bounds-to-string
                     '((T 0.0 1.0 0.0 0.5) (BL 0.0 0.5 0.5 1.0) (BR 0.5 1.0 0.5 1.0)))
                    (concat "┌─────────────────┐\n"
                            "│ T               │\n"
                            "│ 100w×50h        │\n"
                            "├────────┬────────┤\n"
                            "│ BL     │ BR     │\n"
                            "│ 50w×50h│ 50w×50h│\n"
                            "└────────┴────────┘"))))

(ert-deftest spatial-window-test-bts-row-spanning ()
  "L and R span both rows with wrapped dimensions on separator line."
  (should (string= (spatial-window--bounds-to-string
                     '((L 0.0 0.2 0.0 1.0) (T 0.2 0.7 0.0 0.5)
                       (B 0.2 0.7 0.5 1.0) (R 0.7 1.0 0.0 1.0)))
                    (concat "┌─────┬────────────┬────────┐\n"
                            "│ L   │ T          │ R      │\n"
                            "│ 20w │ 50w×50h    │ 30w    │\n"
                            "│ ×   ├────────────┤ ×      │\n"
                            "│ 100h│ B          │ 100h   │\n"
                            "│     │ 50w×50h    │        │\n"
                            "└─────┴────────────┴────────┘"))))

(provide 'spatial-window-test-helper-test)

;;; spatial-window-test-helper-test.el ends here
