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

(ert-deftest spatial-window-test-bts-complex-11-window ()
  "Complex layout: 5 stacked left panes, 2 full-height middle columns,
2 stacked quarter panes, and a narrow top + large bottom on the right.
Row heights are proportional to fractional height."
  (should (string= (spatial-window--bounds-to-string
                     '((A 0.499 0.999 0.015 0.069)
                       (B 0.499 0.999 0.069 0.985)
                       (C 0.001 0.065 0.015 0.074)
                       (D 0.001 0.065 0.074 0.141)
                       (E 0.001 0.065 0.141 0.261)
                       (F 0.001 0.065 0.261 0.502)
                       (G 0.001 0.065 0.502 0.985)
                       (H 0.065 0.127 0.015 0.985)
                       (I 0.127 0.250 0.015 0.985)
                       (J 0.250 0.499 0.015 0.502)
                       (K 0.250 0.499 0.502 0.985)))
                    (concat
                     "┌───────┬───────┬─────────────┬───────────────────────────┬───────────────────────────────────────────────────────┐\n"
                     "│       │       │             │                           │ A                                                     │\n"
                     "│ C     │       │             │                           │ 50w×5h                                                │\n"
                     "│ 6w×6h │       │             │                           ├───────────────────────────────────────────────────────┤\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "├───────┤       │             │                           │                                                       │\n"
                     "│ D     │       │             │                           │                                                       │\n"
                     "│ 6w×7h │       │             │                           │                                                       │\n"
                     "├───────┤       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│ E     │       │             │ J                         │                                                       │\n"
                     "│ 6w×12h│       │             │ 25w×49h                   │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "├───────┤       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│ F     │       │             │                           │                                                       │\n"
                     "│ 6w×24h│       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │ H     │ I           │                           │                                                       │\n"
                     "│       │ 6w×97h│ 12w×97h     │                           │ B                                                     │\n"
                     "│       │       │             │                           │ 50w×92h                                               │\n"
                     "├───────┤       │             ├───────────────────────────┤                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│ G     │       │             │ K                         │                                                       │\n"
                     "│ 6w×48h│       │             │ 25w×48h                   │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "│       │       │             │                           │                                                       │\n"
                     "└───────┴───────┴─────────────┴───────────────────────────┴───────────────────────────────────────────────────────┘"))))

(provide 'spatial-window-test-helper-test)

;;; spatial-window-test-helper-test.el ends here
