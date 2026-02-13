;;; spatial-window-test.el --- Tests for spatial-window -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window.el UI and integration.
;; See spatial-window-geometry-test.el for geometry function tests.

;;; Code:

(require 'ert)
(require 'spatial-window)

;;; History tests

(ert-deftest spatial-window-test-save-layout ()
  "Save layout pushes entries in LIFO order via frame parameter."
  (set-frame-parameter nil 'spatial-window-config nil)
  (should-not (spatial-window--get-history))
  (spatial-window--save-layout 'focus)
  (should (= 1 (length (spatial-window--get-history))))
  (should (eq 'focus (caar (spatial-window--get-history))))
  (spatial-window--save-layout 'kill)
  (should (= 2 (length (spatial-window--get-history))))
  ;; Most recent is first (LIFO)
  (should (eq 'kill (caar (spatial-window--get-history))))
  (should (eq 'focus (car (cadr (spatial-window--get-history))))))

(ert-deftest spatial-window-test-history-eviction ()
  "History is capped at `spatial-window-history-max'."
  (set-frame-parameter nil 'spatial-window-config nil)
  (let ((spatial-window-history-max 3))
    (dotimes (i 5)
      (spatial-window--save-layout (intern (format "act%d" i))))
    (should (= 3 (length (spatial-window--get-history))))
    ;; Most recent entries survive (LIFO order)
    (let ((actions (mapcar #'car (spatial-window--get-history))))
      (should (equal '(act4 act3 act2) actions)))))

;;; Unified action mode tests

(ert-deftest spatial-window-test-act-by-key-kill ()
  "Kill action toggles window selection without immediately deleting."
  (let* ((win1 (selected-window))
         (spatial-window--state
          (spatial-window--make-state
           :assignments `((,win1 . ("q" "w" "e")))
           :action 'kill
           :overlays-visible t)))
    (cl-letf (((symbol-function 'this-command-keys)
               (lambda () "q"))
              ((symbol-function 'spatial-window--show-overlays)
               #'ignore))
      ;; First press adds window to selection
      (spatial-window--act-by-key)
      (should (memq win1 (spatial-window--state-selected-windows spatial-window--state)))
      ;; Second press removes it
      (spatial-window--act-by-key)
      (should-not (memq win1 (spatial-window--state-selected-windows spatial-window--state))))))

(ert-deftest spatial-window-test-act-by-key-select ()
  "Unified act-by-key defaults to window selection."
  (let* ((win1 (selected-window))
         (selected nil)
         (spatial-window--state
          (spatial-window--make-state
           :assignments `((,win1 . ("q" "w" "e"))))))
    (cl-letf (((symbol-function 'this-command-keys)
               (lambda () "q"))
              ((symbol-function 'select-window)
               (lambda (w) (setq selected w)))
              ((symbol-function 'spatial-window--remove-overlays)
               #'ignore))
      (spatial-window--act-by-key)
      (should (eq selected win1)))))

(ert-deftest spatial-window-test-unified-keymap-has-modifiers ()
  "Unified keymap contains action modifier and history navigation keys."
  (let ((map (spatial-window--make-unified-keymap)))
    (should (lookup-key map (kbd "K")))
    (should (lookup-key map (kbd "S")))
    (should (lookup-key map (kbd "F")))
    (should (lookup-key map (kbd "<left>")))
    (should (lookup-key map (kbd "<right>")))
    ;; Layout keys also present
    (should (lookup-key map "q"))
    (should (lookup-key map "a"))))

(ert-deftest spatial-window-test-history-navigation ()
  "Left/right navigate history non-destructively."
  (set-frame-parameter nil 'spatial-window-config nil)
  ;; Push two configs
  (spatial-window--save-layout 'kill)
  (spatial-window--save-layout 'swap)
  (should (= 2 (length (spatial-window--get-history))))
  (let ((spatial-window--state (spatial-window--make-state)))
    ;; Navigate back into history
    (spatial-window--history-back)
    (should (= 0 (spatial-window--state-history-cursor spatial-window--state)))
    (should (spatial-window--state-history-live-config spatial-window--state))
    ;; Go deeper
    (spatial-window--history-back)
    (should (= 1 (spatial-window--state-history-cursor spatial-window--state)))
    ;; Navigate forward
    (spatial-window--history-forward)
    (should (= 0 (spatial-window--state-history-cursor spatial-window--state)))
    ;; Forward to live state
    (spatial-window--history-forward)
    (should-not (spatial-window--state-history-cursor spatial-window--state))
    (should-not (spatial-window--state-history-live-config spatial-window--state))
    ;; History is not modified by navigation
    (should (= 2 (length (spatial-window--get-history))))))

;;; History message tests

(ert-deftest spatial-window-test-history-message-no-history ()
  "No history produces empty message part."
  (set-frame-parameter nil 'spatial-window-config nil)
  (let ((spatial-window--state (spatial-window--make-state)))
    (should (string= "" (spatial-window--history-message-part)))))

(ert-deftest spatial-window-test-history-message-at-live ()
  "At live state with history shows left undo hint."
  (set-frame-parameter nil 'spatial-window-config
                       '((kill . fake-config)))
  (let ((spatial-window--state (spatial-window--make-state)))
    (should (string= " [←] Undo kill" (spatial-window--history-message-part)))))

(ert-deftest spatial-window-test-history-message-browsing-newest ()
  "At cursor=0 shows undo left, redo right, and position."
  (set-frame-parameter nil 'spatial-window-config
                       '((swap . fake1) (kill . fake2)))
  (let ((spatial-window--state (spatial-window--make-state
                                :history-cursor 0)))
    (should (string= " [←] Undo kill [→] Redo swap <1/2>"
                      (spatial-window--history-message-part)))))

(ert-deftest spatial-window-test-history-message-browsing-oldest ()
  "At oldest entry shows only redo right and position."
  (set-frame-parameter nil 'spatial-window-config
                       '((swap . fake1) (kill . fake2)))
  (let ((spatial-window--state (spatial-window--make-state
                                :history-cursor 1)))
    (should (string= " [→] Redo kill <2/2>"
                      (spatial-window--history-message-part)))))

(ert-deftest spatial-window-test-unified-message-includes-history ()
  "Unified message includes action modifiers and history hint."
  (set-frame-parameter nil 'spatial-window-config
                       '((focus . fake)))
  (let ((spatial-window--state (spatial-window--make-state)))
    (should (string-match "\\[K\\]ill.*\\[←\\] Undo focus"
                          (spatial-window--unified-mode-message)))))

;;; Layout-dependent function tests (moved from geometry test files)

(defconst spatial-window-test-layout
  '(("q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
    ("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")
    ("z" "x" "c" "v" "b" "n" "m" "," "." "/"))
  "QWERTY layout for tests.")

(ert-deftest spatial-window-test-invalid-keyboard-layout ()
  "Returns nil and displays message when keyboard layout rows have different lengths."
  (let ((invalid-layout '(("q" "w" "e")
                          ("a" "s")))
        (dummy-grid (vector (vector 'w nil nil) (vector 'w nil nil)))
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (let ((result (spatial-window--assignment-to-keys dummy-grid invalid-layout)))
        (should (null result))
        (should (cl-some (lambda (msg) (string-match-p "Invalid keyboard layout" msg)) messages))))))

(ert-deftest spatial-window-test-format-key-grid ()
  "Format keys as keyboard grid shows assigned keys and dots for unassigned."
  (let ((grid (spatial-window--format-key-grid '("q" "w" "e" "a" "s") spatial-window-test-layout)))
    (should (= (length (split-string grid "\n")) 3))
    (should (string-match-p "^q w e · · · · · · ·$" (car (split-string grid "\n"))))
    (should (string-match-p "^a s · · · · · · · ·$" (nth 1 (split-string grid "\n"))))
    (should (string-match-p "^· · · · · · · · · ·$" (nth 2 (split-string grid "\n"))))))

(ert-deftest spatial-window-test-format-key-grid-empty ()
  "Empty key list produces all dots."
  (let ((grid (spatial-window--format-key-grid '() spatial-window-test-layout)))
    (should (string-match-p "^· · · · · · · · · ·$" (car (split-string grid "\n"))))))

(ert-deftest spatial-window-test-format-key-grid-all-keys ()
  "All keys assigned shows full keyboard."
  (let* ((all-keys (apply #'append spatial-window-test-layout))
         (grid (spatial-window--format-key-grid all-keys spatial-window-test-layout)))
    (should (string-match-p "^q w e r t y u i o p$" (car (split-string grid "\n"))))))

(provide 'spatial-window-test)

;;; spatial-window-test.el ends here
