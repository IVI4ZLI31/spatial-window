;;; spatial-window-test.el --- Tests for spatial-window -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window.el UI and integration.
;; See spatial-window-geometry-test.el for geometry function tests.

;;; Code:

(require 'ert)
(require 'spatial-window)

;;; Focus/unfocus tests

(ert-deftest spatial-window-test-save-and-restore-layout ()
  "Save and restore layout history via frame parameter (no `tab-bar-mode')."
  (set-frame-parameter nil 'spatial-window-config nil)
  (should-not (spatial-window--get-history))
  ;; Push first layout
  (spatial-window--save-layout 'focus)
  (should (spatial-window--get-history))
  (should (= 1 (length (spatial-window--get-history))))
  ;; Push second layout (nested focus)
  (spatial-window--save-layout 'kill)
  (should (= 2 (length (spatial-window--get-history))))
  ;; Pop one level — returns the action symbol
  (should (eq 'kill (spatial-window--restore-layout)))
  (should (= 1 (length (spatial-window--get-history))))
  ;; Pop last level
  (should (eq 'focus (spatial-window--restore-layout)))
  (should-not (spatial-window--get-history))
  ;; Pop from empty stack returns nil
  (should-not (spatial-window--restore-layout)))

(ert-deftest spatial-window-test-history-eviction ()
  "History is capped at `spatial-window-history-max'."
  (set-frame-parameter nil 'spatial-window-config nil)
  (let ((spatial-window-history-max 3))
    (dotimes (i 5)
      (spatial-window--save-layout (intern (format "act%d" i))))
    (should (= 3 (length (spatial-window--get-history))))
    ;; Most recent entries survive (LIFO order)
    (should (eq 'act4 (spatial-window--restore-layout)))
    (should (eq 'act3 (spatial-window--restore-layout)))
    (should (eq 'act2 (spatial-window--restore-layout)))
    (should-not (spatial-window--restore-layout))))

(ert-deftest spatial-window-test-restore-without-saved-layout ()
  "Restore returns nil when no layout is saved."
  (set-frame-parameter nil 'spatial-window-config nil)
  (should-not (spatial-window--restore-layout)))

(ert-deftest spatial-window-test-unfocus-with-saved-layout ()
  "Unfocus restores layout and reports success."
  (let ((msg nil))
    (cl-letf (((symbol-function 'spatial-window--restore-layout)
               (lambda () t))
              ((symbol-function 'message)
               (lambda (fmt &rest _) (setq msg fmt))))
      (spatial-window-unfocus)
      (should (string-match "Restored" msg)))))

(ert-deftest spatial-window-test-unfocus-without-saved-layout ()
  "Unfocus beeps and reports no saved layout."
  (let ((msg nil)
        (beeped nil))
    (cl-letf (((symbol-function 'spatial-window--restore-layout)
               (lambda () nil))
              ((symbol-function 'beep)
               (lambda () (setq beeped t)))
              ((symbol-function 'message)
               (lambda (fmt &rest _) (setq msg fmt))))
      (spatial-window-unfocus)
      (should beeped)
      (should (string-match "No saved" msg)))))

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

(provide 'spatial-window-test)

;;; spatial-window-test.el ends here
