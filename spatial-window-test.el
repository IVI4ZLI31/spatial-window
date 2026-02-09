;;; spatial-window-test.el --- Tests for spatial-window -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for spatial-window.el UI and integration.
;; See spatial-window-geometry-test.el for geometry function tests.

;;; Code:

(require 'ert)
(require 'spatial-window)

;;; UI tests

(ert-deftest spatial-window-test-select-by-key ()
  "Selects window matching pressed key from assignments."
  (let* ((win1 (selected-window))
         (spatial-window--state
          (spatial-window--make-state
           :assignments `((,win1 . ("q" "w" "e"))))))
    (cl-letf (((symbol-function 'this-command-keys)
               (lambda () "w"))
              ((symbol-function 'select-window)
               (lambda (win) win)))
      (should (eq (spatial-window--select-by-key) win1)))))

(ert-deftest spatial-window-test-make-selection-keymap ()
  "Keymap contains all layout keys and \\`C-g'."
  (let ((map (spatial-window--make-selection-keymap)))
    (should (keymapp map))
    (should (lookup-key map "q"))
    (should (lookup-key map "a"))
    (should (lookup-key map "z"))
    (should (lookup-key map (kbd "C-g")))))

;;; Focus/unfocus tests

(ert-deftest spatial-window-test-save-and-restore-layout ()
  "Save and restore layout stack via frame parameter (no `tab-bar-mode')."
  (set-frame-parameter nil 'spatial-window-config nil)
  (should-not (spatial-window--has-saved-layout-p))
  ;; Push first layout
  (spatial-window--save-layout 'focus)
  (should (spatial-window--has-saved-layout-p))
  (should (= 1 (length (spatial-window--has-saved-layout-p))))
  ;; Push second layout (nested focus)
  (spatial-window--save-layout 'kill)
  (should (= 2 (length (spatial-window--has-saved-layout-p))))
  ;; Pop one level — returns the action symbol
  (should (eq 'kill (spatial-window--restore-layout)))
  (should (= 1 (length (spatial-window--has-saved-layout-p))))
  ;; Pop last level
  (should (eq 'focus (spatial-window--restore-layout)))
  (should-not (spatial-window--has-saved-layout-p))
  ;; Pop from empty stack returns nil
  (should-not (spatial-window--restore-layout)))

(ert-deftest spatial-window-test-history-eviction ()
  "History is capped at `spatial-window-history-max'."
  (set-frame-parameter nil 'spatial-window-config nil)
  (let ((spatial-window-history-max 3))
    (dotimes (i 5)
      (spatial-window--save-layout (intern (format "act%d" i))))
    (should (= 3 (length (spatial-window--has-saved-layout-p))))
    ;; Most recent entries survive (LIFO order)
    (should (eq 'act4 (spatial-window--restore-layout)))
    (should (eq 'act3 (spatial-window--restore-layout)))
    (should (eq 'act2 (spatial-window--restore-layout)))
    (should-not (spatial-window--restore-layout))))

(ert-deftest spatial-window-test-restore-without-saved-layout ()
  "Restore returns nil when no layout is saved."
  (set-frame-parameter nil 'spatial-window-config nil)
  (should-not (spatial-window--restore-layout)))

(ert-deftest spatial-window-test-focus-by-key ()
  "Focus saves layout, selects target, and deletes other windows."
  (let* ((win1 (selected-window))
         (saved-config nil)
         (selected-win nil)
         (deleted-others nil)
         (spatial-window--state
          (spatial-window--make-state
           :assignments `((,win1 . ("q" "w" "e"))))))
    (cl-letf (((symbol-function 'this-command-keys)
               (lambda () "q"))
              ((symbol-function 'spatial-window--save-layout)
               (lambda (action) (setq saved-config action)))
              ((symbol-function 'select-window)
               (lambda (win) (setq selected-win win)))
              ((symbol-function 'delete-other-windows)
               (lambda (win) (setq deleted-others win)))
              ((symbol-function 'spatial-window--remove-overlays)
               #'ignore))
      (spatial-window--focus-by-key)
      (should (eq saved-config 'focus))
      (should (eq selected-win win1))
      (should (eq deleted-others win1)))))

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
  "Unified act-by-key dispatches kill action correctly."
  (let* ((win1 (selected-window))
         (deleted nil)
         (spatial-window--state
          (spatial-window--make-state
           :assignments `((,win1 . ("q" "w" "e")))
           :action 'kill)))
    (cl-letf (((symbol-function 'this-command-keys)
               (lambda () "q"))
              ((symbol-function 'window-live-p)
               (lambda (_w) t))
              ((symbol-function 'delete-window)
               (lambda (w) (setq deleted w)))
              ((symbol-function 'spatial-window--remove-overlays)
               #'ignore))
      (spatial-window--act-by-key)
      (should (eq deleted win1)))))

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
  "Unified keymap contains action modifier keys."
  (let ((map (spatial-window--make-unified-keymap)))
    (should (lookup-key map (kbd "K")))
    (should (lookup-key map (kbd "S")))
    (should (lookup-key map (kbd "F")))
    (should (lookup-key map (kbd "U")))
    ;; Layout keys also present
    (should (lookup-key map "q"))
    (should (lookup-key map "a"))))

(provide 'spatial-window-test)

;;; spatial-window-test.el ends here
