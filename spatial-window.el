;;; spatial-window.el --- Jump to windows using keyboard spatial mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Le Wang

;; Author: Le Wang <lewang.dev.26@gmail.com>
;; URL: https://github.com/lewang/spatial-window
;; Version: 0.9.2
;; Package-Requires: ((emacs "28.1") (posframe "1.0.0"))
;; Keywords: convenience, windows

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Spatial-window provides quick window selection by mapping your keyboard
;; layout to your window layout.  Each window displays an overlay showing
;; which keys will select it, based on the spatial correspondence between
;; keyboard position and window position on screen.
;;
;; Your eyes look at the target window, your fingers know where that position
;; is on the keyboard, and you press that key to jump there.
;;
;; Usage:
;;   (require 'spatial-window)
;;   (global-set-key (kbd "M-o") #'spatial-window-select)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'spatial-window-geometry)

(declare-function posframe-show "posframe")
(declare-function posframe-delete "posframe")

(defgroup spatial-window nil
  "Jump to windows using keyboard spatial mapping."
  :group 'windows
  :prefix "spatial-window-")

(defconst spatial-window-layout-qwerty
  '(("q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
    ("a" "s" "d" "f" "g" "h" "j" "k" "l" ";")
    ("z" "x" "c" "v" "b" "n" "m" "," "." "/"))
  "QWERTY keyboard layout.")

(defconst spatial-window-layout-dvorak
  '(("'" "," "." "p" "y" "f" "g" "c" "r" "l")
    ("a" "o" "e" "u" "i" "d" "h" "t" "n" "s")
    (";" "q" "j" "k" "x" "b" "m" "w" "v" "z"))
  "Dvorak keyboard layout.")

(defconst spatial-window-layout-colemak
  '(("q" "w" "f" "p" "g" "j" "l" "u" "y" ";")
    ("a" "r" "s" "t" "d" "h" "n" "e" "i" "o")
    ("z" "x" "c" "v" "b" "k" "m" "," "." "/"))
  "Colemak keyboard layout.")

(defconst spatial-window-extensions-qwerty
  '(("`" . "q")
    ("1" . "q")
    ("2" . "w")
    ("3" . "e")
    ("4" . "r")
    ("5" . "t")
    ("6" . "y")
    ("7" . "u")
    ("8" . "i")
    ("9" . "o")
    ("0" . "p")
    ("-" . "p")
    ("=" . "p")
    ("[" . "p")
    ("]" . "p")
    ("'" . ";"))
  "Edge extension keys for QWERTY layout.
Keys adjacent to the grid edges that map to the nearest layout key,
so overshooting while reaching still selects the intended window.")

(defconst spatial-window-extensions-dvorak nil
  "Edge extension keys for Dvorak layout.")

(defconst spatial-window-extensions-colemak nil
  "Edge extension keys for Colemak layout.")

(defcustom spatial-window-keyboard-layout 'qwerty
  "Keyboard layout for spatial window selection.
Can be a symbol naming a preset layout or a custom list of rows."
  :type '(choice (const :tag "QWERTY" qwerty)
                 (const :tag "Dvorak" dvorak)
                 (const :tag "Colemak" colemak)
                 (repeat :tag "Custom" (repeat string)))
  :group 'spatial-window)

(defcustom spatial-window-overlay-delay nil
  "Seconds to wait before showing overlays, or nil for immediate display.
When set, overlays start hidden and appear after the delay.  If you
know your target window you can act before they render; if you
hesitate they appear automatically."
  :type '(choice (const :tag "Immediate" nil)
                 (number :tag "Delay (seconds)"))
  :group 'spatial-window)

(defun spatial-window--get-layout ()
  "Return the keyboard layout as a list of rows."
  (pcase spatial-window-keyboard-layout
    ('qwerty spatial-window-layout-qwerty)
    ('dvorak spatial-window-layout-dvorak)
    ('colemak spatial-window-layout-colemak)
    ((pred listp) spatial-window-keyboard-layout)
    (_ spatial-window-layout-qwerty)))

(defun spatial-window--get-extensions ()
  "Return edge extension key mapping for current layout.
Returns alist of (extension-key . base-key)."
  (pcase spatial-window-keyboard-layout
    ('qwerty spatial-window-extensions-qwerty)
    ('dvorak spatial-window-extensions-dvorak)
    ('colemak spatial-window-extensions-colemak)
    (_ nil)))

(defface spatial-window-overlay-face
  '((t (:foreground "red" :background "white" :weight bold)))
  "Face for spatial-window key overlay."
  :group 'spatial-window)

(cl-defstruct (spatial-window--state (:constructor spatial-window--make-state))
  "Transient session state for selection modes."
  posframe-buffers
  assignments
  highlighted-windows
  selected-windows
  source-window
  overlays-visible
  selection-active
  action
  undo-count
  overlay-timer)

(defvar spatial-window--state (spatial-window--make-state)
  "Active session state for spatial-window.")

(defface spatial-window-selected-face
  '((t (:foreground "white" :background "red" :weight bold)))
  "Face for selected windows in kill mode."
  :group 'spatial-window)

(defun spatial-window--show-posframe (buf-name x y &optional selected-p)
  "Show posframe BUF-NAME at position X, Y.
If SELECTED-P, use selected face with border."
  (let ((face (if selected-p 'spatial-window-selected-face 'spatial-window-overlay-face)))
    (apply #'posframe-show buf-name
           :poshandler (lambda (_info) (cons x y))
           :foreground-color (face-foreground face nil t)
           :background-color (face-background face nil t)
           :internal-border-width 4
           (when selected-p
             (list :border-width 3
                   :border-color (face-background face nil t))))))

(defun spatial-window--show-overlays (&optional selected-windows)
  "Display key hints as posframes for current assignments.
Reads assignments from state (must be set by caller).
If SELECTED-WINDOWS is non-nil, highlight those windows with a border.
Returns non-nil if overlays were shown, nil if no assignments."
  (require 'posframe)
  ;; Clean up any existing posframes first
  (spatial-window--remove-overlays)
  (let* ((st spatial-window--state)
         (assignments (spatial-window--state-assignments st))
         (idx 0))
    (setf (spatial-window--state-posframe-buffers st) nil)
    (when assignments
      (dolist (pair assignments)
        (let* ((window (car pair))
               (keys (cdr pair))
               (grid-str (spatial-window--format-key-grid keys))
               (buf-name (format " *spatial-window-%d*" idx))
               (edges (window-pixel-edges window))
               (x (nth 0 edges))
               (y (nth 1 edges))
               (selected-p (memq window selected-windows)))
          (setq idx (1+ idx))
          (push buf-name (spatial-window--state-posframe-buffers st))
          (with-current-buffer (get-buffer-create buf-name)
            (erase-buffer)
            (insert grid-str))
          (spatial-window--show-posframe buf-name x y selected-p)))
      ;; Show minibuffer overlay if active
      (when (minibuffer-window-active-p (minibuffer-window))
        (let* ((buf-name " *spatial-window-minibuf*")
               (edges (window-pixel-edges (minibuffer-window)))
               (x (nth 0 edges))
               (y (nth 1 edges)))
          (push buf-name (spatial-window--state-posframe-buffers st))
          (with-current-buffer (get-buffer-create buf-name)
            (erase-buffer)
            (insert "┌────────┐\n")
            (insert "└────────┘"))
          (spatial-window--show-posframe buf-name x y)))
      t)))

(defun spatial-window--remove-overlays ()
  "Hide and cleanup all posframes."
  (let ((st spatial-window--state))
    (dolist (buf-name (spatial-window--state-posframe-buffers st))
      (posframe-delete buf-name))
    (setf (spatial-window--state-posframe-buffers st) nil)
    (when (featurep 'vterm)
      (dolist (win (window-list))
        (when (eq (buffer-local-value 'major-mode (window-buffer win)) 'vterm-mode)
          (force-window-update win))))))

(defun spatial-window--get-target-window ()
  "Return window for pressed key, or nil with error feedback if unbound."
  (let* ((key (this-command-keys))
         (translated (or (cdr (assoc key (spatial-window--get-extensions))) key))
         (target (cl-find-if (lambda (pair)
                               (member translated (cdr pair)))
                             (spatial-window--state-assignments spatial-window--state))))
    (if target
        (car target)
      (message "Key '%s' is unassigned (ambiguous zone)" key)
      (beep)
      nil)))

(defmacro spatial-window--with-target-window (&rest body)
  "Execute BODY with `win' bound to the target window.
If selection mode has ended, pass key through to normal processing.
If no target window found (ambiguous key), do nothing."
  (declare (indent 0) (debug t))
  `(if (null (spatial-window--state-assignments spatial-window--state))
       (setq unread-command-events
             (listify-key-sequence (this-command-keys-vector)))
     (when-let* ((win (spatial-window--get-target-window)))
       ,@body)))

(defun spatial-window--exit-selection-mode ()
  "Exit selection mode: cancel timer, clear flag, and remove overlays."
  (let ((timer (spatial-window--state-overlay-timer spatial-window--state)))
    (when (timerp timer)
      (cancel-timer timer)))
  (setf (spatial-window--state-selection-active spatial-window--state) nil)
  (spatial-window--remove-overlays))

(defun spatial-window--select-by-key ()
  "Select window corresponding to the key that invoked this command."
  (interactive)
  (spatial-window--with-target-window
    (spatial-window--exit-selection-mode)
    (select-window win)))

(defun spatial-window--abort ()
  "Abort window selection and clean up overlays."
  (interactive)
  (let ((timer (spatial-window--state-overlay-timer spatial-window--state)))
    (when (timerp timer)
      (cancel-timer timer)))
  (spatial-window--remove-overlays)
  (spatial-window--reset-state)
  (keyboard-quit))

(defun spatial-window--reset-state ()
  "Reset all state variables for action modes."
  (setq spatial-window--state (spatial-window--make-state)))

(defun spatial-window--show-delayed-overlays ()
  "Timer callback to show overlays after delay."
  (let ((st spatial-window--state))
    (when (and (spatial-window--state-selection-active st)
               (not (spatial-window--state-overlays-visible st)))
      (spatial-window--show-overlays (spatial-window--state-highlighted-windows st))
      (setf (spatial-window--state-overlays-visible st) t))))

(defun spatial-window--select-minibuffer ()
  "Select the minibuffer window if active, otherwise pass through."
  (interactive)
  (spatial-window--exit-selection-mode)
  (if (minibuffer-window-active-p (minibuffer-window))
      (select-window (minibuffer-window))
    ;; Minibuffer not active; pass through
    (setq unread-command-events
          (listify-key-sequence (this-command-keys-vector)))))

(defun spatial-window--cleanup-mode ()
  "Clean up overlays, cancel timers, and reset state after any mode ends."
  (let ((timer (spatial-window--state-overlay-timer spatial-window--state)))
    (when (timerp timer)
      (cancel-timer timer)))
  (spatial-window--remove-overlays)
  (spatial-window--reset-state))

(defun spatial-window--make-mode-keymap (key-action &optional extra-bindings)
  "Create keymap binding all layout keys to KEY-ACTION.
EXTRA-BINDINGS is an alist of (key-string . command) for additional bindings.
\\`C-g' aborts."
  (let ((map (make-sparse-keymap)))
    (dolist (row (spatial-window--get-layout))
      (dolist (key row)
        (define-key map (kbd key) key-action)))
    (dolist (ext (spatial-window--get-extensions))
      (define-key map (kbd (car ext)) key-action))
    (define-key map (kbd "C-g") #'spatial-window--abort)
    (dolist (binding extra-bindings)
      (define-key map (kbd (car binding)) (cdr binding)))
    map))

(defun spatial-window--setup-transient-mode (keymap &optional highlighted message)
  "Common setup for transient selection modes.
KEYMAP is the transient keymap to activate.
HIGHLIGHTED is a list of windows to highlight in overlays.
MESSAGE is displayed in the minibuffer."
  (let ((st spatial-window--state))
    (setf (spatial-window--state-assignments st) (spatial-window--assign-keys)
          (spatial-window--state-highlighted-windows st) highlighted)
    (when (spatial-window--state-assignments st)
      (setf (spatial-window--state-selection-active st) t)
      (if spatial-window-overlay-delay
          (progn
            (setf (spatial-window--state-overlays-visible st) nil)
            (setf (spatial-window--state-overlay-timer st)
                  (run-at-time spatial-window-overlay-delay nil
                               #'spatial-window--show-delayed-overlays)))
        (spatial-window--show-overlays highlighted)
        (setf (spatial-window--state-overlays-visible st) t))
      (when message (message "%s" message))
      (set-transient-map
       keymap
       (lambda ()
         (let ((binding (lookup-key keymap (this-command-keys-vector))))
           (and binding (not (numberp binding))
                (spatial-window--state-selection-active spatial-window--state))))
       #'spatial-window--cleanup-mode))))

(defun spatial-window--make-selection-keymap ()
  "Build transient keymap with all keyboard layout keys.
SPC selects minibuffer if active, otherwise passes through."
  (spatial-window--make-mode-keymap
   #'spatial-window--select-by-key
   '(("SPC" . spatial-window--select-minibuffer))))

;;; Kill mode functions (single kill)

(defun spatial-window--kill-by-key ()
  "Kill the window corresponding to the pressed key."
  (interactive)
  (spatial-window--with-target-window
    (spatial-window--exit-selection-mode)
    (when (window-live-p win)
      (spatial-window--save-layout 'kill)
      (delete-window win))
    (message "Killed window")))

(defun spatial-window-enter-kill-mode ()
  "Enter kill mode for deleting one window."
  (interactive)
  (spatial-window--setup-transient-mode
   (spatial-window--make-mode-keymap #'spatial-window--kill-by-key)
   nil
   "Select window to kill. C-h for hints. C-g to abort."))

;;; Kill-multi mode functions

(defun spatial-window--kill-multi-mode-message ()
  "Display kill-multi mode status message."
  (let ((n (length (spatial-window--state-selected-windows spatial-window--state))))
    (message "<enter> to kill %d window%s. C-g to abort."
             n (if (= n 1) "" "s"))))

(defun spatial-window--toggle-selection ()
  "Toggle the selection of the window corresponding to the pressed key."
  (interactive)
  (spatial-window--with-target-window
    (let ((st spatial-window--state))
      (if (memq win (spatial-window--state-selected-windows st))
          (setf (spatial-window--state-selected-windows st)
                (delq win (spatial-window--state-selected-windows st)))
        (push win (spatial-window--state-selected-windows st)))
      ;; Update highlighted and refresh overlays
      (setf (spatial-window--state-highlighted-windows st)
            (spatial-window--state-selected-windows st))
      (spatial-window--show-overlays (spatial-window--state-highlighted-windows st))
      (spatial-window--kill-multi-mode-message))))

(defun spatial-window--execute-kill-multi ()
  "Kill all selected windows and clean up."
  (interactive)
  (let ((windows-to-kill (spatial-window--state-selected-windows spatial-window--state)))
    (spatial-window--remove-overlays)
    (spatial-window--reset-state)
    (when windows-to-kill
      (spatial-window--save-layout 'kill)
      (dolist (win windows-to-kill)
        (when (window-live-p win)
          (delete-window win))))
    (message "Killed %d window(s)" (length windows-to-kill))))

(defun spatial-window-enter-kill-multi-mode ()
  "Enter kill-multi mode for selecting multiple windows to delete."
  (interactive)
  (setf (spatial-window--state-selected-windows spatial-window--state) nil)
  (spatial-window--setup-transient-mode
   (spatial-window--make-mode-keymap #'spatial-window--toggle-selection
                                     '(("RET" . spatial-window--execute-kill-multi))))
  (spatial-window--kill-multi-mode-message))

;;; Swap mode functions

(defun spatial-window--swap-windows (win1 win2)
  "Swap the buffers displayed in WIN1 and WIN2."
  (let ((buf1 (window-buffer win1))
        (buf2 (window-buffer win2))
        (start1 (window-start win1))
        (start2 (window-start win2))
        (pt1 (window-point win1))
        (pt2 (window-point win2)))
    (set-window-buffer win1 buf2)
    (set-window-buffer win2 buf1)
    (set-window-start win1 start2)
    (set-window-start win2 start1)
    (set-window-point win1 pt2)
    (set-window-point win2 pt1)))

(defun spatial-window--select-swap-target ()
  "Select target window for swap operation."
  (interactive)
  (spatial-window--with-target-window
    (spatial-window--exit-selection-mode)
    (spatial-window--save-layout 'swap)
    (spatial-window--swap-windows (spatial-window--state-source-window spatial-window--state) win)
    (select-window win)
    (message "Swapped windows")))

(defun spatial-window-enter-swap-mode ()
  "Enter swap mode for exchanging window buffers."
  (interactive)
  (setf (spatial-window--state-source-window spatial-window--state) (selected-window))
  (spatial-window--setup-transient-mode
   (spatial-window--make-mode-keymap #'spatial-window--select-swap-target)
   (list (spatial-window--state-source-window spatial-window--state))
   "Swap mode: select target window. C-h for hints. C-g to abort."))

;;; Focus/unfocus mode

(defcustom spatial-window-history-max 20
  "Maximum number of window configurations to keep in history.
Oldest entries are evicted when this limit is exceeded."
  :type 'integer
  :group 'spatial-window)

(defun spatial-window--get-history ()
  "Return the window configuration history list."
  (if (bound-and-true-p tab-bar-mode)
      (alist-get 'spatial-window-config
                 (cdr (tab-bar--current-tab-find)))
    (frame-parameter nil 'spatial-window-config)))

(defun spatial-window--set-history (history)
  "Store HISTORY as the window configuration history."
  (if (bound-and-true-p tab-bar-mode)
      (let* ((tabs (tab-bar-tabs))
             (ct (tab-bar--current-tab-find tabs)))
        (if history
            (setf (alist-get 'spatial-window-config (cdr ct)) history)
          (setf (alist-get 'spatial-window-config (cdr ct) nil 'remove) nil))
        (tab-bar-tabs-set tabs))
    (set-frame-parameter nil 'spatial-window-config history)))

(defun spatial-window--save-layout (action)
  "Push current window configuration onto history with ACTION tag.
Evicts oldest entry when `spatial-window-history-max' is exceeded."
  (let* ((entry (cons action (current-window-configuration)))
         (history (cons entry (spatial-window--get-history))))
    (when (> (length history) spatial-window-history-max)
      (setcdr (nthcdr (1- spatial-window-history-max) history) nil))
    (spatial-window--set-history history)))

(defun spatial-window--restore-layout ()
  "Pop and restore the most recent window configuration.
Each entry is (ACTION . WINDOW-CONFIGURATION).
Returns the ACTION symbol on success, nil otherwise."
  (let ((history (spatial-window--get-history)))
    (when history
      (let ((entry (car history)))
        (set-window-configuration (cdr entry))
        (spatial-window--set-history (cdr history))
        (car entry)))))

(defun spatial-window--has-saved-layout-p ()
  "Return the history list, or nil if empty."
  (spatial-window--get-history))

(defun spatial-window--focus-by-key ()
  "Focus on the window corresponding to the pressed key.
Saves layout, selects target, deletes all other windows."
  (interactive)
  (spatial-window--with-target-window
    (spatial-window--exit-selection-mode)
    (spatial-window--save-layout 'focus)
    (select-window win)
    (let ((ignore-window-parameters t))
      (delete-other-windows win))
    (message "Focused window")))

(defun spatial-window-enter-focus-mode ()
  "Enter focus mode for zooming into a single window."
  (interactive)
  (spatial-window--setup-transient-mode
   (spatial-window--make-mode-keymap #'spatial-window--focus-by-key)
   nil
   "Select window to focus. C-h for hints. C-g to abort."))

(defun spatial-window-unfocus ()
  "Restore the previously saved window layout."
  (interactive)
  (if (spatial-window--restore-layout)
      (message "Restored window layout")
    (beep)
    (message "No saved layout to restore")))

;;; Unified selection with action modifiers

(defun spatial-window--act-by-key ()
  "Apply current action to the window corresponding to the pressed key."
  (interactive)
  (spatial-window--with-target-window
    (let ((action (spatial-window--state-action spatial-window--state)))
      (pcase action
        ('multi-kill
         (let ((st spatial-window--state))
           (if (memq win (spatial-window--state-selected-windows st))
               (setf (spatial-window--state-selected-windows st)
                     (delq win (spatial-window--state-selected-windows st)))
             (push win (spatial-window--state-selected-windows st)))
           (setf (spatial-window--state-highlighted-windows st)
                 (spatial-window--state-selected-windows st))
           (when (spatial-window--state-overlays-visible st)
             (spatial-window--show-overlays (spatial-window--state-highlighted-windows st)))
           (spatial-window--kill-multi-mode-message)))
        (_
         (spatial-window--exit-selection-mode)
         (pcase action
           ('kill
            (when (window-live-p win)
              (spatial-window--save-layout 'kill)
              (delete-window win))
            (message "Killed window"))
           ('swap
            (spatial-window--save-layout 'swap)
            (spatial-window--swap-windows
             (spatial-window--state-source-window spatial-window--state) win)
            (select-window win)
            (message "Swapped windows"))
           ('focus
            (spatial-window--save-layout 'focus)
            (select-window win)
            (let ((ignore-window-parameters t))
              (delete-other-windows win))
            (message "Focused window (unfocus to restore)"))
           (_
            (select-window win))))))))

(defun spatial-window--set-action-kill ()
  "Switch to kill action."
  (interactive)
  (setf (spatial-window--state-action spatial-window--state) 'kill)
  (message "KILL: select window to kill"))

(defun spatial-window--set-action-multi-kill ()
  "Switch to multi-kill action."
  (interactive)
  (let ((st spatial-window--state))
    (setf (spatial-window--state-action st) 'multi-kill
          (spatial-window--state-selected-windows st) nil))
  (message "MULTI-KILL: toggle windows, RET to kill"))

(defun spatial-window--execute-multi-kill-unified ()
  "Execute multi-kill in unified mode."
  (interactive)
  (if (not (eq (spatial-window--state-action spatial-window--state) 'multi-kill))
      (setq unread-command-events
            (listify-key-sequence (this-command-keys-vector)))
    (let ((windows-to-kill (spatial-window--state-selected-windows spatial-window--state)))
      (spatial-window--exit-selection-mode)
      (when windows-to-kill
        (spatial-window--save-layout 'kill)
        (dolist (win windows-to-kill)
          (when (window-live-p win)
            (delete-window win))))
      (message "Killed %d window(s)" (length windows-to-kill)))))

(defun spatial-window--set-action-swap ()
  "Switch to swap action, recording current window as source."
  (interactive)
  (let ((st spatial-window--state))
    (setf (spatial-window--state-action st) 'swap
          (spatial-window--state-source-window st) (selected-window)
          (spatial-window--state-highlighted-windows st)
          (list (selected-window)))
    (when (spatial-window--state-overlays-visible st)
      (spatial-window--show-overlays (spatial-window--state-highlighted-windows st))))
  (message "SWAP: select target window"))

(defun spatial-window--set-action-focus ()
  "Switch to focus action."
  (interactive)
  (setf (spatial-window--state-action spatial-window--state) 'focus)
  (message "FOCUS: select window to focus"))

(defun spatial-window--undo ()
  "Undo the most recent window configuration change.
Stays in selection mode with updated overlays for further actions."
  (interactive)
  (let ((st spatial-window--state)
        (action (spatial-window--restore-layout)))
    (if (not action)
        (progn (beep) (message "Nothing to undo"))
      (setf (spatial-window--state-undo-count st)
            (1+ (or (spatial-window--state-undo-count st) 0)))
      ;; Recompute assignments for restored window layout
      (setf (spatial-window--state-assignments st) (spatial-window--assign-keys))
      (when (spatial-window--state-overlays-visible st)
        (spatial-window--show-overlays (spatial-window--state-highlighted-windows st)))
      (message "%s" (spatial-window--unified-mode-message)))))

(defun spatial-window--undo-message-part ()
  "Return undo portion of mode message, or empty string if no history."
  (let ((stack (spatial-window--has-saved-layout-p)))
    (if (not stack) ""
      (let* ((done (or (spatial-window--state-undo-count spatial-window--state) 0))
             (total (+ done (length stack)))
             (action (car (car stack))))
        (format " (U)ndo %s (%d/%d)" action done total)))))

(defun spatial-window--unified-mode-message ()
  "Return hint message for unified selection mode."
  (format "[K]ill [M]ulti-kill [S]wap [F]ocus%s"
          (spatial-window--undo-message-part)))

(defun spatial-window--make-unified-keymap ()
  "Build unified keymap with layout keys and action modifiers."
  (let ((map (spatial-window--make-mode-keymap
              #'spatial-window--act-by-key
              '(("SPC" . spatial-window--select-minibuffer)
                ("RET" . spatial-window--execute-multi-kill-unified)))))
    (define-key map (kbd "K") #'spatial-window--set-action-kill)
    (define-key map (kbd "M") #'spatial-window--set-action-multi-kill)
    (define-key map (kbd "S") #'spatial-window--set-action-swap)
    (define-key map (kbd "F") #'spatial-window--set-action-focus)
    (define-key map (kbd "U") #'spatial-window--undo)
    map))

;;;###autoload
(defun spatial-window-select ()
  "Select a window by pressing a key corresponding to its spatial position.

layout key to switch to that window immediately.

Uppercase modifiers change the action before selecting:
  K - Kill: then press a key to delete that window
  M - Multi-kill: toggle windows to mark, RET to delete them all
  S - Swap: then press a key to swap buffers with current window
  F - Focus: then press a key to zoom into that window
  U - Undo: cycle back through saved window configurations

When `spatial-window-overlay-delay' is set, overlays appear after
the configured delay instead of immediately."
  (interactive)
  (spatial-window--setup-transient-mode
   (spatial-window--make-unified-keymap)
   nil
   (spatial-window--unified-mode-message)))

(provide 'spatial-window)

;;; spatial-window.el ends here
