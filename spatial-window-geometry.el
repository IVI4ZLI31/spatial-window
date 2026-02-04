;;; spatial-window-geometry.el --- Spatial calculations for spatial-window -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Le Wang

;; Author: Le Wang
;; URL: https://github.com/lewang/spatial-window
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
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

;; This file contains spatial and geometric calculation functions for
;; spatial-window.  These functions handle window bounds computation,
;; key assignment via overlap calculation, and formatting utilities.

;;; Code:

(require 'cl-lib)

;; Forward declaration for layout accessor
(defvar spatial-window-keyboard-layout)
(declare-function spatial-window--get-layout "spatial-window")

;;; Utilities

(defun spatial-window--frame-windows ()
  "Return list of windows in current frame, excluding minibuffer."
  (window-list nil 'no-minibuf))

;;; Core geometry functions

(defun spatial-window--overlap (range1-start range1-end range2-start range2-end)
  "Compute overlap percentage of RANGE1 within RANGE2.
Returns the fraction of RANGE1 that overlaps with RANGE2 (0.0 to 1.0).
Arguments are (RANGE1-START RANGE1-END RANGE2-START RANGE2-END)."
  (let* ((overlap-start (max range1-start range2-start))
         (overlap-end (min range1-end range2-end))
         (overlap-size (max 0.0 (- overlap-end overlap-start)))
         (range1-size (- range1-end range1-start)))
    (if (zerop range1-size)
        0.0
      (/ overlap-size range1-size))))

(defun spatial-window--window-bounds (&optional frame)
  "Return window bounds for FRAME as list of (window x-start x-end y-start y-end).
Coordinates are normalized to 0.0-1.0 range relative to frame size."
  (let* ((frame (or frame (selected-frame)))
         (frame-w (float (frame-pixel-width frame)))
         (frame-h (float (frame-pixel-height frame)))
         (windows (window-list frame 'no-minibuf)))
    (mapcar (lambda (w)
              (let ((edges (window-pixel-edges w)))
                (list w
                      (/ (nth 0 edges) frame-w)   ; x-start
                      (/ (nth 2 edges) frame-w)   ; x-end
                      (/ (nth 1 edges) frame-h)   ; y-start
                      (/ (nth 3 edges) frame-h)))) ; y-end
            windows)))

;;; Key assignment

(defun spatial-window--key-overlaps (kbd-layout window-bounds)
  "Compute overlap between each key and each window.
Returns hash table: key -> list of (window . overlap) sorted by overlap descending."
  (let ((kbd-rows (length kbd-layout))
        (kbd-cols (length (car kbd-layout)))
        (key-overlaps (make-hash-table :test 'equal)))
    (cl-loop for kbd-row from 0 below kbd-rows
             for key-y-start = (/ (float kbd-row) kbd-rows)
             for key-y-end = (/ (float (1+ kbd-row)) kbd-rows)
             do (cl-loop for kbd-col from 0 below kbd-cols
                         for key-x-start = (/ (float kbd-col) kbd-cols)
                         for key-x-end = (/ (float (1+ kbd-col)) kbd-cols)
                         for key = (nth kbd-col (nth kbd-row kbd-layout))
                         do (let ((overlaps
                                   (mapcar (lambda (wb)
                                             (let* ((win (nth 0 wb))
                                                    (win-x-start (nth 1 wb))
                                                    (win-x-end (nth 2 wb))
                                                    (win-y-start (nth 3 wb))
                                                    (win-y-end (nth 4 wb))
                                                    (x-overlap (spatial-window--overlap
                                                                key-x-start key-x-end
                                                                win-x-start win-x-end))
                                                    (y-overlap (spatial-window--overlap
                                                                key-y-start key-y-end
                                                                win-y-start win-y-end)))
                                               (cons win (* x-overlap y-overlap))))
                                           window-bounds)))
                              (puthash key (sort overlaps (lambda (a b) (> (cdr a) (cdr b))))
                                       key-overlaps))))
    key-overlaps))

(defun spatial-window--assign-keys (&optional frame window-bounds kbd-layout)
  "Assign keyboard keys to windows based on spatial overlap.
Returns alist of (window . (list of keys)).

Optional arguments allow dependency injection for testing:
  WINDOW-BOUNDS - list of (window x-start x-end y-start y-end)
  KBD-LAYOUT - keyboard layout as list of rows

Algorithm: For each key position, compute overlap with all windows.
Assign to window with highest overlap if unambiguous. Then ensure
every window has at least one key (unless topologically impossible)."
  (let ((kbd-layout (or kbd-layout (spatial-window--get-layout))))
    ;; Validate keyboard layout: all rows must have same length
    (unless (apply #'= (mapcar #'length kbd-layout))
      (message "Invalid keyboard layout: rows have different lengths")
      nil)
    (when (apply #'= (mapcar #'length kbd-layout))
      (let* ((window-bounds (or window-bounds (spatial-window--window-bounds frame)))
             (kbd-rows (length kbd-layout))
             (kbd-cols (length (car kbd-layout)))
             (num-windows (length window-bounds))
             (tie-threshold 0.10)
             (result (make-hash-table :test 'eq))
             (key-assignments (make-hash-table :test 'equal))
             (key-overlaps (spatial-window--key-overlaps kbd-layout window-bounds)))
        ;; Check if topology allows assignment (not more windows than keys in any dimension)
        (when (> num-windows (* kbd-rows kbd-cols))
          (message "Too many windows: %d windows for %d keys" num-windows (* kbd-rows kbd-cols))
          (cl-return-from spatial-window--assign-keys nil))
        ;; Phase 1: Assign keys to clear winners (not ties)
        (maphash
         (lambda (key overlaps)
           (let* ((best (car overlaps))
                  (second (cadr overlaps))
                  (best-overlap (cdr best))
                  (second-overlap (if second (cdr second) 0.0)))
             (when (and (> best-overlap 0)
                        (or (< second-overlap 0.001)
                            (< (/ second-overlap best-overlap) (- 1.0 tie-threshold))))
               (push key (gethash (car best) result))
               (puthash key (car best) key-assignments))))
         key-overlaps)
        ;; Phase 2: Ensure every window has at least one key
        ;; Only steal from windows that have >1 key, or from unassigned keys
        (dolist (wb window-bounds)
          (let ((win (car wb)))
            (unless (gethash win result)
              ;; Find the key with highest overlap for this window
              ;; Prefer unassigned keys, then keys from windows with >1 key
              (let ((best-key nil)
                    (best-overlap 0)
                    (best-stealable-key nil)
                    (best-stealable-overlap 0))
                (maphash
                 (lambda (key overlaps)
                   (let* ((win-overlap (cdr (assq win overlaps)))
                          (prev-owner (gethash key key-assignments))
                          (prev-owner-keys (when prev-owner (length (gethash prev-owner result)))))
                     (when (and win-overlap (> win-overlap 0))
                       (cond
                        ;; Unassigned key - always prefer these
                        ((not prev-owner)
                         (when (> win-overlap best-overlap)
                           (setq best-key key
                                 best-overlap win-overlap)))
                        ;; Stealable from window with >1 key
                        ((and prev-owner-keys (> prev-owner-keys 1))
                         (when (> win-overlap best-stealable-overlap)
                           (setq best-stealable-key key
                                 best-stealable-overlap win-overlap)))))))
                 key-overlaps)
                ;; Use unassigned key if found, else steal from multi-key window
                (let ((chosen-key (or best-key best-stealable-key)))
                  (when chosen-key
                    (let ((prev-owner (gethash chosen-key key-assignments)))
                      (when prev-owner
                        (puthash prev-owner
                                 (delete chosen-key (gethash prev-owner result))
                                 result)))
                    (push chosen-key (gethash win result))
                    (puthash chosen-key win key-assignments)))))))
        ;; Convert hash to alist, reverse key lists to preserve order
        (let ((alist nil))
          (maphash (lambda (win keys)
                     (push (cons win (nreverse keys)) alist))
                   result)
          alist)))))

;;; Formatting utilities

(defun spatial-window--format-key-grid (keys)
  "Format KEYS as a keyboard grid string.
Returns a string showing which keys are assigned, displayed in keyboard layout."
  (let ((key-set (make-hash-table :test 'equal)))
    (dolist (k keys)
      (puthash k t key-set))
    (mapconcat
     (lambda (row)
       (mapconcat
        (lambda (key)
          (if (gethash key key-set) key "·"))
        row " "))
     (spatial-window--get-layout)
     "\n")))

(provide 'spatial-window-geometry)

;;; spatial-window-geometry.el ends here
