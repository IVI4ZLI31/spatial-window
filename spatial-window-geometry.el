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
;; spatial-window.  These functions handle window grid computation,
;; key assignment, and boundary mapping.

;;; Code:

(require 'cl-lib)
(require 'seq)

;; Forward declaration for layout accessor
(defvar spatial-window-keyboard-layout)
(declare-function spatial-window--get-layout "spatial-window")

;;; Utilities

(defun spatial-window--frame-windows ()
  "Return list of windows in current frame, excluding minibuffer."
  (window-list nil 'no-minibuf))

;;; Core geometry functions

(defun spatial-window--grid-geometry (&optional frame)
  "Return grid geometry for FRAME as (x-coords y-coords edges-list).
X-COORDS and Y-COORDS are sorted lists of unique pixel boundaries.
EDGES-LIST is an alist of (window . edges)."
  (let* ((windows (window-list frame 'no-minibuf))
         (edges-list (mapcar (lambda (w)
                               (cons w (window-pixel-edges w)))
                             windows))
         (x-coords (sort (delete-dups
                          (apply #'append
                                 (mapcar (lambda (we)
                                           (list (nth 1 we) (nth 3 we)))
                                         edges-list)))
                         #'<))
         (y-coords (sort (delete-dups
                          (apply #'append
                                 (mapcar (lambda (we)
                                           (list (nth 2 we) (nth 4 we)))
                                         edges-list)))
                         #'<)))
    (list x-coords y-coords edges-list)))

(defun spatial-window--window-grid (&optional frame)
  "Return a 2D grid of windows for FRAME based on pixel positions.
The grid is a list of rows, each row a list of windows.
Spanning windows appear in all cells they occupy."
  (let* ((geom (spatial-window--grid-geometry frame))
         (x-coords (nth 0 geom))
         (y-coords (nth 1 geom))
         (edges-list (nth 2 geom))
         (grid nil))
    (dolist (y (butlast y-coords))
      (let ((row nil))
        (dolist (x (butlast x-coords))
          (let ((found nil))
            (dolist (we edges-list)
              (let* ((w (car we))
                     (left (nth 1 we))
                     (top (nth 2 we))
                     (right (nth 3 we))
                     (bottom (nth 4 we)))
                (when (and (>= x left) (< x right)
                           (>= y top) (< y bottom))
                  (setq found w))))
            (push found row)))
        (push (nreverse row) grid)))
    (nreverse grid)))

(defun spatial-window--window-info (&optional frame)
  "Return 2D grid with window info for FRAME (default: selected frame).
Each cell is a plist (:window W) where :window is the window object.
Spanning windows appear in all cells they occupy."
  (let ((grid (spatial-window--window-grid frame)))
    (mapcar (lambda (row)
              (mapcar (lambda (win)
                        (list :window win))
                      row))
            grid)))

(defun spatial-window--cell-percentages (&optional frame geom)
  "Return (col-pcts . row-pcts) for grid cells in FRAME.
COL-PCTS is a list of column width percentages.
ROW-PCTS is a list of row height percentages.
If GEOM is provided, use it instead of computing grid geometry."
  (let* ((frame (or frame (selected-frame)))
         (frame-w (frame-pixel-width frame))
         (frame-h (frame-pixel-height frame))
         (geom (or geom (spatial-window--grid-geometry frame)))
         (x-coords (nth 0 geom))
         (y-coords (nth 1 geom)))
    (cons
     ;; Column width percentages
     (cl-loop for i from 0 below (1- (length x-coords))
              collect (/ (float (- (nth (1+ i) x-coords) (nth i x-coords)))
                         frame-w))
     ;; Row height percentages
     (cl-loop for i from 0 below (1- (length y-coords))
              collect (/ (float (- (nth (1+ i) y-coords) (nth i y-coords)))
                         frame-h)))))

;;; Boundary/mapping functions

(defun spatial-window--compute-boundaries (percentages key-count)
  "Compute grid cell boundaries based on PERCENTAGES for KEY-COUNT keys.
Returns list of (start-key . end-key) for each grid cell, non-overlapping.
Each cell is guaranteed at least 1 key.  Returns nil if there are more
cells than keys (e.g., more window rows than keyboard rows)."
  (let* ((n (length percentages))
         (boundaries nil))
    (if (> n key-count)
        ;; More cells than keys: return nil
        nil
      ;; Distribute keys: each cell gets at least 1, remainder by percentage
      (let* ((remainder (- key-count n))
             (prev-end -1))
        (dolist (pct percentages)
          (let* ((extra-keys (round (* pct remainder)))
                 (cell-keys (1+ extra-keys))  ; At least 1 + proportional share
                 (start-key (1+ prev-end))
                 (end-key (+ start-key cell-keys -1)))
            (push (cons start-key end-key) boundaries)
            (setq prev-end end-key)))
        ;; Adjust last boundary to cover remaining keys
        (let* ((boundaries-rev boundaries)
               (last-boundary (car boundaries-rev)))
          (setcar boundaries-rev (cons (car last-boundary) (1- key-count))))
        (nreverse boundaries)))))

(defun spatial-window--boundary-lookup (key-idx boundaries)
  "Find which grid cell KEY-IDX falls into based on BOUNDARIES."
  (let ((result 0))
    (cl-loop for boundary in boundaries
             for idx from 0
             when (and (>= key-idx (car boundary))
                       (<= key-idx (cdr boundary)))
             do (setq result idx))
    result))

(defun spatial-window--column-windows-balanced-p (grid col row-pcts)
  "Return non-nil if column COL has 2 windows of roughly equal height.
GRID is the window info grid, ROW-PCTS is the list of row height percentages.
Returns t if both windows are between 40% and 60% of the column height."
  (let* ((windows (seq-uniq (mapcar (lambda (row)
                                      (plist-get (nth col row) :window))
                                    grid)))
         (win-heights (make-hash-table :test 'eq)))
    (when (= (length windows) 2)
      ;; Sum up row percentages for each window
      (cl-loop for row in grid
               for pct in row-pcts
               for win = (plist-get (nth col row) :window)
               do (puthash win (+ (gethash win win-heights 0) pct) win-heights))
      ;; Check if both windows are between 40-60%
      (let ((h1 (gethash (car windows) win-heights))
            (h2 (gethash (cadr windows) win-heights)))
        (and (>= h1 0.4) (<= h1 0.6)
             (>= h2 0.4) (<= h2 0.6))))))

;;; Key assignment

(defun spatial-window--assign-keys (&optional frame info-grid cell-pcts kbd-layout)
  "Assign keyboard keys to windows based on their layout.
Returns alist of (window . (list of keys)).

Optional arguments allow dependency injection for testing:
  INFO-GRID - 2D grid of window info plists (from `spatial-window--window-info')
  CELL-PCTS - cons of (col-pcts . row-pcts) (from `spatial-window--cell-percentages')
  KBD-LAYOUT - keyboard layout as list of rows

When a column has exactly 2 roughly-equal windows and the keyboard has 3 rows,
the middle row is skipped for that column to improve the spatial mapping."
  (let ((kbd-layout (or kbd-layout (spatial-window--get-layout))))
    ;; Validate keyboard layout: all rows must have same length
    (unless (apply #'= (mapcar #'length kbd-layout))
      (message "Invalid keyboard layout: rows have different lengths")
      nil)
    (when (apply #'= (mapcar #'length kbd-layout))
      (let* ((info-grid (or info-grid (spatial-window--window-info frame)))
             (cell-pcts (or cell-pcts (spatial-window--cell-percentages frame)))
             (grid-rows (length info-grid))
             (grid-cols (length (car info-grid)))
             (kbd-rows (length kbd-layout))
             (kbd-cols (length (car kbd-layout)))
             ;; Build column boundaries based on cell widths
             (col-boundaries (spatial-window--compute-boundaries (car cell-pcts) kbd-cols))
             ;; Build row boundaries based on cell heights
             (row-boundaries (spatial-window--compute-boundaries (cdr cell-pcts) kbd-rows))
             ;; Check which columns have balanced 2-window splits (for row skipping)
             (balanced-cols (cl-loop for col below grid-cols
                                     collect (spatial-window--column-windows-balanced-p
                                              info-grid col (cdr cell-pcts))))
             (result (make-hash-table :test 'eq)))
        ;; Check if we have too many windows for the keyboard layout
        (if (not (and col-boundaries row-boundaries))
            (progn
              (message "Too many %s: %s"
                       (mapconcat #'identity
                                  (delq nil (list (unless col-boundaries "cols")
                                                  (unless row-boundaries "rows")))
                                  " and ")
                       (mapconcat #'identity
                                  (delq nil
                                        (list (unless col-boundaries
                                                (format "%d found of %d max" grid-cols kbd-cols))
                                              (unless row-boundaries
                                                (format "%d found of %d max" grid-rows kbd-rows))))
                                  "; "))
              nil)
          ;; Assign keys to windows
          (cl-loop for kbd-row from 0 below kbd-rows
                   do (cl-loop for kbd-col from 0 below kbd-cols
                               for grid-col = (spatial-window--boundary-lookup kbd-col col-boundaries)
                               for grid-row = (spatial-window--boundary-lookup kbd-row row-boundaries)
                               for balanced-p = (nth grid-col balanced-cols)
                               ;; Skip middle row only if column has 2 balanced windows
                               unless (and (= kbd-row (/ kbd-rows 2)) balanced-p)
                               do (let* ((key (nth kbd-col (nth kbd-row kbd-layout)))
                                         (info (nth grid-col (nth grid-row info-grid)))
                                         (win (plist-get info :window)))
                                    (push key (gethash win result)))))
          ;; Convert hash to alist, reverse key lists to preserve order
          (let ((alist nil))
            (maphash (lambda (win keys)
                       (push (cons win (nreverse keys)) alist))
                     result)
            alist))))))

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
