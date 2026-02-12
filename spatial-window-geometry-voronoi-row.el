;;; spatial-window-geometry-voronoi-row.el --- Row-gated Voronoi mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Le Wang

;; Author: Le Wang <lewang.dev.26@gmail.com>
;; URL: https://github.com/lewang/spatial-window
;; Version: 0.9.3
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
;; Weighted Voronoi assignment with row-band gating. A window must overlap
;; the row band of a cell by at least `spatial-window--row-overlap-min'
;; to be eligible. This discourages non-rectangular cross-row steals.

;;; Code:

(require 'cl-lib)

(defconst spatial-window--row-overlap-min 0.6
  "Minimum vertical overlap (fraction of cell height) to be eligible for a row.")

(defconst spatial-window--voronoi-ambiguity-ratio 0.75
  "Maximum best/second-best score ratio for a cell to be assigned.
When the ratio exceeds this threshold, the cell is ambiguous and left
unassigned.  0.75 corresponds to a ~57/43 split threshold, matching
the production algorithm's y-dominance margin.")

(defun spatial-window--window-centroid (wb)
  "Return centroid (x . y) of window bounds WB."
  (cons (/ (+ (nth 1 wb) (nth 2 wb)) 2.0)
        (/ (+ (nth 3 wb) (nth 4 wb)) 2.0)))

(defun spatial-window--window-area (wb)
  "Return normalized area of window bounds WB."
  (* (- (nth 2 wb) (nth 1 wb))
     (- (nth 4 wb) (nth 3 wb))))

(defun spatial-window--cell-y-overlap (cell-row kbd-rows win-y-start win-y-end)
  "Return vertical overlap fraction for CELL-ROW and window y-bounds."
  (let* ((cell-y-start (/ (float cell-row) kbd-rows))
         (cell-y-end (/ (float (1+ cell-row)) kbd-rows))
         (cell-h (- cell-y-end cell-y-start))
         (y-overlap-start (max cell-y-start win-y-start))
         (y-overlap-end (min cell-y-end win-y-end))
         (y-overlap-size (max 0.0 (- y-overlap-end y-overlap-start))))
    (/ y-overlap-size cell-h)))

(defun spatial-window--assign-cells (kbd-rows kbd-cols window-bounds)
  "Assign each cell to nearest weighted centroid (row-gated Voronoi).
A cell is left unassigned when the best and second-best windows are
vertically stacked (significant x-overlap) and their scores are too
close (ratio >= `spatial-window--voronoi-ambiguity-ratio')."
  (let* ((centroids (mapcar #'spatial-window--window-centroid window-bounds))
         (weights (mapcar #'spatial-window--window-area window-bounds))
         (grid (make-vector kbd-rows nil)))
    (dotimes (row kbd-rows)
      (aset grid row (make-vector kbd-cols nil))
      (dotimes (col kbd-cols)
        (let* ((cx (+ (/ (float col) kbd-cols) (/ 0.5 kbd-cols)))
               (cy (+ (/ (float row) kbd-rows) (/ 0.5 kbd-rows)))
               (best-idx nil)
               (best-score most-positive-fixnum)
               (second-idx nil)
               (second-score most-positive-fixnum)
               (eligible nil))
          (let ((geo-eligible nil))
            (dotimes (i (length window-bounds))
              (let* ((wb (nth i window-bounds))
                     (ov (spatial-window--cell-overlap row col kbd-rows kbd-cols
                                                      (nth 1 wb) (nth 2 wb) (nth 3 wb) (nth 4 wb))))
                (when (> ov 0)
                  (push i geo-eligible)
                  (let ((y-ov (spatial-window--cell-y-overlap row kbd-rows (nth 3 wb) (nth 4 wb))))
                    (when (>= y-ov spatial-window--row-overlap-min)
                      (push i eligible))))))
            ;; Centroid-in-band: prefer windows whose center is in this row
            (let* ((row-y-start (/ (float row) kbd-rows))
                   (row-y-end (/ (float (1+ row)) kbd-rows))
                   (in-band nil))
              (dolist (i geo-eligible)
                (let ((cy (cdr (nth i centroids))))
                  (when (and (>= cy row-y-start) (< cy row-y-end))
                    (push i in-band))))
              (when in-band
                (setq eligible in-band)))
            (when (null eligible)
              (setq eligible geo-eligible))
            (when (null eligible)
              (setq eligible (number-sequence 0 (1- (length window-bounds))))))
          (dolist (i eligible)
            (let* ((c (nth i centroids))
                   (w (max 0.001 (nth i weights)))
                   (dx (- cx (car c)))
                   (dy (- cy (cdr c)))
                   (dist (+ (* dx dx) (* dy dy)))
                   (score (/ dist w)))
              (cond
               ((< score best-score)
                (setq second-score best-score second-idx best-idx
                      best-score score best-idx i))
               ((< score second-score)
                (setq second-score score second-idx i)))))
          (when (and best-idx
                     (not (and second-idx
                               ;; Check if windows are vertically stacked
                               (let* ((best-wb (nth best-idx window-bounds))
                                      (sec-wb (nth second-idx window-bounds))
                                      (x-ov (max 0.0 (- (min (nth 2 best-wb) (nth 2 sec-wb))
                                                         (max (nth 1 best-wb) (nth 1 sec-wb)))))
                                      (min-w (min (- (nth 2 best-wb) (nth 1 best-wb))
                                                  (- (nth 2 sec-wb) (nth 1 sec-wb)))))
                                 (and (> min-w 0)
                                      (> (/ x-ov min-w) 0.5)
                                      (>= (/ best-score second-score)
                                           spatial-window--voronoi-ambiguity-ratio))))))
            (aset (aref grid row) col (car (nth best-idx window-bounds)))))))
    grid))

(defun spatial-window--count-all-keys (final kbd-rows kbd-cols)
  "Count keys per window in FINAL grid of KBD-ROWS x KBD-COLS."
  (let ((counts (make-hash-table :test 'eq)))
    (dotimes (row kbd-rows)
      (dotimes (col kbd-cols)
        (let ((win (aref (aref final row) col)))
          (when win
            (puthash win (1+ (gethash win counts 0)) counts)))))
    counts))

(defun spatial-window--cell-overlap (cell-row cell-col kbd-rows kbd-cols
                                              win-x-start win-x-end win-y-start win-y-end)
  "Return overlap fraction between a cell and a window."
  (let* ((cell-x-start (/ (float cell-col) kbd-cols))
         (cell-x-end (/ (float (1+ cell-col)) kbd-cols))
         (cell-y-start (/ (float cell-row) kbd-rows))
         (cell-y-end (/ (float (1+ cell-row)) kbd-rows))
         (x-overlap-start (max cell-x-start win-x-start))
         (x-overlap-end (min cell-x-end win-x-end))
         (y-overlap-start (max cell-y-start win-y-start))
         (y-overlap-end (min cell-y-end win-y-end))
         (x-overlap-size (max 0.0 (- x-overlap-end x-overlap-start)))
         (y-overlap-size (max 0.0 (- y-overlap-end y-overlap-start)))
         (overlap-area (* x-overlap-size y-overlap-size))
         (cell-width (/ 1.0 kbd-cols))
         (cell-height (/ 1.0 kbd-rows))
         (cell-area (* cell-width cell-height)))
    (/ overlap-area cell-area)))

(defun spatial-window--assign-corners (grid kbd-rows kbd-cols window-bounds)
  "Force corner cells to the window nearest each screen corner."
  (dolist (corner (list (list 0 0 0.0 0.0)
                        (list 0 (1- kbd-cols) 1.0 0.0)
                        (list (1- kbd-rows) 0 0.0 1.0)
                        (list (1- kbd-rows) (1- kbd-cols) 1.0 1.0)))
    (let* ((row (nth 0 corner)) (col (nth 1 corner))
           (sx (nth 2 corner)) (sy (nth 3 corner))
           (best-win nil) (best-dist most-positive-fixnum))
      (dolist (wb window-bounds)
        (let* ((wx (max (nth 1 wb) (min sx (nth 2 wb))))
               (wy (max (nth 3 wb) (min sy (nth 4 wb))))
               (dx (- sx wx)) (dy (- sy wy))
               (d (+ (* dx dx) (* dy dy))))
          (when (< d best-dist)
            (setq best-dist d best-win (car wb)))))
      (when best-win
        (aset (aref grid row) col best-win)))))

(defun spatial-window--connect-row-groups (grid kbd-rows kbd-cols window-bounds)
  "Fill gaps between a window's disconnected cell groups in the same row.
Only fills when the window physically overlaps every gap cell."
  (dotimes (row kbd-rows)
    (let ((win-cols (make-hash-table :test 'eq)))
      ;; Collect columns per window
      (dotimes (col kbd-cols)
        (let ((w (aref (aref grid row) col)))
          (when w (puthash w (cons col (gethash w win-cols nil)) win-cols))))
      ;; Check each window for gaps
      (maphash
       (lambda (win cols)
         (let* ((sorted (sort (copy-sequence cols) #'<))
                (min-col (car sorted))
                (max-col (car (last sorted))))
           (when (> (1+ (- max-col min-col)) (length sorted))
             ;; Has gap — check if window overlaps all gap cells
             (let ((wb (cl-find win window-bounds :key #'car))
                   (can-fill t))
               (cl-loop for c from (1+ min-col) below max-col
                        unless (memq c sorted)
                        do (when (<= (spatial-window--cell-overlap
                                      row c kbd-rows kbd-cols
                                      (nth 1 wb) (nth 2 wb) (nth 3 wb) (nth 4 wb))
                                     0)
                             (setq can-fill nil)))
               (when can-fill
                 (cl-loop for c from (1+ min-col) below max-col
                          unless (memq c sorted)
                          do (aset (aref grid row) c win)))))))
       win-cols))))

(defun spatial-window--ensure-all-windows-have-keys (final kbd-rows kbd-cols window-bounds)
  "Ensure every window gets at least one key by stealing best-overlap cells."
  (let ((counts (spatial-window--count-all-keys final kbd-rows kbd-cols))
        (changed t))
    (while changed
      (setq changed nil)
      (dolist (wb window-bounds)
        (let ((win (car wb)))
          (when (= (gethash win counts 0) 0)
            (let ((best-row nil) (best-col nil) (best-ov 0.0))
              (dotimes (row kbd-rows)
                (dotimes (col kbd-cols)
                  (let* ((ov (spatial-window--cell-overlap
                              row col kbd-rows kbd-cols
                              (nth 1 wb) (nth 2 wb) (nth 3 wb) (nth 4 wb)))
                         (owner (aref (aref final row) col))
                         (can-steal (or (null owner)
                                        (> (gethash owner counts 0) 1))))
                    (when (and can-steal (> ov best-ov))
                      (setq best-row row best-col col best-ov ov)))))
              (when best-row
                (let ((old-owner (aref (aref final best-row) best-col)))
                  (aset (aref final best-row) best-col win)
                  (puthash win 1 counts)
                  (when old-owner
                    (puthash old-owner (1- (gethash old-owner counts 0)) counts))
                  (setq changed t))))))))))

(defun spatial-window--compute-assignment (window-bounds)
  "Compute spatial grid assignment for WINDOW-BOUNDS.
Uses a hardcoded 3×10 grid matching all supported keyboard topologies.
Returns 2D vector (3 rows × 10 cols) of window labels, nil for unassigned cells.
Returns nil with message if more than 30 windows."
  (let ((kbd-rows 3)
        (kbd-cols 10)
        (num-windows (length window-bounds)))
    (if (> num-windows 30)
        (progn
          (message "Too many windows: %d windows for 30 keys" num-windows)
          nil)
      (let ((final (spatial-window--assign-cells kbd-rows kbd-cols window-bounds)))
        (spatial-window--assign-corners final kbd-rows kbd-cols window-bounds)
        (spatial-window--connect-row-groups final kbd-rows kbd-cols window-bounds)
        (spatial-window--ensure-all-windows-have-keys
         final kbd-rows kbd-cols window-bounds)
        final))))

(defun spatial-window--grid-to-strings (grid)
  "Convert assignment GRID to list of space-separated label strings.
Each cell renders as the window's symbol-name, or · for nil."
  (let ((result nil))
    (dotimes (row (length grid))
      (let ((cells nil))
        (dotimes (col (length (aref grid row)))
          (let ((win (aref (aref grid row) col)))
            (push (if win (symbol-name win) "·") cells)))
        (push (mapconcat #'identity (nreverse cells) " ") result)))
    (nreverse result)))

(provide 'spatial-window-geometry-voronoi-row)

;;; spatial-window-geometry-voronoi-row.el ends here
