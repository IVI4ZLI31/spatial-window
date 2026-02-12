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

(defun spatial-window--frame-windows ()
  "Return list of windows in current frame, excluding minibuffer."
  (window-list nil 'no-minibuf))

(defun spatial-window--window-bounds (&optional frame)
  "Return window bounds for FRAME as list of (window x-start x-end y-start y-end)."
  (let* ((frame (or frame (selected-frame)))
         (frame-w (float (frame-pixel-width frame)))
         (frame-h (float (frame-pixel-height frame)))
         (windows (window-list frame 'no-minibuf)))
    (mapcar (lambda (w)
              (let ((edges (window-pixel-edges w)))
                (list w
                      (/ (nth 0 edges) frame-w)
                      (/ (nth 2 edges) frame-w)
                      (/ (nth 1 edges) frame-h)
                      (/ (nth 3 edges) frame-h))))
            windows)))

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
          (dotimes (i (length window-bounds))
            (let* ((wb (nth i window-bounds))
                   (y-ov (spatial-window--cell-y-overlap row kbd-rows
                                                         (nth 3 wb) (nth 4 wb))))
              (when (>= y-ov spatial-window--row-overlap-min)
                (push i eligible))))
          (when (null eligible)
            (setq eligible (number-sequence 0 (1- (length window-bounds)))))
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

(defun spatial-window--final-to-keys (final kbd-rows kbd-cols kbd-layout)
  "Convert FINAL assignment grid to alist of (window . keys)."
  (let ((result (make-hash-table :test 'eq)))
    (dotimes (row kbd-rows)
      (dotimes (col kbd-cols)
        (let ((win (aref (aref final row) col)))
          (when win
            (push (nth col (nth row kbd-layout)) (gethash win result))))))
    (let ((alist nil))
      (maphash (lambda (win keys)
                 (push (cons win (nreverse keys)) alist))
               result)
      alist)))

(defun spatial-window--assign-keys (kbd-layout &optional frame window-bounds)
  "Assign keyboard keys to windows based on row-gated Voronoi.
KBD-LAYOUT is the keyboard layout as list of rows (required)."
  (let ((kbd-layout kbd-layout))
    (if (not (apply #'= (mapcar #'length kbd-layout)))
        (progn
          (message "Invalid keyboard layout: rows have different lengths")
          nil)
      (let* ((window-bounds (or window-bounds (spatial-window--window-bounds frame)))
             (kbd-rows (length kbd-layout))
             (kbd-cols (length (car kbd-layout)))
             (num-windows (length window-bounds)))
        (if (> num-windows (* kbd-rows kbd-cols))
            (progn
              (message "Too many windows: %d windows for %d keys" num-windows (* kbd-rows kbd-cols))
              nil)
          (let ((final (spatial-window--assign-cells kbd-rows kbd-cols window-bounds)))
            (spatial-window--ensure-all-windows-have-keys
             final kbd-rows kbd-cols window-bounds)
            (spatial-window--final-to-keys final kbd-rows kbd-cols kbd-layout)))))))

(defun spatial-window--format-key-grid (keys kbd-layout)
  "Format KEYS as a keyboard grid string using KBD-LAYOUT."
  (let ((key-set (make-hash-table :test 'equal)))
    (dolist (k keys)
      (puthash k t key-set))
    (mapconcat
     (lambda (row)
       (mapconcat
        (lambda (key)
          (if (gethash key key-set) key "·"))
        row " "))
     kbd-layout
     "\n")))

(provide 'spatial-window-geometry-voronoi-row)

;;; spatial-window-geometry-voronoi-row.el ends here
