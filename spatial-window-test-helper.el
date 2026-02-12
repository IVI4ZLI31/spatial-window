;;; spatial-window-test-helper.el --- Test helpers for spatial-window -*- lexical-binding: t; -*-

;;; Commentary:
;; ASCII box-drawing renderer for window-bounds data, used in test output
;; and debugging.

;;; Code:

(require 'cl-lib)

(defun spatial-window--bts-box-char (up down left right)
  "Return box-drawing character for junction with given connections.
UP, DOWN, LEFT, RIGHT are non-nil if a line extends in that direction."
  (let ((key (logior (if up 8 0) (if down 4 0) (if left 2 0) (if right 1 0))))
    (aref [ ?\s   ?─   ?─   ?─     ;; 0-3
            ?│   ?┌   ?┐   ?┬      ;; 4-7
            ?│   ?└   ?┘   ?┴      ;; 8-11
            ?│   ?├   ?┤   ?┼ ]    ;; 12-15
          key)))

(defun spatial-window--bts-grid (window-bounds)
  "Build grid from WINDOW-BOUNDS.
Returns (GRID X-COORDS Y-COORDS) where GRID is a 2D vector of labels."
  (let* ((xs (sort (delete-dups
                    (cl-loop for (_l left right _t _b) in window-bounds
                             collect left collect right))
                   #'<))
         (ys (sort (delete-dups
                    (cl-loop for (_l _left _right top bottom) in window-bounds
                             collect top collect bottom))
                   #'<))
         (ncols (1- (length xs)))
         (nrows (1- (length ys)))
         (grid (make-vector nrows nil)))
    (dotimes (r nrows)
      (aset grid r (make-vector ncols nil)))
    (dolist (entry window-bounds)
      (let* ((label (car entry))
             (left (nth 1 entry)) (right (nth 2 entry))
             (top (nth 3 entry)) (bottom (nth 4 entry)))
        (dotimes (r nrows)
          (when (and (>= (nth r ys) top) (< (nth r ys) bottom))
            (dotimes (c ncols)
              (when (and (>= (nth c xs) left) (< (nth c xs) right))
                (aset (aref grid r) c label)))))))
    (list grid (vconcat xs) (vconcat ys))))

(defun spatial-window--bts-dims-string (left right top bottom)
  "Format dimensions string for window bounds LEFT RIGHT TOP BOTTOM."
  (format "%dw×%dh"
          (round (* (- right left) 100))
          (round (* (- bottom top) 100))))

(defun spatial-window--bts-wrap-dims (dims-str avail-width)
  "Wrap DIMS-STR to fit AVAIL-WIDTH, returning list of lines.
If it fits, return as single-element list.  Otherwise split at × into 3 lines."
  (if (<= (length dims-str) avail-width)
      (list dims-str)
    (if (string-match "\\`\\([^×]+\\)\\(×\\)\\(.+\\)\\'" dims-str)
        (list (match-string 1 dims-str)
              (match-string 2 dims-str)
              (match-string 3 dims-str))
      (list dims-str))))

(defun spatial-window--bts-min-content-width (label dims-str multi-row-p)
  "Compute minimum content width (excluding padding) for a window.
LABEL is the label string, DIMS-STR is the dimensions string.
MULTI-ROW-P means dims can wrap vertically."
  (if multi-row-p
      ;; Multi-row: dims will wrap, so we need max of label and each wrapped segment
      (let ((segments (spatial-window--bts-wrap-dims dims-str 0))) ; force full wrap
        (apply #'max (length label)
               (mapcar #'length segments)))
    ;; Single-row: dims must fit on one line
    (max (length label) (length dims-str))))

(defun spatial-window--bts-col-widths (grid x-coords y-coords window-bounds)
  "Compute column widths using proportional scaling.
GRID is the label grid, X-COORDS and Y-COORDS are coordinate vectors,
WINDOW-BOUNDS is the original bounds list."
  (let* ((ncols (1- (length x-coords)))
         (nrows (1- (length y-coords)))
         (min-scale 0.0))
    ;; For each window, compute minimum scale factor
    (dolist (entry window-bounds)
      (let* ((label (symbol-name (car entry)))
             (left (nth 1 entry)) (right (nth 2 entry))
             (top (nth 3 entry)) (bottom (nth 4 entry))
             (frac-w (- right left))
             (dims (spatial-window--bts-dims-string left right top bottom))
             (row-count 0))
        (dotimes (r nrows)
          (when (and (>= (aref y-coords r) top) (< (aref y-coords r) bottom))
            (cl-incf row-count)))
        (let* ((min-cw (spatial-window--bts-min-content-width label dims (> row-count 1)))
               (need (+ min-cw 1))  ; +1 for left padding
               (scale (/ (float need) frac-w)))
          (when (> scale min-scale)
            (setq min-scale scale)))))
    ;; Compute raw widths
    (let* ((col-widths (make-vector ncols 0))
           (total 0))
      (dotimes (c ncols)
        (let* ((frac (- (aref x-coords (1+ c)) (aref x-coords c)))
               (w (max 3 (round (* frac min-scale)))))
          ;; Ensure label fits in each column
          (dotimes (r nrows)
            (let* ((label (aref (aref grid r) c))
                   (label-str (if label (symbol-name label) ""))
                   (min-w (+ (length label-str) 2)))
              (when (> min-w w) (setq w min-w))))
          (aset col-widths c w)
          (cl-incf total w)))
      ;; Adjust rounding: compute expected total from scale
      (let* ((expected-total (round min-scale))
             (diff (- expected-total total)))
        (when (/= diff 0)
          ;; Find widest column and adjust
          (let ((widest-idx 0)
                (widest-w 0))
            (dotimes (c ncols)
              (when (> (aref col-widths c) widest-w)
                (setq widest-w (aref col-widths c))
                (setq widest-idx c)))
            (aset col-widths widest-idx
                  (max 3 (+ (aref col-widths widest-idx) diff))))))
      col-widths)))

(defun spatial-window--bts-row-heights (grid x-coords y-coords col-widths window-bounds)
  "Compute row heights for the grid using proportional scaling.
GRID, X-COORDS, Y-COORDS, COL-WIDTHS, WINDOW-BOUNDS as in other helpers."
  (let* ((ncols (1- (length x-coords)))
         (nrows (1- (length y-coords)))
         (min-v-scale 0.0))
    ;; Compute minimum vertical scale: for each window, content_lines / frac_height
    (dolist (entry window-bounds)
      (let* ((label (symbol-name (car entry)))
             (left (nth 1 entry)) (right (nth 2 entry))
             (top (nth 3 entry)) (bottom (nth 4 entry))
             (frac-h (- bottom top))
             (dims (spatial-window--bts-dims-string left right top bottom))
             (rows nil)
             (cols nil))
        (dotimes (r nrows)
          (when (and (>= (aref y-coords r) top) (< (aref y-coords r) bottom))
            (push r rows)))
        (dotimes (c ncols)
          (when (and (>= (aref x-coords c) left) (< (aref x-coords c) right))
            (push c cols)))
        (setq rows (nreverse rows) cols (nreverse cols))
        (let* ((span-w (cl-loop for c in cols sum (aref col-widths c)))
               (total-w (+ span-w (1- (length cols))))
               (avail (- total-w 1))
               (content-lines (cons label (spatial-window--bts-wrap-dims dims avail)))
               (nlines (max 2 (length content-lines)))
               (scale (/ (float nlines) frac-h)))
          (when (> scale min-v-scale)
            (setq min-v-scale scale)))))
    ;; Apply proportional heights
    (let* ((row-heights (make-vector nrows 0))
           (total 0))
      (dotimes (r nrows)
        (let* ((frac (- (aref y-coords (1+ r)) (aref y-coords r)))
               (h (max 2 (round (* frac min-v-scale)))))
          (aset row-heights r h)
          (cl-incf total h)))
      ;; Adjust rounding errors: add/subtract from tallest row
      (let* ((expected-total (round min-v-scale))
             (diff (- expected-total total)))
        (when (/= diff 0)
          (let ((tallest-idx 0)
                (tallest-h 0))
            (dotimes (r nrows)
              (when (> (aref row-heights r) tallest-h)
                (setq tallest-h (aref row-heights r))
                (setq tallest-idx r)))
            (aset row-heights tallest-idx
                  (max 2 (+ (aref row-heights tallest-idx) diff))))))
      ;; Pass 2: multi-row windows may need more height
      (dolist (entry window-bounds)
        (let* ((label (symbol-name (car entry)))
               (left (nth 1 entry)) (right (nth 2 entry))
               (top (nth 3 entry)) (bottom (nth 4 entry))
               (dims (spatial-window--bts-dims-string left right top bottom))
               (rows nil)
               (cols nil))
          (dotimes (r nrows)
            (when (and (>= (aref y-coords r) top) (< (aref y-coords r) bottom))
              (push r rows)))
          (dotimes (c ncols)
            (when (and (>= (aref x-coords c) left) (< (aref x-coords c) right))
              (push c cols)))
          (setq rows (nreverse rows) cols (nreverse cols))
          (when (> (length rows) 1)
            (let* ((span-w (cl-loop for c in cols sum (aref col-widths c)))
                   (total-w (+ span-w (1- (length cols))))
                   (avail (- total-w 1))
                   (content-lines (cons label (spatial-window--bts-wrap-dims dims avail)))
                   (nlines (length content-lines))
                   (total-h (+ (cl-loop for r in rows sum (aref row-heights r))
                               (1- (length rows)))))
              (when (> nlines total-h)
                (let ((extra (- nlines total-h)))
                  (while (> extra 0)
                    (dolist (r rows)
                      (when (> extra 0)
                        (cl-incf (aref row-heights r))
                        (cl-decf extra))))))))))
      row-heights)))

(defun spatial-window--bts-cell-label (grid r c nrows ncols)
  "Get label at grid cell (R, C), or nil if out of bounds.
GRID is the label grid, NROWS and NCOLS are dimensions."
  (if (and (>= r 0) (< r nrows) (>= c 0) (< c ncols))
      (aref (aref grid r) c)
    nil))

(defun spatial-window--bts-render-hline (grid y-pos ncols col-widths nrows)
  "Render a horizontal line at Y-POS in the grid.
GRID is the label grid, NCOLS is column count, COL-WIDTHS is column width vector.
NROWS is total rows.  Y-POS is a row boundary: 0 = top, NROWS = bottom."
  (let ((result ""))
    (dotimes (x-idx (1+ ncols))
      (let* ((has-up (and (> y-pos 0)
                          (or (= x-idx 0) (= x-idx ncols)
                              (not (eq (spatial-window--bts-cell-label grid (1- y-pos) (1- x-idx) nrows ncols)
                                       (spatial-window--bts-cell-label grid (1- y-pos) x-idx nrows ncols))))))
             (has-down (and (< y-pos nrows)
                            (or (= x-idx 0) (= x-idx ncols)
                                (not (eq (spatial-window--bts-cell-label grid y-pos (1- x-idx) nrows ncols)
                                         (spatial-window--bts-cell-label grid y-pos x-idx nrows ncols))))))
             (has-left (and (> x-idx 0)
                            (or (= y-pos 0) (= y-pos nrows)
                                (not (eq (spatial-window--bts-cell-label grid (1- y-pos) (1- x-idx) nrows ncols)
                                         (spatial-window--bts-cell-label grid y-pos (1- x-idx) nrows ncols))))))
             (has-right (and (< x-idx ncols)
                             (or (= y-pos 0) (= y-pos nrows)
                                 (not (eq (spatial-window--bts-cell-label grid (1- y-pos) x-idx nrows ncols)
                                          (spatial-window--bts-cell-label grid y-pos x-idx nrows ncols))))))
             (junction (spatial-window--bts-box-char has-up has-down has-left has-right)))
        (setq result (concat result (string junction)))
        (when (< x-idx ncols)
          (let* ((h-line (or (= y-pos 0) (= y-pos nrows)
                             (not (eq (spatial-window--bts-cell-label grid (1- y-pos) x-idx nrows ncols)
                                      (spatial-window--bts-cell-label grid y-pos x-idx nrows ncols)))))
                 (w (aref col-widths x-idx)))
            (setq result (concat result
                                 (if h-line
                                     (make-string w ?─)
                                   (make-string w ?\s))))))))
    result))

(defun spatial-window--bts-separator-junction (grid y-pos x-idx ncols nrows)
  "Compute junction character at (X-IDX, Y-POS) for an interior separator.
GRID, NCOLS, NROWS as context.  Y-POS is between 1 and NROWS-1."
  (let* ((has-up (and (> y-pos 0)
                      (or (= x-idx 0) (= x-idx ncols)
                          (not (eq (spatial-window--bts-cell-label grid (1- y-pos) (1- x-idx) nrows ncols)
                                   (spatial-window--bts-cell-label grid (1- y-pos) x-idx nrows ncols))))))
         (has-down (and (< y-pos nrows)
                        (or (= x-idx 0) (= x-idx ncols)
                            (not (eq (spatial-window--bts-cell-label grid y-pos (1- x-idx) nrows ncols)
                                     (spatial-window--bts-cell-label grid y-pos x-idx nrows ncols))))))
         (has-left (and (> x-idx 0)
                        (not (eq (spatial-window--bts-cell-label grid (1- y-pos) (1- x-idx) nrows ncols)
                                 (spatial-window--bts-cell-label grid y-pos (1- x-idx) nrows ncols)))))
         (has-right (and (< x-idx ncols)
                         (not (eq (spatial-window--bts-cell-label grid (1- y-pos) x-idx nrows ncols)
                                  (spatial-window--bts-cell-label grid y-pos x-idx nrows ncols))))))
    (spatial-window--bts-box-char has-up has-down has-left has-right)))

(defun spatial-window--bts-window-content (label-sym bounds-ht col-widths cols row-heights rows)
  "Compute content lines for a window.
LABEL-SYM is the window label symbol, BOUNDS-HT is the bounds hash table,
COL-WIDTHS and COLS are column info, ROW-HEIGHTS and ROWS are row info.
Returns a list of strings, one per line, left-padded with 1 space."
  (let* ((entry (gethash label-sym bounds-ht))
         (label (symbol-name label-sym))
         (left (nth 1 entry)) (right (nth 2 entry))
         (top (nth 3 entry)) (bottom (nth 4 entry))
         (dims (spatial-window--bts-dims-string left right top bottom))
         (span-w (cl-loop for c in cols sum (aref col-widths c)))
         (total-w (+ span-w (1- (length cols))))
         (avail (- total-w 1))
         (content-lines (cons label (spatial-window--bts-wrap-dims dims avail)))
         ;; Total height available for this window's content
         (total-h (+ (cl-loop for r in rows sum (aref row-heights r))
                     (1- (length rows))))
         (nlines (length content-lines))
         (top-pad (/ (- total-h nlines) 2))
         (result nil))
    ;; Build padded content
    (dotimes (_ top-pad)
      (push (make-string total-w ?\s) result))
    (dolist (line content-lines)
      (let* ((padded (concat " " line))
             (fill (- total-w (length padded))))
        (push (concat padded (if (> fill 0) (make-string fill ?\s) "")) result)))
    (let ((remaining (- total-h (+ top-pad nlines))))
      (dotimes (_ remaining)
        (push (make-string total-w ?\s) result)))
    (nreverse result)))

(defun spatial-window--bts-render-content-line (grid row line-idx col-widths row-heights
                                                     x-coords y-coords bounds-ht nrows ncols)
  "Render one content line for ROW at LINE-IDX.
GRID, COL-WIDTHS, ROW-HEIGHTS, X-COORDS, Y-COORDS, BOUNDS-HT, NROWS, NCOLS as context."
  (let ((result "")
        (c 0))
    (while (< c ncols)
      (let* ((label (aref (aref grid row) c))
             ;; Find all columns this window spans
             (cols (list c))
             (cc (1+ c)))
        (while (and (< cc ncols) (eq (aref (aref grid row) cc) label))
          (push cc cols)
          (setq cc (1+ cc)))
        (setq cols (nreverse cols))
        ;; Find all rows this window spans
        (let* ((entry (and label (gethash label bounds-ht)))
               (top (and entry (nth 3 entry)))
               (bottom (and entry (nth 4 entry)))
               (rows (when entry
                       (let ((rs nil))
                         (dotimes (r nrows)
                           (when (and (>= (aref y-coords r) top)
                                      (< (aref y-coords r) bottom))
                             (push r rs)))
                         (nreverse rs))))
               ;; Compute global line index within the window's total height
               (lines-before-this-row
                (if rows
                    (cl-loop for r in rows
                             while (< r row)
                             sum (+ (aref row-heights r) 1))  ; +1 for separator
                  0))
               (global-line (+ lines-before-this-row line-idx))
               ;; Get all content for this window
               (all-content
                (when label
                  (spatial-window--bts-window-content
                   label bounds-ht col-widths cols row-heights rows)))
               (line-text (if (and all-content (< global-line (length all-content)))
                              (nth global-line all-content)
                            (let ((span-w (cl-loop for col in cols sum (aref col-widths col))))
                              (make-string (+ span-w (1- (length cols))) ?\s)))))
          ;; Left border
          (let* ((left-label (and (> c 0) (aref (aref grid row) (1- c))))
                 (border (if (or (= c 0) (not (eq left-label label)))
                             "│"
                           "")))
            (setq result (concat result border)))
          (setq result (concat result line-text))
          (setq c cc))))
    ;; Right border
    (setq result (concat result "│"))
    result))

(defun spatial-window--bts-render-separator (grid y-pos ncols col-widths row-heights
                                                  x-coords y-coords bounds-ht nrows)
  "Render separator line between row Y-POS-1 and Y-POS.
For spanning windows, shows content; for boundaries, shows horizontal lines.
GRID, NCOLS, COL-WIDTHS, ROW-HEIGHTS, X-COORDS, Y-COORDS, BOUNDS-HT, NROWS as context."
  (let ((result "")
        (c 0))
    (while (< c ncols)
      (let* ((above-label (spatial-window--bts-cell-label grid (1- y-pos) c nrows ncols))
             (below-label (spatial-window--bts-cell-label grid y-pos c nrows ncols))
             (spans-p (and above-label below-label (eq above-label below-label))))
        (if spans-p
            ;; This window spans across the separator — show content
            (let* ((label above-label)
                   (cols (list c))
                   (cc (1+ c)))
              (while (and (< cc ncols) (eq (aref (aref grid (1- y-pos)) cc) label)
                          (eq (aref (aref grid y-pos) cc) label))
                (push cc cols)
                (setq cc (1+ cc)))
              (setq cols (nreverse cols))
              (let* ((junction (spatial-window--bts-separator-junction
                                grid y-pos c ncols nrows))
                     (entry (gethash label bounds-ht))
                     (top (nth 3 entry)) (bottom (nth 4 entry))
                     (rows (let ((rs nil))
                             (dotimes (r nrows)
                               (when (and (>= (aref y-coords r) top)
                                          (< (aref y-coords r) bottom))
                                 (push r rs)))
                             (nreverse rs)))
                     (all-content (spatial-window--bts-window-content
                                   label bounds-ht col-widths cols row-heights rows))
                     (lines-before (cl-loop for r in rows
                                            while (< r y-pos)
                                            sum (+ (aref row-heights r) 1)))
                     (global-line (1- lines-before))
                     (span-w (cl-loop for col in cols sum (aref col-widths col)))
                     (total-w (+ span-w (1- (length cols))))
                     (line-text (if (and all-content (< global-line (length all-content)))
                                    (nth global-line all-content)
                                  (make-string total-w ?\s))))
                (setq result (concat result (string junction) line-text))
                (setq c cc)))
          ;; Boundary — show horizontal line
          (let* ((junction (spatial-window--bts-separator-junction
                            grid y-pos c ncols nrows))
                 (w (aref col-widths c)))
            (setq result (concat result (string junction) (make-string w ?─)))
            (setq c (1+ c))))))
    ;; Right border junction
    (let ((junction (spatial-window--bts-separator-junction
                     grid y-pos ncols ncols nrows)))
      (setq result (concat result (string junction))))
    result))

(defun spatial-window--bts-render (grid x-coords y-coords col-widths row-heights window-bounds)
  "Render the ASCII box-drawing from grid data.
GRID, X-COORDS, Y-COORDS, COL-WIDTHS, ROW-HEIGHTS, WINDOW-BOUNDS as computed."
  (let* ((ncols (1- (length x-coords)))
         (nrows (1- (length y-coords)))
         (lines nil))
    (let ((bounds-ht (make-hash-table :test 'eq)))
      (dolist (entry window-bounds)
        (puthash (car entry) entry bounds-ht))
      ;; Top border
      (push (spatial-window--bts-render-hline grid 0 ncols col-widths nrows)
            lines)
      ;; For each row
      (dotimes (r nrows)
        ;; Content lines
        (let ((h (aref row-heights r)))
          (dotimes (line-idx h)
            (push (spatial-window--bts-render-content-line
                   grid r line-idx col-widths row-heights
                   x-coords y-coords bounds-ht nrows ncols)
                  lines)))
        ;; Separator or bottom border
        (if (< (1+ r) nrows)
            (push (spatial-window--bts-render-separator
                   grid (1+ r) ncols col-widths row-heights
                   x-coords y-coords bounds-ht nrows)
                  lines)
          (push (spatial-window--bts-render-hline grid (1+ r) ncols col-widths nrows)
                lines)))
      (mapconcat #'identity (nreverse lines) "\n"))))

(defun spatial-window--bounds-to-string (window-bounds)
  "Render WINDOW-BOUNDS as ASCII box-drawing art.
WINDOW-BOUNDS is a list of (LABEL LEFT RIGHT TOP BOTTOM) entries
where coordinates are fractional (0.0 to 1.0).

Example:
  (spatial-window--bounds-to-string
   \\='((L 0.0 0.5 0.0 1.0) (R 0.5 1.0 0.0 1.0)))

Returns a string with box-drawing characters showing window layout."
  (let* ((grid-data (spatial-window--bts-grid window-bounds))
         (grid (nth 0 grid-data))
         (x-coords (nth 1 grid-data))
         (y-coords (nth 2 grid-data))
         (col-widths (spatial-window--bts-col-widths grid x-coords y-coords window-bounds))
         (row-heights (spatial-window--bts-row-heights grid x-coords y-coords col-widths window-bounds)))
    (spatial-window--bts-render grid x-coords y-coords col-widths row-heights window-bounds)))

(provide 'spatial-window-test-helper)

;;; spatial-window-test-helper.el ends here
