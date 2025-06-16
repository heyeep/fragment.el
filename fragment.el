;;; fragment.el --- resizeable grid layout -*- lexical-binding: t; -*-

;; Author: Hiep Nguyen

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'cl-lib)

;;; classes

(defclass fragment-component ()
  ((id :initarg :id
       :initform nil
       :documentation "Unique component identifier")

   (created-at :initform nil
               :documentation "Creation timestamp"))
  "Base class for all fragment components")

(cl-defmethod initialize-instance :after ((obj fragment-component) &rest _)
  "Initialize component with generated ID and timestamp."
  (unless (oref obj id)
    (oset obj id (fragment-generate-id)))
  (unless (oref obj created-at)
    (oset obj created-at (current-time))))

(defclass fragment-grid (fragment-component)
  ;; Structure
  ((rows :initarg :rows
         :initform 3
         :type integer
         :documentation "Number of rows in grid")

   (columns :initarg :columns
            :initform 3
            :type integer
            :documentation "Number of columns in grid")

   ;; Dimensions
   (total-width :initform 0
                :type integer
                :documentation "Total grid width in characters")

   (total-height :initform 0
                 :type integer
                 :documentation "Total grid height in lines")

   ;; Sizing
   (column-widths :initform nil
                  :type list
                  :documentation "Width of each column")

   (row-heights :initform nil
                :type list
                :documentation "Height of each row")

   ;; Cell management
   (cells :initform (make-hash-table :test 'equal)
          :documentation "Hash table of (row . col) -> cell")

   (cells-by-id :initform (make-hash-table :test 'equal)
                :documentation "Hash table of id -> cell")

   ;; Layout
   (gap :initarg :gap
        :initform 1
        :type integer
        :documentation "Space between cells")

   ;; Display
   (buffer :initform nil
           :documentation "Buffer displaying this grid")

   (start-marker :initform nil
                 :documentation "Grid start position")

   (end-marker :initform nil
               :documentation "Grid end position"))

  "Grid container managing cell layout and positioning")

(defclass fragment-cell (fragment-component)
  ;; Position
  ((row :initarg :row
        :type integer
        :documentation "Row position in grid")

   (col :initarg :col
        :type integer
        :documentation "Column position in grid")

   ;; Content
   (content :initarg :content
            :initform ""
            :type string
            :documentation "Cell text content")

   ;; Dimensions
   (width :initform 1
          :type integer
          :documentation "Current width in characters")

   (height :initform 1
           :type integer
           :documentation "Current height in lines")

   ;; Constraints
   (min-width :initarg :min-width
              :initform 1
              :type integer)

   (max-width :initarg :max-width
              :initform nil
              :type (or null integer))

   (min-height :initarg :min-height
               :initform 1
               :type integer)

   (max-height :initarg :max-height
               :initform nil
               :type (or null integer))

   ;; Display
   (markers :initform nil
            :documentation "Marker pair for position tracking")

   (overlays :initform nil
             :documentation "List of overlays on this cell")

   ;; Color properties
   (background-color :initarg :background-color
                     :initform nil
                     :type (or null string)
                     :documentation "Background color for this cell")

   (foreground-color :initarg :foreground-color
                     :initform nil
                     :type (or null string)
                     :documentation "Foreground color for this cell")


   ;; Text wrapping
   (text-wrap :initarg :text-wrap
              :initform nil
              :type boolean
              :documentation "Whether to wrap text that exceeds cell width")

   ;; Parent reference
   (grid :initarg :grid
         :initform nil
         :documentation "Parent grid reference")

   ;; Marker tracking for smart color updates
   (last-marker-positions :initform nil
                         :documentation "Cached marker positions (start . end)"))

  "Individual cell within a grid")

;; Clear any previous definitions
(when (fboundp 'fragment-cell-apply-color)
  (fmakunbound 'fragment-cell-apply-color))
(when (fboundp 'fragment-cell-markers-changed-p)
  (fmakunbound 'fragment-cell-markers-changed-p))
(when (fboundp 'fragment-cell-apply-color-if-moved)
  (fmakunbound 'fragment-cell-apply-color-if-moved))

(cl-defmethod fragment-cell-apply-color ((cell fragment-cell))
  "Apply colors to this CELL using its markers."
  (when (and (oref cell markers)
             (or (oref cell background-color) (oref cell foreground-color)))
    (let ((start-marker (fragment-marker-pair-start (oref cell markers)))
          (end-marker (fragment-marker-pair-end (oref cell markers))))
      (let ((start (marker-position start-marker))
            (end (marker-position end-marker)))
        (when (and start end (< start end))
          (fragment-apply-face-to-region cell start end))))))

(cl-defmethod fragment-cell-markers-changed-p ((cell fragment-cell))
  "Check if this CELL's markers have moved since last check."
  (when (oref cell markers)
    (let* ((markers (oref cell markers))
           (current-start (marker-position (fragment-marker-pair-start markers)))
           (current-end (marker-position (fragment-marker-pair-end markers)))
           (last-positions (oref cell last-marker-positions)))

      (if (or (null last-positions)
              (not (equal (cons current-start current-end) last-positions)))
          ;; Positions changed - update cache and return t
          (progn
            (oset cell last-marker-positions (cons current-start current-end))
            t)
        ;; No change
        nil))))

(cl-defmethod fragment-cell-apply-color-if-moved ((cell fragment-cell))
  "Apply colors only if CELL markers have changed."
  (when (fragment-cell-markers-changed-p cell)
    (fragment-cell-apply-color cell)))

;;; Marker Management

(cl-defstruct fragment-marker-pair
  start  ; Marker at cell start
  end    ; Marker at cell end
  cell)  ; Back-reference to cell

(defvar fragment--marker-pool nil
  "Pool of reusable markers for efficiency.")

(defvar fragment--id-counter 0
  "Counter for generating unique IDs.")

;;; Utility Functions

(defun fragment-generate-id ()
  "Generate a unique identifier for components."
  (format "fragment-%d" (cl-incf fragment--id-counter)))

(defun fragment-acquire-marker-pair ()
  "Get or create a marker pair from the pool."
  (let ((start (or (pop fragment--marker-pool) (make-marker)))
        (end (or (pop fragment--marker-pool) (make-marker))))
    (make-fragment-marker-pair :start start :end end)))

(defun fragment-release-marker-pair (pair)
  "Return markers PAIR to pool for reuse."
  (when pair
    (set-marker (fragment-marker-pair-start pair) nil)
    (set-marker (fragment-marker-pair-end pair) nil)
    (push (fragment-marker-pair-start pair) fragment--marker-pool)
    (push (fragment-marker-pair-end pair) fragment--marker-pool)))

(defun fragment-pad-string (str width)
  "Pad STR to specified WIDTH."
  (let ((len (length str)))
    (if (>= len width)
        (substring str 0 width)
      (concat str (make-string (- width len) ?\s)))))

(defun fragment-wrap-text-to-width (text width)
  "Wrap TEXT to specified WIDTH, returning wrapped string."
  (if (<= width 0)
      text
    (let ((lines (split-string text "\n"))
          (wrapped-lines '()))
      (dolist (line lines)
        (if (<= (length line) width)
            (push line wrapped-lines)
          ;; Split long lines into multiple wrapped lines
          (let ((remaining line))
            (while (> (length remaining) width)
              ;; Try to break at word boundary if possible
              (let ((break-point (fragment-find-wrap-point remaining width)))
                (push (substring remaining 0 break-point) wrapped-lines)
                (setq remaining (substring remaining break-point))))
            (when (> (length remaining) 0)
              (push remaining wrapped-lines)))))
      (mapconcat 'identity (nreverse wrapped-lines) "\n"))))

(defun fragment-find-wrap-point (text width)
  "Find optimal wrap point in TEXT within WIDTH, preferring word boundaries."
  (if (<= (length text) width)
      (length text)
    ;; Look for space within wrap width, working backwards from width
    (let ((break-point width))
      (while (and (> break-point 0)
                  (not (= (aref text (1- break-point)) ?\s)))
        (setq break-point (1- break-point)))
      ;; If no space found, break at width (hard break)
      (if (= break-point 0)
          width
        break-point))))

;;; Color Management

(defvar fragment--color-list '("#FF6B6B" "#4ECDC4" "#45B7D1" "#96CEB4" "#FFEAA7"
                               "#DDA0DD" "#98D8E8" "#F7DC6F" "#BB8FCE" "#85C1E9")
  "List of colors for grid cells.")

(defvar fragment--color-index 0
  "Current index in the color list.")

(defun fragment-get-next-color ()
  "Get the next color from the color list, cycling through."
  (let ((color (nth fragment--color-index fragment--color-list)))
    (setq fragment--color-index (mod (1+ fragment--color-index) (length fragment--color-list)))
    color))

;;; Grid Creation and Management

(defun fragment-grid-create (rows cols)
  "Create a new grid with specified ROWS and COLS."
  (let ((grid (fragment-grid :rows rows :columns cols)))
    ;; Initialize cells
    (dotimes (r rows)
      (dotimes (c cols)
        (let ((cell (fragment-cell :row r :col c :grid grid)))
          (fragment-grid-add-cell grid cell))))
    grid))

(defun fragment-grid-add-cell (grid cell)
  "Add a CELL to GRID at its position."
  (let* ((row (oref cell row))
         (col (oref cell col))
         (key (cons row col)))
    ;; Add to position-based lookup
    (puthash key cell (oref grid cells))
    ;; Add to ID-based lookup
    (puthash (oref cell id) cell (oref grid cells-by-id))
    ;; Set parent reference
    (oset cell grid grid)))

(defun fragment-grid-get (grid row col)
  "Get cell at specified ROW and COL in GRID."
  (gethash (cons row col) (oref grid cells)))

(defun fragment-grid-find (grid id)
  "Find GRID by ID."
  (gethash id (oref grid cells-by-id)))

;;; Cell Content Management

(defun fragment-cell-set-content (cell content)
  "Set CELL CONTENT and trigger resize."
  (oset cell content content)
  (fragment-cell-fit-content cell))

(defun fragment-cell-fit-content (cell)
  "Resize CELL to fit its content."
  (let* ((content (oref cell content))
         (text-wrap (oref cell text-wrap))
         (max-width (oref cell max-width))
         (lines (split-string content "\n"))
         (width (apply #'max (mapcar #'length lines)))
         (height (length lines)))

    ;; Apply constraints (no content modification for wrapping)
    (let ((final-width (fragment-constrain width
                                          (oref cell min-width)
                                          (oref cell max-width)))
          (final-height (fragment-constrain height
                                           (oref cell min-height)
                                           (oref cell max-height))))

      ;; Update cell dimensions
      (oset cell width final-width)
      (oset cell height final-height)

      ;; Notify grid of dimension change if attached
      (when (oref cell grid)
        (fragment-grid-cell-resized (oref cell grid) cell)))))

(defun fragment-constrain (value min-val max-val)
  "Constrain VALUE between MIN-VAL and MAX-VAL."
  (let ((result (max value (or min-val value))))
    (if max-val
        (min result max-val)
      result)))

(defun fragment-grid-cell-resized (grid cell)
  "Handle GRID CELL resize notification."
  (fragment-grid-mark-dirty grid))

(defun fragment-grid-mark-dirty (grid)
  "Mark GRID as needing layout recalculation."
  (oset grid total-width 0)
  (oset grid total-height 0))

;;; Minor Mode Declaration (implementation in separate file)

(declare-function fragment-grid-mode "fragment-mode")

;;; Public API

;;;###autoload
(defun fragment-create-grid (rows cols)
  "Create and display a new grid with ROWS x COLS dimensions."
  (interactive "nNumber of rows: \nnNumber of columns: ")
  (let ((grid (fragment-grid-create rows cols)))
    (fragment-grid-render grid)
    grid))

(defun fragment-grid-render (grid)
  "Render GRID to current buffer with proper grid layout."
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    ;; Clear buffer
    (erase-buffer)

    ;; Build grid as string first, then insert with proper markers
    (let ((grid-string (fragment-grid-build-string grid)))
      (insert grid-string)

      ;; Now set markers for each cell based on their positions in the final string
      (fragment-grid-set-all-markers grid))

    ;; Apply colors after rendering with proper cell area coverage
    (fragment-grid-apply-colors grid)

    ;; Add expand buttons
    (fragment-grid-add-expand-buttons grid)

    ;; Make grid content read-only (but not the buttons)
    ;; (fragment-grid-make-readonly grid)  ; Temporarily disabled

    ;; Set current grid for editing
    (setq-local fragment--current-grid grid)))

(defun fragment-grid-build-string (grid)
  "Build the entire GRID as a string with proper layout."
  (let* ((rows (oref grid rows))
         (cols (oref grid columns))
         (gap (oref grid gap))
         (result ""))

    ;; Calculate row heights and column widths
    (let ((row-heights (make-vector rows 0))
          (col-widths (make-vector cols 0)))

      ;; Find max height for each row and max width for each column
      (dotimes (r rows)
        (let ((max-height 0))
          (dotimes (c cols)
            (let ((cell (fragment-grid-get grid r c)))
              (when cell
                (setq max-height (max max-height (oref cell height)))
                (aset col-widths c (max (aref col-widths c) (oref cell width))))))
          (aset row-heights r max-height)))

      ;; Build each row
      (dotimes (r rows)
        (let ((row-height (aref row-heights r)))
          ;; Build each line of this row
          (dotimes (line-idx row-height)
            (let ((line ""))
              ;; Build each column in this line
              (dotimes (c cols)
                (let* ((cell (fragment-grid-get grid r c))
                       (col-width (aref col-widths c))
                       (cell-content (if cell
                                        (let* ((content (oref cell content))
                                               (text-wrap (oref cell text-wrap))
                                               ;; Apply wrapping if enabled
                                               (processed-content (if text-wrap
                                                                     (fragment-wrap-text-to-width content col-width)
                                                                   content))
                                               (lines (split-string processed-content "\n"))
                                               (cell-line (or (nth line-idx lines) "")))
                                          (fragment-pad-string cell-line col-width))
                                      (make-string col-width ?\s))))
                  (setq line (concat line cell-content))
                  ;; Add gap except after last column
                  (when (< c (1- cols))
                    (setq line (concat line (make-string gap ?\s))))))
              ;; Add this line to result
              (setq result (concat result line))
              ;; Add newline except for last line of last row
              (when (not (and (= r (1- rows)) (= line-idx (1- row-height))))
                (setq result (concat result "\n")))))))

      result)))

(defun fragment-grid-set-all-markers (grid)
  "Set markers for all cells within GRID based on their positions in the rendered buffer."
  (let* ((rows (oref grid rows))
         (cols (oref grid columns))
         (gap (oref grid gap)))

    ;; Calculate row heights and column widths (same as in build-string)
    (let ((row-heights (make-vector rows 0))
          (col-widths (make-vector cols 0)))

      ;; Find max height for each row and max width for each column
      (dotimes (r rows)
        (let ((max-height 0))
          (dotimes (c cols)
            (let ((cell (fragment-grid-get grid r c)))
              (when cell
                (setq max-height (max max-height (oref cell height)))
                (aset col-widths c (max (aref col-widths c) (oref cell width))))))
          (aset row-heights r max-height)))

      ;; Calculate buffer positions for each cell
      (let ((current-pos 1)) ; Buffer positions start at 1
        (dotimes (r rows)
          (let ((row-height (aref row-heights r))
                (row-start-pos current-pos))
            ;; For each cell in this row, calculate its position
            (dotimes (c cols)
              (let ((cell (fragment-grid-get grid r c)))
                (when cell
                  (let* ((col-start (let ((pos 0))
                                     (dotimes (prev-c c)
                                       (setq pos (+ pos (aref col-widths prev-c) gap)))
                                     pos))
                         (cell-start (+ row-start-pos col-start))
                         (col-width (aref col-widths c))  ; Use full column width, not just content
                         (cell-height (oref cell height))
                         ;; Cover the entire cell area including padding
                         (cell-end (+ cell-start col-width)))

                    ;; Create and set markers
                    (unless (oref cell markers)
                      (oset cell markers (fragment-acquire-marker-pair)))

                    (let ((start-marker (fragment-marker-pair-start (oref cell markers)))
                          (end-marker (fragment-marker-pair-end (oref cell markers))))
                      (set-marker start-marker cell-start (current-buffer))
                      (set-marker-insertion-type start-marker t)
                      (set-marker end-marker cell-end (current-buffer))
                      (set-marker-insertion-type end-marker nil))))))

            ;; Move to next row
            (dotimes (line-idx row-height)
              (goto-char current-pos)
              (setq current-pos (line-end-position))
              (when (< line-idx (1- row-height))
                (setq current-pos (1+ current-pos)))) ; +1 for newline
            (setq current-pos (1+ current-pos)))))))) ; +1 for final newline

(defun fragment-grid-refresh-moved-colors (grid)
  "Refresh colors only for cells whose markers have moved in GRID."
  (dotimes (r (oref grid rows))
    (dotimes (c (oref grid columns))
      (let ((cell (fragment-grid-get grid r c)))
        (when cell
          (fragment-cell-apply-color-if-moved cell))))))

(defun fragment-apply-face-to-region (cell start end)
  "Apply face to the region defined by START and END based on CELL properties."
  (let ((bg-color (oref cell background-color))
        (fg-color (oref cell foreground-color)))

    (when (or bg-color fg-color)
      (let ((face-spec '()))
        (when bg-color
          (push :background face-spec)
          (push bg-color face-spec))
        (when fg-color
          (push :foreground face-spec)
          (push fg-color face-spec))
        (when face-spec
          (push :weight face-spec)
          (push 'bold face-spec)
          (put-text-property start end 'face (nreverse face-spec)))))))

(defun fragment-apply-visual-wrapping (cell start end wrap-width)
  "Apply text wrap in CELL from START to END with WRAP-WIDTH."
  (when (and wrap-width (> wrap-width 0))
    ;; Get original content from cell object, not from buffer
    (let* ((original-content (oref cell content))
           (cell-height (oref cell height))
           (wrapped-content (fragment-wrap-text-to-width original-content wrap-width))
           (wrapped-lines (split-string wrapped-content "\n"))
           ;; Truncate wrapped content to fit within original cell height
           (truncated-lines (if (> (length wrapped-lines) cell-height)
                               (seq-take wrapped-lines cell-height)
                             wrapped-lines))
           (padded-wrapped (fragment-pad-wrapped-content truncated-lines wrap-width cell-height)))

      ;; Only apply wrapping overlay if content actually needs wrapping
      (unless (string= original-content wrapped-content)
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'display padded-wrapped)
          (overlay-put overlay 'fragment-wrap t)

          ;; Store overlay in cell for cleanup
          (when (slot-boundp cell 'overlays)
            (oset cell overlays (cons overlay (oref cell overlays)))))))))

(defun fragment-pad-wrapped-content (wrapped-lines width height)
  "Pad WRAPPED-LINES to WIDTH and HEIGHT to match grid layout."
  (let ((padded-lines '()))
    ;; Pad each wrapped line to the full width
    (dotimes (i height)
      (let ((line (or (nth i wrapped-lines) "")))
        (push (fragment-pad-string line width) padded-lines)))
    (mapconcat 'identity (nreverse padded-lines) "\n")))

(defun fragment-grid-apply-colors (grid)
  "Apply colors to all cells with proper full-cell coverage in a GRID."
  (let* ((rows (oref grid rows))
         (cols (oref grid columns))
         (gap (oref grid gap)))

    ;; Calculate row heights and column widths (same as in build-string)
    (let ((row-heights (make-vector rows 0))
          (col-widths (make-vector cols 0)))

      ;; Find max height for each row and max width for each column
      (dotimes (r rows)
        (let ((max-height 0))
          (dotimes (c cols)
            (let ((cell (fragment-grid-get grid r c)))
              (when cell
                (setq max-height (max max-height (oref cell height)))
                (aset col-widths c (max (aref col-widths c) (oref cell width))))))
          (aset row-heights r max-height)))

      ;; Apply colors to each cell with full area coverage
      (let ((current-pos 1)) ; Buffer positions start at 1
        (dotimes (r rows)
          (let ((row-height (aref row-heights r))
                (row-start-pos current-pos))

            ;; For each cell in this row, apply colors to its entire area
            (dotimes (c cols)
              (let ((cell (fragment-grid-get grid r c)))
                (when (and cell
                          (or (oref cell background-color) (oref cell foreground-color)))
                  (let* ((col-start (let ((pos 0))
                                     (dotimes (prev-c c)
                                       (setq pos (+ pos (aref col-widths prev-c) gap)))
                                     pos))
                         (col-width (aref col-widths c))
                         (cell-height (oref cell height)))

                    ;; Apply color to each line of the cell
                    (dotimes (line-idx cell-height)
                      (let* ((line-start (+ row-start-pos
                                           (* line-idx
                                              (+ (apply #'+ (append col-widths nil))
                                                 (* (1- cols) gap) 1)) ; +1 for newline
                                           col-start))
                             (line-end (+ line-start col-width)))

                        (when (and (>= line-start 1)
                                   (<= line-end (point-max))
                                   (< line-start line-end))
                          (fragment-apply-face-to-region cell line-start line-end))))))))

            ;; Move to next row
            (dotimes (line-idx row-height)
              (goto-char current-pos)
              (setq current-pos (line-end-position))
              (when (< line-idx (1- row-height))
                (setq current-pos (1+ current-pos)))) ; +1 for newline
            (setq current-pos (1+ current-pos))))))) ; +1 for final newline

(defun fragment-grid-update-markers (grid)
  "Update GRID level markers."
  (let ((start-pos (point-min))
        (end-pos (point-max)))
    (unless (oref grid start-marker)
      (oset grid start-marker (make-marker)))
    (unless (oref grid end-marker)
      (oset grid end-marker (make-marker)))

    (set-marker (oref grid start-marker) start-pos (current-buffer))
    (set-marker-insertion-type (oref grid start-marker) t)
    (set-marker (oref grid end-marker) end-pos (current-buffer))
    (set-marker-insertion-type (oref grid end-marker) t)))

(defun fragment-grid-add-expand-buttons (grid)
  "Add expand buttons below the GRID."
  (save-excursion
    ;; Add buttons below the grid
    (goto-char (point-max))
    (insert "\n")
    (insert-button "[+Row]"
                  'action (lambda (_button) (fragment-expand-row))
                  'face 'default
                  'help-echo "Add row")
    (insert "    ")  ; Some spacing
    (insert-button "[+Col]"
                  'action (lambda (_button) (fragment-expand-column))
                  'face 'default
                  'help-echo "Add column")))

(defun fragment-grid-make-readonly (grid)
  "Make GRID content read-only using text properties, but preserve buttons."
  (let ((start-pos (point-min)))
    ;; Find all button positions to exclude them from read-only
    (save-excursion
      (goto-char start-pos)
      (let ((current-pos start-pos))
        (while (< current-pos (point-max))
          (let ((next-button (next-single-property-change current-pos 'action)))
            (when next-button
              ;; Make text before button read-only
              (when (< current-pos next-button)
                (put-text-property current-pos next-button 'read-only t)
                (put-text-property current-pos next-button 'front-sticky '(read-only))
                (put-text-property current-pos next-button 'rear-nonsticky '(read-only)))
              ;; Skip over the button
              (setq current-pos (or (next-single-property-change next-button 'action) (point-max))))
            (unless next-button
              ;; Make remaining text read-only
              (when (< current-pos (point-max))
                (put-text-property current-pos (point-max) 'read-only t)
                (put-text-property current-pos (point-max) 'front-sticky '(read-only))
                (put-text-property current-pos (point-max) 'rear-nonsticky '(read-only)))
              (setq current-pos (point-max)))))))))

;;; Grid Expansion Functions

(defun fragment-grid-add-column (grid)
  "Add a new column to the right of GRID."
  (let ((rows (oref grid rows))
        (cols (oref grid columns)))
    ;; Update grid dimensions
    (oset grid columns (1+ cols))

    ;; Add new cells in the new column with colors
    (dotimes (r rows)
      (let ((cell (fragment-cell :row r :col cols :grid grid))
            (bg-color (fragment-get-next-color)))
        (oset cell background-color bg-color)
        (oset cell foreground-color "white")
        (fragment-cell-set-content cell (format " %d,%d " r cols))
        (fragment-grid-add-cell grid cell)))

    ;; Re-render the grid
    (fragment-grid-render grid)

    ;; Recalculate layout to fit properly
    (fragment-grid-recalculate-layout grid)))

(defun fragment-grid-add-row (grid)
  "Add a new row to the bottom of GRID."
  (let ((rows (oref grid rows))
        (cols (oref grid columns)))
    ;; Update grid dimensions
    (oset grid rows (1+ rows))

    ;; Add new cells in the new row with colors
    (dotimes (c cols)
      (let ((cell (fragment-cell :row rows :col c :grid grid))
            (bg-color (fragment-get-next-color)))
        (oset cell background-color bg-color)
        (oset cell foreground-color "white")
        (fragment-cell-set-content cell (format " %d,%d " rows c))
        (fragment-grid-add-cell grid cell)))

    ;; Re-render the grid
    (fragment-grid-render grid)

    ;; Recalculate layout to fit properly
    (fragment-grid-recalculate-layout grid)))

(defun fragment-grid-recalculate-layout (grid)
  "Recalculate and resize all cells in GRID to fill the entire window."
  (let ((window-width (window-width))
        (window-height (window-height))
        (rows (oref grid rows))
        (cols (oref grid columns))
        (gap (oref grid gap)))

    ;; Calculate cell dimensions to fill the entire available space
    (let* ((total-gap-width (* (1- cols) gap))
           (total-gap-height (* (1- rows) gap))
           ;; Use almost all available space, leaving minimal room for buttons
           (button-space 2) ; Just enough for the buttons below
           (available-width (- window-width total-gap-width))
           (available-height (- window-height total-gap-height button-space))
           (base-cell-width (/ available-width cols))
           (base-cell-height (/ available-height rows))
           ;; Calculate remainder pixels to distribute
           (width-remainder (% available-width cols))
           (height-remainder (% available-height rows)))

      ;; Resize all cells, distributing remainder pixels
      (dotimes (r rows)
        (dotimes (c cols)
          (let ((cell (fragment-grid-get grid r c)))
            (when cell
              ;; Give extra width/height to first few cells to use up remainder
              (let ((cell-width (+ base-cell-width (if (< c width-remainder) 1 0)))
                    (cell-height (+ base-cell-height (if (< r height-remainder) 1 0))))
                (oset cell width cell-width)
                (oset cell height cell-height))))))

      ;; Re-render with new dimensions that fill the window
      (fragment-grid-render grid))))

(defun fragment-expand-column ()
  "Add a column to the current grid."
  (interactive)
  (message "Expanding column...")
  (when fragment--current-grid
    (fragment-grid-add-column fragment--current-grid))
  (message "Column expansion complete"))

(defun fragment-expand-row ()
  "Add a row to the current grid."
  (interactive)
  (message "Expanding row...")
  (when fragment--current-grid
    (fragment-grid-add-row fragment--current-grid))
  (message "Row expansion complete"))

;;; Cell Editing Functions

(defun fragment-cell-edit ()
  "Edit the cell at current point."
  (interactive)
  (let ((cell (fragment-cell-at-point)))
    (if cell
        (let* ((current-content (oref cell content))
               (new-content (read-string "Edit cell: " current-content)))
          (fragment-cell-edit-content cell new-content))
      (message "No cell found at point"))))

(defun fragment-cell-edit-content (cell new-content)
  "Update CELL with NEW-CONTENT and re-render grid to preserve layout."
  (let ((grid (oref cell grid))
        (inhibit-read-only t))
    ;; Update cell content
    (oset cell content new-content)
    (fragment-cell-fit-content cell)

    ;; Re-render entire grid to maintain layout
    (fragment-grid-render grid)))

(defvar-local fragment--current-grid nil
  "Current grid in this buffer.")

(defun fragment-cell-at-point ()
  "Find the cell at current point position using line/column calculation."
  (when fragment--current-grid
    ;; For now, return the cell at (0,0) as a simple fallback
    ;; TODO: Implement proper position-to-cell mapping
    (fragment-grid-get fragment--current-grid 0 0))))

(provide 'fragment)

;;; fragment.el ends here
