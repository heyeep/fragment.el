;;; fragment.el --- resizeable grid layout -*- lexical-binding: t; -*-

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
  "Base class for all Fragment components")

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

   ;; Parent reference
   (grid :initarg :grid
         :initform nil
         :documentation "Parent grid reference"))

  "Individual cell within a grid")

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
  "Return PAIR of markers to pool for reuse."
  (when pair
    (set-marker (fragment-marker-pair-start pair) nil)
    (set-marker (fragment-marker-pair-end pair) nil)
    (push (fragment-marker-pair-start pair) fragment--marker-pool)
    (push (fragment-marker-pair-end pair) fragment--marker-pool)))

;;; Grid Creation and Management

(defun fragment-grid-create (rows cols)
  "Create a new grid with ROWS and COLS dimensions."
  (let ((grid (fragment-grid :rows rows :columns cols)))
    ;; Initialize cells
    (dotimes (r rows)
      (dotimes (c cols)
        (let ((cell (fragment-cell :row r :col c :grid grid)))
          (fragment-grid-add-cell grid cell))))
    grid))

(defun fragment-grid-add-cell (grid cell)
  "Add a CELL to the GRID."
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
  "Get cell at (ROW, COL) in GRID."
  (gethash (cons row col) (oref grid cells)))

(defun fragment-grid-find (grid id)
  "Find cell by ID in GRID."
  (gethash id (oref grid cells-by-id)))

;;; Cell Content Management

(defun fragment-cell-set-content (cell content)
  "Set CONTENT for CELL and resize it to fit."
  (oset cell content content)
  (fragment-cell-fit-content cell))

(defun fragment-cell-fit-content (cell)
  "Resize CELL to fit its content."
  (let* ((content (oref cell content))
         (lines (split-string content "\n"))
         (width (apply #'max (mapcar #'length lines)))
         (height (length lines))

         ;; Apply constraints
         (final-width (fragment-constrain width
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
      (fragment-grid-cell-resized (oref cell grid) cell))))

(defun fragment-constrain (value min-val max-val)
  "Constrain VALUE between MIN-VAL and MAX-VAL."
  (let ((result (max value (or min-val value))))
    (if max-val
        (min result max-val)
      result)))

(defun fragment-grid-cell-resized (grid cell)
  "Handle CELL resize in GRID."
  (fragment-grid-mark-dirty grid))

(defun fragment-grid-mark-dirty (grid)
  "Mark GRID as needing layout recalculation."
  (oset grid total-width 0)
  (oset grid total-height 0))

;;; minor mode

(declare-function fragment-grid-mode "fragment-mode")

;;; api

;;;###autoload
(defun fragment-create-grid (rows cols)
  "Create and display a new GRID with ROWS x COLS dimensions."
  (interactive "nNumber of rows: \nnNumber of columns: ")
  (let ((grid (fragment-grid-create rows cols)))
    (fragment-grid-render grid)
    grid))

(defun fragment-grid-render (grid)
  "Render GRID to current buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    ;; Clear buffer
    (erase-buffer)

    ;; Render each row
    (dotimes (r (oref grid rows))
      (fragment-grid-render-row grid r))

    ;; Set markers
    (fragment-grid-update-markers grid)

    ;; Apply colors after rendering
    (fragment-grid-apply-all-colors grid)))

(defun fragment-grid-render-row (grid row)
  "Render a single ROW of the GRID."
  (let ((start-pos (point)))
    (dotimes (c (oref grid columns))
      (let ((cell (fragment-grid-get grid row c)))
        (when cell
          (fragment-cell-render cell)
          (when (< c (1- (oref grid columns)))
            (insert (make-string (oref grid gap) ?\s))))))
    (insert "\n")
    start-pos))

(defun fragment-cell-render (cell)
  "Render a single CELL.
  (let* ((content (oref cell content))
         (width (oref cell width))
         (height (oref cell height))
         (lines (split-string content "\n"))
         (start-pos (point)))

    ;; Create marker pair if needed
    (unless (oref cell markers)
      (oset cell markers (fragment-acquire-marker-pair)))

    ;; Set start marker
    (let ((start-marker (fragment-marker-pair-start (oref cell markers))))
      (set-marker start-marker start-pos (current-buffer))
      (set-marker-insertion-type start-marker t))

    ;; Render content with padding
    (dotimes (line-idx height)
      (let ((line (or (nth line-idx lines) "")))
        (let ((padded-line (fragment-pad-string line width)))
          (message "Rendering line %d: '%s' (width=%d)" line-idx padded-line width)
          (insert padded-line))
        (when (< line-idx (1- height))
          (insert "\n"))))

    ;; Set end marker with correct insertion type
    (let ((end-marker (fragment-marker-pair-end (oref cell markers))))
      (set-marker end-marker (point) (current-buffer))
      (set-marker-insertion-type end-marker nil)) ; nil means marker stays at content end

    ;; Apply colors immediately during rendering when we have exact positions
    (when (or (oref cell background-color) (oref cell foreground-color))
      (let ((bg-color (oref cell background-color))
            (fg-color (oref cell foreground-color)))
        (when (< start-pos (point))
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
              (put-text-property start-pos (point) 'face (nreverse face-spec))
              (message "Applied colors during render: %d-%d" start-pos (point))))))))

(defun fragment-cell-apply-colors (cell)
  "Apply colors to a CELL based on its background-color and foreground-color properties."
  (when (and (oref cell markers)
             (or (oref cell background-color) (oref cell foreground-color)))
    (let ((bg-color (oref cell background-color))
          (fg-color (oref cell foreground-color))
          (start-marker (fragment-marker-pair-start (oref cell markers)))
          (end-marker (fragment-marker-pair-end (oref cell markers))))
      (let ((start (marker-position start-marker))
            (end (marker-position end-marker)))
        (when (and start end (< start end))
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
              (put-text-property start end 'face (nreverse face-spec)))))))))

(defun fragment-grid-apply-all-colors (grid)
  "Apply colors to all cells in a GRID."
  (dotimes (r (oref grid rows))
    (dotimes (c (oref grid columns))
      (let ((cell (fragment-grid-get grid r c)))
        (when cell
          (fragment-cell-apply-colors cell)))))))

(defun fragment-pad-string (str width)
  "Pad string STR to WIDTH characters."
  (let ((len (length str)))
    (if (>= len width)
        (substring str 0 width)
      (concat str (make-string (- width len) ?\s)))))

(defun fragment-grid-update-markers (grid)
  "Update GRID start and end markers to cover the entire grid."
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

(provide 'fragment)

;;; fragment.el ends here
