;;; fragment-demo.el --- fragment.el demo -*- lexical-binding: t; -*-

;; Author: Hiep Nguyen

;;; Commentary:

;;; Code:

(add-to-list 'load-path ".")
(require 'fragment)
(require 'fragment-mode)

;; Simple color list
(defvar demo-colors '("#FF6B6B" "#4ECDC4" "#45B7D1" "#96CEB4" "#FFEAA7"
                      "#DDA0DD" "#98D8E8" "#F7DC6F" "#BB8FCE" "#85C1E9"))

(defvar demo-color-index 0)

(defun demo-next-color ()
  "Get next color."
  (let ((color (nth demo-color-index demo-colors)))
    (setq demo-color-index (mod (1+ demo-color-index) (length demo-colors)))
    color))

;;;###autoload
(defun fragment-demo ()
  "Simple colorful demo using built-in color properties."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Fragment Demo*"))
  (erase-buffer)
  (fragment-grid-mode 1)

  ;; Create simple 3x4 grid
  (let ((grid (fragment-create-grid 3 4)))

    ;; No gap for seamless color coverage
    (oset grid gap 0)


    ;; Add content and colors to cells
    (setq demo-color-index 0)
    (dotimes (r 3)
      (dotimes (c 4)
        (let ((cell (fragment-grid-get grid r c))
              (bg-color (demo-next-color)))
          (oset cell background-color bg-color)
          (oset cell foreground-color "white")
          (fragment-cell-set-content cell (format " %02d " (+ (* r 4) c)))
          ;; Set dimensions after content (so fit-content doesn't override)
          (oset cell width 4)
          (oset cell height 4)
          )))

    ;; Re-render after setting content
    (fragment-grid-render grid)

    (goto-char (point-min))
    (message "Demo complete! Colors are now built into Fragment.el cells!")))

;;;###autoload
(defun fragment-text-wrap-demo ()
  "Demo showcasing text wrapping capabilities."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Fragment Text Wrap Demo*"))
  (erase-buffer)
  (fragment-grid-mode 1)

  ;; Create 2x2 grid for text wrapping showcase
  (let ((grid (fragment-create-grid 2 2)))

    ;; No gap for seamless color coverage
    (oset grid gap 0)

    ;; Configure cells with different text wrapping scenarios
    (setq demo-color-index 0)
    (dotimes (r 2)
      (dotimes (c 2)
        (let ((cell (fragment-grid-get grid r c))
              (bg-color (demo-next-color))
              (cell-num (+ (* r 2) c)))
          (oset cell background-color bg-color)
          (oset cell foreground-color "white")

          ;; First set all cells to narrow width to force wrapping scenario
          (oset cell width 12)

          (cond
           ((= cell-num 0) ; Top-left: text wrapping enabled
            (oset cell text-wrap t)
            (fragment-cell-set-content cell "This is a very long text that should wrap when it hits the edge of the allocated cell width"))

           ((= cell-num 1) ; Top-right: no wrapping
            (oset cell text-wrap nil)
            (fragment-cell-set-content cell "This text will overflow without wrapping beyond the cell boundary"))

           ((= cell-num 2) ; Bottom-left: wrapping with newlines
            (oset cell text-wrap t)
            (fragment-cell-set-content cell "Multi-line\ntext content with some longer lines that exceed the cell width and should wrap"))

           ((= cell-num 3) ; Bottom-right: normal short text
            (fragment-cell-set-content cell "Short text"))))))

    ;; Re-render after setting content
    (fragment-grid-render grid)

    (goto-char (point-min))
    (message "Text Wrap Demo: Cell 0=wrapped, 1=truncated, 2=multi+wrap, 3=normal")))

;;;###autoload
(defun fragment-merge-demo ()
  "Demo showcasing cell merging by clicking cells."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Fragment Merge Demo*"))
  (erase-buffer)
  (fragment-grid-mode 1)

  ;; Create 4x4 grid for merge demo
  (let ((grid (fragment-create-grid 4 4)))

    ;; No gap for seamless color coverage
    (oset grid gap 0)

    ;; Add content and colors to cells
    (setq demo-color-index 0)
    (dotimes (r 4)
      (dotimes (c 4)
        (let ((cell (fragment-grid-get grid r c))
              (bg-color (demo-next-color)))
          (oset cell background-color bg-color)
          (oset cell foreground-color "white")
          (fragment-cell-set-content cell (format " %d,%d " r c))
          (oset cell width 8)
          (oset cell height 3))))

    ;; Re-render after setting content
    (fragment-grid-render grid)

    ;; Set up click-to-merge functionality
    (fragment-setup-merge-clicking grid)

    (goto-char (point-min))
    (message "Merge Demo: Click two cells to merge them. First click selects, second merges.")))

(defvar fragment--merge-selection nil
  "Stores the first selected cell for merging.")

(defun fragment-setup-merge-clicking (grid)
  "Set up mouse clicking for merge demo."
  (message "Setting up mouse click handler...")
  ;; Make sure we're in the right buffer and mode
  (setq buffer-read-only nil)
  (local-set-key [down-mouse-1] 'ignore) ; Ignore down events to prevent issues
  (local-set-key [mouse-1]
    (lambda (event)
      (interactive "e")
      (message "Mouse click detected!")
      ;; Prevent any text insertion from the mouse event
      (let ((inhibit-read-only t)
            (click-pos (posn-point (event-start event))))
        (when click-pos
          (goto-char click-pos)
          (fragment-handle-merge-click grid))))))

(defun fragment-handle-merge-click (grid)
  "Handle cell clicking for merge demo."
  (let ((cell (fragment-demo-cell-at-point grid)))
    (if cell
        (let ((row (oref cell row))
              (col (oref cell col)))
          (if fragment--merge-selection
              ;; Second click - merge the cells
              (let ((sel-row (oref fragment--merge-selection row))
                    (sel-col (oref fragment--merge-selection col)))
                (if (and (= row sel-row) (= col sel-col))
                    (message "Same cell selected. Try clicking a different cell.")
                  (progn
                    (fragment-merge-cells-demo grid fragment--merge-selection cell)
                    (setq fragment--merge-selection nil)
                    (message "Cells (%d,%d) and (%d,%d) merged!" sel-row sel-col row col))))
            ;; First click - select the cell
            (setq fragment--merge-selection cell)
            ;; Add visual feedback by temporarily changing the cell content
            ;; BUT don't re-render the whole grid to avoid losing colors
            (let ((original-content (oref cell content)))
              (oset cell content (format " [%d,%d] " row col))
              ;; Update just this cell's content in place using its markers
              (when (oref cell markers)
                (let ((start-marker (fragment-marker-pair-start (oref cell markers)))
                      (end-marker (fragment-marker-pair-end (oref cell markers))))
                  (when (and start-marker end-marker)
                    (let ((inhibit-read-only t)
                          (start-pos (marker-position start-marker))
                          (end-pos (marker-position end-marker)))
                      (when (and start-pos end-pos)
                        (goto-char start-pos)
                        (delete-region start-pos end-pos)
                        (let ((new-content (fragment-pad-string (oref cell content) (- end-pos start-pos))))
                          (insert new-content)
                          ;; Apply color directly to the region we just inserted
                          (fragment-apply-face-to-region cell start-pos (+ start-pos (length new-content))))))))))
            (message "Cell (%d,%d) selected. Click another cell to merge." row col)))
      ;; No cell found
      (message "No cell found at click position"))))

(defun fragment-demo-cell-at-point (grid)
  "Find which cell the cursor is in using hybrid marker + geometry approach."
  (let ((current-pos (point))
        (found-cell nil)
        (rows (oref grid rows))
        (cols (oref grid columns)))

    ;; HYBRID APPROACH: Use markers to define buffer positions, geometry to find multi-line cell areas
    (dotimes (r rows)
      (dotimes (c cols)
        (let ((cell (fragment-grid-get grid r c)))
          (when (and cell (oref cell markers) (not found-cell))
            (let* ((start-marker (fragment-marker-pair-start (oref cell markers)))
                   (end-marker (fragment-marker-pair-end (oref cell markers))))

              (when (and start-marker end-marker)
                (let* (;; Get marker positions - these define the actual cell area in buffer
                       (marker-start-pos (marker-position start-marker))
                       (marker-end-pos (marker-position end-marker))

                       ;; Convert to line/column for multi-line area calculation
                       (marker-start-line (line-number-at-pos marker-start-pos))
                       (marker-start-col (save-excursion (goto-char marker-start-pos) (current-column)))
                       (marker-end-line (line-number-at-pos marker-end-pos))
                       (marker-end-col (save-excursion (goto-char marker-end-pos) (current-column)))

                       ;; Current cursor position
                       (current-line (line-number-at-pos current-pos))
                       (current-col (current-column))

                       ;; Calculate cell area: markers define horizontal span, geometry defines vertical span
                       (cell-width-from-markers (- marker-end-pos marker-start-pos))
                       (cell-height (oref cell height))
                       (cell-area-end-line (+ marker-start-line cell-height -1))
                       (cell-area-end-col (+ marker-start-col cell-width-from-markers)))

                  ;; Check if cursor is within the cell area using marker boundaries + cell height
                  (when (and (>= current-line marker-start-line)
                            (<= current-line cell-area-end-line)
                            (>= current-col marker-start-col)
                            (< current-col cell-area-end-col))
                    (setq found-cell cell)
                    (message "HYBRID: Cell (%d,%d) at cursor L%d C%d - markers define area L%d C%d to L%d C%d"
                             (oref cell row) (oref cell col)
                             current-line current-col
                             marker-start-line marker-start-col
                             cell-area-end-line cell-area-end-col)))))))))

    found-cell))

(defun fragment-merge-cells-demo (grid cell1 cell2)
  "Merge CELL1 and CELL2 in GRID using border-based system."
  (let* ((r1 (oref cell1 row)) (c1 (oref cell1 col))
         (r2 (oref cell2 row)) (c2 (oref cell2 col)))
    (cond
     ;; Horizontal merge (same row, adjacent columns)
     ((and (= r1 r2) (= (abs (- c1 c2)) 1))
      (let ((left-cell (if (< c1 c2) cell1 cell2))
            (right-cell (if (< c1 c2) cell2 cell1)))
        ;; Set merge properties
        (oset left-cell east-open t)
        (oset right-cell west-open t)
        (oset right-cell is-master nil)
        (oset right-cell master-cell left-cell)

        ;; Update colors - both cells get the same color (from master)
        (let ((master-bg-color (oref left-cell background-color))
              (master-fg-color (oref left-cell foreground-color)))
          (oset right-cell background-color master-bg-color)
          (oset right-cell foreground-color master-fg-color)
          (message "DEBUG: After merge - Left cell bg=%s, Right cell bg=%s"
                   (oref left-cell background-color) (oref right-cell background-color)))

        ;; Keep original content of master cell - no content change needed

        ;; Re-render to show the merge
        (fragment-grid-render grid)
        ;; Force a complete color refresh with a small delay to ensure it takes
        (sit-for 0.01)
        (fragment-grid-apply-colors grid)))
     ;; Vertical merge (same column, adjacent rows)
     ((and (= c1 c2) (= (abs (- r1 r2)) 1))
      (let ((top-cell (if (< r1 r2) cell1 cell2))
            (bottom-cell (if (< r1 r2) cell2 cell1)))
        ;; Set merge properties
        (oset top-cell south-open t)
        (oset bottom-cell north-open t)
        (oset bottom-cell is-master nil)
        (oset bottom-cell master-cell top-cell)

        ;; Update colors - both cells get the same color (from master)
        (let ((master-bg-color (oref top-cell background-color))
              (master-fg-color (oref top-cell foreground-color)))
          (oset bottom-cell background-color master-bg-color)
          (oset bottom-cell foreground-color master-fg-color))

        ;; Keep original content of master cell - no content change needed

        ;; Re-render to show the merge
        (fragment-grid-render grid)
        ;; Force a complete color refresh with a small delay to ensure it takes
        (sit-for 0.01)
        (fragment-grid-apply-colors grid)))
     ;; Not adjacent
     (t (message "Cells must be adjacent to merge")))))

;; Auto-run demo when file is loaded
(when (not noninteractive)
  (fragment-demo))

(provide 'fragment-demo)

;;; fragment-demo.el ends here
