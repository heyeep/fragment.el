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

;; Auto-run demo when file is loaded
(when (not noninteractive)
  (fragment-demo))

(provide 'fragment-demo)

;;; fragment-demo.el ends here
