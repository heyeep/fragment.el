;;; fragment-demo.el --- Fragment.el Colorful Demo -*- lexical-binding: t; -*-

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
          (oset cell height 4))))

    ;; Re-render after setting content
    (fragment-grid-render grid)

    (goto-char (point-min))
    (message "Demo complete! Colors are now built into Fragment.el cells!")))

;; Auto-run demo when file is loaded
(when (not noninteractive)
  (fragment-demo))

(provide 'fragment-demo)
