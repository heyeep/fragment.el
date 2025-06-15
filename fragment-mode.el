;;; fragment-mode.el --- minor mode for fragment grid layout -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'fragment)

;;; customization

(defgroup fragment nil
  "Grid layout system."
  :group 'text
  :prefix "fragment-")

(defcustom fragment-auto-resize t
  "Whether to automatically resize grids when window size changes."
  :type 'boolean
  :group 'fragment)

(defcustom fragment-resize-debounce-delay 0.01
  "Delay in seconds before processing window resize events."
  :type 'number
  :group 'fragment)

;; TODO: This should be 0
(defcustom fragment-default-gap 1
  "Default gap size between grid cells."
  :type 'integer
  :group 'fragment)

;;; global

(defvar fragment--grids-in-buffer nil
  "List of grids in the current buffer.")

(defvar fragment--resize-timer nil
  "Timer for debouncing resize events.")

(defvar fragment--last-window-config nil
  "Last known window configuration for change detection.")

(defvar fragment--last-frame-size nil
  "Last known frame size for real-time change detection.")


;;; Grid Registration and Unregistration

(defun fragment-register-grid (grid)
  "Register a grid in the current buffer"
  (unless (memq grid fragment--grids-in-buffer)
    (push grid fragment--grids-in-buffer)))

(defun fragment-unregister-grid (grid)
  "Unregister a grid from the current buffer"
  (setq fragment--grids-in-buffer
        (delq grid fragment--grids-in-buffer)))

(defun fragment-get-grids-in-buffer ()
  "Get all grids in the current buffer"
  fragment--grids-in-buffer)

;;; Real-time resize detection
(defun fragment-realtime-resize-check (&optional window)
  "Check for real-time buffer/window size changes and resize grids"
  (when fragment-grid-mode
    (let* ((win (or window (selected-window)))
           (new-size (cons (window-body-width win) (window-body-height win))))
      (unless (equal new-size fragment--last-frame-size)
        (setq fragment--last-frame-size new-size)
        (fragment-resize-grids-in-buffer)))))

;;; Window Resize Handling

(defun fragment-window-resize-handler (&optional frame)
  "Handle window size changes with debouncing"
  (when fragment-auto-resize
    ;; Cancel existing timer
    (when fragment--resize-timer
      (cancel-timer fragment--resize-timer))

    ;; Set new debounced timer
    (setq fragment--resize-timer
          (run-with-timer
           fragment-resize-debounce-delay
           nil
           #'fragment-process-resize-event
           frame))))

(defun fragment-process-resize-event (&optional frame)
  "Process the actual resize event"
  (setq fragment--resize-timer nil)

  (dolist (window (window-list frame))
    (with-selected-window window
      (when fragment-grid-mode
        (fragment-resize-grids-in-buffer)))))

(defun fragment-resize-grids-in-buffer ()
  "Resize all grids in the current buffer"
  (dolist (grid fragment--grids-in-buffer)
    (when (buffer-live-p (oref grid buffer))
      (fragment-grid-handle-window-resize grid))))

(defun fragment-window-config-change-handler ()
  "Handle window configuration changes"
  (when fragment-auto-resize
    (let ((new-config (current-window-configuration)))
      (unless (equal new-config fragment--last-window-config)
        (setq fragment--last-window-config new-config)
        (fragment-window-resize-handler)))))

;;; Grid Resize Logic

(defun fragment-grid-handle-window-resize (grid)
  "Main resize handler for a specific grid"
  (when (and (oref grid buffer)
             (buffer-live-p (oref grid buffer)))
    (with-current-buffer (oref grid buffer)
      (let* ((window-width (window-body-width))
             (window-height (window-body-height))

             ;; Calculate available space (minus gaps)
             (available-width (- window-width
                                (* (1- (oref grid columns))
                                   (oref grid gap))))
             (available-height (- window-height
                                 (* (1- (oref grid rows))
                                    (oref grid gap))))

             ;; Distribute space to columns and rows
             (column-widths (fragment-calculate-column-widths
                            grid available-width))
             (row-heights (fragment-calculate-row-heights
                          grid available-height)))

        ;; Apply new dimensions
        (fragment-grid-apply-dimensions grid column-widths row-heights)))))

(defun fragment-calculate-column-widths (grid available-width)
  "Calculate width for each column based on available space."
  (let* ((columns (oref grid columns))
         (min-width 1) ; Minimum column width
         (base-width (max min-width (/ available-width columns)))
         (widths (make-list columns base-width))
         (used-width (* columns base-width))
         (remaining-width (- available-width used-width)))

    ;; Distribute any remaining space
    (when (> remaining-width 0)
      (dotimes (i (min remaining-width columns))
        (cl-incf (nth i widths))))

    widths))

(defun fragment-calculate-row-heights (grid available-height)
  "Calculate height for each row based on available space."
  (let* ((rows (oref grid rows))
         (min-height 1) ; Minimum row height
         (base-height (max min-height (/ available-height rows)))
         (heights (make-list rows base-height))
         (used-height (* rows base-height))
         (remaining-height (- available-height used-height)))

    ;; Distribute any remaining space
    (when (> remaining-height 0)
      (dotimes (i (min remaining-height rows))
        (cl-incf (nth i heights))))

    heights))

(defun fragment-grid-apply-dimensions (grid column-widths row-heights)
  "Apply new column widths and row heights to grid."
  (oset grid column-widths column-widths)
  (oset grid row-heights row-heights)

  ;; Update each cell's width and height
  (dotimes (r (oref grid rows))
    (dotimes (c (oref grid columns))
      (let ((cell (fragment-grid-get grid r c)))
        (when cell
          (oset cell width (nth c column-widths))
          (oset cell height (nth r row-heights))))))

  ;; Mark for re-rendering
  (fragment-grid-mark-dirty grid)

  ;; Re-render if buffer is visible
  (when (get-buffer-window (oref grid buffer))
    (fragment-grid-render grid)))

;;; Hook Management

(defun fragment-setup-hooks ()
  "Set up window resize and configuration change hooks."
  (add-hook 'window-size-change-functions
            #'fragment-window-resize-handler nil t)
  (add-hook 'window-configuration-change-hook
            #'fragment-window-config-change-handler nil t)

  ;; Hook into the mechanisms that provide real-time updates
  (when (boundp 'window-state-change-hook)
    (add-hook 'window-state-change-hook #'fragment-realtime-resize-check nil t))
  (when (boundp 'window-buffer-change-functions)
    (add-hook 'window-buffer-change-functions #'fragment-realtime-resize-check nil t))
  (when (boundp 'window-selection-change-functions)
    (add-hook 'window-selection-change-functions #'fragment-realtime-resize-check nil t))

  (add-hook 'kill-buffer-hook
            #'fragment-cleanup-buffer nil t))

(defun fragment-cleanup-hooks ()
  "Clean up hooks when mode is disabled."
  (remove-hook 'window-size-change-functions
               #'fragment-window-resize-handler t)
  (remove-hook 'window-configuration-change-hook
               #'fragment-window-config-change-handler t)
  (remove-hook 'kill-buffer-hook
               #'fragment-cleanup-buffer t))

(defun fragment-cleanup-buffer ()
  "Clean up buffer-local grid resources."
  (dolist (grid fragment--grids-in-buffer)
    (fragment-cleanup-grid grid))
  (setq fragment--grids-in-buffer nil))

(defun fragment-cleanup-grid (grid)
  "Clean up GRID resources."
  ;; Release markers
  (maphash (lambda (_ cell)
             (when (oref cell markers)
               (fragment-release-marker-pair (oref cell markers))
               (oset cell markers nil)))
           (oref grid cells))

  ;; Release grid markers
  (when (oref grid start-marker)
    (set-marker (oref grid start-marker) nil))
  (when (oref grid end-marker)
    (set-marker (oref grid end-marker) nil)))

;;; Grid Positioning and Navigation

(defun fragment-grid-at-point (&optional pos)
  "Get the grid at the current point or POS."
  (let ((pos (or pos (point))))
    (catch 'found
      (dolist (grid fragment--grids-in-buffer)
        (when (and (oref grid start-marker)
                   (oref grid end-marker)
                   (>= pos (marker-position (oref grid start-marker)))
                   (<= pos (marker-position (oref grid end-marker))))
          (throw 'found grid))))))

(defun fragment-cell-at-point (&optional pos)
  "Get the cell at the current point or POS."
  (let ((pos (or pos (point)))
        (grid (fragment-grid-at-point pos)))
    (when grid
      (catch 'found
        (maphash (lambda (_ cell)
                   (when (oref cell markers)
                     (let ((start (marker-position
                                  (fragment-marker-pair-start
                                   (oref cell markers))))
                           (end (marker-position
                                (fragment-marker-pair-end
                                 (oref cell markers)))))
                       (when (and start end (>= pos start) (<= pos end))
                         (throw 'found cell)))))
                 (oref grid cells))))))

;;; Interactive Commands

(defvar fragment-grid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g n") #'fragment-create-grid)
    (define-key map (kbd "C-c C-g r") #'fragment-resize-current-grid)
    (define-key map (kbd "C-c C-g c") #'fragment-edit-cell-content)
    (define-key map (kbd "C-c C-g i") #'fragment-grid-info)
    map)
  "Keymap for fragment mode.")

;;;###autoload
(defun fragment-resize-current-grid ()
  "Manually trigger resize for the current grid."
  (interactive)
  (let ((grid (fragment-grid-at-point)))
    (if grid
        (progn
          (fragment-grid-handle-window-resize grid)
          (message "Grid resized"))
      (message "No grid at point"))))

;;;###autoload
(defun fragment-edit-cell-content ()
  "Edit content of cell at point."
  (interactive)
  (let ((cell (fragment-cell-at-point)))
    (if cell
        (let ((new-content (read-string "Cell content: "
                                       (oref cell content))))
          (fragment-cell-set-content cell new-content)
          (fragment-grid-render (oref cell grid))
          (message "Cell content updated"))
      (message "No cell at point"))))

;;;###autoload
(defun fragment-grid-info ()
  "Show information about the grid at point"
  (interactive)
  (let ((grid (fragment-grid-at-point)))
    (if grid
        (message "Grid: %dx%d, Gap: %d, ID: %s"
                 (oref grid rows)
                 (oref grid columns)
                 (oref grid gap)
                 (oref grid id))
      (message "No grid at point"))))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode fragment-grid-mode
  "Minor mode for fragment GRID layout support."
  :lighter " Grid"
  :keymap fragment-grid-mode-map
  :group 'fragment

  (if fragment-grid-mode
      (progn
        ;; Enable mode
        (fragment-setup-hooks)
        (setq fragment--last-window-config (current-window-configuration))
        (setq fragment--last-frame-size (cons (window-body-width) (window-body-height)))
        (make-local-variable 'fragment--grids-in-buffer)
        (message "Fragment Grid mode enabled"))

    ;; Disable mode
    (fragment-cleanup-hooks)
    (when fragment--resize-timer
      (cancel-timer fragment--resize-timer)
      (setq fragment--resize-timer nil))
    (message "Fragment Grid mode disabled")))

;;; Enhanced Grid Creation for Mode

(defun fragment-create-grid-in-mode (rows cols)
  "Create a grid and register it with the current buffer."
  (let ((grid (fragment-grid-create rows cols)))
    (oset grid buffer (current-buffer))
    (fragment-register-grid grid)
    grid))

;; Override the main creation function when mode is active
(advice-add 'fragment-create-grid :around
            (lambda (orig-fun rows cols)
              (if fragment-grid-mode
                  (fragment-create-grid-in-mode rows cols)
                (funcall orig-fun rows cols))))

(provide 'fragment-mode)

;;; fragment-mode.el ends here
