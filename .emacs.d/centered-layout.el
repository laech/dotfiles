;;; centered-layout.el --- Centers windows.

;;; Commentary:
;;
;; # Usage
;;
;; Use `M-x centered-layout-mode` to turn it on and off.
;;
;; # Customization
;;
;; `centered-layout-columns`: desired content width, default is 80.
;;
;; `centered-layout-apply-right-margin`: t to apply right margin, by
;; default right margin will be set to 0 to allow text to flow to far
;; right.
;;

;;; Code:

(defgroup centered-layout nil
  "Centers windows."
  :group 'emacs)

(defcustom centered-layout-columns 80
  "Visible number of character columns when centered."
  :group 'centered-layout
  :type 'number)

(defcustom centered-layout-apply-right-margin nil
  "If t apply right margin with same width as left margin.
Otherwise set right margin to 0, allowing text to flow to far right."
  :group 'centered-layout
  :type 'boolean)

(defun centered-layout--which-key-window-p (window)
  "Return t if WINDOW is a which-key window."
  (and (boundp 'which-key-buffer-name)
       (string= which-key-buffer-name (buffer-name (window-buffer window)))))

(defun centered-layout--lv-window-p (window)
  "Return t if WINDOW is a lv (used by hydra) window."
  (and (boundp 'lv-wnd)
       (eq window lv-wnd)))

(defun centered-layout--window-body-preferred-pixels (window)
  "Return preferred body width for WINDOW.
Usually `centered-layout-columns', but for dedicated
windows (`window-dedicated-p') like which-key, lv (hydra),
neotree, treemacs etc, return their content width if it is
greater, as typically they are used as dedicate bottom/side
windows and have their own layout, limiting their width will
cause undesired line wrapping/truncation."
  (let ((pixels (* centered-layout-columns (window-font-width window))))
    (if (window-dedicated-p window)
        (max
         pixels
         (car (window-text-pixel-size
               window
               nil
               nil
               (- (window-pixel-width window)
                  (window-scroll-bar-width window)
                  (window-right-divider-width window)
                  (frame-fringe-width (window-frame window))))))
      pixels)))

(defun centered-layout--free-columns (window extra-free-pixels)
  "Computes available columns for layout use for WINDOW.
Adds EXTRA-FREE-PIXELS into the calculation."
  (max
   (ceiling
    (- (+ (window-pixel-width window) extra-free-pixels)
       (window-scroll-bar-width window)
       (window-right-divider-width window)
       (frame-fringe-width (window-frame window))
       (centered-layout--window-body-preferred-pixels window))
    (frame-char-width (window-frame window)))
   0))

(defun centered-layout--compute-margins (window)
  "Compute (LEFT-MARGIN . RIGHT_MARGIN) for WINDOW."
  (let* (
         ;; `window-left-column' is needed to ensure `window-prev-sibling'
         ;; will return a window to the left, otherwise a window above
         ;; can also be returned.
         (dediated-window
          (if (/= 0 (window-left-column window)) (window-prev-sibling window) nil))

         (dediated-window-pixels
          (if dediated-window (window-pixel-width dediated-window) 0))

         (dediated-window-columns
          (if dediated-window (window-total-width dediated-window 'ceiling) 0))

         (margins (centered-layout--free-columns window dediated-window-pixels))
         (margin-left (- (ceiling margins 2) dediated-window-columns))
         (margin-right
          (if centered-layout-apply-right-margin
              (- margins dediated-window-pixels margin-left)
            0)))

    `(,(max 0 margin-left) .
      ,(max 0 margin-right))))

(defun centered-layout--update-window (window)
  "Update margins for WINDOW."
  (let ((margins (centered-layout--compute-margins window)))
    (set-window-margins window (car margins) (cdr margins))))

(defun centered-layout--update-frame (&optional frame)
  "Update margins for FRAME, if nil, update selected frames."
  (walk-windows
   (lambda (window) (centered-layout--update-window window))
   t
   (if frame frame (selected-frame))))

(define-minor-mode centered-layout-mode
  "Toggle centered layout mode."
  :global t
  (cond
   (centered-layout-mode
    (add-hook 'window-size-change-functions 'centered-layout--update-frame)
    (add-hook 'window-configuration-change-hook 'centered-layout--update-frame)
    (add-hook 'text-scale-mode-hook 'centered-layout--update-frame)
    (centered-layout--update-frame))
   (t
    (remove-hook 'window-size-change-functions 'centered-layout--update-frame)
    (remove-hook 'window-configuration-change-hook 'centered-layout--update-frame)
    (remove-hook 'text-scale-mode-hook 'centered-layout--update-frame)
    (walk-windows (lambda (window) (set-window-margins window 0 0)) t))))

(provide 'centered-layout)

;;; centered-layout.el ends here
