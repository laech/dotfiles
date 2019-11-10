;;; center-layout.el --- Centers windows.

;;; Commentary:
;;
;; # Usage
;;
;; Use `M-x center-layout-mode` to turn it on and off.
;;
;; # Customization
;;
;; `center-layout-columns`: desired content width, default is 80.
;;
;; `center-layout-apply-right-margin`: t to apply right margin, by
;; default right margin will be set to 0 to allow text to flow to far
;; right.
;;

;;; Code:

(defgroup center-layout nil
  "Centers windows."
  :group 'emacs)

(defcustom center-layout-columns 80
  "Visible number of character columns when centered."
  :group 'center-layout
  :type 'number)

(defcustom center-layout-apply-right-margin nil
  "If t apply right margin with same width as left margin.
Otherwise set right margin to 0, allowing text to flow to far right."
  :group 'center-layout
  :type 'boolean)

(defun center-layout--which-key-window-p (window)
  "Return t if WINDOW is a which-key window."
  (and (boundp 'which-key-buffer-name)
       (string= which-key-buffer-name (buffer-name (window-buffer window)))))

(defun center-layout--lv-window-p (window)
  "Return t if WINDOW is a lv (used by hydra) window."
  (and (boundp 'lv-wnd)
       (eq window lv-wnd)))

(defun center-layout--window-body-preferred-pixels (window)
  "Return preferred body width for WINDOW.
Usually `center-layout-columns', but for dedicated
windows (`window-dedicated-p') like which-key, lv (hydra),
neotree, treemacs etc, return their content width if it is
greater, as typically they are used as dedicate bottom/side
windows and have their own layout, limiting their width will
cause undesired line wrapping/truncation."
  (let ((pixels (* center-layout-columns (window-font-width window))))
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

(defun center-layout--free-columns (window extra-free-pixels)
  "Computes available columns for layout use for WINDOW.
Adds EXTRA-FREE-PIXELS into the calculation."
  (max
   (ceiling
    (- (+ (window-pixel-width window) extra-free-pixels)
       (window-scroll-bar-width window)
       (window-right-divider-width window)
       (frame-fringe-width (window-frame window))
       (center-layout--window-body-preferred-pixels window))
    (frame-char-width (window-frame window)))
   0))

(defun center-layout--compute-margins (window)
  "Compute (LEFT-MARGIN . RIGHT_MARGIN) for WINDOW."
  (let* (
         ;; `window-left-column' is needed to ensure `window-prev-sibling'
         ;; will return a window to the left, otherwise a window above
         ;; can also be returned.
         (dediated-window
          (if (/= 0 (window-left-column window))
              (window-prev-sibling window)
            nil))

         (dediated-window-pixels
          (if dediated-window (window-pixel-width dediated-window) 0))

         (dediated-window-columns
          (if dediated-window (window-total-width dediated-window 'ceiling) 0))

         (margins (center-layout--free-columns window dediated-window-pixels))
         (margin-left (- (ceiling margins 2) dediated-window-columns))
         (margin-right
          (if center-layout-apply-right-margin
              (- margins dediated-window-pixels margin-left)
            0)))

    `(,(max 0 margin-left) .
      ,(max 0 margin-right))))

(defun center-layout--update-window (window)
  "Update margins for WINDOW."
  (let ((margins (center-layout--compute-margins window)))
    (set-window-margins window (car margins) (cdr margins))))

(defun center-layout--update-frame (&optional frame)
  "Update margins for FRAME, if nil, update selected frames."
  (walk-windows
   (lambda (window) (center-layout--update-window window))
   t
   (if frame frame (selected-frame))))

(defun center-layout--advice (&rest ignored)
  "Advice for `advice-add' to update layout.
Arguments from the adviced functions are IGNORED."
  (center-layout--update-frame))

(define-minor-mode center-layout-mode
  "Toggle center layout mode."
  :global t
  (cond
   (center-layout-mode
    (add-hook 'window-size-change-functions 'center-layout--update-frame)
    (add-hook 'window-configuration-change-hook 'center-layout--update-frame)
    (add-hook 'text-scale-mode-hook 'center-layout--update-frame)
    (add-hook 'scroll-bar-mode-hook 'center-layout--update-frame)
    (advice-add 'set-fringe-mode :after 'center-layout--advice)
    (center-layout--update-frame))
   (t
    (remove-hook 'window-size-change-functions 'center-layout--update-frame)
    (remove-hook 'window-configuration-change-hook 'center-layout--update-frame)
    (remove-hook 'text-scale-mode-hook 'center-layout--update-frame)
    (remove-hook 'scroll-bar-mode-hook 'center-layout--update-frame)
    (advice-remove 'set-fringe-mode 'center-layout--advice)
    (walk-windows (lambda (window) (set-window-margins window 0 0)) t))))

(provide 'center-layout)

;;; center-layout.el ends here
