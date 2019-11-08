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

(defun centered-layout--window-body-preferred-columns (window)
  "Return preferred body width for WINDOW.
For special windows like which-key, lv, allow them to use more than
`centered-layout-columns' but will still center them."
  (if (or (centered-layout--which-key-window-p window)
          (centered-layout--lv-window-p window))
      (/ (car (window-text-pixel-size
               window
               nil
               nil
               (- (window-pixel-width window)
                  (window-scroll-bar-width window)
                  (frame-fringe-width (window-frame window)))))
         (window-font-width window))
    centered-layout-columns))

(defun centered-layout--update-window (window)
  "Update margins for WINDOW.
If FORCE, force update, otherwise only update if window size has
changed."
  (set-window-margins
   window
   (max 0 (/ (- (window-pixel-width window)
                (window-scroll-bar-width window)
                (frame-fringe-width (window-frame window))
                (* (window-font-width window)
                   (centered-layout--window-body-preferred-columns window)))
             (frame-char-width (window-frame window))
             2))
   nil))

(defun centered-layout--update-frame (&optional frame)
  "Update margins for FRAME, if nil, update all frames.
If FORCE, force update, otherwise only update if window size has
changed."
  (walk-windows
   (lambda (window) (centered-layout--update-window window))
   t
   frame))

(define-minor-mode centered-layout-mode
  "Toggle centered layout mode."
  :init-value nil
  :lighter " Centered-Layout"
  :global t
  (cond
   (centered-layout-mode
    (add-hook 'window-size-change-functions 'centered-layout--update-frame)
    (add-hook 'window-configuration-change-hook 'centered-layout--update-frame)
    (centered-layout--update-frame))
   (t
    (remove-hook 'window-size-change-functions 'centered-layout--update-frame)
    (remove-hook 'window-configuration-change-hook 'centered-layout--update-frame)
    (walk-windows (lambda (window) (set-window-margins window 0 0)) t))))

(provide 'centered-layout)

;;; centered-layout.el ends here
