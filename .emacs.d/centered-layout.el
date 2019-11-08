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

(defun centered-layout--window-char-pixels (window)
  "Return number of pixel per character for WINDOW."
  (frame-char-width (window-frame window)))

(defun centered-layout--window-fringe-columns (window)
  "Return fringe width in columns for WINDOW."
  (let ((fringes (window-fringes window)))
    (ceiling (+ (car fringes)
                (cadr fringes))
             (centered-layout--window-char-pixels window))))

(defun centered-layout--window-scroll-bar-columns (window)
  "Return scroll bar width in columns for WINDOW."
  (ceiling (window-scroll-bar-width window)
           (centered-layout--window-char-pixels window)))

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
               (* (- (window-total-width window)
                     (centered-layout--window-fringe-columns window)
                     (centered-layout--window-scroll-bar-columns window))
                  (frame-char-width (window-frame window)))))
         (centered-layout--window-char-pixels window)) ;; TODO current char width
    centered-layout-columns))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun centered-layout--update-window (window)
  "Update margins for WINDOW.
If FORCE, force update, otherwise only update if window size has
changed."
  (set-window-margins
   window
   (max 0 (/ (- (window-total-width window)
                (fringe-columns 'left)
                (fringe-columns 'right)
                (scroll-bar-columns 'left)
                (scroll-bar-columns 'right)
                (centered-layout--window-body-preferred-columns window))
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
    (remove-hook 'window-configuration-change-hook 'centered-layout--update-frame))
   (walk-windows (lambda (window) (set-window-margins window 0 0)) t)))

(provide 'centered-layout)

;;; centered-layout.el ends here
