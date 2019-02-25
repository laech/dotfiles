
(setq
 default-frame-alist
 `((internal-border-width . 0)
   (menu-bar-lines . ,(if (and (equal system-type 'darwin)
                               (display-graphic-p))
                          1 0))
   (ns-appearance . light)
   (ns-transparent-titlebar . t)))

;; By default Emacs automatically detects background color and sets
;; background mode automaticall, but when running inside tmux this
;; detection fails (no support for TERM set to screen, screen-256color
;; etc), so force it to be light (or black to match terminal theme).
(if (string-prefix-p "screen-" (getenv "TERM"))
    (setq frame-background-mode 'light))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t nil))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(company-tooltip-idle-delay 0)
 '(delete-selection-mode t)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(mode-require-final-newline nil)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(mouse-wheel-tilt-scroll t)
 '(olivetti-body-width 100)
 '(olivetti-minimum-body-width 100)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("marmalade" . "https://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
    (olivetti projectile flx counsel ivy which-key undo-tree rainbow-delimiters paredit multiple-cursors magit intero hindent haskell-snippets expand-region exec-path-from-shell diff-hl)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-margin 1)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-cursor nil))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))

(put 'save-column 'lisp-indent-function 0)

(defun start-new-line ()
  (interactive "*")
  (end-of-line)
  (newline-and-indent))

(defun move-line-up ()
  (interactive "*")
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive "*")
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(defun transpose-words-backward ()
  (interactive "*")
  (transpose-words -1))

(defun copy-region-or-line ()
  "Copy region, or line if no region."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (message "Copied line")
    (copy-region-as-kill
     (line-beginning-position)
     (line-beginning-position 2))))

(defun kill-region-or-line ()
  "Kill region, or line if no region."
  (interactive "*")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    ;; C-S-backspace
    (kill-whole-line)))

(defun duplicate-region (n)
  "Duplicate region and activate new region with point."
  (interactive "*p")
  (when (use-region-p)
    (let ((backward (= (point) (region-beginning)))
          (text (buffer-substring (region-beginning) (region-end))))
      (dotimes (i n) (insert text))
      (push-mark (funcall (if backward '+ '-) (point) (* n (length text))))
      (setq deactivate-mark nil))))

(defun duplicate-line (n)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  (save-column
    (kill-whole-line)
    (yank)
    (dotimes (i n) (yank))
    (previous-line)))

(defun duplicate-region-or-line (n)
  "Duplicate region, otherwise duplicate line if no region is active."
  (interactive "*p")
  (if (use-region-p)
      (duplicate-region n)
    (duplicate-line n)))

(defun indent-region-or-buffer ()
  "Indent region, or buffer if no region."
  (interactive "*")
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (indent-region (point-min) (point-max))))

;; Like join-line (M-^) but goes the other direction.
(defun join-line-next ()
  "Joins the next line to this line and fix up whitespace at join."
  (interactive "*")
  (join-line -1))

(defconst initial-mode-line-format mode-line-format)
(defun toggle-mode-line ()
  (interactive)
  (if mode-line-format
      (progn
        (setq-default mode-line-format nil)
        (set-frame-parameter nil 'bottom-divider-width 1))
    (setq-default mode-line-format initial-mode-line-format)
    (set-frame-parameter nil 'bottom-divider-width 0)))

(add-hook 'after-init-hook #'global-undo-tree-mode)
(add-hook 'after-init-hook #'ivy-mode)
(add-hook 'after-init-hook #'counsel-mode)
(add-hook 'after-init-hook #'projectile-mode)
(add-hook 'after-init-hook #'which-key-mode)

(add-hook 'after-init-hook #'global-diff-hl-mode)
(add-hook 'after-init-hook #'diff-hl-flydiff-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(dolist
    (mapping
     '(("C-M-\\" . indent-region-or-buffer)))
  (global-set-key (kbd (car mapping)) (cdr mapping)))

(define-key ctl-x-map (kbd "g") 'magit-status)

(defvar my-keys-mode-map (make-sparse-keymap))
(dolist
    (mapping
     '(("<S-return>" . start-new-line)
       ("C-o" . counsel-find-file)
       ("C-S-o" . projectile-find-file)
       ("C-j" . ivy-switch-buffer)
       ("C-w" . kill-region-or-line)
       ("M-w" . copy-region-or-line)
       ("C-S-s" . save-buffer)
       ("C-S-w" . delete-other-windows)
       ("C-S-d" . duplicate-region-or-line)
       ("M-S-n" . move-line-down)
       ("M-S-p" . move-line-up)
       ("M-S-j" . join-line-next)
       ("M-S-t" . transpose-words-backward)
       ("C-." . mc/mark-next-like-this-word)
       ("C->" . mc/unmark-next-like-this)
       ("C-," . mc/mark-previous-like-this)
       ("C-<" . mc/unmark-previous-like-this)
       ("C-=" . er/expand-region)
       ("C-+" . er/contract-region)
       ("C-:" . counsel-M-x)))
  (define-key my-keys-mode-map (kbd (car mapping)) (cdr mapping)))

(require 'iso-transl)
(define-key my-keys-mode-map (kbd "C-;") ctl-x-map)
(define-key key-translation-map (kbd "C-; 8") 'iso-transl-ctl-x-8-map)

(define-minor-mode my-keys-mode :init-value t)

(defun my-keys-mode-disable ()
  (my-keys-mode 0))

(defun my-keys-have-priority (_)
  "Ensures my keys retain priority over other minor modes."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-mode)
    (let ((my-keys (assq 'my-keys-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist my-keys))))

(add-hook 'minibuffer-setup-hook 'my-keys-mode-disable)
(add-hook 'after-load-functions 'my-keys-have-priority)
(my-keys-mode 1)

(define-globalized-minor-mode global-olivetti-mode olivetti-mode turn-on-olivetti-mode)
(global-olivetti-mode 1)

(with-eval-after-load 'xterm
  (dolist (key (number-sequence 32 127))
    (dolist (mod '((6 . "C-S") (8 . "C-M-S")))
      (let* ((mod-code (car mod))
             (mod-str (cdr mod))
             (full (kbd (format "%s-%c" mod-str key))))

        (define-key xterm-function-map
          (format "\e[27;%d;%d~" mod-code key)
          full)

        (define-key xterm-function-map
          (format "\e[%d;%du" key mod-code)
          full)))))

(with-eval-after-load 'ivy
  (dolist
      (mapping
       '(("<tab>" . ivy-insert-current)
         ("<return>" . ivy-alt-done)))
    (define-key ivy-minibuffer-map (kbd (car mapping)) (cdr mapping))))

(setq ivy-extra-directories nil)
(setq ivy-use-virtual-buffers t)
(setq ivy-initial-inputs-alist nil)
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))

(setq projectile-completion-system 'ivy)
(setq magit-completing-read-function 'ivy-completing-read)

(with-eval-after-load 'projectile
  (define-key mode-specific-map (kbd "p") 'projectile-command-map))

(with-eval-after-load 'paredit
  (defun transpose-sexps-reverse (arg)
    (interactive "*p")
    (transpose-sexps (- arg)))
  (dolist
      (mapping
       '(("C-M-S-t" . transpose-sexps-reverse)))
    (define-key paredit-mode-map (kbd (car mapping)) (cdr mapping))))

(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook #'intero-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode)

  (defun hindent-reformat-region-or-buffer ()
    "Reformat region, or buffer if no region."
    (interactive)
    (if (region-active-p)
        (hindent-reformat-region (region-beginning) (region-end))
      (hindent-reformat-buffer)))

  (define-key haskell-mode-map (kbd "C-M-\\")
    'hindent-reformat-region-or-buffer)

  (with-eval-after-load 'hindent-mode
    (define-key hindent-mode-map (kbd "C-M-\\")
      'hindent-reformat-region-or-buffer))

  (with-eval-after-load 'speedbar (speedbar-add-supported-extension ".hs"))
  (with-eval-after-load 'intero
    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))))

(with-eval-after-load 'speedbar
  (setq speedbar-use-images nil)
  (defun speedbar-set-mode-line-format ()
    (setq mode-line-format nil)))

(with-eval-after-load 'flyspell
  (setq flyspell-issue-message-flag nil))

(unless (display-graphic-p)
  (xterm-mouse-mode)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3))))

(if (display-graphic-p)
    (toggle-mode-line))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'super)
  (set-default-font "Menlo 14")
  (exec-path-from-shell-initialize))

(when (eq system-type 'gnu/linux)
  (set-default-font "Ubuntu Mono 12"))
