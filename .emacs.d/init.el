(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq
 default-frame-alist
 `((internal-border-width . 0)
   (font . ,(if (eq system-type 'gnu/linux) "Monospace-14" "Hack-10"))))

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
 '(avy-keys-alist (quote ((avy-goto-char 97 115 100 102 106 107 108))))
 '(blink-cursor-mode nil)
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(mode-require-final-newline nil)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(mouse-wheel-tilt-scroll t)
 '(package-archive-priorities
   (quote
    (("gnu" . 1)
     ("marmalade" . 1)
     ("melpa-stable" . 1)
     ("melpa-unstable" . 0))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa-unstable" . "https://melpa.org/packages/")
     ("marmalade" . "https://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
    (neotree company-restclient restclient swiper expand-region avy smartparens company-flx yaml-mode treemacs lsp-ui helm sr-speedbar projectile flx counsel ivy which-key undo-tree rainbow-delimiters paredit multiple-cursors magit intero hindent diff-hl)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-margin 1)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(visible-cursor nil)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(word-wrap t))

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

(defun kill-line-backward ()
  "kill from point to start of line."
  (interactive "*")
  (kill-line 0))

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
(add-hook 'after-init-hook #'which-key-mode)

(add-hook 'after-init-hook #'global-diff-hl-mode)
(add-hook 'after-init-hook #'diff-hl-flydiff-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'sh-mode-hook #'flycheck-mode) ;; Needs shellcheck to be installed

(setq my-save-buffer-in-progress nil)
(defun my-save-buffer ()
  (unless my-save-buffer-in-progress
    (setq my-save-buffer-in-progress t)
    (save-some-buffers t)
    (setq my-save-buffer-in-progress nil)))
(add-hook 'window-configuration-change-hook (lambda () (my-save-buffer)))
(add-hook 'focus-out-hook (lambda () (my-save-buffer)))

(define-key ctl-x-map (kbd "g") 'magit-status)
(define-key ctl-x-map (kbd "C-l") 'downcase-dwim)
(define-key ctl-x-map (kbd "C-u") 'upcase-dwim)

(global-set-key [remap kill-region] 'kill-region-or-line)
(global-set-key [remap kill-ring-save] 'copy-region-or-line)
(global-set-key [remap indent-region] 'indent-region-or-buffer)
(global-set-key [remap dabbrev-expand] 'completion-at-point)
(global-set-key [remap isearch-forward] 'swiper)

(dolist
    (mapping
     '(("<S-return>" . start-new-line)
       ("C-r" . avy-goto-char)
       ("C-o" . find-file)
       ("C-S-o" . projectile-find-file)
       ("C-M-o" . xref-find-definitions)
       ("M-o" . imenu)
       ("M-D" . duplicate-region-or-line)
       ("<C-M-backspace>" . kill-line-backward)
       ("C-j" . switch-to-buffer)
       ("M-i" . mc/mark-previous-like-this)
       ("M-I" . mc/unmark-previous-like-this)
       ("M-j" . mc/mark-next-like-this-word)
       ("M-J" . mc/unmark-next-like-this)
       ("M-h" . er/expand-region)
       ("M-H" . er/contract-region)
       ("M-N" . move-line-down)
       ("M-P" . move-line-up)
       ("M-V" . join-line-next)
       ("M-T" . transpose-words-backward)))
  (global-set-key (kbd (car mapping)) (cdr mapping)))

(with-eval-after-load 'expand-region
  (setq expand-region-fast-keys-enabled nil)
  (setq expand-region-smart-cursor t))

(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(add-hook 'after-init-hook #'ivy-mode)
(add-hook 'after-init-hook #'counsel-mode)
(with-eval-after-load 'ivy
  (setq
   ivy-extra-directories nil
   ivy-use-virtual-buffers t
   ivy-initial-inputs-alist nil
   ivy-re-builders-alist '((swiper . ivy--regex-plus) (t . ivy--regex-fuzzy))
   projectile-completion-system 'ivy
   magit-completing-read-function 'ivy-completing-read)
  (dolist
      (mapping
       '(("<tab>" . ivy-insert-current)
         ("C-w" . ivy-yank-word)
         ("C-r" . ivy-previous-line-or-history)
         ("C-j" . nil)
         ("<C-return>" . ivy-immediate-done)
         ("<return>" . ivy-alt-done)))
    (define-key ivy-minibuffer-map (kbd (car mapping)) (cdr mapping))))

(add-hook 'prog-mode-hook #'company-mode)
(with-eval-after-load 'company
  (setq
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-tooltip-idle-delay 0)
  (company-flx-mode)
  (define-key company-mode-map [remap completion-at-point] #'company-complete)
  (mapc
   (lambda (arg) (apply 'define-key company-active-map arg))
   `(([remap next-line] company-select-next)
     ([remap previous-line] company-select-previous)
     (,(kbd "M-n") nil)
     (,(kbd "M-p") nil))))

(add-hook 'after-init-hook #'projectile-mode)
(with-eval-after-load 'projectile
  (define-key mode-specific-map (kbd "p") 'projectile-command-map)
  (define-key search-map (kbd "f") 'projectile-grep))

(add-hook 'haskell-mode-hook 'smartparens-mode)
(with-eval-after-load 'smartparens
  (require 'smartparens-config))

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(with-eval-after-load 'paredit

  (defun transpose-sexps-reverse (arg)
    (interactive "*p")
    (transpose-sexps (- arg)))

  (defun paredit-cut-region-or-sexp ()
    (interactive "*")
    (if (use-region-p)
        (paredit-kill-region (region-beginning) (region-end))
      (beginning-of-line)
      (paredit-kill)
      (paredit-forward-delete)))

  (defun paredit-copy-region-or-sexp ()
    (interactive)
    (if (use-region-p)
        (copy-region-as-kill (region-beginning) (region-end))
      (message "Copied sexp")
      (paredit-copy-sexps-as-kill)))

  (define-key mode-specific-map (kbd "M-s") 'paredit-splice-sexp)
  (define-key mode-specific-map (kbd "M-S") 'paredit-split-sexp)

  (mapc
   (lambda (arg) (apply 'define-key paredit-mode-map arg))
   `(([remap kill-region] paredit-cut-region-or-sexp)
     ([remap kill-ring-save] paredit-copy-region-or-sexp)
     ([remap kill-line] paredit-kill)
     (,(kbd "C-M-S-t") transpose-sexps-reverse)
     (,(kbd "M-s") nil)
     (,(kbd "M-S") nil)
     (,(kbd "C-j") nil))))

(with-eval-after-load 'hindent

  (defun hindent-reformat-region-or-buffer ()
    "Reformat region, or buffer if no region."
    (interactive)
    (if (region-active-p)
        (hindent-reformat-region (region-beginning) (region-end))
      (hindent-reformat-buffer)))

  (define-key hindent-mode-map [remap indent-region]
    #'hindent-reformat-region-or-buffer))

(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook #'intero-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode)

  (with-eval-after-load 'speedbar (speedbar-add-supported-extension ".hs"))
  (with-eval-after-load 'intero
    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))))

(with-eval-after-load 'speedbar
  (setq
   speedbar-use-images nil
   speedbar-show-unknown-files t)
  (defun speedbar-set-mode-line-format ()
    (setq mode-line-format nil)))

(with-eval-after-load 'sr-speedbar
  (setq sr-speedbar-right-side nil))

(with-eval-after-load 'flyspell
  (setq flyspell-issue-message-flag nil))

(add-hook 'restclient-mode-hook #'company-mode)
(with-eval-after-load 'restclient
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-restclient)))

(with-eval-after-load 'magit-diff
  (define-key magit-hunk-section-map (kbd "C-j") nil)
  (mapc
   (lambda (mapping)
     (define-key magit-file-section-map (kbd (car mapping)) (cdr mapping)))
   '(("C-j" . nil))))

(with-eval-after-load 'elisp-mode
  (define-key lisp-interaction-mode-map (kbd "C-j") nil))

(with-eval-after-load 'org
  (mapc
   (lambda (arg) (apply 'define-key org-mode-map arg))
   `((,(kbd "C-j") nil))))

(defun neotree-project-dir ()
  "Open neotree at project root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (when (and project-dir (neo-global--window-exists-p))
      (neotree-dir project-dir)
      (neotree-find file-name))))

(global-set-key (kbd "M-1") 'neotree-project-dir)
(with-eval-after-load 'neotree
  (setq
   neo-autorefresh nil
   neo-show-hidden-files t
   neo-theme 'ascii))

(unless (display-graphic-p)
  (xterm-mouse-mode)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3))))

(if (display-graphic-p)
    (toggle-mode-line))

(defun auto-center-windows ()

  (defun center-window (window triggered-by-size-change)
    (when (and (not (eq (window-buffer window) which-key--buffer))
               (fboundp 'window-pixel-width-before-size-change))

      (when (or (not triggered-by-size-change)
                (/= (window-pixel-width-before-size-change window)
                    (window-pixel-width window)))

        (let* ((width (+ (window-width window)
                         (or (car (window-margins window)) 0)
                         (or (cdr (window-margins window)) 0)))
               (margin (max 0 (/ (- width 80) 2))))
          (set-window-margins window margin)))))

  (defun center-windows (triggered-by-size-change &optional frame)
    (walk-windows
     (lambda (window)
       (center-window window triggered-by-size-change))
     t
     frame))

  (add-hook
   'window-size-change-functions
   (lambda (frame)
     (center-windows t frame)))

  (add-hook
   'window-configuration-change-hook
   (lambda ()
     (center-windows nil))))

(auto-center-windows)

(load "~/.emacs.d/blog.el")

