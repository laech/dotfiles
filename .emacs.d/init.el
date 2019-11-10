;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp); -*-

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq-default
 default-frame-alist
 '((internal-border-width . 0)
   (font . "Monospace-12")))

;; By default Emacs automatically detects background color and sets
;; background mode automaticall, but when running inside tmux this
;; detection fails (no support for TERM set to screen, screen-256color
;; etc), so force it to be light (or black to match terminal theme).
(if (string-prefix-p "screen-" (getenv "TERM"))
    (setq-default frame-background-mode 'light))

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
 '(auto-save-visited-mode t)
 '(avy-keys-alist (quote ((avy-goto-char 97 115 100 102 106 107 108))))
 '(blink-cursor-mode nil)
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(global-auto-revert-mode t)
 '(help-window-select t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(line-spacing 0.3)
 '(make-backup-files nil)
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
     ("melpa-unstable" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (helm-lsp yasnippet flycheck-rust rust-mode company-lsp diminish tide flycheck htmlize neotree company-restclient restclient swiper expand-region avy smartparens company-flx yaml-mode helm projectile flx counsel ivy which-key undo-tree rainbow-delimiters paredit multiple-cursors magit intero hindent diff-hl)))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-margin 1)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-cursor nil)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(word-wrap t))

(setq-default
 mode-line-percent-position nil
 mode-line-mule-info nil
 mode-line-remote nil
 mode-line-frame-identification nil
 mode-line-front-space nil
 mode-line-end-spaces nil
 mode-line-modified
 '(:eval
   (cond ((buffer-modified-p)
          (propertize " * " 'face 'mode-line-modified-face))
         (t "   "))))

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
    (forward-line -1)))

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

(defun my/find-file ()
  (interactive)
  (if (string= nil (projectile-project-root))
      (counsel-find-file)
    (projectile-find-file)))

(defconst initial-mode-line-format mode-line-format)
(defun toggle-mode-line ()
  (interactive)
  (if mode-line-format
      (progn
        (setq-default mode-line-format nil)
        (set-frame-parameter nil 'bottom-divider-width 1))
    (setq-default mode-line-format initial-mode-line-format)
    (set-frame-parameter nil 'bottom-divider-width 0)))

(add-hook 'after-init-hook (lambda () (require 'diminish)))

(add-hook 'after-init-hook #'global-eldoc-mode)
(with-eval-after-load 'eldoc
  (setq-default eldoc-echo-area-use-multiline-p t)
  (with-eval-after-load 'diminish
    (diminish 'eldoc-mode)))

(add-hook 'after-init-hook #'global-undo-tree-mode)
(with-eval-after-load 'undo-tree
  (with-eval-after-load 'diminish
    (diminish 'undo-tree-mode)))

(add-hook 'after-init-hook #'which-key-mode)
(with-eval-after-load 'which-key
  (with-eval-after-load 'diminish
    (diminish 'which-key-mode)))

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "C-j") nil))

(add-hook 'after-init-hook #'global-diff-hl-mode)
(add-hook 'after-init-hook #'diff-hl-flydiff-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(add-hook 'prog-mode-hook #'toggle-truncate-lines)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(defun my-save-all-buffers (&rest ignored)
  "Saves all buffers.
This functions ignores any argument passed to it, allowing it to
be used as a function advice via `advice-add'."
  (save-some-buffers t))
(add-hook 'focus-out-hook 'my-save-all-buffers)

(define-key ctl-x-map (kbd "g") 'magit-status)
(define-key ctl-x-map (kbd "C-l") 'downcase-dwim)
(define-key ctl-x-map (kbd "C-u") 'upcase-dwim)

(global-set-key [remap kill-region] 'kill-region-or-line)
(global-set-key [remap kill-ring-save] 'copy-region-or-line)
(global-set-key [remap indent-region] 'indent-region-or-buffer)

(dolist
    (mapping
     '(("<S-RET>" . start-new-line)
       ("<S-return>" . start-new-line)
       ("C-r" . avy-goto-char)
       ("C-o" . my/find-file)
       ("C-S-o" . find-file)
       ("C-M-o" . xref-find-apropos)
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

(with-eval-after-load 'isearch
  (with-eval-after-load 'diminish
    (diminish 'isearch-mode)))

(with-eval-after-load 'expand-region
  (setq
   expand-region-fast-keys-enabled nil
   expand-region-smart-cursor t))

(with-eval-after-load 'lsp-mode
  (setq-default lsp-prefer-flymake nil)
  (require 'company-lsp)
  (push 'company-lsp company-backends)
  (mapc
   (lambda (arg) (apply 'define-key lsp-mode-map arg))
   `(([remap xref-find-definitions] lsp-find-definition)
     ([remap xref-find-references] lsp-find-references)
     ([remap xref-find-apropos] helm-lsp-workspace-symbol)
     (,(kbd "M-'") lsp-execute-code-action)
     (,(kbd "C-\\") lsp-organize-imports)
     (,(kbd "C-h i") lsp-describe-thing-at-point)
     (,(kbd "C-c r r") lsp-rename)
     (,(kbd "M-g . i") lsp-goto-implementation)
     (,(kbd "M-g . t") lsp-goto-type-definition))))

(add-hook 'after-init-hook #'counsel-mode)
(with-eval-after-load 'counsel
  (with-eval-after-load 'diminish
    (diminish 'counsel-mode)))

(with-eval-after-load 'swiper
  (with-eval-after-load 'isearch
    (global-set-key [remap isearch-forward-regexp] 'swiper)
    (define-key isearch-mode-map (kbd "C-M-s") 'swiper-from-isearch)))

(add-hook 'after-init-hook #'ivy-mode)
(with-eval-after-load 'ivy
  (with-eval-after-load 'diminish
    (diminish 'ivy-mode))
  (setq-default
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
         ("<C-RET>" . ivy-immediate-done)
         ("<RET>" . ivy-alt-done)))
    (define-key ivy-minibuffer-map (kbd (car mapping)) (cdr mapping))))

(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'after-init-hook #'global-company-mode)
(with-eval-after-load 'company
  (with-eval-after-load 'diminish
    (diminish 'company-mode))
  (setq-default
   company-idle-delay 0.5
   company-tooltip-idle-delay 0)
  (company-flx-mode)
  (add-to-list 'company-backends 'company-dabbrev)
  (define-key company-mode-map [remap completion-at-point] #'company-complete)
  (define-key company-mode-map [remap complete-symbol] #'company-complete)
  (mapc
   (lambda (arg) (apply 'define-key company-active-map arg))
   `(([remap next-line] company-select-next)
     ([remap previous-line] company-select-previous)
     (,(kbd "M-n") nil)
     (,(kbd "M-p") nil))))

(add-hook 'after-init-hook #'projectile-mode)
(with-eval-after-load 'projectile
  (with-eval-after-load 'diminish
    (diminish 'projectile-mode))
  (advice-add 'projectile-run-project :before 'my-save-all-buffers)
  (advice-add 'projectile-test-project :before 'my-save-all-buffers)
  (advice-add 'projectile-compile-project :before 'my-save-all-buffers)
  (define-key mode-specific-map (kbd "p") 'projectile-command-map)
  (define-key search-map (kbd "f") 'projectile-grep))

(with-eval-after-load 'smartparens
  (require 'smartparens-config))

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(with-eval-after-load 'paredit
  (with-eval-after-load 'diminish
    (diminish 'paredit-mode))

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

(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook #'intero-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode)
  (add-hook 'haskell-mode-hook #'smartparens-mode)

  (with-eval-after-load 'intero
    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))

  (with-eval-after-load 'hindent
    (defun hindent-reformat-region-or-buffer ()
      "Reformat region, or buffer if no region."
      (interactive)
      (if (region-active-p)
          (hindent-reformat-region (region-beginning) (region-end))
        (hindent-reformat-buffer)))

    (define-key hindent-mode-map [remap indent-region]
      #'hindent-reformat-region-or-buffer)))

(with-eval-after-load 'yasnippet
  (with-eval-after-load 'diminish
    (diminish 'yas-minor-mode)))

(with-eval-after-load 'rust-mode
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files "Cargo.toml"))

  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook
   'rust-mode-hook
   (lambda ()
     (yas-minor-mode t)
     (electric-pair-mode t)
     (lsp-deferred)))

  (define-key rust-mode-map [remap indent-region]
    #'rust-format-buffer))

(with-eval-after-load 'typescript-mode
  (setq-default typescript-indent-level 2)
  (add-hook
   'typescript-mode-hook
   (lambda ()
     (tide-setup)
     (tide-hl-identifier-mode t))))

(with-eval-after-load 'tide
  (with-eval-after-load 'diminish
    (diminish 'tide-mode))
  (setq-default
   tide-completion-ignore-case t
   tide-format-options '(:indentSize 2 :tabSize 2))
  (mapc
   (lambda (arg) (apply 'define-key tide-mode-map arg))
   `(([remap indent-region] tide-format)
     ([remap toggle-input-method] tide-organize-imports)
     ([remap xref-find-references] tide-references)
     (,(kbd "M-RET RET") tide-fix)
     (,(kbd "<f2>") tide-rename-symbol)
     (,(kbd "C-<f2>") tide-rename-file)
     (,(kbd "C-c r") tide-refactor)
     (,(kbd "C-h i") tide-documentation-at-point))))

(with-eval-after-load 'flyspell
  (setq-default flyspell-issue-message-flag nil))

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
  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (setq-local center-layout-apply-right-margin t)
     (setq-local truncate-lines nil)))
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
  (setq-default
   neo-window-fixed-size nil
   neo-autorefresh nil
   neo-show-hidden-files t
   neo-theme 'ascii)
  (if (display-graphic-p)
      (setq-default neo-mode-line-type 'none)))

(unless (display-graphic-p)
  (xterm-mouse-mode)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3))))

(if (display-graphic-p)
    (toggle-mode-line))

(load "~/.emacs.d/blog.el")
(load "~/.emacs.d/center-layout/center-layout.el")
(center-layout-mode t)
