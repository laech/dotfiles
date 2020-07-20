;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp); -*-

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq-default
 default-frame-alist
 '((internal-border-width . 0)
   (font . "Cascadia Code-12")))

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
 '(line-spacing 0.2)
 '(make-backup-files nil)
 '(mode-require-final-newline nil)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(mouse-wheel-tilt-scroll t)
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-margin 1)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(visible-cursor nil)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(word-wrap t))

(defmacro my-add-hook-if-defined (hook func)
  `(add-hook
    (quote ,hook)
    (lambda ()
      (when (functionp (quote ,func)) (,func)))))

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
  (let* ((use-region (use-region-p))
         (start (if use-region (region-beginning) (point-min)))
         (end (if use-region (region-end) (point-max))))
    (indent-region start end)
    (delete-trailing-whitespace start end)))

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

(my-add-hook-if-defined after-init-hook center-layout-mode)

(with-eval-after-load 'eldoc
  (setq-default eldoc-echo-area-use-multiline-p t))

(my-add-hook-if-defined after-init-hook global-undo-tree-mode)
(with-eval-after-load 'undo-tree
  (define-key undo-tree-map (kbd "C-S-z") 'undo-tree-redo)
  (define-key undo-tree-map (kbd "M-Z") 'undo-tree-redo)
  (define-key undo-tree-map (kbd "<redo>") 'undo-tree-redo))

(my-add-hook-if-defined after-init-hook which-key-mode)

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "C-j") nil))

(my-add-hook-if-defined after-init-hook global-diff-hl-mode)
(my-add-hook-if-defined after-init-hook diff-hl-flydiff-mode)
(with-eval-after-load 'diff-hl
  (unless (display-graphic-p) (diff-hl-margin-mode)))

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(my-add-hook-if-defined prog-mode-hook toggle-truncate-lines)
(my-add-hook-if-defined prog-mode-hook rainbow-delimiters-mode)

(defun my-save-all-buffers (&rest ignored)
  "Saves all buffers.
This functions ignores any argument passed to it, allowing it to
be used as a function advice via `advice-add'."
  (save-some-buffers t))
(add-hook 'focus-out-hook 'my-save-all-buffers)

(defun my-buffer-file-name ()
  (interactive)
  (message buffer-file-name))

(define-key ctl-x-map (kbd "g") 'magit-status)
(define-key ctl-x-map (kbd "G") 'magit-list-repositories)
(define-key ctl-x-map (kbd "C-l") 'downcase-dwim)
(define-key ctl-x-map (kbd "C-u") 'upcase-dwim)

(global-set-key [remap kill-region] 'kill-region-or-line)
(global-set-key [remap kill-ring-save] 'copy-region-or-line)
(global-set-key [remap indent-region] 'indent-region-or-buffer)

(dolist
    (mapping
     '(("<f3>" . isearch-forward)
       ("<S-f3>" . isearch-backward)
       ("<f6>" . my-buffer-file-name)
       ("<S-RET>" . start-new-line)
       ("<S-return>" . start-new-line)
       ("C-c" . kill-ring-save)
       ("C-S-c" . kill-ring-save)
       ("C-M-c" . mode-specific-command-prefix)
       ("C-\\" . indent-region)
       ("C-v" . yank)
       ("C-S-v" . yank-pop)
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
       ("M-n" . scroll-up-command)
       ("M-p" . scroll-down-command)
       ("M-N" . move-line-down)
       ("M-P" . move-line-up)
       ("M-V" . join-line-next)
       ("M-T" . transpose-words-backward)))
  (global-set-key (kbd (car mapping)) (cdr mapping)))

(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "C-M-c")
    (lookup-key help-mode-map (kbd "C-c")))
  (define-key help-mode-map (kbd "C-c") nil))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<S-f3>") 'isearch-repeat-backward))

(with-eval-after-load 'expand-region
  (setq
   expand-region-fast-keys-enabled nil
   expand-region-smart-cursor t))

(my-add-hook-if-defined after-init-hook counsel-mode)

(with-eval-after-load 'swiper
  (with-eval-after-load 'isearch
    (global-set-key [remap isearch-forward-regexp] 'swiper)
    (define-key isearch-mode-map (kbd "C-M-s") 'swiper-from-isearch)))

(my-add-hook-if-defined after-init-hook ivy-mode)
(with-eval-after-load 'ivy
  (setq-default
   ivy-extra-directories nil
   ivy-use-virtual-buffers t
   ivy-initial-inputs-alist nil
   ivy-re-builders-alist '((swiper . ivy--regex-plus) (t . ivy--regex-fuzzy))
   projectile-completion-system 'ivy
   magit-completing-read-function 'ivy-completing-read)

  (define-key ivy-minibuffer-map (kbd "C-M-c")
    (lookup-key ivy-minibuffer-map (kbd "C-c")))
  (define-key ivy-minibuffer-map (kbd "C-c") nil)

  (dolist
      (mapping
       '(("<tab>" . ivy-insert-current)
         ("C-w" . ivy-yank-word)
         ("C-r" . ivy-previous-line-or-history)
         ("C-j" . nil)
         ("<C-RET>" . ivy-immediate-done)
         ("<RET>" . ivy-alt-done)))
    (define-key ivy-minibuffer-map (kbd (car mapping)) (cdr mapping))))

(my-add-hook-if-defined after-init-hook global-flycheck-mode)
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-M-c")
    (lookup-key flycheck-mode-map (kbd "C-c")))
  (define-key flycheck-mode-map (kbd "C-c") nil))

(my-add-hook-if-defined after-init-hook global-company-mode)
(with-eval-after-load 'company
  (setq-default
   company-idle-delay 0
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

(my-add-hook-if-defined after-init-hook projectile-mode)
(with-eval-after-load 'projectile
  (advice-add 'projectile-run-project :before 'my-save-all-buffers)
  (advice-add 'projectile-test-project :before 'my-save-all-buffers)
  (advice-add 'projectile-compile-project :before 'my-save-all-buffers)
  (global-set-key (kbd "C-M-p") 'projectile-command-map)
  (define-key search-map (kbd "f") 'projectile-grep))

(my-add-hook-if-defined emacs-lisp-mode-hook enable-paredit-mode)
(my-add-hook-if-defined eval-expression-minibuffer-setup-hook enable-paredit-mode)
(my-add-hook-if-defined ielm-mode-hook enable-paredit-mode)
(my-add-hook-if-defined lisp-interaction-mode-hook enable-paredit-mode)
(my-add-hook-if-defined lisp-mode-hook enable-paredit-mode)
(my-add-hook-if-defined scheme-mode-hook enable-paredit-mode)
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

  (define-key paredit-mode-map (kbd "C-M-c")
    (lookup-key paredit-mode-map (kbd "C-c")))
  (define-key paredit-mode-map (kbd "C-c") nil)


  (mapc
   (lambda (arg) (apply 'define-key paredit-mode-map arg))
   `(([remap kill-region] paredit-cut-region-or-sexp)
     ([remap kill-ring-save] paredit-copy-region-or-sexp)
     ([remap kill-line] paredit-kill)
     (,(kbd "C-M-c M-s") paredit-splice-sexp)
     (,(kbd "C-M-c M-S") paredit-split-sexp)
     (,(kbd "C-M-S-t") transpose-sexps-reverse)
     (,(kbd "M-s") nil)
     (,(kbd "M-S") nil)
     (,(kbd "C-M-p") nil)
     (,(kbd "C-j") nil))))

(with-eval-after-load 'js
  (setq-default js-indent-level 2))

(with-eval-after-load 'flyspell
  (setq-default flyspell-issue-message-flag nil))

(with-eval-after-load 'with-editor
  (define-key with-editor-mode-map (kbd "C-M-c")
    (lookup-key with-editor-mode-map (kbd "C-c")))
  (define-key with-editor-mode-map (kbd "C-c") nil))

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-c")
    (lookup-key magit-status-mode-map (kbd "C-c")))
  (define-key magit-status-mode-map (kbd "C-c") nil))

(with-eval-after-load 'magit-diff
  (define-key magit-hunk-section-map (kbd "C-j") nil)
  (mapc
   (lambda (mapping)
     (define-key magit-file-section-map (kbd (car mapping)) (cdr mapping)))
   '(("C-j" . nil))))

(with-eval-after-load 'diff-hl
  (define-key diff-hl-mode-map (kbd "M-{") 'diff-hl-previous-hunk)
  (define-key diff-hl-mode-map (kbd "M-}") 'diff-hl-next-hunk))

(with-eval-after-load 'elisp-mode
  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (setq center-layout-apply-right-margin t)
     (setq truncate-lines nil)))
  (define-key lisp-interaction-mode-map (kbd "C-j") nil))

(with-eval-after-load 'markdown-mode
  (add-hook
   'markdown-mode-hook
   (lambda ()
     (setq center-layout-apply-right-margin t))))

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

(with-eval-after-load 'sh-script
  (setq sh-basic-offset 2)
  (require 'reformatter)
  (reformatter-define shfmt
    :program "~/.local/bin/shfmt"
    :args '("-i" "2" "-ci")
    :lighter " shfmt")

  (defun shfmt-region-or-buffer ()
    (interactive "*")
    (if (region-active-p)
        (shfmt-region (region-beginning) (region-end))
      (shfmt-buffer)))

  (define-key
    sh-mode-map
    [remap indent-region]
    'shfmt-region-or-buffer))

(unless (display-graphic-p)
  (xterm-mouse-mode)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3))))

(if (display-graphic-p)
    (toggle-mode-line))

(load
 (expand-file-name
  "straight/repos/straight.el/bootstrap.el"
  user-emacs-directory))

(let
    ((packages
      '(
        avy
        company-flx
        counsel
        diff-hl
        dockerfile-mode
        expand-region
        flx
        flycheck
        ivy
        magit
        multiple-cursors
        neotree
        paredit
        projectile
        rainbow-delimiters
        swiper
        undo-tree
        which-key
        yaml-mode)))
  (dolist (package packages)
    (straight-use-package package)))

(straight-use-package
 '(center-layout
   :type git
   :host gitlab
   :repo "lae/emacs-center-layout"))

(straight-use-package
 '(reformatter
   :type git
   :host github
   :repo "purcell/reformatter.el"))
