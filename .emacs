(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq
 default-frame-alist
 '((internal-border-width . 0)
   (font . "Monospace-14")))

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
    (expand-region hydra key-chord avy smartparens company-flx yaml-mode treemacs lsp-ui lsp-java helm sr-speedbar projectile flx counsel ivy which-key undo-tree rainbow-delimiters paredit multiple-cursors magit intero hindent diff-hl)))
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

(defun relocate-prefix-keys (keymap)
  "Use C-u for C-x map, C-M-u for C-c map."
  (mapc
   (lambda (mapping)
     (let* ((old-key (kbd (car mapping)))
            (new-key (kbd (cdr mapping)))
            (old-keymap (lookup-key keymap old-key))
            (new-keymap (lookup-key keymap new-key)))
       (when (and old-keymap (not new-keymap))
         (define-key keymap old-key nil)
         (define-key keymap new-key old-keymap))))
   '(("C-x" . "C-u")
     ("C-c" . "C-M-u"))))

(defun relocate-all-prefix-keys ()
  (mapc
   'relocate-prefix-keys
   (append
    (current-active-maps)
    (mapcar 'cdr minor-mode-map-alist))))
(add-hook
 'after-change-major-mode-hook
 #'relocate-all-prefix-keys)

(define-key key-translation-map (kbd "C-u 8")
  (lookup-key key-translation-map (kbd "C-x 8")))
(define-key key-translation-map (kbd "C-x 8") nil)

;; "C-x @"
(relocate-prefix-keys function-key-map)
(relocate-all-prefix-keys)

(dolist
    (mapping
     '(("<S-return>" . start-new-line)
       ("C-q" . save-buffers-kill-terminal)
       ("C-w" . delete-window)
       ("M-w" . delete-other-windows)
       ("C-e" . switch-to-buffer)
       ("C-r" . avy-goto-char)
       ("C-k" . previous-line)
       ("M-k" . scroll-down-command)
       ("C-M-k" . beginning-of-buffer)
       ("C-j" . next-line)
       ("M-j" . scroll-up-command)
       ("C-M-j" . end-of-buffer)
       ("C-h" . backward-char)
       ("M-h" . backward-word)
       ("C-M-h" . move-beginning-of-line)
       ("C-l" . forward-char)
       ("M-l" . forward-word)
       ("C-M-l" . move-end-of-line)
       ("C-o" . find-file)
       ("C-S-o" . projectile-find-file)
       ("C-M-o" . xref-find-definitions)
       ("M-o" . imenu)
       ("C-a" . mark-whole-buffer)
       ("C-s" . save-buffer)
       ("C-f" . isearch-forward)
       ("M-f" . isearch-backward)
       ("C-z" . undo-tree-undo)
       ("C-y" . undo-tree-redo)
       ("C-x" . kill-region)
       ("C-c" . kill-ring-save)
       ("C-v" . yank)
       ("M-v" . yank-pop)
       ("M-D" . duplicate-region-or-line)
       ("C-M-d" . kill-line)
       ("C-u" . Control-X-prefix)
       ("C-M-u" . mode-specific-command-prefix)
       ("M-u" . er/expand-region)
       ("M-U" . er/contract-region)
       ("M-m" . recenter-top-bottom)
       ("M-N" . move-line-down)
       ("M-P" . move-line-up)
       ("M-V" . join-line-next)
       ("M-T" . transpose-words-backward)))
  (global-set-key (kbd (car mapping)) (cdr mapping)))

;; Make C-S-<key>, C-M-S-<key> work under xterm.
;; See ~/.Xresources for sending these escape codes.
;; See https://github.com/jwiegley/emacs-release/blob/master/lisp/term/xterm.el
;; (with-eval-after-load 'xterm
;;   (dolist (key (number-sequence 33 127)) ;; See ascii
;;     (dolist (mod '((6 . "C-S") (8 . "C-M-S")))
;;       (let* ((mod-code (car mod))
;;              (mod-str (cdr mod))
;;              (full (kbd (format "%s-%c" mod-str key))))
;;         ;; For both XTerm.vt100.formatOtherKeys set to 0 or 1
;;         (define-key xterm-function-map (format "\e[27;%d;%d~" mod-code key) full)
;;         (define-key xterm-function-map (format "\e[%d;%du" key mod-code) full)))))

(with-eval-after-load 'isearch
  (mapc
   (lambda (mapping)
     (define-key isearch-mode-map (kbd (car mapping)) (cdr mapping)))
   '(("C-v" . isearch-yank-kill)
     ("C-f" . isearch-repeat-forward)
     ("M-f" . isearch-repeat-backward)
     ("M-l" . isearch-yank-word-or-char)
     ("C-v" . isearch-yank-kill)
     ("M-v" . isearch-yank-pop))))

(with-eval-after-load 'ivy
  (dolist
      (mapping
       '(("C-v" . nil)))
    (define-key ivy-minibuffer-map (kbd (car mapping)) (cdr mapping))))

(with-eval-after-load 'expand-region
  (setq expand-region-fast-keys-enabled nil)
  (setq expand-region-smart-cursor t))

(with-eval-after-load 'hydra
  (defhydra hydra-mark
    (:columns 3)
    "mark"
    ("n" mc/mark-next-like-this "mark next")
    ("N" mc/unmark-next-like-this "unmark next")
    ("M-n" mc/skip-to-next-like-this "skip next")
    ("p" mc/mark-previous-like-this "mark prev")
    ("P" mc/unmark-previous-like-this "unmark prev")
    ("M-p" mc/skip-to-previous-like-this "skip prev")
    ("t" mc/edit-lines "edit lines" :exit t)
    ("a" mc/edit-beginnings-of-lines "edit line starts" :exit t)
    ("e" mc/edit-ends-of-lines "edit line ends" :exit t)
    ("h" mc/mark-all-like-this "mark all" :exit t)
    ("1" mc/insert-numbers "insert numbers" :exit t)
    ("l" mc/insert-letters "insert letters" :exit t)
    ("m" er/expand-region "expand region")
    ("M" er/contract-region "contract region")
    ("'" mc-hide-unmatched-lines-mode "unmatched lines"))
  (define-key ctl-x-map (kbd "m") 'hydra-mark/body))

(add-hook 'after-init-hook (lambda () (key-chord-mode 1)))
(with-eval-after-load 'key-chord
  (key-chord-define-global "jj" 'avy-goto-word-or-subword-1))

(add-hook 'java-mode-hook (lambda () (require 'lsp-java) (lsp)))
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
   (lambda (mapping)
     (define-key company-active-map (kbd (car mapping)) (cdr mapping)))
   '(("M-n" . nil)
     ("M-p" . nil)
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous))))

(add-hook 'after-init-hook #'projectile-mode)
(with-eval-after-load 'projectile
  (define-key ctl-x-map (kbd "p") 'projectile-command-map)
  (define-key search-map (kbd "f") 'projectile-grep))

(with-eval-after-load 'elisp-mode
  (define-key lisp-interaction-mode-map (kbd "C-j") nil))

(add-hook 'haskell-mode-hook 'smartparens-mode)
(add-hook 'java-mode-hook 'smartparens-mode)
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
     (,(kbd "C-M-d") nil)
     (,(kbd "C-M-u") nil)
     (,(kbd "M-s") nil)
     (,(kbd "M-S") nil)
     (,(kbd "C-j") nil)
     (,(kbd "C-k") nil))))

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
               (margin (max 0 (/ (- width 100) 2))))
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
