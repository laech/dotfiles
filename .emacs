
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(defun fix-fringes ()
  (interactive)
  (set-face-attribute 'fringe nil :background nil))

(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(internal-border-width . 0))
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line nil :box nil :background "gray90")
(fix-fringes)
(if (string-equal system-type "darwin")
    (progn
      (add-to-list 'default-frame-alist '(font . "Inconsolata-16"))
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . light)))
  (progn
    (add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
    (custom-set-variables
     '(menu-bar-mode nil))
    (setq
     mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control)))
     mouse-wheel-progressive-speed nil)))

(setq
 scroll-margin 1
 scroll-conservatively 1)

(custom-set-variables
 '(blink-cursor-mode nil)
 '(frame-background-mode (quote light))
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(line-spacing 0.2)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-tilt-scroll t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(if (string-equal system-type "darwin")
    (use-package
      exec-path-from-shell
      :config
      (exec-path-from-shell-initialize)))

(use-package
  ido-ubiquitous
  :defer t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-use-virtual-buffers t)
  (add-hook 'after-init-hook #'ido-mode)
  (add-hook 'after-init-hook #'ido-everywhere)
  (add-hook 'after-init-hook #'ido-ubiquitous-mode))

(use-package
  paredit
  :defer t
  :init
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode))

(use-package
  rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package
  yasnippet
  :defer t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package magit :defer t)

(defun hindent-reformat-region-or-buffer ()
  "Reformat a region if selected, otherwise the whole buffer."
  (interactive)
  (if (region-active-p)
      (hindent-reformat-region (region-beginning) (region-end))
    (hindent-reformat-buffer)))

(use-package intero :defer t)
(use-package hindent :defer t)
(use-package haskell-snippets :defer t)
(use-package
  haskell-mode
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'intero-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode)
  (with-eval-after-load 'speedbar
    (speedbar-add-supported-extension ".hs"))
  (with-eval-after-load 'intero
    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))
  :bind
  ("C-M-\\" . hindent-reformat-region-or-buffer))

(use-package
  speedbar
  :defer t
  :init
  (setq speedbar-use-images nil)
  (with-eval-after-load 'speedbar
    (defun speedbar-set-mode-line-format ()
      (setq mode-line-format nil))))

(use-package
  smex
  :defer t
  :init
  (global-set-key (kbd "M-x") 'smex))

(use-package
  expand-region
  :defer t
  :init
  (global-set-key (kbd "C-=") 'er/expand-region) ; "C-- C-=" to contract by one
  (global-set-key (kbd "C-+") 'er/contract-region)) 

(use-package
  flyspell
  :defer t
  :init
  (setq flyspell-issue-message-flag nil)
  :config
  (define-key flyspell-mode-map (kbd "C-c $") nil)
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-M-i") nil))

(use-package
  multiple-cursors
  :defer t
  :init
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-.") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C->") 'mc/unmark-next-like-this)
  (global-set-key (kbd "C-,") 'mc/mark-previous-like-this-word)
  (global-set-key (kbd "C-<") 'mc/unmark-previous-like-this))

(use-package
  writeroom-mode
  :defer t
  :init
  (setq writeroom-fringes-outside-margins nil)
  (setq
   writeroom-global-effects
   '(writeroom-set-alpha
     writeroom-set-menu-bar-lines
     writeroom-set-tool-bar-lines
     writeroom-set-vertical-scroll-bars
     writeroom-set-bottom-divider-width))
  :bind
  (("C-M-<" . writeroom-decrease-width)
   ("C-M->" . writeroom-increase-width)
   ("C-M-=" . writeroom-adjust-width)
   ("C-M-?" . writeroom-toggle-mode-line)))

(use-package
  diff-hl
  :defer t
  :init
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  (add-hook 'after-init-hook #'diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package
  which-key
  :defer t
  :init
  (add-hook 'after-init-hook #'which-key-mode))

(use-package
  projectile
  :defer t
  :init
  (add-hook 'after-init-hook #'projectile-global-mode))

(use-package
  undo-tree
  :defer t
  :init
  (add-hook 'after-init-hook #'global-undo-tree-mode))

;; Copy/paste to/from clipboard when running in terminal mode.
;; This also allows integration with configured tmux using the
;; same C-y, M-y keys and see history in tmux's paste buffer.
(unless window-system
  (defun paste-from-clipboard ()
    (shell-command-to-string
     (if (string-equal system-type "darwin")
         "pbpaste"
       "xsel -ob")))

  (defun copy-to-clipboard (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (if (string-equal system-type "darwin")
                      (start-process "phcopy" "*Messages*" "pbcopy")
                    (start-process "xsel" "*Messages*" "xsel" "-ib"))))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'copy-to-clipboard)
  (setq interprogram-paste-function 'paste-from-clipboard))
