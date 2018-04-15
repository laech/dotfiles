
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
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line nil :box nil :background "gray90")
(fix-fringes)
(if (string-equal system-type "darwin")
    (add-to-list 'default-frame-alist '(font . "Inconsolata-16"))
  (progn
    (add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
    (custom-set-variables
     '(menu-bar-mode nil))))

(setq
 redisplay-dont-pause t
 scroll-step 1
 scroll-margin 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1
 auto-window-vscroll nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse 't)

(custom-set-variables
 '(blink-cursor-mode nil)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(line-spacing 0.2)
 '(show-paren-mode t)
 '(scroll-bar-mode nil)
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

(use-package magit :defer t)

(use-package
  haskell-mode
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'intero-mode)
  (add-hook 'haskell-mode-hook #'flyspell-prog-mode)
  (with-eval-after-load 'speedbar
    (speedbar-add-supported-extension ".hs"))
  (with-eval-after-load 'intero
    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))))

(use-package hindent :defer t)
(use-package intero :defer t)

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
  (global-set-key (kbd "C-=") 'er/expand-region)) ; "C-- C-=" to contract

(use-package
  multiple-cursors
  :defer t
  :init
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package
  writeroom-mode
  :defer t
  :init
  (add-to-list
   'window-size-change-functions
   (lambda (frame)
     (progn

       ;; Show bottom divider if there is more than 1 window
       (when (and (> (count-windows) 1)
                  (= (frame-parameter frame 'bottom-divider-width) 0))
         (set-frame-parameter frame 'bottom-divider-width 1))

       ;; Hide bottom divider if there is only 1 window
       (when (and (= (count-windows) 1)
                  (> (frame-parameter frame 'bottom-divider-width) 0))
         (set-frame-parameter frame 'bottom-divider-width 0)))))

  (add-hook 'find-file-hook #'writeroom-mode)

  (with-eval-after-load 'writeroom-mode
    (setq writeroom-global-effects '(writeroom-set-bottom-divider-width)
          writeroom-fringes-outside-margins nil
          writeroom-bottom-divider-width 0)
    (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
    (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
    (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width)
    (define-key writeroom-mode-map (kbd "C-M-?") #'writeroom-toggle-mode-line)))

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
  (add-hook 'prog-mode-hook #'projectile-mode))
