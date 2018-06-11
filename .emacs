
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t nil)))
 '(mode-line ((t (:background "gray90" :foreground "black" :box nil))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :box nil :weight light)))))

(add-to-list 'default-frame-alist '(internal-border-width . 0))
(add-to-list 'default-frame-alist '(ns-appearance . light))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(font . "Inconsolata-16"))
  (progn
    (add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
    (custom-set-variables '(menu-bar-mode nil))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(company-tooltip-idle-delay 0)
 '(elfeed-feeds
   (quote
    (("https://news.ycombinator.com/rss")
     ("https://rachelbythebay.com/w/atom.xml"))))
 '(elfeed-search-title-max-width 120)
 '(elfeed-search-title-min-width 80)
 '(frame-background-mode (quote light))
 '(global-auto-revert-mode t)
 '(ido-enable-flex-matching t)
 '(ido-use-virtual-buffers t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-spacing 0.2)
 '(mode-require-final-newline nil)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(mouse-wheel-tilt-scroll t)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("marmalade" . "https://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
    (writeroom-mode which-key undo-tree smex rainbow-delimiters projectile paredit multiple-cursors magit intero ido-ubiquitous hindent haskell-snippets expand-region exec-path-from-shell elfeed diff-hl)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-margin 1)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(writeroom-fringes-outside-margins nil)
 '(writeroom-global-effects
   (quote
    (writeroom-set-alpha writeroom-set-menu-bar-lines writeroom-set-tool-bar-lines writeroom-set-vertical-scroll-bars writeroom-set-bottom-divider-width))))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(add-hook 'after-init-hook #'global-undo-tree-mode)
(add-hook 'after-init-hook #'ido-everywhere)
(add-hook 'after-init-hook #'ido-ubiquitous-mode)
(add-hook 'after-init-hook #'ido-mode)
(add-hook 'after-init-hook #'projectile-global-mode)
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

(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook #'intero-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode)

  (define-key haskell-mode-map (kbd "C-M-\\")
    (lambda ()
      (interactive)
      (if (region-active-p)
          (hindent-reformat-region (region-beginning) (region-end))
        (hindent-reformat-buffer))))

  (with-eval-after-load 'speedbar (speedbar-add-supported-extension ".hs"))
  (with-eval-after-load 'intero
    (with-eval-after-load 'flycheck
      (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))))

(with-eval-after-load 'speedbar
  (setq speedbar-use-images nil)
  (defun speedbar-set-mode-line-format ()
    (setq mode-line-format nil)))

(with-eval-after-load 'flyspell
  (setq flyspell-issue-message-flag nil)
  (define-key flyspell-mode-map (kbd "C-c $") nil)
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-M-i") nil))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-.") 'mc/mark-all-like-this)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C->") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-<") 'mc/unmark-previous-like-this)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(global-set-key (kbd "M-N") 'move-line-down)
(global-set-key (kbd "M-P") 'move-line-up)

(with-eval-after-load 'writeroom-mode
  (define-key writeroom-mode-map (kbd "C-M-<") 'writeroom-decrease-width)
  (define-key writeroom-mode-map (kbd "C-M->") 'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-M-=") 'writeroom-adjust-width)
  (define-key writeroom-mode-map (kbd "C-M-?") 'writeroom-toggle-mode-line))

;; Copy/paste to/from clipboard when running in terminal mode.
;; This also allows integration with configured tmux using the
;; same C-y, M-y keys and see history in tmux's paste buffer.
(unless window-system

  (setq
   interprogram-cut-function
   (lambda  (text &optional push)
     (let ((process-connection-type nil))
       (let  ((proc (if (eq system-type 'darwin)
                        (start-process "phcopy" "*Messages*" "pbcopy")
                      (start-process "xsel" "*Messages*" "xsel" "-ib"))))
         (process-send-string proc text)
         (process-send-eof proc)))))

  (setq
   interprogram-paste-function
   (lambda ()
     (shell-command-to-string
      (if (string-equal system-type "darwin") "pbpaste" "xsel -ob")))))

(if (string-equal system-type "darwin")
    (exec-path-from-shell-initialize))
