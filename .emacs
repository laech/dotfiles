
(set-face-attribute 'fringe nil :background nil)
(setq inhibit-startup-screen t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(global-auto-revert-mode t)
 '(line-spacing 0.2)
 '(package-selected-packages
   (quote
    (expand-region writeroom-mode diff-hl haskell-snippets which-key hindent projectile sr-speedbar multiple-cursors intero haskell-mode magit rainbow-delimiters paredit exec-path-from-shell smex ido-ubiquitous)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)


;; package

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(package-install-selected-packages)


;; mac

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (set-face-attribute 'default nil :font "Inconsolata-16" )
  (set-frame-font "Inconsolata-16" nil t))


;; ido

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(ido-ubiquitous-mode t)


;; lisp

(autoload 'enable-paredit-mode "paredit" "Turn on paredit." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; haskell

(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)
(with-eval-after-load 'speedbar
  (speedbar-add-supported-extension ".hs"))
(with-eval-after-load 'intero
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))


;; speedbar/sr-speedbar

(setq speedbar-use-images nil)
(setq sr-speedbar-width 26)
(setq sr-speedbar-right-side nil)


;; smex

(global-set-key (kbd "M-x") 'smex)


;; expand-region

(global-set-key (kbd "C-=") 'er/expand-region)
;; "C-- C-=" to contract


;; multiple-cursors

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-.")     'mc/mark-all-like-this)
(global-set-key (kbd "C-.")         'mc/mark-next-like-this)
(global-set-key (kbd "C-,")         'mc/mark-previous-like-this)
(global-set-key (kbd "C->")         'mc/unmark-next-like-this)
(global-set-key (kbd "C-<")         'mc/unmark-previous-like-this)
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-.") nil))


;; writeroom-mode

(add-hook 'find-file-hook #'writeroom-mode)
(with-eval-after-load 'writeroom-mode
  (setq writeroom-fullscreen-effect nil)
  (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
  (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width)
  (define-key writeroom-mode-map (kbd "C-M-?") #'writeroom-toggle-mode-line))


;; modes

(projectile-global-mode)
(yas-global-mode)
(which-key-mode)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)
