
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(fringe-mode 0 nil (fringe))
 '(global-auto-revert-mode t)
 '(package-selected-packages
   (quote
    (projectile sr-speedbar multiple-cursors intero haskell-mode magit rainbow-delimiters paredit exec-path-from-shell smex ido-ubiquitous)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(package-install-selected-packages)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)
(ido-ubiquitous-mode t)


(autoload 'enable-paredit-mode "paredit" "Turn on paredit." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'speedbar-load-hook (lambda () (speedbar-add-supported-extension ".hs")))

(setq speedbar-use-images nil)
(setq sr-speedbar-width 26)
(setq sr-speedbar-right-side nil)

(global-set-key (kbd "M-x") 'smex)


(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-.")     'mc/mark-all-like-this)
(global-set-key (kbd "C-.")         'mc/mark-next-like-this)
(global-set-key (kbd "C-,")         'mc/mark-previous-like-this)
(global-set-key (kbd "C->")         'mc/unmark-next-like-this)
(global-set-key (kbd "C-<")         'mc/unmark-previous-like-this)


(projectile-global-mode)
