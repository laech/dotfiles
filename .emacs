
(defmacro time (name &rest body)
  "Measure function execution time."
  `(let ((start (current-time)))
     ,@body
     (message "==> %.06fs %s" (float-time (time-since start)) ,name)))

(defun fix-fringes ()
  (interactive)
  (set-face-attribute 'fringe nil :background nil))

(time
 "config"

 (add-to-list 'default-frame-alist '(internal-border-width . 0))

 (setq-default inhibit-startup-screen t)

 (set-face-attribute 'mode-line-inactive nil :box nil)
 (set-face-attribute 'mode-line nil :box nil :background "gray90")
 (fix-fringes)

 (if (string-equal system-type "darwin")
     (add-to-list 'default-frame-alist '(font . "Inconsolata-16"))
   (progn
     (add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
     (custom-set-variables
      '(menu-bar-mode nil)))))


(time
 "smooth-scroll"
 (setq-default
  redisplay-dont-pause t
  scroll-step 1
  scroll-margin 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1
  auto-window-vscroll nil
  mouse-wheel-scroll-amount '(1 ((shift) . 1))
  mouse-wheel-progressive-speed nil
  mouse-wheel-follow-mouse 't))


(time
 "custom-set-variables"
 (custom-set-variables
  '(blink-cursor-mode nil)
  '(global-auto-revert-mode t)
  '(indent-tabs-mode nil)
  '(line-spacing 0.2)
  '(show-paren-mode t)
  '(scroll-bar-mode nil)
  '(tool-bar-mode nil)
  '(package-selected-packages
    (quote
     (exec-path-from-shell
      expand-region
      diff-hl
      haskell-mode
      hindent
      ido-ubiquitous
      intero
      magit
      multiple-cursors
      paredit
      projectile
      rainbow-delimiters
      smex
      which-key
      writeroom-mode)))))


(time
 "package"
 (setq package-archives
       '(("gnu" . "https://elpa.gnu.org/packages/")
	 ("melpa-stable" . "https://stable.melpa.org/packages/")
	 ("marmalade" . "https://marmalade-repo.org/packages/")))
 (package-initialize)
 (unless package-archive-contents
   (package-refresh-contents))
 (package-install-selected-packages))


(if (string-equal system-type "darwin")
    (time "exec-path-from-shell-initialize"
          (exec-path-from-shell-initialize)))


(time
 "ido"
 (setq ido-enable-flex-matching t)
 (setq ido-use-virtual-buffers t)
 (ido-mode t)
 (ido-everywhere t)
 (ido-ubiquitous-mode t))


(time
 "lisp"
 (autoload 'enable-paredit-mode "paredit" "Turn on paredit." t)
 (add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
 (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
 (add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
 (add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
 (add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
 (add-hook 'scheme-mode-hook                      #'enable-paredit-mode)
 (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(time
 "haskell"
 (add-hook 'haskell-mode-hook 'intero-mode)
 (add-hook 'haskell-mode-hook 'flyspell-prog-mode)
 (with-eval-after-load 'speedbar
   (speedbar-add-supported-extension ".hs"))
 (with-eval-after-load 'intero
   (with-eval-after-load 'flycheck
     (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))))


(time
 "speedbar"
 (setq speedbar-use-images nil)
 (with-eval-after-load 'speedbar
   (defun speedbar-set-mode-line-format ()
     (setq mode-line-format nil))))


(time
 "smex"
 (global-set-key (kbd "M-x") 'smex))


(time
 "expand-region"
 (global-set-key (kbd "C-=") 'er/expand-region)) ; "C-- C-=" to contract


(time
 "multiple-cursors"
 (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
 (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
 (global-set-key (kbd "C->") 'mc/mark-next-like-this)
 (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))


(time
 "writeroom-mode"

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


(time
 "diff-hl"
 (global-diff-hl-mode)
 (diff-hl-flydiff-mode)
 (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(time "projectile" (add-hook 'prog-mode-hook #'projectile-mode))
(time "which-key" (which-key-mode))
