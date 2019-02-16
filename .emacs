
(setq
 default-frame-alist
 `((internal-border-width . 0)
   (menu-bar-lines . ,(if (and (equal system-type 'darwin) (display-graphic-p)) 1 0))
   (ns-appearance . light)
   (ns-transparent-titlebar . t)
   (font . ,(if (equal system-type 'darwin)
                "Ubuntu Mono-16"
              "Monospace-12"))))

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
 '(fringe ((t nil)))
 '(mode-line ((t (:background "gray90" :foreground "black" :box nil))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :box nil :weight light))))
 '(window-divider ((t (:foreground "gray75")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(company-tooltip-idle-delay 0)
 '(delete-selection-mode t)
 '(global-auto-revert-mode t)
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
    (flx counsel ivy swiper writeroom-mode which-key undo-tree rainbow-delimiters projectile paredit multiple-cursors magit intero hindent haskell-snippets expand-region exec-path-from-shell diff-hl)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-margin 1)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-cursor nil)
 '(writeroom-fringes-outside-margins nil)
 '(writeroom-global-effects
   (quote
    (writeroom-set-alpha writeroom-set-menu-bar-lines writeroom-set-tool-bar-lines writeroom-set-vertical-scroll-bars writeroom-set-bottom-divider-width))))

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

(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "<S-return>") 'start-new-line)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key (kbd "s-d") 'duplicate-region-or-line)
(global-set-key (kbd "M-w") 'copy-region-or-line)
(global-set-key (kbd "C-w") 'kill-region-or-line)
(global-set-key (kbd "M-N") 'move-line-down)
(global-set-key (kbd "M-P") 'move-line-up)
(global-set-key (kbd "M-J") 'join-line-next)
(global-set-key (kbd "M-T") 'transpose-words-backward)

(global-set-key (kbd "C-c C-.") 'mc/mark-all-like-this)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C->") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-<") 'mc/unmark-previous-like-this)

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'after-init-hook #'global-undo-tree-mode)
(add-hook 'after-init-hook #'ivy-mode)
(add-hook 'after-init-hook #'counsel-mode)
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

(setq ivy-extra-directories nil)
(setq ivy-use-virtual-buffers t)
(setq ivy-initial-inputs-alist nil)
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))

(setq projectile-completion-system 'ivy)
(setq magit-completing-read-function 'ivy-completing-read)

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(with-eval-after-load 'paredit
  (defun transpose-sexps-reverse (arg)
    (interactive "*p")
    (transpose-sexps (- arg)))
  (define-key paredit-mode-map (kbd "C-M-S-t") 'transpose-sexps-reverse))

(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook #'intero-mode)
  (add-hook 'haskell-mode-hook #'hindent-mode)

  (defun hindent-reformat-region-or-buffer ()
    "Reformat region, or buffer if no region."
    (interactive)
    (if (region-active-p)
        (hindent-reformat-region (region-beginning) (region-end))
      (hindent-reformat-buffer)))

  (define-key haskell-mode-map (kbd "C-M-\\")
    'hindent-reformat-region-or-buffer)

  (with-eval-after-load 'hindent-mode
    (define-key hindent-mode-map (kbd "C-M-\\")
      'hindent-reformat-region-or-buffer))

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

(with-eval-after-load 'writeroom-mode
  (define-key writeroom-mode-map (kbd "C-M-<") 'writeroom-decrease-width)
  (define-key writeroom-mode-map (kbd "C-M->") 'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-M-=") 'writeroom-adjust-width)
  (define-key writeroom-mode-map (kbd "C-M-?") 'writeroom-toggle-mode-line))

(unless (display-graphic-p)
  (xterm-mouse-mode)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3))))

(if (display-graphic-p)
    (toggle-mode-line))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'super)
  (exec-path-from-shell-initialize))

(when (display-graphic-p)

  (defun fmt-kbd (prefix key)
    (kbd (if (> (length key) 1)
             (concat "<" prefix key ">")
           (concat prefix key))))

  (let ((keyboard
         '(
           "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12"
           "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" "DEL"
           "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "="
           "tab"
           "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "|"
           "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\"
           "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\"" "RET"
           "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'"
           "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?"
           "z" "x" "c" "v" "b" "n" "m" "," "." "/"
           "SPC"
           "insert" "home" "prior"
           "delete" "end" "next"
           "up" "left" "down" "right"))

        (translations
         '(("s-" . "C-")
           ("s-S-" . "C-S-")
           ("s-M-" . "C-M-")
           ("s-M-S-" . "s-M-S-"))))

    (dolist (translation translations)
      (dolist (key keyboard)
        (let ((from-key (fmt-kbd (car translation) key))
              (to-key (fmt-kbd (cdr translation) key)))
          (define-key input-decode-map from-key to-key))))

    (dolist
        (mapping
         '(("C-0" . "C-x C-0")
           ("C--" . "C-x C--")
           ("C-=" . "C-x C-=")
           ("C-q" . "C-x C-c")
           ("C-w" . "C-x 4 0")
           ("C-S-w" . "C-x 1")
           ("C-e" . "C-x b")
           ("C-o" . "C-x C-f")
           ("C-a" . "C-x h")
           ("C-s" . "C-x C-s")
           ("C-d" . "s-d")
           ("C-f" . "C-s")
           ("C-z" . "C-_")
           ("C-S-z" . "C-?")
           ("C-x" . "C-w")
           ("C-c" . "M-w")
           ("C-v" . "C-y")
           ("C-S-v" . "M-y")))
      (define-key input-decode-map
        (kbd (car mapping))
        (kbd (cdr mapping)))

      (global-set-key (kbd "C-S-o") 'counsel-git))))
