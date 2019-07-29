
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

;; Distinguish Tab from C-i
(keyboard-translate ?\C-i ?\H-i)

(dolist
    (mapping
     '(("<S-return>" . start-new-line)
       ("C-q" . save-buffers-kill-terminal)
       ("C-w" . kill-buffer-and-window)
       ("M-w" . delete-other-windows)
       ("C-e" . switch-to-buffer)
       ("C-r" . avy-goto-char)
       ("H-i" . previous-line)
       ("M-i" . scroll-down-command)
       ("H-M-i" . beginning-of-buffer)
       ("C-k" . next-line)
       ("M-k" . scroll-up-command)
       ("C-M-k" . end-of-buffer)
       ("C-j" . backward-char)
       ("M-j" . backward-word)
       ("C-M-j" . move-beginning-of-line)
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
       ("<C-M-backspace>" . kill-line-backward)
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

(with-eval-after-load 'org
  (mapc
   (lambda (arg) (apply 'define-key org-mode-map arg))
   `(([remap beginning-of-line] org-beginning-of-line)
     ([remap end-of-line] org-end-of-line)
     ([remap kill-line] org-kill-line)
     ([remap yank] org-yank)
     ([remap completion-at-point] pcomplete)
     (,(kbd "C-a") nil)
     (,(kbd "C-e") nil)
     (,(kbd "C-k") nil)
     (,(kbd "C-y") nil)
     (,(kbd "C-j") nil)
     (,(kbd "C-M-i") nil))))

(with-eval-after-load 'isearch
  (mapc
   (lambda (mapping)
     (define-key isearch-mode-map (kbd (car mapping)) (cdr mapping)))
   '(("C-v" . isearch-yank-kill)
     ("C-f" . isearch-repeat-forward)
     ("M-f" . isearch-repeat-backward)
     ("M-l" . isearch-yank-word-or-char)
     ("M-v" . isearch-yank-pop))))

(with-eval-after-load 'ivy
  (dolist
      (mapping
       '(("C-v" . nil)
         ("M-v" . nil)
         ("M-i" . nil)
         ("C-j" . nil)
         ("M-j" . nil)
         ("C-M-j" . nil)
         ("C-c" . nil)
         ("<C-tab>" . ivy-alt-done)))
    (define-key ivy-minibuffer-map (kbd (car mapping)) (cdr mapping))))

(with-eval-after-load 'company
  (mapc
   (lambda (mapping)
     (define-key company-active-map (kbd (car mapping)) (cdr mapping)))
   '(("C-k" . company-select-next)
     ("H-i" . company-select-previous))))

(with-eval-after-load 'elisp-mode
  (define-key lisp-interaction-mode-map (kbd "C-j") nil))

(with-eval-after-load 'paredit
  (mapc
   (lambda (arg) (apply 'define-key paredit-mode-map arg))
   `((,(kbd "C-M-d") nil)
     (,(kbd "C-M-u") nil)
     (,(kbd "C-j") nil)
     (,(kbd "C-k") nil))))
