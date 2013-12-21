;; To explore and potentially use later on:
;;  - diff-hl
;;  - linum-relative
;;  - git-gutter
;;  - volatile-highlights
;;  - Yasnippet !
;;  - multiple-cursors
;;  - magit
;;  - expand-region
;;  - (perhaps) web-mode.el: web-mode.el

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path  "~/.emacs.d/elpa/fill-column-indicator-20130807.619/")

;; General configuration

(require 'color-theme)
(setq color-theme-is-global 1)
(load "~/.emacs.d/config/color-theme-wombat.el")
(color-theme-wombat)
(set-cursor-color "white")
(global-hl-line-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode)
(delete-selection-mode t)
(line-number-mode t)
(column-number-mode t)
(windmove-default-keybindings)
(windmove-default-keybindings 'meta)

(setq cursor-type 'hbar)
(setq default-cursor-type 'hbar)
(setq mouse-yank-at-point t)
(setq transient-mark-mode t)
(setq inhibit-startup-message t)
(setq x-select-enable-clipboard t)
(setq isearch-highlight t)
(setq search-highlight t)
(setq-default transient-mark-mode t)
;; replace highlighted text with what I type rather than just
;; inserting at a point
(setq next-line-add-newlines nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(setq frame-title-format
      '("[" system-name "] " (buffer-file-name "%f" (dired-directory dired-directory "%b")) ))
(when window-system
  (set-face-attribute 'default nil :font "Ubuntu Mono 13")
  (set-face-background 'hl-line "#3D3E3A"))

(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Behaviour settings
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Disable backup stuff
(setq backup-inhibited t)
(setq auto-save-default nil)

;; (Re)bindings of keys
; Steve told me to do these :P (from 10 Effective emacs)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
; Prefer kill word to backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
; I miss my vim movement
; I already have a 'delete line' so I won't need the delete blank line so I will
; re-assign this one to something I should use more.
(global-set-key "\C-x\C-o" 'occur)
(global-set-key "\C-c\C-o" 'multi-occur-in-matching-buffers)

(global-set-key "\C-Z" nil)

(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)

(global-set-key "\C-c\C-l" 'line-to-buffer-top)

(global-set-key "\C-xt" 'move-to-char)
(global-set-key "\C-xT" 'moveback-to-char)
(global-set-key [?\C-\*] 'isearch-forward-at-point)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

(global-set-key [?\C-\%] 'joc-bounce-sexp)

(global-set-key [?\C-o] 'bja-open-line-below)
(global-set-key [?\M-o] 'bja-open-line-above)

;; -- Setting up packages. --
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/elpa/auto-complete-20131128.233/dict")
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'python-mode)

;; fill column
(require 'fill-column-indicator)
(setq fci-rule-color "#99968b")

;; Projectile setup
;; (setq projectile-indexing-method 'git)
(require 'grizzl)
(projectile-global-mode t)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
(setq projectile-keymap-prefix (kbd "C-c p"))

(let ((ack_path "~/bin/ack"))
  (if (file-exists-p ack_path)
      (setq ack-and-a-half-executable ack_path)
    )
  )

;; IDO setup
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not \
readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; Jedi setup, perhaps move this to init_python?
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; Modes setup
(autoload 'python-mode "init_python" "" t)

;; Supporting functions for keybindings
;; Vim-like open linie above/below current
(defun bja-open-line-below ()
  (interactive)
  (end-of-line)
  (open-line 1)
  (next-line 1)
  (indent-according-to-mode))

(defun bja-open-line-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode))

;; Stole this from someone, bounces between paraentehisi like '%' in vi
(defun joc-bounce-sexp ()
  "Will bounce between matching parens just like % in vi"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
    (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (error "%s" "Not on a paren, brace, or bracket")))))

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

;; Behaviour functions
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy
a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-end-position 1)))))
;;   (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill
a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end) )
     (list (line-beginning-position)
           (line-beginning-position 2)))))
