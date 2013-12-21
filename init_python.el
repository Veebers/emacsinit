;; Settings for python mode and python programming.

;; (autoload 'python-mode "python-mode" "Python Mode." t)
(require 'python)

;; Keybindings
(define-key python-mode-map "\C-m" 'newline-and-indent)

(setq-default fill-column 79)
;; (add-hook 'text-mode-hook 'turn-on-autofill)

(provide 'init_python)
