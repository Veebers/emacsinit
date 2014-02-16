;; Settings for scheme mode and scheme programming.
(defun customise-scheme-mode ()
  ;; Quack
  ;; Enable Quack mode
  ;; The binary of your interpreter
  (setq scheme-program-name "mzscheme")

  ;; This hook lets you use your theme colours instead of quack's ones.
  (defun scheme-mode-quack-hook ()
    (require 'quack)
    (setq quack-fontify-style 'emacs))
  (add-hook 'scheme-mode-hook 'scheme-mode-quack-hook))

(provide 'init_scheme)
