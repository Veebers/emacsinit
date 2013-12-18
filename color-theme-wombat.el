; http://coderchrome.org/10

(eval-when-compile (require 'color-theme))

(defun color-theme-wombat ()
	(interactive)
	(color-theme-install '(color-theme-wombat
	(
		(foreground-color . "#f6f3e8")
		(background-color . "#242424")
	)
	(font-lock-string-face ((t (:foreground "#95e454"))))
	(font-lock-type-face ((t (:foreground "#cae682"))))
	(font-lock-comment-face ((t (:foreground "#99968b"))))
	(font-lock-variable-name-face ((t (:foreground "#f6f3e8"))))
	(font-lock-comment-delimiter-face ((t (:foreground "#99968b"))))
	(font-lock-doc-face ((t (:foreground "#99968b"))))
	(font-lock-keyword-face ((t (:foreground "#8ac6f2"))))
	(font-lock-constant-face ((t (:foreground "#e5786d"))))
	(font-lock-preprocessor-face ((t (:foreground "#e5786d"))))
	(font-lock-doc-string-face ((t (:foreground "#99968b"))))
	(font-lock-builtin-face ((t (:foreground "#cae682"))))
	(font-lock-function-name-face ((t (:foreground "#cae682"))))

    ;; 2010-11-30 chrisl: This is what i've added / modified
    ;; (mode-line ((t (:background "#444444" :foreground "#cccddd"))))
    (mode-line ((t (:foreground "#cccddd" :background "darkslateblue" :box (:line-width -1 :style released-button)))))
    (region ((t (:background "#555577"))))

    (fringe ((t (:background "grey10"))))

    ;; Modeline stuff
    ;; 242424
    (mode-line-inactive ((t (:background "#444444" :foreground "paleturquoise4"))))
    (mode-line-buffer-id ((t (:foreground "lightyellow" :bold t :weight bold))))
    (mode-line-emphasis ((t (:foreground "#cccddd" :bold t :weight bold))))
)))
