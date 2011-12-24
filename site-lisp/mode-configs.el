
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(autoload 'imbue-mode "imbue" nil t)
(add-to-list 'auto-mode-alist '("\\.imd$" . imbue-mode))

(autoload 'espresso-mode "espresso" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-hook 'espresso-mode-hook
      '(lambda ()
         (setq espresso-indent-level 2)))

(defun turn-on-paredit ()
  (paredit-mode t))

(dolist (mode '(scheme emacs-lisp lisp clojure)) 
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) 
            'turn-on-paredit))

;;; display 'fn' as the lambda symbol
(defun pretty-fn nil 
  (font-lock-add-keywords
   nil `(("(\\(fn\\>\\)" 
          (0 (progn (compose-region (match-beginning 1) (match-end 1) 
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'clojure-mode-hook 'pretty-fn)

(provide 'mode-configs)
