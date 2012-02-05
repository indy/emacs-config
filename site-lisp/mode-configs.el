(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(autoload 'imbue-mode "imbue" nil t)
(add-to-list 'auto-mode-alist '("\\.imd$" . imbue-mode))

(autoload 'espresso-mode "espresso" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(add-hook 'espresso-mode-hook
      '(lambda ()
         (setq espresso-indent-level 2)))

(add-hook 'css-mode-hook 'rainbow-mode)

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

(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map (kbd "C-c e") 'shell-eval-last-expression)
             (define-key clojure-mode-map (kbd "C-c x") 'shell-eval-defun)))


;;; yasnippets for autocompletion
(require 'yasnippet)
(require 'dropdown-list)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))
(yas/global-mode)



(provide 'mode-configs)
