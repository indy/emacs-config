(add-to-list 'auto-mode-alist '("\\.seni$" . scheme-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
;(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
(add-to-list 'auto-mode-alist 
             '("\\.sql$" . (lambda ()
                             (sql-mode)
                             (sql-highlight-postgres-keywords))))
(add-to-list 'auto-mode-alist 
             '("\\.psql$" . (lambda ()
                             (sql-mode)
                             (sql-highlight-postgres-keywords))))


(autoload 'imbue-mode "imbue" nil t)
(add-to-list 'auto-mode-alist '("\\.imd$" . imbue-mode))

;;; flymake
; show flymake notifications using cursor position rather than hovering with a mouse
(require 'flymake-cursor)               

;;; javascript
(require 'flymake-node-jshint)
(setq js-indent-level 2)
(setq flymake-node-jshint-config "~/.emacs.d/site-lisp/jshint-config.json")
;(add-hook 'js-mode-hook (lambda () (flymake-mode 1)))

;;; go mode
;(require 'go-mode)
;(add-hook 'before-save-hook #'gofmt-before-save)

;;; org-mode
(add-hook 'org-mode-hook 'soft-wrap-lines)
(defun soft-wrap-lines ()
  "Make lines wrap at window edge and on word boundary,
in current buffer."
  (interactive)
  (setq truncate-lines nil)
  (setq word-wrap t))

;;; css-mode
(setq css-indent-offset 2)
(add-hook 'css-mode-hook 'rainbow-mode)

;;; sass mode
(add-hook 'sass-mode-hook 'rainbow-mode)


;;; paredit
(defun turn-on-paredit ()
  (paredit-mode t))

(dolist (mode '(scheme emacs-lisp lisp clojure cider-repl)) 
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) 
            'turn-on-paredit))

;;; display 'fn' as the lambda symbol
(defun pretty-fn nil 
  (font-lock-add-keywords
   nil `(("(\\(fn\\>\\)" 
          (0 (progn (compose-region (match-beginning 1) (match-end 1) 
                                    ,(make-char 'greek-iso8859-7 107))
                    nil)))
         ("\\(#\\)("
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    402)
                    nil))))))

;;; clojure
(add-hook 'clojure-mode-hook (lambda () 
                               (pretty-fn)))

; hide the *nrepl-connection* and *nrepl-server* buffers
(setq nrepl-hide-special-buffers t)


(require 'ac-nrepl)

(dolist (mode '(cider-repl cider)) 
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) 
            'ac-nrepl-setup))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(dolist (mode '(auto-complete cider-repl cider)) 
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) 
            'set-auto-complete-as-completion-at-point-function))

;(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)


(provide 'mode-configs)
