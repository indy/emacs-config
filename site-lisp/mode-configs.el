(add-to-list 'auto-mode-alist '("\\.seni$" . scheme-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))

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

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;;; flymake
; show flymake notifications using cursor position rather than hovering with a mouse
;(require 'flymake-cursor)               

;;; javascript
;(require 'flymake-node-jshint)
(setq js2-basic-offset 2)
(setq js-indent-level 2)
(setq js2-global-externs '("require" "expect" "describe" "it" "beforeEach"))
;(setq flymake-node-jshint-config "~/.emacs.d/site-lisp/jshint-config.json")
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

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
;(sp-autoskip-closing-pair 'always)
;; smartparens notes:

;; 1. select some text in an html file and 
;;    press '<' to wrap the selection in a tag
;;

;; Add smartparens-strict-mode to all sp--lisp-modes hooks. C-h v sp--lisp-modes
;; to customize/view this list.
(mapc (lambda (mode)
        (add-hook (intern (format "%s-hook" (symbol-name mode))) 'smartparens-strict-mode))
      sp--lisp-modes)


;; Conveniently set keys into the sp-keymap, limiting the keybinding to buffers
;; with SP mode activated
(mapc (lambda (info)
        (let ((key (kbd (car info)))
              (function (car (cdr info))))
          (define-key sp-keymap key function)))
      '(("C-)" sp-up-sexp)
        ("M-s" sp-splice-sexp)

        ("C-M-f" sp-forward-sexp)
        ("C-M-b" sp-backward-sexp)
 
        ("C-M-d" sp-down-sexp)
        ("C-M-a" sp-backward-down-sexp)
        ("C-S-a" sp-beginning-of-sexp)
        ("C-S-d" sp-end-of-sexp)
 
        ("C-M-e" sp-up-sexp)
 
        ("C-M-u" sp-backward-up-sexp)
        ("C-M-t" sp-transpose-sexp)
 
        ("C-M-n" sp-next-sexp)
        ("C-M-p" sp-previous-sexp)
 
        ("C-M-k" sp-kill-sexp)
        ("C-M-w" sp-copy-sexp)
 
        ("M-<delete>" sp-unwrap-sexp)
        ("M-<backspace>" sp-backward-unwrap-sexp)
 
        ("C-<right>" sp-forward-slurp-sexp)
        ("C-<left>" sp-forward-barf-sexp)
        ("C-M-<left>" sp-backward-slurp-sexp)
        ("C-M-<right>" sp-backward-barf-sexp)
 
        ("C-M-<delete>" sp-splice-sexp-killing-forward)
        ("C-M-<backspace>" sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" sp-splice-sexp-killing-around)

        ("C-]" sp-select-next-thing-exchange)
        ("C-<left_bracket>" sp-select-previous-thing)
        ("C-M-]" sp-select-next-thing)
 
        ("M-F" sp-forward-symbol)
        ("M-B" sp-backward-symbol)
 
        ("H-t" sp-prefix-tag-object)
        ("H-p" sp-prefix-pair-object)
        ("H-s c" sp-convolute-sexp)
        ("H-s a" sp-absorb-sexp)
        ("H-s e" sp-emit-sexp)
        ("H-s p" sp-add-to-previous-sexp)
        ("H-s n" sp-add-to-next-sexp)
        ("H-s j" sp-join-sexp)
        ("H-s s" sp-split-sexp)))

;; This is from authors config, seems to let you jump to the end of the current
;; sexp with paren?
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
;(define-key scheme-mode-map (kbd ")") 'sp-up-sexp)
;(define-key clojure-mode-map (kbd ")") 'sp-up-sexp)

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


;;; scheme
(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map (kbd ")") 'sp-up-sexp)))

;;; clojure
(add-hook 'clojure-mode-hook (lambda ()
                               (define-key clojure-mode-map (kbd ")") 'sp-up-sexp)
                               (pretty-fn)))


(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

; hide the *nrepl-connection* and *nrepl-server* buffers
(setq nrepl-hide-special-buffers t)

(provide 'mode-configs)
