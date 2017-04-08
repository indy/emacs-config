;; http://www.greghendershott.com/2017/02/emacs-themes.html

(defun isg/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defvar isg/theme-hooks nil
  "((theme-id . function) ...)")

(defun isg/add-theme-hook (theme-id hook-func)
  (add-to-list 'isg/theme-hooks (cons theme-id hook-func)))

(defun isg/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `isg/add-theme-hook'."
  (unless no-enable
    (isg/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id isg/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'isg/load-theme-advice)

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

(defun isg/val (property)
  (get 'isg/local property))

(defun isg/start-shell (shell-name)
  "start a new shell"
  (interactive "sShell name: ")
  (shell)
  (if (string= "" shell-name)
      (rename-uniquely)
    (rename-buffer shell-name)))

(defun isg/machine-set-keys ()
  "set machine specific key bindings"
  (mapcar (lambda (kons)
            (global-set-key (car kons) (cdr kons)))
          (isg/val 'key-setup)))

(defun isg/run-machine-function (property)
  (let ((fn (isg/val property)))
    (if fn (funcall fn))))

(defun isg/frame-setup ()
  (when (and (isg/val 'frame-r) (isg/val 'frame-l))
    (setq initial-frame-alist (isg/val 'frame-r)
          default-frame-alist (isg/val 'frame-l)))
  (add-to-list 'default-frame-alist
               (cons 'font
                     (isg/val 'default-font))))

(provide 'helper-functions)
;;; helper-functions.el ends here
