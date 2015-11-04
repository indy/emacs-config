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

(defun isg-val (property)
  (get 'isg-local property))

(defun isg-other-frame ()
  (interactive)
  (new-frame)
  (set-frame-position (selected-frame) 195 0)
  (set-frame-size (selected-frame) 80 55))

(defun isg-start-shell (shell-name)
  "start a new shell"
  (interactive "sShell name: ")
  (shell)
  (if (string= "" shell-name)
      (rename-uniquely)
    (rename-buffer shell-name)))

(defun isg-start-eshell (shell-name)
  "start a new eshell"
  (interactive "sEshell name: ")
  (eshell)
  (if (string= "" shell-name)
      (rename-uniquely)
    (rename-buffer shell-name))
  ; disable yas mode, screws with tab auto-completion
  ;(yas-minor-mode -1)
  )

(defun isg-machine-set-keys ()
  "set machine specific key bindings"
  (mapcar (lambda (kons)
            (global-set-key (car kons) (cdr kons)))
          (isg-val 'key-setup)))

(defun run-isg-machine-function (property)
  (let ((fn (isg-val property)))
    (if fn (funcall fn))))

(defun isg-frame-setup ()
  (when (and (isg-val 'frame-r) (isg-val 'frame-l))
    (setq initial-frame-alist (isg-val 'frame-r)
          default-frame-alist (isg-val 'frame-l)))
  (add-to-list 'default-frame-alist
               (cons 'font
                     (isg-val 'default-font))))

(provide 'helper-functions)
