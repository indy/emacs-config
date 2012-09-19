(defun clojurescript-repl ()
 (interactive)
 (run-lisp "lein trampoline cljsbuild repl-listen"))

;;; send the last sexp to the inferior lisp 
;;; process and then call (main) to re-render
(defun isg-seni-eval-defn (&optional and-go)
  (interactive "P")
  (lisp-do-defun 'lisp-eval-string 'lisp-eval-region)
  (if and-go (switch-to-lisp t))
  (comint-send-string (inferior-lisp-proc) "(main)\n"))

(defun isg-ido-key ()
  (define-key ido-file-completion-map "\C-w" 'ido-delete-backward-word-updir))

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
    (rename-buffer shell-name)))

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
