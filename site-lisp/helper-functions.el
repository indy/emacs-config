
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

(defun isg-machine-set-keys ()
  "set machine specific key bindings"
  (mapcar (lambda (kons)
            (global-set-key (car kons) (cdr kons)))
          (isg-val 'key-setup)))

(defun run-isg-machine-function (property)
  (let ((fn (isg-val property)))
    (if fn (funcall fn))))

(provide 'helper-functions)
