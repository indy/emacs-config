
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Allow input to be sent to somewhere other than inferior-lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar clojure-repl-shell-name "*repl-shell*")

(defun isg-start-clojure-repl-shell ()
  "start a new clojure repl shell"
  (interactive)
  (isg-start-shell clojure-repl-shell-name))


;; This is a total hack: we're hardcoding the name of the shell buffer
(defun shell-send-input (input)
  "Send INPUT into the *shell* buffer and leave it visible."
  (save-selected-window
    (switch-to-buffer-other-window clojure-repl-shell-name)
    (goto-char (point-max))
    (insert input)
    (comint-send-input)))

(defun defun-at-point ()
  "Return the text of the defun at point."
  (apply #'buffer-substring-no-properties
         (region-for-defun-at-point)))

(defun region-for-defun-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun expression-preceding-point ()
  "Return the expression preceding point as a string."
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun shell-eval-last-expression ()
  "Send the expression preceding point to the *shell* buffer."
  (interactive)
  (shell-send-input (expression-preceding-point)))

(defun shell-eval-defun ()
  "Send the current toplevel expression to the *shell* buffer."
  (interactive)
  (shell-send-input (defun-at-point)))



(provide 'helper-functions)
