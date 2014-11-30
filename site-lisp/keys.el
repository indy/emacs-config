(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;;; C-c SPC -> ace-jump-word-mode
;;; C-u C-c SPC -> ace-jump-char-mode
;;; C-u C-u C-c SPC -> ace-jump-line-mode



;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key "\C-x!" 'delete-other-windows)

;;; org mode overrides the default "Shift-cursor keys" navigation
;;; of windmove and for some reason re-inserting the original keyboard
;;; commands in the org-load-hook or org-mode-hook doesn't revert back
;;; the behaviour. So I'm giving up, assigning new keys for windmove
;;; and setting them as globals:
(global-set-key "\M-1"  'windmove-left)
(global-set-key "\M-2"  'windmove-down)
(global-set-key "\M-3"  'windmove-up)
(global-set-key "\M-4"  'windmove-right)

(global-set-key "\M-5"  'winner-undo)
(global-set-key "\M-6"  'winner-redo)

;;; use winner mode keys for undo/redo operations on window configurations
;;; C-c left 
;;; C-c right
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\M-0" 'other-frame)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-m" 'call-last-kbd-macro)
(global-set-key "\M-j" 'eval-print-last-sexp)

(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key "\C-cv" 'magit-status)
(global-set-key "\C-c\C-v" 'magit-status)

(global-set-key "\C-c\C-f" 'flymake-mode)
(global-set-key "\M-7" 'isg-start-shell)
(global-set-key "\M-8" 'isg-start-eshell)
(global-set-key "\M-9" 'isg-seni-eval-defn)

(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)

;;; 
(global-set-key "\C-x\C-g" 'find-file-in-git-repo)


(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z
(define-key helm-map (kbd "C-w") 'backward-kill-word)


(isg-machine-set-keys)                 ; machine specific key bindings

(provide 'keys)
