
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

(global-set-key "\C-cv" 'magit-status)
(global-set-key "\C-c\C-v" 'magit-status)

(global-set-key [f1] 'isg-start-shell)
(global-set-key [f2] 'isg-start-eshell)
(global-set-key [f3] 'isg-start-clojure-repl-shell)

(isg-machine-set-keys)                 ; machine specific key bindings

(provide 'keys)
