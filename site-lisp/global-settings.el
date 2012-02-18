(show-paren-mode t)
(global-font-lock-mode t)

(transient-mark-mode t) ;; highlight selected text region

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default font-lock-maximum-decoration t)
(setq-default indent-tabs-mode nil)
(blink-cursor-mode 0)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq isg-save-folder (isg-val 'save-folder))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(when (fboundp 'winner-mode)
  (winner-mode 1))

(defalias 'list-buffers 'ibuffer)

(setq exec-path (append (isg-val 'extra-exec-paths)  exec-path)
      ring-bell-function (lambda () (message "*beep*"))

      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (isg-val 'url-opener)

      line-move-visual nil
      line-number-mode t
      european-calendar-style t
      display-time-24hr-format t
      default-tab-width 2
      js-indent-level 2
      column-number-mode t
      enable-local-variables nil  ; ignore local variables defined in files
      inhibit-startup-message t
      initial-scratch-message nil
      backup-by-copying t         ; don't clobber symlinks
      backup-directory-alist (isg-val 'save-folder)
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)          ; use versioned backups

;;; os specific settings
(cond 
 ((string-match "osx" (isg-val 'machine-os))
  (setq
   mac-command-modifier 'meta
   default-directory "~/"
   multi-term-program "/bin/bash")
  (fset 'insertPound "#")
  (global-set-key (kbd "C-M-3") 'insertPound))

 ((string-match "linux" (isg-val 'machine-os))
  (setq
   default-directory "~/"
   multi-term-program "/bin/bash")))

;(require 'edit-server)
;(edit-server-start)
(server-start)
(new-frame)

(provide 'global-settings)
