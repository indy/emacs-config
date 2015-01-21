(require 'helm-config)
(setq helm-ff-skip-boring-files t)
(setq helm-boring-file-regexp-list
  '("\\.\\.$" "\\.$" "\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$"  "\\.la$" "\\.o$" "~$" "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$"))
(helm-mode 1)


(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

; give buffers unique names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(require 'deft)
(setq deft-directory "~/notes/deft"
      deft-extension "org"
      deft-text-mode 'org-mode
      deft-use-filename-as-title t
      deft-auto-save-interval 5.0)

; reload files that have been changed outside of emacs (e.g. Eclipse autoformat)
(global-auto-revert-mode t)

(require 'ace-jump-mode)
(require 'expand-region)
(require 'find-file-in-git-repo)

; auto-complete configuration
; ---------------------------
(require 'auto-complete-config)

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

(if (not (file-exists-p (isg-val 'save-folder)))
    (make-directory (isg-val 'save-folder)))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(when (fboundp 'winner-mode)
  (winner-mode 1))

(defalias 'list-buffers 'ibuffer)

(setq exec-path (append (isg-val 'extra-exec-paths)  exec-path)
      ring-bell-function (lambda () (message "*beep*"))

      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (isg-val 'url-opener)


      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      ediff-window-setup-function 'ediff-setup-windows-plain

      line-move-visual nil
      line-number-mode t
      european-calendar-style t
      display-time-24hr-format t
      default-tab-width 2
      column-number-mode t
      enable-local-variables nil  ; ignore local variables defined in files
      inhibit-startup-message t
      initial-scratch-message nil
      backup-by-copying t         ; don't clobber symlinks
      backup-directory-alist `((".*" . ,(isg-val 'save-folder)))
      auto-save-file-name-transforms `((".*" ,(isg-val 'save-folder) t))
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

;;; allow emacs to edit chrome textboxes
; (edit-server-start)

;(load "server")
;(unless (server-running-p) (server-start))
(new-frame)

(provide 'global-settings)
