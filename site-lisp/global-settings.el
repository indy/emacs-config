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

; Use dictionaries and yasnippet by default
(setq-default ac-sources 
              (append '(ac-source-yasnippet ac-source-dictionary) ac-sources))

(global-auto-complete-mode t)
(setq ac-auto-start 2     ; Start auto-completion after 2 characters of a word
      ac-ignore-case nil  ; case sensitivity is important when finding matches
      ac-use-menu-map t  ; navigate pop-up menu with C-n, C-p
      ac-modes (cons 'clojurescript-mode (cons 'go-mode ac-modes))) ; enable ac when in go-mode

; yasnippet configuration
; ------------------------
(require 'yasnippet)
;(require 'dropdown-list)
(yas-global-mode 1)
(setq yas-snippet-dirs "~/.emacs.d/snippets")
;(yas/load-directory yas-snippet-dirs)
(setq yas-prompt-functions '(;yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

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

(load "server")
(unless (server-running-p) (server-start))
;(server-start)
(new-frame)

(provide 'global-settings)
