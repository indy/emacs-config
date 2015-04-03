;;; essential package
(require 'cl)

;;; location of my customisations
(push "~/.emacs.d/site-lisp" load-path)

;;; ensure essential packages have been installed
(require 'package-manager)

(push "~/.emacs.d/external" load-path)  ; third party code that hasn't
                                        ; been packaged yet
(push "~/.emacs.d/external/auto-complete" load-path)
(push "~/.emacs.d/external/expand-region.el" load-path)

(require 'magit)
(require 'helper-functions)
(require 'machine-settings)
(require 'mode-configs)

(isg-frame-setup)

(run-isg-machine-function 'post-setup-fn)

(require 'global-settings)

(require 'keys)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("09cf608f7247a53f1caffb909822e2f4cf8205dd9d6c0040c029d84e7d4eb5a1" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'actress)

;; helm crap
(setq helm-display-header-line nil)
(set-face-attribute 'helm-source-header nil :height 0.1)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)
