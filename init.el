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

(require 'color-theme-actress)
(color-theme-actress)

(isg-frame-setup)

(run-isg-machine-function 'post-setup-fn)

(require 'global-settings)

(require 'keys)
