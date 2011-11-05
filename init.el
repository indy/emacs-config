(require 'cl)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;  installed the following packages:
;
;  ido-ubiquitous     0.4
;  magit              1.0.0
;  paredit            22
;  slime              20100404.1
;  clojure-mode       1.11.1
;  clojure-test-mode  1.5.6
;  clojurescript-mode 0.5
;  markdown-mode      1.8.1
;  color-theme        6.6.1
;  color-theme-actress 0.1.0

(push "~/.emacs.d/site-lisp" load-path) ; my customisations
(push "~/.emacs.d/external" load-path)  ; third party code that hasn't
                                        ; been packaged yet

(require 'magit)
(require 'helper-functions)
(require 'machine-settings)
(require 'mode-configs)

(require 'color-theme-actress)
(color-theme-actress)

(require 'keys)

(run-isg-machine-function 'frame-setup-fn)
(run-isg-machine-function 'post-setup-fn)
(require 'global-settings)

