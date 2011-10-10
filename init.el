(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;  installed the following packages:
;
;  clojure-mode       1.11.1
;  clojure-test-mode  1.5.6
;  clojurescript-mode 0.5
;  color-theme        6.6.1
;  color-theme-actress 0.1.0
;  ido-ubiquitous     0.1
;  magit              1.0.0
;  markdown-mode      1.8.1
;  slime              20100404.1
;  starter-kit        2.0.2
;  starter-kit-lisp   2.0.3


(push "~/.emacs.d/site-lisp" load-path) ; my customisations
(push "~/.emacs.d/external" load-path)  ; third party code that hasn't
                                        ; been packaged yet

(require 'magit)
(require 'helper-functions)
(require 'machine-settings)
(require 'mode-configs)
(require 'global-settings)

(require 'color-theme-actress)
(color-theme-actress)

(require 'keys)
(run-isg-machine-function 'frame-setup-fn)
(run-isg-machine-function 'post-setup-fn)
