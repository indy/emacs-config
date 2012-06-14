(require 'cl)

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
;  color-theme-act... 0.1.0
;  haml-mode          3.0.14
;  ido-ubiquitous     0.8
;  magit              1.0.0
;  markdown-mode      1.8.1
;  paredit            22
;  parenface          1.1
;  php-mode           1.5.0
;  rainbow-mode       0.2
;  sass-mode          3.0.14
;  slime              20100404.1
;  yasnippet          0.6.1

(push "~/.emacs.d/site-lisp" load-path) ; my customisations
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
