(require 'cl)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;  installed the following packages:
;
;  clojure-mode       1.11.1       installed  Major mode for Clojure code
;  clojure-test-mode  1.5.6        installed  Minor mode for Clojure tests
;  clojurescript-mode 0.5          installed  Major mode for ClojureScript code
;  color-theme        6.6.1        installed  install color themes
;  color-theme-actress0.1.0        installed  A dark color theme for GNU Emacs.
;  ido-ubiquitous     0.8          installed  Use ido (nearly) everywhere.
;  magit              1.0.0        installed  Control Git from Emacs.
;  markdown-mode      1.8.1        installed  Emacs Major mode for Markdown-formatted text files
;  paredit            22           installed  minor mode for editing parentheses  -*- Mode: Emacs-Lisp -*-
;  parenface          1.1          installed  Provide a face for parens in lisp modes.
;  slime              20100404.1   installed  Superior Lisp Interaction Mode for Emacs


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

(isg-frame-setup)
(run-isg-machine-function 'post-setup-fn)
(require 'global-settings)

