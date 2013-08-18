(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;;; copied from http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

(defvar prelude-packages
  '(ac-nrepl
    ace-jump-mode
    auto-complete
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    coffee-mode
    color-theme
    color-theme-actress
    dash
    deft
    edit-server
    find-file-in-git-repo
    git-commit-mode
    git-rebase-mode
    go-mode
    groovy-mode
    haml-mode
    htmlize
    ido-ubiquitous
    impatient-mode
    js-comint
    lua-mode
    magit
    markdown-mode
    nrepl
    paredit
    parenface
    php-mode
    popup
    rainbow-mode
    sass-mode
    simple-httpd
    slime
    yasnippet)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'package-manager)
