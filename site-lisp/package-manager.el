(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;; the bleeding edge melpa library can be added with:
;
; (add-to-list 'package-archives
;              '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)

;;; copied from http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

(defvar prelude-packages
  '(ace-jump-mode
    ag
    auto-complete
    cider
    clojure-cheatsheet
    clojure-mode
    clojurescript-mode
    color-theme
    company
    dash
    deft
    edit-server
    find-file-in-git-repo
    git-commit-mode
    git-rebase-mode
    go-mode
    helm
    htmlize
    js-comint
    js2-mode
    magit
    markdown-mode
    parenface
    pkg-info
    popup
    rainbow-mode
    rust-mode
    simple-httpd
    smartparens
    toml-mode
    typescript
    web-mode
    ws-butler)
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
