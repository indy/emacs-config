;; packages that this config doesn't use anymore:
;; edit-server-20141231.1358

;; timing code
(defvar isg-section-start-time (float-time))
(defvar isg-section-end-time (float-time))
(defvar isg-timings '())

(defun isg-time-section (msg)
  (setq isg-section-end-time (float-time))
  (add-to-list 'isg-timings
               (cons msg (format "%.3f" (- isg-section-end-time
                                           isg-section-start-time))) t)
  (setq isg-section-start-time (float-time)))

;;; essential package
(require 'cl)
(require 'org)

;(when (memq window-system '(mac ns))
;  (exec-path-from-shell-initialize))

;;; location of my customisations
(push "~/.emacs.d/site-lisp" load-path)
(push "~/.emacs.d/external" load-path)  ; third party code that hasn't
                                        ; been packaged yet
(isg-time-section "initial essential setup")
;; ----------------------------------------------------------------------------


(require 'helper-functions)
(isg-time-section "loading helper functions")
;; ----------------------------------------------------------------------------


(require 'machine-settings)

(cl-labels ((load-settings (which)
                           (mapcar (lambda (pair)
                                     (put 'isg-local (car pair) (cadr pair)))
                                   which)))
  (load-settings (isg-default-machine-settings))
  (load-settings (isg-machine-settings)))
(isg-time-section "machine-settings")
;; ----------------------------------------------------------------------------

(require 'package)
(setcdr (last package-archives)
        '(("marmalade" . "https://marmalade-repo.org/packages/")
          ("melpa" . "https://melpa.milkbox.net/packages/")
          ("melpa-stable" . "https://melpa-stable.milkbox.net/packages/")))

(package-initialize) ; most of this section's time is spent here

(defvar prelude-packages
  '(use-package)
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

;; see: https://github.com/jwiegley/use-package
(require 'use-package)

;; after use-package-always-ensure is set, all subsequent use-package
;; statements will download packages if needed
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(isg-time-section "loading use-package")
;; ----------------------------------------------------------------------------

(use-package ag
  :commands (ag ag-regexp)
  :init
  (use-package helm-ag
    :commands helm-ag)
  (global-set-key "\C-\M-s"  'ag-project)
  :config
  ;; quick fix so that C-M-s ignores the dist folder in seni-web
  (setq ag-ignore-list (list "dist")))

(isg-time-section "ag")
;; ----------------------------------------------------------------------------

(use-package auto-complete-config
  :ensure auto-complete
  :defer t)

(isg-time-section "auto-complete-config")
;; ----------------------------------------------------------------------------


(use-package avy
  :bind ("M-h" . avy-goto-char-timer))

(isg-time-section "avy")
;; ----------------------------------------------------------------------------


(use-package cider
  :defer t
  :init
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(isg-time-section "cider")
;; ----------------------------------------------------------------------------


(use-package clojure-mode
  :mode "\\.clj\\'"
  :init
  (use-package clojure-cheatsheet :defer t)
  :config
  (define-key clojure-mode-map (kbd ")") 'sp-up-sexp)
  (pretty-fn))

(isg-time-section "clojure-mode")
;; ----------------------------------------------------------------------------


(use-package clojurescript-mode
  :mode "\\.cljs\\'")

(isg-time-section "clojurescript-mode")
;; ----------------------------------------------------------------------------


(use-package color-theme :defer t)

(isg-time-section "color-theme")
;; ----------------------------------------------------------------------------


(use-package company
  :defer t
  :config
  (eval-after-load 'company
    '(progn
       (add-to-list 'company-backends 'company-racer)

       ;; Reduce the time after which the company auto completion popup opens
       ;; Reduce the number of characters before company kicks in
       (setq company-idle-delay 0.2
             company-minimum-prefix-length 1
             company-tooltip-align-annotations t)
       
       (define-key company-active-map (kbd "C-n") 'company-select-next)
       (define-key company-active-map (kbd "C-p") 'company-select-previous))))

(isg-time-section "company")
;; ----------------------------------------------------------------------------


(use-package company-racer :defer t)

(isg-time-section "company-racer")
;; ----------------------------------------------------------------------------

(use-package csharp-mode
  :mode "\\.cs\\'"
  :init
  :config
  (setq default-tab-width 4))

(isg-time-section "csharp-mode")



(use-package css-mode
  :mode (("\\.css\\'" . css-mode)
         ("\\.less\\'" . css-mode))
  :config
  (use-package rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (setq css-indent-offset 2))

(isg-time-section "css-mode")
;; ----------------------------------------------------------------------------


(use-package dash :defer t)

(isg-time-section "dash")
;; ----------------------------------------------------------------------------


(use-package deft
  :commands deft
  :config
  (setq deft-directory "~/notes/deft"
        deft-extension "org"
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-auto-save-interval 5.0))

(isg-time-section "deft")
;; ----------------------------------------------------------------------------


;; have to ensure that this is run at startup so that 'cargo' can be
;; found when in rust mode and also so that the eshell works as expected
;;
(use-package exec-path-from-shell
    :ensure t
    :demand t
    :config
    ;; (exec-path-from-shell-copy-env "TWITTER_CONSUMER_KEY")
    ;; (exec-path-from-shell-copy-env "TWITTER_CONSUMER_SECRET")
    ;; (exec-path-from-shell-copy-env "TWITTER_ACCESS_TOKEN")
    ;; (exec-path-from-shell-copy-env "TWITTER_ACCESS_TOKEN_SECRET")
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "RUST_SRC_PATH")
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(isg-time-section "expand-region")
;; ----------------------------------------------------------------------------


(use-package fill-column-indicator
  :commands fci-mode
  :config
  (setq fci-rule-color "#0f2f2f"))

(isg-time-section "fill-column-indicator")
;; ----------------------------------------------------------------------------


;; setting up flycheck for eslint checks using instructions from:
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;
;; C-c ! l : see full list of errors
;; C-c ! n : next error
;; C-c ! p : previous error
(use-package flycheck
  :config
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; (setq flycheck-eslintrc "~/work/seni-web/.eslintrc.json")

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist))))

(isg-time-section "flycheck")
;; ----------------------------------------------------------------------------


(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode))
  :init
  (autoload 'glsl-mode "glsl-mode" nil t))

(isg-time-section "glsl-mode")
;; ----------------------------------------------------------------------------


(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(isg-time-section "go-mode")
;; ----------------------------------------------------------------------------


(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (require 'helm-config)
  (setq helm-candidate-number-limit 100)
  ;; From https://gist.github.com/antifuchs/9238468
  (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
        helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
        helm-yas-display-key-on-candidate t
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t
        helm-boring-file-regexp-list '("\\.\\.$" "\\.$" "\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$"  "\\.la$" "\\.o$" "~$" "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$")
        helm-display-header-line nil
        helm-ff-guess-ffap-urls nil
        helm-autoresize-max-height 30
        helm-autoresize-min-height 30)
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-j") 'helm-semantic-or-imenu)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB works in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  ;; helm crap
  (set-face-attribute 'helm-source-header nil :height 0.1)
  (helm-autoresize-mode 1))

(isg-time-section "helm")
;; ----------------------------------------------------------------------------


(use-package helm-ls-git
  :bind ("\C-x\C-g" . helm-browse-project)
  :defer t)

(isg-time-section "helm-ls-git")
;; ----------------------------------------------------------------------------


(use-package helm-themes
  :ensure t)
(isg-time-section "helm-themes")
;; ----------------------------------------------------------------------------


(use-package htmlize
  :commands htmlize-buffer)

(isg-time-section "htmlize")
;; ----------------------------------------------------------------------------


(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.es6\\'" . js2-mode))
  :init
  (use-package js-comint :defer t)
  (add-hook 'js2-mode-hook 'company-mode)
  (add-hook 'js2-mode-hook 'ws-butler-mode)
  :config
  (setq js2-basic-offset 2)
  (setq js-indent-level 2)
  (setq js2-global-externs '("require" "expect" "describe" "it" "beforeEach"))
  (define-key js2-mode-map (kbd "<tab>") #'company-indent-or-complete-common))

(isg-time-section "js2-mode")
;; ----------------------------------------------------------------------------


(use-package magit
  :commands magit-status
  :init
  (global-set-key "\C-cv" 'magit-status)
  (global-set-key "\C-c\C-v" 'magit-status)
  :config
  (setq magit-push-always-verify nil))

(isg-time-section "magit")
;; ----------------------------------------------------------------------------


(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(isg-time-section "markdown-mode")
;; ----------------------------------------------------------------------------


(use-package parenface :defer t)

(isg-time-section "parenface")
;; ----------------------------------------------------------------------------


(use-package pkg-info :defer t)

(isg-time-section "pkg-info")
;; ----------------------------------------------------------------------------


(use-package popup :defer t)

(isg-time-section "popup")
;; ----------------------------------------------------------------------------


;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/
;;
;;
;; cargo.el is a minor mode which allows us to run cargo commands from emacs like:
;;
;; C-c C-c C-b to run cargo build
;; C-c C-c C-r to run cargo run
;; C-c C-c C-t to run cargo test
;;
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (use-package cargo)
  (use-package flycheck-rust
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (use-package racer
    :init
    (if (string-match "osx" (isg-val 'machine-os))
        (setq racer-cmd "/Users/indy/code/rust/racer/target/release/racer"
              racer-rust-src-path "/Users/indy/code/rust/rust/src/")
      (setq racer-cmd "/home/indy/code/rust/racer/target/release/racer"
            racer-rust-src-path "/home/indy/code/rust/rust/src/"))
    :config
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode))
  (add-hook 'rust-mode-hook
            '(lambda ()
               (racer-mode)
               (cargo-minor-mode)
               ;; Key binding to jump to method definition
               (local-set-key (kbd "M-.") #'racer-find-definition)
               (local-set-key (kbd "<tab>") #'company-indent-or-complete-common)
               (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))

(isg-time-section "rust-mode")
;; ----------------------------------------------------------------------------


(use-package simple-httpd :defer t)

(isg-time-section "simple-httpd")
;; ----------------------------------------------------------------------------


(use-package smartparens-config
  :ensure smartparens
  :defer t
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (mapc (lambda (mode)
          (add-hook (intern (format "%s-hook" (symbol-name mode))) 'smartparens-strict-mode))
        sp--lisp-modes)
  ;; Conveniently set keys into the sp-keymap, limiting the keybinding to buffers
  ;; with SP mode activated
  (mapc (lambda (info)
          (let ((key (kbd (car info)))
                (function (car (cdr info))))
            (define-key sp-keymap key function)))
        '(("C-)" sp-up-sexp)
          ("M-s" sp-splice-sexp)

          ("C-M-f" sp-forward-sexp)
          ("C-M-b" sp-backward-sexp)
          
          ("C-M-d" sp-down-sexp)
          ("C-M-a" sp-backward-down-sexp)
          ("C-S-a" sp-beginning-of-sexp)
          ("C-S-d" sp-end-of-sexp)
          
          ("C-M-e" sp-up-sexp)
          
          ("C-M-u" sp-backward-up-sexp)
          ("C-M-t" sp-transpose-sexp)
          
          ("C-M-n" sp-next-sexp)
          ("C-M-p" sp-previous-sexp)
          
          ("C-M-k" sp-kill-sexp)
          ("C-M-w" sp-copy-sexp)
          
          ("M-<delete>" sp-unwrap-sexp)
          ("M-<backspace>" sp-backward-unwrap-sexp)
          
          ("C-<right>" sp-forward-slurp-sexp)
          ("C-<left>" sp-forward-barf-sexp)
          ("C-M-<left>" sp-backward-slurp-sexp)
          ("C-M-<right>" sp-backward-barf-sexp)
          
          ("C-M-<delete>" sp-splice-sexp-killing-forward)
          ("C-M-<backspace>" sp-splice-sexp-killing-backward)
          ("C-S-<backspace>" sp-splice-sexp-killing-around)

          ("C-]" sp-select-next-thing-exchange)
          ("C-<left_bracket>" sp-select-previous-thing)
          ("C-M-]" sp-select-next-thing)
          
          ("M-F" sp-forward-symbol)
          ("M-B" sp-backward-symbol)
          
          ("H-t" sp-prefix-tag-object)
          ("H-p" sp-prefix-pair-object)
          ("H-s c" sp-convolute-sexp)
          ("H-s a" sp-absorb-sexp)
          ("H-s e" sp-emit-sexp)
          ("H-s p" sp-add-to-previous-sexp)
          ("H-s n" sp-add-to-next-sexp)
          ("H-s j" sp-join-sexp)
          ("H-s s" sp-split-sexp)))

  ;; This is from authors config, seems to let you jump to the end of the current
  ;; sexp with paren?
  (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
  ;; (define-key scheme-mode-map (kbd ")") 'sp-up-sexp)
  ;; (define-key clojure-mode-map (kbd ")") 'sp-up-sexp)
  )

(isg-time-section "smartparens-config")
;; ----------------------------------------------------------------------------


(use-package toml-mode
  :mode "\\.toml\\'")

(isg-time-section "toml-mode")
;; ----------------------------------------------------------------------------


(use-package typescript-mode
  :mode "\\.ts\\'")

(isg-time-section "typescript")
;; ----------------------------------------------------------------------------


(use-package web-mode
  :mode "\\.jsx\\'"
  :init
  (add-hook 'web-mode-hook 'ws-butler-mode)
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(isg-time-section "web-mode")
;; ----------------------------------------------------------------------------


(use-package ws-butler :defer t)

(isg-time-section "ws-butler")
;; ----------------------------------------------------------------------------

(autoload 'seni-mode "seni" nil t)
(add-to-list 'auto-mode-alist '("\\.seni$" . seni-mode))
(add-hook 'seni-mode-hook 'smartparens-strict-mode)


(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
(add-to-list 'auto-mode-alist 
             '("\\.sql$" . (lambda ()
                             (sql-mode)
                             (sql-highlight-postgres-keywords))))
(add-to-list 'auto-mode-alist 
             '("\\.psql$" . (lambda ()
                             (sql-mode)
                             (sql-highlight-postgres-keywords))))

(autoload 'imbue-mode "imbue" nil t)
(add-to-list 'auto-mode-alist '("\\.imd$" . imbue-mode))

;;; org-mode
(add-hook 'org-mode-hook 'soft-wrap-lines)
(defun soft-wrap-lines ()
  "Make lines wrap at window edge and on word boundary,
in current buffer."
  (interactive)
  (setq truncate-lines nil)
  (setq word-wrap t))

;;; scheme
(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map (kbd ")") 'sp-up-sexp)))

(add-hook 'seni-mode-hook
          (lambda ()
            (define-key seni-mode-map (kbd ")") 'sp-up-sexp)))


; hide the *nrepl-connection* and *nrepl-server* buffers
(setq nrepl-hide-special-buffers t)
(isg-time-section "misc. mode configs")
;; ----------------------------------------------------------------------------


(isg-frame-setup)
(run-isg-machine-function 'post-setup-fn)
(isg-time-section "frame setup")
;; ----------------------------------------------------------------------------


(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

; give buffers unique names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


; reload files that have been changed outside of emacs (e.g. Eclipse autoformat)
(global-auto-revert-mode t)


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

(setq ring-bell-function (lambda () (message "*beep*"))

      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (isg-val 'url-opener)

      standard-indent 2

      create-lockfiles nil        ; don't create lockfiles

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
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t           ; use versioned backups
      create-lockfiles nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

(new-frame)
(isg-time-section "global settings")
;; ----------------------------------------------------------------------------


;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key "\C-x!" 'delete-other-windows)

;;; org mode overrides the default "Shift-cursor keys" navigation
;;; of windmove and for some reason re-inserting the original keyboard
;;; commands in the org-load-hook or org-mode-hook doesn't revert back
;;; the behaviour. So I'm giving up, assigning new keys for windmove
;;; and setting them as globals:
(global-set-key "\M-1"  'windmove-left)
(global-set-key "\M-2"  'windmove-down)
(global-set-key "\M-3"  'windmove-up)
(global-set-key "\M-4"  'windmove-right)

(global-set-key "\M-5"  'winner-undo)
(global-set-key "\M-6"  'winner-redo)

;;; use winner mode keys for undo/redo operations on window configurations
;;; C-c left 
;;; C-c right
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\M-0" 'other-frame)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-m" 'call-last-kbd-macro)
(global-set-key "\M-j" 'eval-print-last-sexp)

(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key "\C-c\C-f" 'flymake-mode)

(global-set-key (kbd "C-M-;") 'comment-region)

(defun isg-start-eshell (shell-name)
  (interactive "sEshell name: ")
  (eshell)
  (if (string= "" shell-name)
      (rename-uniquely)
    (rename-buffer shell-name)))

(global-set-key "\M-7" 'isg-start-shell)
(global-set-key "\M-8" 'isg-start-eshell)

(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)
(global-set-key "\M-[" 'backward-paragraph)
(global-set-key "\M-]" 'forward-paragraph)
(global-set-key (kbd "<M-up>") 'backward-paragraph)
(global-set-key (kbd "<M-down>") 'forward-paragraph)

(global-set-key "\C-x\C-f" 'helm-find-files)

(global-set-key (kbd "C-<return>") 'electric-newline-and-maybe-indent)

;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(isg-machine-set-keys)                 ; machine specific key bindings

(isg-time-section "keyboard config")
;; ----------------------------------------------------------------------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4d0c1008debaa663eae9ecd86cdd56ca35e65a225b6fbd90d2e359b6acb2226a" default)))
 '(package-selected-packages
   (quote
    (cargo exec-path-from-shell ws-butler web-mode use-package typescript toml-mode smartparens simple-httpd rainbow-mode racer parenface markdown-mode magit js2-mode js-comint htmlize go-mode find-file-in-git-repo edit-server deft company-racer color-theme clojurescript-mode clojure-cheatsheet avy auto-complete ag helm-themes helm-ls-git glsl-mode flycheck expand-region helm-ag flycheck-rust))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'actress)

(isg-time-section "theme")
; (message "startup time %s." (emacs-init-time))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

(server-start)

(provide 'init)
;;; init.el ends here
