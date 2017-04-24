(require 'package)
(setq package-enable-at-startup nil)
(setcdr (last package-archives)
        '(("melpa-stable" . "https://stable.melpa.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))

(package-initialize) ; most of this section's time is spent here
(setq package-check-signature nil)

(unless (package-installed-p 'use-package)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (package-install 'use-package))

;(require 'use-package)

;; after use-package-always-ensure is set, all subsequent use-package
;; statements will download packages if needed
(setq use-package-always-ensure t)
(setq use-package-verbose t)

(require 'cl)
(push "~/.emacs.d/external" load-path)  ; third party code that isn't in melpa-stable yet

(require 'pomidor)
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil
      pomidor-sound-overwork nil)
(global-set-key (kbd "<f12>") #'pomidor)

;;; display 'fn' as the lambda symbol
(defun pretty-fn nil 
  (font-lock-add-keywords
   nil `(("(\\(fn\\>\\)" 
          (0 (progn (compose-region (match-beginning 1) (match-end 1) 
                                    ,(make-char 'greek-iso8859-7 107))
                    nil)))
         ("\\(#\\)("
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    402)
                    nil))))))

(defun isg/val (property)
  (get 'isg/local property))

(defun isg/start-shell (shell-name)
  "start a new shell"
  (interactive "sShell name: ")
  (shell)
  (if (string= "" shell-name)
      (rename-uniquely)
    (rename-buffer shell-name)))

(defun isg/default-machine-settings ()
  "Settings which apply to most of the machines."
  '((foreground-color "grey60")
    (background-color "black")
    (default-font "6x12")
    (machine-os "linux") ; one of "linux" "osx" "windows"
    (url-opener "chromium-browser")
    (save-folder "/tmp/emacs.d-saves")
    (deft-directory "~/notes/deft")
    (racer-cmd "/home/indy/code/rust/racer/target/release/racer")
    (racer-rust-src-path "/home/indy/code/rust/rust/src/")
    (get-extra-paths (lambda ()
                       (list (concat (getenv "HOME") "/local/bin")
                             "/usr/local/bin"
                             "/usr/local/go/bin")))))

(defun isg/machine-settings ()
  "System specific overrides go here."
  (cond
   ((string-match "^BERTRAND" system-name)  ; 2016 Windows PC
    '((default-font "-outline-Courier New-normal-normal-normal-mono-13-*-*-*-c-*-fontset-startup")
      (machine-os "windows") ; one of "linux" "osx" "windows"
      (racer-cmd "C:\\Users\\indy\\bin\\racer.exe")
      (racer-rust-src-path "c:\\Users\\indy\\.rustup\\toolchains\\nightly-x86_64-pc-windows-msvc\\lib\\rustlib\\src\\rust\\src\\")
      (save-folder "d:/scratch/emacs-saves")
      (url-opener "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome")
      (deft-directory "d:/Google Drive/Docs/notes/deft")
      (frame-l ((top . 0) (left . 600) (width . 120) (height . 87)))
      (frame-r ((top . 0) (left . 1575) (width . 120) (height . 87)))))

   ((string-match "^localhost" system-name)  ; chromebook
    '((post-setup-fn (lambda ()
                       (setenv "GOPATH" (concat (getenv "HOME") "/work/go"))))
      (get-extra-paths (lambda ()
                         (list (concat (getenv "GOPATH") "/bin")
                               (concat (getenv "HOME") "/local/bin")
                               "/usr/local/bin")))
      (frame-r ((top . 0) (left . 780) (width . 80) (height . 59)))
      (frame-l ((top . 0) (left . 210) (width . 80) (height . 59)))))

   ((string-match "^debian" system-name)  ; debian vm on ernesto
    '((post-setup-fn (lambda ()
                       (setq x-super-keysym 'meta)))))

   ((string-match "^ernesto" system-name) ; Macbook Air i5
    '((default-font "-apple-Inconsolata-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
      (machine-os "osx")
      (racer-cmd "/Users/indy/code/rust/racer/target/release/racer")
      (racer-rust-src-path "/Users/indy/code/rust/rust/src/")
      (url-opener "open")

      (frame-r ((top . 0) (left . 746) (width . 100) (height . 55)))
      (frame-l ((top . 0) (left . 126) (width . 100) (height . 55)))

      ;;(frame-r ((top . 0) (left . 864) (width . 80) (height . 55)))
      ;;(frame-l ((top . 0) (left . 362) (width . 80) (height . 55)))

      (post-setup-fn (lambda ()
                       (setq inferior-lisp-program "lein repl")))))

   ((string-match "^che" system-name)  ; asus ul20a
    '((post-setup-fn (lambda ()
                       (setenv "GOPATH" (concat (getenv "HOME") "/scratch/go"))))
      (get-extra-paths (lambda ()
                         (list (concat (getenv "GOPATH") "/bin")
                               (concat (getenv "HOME") "/local/bin")
                               "/usr/local/bin"
                               "/usr/local/go/bin"
                               "/home/indy/code/rust/racer/target/release")))
      
      (key-setup (([mouse-1] . nil)
                  ([double-mouse-1] . nil)
                  ([drag-mouse-1] . nil)
                  ([down-mouse-1] . nil)))))

   
   ((string-match "^raul" system-name)  ; eee 1000
    '((hyperspec-root "file:////home/user/docs/cl/HyperSpec/")
      ;; (default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-20-*-*-*-m-0-fontset-startup")
      (post-setup-fn (lambda ()
                       (setenv "PATH"
                               (concat
                                (concat (getenv "HOME") "/local/bin:")
                                "/usr/local/bin:" 
                                (getenv "PATH")))
;                        (isg/start-shell "default-shell")
                       (switch-to-buffer "*scratch*")))


      (frame-r ((top . 1) (left . 5) (width . 80) (height . 46)))
      (frame-l ((top . 0) (left . 509) (width . 80) (height . 46)))

      (key-setup (([mouse-1] . nil) ; accidently touching touchpad won't shift cursor
                  ([double-mouse-1] . nil)
                  ([drag-mouse-1] . nil)
                  ([down-mouse-1] . nil)))))

   
   ((string-match "^blue" system-name) ; G5 iMac at home
    '((default-font "-apple-andale mono-medium-r-normal--0-0-0-0-m-0-mac-roman")
      (machine-os "osx")
      (save-folder "~/.emacs.d/saves")
      (frame-r ((top . 20) (left . 320) (width . 80) (height . 71)))
      (frame-l ((top . 20) (left . 902) (width . 80) (height . 71)))))

   ((string-match "^GOSHCC" system-name)  ; GOSH PC
    '(
      (default-font "-outline-Courier New-normal-normal-normal-mono-13-*-*-*-c-*-fontset-startup")
      (machine-os "windows") ; one of "linux" "osx" "windows"
      (save-folder "~/emacs-saves")
      (post-setup-fn (lambda ()
                       (find-file "k:/Direct Marketing/Data Analyst/indy/notes/done.org")
                       (find-file "k:/Direct Marketing/Data Analyst/indy/notes/notes.org")
                       (find-file "k:/Direct Marketing/Data Analyst/indy/notes/tasks.org")))

      (frame-r ((top . 0) (left . 0) (width . 80) (height . 60)))
      (frame-l ((top . 20) (left . 510) (width . 80) (height . 60)))))
   

   ((equal system-name "green")         ; old laptop thrown out by orange
    '((hyperspec-root "file:///usr/share/common-lisp/isg/hyperspec/HyperSpec/")))))

; Windows
; default font: "file:///usr/share/common-lisp/isg-hyperspec/HyperSpec/"
; inferior lisp: "C:\\home\\bin\\sbcl\\sbcl.exe"
; (post-setup-fn (lambda ()
; ((setq exec-path (append exec-path '("c:\\home\\bin\\emacs-22.1\\bin")))
; (load-file "~/.emacs.d/site-lisp/external/gnuserv.el")
; (gnuserv-start)
; (setq gnuserv-frame(selected-frame)))))

(cl-labels ((load-settings (which)
                           (mapcar (lambda (pair)
                                     (put 'isg/local (car pair) (cadr pair)))
                                   which)))
  (load-settings (isg/default-machine-settings))
  (load-settings (isg/machine-settings)))

(use-package magit
  :pin melpa-stable
  :commands magit-status
  :init
  (global-set-key "\C-cv" 'magit-status)
  (global-set-key "\C-c\C-v" 'magit-status)
  :config
  (setq magit-push-always-verify nil))

(use-package org
  :pin gnu
  :demand t)

;;; org-mode
(defun soft-wrap-lines ()
  "Make lines wrap at window edge and on word boundary, in current buffer."
  (interactive)
  (setq truncate-lines nil)
  (setq word-wrap t))
(add-hook 'org-mode-hook 'soft-wrap-lines)

(use-package hydra
  :pin melpa-stable
  :ensure t
  :config
  (setq hydra-lv nil)) ;use echo area

(use-package ivy
  :pin melpa-stable
  :demand t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))


(use-package swiper
  :pin melpa-stable
  :init
  (global-set-key (kbd "C-s")
                  (lambda ()
                    (interactive)
                    (swiper (format "%s" (or (thing-at-point 'symbol) ""))))))

(use-package counsel
  :pin melpa-stable
  :init
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-M-s")
                  (lambda ()
                    (interactive)
                    (counsel-git-grep nil
                                      (format "%s" (or (thing-at-point 'symbol) "")))))
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)

  ;; use the hydra equivalents instead (C-c f ...)
  ;;
  ;; (global-set-key (kbd "C-x C-g") 'counsel-git)
  ;; (global-set-key (kbd "C-x C-r") 'counsel-rg)

  ;; I don't use these bindings - should learn what they do one day
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))


(defhydra isg/hydra-counsel (:hint nil :color pink)
  "
Counsel search
----------------------------------------------------
_r_: ripgrep  _f_: find file _g_: git
_u_: function _v_: variable  _l_: library _s_: symbol
"
  ("f" counsel-find-file)
  ("g" counsel-git)
  ("r" (lambda ()
         (interactive)
         (counsel-rg (format "%s" (or (thing-at-point 'symbol) "")))))
  ("u" counsel-describe-function)
  ("v" counsel-describe-variable)
  ("l" counsel-find-library)
  ("s" counsel-info-lookup-symbol)
  ("RET" nil "done" :color blue))

(bind-keys ("C-c f"  . isg/hydra-counsel/body))

(add-hook 'c-mode-hook (lambda ()
                         (setq comment-start "// "
                               comment-end   "")
                         (highlight-thing-mode)))

(use-package cider
  :pin melpa-stable
  :defer t
  :init
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(use-package clojure-mode
  :pin melpa-stable
  :mode "\\.clj\\'"
  :config
  (define-key clojure-mode-map (kbd ")") 'sp-up-sexp)
  (pretty-fn))

(setq nrepl-hide-special-buffers t)

(use-package csharp-mode
  :pin melpa-stable
  :mode "\\.cs\\'"
  :init
  :config
  (setq default-tab-width 4))

(use-package css-mode
  :pin melpa-stable
  :mode (("\\.css\\'" . css-mode)
         ("\\.less\\'" . css-mode))
  :config
  (use-package rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (setq css-indent-offset 2))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode))
  :init
  (autoload 'glsl-mode "glsl-mode" nil t))

(use-package go-mode
  :pin melpa-stable
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))

(use-package js2-mode
  :pin melpa-stable
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

(use-package web-mode
  :pin melpa-stable
  :mode "\\.jsx\\'"
  :init
  (add-hook 'web-mode-hook 'ws-butler-mode)
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(use-package markdown-mode
  :pin melpa-stable
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

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
  :pin melpa-stable
  :mode "\\.rs\\'"
  :config
  (use-package cargo)
  (use-package flycheck-rust
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (use-package racer
    :init
    (setq racer-cmd (isg/val 'racer-cmd)
          racer-rust-src-path (isg/val 'racer-rust-src-path))
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

(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map (kbd ")") 'sp-up-sexp)))

(autoload 'seni-mode "seni" nil t)
(add-to-list 'auto-mode-alist '("\\.seni$" . seni-mode))
(add-hook 'seni-mode-hook 'smartparens-strict-mode)
(add-hook 'seni-mode-hook
          (lambda ()
            (define-key seni-mode-map (kbd ")") 'sp-up-sexp)))

(use-package shader-mode
  :mode "\\.shader\\'"
  :config
  (setq shader-indent-offset 2))

(add-to-list 'auto-mode-alist
             '("\\.sql$" . (lambda ()
                             (sql-mode)
                             (sql-highlight-postgres-keywords))))
(add-to-list 'auto-mode-alist
             '("\\.psql$" . (lambda ()
                             (sql-mode)
                             (sql-highlight-postgres-keywords))))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package typescript-mode
  :pin melpa-stable
  :mode "\\.ts\\'")

(defun isg/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defvar isg/theme-hooks nil
  "((theme-id . function) ...)")

(defun isg/add-theme-hook (theme-id hook-func)
  (add-to-list 'isg/theme-hooks (cons theme-id hook-func)))

(defun isg/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `isg/add-theme-hook'."
  (unless no-enable
    (isg/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id isg/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'isg/load-theme-advice)

(use-package color-theme
  :defer t)

(use-package color-theme-sanityinc-solarized
  :pin melpa-stable
  :defer t)

(use-package material-theme
  :pin melpa-stable
  :defer t)

(use-package dracula-theme
  :pin melpa-stable
  :defer t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'actress t)

(defhydra isg/hydra-themes (:hint nil :color pink)
  "
Themes
----------------------------------------------------
_A_: Actress _M_: Material       _S_: Solarized
_D_: Dracula _m_: Material Light _s_: Solarized light
_DEL_: none
"
  ("A" (load-theme 'actress t))
  ("D" (load-theme 'dracula t))
  ("s" (load-theme 'sanityinc-solarized-light t))
  ("S" (load-theme 'sanityinc-solarized-dark t))
  ("M" (load-theme 'material t))
  ("m" (load-theme 'material-light t))
  ("DEL" (isg/disable-all-themes))
  ("RET" nil "done" :color blue))

(bind-keys ("C-c w t"  . isg/hydra-themes/body))

(use-package auto-complete
  :commands auto-complete-mode
  :config
  (ac-set-trigger-key "TAB")
  (ac-config-default)

  (setq ac-delay 0.02)
  (setq ac-use-menu-map t)
  (setq ac-menu-height 50)
  (setq ac-use-quick-help nil) 
  (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
  (setq ac-ignore-case nil)
  (setq ac-dwim  t)
  (setq ac-fuzzy-enable t)

  (use-package ac-dabbrev
    :config
    (progn
      (add-to-list 'ac-sources 'ac-source-dabbrev))))

(use-package avy
  :pin melpa-stable
  :bind ("M-h" . avy-goto-char-timer))

(use-package company
  :pin melpa-stable
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

(use-package company-racer
  :defer t)

(use-package deft
  :pin melpa-stable
  :commands deft
  :config
  (setq deft-directory (isg/val 'deft-directory)
        deft-extension "org"
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-auto-save-interval 5.0))

;; have to ensure that this is run at startup so that 'cargo' can be
;; found when in rust mode and also so that the eshell works as expected
;;
(use-package exec-path-from-shell
  :pin melpa-stable
    :ensure t
    :demand t
    :config
    ;; (exec-path-from-shell-copy-env "TWITTER_CONSUMER_KEY")
    ;; (exec-path-from-shell-copy-env "TWITTER_CONSUMER_SECRET")
    ;; (exec-path-from-shell-copy-env "TWITTER_ACCESS_TOKEN")
    ;; (exec-path-from-shell-copy-env "TWITTER_ACCESS_TOKEN_SECRET")
    (if (not (string-equal (isg/val 'machine-os) "windows"))
        (exec-path-from-shell-copy-env "GOPATH"))
    (exec-path-from-shell-setenv "RUST_SRC_PATH" (isg/val 'racer-rust-src-path))
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize)))

;; setting up flycheck for eslint checks using instructions from:
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;
;; C-c ! l : see full list of errors
;; C-c ! n : next error
;; C-c ! p : previous error
(use-package flycheck
  :pin melpa-stable
  :config
  (setq-default flycheck-disabled-checkers
                (list 'json-jsonlist
                      'javascript-jshint ;; disable jshint since we prefer eslint checking
                      'emacs-lisp-checkdoc))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; (setq flycheck-eslintrc "~/work/seni-web/.eslintrc.json")
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck"))

(use-package highlight-thing
  :config
  (setq highlight-thing-delay-seconds 0.5)
  (setq highlight-thing-exclude-thing-under-point t))

(use-package htmlize
  :pin melpa-stable
  :commands htmlize-buffer)

(use-package pkg-info
  :pin melpa-stable
  :defer t)

(use-package popup
  :pin melpa-stable
  :defer t)

(use-package simple-httpd
  :pin melpa-stable
  :defer t)

(use-package smartparens-config
  :pin melpa-stable
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

(use-package which-key
  :pin melpa-stable
  :demand t
  :config
  (which-key-mode))

(use-package ws-butler
  :pin melpa-stable
  :defer t)

(defun isg/frame-setup ()
  (when (and (isg/val 'frame-r) (isg/val 'frame-l))
    (setq initial-frame-alist (isg/val 'frame-r)
          default-frame-alist (isg/val 'frame-l)))
  (add-to-list 'default-frame-alist
               (cons 'font
                     (isg/val 'default-font))))

(defun isg/run-machine-function (property)
  (let ((fn (isg/val property)))
    (if fn (funcall fn))))

(isg/frame-setup)
(isg/run-machine-function 'post-setup-fn)

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

(if (not (file-exists-p (isg/val 'save-folder)))
    (make-directory (isg/val 'save-folder)))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(when (fboundp 'winner-mode)
  (winner-mode 1))

(defalias 'list-buffers 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("c" (mode . c-mode))
               ("rust" (mode . rust-mode))
               ("js" (mode . js2-mode))
               ("org" (mode . org-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ring-bell-function (lambda () (message "*beep*"))

      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (isg/val 'url-opener)

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
 ((string-match "osx" (isg/val 'machine-os))
  (setq
   mac-command-modifier 'meta
   default-directory "~/"
   multi-term-program "/bin/bash")
  
  (fset 'insertPound "#")
  (global-set-key (kbd "C-M-3") 'insertPound))
 ((string-match "linux" (isg/val 'machine-os))
  (setq
   default-directory "~/"
   multi-term-program "/bin/bash")))

(new-frame)

;; C-c   == user defined prefixes
;; C-c w == window related functions

(defhydra isg/hydra-text-scale (:hint nil :color pink)
  "
Text Scale
----------------------------------------------------
_g_: greater
_l_: lesser
"
  ("g" text-scale-increase)
  ("l" text-scale-decrease)
  ("RET" nil "done" :color blue))

(bind-keys ("C-c w s"  . isg/hydra-text-scale/body))

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

(defun isg/start-eshell (shell-name)
  "SHELL-NAME the name of the shell."
  (interactive "sEshell name: ")
  (eshell)
  (if (string= "" shell-name)
      (rename-uniquely)
    (rename-buffer shell-name)))
;;; access server via ssh in eshell with:
;;; $ cd /ssh:indy.io:

(global-set-key "\M-7" 'isg/start-shell)
(global-set-key "\M-8" 'isg/start-eshell)

(global-set-key (kbd "<up>") 'scroll-down-line)
(global-set-key (kbd "<down>") 'scroll-up-line)
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)

(global-set-key (kbd "C-<return>") 'electric-newline-and-maybe-indent)

;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(defun isg/machine-set-keys ()
  "set machine specific key bindings"
  (mapcar (lambda (kons)
            (global-set-key (car kons) (cdr kons)))
          (isg/val 'key-setup)))
(isg/machine-set-keys)                 ; machine specific key bindings


;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'tramp)
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plinkx"))

(auto-complete-mode)

; (server-start)

;(use-package atomic-chrome
;  :config
;  (atomic-chrome-start-server))
;(require 'atomic-chrome)
;(atomic-chrome-start-server)
