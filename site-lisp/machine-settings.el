
(defun isg-default-machine-settings ()
  "settings which apply to most of the machines apart from 1 or 2 stragglers"
  '((foreground-color "grey60")
    (background-color "black")
    (default-font "6x12")
    (machine-os "linux") ; one of "linux" "osx" "windows"
    (save-folder (("." . "/tmp/emacs.d-saves")))
    (extra-exec-paths ("/usr/local/bin"))))

(defun isg-machine-settings ()
  "system specific overrides go here"
  (cond
   ((string-match "^ernesto.local" system-name) ; Macbook Air i5
    '((default-font "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
      (extra-exec-paths ("/Users/indy/local/bin" "/usr/local/bin"))
      (machine-os "osx")
      (save-folder (("." . "~/.emacs.d/saves")))

      (post-setup-fn (lambda ()
                       (ido-mode)
                       ; need to update path so lein can work
                       (setenv "CLOJURESCRIPT_HOME"
                               (concat (getenv "HOME")
                                       "/code/clojure/clojurescript"))
                       (setenv "PATH" 
                               (concat
                                (concat (getenv "CLOJURESCRIPT_HOME") "/bin:")
                                (concat (getenv "HOME") "/local/bin:")
                                "/usr/local/bin:" 
                                (getenv "PATH")))
                       
                       (setq inferior-lisp-program
                             (concat (getenv "HOME")
                                     "/code/clojure/clojurescript/script/browser-repl"))                       
                       (setq
                        ring-bell-function (lambda () (message "*beep*"))
                        mac-command-modifier 'meta
                        default-directory "~/"
                        browse-url-browser-function 'browse-url-generic
                        browse-url-generic-program "chromium-browser"
                        multi-term-program "/bin/bash")

                       (fset 'insertPound "#")
                       (global-set-key (kbd "M-3") 'insertPound)

                       (when (fboundp 'windmove-default-keybindings)
                         (windmove-default-keybindings))
                       (when (fboundp 'winner-mode)
                         (winner-mode 1))
;                       (electric-pair-mode)
                    ;                      (require 'edit-server)
                    ;                      (edit-server-start)
                       ))


      (frame-setup-fn (lambda ()
                        (set-frame-position (selected-frame) 780 0)
                        (set-frame-size (selected-frame) 80 55)
                        (add-to-list 'default-frame-alist
                                     (cons 'font
                                           (isg-val 'default-font)))))))

   ((string-match "^che" system-name)  ; asus ul20a
    '((post-setup-fn (lambda ()
                       (setenv "PATH" 
                               (concat
                                (concat (getenv "HOME") "/local/bin:")
                                "/usr/local/bin:" 
                                (getenv "PATH")))
                       (setenv "CLOJURESCRIPT_HOME"
                               (concat (getenv "HOME")
                                       "/code/clojure/clojurescript"))
                       (setq inferior-lisp-program
                             (concat (getenv "HOME")
                                     "/code/clojure/clojurescript/script/browser-repl"))
                       (setq
                        default-directory "~/"
                        browse-url-browser-function 'browse-url-generic
                        browse-url-generic-program "chromium-browser"
                        multi-term-program "/bin/bash")
                       (when (fboundp 'windmove-default-keybindings)
                         (windmove-default-keybindings))
                       (when (fboundp 'winner-mode)
                         (winner-mode 1))
;                      (electric-pair-mode)
;                       (require 'edit-server)
;                       (edit-server-start)
                       (new-frame)
                                        ;                      (global-set-key "\C-x1" 'isg-hack-delete-other-windows)
                       ))
      (frame-setup-fn (lambda ()
                        (setq initial-frame-alist '((top . 0)
                                                    (left . 340)
                                                    (width . 80)
                                                    (height . 59))
                              default-frame-alist '((top . 0)
                                                    (left . 880)
                                                    (width . 80)
                                                    (height . 59)))
                        (add-to-list 'default-frame-alist
                                     (cons 'font
                                           (isg-val 'default-font)))))
      (key-setup (([mouse-1] . nil)
                  ([double-mouse-1] . nil)
                  ([drag-mouse-1] . nil)
                  ([down-mouse-1] . nil)))))

   ((string-match "^raul" system-name)  ; eee 1000
    '((hyperspec-root "file:////home/user/docs/cl/HyperSpec/")
                                        ;(default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-20-*-*-*-m-0-fontset-startup")
      (post-setup-fn (lambda ()
                       (setenv "PATH" 
                               (concat
                                (concat (getenv "HOME") "/local/bin:")
                                "/usr/local/bin:" 
                                (getenv "PATH")))
                       (setenv "CLOJURESCRIPT_HOME"
                               (concat (getenv "HOME")
                                       "/code/clojure/clojurescript"))
                       (setq inferior-lisp-program
                             (concat (getenv "HOME")
                                     "/code/clojure/clojurescript/script/browser-repl"))                       
                       (setq
                        browse-url-browser-function 'browse-url-generic
                        browse-url-generic-program "chromium-browser")
                       (when (fboundp 'windmove-default-keybindings)
                         (windmove-default-keybindings))
                       (when (fboundp 'winner-mode)
                         (winner-mode 1))
;                        (isg-start-shell "default-shell")
;                        (require 'edit-server)
;                        (edit-server-start)
                       (new-frame)
                       (switch-to-buffer "*scratch*")))
      (frame-setup-fn (lambda ()
                        (setq initial-frame-alist '((top . 1)
                                                    (left . 5)
                                                    (width . 80)
                                                    (height . 46))
                              default-frame-alist '((top . 0)
                                                    (left . 509)
                                                    (width . 80)
                                                    (height . 46)))
                        (add-to-list 'default-frame-alist
                                     (cons 'font
                                           (isg-val 'default-font)))))
      (key-setup (([mouse-1] . nil) ; accidently touching touchpad won't shift cursor
                  ([double-mouse-1] . nil)
                  ([drag-mouse-1] . nil)
                  ([down-mouse-1] . nil)))))
   ((string-match "^blue" system-name) ; G5 iMac at home
    '((default-font "-apple-andale mono-medium-r-normal--0-0-0-0-m-0-mac-roman")
      (extra-exec-paths ("/Users/indy/bin" "/usr/local/bin" "/opt/local/bin"))
      (machine-os "osx")
      (save-folder (("." . "~/.emacs.d/saves")))
      (frame-setup-fn (lambda ()
                        (setq initial-frame-alist '((top . 20)
                                                    (left . 320)
                                                    (width . 80)
                                                    (height . 71))
                              default-frame-alist '((top . 20)
                                                    (left . 902)
                                                    (width . 80)
                                                    (height . 71)))
                        (add-to-list 'default-frame-alist
                                     (cons 'font
                                           (isg-val 'default-font)))))))
   ((string-match "^GOSHCC" system-name)  ; GOSH PC
    '(
      (default-font "-outline-Courier New-normal-normal-normal-mono-13-*-*-*-c-*-fontset-startup")
      (machine-os "windows") ; one of "linux" "osx" "windows"
      (save-folder (("." . "~/emacs-saves")))
      (extra-exec-paths ("c:\\home\\bin\\emacs-23.1\\bin"))
      (post-setup-fn (lambda ()
                       (ido-mode)
                       (find-file "k:/Direct Marketing/Data Analyst/indy/notes/done.org")
                       (find-file "k:/Direct Marketing/Data Analyst/indy/notes/notes.org")
                       (find-file "k:/Direct Marketing/Data Analyst/indy/notes/tasks.org")))
      (frame-setup-fn (lambda ()
                        (setq initial-frame-alist '((top . 0)
                                                    (left . 0)
                                                    (width . 80)
                                                    (height . 60))
                              default-frame-alist '((top . 20)
                                                    (left . 510)
                                                    (width . 80)
                                                    (height . 60)))
                        (add-to-list 'default-frame-alist
                                     (cons 'font
                                           (isg-val 'default-font)))))))

   ((equal system-name "green")         ; old laptop thrown out by orange
    '((hyperspec-root "file:///usr/share/common-lisp/isg-hyperspec/HyperSpec/")))))

; Windows
; default font: "file:///usr/share/common-lisp/isg-hyperspec/HyperSpec/"
; inferior lisp: "C:\\home\\bin\\sbcl\\sbcl.exe"
; (post-setup-fn (lambda ()
; ((setq exec-path (append exec-path '("c:\\home\\bin\\emacs-22.1\\bin")))
; (load-file "~/.emacs.d/site-lisp/external/gnuserv.el")
; (gnuserv-start)
; (setq gnuserv-frame(selected-frame)))))

(labels ((load-settings (which)
                        (mapcar (lambda (pair)
                                  (put 'isg-local (car pair) (cadr pair)))
                                which)))
  (load-settings (isg-default-machine-settings))
  (load-settings (isg-machine-settings)))

(provide 'machine-settings)


;; w3m - browsing t'internet from within emacs

;; use w3m when looking at hyperspec
;; from http://bc.tech.coop/blog/070208.html
;;(setq browse-url-browser-function '(("hyperspec" . w3m-browse-url)
;;				    ("weitz" . w3m-browse-url)
;;				    ("." . browse-url-default-macosx-browser)))



;;(post-setup-fn (lambda ()
;;(if window-system
;;(require 'w3m-load))
;;(new-frame)
;;(setq browse-url-browser-function 'w3m-browse-url)))
