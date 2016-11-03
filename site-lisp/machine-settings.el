;;; machine-settings.el --- machine dependent settings

;; Author: Inderjit Gill <email@indy.io>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Simple abstraction for machine dependenet settings

;;; Code:

(defun isg-default-machine-settings ()
  "Settings which apply to most of the machines apart from 1 or 2 stragglers."
  '((foreground-color "grey60")
    (background-color "black")
    (default-font "6x12")
    (machine-os "linux") ; one of "linux" "osx" "windows"
    (url-opener "chromium-browser")
    (save-folder "/tmp/emacs.d-saves")
    (deft-directory "~/notes/deft")
    (get-extra-paths (lambda ()
                       (list (concat (getenv "HOME") "/local/bin")
                             "/usr/local/bin"
                             "/usr/local/go/bin")))))

(defun isg-machine-settings ()
  "System specific overrides go here."
  (cond
   ((string-match "^BERTRAND" system-name)  ; 2016 Windows PC
    '(
      (default-font "-outline-Courier New-normal-normal-normal-mono-13-*-*-*-c-*-fontset-startup")
      (machine-os "windows") ; one of "linux" "osx" "windows"
      (save-folder "d:/scratch/emacs-saves")
      (deft-directory "d:/Google Drive/Docs/notes/deft")
      (frame-r ((top . 0) (left . 870) (width . 80) (height . 87)))
      (frame-l ((top . 0) (left . 210) (width . 80) (height . 87)))))

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
      (url-opener "open")

      (frame-r ((top . 0) (left . 864) (width . 80) (height . 55)))
      (frame-l ((top . 0) (left . 362) (width . 80) (height . 55)))

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
;                        (isg-start-shell "default-shell")
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
    '((hyperspec-root "file:///usr/share/common-lisp/isg-hyperspec/HyperSpec/")))))

; Windows
; default font: "file:///usr/share/common-lisp/isg-hyperspec/HyperSpec/"
; inferior lisp: "C:\\home\\bin\\sbcl\\sbcl.exe"
; (post-setup-fn (lambda ()
; ((setq exec-path (append exec-path '("c:\\home\\bin\\emacs-22.1\\bin")))
; (load-file "~/.emacs.d/site-lisp/external/gnuserv.el")
; (gnuserv-start)
; (setq gnuserv-frame(selected-frame)))))

(provide 'machine-settings)
;;; machine-settings.el ends here
