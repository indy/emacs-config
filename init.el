;;; init.el --- emacs initialisation file

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
;; This is my init file (and all of this preamble is here just to stop
;; FlyCheck from complaining).

;;; Code:

;; set this to t in order to rebuild isg-init.el from isg-init.org
;;

(defvar isg/fully-refreshed-load nil)
(defvar isg/timing-hash (make-hash-table :test 'equal))

;; here's the most recent package-selected-packages list which should be copied into isg-custom.el:
;; (package-selected-packages
;;    (quote
;;     (ag avy cargo cmake-mode color-theme color-theme-sanityinc-solarized company company-racer counsel csharp-mode deft delight edit-server esh-autosuggest exec-path-from-shell find-file-in-git-repo flycheck flycheck-rust glsl-mode go-mode highlight-thing hindent htmlize hydra interleave ivy ivy-rich js-comint js2-mode magit markdown-mode olivetti org org-brain org-bullets org-gcal racer rainbow-mode rjsx-mode rust-mode shader-mode smartparens swiper toml-mode undo-tree use-package volatile-highlights web-mode wgrep which-key ws-butler yaml-mode)))

(defun isg/time-section-start (key)
  (puthash key (float-time) isg/timing-hash))

(defun isg/time-section-stop (key)
  (if (gethash key isg/timing-hash)
      (puthash key (- (float-time)
                      (gethash key isg/timing-hash))
               isg/timing-hash)))

(defmacro isg/timer (key &rest body)
  `(progn
     (isg/time-section-start ,key)
     ,@body
     (isg/time-section-stop ,key)))

(defun isg/time-show-slower-than (min)
    (progn
      (message "________________________START TIMES________________________")
      (maphash (lambda (k _v) (if (> _v min) (message "%.3f: %s" _v k))) isg/timing-hash)
      (message "___________________________________________________________")
      nil))

(isg/time-section-start "overall")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(isg/timer "package"
           (require 'package)
           (setq package-enable-at-startup nil)
           (setcdr (last package-archives)
                   '(("melpa-stable" . "https://stable.melpa.org/packages/")
                     ("melpa" . "https://melpa.org/packages/")
                     ("org" . "https://orgmode.org/elpa/")))

           (setq package-check-signature nil)

           (unless (package-installed-p 'use-package)
             ;; check for new packages (package versions)
             (message "%s" "Emacs Prelude is now refreshing its package database...")
             (package-refresh-contents)
             (message "%s" " done.")
             (package-install 'use-package)))

;; after use-package-always-ensure is set, all subsequent use-package
;; statements will download packages if needed
(setq use-package-always-ensure t)
(setq use-package-verbose t)

(require 'cl)
; third party code that isn't in melpa-stable yet
(push "~/.emacs.d/external" load-path)

(if isg/fully-refreshed-load
    (isg/timer "load isg-init.org"
               (org-babel-load-file (expand-file-name "~/.emacs.d/isg-init.org")))
  (isg/timer "load isg-init.el"
             (load "~/.emacs.d/isg-init.el")))

(isg/timer "custom-file"
           (setq custom-file "~/.emacs.d/isg-custom.el")
           (load custom-file))

(isg/time-section-stop "overall")
;; show slowest sections in the *Messages* buffer
(isg/time-show-slower-than 0.09)

(provide 'init)
