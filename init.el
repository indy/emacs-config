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

;; packages that this config doesn't use anymore:
;; edit-server-20141231.1358

;; timing code
(defvar isg/section-start-time (float-time))
(defvar isg/section-end-time (float-time))
(defvar isg/timings '())

(defun isg/time-section (msg)
  (setq isg/section-end-time (float-time))
  (add-to-list 'isg/timings
               (cons msg (format "%.3f" (- isg/section-end-time
                                           isg/section-start-time))) t)
  (setq isg/section-start-time (float-time)))

;; ----------------------------------------------------------------------------
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

;; see: https://github.com/jwiegley/use-package
;(require 'use-package)

;; after use-package-always-ensure is set, all subsequent use-package
;; statements will download packages if needed
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(isg/time-section "loading use-package")


(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#fdf6e3"))
 '(custom-safe-themes
   (quote
    ("0b7febf170c1a84420220d7014923cb4fceff8e276f82cbd5175dbd0b2cf77e5" default)))
 '(fci-rule-color "#eee8d5")
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (dracula-theme org highlight-thing material-theme which-key hydra use-package color-theme-sanityinc-solarized counsel swiper ivy cider clojure-mode csharp-mode shader-mode atomic-chrome cargo exec-path-from-shell ws-butler web-mode typescript-mode toml-mode smartparens simple-httpd rainbow-mode racer rust-mode markdown-mode magit js2-mode js-comint htmlize go-mode find-file-in-git-repo edit-server deft company-racer color-theme avy auto-complete ag glsl-mode flycheck flycheck-rust)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
