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


;; Added by Package.el.  This must come before configurations of
;; installed packages.
(package-initialize)

(org-babel-load-file (expand-file-name "~/.emacs.d/isg-init.org"))

(setq custom-file "~/.emacs.d/isg-custom.el")
(load custom-file)

(provide 'init)
