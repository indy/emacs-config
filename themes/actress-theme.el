;;; actress-theme.el --- Emacs 24 theme with a light background.

;; Copyright (C) 2015 Inderjit Gill <email@indy.io>

;; Author: Inderjit Gill
;; Keywords: dark color theme
;; URL: http://github.com/indy/color-theme-actress
;; Version: 0.2.2
;; Package-Requires: ((emacs "24"))
;; This version is based on mark-theme (https://github.com/mjfeller/mark-theme)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;;; Code:

(deftheme actress)
(let ((class '((class color) (min-colors 89)))
      (bg1 "#181916")
      (bg2 "#282820")
      (bg3 "#383830")
      (bg4 "#121310")
      (fg1 "#999999")
      (fg2 "#827840")
      (fg3 "#bdb8ac")
      (fg4 "#a9a49a")
      (key2 "#1f1f1f")
      (key3 "#040404")
      (cursor "#686860")
      (correct "#2f4f4f")
      (incorrect "#8B372E")
      (builtin "#6C9B9B")
      (keyword "#6C9B9B")
      (const   "#6C9B9B")
      (comment "#2f4f4f")
      (func    "#8B372E")
      (str     "#2e8b57")
      (type    "#5DD795")
      (var     "#827840")
      (warning "#794198"))
  (custom-theme-set-faces
   'actress
   `(default ((,class (:background ,bg1 :foreground ,fg1))))

   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   `(cursor ((,class (:background ,cursor))))
   `(default-italic ((,class (:italic t))))
   `(ffap ((,class (:foreground ,fg4))))

   `(font-latex-bold-face ((,class (:foreground ,type))))
   `(font-latex-italic-face ((,class (:foreground ,key3 :italic t))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
   `(font-latex-string-face ((,class (:foreground ,str))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func :bold t))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type ))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))

   `(fringe ((,class (:background ,bg1 :foreground ,fg4))))

   `(gnus-header-content ((,class (:foreground ,keyword))))
   `(gnus-header-from ((,class (:foreground ,var))))
   `(gnus-header-name ((,class (:foreground ,type))))
   `(gnus-header-subject ((,class (:foreground ,func :bold t))))

   `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
   `(hl-line ((,class (:background  ,bg2))))
   `(icompletep-determined ((,class :foreground ,builtin)))

   `(ido-first-match ((,class (:foreground ,keyword :bold t))))
   `(ido-only-match ((,class (:foreground ,warning))))

   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))

   `(isearch ((,class (:bold t :foreground ,warning :background ,bg3))))

   `(js2-external-variable ((,class (:foreground ,const  ))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,key2))))
   `(js2-private-function-call ((,class (:foreground ,const))))

   `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))

   `(link ((,class (:foreground ,const :underline t))))

   `(magit-branch ((,class (:foreground ,const :weight bold))))
   `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
   `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))
   `(magit-diffstat-added   ((,class (:foreground ,type))))
   `(magit-diffstat-removed ((,class (:foreground ,var))))
   `(magit-hash ((,class (:foreground ,fg2))))
   `(magit-hunk-heading           ((,class (:background ,bg3))))
   `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
   `(magit-item-highlight ((,class :background ,bg3)))
   `(magit-log-author ((,class (:foreground ,fg3))))
   `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
   `(magit-process-ok ((,class (:foreground ,func :weight bold))))
   `(magit-process-ok ((,class :foreground ,type)))
   `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
   `(magit-section-highlight      ((,class (:background ,bg2))))

   `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))

   `(mode-line ((,class (:box (:line-width 1 :color ,bg3 :style none) :bold t :foreground ,fg2 :background ,bg3))))
   `(mode-line-buffer-id ((,class (:bold t :foreground ,fg2 :background nil))))
   `(mode-line-highlight ((,class (:background ,bg4))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color ,bg2 :style none) :foreground ,fg2 :background ,bg1))))

   `(mu4e-cited-1-face ((,class (:foreground ,fg2))))
   `(mu4e-cited-7-face ((,class (:foreground ,fg3))))
   `(mu4e-header-mark-face ((,class (:foreground ,type))))
   `(mu4e-view-url-number-face ((,class (:foreground ,type))))

   `(org-agenda-date ((,class (:foreground ,var :height 1.1 ))))
   `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
   `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
   `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
   `(org-block ((,class (:foreground ,fg3))))
   `(org-code ((,class (:foreground ,fg2))))
   `(org-date ((,class (:underline t :foreground ,var) )))
   `(org-document-info-keyword ((,class (:foreground ,func))))
   `(org-done ((,class (:bold t :foreground ,bg4))))
   `(org-ellipsis ((,class (:foreground ,builtin))))
   `(org-footnote  ((,class (:underline t :foreground ,fg4))))
   `(org-hide ((,class (:foreground ,fg4))))
   `(org-level-1 ((,class (:bold t :foreground ,fg2 :height 1.1))))
   `(org-level-2 ((,class (:bold nil :foreground ,fg3))))
   `(org-level-3 ((,class (:bold t :foreground ,fg4))))
   `(org-level-4 ((,class (:bold nil :foreground ,bg4))))
   `(org-link ((,class (:underline t :foreground ,type ))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-scheduled ((,class (:foreground ,type))))
   `(org-sexp-date ((,class (:foreground ,fg4))))
   `(org-special-keyword ((,class (:foreground ,func))))
   `(org-todo ((,class :foreground ,keyword :bold t)))
   `(org-verbatim ((,class (:foreground ,bg3 :underline t :slant italic))))
   `(org-verbatim ((,class (:foreground ,fg4))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-warning ((,class (:underline t :foreground ,warning))))

   `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,var)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg1)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,var)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))

   `(region ((,class (:background ,fg1 :foreground ,bg1))))
   `(show-paren-match-face ((,class (:background ,warning))))
   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))

   `(sp-show-pair-match-face ((,class (:background ,correct))))
   `(sp-show-pair-mismatch-face ((,class (:background ,incorrect))))

   `(term ((,class (:foreground ,fg1 :background ,bg1))))
   `(term-color-black ((,class (:foreground ,bg3 :background ,bg3))))
   `(term-color-blue ((,class (:foreground ,func :background ,func))))
   `(term-color-cyan ((,class (:foreground ,str :background ,str))))
   `(term-color-green ((,class (:foreground ,type :background ,bg3))))
   `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
   `(term-color-red ((,class (:foreground ,keyword :background ,bg3))))
   `(term-color-white ((,class (:foreground ,fg2 :background ,fg2))))
   `(term-color-yellow ((,class (:foreground ,var :background ,var))))

   `(trailing-whitespace ((,class :foreground nil :background ,warning)))

   `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,keyword)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))

   `(vertical-border ((,class (:foreground ,fg3))))

   `(warning ((,class (:foreground ,warning))))

   `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
   `(web-mode-html-tag-face ((,class (:foreground ,builtin))))
   `(web-mode-keyword-face ((,class (:foreground ,keyword))))
   `(web-mode-string-face ((,class (:foreground ,str))))
   `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))

   ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'actress)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; actress-theme.el ends here
