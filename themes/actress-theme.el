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
      (cursor "#667e51")
      (bg1 "#040702")
      (bg2 "#192111")
      (bg3 "#2b3820")
      (bg4 "#3f5030")
      (fg1 "#909090")
      (fg2 "#767676")
      (fg3 "#5e5e5e")
      (fg4 "#464646")
      (clock12 "#f07e30")
      (clock01 "#c1962f")
      (clock02 "#9ca32f")
      (clock03 "#5eb02f")
      (clock04 "#31b17f")
      (clock05 "#33aea2")
      (clock06 "#35abbd")
      (clock07 "#38a5e6")
      (clock08 "#9d8ff5")
      (clock09 "#e16df5")
      (clock10 "#f667c7")
      (clock11 "#f77091")
      (hi1 "#ffff00")
      (hi2 "#d6fff7")
      (error "#f7002c")
      (warning "#c36000")
      (success "#2f4f4f")
      (builtin "#6C9B9B")
      (keyword "#488080")
      (const "#6C9B9B")
      (comment "#2f4f4f")
      (func "#d0454a")
      (str "#458354")
      (type "#4a7e8c")
      (var "#827840"))
  (custom-theme-set-faces
   'actress
   `(default ((,class (:background ,bg1 :foreground ,fg1))))

   ;; hi-yellow is used by highlight-thing
   `(hi-yellow ((,class (:background ,bg3))))

   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   `(cursor ((,class (:background ,cursor))))
   `(default-italic ((,class (:italic t))))
   `(ffap ((,class (:foreground ,fg4))))

   `(ivy-current-match ((,class (:foreground ,hi1 :background ,bg2))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,hi2))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,hi2))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,hi2))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,hi2))))

   ;`(ivy-confirm-face ((,class (:foreground ,hi2))))
   ;`(ivy-match-required-face ((,class (:foreground ,hi4))))
   ;`(ivy-virtual ((,class (:foreground ,hi4))))
   ;`(ivy-action ((,class (:foreground ,hi4))))

   `(company-tooltip ((,class (:background ,bg3 :inherit default))))
   `(company-scrollbar-bg ((,class (:background  ,bg3))))
   `(company-scrollbar-fg ((,class (:background  ,fg2))))
   `(company-tooltip-annotation ((,class (:foreground ,warning)))) ; ???
   `(company-tooltip-common ((,class (:foreground ,hi2))))
   `(company-tooltip-selection ((,class (:foreground ,hi1 :background ,bg2))))
   `(company-preview-common ((,class (:foreground ,fg3 :background ,bg4))))

     ;;    (eshell-ls-archive                            :foreground base08)
     ;; (eshell-ls-backup                             :foreground base0F)
     ;; (eshell-ls-clutter                            :foreground base09)
     ;; (eshell-ls-directory                          :foreground base0D)
     ;; (eshell-ls-executable                         :foreground base0B)
     ;; (eshell-ls-missing                            :foreground base08)
     ;; (eshell-ls-product                            :foreground base0F)
     ;; (eshell-ls-readonly                           :foreground base06)
     ;; (eshell-ls-special                            :foreground base0E)
     ;; (eshell-ls-symlink                            :foreground base0C)
     ;; (eshell-ls-unreadable                         :foreground base04)
   ;; (eshell-prompt                                :foreground base05)

   `(undo-tree-visualizer-default-face   ((,class (:foreground ,comment))))
   `(undo-tree-visualizer-current-face   ((,class (:foreground ,hi1))))
   `(undo-tree-visualizer-active-branch-face   ((,class (:foreground ,fg3))))
   `(undo-tree-visualizer-register-face   ((,class (:foreground ,hi2))))

   ;; flycheck-mode
   `(flycheck-error ((,class (:underline (:style wave :color ,error)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,keyword)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,warning)))))

   ;; flymake-mode   
   `(flymake-warnline ((,class (:background ,bg3 :underline (:style wave :color ,warning)))))
   `(flymake-errline ((,class (:background ,bg3 :underline (:style wave :color ,error)))))

   `(font-latex-bold-face ((,class (:foreground ,type))))
   `(font-latex-italic-face ((,class (:foreground ,fg3 :italic t))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
   `(font-latex-string-face ((,class (:foreground ,str))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func))))
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
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,fg3))))
   `(js2-private-function-call ((,class (:foreground ,const))))

;; haskell-keyword-face
;; haskell-type-face
;; haskell-constructor-face
;; haskell-definition-face
;; haskell-operator-face
;; haskell-pragma-face
;; haskell-liquid-haskell-annotation-face
;; haskell-literate-comment-face
;; haskell-quasi-quote-face

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
   `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
   `(magit-section-highlight      ((,class (:background ,bg2))))

   `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))

   `(mode-line ((,class (:box (:line-width 1 :color ,bg2 :style none) :bold nil :foreground ,fg2 :background ,bg2))))
   `(mode-line-buffer-id ((,class (:bold nil :foreground ,fg1 :background nil))))
   `(mode-line-highlight ((,class (:background ,bg4))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color ,bg2 :style none) :foreground ,fg4 :background ,bg1))))

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
   `(org-level-1 ((,class (:bold nil :foreground ,clock12))))
   `(org-level-2 ((,class (:bold nil :foreground ,clock02))))
   `(org-level-3 ((,class (:bold nil :foreground ,clock04))))
   `(org-level-4 ((,class (:bold nil :foreground ,clock06))))
   `(org-level-5 ((,class (:bold nil :foreground ,clock08))))
   `(org-level-6 ((,class (:bold nil :foreground ,clock10))))
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

   `(rainbow-delimiters-depth-1-face ((,class :foreground ,clock12)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,clock01)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,clock02)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,clock03)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,clock04)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,clock05)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,clock06)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,clock07)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))

   `(region ((,class (:background ,fg1 :foreground ,bg1))))
   `(show-paren-match-face ((,class (:background ,warning))))
   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))

   `(sp-show-pair-match-face ((,class (:background ,success))))
   `(sp-show-pair-mismatch-face ((,class (:background ,error))))

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
   ;; `(whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))))
   ;; `(whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   ;; `(whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   ;; `(whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))))
   ;; `(whitespace-line ((,class (:background nil :foreground ,red))))
   ;; `(whitespace-indentation ((,class (:background nil :foreground ,aqua))))
   ;; `(whitespace-space ((,class (:background nil :foreground ,selection))))
   ;; `(whitespace-newline ((,class (:background nil :foreground ,selection))))
   ;; `(whitespace-tab ((,class (:background nil :foreground ,selection))))
   ;; `(whitespace-hspace ((,class (:background nil :foreground ,selection))))
   

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
