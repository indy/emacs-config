;;; imbue-mode.el --- Major mode to edit Imbue files in Emacs
;;; _heavily_ based on Jason Blevin's markdown-mode.el

;; Copyright (C) 2011 Inderjit Gill
;; Copyright (C) 2007, 2008, 2009 Jason Blevins

;; Version: 1.7
;; Keywords: Markdown major mode
;; Author: Jason Blevins <jrblevin@sdf.lonestar.org>
;; URL: http://jblevins.org/projects/markdown-mode/

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; markdown-mode is a major mode for editing [Markdown][]-formatted
;; text files in GNU Emacs.  markdown-mode is free software, licensed
;; under the GNU GPL.
;;
;;  [Markdown]: http://daringfireball.net/projects/markdown/
;;
;; The latest stable version is markdown-mode 1.7, released on October 1, 2009:
;;
;;    * [markdown-mode.el][]
;;    * [Screenshot][]
;;    * [Release notes][]
;;
;; markdown-mode is also available in the Debian
;; [emacs-goodies-el](http://packages.debian.org/emacs-goodies-el)
;; package (beginning with revision 27.0-1) and the OpenBSD
;; [textproc/markdown-mode](http://pkgsrc.se/textproc/markdown-mode) package.
;;
;;  [markdown-mode.el]: http://jblevins.org/projects/markdown-mode/markdown-mode.el
;;  [screenshot]: http://jblevins.org/projects/markdown-mode/screenshots/20080604-001.png
;;  [release notes]: http://jblevins.org/projects/markdown-mode/rev-1-7

;; The latest development version can be downloaded directly
;; ([markdown-mode.el][devel.el]) or it can be obtained from the
;; (browsable and clonable) Git repository at
;; <http://jblevins.org/git/markdown-mode.git>.  The entire repository,
;; including the full project history, can be cloned via the Git protocol
;; by running
;;
;;     git clone git://jblevins.org/git/markdown-mode.git
;;
;;  [devel.el]: http://jblevins.org/git/markdown-mode.git/plain/markdown-mode.el

;;; Dependencies:

;; imbue-mode requires easymenu, a standard package since GNU Emacs
;; 19 and XEmacs 19, which provides a uniform interface for creating
;; menus in GNU Emacs and XEmacs.

;;; Installation:

;; Make sure to place `imbue-mode.el` somewhere in the load-path and add
;; the following lines to your `.emacs` file to associate imbue-mode
;; with `.text` files:
;;
;;     (autoload 'imbue-mode "imbue-mode.el"
;;        "Major mode for editing Imbue files" t)
;;     (setq auto-mode-alist
;;        (cons '("\\.text" . imbue-mode) auto-mode-alist))
;;
;; There is no consensus on an official file extension so change `.text` to
;; `.mdwn`, `.md`, `.mdt`, or whatever you call your imbue files.

;;; Customization:

;; Although no configuration is *necessary* there are a few things
;; that can be customized.  The `M-x customize-mode` command
;; provides an interface to all of the possible customizations:
;;
;;   * `imbue-command` - the command used to run Imbue
;;     (default: `imbue`).
;;
;;   * `imbue-hr-length` - the length of horizontal rules
;;     (default: `5`).
;;
;;   * `imbue-bold-underscore` - set to a non-nil value to use two
;;     underscores for bold instead of two asterisks (default: `nil`).
;;
;;   * `imbue-italic-underscore` - set to a non-nil value to use
;;     underscores for italic instead of asterisks (default: `nil`).
;;
;;   * `imbue-indent-function` - the function to use for automatic
;;     indentation (default: `imbue-indent-line`).
;;
;;   * `imbue-indent-on-enter` - set to a non-nil value to
;;     automatically indent new lines when the enter key is pressed
;;     (default: `t`)
;;
;;   * `imbue-uri-types` - a list of protocols for URIs that
;;     `imbue-mode' should highlight.
;;
;;   * `imbue-enable-math` - syntax highlighting for
;;     LaTeX fragments (default: `nil`).
;;
;; Additionally, the faces used for syntax highlighting can be modified to
;; your liking by issuing `M-x customize-group RET imbue-faces`
;; or by using the "Imbue Faces" link at the bottom of the mode
;; customization screen.

;;; Usage:

;; Keybindings are grouped by prefixes based on their function.  For
;; example, commands dealing with headers begin with `C-c C-t`.  The
;; primary commands in each group will are described below.  You can
;; obtain a list of all keybindings by pressing `C-c C-h`.
;;
;;   * Anchors: `C-c C-a`
;;
;;     `C-c C-a l` inserts inline links of the form `[text](url)`.  If
;;     there is an active region, text in the region is used for the
;;     link text.  `C-c C-a w` acts similarly for wiki links of the
;;     form `[[WikiLink]]`.
;;
;;   * Commands: `C-c C-c`
;;
;;     `C-c C-c m` will run Imbue on the current buffer and preview
;;     the output in another buffer while `C-c C-c p` runs Imbue on
;;     the current buffer and previews the output in a browser.
;;
;;     `C-c C-c c` will check for undefined references.  If there are
;;     any, a small buffer will open with a list of undefined
;;     references and the line numbers on which they appear.  In Emacs
;;     22 and greater, selecting a reference from this list and
;;     pressing `RET` will insert an empty reference definition at the
;;     end of the buffer.  Similarly, selecting the line number will
;;     jump to the corresponding line.
;;
;;   * Images: `C-c C-i`
;;
;;     `C-c C-i i` inserts an image, using the active region (if any)
;;     as the alt text.
;;
;;   * Physical styles: `C-c C-p`
;;
;;     These commands all act on text in the active region, if any,
;;     and insert empty markup fragments otherwise.  `C-c C-p b` makes
;;     the selected text bold, `C-c C-p f` formats the region as
;;     fixed-width text, and `C-c C-p i` is used for italic text.
;;
;;   * Logical styles: `C-c C-s`
;;
;;     These commands all act on text in the active region, if any,
;;     and insert empty markup fragments otherwise.  Logical styles
;;     include blockquote (`C-c C-s b`), preformatted (`C-c C-s p`),
;;     code (`C-c C-s c`), emphasis (`C-c C-s e`), and strong (`C-c
;;     C-s s`).
;;
;;   * Headers: `C-c C-t`
;;
;;     All header commands use text in the active region, if any, as
;;     the header text.  To insert an atx or hash style level-n
;;     header, press `C-c C-t n` where n is between 1 and 6.  For a
;;     top-level setext or underline style header press `C-c C-t t`
;;     (mnemonic: title) and for a second-level underline-style header
;;     press `C-c C-t s` (mnemonic: section).
;;
;;   * Other commands
;;
;;     `C-c -` inserts a horizontal rule.
;;
;; Many of the commands described above behave differently depending on
;; whether Transient Mark mode is enabled or not.  When it makes sense,
;; if Transient Mark mode is on and a region is active, the command
;; applies to the text in the region (e.g., `C-c C-p b` makes the region
;; bold).  For users who prefer to work outside of Transient Mark mode,
;; in Emacs 22 it can be enabled temporarily by pressing `C-SPC C-SPC`.
;;
;; When applicable, commands that specifically act on the region even
;; outside of Transient Mark mode have the same keybinding as the with
;; the exception of an additional `C-` prefix.  For example,
;; `imbue-insert-blockquote` is bound to `C-c C-s b` and only acts on
;; the region in Transient Mark mode while `imbue-blockquote-region`
;; is bound to `C-c C-s C-b` and always applies to the region (when
;; nonempty).
;;
;; imbue-mode supports outline-minor-mode as well as org-mode-style
;; visibility cycling for atx- or hash-style headers.  There are two
;; types of visibility cycling: Pressing `S-TAB` cycles globally between
;; the table of contents view (headers only), outline view (top-level
;; headers only), and the full document view.  Pressing `TAB` while the
;; point is at a header will cycle through levels of visibility for the
;; subtree: completely folded, visible children, and fully visible.
;; Note that mixing hash and underline style headers will give undesired
;; results.

;;; Extensions:

;; Besides supporting the basic Imbue syntax, imbue-mode also
;; includes syntax highlighting for `[[Wiki Links]]` by default.
;;
;; [SmartyPants][] support is possible by customizing `imbue-command`.
;; If you install `SmartyPants.pl` at, say, `/usr/local/bin/smartypants`,
;; then you can set `imbue-command` to `"imbue | smartypants"`.
;; You can do this either by using `M-x customize-group imbue`
;; or by placing the following in your `.emacs` file:
;;
;;     (defun imbue-custom ()
;;       "imbue-mode-hook"
;;       (setq imbue-command "imbue | smartypants"))
;;     (add-hook 'imbue-mode-hook '(lambda() (imbue-custom)))
;;
;; [SmartyPants]: http://daringfireball.net/projects/smartypants/
;;
;; Experimental syntax highlighting for mathematical expressions written
;; in LaTeX (only expressions denoted by `$..$`, `$$..$$`, or `\[..\]`)
;; can be enabled by setting `imbue-enable-math` to a non-nil value,
;; either via customize or by placing `(setq imbue-enable-itex t)`
;; in `.emacs`, and restarting Emacs.

;;; Acknowledgments:

;; imbue-mode has benefited greatly from the efforts of the
;; following people:
;;
;;   * Cyril Brulebois <cyril.brulebois@enst-bretagne.fr> for Debian packaging.
;;   * Conal Elliott <conal@conal.net> for a font-lock regexp patch.
;;   * Edward O'Connor <hober0@gmail.com> for a font-lock regexp fix.
;;   * Greg Bognar <greg_bognar@hms.harvard.edu> for menus and a patch.
;;   * Daniel Burrows <dburrows@debian.org> for filing Debian bug #456592.
;;   * Peter S. Galbraith <psg@debian.org> for maintaining emacs-goodies-el.
;;   * Dmitry Dzhus <mail@sphinx.net.ru> for reference checking functions.
;;   * Bryan Kyle <bryan.kyle@gmail.com> for indentation code.
;;   * intrigeri <intrigeri@boum.org> for face customizations.
;;   * Ankit Solanki <ankit.solanki@gmail.com> for longlines.el compatibility.
;;   * Hilko Bengen <bengen@debian.org> for proper XHTML output.
;;   * Jose A. Ortega Ruiz <jao@gnu.org> for Emacs 23 fixes.
;;   * Alec Resnick <alec@sproutward.org> for bug reports.
;;   * Peter Williams <pezra@barelyenough.org> for fill-paragraph enhancements.

;;; Bugs:

;; Although imbue-mode is developed and tested primarily using
;; GNU Emacs 23, compatibility with GNU Emacs 21 and 22 is also a
;; priority.
;;
;; imbue-mode's syntax highlighting is accomplished using the
;; search-based fontification features of Emacs through a series of
;; regular expressions.  Unfortunately, Emacs has trouble highlighting
;; multi-line constructs using regular expressions and this creates
;; several syntax-highlighting quirks such as mistaking indented
;; lists for preformatted text, etc.  Making imbue-mode's syntax
;; highlighting more robust through the use of matching functions
;; or syntactic font lock is a high-priority item for future work.
;;
;; If you find any bugs not mentioned here, please construct a test
;; case and/or a patch and email me at <jrblevin@sdf.lonestar.org>.

;;; History:

;; imbue-mode was written and is maintained by Jason Blevins.  The
;; first version was released on May 24, 2007.
;;
;;   * 2007-05-24: Version 1.1
;;   * 2007-05-25: Version 1.2
;;   * 2007-06-05: [Version 1.3][]
;;   * 2007-06-29: Version 1.4
;;   * 2008-05-24: [Version 1.5][]
;;   * 2008-06-04: [Version 1.6][]
;;   * 2008-10-01: [Version 1.7][]
;;
;; [Version 1.3]: http://jblevins.org/projects/imbue-mode/rev-1-3
;; [Version 1.5]: http://jblevins.org/projects/imbue-mode/rev-1-5
;; [Version 1.6]: http://jblevins.org/projects/imbue-mode/rev-1-6
;; [Version 1.7]: http://jblevins.org/projects/imbue-mode/rev-1-7




;;; Code:

(require 'easymenu)
(require 'outline)


;;; Customizable variables ====================================================

;; Current revision
(defconst imbue-mode-version "1.7-dev")

;; A hook for users to run their own code when the mode is loaded.
(defvar imbue-mode-hook nil)


;;; Customizable variables ====================================================

(defgroup imbue nil
  "Major mode for editing text files in Imbue format."
  :prefix "imbue-"
  :group 'wp
  :link '(url-link "http://jblevins.org/projects/imbue-mode/"))

(defcustom imbue-command "imbue"
  "Command to run imbue."
  :group 'imbue
  :type 'string)

(defcustom imbue-hr-length 5
  "Length of horizonal rules."
  :group 'imbue
  :type 'integer)

(defcustom imbue-bold-underscore nil
  "Use two underscores for bold instead of two asterisks."
  :group 'imbue
  :type 'boolean)

(defcustom imbue-italic-underscore nil
  "Use underscores for italic instead of asterisks."
  :group 'imbue
  :type 'boolean)

(defcustom imbue-indent-function 'imbue-indent-line
  "Function to use to indent."
  :group 'imbue
  :type 'function)

(defcustom imbue-indent-on-enter t
  "Automatically indent new lines when enter key is pressed."
  :group 'imbue
  :type 'boolean)

(defcustom imbue-uri-types
  '("acap" "cid" "data" "dav" "fax" "file" "ftp" "gopher" "http" "https"
    "imap" "ldap" "mailto" "mid" "modem" "news" "nfs" "nntp" "pop" "prospero"
    "rtsp" "service" "sip" "tel" "telnet" "tip" "urn" "vemmi" "wais")
  "Link types for syntax highlighting of URIs."
  :group 'imbue
  :type 'list)

(defcustom imbue-enable-math nil
  "Syntax highlighting for inline LaTeX expressions.
This will not take effect until Emacs is restarted."
  :group 'imbue
  :type 'boolean)

(defcustom imbue-css-path nil
  "CSS file to include in the output XHTML"
  :group 'imbue
  :type 'string)

;;; Font lock =================================================================

(require 'font-lock)


(defvar imbue-italic-face 'imbue-italic-face
  "Face name to use for italic text.")

(defvar imbue-bold-face 'imbue-bold-face
  "Face name to use for bold text.")

(defvar imbue-header-face 'imbue-header-face
  "Face name to use as a base for headers.")

(defvar imbue-header-face-1 'imbue-header-face-1
  "Face name to use for level-1 headers.")

(defvar imbue-header-face-2 'imbue-header-face-2
  "Face name to use for level-2 headers.")

(defvar imbue-header-face-3 'imbue-header-face-3
  "Face name to use for level-3 headers.")

(defvar imbue-header-face-4 'imbue-header-face-4
  "Face name to use for level-4 headers.")

(defvar imbue-header-face-5 'imbue-header-face-5
  "Face name to use for level-5 headers.")

(defvar imbue-header-face-6 'imbue-header-face-6
  "Face name to use for level-6 headers.")

(defvar imbue-inline-code-face 'imbue-inline-code-face
  "Face name to use for inline code.")

(defvar imbue-list-face 'imbue-list-face
  "Face name to use for list markers.")

(defvar imbue-blockquote-face 'imbue-blockquote-face
  "Face name to use for blockquote.")

(defvar imbue-pre-face 'imbue-pre-face
  "Face name to use for preformatted text.")

(defvar imbue-link-face 'imbue-link-face
  "Face name to use for links.")

(defvar imbue-reference-face 'imbue-reference-face
  "Face name to use for reference.")

(defvar imbue-url-face 'imbue-url-face
  "Face name to use for URLs.")

(defvar imbue-link-title-face 'imbue-link-title-face
  "Face name to use for reference link titles.")

(defvar imbue-comment-face 'imbue-comment-face
  "Face name to use for HTML comments.")

(defvar imbue-head-face 'imbue-head-face
  "Face name to use for head.")

(defvar imbue-math-face 'imbue-math-face
  "Face name to use for LaTeX expressions.")


(defgroup imbue-faces nil
  "Faces used in Imbue Mode"
  :group 'imbue
  :group 'faces)

(defface imbue-italic-face
  '((t :inherit font-lock-variable-name-face :italic t))
  "Face for italic text."
  :group 'imbue-faces)

(defface imbue-bold-face
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face for bold text."
  :group 'imbue-faces)

(defface imbue-header-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Base face for headers."
  :group 'imbue-faces)

(defface imbue-header-face-1
  '((t :inherit imbue-header-face))
  "Face for level-1 headers."
  :group 'imbue-faces)

(defface imbue-header-face-2
  '((t :inherit imbue-header-face))
  "Face for level-2 headers."
  :group 'imbue-faces)

(defface imbue-header-face-3
  '((t :inherit imbue-header-face))
  "Face for level-3 headers."
  :group 'imbue-faces)

(defface imbue-header-face-4
  '((t :inherit imbue-header-face))
  "Face for level-4 headers."
  :group 'imbue-faces)

(defface imbue-header-face-5
  '((t :inherit imbue-header-face))
  "Face for level-5 headers."
  :group 'imbue-faces)

(defface imbue-header-face-6
  '((t :inherit imbue-header-face))
  "Face for level-6 headers."
  :group 'imbue-faces)

(defface imbue-inline-code-face
  '((t :inherit font-lock-constant-face))
  "Face for inline code."
  :group 'imbue-faces)

(defface imbue-list-face
  '((t :inherit font-lock-builtin-face))
  "Face for list item markers."
  :group 'imbue-faces)

(defface imbue-blockquote-face
  '((t :inherit font-lock-doc-face))
  "Face for blockquote sections."
  :group 'imbue-faces)

(defface imbue-pre-face
  '((t :inherit font-lock-constant-face))
  "Face for preformatted text."
  :group 'imbue-faces)

(defface imbue-link-face
  '((t :inherit font-lock-keyword-face))
  "Face for links."
  :group 'imbue-faces)

(defface imbue-reference-face
  '((t :inherit font-lock-type-face))
  "Face for link references."
  :group 'imbue-faces)

(defface imbue-url-face
  '((t :inherit font-lock-string-face))
  "Face for URLs."
  :group 'imbue-faces)

(defface imbue-link-title-face
  '((t :inherit font-lock-comment-face))
  "Face for reference link titles."
  :group 'imbue-faces)

(defface imbue-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for HTML comments."
  :group 'imbue-faces)


(defface imbue-head-face
  '((t :inherit font-lock-constant-face))
  "Face for inline code."
  :group 'imbue-faces)


(defface imbue-math-face
  '((t :inherit font-lock-string-face))
  "Face for LaTeX expressions."
  :group 'imbue-faces)

(defconst imbue-regex-link-inline
  "\\(!?\\[[^]]*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file).")

(defconst imbue-regex-link-reference
  "\\(!?\\[[^]]+?\\]\\)[ ]?\\(\\[[^]]*?\\]\\)"
  "Regular expression for a reference link [text][id].")

(defconst imbue-regex-reference-definition
  "^ \\{0,3\\}\\(\\[.+?\\]\\):\\s *\\(.*?\\)\\s *\\( \"[^\"]*\"$\\|$\\)"
  "Regular expression for a link definition [id]: ...")

(defconst imbue-regex-header-1-atx
  "^\\(# \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 1 atx-style (hash mark) headers.")

(defconst imbue-regex-header-2-atx
  "^\\(## \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 2 atx-style (hash mark) headers.")

(defconst imbue-regex-header-3-atx
  "^\\(### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 3 atx-style (hash mark) headers.")

(defconst imbue-regex-header-4-atx
  "^\\(#### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 4 atx-style (hash mark) headers.")

(defconst imbue-regex-header-5-atx
  "^\\(##### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 5 atx-style (hash mark) headers.")

(defconst imbue-regex-header-6-atx
  "^\\(###### \\)\\(.*?\\)\\($\\| #+$\\)"
  "Regular expression for level 6 atx-style (hash mark) headers.")

(defconst imbue-regex-header-1-setext
  "^\\(.*\\)\n\\(===+\\)$"
  "Regular expression for level 1 setext-style (underline) headers.")

(defconst imbue-regex-header-2-setext
  "^\\(.*\\)\n\\(---+\\)$"
  "Regular expression for level 2 setext-style (underline) headers.")

(defconst imbue-regex-hr
  "^\\(\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*\\|-[ ]?-[ ]?-[--- ]*\\)$"
  "Regular expression for matching Imbue horizontal rules.")

(defconst imbue-regex-code
  "\\(^\\|[^\\]\\)\\(\\(`\\{1,2\\}\\)\\([^ \\]\\|[^ ].*?[^ \\]\\)\\3\\)"
  "Regular expression for matching inline code fragments.")

(defconst imbue-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst imbue-regex-list
  "^[ \t]*\\([0-9]+\\.\\|[\\*\\+-]\\) "
  "Regular expression for matching list markers.")

(defconst imbue-regex-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\(.\\|\n\\)*?[^\\ ]\\3\\)"
  "Regular expression for matching bold text.")

(defconst imbue-regex-italic
  "\\(^\\|[^\\]\\)\\(\\([*_]\\)\\([^ \\]\\3\\|[^ ]\\(.\\|\n\\)*?[^\\ ]\\3\\)\\)"
  "Regular expression for matching italic text.")

(defconst imbue-regex-blockquote
  "^>.*$"
  "Regular expression for matching blockquote lines.")

(defconst imbue-regex-line-break
  "  $"
  "Regular expression for matching line breaks.")

(defconst imbue-regex-wiki-link
  "\\[\\[[^]]+\\]\\]"
  "Regular expression for matching wiki links.")

(defconst imbue-regex-uri
  (concat
   "\\(" (mapconcat 'identity imbue-uri-types "\\|")
   "\\):[^]\t\n\r<>,;() ]+")
  "Regular expression for matching inline URIs.")

(defconst imbue-regex-angle-uri
  (concat
   "\\(<\\)\\("
   (mapconcat 'identity imbue-uri-types "\\|")
   "\\):[^]\t\n\r<>,;()]+\\(>\\)")
  "Regular expression for matching inline URIs in angle brackets.")

(defconst imbue-regex-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses.")

(defconst imbue-regex-latex-expression
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for itex $..$ or $$..$$ math mode expressions.")

(defconst imbue-regex-latex-display
    "^\\\\\\[\\(.\\|\n\\)*?\\\\\\]$"
  "Regular expression for itex \[..\] display mode expressions.")

(defconst imbue-regex-list-indent
  "^\\(\\s *\\)\\([0-9]+\\.\\|[\\*\\+-]\\)\\(\\s +\\)"
  "Regular expression for matching indentation of list items.")

; From html-helper-mode
(defun imbue-match-comments (last)
  "Matches HTML comments from the point to LAST"
  (cond ((search-forward "<!--" last t)
         (backward-char 4)
         (let ((beg (point)))
           (cond ((search-forward-regexp "--[ \t]*>" last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun imbue-match-head (last)
  (cond ((search-forward "---" last t)
         (backward-char 3)
         (let ((beg (point)))
           (forward-char 3)
           (cond ((search-forward-regexp "---" last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defvar imbue-mode-font-lock-keywords-basic
  (list
   '(imbue-match-comments 0 imbue-comment-face t t)
   '(imbue-match-head 0 imbue-head-face t t)
   (cons imbue-regex-code '(2 imbue-inline-code-face))
   (cons imbue-regex-pre 'imbue-pre-face)
   (cons imbue-regex-blockquote 'imbue-blockquote-face)
   (cons imbue-regex-header-1-setext 'imbue-header-face-1)
   (cons imbue-regex-header-2-setext 'imbue-header-face-2)
   (cons imbue-regex-header-1-atx 'imbue-header-face-1)
   (cons imbue-regex-header-2-atx 'imbue-header-face-2)
   (cons imbue-regex-header-3-atx 'imbue-header-face-3)
   (cons imbue-regex-header-4-atx 'imbue-header-face-4)
   (cons imbue-regex-header-5-atx 'imbue-header-face-5)
   (cons imbue-regex-header-6-atx 'imbue-header-face-6)
   (cons imbue-regex-hr 'imbue-header-face)
   (cons imbue-regex-list 'imbue-list-face)
   (cons imbue-regex-link-inline
         '((1 imbue-link-face t)
           (2 imbue-url-face t)))
   (cons imbue-regex-link-reference
         '((1 imbue-link-face t)
           (2 imbue-reference-face t)))
   (cons imbue-regex-reference-definition
         '((1 imbue-reference-face t)
           (2 imbue-url-face t)
           (3 imbue-link-title-face t)))
   (cons imbue-regex-wiki-link 'imbue-link-face)
   (cons imbue-regex-bold '(2 imbue-bold-face))
   (cons imbue-regex-italic '(2 imbue-italic-face))
   (cons imbue-regex-angle-uri 'imbue-link-face)
   (cons imbue-regex-uri 'imbue-link-face)
   (cons imbue-regex-email 'imbue-link-face)
   )
  "Syntax highlighting for Imbue files.")

(defconst imbue-mode-font-lock-keywords-latex
  (list
   ;; Math mode $..$ or $$..$$
   (cons imbue-regex-latex-expression '(2 imbue-math-face))
   ;; Display mode equations with brackets: \[ \]
   (cons imbue-regex-latex-display 'imbue-math-face)
   ;; Equation reference (eq:foo)
   (cons "(eq:\\w+)" 'imbue-reference-face)
   ;; Equation reference \eqref{foo}
   (cons "\\\\eqref{\\w+}" 'imbue-reference-face))
  "Syntax highlighting for LaTeX fragments.")

(defvar imbue-mode-font-lock-keywords
  (append
   (if imbue-enable-math
       imbue-mode-font-lock-keywords-latex)
   imbue-mode-font-lock-keywords-basic)
  "Default highlighting expressions for Imbue mode.")



;;; Syntax Table ==============================================================

(defvar imbue-mode-syntax-table
  (let ((imbue-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" "w" imbue-mode-syntax-table)
    imbue-mode-syntax-table)
  "Syntax table for `imbue-mode'.")



;;; Element Insertion =========================================================

(defun imbue-wrap-or-insert (s1 s2)
 "Insert the strings S1 and S2.
If Transient Mark mode is on and a region is active, wrap the strings S1
and S2 around the region."
 (if (and transient-mark-mode mark-active)
     (let ((a (region-beginning)) (b (region-end)))
       (goto-char a)
       (insert s1)
       (goto-char (+ b (length s1)))
       (insert s2))
   (insert s1 s2)))

(defun imbue-insert-hr ()
  "Insert a horizonal rule."
  (interactive)
  (let (hr)
    (dotimes (count (- imbue-hr-length 1) hr)        ; Count to n - 1
      (setq hr (concat "* " hr)))                       ; Build HR string
    (setq hr (concat hr "*\n"))                         ; Add the n-th *
    (insert hr)))

(defun imbue-insert-bold ()
  "Insert markup for a bold word or phrase.
If Transient Mark mode is on and a region is active, it is made bold."
  (interactive)
  (if imbue-bold-underscore
      (imbue-wrap-or-insert "__" "__")
    (imbue-wrap-or-insert "**" "**"))
  (backward-char 2))

(defun imbue-insert-italic ()
  "Insert markup for an italic word or phrase.
If Transient Mark mode is on and a region is active, it is made italic."
  (interactive)
  (if imbue-italic-underscore
      (imbue-wrap-or-insert "_" "_")
    (imbue-wrap-or-insert "*" "*"))
  (backward-char 1))

(defun imbue-insert-code ()
  "Insert markup for an inline code fragment.
If Transient Mark mode is on and a region is active, it is marked
as inline code."
  (interactive)
  (imbue-wrap-or-insert "`" "`")
  (backward-char 1))

(defun imbue-insert-link ()
  "Insert an inline link of the form []().
If Transient Mark mode is on and a region is active, it is used
as the link text."
  (interactive)
  (imbue-wrap-or-insert "[" "]")
  (insert "()")
  (backward-char 1))

(defun imbue-insert-wiki-link ()
  "Insert a wiki link of the form [[WikiLink]].
If Transient Mark mode is on and a region is active, it is used
as the link text."
  (interactive)
  (imbue-wrap-or-insert "[[" "]]")
  (backward-char 2))

(defun imbue-insert-image ()
  "Insert an inline image tag of the form ![]().
If Transient Mark mode is on and a region is active, it is used
as the alt text of the image."
  (interactive)
  (imbue-wrap-or-insert "![" "]")
  (insert "()")
  (backward-char 1))

(defun imbue-insert-header-1 ()
  "Insert a first level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (imbue-insert-header 1))

(defun imbue-insert-header-2 ()
  "Insert a second level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (imbue-insert-header 2))

(defun imbue-insert-header-3 ()
  "Insert a third level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (imbue-insert-header 3))

(defun imbue-insert-header-4 ()
  "Insert a fourth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (imbue-insert-header 4))

(defun imbue-insert-header-5 ()
  "Insert a fifth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (imbue-insert-header 5))

(defun imbue-insert-header-6 ()
  "Insert a sixth level atx-style (hash mark) header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (imbue-insert-header 6))

(defun imbue-insert-header (n)
  "Insert an atx-style (hash mark) header.
With no prefix argument, insert a level-1 header.  With prefix N,
insert a level-N header.  If Transient Mark mode is on and the
region is active, it is used as the header text."
  (interactive "p")
  (unless n                             ; Test to see if n is defined
    (setq n 1))                         ; Default to level 1 header
  (let (hdr hdrl hdrr)
    (dotimes (count n hdr)
      (setq hdr (concat "#" hdr)))      ; Build a hash mark header string
    (setq hdrl (concat hdr " "))
    (setq hdrr (concat " " hdr))
    (imbue-wrap-or-insert hdrl hdrr))
  (backward-char (+ 1 n)))

(defun imbue-insert-title ()
  "Insert a setext-style (underline) first level header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((a (region-beginning))
            (b (region-end))
            (len 0)
            (hdr))
        (setq len (- b a))
        (dotimes (count len hdr)
          (setq hdr (concat "=" hdr)))  ; Build a === title underline
        (end-of-line)
        (insert "\n" hdr "\n"))
    (insert "\n==========\n")
    (backward-char 12)))

(defun imbue-insert-section ()
  "Insert a setext-style (underline) second level header.
If Transient Mark mode is on and a region is active, it is used
as the header text."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (let ((a (region-beginning))
            (b (region-end))
            (len 0)
            (hdr))
        (setq len (- b a))
        (dotimes (count len hdr)
          (setq hdr (concat "-" hdr)))  ; Build a --- section underline
        (end-of-line)
        (insert "\n" hdr "\n"))
    (insert "\n----------\n")
    (backward-char 12)))

(defun imbue-insert-blockquote ()
  "Start a blockquote section (or blockquote the region).
If Transient Mark mode is on and a region is active, it is used as
the blockquote text."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (imbue-blockquote-region (region-beginning) (region-end))
    (insert "> ")))

(defun imbue-block-region (beg end prefix)
  "Format the region using a block prefix.
Arguments BEG and END specify the beginning and end of the
region.The characters PREFIX will appear at the beginning
of each line."
  (if mark-active
      (save-excursion
        (let ((endpos end))
          ; Ensure that there is a leading blank line
          (goto-char beg)
          (while (not (looking-back "\n\n" 2))
            (insert "\n")
            (setq endpos (+ 1 endpos)))
          ; Insert blockquote characters
          (move-to-left-margin)
          (while (< (point-at-bol) endpos)
            (insert prefix)
            (setq endpos (+ (length prefix) endpos))
            (forward-line))
          ; Move back before any blank lines at the end
          (goto-char endpos)
          (while (looking-back "\n" 1)
            (backward-char))
          ; Ensure one blank line at the end
          (while (not (looking-at "\n\n"))
            (insert "\n")
            (backward-char))))))

(defun imbue-blockquote-region (beg end)
  "Blockquote the region.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (imbue-block-region beg end "> "))

(defun imbue-insert-pre ()
  "Start a preformatted section (or apply to the region).
If Transient Mark mode is on and a region is active, it is marked
as preformatted text."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (imbue-pre-region (region-beginning) (region-end))
    (insert "    ")))

(defun imbue-pre-region (beg end)
  "Format the region as preformatted text.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (imbue-block-region beg end "    "))

;;; Indentation ====================================================================

;;; Indentation functions contributed by Bryan Kyle <bryan.kyle@gmail.com>..

(defun imbue-indent-find-next-position (cur-pos positions)
  "Return the position after the index of CUR-POS in POSITIONS."
  (while (and positions
              (not (equal cur-pos (car positions))))
    (setq positions (cdr positions)))
  (or (cadr positions) 0))

(defun imbue-prev-line-indent-p ()
  "Return t if the previous line is indented."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (if (re-search-forward "^\\s " (point-at-eol) t) t)))

(defun imbue-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (when (re-search-forward "^\\s +" (point-at-eol) t)
        (current-column))))

(defun imbue-prev-list-indent ()
  "Return position of the first non-list-marker on the previous line."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (when (re-search-forward imbue-regex-list-indent (point-at-eol) t)
        (current-column))))

(defun imbue-indent-line ()
  "Indent the current line using some heuristics."
  (interactive)
  (if (imbue-prev-line-indent-p)
      ;; If the current column is any of the positions, remove all
      ;; of the positions up-to and including the current column
      (indent-line-to
       (imbue-indent-find-next-position
        (current-column) (imbue-calc-indents)))))

(defun imbue-calc-indents ()
  "Return a list of indentation columns to cycle through."
  (let (pos
        prev-line-pos
        positions
        computed-pos)

    ;; Previous line indent
    (setq prev-line-pos (imbue-prev-line-indent))
    (setq positions (cons prev-line-pos positions))

    ;; Previous non-list-marker indent
    (setq positions (cons (imbue-prev-list-indent) positions))

    ;; Indentation of the previous line + tab-width
    (cond
     (prev-line-pos
      (setq positions (cons (+ prev-line-pos tab-width) positions)))
     (t
      (setq positions (cons tab-width positions))))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos
             (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of preceeding list item
    (setq pos
          (save-excursion
            (forward-line -1)
            (catch 'break
              (while (not (equal (point) (point-min)))
                (forward-line -1)
                (goto-char (point-at-bol))
                (when (re-search-forward imbue-regex-list-indent (point-at-eol) t)
                  (throw 'break (length (match-string 1)))))
              nil)))
    (if pos
        (setq positions (cons pos positions)))

    ;; First column
    (setq positions (cons 0 (reverse positions)))

    positions))

(defun imbue-enter-key ()
  "Insert a newline and optionally indent the next line."
  (interactive)
  (newline)
  (if imbue-indent-on-enter
      (funcall indent-line-function)))



;;; Keymap ====================================================================

(defvar imbue-mode-map
  (let ((imbue-mode-map (make-keymap)))
    ;; Element insertion
    (define-key imbue-mode-map "\C-c\C-al" 'imbue-insert-link)
    (define-key imbue-mode-map "\C-c\C-aw" 'imbue-insert-wiki-link)
    (define-key imbue-mode-map "\C-c\C-ii" 'imbue-insert-image)
    (define-key imbue-mode-map "\C-c\C-t1" 'imbue-insert-header-1)
    (define-key imbue-mode-map "\C-c\C-t2" 'imbue-insert-header-2)
    (define-key imbue-mode-map "\C-c\C-t3" 'imbue-insert-header-3)
    (define-key imbue-mode-map "\C-c\C-t4" 'imbue-insert-header-4)
    (define-key imbue-mode-map "\C-c\C-t5" 'imbue-insert-header-5)
    (define-key imbue-mode-map "\C-c\C-t6" 'imbue-insert-header-6)
    (define-key imbue-mode-map "\C-c\C-pb" 'imbue-insert-bold)
    (define-key imbue-mode-map "\C-c\C-ss" 'imbue-insert-bold)
    (define-key imbue-mode-map "\C-c\C-pi" 'imbue-insert-italic)
    (define-key imbue-mode-map "\C-c\C-se" 'imbue-insert-italic)
    (define-key imbue-mode-map "\C-c\C-pf" 'imbue-insert-code)
    (define-key imbue-mode-map "\C-c\C-sc" 'imbue-insert-code)
    (define-key imbue-mode-map "\C-c\C-sb" 'imbue-insert-blockquote)
    (define-key imbue-mode-map "\C-c\C-s\C-b" 'imbue-blockquote-region)
    (define-key imbue-mode-map "\C-c\C-sp" 'imbue-insert-pre)
    (define-key imbue-mode-map "\C-c\C-s\C-p" 'imbue-pre-region)
    (define-key imbue-mode-map "\C-c-" 'imbue-insert-hr)
    (define-key imbue-mode-map "\C-c\C-tt" 'imbue-insert-title)
    (define-key imbue-mode-map "\C-c\C-ts" 'imbue-insert-section)
	;; Indentation
	(define-key imbue-mode-map "\C-m" 'imbue-enter-key)
    ;; Visibility cycling
    (define-key imbue-mode-map (kbd "<tab>") 'imbue-cycle)
    (define-key imbue-mode-map (kbd "<S-iso-lefttab>") 'imbue-shifttab)
    ;; Imbue functions
    (define-key imbue-mode-map "\C-c\C-cm" 'imbue)
    (define-key imbue-mode-map "\C-c\C-cp" 'imbue-preview)
    ;; References
    (define-key imbue-mode-map "\C-c\C-cc" 'imbue-check-refs)
    imbue-mode-map)
  "Keymap for Imbue major mode.")

;;; Menu ==================================================================

(easy-menu-define imbue-mode-menu imbue-mode-map
  "Menu for Imbue mode"
  '("Imbue"
    ("Show/Hide"
     ["Cycle visibility" imbue-cycle (outline-on-heading-p)]
     ["Cycle global visibility" imbue-shifttab])
    "---"
    ["Compile" imbue]
    ["Preview" imbue-preview]
    "---"
    ("Headers (setext)"
     ["Insert Title" imbue-insert-title]
     ["Insert Section" imbue-insert-section])
    ("Headers (atx)"
     ["First level" imbue-insert-header-1]
     ["Second level" imbue-insert-header-2]
     ["Third level" imbue-insert-header-3]
     ["Fourth level" imbue-insert-header-4]
     ["Fifth level" imbue-insert-header-5]
     ["Sixth level" imbue-insert-header-6])
    "---"
    ["Bold" imbue-insert-bold]
    ["Italic" imbue-insert-italic]
    ["Blockquote" imbue-insert-blockquote]
    ["Preformatted" imbue-insert-pre]
    ["Code" imbue-insert-code]
    "---"
    ["Insert inline link" imbue-insert-link]
    ["Insert image" imbue-insert-image]
    ["Insert horizontal rule" imbue-insert-hr]
    "---"
    ["Check references" imbue-check-refs]
    "---"
    ["Version" imbue-show-version]
    ))



;;; References ================================================================

;;; Undefined reference checking code by Dmitry Dzhus <mail@sphinx.net.ru>.

(defconst imbue-refcheck-buffer
  "*Undefined references for %BUFFER%*"
  "Pattern for name of buffer for listing undefined references.
The string %BUFFER% will be replaced by the corresponding
`imbue-mode' buffer name.")

(defun imbue-has-reference-definition (reference)
    "Find out whether Imbue REFERENCE is defined.

REFERENCE should include the square brackets, like [this]."
    (let ((reference (downcase reference)))
      (save-excursion
        (goto-char (point-min))
        (catch 'found
          (while (re-search-forward imbue-regex-reference-definition nil t)
            (when (string= reference (downcase (match-string-no-properties 1)))
              (throw 'found t)))))))

(defun imbue-get-undefined-refs ()
  "Return a list of undefined Imbue references.

Result is an alist of pairs (reference . occurencies), where
occurencies is itself another alist of pairs (label .
line-number).

For example, an alist corresponding to [Nice editor][Emacs] at line 12,
\[GNU Emacs][Emacs] at line 45 and [manual][elisp] at line 127 is
\((\"[emacs]\" (\"[Nice editor]\" . 12) (\"[GNU Emacs]\" . 45)) (\"[elisp]\" (\"[manual]\" . 127)))."
  (let ((missing))
    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward imbue-regex-link-reference nil t)
        (let* ((label (match-string-no-properties 1))
               (reference (match-string-no-properties 2))
               (target (downcase (if (string= reference "[]") label reference))))
          (unless (imbue-has-reference-definition target)
            (let ((entry (assoc target missing)))
              (if (not entry)
                  (add-to-list 'missing (cons target
                                              (list (cons label (imbue-line-number-at-pos)))) t)
                (setcdr entry
                        (append (cdr entry) (list (cons label (imbue-line-number-at-pos))))))))))
      missing)))

(defun imbue-add-missing-ref-definition (ref buffer &optional recheck)
  "Add blank REF definition to the end of BUFFER.

REF is a Imbue reference in square brackets, like \"[lisp-history]\".

When RECHECK is non-nil, BUFFER gets rechecked for undefined
references so that REF disappears from the list of those links."
  (with-current-buffer buffer
      (when (not (eq major-mode 'imbue-mode))
        (error "Not available in current mode"))
      (goto-char (point-max))
      (indent-new-comment-line)
      (insert (concat ref ": ")))
  (switch-to-buffer-other-window buffer)
  (goto-char (point-max))
  (when recheck
    (imbue-check-refs t)))

;; Button which adds an empty Imbue reference definition to the end
;; of buffer specified as its 'target-buffer property. Reference name
;; is button's label
(when (>= emacs-major-version 22)
  (define-button-type 'imbue-ref-button
    'help-echo "Push to create an empty reference definition"
    'face 'bold
    'action (lambda (b)
              (imbue-add-missing-ref-definition
               (button-label b) (button-get b 'target-buffer) t))))

;; Button jumping to line in buffer specified as its 'target-buffer
;; property. Line number is button's 'line property.
(when (>= emacs-major-version 22)
  (define-button-type 'goto-line-button
    'help-echo "Push to go to this line"
    'face 'italic
    'action (lambda (b)
              (message (button-get b 'buffer))
              (switch-to-buffer-other-window (button-get b 'target-buffer))
              (goto-line (button-get b 'target-line)))))

(defun imbue-check-refs (&optional silent)
  "Show all undefined Imbue references in current `imbue-mode' buffer.

If SILENT is non-nil, do not message anything when no undefined
references found.

Links which have empty reference definitions are considered to be
defined."
  (interactive "P")
  (when (not (eq major-mode 'imbue-mode))
    (error "Not available in current mode"))
  (let ((oldbuf (current-buffer))
        (refs (imbue-get-undefined-refs))
        (refbuf (get-buffer-create (replace-regexp-in-string
                                 "%BUFFER%" (buffer-name)
                                 imbue-refcheck-buffer t))))
    (if (null refs)
        (progn
          (when (not silent)
            (message "No undefined references found"))
          (kill-buffer refbuf))
      (with-current-buffer refbuf
        (when view-mode
          (View-exit-and-edit))
        (erase-buffer)
        (insert "Following references lack definitions:")
        (newline 2)
        (dolist (ref refs)
          (let ((button-label (format "%s" (car ref))))
            (if (>= emacs-major-version 22)
                ;; Create a reference button in Emacs 22
                (insert-text-button button-label
                                    :type 'imbue-ref-button
                                    'target-buffer oldbuf)
              ;; Insert reference as text in Emacs < 22
              (insert button-label)))
          (insert " (")
          (dolist (occurency (cdr ref))
            (let ((line (cdr occurency)))
              (if (>= emacs-major-version 22)
                  ;; Create a line number button in Emacs 22
                  (insert-button (number-to-string line)
                                 :type 'goto-line-button
                                 'target-buffer oldbuf
                                 'target-line line)
                ;; Insert line number as text in Emacs < 22
                (insert (number-to-string line)))
              (insert " "))) (delete-backward-char 1)
          (insert ")")
          (newline))
        (view-buffer-other-window refbuf)
        (goto-line 4)))))


;;; Outline ===================================================================

;; The following visibility cycling code was taken from org-mode
;; by Carsten Dominik and adapted for imbue-mode.

(defvar imbue-cycle-global-status 1)
(defvar imbue-cycle-subtree-status nil)

;; Based on org-end-of-subtree from org.el
(defun imbue-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil."
  (outline-back-to-heading invisible-OK)
  (let ((first t)
        (level (funcall outline-level)))
    (while (and (not (eobp))
                (or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

;; Based on org-cycle from org.el.
(defun imbue-cycle (&optional arg)
  "Visibility cycling for Imbue mode.
If ARG is t, perform global visibility cycling.  If the point is
at an atx-style header, cycle visibility of the corresponding
subtree.  Otherwise, insert a tab using `indent-relative'."
  (interactive "P")
  (cond
     ((eq arg t) ;; Global cycling
      (cond
       ((and (eq last-command this-command)
             (eq imbue-cycle-global-status 2))
        ;; Move from overview to contents
        (hide-sublevels 1)
        (message "CONTENTS")
        (setq imbue-cycle-global-status 3))

       ((and (eq last-command this-command)
             (eq imbue-cycle-global-status 3))
        ;; Move from contents to all
        (show-all)
        (message "SHOW ALL")
        (setq imbue-cycle-global-status 1))

       (t
        ;; Defaults to overview
        (hide-body)
        (message "OVERVIEW")
        (setq imbue-cycle-global-status 2))))

     ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      ;; At a heading: rotate between three different views
      (outline-back-to-heading)
      (let ((goal-column 0) eoh eol eos)
        ;; Determine boundaries
        (save-excursion
          (outline-back-to-heading)
          (save-excursion
            (beginning-of-line 2)
            (while (and (not (eobp)) ;; this is like `next-line'
                        (get-char-property (1- (point)) 'invisible))
              (beginning-of-line 2)) (setq eol (point)))
          (outline-end-of-heading)   (setq eoh (point))
          (imbue-end-of-subtree t)
          (skip-chars-forward " \t\n")
          (beginning-of-line 1) ; in case this is an item
          (setq eos (1- (point))))
        ;; Find out what to do next and set `this-command'
      (cond
         ((= eos eoh)
          ;; Nothing is hidden behind this heading
          (message "EMPTY ENTRY")
          (setq imbue-cycle-subtree-status nil))
         ((>= eol eos)
          ;; Entire subtree is hidden in one line: open it
          (show-entry)
          (show-children)
          (message "CHILDREN")
          (setq imbue-cycle-subtree-status 'children))
         ((and (eq last-command this-command)
               (eq imbue-cycle-subtree-status 'children))
          ;; We just showed the children, now show everything.
          (show-subtree)
          (message "SUBTREE")
          (setq imbue-cycle-subtree-status 'subtree))
         (t
          ;; Default action: hide the subtree.
          (hide-subtree)
          (message "FOLDED")
          (setq imbue-cycle-subtree-status 'folded)))))

     (t
      (message "TAB")
      (funcall indent-line-function))))

;; Based on org-shifttab from org.el.
(defun imbue-shifttab ()
  "Global visibility cycling.
Calls `imbue-cycle' with argument t."
  (interactive)
  (imbue-cycle t))

;;; Commands ==================================================================

(defun imbue ()
  "Run imbue on the current buffer and preview the output in another buffer."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (shell-command-on-region (region-beginning) (region-end) imbue-command
                               "*imbue-output*" nil)
    (shell-command-on-region (point-min) (point-max) imbue-command
                             "*imbue-output*" nil))
  (let (title)
    (setq title (buffer-name))
    (save-excursion
      (set-buffer "*imbue-output*")
      (goto-char (point-min))
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
              "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
              "\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"
              "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n"
              "<head>\n<title>")
      (insert title)
      (insert "</title>\n")
      (if imbue-css-path
          (insert "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
                  imbue-css-path
                  "\"  />\n"))
      (insert "</head>\n\n"
              "<body>\n\n")
      (goto-char (point-max))
      (insert "\n"
              "</body>\n"
              "</html>\n"))))

(defun imbue-preview ()
  "Run imbue on the current buffer and preview the output in a browser."
  (interactive)
  (imbue)
  (browse-url-of-buffer "*imbue-output*"))


;;; Miscellaneous =============================================================

(defun imbue-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of `line-number-at-pos' for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun imbue-nobreak-p ()
  "Returns nil if it is ok for fill-paragraph to insert a line
  break at point"
  ;; are we inside in square brackets
  (looking-back "\\[[^]]*"))



;;; Mode definition  ==========================================================

(defun imbue-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "imbue-mode, version %s" imbue-mode-version))

(define-derived-mode imbue-mode text-mode "Imbue"
  "Major mode for editing Imbue files."
  ;; Comments
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "<!--[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 0)
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(imbue-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; For menu support in XEmacs
  (easy-menu-add imbue-mode-menu imbue-mode-map)
  ;; Make filling work with lists (unordered, ordered, and definition)
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|^[ \t]*[*+-] \\|^[ \t*][0-9]+\\.\\|^[ \t]*: ")
  ;; Outline mode
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "#+")
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  ;; Indentation and filling
  (make-local-variable 'fill-nobreak-predicate)
  (add-hook 'fill-nobreak-predicate 'imbue-nobreak-p)
  (setq indent-line-function imbue-indent-function))

;(add-to-list 'auto-mode-alist '("\\.text$" . imbue-mode))

(provide 'imbue-mode)

;;; imbue-mode.el ends here
