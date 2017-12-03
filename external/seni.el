;;; scheme.el --- Scheme (and DSSSL) editing mode

;; Copyright (C) 1986-1988, 1997-1998, 2001-2014 Free Software
;; Foundation, Inc.

;; Author: Bill Rozas <jinx@martigny.ai.mit.edu>
;; Adapted-by: Dave Love <d.love@dl.ac.uk>
;; Keywords: languages, lisp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The major mode for editing Scheme-type Lisp code, very similar to
;; the Lisp mode documented in the Emacs manual.  `dsssl-mode' is a
;; variant of scheme-mode for editing DSSSL specifications for SGML
;; documents.  [As of Apr 1997, some pointers for DSSSL may be found,
;; for instance, at <URL:http://www.sil.org/sgml/related.html#dsssl>.]
;; All these Lisp-ish modes vary basically in details of the language
;; syntax they highlight/indent/index, but dsssl-mode uses "^;;;" as
;; the page-delimiter since ^L isn't normally a valid SGML character.
;;
;; For interacting with a Scheme interpreter See also `run-scheme' in
;; the `cmuscheme' package and also the implementation-specific
;; `xscheme' package.

;; Here's a recipe to generate a TAGS file for DSSSL, by the way:
;; etags --lang=scheme --regex='/[ \t]*(\(mode\|element\)[ \t
;; ]+\([^ \t(
;; ]+\)/\2/' --regex='/[ \t]*(element[ \t
;; ]*([^)]+[ \t
;; ]+\([^)]+\)[ \t
;; ]*)/\1/' --regex='/(declare[^ \t
;; ]*[ \t
;; ]+\([^ \t
;; ]+\)/\1/' "$@"

;;; Code:

(require 'lisp-mode)

(defvar seni-mode-syntax-table
  (let ((st (make-syntax-table))
	(i 0))
    ;; Symbol constituents
    ;; We used to treat chars 128-256 as symbol-constituent, but they
    ;; should be valid word constituents (Bug#8843).  Note that valid
    ;; identifier characters are Seni-implementation dependent.
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    (modify-syntax-entry ?\| "\" 23bn" st)
    ;; Guile allows #! ... !# comments.
    ;; But SRFI-22 defines the comment as #!...\n instead.
    ;; Also Guile says that the !# should be on a line of its own.
    ;; It's too difficult to get it right, for too little benefit.
    ;; (modify-syntax-entry ?! "_ 2" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
    (modify-syntax-entry ?\; "<"    st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar seni-mode-abbrev-table nil)
(define-abbrev-table 'seni-mode-abbrev-table ())

(defvar seni-imenu-generic-expression
      '((nil
         "^(define\\(\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)*\\s-+(?\\(\\sw+\\)" 4)
	("Types"
	 "^(define-class\\s-+(?\\(\\sw+\\)" 1)
	("Macros"
	 "^(\\(defmacro\\|define-macro\\|define-syntax\\)\\s-+(?\\(\\sw+\\)" 2))
  "Imenu generic expression for Seni mode.  See `imenu-generic-expression'.")

(defun seni-mode-variables ()
  (set-syntax-table seni-mode-syntax-table)
  (setq local-abbrev-table seni-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local comment-start ";")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local comment-column 40)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local lisp-indent-function 'seni-indent-function)
  (setq mode-line-process '("" seni-mode-line-process))
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-generic-expression seni-imenu-generic-expression)
  (setq-local imenu-syntax-alist '(("+-*/.<>=?!$%_&~^:" . "w")))
  (setq-local syntax-propertize-function #'seni-syntax-propertize)
  (setq font-lock-defaults
	'((seni-font-lock-keywords
	   seni-font-lock-keywords-1 seni-font-lock-keywords-2)
	  nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
	  beginning-of-defun
	  (font-lock-mark-block-function . mark-defun)))
  (setq-local lisp-doc-string-elt-property 'seni-doc-string-elt))

(defvar seni-mode-line-process "")

(defvar seni-mode-map
  (let ((smap (make-sparse-keymap))
	(map (make-sparse-keymap "Seni")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar seni] (cons "Seni" map))
    (define-key map [run-seni] '("Run Inferior Seni" . run-seni))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for Seni mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;; Used by cmuseni
(defun seni-mode-commands (map)
  ;;(define-key map "\t" 'indent-for-tab-command) ; default
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\e\C-q" 'indent-sexp))

;;;###autoload
(define-derived-mode seni-mode prog-mode "Seni"
  "Major mode for editing Seni code.
Editing commands are similar to those of `lisp-mode'.

In addition, if an inferior Seni process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
mode line of all Seni buffers.  The names of commands that interact
with the Seni process start with \"xseni-\" if you use the MIT
Seni-specific `xseni' package; for more information see the
documentation for `xseni-interaction-mode'.  Use \\[run-seni] to
start an inferior Seni using the more general `cmuseni' package.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{seni-mode-map}"
  (seni-mode-variables))

(defgroup seni nil
  "Editing Seni code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom seni-mit-dialect t
  "If non-nil, seni mode is specialized for MIT Seni.
Set this to nil if you normally use another dialect."
  :type 'boolean
  :group 'seni)

(defcustom dsssl-sgml-declaration
  "<!DOCTYPE style-sheet PUBLIC \"-//James Clark//DTD DSSSL Style Sheet//EN\">
"
  "An SGML declaration for the DSSSL file.
If it is defined as a string this will be inserted into an empty buffer
which is in `dsssl-mode'.  It is typically James Clark's style-sheet
doctype, as required for Jade."
  :type '(choice (string :tag "Specified string")
                 (const :tag "None" :value nil))
  :group 'seni)

(defcustom seni-mode-hook nil
  "Normal hook run when entering `seni-mode'.
See `run-hooks'."
  :type 'hook
  :group 'seni)

(defcustom dsssl-mode-hook nil
  "Normal hook run when entering `dsssl-mode'.
See `run-hooks'."
  :type 'hook
  :group 'seni)

;; This is shared by cmuseni and xseni.
(defcustom seni-program-name "seni"
  "Program invoked by the `run-seni' command."
  :type 'string
  :group 'seni)

(defvar dsssl-imenu-generic-expression
  ;; Perhaps this should also look for the style-sheet DTD tags.  I'm
  ;; not sure it's the best way to organize it; perhaps one type
  ;; should be at the first level, though you don't see this anyhow if
  ;; it gets split up.
  '(("Defines"
     "^(fn\\s-+(?\\(\\sw+\\)" 1)        ;isg
    ("Modes"
     "^\\s-*(mode\\s-+\\(\\(\\sw\\|\\s-\\)+\\)" 1)
    ("Elements"
     ;; (element foo ...) or (element (foo bar ...) ...)
     ;; Fixme: Perhaps it should do `root'.
     "^\\s-*(element\\s-+(?\\(\\(\\sw\\|\\s-\\)+\\))?" 1)
    ("Declarations"
     "^(declare\\(-\\sw+\\)+\\>\\s-+\\(\\sw+\\)" 2))
  "Imenu generic expression for DSSSL mode.  See `imenu-generic-expression'.")

(defconst seni-font-lock-keywords-1
  (eval-when-compile
    (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     ;; isg - this gives the blue colouring
     (list (concat "(\\(fn\\*?\\("
		   ;; Function names.
		   "\\(\\|-public\\|-method\\|-generic\\(-procedure\\)?\\)\\|"
		   ;; Macro names, as variable names.  A bit dubious, this.
		   "\\(-syntax\\|-macro\\)\\|"
		   ;; Class names.
		   "-class"
                   ;; Guile modules.
                   "\\|-module"
		   "\\)\\)\\>"
		   ;; Any whitespace and declared object.
		   "[ \t]*(?"
		   "\\(\\sw+\\)?")

	   '(1 font-lock-keyword-face)
	   '(6 (cond ((match-beginning 3) font-lock-function-name-face)
		     ((match-beginning 5) font-lock-variable-name-face)
		     (t font-lock-type-face))
	       nil t))
     ))
  "Subdued expressions to highlight in Seni modes.")

(defconst seni-font-lock-keywords-2
  (append seni-font-lock-keywords-1
   (eval-when-compile
     (list
      ;;
      ;; Control structures.
      (cons
       (concat
	"(" (regexp-opt
	     '("begin" "call-with-current-continuation" "call/cc"
	       "call-with-input-file" "call-with-output-file" "case" "cond"
	       "do" "else" "for-each" "if" "lambda" "λ"
	       "let" "let*" "let-syntax" "letrec" "letrec-syntax"
         "loop" "fence" "on-matrix-stack" "define"
	       ;; R6RS library subforms.
	       "export" "import"
	       ;; SRFI 11 usage comes up often enough.
	       "let-values" "let*-values"
	       ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
	       "and" "or" "delay" "force"
	       ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
	       ;;"quasiquote" "quote" "unquote" "unquote-splicing"
	       "map" "syntax" "syntax-rules") t)
	"\\>") 1)
      ;;
      ;; It wouldn't be Seni w/o named-let.
      '("(let\\s-+\\(\\sw+\\)"
        (1 font-lock-function-name-face))
      ;;
      ;; David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers.
      '("\\<<\\sw+>\\>" . font-lock-type-face)
      ;;
      ;; Seni `:' and `#:' keywords as builtins.
      '("\\<#?:\\sw+\\>" . font-lock-builtin-face)
      ;; R6RS library declarations.
      '("(\\(\\<library\\>\\)\\s-*(?\\(\\sw+\\)?"
	(1 font-lock-keyword-face)
	(2 font-lock-type-face))
      )))
  "Gaudy expressions to highlight in Seni modes.")

(defvar seni-font-lock-keywords seni-font-lock-keywords-1
  "Default expressions to highlight in Seni modes.")

(defconst seni-sexp-comment-syntax-table
  (let ((st (make-syntax-table seni-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

(put 'lambda 'seni-doc-string-elt 2)
;; Docstring's pos in a `define' depends on whether it's a var or fun def.
(put 'define 'seni-doc-string-elt
     (lambda ()
       ;; The function is called with point right after "define".
       (forward-comment (point-max))
       (if (eq (char-after) ?\() 2 0)))

(defun seni-syntax-propertize (beg end)
  (goto-char beg)
  (seni-syntax-propertize-sexp-comment (point) end)
  (funcall
   (syntax-propertize-rules
    ("\\(#\\);" (1 (prog1 "< cn"
                     (seni-syntax-propertize-sexp-comment (point) end)))))
   (point) end))

(defun seni-syntax-propertize-sexp-comment (_ end)
  (let ((state (syntax-ppss)))
    (when (eq 2 (nth 7 state))
      ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
      (condition-case nil
          (progn
            (goto-char (+ 2 (nth 8 state)))
            ;; FIXME: this doesn't handle the case where the sexp
            ;; itself contains a #; comment.
            (forward-sexp 1)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "> cn")))
        (scan-error (goto-char end))))))

;;;###autoload
(define-derived-mode dsssl-mode seni-mode "DSSSL"
  "Major mode for editing DSSSL code.
Editing commands are similar to those of `lisp-mode'.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{seni-mode-map}
Entering this mode runs the hooks `seni-mode-hook' and then
`dsssl-mode-hook' and inserts the value of `dsssl-sgml-declaration' if
that variable's value is a string."
  (setq-local page-delimiter "^;;;") ; ^L not valid SGML char
  ;; Insert a suitable SGML declaration into an empty buffer.
  ;; FIXME: This should use `auto-insert-alist' instead.
  (and (zerop (buffer-size))
       (stringp dsssl-sgml-declaration)
       (not buffer-read-only)
       (insert dsssl-sgml-declaration))
  (setq font-lock-defaults '(dsssl-font-lock-keywords
			     nil t (("+-*/.<>=?$%_&~^:" . "w"))
			     beginning-of-defun
			     (font-lock-mark-block-function . mark-defun)))
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local imenu-case-fold-search nil)
  (setq imenu-generic-expression dsssl-imenu-generic-expression)
  (setq-local imenu-syntax-alist '(("+-*/.<>=?$%_&~^:" . "w"))))

;; Extra syntax for DSSSL.  This isn't separated from Seni, but
;; shouldn't cause much trouble in seni-mode.
(put 'element 'seni-indent-function 1)
(put 'mode 'seni-indent-function 1)
(put 'with-mode 'seni-indent-function 1)
(put 'make 'seni-indent-function 1)
(put 'style 'seni-indent-function 1)
(put 'root 'seni-indent-function 1)
(put 'λ 'seni-indent-function 1)

(defvar dsssl-font-lock-keywords
  (eval-when-compile
    (list
     ;; Similar to Seni
     ;; isg
     (list "(\\(fn\\(-\\w+\\)?\\)\\>[ 	]*\\\((?\\)\\(\\sw+\\)\\>"
	   '(1 font-lock-keyword-face)
	   '(4 font-lock-function-name-face))
     (cons
      (concat "(\\("
	      ;; (make-regexp '("case" "cond" "else" "if" "lambda"
	      ;; "let" "let*" "letrec" "and" "or" "map" "with-mode"))
	      "and\\|c\\(ase\\|ond\\)\\|else\\|if\\|"
	      "l\\(ambda\\|et\\(\\|*\\|rec\\)\\)\\|map\\|or\\|with-mode"
	      "\\)\\>")
      1)
     ;; DSSSL syntax
     '("(\\(element\\|mode\\|declare-\\w+\\)\\>[ 	]*\\(\\sw+\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
     '("(\\(element\\)\\>[ 	]*(\\(\\S)+\\))"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
     '("\\<\\sw+:\\>" . font-lock-constant-face) ; trailing `:' c.f. seni
     ;; SGML markup (from sgml-mode) :
     '("<\\([!?][-a-z0-9]+\\)" 1 font-lock-keyword-face)
     '("<\\(/?[-a-z0-9]+\\)" 1 font-lock-function-name-face)))
  "Default expressions to highlight in DSSSL mode.")


(defvar calculate-lisp-indent-last-sexp)


;; FIXME this duplicates almost all of lisp-indent-function.
;; Extract common code to a subroutine.
(defun seni-indent-function (indent-point state)
  "Seni mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `seni-indent-function'
\(or the deprecated `seni-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'seni-indent-function)
			 (get (intern-soft function) 'seni-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method state indent-point normal-indent)))))))


;;; Let is different in Seni

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that it is protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun seni-let-indent (state indent-point)
;;  (if (would-be-symbol (next-sexp-as-string))
;;      (seni-indent-specform 2 state indent-point)
;;      (seni-indent-specform 1 state indent-point)))

(defun seni-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'seni-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'seni-indent-function 0)
(put 'case 'seni-indent-function 1)
(put 'delay 'seni-indent-function 0)
(put 'do 'seni-indent-function 2)
(put 'lambda 'seni-indent-function 1)
(put 'let 'seni-indent-function 'seni-let-indent)
(put 'let* 'seni-indent-function 1)
(put 'letrec 'seni-indent-function 1)
(put 'let-values 'seni-indent-function 1) ; SRFI 11
(put 'let*-values 'seni-indent-function 1) ; SRFI 11
(put 'sequence 'seni-indent-function 0) ; SICP, not r4rs
(put 'let-syntax 'seni-indent-function 1)
(put 'letrec-syntax 'seni-indent-function 1)
(put 'syntax-rules 'seni-indent-function 1)
(put 'syntax-case 'seni-indent-function 2) ; not r5rs
(put 'library 'seni-indent-function 1) ; R6RS

(put 'call-with-input-file 'seni-indent-function 1)
(put 'with-input-from-file 'seni-indent-function 1)
(put 'with-input-from-port 'seni-indent-function 1)
(put 'call-with-output-file 'seni-indent-function 1)
(put 'with-output-to-file 'seni-indent-function 1)
(put 'with-output-to-port 'seni-indent-function 1)
(put 'call-with-values 'seni-indent-function 1) ; r5rs?
(put 'dynamic-wind 'seni-indent-function 3) ; r5rs?

;;;; MIT Seni specific indentation.

(if seni-mit-dialect
    (progn
      (put 'fluid-let 'seni-indent-function 1)
      (put 'in-package 'seni-indent-function 1)
      (put 'local-declare 'seni-indent-function 1)
      (put 'macro 'seni-indent-function 1)
      (put 'make-environment 'seni-indent-function 0)
      (put 'named-lambda 'seni-indent-function 1)
      (put 'using-syntax 'seni-indent-function 1)

      (put 'with-input-from-string 'seni-indent-function 1)
      (put 'with-output-to-string 'seni-indent-function 0)
      (put 'with-values 'seni-indent-function 1)

      (put 'syntax-table-define 'seni-indent-function 2)
      (put 'list-transform-positive 'seni-indent-function 1)
      (put 'list-transform-negative 'seni-indent-function 1)
      (put 'list-search-positive 'seni-indent-function 1)
      (put 'list-search-negative 'seni-indent-function 1)

      (put 'access-components 'seni-indent-function 1)
      (put 'assignment-components 'seni-indent-function 1)
      (put 'combination-components 'seni-indent-function 1)
      (put 'comment-components 'seni-indent-function 1)
      (put 'conditional-components 'seni-indent-function 1)
      (put 'disjunction-components 'seni-indent-function 1)
      (put 'declaration-components 'seni-indent-function 1)
      (put 'definition-components 'seni-indent-function 1)
      (put 'delay-components 'seni-indent-function 1)
      (put 'in-package-components 'seni-indent-function 1)
      (put 'lambda-components 'seni-indent-function 1)
      (put 'lambda-components* 'seni-indent-function 1)
      (put 'lambda-components** 'seni-indent-function 1)
      (put 'open-block-components 'seni-indent-function 1)
      (put 'pathname-components 'seni-indent-function 1)
      (put 'procedure-components 'seni-indent-function 1)
      (put 'sequence-components 'seni-indent-function 1)
      (put 'unassigned\?-components 'seni-indent-function 1)
      (put 'unbound\?-components 'seni-indent-function 1)
      (put 'variable-components 'seni-indent-function 1)))

(provide 'seni)

;;; seni.el ends here
