;;; atscript.el --- Major mode for editing atscript

;;; Version: 0.0.0
;;; Author: indy
;;; URL: https://github.com/indy/atscript-mode

;; -----------------------------------------------------------------------------------
;;     Atscript support for Emacs
;;     Unmodified original source available at http://www.karllandstrom.se/downloads/emacs/javascript.el
;;     Copyright (c) 2008 Free Software Foundation
;;     Portions Copyright (C) Microsoft Open Technologies, Inc. All rights reserved.
;; 
;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;; 
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;; 
;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; -------------------------------------------------------------------------------------------

;;; Commentary

;; This is a fork of typescript-mode provided by Microsoft.

;; Original commentary is below.
;; This is based on Karl Landstrom's barebones typescript-mode. This
;; is much more robust and works with cc-mode's comment filling
;; (mostly).
;; The modifications to the original javascript.el mode mainly consisted in 
;; replacing "javascript" with "atscript"
;;
;; The main features of this atscript mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments, C preprocessor fontification, and MozRepl integration.
;;
;; 
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments
;;
;; Exported names start with "atscript-"; private names start with
;; "atscript--".

;;; Code:

(eval-and-compile
  (require 'cc-mode)
  (require 'font-lock)
  (require 'newcomment)
  (require 'imenu)
  (require 'etags)
  (require 'thingatpt)
  (require 'easymenu)
  (require 'moz nil t)
  (require 'atscripton nil t))

(eval-when-compile
  (require 'cl)
  (require 'comint)
  (require 'ido))

(defvar inferior-moz-buffer)
(defvar moz-repl-name)
(defvar ido-cur-list)
(declare-function ido-mode "ido" ())
(declare-function inferior-moz-process "ext:mozrepl" ())

;;; Constants

(defconst atscript--name-start-re "[a-zA-Z_$]"
  "Regexp matching the start of a atscript identifier, without grouping.")

(defconst atscript--stmt-delim-chars "^;{}?:")

(defconst atscript--name-re (concat atscript--name-start-re
                              "\\(?:\\s_\\|\\sw\\)*")
  "Regexp matching a atscript identifier, without grouping.")

(defconst atscript--objfield-re (concat atscript--name-re ":")
  "Regexp matching the start of a atscript object field.")

(defconst atscript--dotted-name-re
  (concat atscript--name-re "\\(?:\\." atscript--name-re "\\)*")
  "Regexp matching a dot-separated sequence of atscript names.")

(defconst atscript--cpp-name-re atscript--name-re
  "Regexp matching a C preprocessor name.")

(defconst atscript--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defconst atscript--plain-method-re
  (concat "^\\s-*?\\(" atscript--dotted-name-re "\\)\\.prototype"
          "\\.\\(" atscript--name-re "\\)\\s-*?=\\s-*?\\(function\\)\\_>")
  "Regexp matching an explicit atscript prototype \"method\" declaration.
Group 1 is a (possibly-dotted) class name, group 2 is a method name,
and group 3 is the 'function' keyword.")

(defconst atscript--plain-class-re
  (concat "^\\s-*\\(" atscript--dotted-name-re "\\)\\.prototype"
          "\\s-*=\\s-*{")
  "Regexp matching a atscript explicit prototype \"class\" declaration.
An example of this is \"Class.prototype = { method1: ...}\".")

;; var NewClass = BaseClass.extend(
(defconst atscript--mp-class-decl-re
  (concat "^\\s-*var\\s-+"
          "\\(" atscript--name-re "\\)"
          "\\s-*=\\s-*"
          "\\(" atscript--dotted-name-re
          "\\)\\.extend\\(?:Final\\)?\\s-*(\\s-*{?\\s-*$"))

;; var NewClass = Class.create()
(defconst atscript--prototype-obsolete-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" atscript--dotted-name-re "\\)"
          "\\s-*=\\s-*Class\\.create()"))

(defconst atscript--prototype-objextend-class-decl-re-1
  (concat "^\\s-*Object\\.extend\\s-*("
          "\\(" atscript--dotted-name-re "\\)"
          "\\s-*,\\s-*{"))

(defconst atscript--prototype-objextend-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" atscript--dotted-name-re "\\)"
          "\\s-*=\\s-*Object\\.extend\\s-*\("))

;; var NewClass = Class.create({
(defconst atscript--prototype-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" atscript--name-re "\\)"
          "\\s-*=\\s-*Class\\.create\\s-*(\\s-*"
          "\\(?:\\(" atscript--dotted-name-re "\\)\\s-*,\\s-*\\)?{?"))

;; Parent class name(s) (yes, multiple inheritance in atscript) are
;; matched with dedicated font-lock matchers
(defconst atscript--dojo-class-decl-re
  (concat "^\\s-*dojo\\.declare\\s-*(\"\\(" atscript--dotted-name-re "\\)"))

(defconst atscript--extatscript-class-decl-re-1
  (concat "^\\s-*Ext\\.extend\\s-*("
          "\\s-*\\(" atscript--dotted-name-re "\\)"
          "\\s-*,\\s-*\\(" atscript--dotted-name-re "\\)")
  "Regexp matching an ExtATSCRIPT class declaration (style 1).")

(defconst atscript--extatscript-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" atscript--name-re "\\)"
          "\\s-*=\\s-*Ext\\.extend\\s-*(\\s-*"
          "\\(" atscript--dotted-name-re "\\)")
  "Regexp matching an ExtATSCRIPT class declaration (style 2).")

(defconst atscript--mochikit-class-re
  (concat "^\\s-*MochiKit\\.Base\\.update\\s-*(\\s-*"
          "\\(" atscript--dotted-name-re "\\)")
  "Regexp matching a MochiKit class declaration.")

(defconst atscript--dummy-class-style
  '(:name "[Automatically Generated Class]"))

(defconst atscript--class-styles
  `((:name            "Plain"
     :class-decl      ,atscript--plain-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       atscript)

    (:name            "MochiKit"
     :class-decl      ,atscript--mochikit-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       mochikit)

    (:name            "Prototype (Obsolete)"
     :class-decl      ,atscript--prototype-obsolete-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Modern)"
     :class-decl      ,atscript--prototype-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend)"
     :class-decl      ,atscript--prototype-objextend-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend) 2"
     :class-decl      ,atscript--prototype-objextend-class-decl-re-2
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Dojo"
     :class-decl      ,atscript--dojo-class-decl-re
     :contexts        (toplevel)
     :framework       dojo)

    (:name            "ExtATSCRIPT (style 1)"
     :class-decl      ,atscript--extatscript-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       extatscript)

    (:name            "ExtATSCRIPT (style 2)"
     :class-decl      ,atscript--extatscript-class-decl-re-2
     :contexts        (toplevel)
     :framework       extatscript)

    (:name            "Merrill Press"
     :class-decl      ,atscript--mp-class-decl-re
     :contexts        (toplevel)
     :framework       merrillpress))

  "List of atscript class definition styles.

A class definition style is a plist with the following keys:

:name is a human-readable name of the class type

:class-decl is a regular expression giving the start of the
class.  Its first group must match the name of its class.  If there
is a parent class, the second group should match, and it should be
the name of the class.

If :prototype is present and non-nil, the parser will merge
declarations for this constructs with others at the same lexical
level that have the same name.  Otherwise, multiple definitions
will create multiple top-level entries.  Don't use :prototype
unnecessarily: it has an associated cost in performance.

If :strip-prototype is present and non-nil, then if the class
name as matched contains
")

(defconst atscript--available-frameworks
  (loop with available-frameworks
        for style in atscript--class-styles
        for framework = (plist-get style :framework)
        unless (memq framework available-frameworks)
        collect framework into available-frameworks
        finally return available-frameworks)
  "List of available atscript frameworks symbols.")

(defconst atscript--function-heading-1-re
  (concat
   "^\\s-*function\\s-+\\(" atscript--name-re "\\)")
  "Regexp matching the start of a atscript function header.
Match group 1 is the name of the function.")

(defconst atscript--function-heading-2-re
  (concat
   "^\\s-*\\(" atscript--name-re "\\)\\s-*:\\s-*function\\_>")
  "Regexp matching the start of a function entry in an associative array.
Match group 1 is the name of the function.")

(defconst atscript--function-heading-3-re
  (concat
   "^\\s-*\\(?:var\\s-+\\)?\\(" atscript--dotted-name-re "\\)"
   "\\s-*=\\s-*function\\_>")
  "Regexp matching a line in the atscript form \"var MUMBLE = function\".
Match group 1 is MUMBLE.")

(defconst atscript--macro-decl-re
  (concat "^\\s-*#\\s-*define\\s-+\\(" atscript--cpp-name-re "\\)\\s-*(")
  "Regexp matching a CPP macro definition, up to the opening parenthesis.
Match group 1 is the name of the macro.")

(defun atscript--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defconst atscript--keyword-re
  (atscript--regexp-opt-symbol
   '("any" "boolean" "break" "case" "catch" "class" "constructor"
     "const" "continue" "declare" "default" "delete" "do" "else"
     "enum" "export" "extends" "extern" "false" "finally" "for"
     "function" "goto" "if" "implements" "import" "in"
     "instanceof" "interface" "let" "module" "new" "null" "number"
      "private" "public" "return" "static" "string"
     "super" "switch" "symbol" "this" "throw" "true" 
     "try" "typeof" "var" "void" 
     "while" ))
  "Regexp matching any atscript keyword.")

(defconst atscript--basic-type-re
  (atscript--regexp-opt-symbol
   '("bool" "string" "number" "any" "void"))
  "Regular expression matching any predefined type in atscript.")

(defconst atscript--constant-re
  (atscript--regexp-opt-symbol '("false" "null" "undefined"
                                 "Infinity" "NaN"
                                 "true" "arguments" "this"))
  "Regular expression matching any future reserved words in atscript.")


(defconst atscript--font-lock-keywords-1
  (list
   "\\_<import\\_>"
   (list atscript--function-heading-1-re 1 font-lock-function-name-face)
   (list atscript--function-heading-2-re 1 font-lock-function-name-face))
  "Level one font lock keywords for `atscript-mode'.")

(defconst atscript--font-lock-keywords-2
  (append atscript--font-lock-keywords-1
          (list (list atscript--keyword-re 1 font-lock-keyword-face)
                (list "\\_<for\\_>"
                      "\\s-+\\(each\\)\\_>" nil nil
                      (list 1 'font-lock-keyword-face))
                (cons atscript--basic-type-re font-lock-type-face)
                (cons atscript--constant-re font-lock-constant-face)))
  "Level two font lock keywords for `atscript-mode'.")

;; atscript--pitem is the basic building block of the lexical
;; database. When one refers to a real part of the buffer, the region
;; of text to which it refers is split into a conceptual header and
;; body. Consider the (very short) block described by a hypothetical
;; atscript--pitem:
;;
;;   function foo(a,b,c) { return 42; }
;;   ^                    ^            ^
;;   |                    |            |
;;   +- h-begin           +- h-end     +- b-end
;;
;; (Remember that these are buffer positions, and therefore point
;; between characters, not at them. An arrow drawn to a character
;; indicates the corresponding position is between that character and
;; the one immediately preceding it.)
;;
;; The header is the region of text [h-begin, h-end], and is
;; the text needed to unambiguously recognize the start of the
;; construct. If the entire header is not present, the construct is
;; not recognized at all. No other pitems may be nested inside the
;; header.
;;
;; The body is the region [h-end, b-end]. It may contain nested
;; atscript--pitem instances. The body of a pitem may be empty: in
;; that case, b-end is equal to header-end.
;;
;; The three points obey the following relationship:
;;
;;   h-begin < h-end <= b-end
;;
;; We put a text property in the buffer on the character *before*
;; h-end, and if we see it, on the character *before* b-end.
;;
;; The text property for h-end, atscript--pstate, is actually a list
;; of all atscript--pitem instances open after the marked character.
;;
;; The text property for b-end, atscript--pend, is simply the
;; atscript--pitem that ends after the marked character. (Because
;; pitems always end when the paren-depth drops below a critical
;; value, and because we can only drop one level per character, only
;; one pitem may end at a given character.)
;;
;; In the structure below, we only store h-begin and (sometimes)
;; b-end. We can trivially and quickly find h-end by going to h-begin
;; and searching for an atscript--pstate text property. Since no other
;; atscript--pitem instances can be nested inside the header of a
;; pitem, the location after the character with this text property
;; must be h-end.
;;
;; atscript--pitem instances are never modified (with the exception
;; of the b-end field). Instead, modified copies are added at subseqnce parse points.
;; (The exception for b-end and its caveats is described below.)
;;

(defstruct (atscript--pitem (:type list))
  ;; IMPORTANT: Do not alter the position of fields within the list.
  ;; Various bits of code depend on their positions, particularly
  ;; anything that manipulates the list of children.

  ;; List of children inside this pitem's body
  (children nil :read-only t)

  ;; When we reach this paren depth after h-end, the pitem ends
  (paren-depth nil :read-only t)

  ;; Symbol or class-style plist if this is a class
  (type nil :read-only t)

  ;; See above
  (h-begin nil :read-only t)

  ;; List of strings giving the parts of the name of this pitem (e.g.,
  ;; '("MyClass" "myMethod"), or t if this pitem is anonymous
  (name nil :read-only t)

  ;; THIS FIELD IS MUTATED, and its value is shared by all copies of
  ;; this pitem: when we copy-and-modify pitem instances, we share
  ;; their tail structures, so all the copies actually have the same
  ;; terminating cons cell. We modify that shared cons cell directly.
  ;;
  ;; The field value is either a number (buffer location) or nil if
  ;; unknown.
  ;;
  ;; If the field's value is greater than `atscript--cache-end', the
  ;; value is stale and must be treated as if it were nil. Conversely,
  ;; if this field is nil, it is guaranteed that this pitem is open up
  ;; to at least `atscript--cache-end'. (This property is handy when
  ;; computing whether we're inside a given pitem.)
  ;;
  (b-end nil))

;; The pitem we start parsing with.
(defconst atscript--initial-pitem
  (make-atscript--pitem
   :paren-depth most-negative-fixnum
   :type 'toplevel))

;;; User Customization

(defgroup atscript nil
  "Customization variables for atscript mode."
  :tag "atscript"
  :group 'languages)

(defcustom atscript-indent-level 4
  "Number of spaces for each indentation step in `atscript-mode'."
  :type 'integer
  :group 'atscript)

(defcustom atscript-expr-indent-offset 0
  "Number of additional spaces used for indentation of continued expressions.
The value must be no less than minus `atscript-indent-level'."
  :type 'integer
  :group 'atscript)

(defcustom atscript-auto-indent-flag t
  "Whether to automatically indent when typing punctuation characters.
If non-nil, the characters {}();,: also indent the current line
in atscript mode."
  :type 'boolean
  :group 'atscript)

(defcustom atscript-flat-functions nil
  "Treat nested functions as top-level functions in `atscript-mode'.
This applies to function movement, marking, and so on."
  :type 'boolean
  :group 'atscript)

(defcustom atscript-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `atscript-mode'."
  :type 'function
  :group 'atscript)

(defcustom atscript-enabled-frameworks atscript--available-frameworks
  "Frameworks recognized by `atscript-mode'.
To improve performance, you may turn off some frameworks you
seldom use, either globally or on a per-buffer basis."
  :type (cons 'set (mapcar (lambda (x)
                             (list 'const x))
                           atscript--available-frameworks))
  :group 'atscript)

(defcustom atscript-atscript-switch-tabs
  (and (memq system-type '(darwin)) t)
  "Whether `atscript-mode' should display tabs while selecting them.
This is useful only if the windowing system has a good mechanism
for preventing Firefox from stealing the keyboard focus."
  :type 'boolean
  :group 'atscript)

(defcustom atscript-atscript-tmpdir
  "~/.emacs.d/atscript/atscript"
  "Temporary directory used by `atscript-mode' to communicate with Mozilla.
This directory must be readable and writable by both Mozilla and
Emacs."
  :type 'directory
  :group 'atscript)

(defcustom atscript-atscript-timeout 5
  "Reply timeout for executing commands in Mozilla via `atscript-mode'.
The value is given in seconds.  Increase this value if you are
getting timeout messages."
  :type 'integer
  :group 'atscript)

;;; KeyMap

(defvar atscript-mode-map
  (let ((keymap (make-sparse-keymap)))
    (mapc (lambda (key)
	    (define-key keymap key #'atscript-insert-and-indent))
	  '("{" "}" "(" ")" ":" ";" ","))
    (define-key keymap [(control ?c) (meta ?:)] #'atscript-eval)
    (define-key keymap [(control ?c) (control ?j)] #'atscript-set-atscript-context)
    (define-key keymap [(control meta ?x)] #'atscript-eval-defun)
    (define-key keymap [(meta ?.)] #'atscript-find-symbol)
    (easy-menu-define nil keymap "atscript Menu"
      '("atscript"
        ["Select new Mozilla context&" atscript-set-atscript-context
         (fboundp #'inferior-moz-process)]
        ["Evaluate expression in Mozilla context&" atscript-eval
         (fboundp #'inferior-moz-process)]
        ["Send current function to Mozilla&" atscript-eval-defun
         (fboundp #'inferior-moz-process)]))
    keymap)
  "Keymap for `atscript-mode'.")

(defun atscript-insert-and-indent (key)
  "Run the command bound to KEY, and indent if necessary.
Indentation does not take place if point is in a string or
comment."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (let ((syntax (save-restriction (widen) (syntax-ppss))))
    (when (or (and (not (nth 8 syntax))
                   atscript-auto-indent-flag)
              (and (nth 4 syntax)
                   (eq (current-column)
                       (1+ (current-indentation)))))
      (indent-according-to-mode))))


;;; Syntax table and parsing

(defvar atscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    table)
  "Syntax table for `atscript-mode'.")

(defvar atscript--quick-match-re nil
  "Autogenerated regexp used by `atscript-mode' to match buffer constructs.")

(defvar atscript--quick-match-re-func nil
  "Autogenerated regexp used by `atscript-mode' to match constructs and functions.")

(make-variable-buffer-local 'atscript--quick-match-re)
(make-variable-buffer-local 'atscript--quick-match-re-func)

(defvar atscript--cache-end 1
  "Last valid buffer position for the `atscript-mode' function cache.")
(make-variable-buffer-local 'atscript--cache-end)

(defvar atscript--last-parse-pos nil
  "Latest parse position reached by `atscript--ensure-cache'.")
(make-variable-buffer-local 'atscript--last-parse-pos)

(defvar atscript--state-at-last-parse-pos nil
  "Parse state at `atscript--last-parse-pos'.")
(make-variable-buffer-local 'atscript--state-at-last-parse-pos)

(defun atscript--flatten-list (list)
  (loop for item in list
        nconc (cond ((consp item)
                     (atscript--flatten-list item))
                    (item (list item)))))

(defun atscript--maybe-join (prefix separator suffix &rest list)
  "Helper function for `atscript--update-quick-match-re'.
If LIST contains any element that is not nil, return its non-nil
elements, separated by SEPARATOR, prefixed by PREFIX, and ended
with SUFFIX as with `concat'.  Otherwise, if LIST is empty, return
nil.  If any element in LIST is itself a list, flatten that
element."
  (setq list (atscript--flatten-list list))
  (when list
    (concat prefix (mapconcat #'identity list separator) suffix)))

(defun atscript--update-quick-match-re ()
  "Internal function used by `atscript-mode' for caching buffer constructs.
This updates `atscript--quick-match-re', based on the current set of
enabled frameworks."
  (setq atscript--quick-match-re
        (atscript--maybe-join
         "^[ \t]*\\(?:" "\\|" "\\)"

         ;; #define mumble
         "#define[ \t]+[a-zA-Z_]"

         (when (memq 'extatscript atscript-enabled-frameworks)
           "Ext\\.extend")

         (when (memq 'prototype atscript-enabled-frameworks)
           "Object\\.extend")

          ;; var mumble = THING (
         (atscript--maybe-join
          "\\(?:var[ \t]+\\)?[a-zA-Z_$0-9.]+[ \t]*=[ \t]*\\(?:"
          "\\|"
          "\\)[ \t]*\("

          (when (memq 'prototype atscript-enabled-frameworks)
                    "Class\\.create")

          (when (memq 'extatscript atscript-enabled-frameworks)
            "Ext\\.extend")

          (when (memq 'merrillpress atscript-enabled-frameworks)
            "[a-zA-Z_$0-9]+\\.extend\\(?:Final\\)?"))

         (when (memq 'dojo atscript-enabled-frameworks)
           "dojo\\.declare[ \t]*\(")

         (when (memq 'mochikit atscript-enabled-frameworks)
           "MochiKit\\.Base\\.update[ \t]*\(")

         ;; mumble.prototypeTHING
         (atscript--maybe-join
          "[a-zA-Z_$0-9.]+\\.prototype\\(?:" "\\|" "\\)"

          (when (memq 'atscript atscript-enabled-frameworks)
            '( ;; foo.prototype.bar = function(
              "\\.[a-zA-Z_$0-9]+[ \t]*=[ \t]*function[ \t]*\("

              ;; mumble.prototype = {
              "[ \t]*=[ \t]*{")))))

  (setq atscript--quick-match-re-func
        (concat "function\\|" atscript--quick-match-re)))

(defun atscript--forward-text-property (propname)
  "Move over the next value of PROPNAME in the buffer.
If found, return that value and leave point after the character
having that value; otherwise, return nil and leave point at EOB."
  (let ((next-value (get-text-property (point) propname)))
    (if next-value
        (forward-char)

      (goto-char (next-single-property-change
                  (point) propname nil (point-max)))
      (unless (eobp)
        (setq next-value (get-text-property (point) propname))
        (forward-char)))

    next-value))

(defun atscript--backward-text-property (propname)
  "Move over the previous value of PROPNAME in the buffer.
If found, return that value and leave point just before the
character that has that value, otherwise return nil and leave
point at BOB."
    (unless (bobp)
      (let ((prev-value (get-text-property (1- (point)) propname)))
        (if prev-value
            (backward-char)

          (goto-char (previous-single-property-change
                      (point) propname nil (point-min)))

          (unless (bobp)
            (backward-char)
            (setq prev-value (get-text-property (point) propname))))

        prev-value)))

(defsubst atscript--forward-pstate ()
  (atscript--forward-text-property 'atscript--pstate))

(defsubst atscript--backward-pstate ()
  (atscript--backward-text-property 'atscript--pstate))

(defun atscript--pitem-goto-h-end (pitem)
  (goto-char (atscript--pitem-h-begin pitem))
  (atscript--forward-pstate))

(defun atscript--re-search-forward-inner (regexp &optional bound count)
  "Helper function for `atscript--re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (atscript--beginning-of-macro)
                            (c-end-of-macro)
                            (point)))))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            ((and (not (and orig-macro-end
                            (<= (point) orig-macro-end)))
                  (atscript--beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun atscript--re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(atscript--re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(atscript--re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(atscript--re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun atscript--re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `atscript--re-search-backward'."
  (let ((parse)
        str-terminator
        (orig-macro-start
         (save-excursion
           (and (atscript--beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (atscript--beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun atscript--re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(atscript--re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(atscript--re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(atscript--re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun atscript--forward-expression ()
  "Move forward over a whole atscript expression.
This function doesn't move over expressions continued across
lines."
  (loop
   ;; non-continued case; simplistic, but good enough?
   do (loop until (or (eolp)
                      (progn
                        (forward-comment most-positive-fixnum)
                        (memq (char-after) '(?\, ?\; ?\] ?\) ?\}))))
            do (forward-sexp))

   while (and (eq (char-after) ?\n)
              (save-excursion
                (forward-char)
                (atscript--continued-expression-p)))))

(defun atscript--forward-function-decl ()
  "Move forward over a atscript function declaration.
This puts point at the 'function' keyword.

If this is a syntactically-correct non-expression function,
return the name of the function, or t if the name could not be
determined.  Otherwise, return nil."
  (assert (looking-at "\\_<function\\_>"))
  (let ((name t))
    (forward-word)
    (forward-comment most-positive-fixnum)
    (when (looking-at atscript--name-re)
      (setq name (match-string-no-properties 0))
      (goto-char (match-end 0)))
    (forward-comment most-positive-fixnum)
    (and (eq (char-after) ?\( )
         (ignore-errors (forward-list) t)
         (progn (forward-comment most-positive-fixnum)
                (and (eq (char-after) ?{)
                     name)))))

(defun atscript--function-prologue-beginning (&optional pos)
  "Return the start of the atscript function prologue containing POS.
A function prologue is everything from start of the definition up
to and including the opening brace.  POS defaults to point.
If POS is not in a function prologue, return nil."
  (let (prologue-begin)
    (save-excursion
      (if pos
          (goto-char pos)
        (setq pos (point)))

      (when (save-excursion
              (forward-line 0)
              (or (looking-at atscript--function-heading-2-re)
                  (looking-at atscript--function-heading-3-re)))

        (setq prologue-begin (match-beginning 1))
        (when (<= prologue-begin pos)
          (goto-char (match-end 0))))

      (skip-syntax-backward "w_")
      (and (or (looking-at "\\_<function\\_>")
               (atscript--re-search-backward "\\_<function\\_>" nil t))

           (save-match-data (goto-char (match-beginning 0))
                            (atscript--forward-function-decl))

           (<= pos (point))
           (or prologue-begin (match-beginning 0))))))

(defun atscript--beginning-of-defun-raw ()
  "Helper function for `atscript-beginning-of-defun'.
Go to previous defun-beginning and return the parse state for it,
or nil if we went all the way back to bob and don't find
anything."
  (atscript--ensure-cache)
  (let (pstate)
    (while (and (setq pstate (atscript--backward-pstate))
                (not (eq 'function (atscript--pitem-type (car pstate))))))
    (and (not (bobp)) pstate)))

(defun atscript--pstate-is-toplevel-defun (pstate)
  "Helper function for `atscript--beginning-of-defun-nested'.
If PSTATE represents a non-empty top-level defun, return the
top-most pitem.  Otherwise, return nil."
  (loop for pitem in pstate
        with func-depth = 0
        with func-pitem
        if (eq 'function (atscript--pitem-type pitem))
        do (incf func-depth)
        and do (setq func-pitem pitem)
        finally return (if (eq func-depth 1) func-pitem)))

(defun atscript--beginning-of-defun-nested ()
  "Helper function for `atscript--beginning-of-defun'.
Return the pitem of the function we went to the beginning of."
  (or
   ;; Look for the smallest function that encloses point...
   (loop for pitem in (atscript--parse-state-at-point)
         if (and (eq 'function (atscript--pitem-type pitem))
                 (atscript--inside-pitem-p pitem))
         do (goto-char (atscript--pitem-h-begin pitem))
         and return pitem)

   ;; ...and if that isn't found, look for the previous top-level
   ;; defun
   (loop for pstate = (atscript--backward-pstate)
         while pstate
         if (atscript--pstate-is-toplevel-defun pstate)
         do (goto-char (atscript--pitem-h-begin it))
         and return it)))

(defun atscript--beginning-of-defun-flat ()
  "Helper function for `atscript-beginning-of-defun'."
  (let ((pstate (atscript--beginning-of-defun-raw)))
    (when pstate
      (goto-char (atscript--pitem-h-begin (car pstate))))))

(defun atscript-beginning-of-defun (&optional arg)
  "Value of `beginning-of-defun-function' for `atscript-mode'."
  (setq arg (or arg 1))
  (while (and (not (eobp)) (< arg 0))
    (incf arg)
    (when (and (not atscript-flat-functions)
               (or (eq (atscript-syntactic-context) 'function)
                   (atscript--function-prologue-beginning)))
      (atscript-end-of-defun))

    (if (atscript--re-search-forward
         "\\_<function\\_>" nil t)
        (goto-char (atscript--function-prologue-beginning))
      (goto-char (point-max))))

  (while (> arg 0)
    (decf arg)
    ;; If we're just past the end of a function, the user probably wants
    ;; to go to the beginning of *that* function
    (when (eq (char-before) ?})
      (backward-char))

    (let ((prologue-begin (atscript--function-prologue-beginning)))
      (cond ((and prologue-begin (< prologue-begin (point)))
             (goto-char prologue-begin))

            (atscript-flat-functions
             (atscript--beginning-of-defun-flat))
            (t
             (atscript--beginning-of-defun-nested))))))

(defun atscript--flush-caches (&optional beg ignored)
  "Flush the `atscript-mode' syntax cache after position BEG.
BEG defaults to `point-min', meaning to flush the entire cache."
  (interactive)
  (setq beg (or beg (save-restriction (widen) (point-min))))
  (setq atscript--cache-end (min atscript--cache-end beg)))

(defmacro atscript--debug (&rest arguments)
  ;; `(message ,@arguments)
  )

(defun atscript--ensure-cache--pop-if-ended (open-items paren-depth)
  (let ((top-item (car open-items)))
    (when (<= paren-depth (atscript--pitem-paren-depth top-item))
      (assert (not (get-text-property (1- (point)) 'atscript-pend)))
      (put-text-property (1- (point)) (point) 'atscript--pend top-item)
      (setf (atscript--pitem-b-end top-item) (point))
      (setq open-items
            ;; open-items must contain at least two items for this to
            ;; work, but because we push a dummy item to start with,
            ;; that assumption holds.
            (cons (atscript--pitem-add-child (second open-items) top-item)
                  (cddr open-items)))))
  open-items)

(defmacro atscript--ensure-cache--update-parse ()
  "Helper function for `atscript--ensure-cache'.
Update parsing information up to point, referring to parse,
prev-parse-point, goal-point, and open-items bound lexically in
the body of `atscript--ensure-cache'."
  `(progn
     (setq goal-point (point))
     (goto-char prev-parse-point)
     (while (progn
              (setq open-items (atscript--ensure-cache--pop-if-ended
                                open-items (car parse)))
              ;; Make sure parse-partial-sexp doesn't stop because we *entered*
              ;; the given depth -- i.e., make sure we're deeper than the target
              ;; depth.
              (assert (> (nth 0 parse)
                         (atscript--pitem-paren-depth (car open-items))))
              (setq parse (parse-partial-sexp
                           prev-parse-point goal-point
                           (atscript--pitem-paren-depth (car open-items))
                           nil parse))

;;              (let ((overlay (make-overlay prev-parse-point (point))))
;;                (overlay-put overlay 'face '(:background "red"))
;;                (unwind-protect
;;                     (progn
;;                       (atscript--debug "parsed: %S" parse)
;;                       (sit-for 1))
;;                  (delete-overlay overlay)))

              (setq prev-parse-point (point))
              (< (point) goal-point)))

     (setq open-items (atscript--ensure-cache--pop-if-ended
                       open-items (car parse)))))

(defun atscript--show-cache-at-point ()
  (interactive)
  (require 'pp)
  (let ((prop (get-text-property (point) 'atscript--pstate)))
    (with-output-to-temp-buffer "*Help*"
      (pp prop))))

(defun atscript--split-name (string)
  "Split a atscript name into its dot-separated parts.
This also removes any prototype parts from the split name
\(unless the name is just \"prototype\" to start with)."
  (let ((name (save-match-data
                (split-string string "\\." t))))
    (unless (and (= (length name) 1)
                 (equal (car name) "prototype"))

      (setq name (remove "prototype" name)))))

(defvar atscript--guess-function-name-start nil)

(defun atscript--guess-function-name (position)
  "Guess the name of the atscript function at POSITION.
POSITION should be just after the end of the word \"function\".
Return the name of the function, or nil if the name could not be
guessed.

This function clobbers match data.  If we find the preamble
begins earlier than expected while guessing the function name,
set `atscript--guess-function-name-start' to that position; otherwise,
set that variable to nil."
  (setq atscript--guess-function-name-start nil)
  (save-excursion
    (goto-char position)
    (forward-line 0)
    (cond
     ((looking-at atscript--function-heading-3-re)
      (and (eq (match-end 0) position)
           (setq atscript--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1)))

     ((looking-at atscript--function-heading-2-re)
      (and (eq (match-end 0) position)
           (setq atscript--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1))))))

(defun atscript--clear-stale-cache ()
  ;; Clear any endings that occur after point
  (let (end-prop)
    (save-excursion
      (while (setq end-prop (atscript--forward-text-property
                             'atscript--pend))
        (setf (atscript--pitem-b-end end-prop) nil))))

  ;; Remove any cache properties after this point
  (remove-text-properties (point) (point-max)
                          '(atscript--pstate t atscript--pend t)))

(defun atscript--ensure-cache (&optional limit)
  "Ensures brace cache is valid up to the character before LIMIT.
LIMIT defaults to point."
  (setq limit (or limit (point)))
  (when (< atscript--cache-end limit)

    (c-save-buffer-state
        (open-items
         orig-match-start
         orig-match-end
         orig-depth
         parse
         prev-parse-point
         name
         case-fold-search
         filtered-class-styles
         new-item
         goal-point
         end-prop)

      ;; Figure out which class styles we need to look for
      (setq filtered-class-styles
            (loop for style in atscript--class-styles
                  if (memq (plist-get style :framework)
                           atscript-enabled-frameworks)
                  collect style))

      (save-excursion
        (save-restriction
          (widen)

          ;; Find last known good position
          (goto-char atscript--cache-end)
          (unless (bobp)
            (setq open-items (get-text-property
                              (1- (point)) 'atscript--pstate))

            (unless open-items
              (goto-char (previous-single-property-change
                          (point) 'atscript--pstate nil (point-min)))

              (unless (bobp)
                (setq open-items (get-text-property (1- (point))
                                                    'atscript--pstate))
                (assert open-items))))

          (unless open-items
            ;; Make a placeholder for the top-level definition
            (setq open-items (list atscript--initial-pitem)))

          (setq parse (syntax-ppss))
          (setq prev-parse-point (point))

          (atscript--clear-stale-cache)

          (narrow-to-region (point-min) limit)

          (loop while (re-search-forward atscript--quick-match-re-func nil t)
                for orig-match-start = (goto-char (match-beginning 0))
                for orig-match-end = (match-end 0)
                do (atscript--ensure-cache--update-parse)
                for orig-depth = (nth 0 parse)

                ;; Each of these conditions should return non-nil if
                ;; we should add a new item and leave point at the end
                ;; of the new item's header (h-end in the
                ;; atscript--pitem diagram). This point is the one
                ;; after the last character we need to unambiguously
                ;; detect this construct. If one of these evaluates to
                ;; nil, the location of the point is ignored.
                if (cond
                    ;; In comment or string
                    ((nth 8 parse) nil)

                    ;; Regular function declaration
                    ((and (looking-at "\\_<function\\_>")
                          (setq name (atscript--forward-function-decl)))

                     (when (eq name t)
                       (setq name (atscript--guess-function-name orig-match-end))
                       (if name
                           (when atscript--guess-function-name-start
                             (setq orig-match-start
                                   atscript--guess-function-name-start))

                         (setq name t)))

                     (assert (eq (char-after) ?{))
                     (forward-char)
                     (make-atscript--pitem
                      :paren-depth orig-depth
                      :h-begin orig-match-start
                      :type 'function
                      :name (if (eq name t)
                                name
                              (atscript--split-name name))))

                    ;; Macro
                    ((looking-at atscript--macro-decl-re)

                     ;; Macros often contain unbalanced parentheses.
                     ;; Make sure that h-end is at the textual end of
                     ;; the macro no matter what the parenthesis say.
                     (c-end-of-macro)
                     (atscript--ensure-cache--update-parse)

                     (make-atscript--pitem
                      :paren-depth (nth 0 parse)
                      :h-begin orig-match-start
                      :type 'macro
                      :name (list (match-string-no-properties 1))))

                    ;; "Prototype function" declaration
                    ((looking-at atscript--plain-method-re)
                     (goto-char (match-beginning 3))
                     (when (save-match-data
                             (atscript--forward-function-decl))
                       (forward-char)
                       (make-atscript--pitem
                        :paren-depth orig-depth
                        :h-begin orig-match-start
                        :type 'function
                        :name (nconc (atscript--split-name
                                      (match-string-no-properties 1))
                                     (list (match-string-no-properties 2))))))

                    ;; Class definition
                    ((loop with syntactic-context =
                           (atscript--syntactic-context-from-pstate open-items)
                           for class-style in filtered-class-styles
                           if (and (memq syntactic-context
                                         (plist-get class-style :contexts))
                                   (looking-at (plist-get class-style
                                                          :class-decl)))
                           do (goto-char (match-end 0))
                           and return
                           (make-atscript--pitem
                            :paren-depth orig-depth
                            :h-begin orig-match-start
                            :type class-style
                            :name (atscript--split-name
                                   (match-string-no-properties 1))))))

                do (atscript--ensure-cache--update-parse)
                and do (push it open-items)
                and do (put-text-property
                        (1- (point)) (point) 'atscript--pstate open-items)
                else do (goto-char orig-match-end))

          (goto-char limit)
          (atscript--ensure-cache--update-parse)
          (setq atscript--cache-end limit)
          (setq atscript--last-parse-pos limit)
          (setq atscript--state-at-last-parse-pos open-items)
          )))))

(defun atscript--end-of-defun-flat ()
  "Helper function for `atscript-end-of-defun'."
  (loop while (atscript--re-search-forward "}" nil t)
        do (atscript--ensure-cache)
        if (get-text-property (1- (point)) 'atscript--pend)
        if (eq 'function (atscript--pitem-type it))
        return t
        finally do (goto-char (point-max))))

(defun atscript--end-of-defun-nested ()
  "Helper function for `atscript-end-of-defun'."
  (message "test")
  (let* (pitem
         (this-end (save-excursion
                     (and (setq pitem (atscript--beginning-of-defun-nested))
                          (atscript--pitem-goto-h-end pitem)
                          (progn (backward-char)
                                 (forward-list)
                                 (point)))))
         found)

    (if (and this-end (< (point) this-end))
        ;; We're already inside a function; just go to its end.
        (goto-char this-end)

      ;; Otherwise, go to the end of the next function...
      (while (and (atscript--re-search-forward "\\_<function\\_>" nil t)
                  (not (setq found (progn
                                     (goto-char (match-beginning 0))
                                     (atscript--forward-function-decl))))))

      (if found (forward-list)
        ;; ... or eob.
        (goto-char (point-max))))))

(defun atscript-end-of-defun (&optional arg)
  "Value of `end-of-defun-function' for `atscript-mode'."
  (setq arg (or arg 1))
  (while (and (not (bobp)) (< arg 0))
    (incf arg)
    (atscript-beginning-of-defun)
    (atscript-beginning-of-defun)
    (unless (bobp)
      (atscript-end-of-defun)))

  (while (> arg 0)
    (decf arg)
    ;; look for function backward. if we're inside it, go to that
    ;; function's end. otherwise, search for the next function's end and
    ;; go there
    (if atscript-flat-functions
        (atscript--end-of-defun-flat)

      ;; if we're doing nested functions, see whether we're in the
      ;; prologue. If we are, go to the end of the function; otherwise,
      ;; call atscript--end-of-defun-nested to do the real work
      (let ((prologue-begin (atscript--function-prologue-beginning)))
        (cond ((and prologue-begin (<= prologue-begin (point)))
               (goto-char prologue-begin)
               (re-search-forward "\\_<function")
               (goto-char (match-beginning 0))
               (atscript--forward-function-decl)
               (forward-list))

              (t (atscript--end-of-defun-nested)))))))

(defun atscript--beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at atscript--opt-cpp-start))
          t
        (goto-char here)
        nil))))

(defun atscript--backward-syntactic-ws (&optional lim)
  "Simple implementation of `c-backward-syntactic-ws' for `atscript-mode'."
  (save-restriction
    (when lim (narrow-to-region lim (point-max)))

    (let ((in-macro (save-excursion (atscript--beginning-of-macro)))
          (pos (point)))

      (while (progn (unless in-macro (atscript--beginning-of-macro))
                    (forward-comment most-negative-fixnum)
                    (/= (point)
                        (prog1
                            pos
                          (setq pos (point)))))))))

(defun atscript--forward-syntactic-ws (&optional lim)
  "Simple implementation of `c-forward-syntactic-ws' for `atscript-mode'."
  (save-restriction
    (when lim (narrow-to-region (point-min) lim))
    (let ((pos (point)))
      (while (progn
               (forward-comment most-positive-fixnum)
               (when (eq (char-after) ?#)
                 (c-end-of-macro))
               (/= (point)
                   (prog1
                       pos
                     (setq pos (point)))))))))

;; Like (up-list -1), but only considers lists that end nearby"
(defun atscript--up-nearby-list ()
  (save-restriction
    ;; Look at a very small region so our compuation time doesn't
    ;; explode in pathological cases.
    (narrow-to-region (max (point-min) (- (point) 500)) (point))
    (up-list -1)))

(defun atscript--inside-param-list-p ()
  "Return non-nil iff point is in a function parameter list."
  (ignore-errors
    (save-excursion
      (atscript--up-nearby-list)
      (and (looking-at "(")
           (progn (forward-symbol -1)
                  (or (looking-at "function")
                      (progn (forward-symbol -1)
                             (looking-at "function"))))))))

(defun atscript--inside-dojo-class-list-p ()
  "Return non-nil iff point is in a Dojo multiple-inheritance class block."
  (ignore-errors
    (save-excursion
      (atscript--up-nearby-list)
      (let ((list-begin (point)))
        (forward-line 0)
        (and (looking-at atscript--dojo-class-decl-re)
             (goto-char (match-end 0))
             (looking-at "\"\\s-*,\\s-*\\[")
             (eq (match-end 0) (1+ list-begin)))))))

(defun atscript--syntax-begin-function ()
  (when (< atscript--cache-end (point))
    (goto-char (max (point-min) atscript--cache-end)))

  (let ((pitem))
    (while (and (setq pitem (car (atscript--backward-pstate)))
                (not (eq 0 (atscript--pitem-paren-depth pitem)))))

    (when pitem
      (goto-char (atscript--pitem-h-begin pitem )))))

;;; Font Lock
(defun atscript--make-framework-matcher (framework &rest regexps)
  "Helper function for building `atscript--font-lock-keywords'.
Create a byte-compiled function for matching a concatenation of
REGEXPS, but only if FRAMEWORK is in `atscript-enabled-frameworks'."
  (setq regexps (apply #'concat regexps))
  (byte-compile
   `(lambda (limit)
      (when (memq (quote ,framework) atscript-enabled-frameworks)
        (re-search-forward ,regexps limit t)))))

(defvar atscript--tmp-location nil)
(make-variable-buffer-local 'atscript--tmp-location)

(defun atscript--forward-destructuring-spec (&optional func)
  "Move forward over a atscript destructuring spec.
If FUNC is supplied, call it with no arguments before every
variable name in the spec.  Return true iff this was actually a
spec.  FUNC must preserve the match data."
  (case (char-after)
    (?\[
     (forward-char)
     (while
         (progn
           (forward-comment most-positive-fixnum)
           (cond ((memq (char-after) '(?\[ ?\{))
                  (atscript--forward-destructuring-spec func))

                 ((eq (char-after) ?,)
                  (forward-char)
                  t)

                 ((looking-at atscript--name-re)
                  (and func (funcall func))
                  (goto-char (match-end 0))
                  t))))
     (when (eq (char-after) ?\])
       (forward-char)
       t))

    (?\{
     (forward-char)
     (forward-comment most-positive-fixnum)
     (while
         (when (looking-at atscript--objfield-re)
           (goto-char (match-end 0))
           (forward-comment most-positive-fixnum)
           (and (cond ((memq (char-after) '(?\[ ?\{))
                       (atscript--forward-destructuring-spec func))
                      ((looking-at atscript--name-re)
                       (and func (funcall func))
                       (goto-char (match-end 0))
                       t))
                (progn (forward-comment most-positive-fixnum)
                       (when (eq (char-after) ?\,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t)))))
     (when (eq (char-after) ?\})
       (forward-char)
       t))))

(defun atscript--variable-decl-matcher (limit)
  "Font-lock matcher for variable names in a variable declaration.
This is a cc-mode-style matcher that *always* fails, from the
point of view of font-lock.  It applies highlighting directly with
`font-lock-apply-highlight'."
  (condition-case nil
      (save-restriction
        (narrow-to-region (point-min) limit)

        (let ((first t))
          (forward-comment most-positive-fixnum)
          (while
              (and (or first
                       (when (eq (char-after) ?,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t))
                   (cond ((looking-at atscript--name-re)
                          (font-lock-apply-highlight
                           '(0 font-lock-variable-name-face))
                          (goto-char (match-end 0)))

                         ((save-excursion
                            (atscript--forward-destructuring-spec))

                          (atscript--forward-destructuring-spec
                           (lambda ()
                             (font-lock-apply-highlight
                              '(0 font-lock-variable-name-face)))))))

            (forward-comment most-positive-fixnum)
            (when (eq (char-after) ?=)
              (forward-char)
              (atscript--forward-expression)
              (forward-comment most-positive-fixnum))

            (setq first nil))))

    ;; Conditions to handle
    (scan-error nil)
    (end-of-buffer nil))

  ;; Matcher always "fails"
  nil)

(defconst atscript--font-lock-keywords-3
  `(
    ;; This goes before keywords-2 so it gets used preferentially
    ;; instead of the keywords in keywords-2. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    ,@cpp-font-lock-keywords ; from font-lock.el

    ,@atscript--font-lock-keywords-2

    ("\\.\\(prototype\\)\\_>"
     (1 font-lock-constant-face))

    ;; Highlights class being declared, in parts
    (atscript--class-decl-matcher
     ,(concat "\\(" atscript--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (match-beginning 1))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class, in parts, if available
    (atscript--class-decl-matcher
     ,(concat "\\(" atscript--name-re "\\)\\(?:\\.\\|.*$\\)")
     (if (match-beginning 2)
         (progn
           (setq atscript--tmp-location (match-end 2))
           (goto-char atscript--tmp-location)
           (insert "=")
           (goto-char (match-beginning 2)))
       (setq atscript--tmp-location nil)
       (goto-char (point-at-eol)))
     (when atscript--tmp-location
       (save-excursion
         (goto-char atscript--tmp-location)
         (delete-char 1)))
     (1 font-lock-type-face))

    ;; Highlights parent class
    (atscript--class-decl-matcher
     (2 font-lock-type-face nil t))

    ;; Dojo needs its own matcher to override the string highlighting
    (,(atscript--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" atscript--dotted-name-re "\\)"
       "\\(?:\"\\s-*,\\s-*\\(" atscript--dotted-name-re "\\)\\)?")
     (1 font-lock-type-face t)
     (2 font-lock-type-face nil t))

    ;; Match Dojo base classes. Of course Mojo has to be different
    ;; from everything else under the sun...
    (,(atscript--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" atscript--dotted-name-re "\\)\"\\s-*,\\s-*\\[")
     ,(concat "[[,]\\s-*\\(" atscript--dotted-name-re "\\)\\s-*"
              "\\(?:\\].*$\\)?")
     (backward-char)
     (end-of-line)
     (1 font-lock-type-face))

    ;; continued Dojo base-class list
    (,(atscript--make-framework-matcher
       'dojo
       "^\\s-*" atscript--dotted-name-re "\\s-*[],]")
     ,(concat "\\(" atscript--dotted-name-re "\\)"
              "\\s-*\\(?:\\].*$\\)?")
     (if (save-excursion (backward-char)
                         (atscript--inside-dojo-class-list-p))
         (forward-symbol -1)
       (end-of-line))
     (end-of-line)
     (1 font-lock-type-face))

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" atscript--basic-type-re)
      (list #'atscript--variable-decl-matcher nil nil nil))

    ;; class instantiation
    ,(list
      (concat "\\_<new\\_>\\s-+\\(" atscript--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; instanceof
    ,(list
      (concat "\\_<instanceof\\_>\\s-+\\(" atscript--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; formal parameters
    ,(list
      (concat
       "\\_<function\\_>\\(\\s-+" atscript--name-re "\\)?\\s-*(\\s-*"
       atscript--name-start-re)
      (list (concat "\\(" atscript--name-re "\\)\\(\\s-*).*\\)?")
            '(backward-char)
            '(end-of-line)
            '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    ,(list
      (concat
       "^\\s-*" atscript--name-re "\\s-*[,)]")
      (list atscript--name-re
            '(if (save-excursion (backward-char)
                                 (atscript--inside-param-list-p))
                 (forward-symbol -1)
               (end-of-line))
            '(end-of-line)
            '(0 font-lock-variable-name-face))))
  "Level three font lock for `atscript-mode'.")

(defun atscript--inside-pitem-p (pitem)
  "Return whether point is inside the given pitem's header or body."
  (atscript--ensure-cache)
  (assert (atscript--pitem-h-begin pitem))
  (assert (atscript--pitem-paren-depth pitem))

  (and (> (point) (atscript--pitem-h-begin pitem))
       (or (null (atscript--pitem-b-end pitem))
           (> (atscript--pitem-b-end pitem) (point)))))

(defun atscript--parse-state-at-point ()
  "Parse the atscript program state at point.
Return a list of `atscript--pitem' instances that apply to point, most
specific first.  In the worst case, the current toplevel instance
will be returned."
  (save-excursion
    (save-restriction
      (widen)
      (atscript--ensure-cache)
      (let* ((bound (if (eobp) (point) (1+ (point))))
             (pstate (or (save-excursion
                           (atscript--backward-pstate))
                         (list atscript--initial-pitem))))

        ;; Loop until we either hit a pitem at BOB or pitem ends after
        ;; point (or at point if we're at eob)
        (loop for pitem = (car pstate)
              until (or (eq (atscript--pitem-type pitem)
                            'toplevel)
                        (atscript--inside-pitem-p pitem))
              do (pop pstate))

        pstate))))

(defun atscript--syntactic-context-from-pstate (pstate)
  "Return the atscript syntactic context corresponding to PSTATE."
  (let ((type (atscript--pitem-type (car pstate))))
    (cond ((memq type '(function macro))
           type)
          ((consp type)
           'class)
          (t 'toplevel))))

(defun atscript-syntactic-context ()
  "Return the atscript syntactic context at point.
When called interatively, also display a message with that
context."
  (interactive)
  (let* ((syntactic-context (atscript--syntactic-context-from-pstate
                             (atscript--parse-state-at-point))))

    (when (called-interactively-p 'interactive)
      (message "Syntactic context: %s" syntactic-context))

    syntactic-context))

(defun atscript--class-decl-matcher (limit)
  "Font lock function used by `atscript-mode'.
This performs fontification according to `atscript--class-styles'."
  (loop initially (atscript--ensure-cache limit)
        while (re-search-forward atscript--quick-match-re limit t)
        for orig-end = (match-end 0)
        do (goto-char (match-beginning 0))
        if (loop for style in atscript--class-styles
                 for decl-re = (plist-get style :class-decl)
                 if (and (memq (plist-get style :framework)
                               atscript-enabled-frameworks)
                         (memq (atscript-syntactic-context)
                               (plist-get style :contexts))
                         decl-re
                         (looking-at decl-re))
                 do (goto-char (match-end 0))
                 and return t)
        return t
        else do (goto-char orig-end)))

(defconst atscript--font-lock-keywords
  '(atscript--font-lock-keywords-3 atscript--font-lock-keywords-1
                                   atscript--font-lock-keywords-2
                                   atscript--font-lock-keywords-3)
  "Font lock keywords for `atscript-mode'.  See `font-lock-keywords'.")

;; XXX: atscript can continue a regexp literal across lines so long
;; as the newline is escaped with \. Account for that in the regexp
;; below.
(defconst atscript--regexp-literal
  "[=(,:]\\(?:\\s-\\|\n\\)*\\(/\\)\\(?:\\\\/\\|[^/*]\\)\\(?:\\\\/\\|[^/]\\)*\\(/\\)"
  "Regexp matching a atscript regular expression literal.
Match groups 1 and 2 are the characters forming the beginning and
end of the literal.")

;; we want to match regular expressions only at the beginning of
;; expressions
(defconst atscript-font-lock-syntactic-keywords
  `((,atscript--regexp-literal (1 "|") (2 "|")))
  "Syntactic font lock keywords matching regexps in atscript.
See `font-lock-keywords'.")

;;; Indentation

(defconst atscript--possibly-braceless-keyword-re
  (atscript--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst atscript--indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (atscript--regexp-opt-symbol '("in" "instanceof")))
  "Regexp matching operators that affect indentation of continued expressions.")


(defun atscript--looking-at-operator-p ()
  "Return non-nil if point is on a atscript operator, other than a comma."
  (save-match-data
    (and (looking-at atscript--indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (atscript--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (looking-at "?")))))))


(defun atscript--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (atscript--looking-at-operator-p)
        (and (atscript--re-search-backward "\n" nil t)
	     (progn
	       (skip-chars-backward " \t")
	       (or (bobp) (backward-char))
	       (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (atscript--looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun atscript--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
	  (atscript--re-search-backward "\\_<do\\_>" (point-at-bol) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (atscript--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (atscript--re-search-forward
			   "\\_<while\\_>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun atscript--ctrl-statement-indentation ()
  "Helper function for `atscript--proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
                 (progn
                   (atscript--re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at atscript--possibly-braceless-keyword-re))
                 (not (atscript--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) atscript-indent-level)))))

(defun atscript--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c atscript-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun atscript--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)
           (atscript--get-c-offset 'c (nth 8 parse-status)))
          ((nth 8 parse-status) 0) ; inside string
          ((atscript--ctrl-statement-indentation))
          ((eq (char-after) ?#) 0)
          ((save-excursion (atscript--beginning-of-macro)) 4)
          ((nth 1 parse-status)
           (let ((same-indent-p (looking-at
                                 "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                 (continued-expr-p (atscript--continued-expression-p)))
             (goto-char (nth 1 parse-status))
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn
                   (skip-syntax-backward " ")
		   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (cond (same-indent-p
                          (current-column))
                         (continued-expr-p
                          (+ (current-column) (* 2 atscript-indent-level)
                             atscript-expr-indent-offset))
                         (t
                          (+ (current-column) atscript-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((atscript--continued-expression-p)
           (+ atscript-indent-level atscript-expr-indent-offset))
          (t 0))))

(defun atscript-indent-line ()
  "Indent the current line as atscript."
  (interactive)
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))
      (indent-line-to (atscript--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

;;; Filling

(defun atscript-c-fill-paragraph (&optional justify)
  "Fill the paragraph with `c-fill-paragraph'."
  (interactive "*P")
  (cl-flet ((c-forward-sws
          (&optional limit)
          (atscript--forward-syntactic-ws limit))
         (c-backward-sws
          (&optional limit)
          (atscript--backward-syntactic-ws limit))
         (c-beginning-of-macro
          (&optional limit)
          (atscript--beginning-of-macro limit)))
    (let ((fill-paragraph-function 'c-fill-paragraph))
      (c-fill-paragraph justify))))

;;; Type database and Imenu

;; We maintain a cache of semantic information, i.e., the classes and
;; functions we've encountered so far. In order to avoid having to
;; re-parse the buffer on every change, we cache the parse state at
;; each interesting point in the buffer. Each parse state is a
;; modified copy of the previous one, or in the case of the first
;; parse state, the empty state.
;;
;; The parse state itself is just a stack of atscript--pitem
;; instances. It starts off containing one element that is never
;; closed, that is initially atscript--initial-pitem.
;;


(defun atscript--pitem-format (pitem)
  (let ((name (atscript--pitem-name pitem))
        (type (atscript--pitem-type pitem)))

    (format "name:%S type:%S"
            name
            (if (atom type)
                type
              (plist-get type :name)))))

(defun atscript--make-merged-item (item child name-parts)
  "Helper function for `atscript--splice-into-items'.
Return a new item that is the result of merging CHILD into
ITEM.  NAME-PARTS is a list of parts of the name of CHILD
that we haven't consumed yet."
  (atscript--debug "atscript--make-merged-item: {%s} into {%s}"
                   (atscript--pitem-format child)
                   (atscript--pitem-format item))

  ;; If the item we're merging into isn't a class, make it into one
  (unless (consp (atscript--pitem-type item))
    (atscript--debug "atscript--make-merged-item: changing dest into class")
    (setq item (make-atscript--pitem
                :children (list item)

                ;; Use the child's class-style if it's available
                :type (if (atom (atscript--pitem-type child))
                          atscript--dummy-class-style
                  (atscript--pitem-type child))

                :name (atscript--pitem-strname item))))

  ;; Now we can merge either a function or a class into a class
  (cons (cond
         ((cdr name-parts)
          (atscript--debug "atscript--make-merged-item: recursing")
          ;; if we have more name-parts to go before we get to the
          ;; bottom of the class hierarchy, call the merger
          ;; recursively
          (atscript--splice-into-items (car item) child
                                       (cdr name-parts)))

         ((atom (atscript--pitem-type child))
          (atscript--debug "atscript--make-merged-item: straight merge")
          ;; Not merging a class, but something else, so just prepend
          ;; it
          (cons child (car item)))

         (t
          ;; Otherwise, merge the new child's items into those
          ;; of the new class
          (atscript--debug "atscript--make-merged-item: merging class contents")
          (append (car child) (car item))))
        (cdr item)))

(defun atscript--pitem-strname (pitem)
  "Last part of the name of PITEM, as a string or symbol."
  (let ((name (atscript--pitem-name pitem)))
    (if (consp name)
        (car (last name))
      name)))

(defun atscript--splice-into-items (items child name-parts)
  "Splice CHILD into the `atscript--pitem' ITEMS at NAME-PARTS.
If a class doesn't exist in the tree, create it.  Return
the new items list.  NAME-PARTS is a list of strings given
the broken-down class name of the item to insert."

  (let ((top-name (car name-parts))
        (item-ptr items)
        new-items last-new-item new-cons item)

    (atscript--debug "atscript--splice-into-items: name-parts: %S items:%S"
             name-parts
             (mapcar #'atscript--pitem-name items))

    (assert (stringp top-name))
    (assert (> (length top-name) 0))

    ;; If top-name isn't found in items, then we build a copy of items
    ;; and throw it away. But that's okay, since most of the time, we
    ;; *will* find an instance.

    (while (and item-ptr
                (cond ((equal (atscript--pitem-strname (car item-ptr)) top-name)
                       ;; Okay, we found an entry with the right name. Splice
                       ;; the merged item into the list...
                       (setq new-cons (cons (atscript--make-merged-item
                                             (car item-ptr) child
                                             name-parts)
                                            (cdr item-ptr)))

                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))

                       ;; ...and terminate the loop
                       nil)

                      (t
                       ;; Otherwise, copy the current cons and move onto the
                       ;; text. This is tricky; we keep track of the tail of
                       ;; the list that begins with new-items in
                       ;; last-new-item.
                       (setq new-cons (cons (car item-ptr) nil))
                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))
                       (setq last-new-item new-cons)

                       ;; Go to the next cell in items
                       (setq item-ptr (cdr item-ptr))))))

    (if item-ptr
        ;; Yay! We stopped because we found something, not because
        ;; we ran out of items to search. Just return the new
        ;; list.
        (progn
          (atscript--debug "search succeeded: %S" name-parts)
          new-items)

      ;; We didn't find anything. If the child is a class and we don't
      ;; have any classes to drill down into, just push that class;
      ;; otherwise, make a fake class and carry on.
      (atscript--debug "search failed: %S" name-parts)
      (cons (if (cdr name-parts)
                ;; We have name-parts left to process. Make a fake
                ;; class for this particular part...
                (make-atscript--pitem
                 ;; ...and recursively digest the rest of the name
                 :children (atscript--splice-into-items
                            nil child (cdr name-parts))
                 :type atscript--dummy-class-style
                 :name top-name)

              ;; Otherwise, this is the only name we have, so stick
              ;; the item on the front of the list
              child)
            items))))

(defun atscript--pitem-add-child (pitem child)
  "Copy `atscript--pitem' PITEM, and push CHILD onto its list of children."
  (assert (integerp (atscript--pitem-h-begin child)))
  (assert (if (consp (atscript--pitem-name child))
              (loop for part in (atscript--pitem-name child)
                    always (stringp part))
            t))

  ;; This trick works because we know (based on our defstructs) that
  ;; the child list is always the first element, and so the second
  ;; element and beyond can be shared when we make our "copy".
  (cons

   (let ((name (atscript--pitem-name child))
         (type (atscript--pitem-type child)))

     (cond ((cdr-safe name) ; true if a list of at least two elements
            ;; Use slow path because we need class lookup
            (atscript--splice-into-items (car pitem) child name))

           ((and (consp type)
                 (plist-get type :prototype))

            ;; Use slow path because we need class merging. We know
            ;; name is a list here because down in
            ;; `atscript--ensure-cache', we made sure to only add
            ;; class entries with lists for :name
            (assert (consp name))
            (atscript--splice-into-items (car pitem) child name))

           (t
            ;; Fast path
            (cons child (car pitem)))))

   (cdr pitem)))

(defun atscript--maybe-make-marker (location)
  "Return a marker for LOCATION if `imenu-use-markers' is non-nil."
  (if imenu-use-markers
      (set-marker (make-marker) location)
    location))

(defun atscript--pitems-to-imenu (pitems unknown-ctr)
  "Convert PITEMS, a list of `atscript--pitem' structures, to imenu format."

  (let (imenu-items pitem pitem-type pitem-name subitems)

    (while (setq pitem (pop pitems))
      (setq pitem-type (atscript--pitem-type pitem))
      (setq pitem-name (atscript--pitem-strname pitem))
      (when (eq pitem-name t)
        (setq pitem-name (format "[unknown %s]"
                                 (incf (car unknown-ctr)))))

      (cond
       ((memq pitem-type '(function macro))
        (assert (integerp (atscript--pitem-h-begin pitem)))
        (push (cons pitem-name
                    (atscript--maybe-make-marker
                     (atscript--pitem-h-begin pitem)))
              imenu-items))

       ((consp pitem-type) ; class definition
        (setq subitems (atscript--pitems-to-imenu
                        (atscript--pitem-children pitem)
                        unknown-ctr))
        (cond (subitems
               (push (cons pitem-name subitems)
                     imenu-items))

              ((atscript--pitem-h-begin pitem)
               (assert (integerp (atscript--pitem-h-begin pitem)))
               (setq subitems (list
                               (cons "[empty]"
                                     (atscript--maybe-make-marker
                                      (atscript--pitem-h-begin pitem)))))
               (push (cons pitem-name subitems)
                     imenu-items))))

       (t (error "Unknown item type: %S" pitem-type))))

    imenu-items))

(defun atscript--imenu-create-index ()
  "Return an imenu index for the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (atscript--ensure-cache)
      (assert (or (= (point-min) (point-max))
                  (eq atscript--last-parse-pos (point))))
      (when atscript--last-parse-pos
        (let ((state atscript--state-at-last-parse-pos)
              (unknown-ctr (cons -1 nil)))

          ;; Make sure everything is closed
          (while (cdr state)
            (setq state
                  (cons (atscript--pitem-add-child (second state) (car state))
                        (cddr state))))

          (assert (= (length state) 1))

          ;; Convert the new-finalized state into what imenu expects
          (atscript--pitems-to-imenu
           (car (atscript--pitem-children state))
           unknown-ctr))))))

;; Silence the compiler.
(defvar which-func-imenu-joiner-function)

(defun atscript--which-func-joiner (parts)
  (mapconcat #'identity parts "."))

(defun atscript--imenu-to-flat (items prefix symbols)
  (loop for item in items
        if (imenu--subalist-p item)
        do (atscript--imenu-to-flat
            (cdr item) (concat prefix (car item) ".")
            symbols)
        else
        do (let* ((name (concat prefix (car item)))
                  (name2 name)
                  (ctr 0))

             (while (gethash name2 symbols)
               (setq name2 (format "%s<%d>" name (incf ctr))))

             (puthash name2 (cdr item) symbols))))

(defun atscript--get-all-known-symbols ()
  "Return a hash table of all atscript symbols.
This searches all existing `atscript-mode' buffers. Each key is the
name of a symbol (possibly disambiguated with <N>, where N > 1),
and each value is a marker giving the location of that symbol."
  (loop with symbols = (make-hash-table :test 'equal)
        with imenu-use-markers = t
        for buffer being the buffers
        for imenu-index = (with-current-buffer buffer
                            (when (eq major-mode 'atscript-mode)
                              (atscript--imenu-create-index)))
        do (atscript--imenu-to-flat imenu-index "" symbols)
        finally return symbols))

(defvar atscript--symbol-history nil
  "History of entered atscript symbols.")

(defun atscript--read-symbol (symbols-table prompt &optional initial-input)
  "Helper function for `atscript-find-symbol'.
Read a symbol from SYMBOLS-TABLE, which is a hash table like the
one from `atscript--get-all-known-symbols', using prompt PROMPT and
initial input INITIAL-INPUT.  Return a cons of (SYMBOL-NAME
. LOCATION), where SYMBOL-NAME is a string and LOCATION is a
marker."
  (unless ido-mode
    (ido-mode t)
    (ido-mode nil))

  (let ((choice (ido-completing-read
                 prompt
                 (loop for key being the hash-keys of symbols-table
                       collect key)
                 nil t initial-input 'atscript--symbol-history)))
    (cons choice (gethash choice symbols-table))))

(defun atscript--guess-symbol-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (save-excursion
        (goto-char (car bounds))
        (when (eq (char-before) ?.)
          (backward-char)
          (setf (car bounds) (point))))
      (buffer-substring (car bounds) (cdr bounds)))))

(defun atscript-find-symbol (&optional arg)
  "Read a atscript symbol and jump to it.
With a prefix argument, restrict symbols to those from the
current buffer.  Pushes a mark onto the tag ring just like
`find-tag'."
  (interactive "P")
  (let (symbols marker)
    (if (not arg)
        (setq symbols (atscript--get-all-known-symbols))
      (setq symbols (make-hash-table :test 'equal))
      (atscript--imenu-to-flat (atscript--imenu-create-index)
                               "" symbols))

    (setq marker (cdr (atscript--read-symbol
                       symbols "Jump to: "
                       (atscript--guess-symbol-at-point))))

    (ring-insert find-tag-marker-ring (point-marker))
    (switch-to-buffer (marker-buffer marker))
    (push-mark)
    (goto-char marker)))

;;; MozRepl integration

(put 'atscript-moz-bad-rpc 'error-conditions '(error timeout))
(put 'atscript-moz-bad-rpc 'error-message "Mozilla RPC Error")

(put 'atscript-atscript-error 'error-conditions '(error atscript-error))
(put 'atscript-atscript-error 'error-message "atscript Error")

(defun atscript--wait-for-matching-output
  (process regexp timeout &optional start)
  "Wait TIMEOUT seconds for PROCESS to output a match for REGEXP.
On timeout, return nil.  On success, return t with match data
set.  If START is non-nil, look for output starting from START.
Otherwise, use the current value of `process-mark'."
  (with-current-buffer (process-buffer process)
    (loop with start-pos = (or start
                               (marker-position (process-mark process)))
          with end-time = (+ (float-time) timeout)
          for time-left = (- end-time (float-time))
          do (goto-char (point-max))
          if (looking-back regexp start-pos) return t
          while (> time-left 0)
          do (accept-process-output process time-left nil t)
          do (goto-char (process-mark process))
          finally do (signal
                      'atscript-moz-bad-rpc
                      (list (format "Timed out waiting for output matching %S" regexp))))))

(defstruct atscript--atscript-handle
  ;; Integer, mirrors the value we see in ATSCRIPT
  (id nil :read-only t)

  ;; Process to which this thing belongs
  (process nil :read-only t))

(defun atscript--atscript-handle-expired-p (x)
  (not (eq (atscript--atscript-handle-process x)
           (inferior-moz-process))))

(defvar atscript--atscript-references nil
  "Maps Elisp atscript proxy objects to their atscript IDs.")

(defvar atscript--atscript-process nil
  "The most recent MozRepl process object.")

(defvar atscript--atscript-gc-idle-timer nil
  "Idle timer for cleaning up ATSCRIPT object references.")

(defvar atscript--atscript-last-gcs-done nil)

(defconst atscript--moz-interactor
  (replace-regexp-in-string
   "[ \n]+" " "
   ; */" Make Emacs happy
"(function(repl) {
  repl.defineInteractor('atscript', {
    onStart: function onStart(repl) {
      if(!repl._atscriptObjects) {
        repl._atscriptObjects = {};
        repl._atscriptLastID = 0;
        repl._atscriptGC = this._atscriptGC;
      }
      this._input = '';
    },

    _atscriptGC: function _atscriptGC(ids_in_use) {
      var objects = this._atscriptObjects;
      var keys = [];
      var num_freed = 0;

      for(var pn in objects) {
        keys.push(Number(pn));
      }

      keys.sort(function(x, y) x - y);
      ids_in_use.sort(function(x, y) x - y);
      var i = 0;
      var j = 0;

      while(i < ids_in_use.length && j < keys.length) {
        var id = ids_in_use[i++];
        while(j < keys.length && keys[j] !== id) {
          var k_id = keys[j++];
          delete objects[k_id];
          ++num_freed;
        }
        ++j;
      }

      while(j < keys.length) {
        var k_id = keys[j++];
        delete objects[k_id];
        ++num_freed;
      }

      return num_freed;
    },

    _mkArray: function _mkArray() {
      var result = [];
      for(var i = 0; i < arguments.length; ++i) {
        result.push(arguments[i]);
      }
      return result;
    },

    _parsePropDescriptor: function _parsePropDescriptor(parts) {
      if(typeof parts === 'string') {
        parts = [ parts ];
      }

      var obj = parts[0];
      var start = 1;

      if(typeof obj === 'string') {
        obj = window;
        start = 0;
      } else if(parts.length < 2) {
        throw new Error('expected at least 2 arguments');
      }

      for(var i = start; i < parts.length - 1; ++i) {
        obj = obj[parts[i]];
      }

      return [obj, parts[parts.length - 1]];
    },

    _getProp: function _getProp(/*...*/) {
      if(arguments.length === 0) {
        throw new Error('no arguments supplied to getprop');
      }

      if(arguments.length === 1 &&
         (typeof arguments[0]) !== 'string')
      {
        return arguments[0];
      }

      var [obj, propname] = this._parsePropDescriptor(arguments);
      return obj[propname];
    },

    _putProp: function _putProp(properties, value) {
      var [obj, propname] = this._parsePropDescriptor(properties);
      obj[propname] = value;
    },

    _delProp: function _delProp(propname) {
      var [obj, propname] = this._parsePropDescriptor(arguments);
      delete obj[propname];
    },

    _typeOf: function _typeOf(thing) {
      return typeof thing;
    },

    _callNew: function(constructor) {
      if(typeof constructor === 'string')
      {
        constructor = window[constructor];
      } else if(constructor.length === 1 &&
                typeof constructor[0] !== 'string')
      {
        constructor = constructor[0];
      } else {
        var [obj,propname] = this._parsePropDescriptor(constructor);
        constructor = obj[propname];
      }

      /* Hacky, but should be robust */
      var s = 'new constructor(';
      for(var i = 1; i < arguments.length; ++i) {
        if(i != 1) {
          s += ',';
        }

        s += 'arguments[' + i + ']';
      }

      s += ')';
      return eval(s);
    },

    _callEval: function(thisobj, atscript) {
      return eval.call(thisobj, atscript);
    },

    getPrompt: function getPrompt(repl) {
      return 'EVAL>'
    },

    _lookupObject: function _lookupObject(repl, id) {
      if(typeof id === 'string') {
        switch(id) {
        case 'global':
          return window;
        case 'nil':
          return null;
        case 't':
          return true;
        case 'false':
          return false;
        case 'undefined':
          return undefined;
        case 'repl':
          return repl;
        case 'interactor':
          return this;
        case 'NaN':
          return NaN;
        case 'Infinity':
          return Infinity;
        case '-Infinity':
          return -Infinity;
        default:
          throw new Error('No object with special id:' + id);
        }
      }

      var ret = repl._atscriptObjects[id];
      if(ret === undefined) {
        throw new Error('No object with id:' + id + '(' + typeof id + ')');
      }
      return ret;
    },

    _findOrAllocateObject: function _findOrAllocateObject(repl, value) {
      if(typeof value !== 'object'  && typeof value !== 'function') {
        throw new Error('_findOrAllocateObject called on non-object('
                        + typeof(value) + '): '
                        + value)
      }

      for(var id in repl._atscriptObjects) {
        id = Number(id);
        var obj = repl._atscriptObjects[id];
        if(obj === value) {
          return id;
        }
      }

      var id = ++repl._atscriptLastID;
      repl._atscriptObjects[id] = value;
      return id;
    },

    _fixupList: function _fixupList(repl, list) {
      for(var i = 0; i < list.length; ++i) {
        if(list[i] instanceof Array) {
          this._fixupList(repl, list[i]);
        } else if(typeof list[i] === 'object') {
          var obj = list[i];
          if(obj.funcall) {
            var parts = obj.funcall;
            this._fixupList(repl, parts);
            var [thisobj, func] = this._parseFunc(parts[0]);
            list[i] = func.apply(thisobj, parts.slice(1));
          } else if(obj.objid) {
            list[i] = this._lookupObject(repl, obj.objid);
          } else {
            throw new Error('Unknown object type: ' + obj.toSource());
          }
        }
      }
    },

    _parseFunc: function(func) {
      var thisobj = null;

      if(typeof func === 'string') {
        func = window[func];
      } else if(func instanceof Array) {
        if(func.length === 1 && typeof func[0] !== 'string') {
          func = func[0];
        } else {
          [thisobj, func] = this._parsePropDescriptor(func);
          func = thisobj[func];
        }
      }

      return [thisobj,func];
    },

    _encodeReturn: function(value, array_as_mv) {
      var ret;

      if(value === null) {
        ret = ['special', 'null'];
      } else if(value === true) {
        ret = ['special', 'true'];
      } else if(value === false) {
        ret = ['special', 'false'];
      } else if(value === undefined) {
        ret = ['special', 'undefined'];
      } else if(typeof value === 'number') {
        if(isNaN(value)) {
          ret = ['special', 'NaN'];
        } else if(value === Infinity) {
          ret = ['special', 'Infinity'];
        } else if(value === -Infinity) {
          ret = ['special', '-Infinity'];
        } else {
          ret = ['atom', value];
        }
      } else if(typeof value === 'string') {
        ret = ['atom', value];
      } else if(array_as_mv && value instanceof Array) {
        ret = ['array', value.map(this._encodeReturn, this)];
      } else {
        ret = ['objid', this._findOrAllocateObject(repl, value)];
      }

      return ret;
    },

    _handleInputLine: function _handleInputLine(repl, line) {
      var ret;
      var array_as_mv = false;

      try {
        if(line[0] === '*') {
          array_as_mv = true;
          line = line.substring(1);
        }
        var parts = eval(line);
        this._fixupList(repl, parts);
        var [thisobj, func] = this._parseFunc(parts[0]);
        ret = this._encodeReturn(
          func.apply(thisobj, parts.slice(1)),
          array_as_mv);
      } catch(x) {
        ret = ['error', x.toString() ];
      }

      var ATSCRIPTON = Components.classes['@mozilla.org/dom/atscripton;1'].createInstance(Components.interfaces.nsIATSCRIPTON);
      repl.print(ATSCRIPTON.encode(ret));
      repl._prompt();
    },

    handleInput: function handleInput(repl, chunk) {
      this._input += chunk;
      var match, line;
      while(match = this._input.match(/.*\\n/)) {
        line = match[0];

        if(line === 'EXIT\\n') {
          repl.popInteractor();
          repl._prompt();
          return;
        }

        this._input = this._input.substring(line.length);
        this._handleInputLine(repl, line);
      }
    }
  });
})
")

  "String to set MozRepl up into a simple-minded evaluation mode.")

(defun atscript--atscript-encode-value (x)
  "Marshall the given value for ATSCRIPT.
Strings and numbers are ATSCRIPTON-encoded.  Lists (including nil) are
made into atscript array literals and their contents encoded
with `atscript--atscript-encode-value'."
  (cond ((stringp x) (atscripton-encode-string x))
        ((numberp x) (atscripton-encode-number x))
        ((symbolp x) (format "{objid:%S}" (symbol-name x)))
        ((atscript--atscript-handle-p x)

         (when (atscript--atscript-handle-expired-p x)
           (error "Stale ATSCRIPT handle"))

         (format "{objid:%s}" (atscript--atscript-handle-id x)))

        ((sequencep x)
         (if (eq (car-safe x) 'atscript--funcall)
             (format "{funcall:[%s]}"
                     (mapconcat #'atscript--atscript-encode-value (cdr x) ","))
           (concat
            "[" (mapconcat #'atscript--atscript-encode-value x ",") "]")))
        (t
         (error "Unrecognized item: %S" x))))

(defconst atscript--atscript-prompt-regexp "\\(repl[0-9]*\\)> $")
(defconst atscript--atscript-repl-prompt-regexp "^EVAL>$")
(defvar atscript--atscript-repl-depth 0)

(defun atscript--atscript-wait-for-eval-prompt ()
  (atscript--wait-for-matching-output
   (inferior-moz-process)
   atscript--atscript-repl-prompt-regexp atscript-atscript-timeout

   ;; start matching against the beginning of the line in
   ;; order to catch a prompt that's only partially arrived
   (save-excursion (forward-line 0) (point))))

(defun atscript--atscript-enter-repl ()
  (inferior-moz-process) ; called for side-effect
  (with-current-buffer inferior-moz-buffer
    (goto-char (point-max))

    ;; Do some initialization the first time we see a process
    (unless (eq (inferior-moz-process) atscript--atscript-process)
      (setq atscript--atscript-process (inferior-moz-process))
      (setq atscript--atscript-references (make-hash-table :test 'eq :weakness t))
      (setq atscript--atscript-repl-depth 0)

      ;; Send interactor definition
      (comint-send-string atscript--atscript-process atscript--moz-interactor)
      (comint-send-string atscript--atscript-process
                          (concat "(" moz-repl-name ")\n"))
      (atscript--wait-for-matching-output
       (inferior-moz-process) atscript--atscript-prompt-regexp
       atscript-atscript-timeout))

    ;; Sanity check
    (when (looking-back atscript--atscript-prompt-regexp
                        (save-excursion (forward-line 0) (point)))
      (setq atscript--atscript-repl-depth 0))

    (if (> atscript--atscript-repl-depth 0)
        ;; If atscript--atscript-repl-depth > 0, we *should* be seeing an
        ;; EVAL> prompt. If we don't, give Mozilla a chance to catch
        ;; up with us.
        (atscript--atscript-wait-for-eval-prompt)

      ;; Otherwise, tell Mozilla to enter the interactor mode
      (insert (match-string-no-properties 1)
              ".pushInteractor('atscript')")
      (comint-send-input nil t)
      (atscript--wait-for-matching-output
       (inferior-moz-process) atscript--atscript-repl-prompt-regexp
       atscript-atscript-timeout))

    (incf atscript--atscript-repl-depth)))

(defun atscript--atscript-leave-repl ()
  (assert (> atscript--atscript-repl-depth 0))
  (when (= 0 (decf atscript--atscript-repl-depth))
    (with-current-buffer inferior-moz-buffer
      (goto-char (point-max))
      (atscript--atscript-wait-for-eval-prompt)
      (insert "EXIT")
      (comint-send-input nil t)
      (atscript--wait-for-matching-output
       (inferior-moz-process) atscript--atscript-prompt-regexp
       atscript-atscript-timeout))))

(defsubst atscript--atscript-not (value)
  (memq value '(nil null false undefined)))

(defsubst atscript--atscript-true (value)
  (not (atscript--atscript-not value)))

(eval-and-compile
  (defun atscript--optimize-arglist (arglist)
    "Convert immediate atscript< and atscript! references to deferred ones."
    (loop for item in arglist
          if (eq (car-safe item) 'atscript<)
          collect (append (list 'list ''atscript--funcall
                                '(list 'interactor "_getProp"))
                          (atscript--optimize-arglist (cdr item)))
          else if (eq (car-safe item) 'atscript>)
          collect (append (list 'list ''atscript--funcall
                                '(list 'interactor "_putProp"))

                          (if (atom (cadr item))
                              (list (cadr item))
                            (list
                             (append
                              (list 'list ''atscript--funcall
                                    '(list 'interactor "_mkArray"))
                              (atscript--optimize-arglist (cadr item)))))
                          (atscript--optimize-arglist (cddr item)))
          else if (eq (car-safe item) 'atscript!)
          collect (destructuring-bind (ignored function &rest body) item
                    (append (list 'list ''atscript--funcall
                                  (if (consp function)
                                      (cons 'list
                                            (atscript--optimize-arglist function))
                                    function))
                            (atscript--optimize-arglist body)))
          else
          collect item)))

(defmacro atscript--atscript-get-service (class-name interface-name)
    `(atscript! ("Components" "classes" ,class-name "getService")
        (atscript< "Components" "interfaces" ,interface-name)))

(defmacro atscript--atscript-create-instance (class-name interface-name)
  `(atscript! ("Components" "classes" ,class-name "createInstance")
        (atscript< "Components" "interfaces" ,interface-name)))

(defmacro atscript--atscript-qi (object interface-name)
  `(atscript! (,object "QueryInterface")
        (atscript< "Components" "interfaces" ,interface-name)))

(defmacro with-atscript (&rest forms)
  "Run FORMS with the Mozilla repl set up for atscript commands.
Inside the lexical scope of `with-atscript', `atscript?', `atscript!',
`atscript-new', `atscript-eval', `atscript-list', `atscript<', `atscript>', `atscript-get-service',
`atscript-create-instance', and `atscript-qi' are defined."

  `(progn
     (atscript--atscript-enter-repl)
     (unwind-protect
         (macrolet ((atscript? (&rest body) `(atscript--atscript-true ,@body))
                    (atscript! (function &rest body)
                         `(atscript--atscript-funcall
                           ,(if (consp function)
                                (cons 'list
                                      (atscript--optimize-arglist function))
                              function)
                           ,@(atscript--optimize-arglist body)))

                    (atscript-new (function &rest body)
                            `(atscript--atscript-new
                              ,(if (consp function)
                                   (cons 'list
                                         (atscript--optimize-arglist function))
                                 function)
                              ,@body))

                    (atscript-eval (thisobj atscript)
                            `(atscript--atscript-eval
                              ,@(atscript--optimize-arglist
                                 (list thisobj atscript))))

                    (atscript-list (&rest args)
                             `(atscript--atscript-list
                               ,@(atscript--optimize-arglist args)))

                    (atscript-get-service (&rest args)
                                    `(atscript--atscript-get-service
                                      ,@(atscript--optimize-arglist args)))

                    (atscript-create-instance (&rest args)
                                        `(atscript--atscript-create-instance
                                          ,@(atscript--optimize-arglist args)))

                    (atscript-qi (&rest args)
                           `(atscript--atscript-qi
                             ,@(atscript--optimize-arglist args)))

                    (atscript< (&rest body) `(atscript--atscript-get
                                        ,@(atscript--optimize-arglist body)))
                    (atscript> (props value)
                         `(atscript--atscript-funcall
                           '(interactor "_putProp")
                           ,(if (consp props)
                                (cons 'list
                                      (atscript--optimize-arglist props))
                              props)
                           ,@(atscript--optimize-arglist (list value))
                           ))
                    (atscript-handle? (arg) `(atscript--atscript-handle-p ,arg)))
           ,@forms)
       (atscript--atscript-leave-repl))))

(defvar atscript--atscript-array-as-list nil
  "Whether to listify any Array returned by a Mozilla function.
If nil, the whole Array is treated as a ATSCRIPT symbol.")

(defun atscript--atscript-decode-retval (result)
  (ecase (intern (first result))
         (atom (second result))
         (special (intern (second result)))
         (array
          (mapcar #'atscript--atscript-decode-retval (second result)))
         (objid
          (or (gethash (second result)
                       atscript--atscript-references)
              (puthash (second result)
                       (make-atscript--atscript-handle
                        :id (second result)
                        :process (inferior-moz-process))
                       atscript--atscript-references)))

         (error (signal 'atscript-atscript-error (list (second result))))))

(defun atscript--atscript-funcall (function &rest arguments)
  "Call the Mozilla function FUNCTION with arguments ARGUMENTS.
If function is a string, look it up as a property on the global
object and use the global object for `this'.
If FUNCTION is a list with one element, use that element as the
function with the global object for `this', except that if that
single element is a string, look it up on the global object.
If FUNCTION is a list with more than one argument, use the list
up to the last value as a property descriptor and the last
argument as a function."

  (with-atscript
   (let ((argstr (atscript--atscript-encode-value
                  (cons function arguments))))

     (with-current-buffer inferior-moz-buffer
       ;; Actual funcall
       (when atscript--atscript-array-as-list
         (insert "*"))
       (insert argstr)
       (comint-send-input nil t)
       (atscript--wait-for-matching-output
        (inferior-moz-process) "EVAL>"
        atscript-atscript-timeout)
       (goto-char comint-last-input-end)

       ;; Read the result
       (let* ((atscripton-array-type 'list)
              (result (prog1 (atscripton-read)
                        (goto-char (point-max)))))
         (atscript--atscript-decode-retval result))))))

(defun atscript--atscript-new (constructor &rest arguments)
  "Call CONSTRUCTOR as a constructor, with arguments ARGUMENTS.
CONSTRUCTOR is a ATSCRIPT handle, a string, or a list of these things."
  (apply #'atscript--atscript-funcall
         '(interactor "_callNew")
         constructor arguments))

(defun atscript--atscript-eval (thisobj atscript)
  (atscript--atscript-funcall '(interactor "_callEval") thisobj atscript))

(defun atscript--atscript-list (&rest arguments)
  "Return a Lisp array resulting from evaluating each of ARGUMENTS."
  (let ((atscript--atscript-array-as-list t))
    (apply #'atscript--atscript-funcall '(interactor "_mkArray")
           arguments)))

(defun atscript--atscript-get (&rest props)
  (apply #'atscript--atscript-funcall '(interactor "_getProp") props))

(defun atscript--atscript-put (props value)
  (atscript--atscript-funcall '(interactor "_putProp") props value))

(defun atscript-gc (&optional force)
  "Tell the repl about any objects we don't reference anymore.
With argument, run even if no intervening GC has happened."
  (interactive)

  (when force
    (setq atscript--atscript-last-gcs-done nil))

  (let ((this-gcs-done gcs-done) keys num)
    (when (and atscript--atscript-references
               (boundp 'inferior-moz-buffer)
               (buffer-live-p inferior-moz-buffer)

               ;; Don't bother running unless we've had an intervening
               ;; garbage collection; without a gc, nothing is deleted
               ;; from the weak hash table, so it's pointless telling
               ;; MozRepl about that references we still hold
               (not (eq atscript--atscript-last-gcs-done this-gcs-done))

               ;; Are we looking at a normal prompt? Make sure not to
               ;; interrupt the user if he's doing something
               (with-current-buffer inferior-moz-buffer
                 (save-excursion
                   (goto-char (point-max))
                   (looking-back atscript--atscript-prompt-regexp
                                 (save-excursion (forward-line 0) (point))))))

      (setq keys (loop for x being the hash-keys
                       of atscript--atscript-references
                       collect x))
      (setq num (atscript--atscript-funcall '(repl "_atscriptGC") (or keys [])))

      (setq atscript--atscript-last-gcs-done this-gcs-done)
      (when (called-interactively-p 'interactive)
        (message "Cleaned %s entries" num))

      num)))

(run-with-idle-timer 30 t #'atscript-gc)

(defun atscript-eval (atscript)
  "Evaluate the atscript in ATSCRIPT and return ATSCRIPTON-decoded result."
  (interactive "Matscript to evaluate: ")
  (with-atscript
   (let* ((content-window (atscript--atscript-content-window
                           (atscript--get-atscript-context)))
          (result (atscript-eval content-window atscript)))
     (when (called-interactively-p 'interactive)
       (message "%s" (atscript! "String" result)))
     result)))

(defun atscript--get-tabs ()
  "Enumerate all atscript contexts available.
Each context is a list:
   (TITLE URL BROWSER TAB TABBROWSER) for content documents
   (TITLE URL WINDOW) for windows

All tabs of a given window are grouped together.  The most recent
window is first.  Within each window, the tabs are returned
left-to-right."
  (with-atscript
   (let (windows)

     (loop with window-mediator = (atscript! ("Components" "classes"
                                        "@mozilla.org/appshell/window-mediator;1"
                                        "getService")
                                       (atscript< "Components" "interfaces"
                                            "nsIWindowMediator"))
           with enumerator = (atscript! (window-mediator "getEnumerator") nil)

           while (atscript? (atscript! (enumerator "hasMoreElements")))
           for window = (atscript! (enumerator "getNext"))
           for window-info = (atscript-list window
                                      (atscript< window "document" "title")
                                      (atscript! (window "location" "toString"))
                                      (atscript< window "closed")
                                      (atscript< window "windowState"))

           unless (or (atscript? (fourth window-info))
                      (eq (fifth window-info) 2))
           do (push window-info windows))

     (loop for window-info in windows
           for window = (first window-info)
           collect (list (second window-info)
                         (third window-info)
                         window)

           for gbrowser = (atscript< window "gBrowser")
           if (atscript-handle? gbrowser)
           nconc (loop
                  for x below (atscript< gbrowser "browsers" "length")
                  collect (atscript-list (atscript< gbrowser
                                        "browsers"
                                        x
                                        "contentDocument"
                                        "title")

                                   (atscript! (gbrowser
                                         "browsers"
                                         x
                                         "contentWindow"
                                         "location"
                                         "toString"))
                                   (atscript< gbrowser
                                        "browsers"
                                        x)

                                   (atscript! (gbrowser
                                         "tabContainer"
                                         "childNodes"
                                         "item")
                                        x)

                                   gbrowser))))))

(defvar atscript-read-tab-history nil)

(defun atscript--read-tab (prompt)
  "Read a Mozilla tab with prompt PROMPT.
Return a cons of (TYPE . OBJECT).  TYPE is either 'window or
'tab, and OBJECT is a atscript handle to a ChromeWindow or a
browser, respectively."

  ;; Prime IDO
  (unless ido-mode
    (ido-mode t)
    (ido-mode nil))

  (with-atscript
   (lexical-let ((tabs (atscript--get-tabs)) selected-tab-cname
                 selected-tab prev-hitab)

     ;; Disambiguate names
     (setq tabs (loop with tab-names = (make-hash-table :test 'equal)
                      for tab in tabs
                      for cname = (format "%s (%s)" (second tab) (first tab))
                      for num = (incf (gethash cname tab-names -1))
                      if (> num 0)
                      do (setq cname (format "%s <%d>" cname num))
                      collect (cons cname tab)))

     (cl-labels ((find-tab-by-cname
               (cname)
               (loop for tab in tabs
                     if (equal (car tab) cname)
                     return (cdr tab)))

              (mogrify-highlighting
               (hitab unhitab)

               ;; Hack to reduce the number of
               ;; round-trips to mozilla
               (let (cmds)
                 (cond
                  ;; Highlighting tab
                  ((fourth hitab)
                   (push '(atscript! ((fourth hitab) "setAttribute")
                               "style"
                               "color: red; font-weight: bold")
                         cmds)

                   ;; Highlight window proper
                   (push '(atscript! ((third hitab)
                                "setAttribute")
                               "style"
                               "border: 8px solid red")
                         cmds)

                   ;; Select tab, when appropriate
                   (when atscript-atscript-switch-tabs
                     (push
                      '(atscript> ((fifth hitab) "selectedTab") (fourth hitab))
                      cmds)))

                  ;; Hilighting whole window
                  ((third hitab)
                   (push '(atscript! ((third hitab) "document"
                                "documentElement" "setAttribute")
                               "style"
                               (concat "-moz-appearance: none;"
                                       "border: 8px solid red;"))
                         cmds)))

                 (cond
                  ;; Unhighlighting tab
                  ((fourth unhitab)
                   (push '(atscript! ((fourth unhitab) "setAttribute") "style" "")
                         cmds)
                   (push '(atscript! ((third unhitab) "setAttribute") "style" "")
                         cmds))

                  ;; Unhighlighting window
                  ((third unhitab)
                   (push '(atscript! ((third unhitab) "document"
                                "documentElement" "setAttribute")
                               "style" "")
                         cmds)))

                 (eval (list 'with-atscript
                             (cons 'atscript-list (nreverse cmds))))))

              (command-hook
               ()
               (let* ((tab (find-tab-by-cname (car ido-matches))))
                 (mogrify-highlighting tab prev-hitab)
                 (setq prev-hitab tab)))

              (setup-hook
               ()
               ;; Fiddle with the match list a bit: if our first match
               ;; is a tabbrowser window, rotate the match list until
               ;; the active tab comes up
               (let ((matched-tab (find-tab-by-cname (car ido-matches))))
                 (when (and matched-tab
                            (null (fourth matched-tab))
                            (equal "navigator:browser"
                                   (atscript! ((third matched-tab)
                                         "document"
                                         "documentElement"
                                         "getAttribute")
                                        "windowtype")))

                   (loop with tab-to-match = (atscript< (third matched-tab)
                                                  "gBrowser"
                                                  "selectedTab")

                         with index = 0
                         for match in ido-matches
                         for candidate-tab = (find-tab-by-cname match)
                         if (eq (fourth candidate-tab) tab-to-match)
                         do (setq ido-cur-list (ido-chop ido-cur-list match))
                         and return t)))

               (add-hook 'post-command-hook #'command-hook t t)))


       (unwind-protect
           (setq selected-tab-cname
                 (let ((ido-minibuffer-setup-hook
                        (cons #'setup-hook ido-minibuffer-setup-hook)))
                   (ido-completing-read
                    prompt
                    (mapcar #'car tabs)
                    nil t nil
                    'atscript-read-tab-history)))

         (when prev-hitab
           (mogrify-highlighting nil prev-hitab)
           (setq prev-hitab nil)))

       (add-to-history 'atscript-read-tab-history selected-tab-cname)

       (setq selected-tab (loop for tab in tabs
                                if (equal (car tab) selected-tab-cname)
                                return (cdr tab)))

       (if (fourth selected-tab)
           (cons 'browser (third selected-tab))
         (cons 'window (third selected-tab)))))))

(defun atscript--guess-eval-defun-info (pstate)
  "Helper function for `atscript-eval-defun'.
Return a list (NAME . CLASSPARTS), where CLASSPARTS is a list of
strings making up the class name and NAME is the name of the
function part."
  (cond ((and (= (length pstate) 3)
              (eq (atscript--pitem-type (first pstate)) 'function)
              (= (length (atscript--pitem-name (first pstate))) 1)
              (consp (atscript--pitem-type (second pstate))))

         (append (atscript--pitem-name (second pstate))
                 (list (first (atscript--pitem-name (first pstate))))))

        ((and (= (length pstate) 2)
              (eq (atscript--pitem-type (first pstate)) 'function))

         (append
          (butlast (atscript--pitem-name (first pstate)))
          (list (car (last (atscript--pitem-name (first pstate)))))))

        (t (error "Function not a toplevel defun or class member"))))

(defvar atscript--atscript-context nil
  "The current atscript context.
This is a cons like the one returned from `atscript--read-tab'.
Change with `atscript-set-atscript-context'.")

(defconst atscript--atscript-inserter
  "(function(func_info,func) {
    func_info.unshift('window');
    var obj = window;
    for(var i = 1; i < func_info.length - 1; ++i) {
      var next = obj[func_info[i]];
      if(typeof next !== 'object' && typeof next !== 'function') {
        next = obj.prototype && obj.prototype[func_info[i]];
        if(typeof next !== 'object' && typeof next !== 'function') {
          alert('Could not find ' + func_info.slice(0, i+1).join('.') +
                ' or ' + func_info.slice(0, i+1).join('.') + '.prototype');
          return;
        }

        func_info.splice(i+1, 0, 'prototype');
        ++i;
      }
    }

    obj[func_info[i]] = func;
    alert('Successfully updated '+func_info.join('.'));
  })")

(defun atscript-set-atscript-context (context)
  "Set the atscript context to CONTEXT.
When called interactively, prompt for CONTEXT."
  (interactive (list (atscript--read-tab "atscript Context: ")))
  (setq atscript--atscript-context context))

(defun atscript--get-atscript-context ()
  "Return a valid atscript context.
If one hasn't been set, or if it's stale, prompt for a new one."
  (with-atscript
   (when (or (null atscript--atscript-context)
             (atscript--atscript-handle-expired-p (cdr atscript--atscript-context))
             (ecase (car atscript--atscript-context)
               (window (atscript? (atscript< (cdr atscript--atscript-context) "closed")))
               (browser (not (atscript? (atscript< (cdr atscript--atscript-context)
                                       "contentDocument"))))))
     (setq atscript--atscript-context (atscript--read-tab "atscript Context: ")))
   atscript--atscript-context))

(defun atscript--atscript-content-window (context)
  (with-atscript
   (ecase (car context)
     (window (cdr context))
     (browser (atscript< (cdr context)
                   "contentWindow" "wrappedATSCRIPTObject")))))

(defun atscript--make-nsilocalfile (path)
  (with-atscript
   (let ((file (atscript-create-instance "@mozilla.org/file/local;1"
                                   "nsILocalFile")))
     (atscript! (file "initWithPath") path)
     file)))

(defun atscript--atscript-add-resource-alias (alias path)
  (with-atscript
   (let* ((io-service (atscript-get-service "@mozilla.org/network/io-service;1"
                                                "nsIIOService"))
          (res-prot (atscript! (io-service "getProtocolHandler") "resource"))
          (res-prot (atscript-qi res-prot "nsIResProtocolHandler"))
          (path-file (atscript--make-nsilocalfile path))
          (path-uri (atscript! (io-service "newFileURI") path-file)))
     (atscript! (res-prot "setSubstitution") alias path-uri))))

(defun* atscript-eval-defun ()
  "Update a Mozilla tab using the atscript defun at point."
  (interactive)

  ;; This function works by generating a temporary file that contains
  ;; the function we'd like to insert. We then use the elisp-atscript bridge
  ;; to command mozilla to load this file by inserting a script tag
  ;; into the document we set. This way, debuggers and such will have
  ;; a way to find the source of the just-inserted function.
  ;;
  ;; We delete the temporary file if there's an error, but otherwise
  ;; we add an unload event listener on the Mozilla side to delete the
  ;; file.

  (save-excursion
    (let (begin end pstate defun-info temp-name defun-body)
      (atscript-end-of-defun)
      (setq end (point))
      (atscript--ensure-cache)
      (atscript-beginning-of-defun)
      (re-search-forward "\\_<function\\_>")
      (setq begin (match-beginning 0))
      (setq pstate (atscript--forward-pstate))

      (when (or (null pstate)
                (> (point) end))
        (error "Could not locate function definition"))

      (setq defun-info (atscript--guess-eval-defun-info pstate))

      (let ((overlay (make-overlay begin end)))
        (overlay-put overlay 'face 'highlight)
        (unwind-protect
            (unless (y-or-n-p (format "Send %s to Mozilla? "
                                      (mapconcat #'identity defun-info ".")))
              (message "") ; question message lingers until next command
              (return-from atscript-eval-defun))
          (delete-overlay overlay)))

      (setq defun-body (buffer-substring-no-properties begin end))

      (make-directory atscript-atscript-tmpdir t)

      ;; (Re)register a Mozilla resource URL to point to the
      ;; temporary directory
      (atscript--atscript-add-resource-alias "atscript" atscript-atscript-tmpdir)

      (setq temp-name (make-temp-file (concat atscript-atscript-tmpdir
                                             "/atscript-")
                                      nil ".atscript"))
      (unwind-protect
          (with-atscript
            (with-temp-buffer
              (insert atscript--atscript-inserter)
              (insert "(")
              (insert (atscripton-encode-list defun-info))
              (insert ",\n")
              (insert defun-body)
              (insert "\n)")
              (write-region (point-min) (point-max) temp-name
                            nil 1))

            ;; Give Mozilla responsibility for deleting this file
            (let* ((content-window (atscript--atscript-content-window
                                    (atscript--get-atscript-context)))
                   (content-document (atscript< content-window "document"))
                   (head (if (atscript? (atscript< content-document "body"))
                             ;; Regular content
                             (atscript< (atscript! (content-document "getElementsByTagName")
                                       "head")
                                  0)
                           ;; Chrome
                           (atscript< content-document "documentElement")))
                   (elem (atscript! (content-document "createElementNS")
                              "http://www.w3.org/1999/xhtml" "script")))

              (atscript! (elem "setAttribute") "type" "text/atscript")
              (atscript! (elem "setAttribute") "src"
                   (format "resource://atscript/%s"
                           (file-name-nondirectory temp-name)))

              (atscript! (head "appendChild") elem)

              (atscript! (content-window "addEventListener") "unload"
                   (atscript! ((atscript-new
                          "Function" "file"
                          "return function() { file.remove(false) }"))
                        (atscript--make-nsilocalfile temp-name))
                   'false)
              (setq temp-name nil)



              ))

        ;; temp-name is set to nil on success
        (when temp-name
          (delete-file temp-name))))))

;;; Main Function

;;;###autoload
(define-derived-mode atscript-mode nil "atscript"
  "Major mode for editing atscript.

Key bindings:

\\{atscript-mode-map}"

  :group 'atscript
  :syntax-table atscript-mode-syntax-table

  (set (make-local-variable 'indent-line-function) 'atscript-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       'atscript-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'atscript-end-of-defun)

  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'font-lock-defaults)
       (list atscript--font-lock-keywords
	     nil nil nil nil
	     '(font-lock-syntactic-keywords
               . atscript-font-lock-syntactic-keywords)))

  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'which-func-imenu-joiner-function)
       #'atscript--which-func-joiner)

  ;; Comments
  (setq comment-start "// ")
  (setq comment-end "")
  (set (make-local-variable 'fill-paragraph-function)
       'atscript-c-fill-paragraph)

  ;; Parse cache
  (add-hook 'before-change-functions #'atscript--flush-caches t t)

  ;; Frameworks
  (atscript--update-quick-match-re)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-create-index-function)
       #'atscript--imenu-create-index)

  (setq major-mode 'atscript-mode)
  (setq mode-name "atscript")

  ;; for filling, pretend we're cc-mode
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "$"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))

  (set (make-local-variable 'syntax-begin-function)
       #'atscript--syntax-begin-function)

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists,
  ;; etc. and and produce maddening "unbalanced parenthesis" errors.
  ;; When we attempt to find the error and scroll to the portion of
  ;; the buffer containing the problem, JIT-lock will apply the
  ;; correct syntax to the regular expresion literal and the problem
  ;; will mysteriously disappear.
  (font-lock-set-defaults)

  (let (font-lock-keywords) ; leaves syntactic keywords intact
    (font-lock-fontify-buffer)))

;;;###autoload
(eval-after-load 'folding
  '(when (fboundp 'folding-add-to-marks-list)
     (folding-add-to-marks-list 'atscript-mode "// {{{" "// }}}" )))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ats$" . atscript-mode))

(provide 'atscript)

;; arch-tag: 1a0d0409-e87f-4fc7-a58c-3731c66ddaac
;;; atscript.el ends here
