;;; js2-old-indent.el --- Indentation code kept for compatibility

;; Copyright (C) 2015  Free Software Foundation, Inc.

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

;; All features of this indentation code have been ported to Emacs's
;; built-in `js-mode' by now, so we derive from it.  An older
;; commentary follows.

;; This code is kept for Emacs 24.5 and ealier.

;; This indenter is based on Karl Landström's "javascript.el" indenter.
;; Karl cleverly deduces that the desired indentation level is often a
;; function of paren/bracket/brace nesting depth, which can be determined
;; quickly via the built-in `parse-partial-sexp' function.  His indenter
;; then does some equally clever checks to see if we're in the context of a
;; substatement of a possibly braceless statement keyword such as if, while,
;; or finally.  This approach yields pretty good results.

;; The indenter is often "wrong", however, and needs to be overridden.
;; The right long-term solution is probably to emulate (or integrate
;; with) cc-engine, but it's a nontrivial amount of coding.  Even when a
;; parse tree from `js2-parse' is present, which is not true at the
;; moment the user is typing, computing indentation is still thousands
;; of lines of code to handle every possible syntactic edge case.

;; In the meantime, the compromise solution is that we offer a "bounce
;; indenter", configured with `js2-bounce-indent-p', which cycles the
;; current line indent among various likely guess points.  This approach
;; is far from perfect, but should at least make it slightly easier to
;; move the line towards its desired indentation when manually
;; overriding Karl's heuristic nesting guesser.

;; I've made miscellaneous tweaks to Karl's code to handle some Ecma
;; extensions such as `let' and Array comprehensions.  Major kudos to
;; Karl for coming up with the initial approach, which packs a lot of
;; punch for so little code. -- Steve

;;; Code:

(defvar js2-language-version)

(declare-function js2-mark-safe-local "js2-mode")
(declare-function js2-backward-sws "js2-mode")
(declare-function js2-forward-sws "js2-mode")
(declare-function js2-same-line "js2-mode")

(defcustom js2-basic-offset (if (and (boundp 'c-basic-offset)
                                     (numberp c-basic-offset))
                                c-basic-offset
                              4)
  "Number of spaces to indent nested statements.
Similar to `c-basic-offset'."
  :group 'js2-mode
  :safe 'integerp
  :type 'integer)

(defcustom js2-pretty-multiline-declarations t
  "Non-nil to line up multiline declarations vertically:

  var a = 10,
      b = 20,
      c = 30;

If the value is t, and the first assigned value in the
declaration is a function/array/object literal spanning several
lines, it won't be indented additionally:

  var o = {                   var bar = 2,
    foo: 3          vs.           o = {
  },                                foo: 3
      bar = 2;                    };

If the value is `all', it will always be indented additionally:

  var o = {
        foo: 3
      };

  var o = {
        foo: 3
      },
      bar = 2;

If the value is `dynamic', it will be indented additionally only
if the declaration contains more than one variable:

  var o = {
    foo: 3
  };

  var o = {
        foo: 3
      },
      bar = 2;"
  :group 'js2-mode
  :safe  'symbolp
  :type 'symbol)

(defcustom js2-indent-switch-body nil
  "When nil, case labels are indented on the same level as the
containing switch statement.  Otherwise, all lines inside
switch statement body are indented one additional level."
  :type 'boolean
  :safe 'booleanp
  :group 'js2-mode)

(defconst js2-possibly-braceless-keywords-re
  (concat "else[ \t]+if\\|for[ \t]+each\\|"
          (regexp-opt '("catch" "do" "else" "finally" "for" "if"
                        "try" "while" "with" "let")))
  "Regular expression matching keywords that are optionally
followed by an opening brace.")

(defconst js2-indent-operator-re
  (concat "[-+*/%<>&^|?:.]\\([^-+*/]\\|$\\)\\|!?=\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions.")

(defconst js2-declaration-keyword-re
  (regexp-opt '("var" "let" "const") 'words)
  "Regular expression matching variable declaration keywords.")

(defun js2-re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `js2-re-search-forward'."
  (let (parse saved-point)
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (if saved-point
                      (parse-partial-sexp saved-point (point))
                    (syntax-ppss (point))))
      (cond ((nth 3 parse)
             (re-search-forward
              (concat "\\(\\=\\|[^\\]\\|^\\)" (string (nth 3 parse)))
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            (t
             (setq count (1- count))))
      (setq saved-point (point))))
  (point))

(defun js2-re-search-forward (regexp &optional bound noerror count)
  "Search forward but ignore strings and comments.
Invokes `re-search-forward' but treats the buffer as if strings
and comments have been removed."
  (let ((saved-point (point)))
    (condition-case err
        (cond ((null count)
               (js2-re-search-forward-inner regexp bound 1))
              ((< count 0)
               (js2-re-search-backward-inner regexp bound (- count)))
              ((> count 0)
               (js2-re-search-forward-inner regexp bound count)))
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun js2-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js2-re-search-backward'."
  (let (parse)
    (while (> count 0)
      (re-search-backward regexp bound)
      (setq parse (syntax-ppss (point)))
      (cond ((nth 3 parse)
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse)))
              (line-beginning-position) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            (t
             (setq count (1- count))))))
  (point))

(defun js2-re-search-backward (regexp &optional bound noerror count)
  "Search backward but ignore strings and comments.
Invokes `re-search-backward' but treats the buffer as if strings
and comments have been removed."
  (let ((saved-point (point)))
    (condition-case err
        (cond ((null count)
               (js2-re-search-backward-inner regexp bound 1))
              ((< count 0)
               (js2-re-search-forward-inner regexp bound (- count)))
              ((> count 0)
               (js2-re-search-backward-inner regexp bound count)))
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun js2-looking-at-operator-p ()
  "Return non-nil if text after point is a non-comma operator."
  (and (looking-at js2-indent-operator-re)
       (or (not (looking-at ":"))
           (save-excursion
             (and (js2-re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                  (looking-at "?"))))))

(defun js2-continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js2-looking-at-operator-p)
        (when (catch 'found
                (while (and (re-search-backward "\n" nil t)
                            (let ((state (syntax-ppss)))
                              (when (nth 4 state)
                                (goto-char (nth 8 state))) ;; skip comments
                              (skip-chars-backward " \t")
                              (if (bolp)
                                  t
                                (throw 'found t))))))
          (backward-char)
          (when (js2-looking-at-operator-p)
            (backward-char)
            (not (looking-at "\\*\\|\\+\\+\\|--\\|/[/*]")))))))

(defun js2-end-of-do-while-loop-p ()
  "Return non-nil if word after point is `while' of a do-while
statement, else returns nil. A braceless do-while statement
spanning several lines requires that the start of the loop is
indented to the same column as the current line."
  (interactive)
  (save-excursion
    (when (looking-at "\\s-*\\_<while\\_>")
      (if (save-excursion
            (skip-chars-backward "[ \t\n]*}")
            (looking-at "[ \t\n]*}"))
          (save-excursion
            (backward-list) (backward-word 1) (looking-at "\\_<do\\_>"))
        (js2-re-search-backward "\\_<do\\_>" (point-at-bol) t)
        (or (looking-at "\\_<do\\_>")
            (let ((saved-indent (current-indentation)))
              (while (and (js2-re-search-backward "^[ \t]*\\_<" nil t)
                          (/= (current-indentation) saved-indent)))
              (and (looking-at "[ \t]*\\_<do\\_>")
                   (not (js2-re-search-forward
                         "\\_<while\\_>" (point-at-eol) t))
                   (= (current-indentation) saved-indent))))))))

(defun js2-multiline-decl-indentation ()
  "Return the declaration indentation column if the current line belongs
to a multiline declaration statement.  See `js2-pretty-multiline-declarations'."
  (let (forward-sexp-function ; use Lisp version
        at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at js2-declaration-keyword-re))
        (when (looking-at js2-indent-operator-re)
          (goto-char (match-end 0))) ; continued expressions are ok
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (js2-backward-sws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2 (skip-syntax-backward ".")
                                     (looking-at js2-indent-operator-re)
                                   (js2-backward-sws))
                                 (not (eq (char-before) ?\;)))
                            (js2-same-line pos)))))
          (condition-case _
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at js2-declaration-keyword-re)
          (goto-char (match-end 0))
          (1+ (current-column)))))))

(defun js2-ctrl-statement-indentation ()
  "Return the proper indentation of current line if it is a control statement.
Returns an indentation if this line starts the body of a control
statement without braces, else returns nil."
  (let (forward-sexp-function)
    (save-excursion
      (back-to-indentation)
      (when (and (not (js2-same-line (point-min)))
                 (not (looking-at "{"))
                 (js2-re-search-backward "[[:graph:]]" nil t)
                 (not (looking-at "[{([]"))
                 (progn
                   (forward-char)
                   (when (= (char-before) ?\))
                     ;; scan-sexps sometimes throws an error
                     (ignore-errors (backward-sexp))
                     (skip-chars-backward " \t" (point-at-bol)))
                   (let ((pt (point)))
                     (back-to-indentation)
                     (when (looking-at "}[ \t]*")
                       (goto-char (match-end 0)))
                     (and (looking-at js2-possibly-braceless-keywords-re)
                          (= (match-end 0) pt)
                          (not (js2-end-of-do-while-loop-p))))))
        (+ (current-indentation) js2-basic-offset)))))

(defun js2-indent-in-array-comp (parse-status)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((bracket (nth 1 parse-status))
        (end (point)))
    (when bracket
      (save-excursion
        (goto-char bracket)
        (when (looking-at "\\[")
          (forward-char 1)
          (js2-forward-sws)
          (if (looking-at "[[{]")
              (let (forward-sexp-function) ; use Lisp version
                (forward-sexp)             ; skip destructuring form
                (js2-forward-sws)
                (if (and (/= (char-after) ?,) ; regular array
                         (looking-at "for"))
                    (match-beginning 0)))
            ;; to skip arbitrary expressions we need the parser,
            ;; so we'll just guess at it.
            (if (and (> end (point)) ; not empty literal
                     (re-search-forward "[^,]]* \\(for\\) " end t)
                     ;; not inside comment or string literal
                     (let ((state (parse-partial-sexp bracket (point))))
                       (not (or (nth 3 state) (nth 4 state)))))
                (match-beginning 1))))))))

(defun js2-array-comp-indentation (parse-status for-kwd)
  (if (js2-same-line for-kwd)
      ;; first continuation line
      (save-excursion
        (goto-char (nth 1 parse-status))
        (forward-char 1)
        (skip-chars-forward " \t")
        (current-column))
    (save-excursion
      (goto-char for-kwd)
      (current-column))))

(defun js2-maybe-goto-declaration-keyword-end (bracket)
  "Helper function for `js2-proper-indentation'.
Depending on the value of `js2-pretty-multiline-declarations',
move point to the end of a variable declaration keyword so that
indentation is aligned to that column."
  (cond
   ((eq js2-pretty-multiline-declarations 'all)
    (when (looking-at js2-declaration-keyword-re)
      (goto-char (1+ (match-end 0)))))
   ((eq js2-pretty-multiline-declarations 'dynamic)
    (let (declaration-keyword-end
          at-closing-bracket-p
          comma-p)
      (when (looking-at js2-declaration-keyword-re)
        ;; Preserve the match data lest it somehow be overridden.
        (setq declaration-keyword-end (match-end 0))
        (save-excursion
          (goto-char bracket)
          (setq at-closing-bracket-p
                ;; Handle scan errors gracefully.
                (condition-case nil
                    (progn
                      ;; Use the regular `forward-sexp-function' because the
                      ;; normal one for this mode uses the AST.
                      (let (forward-sexp-function)
                        (forward-sexp))
                      t)
                  (error nil)))
          (when at-closing-bracket-p
            (js2-forward-sws)
            (setq comma-p (looking-at-p ","))))
        (when comma-p
          (goto-char (1+ declaration-keyword-end))))))))

(cl-defun js2-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (when (nth 4 parse-status)
      (cl-return-from js2-proper-indentation (js2--comment-indent parse-status)))
    (let* ((at-closing-bracket (looking-at "[]})]"))
           (same-indent-p (or at-closing-bracket
                              (looking-at "\\_<case\\_>[^:]")
                              (and (looking-at "\\_<default:")
                                   (save-excursion
                                     (js2-backward-sws)
                                     (not (memq (char-before) '(?, ?{)))))))
           (continued-expr-p (js2-continued-expression-p))
           (declaration-indent (and js2-pretty-multiline-declarations
                                    (js2-multiline-decl-indentation)))
           (bracket (nth 1 parse-status))
           beg indent)
      (cond
       ;; indent array comprehension continuation lines specially
       ((and bracket
             (>= js2-language-version 170)
             (not (js2-same-line bracket))
             (setq beg (js2-indent-in-array-comp parse-status))
             (>= (point) (save-excursion
                           (goto-char beg)
                           (point-at-bol)))) ; at or after first loop?
        (js2-array-comp-indentation parse-status beg))

       ((js2-ctrl-statement-indentation))

       ((and declaration-indent continued-expr-p)
        (+ declaration-indent js2-basic-offset))

       (declaration-indent)

       (bracket
        (goto-char bracket)
        (cond
         ((looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
          (when (save-excursion (skip-chars-backward " \t)")
                                (looking-at ")"))
            (backward-list))
          (back-to-indentation)
          (js2-maybe-goto-declaration-keyword-end bracket)
          (setq indent
                (cond (same-indent-p
                       (current-column))
                      (continued-expr-p
                       (+ (current-column) (* 2 js2-basic-offset)))
                      (t
                       (+ (current-column) js2-basic-offset))))
          (if (and js2-indent-switch-body
                   (not at-closing-bracket)
                   (looking-at "\\_<switch\\_>"))
              (+ indent js2-basic-offset)
            indent))
         (t
          (unless same-indent-p
            (forward-char)
            (skip-chars-forward " \t"))
          (current-column))))

       (continued-expr-p js2-basic-offset)

       (t 0)))))

(defun js2--comment-indent (parse-status)
  "Indentation inside a multi-line block comment continuation line."
  (save-excursion
    (goto-char (nth 8 parse-status))
    (if (looking-at "/\\*")
        (+ 1 (current-column))
      0)))

(defun js2-old-indent-line (&optional bounce-backwards)
  "Indent the current line as JavaScript source text."
  (interactive)
  (let (parse-status offset
        ;; Don't whine about errors/warnings when we're indenting.
        ;; This has to be set before calling parse-partial-sexp below.
        (inhibit-point-motion-hooks t))
    (setq parse-status (save-excursion
                         (syntax-ppss (point-at-bol)))
          offset (- (point) (save-excursion
                              (back-to-indentation)
                              (point))))
    ;; Don't touch multiline strings.
    (unless (nth 3 parse-status)
      (indent-line-to (js2-proper-indentation parse-status))
      (when (cl-plusp offset)
        (forward-char offset)))))

(provide 'js2-old-indent)

;;; js2-old-indent.el ends here
