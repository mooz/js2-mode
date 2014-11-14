;;; tests/parser.el --- Some tests for js2-mode.

;; Copyright (C) 2009, 2011-2013  Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'js2-mode)

(defmacro js2-deftest (name buffer-contents &rest body)
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (with-temp-buffer
       (save-excursion
         (insert ,buffer-contents))
       (unwind-protect
           (progn
             ,@body)
         (fundamental-mode)))))

(put 'js2-deftest 'lisp-indent-function 'defun)

(defun js2-test-string-to-ast (s)
  (ert-with-test-buffer (:name 'origin)
    (insert s)
    (js2-mode)
    (should (null js2-mode-buffer-dirty-p))
    js2-mode-ast))

(defun* js2-test-parse-string (code-string &key syntax-error errors-count
                                                reference)
  (let ((ast (js2-test-string-to-ast code-string)))
    (if syntax-error
        (let ((errors (js2-ast-root-errors ast)))
          (should (= (or errors-count 1) (length errors)))
          (destructuring-bind (_ pos len) (first errors)
            (should (string= syntax-error (substring code-string
                                                     (1- pos) (+ pos len -1))))))
      (should (= 0 (length (js2-ast-root-errors ast))))
      (ert-with-test-buffer (:name 'copy)
        (js2-print-tree ast)
        (skip-chars-backward " \t\n")
        (should (string= (or reference code-string)
                         (buffer-substring-no-properties
                          (point-min) (point))))))))

(defmacro* js2-deftest-parse (name code-string &key bind syntax-error errors-count
                                                    reference)
  "Parse CODE-STRING.  If SYNTAX-ERROR is nil, print syntax tree
with `js2-print-tree' and assert the result to be equal to
REFERENCE, if present, or the original string.  If SYNTAX-ERROR
is passed, expect syntax error highlighting substring equal to
SYNTAX-ERROR value.  BIND defines bindings to apply them around
the test."
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (let ,(append bind '((js2-basic-offset 2)))
       (js2-test-parse-string ,code-string
                              :syntax-error ,syntax-error
                              :errors-count ,errors-count
                              :reference ,reference))))

(put 'js2-deftest-parse 'lisp-indent-function 'defun)

;;; Basics

(js2-deftest-parse variable-assignment
  "a = 1;")

(js2-deftest-parse empty-object-literal
  "b = {};")

(js2-deftest-parse empty-array-literal
  "c = [];")

(js2-deftest-parse array-with-missing-elements
  "var a = [1, 2, ,];")

(js2-deftest-parse comma-after-regexp
  "d = /eee/, 42;")

(js2-deftest-parse return-statement
  "function foo() {\n  return 2;\n}")

(js2-deftest-parse function-statement
  "function foo() {\n}")

(js2-deftest-parse function-statement-inside-block
  "if (true) {\n  function foo() {\n  }\n}")

(js2-deftest-parse function-expression-statements-are-verboten
  "function() {}" :syntax-error "(")

(js2-deftest-parse member-expr-as-function-name
  "function a.b.c[2](x, y) {\n}"
  :bind ((js2-allow-member-expr-as-function-name t)))

(js2-deftest-parse named-function-expression
  "a = function b() {};")

(js2-deftest-parse parenthesized-expression
  "(1 + 2);")

(js2-deftest-parse for-with-in-operator-in-parens
  "for (var y = (0 in []) in {}) {\n}")

(js2-deftest-parse for-with-in-operator-in-cond
  "for (var y = 1 ? 0 in [] : false in {}) {\n}")

(js2-deftest-parse let-expression
  "(let (x = 42) x);")

(js2-deftest-parse let-expression-statement
  "let (x = 42) x;")

;;; Callers of `js2-valid-prop-name-token'

(js2-deftest-parse parse-property-access-when-not-keyword
  "A.foo = 3;")

(js2-deftest-parse parse-property-access-when-keyword
  "A.in = 3;"
  :bind ((js2-allow-keywords-as-property-names t)))

(js2-deftest-parse parse-property-access-when-keyword-no-xml
  "A.in = 3;"
  :bind ((js2-allow-keywords-as-property-names t)
         (js2-compiler-xml-available nil)))

(js2-deftest-parse parse-object-literal-when-not-keyword
  "a = {b: 1};")

(js2-deftest-parse parse-object-literal-when-keyword
  "a = {in: 1};"
  :bind ((js2-allow-keywords-as-property-names t)))

;;; 'of' contextual keyword

(js2-deftest-parse parse-legacy-array-comp-loop-with-of
  "[a for (a of [])];")

(js2-deftest-parse parse-array-comp-loop
  "[for (a of []) a];")

(js2-deftest-parse parse-for-of
  "for (var a of []) {\n}")

(js2-deftest-parse of-can-be-var-name
  "var of = 3;")

(js2-deftest-parse of-can-be-function-name
  "function of() {\n}")

;;; Destructuring binding

(js2-deftest-parse destruct-in-declaration
  "var {a, b} = {a: 1, b: 2};")

(js2-deftest-parse destruct-in-arguments
  "function f({a: aa, b: bb}) {\n}")

(js2-deftest-parse destruct-in-array-comp-loop
  "[a + b for ([a, b] in [[0, 1], [1, 2]])];")

(js2-deftest-parse destruct-in-catch-clause
  "try {\n} catch ({a, b}) {\n  a + b;\n}")

;;; Function parameters

(js2-deftest-parse function-with-default-parameters
  "function foo(a = 1, b = a + 1) {\n}")

(js2-deftest-parse function-with-no-default-after-default
  "function foo(a = 1, b) {\n}"
  :syntax-error "b")

(js2-deftest-parse function-with-destruct-after-default
  "function foo(a = 1, {b, c}) {\n}"
  :syntax-error "{")

(js2-deftest-parse function-with-rest-parameter
  "function foo(a, b, ...rest) {\n}")

(js2-deftest-parse function-with-param-after-rest-parameter
  "function foo(a, ...b, rest) {\n}"
  :syntax-error "rest")

(js2-deftest-parse function-with-destruct-after-rest-parameter
  "function foo(a, ...b, {}) {\n}"
  :syntax-error "{}")

(js2-deftest-parse function-with-rest-after-default-parameter
  "function foo(a = 1, ...rest) {\n}")

;;; Spread operator

(js2-deftest-parse spread-in-array-literal
  "[1, ...[2, 3], 4, ...[5, 6]];")

(js2-deftest-parse spread-in-function-call
  "f(3, ...[t(2), t(3)], 42, ...[t(4)]);")

;;; Arrow functions

(js2-deftest-parse arrow-function-with-empty-args-and-no-curlies
  "() => false;" :reference "() => {false};")

(js2-deftest-parse arrow-function-with-args-and-curlies
  "(a, b = 1, ...c) => {  c;\n};")

(js2-deftest-parse parenless-arrow-function-prohibits-rest
  "...b => {b + 1;};" :syntax-error "=>" :errors-count 2)

(js2-deftest-parse parenless-arrow-function-prohibits-destructuring
  "[a, b] => {a + b;};" :syntax-error "=>" :errors-count 5)

;;; Automatic semicolon insertion

(js2-deftest-parse no-auto-semi-insertion-after-if
  "if (true) {\n}")

(js2-deftest-parse auto-semi-insertion-after-function
  "a = function() {}" :reference "a = function() {};")

(js2-deftest-parse auto-semi-one-variable-per-line
  "x\ny" :reference "x;\ny;")

;;; Labels

(js2-deftest-parse labeled-stmt-node
  "foo:\nbar:\nx = y + 1;")

(js2-deftest no-label-node-inside-expr "x = y:"
  (let (js2-parse-interruptable-p)
    (js2-mode))
  (let ((assignment (js2-expr-stmt-node-expr (car (js2-scope-kids js2-mode-ast)))))
    (should (js2-name-node-p (js2-assign-node-right assignment)))))

(js2-deftest-parse label-and-loops "for (; ; ) {
  loop:
  for (; ; ) {
    continue loop;
  }
}")

;;; Generators

(js2-deftest-parse legacy-generator "function foo() {\n  yield 1;\n}")

(js2-deftest-parse legacy-generator-cannot-return
  "function foo() {\n  yield 1;\n return 2;\n}" :syntax-error "return 2")

(js2-deftest-parse harmony-generator "function* bar() {\n  yield 2;\n  return 3;\n}")

(js2-deftest-parse harmony-generator-yield-star "(function*(a) {  yield* a;\n});")

;;; Comprehensions

(js2-deftest-parse parse-legacy-array-comp-loop-with-filter
  "[a for (a in b) if (a == 2)];")

(js2-deftest-parse parse-array-comp-loop-with-filters
  "[for (a in b) if (a == 2) if (b != 10) a];")

(js2-deftest-parse parse-generator-comp-loop-with-filters
  "(for (x of y) if (x != 4) x);")

(js2-deftest-parse parse-array-comp-with-yield-is-ok
  "(function() {  return [for (x of []) yield x];\n});")

(js2-deftest-parse parse-generator-comp-with-yield-is-not-ok
  "(function() {  return (for (x of []) yield x);\n});"
  :syntax-error "yield")

(js2-deftest-parse parse-generator-comp-with-yield-inside-function-is-ok
  "(for (x of []) function*() {  yield x;\n});")

;;; Numbers

(js2-deftest-parse decimal-starting-with-zero "081;" :reference "81;")

(js2-deftest-parse huge-hex "0x0123456789abcdefABCDEF;" :reference "-1;")

(js2-deftest-parse octal-without-o "071;" :reference "57;")

(js2-deftest-parse hex-number-okay "0x123;" :reference "291;")

(js2-deftest-parse hex-number-broken "0xz23;"
  :syntax-error "0xz" :errors-count 2)

(js2-deftest-parse binary-number-okay "0b101;" :reference "5;")

(js2-deftest-parse binary-number-broken "0b210;"
  :syntax-error "0b2" :errors-count 2)

(js2-deftest-parse octal-number-okay "0o765;" :reference "501;")

(js2-deftest-parse octal-number-broken "0o812;"
  :syntax-error "0o8" :errors-count 2)

;;; Modules

(js2-deftest parse-import-named-exports "{ one, two as dos } from 'foo/bar';"
  (js2-init-scanner)
  (let ((node (make-js2-import-node :pos 1)))
    (js2-parse-import-named-exports node)
    (should (= 2 (length (js2-import-node-bindings node))))))

(js2-deftest parse-import-module-id "from 'foo/bar';"
  (js2-init-scanner)
  (let ((node (make-js2-import-node :pos 1)))
    (js2-parse-import-module-id node)
    (should (equal "foo/bar" (js2-import-node-module-id node)))
    (should (equal 14 (js2-import-node-len node)))))

(js2-deftest parse-import-module-id "from 'foo/bar';"
  (js2-init-scanner)
  (let ((import-node (make-js2-import-node :pos 1)))
    (js2-parse-import-module-id import-node)
    (should (string= "foo/bar" (js2-import-node-module-id import-node)))
    (should (= 14 (js2-node-len import-node)))))

(js2-deftest match-import-binding "one;"
  (js2-init-scanner)
  (let ((binding (js2-match-import-binding)))
    (should (not (equal nil binding)))
    (should (equal "one" (js2-import-binding-node-export-name binding)))
    (should (equal "one" (js2-import-binding-node-name binding))))
  (should (eq js2-SEMI (js2-next-token))))

(js2-deftest match-renamed-export "one as uno;"
  (js2-init-scanner)
  (let ((binding (js2-match-import-binding)))
    (should (not (equal nil binding)))
    (should (equal "one" (js2-import-binding-node-export-name binding)))
    (should (equal "uno" (js2-import-binding-node-name binding))))
  (should (eq js2-SEMI (js2-next-token))))

(js2-deftest match-import-binding-before-module-id "beer from 'tap'"
  (js2-init-scanner)
  (let ((binding (js2-match-import-binding)))
    (should (not (equal nil binding)))
    (should (equal "beer" (js2-import-binding-node-export-name binding)))
    (should (equal "beer" (js2-import-binding-node-name binding)))
    (should (js2-match-token js2-NAME))
    (should (equal "from" (js2-current-token-string)))))

(js2-deftest match-renamed-import-of-all-exports "* as lib"
  (js2-init-scanner)
  (let ((binding (js2-match-import-binding)))
    (should (not (equal nil binding)))
    (should (equal "lib" (js2-import-binding-node-name binding)))
    (should (equal "*" (js2-import-binding-node-export-name binding)))))

(js2-deftest parse-non-matching-renamed-import "one, two"
  (js2-init-scanner)
  (let ((binding (js2-match-import-binding)))
    (should (not (equal nil binding)))
    (should (equal "one" (js2-import-binding-node-name binding))))
  (should (eq js2-COMMA (js2-next-token))))

(js2-deftest default-import "import theDefault from 'src/lib';"
  (js2-init-scanner)
  (should (eq js2-IMPORT (js2-next-token)))
  (let* ((node (js2-parse-import))
         (module-id (js2-import-node-module-id node)))
    (should (not (eq nil node)))
    (should (equal "src/lib" module-id))))

(js2-deftest complex-import "import * as lib, {one as uno, two as dos} from 'src/lib';"
  (js2-init-scanner)
  (should (eq js2-IMPORT (js2-next-token)))
  (let ((node (js2-parse-import)))
    (should (= 56 (js2-import-node-len node)))
    (should (equal "src/lib" (js2-import-node-module-id node)))
    (let ((default (js2-import-node-default node)))
      (should (not (equal nil default)))
      (should (equal "lib" (js2-import-binding-node-name default))))
    (let ((bindings (js2-import-node-bindings node)))
      (should (= 2 (length bindings)))
      (let ((first (nth 0 bindings))
            (second (nth 1 bindings)))
        (should (equal "uno" (js2-import-binding-node-name first)))
        (should (equal "dos" (js2-import-binding-node-name second)))))))
(js2-deftest import-that-renames-default "import * as lib from 'src/mylib'"
  (js2-init-scanner)
  (should (eq js2-IMPORT (js2-next-token)))
  (let ((import (js2-parse-import)))
    (should (not (equal nil import)))
    (pp import)
    (should (equal "lib" (js2-import-binding-node-name (js2-import-node-default import))))
    (should (equal "src/mylib" (js2-import-node-module-id import)))))


(js2-deftest-parse import-default-only "import theDefault from 'src/lib';"
  :reference "import theDefault from 'src/lib'")
(js2-deftest-parse import-named-only "import {one, two} from 'src/lib';")
(js2-deftest-parse import-default-and-named
  "import theDefault, {one, two} from 'src/lib';"
  :reference "import theDefault, {one, two} from 'src/lib'")
(js2-deftest-parse import-renaming-default "import * as lib from 'src/mylib';"
  :reference "import * as lib from 'src/mylib'")
(js2-deftest-parse import-renaming-named "import {one as uno, two as dos} from 'src/lib';")

;; TODO
;; (js2-deftest-parse import-module-metadata "import {url} from this module;")

(js2-deftest export-tokens "export default"
  (js2-init-scanner)
  (should (eq js2-EXPORT (js2-next-token)))
  (should (eq js2-DEFAULT (js2-next-token))))

(js2-deftest export-default "export default new Object();"
  (js2-init-scanner)
  (should (eq js2-EXPORT (js2-next-token)))
  (let* ((node (js2-parse-export))
         (expr (js2-export-node-expr node)))
    (should (js2-export-node-default-p node))
    (should (eq node (js2-node-parent expr)))))


(js2-deftest-parse named-export-function-expression "export function hereIsANamedFunction() {};")
(js2-deftest-parse export-variable "export foo;")

;; TODO
;; (js2-deftest-parse export-const "export const PI = Math.PI;")
;; (js2-deftest-parse export-let "export let foo = 'bar'")

;; (js2-deftest-parse re-export-all  "export * from 'src/other_module';")
;; (js2-deftest-parse rexport-some "export {foo, bar as bang} from 'src/other_module';")


;;; Scopes

(js2-deftest ast-symbol-table-includes-fn-node "function foo() {}"
  (js2-mode)
  (let ((entry (js2-scope-get-symbol js2-mode-ast 'foo)))
    (should (= (js2-symbol-decl-type entry) js2-FUNCTION))
    (should (equal (js2-symbol-name entry) "foo"))
    (should (js2-function-node-p (js2-symbol-ast-node entry)))))

(js2-deftest fn-symbol-table-includes-nested-fn "function foo() {
  function bar() {}
  var x;
}"
  (js2-mode)
  (let* ((scope (js2-node-at-point (point-min)))
         (fn-entry (js2-scope-get-symbol scope 'bar))
         (var-entry (js2-scope-get-symbol scope 'x)))
    (should (string= (js2-name-node-name (js2-function-node-name scope)) "foo"))
    (should (= (js2-symbol-decl-type fn-entry) js2-FUNCTION))
    (should (js2-function-node-p (js2-symbol-ast-node fn-entry)))
    (should (= (js2-symbol-decl-type var-entry) js2-VAR))
    (should (js2-name-node-p (js2-symbol-ast-node var-entry)))))

;;; Tokenizer

(js2-deftest get-token "(1+1)"
  (js2-init-scanner)
  (should (eq js2-LP (js2-next-token)))
  (should (eq js2-NUMBER (js2-next-token)))
  (should (eq js2-ADD (js2-next-token)))
  (should (eq js2-NUMBER (js2-next-token)))
  (should (eq js2-RP (js2-next-token))))

(js2-deftest unget-token "()"
  (js2-init-scanner)
  (should (eq js2-LP (js2-next-token)))
  (js2-unget-token)
  (should (eq js2-LP (js2-next-token)))
  (should (eq js2-RP (js2-next-token))))

(js2-deftest get-token-or-eol "x\n++;"
  (js2-init-scanner)
  (should (eq js2-NAME (js2-next-token)))
  (should (eq js2-EOL (js2-peek-token-or-eol)))
  (should (eq js2-INC (js2-next-token)))
  (should (eq js2-SEMI (js2-peek-token-or-eol))))

(js2-deftest unget-token-over-eol-and-comment "x\n//abc\ny"
  (js2-init-scanner)
  (should (eq js2-NAME (js2-next-token)))
  (should (eq js2-NAME (js2-next-token)))
  (should (equal "y" (js2-current-token-string)))
  (js2-unget-token)
  (should (eq js2-NAME (js2-current-token-type)))
  (should (equal "x" (js2-current-token-string))))

(js2-deftest ts-seek "(1+2)"
  (js2-init-scanner)
  (should (eq js2-LP (js2-next-token)))
  (should (eq js2-NUMBER (js2-next-token)))
  (js2-unget-token)
  (let ((state (make-js2-ts-state)))
    (should (eq js2-NUMBER (js2-next-token)))
    (should (eq js2-ADD (js2-next-token)))
    (js2-ts-seek state)
    (should (eq 1 js2-ti-lookahead))
    (should (eq js2-NUMBER (js2-next-token)))
    (should (eq 1 (js2-token-number
                   (js2-current-token))))))

;;; Error handling

(js2-deftest for-node-with-error-len "for "
  (js2-mode)
  (let ((node (js2-node-at-point (point-min))))
    (should (= (js2-node-len (js2-node-parent node)) 4))))

(js2-deftest function-without-parens-error "function b {}"
  ;; Should finish the parse.
  (js2-mode))
