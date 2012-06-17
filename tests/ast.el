(require 'ert)
(require 'ert-x)
(require 'js2-mode)

(defun js2-test-string-to-ast (s)
  (ert-with-test-buffer (:name 'origin)
      (insert s)
      (js2-mode)
      (should (null js2-mode-buffer-dirty-p))
      js2-mode-ast))

(defun js2-test-ast-string (code-string)
  (let ((ast (js2-test-string-to-ast code-string)))
    (ert-with-test-buffer (:name 'copy)
      (js2-print-tree ast)
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (should (string= code-string
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))))

(defmacro js2-deftest-ast (name code-string &optional bindings)
  "Parse CODE-STRING, print it out with `js2-print-tree', and
assert the result to be equal to the original string.
When BINDINGS are specified, apply them around the test."
  `(ert-deftest ,name ()
     (let ,bindings
       (js2-test-ast-string ,code-string))))

(put 'js2-deftest-ast 'lisp-indent-function 'defun)

;;; Callers of `js2-valid-prop-name-token'.

(js2-deftest-ast parse-property-access-when-not-keyword
  "A.foo = 3;")

(js2-deftest-ast parse-property-access-when-keyword
  "A.in = 3;"
  ((js2-allow-keywords-as-property-names t)))

(js2-deftest-ast parse-property-access-when-keyword-no-xml
  "A.in = 3;"
  ((js2-allow-keywords-as-property-names t)
   (js2-compiler-xml-available nil)))

(js2-deftest-ast parse-array-literal-when-not-keyword
  "a = {b:1};")

(js2-deftest-ast parse-array-literal-when-keyword
  "a = {in:1};"
  ((js2-allow-keywords-as-property-names t)))

;;; 'of' contextual keyword.

(js2-deftest-ast parse-array-comp-loop-with-of
  "[a for (a of [])];")

(js2-deftest-ast parse-for-of
  "for (var a of []) {\n}")

(js2-deftest-ast of-can-be-var-name
  "var of = 3;")

(js2-deftest-ast of-can-be-function-name
  "function of() {\n}")

;;; Destructuring binding.

(js2-deftest-ast destruct-in-declaration
  "var {a, b} = {a:1, b:2};")

(js2-deftest-ast destruct-in-arguments
  "function f({a:aa, b:bb}) {\n}")

(js2-deftest-ast destruct-in-array-comp-loop
  "[a + b for ([a, b] in [[0, 1], [1, 2]])];")

;; (js2-deftest-ast destruct-in-catch-clause
;;   "try {\n} catch ({a, b}) {return a + b;}")
