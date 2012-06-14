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

;;; Other tests.

(js2-deftest-ast parse-array-comp-loop
  "[a for (a in [])];")
