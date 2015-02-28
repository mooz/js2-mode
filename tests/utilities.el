;;; tests/utilities.el --- Some tests for js2-mode.

;; Copyright (C) 2009, 2011-2015  Free Software Foundation, Inc.

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
(require 'js2-mode)
(require 'cl-lib)

(defun js2-test-declare-variable (before after point args)
  (let ((before (replace-regexp-in-string "^ *|" "" before))
        (after (replace-regexp-in-string "^ *|" "" after)))
    (with-temp-buffer
      (insert before)
      (js2-mode)
      (goto-char point)
      (apply 'js2-declare-variable args)
      (should (string= after (buffer-substring-no-properties
                              (point-min) (point-max)))))))

(cl-defmacro js2-deftest-declare-variable (name &key before
                                                &key after
                                                &key point
                                                &key args
                                                &key padding)
  `(ert-deftest ,(intern (format "js2-declare-variable-%s" name)) ()
     (let ((js2-basic-offset 4)
           (indent-tabs-mode nil)
           (js2-pretty-multiline-declarations t)
           (inhibit-point-motion-hooks t)
           (js2-declare-variable-padding ,padding))
       (js2-test-declare-variable ,before ,after ,point ,args))))

(js2-deftest-declare-variable empty
  :before ""
  :after  "var a;"
  :point 1
  :args '("a"))

(js2-deftest-declare-variable intialiser
  :before ""
  :after  "var a = 1;"
  :point 1
  :args '("a" "1"))

(js2-deftest-declare-variable append
  :before "var a;"
  :after  "var a,
          |    b;"
  :point 1
  :args '("b"))

(js2-deftest-declare-variable padding
  :before ""
  :after  "
           |var a;
           |"
  :point 1
  :args '("a")
  :padding t)

(js2-deftest-declare-variable nudge
  :before "void 0;"
  :after  "var a;
          |void 0;"
  :point 1
  :args '("a"))

(js2-deftest-declare-variable empty-function
  :before "function f() {
          |
          |}"
  :after  "function f() {
          |    var a;
          |}"
  :point 16 ; Inside the function block
  :args '("a"))

(js2-deftest-declare-variable append-function
  :before "function f() {
          |    var a;
          |}"
  :after  "function f() {
          |    var a,
          |        b;
          |}"
  :point 16
  :args '("b"))

(js2-deftest-declare-variable function-padding
  :before "function f() {
          |
          |}"
  :after  "function f() {
          |
          |    var a;
          |
          |}"
  :point 16
  :args '("a")
  :padding t)

(js2-deftest-declare-variable function-nudge
  :before "function f() {
          |    void 0;
          |}"
  :after  "function f() {
          |    var a;
          |    void 0;
          |}"
  :point 16
  :args '("a"))

(js2-deftest-declare-variable empty-catch
  :before "try {
          |
          |} catch (e) {
          |
          |}"
  :after  "var a;
          |try {
          |
          |} catch (e) {
          |
          |}"
  :point 22 ; Inside the catch block
  :args '("a"))

(js2-deftest-declare-variable use-strict-double
  :before "\"use strict\";"
  :after  "\"use strict\";
          |var a;"
  :point 1
  :args '("a"))

(js2-deftest-declare-variable use-strict-single
  :before "'use strict';"
  :after  "'use strict';
          |var a;"
  :point 1
  :args '("a"))

(js2-deftest-declare-variable use-strict-asi
  :before "'use strict'"
  :after  "'use strict'
          |var a;"
  :point 1
  :args '("a"))

(js2-deftest-declare-variable use-strict-padding
  :before "'use strict';"
  :after  "'use strict';
          |
          |var a;
          |"
  :point 1
  :args '("a")
  :padding t)

(js2-deftest-declare-variable comment-singleline
  :before "// comment
          |" ; TODO: Remove newline
  :after  "// comment
          |var a;"
  :point 1
  :args '("a"))

(js2-deftest-declare-variable comment-multiline
  :before "/* comment */
          |" ; TODO: Remove newline
  :after  "/* comment */
          |var a;"
  :point 1
  :args '("a"))

(js2-deftest-declare-variable comment-padding
  :before "// comment
          " ; TODO: Remove newline
  :after  "// comment
          |
          |var a;
          |"
  :point 1
  :args '("a")
  :padding 1)

(js2-deftest-declare-variable use-strict-then-comment
  :before "'use strict';
          |// comment
          |" ; TODO: Remove newline
  :after  "'use strict';
          |// comment
          |var a;"
  :point 1
  :args '("a"))

(js2-deftest-declare-variable use-strict-then-space-then-comment
  :before "'use strict';
          |
          |// comment
          |" ; TODO: Remove newline
  :after  "'use strict';
          |
          |// comment
          |var a;"
  :point 1
  :args '("a"))

(js2-deftest-declare-variable scope
  :before "if (true) {
          |
          |}"
  :after  "var a;
          |if (true) {
          |
          |}"
  :point 13 ; Inside the if block
  :args '("a"))

(js2-deftest-declare-variable scope
  :before "if (true) {
          |    if (true) {
          |
          |    }
          |}"
  :after  "var a;
          |if (true) {
          |    if (true) {
          |
          |    }
          |}"
  :point 29 ; Inside the nested if block
  :args '("a"))
