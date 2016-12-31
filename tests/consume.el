;;; tests/consume.el --- Some tests for js2-mode.

;; Copyright (C) 2016  Free Software Foundation, Inc.

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

(defun js2-mode--and-parse ()
  (js2-mode)
  (js2-reparse))

;;; Comments

(ert-deftest js2-comments-between ()
  (with-temp-buffer
    (insert "0\n//\n[0,/* */1]")
    (js2-mode--and-parse)
    (let ((comments-list (js2-ast-root-comments js2-mode-ast))
           comments)
      (setq comments (js2-comments-between 1 2 comments-list))
      (should (null comments))
      ;; comment head between region
      (setq comments (js2-comments-between 1 3 comments-list))
      (should (= (length comments) 1))
      ;; comment body between region
      (setq comments (js2-comments-between 4 5 comments-list))
      (should (= (length comments) 1))
      ;; comment tail between region
      (setq comments (js2-comments-between 5 6 comments-list))
      (should (= (length comments) 1))
      (setq comments (js2-comments-between 6 6 comments-list))
      (should (null comments))
      (setq comments (js2-comments-between 10 12 comments-list))
      (should (= (length comments) 1))
      ;; multiple comments between
      (setq comments (js2-comments-between 5 15 comments-list))
      (should (= (length comments) 2))
      ;; pass comments-list when no AST available
      (setq js2-mode-ast nil)
      (setq comments (js2-comments-between 8 9 comments))
      (should (= (length comments) 1))
      )))
