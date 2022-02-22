;; Copyright 2022 Curtis Klassen
;; This file is part of Spiderweb Wiki.

;; Spiderweb Wiki is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Spiderweb Wiki is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Spiderweb Wiki.  If not, see <https://www.gnu.org/licenses/>.

(in-package :web)

;;; matching-symbols makes no guarantees about the order in which symbols are
;;; returned, but any given symbol will only be returned once, regardless of how many
;;; times it appears in the tree
(defun matching-symbols (test-fun tree)
  (am:->> tree
    (alexandria:flatten)
    (remove-if-not test-fun)
    (remove-duplicates)))

(defun anon-arg-number (sym)
  (am:-> sym
    (symbol-name)
    (parse-integer :start 1)))

(defun anon-arg? (sym)
  (and (symbolp sym)
       (ppcre:scan "^_[0-9]*$" (symbol-name sym))))

;;; The list returned by ensure-anon-args will always be in order
(defun ensure-anon-args (bindings)
  (if (> (length bindings) 0)
      (am:-<>> bindings
	(mapcar #'anon-arg-number)
	(reduce #'max)
	(loop for x from 0 upto am:<>
	      collect (intern (format nil "_~a" x))))
      nil))

;;; TODO: handle advanced args (&rest, &key, &optional)
;;; Macro to speed up creation of lambdas
;;; Automatically binds symbols in the body of the form (_n : n ∈ ℤ) to the nth
;;; argument of the lambda
;;; Example : (λ-macro (+ _1 _2)) -> (lambda (_0 _1 _2) (+ _1 _2))
;;; Even handles discontinuous and null argument lists !
(defmacro λ-macro (&body body)
  (let* ((bound-in-body (am:->> body
			  (matching-symbols #'anon-arg?)))
	 (ensured (ensure-anon-args bound-in-body)))
    `#'(lambda ,ensured
	 (declare (ignorable ,@ensured))
	 ,@body)))

;;; If λ-reader reads a list of lists, all statements get passed to λ-macro
(defun λ-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((form (read stream t nil t)))
    (if (and (listp form)
	     (eq :progn (car form)))
	`(λ-macro ,@(cdr form))
	`(λ-macro ,form))))

(set-dispatch-macro-character #\# #\λ #'λ-reader)
