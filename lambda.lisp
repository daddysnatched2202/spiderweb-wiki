;; Copyright 2021-2024 Curtis Klassen
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

;; Modified from alexandria (lists.lisp:356)
(defun lambda/flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
               (unless (and subtree
                            (consp subtree)
                            (eq (car subtree)
                                'λ-macro))
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

;;; matching-symbols makes no guarantees about the order in which symbols are
;;; returned, but any given symbol will only be returned once, regardless of how many
;;; times it appears in the tree
(defun matching-symbols (test-fun tree)
  (am:->> tree
    (lambda/flatten)
    (remove-if-not (alexandria:conjoin #'symbolp test-fun))
    (remove-duplicates)))

(defun anon-arg-number (sym)
  (am:-> sym
    (symbol-name)
    (parse-integer :start 1)))

(defun anon-arg? (sym)
  (and (symbolp sym)
       (ppcre:scan "^_[0-9]*$" (symbol-name sym))))

;;; TODO: handle advanced args (&rest, &key, &optional)
;;; Macro to speed up creation of lambdas
;;; Automatically binds symbols in the body of the form (_n : n ∈ ℕ)
;;; to the nth argument of the lambda
;;; Example : (λ-macro () (+ _1 _2)) -> (lambda (_0 _1 _2) (+ _1 _2))
;;; Even handles discontinuous and null argument lists !
(defmacro λ-macro ((&rest args) &body body)
  (destructuring-bind (&key name let) args
    (labels ((ensure-anon-args (bindings)
	       (if (> (length bindings) 0)
		   (am:->> bindings
		     (mapcar #'anon-arg-number)
		     (reduce #'max)
                     (1+)
                     (alexandria:iota)
                     (mapcar (lambda (x) (intern (format nil "_~a" x))))))))
      (am:-<>> body
        (matching-symbols #'anon-arg?)
        (ensure-anon-args)
        (let* ((lambda-args am:<>)
               (lambda-form `(lambda ,lambda-args
                               (declare (ignorable ,@lambda-args))
                               ,@(if let
                                     `((let* ,let
                                         ,@body))
                                     body))))
          (if name
              `(labels ((,name ,@(cdr lambda-form)))
                 #',name)
              lambda-form))))))

;;; If the first element of the list read by λ-reader is a list whose car
;;; is :arg-list, then its cdr will be passed as the args for λ-macro
;;; Example : #λ((:arg-list a b c) (+ _0 _2)) -> (λ-macro (a b c) (+ _0 _2))
(defun λ-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((form (read stream t nil t))
         (arg-list (if (and (listp form)
                            (listp (car form))
                            (eq (caar form) :arg-list))
                       (cdar form)
                       nil))
         (statements (if arg-list
                         (cdr form)
                         (list form))))
    `(λ-macro ,arg-list ,@statements)))

(set-dispatch-macro-character #\# #\λ #'λ-reader)
