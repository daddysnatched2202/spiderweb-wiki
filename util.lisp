;; Copyright 2021, 2022 Curtis Klassen
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

;;; Returns the first item in ls for which pred returns a true value
;;; If none of the items in ls match, then otherwise will be used as follows:
;;; If otherwise is a function, it will be called with no arguments (the primary use
;;; is to signal an error if there are no matches)
;;; If otherwise is not a function, then it will be returned as-is
;;; Call-otherwise can be set to nil if otherwise is a function / closure that you
;;; want to return, not call
(defun first-matching (ls pred otherwise &optional (call-otherwise t))
  (loop for x in ls
	if (funcall pred x)
	  do (return x)
	finally (return (if (and (functionp otherwise)
				 call-otherwise)
			    (funcall otherwise)
			    otherwise))))

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
;;; Automatically binds symbols in the body of the form (_n : n ∈ ℤ) to the
;;; nth argument of the lambda
;;; Example : (λ-macro (+ _1 _2)) -> (lambda (_0 _1 _2) (+ _1 _2))
;;; Even handles discontinuous and null argument lists !
(defmacro λ-macro (&body body)
  (let* ((bound-in-body (am:->> body
			  (matching-symbols #'anon-arg?)))
	 (ensured (ensure-anon-args bound-in-body))
	 (diff (set-difference ensured bound-in-body)))
    `#'(lambda ,ensured
	 (declare (ignore ,@diff))
	 ,@body)))

(defun λ-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(λ-macro ,(read stream t nil t)))

(set-dispatch-macro-character #\# #\λ #'λ-reader)

(defmacro let-bound ((&rest bindings) &body body)
  (labels ((bound-pred (x)
	     (member x (mapcar #'car bindings)))
	   (used-pred (x bound)
	     (member (car x) bound)))
    (let* ((bound-in-body (matching-symbols #'bound-pred
					    body))
	   (used-bindings (remove-if-not
			   (alexandria:rcurry #'used-pred bound-in-body)
			   bindings)))
      `(let* ,used-bindings ,@body))))

(defun make-rel-path (str)
  (make-pathname :directory (concatenate 'string *base-path* str)))

(defmacro html/with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:style (css/std)))
      (:body
       (:script :src "https://code.jquery.com/jquery-3.6.0.min.js")
       ,@body)
      (:footer (:a :href "/notes" "Note Index")
	       (:a :href "/licenses" "Licenses")))))

(defmacro ningle/route ((path &rest keys) (&rest param-list) &body body)
  (alexandria:with-gensyms (params maybe-key)
    (labels ((make-binding (sym)
	       (if (listp sym)
		   (destructuring-bind (name &key array
					       (key (ps:symbol-to-js-string name))
					       default)
		       sym
		     (let ((getter (list (if array
					     'get-param-array
					     'get-param)
					 key
					 params)))
		       (list name
			     (if default
				 `(alexandria:if-let ((,maybe-key ,getter))
				    ,maybe-key
				    ,default)
				 getter))))
		   (list sym (get-param (ps:symbol-to-js-string sym) params)))))
      (let* ((bindings (mapcar #'make-binding param-list)))
	`(setf (ningle:route *app* ,path ,@keys)
	       #'(lambda (,params)
		   (alexandria:if-let ,bindings
		     (progn ,@body)
		     (warn "Could not fill params for route ~a; required params ~a, 
got params ~a"
			   ,path
			   ',param-list
			   ,params))))))))

(defun ningle/respond-type (type)
  (setf (lack.response:response-headers ningle:*response*)
	(append (lack.response:response-headers ningle:*response*)
		(list :content-type type))))

(defun get-param (param params)
  (cdr (assoc param params :test #'equal)))

(defun get-param-array (param params)
  (am:-<>> params
    (remove param am:<> :test-not #'equal :key #'car)
    (mapcar #'cdr)))
