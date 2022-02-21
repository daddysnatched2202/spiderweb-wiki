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

;;; Returns the first item in ls for which pred returns a true value
;;; If none of the items in ls match, then otherwise will be used as follows:
;;; If otherwise is a function, it will be called with no arguments (the primary use
;;; is to signal a condition if there are no matches)
;;; If otherwise is not a function, then it will be returned as-is
;;; Call-otherwise can be set to nil if otherwise is a function / closure that you
;;; want to return, not call
(defun first-matching (ls pred otherwise &key (call-otherwise t))
  (loop for x in ls
	if (funcall pred x)
	  do (return x)
	finally (return (if (and (functionp otherwise)
				 call-otherwise)
			    (funcall otherwise)
			    otherwise))))

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

(defmacro ningle/route ((path &rest keys) (&rest param-list) &body body)
  (alexandria:with-gensyms (params maybe-key)
    (labels ((make-optional (opt getter)
	       (if opt
		   `(alexandria:if-let ((,maybe-key ,getter))
		      ,maybe-key
		      ,opt)
		   getter))
	     (make-getter (key array ls)
	       (if array
		   `(get-param-array ,key ,ls)
		   `(get-param ,key ,ls)))
	     (make-binding (sym)
	       (if (listp sym)
		   (let* ((plist (cdr sym))
			  (bind (car sym))
			  (key (getf plist :key (ps:symbol-to-js-string bind)))
			  (array (getf plist :array))
			  (opt (getf plist :optional)))
		     `(,bind ,(make-optional opt (make-getter key array params))))
		   `(,sym (get-param ,(ps:symbol-to-js-string sym) ,params)))))
      (let* ((bindings (mapcar #'make-binding param-list)))
	`(setf (ningle:route *app* ,path ,@keys)
	       #'(lambda (,params)
		   (declare (ignorable ,params))
		   (alexandria:if-let ,bindings
		     (progn ,@body)
		     (warn "Could not fill params for route ~a, required params ~a, 
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
