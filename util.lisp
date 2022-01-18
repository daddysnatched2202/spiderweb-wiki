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
;;; returned
(defun matching-symbols (test-fun tree)
  (let ((l))
    (tree-equal tree tree
		:test #'(lambda (x y)
			  (declare (ignore y))
			  (if (and x
				   (symbolp x)
				   (funcall test-fun x))
			      (push x l))
			  x))
    l))

;;; Returns the first item in ls for which pred returns a true value
;;; If none of the items in ls match, then otherwise will be used as follows:
;;; If otherwise is a function, it will be called with no arguments (the primary use
;;; is to signal an error if there are no matches)
;;; If otherwise is not a function, then it will be returned as-is
;;; The call-otherwise arg can be used if otherwise is a function / closure that you
;;; want to return, not call
(defun first-matching (ls pred otherwise &optional (call-otherwise t))
  (loop for x in ls
	if (funcall pred x)
	  do (return x)
	finally (if (and (functionp otherwise)
			 call-otherwise)
		    (return (funcall otherwise))
		    (return otherwise))))

(defun anon-arg-number (sym)
  (a-m:-> (symbol-name sym)
    (parse-integer :start 1)))

(defun anon-arg? (sym)
  (and (symbolp sym)
       (ppcre:scan "^_[0-9]*$" (symbol-name sym))))

;;; The list returned by ensure-anon-args will always be in order
(defun ensure-anon-args (bindings)
  (a-m:-<>> bindings
    (mapcar #'anon-arg-number)
    (reduce #'max)
    (loop for x from 0 upto a-m:<>
	  collect (intern (format nil "_~a" x)))))

(defun anon-args-sort (bindings)
  (sort (copy-list bindings)
	#'<
	:key #'anon-arg-number))

;;; TODO: handle advanced args (&rest, &key, &optional)
(defmacro λ-macro (&body body)
  (let* ((bound-in-body (a-m:->> body
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

;;; TODO: have a system where different themes can be slotted in and out
(defmacro css/with-nord-palette (&body body)
  `(let ((nord0  "#2E3440")
	 (nord1  "#3B4252")
	 (nord2  "#434C5E")
	 (nord3  "#4C566A")

	 (nord4  "#D8DEE9")
	 (nord5  "#E5E9F0")
	 (nord6  "#ECEFF4")

	 (nord7  "#8FBCBB")
	 (nord8  "#88C0D0")
	 (nord9  "#81A1C1")
	 (nord10 "#5E81AC")

	 (nord11 "#BF616A")
	 (nord12 "#D08770")
	 (nord13 "#EBCB8B")
	 (nord14 "#A3BE8C")
	 (nord15 "#B48EAD"))
     ,@body))

(defun css/std ()
  (css/with-nord-palette
   (lass:compile-and-write `(footer :position absolute
				    :bottom 0px
				    :left 0px
				    :height 3em
				    :width 100%
				    :background-color ,nord1
				    (a :bottom -15px
				       :margin 5px
				       :position relative))
			   `(body :background-color ,nord0
				  :color ,nord5)
			   `(p :color ,nord5)
			   `(a :background-color ,nord2
			       :color ,nord5
			       :text-decoration none
			       :padding 4px)
			   `("a:hover" :background-color ,nord3))))

(defmacro ningle/route ((path &rest keys) (&rest param-list) &body body)
  (alexandria:with-gensyms (params maybe-key)
    (labels ((make-binding (sym)
	       (if (listp sym)
		   (destructuring-bind (name &key array
					       (key (ps:symbol-to-js-string name))
					       default)
		       sym
		     (let ((getter `(,(if array
					  'get-param-array
					  'get-param)
				     ,key
				     ,params)))
		       `(,name
			 ,(if default
			      `(alexandria:if-let ((,maybe-key ,getter))
				 ,maybe-key
				 ,default)
			      getter))))
		   `(,sym (get-param ,(ps:symbol-to-js-string sym) ,params)))))
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
  (loop for i in (remove param params :test-not #'equal :key #'car)
     collect (cdr i)))
