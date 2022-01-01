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

(defmacro λ-macro (&body body)
  (alexandria:with-gensyms (args)
    (let ((bindings
	   (loop for i in
		(let ((l))
		  (tree-equal body body
			      :test #'(lambda (x y)
					(declare (ignore y))
					(if (and (symbolp x)
						 (ppcre:scan "_[0-9]+$"
							     (symbol-name x)))
					    (push x l))
					x))
		  l)
		 collect `(,i (nth ,(parse-integer (subseq (symbol-name i) 1))
				   ,args)))))
      `#'(lambda (&rest ,args)
	   (let (,@bindings)
	     ,@body)))))

(defun λ-reader (stream subchar arg)
  (declare (ignore subchar
		   arg))
  `(λ-macro ,(read stream t nil t)))

(set-dispatch-macro-character #\# #\λ #'λ-reader)

(defmacro ignoring-let ((&rest bindings) &body body)
  (let* ((bind-syms (mapcar #'car bindings))
	 (ignores `(declare (ignore ,@bind-syms))))
    `(let (,@bindings)
       ,ignores
       ,@body)))

(defun make-rel-path (str)
  (make-pathname :directory (concatenate 'string *base-path* str)))

(defmacro html/with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:style (css/std)))
      (:body ,@body)
      (:footer (:a :href "/licenses" "License Info")))))

(defmacro css/with-nord-palette (&body body)
  `(ignoring-let ((nord0  "#2E3440")
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
				    :height 1.5em
				    :background-color ,nord0)
			   `(body :background-color ,nord1)
			   `(p :color ,nord5))))

(defmacro ningle/route ((path &rest keys) (&rest param-list) &body body)
  (alexandria:with-gensyms (params maybe-key)
    (labels ((make-binding (sym)
	       (if (listp sym)
		   (destructuring-bind (name &key array
					     (key (ps:symbol-to-js-string name))
					     default)
		       sym
		     (let ((get `(,(if array
				      'get-param-array
				      'get-param)
				   ,key
				   ,params)))
		       `(,name
			 ,(if default
			      `(alexandria:if-let ((,maybe-key ,get))
				 ,maybe-key
				 ,default)
			      get))))
		   `(,sym (get-param ,(ps:symbol-to-js-string sym) ,params)))))
      (let* ((bindings (mapcar #'make-binding param-list)))
	`(setf (ningle:route *app* ,path ,@keys)
	       #'(lambda (,params)
		   (alexandria:if-let ,bindings
		     (progn ,@body)
		     (warn "Could not fill params for route ~a, required params ~a, got params ~a"
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
