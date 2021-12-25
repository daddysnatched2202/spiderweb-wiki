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

(defvar *app* (make-instance 'ningle:app))

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

;;; endpoints
(ningle/route ("/json/notes") ()
  (ningle/respond-type "application/json")
  (jonathan:to-json (mapcar #'class->serial (all-notes))
		    :from :alist))

(ningle/route ("/") ()
  (html/with-page (:title "Welcome")
    (:p "Hello There")))

;;; TODO: system for different licenses / repos using classes
(ningle/route ("/licenses") ()
  (html/with-page (:title "License Info")
    (:a :href "https://github.com/daddysnatched2202/spiderweb-wiki" "Source Code")
    (" for this application is available under the terms of the ")
    (:a :href "https://www.gnu.org/licenses/gpl-3.0.html" "GNU GPL v3.0")
    (:p "Content on this website is licensed under the terms of the Creative Commons
    BY-SA 4.0 license; you can obtain a copy of this license ")
    (:a :href "https://creativecommons.org/licenses/by-sa/4.0/" "here")))
