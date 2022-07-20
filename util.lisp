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

;;; Returns the first item in ls for which pred returns a true value If none of the
;;; items in ls match, then 'otherwise' will be returned
;;; If 'err' is not nil, then it will be used if there is no match; it should
;;; be either
;;; 1. A list which is the arguments that will be passed to 'error'
;;; 2. A function that will be called with no arguments
(defun first-matching (ls pred &key (otherwise nil) (err nil))
  (loop for x in ls
	if (funcall pred x)
	  do (return x)
	finally (return (if (and err
				 (not otherwise))
			    (typecase err
				  (list (apply #'error err))
				  (function (funcall err))
				  (t (error "The value of `err` in call to ~
                                             `first-matching` must be either a ~
                                             list or a function; ~%it in fact ~
                                             has the value `~a`, ~
                                             which is of type `~a`"
					    err
					    (type-of err))))
			    otherwise))))

(defun first-x (ls x)
  (loop for y from 0 below x
        for l in ls
        collect l))

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
  (concatenate 'string *base-path* str))

(defmacro ningle/route ((path &rest keys) (&rest param-list) &body body)
  (alexandria:with-gensyms (maybe-key)
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
		     `(,bind ,(make-optional opt (make-getter key array 'params))))
		   `(,sym (get-param ,(ps:symbol-to-js-string sym) params)))))
      (let* ((bindings (mapcar #'make-binding param-list))
	     (page `#'(lambda (params)
			(declare (ignorable params))
			(alexandria:if-let ,bindings
			  (progn ,@body)
			  (warn "Could not fill params for route `~a`; required ~
                                 params `~a`, got params `~a`~%"
				,path
				',param-list
				params)))))
	`(progn (setf (ningle:route *app* ,path ,@keys)
		      ,page)
		(setf (ningle:route *app*
				    ,(if (string= (str:s-last path) "/")
					 (str:substring 0 -1 path)
					 (str:concat path "/"))
				    ,@keys)
		      ,page))))))

(defun ningle/push-response (response)
  (alexandria:appendf (lack.response:response-headers ningle:*response*)
                      response))

(defun ningle/add-response-header (key val)
  (ningle/push-response (list key val)))

(defun ningle/respond-type (type)
  (ningle/add-response-header :content-type type))

(defun ningle/set-response-status (code)
  (setf (lack.response:response-status ningle:*response*)
	code)
  (princ-to-string code))

;;; can be used in an end point for caching static files on the client
;;; file-type should be a string which will be passed to ningle/respond-type, or nil
;;; if you're setting it manually
(defun ningle/cache-file (file hash &key (file-type nil))
  (am:-<> ningle:*request*
    (lack.request:request-headers)
    (gethash "if-none-match" am:<>)
    (if (string= hash am:<>)
        (ningle/set-response-status 304)
        (progn (if file-type
                   (ningle/respond-type file-type))
               (ningle/add-response-header "Cache-Control" "must-revalidate")
               (ningle/add-response-header "ETag" (princ-to-string hash))
               (ningle/set-response-status 200)
               file))))

;;; can be used to redirect the user to a different page
(defun ningle/redirect (url &key (type :tmp))
  (ningle/add-response-header "Location" url)
  (ningle/set-response-status (case type
                                (:permanent 301)
                                (:tmp 307))))

(defun get-param (param params)
  (cdr (assoc param params :test #'equal)))

(defun get-param-array (param params)
  (am:-<>> params
    (remove param
	    am:<>
	    :test-not #'equal
	    :key #'car)
    (mapcar #'cdr)))
