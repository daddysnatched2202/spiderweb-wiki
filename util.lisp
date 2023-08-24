;; Copyright 2021-2023 Curtis Klassen
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

(defun make-rel-path (pathname)
  (merge-pathnames
   pathname
   (asdf:system-source-directory "web")))

;;; Returns the first item in ls for which pred returns a true value
;;; If none of the items in ls match, then 'otherwise' will be returned
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
			    (ctypecase err
                              (list (apply #'error err))
                              (function (funcall err)))
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

(defmacro err!=nil ((&rest conditions) &body body)
  (labels ((make-mask (c)
             `(,c () nil)))
    `(handler-case (progn ,@body)
       ,@(if conditions
             (mapcar #'make-mask conditions)
             (list (make-mask 'error))))))
