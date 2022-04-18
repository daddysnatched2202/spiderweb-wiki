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

(defparameter *jquery-url* "/wiki/jquery.js")
(defvar *jquery-file*)
(defvar *jquery-hash*)

(defmacro html/with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:style (css/std)))
      (:body
       (:script :src *jquery-url*)
       ,@body)
      (:footer (:a :href "/wiki/notes" "Note Index")
	       (:a :href "/wiki/licenses" "Licenses")
	       (:a :href "/wiki/make-note" "New Note")
	       (:a :href "/wiki/search" "Search")))))

(defun note/url (note prefix)
  (ccase prefix
    (:render (format nil "/notes/~a" (path->string (note/path note))))
    (:json (format nil "/json/notes/~a" (path->string (note/path note))))))

(defun node/url (node)
  (format nil "/notes/~a" (node/name node)))

(defclass wiki-parser () ())
(defmethod 3bmd::process-wiki-link ((parser wiki-parser)
				    normalized-target
				    formatted-target
				    args
				    stream)
  (declare (ignore parser
		   normalized-target))
  (let ((link-text (ana:aif (car args)
		       ana:it
		       (format nil "~{~a~^ ~}" (string->path formatted-target)))))
    (format stream
	    "<a href=\"~a\">~a</a>"
	    (am:-> formatted-target
	      (string->path)
	      (note/with-path)
	      (note/url :render))
	    link-text)))

(setf 3bmd-wiki:*wiki-links* t)
(setf 3bmd-wiki:*wiki-processor* (make-instance 'wiki-parser))

(defun preview-note (note stream)
  (let ((spinneret:*html* stream))
    (spinneret:with-html
      (list :div :class "note-preview"
	     (list :a (path->string (note/path note))
	      :href (note/url note :render))
	     (list :p (note/content note))))))

(defun render-note (note stream)
  (let ((spinneret:*html* stream))
    (spinneret:with-html
      `((:h1 ,(path->string (note/path note)))
	(:div :class "path-elements"
	 (dolist (n (note/path note))
	   (:a (node/name n) :href (node/url n))))))))
