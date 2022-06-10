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
       (:script :src ,*jquery-url*)
       ,@body)
      (:footer (:a :href "/wiki/notes" "Note Index")
	       (:a :href "/wiki/licenses" "Licenses")
	       (:a :href "/wiki/make-note" "New Note")
	       (:a :href "/wiki/search" "Search")))))

(defun note/url (note &key (prefix :render))
  (ccase prefix
    (:render (format nil
                     "/wiki/notes/~a"
                     (path->string (note/path note))))
    (:json (format nil
                   "/wiki/json/notes/~a"
                   (path->string (note/path note))))
    (:edit (format nil
                   "/wiki/edit-note/~a"
                   (path->string (note/path note))))))

(defun node/url (node)
  (format nil "/wiki/notes/~a" (node/name node)))

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
                            (format nil
                                    "~{~a~^ ~}"
                                    (string->path formatted-target)))))
    (format stream
	    "<a href=\"~a\">~a</a>"
	    (am:-> formatted-target
	      (string->path)
	      (note/with-path)
	      (note/url))
	    link-text)))

(setf 3bmd-wiki:*wiki-links* t)
(setf 3bmd-wiki:*wiki-processor* (make-instance 'wiki-parser))

(defun note/preview (note &key (max-len 5))
  (let* ((lines (str:lines (note/content note)))
         (shortened-content (str:concat (str:unlines (first-x lines max-len))
                                        (if (> (length lines) max-len)
                                            "…"
                                            ""))))
    (spinneret:with-html-string
      (:div :class "note-preview"
            (:a :href (note/url note) (path->string (note/path note)))
            (:raw (with-output-to-string (s)
                    (3bmd:parse-string-and-print-to-stream shortened-content s)))))))
