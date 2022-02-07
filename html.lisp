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

(defclass wiki-parser () ())
(defmethod 3bmd::process-wiki-link ((parser wiki-parser)
				    normalized-target
				    formatted-target
				    args
				    stream)
  (declare (ignore parser
		   normalized-target
		   formatted-target
		   args
		   stream)))

(setf 3bmd-wiki:*wiki-links* t)
(setf 3bmd-wiki:*wiki-processor* (make-instance 'wiki-parser))

(defun preview-note (note stream)
  (let ((spinneret:*html* stream))
    (spinneret:with-html
      `(:div.note-preview
	(:a ,(note/path note)
	 :href ,(format nil
			"/notes/render/~a"
			(note/path note)))
	(:p ,(note/content note))))))
