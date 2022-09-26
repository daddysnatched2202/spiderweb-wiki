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
       ,(if (eq *jquery-source* :cdn)
            (list :script :src *jquery-path*)
	    (list :script :src *jquery-url*))
       ,@body)
      (:footer (:a :href "/wiki/notes" "Note Index")
	       (:a :href "/wiki/licenses" "Licenses")
	       (:a :href "/wiki/make-note" "New Note")
	       (:a :href "/wiki/search" "Search")))))

(defun note->url (note &key (prefix :render))
  (format nil
          (ccase prefix
            (:render "/wiki/notes/~a")
            (:json "/wiki/json/~a")
            (:edit "/wiki/edit-note/~a")
            (:delete "/wiki/delete-note/~a"))
          (am:-> note
            (convert-path)
            (path->string))))

(defun node->url (node)
  (format nil "/wiki/node/~a" (node/name node)))

(defun path->url (path)
  (let ((conv (convert-path path)))
    (cond ((note/exists? conv)
           (note->url (note/with-path conv)))
          ((= (length conv)
              1)
           (node->url (car conv)))
          ((> (length conv)
              1)
           (format nil "/wiki/make-note/~a" (path->string path)))
          (t (warn "Tried to get URL for bad path of value `~a`" path)
             "/wiki/"))))

(defclass wiki-parser () ())
(defmethod 3bmd-wiki:process-wiki-link ((parser wiki-parser)
				        normalized-target
				        formatted-target
				        args
				        stream)
  (declare (ignore parser
		   normalized-target))
  (let ((link-text (ana:aif (car args)
                            ana:it
                            (am:-> formatted-target
                              (string->path)
                              (path->string)))))
    (labels ((generate-link (target label)
               (format stream
                       "<a href=\"~a\">~a</a>"
                       target
                       label)))
      (cond ((ana:aand (cadr args)
                       (string= ana:it "url"))
             (generate-link formatted-target
                            link-text))
            ((or (ana:aand (cadr args)
                           (string= ana:it "note"))
                 (null (cadr args)))
             (generate-link (path->url formatted-target)
                            link-text))
            (t (generate-link "/wiki"
                              (format nil "BAD LINK `~a`" formatted-target)))))))

(setf 3bmd-wiki:*wiki-links* t)
(setf 3bmd-wiki:*wiki-processor* (make-instance 'wiki-parser))

(defun html/preview-note (note &key (max-len 5) (class "note-preview"))
  (let* ((lines (str:lines (note/content note)))
         (unlines (str:unlines lines))
         (shortened-content (if (> (length lines) max-len)
                                (str:concat unlines "…")
                                unlines)))
    (spinneret:with-html-string
      (:div :class class
            (:a :href (note->url note) (path->string (note/path note)))
            (:raw (with-output-to-string (s)
                    (3bmd:parse-string-and-print-to-stream shortened-content s)))))))

(defun html/gen-note-previews (notes &key (max-len 5) (class "note-preview"))
  (spinneret:with-html-string
    (dolist (n notes)
      (:raw (html/preview-note n :max-len max-len :class class)))))
