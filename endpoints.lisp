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

;;; endpoints
(ningle/route ("/json/notes") ()
  (ningle/respond-type "application/json")
  (jonathan:to-json (mapcar #'obj->serial *notes*)
		    :from :alist))

(ningle/route ("/") ()
  (html/with-page (:title "Welcome")
    (:p "Henlo luser")))

;;; TODO: system for different licenses / repos using classes
(ningle/route ("/licenses") ()
  (html/with-page (:title "License Info")
    (:a :href "https://github.com/daddysnatched2202/spiderweb-wiki" "Source code")
    (" available under the terms of the ")
    (:a :href "https://www.gnu.org/licenses/gpl-3.0.html" "GNU GPL version 3")
    (:p "Content on this website is licensed under the ")
    (:a :href "https://creativecommons.org/licenses/by-sa/4.0/"
	"Creative Commons BY-SA 4.0")
    (" license")))

(ningle/route ("/notes") ()
  (html/with-page (:title "Note Index")
    (:h1 "Path Elements")
    (:div :class "grid-container"
	  (dolist (n *nodes*)
	    (:a :href (format nil "/notes/~a" (node/name n))
		(node/name n))))
    (:h1 "Notes")
    (:div :class "" (dolist (n *notes*) (preview-note n spinneret:*html*)))))

(ningle/route ("/notes/:path") ((path :key :path))
  (let ((n (note/with-path (string->path path))))
    (cond
      ((= (length path) 1)
       (let ((name (node/name (car path))))
	 (html/with-page (:title name)
	   (:h1 (format nil "Category Page: ~a" name)))))
      (n
       (html/with-page (:title (am:-> n
				 (note/path)
				 (last)))
	 (:h1 (am:-> n
		(note/path)
		(path->string)))))
      (t (html/with-page (:title (path->string path))
	   (:p "Note does not exist"))))))
