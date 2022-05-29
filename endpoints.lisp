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

;;; can be used in an end point for caching static files on the client
;;; file-type should be a string which will be passed to ningle/respond-type, or nil
(defun ningle/cache-file (file hash &key (file-type nil))
  (let* ((req (lack.request:request-headers ningle:*request*))
	 (str (gethash "if-none-match" req)))
    (if (string= str hash)
	(progn
	  (ningle/set-response-status 304)
	  "Cache up to date")
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
                                (:tmp 307)))
  "Redirect")

;;; endpoints
(ningle/route ("/wiki/json/notes") ()
  (ningle/respond-type "application/json")
  (jonathan:to-json (mapcar #'obj->serial (db/all-notes))
		    :from :alist))

;;; TODO: system for different licenses / repos using classes
(ningle/route ("/wiki/licenses") ()
  (html/with-page (:title "License Info")
    (:a :href "https://github.com/daddysnatched2202/spiderweb-wiki" "Source code")
    (" available under the terms of the ")
    (:a :href "https://www.gnu.org/licenses/gpl-3.0.html" "GNU GPL version 3")
    (:p "Content on this website is licensed under the ")
    (:a :href "https://creativecommons.org/licenses/by-sa/4.0/"
	"Creative Commons BY-SA 4.0")
    (" license")))

;;; TODO: code for note previews
(ningle/route ("/wiki/notes") ()
  (html/with-page (:title "Note Index")
    (:h1 "Path Elements")
    (:div :class "grid-container"
	  (dolist (n (db/all-nodes))
	    (:a :href (format nil "/notes/~a" (node/name n))
		(node/name n))))
    (:h1 "Notes")
    (:div :class "notes-preview" (dolist (n (db/all-notes))))))

(ningle/route ("/wiki/make-note") ()
  (html/with-page (:title "New Note")
    (:h1 "New Note")
    (:form :action "/wiki/make-note"
           :method "post"
           :autocomplete "off"
           :class "note-edit"
           (:input :type "text" :name "path")
           (:textarea :name "content"
                      :rows 50)
           (:input :type "submit"))))

(ningle/route ("/wiki/make-note" :method :post) (path content)
  (handler-case (note/new path content)
    (error (e)
      (html/with-page (:title "Error")
        (:p (format nil "Encountered an error while trying to create note `~a`: ~a"
                    (path->string path)
                    (princ-to-string e)))))
    (:no-error (e)
      (declare (ignore e))
      (html/with-page (:title "Success")
        (:p (format nil "Made note `~a` successfully"
                 path))))))

(ningle/route ("/wiki/notes/:path") ((path :key :path))
  (let ((n (note/with-path (string->path path))))
    (cond
      ((= (length path) 1)
       (let ((name (node/name (car path))))
	 (html/with-page (:title name)
	   (:h1 (format nil "Category Page: ~a" name)))))
      ((not (null n))
       (html/with-page (:title (am:-> n
				 (note/path)
				 (last)))
	 (:h1 (am:-> n
		(note/path)
		(path->string)))))
      (t (html/with-page (:title (path->string path))
	   (:p "Note does not exist"))))))

(ningle/route ("/wiki") ()
  (ningle/redirect "/wiki/notes"))

;;; We use lisp to cache the file instead of nginx to automate downloading the file
(setf (ningle/app:route *app* *jquery-url*)
      (lambda (params)
	(declare (ignore params))
	(if (eq *jquery-source* :cdn)
	    (warn "Requested jquery URL when wiki is set to use an external CDN")
	    (ningle/cache-file *jquery-file*
			       *jquery-hash*
			       :file-type "text/javascript"))))
