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

(defvar *app* (make-instance 'ningle:app))

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

(ningle/route ("/wiki/notes") ()
  (html/with-page (:title "Note Index")
    (:h1 "Nodes")
    (:div :class "nodes-container"
	  (dolist (n (am:-> (db/all-nodes)
                       (sort #'string< :key #'node/name)))
	    (:a :href (node->url n)
		(node/name n))))
    (:h1 "Notes")
    (:div :class "notes-container"
          (:raw (html/gen-note-previews (db/all-notes)
                                        :class "note-preview-grid")))))

(ningle/route ("/wiki/notes/:path")
    (:binding-list ((path-text :key :path))
     :fail-clause (note/does-not-exist-error ()
                                             (ningle/redirect
                                              (format nil "/wiki/make-note/~a"
                                                      path-text))))
  (let* ((path (string->path path-text))
         (node (note/with-path path-text)))
    (html/with-page (:title (path->string path))
      (:a :href (note->url node)
          :class "note-title"
          (am:-> node
            (note/path)
            (path->string)))
      (:div :class "note-preview"
       (:raw (if (eq :text/markdown (note/type node))
                 (with-output-to-string (s)
                   (3bmd:parse-string-and-print-to-stream
                    (note/content node)
                    s))
                 (error "~a is not a markdown note; Only markdown notes are ~
                       supported right now"
                        node))))
      (when (db/links-from path)
        (:h2 "Linked Notes")
        (:div :class "link-box"
              (dolist (l (db/links-from path))
                (:a :href (note->url (link/to l))
                    (path->string (link/to l))))))
      (when (db/links-to path)
        (:h2 "Backlinks")
        (:div :class "link-box"
              (dolist (l (db/links-to path))
                (:a :href (note->url (link/from l))
                    (path->string (link/from l))))))
      (:div :class "note-button-bar"
            (:a :href (note->url node :prefix :edit)
                :class "note-button-edit"
                "Edit This Note")
            (:a :href "#"
                :class "note-button-delete"
                "Delete This Note")
            (:div :class "note-dialog-delete"
                  (:p "Are you sure you want to delete this note?")
                  (:br)
                  (:a :href "#"
                      :class "note-delete-confirm"
                      "Confirm")))
      (:script (:raw (script/note-page (path->string path)))))))

(ningle/route ("/wiki/notes/:path" :method :delete)
    (:binding-list ((path :key :path)))
  (note/delete path :delete-nodes t)
  (html/with-page (:title "Success")))

(ningle/route ("/wiki/make-note") ()
  (html/with-page (:title "New Note")
    (:h1 "New Note")
    (:form :action "/wiki/make-note"
           :method "post"
           :autocomplete "off"
           :class "note-edit-form"
           (:input :type "text"
                   :name "path")
           (:textarea :name "content"
                      :rows 50)
           (:input :type "submit"
                   :value "Make Note"))))

(ningle/route ("/wiki/make-note/:init-path")
    (:binding-list ((init-path :key :init-path)))
  (html/with-page (:title "New Note")
    (:h1 "New Note")
    (:form :action "/wiki/make-note"
           :method "post"
           :autocomplete "off"
           :class "note-edit-form"
           (:input :type "text"
                   :name "path"
                   :value (path->string (string->path init-path)))
           (:textarea :name "content"
                      :rows 50)
           (:input :type "submit"
                   :value "Make Note"))))

(ningle/route ("/wiki/make-note" :method :post)
    (:binding-list (path content)
     :fail-clause (error (e)
                         (html/with-page (:title "Error")
                           (:p (format nil
                                       "Encountered an error while trying to ~
                                        create note `~a`: ~a"
                                       (path->string path)
                                       (princ-to-string e))))))
  (html/with-page (:title "Success")
    (:p (format nil
                "Made note `~a` successfully"
                (path->string (note/path (note/new path content)))))))

(ningle/route ("/wiki/edit-note/:path")
    (:binding-list ((path-text :key :path)))
  (let ((note (note/with-path path-text)))
    (html/with-page (:title "Edit Note")
      (:h1 (format nil "Editing ~a" (path->string path-text)))
      (:form :action "/wiki/edit-note"
             :method "post"
             :autocomplete "off"
             :class "note-edit-form"
             (:input :type "text"
                     :name "new-path"
                     :value (am:-> note
                              (note/path)
                              (path->string)))
             (:textarea :name "new-content"
                        :rows 50
                        (note/content note))
             (:input :type "submit"
                     :value "Edit Note")
             (:input :type "hidden"
                     :name "old-path"
                     :value path-text)))))

(ningle/route ("/wiki/edit-note" :method :post)
    (:binding-list ((old-path :key "old-path")
                    (new-path :key "new-path")
                    (new-content :key "new-content")))
  (note/edit (note/with-path old-path)
             :path new-path
             :content new-content)
  (html/with-page (:title "Success")
    (:p (format nil
                "Note `~a` was edited"
                (path->string old-path)))))

(ningle/route ("/wiki/node/:node")
    (:binding-list ((node-text :key :node)))
  (let ((path (string->path node-text)))
    (cond ((> (length path) 1)
           (html/with-page (:title "Error")
             (:p (format nil
                         "Path supplied to `/wiki/node` must have only one path ~
                          element; its actual value is `~a`"
                         (path->string path)))))
          (t (html/with-page (:title node-text)
               (:h1 (format nil "Category Page: ~a" node-text))
               (:br)
               (:raw (html/gen-note-previews (note/all-with-node (car path))))
               (:h1 "Rename?")
               (:form :action "/wiki/node-rename"
                      :method "post"
                      :autocomplete "off"
                      :class "node-edit-form"
                      (:input :type "text"
                              :name "new-name"
                              :value node-text)
                      (:input :type "submit"
                              :value "Change Path")
                      (:input :type "hidden"
                              :name "old-name"
                              :value node-text)))))))

(ningle/route ("/wiki/node-rename" :method :post)
    (:binding-list ((old-name :key "old-name")
                    (new-name :key "new-name")))
  (node/rename old-name new-name)
  (html/with-page (:title "Success")
    (:p "Node renamed")))

(ningle/route ("/wiki/search") ()
  (html/with-page (:title "Search")
    (:h1 "Search Notes")
    (:input :class "search-input")
    (:a :href "#" :class "search-paths-button" "Search Paths")
    (:a :href "#" :class "search-notes-button" "Search Notes")
    (:h1 "Results")
    (:div :class "search-results")
    (:script (:raw (script/search-page)))))

(ningle/route ("/wiki") ()
  (ningle/redirect "/wiki/notes"))

(ningle/route ("/wiki/json/notes") ()
  (handler-case (jonathan:to-json (general->serial (db/all-notes))
                                  :from :alist)
    (condition (c)
      (html/with-page (:title "Error")
        (:p (format nil "Error: ~a" c))))
    (:no-error (json)
      (ningle/respond-type "application/json")
      json)))

(ningle/route ("/wiki/get-url" :method :post)
    (:binding-list ((note-path :key "note-path")
                    (url-type :key "url-type")))
  (note->url note-path
            :prefix (intern (str:upcase url-type)
                            "KEYWORD")))

;;; We use lisp for the cache instead of nginx to automate downloading the file
(setf (ningle/app:route *app* *jquery-url*)
      (lambda (params)
	(declare (ignore params))
	(if (eq *jquery/source-type* :cdn)
	    (ningle/redirect *jquery/source-url*)
	    (ningle/cache-file *jquery-file*
			       *jquery-hash*
			       :file-type "text/javascript"))))
