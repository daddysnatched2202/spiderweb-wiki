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

(ps:defpsmacro jquery ((item func) (&rest lambda-list) &body body)
  `(ps:chain ($ ,item) (,func (lambda ,lambda-list ,@body))))

(ps:defpsmacro jquery-single (item function)
  `(ps:chain ($ ,item) ,function))

(defun script/lib ()
  `(ps:defun make-url (path type)
     (ps:chain $
               (post "/wiki/get-url"
                     (ps:create
                      "note-path" path
                      "url-type" type)))))

(defun script/search-page ()
  (ps:ps*
   (script/lib)))

(defun script/note-page (note-path)
  (ps:ps*
   (script/lib)
   `(jquery
     (document ready) ()
     (jquery-single ".note-dialog-delete" (hide))
     (jquery (".note-button-delete" click) ()
             (jquery-single ".note-dialog-delete" (toggle)))
     (jquery (".note-delete-confirm" click) ()
             (ps:chain $
                       (ajax (ps:create
                              "url" ,(note->url note-path)
                              "type" "DELETE"
                              "success" (lambda (result)
                                          (ps:chain window
                                                    location
                                                    (replace "/wiki/notes"))))))))))
