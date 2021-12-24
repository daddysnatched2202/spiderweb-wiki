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

(defclass note ()
  ((content
    :initarg :content
    :reader note/content)
   (type
    :initarg :type
    :reader note/type)
   (path
    :initarg :type
    :reader note/path
    :index-type b.i:hash-list-index
    :index-reader notes-with-node)
   (store-obj
    :initarg :store-obj
    :accessor note/store-obj))
  (:metaclass b.i:indexed-class))

(defclass node ()
  ((name
    :initarg :name
    :reader node/name
    :index-type b.i:string-unique-index
    :index-reader node-with-name))
  (:metaclass b.i:indexed-class))

(defclass breakout-node ()
  ((parent
    :initarg :parent
    :reader breakout-node/parent)
   (breakout
    :initarg :breakout
    :reader breakout-node/breakout))
  (:metaclass b.i:indexed-class))

(defclass link ()
  ((text
    :initarg :text
    :reader link/text)
   (from
    :initarg :from
    :reader link/from)
   (to
    :initarg :to
    :reader link/to))
  (:metaclass b.i:indexed-class))

(defun path= (a b)
  (equal a b))

(defun note-has-node (note node)
  (member node (note/path note)))

(defun note-with-path (path)
  (loop for n in (notes-with-node (car path))
	if (path= (note/path n) path)
	do (return-from note-with-path n)))

(defun notes-with-partial-path (path)
  (labels ((rec (path notes)
	     (cond
	       ((null path) notes)
	       (t (remove-if-not #λ(note-has-node _0 (car path))
				 (rec (cdr path) notes))))))
    (rec (cdr path) (notes-with-node (car path)))))

(defun node-from-string (str)
  (let* ((space '(" " . "-"))
	 (breakout #\&)
	 (conv (a-m:->> str
		 (str:replace-all (car space) (cdr space))
		 (str:downcase)))
	 (split-conv (str:split breakout conv)))
    (if (nth-value 1 (node-with-name conv))
	(node-with-name conv)
	(if (> (length split-conv) 1)
	    (make-instance 'breakout-node
			   :name conv
			   :breakout (cdr split-conv)
			   :parent (car split-conv))
	    (make-instance 'node
			   :name conv)))))

(defun path-from-string (str)
  (let ((sep #\:))
    (a-m:-<>> (str:split sep str :omit-nulls t)
      (copy-list)
      (sort a-m:<> #'string<)
      (mapcar #'node-from-string))))

(defun find-links (content)
  (let ((links))
    (cl-ppcre:do-register-groups (str)
	("\\[\\[(.*?)\\]\\]" content)
      (push str links))
    (mapcar #λ(str:split #\| _0) links)))

(defun new-note (path-str content &key (type :text/markdown))
  (let ((p (path-from-string path-str)))
    (if (note-with-path p)
	(error "Note already exists: ~a" path-str)
	(make-instance 'note
		       :path p
		       :type type
		       :content content
		       :store-obj nil))))

(defun delete-note ())

(defun clear-db ()
  (loop for obj in (bknr.datastore:all-store-objects)
	do (bknr.datastore:delete-object obj)))

(defun load-db (path)
  (make-instance 'bknr.datastore:mp-store
		 :directory path
		 :subsystems (list
			      (make-instance
			       'bknr.datastore:store-object-subsystem))))

(defun close-db ()
  (bknr.datastore:close-store))

(defun all-objects ()
  (bknr.datastore:all-store-objects))

(defun all-notes ()
  (bknr.datastore:store-objects-with-class 'note))
