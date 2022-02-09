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

(defvar *nodes* nil)
(defvar *notes* nil)

(defclass note ()
  ((content
    :initarg :content
    :reader note/content)
   (type
    :initarg :type
    :reader note/type)
   (path
    :initarg :path
    :reader note/path)))

(defclass node ()
  ((name
    :initarg :name
    :reader node/name)))

(defclass link ()
  ((text
    :initarg :text
    :reader link/text)
   (from
    :initarg :from
    :reader link/from)
   (to
    :initarg :to
    :reader link/to)))

(defun path= (a b)
  (equal a b))

(defun note-has-node (note node)
  (member node (note/path note)))

(defun notes-with-node (node)
  (loop for n in *notes*
	if (note-has-node n node)
	  collect n))

(defun note-with-path (path)
  (loop for n in (notes-with-node (car path))
	if (path= (note/path n) path)
	  do (return n)))

(defun notes-with-partial-path (path)
  (labels ((rec (path notes)
	     (cond
	       ((null path) notes)
	       (t (remove-if-not #λ(note-has-node _0 (car path))
				 (rec (cdr path) notes))))))
    (rec (cdr path) (notes-with-node (car path)))))

(defun node-with-name (name)
  (find name *nodes* :test #'node/name))

(let ((space '(" " . "-"))
      (break-char #\&)
      (sep #\:))
  (defun string->node (str)
    (let* ((rem-space (am:->> str
			(str:replace-all (car space) (cdr space))
			(str:downcase)))
	   (split-conv (str:split break-char rem-space)))
      (if (nth-value 1 (node-with-name rem-space))
	  (node-with-name rem-space)
	  (if (> (length split-conv) 1)
	      (make-instance 'breakout-node
			     :name rem-space
			     :breakout (cdr split-conv)
			     :parent (car split-conv))
	      (make-instance 'node
			     :name rem-space)))))

  (defun string->path (str)
    (am:-<>> (str:split sep str :omit-nulls t)
      (copy-list)
      (sort am:<> #'string<)
      (mapcar #'string->node)))

  (defun path->string (path)
    (str:join sep path)))

(defun find-links (content)
  (let ((links))
    (cl-ppcre:do-register-groups (str)
	("\\[\\[(.*?)\\]\\]" content)
      (push (str:split #\| str) links))
    links))

(defun new-note (path-str content &key (type :text/markdown))
  (let ((p (string->path path-str)))
    (if (note-with-path p)
	(error "Note already exists: ~a" path-str)
	(make-instance 'note
		       :path p
		       :type type
		       :content content
		       :store-obj nil))))

(defun delete-note (path)
  (b.d:with-transaction ()
    (b.d:delete-object (note-with-path path))))

(defun move-note (old-path new-path)
  (let* ((n (note-with-path old-path))
	 (cont (note/content n))
	 (type (note/type n)))
    (b.d:with-transaction ()
      (b.d:delete-object n)
      (new-note (path->string new-path)
		cont
		:type type))))

(defun clear-db ()
  (b.d:with-transaction ()
      (loop for obj in (b.d:all-store-objects)
	    do (b.d:delete-object obj))))

(defun load-db-local (path)
  (make-instance 'b.d:mp-store
		 :directory path
		 :subsystems (list
			      (make-instance
			       'b.d:store-object-subsystem))))

(defun load-credentials ()
  (if (eq *storage-type* :s3)
      (setf zs3:*credentials* (zs3:file-credentials *base-path*))
      (error "Trying to load s3 credentials when *storage-type* is not set for s3")))

(defun make-db-specs ()
  (let ((nt (find-class 'note))
	(nd (find-class 'node)))
    (if (or (gethash nt *class-specs*)
	    (gethash nd *class-specs*))
	(error "Tried to make db specs twice !")
	(list (make-class-spec 'note
			       (list (make-slot-spec nt 'content nil)
				     (make-slot-spec nt 'type nil)
				     (make-slot-spec nt 'path nil)))
	      (make-class-spec 'node
			       (list (make-slot-spec nd 'name nil)))))))
