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

(defclass persistent-indexed-class (b.d:persistent-class b.i:indexed-class)
  ())

(defclass note ()
  ((content
    :initarg :content
    :reader note/content)
   (type
    :initarg :type
    :reader note/type)
   (path
    :initarg :path
    :reader note/path
    :index-type b.i:hash-list-index
    :index-reader note/all-with-node))
  (:metaclass persistent-indexed-class))

(defclass node ()
  ((name
    :initarg :name
    :reader node/name
    :index-type b.i:string-unique-index
    :index-reader node/with-name))
  (:metaclass persistent-indexed-class))

(defclass breakout-node ()
  ((parent
    :initarg :parent
    :reader breakout-node/parent)
   (breakout
    :initarg :breakout
    :reader breakout-node/breakout))
  (:metaclass persistent-indexed-class))

(defclass link ()
  ((text
    :initarg :text
    :accessor link/text)
   (from
    :initarg :from
    :accessor link/from)
   (to
    :initarg :to
    :accessor link/to))
  (:metaclass b.d:persistent-class))

(defun db/all-notes ()
  (b.d:store-objects-with-class 'note))

(defun db/all-nodes ()
  (db/store-objects-of-classes 'node 'breakout-node))

(defun db/all-links ()
  (b.d:store-objects-with-class 'link))

(defun db/links-to (path)
  (remove-if-not #λ(path= (link/to _0) path)
		 (db/all-links)))

(defun db/links-from (path)
  (remove-if-not #λ(path= (link/from _0) path)
		 (db/all-links)))

(defun link/exists? (from to)
  (find-if #λ(and (path= from (link/from _0))
		  (path= to (link/to _0)))
	   (db/all-links)))

(defun path= (a b)
  (string= (path->string a)
	   (path->string b)))

(defun link= (a b)
  (and (path= (link/from a)
	      (link/from b))
       (path= (link/to a)
	      (link/to b))))

(defun note/has-node? (note node)
  (member node (note/path note)))

(defun note/with-path (path)
  (first-matching (note/all-with-node (car path))
		  #λ(path= (note/path _0) path)
		  :err (list "No note with path '~a'" path)))

(defun note/all-with-partial-path (path)
  (labels ((rec (path notes)
	     (if path
		 (remove-if-not (alexandria:rcurry #'note/has-node? (car path))
				(rec (cdr path) notes))
		 notes)))
    (rec (cdr path) (note/all-with-node (car path)))))

(let ((space '(" " . "-"))
      (break-char #\&)
      (sep #\:))
  (defun string->node (str)
    (let* ((rem-space (am:->> str
			(str:replace-all (car space) (cdr space))
			(str:downcase)))
	   (split-conv (str:split break-char rem-space)))
      (ana:aif (node/with-name rem-space)
	       ana:it
	       (b.d:with-transaction ()
                 (if (> (length split-conv) 1)
                     (make-instance 'breakout-node
                                    :name rem-space
                                    :breakout (cdr split-conv)
                                    :parent (car split-conv))
                     (make-instance 'node
                                    :name rem-space))))))

  (defun string->path (str)
    (am:-<>> (str:split sep str :omit-nulls t)
      (copy-list)
      (sort am:<> #'string<)
      (mapcar #'string->node)))

  (defun path->string (path)
    (am:->> path
      (mapcar #'node/name)
      (str:join sep))))

(defun find-links (content)
  (let ((links))
    (cl-ppcre:do-register-groups (str)
	("\\[\\[(.*?)\\]\\]" content)
      (push (str:split #\| str) links))
    links))

(defun link/new (from to text)
  (b.d:with-transaction ()
    (ana:aif (link/exists? from to)
	     (setf (link/from ana:it) from
		   (link/to ana:it) to
		   (link/text ana:it) text)
	     (make-instance 'link
			    :from from
			    :to to
			    :text text))))

(defun link/delete (l)
  (b.d:with-transaction ()
    (b.d:delete-object l)))

(defun note/new (path-str content &key (type :text/markdown))
  (let ((p (string->path path-str)))
    (if (note/with-path p)
	(error "Note already exists: ~a" path-str)
	(let ((n (make-instance 'note
				:path p
				:type type
				:content content)))
	  (b.d:with-transaction ()
	    (mapcar #λ(link/new (car _0)
			        (cadr _0)
			        (ana:aif (caddr _0)
				         ana:it
				         (car _0)))
		    (find-links content)))
	  n))))

(defun note/delete (path)
  (b.d:with-transaction ()
    (mapcar #'link/delete (db/links-from path))
    (b.d:delete-object (note/with-path path))))

(defun note/move (old-path new-path)
  (let* ((n (note/with-path old-path))
         (cont (note/content n))
	 (type (note/type n)))
    (b.d:with-transaction ()
      (note/delete old-path)
      (note/new (path->string new-path)
                cont
                :type type))))

(defun db/clear ()
  (b.d:with-transaction ()
    (loop for obj in (b.d:all-store-objects)
          do (b.d:delete-object obj))))

(defun db/load-credentials ()
  (if (eq *storage-type* :s3)
      (setf zs3:*credentials* (zs3:file-credentials (make-rel-path "s3")))
      (error "Trying to load s3 credentials when *storage-type* is not set for s3")))

(defun db/load-local (path)
  (make-instance 'b.d:mp-store
                 :directory path
                 :subsystems (list (make-instance 'b.d:store-object-subsystem))))

(defun db/store-objects-of-classes (&rest classes)
  (am:->> classes
    (mapcar #'b.d:store-objects-with-class)
    (apply #'append)))

(defun db/close ()
  (b.d:close-store))
