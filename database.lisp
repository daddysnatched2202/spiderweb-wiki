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
(defvar *links* nil)

(defclass note (serializable)
  ((content
    :initarg :content
    :reader note/content)
   (type
    :initarg :type
    :reader note/type)
   (path
    :initarg :path
    :reader note/path)
   (class-spec
    :initform (let ((n (find-class 'note)))
		(make-instance 'class-spec
			       :ref n
			       :slot-specs (list
					    (make-slot-spec n
							    'content
							    nil)
					    (make-slot-spec n
							    'type
							    nil)
					    (make-slot-spec n
							    'path
							    nil)))))))

(defclass node (serializable)
  ((name
    :initarg :name
    :reader node/name)
   (class-spec
    :initform (let ((n (find-class 'node)))
		(make-instance 'class-spec
			       :ref n
			       :slot-specs (list (make-slot-spec n
								 'name
								 nil)))))))

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
  (string= (path->string a)
	   (path->string b)))

(defun link= (a b)
  (and (path= (link/from a)
	      (link/from b))
       (path= (link/to a)
	      (link/to b))))

(defun note/has-node? (note node)
  (member node (note/path note)))

(defun note/all-with-node (node)
  (remove-if-not #λ(note/has-node? _0 node) *notes*))

(defun note/with-path (path)
  (loop for n in (note/all-with-node (car path))
	if (path= (note/path n) path)
	  do (return n)))

(defun note/all-with-partial-path (path)
  (labels ((rec (path notes)
	     (if path
		 (remove-if-not (alexandria:rcurry #'note/has-node? (car path))
				(rec (cdr path) notes))
		 notes)))
    (rec (cdr path) (note/all-with-node (car path)))))

(defun node/find-with-name (name)
  (find name *nodes* :key #'node/name :test #'string=))

(let ((space '(" " . "-"))
      (break-char #\&)
      (sep #\:))
  (defun string->node (str)
    (let* ((rem-space (am:->> str
			(str:replace-all (car space) (cdr space))
			(str:downcase)))
	   (split-conv (str:split break-char rem-space)))
      (ana:aif (node/find-with-name rem-space)
	  ana:it
	  (let ((node (if (> (length split-conv) 1)
			  (make-instance 'breakout-node
					 :name rem-space
					 :breakout (cdr split-conv)
					 :parent (car split-conv))
			  (make-instance 'node
					 :name rem-space))))
	    (push node *nodes*)
	    node))))

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
  (let* ((l (make-instance 'link
			   :from from
			   :to to
			   :text text)))
    (when (find l *links* :test #'link=)
      (link/delete l))
    (push l *links*)))

(defun link/delete (l)
  (setq *links* (delete l *links* :test #'link=)))

(defun note/new (path-str content &key (type :text/markdown))
  (let ((p (string->path path-str)))
    (if (note/with-path p)
	(error "Note already exists: ~a" path-str)
	(let ((n (make-instance 'note
				:path p
				:type type
				:content content)))
	  (push n *notes*)
	  (mapcar #λ(link/new (car _0)
			      (cadr _0)
			      (ana:aif (caddr _0)
				       ana:it
				       (car _0)))
		  (find-links content))
	  n))))

(defun note/delete (path)
  (setf *notes* (remove-if #λ(path= (note/path _0) path)
			   *notes*)))

(defun note/move (old-path new-path)
  (let* ((n (note/with-path old-path))
	 (cont (note/content n))
	 (type (note/type n)))
    (note/delete old-path)
    (note/new (path->string new-path)
	      cont
	      :type type)))

(defun db/clear ()
  (setf *notes* nil))

(defun file-contents (path)
  (with-open-file (stream path :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun db/load-local (path)
  (am:-> path
    (file-contents)
    (jonathan:parse)))

(defun db/save-local (path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (am:->> *notes*
      (general->serial)
      (jonathan:to-json)
      (format stream "~a"))))

(defun db/load-credentials ()
  (if (eq *storage-type* :s3)
      (setf zs3:*credentials* (zs3:file-credentials (make-rel-path "s3")))
      (error "Trying to load s3 credentials when *storage-type* is not set for s3")))
