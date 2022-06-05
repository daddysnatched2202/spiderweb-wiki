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

(defvar *space-char*)
(defvar *break-char*)
(defvar *sep-char*)

(set-unless-bound *space-char* '(" " . "-"))
(set-unless-bound *break-char* "&")
(set-unless-bound *sep-char* ":")

(defclass note (b.d:store-object)
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
  (:metaclass b.d:persistent-class))

(defclass node (b.d:store-object)
  ((name
    :initarg :name
    :reader node/name
    :index-type b.i:string-unique-index
    :index-reader node/with-name))
  (:metaclass b.d:persistent-class))

(defclass breakout-node (b.d:store-object)
  ((parent
    :initarg :parent
    :reader breakout-node/parent)
   (breakout
    :initarg :breakout
    :reader breakout-node/breakout))
  (:metaclass b.d:persistent-class))

(defclass link (b.d:store-object)
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

(define-condition note/already-exists-error (error)
  ((path :initarg :path
         :reader err/path))
  (:report (lambda (condition stream)
             (format stream "Note already exists: `~a`" (err/path condition)))))

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

(defun convert-path (path)
  (typecase path
    (list path)
    (string (string->path path))
    (t (error "Path `~a` is not valid" path))))

(defun note/has-node? (note node)
  (member node (note/path note)))

(defun note/with-path (path)
  (let ((converted-path (convert-path path)))
    (first-matching (note/all-with-node (car converted-path))
                    #λ(path= (note/path _0) converted-path)
                    :err #λ(error "Note with path `~a` not found" path))))

(defun note/all-with-partial-path (path)
  (labels ((rec (path notes)
             (if path
                 (remove-if-not (alexandria:rcurry #'note/has-node? (car path))
                                (rec (cdr path) notes))
                 notes)))
    (rec (cdr path) (note/all-with-node (car path)))))

(defun string->node (str)
  (let* ((rem-space (am:->> str
                      (str:replace-all (car *space-char*) (cdr *space-char*))
                      (str:downcase)))
         (split-conv (str:split *break-char* rem-space)))
    (ana:aif (node/with-name rem-space)
             ana:it
             (if (> (length split-conv) 1)
                 (make-instance 'breakout-node
                                :name rem-space
                                :breakout (cdr split-conv)
                                :parent (car split-conv))
                 (make-instance 'node
                                :name rem-space)))))

(defun string->path (str)
  (am:-<>> str
    (str:downcase)
    (str:split *sep-char* am:<> :omit-nulls t)
    (copy-list)
    (sort am:<> #'string<)
    (mapcar #'string->node)))

(defun path->string (path)
  (typecase path
    (list (am:->> path
            (mapcar #'node/name)
            (str:join *sep-char*)))
    (string path)
    (t (error "Argument `path` in call to `path->string` has value `~a`, which is ~
              of bad type ~a"
              path
              (type-of path)))))

(defun find-links (content)
  (let ((links))
    (cl-ppcre:do-register-groups (str)
	("\\[\\[(.*?)\\]\\]" content)
      (push (str:split #\| str) links))
    links))

;;; should this function do something different in case a link already exists between
;;; two notes ??
(defun link/new (from to text)
  (make-instance 'link
                 :from from
                 :to to
                 :text text))

(defun link/delete (l)
  (b.d:delete-object l))

(defun note/new (path content &key (type :text/markdown))
  (let ((p (typecase path
             (list path)
             (string (string->path path)))))
    (if (handler-case (note/with-path p)
          (error (e)
            (declare (ignore e))
            nil))
	(error 'note/already-exists-error :path path)
        (let ((n (make-instance 'note
                                :path p
                                :type type
                                :content content)))
          (mapcar #λ(link/new p
                              (string->path (car _0))
                              (ana:aif (cadr _0)
                                       ana:it
                                       (car _0)))
                  (find-links content))
          n))))

(defun note/edit (note &key path content type)
  (let ((old-path (note/path note))
        (old-content (note/content note))
        (old-type (note/type note)))
    (macrolet ((if-set (sym)
                 (list 'if sym
                       sym
                       (intern (str:concat "OLD-" (symbol-name sym))))))
      (note/delete path)
      (note/new (if-set path)
                (if-set content)
                :type (if-set type)))))

(defun note/delete (path)
  (mapcar #'link/delete (db/links-from path))
  (b.d:delete-object (note/with-path path)))

(defun db/clear ()
  (loop for obj in (b.d:all-store-objects)
        do (b.d:delete-object obj)))

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
