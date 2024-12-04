;; Copyright 2021-2024 Curtis Klassen
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

(defvar *link-regexp* "\\[\\[(.*?)\\]\\]")

(defclass note (serializable)
  ((content
    :initarg :content
    :reader note/content)
   (type
    :initarg :type
    :reader note/type
    :initform :text/markdown)
   (path
    :initarg :path
    :reader note/path
    :index-type b.i:hash-list-index
    :index-reader note//all-with-node)
   (class-spec
    :initform '((:key content)
                (:key type)
                (:key path))
    :transient t
    :allocation :class)
   (id
    :reader note/id
    :index-type b.i:slot-index
    :index-reader note/with-id))
  (:metaclass b.d:persistent-class))

(defclass node (serializable)
  ((name
    :initarg :name
    :reader node/name
    :index-type b.i:string-unique-index
    :index-reader node/with-name)
   (class-spec
    :initform '((:key name))
    :transient t
    :allocation :class)
   (id
    :reader node/id
    :index-type b.i:slot-index
    :index-reader node/with-id))
  (:metaclass b.d:persistent-class))

(defclass breakout-node (serializable)
  ((parent
    :initarg :parent
    :reader breakout-node/parent)
   (breakout
    :initarg :breakout
    :reader breakout-node/breakout)
   (class-spec
    :initform '((:key parent)
                (:key breakout))
    :transient t
    :allocation :class))
  (:metaclass b.d:persistent-class))

(defclass link (serializable)
  ((text
    :initarg :text
    :accessor link/text)
   (from
    :initarg :from
    :accessor link/from)
   (to
    :initarg :to
    :accessor link/to)
   (class-spec
    :initform '((:key text)
                (:key from)
                (:key to))
    :transient t
    :allocation :class))
  (:metaclass b.d:persistent-class))

(define-condition note/already-exists-error (error)
  ((path :initarg :path
         :reader err/path))
  (:report (lambda (condition stream)
             (format stream
                     "Note already exists: `~a`"
                     (path->string (err/path condition))))))

(define-condition note/does-not-exist-error (error)
  ((path :initarg :path
         :reader err/path))
  (:report (lambda (condition stream)
             (format stream
                     "Note does not exist: `~a`"
                     (path->string (err/path condition))))))

(defmethod initialize-instance :after ((instance note/already-exists-error)
                                       &rest args)
  (declare (ignore args))
  (setf (slot-value instance 'path)
        (convert-path (err/path instance))))

(defmethod initialize-instance :after ((instance note/does-not-exist-error)
                                       &rest args)
  (declare (ignore args))
  (setf (slot-value instance 'path)
        (convert-path (err/path instance))))

(defmethod initialize-instance :after ((instance link)
                                       &rest args)
  (declare (ignore args))
  (setf (slot-value instance 'from) (convert-path (slot-value instance 'from))
        (slot-value instance 'to) (convert-path (slot-value instance 'to))))

(defmethod initialize-instance :after ((instance note)
                                       &rest args)
  (declare (ignore args))
  (setf (slot-value instance 'path)
        (convert-path (slot-value instance 'path))))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream)
    (format stream "node: ~a, id: ~a" (node/name n) (node/id n))))

(defmethod print-object ((n note) stream)
  (print-unreadable-object (n stream)
    (format stream "note: ~a, id: ~a" (path->string (note/path n)) (note/id n))))

(defmethod print-object ((l link) stream)
  (print-unreadable-object (l stream)
    (format stream
            "link: ~a -> ~a"
            (path->string (link/from l))
            (path->string (link/to l)))))

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

(defun db/links-with (node)
  (let ((conv (convert-node node)))
    (remove-if-not #λ(or (path/has-node? (link/from _0) conv)
                         (path/has-node? (link/to _0) conv))
                   (db/all-links))))

(defun link/exists? (from to)
  (find-if #λ(and (path= from (link/from _0))
		  (path= to (link/to _0)))
	   (db/all-links)))

(defun path= (a b)
  (and (every #λ(member _0 b :test #'node=) a)
       (every #λ(member _0 a :test #'node=) b)))

(defun link= (a b)
  (and (path= (link/from a)
	      (link/from b))
       (path= (link/to a)
	      (link/to b))))

(defun node= (a b)
  (equalp (node/id a) (node/id b)))

(defun convert-node (node)
  (ctypecase node
    (string (string->node node))
    (node node)
    (byte-array-16 (node/with-id node))
    (t (error "Node `~a` is not valid" node))))

(defun convert-path (path)
  (ctypecase path
    (list (mapcar #'convert-node path))
    (string (string->path path))
    (note (note/path path))
    (t (error "Path `~a` is not valid" path))))

(defun path->id (path)
  (am:-> path
    (path->string)))

(defun path/has-node? (path node)
  (member (convert-node node) (convert-path path) :test #'node=))

(defun note/has-node? (note node)
  (am:-> note
    (note/path)
    (path/has-node? node)))

(defun link/has-node? (link node)
  (or (path/has-node? (link/from link) node)
      (path/has-node? (link/to link) node)))

(defun note/exists? (path)
  (err!=nil () (note/with-path path)))

(defun note/find (id)
  (typecase id
    (string (note/with-path id))
    (list (note/with-path id))
    (node (note/all-with-node id))
    (byte-array-16 (note/with-id id))))

(defun note/with-path (path)
  (let ((converted-path (convert-path path)))
    (first-matching (note/all-with-node (car converted-path))
                    #λ(path= (note/path _0) converted-path)
                    :err #λ(error 'note/does-not-exist-error :path path))))

(defun note/all-with-node (node)
  (let ((conv (convert-node node)))
    (remove-duplicates (note//all-with-node conv)
                       :test #λ(path= (note/path _0)
                                      (note/path _1)))))

(defun link/all-with-node (node)
  (let ((conv (convert-node node)))
    (remove-duplicates (remove-if-not #λ(link/has-node? _0 conv)
                                      (db/all-links))
                       :test #'link=)))

(defun note/all-with-partial-path (path)
  (labels ((rec (path notes)
             (if path
                 (remove-if-not #λ(note/has-node? _0 (car path))
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

(let ((state (make-random-state)))
  (defun generate-id (str)))

(defun path->string (path)
  (ctypecase path
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
    (cl-ppcre:do-register-groups (str) (*link-regexp* content)
      (unless (str:starts-with? "http" str)
        (push (str:split #\| str) links)))
    links))

(defun generate-link (path &optional text)
  (if text
      (format nil "[[~a|~a]]" (path->string path) text)
      (format nil "[[~a]]" (path->string path))))

;;; should this function do something different in case a link already exists between
;;; two notes ??
(defun link/new (from to text)
  (make-instance 'link
                 :from from
                 :to to
                 :text text
                 :id (generate-id from)))

(defun note/new (path content &key (type :text/markdown))
  (if (note/exists? path)
      (error 'note/already-exists-error :path path)
      (let* ((p (convert-path path))
             (n (make-instance
                 'note
                 :path p
                 :type type
                 :content content
                 :id (path->id p))))
        (mapcar #λ(link/new path
                            (string->path (car _0))
                            (ana:aif (cadr _0)
                                     ana:it
                                     (car _0)))
                (find-links content))
        n)))

(defun link/delete (l)
  (b.d:delete-object l))

(defun node/delete (n)
  (labels ((finder (ls)
             (find-if #λ(typecase _0
                          (note (note/has-node? _0 n))
                          (link (link/has-node? _0 n))
                          ;; this should never happen
                          (t (error "obj `~a` passed to `finder` has a bad type"
                                    _0)))
                      ls)))
    (ana:awhen (or (db/links-with n)
                   (note/all-with-node n))
      (error "Deleting node `~a` when it is still referenced by `~a`"
             n
             (finder ana:it))))
  (b.d:delete-object n))

(defun note/delete (id &key (delete-nodes nil))
  (let ((note (note/find id)))
    (mapcar #'link/delete (db/links-from (note/path note)))
    (mapcar #'link/delete (db/links-to (note/path note)))
    (b.d:delete-object note)
    (if delete-nodes
        (db/clean-nodes))))

(defun node/rename (old new))

(defun note/edit (note &key path content type (delete-nodes t))
  (macrolet ((if-set (sym)
               `(if (or (and (stringp ,sym)
                             (str:empty? ,sym))
                        (null ,sym))
                    ,(intern (str:concat "OLD-" (symbol-name sym)))
                    ,sym)))
    (let ((old-path (note/path note))
          (old-content (note/content note))
          (old-type (note/type note)))
      (note/delete note :delete-nodes delete-nodes)
      (note/new (if-set path)
                (if-set content)
                :type (if-set type)))))

(defun db/clean-nodes ()
  (mapcar #λ(if (and _0
                     (= (length (note/all-with-node _0))
                        0)
                     (= (length (link/all-with-node _0))
                        0))
                (progn (node/delete _0)
                       nil)
                _0)
          (db/all-nodes)))

(defun db/clear ()
  (loop for obj in (b.d:all-store-objects)
        do (b.d:delete-object obj)))

(defun db/load-credentials ()
  (if (eq *storage/type* :s3)
      (setf zs3:*credentials* (zs3:file-credentials (make-rel-path "s3")))
      (error "Trying to load s3 credentials when *storage/type* is not set for s3")))

(defun db/load-local (path)
  (ensure-directories-exist path)
  (make-instance 'b.d:mp-store
                 :directory (car (directory path))
                 :subsystems (list (make-instance 'b.d:store-object-subsystem))))

(defun db/store-objects-of-classes (&rest classes)
  (am:->> classes
    (mapcar #'b.d:store-objects-with-class)
    (apply #'append)))

(defun db/close ()
  (b.d:close-store))
