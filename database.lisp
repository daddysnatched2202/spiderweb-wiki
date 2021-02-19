(in-package :web)

(defclass note (bknr.datastore:store-object)
  ((content
    :accessor note/content
    :initarg :content
    :initform nil)
   (content-type
    :accessor note/content-type
    :initarg :type
    :initform nil
    :index-type bknr.indices:hash-index
    :index-reader notes/with-type)
   (path-nodes
    :accessor note/path-nodes
    :initarg :path-nodes
    :initform nil
    :index-type bknr.indices:hash-list-index
    :index-initargs (:test 'equalp)
    :index-reader notes/with-node)
   (links
    :accessor note/links
    :initarg :links
    :initform nil))
  (:metaclass bknr.datastore:persistent-class))

(defclass node (bknr.datastore:store-object)
  ((name
    :accessor node/name
    :initarg :name
    :initform nil
    :index-type bknr.indices:string-unique-index
    :index-reader node/with-name
    :index-initargs (:test 'equalp)))
  (:metaclass bknr.datastore:persistent-class))

(defclass breakout-node (node)
  ((breakout-path
    :accessor breakout-node/path
    :initarg :breakout-path
    :initform nil)
   (parent
    :accessor breakout-node/parent
    :initarg :parent
    :initform nil))
  (:metaclass bknr.datastore:persistent-class))

(defclass link (bknr.datastore:store-object)
  ((text
    :accessor link/text
    :initarg :text
    :initform nil)
   (to-path
    :accessor link/to
    :initarg :to-path
    :initform nil
    :index-type bknr.indices:hash-index
    :index-initargs (:test 'equal)
    :index-reader links/with-to)
   (from-path
    :accessor link/from
    :initarg :from-path
    :initform nil
    :index-type bknr.indices:hash-index
    :index-initargs (:test 'equal)
    :index-reader links/with-from))
  (:metaclass bknr.datastore:persistent-class))

(defun clear-db ()
  (loop for obj in (bknr.datastore:all-store-objects)
     do (bknr.datastore:delete-object obj)))

(defun load-db (path)
  (make-instance 'bknr.datastore:mp-store
		 :directory path
		 :subsystems (list (make-instance 'bknr.datastore:store-object-subsystem))))

(defun make-serial-specs ()
  (let ((args '((note . (((content "content")
			  (content-type "content-type")
			  (path-nodes "path-nodes" (:seq node))
			  (links "links" (:seq link)))
			 :deserial? nil))
		(node . (((name "name"))
			 :deserial? t))
		(breakout-node . (((breakout-path "breakout-path")
				   (parent "parent"))
				  :deserial? t))
		(link . (((text "text")
			  (to-path "to-path" (:seq node))
			  (from-path "from-path" (:seq node)))
			 :deserial? t)))))
    (loop for c in '(note node breakout-node link)
       do (closer-mop:ensure-finalized (find-class c))
	 (apply #'make-class-spec c (cdr (assoc c args))))))

(defun close-db ()
  (bknr.datastore:close-store))

(defun path= (a b)
  (equal a b))

(defun note-has-node (note node)
  (member node (note/path-nodes note)))

(defun note-with-path (path)
  (loop for n in (notes-with-path-node (car path))
     if (path= (note/path-nodes n) path)
     do (return-from note-with-path n)))

(defun notes-with-path-node (node)
  (notes/with-node node))

(defun notes-with-partial-path (path)
  (labels ((rec (path notes)
	     (cond
	       ((null path) notes)
	       (t (remove-if-not #λ(note-has-node _0 (car path))
				 (rec (cdr path) notes))))))
    (rec (cdr path) (notes-with-path-node (car path)))))

(defun node-from-string (str)
  (let* ((space '(" " . "-"))
	 (breakout #\&)
	 (conv (arrow-macros:->> str
		 (str:replace-all (car space) (cdr space))
		 (str:downcase)))
	 (split-conv (str:split breakout conv)))
    (if (nth-value 1 (node/with-name conv))
	(node/with-name conv)
	(if (> (length split-conv) 1)
	    (make-instance 'breakout-node
			   :name conv
			   :breakout-path (cdr split-conv)
			   :parent (car split-conv))
	    (make-instance 'node
			   :name conv)))))

(defun path-from-string (str)
  (let ((sep #\:))
    (arrow-macros:-<>> (str:split sep str :omit-nulls t)
      (copy-list)
      (sort arrow-macros:<> #'string<)
      (mapcar #'node-from-string))))

(defun all-objects ()
  (bknr.datastore:all-store-objects))

(defun find-links (content)
  (let ((links))
    (cl-ppcre:do-register-groups (str)
	("\\[\\[(.*?)\\]\\]" content)
      (push str links))
    (mapcar #λ(str:split #\| _0) links)))

(defun make-links (note)
  (loop for l in (find-links (note/content note))
     do (make-instance 'link
		       :from-path (note/path-nodes note)
		       :to-path (path-from-string (car l))
		       :text (cadr l))))

(defun delete-note (note)
  (bknr.datastore:with-transaction ()
    (bknr.datastore:delete-object note)
    (loop for l in (links/with-from (path-from-string (note/path-nodes note)))
       do (bknr.datastore:delete-object l))))

(defun new-note (content type string-path)
  (let ((n (if (note-with-path (path-from-string string-path))
	       (error "Note with path already ~a exists" string-path)
	       (make-instance 'note
			      :content content
			      :type type
			      :path-nodes (path-from-string string-path)))))
    (make-links n)))

;;; Here are the functions for transfering from the old facts-based db to the new one
;;; Just in case there are problems
;; (defun fact-notes ()
;;   (let ((notes))
;;     (facts:with ((?note :is-a :note))
;;       (push (list ?note
;; 		  (facts:with ((?note :is-a :note :content ?content))
;; 		    (return ?content))
;; 		  (let ((paths))
;; 		    (facts:with ((?note :is-a :note :has-path ?path))
;; 		      (push ?path paths))
;; 		    paths))
;; 	    notes))
;;     notes))

;; (defun transfer ()
;;   (loop for n in (fact-notes)
;;      for str-path = (str:join #\: (caddr n))
;;      for content = (cadr n)
;;      do (new-note content :text/markdown str-path)))
