(in-package :web)

(defparameter *base-path* "home/curtis/Documents/programming/lisp/web2/")

(defvar *app* (make-instance 'ningle:app))
(defvar *handler*)
(defvar *jquery-file* (make-array 300000 :element-type 'character :adjustable t))

(defun run ()
  (load-jquery)
  (load-db (make-rel-path "datastore"))
  (setf *handler* (clack:clackup *app*)))

(defun stop ()
  ;; (save-db)
  (clack:stop *handler*))

(defun load-jquery ()
  (with-open-file (s (make-rel-path "jquery.js"))))

(defun make-rel-path (str)
  (make-pathname :directory (concatenate 'string *base-path* str)))
