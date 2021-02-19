(in-package :web)

(defparameter *base-path* "home/curtis/Documents/programming/lisp/web2/")

(defvar *handler*)
(defvar *jquery-file* (make-array 300000 :element-type 'character :adjustable t))

(defun run ()
  (load-db (make-rel-path "datastore"))
  (setf *handler* (clack:clackup *app*)))

(defun stop ()
  ;; (save-db)
  (clack:stop *handler*))
