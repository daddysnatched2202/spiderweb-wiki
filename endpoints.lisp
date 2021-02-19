(in-package :web)

(defmacro ningle/route ((path &rest keys) (&rest param-list) &body body)
  (alexandria:with-gensyms (params maybe-key)
    (labels ((make-binding (sym)
	       (if (listp sym)
		   (destructuring-bind (name &key array
					     (key (ps:symbol-to-js-string name))
					     default)
		       sym
		     (let ((get `(,(if array
				      'get-param-array
				      'get-param)
				   ,key
				   ,params)))
		       `(,name
			 ,(if default
			      `(alexandria:if-let ((,maybe-key ,get))
				 ,maybe-key
				 ,default)
			      get))))
		   `(,sym (get-param ,(ps:symbol-to-js-string sym) ,params)))))
      (let* ((bindings (mapcar #'make-binding param-list)))
	`(setf (ningle:route *app* ,path ,@keys)
	       #'(lambda (,params)
		   (alexandria:if-let ,bindings
		     (progn ,@body)
		     (warn "Could not fill params for route ~a, required params ~a, got params ~a"
			   ,path
			   ',param-list
			   ,params))))))))

(defun ningle/respond-type (type)
  (setf (lack.response:response-headers ningle:*response*)
	(append (lack.response:response-headers ningle:*response*)
		(list :content-type type))))

(defun get-param (param params)
  (cdr (assoc param params :test #'equal)))

(defun get-param-array (param params)
  (loop for i in (remove param params :test-not #'equal :key #'car)
     collect (cdr i)))

