(in-package :web)

(defmacro λ (&body body)
  (alexandria:with-gensyms (args)
    (let ((bindings (loop for i in
			 (let ((l))
			   (tree-equal body body
				       :test #'(lambda (x y)
						 (declare (ignore y))
						 (if (and (symbolp x)
							  (ppcre:scan "_[0-9]+$" (symbol-name x)))
						     (push x l))
						 x))
			   l)
		       collect `(,i (nth ,(parse-integer (subseq (symbol-name i) 1)) ,args)))))
      `#'(lambda (&rest ,args)
	   (let (,@bindings)
	     ,@body)))))

(defun make-rel-path (str)
  (make-pathname :directory (concatenate 'string *base-path* str)))
