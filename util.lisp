(in-package :web)

(defmacro λ-macro (&body body)
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

(defun λ-reader (stream subchar arg)
  (declare (ignore subchar))
  (declare (ignore arg))
  `(λ-macro ,(read stream t nil t)))

(set-dispatch-macro-character #\# #\λ #'λ-reader)

(defun make-rel-path (str)
  (make-pathname :directory (concatenate 'string *base-path* str)))
