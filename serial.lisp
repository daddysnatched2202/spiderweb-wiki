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

;;; todo: Optional type declarations in encoded json
;;; todo: handle types that differ in serialization vs deserialization (symbols)
;;; todo: custom init functions
;;; todo: optionally disallow deserializaing

;;; type-def
;;; atomic
;;; seq (sequences have to be lists for now)
;;; class 
;;; any (nil), deserializes directly
;;; inherit

;;; t, t : class
;;; (:seq t), t : class

(defvar *class-specs* (make-hash-table))

(defclass test-class () ((content :initarg :content)))
(defclass test-sub (test-class) ((morc :initarg :morc)))
(defclass test-cont-class () ((name :initarg :name)))

(defclass slot-spec () ((ref
			 :initarg :ref
			 :accessor slot-spec/ref)
			(type-def
			 :initarg :type-def
			 :accessor slot-spec/type-def)
			(key
			 :initarg :key
			 :accessor slot-spec/key)
			(class-ref
			 :initarg :class-ref
			 :accessor slot-spec/class-ref)))

(defclass class-spec () ((ref
			  :initarg :ref
			  :accessor class-spec/ref)
			 (slot-specs
			  :initarg :slot-specs
			  :accessor class-spec/slot-specs)
			 (deserial?
			  :initarg :deserial
			  :accessor class-spec/deserial?
			  :initform t)))

(defun class->serial (obj)
  (multiple-value-bind (class-spec found)
      (gethash (class-of obj)
	       *class-specs*)
    (if found
	(loop for s in (class-spec/slot-specs class-spec)
	   collect `(,(slot-spec/key s) .
		      ,(arrow-macros:->> s
					 (slot-spec/ref)
					 (closer-mop:slot-definition-name)
					 (slot-value obj)
					 (general->serial))))
	(error "No class spec for class ~a" (class-of obj)))))

(defun general->serial (obj)
  (cond
    ((nth-value 1 (gethash (class-of obj) *class-specs*))
     (class->serial obj))
    ((null obj) nil)
    ((listp obj)
     (cons (general->serial (car obj)) (general->serial (cdr obj))))
    (t
     obj)))

;;; doesn't do type check (not a problem since serial->slot does it)
(defun can-interpret-as-class (ls class-spec)
  (when (class-spec/deserial? class-spec)
    (let ((all-slots-accounted
	   (every #λ(assoc _0 ls :test #'equalp)
		  (mapcar #'slot-spec/key
			  (class-spec/slot-specs class-spec))))
	  (all-keys-accounted
	   (every #λ(member _0
			    (mapcar #'slot-spec/key
				    (class-spec/slot-specs class-spec))
			    :test #'equalp)
		  (mapcar #'car ls))))
      (cond
	((and all-slots-accounted all-keys-accounted)
	 :perfect)
	(all-slots-accounted
	 :subclass)
	(t nil)))))

;;; Determines the most correct class-spec for the object
;;; If a class is specified and it has children we have specs for:
;;; If the object matches the parent class perfectly (no extra data),
;;; the parent-class is returned
;;; If there is a child-class (recursive) that matches the data perfectly,
;;; that child will be returned
;;; If there is no perfect match, nil will be returned
(defun super-type-check (obj class)
  (if (listp obj)
      (alexandria:if-let ((subclass-specs
			   (arrow-macros:->>
			    class
			    (closer-mop:class-direct-subclasses)
			    (mapcar #λ(gethash _0 *class-specs*))))
			  (c-spec (gethash class *class-specs*)))
	  (if (and (can-interpret-as-class obj c-spec)
		   (eq :perfect (can-interpret-as-class obj c-spec)))
	      class
	      (loop for child-class in (closer-mop:class-direct-subclasses class)
		 if (eq child-class (super-type-check obj child-class))
		 do (return-from super-type-check child-class)
		 finally (return-from super-type-check nil)))
	(if (can-interpret-as-class obj c-spec)
	    class))
      (if (closer-mop:subclassp (class-of obj) class)
	  t)))

;;; Only supports typedef of first direct superclass (todo: handle inheritance
;;; properly. oh boy)
(defun serial->slot (obj slot-spec)
  (let ((spec (slot-spec/type-def slot-spec)))
    (trivia:match spec
		  (nil
		   obj)
		  (:inherit
		   (let* ((first-super
			   (loop named lop
			      for i in
				(arrow-macros:->> slot-spec
						  (slot-spec/class-ref)
						  (closer-mop:class-direct-superclasses))
			      if (nth-value 1 (gethash i
						       *class-specs*))
			      do (return-from lop (gethash i *class-specs*))
			      finally (error "Could not find class-spec for inherited slot ~a"
					     (slot-spec/key slot-spec))))
			  (super-specs (arrow-macros:->> first-super
							 (class-spec/slot-specs)))
			  (correct-spec (loop for s in super-specs
					   if (closer-mop:slot-definition-name
					       (slot-spec/ref s))
					   return s
					   finally (error "Could not find valid slot spec for ~a"
							  (slot-spec/key slot-spec)))))
		     (serial->slot obj correct-spec)))
		  ((list :seq a)
		   (if (and (listp obj)
			    (every #λ(super-type-check _0 (find-class a))
				   obj))
		       (mapcar #λ(serial->obj _0 a)
			       obj)
		       (error "~a failed type check of def ~a"
			      obj
			      spec)))
		  ((list _ _)
		   (error "~a is not a valid type definition" spec))
		  (a
		   (if (super-type-check obj (find-class a))
		       (serial->obj obj a)
		       (error "~a failed type check of def ~a"
			      obj
			      spec))))))
(defun init-class (class-spec alist)
  (unless (class-spec/deserial? class-spec)
    (error "Class-spec ~a is not allowed to be deserialized" class-spec))
  (let* ((ref (class-spec/ref class-spec))
	 (obj (make-instance ref)))
    (loop for s in (class-spec/slot-specs class-spec)
       for slot-name = (closer-mop:slot-definition-name (slot-spec/ref s))
       for aso = (assoc (slot-spec/key s) alist :test #'equal)
       do (if aso
	      (setf (slot-value obj slot-name)
		    (serial->slot (cdr aso) s))
	      (error "Tried to serialize alist ~a into class ~a, but slot ~a was not found"
		     alist
		     ref
		     slot-name)))
    obj))

(defun serial->obj (alist class-sym)
  (multiple-value-bind (val found) (gethash (find-class class-sym)
					    *class-specs*)
    (declare (ignore val))
    (if found
	(alexandria:if-let ((c (super-type-check alist (find-class class-sym))))
	    (init-class (gethash c *class-specs*) alist)
	  (error "Could not type check class ~a" class-sym))
	(error "Could not find class ~a" class-sym))))

;;; todo: allow inheritance of key
(defun make-slot-spec (class slot-name key &optional type)
  (make-instance 'slot-spec
		 :ref (loop for s in (closer-mop:class-slots class)
			 if (eq (closer-mop:slot-definition-name s)
				slot-name)
			 return s
			 finally (error "No slot found for ~a in class ~a"
					slot-name
					class))
		 :key key
		 :type-def type
		 :class-ref class))

(defun make-class-spec (class-sym spec &key (deserial? t))
  (let* ((c (find-class class-sym))
	 (ses (loop for s in spec
		 collect (apply #'make-slot-spec c s))))
    (setf (gethash c *class-specs*)
	  (make-instance 'class-spec :ref c :slot-specs ses :deserial deserial?))))
