;;; ----------------------------------------------------------------------------
;;; gobject.gobject-class.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

(defclass gobject-class (standard-class)
  ((g-type-name :initform nil
                :accessor gobject-class-g-type-name)
   (direct-g-type-name :initform nil
                       :initarg :g-type-name
                       :accessor gobject-class-direct-g-type-name)
   (g-type-initializer :initform nil
                       :initarg :g-type-initializer
                       :reader gobject-class-g-type-initializer)
   (interface-p :initform nil
                :initarg :g-interface-p
                :reader gobject-class-interface-p))
  (:documentation "Metaclass for GObject-based classes."))

(export 'gobject-class)
(export 'gobject-class-g-type-name)
(export 'gobject-class-direct-g-type-name)
(export 'gobject-class-g-type-initializer)
(export 'gobject-class-interface-p)

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :around ((class gobject-class) &rest initargs)
  (apply #'call-next-method class
         (compute-new-initargs-for-metaclass initargs 'g-object)))

(defmethod reinitialize-instance :around ((class gobject-class)
                                          &rest initargs
                                          &key (direct-superclasses nil d-s-p)
                                          &allow-other-keys)
  (declare (ignore direct-superclasses))
  (if d-s-p
      (apply #'call-next-method class
             (compute-new-initargs-for-metaclass initargs 'g-object))
      (call-next-method)))

;;; ----------------------------------------------------------------------------

(defun compute-new-initargs-for-metaclass (initargs base-class)
  (if (initargs-have-base-in-superclass initargs base-class)
      initargs
      (append (filter-from-initargs initargs :direct-superclasses)
          (list :direct-superclasses
            (append (getf initargs :direct-superclasses)
                (list (find-class base-class)))))))

(defun initargs-have-base-in-superclass (initargs base-class)
  (let ((d-s (getf initargs :direct-superclasses)))
    (loop
      for class in d-s
      thereis (subtypep class base-class))))

(defun filter-from-initargs (initargs removed-key)
  (loop
    for (key value) on initargs by #'cddr
    unless (eq key removed-key)
    collect key and collect value))

(defun filter-initargs-by-class (class initargs)
  (iter (with slots = (class-slots class))
        (for (arg-name arg-value) on initargs by #'cddr)
        (for slot = (find arg-name slots :key #'slot-definition-initargs
                                         :test 'member))
        (unless (and slot (typep slot 'gobject-effective-slot-definition))
          (nconcing (list arg-name arg-value)))))

;;; ----------------------------------------------------------------------------

(defun initialize-gobject-class-g-type (class)
  (if (gobject-class-g-type-initializer class)
      (let* ((initializer-fn-ptr (foreign-symbol-pointer
                                   (gobject-class-g-type-initializer class)))
             (type (when initializer-fn-ptr
                      (foreign-funcall-pointer initializer-fn-ptr nil g-type))))
        (if (null initializer-fn-ptr)
            (warn "Type initializer for class '~A' (GType '~A') is invalid: ~
                   foreign symbol '~A'"
                  (gobject-class-direct-g-type-name class)
                  (class-name class)
                  (gobject-class-g-type-initializer class))
            (progn
              (when (eq (gtype +g-type-invalid+) type)
                (warn "Declared GType name '~A' for class '~A' is invalid ~
                      ('~A' returned 0)"
                      (gobject-class-direct-g-type-name class)
                      (class-name class)
                      (gobject-class-g-type-initializer class)))
              (unless (eq (gtype (gobject-class-direct-g-type-name class)) type)
                (warn "Declared GType name '~A' for class '~A' does not match ~
                      actual GType name '~A'"
                      (gobject-class-direct-g-type-name class)
                      (class-name class)
                      (gtype-name type))))))
      (when (zerop (gtype-id (gtype (gobject-class-direct-g-type-name class))))
        (warn "Declared GType name '~A' for class '~A' is invalid ~
              (g_type_name returned 0)"
              (gobject-class-direct-g-type-name class)
              (class-name class)))))

;;; ----------------------------------------------------------------------------

(defmethod finalize-inheritance :after ((class gobject-class))
  (iter (for superclass in (class-direct-superclasses class))
        (unless (class-finalized-p superclass)
                (finalize-inheritance superclass)))
  (setf (gobject-class-g-type-name class)
        (or (gobject-class-direct-g-type-name class)
            (let ((gobject-superclass
                   (iter (for superclass in (class-direct-superclasses class))
                         (finding superclass
                                  such-that (typep superclass
                                                    'gobject-class)))))
              (assert gobject-superclass)
              (gobject-class-g-type-name gobject-superclass)))))

;;; ----------------------------------------------------------------------------

(defclass gobject-direct-slot-definition (standard-direct-slot-definition)
  ((g-property-type :initform nil
                    :initarg :g-property-type
                    :reader gobject-direct-slot-definition-g-property-type)))

(defclass gobject-effective-slot-definition (standard-effective-slot-definition)
  ((g-property-type :initform nil
                    :initarg :g-property-type
                    :accessor
                    gobject-effective-slot-definition-g-property-type)))

(defclass gobject-property-direct-slot-definition
          (gobject-direct-slot-definition)
  ((g-property-name :initform nil
                    :initarg :g-property-name
                    :reader
                    gobject-property-direct-slot-definition-g-property-name)))

(defclass gobject-property-effective-slot-definition
          (gobject-effective-slot-definition)
  ((g-property-name :initform nil
                    :initarg :g-property-name
                    :accessor
                    gobject-property-effective-slot-definition-g-property-name)))

(defclass gobject-fn-direct-slot-definition (gobject-direct-slot-definition)
  ((g-getter-name :initform nil
                  :initarg :g-getter
                  :reader gobject-fn-direct-slot-definition-g-getter-name)
   (g-setter-name :initform nil
                  :initarg :g-setter
                  :reader gobject-fn-direct-slot-definition-g-setter-name)))

(defclass gobject-fn-effective-slot-definition
          (gobject-effective-slot-definition)
  ((g-getter-name :initform nil
                  :initarg :g-getter
                  :accessor gobject-fn-effective-slot-definition-g-getter-name)
   (g-setter-name :initform nil
                  :initarg :g-setter
                  :accessor gobject-fn-effective-slot-definition-g-setter-name)
   (g-getter-fn :initform nil
                :accessor gobject-fn-effective-slot-definition-g-getter-fn)
   (g-setter-fn :initform nil
                :accessor gobject-fn-effective-slot-definition-g-setter-fn)))

(defmethod validate-superclass ((class gobject-class)
                                (superclass standard-class))
  t)

(defmethod direct-slot-definition-class ((class gobject-class) &rest initargs
                                         &key allocation &allow-other-keys)
  (declare (ignore initargs))
  (case allocation
    (:gobject-property (find-class 'gobject-property-direct-slot-definition))
    (:gobject-fn (find-class 'gobject-fn-direct-slot-definition))
    (otherwise (call-next-method))))

(defvar *e-s-d* nil)

(defmethod effective-slot-definition-class ((class gobject-class)
                                            &rest initargs)
  (declare (ignore initargs))
  (or *e-s-d* (call-next-method)))

(defmethod compute-effective-slot-definition ((class gobject-class)
                                              name
                                              direct-slots)
  (let ((effective-slot
         (let ((*e-s-d*
                (loop
                  for slot in direct-slots
                  when (typep slot 'gobject-direct-slot-definition)
                  return (etypecase slot
                         (gobject-property-direct-slot-definition
                          (find-class 'gobject-property-effective-slot-definition))
                         (gobject-fn-direct-slot-definition
                          (find-class 'gobject-fn-effective-slot-definition))))))
           (call-next-method))))
    (when (typep effective-slot 'gobject-effective-slot-definition)
      (let ((allocation
             (loop
               for direct-slot in direct-slots
               when (slot-definition-allocation direct-slot)
               return (slot-definition-allocation direct-slot)))
            (property-name
             (loop
               for direct-slot in direct-slots
               when (and (typep direct-slot
                                'gobject-property-direct-slot-definition)
                         (gobject-property-direct-slot-definition-g-property-name direct-slot))
               return (gobject-property-direct-slot-definition-g-property-name direct-slot)))
            (property-type
             (loop
               for direct-slot in direct-slots
               when (gobject-direct-slot-definition-g-property-type direct-slot)
               return (gobject-direct-slot-definition-g-property-type direct-slot)))
            (property-getter
             (loop
               for direct-slot in direct-slots
               when (and (typep direct-slot 'gobject-fn-direct-slot-definition)
                         (gobject-fn-direct-slot-definition-g-getter-name direct-slot))
               return (gobject-fn-direct-slot-definition-g-getter-name direct-slot)))
            (property-setter
             (loop
               for direct-slot in direct-slots
               when (and (typep direct-slot 'gobject-fn-direct-slot-definition)
                         (gobject-fn-direct-slot-definition-g-setter-name direct-slot))
               return (gobject-fn-direct-slot-definition-g-setter-name direct-slot))))
        (setf (gobject-effective-slot-definition-g-property-type effective-slot)
              (gobject-effective-slot-definition-g-property-type effective-slot))
        (ecase allocation
          (:gobject-property
           (assert property-name
                   nil
                   "G-PROPERTY-NAME for slot ~A on class ~A must be specified"
                   name
                   (class-name class))
           (setf (gobject-property-effective-slot-definition-g-property-name effective-slot)
                 property-name))
          (:gobject-fn
           (assert (or property-getter property-setter)
                   nil
                   "At least one of G-PROPERTY-GETTER or G-PROPERTY-SETTER ~
                    for slot ~A on class ~A must be specified"
                   name
                   (class-name class))
           (when (or (and property-getter
                          (stringp property-getter))
                     (and property-setter
                          (stringp property-setter)))
             (assert property-type
                     nil
                     "G-PROPERTY-TYPE for slot ~A on class ~A must be ~
                      specified because at least one of accessor is specified ~
                      as a foreign function"
                     name
                     (class-name class)))
           (setf (gobject-fn-effective-slot-definition-g-getter-name effective-slot)
                 property-getter
                 (gobject-fn-effective-slot-definition-g-setter-name effective-slot)
                 property-setter
                 (gobject-fn-effective-slot-definition-g-getter-fn effective-slot)
                 (and property-getter
                      (if (stringp property-getter)
                          (compile nil
                                   (if (foreign-symbol-pointer property-getter)
                                       `(lambda (object)
                                          (foreign-funcall ,property-getter
                                                           g-object object
                                                           ,property-type))
                                       `(lambda (object)
                                          (declare (ignore object))
                                          (error "Property getter ~A is not available"
                                                 ,property-getter))))
                          property-getter))
                 (gobject-fn-effective-slot-definition-g-setter-fn effective-slot)
                 (and property-setter
                      (if (stringp property-setter)
                          (compile nil
                                   (if (foreign-symbol-pointer property-setter)
                                       `(lambda (object new-value)
                                          (foreign-funcall ,property-setter
                                                           g-object
                                                           object
                                                           ,property-type
                                                           new-value
                                                           :void))
                                       `(lambda (object)
                                          (declare (ignore object))
                                          (error "Property setter ~A is not avaiable"
                                                 ,property-setter))))
                                                 property-setter)))))))
    effective-slot))

;;; ----------------------------------------------------------------------------

(defmethod slot-boundp-using-class
                             ((class gobject-class) object
                              (slot gobject-property-effective-slot-definition))
  (handler-case
    (and (slot-boundp object 'pointer)
         (pointer object)
         (progn
           (class-property-type
               (g-type-from-instance (pointer object))
               (gobject-property-effective-slot-definition-g-property-name slot)
               :assert-readable t)
             t))
    (property-unreadable-error () nil)))

(defmethod slot-boundp-using-class ((class gobject-class) object
                                    (slot gobject-fn-effective-slot-definition))
  (and (slot-boundp object 'pointer)
       (pointer object)
       (not (null (gobject-fn-effective-slot-definition-g-getter-fn slot)))))

(defmethod slot-boundp-using-class ((class gobject-class) object
                                    (slot gobject-effective-slot-definition))
  (slot-boundp object 'pointer))


(defmethod slot-value-using-class ((class gobject-class) object
                                   (slot gobject-property-effective-slot-definition))
  (g-object-get-property
               (pointer object)
               (gobject-property-effective-slot-definition-g-property-name slot)
               (gobject-effective-slot-definition-g-property-type slot)))

(defmethod (setf slot-value-using-class)
           (new-value (class gobject-class)
            object (slot gobject-property-effective-slot-definition))
  (g-object-set-property (pointer object)
                         (gobject-property-effective-slot-definition-g-property-name slot)
                         new-value
                         (gobject-effective-slot-definition-g-property-type slot))
  new-value)

(defmethod slot-value-using-class ((class gobject-class) object
                                   (slot gobject-fn-effective-slot-definition))
  (let ((fn (gobject-fn-effective-slot-definition-g-getter-fn slot)))
    (funcall fn object)))

(defmethod (setf slot-value-using-class)
           (new-value (class gobject-class)
            object (slot gobject-fn-effective-slot-definition))
  (funcall (gobject-fn-effective-slot-definition-g-setter-fn slot)
           object
           new-value)
  new-value)

(defmethod slot-makunbound-using-class ((class gobject-class) object
                                        (slot gobject-effective-slot-definition))
  (declare (ignore object))
  nil)

;;; --- End of file gobject.gobject-class.lisp ---------------------------------
