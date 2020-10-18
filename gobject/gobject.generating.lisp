;;; ----------------------------------------------------------------------------
;;; gobject.generating.lisp
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

(defvar *lisp-name-exceptions* nil)
(defvar *known-interfaces* (make-hash-table :test 'equal))
(defvar *additional-properties* nil)

;;; ----------------------------------------------------------------------------

(defstruct property
  name
  accessor-name
  readable
  writable)

(defstruct (gobject-property (:include property))
  gname
  type)

(defstruct (cffi-property (:include property))
  type
  reader
  writer)

;;; ----------------------------------------------------------------------------

(defmethod make-load-form ((object gobject-property) &optional env)
  (declare (ignore env))
  `(make-gobject-property :name ',(property-name object)
                          :accessor-name ',(property-accessor-name object)
                          :readable ',(property-readable object)
                          :writable ',(property-writable object)
                          :gname ',(gobject-property-gname object)
                          :type ',(gobject-property-type object)))

(defmethod make-load-form ((object cffi-property) &optional env)
  (declare (ignore env))
  `(make-cffi-property :name ',(property-name object)
                       :accessor-name ',(property-accessor-name object)
                       :readable ',(property-readable object)
                       :writable ',(property-writable object)
                       :type ',(cffi-property-type object)
                       :reader ',(cffi-property-reader object)
                       :writer ',(cffi-property-writer object)))

;;; ----------------------------------------------------------------------------

;; Needed for the macros define-g-object-class and define-g-interface

(defun parse-property (spec)
  (cond
    ((eq (first spec) :cffi)
     (parse-cffi-property (rest spec)))
    (t
     (parse-gobject-property spec))))

(defun parse-gobject-property (spec)
  (destructuring-bind (name accessor-name gname type readable writable) spec
    (make-gobject-property :name name
                           :accessor-name accessor-name
                           :gname gname
                           :type type
                           :readable readable
                           :writable writable)))

(defun parse-cffi-property (spec)
  (destructuring-bind (name accessor-name type reader writer) spec
    (make-cffi-property :name name
                        :accessor-name accessor-name
                        :type type
                        :reader reader
                        :writer writer
                        :readable (not (null reader))
                        :writable (not (null writer)))))

;;; ----------------------------------------------------------------------------

;; Are these functions in use?

(defun property->method-arg (property)
  (when (or (gobject-property-p property)
            (and (cffi-property-p property)
                 (property-writable property)))
    (let ((name (property-name property)))
      `(,name nil ,(name->supplied-p name)))))

(defun gobject-property->arg-push (property)
  (assert (typep property 'gobject-property))
  (with-slots (name type gname) property
    `(when ,(name->supplied-p name)
       (push ,gname arg-names)
       (push ,type arg-types)
       (push ,name arg-values))))

(defun cffi-property->initarg (property)
  (assert (typep property 'cffi-property))
  (when (property-writable property)
    (with-slots (accessor-name name type writer) property
      `(when ,(name->supplied-p name)
         (setf (,accessor-name object) ,name)))))

(defun name->supplied-p (name)
  (make-symbol (format nil "~A-SUPPLIED-P" (symbol-name name))))

;;; ----------------------------------------------------------------------------

;; Generate the name of a slot accessor

(defvar *strip-prefix* "")

(defun accessor-name (class-name property-name)
  (intern (format nil "~A-~A"
                      (symbol-name class-name)
                      (lispify-name property-name))
          *lisp-name-package*))

(defun lispify-name (name)
  (with-output-to-string (stream)
    (loop for c across (strip-start name *strip-prefix*)
       for firstp = t then nil
       do (when (and (not firstp) (upper-case-p c)) (write-char #\- stream))
          do (write-char (char-upcase c) stream))))

(defun strip-start (name prefix)
  (if (starts-with name prefix)
      (subseq name (length prefix))
      name))

(defun starts-with (name prefix)
  (and prefix
       (> (length name) (length prefix))
       (string= (subseq name 0 (length prefix)) prefix)))

;;; ----------------------------------------------------------------------------

(defgeneric property->reader (class property))

(defmethod property->reader (class (property gobject-property))
  (with-slots (accessor-name type gname) property
   `(defmethod ,accessor-name ((object ,class))
      (g-object-property object ,gname ,type))))

(defmethod property->reader (class (property cffi-property))
  (with-slots (accessor-name type reader) property
    (etypecase reader
      (string `(defmethod ,accessor-name ((object ,class))
                 (foreign-funcall ,reader g-object object ,type)))
      (symbol `(defmethod ,accessor-name ((object ,class))
                 (funcall ',reader object))))))

;;; ----------------------------------------------------------------------------

(defgeneric property->writer (class property))

(defmethod property->writer (class (property gobject-property))
  (with-slots (accessor-name type gname) property
    `(defmethod (setf ,accessor-name) (new-value (object ,class))
       (setf (g-object-property object ,gname ,type) new-value))))

(defmethod property->writer (class (property cffi-property))
  (with-slots (accessor-name type writer) property
    (etypecase writer
      (string `(defmethod (setf ,accessor-name) (new-value (object ,class))
                 (foreign-funcall ,writer g-object object ,type new-value :void)
                 new-value))
      (symbol `(defmethod (setf ,accessor-name) (new-value (object ,class))
                 (funcall ',writer object new-value)
                 new-value)))))

;;; ----------------------------------------------------------------------------

(defun property->accessors (class property export)
  (append (when (property-readable property)
            (list (property->reader class property)))
          (when (property-writable property)
            (list (property->writer class property)))
          (when export
            (list `(export ',(property-accessor-name property)
                           (find-package
                             ,(package-name
                                (symbol-package
                                 (property-accessor-name property)))))))))

;;; ----------------------------------------------------------------------------

(defun interface->lisp-class-name (interface)
  (etypecase interface
    (symbol interface)
    (string (or (gethash interface *known-interfaces*)
                (error "Unknown interface ~A" interface)))))

;;; ----------------------------------------------------------------------------

(defun meta-property->slot (class-name property)
  `(,(property-name property)
     :allocation ,(if (gobject-property-p property)
                      :gobject-property
                      :gobject-fn)
     :g-property-type ,(if (gobject-property-p property)
                           (gobject-property-type property)
                           (cffi-property-type property))
     :accessor ,(intern (format nil "~A-~A"
                                (symbol-name class-name)
                                (property-name property))
                        (symbol-package class-name))
     ,@(when (if (gobject-property-p property)
                 t
                 (not (null (cffi-property-writer property))))
             `(:initarg
               ,(intern (string-upcase (property-name property))
                        (find-package :keyword))))
     ,@(if (gobject-property-p property)
           `(:g-property-name ,(gobject-property-gname property))
           `(:g-getter ,(cffi-property-reader property)
              :g-setter ,(cffi-property-writer property)))))

;;; ----------------------------------------------------------------------------

(defmacro define-g-object-class (g-type-name name
                                 (&key (superclass 'g-object)
                                       (export t)
                                       interfaces
                                       type-initializer)
                                 (&rest properties))
  (setf properties (mapcar #'parse-property properties))
  `(progn
     (defclass ,name (,@(when (and superclass
                                   (not (eq superclass 'g-object)))
                          (list superclass))
                      ,@(mapcar #'interface->lisp-class-name interfaces))
       (,@(mapcar (lambda (property)
                     (meta-property->slot name property))
                   properties))
       (:metaclass gobject-class)
       (:g-type-name . ,g-type-name)
       ,@(when type-initializer
           (list `(:g-type-initializer . ,type-initializer))))
     ,@(when export
         (cons `(export ',name
                         (find-package
                           ,(package-name (symbol-package name))))
               (mapcar (lambda (property)
                         `(export ',(intern (format nil "~A-~A"
                                                       (symbol-name name)
                                                       (property-name property))
                                              (symbol-package name))
                                   (find-package
                                     ,(package-name (symbol-package name)))))
                        properties)))))

;;; ----------------------------------------------------------------------------

(defmacro define-g-interface (g-type-name name (&key (export t)
                                                     type-initializer)
                                                &body properties)
  (setf properties (mapcar #'parse-property properties))
  `(progn
     (defclass ,name ()
       (,@(mapcar (lambda (property) (meta-property->slot name property))
                  properties))
       (:metaclass gobject-class)
       (:g-type-name . ,g-type-name)
       (:g-interface-p . t)
       ,@(when type-initializer
               (list `(:g-type-initializer . ,type-initializer))))
     ,@(when export
         (cons `(export ',name
                        (find-package ,(package-name (symbol-package name))))
               (mapcar (lambda (property)
                         `(export ',(intern (format nil "~A-~A"
                                                    (symbol-name name)
                                                    (property-name property))
                                            (symbol-package name))
                                  (find-package
                                   ,(package-name (symbol-package name)))))
                       properties)))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,g-type-name *known-interfaces*) ',name))))

;;; --- gobject.generating.lisp ------------------------------------------------
