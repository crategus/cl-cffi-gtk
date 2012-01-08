;;; ----------------------------------------------------------------------------
;;; gobject.utils.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; ----------------------------------------------------------------------------
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

;; Get the definition of a GType

(defun get-g-type-definition (type &optional lisp-name-package)
  (maybe-call-type-init type)
  (cond
    ((g-type-is-a type (gtype +g-type-enum+))
     (get-g-enum-definition type lisp-name-package))
    ((g-type-is-a type (gtype +g-type-flags+))
     (get-g-flags-definition type lisp-name-package))
    ((g-type-is-a type (gtype +g-type-interface+))
     (get-g-interface-definition type lisp-name-package))
    ((g-type-is-a type (gtype +g-type-object+))
     (get-g-class-definition type lisp-name-package))
    (t
     (error "Do not know how to automatically generate type definition for ~A type ~A"
            (gtype-name (g-type-fundamental type))
            (or (ignore-errors (gtype-name (gtype type)))
                type)))))

;;; ----------------------------------------------------------------------------

(defun maybe-call-type-init (type)
  (when (and (stringp type)
             (null (gtype type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name)
                                 ()
                                 :int)))))

(defun probable-type-init-name (type-name)
  (with-output-to-string (stream)
    (iter (for c in-string type-name)
          (for prev-c previous c)
          (when (and (not (first-iteration-p))
                     (upper-case-p c)
                     (not (upper-case-p prev-c))
                     (not (char= prev-c #\_)))
            (write-char #\_ stream))
          (write-char (char-downcase c) stream))
    (write-string "_get_type" stream)))

;;; ----------------------------------------------------------------------------

;; Get the definition of a GEnum type

(defun get-g-enum-definition (type &optional lisp-name-package)
  (when (and (stringp type)
             (null (gtype type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name)
                                 ()
                                 :int))))
  (when *generated-types*
    (setf (gethash (gtype-name (gtype type)) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package
                                  *lisp-name-package* *package*))
         (g-type (gtype type))
         (g-name (gtype-name g-type))
         (name (g-name->name g-name))
         (items (get-enum-items g-type))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-enum ,g-name ,name
         (:export t
                  ,@(when (foreign-symbol-pointer probable-type-initializer)
                          (list :type-initializer
                                probable-type-initializer)))
       ,@(mapcar #'enum-value->definition items))))

(defun enum-value->definition (enum-value)
  (let ((value-name (intern (lispify-name (enum-item-nick enum-value))
                            (find-package :keyword)))
        (numeric-value (enum-item-value enum-value)))
    `(,value-name ,numeric-value)))

;;; ----------------------------------------------------------------------------

;; Get the definition of a GFlags type

(defun get-g-flags-definition (type &optional lisp-name-package)
  (when (and (stringp type) (null (gtype type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name)
                                 ()
                                 :int))))
  (when *generated-types*
    (setf (gethash (gtype-name (gtype type)) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package
                                  *lisp-name-package* *package*))
         (g-type (gtype type))
         (g-name (gtype-name g-type))
         (name (g-name->name g-name))
         (items (get-flags-items g-type))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-flags ,g-name ,name
         (:export t
                  ,@(when (foreign-symbol-pointer probable-type-initializer)
                          (list :type-initializer
                                probable-type-initializer)))
       ,@(mapcar #'flags-value->definition items))))

(defun flags-value->definition (flags-value)
  (let ((value-name (intern (lispify-name (flags-item-nick flags-value))
                            (find-package :keyword)))
        (numeric-value (flags-item-value flags-value)))
    `(,value-name ,numeric-value)))

;;; ----------------------------------------------------------------------------

;; Helper functions for getting the definitions

(defun property->property-definition (class-name property)
  (let ((name (g-name->name (g-class-property-definition-name property)))
        (accessor-name (accessor-name class-name
                                      (g-class-property-definition-name property)))
        (g-name (g-class-property-definition-name property))
        (type (gtype-name (g-class-property-definition-type property)))
        (readable (g-class-property-definition-readable property))
        (writable (and (g-class-property-definition-writable property)
                       (not (g-class-property-definition-constructor-only property)))))
    `(,name ,accessor-name ,g-name ,type ,readable ,writable)))

(defun starts-with (name prefix)
  (and prefix
       (> (length name) (length prefix))
       (string= (subseq name 0 (length prefix)) prefix)))

(defun g-name->name (name)
  (or (second (assoc name *lisp-name-exceptions* :test 'equal))
      (intern (string-upcase (lispify-name name)) *lisp-name-package*)))

;;; ----------------------------------------------------------------------------

;; Get the defintion of a GInterface type

;;; ----------------------------------------------------------------------------

;; A Helper method

(defclass print-readtime-condition ()
  ((condition :initarg :condition)))

(defmethod print-object ((o print-readtime-condition) stream)
  (format stream "#~A" (slot-value o 'condition)))

;;; ----------------------------------------------------------------------------

(defun get-g-interface-definition (interface &optional lisp-name-package)
  (when (and (stringp interface) (null (ignore-errors (gtype interface))))
    (let ((type-init-name (probable-type-init-name interface)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name)
                                 ()
                                 :int))))
  (when *generated-types*
    (setf (gethash (gtype-name (gtype interface)) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package
                                  *lisp-name-package* *package*))
         (type (gtype interface))
         (g-name (gtype-name type))
         (name (g-name->name g-name))
         (properties (sort (copy-list (interface-properties type))
                           #'string< :key #'g-class-property-definition-name))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-interface ,g-name ,name
         (:export t
                  ,@(when (foreign-symbol-pointer probable-type-initializer)
                          `(:type-initializer ,probable-type-initializer)))
       ,@(append (mapcar (lambda (property)
                           (property->property-definition name property))
                         properties)
                 (mapcan (lambda (property-definition)
                           (if (eq :cond (car property-definition))
                               (list (make-instance 'print-readtime-condition
                                                    :condition
                                                    (cadr property-definition))
                                     (cddr property-definition))
                               (list property-definition)))
                         (cdr (find g-name *additional-properties*
                                    :key 'car
                                    :test 'string=)))))))

;; Returns a list of properties of GObject interface g-type. Each property is
;; described by an object of type g-class-property-definition. g-type is an
;; integer or a string specifying the GType

(defun interface-properties (g-type)
  (assert (g-type-is-a g-type +g-type-interface+))
  (with-unwind (g-iface (g-type-default-interface-ref g-type)
                        g-type-default-interface-unref)
    (with-foreign-object (n-properties :uint)
      (with-unwind (params (g-object-interface-list-properties
                            g-iface n-properties)
                           g-free)
        (loop
           for i from 0 below (mem-ref n-properties :uint)
           for param = (mem-aref params :pointer i)
           collect (parse-g-param-spec param))))))

;;; ----------------------------------------------------------------------------

;; Get the defintion of a GClass type

(defun get-g-class-definition (type &optional lisp-name-package)
  (when (and (stringp type)
             (null (ignore-errors (gtype type))))
    (let ((type-init-name (probable-type-init-name type)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name)
                                 ()
                                 :int))))
  (when *generated-types*
    (setf (gethash (gtype-name (gtype type)) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package
                                  *lisp-name-package*
                                  *package*))
         (g-type (gtype type))
         (g-name (gtype-name g-type))
         (name (g-name->name g-name))
         (superclass-g-type (g-type-parent g-type))
         (superclass-name (g-name->name (gtype-name superclass-g-type)))
         (interfaces (g-type-interfaces g-type))
         (properties (class-properties g-type))
         (type-init-name (probable-type-init-name g-name))
         (own-properties
          (sort (copy-list (remove g-type
                                   properties
                                   :key #'g-class-property-definition-owner-type
                                   :test-not #'g-type=))
                #'string< :key #'g-class-property-definition-name)))
    `(define-g-object-class ,g-name ,name 
         (:superclass ,superclass-name
          :export t
          :interfaces
           (,@(sort (mapcar #'gtype-name interfaces) 'string<))
           ,@(when (and (foreign-symbol-pointer type-init-name)
                        (not (null-pointer-p (foreign-symbol-pointer type-init-name))))
               `(:type-initializer ,type-init-name)))
       (,@(mapcar (lambda (property)
                    (property->property-definition name property))
                  own-properties)
          ,@(mapcan (lambda (property-definition)
                      (if (eq :cond (car property-definition))
                          (list (make-instance 'print-readtime-condition
                                               :condition
                                               (cadr property-definition))
                                (cddr property-definition))
                          (list property-definition)))
                    (cdr (find g-name *additional-properties*
                               :key 'car :test 'string=)))))))

;; Returns a list of properties of GObject class @code{g-type}. Each property
;; is described by an object of type g-class-property-definition. g-type is an
;; integer or a string specifying the GType

(defun class-properties (g-type)
  (assert (g-type-is-a g-type +g-type-object+))
  (with-unwind (g-class (g-type-class-ref g-type) g-type-class-unref)
    (with-foreign-object (n-properties :uint)
      (with-unwind (params (g-object-class-list-properties g-class n-properties)
                           g-free)
        (loop
           for i from 0 below (mem-ref n-properties :uint)
           for param = (mem-aref params :pointer i)
           collect (parse-g-param-spec param))))))

;;; ----------------------------------------------------------------------------

(defvar *referenced-types*)

(defun generate-types-hierarchy-to-file (file root-type
                                              &key include-referenced
                                              prefix package exceptions
                                              prologue interfaces enums flags
                                              objects exclusions
                                              additional-properties)
  (if (not (streamp file))
      (with-open-file (stream file :direction :output :if-exists :supersede)
        (generate-types-hierarchy-to-file stream root-type
                                          :prefix prefix
                                          :package package
                                          :exceptions exceptions
                                          :prologue prologue
                                          :include-referenced include-referenced
                                          :interfaces interfaces
                                          :enums enums
                                          :flags flags
                                          :objects objects
                                          :exclusions exclusions
                                          :additional-properties
                                          additional-properties))
      (let* ((*generation-exclusions* (mapcar #'gtype exclusions))
             (*lisp-name-package* (or package *package*))
             (*package* *lisp-name-package*)
             (*strip-prefix* (or prefix ""))
             (*lisp-name-exceptions* exceptions)
             (*print-case* :downcase)
             (*additional-properties* additional-properties)
             (*generated-types* (make-hash-table :test 'equalp))
             (referenced-types (and include-referenced
                                    (filter-types-by-prefix
                                     (get-referenced-types root-type)
                                     prefix))))
        (setf exclusions (mapcar #'gtype exclusions))
        (when prologue
          (write-string prologue file)
          (terpri file))
        (when include-referenced
          (loop
            for interface in interfaces
            do
            (loop
              for referenced-type in (get-shallow-referenced-types interface)
              do (pushnew referenced-type referenced-types :test 'g-type=)))
          (loop
            for object in objects
            do
            (loop
              for referenced-type in (get-shallow-referenced-types object)
              do (pushnew referenced-type referenced-types :test 'g-type=)))
          (loop
             for enum-type in (filter-types-by-fund-type
                               referenced-types "GEnum")
             for def = (get-g-enum-definition enum-type)
             unless (member enum-type exclusions :test 'g-type=)
             do (format file "~S~%~%" def))
            
          (loop
             for flags-type in (filter-types-by-fund-type
                                referenced-types "GFlags")
             for def = (get-g-flags-definition flags-type)
             unless (member flags-type exclusions :test 'g-type=)
             do (format file "~S~%~%" def)))
        (loop
           with auto-enums = (and include-referenced
                                  (filter-types-by-fund-type
                                   referenced-types "GEnum"))
           for enum in enums
           for def = (get-g-enum-definition enum)
           unless (find enum auto-enums :test 'g-type=)
           do (format file "~S~%~%" def))
        (loop
           with auto-flags = (and include-referenced
                                  (filter-types-by-fund-type
                                   referenced-types "GFlags"))
           for flags-type in flags
           for def = (get-g-flags-definition flags-type)
           unless (find flags-type auto-flags :test 'g-type=)
           do (format file "~S~%~%" def))
        (loop
           for interface in interfaces
           for def = (get-g-interface-definition interface)
           do (format file "~S~%~%" def))
        (loop
           for def in (get-g-class-definitions-for-root root-type)
           do (format file "~S~%~%" def))
        (iter (for object in objects)
              (unless (gethash (gtype-name (gtype object)) *generated-types*)
                (for def = (get-g-class-definition object))
                (format file "~S~%~%" def))))))

;;; ----------------------------------------------------------------------------

;; Helper functions for generate-types-hierarchy-to-file

(defun get-g-class-definitions-for-root (type)
  (setf type (gtype type))
  (get-g-class-definitions-for-root-1 type))

(defun get-g-class-definitions-for-root-1 (type)
  (unless (member (gtype type) *generation-exclusions* :test 'g-type=)
    (iter (when (first-iteration-p)
            (unless (and *generated-types*
                         (gethash (gtype-name (gtype type)) *generated-types*))
              (appending (list (get-g-class-definition type)))))
          (for child-type in (sort (copy-list (g-type-children type))
                                   #'string< :key #'gtype-name))
          (appending (get-g-class-definitions-for-root-1 child-type)))))

;;; ----------------------------------------------------------------------------

(defun get-referenced-types-1 (type)
  (setf type (gtype type))
  (loop
     for property-type in (sort (copy-list (get-shallow-referenced-types type))
                                #'string> :key #'gtype-name)
     do (pushnew property-type *referenced-types* :test 'g-type=))
  (loop
     for type in (sort (copy-list (g-type-children type))
                       #'string< :key #'gtype-name)
     do (get-referenced-types-1 type)))

(defun get-referenced-types (root-type)
  (let (*referenced-types*)
    (get-referenced-types-1 (gtype root-type))
    *referenced-types*))

;;; ----------------------------------------------------------------------------

(defun filter-types-by-prefix (types prefix)
  (remove-if-not
   (lambda (type)
     (starts-with (gtype-name (gtype type)) prefix))
   types))

(defun filter-types-by-fund-type (types fund-type)
  (setf fund-type (gtype fund-type))
  (remove-if-not
   (lambda (type)
     (equal (g-type-fundamental (gtype type)) fund-type))
   types))

;;; ----------------------------------------------------------------------------

(defun get-shallow-referenced-types (type)
  (setf type (gtype type))
  (remove-duplicates (sort (loop
                             for property in (class-or-interface-properties type)
                             when (g-type= type (g-class-property-definition-owner-type property))
                             collect (g-class-property-definition-type property))
                           #'string<
                           :key #'gtype-name)
                     :test 'equal))

(defun class-or-interface-properties (type)
  (setf type (gtype type))
  (cond 
    ((g-type= (g-type-fundamental type) (gtype +g-type-object+))
     (class-properties type))
    ((g-type= (g-type-fundamental type) (gtype +g-type-interface+))
     (interface-properties type))))

;;; --- End of file gobject.utils.lisp -----------------------------------------
