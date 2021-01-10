;;; ----------------------------------------------------------------------------
;;; gobject.foreign-gobject-subclassing.lisp
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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

(defvar *registered-types* (make-hash-table :test 'equal))

(defstruct object-type
  name
  class
  parent
  interfaces
  properties)

;;; ----------------------------------------------------------------------------

(defun instance-init (instance class)
  (log-for :subclass
           ":subclass INSTANCE-INIT for ~A for type ~A (creating ~A)~%"
           instance
           (gtype-name (foreign-slot-value class '(:struct g-type-class) :type))
           *current-creating-object*)
  (unless (or *current-creating-object*
              *currently-making-object-p*
              (gethash (pointer-address instance) *foreign-gobjects-strong*)
              (gethash (pointer-address instance) *foreign-gobjects-weak*))
    (log-for :subclass "Proceeding with initialization...~%")
    (let* ((type (foreign-slot-value class '(:struct g-type-class) :type))
           (type-name (gtype-name type))
           (lisp-type-info (gethash type-name *registered-types*))
           (lisp-class (object-type-class lisp-type-info)))
      (make-instance lisp-class :pointer instance))))

(defcallback instance-init-cb :void ((instance :pointer) (class :pointer))
  (instance-init instance class))

;;; ----------------------------------------------------------------------------

(defun class-init (class data)
  (declare (ignore data))
  (log-for :subclass
           ":subclass CLASS-INIT for ~A~%"
           (gtype-name (g-type-from-class class)))
  (let* ((type-name (gtype-name (g-type-from-class class)))
         (lisp-type-info (gethash type-name *registered-types*))
         (lisp-class (object-type-class lisp-type-info)))
    (register-object-type type-name lisp-class))
  (setf (foreign-slot-value class '(:struct g-object-class) :get-property)
        (callback c-object-property-get)
        (foreign-slot-value class '(:struct g-object-class) :set-property)
        (callback c-object-property-set))
  (install-properties class))

(defcallback class-init-cb :void ((class :pointer) (data :pointer))
  (class-init class data))

;;; ----------------------------------------------------------------------------

(defun install-properties (class)
  (let* ((name (gtype-name (foreign-slot-value class
                                               '(:struct g-type-class) :type)))
         (lisp-type-info (gethash name *registered-types*)))
    (iter (for property in (object-type-properties lisp-type-info))
          (for param-spec = (property->param-spec property))
          (for property-id from 123) ; FIXME: ???
          (log-for :subclass
                   ":subclass INSTALL-PROPERTIES installing ~A~%"
                   property)
          (%g-object-class-install-property class property-id param-spec))))

;;; ----------------------------------------------------------------------------

(defun minimum-foreign-integer (type &optional (signed t))
  (if signed
      (- (ash 1 (1- (* 8 (foreign-type-size type)))))
      0))

(defun maximum-foreign-integer (type &optional (signed t))
  (if signed
      (1- (ash 1 (1- (* 8 (foreign-type-size type)))))
      (1- (ash 1 (* 8 (foreign-type-size type))))))

;;; ----------------------------------------------------------------------------

(defun property->param-spec (property)
  (destructuring-bind (property-name
                       property-type
                       accessor
                       property-get-fn
                       property-set-fn)
      property
    (declare (ignore accessor))
    (let ((property-g-type (gtype property-type))
          (flags (append (when property-get-fn (list :readable))
                         (when property-set-fn (list :writable)))))
      (ev-case (g-type-fundamental property-g-type)
        ((gtype +g-type-invalid+)
         (error "GValue is of invalid type ~A (~A)"
                property-g-type (gtype-name property-g-type)))
        ((gtype +g-type-none+) nil)
        ((gtype +g-type-char+)
         (g-param-spec-char property-name
                            property-name
                            property-name
                            (minimum-foreign-integer :char)
                            (maximum-foreign-integer :char)
                            0
                            flags))
        ((gtype +g-type-uchar+)
         (g-param-spec-uchar property-name
                             property-name
                             property-name
                             (minimum-foreign-integer :uchar nil)
                             (maximum-foreign-integer :uchar nil)
                             0
                             flags))
        ((gtype +g-type-boolean+)
         (g-param-spec-boolean property-name
                               property-name
                               property-name
                               nil
                               flags))
        ((gtype +g-type-int+)
         (g-param-spec-int property-name
                           property-name
                           property-name
                           (minimum-foreign-integer :int)
                           (maximum-foreign-integer :int)
                           0
                           flags))
        ((gtype +g-type-uint+)
         (g-param-spec-uint property-name
                            property-name
                            property-name
                            (minimum-foreign-integer :uint nil)
                            (maximum-foreign-integer :uint nil)
                            0
                            flags))
        ((gtype +g-type-long+)
         (g-param-spec-long property-name
                            property-name
                            property-name
                            (minimum-foreign-integer :long)
                            (maximum-foreign-integer :long)
                            0
                            flags))
        ((gtype +g-type-ulong+)
         (g-param-spec-ulong property-name
                             property-name
                             property-name
                             (minimum-foreign-integer :ulong nil)
                             (maximum-foreign-integer :ulong nil)
                             0
                             flags))
        ((gtype +g-type-int64+)
         (g-param-spec-int64 property-name
                             property-name
                             property-name
                             (minimum-foreign-integer :int64)
                             (maximum-foreign-integer :int64)
                             0
                             flags))
        ((gtype +g-type-uint64+)
         (g-param-spec-uint64 property-name
                              property-name
                              property-name
                              (minimum-foreign-integer :uint64 nil)
                              (maximum-foreign-integer :uint64 t)
                              0
                              flags))
        ((gtype +g-type-enum+)
         (g-param-spec-enum property-name
                            property-name
                            property-name
                            property-g-type
                            (enum-item-value
                              (first (get-enum-items property-g-type)))
                            flags))
        ((gtype +g-type-flags+)
         (g-param-spec-enum property-name
                            property-name
                            property-name
                            property-g-type
                            (flags-item-value
                              (first (get-flags-items property-g-type)))
                            flags))
        ((gtype +g-type-float+)
         (g-param-spec-float property-name
                             property-name
                             property-name
                             most-negative-single-float
                             most-positive-single-float
                             0.0
                             flags))
        ((gtype +g-type-double+)
         (g-param-spec-double property-name
                              property-name
                              property-name
                              most-negative-double-float
                              most-positive-double-float
                              0.0d0
                              flags))
        ((gtype +g-type-string+)
         (g-param-spec-string property-name
                              property-name
                              property-name
                              ""
                              flags))
        ((gtype +g-type-pointer+)
         (g-param-spec-pointer property-name
                               property-name
                               property-name
                               flags))
        ((gtype +g-type-boxed+)
         (g-param-spec-boxed property-name
                             property-name
                             property-name
                             property-g-type
                             flags))
;       (+g-type-param+ (parse-g-value-param gvalue))
        ((gtype +g-type-object+)
         (g-param-spec-object property-name
                              property-name
                              property-name
                              property-g-type
                              flags))
;       (+g-type-interface+ )
        (t
         (error "Unknown type: ~A (~A)"
                property-g-type (gtype-name property-g-type)))))))

;;; ----------------------------------------------------------------------------

(defun vtable-item->cstruct-item (item)
  (if (eq :skip (first item))
      (rest item)
      (list (first item) :pointer)))

(defstruct vtable-method-info
  slot-name
  name
  return-type
  args
  callback-name
  impl-call)

(defmethod make-load-form ((object vtable-method-info) &optional environment)
  (declare (ignore environment))
  `(make-vtable-method-info :slot-name ',(vtable-method-info-slot-name object)
                            :name ',(vtable-method-info-name object)
                            :return-type
                            ',(vtable-method-info-return-type object)
                            :args ',(vtable-method-info-args object)
                            :callback-name
                            ',(vtable-method-info-callback-name object)))

(defun vtable-methods (iface-name items)
  (iter (for item in items)
        (when (eq :skip (first item)) (next-iteration))
        (destructuring-bind (name (return-type &rest args) &key impl-call) item
          (for method-name = (intern (format nil "~A-~A-IMPL"
                                             (symbol-name iface-name)
                                             (symbol-name name))))
          (for callback-name = (intern (format nil "~A-~A-CALLBACK"
                                               (symbol-name iface-name)
                                               (symbol-name name))))
          (collect (make-vtable-method-info :slot-name name
                                            :name method-name
                                            :return-type return-type
                                            :args args
                                            :callback-name callback-name
                                            :impl-call impl-call)))))

(defvar *vtables* (make-hash-table :test 'equal))

(defstruct vtable-description
  type-name
  cstruct-name
  methods)

(defmacro define-vtable ((type-name name) &body items)
  (let ((cstruct-name (intern (format nil "~A-VTABLE" (symbol-name name))))
        (methods (vtable-methods name items)))
    `(progn
       (defcstruct ,cstruct-name ,@(mapcar #'vtable-item->cstruct-item items))
       (setf (gethash ,type-name *vtables*)
             (make-vtable-description :type-name ,type-name
                                      :cstruct-name ',cstruct-name
                                      :methods
                                      (list ,@(mapcar #'make-load-form methods))))
       ,@(iter (for method in methods)
               (for args =
                    (if (vtable-method-info-impl-call method)
                        (first (vtable-method-info-impl-call method))
                        (mapcar #'first (vtable-method-info-args method))))
               (collect `(defgeneric ,(vtable-method-info-name method) (,@args)))
               (collect `(glib-defcallback
                            ,(vtable-method-info-callback-name method)
                            ,(vtable-method-info-return-type method)
                             (,@(vtable-method-info-args method))
                             (restart-case
                               ,(if (vtable-method-info-impl-call method)
                                    `(progn
                                       ,@(rest (vtable-method-info-impl-call method)))
                                    `(,(vtable-method-info-name method)
                                       ,@(mapcar #'first
                                                 (vtable-method-info-args method))))
                               (return-from-interface-method-implementation
                                 (v)
                                 :interactive
                                 (lambda () (list (eval (read)))) v))))))))

;;; ----------------------------------------------------------------------------

(defun interface-init (iface data)
  (destructuring-bind (class-name interface-name)
      (prog1
        (glib::get-stable-pointer-value data)
        (glib::free-stable-pointer data))
    (declare (ignorable class-name))
    (let* ((vtable (gethash interface-name *vtables*))
           (vtable-cstruct (vtable-description-cstruct-name vtable)))
      (log-for :subclass "interface-init for class ~A and interface ~A~%"
               class-name
               interface-name)
      (iter (for method in (vtable-description-methods vtable))
            (for cb = (get-callback (vtable-method-info-callback-name method)))
            (for slot-name = (vtable-method-info-slot-name method))
            (log-for :subclass "->setting method ~A to ~A~%" method cb)
            (setf (foreign-slot-value iface vtable-cstruct slot-name) cb)))))

(defcallback c-interface-init :void ((iface :pointer) (data :pointer))
  (interface-init iface data))

;;; ----------------------------------------------------------------------------

(defun add-interface (name interface)
  (let* ((interface-info (list name interface))
         (interface-info-ptr (glib::allocate-stable-pointer interface-info)))
    (with-foreign-object (info '(:struct g-interface-info))
      (setf (foreign-slot-value info '(:struct g-interface-info) :interface-init)
            (callback c-interface-init)
            (foreign-slot-value info '(:struct g-interface-info) :interface-data)
            interface-info-ptr)
      (g-type-add-interface-static (gtype name) (gtype interface) info))))

(defun add-interfaces (name)
  (let* ((lisp-type-info (gethash name *registered-types*))
         (interfaces (object-type-interfaces lisp-type-info)))
    (iter (for interface in interfaces)
          (add-interface name interface))))

;;; ----------------------------------------------------------------------------

(defun object-property-get (object property-id g-value pspec)
  (declare (ignore property-id))
  (let* ((lisp-object (or (gethash (pointer-address object)
                                   *foreign-gobjects-strong*)
                          (gethash (pointer-address object)
                                   *foreign-gobjects-weak*)))
         (property-name (foreign-slot-value pspec
                                            '(:struct g-param-spec)
                                            :name))
         (property-type (foreign-slot-value pspec
                                            '(:struct g-param-spec)
                                            :value-type))
         (type-name (gtype-name (foreign-slot-value pspec
                                                    '(:struct g-param-spec)
                                                    :owner-type)))
         (lisp-type-info (gethash type-name *registered-types*))
         (property-info (find property-name
                              (object-type-properties lisp-type-info)
                              :test 'string= :key 'first))
         (property-get-fn (fourth property-info)))
    (log-for :subclass "get(~A,'~A')~%" lisp-object property-name)
    (let ((value (restart-case
                   (funcall property-get-fn lisp-object)
                   (return-from-property-getter (value)
                                                :interactive
                                                (lambda ()
                                                  (format t "Enter new value: ")
                                                  (list (eval (read))))
                                                value))))
      (set-g-value g-value value property-type))))

(defcallback c-object-property-get :void
    ((object :pointer) (property-id :uint) (value :pointer) (pspec :pointer))
  (object-property-get object property-id value pspec))

;;; ----------------------------------------------------------------------------

(defun object-property-set (object property-id value pspec)
  (declare (ignore property-id))
  (let* ((lisp-object (or (gethash (pointer-address object)
                                   *foreign-gobjects-strong*)
                          (gethash (pointer-address object)
                                   *foreign-gobjects-weak*)))
         (property-name (foreign-slot-value pspec
                                            '(:struct g-param-spec) :name))
         (type-name (gtype-name (foreign-slot-value pspec
                                                    '(:struct g-param-spec)
                                                    :owner-type)))
         (lisp-type-info (gethash type-name *registered-types*))
         (property-info (find property-name
                              (object-type-properties lisp-type-info)
                              :test 'string= :key 'first))
         (property-set-fn (fifth property-info))
         (new-value (parse-g-value value)))
    (log-for :subclass "set(~A,'~A',~A)~%" lisp-object property-name new-value)
    (restart-case
        (funcall property-set-fn new-value lisp-object)
      (return-without-error-from-property-setter () nil))))

(defcallback c-object-property-set :void
    ((object :pointer) (property-id :uint) (value :pointer) (pspec :pointer))
  (object-property-set object property-id value pspec))

;;; ----------------------------------------------------------------------------

(defmacro register-object-type-implementation (name class parent interfaces properties)
  (unless (stringp parent)
    (setf parent (gtype-name (gtype parent))))

  `(progn
     (setf (gethash ,name *registered-types*)
           (make-object-type :name ,name
                             :class ',class
                             :parent ,parent
                             :interfaces ',interfaces
                             :properties ',properties))
     (glib-init::at-init (',class)
       (log-for :subclass
                "Registering GObject type implementation ~A for type ~A~%"
                ',class ,name)
       (with-foreign-object (query '(:struct g-type-query))
         (g-type-query (gtype ,parent) query)
         (g-type-register-static-simple (gtype ,parent)
                                        ,name
                                        (foreign-slot-value query
                                                            '(:struct g-type-query)
                                                            :class-size)
                                        (callback class-init-cb)
                                        (foreign-slot-value query
                                                            '(:struct g-type-query)
                                                            :instance-size)
                                        (callback instance-init-cb) nil))
       (add-interfaces ,name))
     (defmethod initialize-instance :before ((object ,class) &key pointer)
       (log-for :subclass
                ":subclass INITIAlIZE-INSTANCE :before ~A :pointer ~A~%"
                object pointer)
       (unless (or pointer
                   (and (slot-boundp object 'pointer)
                        (pointer object)))
         (log-for :subclass "calling g-object-constructor~%")
         (setf (pointer object)
               (call-gobject-constructor ,name nil nil)
               (g-object-has-reference object) t)))
     (progn
       ,@(iter (for (prop-name prop-type prop-accessor prop-reader prop-writer)
                    in properties)
               (declare (ignorable prop-type))
               (when prop-reader
                 (collect `(defun ,prop-accessor (object)
                             (g-object-property object ,prop-name))))
               (when prop-writer
                 (collect `(defun (setf ,prop-accessor) (new-value object)
                             (setf (g-object-property object ,prop-name)
                                   new-value))))))
     ,name))

;;; ----------------------------------------------------------------------------

;; This is a hack to transform the list of properties in an new order for
;; the functions which register a foreign class. Consider to reimplement this
;; to avoid different orders of property lists.

(defun properties-to-new-list (properties)
  (loop for property in properties
        collect (list (third property)
                      (fourth property)
                      (second property)
                      (fifth property)
                      (sixth property))))

(defmacro define-foreign-g-object-class (g-type-name name
                                 (&key (superclass 'g-object)
                                       (export t)
                                       interfaces
                                       type-initializer)
                                 (&rest properties))

  (declare (ignore export type-initializer))

;  (setf properties (mapcar #'parse-property properties))

  (let ((props (mapcar #'parse-property properties))
        (parent (if (stringp superclass)
                    superclass
                    (gobject-class-g-type-name (find-class superclass)))))

    (setf properties (properties-to-new-list properties))

  `(progn

     (setf (gethash ,g-type-name *registered-types*)
           (make-object-type :name ,g-type-name
                             :class ',name
                             :parent ,parent
                             :interfaces ',interfaces
                             :properties ',properties))

     (glib-init::at-init (',name)
       (log-for :subclass
                "Debug sublcass: Registering GObject type ~A for type ~A~%"
                ',name ,g-type-name)
       (with-foreign-object (query '(:struct g-type-query))
         (g-type-query (gtype ,parent) query)
         (g-type-register-static-simple (gtype ,parent)
                                        ,g-type-name
                                        (foreign-slot-value query
                                                            '(:struct g-type-query)
                                                            :class-size)
                                        (callback class-init-cb)
                                        (foreign-slot-value query
                                                            '(:struct g-type-query)
                                                            :instance-size)
                                        (callback instance-init-cb) nil))
       (add-interfaces ,g-type-name))


     (defclass ,name (,@(when (and superclass
                                   (not (eq superclass 'g-object)))
                          (list superclass))
                      ,@(mapcar #'interface->lisp-class-name interfaces))
       ;; Generate the slot definitions from the given properties
       (,@(mapcar (lambda (property)
                     (meta-property->slot name property))
                   props))
       (:metaclass gobject-class)
;       (:g-type-name . ,g-type-name)
;       ,@(when type-initializer
;           (list `(:g-type-initializer . ,type-initializer)))
)


     (defmethod initialize-instance :before ((object ,name) &key pointer)
       (log-for :subclass
                ":subclass INITIAlIZE-INSTANCE :before ~A :pointer ~A~%"
                object pointer)
       (unless (or pointer
                   (and (slot-boundp object 'pointer)
                        (pointer object)))
         (log-for :subclass ":subclass calling g-object-constructor~%")
         (setf (pointer object)
               (call-gobject-constructor ,g-type-name nil nil)
               (g-object-has-reference object) t)))

     (progn
       ,@(iter (for (prop-name prop-type prop-accessor prop-reader prop-writer)
                    in properties)
               (declare (ignorable prop-type))
               (when prop-reader
                 (collect `(defun ,prop-accessor (object)
                             (g-object-property object ,prop-name))))
               (when prop-writer
                 (collect `(defun (setf ,prop-accessor) (new-value object)
                             (setf (g-object-property object ,prop-name)
                                   new-value))))))


;     ,@(when export
;         (cons `(export ',name
;                         (find-package
;                           ,(package-name (symbol-package name))))
;               (mapcar (lambda (property)
;                         `(export ',(intern (format nil "~A-~A"
;                                                       (symbol-name name)
;                                                       (property-name property))
;                                              (symbol-package name))
;                                   (find-package
;                                     ,(package-name (symbol-package name)))))
;                        props)))

)))

(export 'define-foreign-g-object-class)

;;; --- End of file gobject.foreign-gobject-subclassing.lisp -------------------
