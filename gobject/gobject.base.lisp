;;; ----------------------------------------------------------------------------
;;; gobject.base.lisp
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.66 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;;
;;; GObject
;;;
;;;     The base object type
;;;
;;; Types and Values
;;;
;;;     GObject
;;;     GObjectClass
;;;     GObjectConstructParam                              not exported
;;;     GParameter                                         not exported
;;;     GInitiallyUnowned
;;;     GInitiallyUnownedClass                             not implemented
;;;
;;; Functions
;;;
;;;     GObjectGetPropertyFunc
;;;     GObjectSetPropertyFunc
;;;     GObjectFinalizeFunc
;;;
;;;     G_TYPE_IS_OBJECT
;;;     G_OBJECT
;;;     G_IS_OBJECT
;;;     G_OBJECT_CLASS
;;;     G_IS_OBJECT_CLASS
;;;     G_OBJECT_GET_CLASS
;;;     G_OBJECT_TYPE
;;;     G_OBJECT_TYPE_NAME
;;;     G_OBJECT_CLASS_TYPE
;;;     G_OBJECT_CLASS_NAME
;;;
;;;     g_object_class_install_property                    not exported
;;;     g_object_class_install_properties                  not exported
;;;     g_object_class_find_property
;;;     g_object_class_list_properties
;;;     g_object_class_override_property                   not exported
;;;     g_object_interface_install_property                not exported
;;;     g_object_interface_find_property
;;;     g_object_interface_list_properties
;;;     g_object_new
;;;     g_object_new_with_properties                       not imeplemented
;;;     g_object_newv                                      not exported
;;;     g_object_ref                                       not exported
;;;     g_object_unref                                     not exported
;;;     g_object_ref_sink                                  not exported
;;;     g_set_object                                       not implemented
;;;     g_clear_object                                     not implemented
;;;     g_object_is_floating                               not exported
;;;     g_object_force_floating                            not exported
;;;
;;;     GWeakNotify
;;;     g_object_weak_ref
;;;     g_object_weak_unref                                not exported
;;;     g_object_add_weak_pointer                          not exported
;;;     g_object_remove_weak_pointer
;;;     g_set_weak_pointer
;;;     g_clear_weak_pointer
;;;
;;;     GToggleNotify
;;;     g_object_add_toggle_ref                            not exported
;;;     g_object_remove_toggle_ref                         not exported
;;;     g_object_connect
;;;     g_object_disconnect
;;;
;;;     g_object_set
;;;     g_object_setv
;;;     g_object_get
;;;     g_object_getv
;;;
;;;     g_object_notify
;;;     g_object_notify_by_pspec
;;;     g_object_freeze_notify
;;;     g_object_thaw_notify
;;;
;;;     g_object_get_data
;;;     g_object_set_data
;;;     g_object_set_data_full
;;;     g_object_steal_data
;;;     g_object_dup_data
;;;     g_object_replace_data
;;;     g_object_get_qdata
;;;     g_object_set_qdata
;;;     g_object_set_qdata_full
;;;     g_object_steal_qdata
;;;     g_object_dup_qdata
;;;     g_object_replace-qdata
;;;     g_object_set_property
;;;     g_object_get_property
;;;     g_object_new_valist
;;;     g_object_set_valist
;;;     g_object_get_valist
;;;     g_object_watch_closure
;;;     g_object_run_dispose
;;;
;;;     G_OBJECT_WARN_INVALID_PROPERTY_ID
;;;
;;;     g_weak_ref_init
;;;     g_weak_ref_clear
;;;     g_weak_ref_get
;;;     g_weak_ref_set
;;;     g_assert_finalize_object
;;;
;;; Signals
;;;
;;;     void    notify    No Hooks
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GBinding
;;;     ├── GInitiallyUnowned
;;;     ╰── GTypeModule
;;; ----------------------------------------------------------------------------

(in-package :gobject)

(defvar *foreign-gobjects-weak*
        (make-weak-hash-table :test 'equal :weakness :value))
(defvar *foreign-gobjects-strong* (make-hash-table :test 'equal))
(defvar *current-creating-object* nil)
(defvar *current-object-from-pointer* nil)
(defvar *currently-making-object-p* nil)

(glib-init::at-finalize ()
  (clrhash *foreign-gobjects-weak*)
  (clrhash *foreign-gobjects-strong*)
  (setf *current-creating-object* nil
        *current-object-from-pointer* nil
        *currently-making-object-p* nil))

;;; ----------------------------------------------------------------------------

;; Stores the Lisp class name of the gobject-class for the GType name
;; e.g. the GType name "GtkButton" has the Lisp class name GTK-BUTTON

(defvar *registered-object-types* (make-hash-table :test 'equal))

(defun register-object-type (name class-name)
  (setf (gethash name *registered-object-types*) class-name))

(defun registered-object-type-by-name (name)
  (gethash name *registered-object-types*))

;;; ----------------------------------------------------------------------------
;;; struct GParameter
;;; ----------------------------------------------------------------------------

;; This structure is only needed in the construction of an object and is
;; not exported.

;; The generalized calculation of the size and offset works for sbcl and ccl on
;; a 32-bit Linux. Check this for more system.

#-windows
(defcstruct (g-parameter :size #.(+ (foreign-type-size :string)
                                    (foreign-type-size '(:struct g-value))))
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:value (:struct g-value)
          :offset #.(foreign-type-size :string))) ; A struct, not a pointer.

#+windows
(defcstruct g-parameter
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:value (:struct g-value))) ; A struct, not a pointer.

#+cl-cffi-gtk-documentation
(setf (gethash 'g-parameter atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'g-parameter atdoc:*external-symbols*)
 "@version{2013-10-25}
  @begin{short}
    The @sym{g-parameter} structure is an auxiliary structure used to hand
    parameter name/value pairs to the function @fun{g-object-newv}.
  @end{short}
  @begin{pre}
(defcstruct g-parameter
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:value g-value))
  @end{pre}
  @begin[code]{table}
    @entry[:name]{The parameter name.}
    @entry[:value]{The parameter value.}
  @end{table}
  @see-symbol{g-value}
  @see-function{g-object-newv}")

;;; ----------------------------------------------------------------------------
;;; struct GObject
;;;
;;; All the fields in the GObject structure are private to the GObject
;;; implementation and should never be accessed directly.
;;; ----------------------------------------------------------------------------

;; %g-object is not needed in the implementation.
;; It is defined to access the property ref-count for debugging the code.

(defcstruct %g-object
  (:type-instance (:pointer (:struct g-type-instance)))
  (:ref-count :uint)
  (:data :pointer))

;; Accessor for the slot ref-count of %g-object

(defun ref-count (pointer)
  (foreign-slot-value (if (pointerp pointer)
                          pointer
                          (pointer pointer))
                      '(:struct %g-object) :ref-count))

;;; ----------------------------------------------------------------------------

;; Define the base class g-object

(defclass g-object ()
  ((pointer
    :type (or null cffi:foreign-pointer)
    :initarg :pointer
    :accessor pointer
    :initform nil)
   (has-reference
    :type boolean
    :accessor g-object-has-reference
    :initform nil)
   (signal-handlers
    :type (array t *)
    :initform (make-array 0 :adjustable t :fill-pointer t)
    :reader g-object-signal-handlers)))

(export 'g-object)
(export 'pointer) ; TODO: g-object-pointer might be more consistent
(export 'g-object-has-reference)
(export 'g-object-signal-handlers)

;; Add g-object to the global Hash table *registered-object-types*

(register-object-type "GObject" 'g-object)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'g-object 'type)
 "@version{2020-11-15}
  @begin{short}
    The @sym{g-object} class is the fundamental type providing the common
    attributes and methods for all object types in GTK+, Pango and other
    libraries based on the @sym{g-object} class.
  @end{short}
  The @sym{g-object} class provides methods for object construction and
  destruction, property access methods, and signal support. Signals are
  described in detail in the
  @url[https://developer.gnome.org/gobject/stable/signal.html]{GObject documentation}.
  @begin[Lisp Implementation]{dictionary}
    In the Lisp implementation three slots are added. The slot @code{pointer}
    holds the pointer to the C instance of the object. The slot
    @code{signal-handlers} stores all signals which are connected to an
    instance. The slot @code{has-reference} is initialized to the value @code{t}
    during creation of an object. See the slot access functions for examples.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"notify\" signal}
    @begin{pre}
 lambda (object pspec)    : No Hooks
    @end{pre}
    The \"notify\" signal is emitted on an object when one of its properties
    has been changed. Note that getting this signal does not guarantee that the
    value of the property has actually changed, it may also be emitted when the
    setter for the property is called to reinstate the previous value. This
    signal is typically used to obtain change notification for a single
    property, by specifying the property name as a detail in the
    @fun{g-signal-connect} call, like this:
    @begin{pre}
(g-signal-connect switch \"notify::active\"
   (lambda (widget pspec)
     (declare (ignore pspec))
     (if (gtk-switch-active widget)
         (setf (gtk-label-label label) \"The Switch is ON\")
         (setf (gtk-label-label label) \"The Switch is OFF\"))))
    @end{pre}
    It is important to note that you must use canonical parameter names as
    detail strings for the notify signal.
    @begin[code]{table}
      @entry[object]{The @sym{g-object} which received the signal.}
      @entry[pspec]{The @symbol{g-param-spec} structure of the property which
        changed.}
    @end{table}
  @end{dictionary}
  @see-slot{pointer}
  @see-slot{g-object-has-reference}
  @see-slot{g-object-signal-handlers}
  @see-function{g-signal-connect}
  @see-symbol{g-param-spec}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor details
;;; ----------------------------------------------------------------------------

;;; --- pointer ----------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pointer" 'g-object) 't)
 "Holds a foreign pointer to the C instance of a GObject.")

#+cl-cffi-gtk-documentation
(setf (gethash 'pointer atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'pointer 'function)
 "@version{2020-11-15}
  @short{Accessor of the @code{pointer} slot of the @class{g-object} class.}

  The accessor @sym{pointer} gets the foreign C pointer of an instance which is
  stored in a slot of the Lisp @class{g-object} class.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq label (make-instance 'gtk-label)) => #<GTK-LABEL {E2DB181@}>
(pointer label) => #.(SB-SYS:INT-SAP #X081BDAE0)
    @end{pre}
  @end{dictionary}
  @see-class{g-object}")

;;; --- g-object-has-reference -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-reference" 'g-object) 't)
 "Holds the value @em{true} when the instance is successfully registered.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-object-has-reference atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-object-has-reference 'function)
 "@version{2020-10-17}
  @begin{short}
    Accessor of the @code{has-reference} slot of the @class{g-object} class.
  @end{short}

  The slot is set to @em{true} when registering an object during creation.
  @see-class{g-object}")

;;; --- g-object-signal-handlers -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "signal-handlers" 'g-object) 't)
 "An array of signals handlers which are connected to the instance.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-object-signal-handlers atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-object-signal-handlers 'function)
 "@version{2020-11-15}
  @begin{short}
    Returns an array of signal handlers which are connected to an instance of
    a GObject.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(setq button (make-instance 'gtk-button))
=> #<GTK-BUTTON {E319359@}>
(g-signal-connect button \"clicked\" (lambda () ))
=> 27
(g-object-signal-handlers button)
=> #(#<FUNCTION (LAMBDA #) {E324855@}>)
(g-signal-connect button \"destroy\" (lambda () ))
=> 28
(g-object-signal-handlers button)
=> #(#<FUNCTION (LAMBDA #) {E324855@}> #<FUNCTION (LAMBDA #) {E336EDD@}>)
    @end{pre}
  @end{dictionary}
  @see-class{g-object}")

;;; ----------------------------------------------------------------------------

;; GC for weak pointers

(defvar *gobject-gc-hooks-lock*
        (make-recursive-lock "gobject-gc-hooks-lock"))
(defvar *gobject-gc-hooks* nil) ; pointers to objects to be freed

;;; ----------------------------------------------------------------------------

(defmethod release ((obj g-object))
  (cancel-finalization obj)
  (let ((p (pointer obj)))
    (setf (pointer obj) nil)
    (dispose-carefully p)))

(defun dispose-carefully (pointer)
  (handler-case
      (register-gobject-for-gc pointer)
    (error (e)
      (log-for :gc  "Error in dispose: ~A~%" e)
      (format t "Error in dispose: ~A~%" e))))

(defun activate-gc-hooks ()
  (with-recursive-lock-held (*gobject-gc-hooks-lock*)
    (when *gobject-gc-hooks*
      (log-for :gc "activating gc hooks for objects: ~A~%" *gobject-gc-hooks*)
      (loop for pointer in *gobject-gc-hooks*
            do (%g-object-remove-toggle-ref pointer
                                            (callback g-toggle-notify)
                                            (null-pointer)))
      (setf *gobject-gc-hooks* nil)))
  nil)

(defun register-gobject-for-gc (pointer)
  (with-recursive-lock-held (*gobject-gc-hooks-lock*)
    (let ((locks-were-present (not (null *gobject-gc-hooks*))))
      (push pointer *gobject-gc-hooks*)
      (unless locks-were-present
        (log-for :gc "adding idle-gc-hook to main loop~%")
        (g-idle-add #'activate-gc-hooks)))))

;(defcallback g-idle-gc-hook :boolean ((data :pointer))
;  (declare (ignore data))
;  (format t "~%~%IN G-IDLE-GC-HOOKS~%~%")
;  (activate-gc-hooks)
;  nil)

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :around ((obj g-object) &key)
  (when *currently-making-object-p*
    (setf *currently-making-object-p* t))
  (let ((*current-creating-object* obj))
    (log-for :subclass
             ":subclass INITIALIZE-INSTANCE :around ~
              *current-creating-object* is ~A~%" obj)
    (call-next-method)))

(defmethod initialize-instance :after ((obj g-object) &key &allow-other-keys)
  (unless (slot-boundp obj 'pointer)
    (error "Pointer slot is not initialized for ~A" obj))
  (let* ((pointer (pointer obj))
         (s (format nil "~A" obj)))
    (finalize obj
              (lambda ()
                (log-for :gc "~A ~A is queued for GC (having ~A refs)~%"
                         (g-type-from-instance pointer)
                         pointer
                         (ref-count pointer))
                (handler-case
                  (dispose-carefully pointer)
                  (error (e)
                    (log-for :gc "Error in finalizer for ~A: ~A~%" s e)
                    (format t "Error in finalizer for ~A: ~A~%" s e))))))
  (register-g-object obj)
  (activate-gc-hooks))

;;; ----------------------------------------------------------------------------

(defun register-g-object (obj)
  (log-for :gc "registered GObject ~A (~A) with initial ref-count ~A ~A~%"
           (pointer obj)
           obj
           (ref-count obj)
           (if (%g-object-is-floating (pointer obj)) "(floating)" ""))
  (when (should-ref-sink-at-creation obj)
    (log-for :gc "g_object_ref_sink(~A)~%" (pointer obj))
    (%g-object-ref-sink (pointer obj)))
  (setf (g-object-has-reference obj) t)
  (setf (gethash (pointer-address (pointer obj)) *foreign-gobjects-strong*)
        obj)
  (%g-object-add-toggle-ref (pointer obj)
                            (callback g-toggle-notify)
                            (null-pointer))
  (%g-object-unref (pointer obj)))

;;; ----------------------------------------------------------------------------

;; TODO: This function is not used.

(defcallback gobject-weak-ref-finalized :void
    ((data :pointer) (pointer :pointer))
  (declare (ignore data))
  (log-for :gc "~A is weak-ref-finalized with ~A refs~%"
           pointer (ref-count pointer))
  (remhash (pointer-address pointer) *foreign-gobjects-weak*)
  (when (gethash (pointer-address pointer) *foreign-gobjects-strong*)
    (warn "GObject at ~A was weak-ref-finalized while still holding lisp-side ~
           strong reference to it"
          pointer)
    (log-for :gc "GObject at ~A was weak-ref-finalized while still holding ~
                  lisp-side strong reference to it"
             pointer))
  (remhash (pointer-address pointer) *foreign-gobjects-strong*))

;;; ----------------------------------------------------------------------------

;; If object was not created from lisp-side, we should ref it
;; If an object is regular g-object, we should not ref-sink it
;; If an object is GInitiallyUnowned, then it is created with a floating
;; reference, we should ref-sink it
;; A special case is GtkWindow: we should ref-sink it anyway

(defun should-ref-sink-at-creation (object)
  (let ((r (cond
             ;; not new objects should be ref_sunk
             ((equal *current-object-from-pointer* (pointer object))
              (log-for :gc "*cur-obj-from-ptr* ")
              t)
             ;; g_object_new returns objects with ref=1, we should save this ref
             ((eq object *current-creating-object*)
              ;; but GInitiallyUnowned objects should be ref_sunk
              (typep object 'g-initially-unowned))
             (t t))))
    (log-for :gc "(should-ref-sink-at-creation ~A) => ~A~%" object r)
    r))

;;; ----------------------------------------------------------------------------

;; Define the type foreign-g-object-type and the type transformation rules.

(define-foreign-type foreign-g-object-type ()
  ((sub-type :reader sub-type
             :initarg :sub-type
             :initform 'g-object)
   (already-referenced :reader foreign-g-object-type-already-referenced
                       :initarg :already-referenced
                       :initform nil))
  (:actual-type :pointer))

(define-parse-method g-object (&rest args)
  (let* ((sub-type (first (remove-if #'keywordp args)))
         (flags (remove-if-not #'keywordp args))
         (already-referenced (not (null (find :already-referenced flags)))))
    (make-instance 'foreign-g-object-type
                   :sub-type sub-type
                   :already-referenced already-referenced)))

(defmethod translate-to-foreign (object (type foreign-g-object-type))
  (cond
    ((null object)
     (null-pointer))
    ((pointerp object) object)
    ((null (pointer object))
     (error "Object ~A has been disposed" object))
    ((typep object 'g-object)
     (when (sub-type type)
       (assert (typep object (sub-type type))
               nil
               "Object ~A is not a subtype of ~A" object (sub-type type)))
     (pointer object))
    (t (error "Object ~A is not translatable as GObject*" object))))

(defmethod translate-from-foreign (pointer (type foreign-g-object-type))
  (let ((object (get-g-object-for-pointer pointer)))
    (when (and object
               (foreign-g-object-type-already-referenced type))
      (%g-object-unref (pointer object)))
    object))

;; Translate a pointer to a C object to the corresponding Lisp object.
;; If a correpondig Lisp object does not exist, create the Lisp object.

(defun get-g-object-for-pointer (pointer)
  (unless (null-pointer-p pointer)
    (or (gethash (pointer-address pointer) *foreign-gobjects-strong*)
        (gethash (pointer-address pointer) *foreign-gobjects-weak*)
        (progn
          (log-for :gc "Now creating object for ~A~%" pointer)
          (create-gobject-from-pointer pointer)))))

;;; ----------------------------------------------------------------------------

;; Create a Lisp object from a C pointer to an existing C object.

(defun create-gobject-from-pointer (pointer)
  (flet (;; Get the corresponing lisp type for a GType
         (get-gobject-lisp-type (gtype)
            (iter (while (not (null gtype)))
                  (for lisp-type = (gethash (gtype-name gtype)
                                            *registered-object-types*))
                  (when lisp-type (return lisp-type))
                  (setf gtype (g-type-parent gtype)))))
    (let* ((gtype (g-type-from-instance pointer))
           (lisp-type (get-gobject-lisp-type gtype)))
      (unless lisp-type
        (error "Type ~A is not registered with REGISTER-OBJECT-TYPE"
               (gtype-name gtype)))
      (let ((*current-object-from-pointer* pointer))
        (make-instance lisp-type :pointer pointer)))))

;;; ----------------------------------------------------------------------------

(define-condition property-access-error (error)
  ((property-name :initarg :property-name
                  :reader property-access-error-property-name)
   (class-name :initarg :class-name
               :reader property-access-error-class-name)
   (message :initarg :message :reader property-access-error-message))
  (:report (lambda (condition stream)
             (format stream "Error accessing property '~A' on class '~A': ~A"
                     (property-access-error-property-name condition)
                     (property-access-error-class-name condition)
                     (property-access-error-message condition)))))

(define-condition property-unreadable-error (property-access-error)
  ()
  (:default-initargs :message "property is not readable"))

(define-condition property-unwritable-error (property-access-error)
  ()
  (:default-initargs :message "property is not writable"))

;;; ----------------------------------------------------------------------------

;; Get the type of a property for a class.

(defun class-property-type (object-type property-name
                            &key assert-readable assert-writable)
  (let* ((property (class-property-info object-type property-name)))
    (when (and assert-readable
               (not (param-spec-readable property)))
      (error 'property-unreadable-error
             :property-name property-name
             :class-name (gtype-name (gtype object-type))))
    (when (and assert-writable
               (not (param-spec-writable property)))
      (error 'property-unwritable-error
             :property-name property-name
             :class-name (gtype-name (gtype object-type))))
    (param-spec-type property)))

;; Get the definition of a property for the GObject type. Both arguments are of
;; type string, e.g. (class-property-info "GtkLabel" "label")

(defun class-property-info (gtype property-name)
  (let ((class (g-type-class-ref gtype)))
    (unwind-protect
      (let ((pspec (%g-object-class-find-property class property-name)))
        (when pspec
          (parse-g-param-spec pspec)))
      (g-type-class-unref class))))

;;; ----------------------------------------------------------------------------

(defmethod make-instance ((class gobject-class) &rest initargs &key pointer)
  (log-for :subclass
           ":subclass MAkE-INSTANCE ~A ~{~A~^ ~})~%" class initargs)
  (ensure-finalized class)
  (let ((*currently-making-object-p* t))
    (if pointer
        (progn
          (assert (= (length initargs) 2)
                  nil
                  "POINTER can not be combined with other initargs (~A)"
                  initargs)
          (call-next-method))
        (let* ((default-initargs
                (iter (for (arg value) in (class-default-initargs class))
                      (nconcing (list arg value))))
               (effective-initargs (append initargs default-initargs))
               (pointer (create-gobject-from-class class effective-initargs)))
          (apply #'call-next-method class
                 :pointer pointer
                 effective-initargs)))))

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance ((instance g-object) &rest initargs
                                                    &key &allow-other-keys)
  (let ((filtered-initargs (filter-initargs-by-class (class-of instance)
                                                     initargs)))
    (apply #'call-next-method instance filtered-initargs)))

;;; ----------------------------------------------------------------------------

(defun create-gobject-from-class (class initargs)
  (when (gobject-class-interface-p class)
    (error "Trying to create instance of GInterface '~A' (class '~A')"
           (gobject-class-g-type-name class)
           (class-name class)))
  (let (arg-names arg-values arg-types nc-setters nc-arg-values)
    (declare (dynamic-extent arg-names arg-values arg-types
                             nc-setters nc-arg-values))
    (loop
      for (arg-name arg-value) on initargs by #'cddr
      for slot = (find arg-name (class-slots class)
                       :key 'slot-definition-initargs
                       :test 'member)
      when (and slot (typep slot 'gobject-effective-slot-definition))
      do (typecase slot
           (gobject-property-effective-slot-definition
            (push (gobject-property-effective-slot-definition-g-property-name slot)
                  arg-names)
            (push arg-value arg-values)
            (push (gobject-effective-slot-definition-g-property-type slot)
                  arg-types))
           (gobject-fn-effective-slot-definition
            (push (gobject-fn-effective-slot-definition-g-setter-fn slot)
                  nc-setters)
            (push arg-value nc-arg-values))))
    (let ((object (call-gobject-constructor (gobject-class-g-type-name class)
                                            arg-names
                                            arg-values
                                            arg-types)))
      (loop
         for fn in nc-setters
         for value in nc-arg-values
         do (funcall fn object value))
      object)))

;;; ----------------------------------------------------------------------------

(defun call-gobject-constructor (object-type args-names args-values
                                 &optional args-types)
  (unless args-types
    (setf args-types
          (mapcar (lambda (name)
                    (class-property-type object-type name))
                  args-names)))
  (let ((args-count (length args-names)))
    (with-foreign-object (parameters '(:struct g-parameter) args-count)
      (loop
        for i from 0 below args-count
        for arg-name in args-names
        for arg-value in args-values
        for arg-type in args-types
        for arg-g-type = (if arg-type
                             arg-type
                             (class-property-type object-type arg-name))
        for parameter = (mem-aptr parameters '(:struct g-parameter) i)
        do (setf (foreign-slot-value parameter '(:struct g-parameter) :name)
                 arg-name)
        do (set-g-value (foreign-slot-pointer parameter
                                              '(:struct g-parameter) :value)
                        arg-value
                        arg-g-type
                        :zero-g-value t))
      (unwind-protect
        (%g-object-newv object-type args-count parameters)
        (loop
          for i from 0 below args-count
          for parameter = (mem-aptr parameters '(:struct g-parameter) i)
          do (foreign-string-free
                 (mem-ref (foreign-slot-pointer parameter
                                                '(:struct g-parameter)
                                                :name)
                          :pointer))
          do (g-value-unset (foreign-slot-pointer parameter
                                                  '(:struct g-parameter)
                                                  :value)))))))

;;; ----------------------------------------------------------------------------
;;; struct GObjectClass
;;; ----------------------------------------------------------------------------

(defcstruct g-object-class
  (:type-class (:pointer (:struct g-type-class)))
  (:construct-properties :pointer)
  (:constructor :pointer)
  (:set-property :pointer)
  (:get-property :pointer)
  (:dispose :pointer)
  (:finalize :pointer)
  (:dispatch-properties-changed :pointer)
  (:notify :pointer)
  (:constructed :pointer)
  (:pdummy :pointer :count 7))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-object-class atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-object-class atdoc:*external-symbols*)
 "@version{2020-11-15}
  @short{The class structure for the @class{g-object} type.}
  @begin{pre}
(defcstruct g-object-class
  (:type-class g-type-class)
  (:construct-properties :pointer)
  (:constructor :pointer)
  (:set-property :pointer)
  (:get-property :pointer)
  (:dispose :pointer)
  (:finalize :pointer)
  (:dispatch-properties-changed :pointer)
  (:notify :pointer)
  (:constructed :pointer)
  (:pdummy :pointer :count 7))
  @end{pre}
  @begin[code]{table}
    @begin[:type-class]{entry}
      The parent class.
    @end{entry}
    @begin[:constructor]{entry}
      The @code{constructor} function is called by the function
      @fun{g-object-new} to complete the object initialization after all the
      construction properties are set. The first thing a @code{constructor}
      implementation must do is chain up to the @code{constructor} of the
      parent class. Overriding @code{constructor} should be rarely needed,
      e.g. to handle construct properties, or to implement singletons.
    @end{entry}
    @begin[:set-property]{entry}
      The generic setter for all properties of this type. Should be overridden
      for every type with properties. Implementations of @code{set-property}
      do not need to emit property change notification explicitly, this is
      handled by the type system.
    @end{entry}
    @begin[:get-property]{entry}
      The generic getter for all properties of this type. Should be overridden
      for every type with properties.
    @end{entry}
    @begin[:dispose]{entry}
      The @code{dispose} function is supposed to drop all references to other
      objects, but keep the instance otherwise intact, so that client method
      invocations still work. It may be run multiple times (due to reference
      loops). Before returning, @code{dispose} should chain up to the
      @code{dispose} method of the parent class.
    @end{entry}
    @begin[:finalize]{entry}
      Instance finalization function, should finish the finalization of the
      instance begun in @code{dispose} and chain up to the @code{finalize}
      method of the parent class.
    @end{entry}
    @begin[:dispatch-properties-changed]{entry}
      Emits property change notification for a bunch of properties. Overriding
      @code{dispatch-properties-changed} should be rarely needed.
    @end{entry}
    @begin[:notify]{entry}
      The class closure for the notify signal.
    @end{entry}
    @begin[:constructed]{entry}
      The @code{constructed} function is called by the function
      @fun{g-object-new} as the final step of the object creation process. At
      the point of the call, all construction properties have been set on the
      object. The purpose of this call is to allow for object initialisation
      steps that can only be performed after construction properties have been
      set. @code{constructed} implementors should chain up to the
      @code{constructed} call of their parent class to allow it to complete
      its initialisation.
    @end{entry}
  @end{table}
  @see-class{g-object}")

(export 'g-object-class)

;;; ----------------------------------------------------------------------------
;;; struct GObjectConstructParam
;;; ----------------------------------------------------------------------------

;; This structure is not needed in the implementation of the Lisp library
;; and is not exported.

(defcstruct g-object-construct-param
  (:pspec (:pointer (:struct g-param-spec)))
  (:value (:pointer (:struct g-value))))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-object-construct-param atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-object-construct-param atdoc:*external-symbols*)
 "@version{2020-2-17}
  @begin{short}
    The @sym{g-object-construct-param} structure is an auxiliary structure used
    to hand @symbol{g-param-spec}/@symbol{g-value} pairs to the constructor of
    a @symbol{g-object-class} structure.
  @end{short}
  @begin{pre}
(defcstruct g-object-construct-param
  (:pspec (:pointer g-param-spec))
  (:value (:pointer g-value)))
  @end{pre}
  @begin[code]{table}
    @entry[:pspec]{The @symbol{g-param-spec} of the construct parameter.}
    @entry[:value]{The value to set the parameter to.}
  @end{table}
  @see-symbol{g-object-class}
  @see-symbol{g-param-spec}
  @see-symbol{g-value}")

;;; ----------------------------------------------------------------------------
;;; GInitiallyUnowned
;;;
;;; typedef struct _GObject GInitiallyUnowned;
;;;
;;; All the fields in the GInitiallyUnowned structure are private to the
;;; GInitiallyUnowned implementation and should never be accessed directly.
;;; ----------------------------------------------------------------------------

(defclass g-initially-unowned (g-object)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "GInitiallyUnowned")
  (:g-type-initializer . "g_initially_unowned_get_type")
  (:documentation "Base class that has initial 'floating' reference."))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-initially-unowned 'type)
 "@version{2020-11-15}
  @begin{short}
    The @sym{g-initially-unowned} class is derived from the @class{g-object}
    class. The only difference between the two is that the initial reference of
    a @sym{g-initially-unowned} object is flagged as a floating reference.
  @end{short}
  This means that it is not specifically claimed to be \"owned\" by any code
  portion.

  The floating reference can be converted into an ordinary reference by
  calling the function @code{g_object_ref_sink()}. For already sunken objects
  (objects that do not have a floating reference anymore), the function
  @code{g_object_ref_sink()} is equivalent to the function @code{g_object_ref()}
  and returns a new reference. Since floating references are useful almost
  exclusively for C convenience, language bindings that provide automated
  reference and memory ownership maintenance (such as smart pointers
  or garbage collection) should not expose floating references in their API.
  @see-class{g-object}")

(export 'g-initially-unowned)

;;; ----------------------------------------------------------------------------
;;; GInitiallyUnownedClass
;;;
;;; typedef struct _GObjectClass GInitiallyUnownedClass;
;;;
;;; The class structure for the GInitiallyUnowned type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GObjectGetPropertyFunc ()
;;;
;;; void (*GObjectGetPropertyFunc) (GObject *object,
;;;                                 guint property_id,
;;;                                 GValue *value,
;;;                                 GParamSpec *pspec);
;;;
;;; The type of the get_property function of GObjectClass.
;;;
;;; object :
;;;     a GObject
;;;
;;; property_id :
;;;     the numeric id under which the property was registered with
;;;     g_object_class_install_property().
;;;
;;; value :
;;;     a GValue to return the property value in
;;;
;;; pspec :
;;;     the GParamSpec describing the property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GObjectSetPropertyFunc ()
;;;
;;; void (*GObjectSetPropertyFunc) (GObject *object,
;;;                                 guint property_id,
;;;                                 const GValue *value,
;;;                                 GParamSpec *pspec);
;;;
;;; The type of the set_property function of GObjectClass.
;;;
;;; object :
;;;     a GObject
;;;
;;; property_id :
;;;     the numeric id under which the property was registered with
;;;     g_object_class_install_property().
;;;
;;; value :
;;;     the new value for the property
;;;
;;; pspec :
;;;     the GParamSpec describing the property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GObjectFinalizeFunc ()
;;;
;;; void (*GObjectFinalizeFunc) (GObject *object);
;;;
;;; The type of the finalize function of GObjectClass.
;;;
;;; object :
;;;     the GObject being finalized
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_OBJECT()
;;; ----------------------------------------------------------------------------

(defun g-type-is-object (gtype)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-15}
  @argument[gtype]{a @class{g-type} ID to check}
  @begin{return}
    @em{False} or @em{true}, indicating whether @arg{gtype} is a
    @var{+g-type-object+} type.
  @end{return}
  @begin{short}
    Checks if the passed in @arg{gtype} ID is a @var{+g-type-object+} type or
    derived from it.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g-type-is-object (gtype \"GtkLabel\")) => T
(g-type-is-object (gtype \"gboolean\")) => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-class{g-object}
  @see-variable{+g-type-object+}"
  (eq (g-type-fundamental (gtype gtype)) (gtype +g-type-object+)))

(export 'g-type-is-object)

;;; ----------------------------------------------------------------------------
;;; G_OBJECT()
;;;
;;; #define G_OBJECT(object)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((object), G_TYPE_OBJECT, GObject))
;;;
;;; Casts a GObject or derived pointer into a (GObject*) pointer. Depending on
;;; the current debugging level, this function may invoke certain runtime checks
;;; to identify invalid casts.
;;;
;;; object :
;;;     Object which is subject to casting.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_OBJECT()
;;; ----------------------------------------------------------------------------

(defun g-is-object (object)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-15}
  @argument[object]{instance to check for being a @var{+g-type-object+} type}
  @begin{short}
    Checks whether @arg{object} is of type @var{+g-type-object+}.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g-is-object (make-instance 'gtk-button)) => T
    @end{pre}
  @end{dictionary}
  @see-class{g-object}
  @see-variable{+g-type-object+}"
  (g-type-check-instance-type object +g-type-object+))

(export 'g-is-object)

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_CLASS()
;;;
;;; #define G_OBJECT_CLASS(class)
;;;         (G_TYPE_CHECK_CLASS_CAST ((class), G_TYPE_OBJECT, GObjectClass))
;;;
;;; Casts a derived GObjectClass structure into a GObjectClass structure.
;;;
;;; class :
;;;     a valid GObjectClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_OBJECT_CLASS()
;;; ----------------------------------------------------------------------------

(defun g-is-object-class (class)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-15}
  @argument[class]{a foreign pointer to a @symbol{g-object-class} structure}
  @begin{short}
    Checks whether @arg{class} is a @symbol{g-object-class} structure
    of type @var{+g-type-object+} or derived.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(g-object-class (make-instance 'gtk-button))
=> #.(SB-SYS:INT-SAP #X557BB1322590)
(g-is-object-class *) => T
    @end{pre}
  @end{dictionary}
  @see-class{g-object}
  @see-symbol{g-object-class}
  @see-variable{+g-type-object+}"
  (g-type-check-class-type class +g-type-object+))

(export 'g-is-object-class)

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_GET_CLASS() -> g-object-class
;;; ----------------------------------------------------------------------------

(defun g-object-class (object)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-15}
  @argument[object]{a @class{g-object} instance}
  @return{A foreign pointer to the @symbol{g-object-class} structure.}
  @short{Gets the class structure associated to a @class{g-object} instance.}
  @begin[Examples]{dictionary}
    @begin{pre}
(g-object-class (make-instance 'gtk-button))
=> #.(SB-SYS:INT-SAP #X557BB1322590)
    @end{pre}
  @end{dictionary}
  @see-class{g-object}
  @see-symbol{g-object-class}"
  (g-type-instance-class object))

(export 'g-object-class)

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_TYPE()
;;; ----------------------------------------------------------------------------

(defun g-object-type (object)
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-14}
  @argument[object]{a @class{g-object} instance to return the type ID for}
  @return{A @class{g-type} ID of @arg{object}.}
  @short{Gets the type ID for the instance of an object.}
  Returns @code{nil} if @arg{object} is @code{nil}.
  @begin[Examples]{dictionary}
    @begin{pre}
(g-object-type (make-instance 'gtk-label))
=> #S(GTYPE :NAME \"GtkLabel\" :%ID 134905144)
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-class{g-object}
  @see-function{g-object-type-name}"
  (when object
    (g-type-from-instance object)))

(export 'g-object-type)

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_TYPE_NAME()
;;; ----------------------------------------------------------------------------

(defun g-object-type-name (object)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-28}
  @argument[object]{a @class{g-object} instance to return the type name for}
  @return{A string with type name of @arg{object}.}
  @short{Gets the name of the type for an instance.}
  Returns @code{nil}, if @arg{object} is @code{nil}.
  @begin[Examples]{dictionary}
    @begin{pre}
(g-object-type-name (make-instance 'gtk-label)) => \"GtkLabel\"
    @end{pre}
  @end{dictionary}
  @see-class{g-object}
  @see-function{g-object-type}"
  (when object
    (g-type-name (g-object-type object))))

(export 'g-object-type-name)

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_CLASS_TYPE()
;;; ----------------------------------------------------------------------------

(defun g-object-class-type (class)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-15}
  @argument[class]{a foreign pointer to a @symbol{g-object-class} structure}
  @return{The @class{g-type} ID of @arg{class}.}
  @short{Gets the type ID of a class structure.}
  @begin[Examples]{dictionary}
    @begin{pre}
(g-object-class-type (g-type-class-ref \"GtkLabel\"))
=> #<GTYPE :name \"GtkLabel\" :id 93989740834480>
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-class{g-object}
  @see-symbol{g-object-class}"
  (g-type-from-class class))

(export 'g-object-class-type)

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_CLASS_NAME()
;;; ----------------------------------------------------------------------------

(defun g-object-class-name (class)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-15}
  @argument[class]{a foreign pointer of a @symbol{g-object-class} structure}
  @return{A string with the type name of @arg{class}.}
  @short{Returns the name of a class structure's type.}
  @begin[Examples]{dictionary}
    @begin{pre}
(g-object-class-name (g-type-class-ref \"GtkLabel\")) => \"GtkLabel\"
    @end{pre}
  @end{dictionary}
  @see-class{g-object}
  @see-symbol{g-object-class}"
  (g-type-name (g-type-from-class class)))

(export 'g-object-class-name)

;;; ----------------------------------------------------------------------------
;;; g_object_class_install_property ()                     not exported
;;; ----------------------------------------------------------------------------

;; For internal use and not exported.

(defcfun ("g_object_class_install_property" %g-object-class-install-property)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-17}
  @argument[class]{a @symbol{g-object-class} structure}
  @argument[property-id]{the ID for the new property}
  @argument[pspec]{the @symbol{g-param-spec} for the new property}
  @begin{short}
    Installs a new property. This is usually done in the class initializer.
  @end{short}

  Note that it is possible to redefine a property in a derived class, by
  installing a property with the same name. This can be useful at times, e.g.
  to change the range of allowed values or the default value.
  @see-class{g-object}
  @see-symbol{g-object-class}
  @see-symbol{g-param-spec}"
  (class (:pointer (:struct g-object-class)))
  (property-id :uint)
  (pspec (:pointer (:struct g-param-spec))))

;;; ----------------------------------------------------------------------------
;;; g_object_class_install_properties ()
;;;
;;; void g_object_class_install_properties (GObjectClass *oclass,
;;;                                         guint n_pspecs,
;;;                                         GParamSpec **pspecs);
;;;
;;; Installs new properties from an array of GParamSpecs. This is usually done
;;; in the class initializer.
;;;
;;; The property id of each property is the index of each GParamSpec in the
;;; pspecs array.
;;;
;;; The property id of 0 is treated specially by GObject and it should not be
;;; used to store a GParamSpec.
;;;
;;; This function should be used if you plan to use a static array of
;;; GParamSpecs and g_object_notify_by_pspec(). For instance, this class
;;; initialization:
;;;
;;;   enum {
;;;     PROP_0, PROP_FOO, PROP_BAR, N_PROPERTIES
;;;   };
;;;
;;;   static GParamSpec *obj_properties[N_PROPERTIES] = { NULL, };
;;;
;;;   static void
;;;   my_object_class_init (MyObjectClass *klass)
;;;   {
;;;     GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
;;;
;;;     obj_properties[PROP_FOO] =
;;;       g_param_spec_int ("foo", "Foo", "Foo",
;;;                         -1, G_MAXINT,
;;;                         0,
;;;                         G_PARAM_READWRITE);
;;;
;;;     obj_properties[PROP_BAR] =
;;;       g_param_spec_string ("bar", "Bar", "Bar",
;;;                            NULL,
;;;                            G_PARAM_READWRITE);
;;;
;;;     gobject_class->set_property = my_object_set_property;
;;;     gobject_class->get_property = my_object_get_property;
;;;     g_object_class_install_properties (gobject_class,
;;;                                        N_PROPERTIES,
;;;                                        obj_properties);
;;;   }
;;;
;;; allows calling g_object_notify_by_pspec() to notify of property changes:
;;;
;;;   void
;;;   my_object_set_foo (MyObject *self, gint foo)
;;;   {
;;;     if (self->foo != foo)
;;;       {
;;;         self->foo = foo;
;;;         g_object_notify_by_pspec (G_OBJECT (self),
;;;                                   obj_properties[PROP_FOO]);
;;;       }
;;;    }
;;;
;;; oclass :
;;;     a GObjectClass
;;;
;;; n_pspecs :
;;;     the length of the GParamSpecs array
;;;
;;; pspecs :
;;;     the GParamSpecs array defining the new properties
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_class_find_property ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_class_find_property" %g-object-class-find-property)
    (:pointer (:struct g-param-spec))
  (class (:pointer (:struct g-object-class)))
  (property-name :string))

(defun g-object-class-find-property (gtype property-name)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-15}
  @argument[gtype]{a @class{g-type} ID for an object class type}
  @argument[property-name]{a string with the name of the property to look up}
  @begin{return}
    The @symbol{g-param-spec} structure for the property, or @code{nil} if the
    class does not have a property of that name.
  @end{return}
  @begin{short}
    Looks up the @symbol{g-param-spec} structure for a property of a class
    type.
  @end{short}
  @begin[Examples]{dictionary}
    The @symbol{g-param-spec} structure for the property \"label\" of a
    @class{gtk-button} object is looked up.
    @begin{pre}
(setq pspec (g-object-class-find-property \"GtkLabel\" \"label\"))
=> #.(SB-SYS:INT-SAP #X009FC730)
(g-param-spec-type pspec)
=> #<GTYPE :name \"GParamString\" :id 9641904>
(g-param-spec-value-type pspec)
=> #<GTYPE :name \"gchararray\" :id 64>
(g-param-spec-name pspec)
=> \"label\"
    @end{pre}
  @end{dictionary}
  @see-class{g-object}
  @see-symbol{g-object-class}
  @see-symbol{g-param-spec}"
  (assert (g-type-is-a gtype +g-type-object+))
  (let ((class (g-type-class-ref gtype)))
    (unwind-protect
      (let ((pspec (%g-object-class-find-property class property-name)))
        (when (not (null-pointer-p pspec)) pspec))
      (g-type-class-unref class))))

(export 'g-object-class-find-property)

;;; ----------------------------------------------------------------------------
;;; g_object_class_list_properties ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_class_list_properties" %g-object-class-list-properties)
    (:pointer (:pointer (:struct g-param-spec)))
  (class (:pointer (:struct g-object-class)))
  (n-props (:pointer :uint)))

(defun g-object-class-list-properties (gtype)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-16}
  @argument[gtype]{a @class{g-type} ID of a object class}
  @return{A list of @symbol{g-param-spec} structures.}
  @begin{short}
    Gets a list of @symbol{g-param-spec} structures for all properties of a
    class.
  @end{short}
  @begin[Note]{dictionary}
    The C implementation of the corresponding function does not take a type ID,
    but a @symbol{g-object-class} structure as the argument. The Lisp function
    gets the class structure with the function @fun{g-type-class-ref}.
  @end{dictionary}
  @see-class{g-object}
  @see-class{g-type}
  @see-symbol{g-object-class}
  @see-symbol{g-param-spec}
  @see-function{g-type-class-ref}"
  (assert (g-type-is-a gtype +g-type-object+))
  (let ((class (g-type-class-ref gtype)))
    (unwind-protect
      (with-foreign-object (n-props :uint)
        (let ((pspecs (%g-object-class-list-properties class n-props)))
          (unwind-protect
            (loop for count from 0 below (mem-ref n-props :uint)
                  for pspec = (mem-aref pspecs :pointer count)
                  collect pspec)
          (g-free pspecs))))
      (g-type-class-unref class))))

(export 'g-object-class-list-properties)

;;; ----------------------------------------------------------------------------
;;; g_object_class_override_property ()                    not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_class_override_property"
          %g-object-class-override-property) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-17}
  @argument[class]{a @symbol{g-object-class} structure}
  @argument[property-id]{the new property ID}
  @argument[name]{the name of a property registered in a parent class or in an
    interface of this class}
  @begin{short}
    Registers @arg{property-id} as referring to a property with the name
    @arg{name} in a parent class or in an interface implemented by @arg{class}.
  @end{short}
  This allows this class to override a property implementation in a parent class
  or to provide the implementation of a property from an interface.
  @begin[Note]{dictionary}
    Internally, overriding is implemented by creating a property of type
    @code{GParamSpecOverride}; generally operations that query the properties of
    the object class, such as the functions @fun{g-object-class-find-property}
    or @fun{g-object-class-list-properties} will return the overridden property.
    However, in one case, the @code{construct_properties} argument of the
    constructor virtual function, the @code{GParamSpecOverride} is passed
    instead, so that the @code{param_id} field of the @symbol{g-param-spec}
    CStruct will be correct. For virtually all uses, this makes no difference.
    If you need to get the overridden property, you can call the
    @fun{g-param-spec-get-redirect-target} function.
  @end{dictionary}
  @see-class{g-object}
  @see-symbol{g-object-class}
  @see-function{g-object-class-find-property}
  @see-function{g-object-class-list-properties}
  @see-function{g-param-spec-get-redirect-target}"
  (class (:pointer (:struct g-object-class)))
  (property-id :uint)
  (name :string))

;;; ----------------------------------------------------------------------------
;;; g_object_interface_install_property ()                 not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_interface_install_property"
          %g-object-interface-install-property) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-17}
  @argument[iface]{any interface vtable for the interface, or the default
    vtable for the interface}
  @argument[pspec]{the @symbol{g-param-spec} CStruct for the new property}
  @begin{short}
    Add a property to an interface; this is only useful for interfaces that are
    added to GObject-derived types.
  @end{short}
  Adding a property to an interface forces all objects classes with that
  interface to have a compatible property. The compatible property could be a
  newly created @symbol{g-param-spec} CStruct, but normally the
  @fun{g-object-class-override-property} function will be used so that the
  object class only needs to provide an implementation and inherits the property
  description, default value, bounds, and so forth from the interface
  property.

  This function is meant to be called from the interface's default vtable
  initialization function (the @code{class_init} member of
  @symbol{g-type-info}.) It must not be called after after @code{class_init} has
  been called for any object types implementing this interface.
  @see-class{g-object}
  @see-symbol{g-param-spec}
  @see-symbol{g-type-info}
  @see-function{g-object-class-override-property}"
  (iface :pointer)
  (pspec (:pointer (:struct g-param-spec))))

;;; ----------------------------------------------------------------------------
;;; g_object_interface_find_property ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_interface_find_property" %g-object-interface-find-property)
    (:pointer (:struct g-param-spec))
  (iface (:pointer (:struct g-type-interface)))
  (property-name :string))

(defun g-object-interface-find-property (gtype property-name)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-15}
  @argument[gtype]{a @class{g-type} for an interface type}
  @argument[property-name]{a string with the name of a property to lookup}
  @begin{return}
    The @symbol{g-param-spec} structure for the property of the interface with
    the name @arg{property-name}, or @code{nil} if no such property exists.
  @end{return}
  @begin{short}
    Find the @symbol{g-param-spec} structure with the given property name for
    an interface.
  @end{short}
  @see-class{g-object}
  @see-class{g-type}
  @see-symbol{g-param-spec}"
  (assert (g-type-is-a gtype +g-type-interface+))
  (let ((iface (g-type-default-interface-ref gtype)))
    (unwind-protect
      (let ((pspec (%g-object-interface-find-property iface property-name)))
        (when (not (null-pointer-p pspec)) pspec))
      (g-type-default-interface-unref iface))))

(export 'g-object-interface-find-property)

;;; ----------------------------------------------------------------------------
;;; g_object_interface_list_properties ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_interface_list_properties"
          %g-object-interface-list-properties) (:pointer (:struct g-param-spec))
  (iface (:pointer (:struct g-type-interface)))
  (n-props (:pointer :uint)))

(defun g-object-interface-list-properties (gtype)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-16}
  @argument[gtype]{a @class{g-type} ID of an interface}
  @begin{short}
    A list of @symbol{g-param-spec} structures for all properties of an
    interface.
  @end{short}
  @begin{short}
    Lists the properties of an interface.
  @end{short}
  @begin[Note]{dictionary}
    The C implementation of the corresponding function does not take a
    type ID, but a vtable as the first argument. The Lisp function gets
    the vtable with the function @fun{g-type-default-interface-ref}.
  @end{dictionary}
  @see-class{g-object}
  @see-class{g-type}
  @see-symbol{g-param-spec}
  @see-function{g-type-default-interface-ref}"
  (assert (g-type-is-a gtype +g-type-interface+))
  (let ((iface (g-type-default-interface-ref gtype)))
    (unwind-protect
      (with-foreign-object (n-props :uint)
        (let ((pspecs (%g-object-interface-list-properties iface n-props)))
          (unwind-protect
            (loop for count from 0 below (mem-ref n-props :uint)
                  for pspec = (mem-aref pspecs :pointer count)
                  collect pspec)
            (g-free pspecs))))
      (g-type-default-interface-unref iface))))

(export 'g-object-interface-list-properties)

;;; ----------------------------------------------------------------------------
;;; g_object_new ()
;;; ----------------------------------------------------------------------------

(defun g-object-new (object-type &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-16}
  @argument[object-type]{the @class{g-type} ID of the @class{g-object} subtype
    to instantiate}
  @argument[args]{pairs of the property name and value}
  @begin{short}
    Creates a new instance of a @class{g-object} subtype and sets its
    properties.
  @end{short}
  Construction parameters which are not explicitly specified are set to their
  default values.
  @begin[Note]{dictionary}
    In the Lisp implementation this function calls the method
    @code{make-instance} to create the new instance.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(g-object-new \"GtkButton\" :label \"text\" :margin 6)
=> #<GTK-BUTTON {D941381@}>
    @end{pre}
    This is equivalent to:
    @begin{pre}
(make-instance 'gtk-button :label \"text\" :margin 6)
=> #<GTK-BUTTON {D947381@}>
    @end{pre}
  @end{dictionary}
  @see-class{g-object}
  @see-class{g-type}"
  (let ((lisp-type (gethash object-type *registered-object-types*)))
    (apply 'make-instance (cons lisp-type args))))

(export 'g-object-new)

;;; ----------------------------------------------------------------------------
;;; g_object_new_with_properties ()
;;;
;;; GObject *
;;; g_object_new_with_properties (GType object_type,
;;;                               guint n_properties,
;;;                               const char *names[],
;;;                               const GValue values[]);
;;;
;;; Creates a new instance of a GObject subtype and sets its properties using
;;; the provided arrays. Both arrays must have exactly n_properties elements,
;;; and the names and values correspond by index.
;;;
;;; Construction parameters (see G_PARAM_CONSTRUCT, G_PARAM_CONSTRUCT_ONLY)
;;; which are not explicitly specified are set to their default values.
;;;
;;; object_type :
;;;     the object type to instantiate
;;;
;;; n_properties :
;;;     the number of properties
;;;
;;; names :
;;;     the names of each property to be set.
;;;
;;; values :
;;;     the values of each property to be set.
;;;
;;; Returns :
;;;     a new instance of object_type .
;;;
;;; Since 2.54
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_newv ()                                       not exported
;;; ----------------------------------------------------------------------------

;; This function is called internally in the Lisp library to create an object
;; and is not exported.

(defcfun ("g_object_newv" %g-object-newv) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-10-27}
  @argument[object-type]{the type ID of the @class{g-object} subtype to
    instantiate}
  @argument[n-parameters]{the length of the parameters array}
  @argument[parameters]{an array of @symbol{g-parameter}}
  @return{A new instance of @arg{object-type}.}
  @begin{short}
    Creates a new instance of a @class{g-object} subtype and sets its
    properties.
  @end{short}

  Construction parameters, see @code{:construct} and @code{:construct-only} of
  type @symbol{g-param-spec-flags}, which are not explicitly specified are set
  to their default values.
  @see-class{g-object}
  @see-symbol{g-parameter}
  @see-symbol{g-param-spec-flags}
  @see-function{g-object-new}"
  (object-type g-type)
  (n-parameter :uint)
  (parameters :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_ref ()                                        not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(defcfun ("g_object_ref" %g-object-ref) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2014-11-13}
  @argument[object]{a @class{g-object} instance}
  @return{The same @arg{object}.}
  @short{Increases the reference count of object.}
  @see-class{g-object}"
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_unref ()                                      not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(defcfun ("g_object_unref" %g-object-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-11-13}
  @argument[object]{a @class{g-object} instance}
  @begin{short}
    Decreases the reference count of @arg{object}.
  @end{short}
  When its reference count drops to 0, the object is finalized (i.e. its memory
  is freed).
  @see-class{g-object}"
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_ref_sink ()                                   not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(defcfun ("g_object_ref_sink" %g-object-ref-sink) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2020-2-17}
  @argument[object]{a @class{g-object} instance}
  @return{@arg{object}}
  @begin{short}
    Increase the reference count of @arg{object}, and possibly remove the
    floating reference, if @arg{object} has a floating reference.
  @end{short}
  In other words, if the @arg{object} is floating, then this call \"assumes
  ownership\" of the floating reference, converting it to a normal reference by
  clearing the floating flag while leaving the reference count unchanged. If
  the @arg{object} is not floating, then this call adds a new normal reference
  increasing the reference count by one.
  @see-class{g-object}"
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; g_set_object ()
;;;
;;; gboolean
;;; g_set_object (GObject **object_ptr,
;;;               GObject *new_object);
;;;
;;; Updates a GObject pointer to refer to new_object . It increments the
;;; reference count of new_object (if non-NULL), decrements the reference count
;;; of the current value of object_ptr (if non-NULL), and assigns new_object to
;;; object_ptr . The assignment is not atomic.
;;;
;;; object_ptr must not be NULL.
;;;
;;; A macro is also included that allows this function to be used without
;;; pointer casts. The function itself is static inline, so its address may
;;; vary between compilation units.
;;;
;;; One convenient usage of this function is in implementing property setters:
;;;
;;; void
;;; foo_set_bar (Foo *foo,
;;;              Bar *new_bar)
;;; {
;;;   g_return_if_fail (IS_FOO (foo));
;;;   g_return_if_fail (new_bar == NULL || IS_BAR (new_bar));
;;;
;;;   if (g_set_object (&foo->bar, new_bar))
;;;     g_object_notify (foo, "bar");
;;; }
;;;
;;; object_ptr :
;;;     a pointer to a GObject reference
;;;
;;; new_object :
;;;     a pointer to the new GObject to assign to it, or NULL to clear the
;;;     pointer.
;;;
;;; Returns :
;;;     TRUE if the value of object_ptr changed, FALSE otherwise
;;;
;;; Since 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_clear_object ()
;;;
;;; void g_clear_object (volatile GObject **object_ptr);
;;;
;;; Clears a reference to a GObject.
;;;
;;; object_ptr must not be NULL.
;;;
;;; If the reference is NULL then this function does nothing. Otherwise, the
;;; reference count of the object is decreased and the pointer is set to NULL.
;;;
;;; This function is threadsafe and modifies the pointer atomically, using
;;; memory barriers where needed.
;;;
;;; A macro is also included that allows this function to be used without
;;; pointer casts.
;;;
;;; object_ptr :
;;;     a pointer to a GObject reference
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INITIALLY_UNOWNED
;;;
;;; #define G_TYPE_INITIALLY_UNOWNED (g_initially_unowned_get_type())
;;;
;;; The type for GInitiallyUnowned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_is_floating ()                                not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(defcfun ("g_object_is_floating" %g-object-is-floating) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-2-17}
  @argument[object]{a @class{g-object} instance}
  @return{@em{True} if @arg{object} has a floating reference.}
  @begin{short}
    Checks whether @arg{object} has a floating reference.
  @end{short}
  @see-class{g-object}"
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_force_floating ()                             not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(defcfun ("g_object_force_floating" %g-object-force-floating) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-17}
  @argument[object]{a @class{g-object} instance}
  @begin{short}
    This function is intended for @class{g-object} implementations to re-enforce
    a floating object reference.
  @end{short}
  Doing this is seldom required: all @class{g-initially-unowned}'s are created
  with a floating reference which usually just needs to be sunken by calling
  the function @fun{g-object-ref-sink}.
  @see-class{g-object}
  @see-class{g-initially-unowned}
  @see-function{g-object-ref-sink}"
  (object :pointer))

;;; ----------------------------------------------------------------------------
;;; GWeakNotify ()
;;;
;;; void (*GWeakNotify) (gpointer data,
;;;                      GObject *where_the_object_was);
;;;
;;; A GWeakNotify function can be added to an object as a callback that gets
;;; triggered when the object is finalized. Since the object is already being
;;; finalized when the GWeakNotify is called, there's not much you could do with
;;; the object, apart from e.g. using its address as hash-index or the like.
;;;
;;; data :
;;;     data that was provided when the weak reference was established
;;;
;;; where_the_object_was :
;;;     the object being finalized
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_weak_ref ()                                   not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(defcfun ("g_object_weak_ref" %g-object-weak-ref) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-17}
  @argument[object]{@class{g-object} instance to reference weakly}
  @argument[notify]{callback to invoke before the @arg{object} is freed}
  @argument[data]{extra data to pass to @arg{notify}}
  @begin{short}
    Adds a weak reference callback to an object.
  @end{short}
  Weak references are used for notification when an object is finalized. They
  are called \"weak references\" because they allow you to safely hold a pointer
  to an object without calling @fun{g-object-ref} (@fun{g-object-ref} adds a
  strong reference, that is, forces the object to stay alive).

  Note that the weak references created by this method are not thread-safe:
  they cannot safely be used in one thread if the object's last
  @fun{g-object-unref} might happen in another thread. Use @class{g-weak-ref} if
  thread-safety is required.
  @see-class{g-object}
  @see-function{g-object-ref}
  @see-function{g-object-unref}"
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_weak_unref ()                                 not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(defcfun ("g_object_weak_unref" %g-object-weak-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-17}
  @argument[object]{@class{g-object} instance to remove a weak reference from}
  @argument[notify]{callback to search for}
  @argument[data]{data to search for}
  @begin{short}
    Removes a weak reference callback to an object.
  @end{short}
  @see-class{g-object}"
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_add_weak_pointer ()
;;;
;;; void g_object_add_weak_pointer (GObject *object,
;;;                                 gpointer *weak_pointer_location);
;;;
;;; Adds a weak reference from weak_pointer to object to indicate that the
;;; pointer located at weak_pointer_location is only valid during the lifetime
;;; of object. When the object is finalized, weak_pointer will be set to NULL.
;;;
;;; Note that as with g_object_weak_ref(), the weak references created by this
;;; method are not thread-safe: they cannot safely be used in one thread if the
;;; object's last g_object_unref() might happen in another thread. Use GWeakRef
;;; if thread-safety is required.
;;;
;;; object :
;;;     The object that should be weak referenced.
;;;
;;; weak_pointer_location :
;;;     The memory address of a pointer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_remove_weak_pointer ()
;;;
;;; void g_object_remove_weak_pointer (GObject *object,
;;;                                    gpointer *weak_pointer_location);
;;;
;;; Removes a weak reference from object that was previously added using
;;; g_object_add_weak_pointer(). The weak_pointer_location has to match the one
;;; used with g_object_add_weak_pointer().
;;;
;;; object :
;;;     The object that is weak referenced.
;;;
;;; weak_pointer_location :
;;;     The memory address of a pointer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_set_weak_pointer ()
;;;
;;; gboolean
;;; g_set_weak_pointer (gpointer *weak_pointer_location,
;;;                     GObject *new_object);
;;;
;;; Updates a pointer to weakly refer to new_object . It assigns new_object to
;;; weak_pointer_location and ensures that weak_pointer_location will
;;; automaticaly be set to NULL if new_object gets destroyed. The assignment is
;;; not atomic. The weak reference is not thread-safe, see
;;; g_object_add_weak_pointer() for details.
;;;
;;; weak_pointer_location must not be NULL.
;;;
;;; A macro is also included that allows this function to be used without
;;; pointer casts. The function itself is static inline, so its address may
;;; vary between compilation units.
;;;
;;; One convenient usage of this function is in implementing property setters:
;;;
;;; void
;;; foo_set_bar (Foo *foo,
;;;              Bar *new_bar)
;;; {
;;;   g_return_if_fail (IS_FOO (foo));
;;;   g_return_if_fail (new_bar == NULL || IS_BAR (new_bar));
;;;
;;;   if (g_set_weak_pointer (&foo->bar, new_bar))
;;;     g_object_notify (foo, "bar");
;;; }
;;;
;;; weak_pointer_location :
;;;     the memory address of a pointer
;;;
;;; new_object :
;;;     a pointer to the new GObject to assign to it, or NULL to clear the
;;;     pointer.
;;;
;;; Returns :
;;;     TRUE if the value of weak_pointer_location changed, FALSE otherwise
;;;
;;; Since 2.56
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_clear_weak_pointer ()
;;;
;;; void
;;; g_clear_weak_pointer (gpointer *weak_pointer_location);
;;;
;;; Clears a weak reference to a GObject.
;;;
;;; weak_pointer_location must not be NULL.
;;;
;;; If the weak reference is NULL then this function does nothing. Otherwise,
;;; the weak reference to the object is removed for that location and the
;;; pointer is set to NULL.
;;;
;;; A macro is also included that allows this function to be used without
;;; pointer casts. The function itself is static inline, so its address may
;;; vary between compilation units.
;;;
;;; weak_pointer_location :
;;;     The memory address of a pointer
;;;
;;; Since 2.56
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GToggleNotify ()
;;;
;;; void (*GToggleNotify) (gpointer data,
;;;                        GObject *object,
;;;                        gboolean is_last_ref);
;;;
;;; A callback function used for notification when the state of a toggle
;;; reference changes. See g_object_add_toggle_ref().
;;;
;;; data :
;;;     Callback data passed to g_object_add_toggle_ref()
;;;
;;; object :
;;;     The object on which g_object_add_toggle_ref() was called.
;;;
;;; is_last_ref :
;;;     TRUE if the toggle reference is now the last reference to the object.
;;;     FALSE if the toggle reference was the last reference and there are now
;;;     other references.
;;; ----------------------------------------------------------------------------

(defcallback g-toggle-notify :void
    ((data :pointer) (object :pointer) (is-last-ref :boolean))
  (declare (ignore data))
  (log-for :gc "~A is now ~A with ~A refs~%"
           object
           (if is-last-ref "weak pointer" "strong pointer")
           (ref-count object))
  (log-for :gc "obj: ~A~%"
           (or (gethash (pointer-address object) *foreign-gobjects-strong*)
               (gethash (pointer-address object) *foreign-gobjects-weak*)))
  (if is-last-ref
      (let* ((obj-adr (pointer-address object))
             (obj (gethash obj-adr *foreign-gobjects-strong*)))
        (if obj
            (progn
              (remhash obj-adr *foreign-gobjects-strong*)
              (setf (gethash obj-adr *foreign-gobjects-weak*) obj))
            (progn
              (log-for :gc "GObject at ~A has no lisp-side (strong) reference"
                        object)
              (warn "GObject at ~A has no lisp-side (strong) reference"
                    object))))
      (let* ((obj-adr (pointer-address object))
             (obj (gethash obj-adr *foreign-gobjects-weak*)))
        (unless obj
          (log-for :gc "GObject at ~A has no lisp-side (weak) reference"
                   object)
          (warn "GObject at ~A has no lisp-side (weak) reference" object))
        (remhash obj-adr *foreign-gobjects-weak*)
        (setf (gethash obj-adr *foreign-gobjects-strong*) obj))))

;;; ----------------------------------------------------------------------------
;;; g_object_add_toggle_ref ()                             not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(defcfun ("g_object_add_toggle_ref" %g-object-add-toggle-ref) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-17}
  @argument[object]{a @class{g-object} instance}
  @argument[notify]{a function to call when this reference is the last reference
    to the @arg{object}, or is no longer the last reference}
  @argument[data]{data to pass to notify}
  @begin{short}
    Increases the reference count of the @arg{object} by one and sets a callback
    to be called when all other references to the @arg{object} are dropped, or
    when this is already the last reference to the @arg{object} and another
    reference is established.
  @end{short}

  This functionality is intended for binding @arg{object} to a proxy object
  managed by another memory manager. This is done with two paired references:
  the strong reference added by the @sym{g-object-add-toggle-ref} function and a
  reverse reference to the proxy object which is either a strong reference or
  weak reference.

  The setup is that when there are no other references to @arg{object}, only a
  weak reference is held in the reverse direction from @arg{object} to the proxy
  object, but when there are other references held to @arg{object}, a strong
  reference is held. The @arg{notify} callback is called when the reference from
  @arg{object} to the proxy object should be toggled from strong to weak
  (@code{is_last_ref} @em{true}) or weak to strong (@code{is_last_ref}
  @code{nil}).

  Since a (normal) reference must be held to the object before calling the
  @sym{g-object-add-toggle-ref} function, the initial state of the reverse link
  is always strong.

  Multiple toggle references may be added to the same gobject, however if
  there are multiple toggle references to an object, none of them will ever be
  notified until all but one are removed. For this reason, you should only
  ever use a toggle reference if there is important state in the proxy object.
  @see-class{g-object}"
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_remove_toggle_ref ()                          not exported
;;; ----------------------------------------------------------------------------

;; The memory management is done in the Lisp library. We do not export this
;; function.

(defcfun ("g_object_remove_toggle_ref" %g-object-remove-toggle-ref) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-17}
  @argument[object]{a @class{g-object} instance}
  @argument[notify]{a function to call when this reference is the last reference
    to the @arg{object}, or is no longer the last reference}
  @argument[data]{data to pass to @arg{notify}}
  @begin{short}
    Removes a reference added with the @fun{g-object-add-toggle-ref} function.
    The reference count of the @arg{object} is decreased by one.
  @end{short}
  @see-class{g-object}
  @see-function{g-object-add-toggle-ref}"
  (object :pointer)
  (notify :pointer)
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_object_connect ()
;;;
;;; gpointer g_object_connect (gpointer object,
;;;                            const gchar *signal_spec,
;;;                            ...);
;;;
;;; A convenience function to connect multiple signals at once.
;;;
;;; The signal specs expected by this function have the form
;;; "modifier::signal_name", where modifier can be one of the following:
;;;
;;; signal
;;;
;;;     equivalent to g_signal_connect_data (..., NULL, 0)
;;;
;;; object_signal, object-signal
;;;
;;;     equivalent to g_signal_connect_object (..., 0)
;;;
;;; swapped_signal, swapped-signal
;;;
;;;     equivalent to g_signal_connect_data (..., NULL, G_CONNECT_SWAPPED)
;;;
;;; swapped_object_signal, swapped-object-signal
;;;
;;;     equivalent to g_signal_connect_object (..., G_CONNECT_SWAPPED)
;;;
;;; signal_after, signal-after
;;;
;;;     equivalent to g_signal_connect_data (..., NULL, G_CONNECT_AFTER)
;;;
;;; object_signal_after, object-signal-after
;;;
;;;     equivalent to g_signal_connect_object (..., G_CONNECT_AFTER)
;;;
;;; swapped_signal_after, swapped-signal-after
;;;
;;;     equivalent to g_signal_connect_data (..., NULL,
;;;                                          G_CONNECT_SWAPPED |
;;;                                          G_CONNECT_AFTER)
;;;
;;; swapped_object_signal_after, swapped-object-signal-after
;;;
;;;   equivalent to g_signal_connect_object (..., G_CONNECT_SWAPPED |
;;;                                               G_CONNECT_AFTER)
;;;
;;;   menu->toplevel = g_object_connect (g_object_new (GTK_TYPE_WINDOW,
;;;                              "type", GTK_WINDOW_POPUP,
;;;                              "child", menu,
;;;                              NULL),
;;;                  "signal::event", gtk_menu_window_event, menu,
;;;                  "signal::size_request", gtk_menu_window_size_request, menu,
;;;                  "signal::destroy", gtk_widget_destroyed, &menu->toplevel,
;;;                  NULL);
;;;
;;; object :
;;;     a GObject
;;;
;;; signal_spec :
;;;     the spec for the first signal
;;;
;;; ... :
;;;     GCallback for the first signal, followed by data for the first signal,
;;;     followed optionally by more signal spec/callback/data triples, followed
;;;     by NULL
;;;
;;; Returns :
;;;     object
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_disconnect ()
;;;
;;; void g_object_disconnect (gpointer object,
;;;                           const gchar *signal_spec,
;;;                           ...);
;;;
;;; A convenience function to disconnect multiple signals at once.
;;;
;;; The signal specs expected by this function have the form "any_signal", which
;;; means to disconnect any signal with matching callback and data, or
;;; "any_signal::signal_name", which only disconnects the signal named
;;; "signal_name".
;;;
;;; object :
;;;     a GObject
;;;
;;; signal_spec :
;;;     the spec for the first signal
;;;
;;; ... :
;;;     GCallback for the first signal, followed by data for the first signal,
;;;     followed optionally by more signal spec/callback/data triples, followed
;;;     by NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_set ()
;;;
;;; void g_object_set (gpointer object, const gchar *first_property_name, ...);
;;;
;;; Sets properties on an object.
;;;
;;; object :
;;;     a GObject
;;;
;;; first_property_name :
;;;     name of the first property to set
;;;
;;; ... :
;;;     value for the first property, followed optionally by more name/value
;;;     pairs, followed by NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_setv ()
;;;
;;; void
;;; g_object_setv (GObject *object,
;;;                guint n_properties,
;;;                const gchar *names[],
;;;                const GValue values[]);
;;;
;;; Sets n_properties properties for an object . Properties to be set will be
;;; taken from values . All properties must be valid. Warnings will be emitted
;;; and undefined behaviour may result if invalid properties are passed in.
;;;
;;; object :
;;;     a GObject
;;;
;;; n_properties :
;;;     the number of properties
;;;
;;; names :
;;;     the names of each property to be set.
;;;
;;; values :
;;;     the values of each property to be set.
;;;
;;; Since 2.54
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_get ()
;;;
;;; void g_object_get (gpointer object, const gchar *first_property_name, ...);
;;;
;;; Gets properties of an object.
;;;
;;; In general, a copy is made of the property contents and the caller is
;;; responsible for freeing the memory in the appropriate manner for the type,
;;; for instance by calling g_free() or g_object_unref().
;;;
;;; Example 2. Using g_object_get()
;;;
;;; An example of using g_object_get() to get the contents of three properties -
;;; one of type G_TYPE_INT, one of type G_TYPE_STRING, and one of type
;;; G_TYPE_OBJECT:
;;;
;;;   gint intval;
;;;   gchar *strval;
;;;   GObject *objval;
;;;
;;;   g_object_get (my_object,
;;;                 "int-property", &intval,
;;;                 "str-property", &strval,
;;;                 "obj-property", &objval,
;;;                 NULL);
;;;
;;;   // Do something with intval, strval, objval
;;;
;;;   g_free (strval);
;;;   g_object_unref (objval);
;;;
;;; object :
;;;     a GObject
;;;
;;; first_property_name :
;;;     name of the first property to get
;;;
;;; ... :
;;;     return location for the first property, followed optionally by more
;;;     name/return location pairs, followed by NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_getv ()
;;;
;;; void
;;; g_object_getv (GObject *object,
;;;                guint n_properties,
;;;                const gchar *names[],
;;;                GValue values[]);
;;;
;;; Gets n_properties properties for an object . Obtained properties will be set
;;; to values . All properties must be valid. Warnings will be emitted and
;;; undefined behaviour may result if invalid properties are passed in.
;;;
;;; object :
;;;     a GObject
;;;
;;; n_properties :
;;;     the number of properties
;;;
;;; names :
;;;     the names of each property to get.
;;;
;;; values :
;;;     the values of each property to get.
;;;
;;; Since 2.54
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_notify ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_notify" g-object-notify) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-16}
  @argument[object]{a @class{g-object} instance}
  @argument[property-name]{a string with the name of a property installed on
    the class of @arg{object}}
  @begin{short}
    Emits a \"notify\" signal for the property @arg{property-name} on the
    object.
  @end{short}
  @see-class{g-object}"
  (object g-object)
  (property-name :string))

(export 'g-object-notify)

;;; ----------------------------------------------------------------------------
;;; g_object_notify_by_pspec ()
;;;
;;; void g_object_notify_by_pspec (GObject *object, GParamSpec *pspec);
;;;
;;; Emits a "notify" signal for the property specified by pspec on object.
;;;
;;; This function omits the property name lookup, hence it is faster than
;;; g_object_notify().
;;;
;;; One way to avoid using g_object_notify() from within the class that
;;; registered the properties, and using g_object_notify_by_pspec() instead, is
;;; to store the GParamSpec used with g_object_class_install_property() inside a
;;; static array, e.g.:
;;;
;;;   enum
;;;   {
;;;     PROP_0,
;;;     PROP_FOO,
;;;     PROP_LAST
;;;   };
;;;
;;;   static GParamSpec *properties[PROP_LAST];
;;;
;;;   static void
;;;   my_object_class_init (MyObjectClass *klass)
;;;   {
;;;     properties[PROP_FOO] = g_param_spec_int ("foo", "Foo", "The foo",
;;;                                              0, 100,
;;;                                              50,
;;;                                              G_PARAM_READWRITE);
;;;     g_object_class_install_property (gobject_class,
;;;                                      PROP_FOO,
;;;                                      properties[PROP_FOO]);
;;;   }
;;;
;;; and then notify a change on the "foo" property with:
;;;
;;;   g_object_notify_by_pspec (self, properties[PROP_FOO]);
;;;
;;; object :
;;;     a GObject
;;;
;;; pspec :
;;;     the GParamSpec of a property installed on the class of object.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_freeze_notify ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_freeze_notify" g-object-freeze-notify) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-10-17}
  @argument[object]{a @class{g-object} instance}
  @begin{short}
    Increases the freeze count on the object.
  @end{short}
  If the freeze count is non-zero, the emission of \"notify\" signals on the
  object is stopped. The signals are queued until the freeze count is decreased
  to zero.

  This is necessary for accessors that modify multiple properties to prevent
  premature notification while the object is still being modified.
  @see-class{g-object}
  @see-function{g-object-thaw-notify}"
  (object g-object))

(export 'g-object-freeze-notify)

;;; ----------------------------------------------------------------------------
;;; g_object_thaw_notify ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_thaw_notify" g-object-thaw-notify) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-11-16}
  @argument[object]{a @class{g-object} instance}
  @begin{short}
    Reverts the effect of a previous call to the function
    @fun{g-object-freeze-notify}.
  @end{short}
  The freeze count is decreased on the object and when it reaches zero, all
  queued \"notify\" signals are emitted.

  It is an error to call this function when the freeze count is zero.
  @see-class{g-object}
  @see-function{g-object-freeze-notify}"
  (object g-object))

(export 'g-object-thaw-notify)

;;; ----------------------------------------------------------------------------
;;; g_object_get_data ()
;;; g_object_set_data () -> g-object-data
;;; ----------------------------------------------------------------------------

(defun (setf g-object-data) (data object key)
  (foreign-funcall "g_object_set_data"
                   g-object object
                   :string key
                   :pointer data
                   :void)
  data)

(defcfun ("g_object_get_data" g-object-data) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2020-9-23}
  @syntax[]{(g-object-data object key) => data}
  @syntax[]{(setf (g-object-data object key) data)}
  @argument[object]{a @class{g-object} instance containing the associations}
  @argument[key]{a string with the name of the key}
  @argument[data]{a pointer as data to associate with that key}
  @begin{short}
    Each object carries around a table of associations from strings to pointers.
  @end{short}
  The function @sym{g-object-data} gets a named field from the objects table of
  associations. The function @sym{(setf g-object-data)} sets an association.

  If @arg{object} already had an association with that name, the old
  association will be destroyed.
  @begin[Examples]{dictionary}
    Set an integer as a pointer for a property on a GtkButton.
    @begin{pre}
  (defvar button (make-instance 'gtk-button))
=> BUTTON
  (setf (g-object-data button \"property\") (make-pointer 100))
=> #.(SB-SYS:INT-SAP #X00000064)
  (g-object-data button \"property\")
=> #.(SB-SYS:INT-SAP #X00000064)
  (pointer-address *)
=> 100
    @end{pre}
  @end{dictionary}
  @see-class{g-object}"
  (object g-object)
  (key :string))

(export 'g-object-data)

;;; ----------------------------------------------------------------------------
;;; GDestroyNotify ()  from the GLIB documentation
;;;
;;; void
;;; (*GDestroyNotify) (gpointer data);
;;;
;;; Specifies the type of function which is called when a data element is
;;; destroyed. It is passed the pointer to the data element and should free any
;;; memory and resources allocated for it.
;;;
;;; data :
;;;     the data element.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_set_data_full ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_set_data_full" g-object-set-data-full) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-30}
  @argument[object]{@class{g-object} instance containing the associations}
  @argument[key]{a string with the name of the key}
  @argument[data]{data as a pointer to associate with that @arg{key}}
  @argument[destroy]{callback function to call when the association is
    destroyed}
  @begin{short}
    Like the function @fun{g-object-data} except it adds notification for
    when the association is destroyed, either by setting it to a different
    value or when the object is destroyed.
  @end{short}

  Note that the @arg{destroy} callback is not called if @arg{data} is
  @code{nil}.
  @begin[Note]{dictionary}
    You must define a @code{callback}-function and pass the pointer of the
    callback function. See the example for the syntax to use.
  @end{dictionary}
  @begin[Examples]{dictionary}
    This code example shows the usage of the function
    @sym{g-object-set-data-full} with a destroy notify handler. The code is
    taken from the @code{gtk-widget-factory} demo.
    @begin{pre}
;; Callback function for the destroy notify handler
(defcallback pulse-remove :void ((data :pointer))
  (g-source-remove (pointer-address data)))

;; Function called by the timeout handler
(defun pulse-it (widget)
  ;; Pulse the widget which is an entry or a progress bar
  (if (eq 'gtk-entry (type-of widget))
      (gtk-entry-progress-pulse widget)
      (gtk-progress-bar-pulse widget))
  ;; Set a timeout handler and store the handler on the property list
  (g-object-set-data-full widget
                          \"pulse-id\"
                          (make-pointer (g-timeout-add *pulse-time*
                                                       (lambda ()
                                                         (pulse-it widget))))
                          (callback pulse-remove))
  ;; Remove the source
  +g-source-remove+)
    @end{pre}
  @end{dictionary}
  @see-class{g-object}
  @see-function{g-object-data}"
  (object g-object)
  (key :string)
  (data :pointer)
  (destroy :pointer))

(export 'g-object-set-data-full)

;;; ----------------------------------------------------------------------------
;;; g_object_steal_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_steal_data" g-object-steal-data) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2020-10-17}
  @argument[object]{@class{g-object} instance containing the associations}
  @argument[key]{a string with the name of the key}
  @return{The data as a pointer if found, or @code{nil} if no such data exists.}
  @begin{short}
    Remove a specified datum from the @arg{object}'s data associations, without
    invoking the association's destroy handler.
  @end{short}
  @see-class{g-object}
  @see-function{g-object-data}
  @see-function{g-object-set-data-full}"
  (object g-object)
  (key :string))

(export 'g-object-steal-data)

;;;-----------------------------------------------------------------------------
;;; g_object_dup_data ()
;;;
;;; gpointer g_object_dup_data (GObject *object,
;;;                             const gchar *key,
;;;                             GDuplicateFunc dup_func,
;;;                             gpointer user_data);
;;;
;;; This is a variant of g_object_get_data() which returns a 'duplicate' of the
;;; value. dup_func defines the meaning of 'duplicate' in this context, it could
;;; e.g. take a reference on a ref-counted object.
;;;
;;; If the key is not set on the object then dup_func will be called with a NULL
;;; argument.
;;;
;;; Note that dup_func is called while user data of object is locked.
;;;
;;; This function can be useful to avoid races when multiple threads are using
;;; object data on the same key on the same object.
;;;
;;; object :
;;;     the GObject to store user data on
;;;
;;; key :
;;;     a string, naming the user data pointer
;;;
;;; dup_func :
;;;     function to dup the value. [allow-none]
;;;
;;; user_data :
;;;     passed as user_data to dup_func. [allow-none]
;;;
;;; Returns :
;;;     the result of calling dup_func on the value associated with key on
;;;     object, or NULL if not set. If dup_func is NULL, the value is returned
;;;     unmodified.
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_replace_data ()
;;;
;;; gboolean g_object_replace_data (GObject *object,
;;;                                 const gchar *key,
;;;                                 gpointer oldval,
;;;                                 gpointer newval,
;;;                                 GDestroyNotify destroy,
;;;                                 GDestroyNotify *old_destroy);
;;;
;;; Compares the user data for the key key on object with oldval, and if they
;;; are the same, replaces oldval with newval.
;;;
;;; This is like a typical atomic compare-and-exchange operation, for user data
;;; on an object.
;;;
;;; If the previous value was replaced then ownership of the old value (oldval)
;;; is passed to the caller, including the registred destroy notify for it
;;; (passed out in old_destroy). Its up to the caller to free this as he wishes,
;;; which may or may not include using old_destroy as sometimes replacement
;;; should not destroy the object in the normal way.
;;;
;;; Return: TRUE if the existing value for key was replaced by newval, FALSE
;;; otherwise.
;;;
;;; object :
;;;     the GObject to store user data on
;;;
;;; key :
;;;     a string, naming the user data pointer
;;;
;;; oldval :
;;;     the old value to compare against. [allow-none]
;;;
;;; newval :
;;;     the new value. [allow-none]
;;;
;;; destroy :
;;;     a destroy notify for the new value. [allow-none]
;;;
;;; old_destroy :
;;;     destroy notify for the existing value. [allow-none]
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_get_qdata ()
;;;
;;; gpointer g_object_get_qdata (GObject *object, GQuark quark);
;;;
;;; This function gets back user data pointers stored via g_object_set_qdata().
;;;
;;; object :
;;;     The GObject to get a stored user data pointer from
;;;
;;; quark :
;;;     A GQuark, naming the user data pointer
;;;
;;; Returns :
;;;     The user data pointer set, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_set_qdata ()
;;;
;;; void g_object_set_qdata (GObject *object, GQuark quark, gpointer data);
;;;
;;; This sets an opaque, named pointer on an object. The name is specified
;;; through a GQuark (retrived e.g. via g_quark_from_static_string()), and the
;;; pointer can be gotten back from the object with g_object_get_qdata() until
;;; the object is finalized. Setting a previously set user data pointer,
;;; overrides (frees) the old pointer set, using NULL as pointer essentially
;;; removes the data stored.
;;;
;;; object :
;;;     The GObject to set store a user data pointer
;;;
;;; quark :
;;;     A GQuark, naming the user data pointer
;;;
;;; data :
;;;     An opaque user data pointer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_set_qdata_full ()
;;;
;;; void g_object_set_qdata_full (GObject *object,
;;;                               GQuark quark,
;;;                               gpointer data,
;;;                               GDestroyNotify destroy);
;;;
;;; This function works like g_object_set_qdata(), but in addition, a
;;; void (*destroy) (gpointer) function may be specified which is called with
;;; data as argument when the object is finalized, or the data is being
;;; overwritten by a call to g_object_set_qdata() with the same quark.
;;;
;;; object :
;;;     The GObject to set store a user data pointer
;;;
;;; quark :
;;;     A GQuark, naming the user data pointer
;;;
;;; data :
;;;     An opaque user data pointer
;;;
;;; destroy :
;;;     Function to invoke with data as argument, when data needs to be freed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_steal_qdata ()
;;;
;;; gpointer g_object_steal_qdata (GObject *object, GQuark quark);
;;;
;;; This function gets back user data pointers stored via g_object_set_qdata()
;;; and removes the data from object without invoking its destroy() function (if
;;; any was set). Usually, calling this function is only required to update user
;;; data pointers with a destroy notifier, for example:
;;;
;;;   void
;;;   object_add_to_user_list (GObject     *object,
;;;                            const gchar *new_string)
;;;   {
;;;     // the quark, naming the object data
;;;     GQuark quark_string_list = g_quark_from_static_string("my-string-list");
;;;     // retrive the old string list
;;;     GList *list = g_object_steal_qdata (object, quark_string_list);
;;;
;;;     // prepend new string
;;;     list = g_list_prepend (list, g_strdup (new_string));
;;;     // this changed 'list', so we need to set it again
;;;     g_object_set_qdata_full (object, quark_string_list, list,
;;;                              free_string_list);
;;;   }
;;;   static void
;;;   free_string_list (gpointer data)
;;;   {
;;;     GList *node, *list = data;
;;;
;;;     for (node = list; node; node = node->next)
;;;       g_free (node->data);
;;;     g_list_free (list);
;;;   }
;;;
;;; Using g_object_get_qdata() in the above example, instead of
;;; g_object_steal_qdata() would have left the destroy function set, and thus
;;; the partial string list would have been freed upon
;;; g_object_set_qdata_full().
;;;
;;; object :
;;;     The GObject to get a stored user data pointer from
;;;
;;; quark :
;;;     A GQuark, naming the user data pointer
;;;
;;; Returns :
;;;     The user data pointer set, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_dup_qdata ()
;;;
;;; gpointer g_object_dup_qdata (GObject *object,
;;;                              GQuark quark,
;;;                              GDuplicateFunc dup_func,
;;;                              gpointer user_data);
;;;
;;; This is a variant of g_object_get_qdata() which returns a 'duplicate' of the
;;; value. dup_func defines the meaning of 'duplicate' in this context, it could
;;; e.g. take a reference on a ref-counted object.
;;;
;;; If the quark is not set on the object then dup_func will be called with a
;;; NULL argument.
;;;
;;; Note that dup_func is called while user data of object is locked.
;;;
;;; This function can be useful to avoid races when multiple threads are using
;;; object data on the same key on the same object.
;;;
;;; object :
;;;     the GObject to store user data on
;;;
;;; quark :
;;;     a GQuark, naming the user data pointer
;;;
;;; dup_func :
;;;     function to dup the value. [allow-none]
;;;
;;; user_data :
;;;     passed as user_data to dup_func. [allow-none]
;;;
;;; Returns :
;;;     the result of calling dup_func on the value associated with quark on
;;;     object, or NULL if not set. If dup_func is NULL, the value is returned
;;;     unmodified.
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_replace_qdata ()
;;;
;;; gboolean g_object_replace_qdata (GObject *object,
;;;                                  GQuark quark,
;;;                                  gpointer oldval,
;;;                                  gpointer newval,
;;;                                  GDestroyNotify destroy,
;;;                                  GDestroyNotify *old_destroy);
;;;
;;; Compares the user data for the key quark on object with oldval, and if they
;;; are the same, replaces oldval with newval.
;;;
;;; This is like a typical atomic compare-and-exchange operation, for user data
;;; on an object.
;;;
;;; If the previous value was replaced then ownership of the old value (oldval)
;;; is passed to the caller, including the registred destroy notify for it
;;; (passed out in old_destroy). Its up to the caller to free this as he wishes,
;;; which may or may not include using old_destroy as sometimes replacement
;;; should not destroy the object in the normal way.
;;;
;;; Return: TRUE if the existing value for quark was replaced by newval, FALSE
;;; otherwise.
;;;
;;; object :
;;;     the GObject to store user data on
;;;
;;; quark :
;;;     a GQuark, naming the user data pointer
;;;
;;; oldval :
;;;     the old value to compare against. [allow-none]
;;;
;;; newval :
;;;     the new value. [allow-none]
;;;
;;; destroy :
;;;     a destroy notify for the new value. [allow-none]
;;;
;;; old_destroy :
;;;     destroy notify for the existing value. [allow-none]
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_set_property ()
;;; g_object_get_property () -> g-object-property
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_set_property" %g-object-set-property) :void
  (object g-object)
  (property-name :string)
  (value (:pointer (:struct g-value))))

(defun (setf g-object-property) (value object property-name
                                       &optional property-type)
  (unless property-type
    (setf property-type
          (class-property-type (g-type-from-instance object)
                               property-name
                               :assert-writable t)))
  (with-foreign-object (new-value '(:struct g-value))
    (unwind-protect
      (progn
        (set-g-value new-value value property-type :zero-g-value t)
        (%g-object-set-property object property-name new-value))
      (g-value-unset new-value)))
  value)

(defcfun ("g_object_get_property" %g-object-get-property) :void
  (object g-object)
  (property-name :string)
  (value (:pointer (:struct g-value))))

(defun g-object-property (object property-name &optional property-type)
 #+cl-cffi-gtk-documentation
 "@version{2020-10-17}
  @syntax[]{(g-object-property object property-name property-type) => value}
  @syntax[]{(setf (g-object-property object property-name property-type) value)}
  @argument[object]{a @class{g-object} instance}
  @argument[property-name]{a string with the name of the property}
  @argument[property-type]{the optional @class{g-type} ID of the property}
  @argument[value]{a value for the property}
  @short{Accessor of the value of the property of an object.}
  @see-class{g-object}
  @see-class{g-type}"
  (restart-case
    (unless property-type
      (setf property-type
            (class-property-type (g-type-from-instance object)
                                 property-name
                                 :assert-readable t)))
    (return-nil () (return-from g-object-property nil)))
  (with-foreign-object (value '(:struct g-value))
    (unwind-protect
      (progn
        (g-value-init value property-type)
        (%g-object-get-property object property-name value)
        (parse-g-value value))
      (g-value-unset value))))

(export 'g-object-property)

;;; ----------------------------------------------------------------------------
;;; g_object_new_valist ()
;;;
;;; GObject * g_object_new_valist (GType object_type,
;;;                                const gchar *first_property_name,
;;;                                va_list var_args);
;;;
;;; Creates a new instance of a GObject subtype and sets its properties.
;;;
;;; Construction parameters (see G_PARAM_CONSTRUCT, G_PARAM_CONSTRUCT_ONLY)
;;; which are not explicitly specified are set to their default values.
;;;
;;; object_type :
;;;     the type id of the GObject subtype to instantiate
;;;
;;; first_property_name :
;;;     the name of the first property
;;;
;;; var_args :
;;;     the value of the first property, followed optionally by more name/value
;;;     pairs, followed by NULL
;;;
;;; Returns :
;;;     a new instance of object_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_set_valist ()
;;;
;;; void g_object_set_valist (GObject *object,
;;;                           const gchar *first_property_name,
;;;                           va_list var_args);
;;;
;;; Sets properties on an object.
;;;
;;; object :
;;;     a GObject
;;;
;;; first_property_name :
;;;     name of the first property to set
;;;
;;; var_args :
;;;     value for the first property, followed optionally by more name/value
;;;     pairs, followed by NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_get_valist ()
;;;
;;; void g_object_get_valist (GObject *object,
;;;                           const gchar *first_property_name,
;;;                           va_list var_args);
;;;
;;; Gets properties of an object.
;;;
;;; In general, a copy is made of the property contents and the caller is
;;; responsible for freeing the memory in the appropriate manner for the type,
;;; for instance by calling g_free() or g_object_unref().
;;;
;;; See g_object_get().
;;;
;;; object :
;;;     a GObject
;;;
;;; first_property_name :
;;;     name of the first property to get
;;;
;;; var_args :
;;;     return location for the first property, followed optionally by more
;;;     name/return location pairs, followed by NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_watch_closure ()
;;;
;;; void g_object_watch_closure (GObject *object, GClosure *closure);
;;;
;;; This function essentially limits the life time of the closure to the life
;;; time of the object. That is, when the object is finalized, the closure is
;;; invalidated by calling g_closure_invalidate() on it, in order to prevent
;;; invocations of the closure with a finalized (nonexisting) object. Also,
;;; g_object_ref() and g_object_unref() are added as marshal guards to the
;;; closure, to ensure that an extra reference count is held on object during
;;; invocation of the closure. Usually, this function will be called on closures
;;; that use this object as closure data.
;;;
;;; object :
;;;     GObject restricting lifetime of closure
;;;
;;; closure :
;;;     GClosure to watch
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_run_dispose ()
;;;
;;; void g_object_run_dispose (GObject *object);
;;;
;;; Releases all references to other objects. This can be used to break
;;; reference cycles.
;;;
;;; This functions should only be called from object system implementations.
;;;
;;; object :
;;;     a GObject
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_WARN_INVALID_PROPERTY_ID()
;;;
;;; #define G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec)
;;;
;;; This macro should be used to emit a standard warning about unexpected
;;; properties in set_property() and get_property() implementations.
;;;
;;; object :
;;;     the GObject on which set_property() or get_property() was called
;;;
;;; property_id :
;;;     the numeric id of the property
;;;
;;; pspec :
;;;     the GParamSpec of the property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GWeakRef
;;;
;;; typedef struct {
;;; } GWeakRef;
;;;
;;; A structure containing a weak reference to a GObject. It can either be empty
;;; (i.e. point to NULL), or point to an object for as long as at least one
;;; "strong" reference to that object exists. Before the object's
;;; GObjectClass.dispose method is called, every GWeakRef associated with
;;; becomes empty (i.e. points to NULL).
;;;
;;; Like GValue, GWeakRef can be statically allocated, stack- or heap-allocated,
;;; or embedded in larger structures.
;;;
;;; Unlike g_object_weak_ref() and g_object_add_weak_pointer(), this weak
;;; reference is thread-safe: converting a weak pointer to a reference is atomic
;;; with respect to invalidation of weak pointers to destroyed objects.
;;;
;;; If the object's GObjectClass.dispose method results in additional references
;;; to the object being held, any GWeakRefs taken before it was disposed will
;;; continue to point to NULL. If GWeakRefs are taken after the object is
;;; disposed and re-referenced, they will continue to point to it until its
;;; refcount goes back to zero, at which point they too will be invalidated.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_weak_ref_init ()
;;;
;;; void g_weak_ref_init (GWeakRef *weak_ref, gpointer object);
;;;
;;; Initialise a non-statically-allocated GWeakRef.
;;;
;;; This function also calls g_weak_ref_set() with object on the
;;; freshly-initialised weak reference.
;;;
;;; This function should always be matched with a call to g_weak_ref_clear(). It
;;; is not necessary to use this function for a GWeakRef in static storage
;;; because it will already be properly initialised. Just use g_weak_ref_set()
;;; directly.
;;;
;;; weak_ref :
;;;     uninitialized or empty location for a weak reference
;;;
;;; object :
;;;     a GObject or NULL
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_weak_ref_clear ()
;;;
;;; void g_weak_ref_clear (GWeakRef *weak_ref);
;;;
;;; Frees resources associated with a non-statically-allocated GWeakRef. After
;;; this call, the GWeakRef is left in an undefined state.
;;;
;;; You should only call this on a GWeakRef that previously had
;;; g_weak_ref_init() called on it.
;;;
;;; weak_ref :
;;;     location of a weak reference, which may be empty
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_weak_ref_get ()
;;;
;;; gpointer g_weak_ref_get (GWeakRef *weak_ref);
;;;
;;; If weak_ref is not empty, atomically acquire a strong reference to the
;;; object it points to, and return that reference.
;;;
;;; This function is needed because of the potential race between taking the
;;; pointer value and g_object_ref() on it, if the object was losing its last
;;; reference at the same time in a different thread.
;;;
;;; The caller should release the resulting reference in the usual way, by using
;;; g_object_unref().
;;;
;;; weak_ref :
;;;     location of a weak reference to a GObject
;;;
;;; Returns :
;;;     the object pointed to by weak_ref, or NULL if it was empty
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_weak_ref_set ()
;;;
;;; void g_weak_ref_set (GWeakRef *weak_ref, gpointer object);
;;;
;;; Change the object to which weak_ref points, or set it to NULL.
;;;
;;; You must own a strong reference on object while calling this function.
;;;
;;; weak_ref :
;;;     location for a weak reference
;;;
;;; object :
;;;     a GObject or NULL
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_assert_finalize_object ()
;;;
;;; void
;;; g_assert_finalize_object (GObject *object);
;;;
;;; Assert that object is non-NULL, then release one reference to it with
;;; g_object_unref() and assert that it has been finalized (i.e. that there are
;;; no more references).
;;;
;;; If assertions are disabled via G_DISABLE_ASSERT, this macro just calls
;;; g_object_unref() without any further checks.
;;;
;;; This macro should only be used in regression tests.
;;;
;;; object :
;;;     an object.
;;;
;;; Since 2.62
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.base.lisp ------------------------------------------
