;;; ----------------------------------------------------------------------------
;;; gobject.base.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GObject Reference Manual Version 2.32.4. See http://www.gtk.org
;;; The API documentation of the Lisp binding is available at
;;; http://www.crategus.com/books/cl-cffi-gtk/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; The base object type
;;;
;;; Synopsis
;;;
;;;    GParameter
;;;    GObject
;;;    GObjectClass
;;;    GObjectConstructParam
;;;
;;;    G_TYPE_IS_OBJECT
;;;    G_OBJECT
;;;    G_IS_OBJECT
;;;    G_OBJECT_CLASS
;;;    G_IS_OBJECT_CLASS
;;;    G_OBJECT_GET_CLASS
;;;    G_OBJECT_TYPE
;;;    G_OBJECT_TYPE_NAME
;;;    G_OBJECT_CLASS_TYPE
;;;    G_OBJECT_CLASS_NAME
;;;
;;;    g_object_class_install_property
;;;    g_object_class_install_properties
;;;    g_object_class_find_property
;;;    g_object_class_list_properties
;;;    g_object_class_override_property
;;;    g_object_interface_install_property
;;;    g_object_interface_find_property
;;;    g_object_interface_list_properties
;;;    g_object_new
;;;    g_object_newv
;;;
;;;    g_object_ref
;;;    g_object_unref
;;;    g_object_ref_sink
;;;    g_clear_object
;;;
;;;    GInitiallyUnowned
;;;    GInitiallyUnownedClass
;;;
;;;    G_TYPE_INITIALLY_UNOWNED
;;;
;;;    g_object_is_floating
;;;    g_object_force_floating
;;;    g_object_weak_ref
;;;    g_object_weak_unref
;;;    g_object_add_weak_pointer
;;;    g_object_remove_weak_pointer
;;;    g_object_add_toggle_ref
;;;    g_object_remove_toggle_ref
;;;    g_object_connect
;;;    g_object_disconnect
;;;    g_object_set
;;;    g_object_get
;;;    g_object_notify
;;;    g_object_notify_by_pspec
;;;    g_object_freeze_notify
;;;    g_object_thaw_notify
;;;    g_object_get_data
;;;    g_object_set_data
;;;    g_object_set_data_full
;;;    g_object_steal_data
;;;    g_object_get_qdata
;;;    g_object_set_qdata
;;;    g_object_set_qdata_full
;;;    g_object_steal_qdata
;;;    g_object_set_property
;;;    g_object_get_property
;;;    g_object_new_valist
;;;    g_object_set_valist
;;;    g_object_get_valist
;;;    g_object_watch_closure
;;;    g_object_run_dispose
;;;
;;;    G_OBJECT_WARN_INVALID_PROPERTY_ID
;;;
;;;    GWeakRef
;;;
;;;    g_weak_ref_init
;;;    g_weak_ref_clear
;;;    g_weak_ref_get
;;;    g_weak_ref_set
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GBinding
;;;    +----GTypeModule
;;;
;;; Signals
;;;
;;;   "notify"                                         : No Hooks
;;;
;;; Description
;;;
;;; GObject is the fundamental type providing the common attributes and methods
;;; for all object types in GTK+, Pango and other libraries based on GObject.
;;; The GObject class provides methods for object construction and destruction,
;;; property access methods, and signal support. Signals are described in detail
;;; in Signals(3).
;;;
;;; GInitiallyUnowned is derived from GObject. The only difference between the
;;; two is that the initial reference of a GInitiallyUnowned is flagged as a
;;; floating reference. This means that it is not specifically claimed to be
;;; "owned" by any code portion. The main motivation for providing floating
;;; references is C convenience. In particular, it allows code to be written as:
;;;
;;;   container = create_container ();
;;;   container_add_child (container, create_child());
;;;
;;; If container_add_child() will g_object_ref_sink() the passed in child, no
;;; reference of the newly created child is leaked. Without floating references,
;;; container_add_child() can only g_object_ref() the new child, so to implement
;;; this code without reference leaks, it would have to be written as:
;;;
;;;   Child *child;
;;;   container = create_container ();
;;;   child = create_child ();
;;;   container_add_child (container, child);
;;;   g_object_unref (child);
;;;
;;; The floating reference can be converted into an ordinary reference by
;;; calling g_object_ref_sink(). For already sunken objects (objects that don't
;;; have a floating reference anymore), g_object_ref_sink() is equivalent to
;;; g_object_ref() and returns a new reference. Since floating references are
;;; useful almost exclusively for C convenience, language bindings that provide
;;; automated reference and memory ownership maintenance (such as smart pointers
;;; or garbage collection) should not expose floating references in their API.
;;;
;;; Some object implementations may need to save an objects floating state
;;; across certain code portions (an example is GtkMenu), to achieve this, the
;;; following sequence can be used:
;;;
;;;   /* save floating state */
;;;   gboolean was_floating = g_object_is_floating (object);
;;;   g_object_ref_sink (object);
;;;   /* protected code portion */
;;;   ...;
;;;   /* restore floating state */
;;;   if (was_floating)
;;;     g_object_force_floating (object);
;;;   g_object_unref (object); /* release previously acquired reference */
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "notify" signal
;;;
;;; void user_function (GObject    *gobject,
;;;                     GParamSpec *pspec,
;;;                     gpointer    user_data)      : No Hooks
;;;
;;; The notify signal is emitted on an object when one of its properties has
;;; been changed. Note that getting this signal doesn't guarantee that the value
;;; of the property has actually changed, it may also be emitted when the setter
;;; for the property is called to reinstate the previous value.
;;;
;;; This signal is typically used to obtain change notification for a single
;;; property, by specifying the property name as a detail in the
;;; g_signal_connect() call, like this:
;;;
;;;   g_signal_connect (text_view->buffer, "notify::paste-target-list",
;;;                     G_CALLBACK (gtk_text_view_target_list_notify),
;;;                     text_view)
;;;
;;; It is important to note that you must use canonical parameter names as
;;; detail strings for the notify signal.
;;;
;;; gobject :
;;;     the object which received the signal.
;;;
;;; pspec :
;;;     the GParamSpec of the property which changed.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

(defvar *foreign-gobjects-weak*
        (make-weak-hash-table :test 'equal :weakness :value))
(defvar *foreign-gobjects-strong* (make-hash-table :test 'equal))
(defvar *current-creating-object* nil)
(defvar *current-object-from-pointer* nil)
(defvar *currently-making-object-p* nil)

(glib::at-finalize ()
  (clrhash *foreign-gobjects-weak*)
  (clrhash *foreign-gobjects-strong*)
  (setf *current-creating-object* nil
        *current-object-from-pointer* nil
        *currently-making-object-p* nil))

;;; ----------------------------------------------------------------------------

(defvar *registered-object-types* (make-hash-table :test 'equal))

(defun register-object-type (name type)
  (setf (gethash name *registered-object-types*) type))

(defun registered-object-type-by-name (name)
  (gethash name *registered-object-types*))

;;; ----------------------------------------------------------------------------
;;; struct GParameter
;;; ----------------------------------------------------------------------------

(defcstruct g-parameter
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:value g-value))

(export 'g-parameter)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-parameter atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'g-parameter atdoc:*external-symbols*)
 "@version{2013-2-16}
  @begin{short}
    The @sym{g-parameter} struct is an auxiliary structure used to hand
    parameter name/value pairs to @fun{g-object-newv}.
  @end{short}
  @begin{pre}
(defcstruct g-parameter
  (:name (:string :free-from-foreign nil :free-to-foreign nil))
  (:value g-value))
  @end{pre}
  @begin[code]{table}
    @entry[:name]{the parameter name}
    @entry[:value]{the parameter value}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; struct GObject
;;;
;;; struct GObject;
;;;
;;; All the fields in the GObject structure are private to the GObject
;;; implementation and should never be accessed directly.
;;; ----------------------------------------------------------------------------

;; %g-object is not needed for the implementation.
;; It is defined to access the property ref-count for debugging the code.

(defcstruct %g-object
  (:type-instance g-type-instance)
  (:ref-count :uint)
  (:data :pointer))

;(export '%g-object)

;; Accessor for the slot ref-count of %g-object

(defun ref-count (pointer)
  (foreign-slot-value (if (pointerp pointer)
                          pointer
                          (pointer pointer))
                      '%g-object :ref-count))

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
 "@version{2013-2-16}
  @begin{short}
    @sym{g-object} is the fundamental type providing the common attributes and
    methods for all object types in GTK+, Pango and other libraries based on
    @sym{g-object}.
  @end{short}
  The @sym{g-object} class provides methods for object construction and
  destruction, property access methods, and signal support. Signals are
  described in detail in Signals(3).

  All the fields in the GObject structure are private to the GObject
  implementation and should never be accessed directly.
  @begin[Lisp Implementation]{dictionary}
    In the Lisp implementation three slots are added. The slot @code{pointer}
    holds the pointer to the C instance of the object. The slot
    @code{signal-handlers} stores all signals which are connected to an
    instance. The slot @code{has-reference} is initialized to the value @code{t}
    during creation of an object, but is not in use in the code. See the
    slot access functions for examples.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"notify\" signal}
    The \"notify\" signal is emitted on an object when one of its properties has
    been changed. Note that getting this signal doesn't guarantee that the value
    of the property has actually changed, it may also be emitted when the setter
    for the property is called to reinstate the previous value.@br{}
    This signal is typically used to obtain change notification for a single
    property, by specifying the property name as a detail in the
    @fun{g-signal-connect} call, like this:
    @begin{pre}
 g_signal_connect (text_view->buffer, \"notify::paste-target-list\",
                   G_CALLBACK (gtk_text_view_target_list_notify),
                   text_view)
    @end{pre}
    It is important to note that you must use canonical parameter names as
    detail strings for the notify signal.
    @begin{pre}
 void user_function (GObject    *gobject,
                     GParamSpec *pspec,
                     gpointer    user_data)      : No Hooks
    @end{pre}
    @begin[code]{table}
      @entry[gobject]{the object which received the signal.}
      @entry[pspec]{the @symbol{g-param-spec} of the property which changed.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
  @end{dictionary}
  @see-slot{pointer}
  @see-slot{g-object-has-reference}
  @see-slot{g-object-signal-handlers}")

;;; ----------------------------------------------------------------------------
;;;
;;; Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pointer" 'g-object) 't)
 "Holds a foreign pointer to the C instance of a GObject.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-reference" 'g-object) 't)
 "Holds the value @code{t} when the instance is successfully registered.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "signal-handlers" 'g-object) 't)
 "An array of signals handlers which are connected to the instance.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'pointer atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'pointer 'function)
 "@version{2012-12-26}
  @short{Accessor of the slot @code{pointer} of the @class{g-object} class.}
  
  The accessor @sym{pointer} gets the foreign C pointer of an instance which is
  stored in a slot of the Lisp @class{g-object} class.
  @begin[Example]{dictionary}
    @begin{pre}
 (setq label (make-instance 'gtk-label))
=> #<GTK-LABEL {E2DB181@}>
 (pointer label)
=> #.(SB-SYS:INT-SAP #X081BDAE0)
    @end{pre}
  @end{dictionary}
  @see-class{g-object}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-object-has-reference atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-object-has-reference 'function)
 "@version{2012-12-26}
  @begin{short}
    Accessor of the slot @code{has-reference} of the @class{g-object} class.
  @end{short}
  
  Note: The slot is set to @code{t} when registering an object during creation.
  The slot is not in use at any place in the code.
  @see-class{g-object}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-object-signal-handlers atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-object-signal-handlers 'function)
 "@version{2012-12-26}
  @begin{short}
    Returns an array of signal handlers which are connected to an instance of
    an GObject.
  @end{short}
  @begin[Example]{dictionary}
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
  @end{dictionary}")

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

(defun register-gobject-for-gc (pointer)
  (with-recursive-lock-held (*gobject-gc-hooks-lock*)
    (let ((locks-were-present (not (null *gobject-gc-hooks*))))
      (push pointer *gobject-gc-hooks*)
      (unless locks-were-present
        (log-for :gc "adding idle-gc-hook to main loop~%")
        (glib::%g-idle-add (callback g-idle-gc-hook) (null-pointer))))))

(defcallback g-idle-gc-hook :boolean ((data :pointer))
  (declare (ignore data))
  (activate-gc-hooks)
  nil)

(defun activate-gc-hooks ()
  (with-recursive-lock-held (*gobject-gc-hooks-lock*)
    (when *gobject-gc-hooks*
      (log-for :gc "activating gc hooks for objects: ~A~%" *gobject-gc-hooks*)
      (loop
         for pointer in *gobject-gc-hooks*
         do (g-object-remove-toggle-ref pointer
                                        (callback g-toggle-notify)
                                        (null-pointer)))
      (setf *gobject-gc-hooks* nil))))

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :around ((obj g-object) &key)
  (when *currently-making-object-p*
    (setf *currently-making-object-p* t))
  (let ((*current-creating-object* obj))
    (log-for :subclass "initialize-instance :around; ~
             *current-creating-object* = ~A~%" obj)
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
           (if (g-object-is-floating (pointer obj)) "(floating)" ""))
  (when (should-ref-sink-at-creation obj)
    (log-for :gc "g_object_ref_sink(~A)~%" (pointer obj))
    (g-object-ref-sink (pointer obj)))
  (setf (g-object-has-reference obj) t)
  (setf (gethash (pointer-address (pointer obj)) *foreign-gobjects-strong*)
        obj)
  (g-object-add-toggle-ref (pointer obj)
                           (callback g-toggle-notify)
                           (null-pointer))
  (g-object-unref (pointer obj)))

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
      (g-object-unref (pointer object)))
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

(defun class-property-info (type property-name)
  (with-unwind (class (g-type-class-ref type) g-type-class-unref)
    (let* ((param-spec (g-object-class-find-property class property-name)))
      (when param-spec
        (parse-g-param-spec param-spec)))))

;;; ----------------------------------------------------------------------------

(defmethod make-instance ((class gobject-class) &rest initargs &key pointer)
  (log-for :subclass "(make-instance ~A ~{~A~^ ~})~%" class initargs)
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

(defmethod initialize-instance :after ((object gobject-class)
                                       &key &allow-other-keys)
  (when (gobject-class-direct-g-type-name object)
    (register-object-type (gobject-class-direct-g-type-name object)
                          (class-name object))
    (glib::at-init (object)
             (initialize-gobject-class-g-type object))))

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
    (with-foreign-object (parameters 'g-parameter args-count)
      (loop
         for i from 0 below args-count
         for arg-name in args-names
         for arg-value in args-values
         for arg-type in args-types
         for arg-g-type = (if arg-type
                              arg-type
                              (class-property-type object-type arg-name))
         for parameter = (mem-aref parameters 'g-parameter i)
         do (setf (foreign-slot-value parameter 'g-parameter :name) arg-name)
         do (set-g-value (foreign-slot-value parameter 'g-parameter :value)
                         arg-value arg-g-type :zero-g-value t))
      (unwind-protect
           (g-object-newv object-type args-count parameters)
        (loop
           for i from 0 below args-count
           for parameter = (mem-aref parameters 'g-parameter i)
           do (foreign-string-free (mem-ref (foreign-slot-pointer parameter
                                                                  'g-parameter
                                                                  :name)
                                            :pointer))
           do (g-value-unset (foreign-slot-pointer parameter
                                                   'g-parameter
                                                   :value)))))))

;;; ----------------------------------------------------------------------------
;;; struct GObjectClass
;;;
;;; struct GObjectClass {
;;;   GTypeClass   g_type_class;
;;;
;;;   /* seldom overidden */
;;;   GObject* (*constructor)  (GType                  type,
;;;                             guint                  n_construct_properties,
;;;                             GObjectConstructParam *construct_properties);
;;;   /* overridable methods */
;;;   void     (*set_property) (GObject        *object,
;;;                             guint           property_id,
;;;                             const GValue   *value,
;;;                             GParamSpec     *pspec);
;;;   void     (*get_property) (GObject        *object,
;;;                             guint           property_id,
;;;                             GValue         *value,
;;;                             GParamSpec     *pspec);
;;;   void     (*dispose)      (GObject        *object);
;;;   void     (*finalize)     (GObject        *object);
;;;   /* seldom overidden */
;;;   void     (*dispatch_properties_changed)
;;;                            (GObject      *object,
;;;                             guint       n_pspecs,
;;;                             GParamSpec  **pspecs);
;;;   /* signals */
;;;   void     (*notify)       (GObject *object,
;;;                             GParamSpec *pspec);
;;;
;;;   /* called when done constructing */
;;;   void     (*constructed)   (GObject *object);
;;; };
;;;
;;; The class structure for the GObject type.
;;;
;;; Example 1. Implementing singletons using a constructor
;;;
;;;   static MySingleton *the_singleton = NULL;
;;;
;;;   static GObject*
;;;   my_singleton_constructor (GType                  type,
;;;                             guint                  n_construct_params,
;;;                             GObjectConstructParam *construct_params)
;;;   {
;;;     GObject *object;
;;;
;;;     if (!the_singleton)
;;;       {
;;;         object = G_OBJECT_CLASS (parent_class)->constructor
;;;                                                         (type,
;;;                                                          n_construct_params,
;;;                                                          construct_params);
;;;         the_singleton = MY_SINGLETON (object);
;;;       }
;;;     else
;;;       object = g_object_ref (G_OBJECT (the_singleton));
;;;
;;;     return object;
;;;   }
;;;
;;;
;;; GTypeClass g_type_class;
;;;     the parent class
;;;
;;; constructor ()
;;;     the constructor function is called by g_object_new() to complete the
;;;     object initialization after all the construction properties are set. The
;;;     first thing a constructor implementation must do is chain up to the
;;;     constructor of the parent class. Overriding constructor should be rarely
;;;     needed, e.g. to handle construct properties, or to implement singletons.
;;;
;;; set_property ()
;;;     the generic setter for all properties of this type. Should be overridden
;;;     for every type with properties. Implementations of set_property don't
;;;     need to emit property change notification explicitly, this is handled by
;;;     the type system.
;;;
;;; get_property ()
;;;     the generic getter for all properties of this type. Should be overridden
;;;     for every type with properties.
;;;
;;; dispose ()
;;;     the dispose function is supposed to drop all references to other
;;;     objects, but keep the instance otherwise intact, so that client method
;;;     invocations still work. It may be run multiple times (due to reference
;;;     loops). Before returning, dispose should chain up to the dispose method
;;;     of the parent class.
;;;
;;; finalize ()
;;;     instance finalization function, should finish the finalization of the
;;;     instance begun in dispose and chain up to the finalize method of the
;;;     parent class.
;;;
;;; dispatch_properties_changed ()
;;;     emits property change notification for a bunch of properties. Overriding
;;;     dispatch_properties_changed should be rarely needed.
;;;
;;; notify ()
;;;     the class closure for the notify signal
;;;
;;; constructed ()
;;;     the constructed function is called by g_object_new() as the final step
;;;     of the object creation process. At the point of the call, all
;;;     construction properties have been set on the object. The purpose of this
;;;     call is to allow for object initialisation steps that can only be
;;;     performed after construction properties have been set. constructed
;;;     implementors should chain up to the constructed call of their parent
;;;     class to allow it to complete its initialisation.
;;; ----------------------------------------------------------------------------

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

(export 'g-object-class)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-object-class atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-object-class atdoc:*external-symbols*)
 "@version{2012-12-26}
  @short{The class structure for the GObject type.}

  Example 1. Implementing singletons using a constructor
  @begin{pre}
 static MySingleton *the_singleton = NULL;

 static GObject*
 my_singleton_constructor (GType                  type,
                           guint                  n_construct_params,
                           GObjectConstructParam *construct_params)
 {
   GObject *object;

   if (!the_singleton)
     {
       object = G_OBJECT_CLASS (parent_class)->constructor
                                                       (type,
                                                        n_construct_params,
                                                        construct_params);
       the_singleton = MY_SINGLETON (object);
     @}
   else
     object = g_object_ref (G_OBJECT (the_singleton));

   return object;
 @}
  @end{pre}
  @begin[Lisp Implementation]{dictionary}
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
    @begin{table}
      @begin[:type-class]{entry}
        the parent class
      @end{entry}
      @begin[:constructor]{entry}
        the constructor function is called by g_object_new() to complete the
        object initialization after all the construction properties are set. The
        first thing a constructor implementation must do is chain up to the
        constructor of the parent class. Overriding constructor should be rarely
        needed, e.g. to handle construct properties, or to implement singletons.
      @end{entry}
      @begin[:set-property]{entry}
        the generic setter for all properties of this type. Should be overridden
        for every type with properties. Implementations of set_property don't
        need to emit property change notification explicitly, this is handled by
        the type system.
      @end{entry}
      @begin[:get-property]{entry}
        the generic getter for all properties of this type. Should be overridden
        for every type with properties.
      @end{entry}
      @begin[:dispose]{entry}
        the dispose function is supposed to drop all references to other
        objects, but keep the instance otherwise intact, so that client method
        invocations still work. It may be run multiple times (due to reference
        loops). Before returning, dispose should chain up to the dispose method
        of the parent class.
      @end{entry}
      @begin[:finalize]{entry}
        instance finalization function, should finish the finalization of the
        instance begun in dispose and chain up to the finalize method of the
        parent class.
      @end{entry}
      @begin[:dispatch-properties-changed]{entry}
        emits property change notification for a bunch of properties. Overriding
        dispatch_properties_changed should be rarely needed.
      @end{entry}
      @begin[:notify]{entry}
        the class closure for the notify signal
      @end{entry}
      @begin[:constructed]{entry}
        the constructed function is called by g_object_new() as the final step
        of the object creation process. At the point of the call, all
        construction properties have been set on the object. The purpose of this
        call is to allow for object initialisation steps that can only be
        performed after construction properties have been set. constructed
        implementors should chain up to the constructed call of their parent
        class to allow it to complete its initialisation.
      @end{entry}
    @end{table}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; struct GObjectConstructParam
;;;
;;; struct GObjectConstructParam {
;;;   GParamSpec *pspec;
;;;   GValue     *value;
;;; };
;;;
;;; The GObjectConstructParam struct is an auxiliary structure used to hand
;;; GParamSpec/GValue pairs to the constructor of a GObjectClass.
;;;
;;; GParamSpec *pspec;
;;;     the GParamSpec of the construct parameter
;;;
;;; GValue *value;
;;;     the value to set the parameter to
;;; ----------------------------------------------------------------------------

(defcstruct g-object-construct-param
  (:pspec (:pointer g-param-spec))
  (:value (:pointer g-value)))

(export 'g-object-construct-param)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-object-construct-param atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-object-construct-param atdoc:*external-symbols*)
 "@version{2012-12-26}
  @begin{short}
    The GObjectConstructParam struct is an auxiliary structure used to hand
    GParamSpec/GValue pairs to the constructor of a GObjectClass.
  @end{short}
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(defcstruct g-object-construct-param
  (:pspec (:pointer g-param-spec))
  (:value (:pointer g-value)))
    @end{pre}
    @begin{table}
      @entry[:pspec]{the GParamSpec of the construct parameter}
      @entry[:value]{the value to set the parameter to}
    @end{table}
  @end{dictionary}")

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

(defun g-type-is-object (type)
 #+cl-cffi-gtk-documentation
 "@version{2012-2-16}
  @argument[type]{Type id to check}
  @return{@code{nil} or @arg{true}, indicating whether type is a
    @var{+g-type-object+}.}
  @begin{short}
    Check if the passed in type id is a @var{+g-type-object+} or derived from it.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-is-object (gtype \"GtkLabel\")) => T
 (g-type-is-object (gtype \"gboolean\")) => NIL
    @end{pre}
  @end{dictionary}"
  (eql (gtype-id (g-type-fundamental (gtype type))) +g-type-object+))

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
;;;
;;; #define G_IS_OBJECT(object)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((object), G_TYPE_OBJECT))
;;;
;;; Checks whether a valid GTypeInstance pointer is of type G_TYPE_OBJECT.
;;;
;;; object :
;;;     Instance to check for being a G_TYPE_OBJECT.
;;; ----------------------------------------------------------------------------

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
;;;
;;; #define G_IS_OBJECT_CLASS(class)
;;;         (G_TYPE_CHECK_CLASS_TYPE ((class), G_TYPE_OBJECT))
;;;
;;; Checks whether class "is a" valid GObjectClass structure of type
;;; G_TYPE_OBJECT or derived.
;;;
;;; class :
;;;     a GObjectClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_GET_CLASS()
;;;
;;; #define G_OBJECT_GET_CLASS(object)
;;;         (G_TYPE_INSTANCE_GET_CLASS ((object), G_TYPE_OBJECT, GObjectClass))
;;;
;;; Get the class structure associated to a GObject instance.
;;;
;;; object :
;;;     a GObject instance.
;;;
;;; Returns :
;;;     pointer to object class structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_TYPE()
;;;
;;; #define G_OBJECT_TYPE(object) (G_TYPE_FROM_INSTANCE (object))
;;;
;;; Get the type id of an object.
;;;
;;; object :
;;;     Object to return the type id for.
;;;
;;; Returns :
;;;     Type id of object.
;;; ----------------------------------------------------------------------------

(defun g-object-type (object)
  (g-type-from-instance object))

(export 'g-object-type)

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_TYPE_NAME()
;;;
;;; #define G_OBJECT_TYPE_NAME(object)  (g_type_name (G_OBJECT_TYPE (object)))
;;;
;;; Get the name of an object's type.
;;;
;;; object :
;;;     Object to return the type name for.
;;;
;;; Returns :
;;;     Type name of object. The string is owned by the type system and should
;;;     not be freed.
;;; ----------------------------------------------------------------------------

(defun g-object-type-name (object)
  (g-type-name (g-object-type object)))

(export 'g-object-type-name)

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_CLASS_TYPE()
;;;
;;; #define G_OBJECT_CLASS_TYPE(class) (G_TYPE_FROM_CLASS (class))
;;;
;;; Get the type id of a class structure.
;;;
;;; class :
;;;     a valid GObjectClass
;;;
;;; Returns :
;;;     Type id of class.
;;; ----------------------------------------------------------------------------

(defun g-object-class-type (class)
  (g-type-from-class class))

(export 'g-object-class-type)

;;; ----------------------------------------------------------------------------
;;; G_OBJECT_CLASS_NAME()
;;;
;;; #define G_OBJECT_CLASS_NAME(class)
;;;         (g_type_name (G_OBJECT_CLASS_TYPE (class)))
;;;
;;; Return the name of a class structure's type.
;;;
;;; class :
;;;     a valid GObjectClass
;;;
;;; Returns :
;;;     Type name of class. The string is owned by the type system and should
;;;     not be freed.
;;; ----------------------------------------------------------------------------

(defun g-object-class-name (class)
  (g-type-name (g-type-from-class class)))

(export 'g-object-class-name)

;;; ----------------------------------------------------------------------------
;;; g_object_class_install_property ()
;;;
;;; void g_object_class_install_property (GObjectClass *oclass,
;;;                                       guint property_id,
;;;                                       GParamSpec *pspec);
;;;
;;; Installs a new property. This is usually done in the class initializer.
;;;
;;; Note that it is possible to redefine a property in a derived class, by
;;; installing a property with the same name. This can be useful at times, e.g.
;;; to change the range of allowed values or the default value.
;;;
;;; oclass :
;;;     a GObjectClass
;;;
;;; property_id :
;;;     the id for the new property
;;;
;;; pspec :
;;;     the GParamSpec for the new property
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_class_install_property" g-object-class-install-property)
    :void
  (class (:pointer g-object-class))
  (property-id :uint)
  (pspec (:pointer g-param-spec)))

(export 'g-object-class-install-property)

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
;;;
;;; GParamSpec * g_object_class_find_property (GObjectClass *oclass,
;;;                                            const gchar *property_name);
;;;
;;; Looks up the GParamSpec for a property of a class.
;;;
;;; oclass :
;;;     a GObjectClass
;;;
;;; property_name :
;;;     the name of the property to look up
;;;
;;; Returns :
;;;     the GParamSpec for the property, or NULL if the class doesn't have a
;;;     property of that name
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_class_find_property" g-object-class-find-property)
    (:pointer g-param-spec)
  (class (:pointer g-object-class))
  (property-name :string))

(export 'g-object-class-find-property)

;;; ----------------------------------------------------------------------------
;;; g_object_class_list_properties ()
;;;
;;; GParamSpec ** g_object_class_list_properties (GObjectClass *oclass,
;;;                                               guint *n_properties);
;;;
;;; Get an array of GParamSpec* for all properties of a class.
;;;
;;; oclass :
;;;     a GObjectClass
;;;
;;; n_properties :
;;;     return location for the length of the returned array
;;;
;;; Returns :
;;;     an array of GParamSpec* which should be freed after use
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_class_list_properties" %g-object-class-list-properties)
    (:pointer (:pointer g-param-spec))
  (class (:pointer g-object-class))
  (n-properties (:pointer :uint)))

(defun g-object-class-list-properties (type)
  (assert (g-type-is-a type +g-type-object+))
  (with-unwind (class (g-type-class-ref type) g-type-class-unref)
    (with-foreign-object (n-properties :uint)
      (with-unwind (params (%g-object-class-list-properties class n-properties)
                           g-free)
        (loop
           for i from 0 below (mem-ref n-properties :uint)
           for param = (mem-aref params :pointer i)
         collect (parse-g-param-spec param))))))

(export 'g-object-class-list-properties)

;;; ----------------------------------------------------------------------------
;;; g_object_class_override_property ()
;;;
;;; void g_object_class_override_property (GObjectClass *oclass,
;;;                                        guint property_id,
;;;                                        const gchar *name);
;;;
;;; Registers property_id as referring to a property with the name name in a
;;; parent class or in an interface implemented by oclass. This allows this
;;; class to override a property implementation in a parent class or to provide
;;; the implementation of a property from an interface.
;;;
;;; Note
;;;
;;; Internally, overriding is implemented by creating a property of type
;;; GParamSpecOverride; generally operations that query the properties of the
;;; object class, such as g_object_class_find_property() or
;;; g_object_class_list_properties() will return the overridden property.
;;; However, in one case, the construct_properties argument of the constructor
;;; virtual function, the GParamSpecOverride is passed instead, so that the
;;; param_id field of the GParamSpec will be correct. For virtually all uses,
;;; this makes no difference. If you need to get the overridden property, you
;;; can call g_param_spec_get_redirect_target().
;;;
;;; oclass :
;;;     a GObjectClass
;;;
;;; property_id :
;;;     the new property ID
;;;
;;; name :
;;;     the name of a property registered in a parent class or in an interface
;;;     of this class.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_class_override_property"
           g-object-class-override-property) :void
  (class (:pointer g-object-class))
  (property-id :uint)
  (name :string))

(export 'g-object-class-override-property)

;;; ----------------------------------------------------------------------------
;;; g_object_interface_install_property ()
;;;
;;; void g_object_interface_install_property (gpointer g_iface,
;;;                                           GParamSpec *pspec);
;;;
;;; Add a property to an interface; this is only useful for interfaces that are
;;; added to GObject-derived types. Adding a property to an interface forces all
;;; objects classes with that interface to have a compatible property. The
;;; compatible property could be a newly created GParamSpec, but normally
;;; g_object_class_override_property() will be used so that the object class
;;; only needs to provide an implementation and inherits the property
;;; description, default value, bounds, and so forth from the interface
;;; property.
;;;
;;; This function is meant to be called from the interface's default vtable
;;; initialization function (the class_init member of GTypeInfo.) It must not be
;;; called after after class_init has been called for any object types
;;; implementing this interface.
;;;
;;; g_iface :
;;;     any interface vtable for the interface, or the default vtable for the
;;;     interface.
;;;
;;; pspec :
;;;     the GParamSpec for the new property
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_interface_install_property"
           g-object-interface-install-property) :void
  (g-iface :pointer)
  (pspec (:pointer g-param-spec)))

(export 'g-object-interface-install-property)

;;; ----------------------------------------------------------------------------
;;; g_object_interface_find_property ()
;;;
;;; GParamSpec * g_object_interface_find_property (gpointer g_iface,
;;;                                                const gchar *property_name);
;;;
;;; Find the GParamSpec with the given name for an interface. Generally, the
;;; interface vtable passed in as g_iface will be the default vtable from
;;; g_type_default_interface_ref(), or, if you know the interface has already
;;; been loaded, g_type_default_interface_peek().
;;;
;;; g_iface :
;;;     any interface vtable for the interface, or the default vtable for the
;;;     interface
;;;
;;; property_name :
;;;     name of a property to lookup.
;;;
;;; Returns :
;;;     the GParamSpec for the property of the interface with the name
;;;     property_name, or NULL if no such property exists
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_interface_find_property" g-object-interface-find-property)
    (:pointer g-param-spec)
  (interface :pointer)
  (property-name :string))

(export 'g-object-interface-find-property)

;;; ----------------------------------------------------------------------------
;;; g_object_interface_list_properties ()
;;;
;;; GParamSpec ** g_object_interface_list_properties (gpointer g_iface,
;;;                                                   guint *n_properties_p);
;;;
;;; Lists the properties of an interface.Generally, the interface vtable passed
;;; in as g_iface will be the default vtable from
;;; g_type_default_interface_ref(), or, if you know the interface has already
;;; been loaded, g_type_default_interface_peek().
;;;
;;; g_iface :
;;;     any interface vtable for the interface, or the default vtable for the
;;;     interface
;;;
;;; n_properties_p :
;;;     location to store number of properties returned
;;;
;;; Returns :
;;;     a pointer to an array of pointers to GParamSpec structures. The
;;;     paramspecs are owned by GLib, but the array should be freed with
;;;     g_free() when you are done with it.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_interface_list_properties"
          %g-object-interface-list-properties) (:pointer g-param-spec)
  (interface :pointer)
  (n-properties (:pointer :uint)))

(defun g-object-interface-list-properties (type)
  (assert (g-type-is-a type +g-type-interface+))
  (with-unwind (g-iface (g-type-default-interface-ref type)
                        g-type-default-interface-unref)
    (with-foreign-object (n-props :uint)
      (with-unwind (params (%g-object-interface-list-properties g-iface n-props)
                           g-free)
        (loop
           for i from 0 below (mem-ref n-props :uint)
           for param = (mem-aref params :pointer i)
           collect (parse-g-param-spec param))))))

(export 'g-object-interface-list-properties)

;;; ----------------------------------------------------------------------------
;;; g_object_new ()
;;;
;;; gpointer g_object_new (GType object_type,
;;;                        const gchar *first_property_name,
;;;                        ...);
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
;;; ... :
;;;     the value of the first property, followed optionally by more name/value
;;;     pairs, followed by NULL
;;;
;;; Returns :
;;;     a new instance of object_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_newv ()
;;;
;;; gpointer g_object_newv (GType object_type,
;;;                         guint n_parameters,
;;;                         GParameter *parameters);
;;;
;;; Creates a new instance of a GObject subtype and sets its properties.
;;;
;;; Construction parameters (see G_PARAM_CONSTRUCT, G_PARAM_CONSTRUCT_ONLY)
;;; which are not explicitly specified are set to their default values.
;;;
;;; Rename to: g_object_new
;;;
;;; object_type :
;;;     the type id of the GObject subtype to instantiate
;;;
;;; n_parameters :
;;;     the length of the parameters array
;;;
;;; parameters :
;;;     an array of GParameter
;;;
;;; Returns :
;;;     a new instance of object_type
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_newv" g-object-newv) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-2-16}
  @argument[object-type]{the type id of the @class{g-object} subtype to
    instantiate}
  @argument[n-parameters]{the length of the parameters array}
  @argument[parameters]{an array of @symbol{g-parameter}}
  @return{A new instance of @arg{object-type}.}
  @begin{short}
    Creates a new instance of a @class{g-object} subtype and sets its
    properties.
  @end{short}

  Construction parameters (see @code{:construct} and @code{:construct-only} of 
  type @symbol{g-param-spec-flags}) which are not explicitly specified are set
  to their default values."
  (object-type g-type)
  (n-parameter :uint)
  (parameters (:pointer g-parameter)))

(export 'g-object-newv)

;;; ----------------------------------------------------------------------------
;;; g_object_ref ()
;;;
;;; gpointer g_object_ref (gpointer object);
;;;
;;; Increases the reference count of object.
;;;
;;; object :
;;;     a GObject
;;;
;;; Returns :
;;;     the same object
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_ref" g-object-ref) :pointer
  (object :pointer))

(export 'g-object-ref)

;;; ----------------------------------------------------------------------------
;;; g_object_unref ()
;;;
;;; void g_object_unref (gpointer object);
;;;
;;; Decreases the reference count of object. When its reference count drops to
;;; 0, the object is finalized (i.e. its memory is freed).
;;;
;;; object :
;;;     a GObject
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_unref" g-object-unref) :void
  (object :pointer))

(export 'g-object-unref)

;;; ----------------------------------------------------------------------------
;;; g_object_ref_sink ()
;;;
;;; gpointer g_object_ref_sink (gpointer object);
;;;
;;; Increase the reference count of object, and possibly remove the floating
;;; reference, if object has a floating reference.
;;;
;;; In other words, if the object is floating, then this call "assumes
;;; ownership" of the floating reference, converting it to a normal reference by
;;; clearing the floating flag while leaving the reference count unchanged. If
;;; the object is not floating, then this call adds a new normal reference
;;; increasing the reference count by one.
;;;
;;; object :
;;;     a GObject
;;;
;;; Returns :
;;;     object
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_ref_sink" g-object-ref-sink) :pointer
  (object :pointer))

(export 'g-object-ref-sink)

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
;;; GInitiallyUnowned
;;;
;;; typedef struct _GObject GInitiallyUnowned;
;;;
;;; All the fields in the GInitiallyUnowned structure are private to the
;;; GInitiallyUnowned implementation and should never be accessed directly.
;;; ----------------------------------------------------------------------------

(defctype %g-initially-unowned %g-object)

(export '%g-initially-unowned)

;;; ----------------------------------------------------------------------------

(defclass g-initially-unowned (g-object)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "GInitiallyUnowned")
  (:g-type-initializer . "g_initially_unowned_get_type")
  (:documentation "Base class that has initial 'floating' reference."))

(export 'g-initially-unowned)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'g-initially-unowned 'type)
 "@version{2012-12-29}
  @begin{short}
    GInitiallyUnowned is derived from GObject. The only difference between the
    two is that the initial reference of a GInitiallyUnowned is flagged as a
    floating reference.
  @end{short}
  This means that it is not specifically claimed to be
  \"owned\" by any code portion. The main motivation for providing floating
  references is C convenience. In particular, it allows code to be written as:
  @begin{pre}
 container = create_container ();
 container_add_child (container, create_child());
  @end{pre} 
  If container_add_child() will g_object_ref_sink() the passed in child, no
  reference of the newly created child is leaked. Without floating references,
  container_add_child() can only g_object_ref() the new child, so to implement
  this code without reference leaks, it would have to be written as:
  @begin{pre}
 Child *child;
 container = create_container ();
 child = create_child ();
 container_add_child (container, child);
 g_object_unref (child);
  @end{pre}
  The floating reference can be converted into an ordinary reference by
  calling g_object_ref_sink(). For already sunken objects (objects that don't
  have a floating reference anymore), g_object_ref_sink() is equivalent to
  g_object_ref() and returns a new reference. Since floating references are
  useful almost exclusively for C convenience, language bindings that provide
  automated reference and memory ownership maintenance (such as smart pointers
  or garbage collection) should not expose floating references in their API.

  Some object implementations may need to save an objects floating state
  across certain code portions (an example is GtkMenu), to achieve this, the
  following sequence can be used:
  @begin{pre}
 /* save floating state */
 gboolean was_floating = g_object_is_floating (object);
 g_object_ref_sink (object);
 /* protected code portion */
    ...;
 /* restore floating state */
 if (was_floating)
    g_object_force_floating (object);
 g_object_unref (object); /* release previously acquired reference */
  @end{pre}
  All the fields in the GInitiallyUnowned structure are private to the
  GInitiallyUnowned implementation and should never be accessed directly.
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(defclass g-initially-unowned (g-object)
  ()
  (:metaclass gobject-class)
  (:g-type-name . \"GInitiallyUnowned\")
  (:g-type-initializer . \"g_initially_unowned_get_type\"))
    @end{pre}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; GInitiallyUnownedClass
;;;
;;; typedef struct _GObjectClass GInitiallyUnownedClass;
;;;
;;; The class structure for the GInitiallyUnowned type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INITIALLY_UNOWNED
;;;
;;; #define G_TYPE_INITIALLY_UNOWNED (g_initially_unowned_get_type())
;;;
;;; The type for GInitiallyUnowned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_object_is_floating ()
;;;
;;; gboolean g_object_is_floating (gpointer object);
;;;
;;; Checks whether object has a floating reference.
;;;
;;; object :
;;;     a GObject
;;;
;;; Returns :
;;;     TRUE if object has a floating reference
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_is_floating" g-object-is-floating) :boolean
  (object :pointer))

(export 'g-object-is-floating)

;;; ----------------------------------------------------------------------------
;;; g_object_force_floating ()
;;;
;;; void g_object_force_floating (GObject *object);
;;;
;;; This function is intended for GObject implementations to re-enforce a
;;; floating object reference. Doing this is seldom required: all
;;; GInitiallyUnowneds are created with a floating reference which usually just
;;; needs to be sunken by calling g_object_ref_sink().
;;;
;;; object :
;;;     a GObject
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_force_floating" g-object-force-floating) :void
  (object :pointer))

(export 'g-object-force-floating)

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
;;; g_object_weak_ref ()
;;;
;;; void g_object_weak_ref (GObject *object, GWeakNotify notify, gpointer data);
;;;
;;; Adds a weak reference callback to an object. Weak references are used for
;;; notification when an object is finalized. They are called "weak references"
;;; because they allow you to safely hold a pointer to an object without calling
;;; g_object_ref() (g_object_ref() adds a strong reference, that is, forces the
;;; object to stay alive).
;;;
;;; Note that the weak references created by this method are not thread-safe:
;;; they cannot safely be used in one thread if the object's last
;;; g_object_unref() might happen in another thread. Use GWeakRef if
;;; thread-safety is required.
;;;
;;; object :
;;;     GObject to reference weakly
;;;
;;; notify :
;;;     callback to invoke before the object is freed
;;;
;;; data :
;;;     extra data to pass to notify
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_weak_ref" g-object-weak-ref) :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

(export 'g-object-weak-ref)

;;; ----------------------------------------------------------------------------
;;; g_object_weak_unref ()
;;;
;;; void g_object_weak_unref (GObject *object,
;;;                           GWeakNotify notify,
;;;                           gpointer data);
;;;
;;; Removes a weak reference callback to an object.
;;;
;;; object :
;;;     GObject to remove a weak reference from
;;;
;;; notify :
;;;     callback to search for
;;;
;;; data :
;;;     data to search for
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_weak_unref" g-object-weak-unref) :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

(export 'g-object-weak-unref)

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
;;; g_object_add_toggle_ref ()
;;;
;;; void g_object_add_toggle_ref (GObject *object,
;;;                               GToggleNotify notify,
;;;                               gpointer data);
;;;
;;; Increases the reference count of the object by one and sets a callback to be
;;; called when all other references to the object are dropped, or when this is
;;; already the last reference to the object and another reference is
;;; established.
;;;
;;; This functionality is intended for binding object to a proxy object managed
;;; by another memory manager. This is done with two paired references: the
;;; strong reference added by g_object_add_toggle_ref() and a reverse reference
;;; to the proxy object which is either a strong reference or weak reference.
;;;
;;; The setup is that when there are no other references to object, only a weak
;;; reference is held in the reverse direction from object to the proxy object,
;;; but when there are other references held to object, a strong reference is
;;; held. The notify callback is called when the reference from object to the
;;; proxy object should be toggled from strong to weak (is_last_ref true) or
;;; weak to strong (is_last_ref false).
;;;
;;; Since a (normal) reference must be held to the object before calling
;;; g_object_add_toggle_ref(), the initial state of the reverse link is always
;;; strong.
;;;
;;; Multiple toggle references may be added to the same gobject, however if
;;; there are multiple toggle references to an object, none of them will ever be
;;; notified until all but one are removed. For this reason, you should only
;;; ever use a toggle reference if there is important state in the proxy object.
;;;
;;; object :
;;;     a GObject
;;;
;;; notify :
;;;     a function to call when this reference is the last reference to the
;;;     object, or is no longer the last reference.
;;;
;;; data :
;;;     data to pass to notify
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_add_toggle_ref" g-object-add-toggle-ref) :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

(export 'g-object-add-toggle-ref)

;;; ----------------------------------------------------------------------------
;;; g_object_remove_toggle_ref ()
;;;
;;; void g_object_remove_toggle_ref (GObject *object,
;;;                                  GToggleNotify notify,
;;;                                  gpointer data);
;;;
;;; Removes a reference added with g_object_add_toggle_ref(). The reference
;;; count of the object is decreased by one.
;;;
;;; object :
;;;     a GObject
;;;
;;; notify :
;;;     a function to call when this reference is the last reference to the
;;;     object, or is no longer the last reference.
;;;
;;; data :
;;;     data to pass to notify
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_remove_toggle_ref" g-object-remove-toggle-ref) :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

(export 'g-object-remove-toggle-ref)

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
;;; g_object_notify ()
;;;
;;; void g_object_notify (GObject *object, const gchar *property_name);
;;;
;;; Emits a "notify" signal for the property property_name on object.
;;;
;;; When possible, eg. when signaling a property change from within the class
;;; that registered the property, you should use g_object_notify_by_pspec()
;;; instead.
;;;
;;; object :
;;;     a GObject
;;;
;;; property_name :
;;;     the name of a property installed on the class of object.
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_notify" g-object-notify) :void
  (object :pointer)
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
;;;
;;; void g_object_freeze_notify (GObject *object);
;;;
;;; Increases the freeze count on object. If the freeze count is non-zero, the
;;; emission of "notify" signals on object is stopped. The signals are queued
;;; until the freeze count is decreased to zero.
;;;
;;; This is necessary for accessors that modify multiple properties to prevent
;;; premature notification while the object is still being modified.
;;;
;;; object :
;;;     a GObject
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_freeze_notify" g-object-freeze-notify) :void
  (object :pointer))

(export 'g-object-freeze-notify)

;;; ----------------------------------------------------------------------------
;;; g_object_thaw_notify ()
;;;
;;; void g_object_thaw_notify (GObject *object);
;;;
;;; Reverts the effect of a previous call to g_object_freeze_notify(). The
;;; freeze count is decreased on object and when it reaches zero, all queued
;;; "notify" signals are emitted.
;;;
;;; It is an error to call this function when the freeze count is zero.
;;;
;;; object :
;;;     a GObject
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_thaw_notify" g-object-thaw-notify) :void
  (object :pointer))

(export 'g-object-thaw-notify)

;;; ----------------------------------------------------------------------------
;;; g_object_get_data ()
;;;
;;; gpointer g_object_get_data (GObject *object, const gchar *key);
;;;
;;; Gets a named field from the objects table of associations (see
;;; g_object_set_data()).
;;;
;;; object :
;;;     GObject containing the associations
;;;
;;; key :
;;;     name of the key for that association
;;;
;;; Returns :
;;;     the data if found, or NULL if no such data exists
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_get_data" g-object-get-data) :pointer
  (object g-object)
  (key :string))

(export 'g-object-get-data)

;;; ----------------------------------------------------------------------------
;;; g_object_set_data ()
;;;
;;; void g_object_set_data (GObject *object, const gchar *key, gpointer data);
;;;
;;; Each object carries around a table of associations from strings to pointers.
;;; This function lets you set an association.
;;;
;;; If the object already had an association with that name, the old association
;;; will be destroyed.
;;;
;;; object :
;;;     GObject containing the associations.
;;;
;;; key :
;;;     name of the key
;;;
;;; data :
;;;     data to associate with that key
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_set_data" g-object-set-data) :void
  (object g-object)
  (key :string)
  (new-value :pointer))

(export 'g-object-set-data)

;;; ----------------------------------------------------------------------------
;;; g_object_set_data_full ()
;;;
;;; void g_object_set_data_full (GObject *object,
;;;                              const gchar *key,
;;;                              gpointer data,
;;;                              GDestroyNotify destroy);
;;;
;;; Like g_object_set_data() except it adds notification for when the
;;; association is destroyed, either by setting it to a different value or when
;;; the object is destroyed.
;;;
;;; Note that the destroy callback is not called if data is NULL.
;;;
;;; object :
;;;     GObject containing the associations
;;;
;;; key :
;;;     name of the key
;;;
;;; data :
;;;     data to associate with that key
;;;
;;; destroy :
;;;     function to call when the association is destroyed
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_set_data_full" g-object-set-data-full) :void
  (object :pointer)
  (key :string)
  (data :pointer)
  (destory :pointer))

(export 'g-object-set-data-full)

;;; ----------------------------------------------------------------------------
;;; g_object_steal_data ()
;;;
;;; gpointer g_object_steal_data (GObject *object, const gchar *key);
;;;
;;; Remove a specified datum from the object's data associations, without
;;; invoking the association's destroy handler.
;;;
;;; object :
;;;     GObject containing the associations
;;;
;;; key :
;;;     name of the key
;;;
;;; Returns :
;;;     the data if found, or NULL if no such data exists
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_steal_data" g-object-steal-data) :pointer
  (object :pointer)
  (key :string))

(export 'g-object-steal-data)

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
;;; g_object_set_property ()
;;;
;;; void g_object_set_property (GObject *object,
;;;                             const gchar *property_name,
;;;                             const GValue *value);
;;;
;;; Sets a property on an object.
;;;
;;; object :
;;;     a GObject
;;;
;;; property_name :
;;;     the name of the property to set
;;;
;;; value :
;;;     the value
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_set_property" %g-object-set-property) :void
  (object :pointer)
  (property-name :string)
  (value (:pointer g-value)))

(defun set-gobject-property (object-ptr property-name new-value
                                        &optional property-type)
  (unless property-type
    (setf property-type
          (class-property-type (g-type-from-instance object-ptr)
                               property-name
                               :assert-writable t)))
  (with-foreign-object (value 'g-value)
    (set-g-value value new-value property-type :zero-g-value t)
    (unwind-protect
      (%g-object-set-property object-ptr property-name value)
      (g-value-unset value))))

;;; ----------------------------------------------------------------------------
;;; g_object_get_property ()
;;;
;;; void g_object_get_property (GObject *object,
;;;                             const gchar *property_name,
;;;                             GValue *value);
;;;
;;; Gets a property of an object. value must have been initialized to the
;;; expected type of the property (or a type to which the expected type can be
;;; transformed) using g_value_init().
;;;
;;; In general, a copy is made of the property contents and the caller is
;;; responsible for freeing the memory by calling g_value_unset().
;;;
;;; Note that g_object_get_property() is really intended for language bindings,
;;; g_object_get() is much more convenient for C programming.
;;;
;;; object :
;;;     a GObject
;;;
;;; property_name :
;;;     the name of the property to get
;;;
;;; value :
;;;     return location for the property value
;;; ----------------------------------------------------------------------------

(defcfun ("g_object_get_property" %g-object-get-property) :void
  (object :pointer)
  (property-name :string)
  (value (:pointer g-value)))

(defun get-gobject-property (object-ptr property-name &optional property-type)
  (restart-case
    (unless property-type
      (setf property-type
            (class-property-type (g-type-from-instance object-ptr)
                                 property-name
                                 :assert-readable t)))
    (return-nil () (return-from get-gobject-property nil)))
  (with-foreign-object (value 'g-value)
    (g-value-zero value)
    (g-value-init value property-type)
    (%g-object-get-property object-ptr property-name value)
    (unwind-protect
      (parse-g-value value)
      (g-value-unset value))))

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

;;; --- End of file gobject.base.lisp ------------------------------------------
