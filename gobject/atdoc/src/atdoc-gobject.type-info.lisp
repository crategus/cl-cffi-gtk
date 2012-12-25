;;; ----------------------------------------------------------------------------
;;; atdoc-gobject.type-info.lisp
;;;
;;; Documentation strings for the library GObject.
;;;
;;; The documentation of this file has been copied from the
;;; GObject Reference Manual Version 2.32.4. See http://www.gtk.org
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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

(setf (documentation '+g-type-invalid+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 0)}}
  An invalid @class{g-type} used as error return value in some functions which
  return a @class{g-type}.
  @see-function{g-type-make-fundamental}")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_NONE
;;;
;;; #define G_TYPE_NONE G_TYPE_MAKE_FUNDAMENTAL (1)
;;;
;;; A fundamental type which is used as a replacement for the C void return
;;; type.
;;; ----------------------------------------------------------------------------

(setf (documentation '+g-type-void+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 1)}}
  A fundamental type which is used as a replacement for the C void return type.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-interface+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 2)}}
  The fundamental type from which all interfaces are derived.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-char+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 3)}}
  The fundamental type corresponding to @code{gchar}. The type designated by
  @code{G_TYPE_CHAR} is unconditionally an 8-bit signed integer. This may or may
  not be the same type a the C type \"@code{gchar}\".
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-uchar+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 4)}}
  The fundamental type corresponding to @code{guchar}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-boolean+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 5)}}
  The fundamental type corresponding to @code{gboolean}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-int+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 6)}}
  The fundamental type corresponding to @code{gint}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-uint+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 7)}}
  The fundamental type corresponding to @code{guint}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-long+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 8)}}
  The fundamental type corresponding to @code{glong}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-ulong+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 9)}}
  The fundamental type corresponding to @code{gulong}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-int64+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 10)}}
  The fundamental type corresponding to @code{gint64}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-uint64+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 11)}}
  The fundamental type corresponding to @code{guint64}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-enum+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 12)}}
  The fundamental type from which all enumeration types are derived.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-flags+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 13)}}
  The fundamental type from which all flags types are derived.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-float+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 14)}}
  The fundamental type corresponding to @code{gfloat}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-double+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 15)}}
  The fundamental type corresponding to @code{gdouble}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-string+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 16)}}
  The fundamental type corresponding to nul-terminated C strings.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-pointer+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 17)}}
  The fundamental type corresponding to @code{gpointer}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-boxed+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 18)}}
  The fundamental type from which all boxed types are derived.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-param+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 19)}}
  The fundamental type from which all @code{GParamSpec} types are derived.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-object+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 20)}}
  The fundamental type for @code{GObject}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-variant+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 21)}}
  @short{The fundamental type corresponding to @code{GVariant}.}

  All floating @code{GVariant} instances passed through the @code{GType} system
  are consumed.

  Note that callbacks in closures, and signal handlers for signals of return
  type @code{G_TYPE_VARIANT}, must never return floating variants.
 
  Note: GLib 2.24 did include a boxed type with this name. It was replaced
  with this fundamental type in 2.26.

  Since 2.26
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-reserved-glib-first+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 22)}}
  First fundamental type number to create a new fundamental type id with
  @fun{g-type-make-fundamental} reserved for GLib.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-reserved-glib-last+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 31)}}
  Last fundamental type number reserved for GLib.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-reserved-bse-first+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 32)}}
  First fundamental type number to create a new fundamental type id with
  @fun{g-type-make-fundamental} reserved for BSE.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-reserved-bse-last+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 48)}}
  Last fundamental type number reserved for BSE.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-reserved-user-first+ 'variable)
 "@variable-value{@code{(g-type-make-fundamental 49)}}
  First available fundamental type number to create new fundamental type id
  with @fun{g-type-make-fundamental}.
  @see-function{g-type-make-fundamental}")

(setf (documentation '+g-type-fundamental-max+ 'variable)
 "@version{2012-12-16}
  @variable-value{120}
  An integer constant that represents the number of identifiers reserved for
  types that are assigned at compile-time.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-gtype 'function)
 "The type for @code{GType}.")

(setf (gethash 'g-type-flags atdoc:*symbol-name-alias*) "Bitfield")
(setf (gethash 'g-type-flags atdoc:*external-symbols*)
 "@version{2012-12-16}
  @short{Bit masks used to check or determine characteristics of a type.}
  @begin{table}
    @begin[:abstract]{entry}
      Indicates an abstract type. No instances can be created for an abstract
      type.
    @end{entry}
    @begin[:value-abstract]{entry}
      Indicates an abstract value type, i.e. a type that introduces a value
      table, but can't be used for @fun{g-value-init}.
    @end{entry}
  @end{table}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-type-fundamental-flags atdoc:*symbol-name-alias*) "Bitfield")
(setf (gethash 'g-type-fundamental-flags atdoc:*external-symbols*)
 "@version{2012-12-16}
  @begin{short}
    Bit masks used to check or determine specific characteristics of a
   fundamental type.
  @end{short}
  @begin{table}
    @entry[:classed]{Indicates a classed type.}
    @entry[:instantiatable]{Indicates an instantiable type (implies classed).}
    @entry[:derivable]{Indicates a flat derivable type.}
    @entry[:deep-derivable]{Indicates a deep derivable type (implies
      derivable).}
  @end{table}")

#|
;;; ----------------------------------------------------------------------------
;;; GType
;;;
;;; A numerical value which represents the unique identifier of a registered
;;; type.
;;; ----------------------------------------------------------------------------

;; %g-type represents the foreign type GType

(defctype %g-type g-size)

;;; ----------------------------------------------------------------------------

;; gtype is a Lisp representation of a foreign GType

(defstruct gtype
  name
  %id)

;;; ----------------------------------------------------------------------------

;; Global hash tables to store names and ids of foreign GTypes

(defvar *name-to-gtype* (make-hash-table :test 'equal))
(defvar *id-to-gtype* (make-hash-table))
(defvar *gtype-lock* (bt:make-lock "gtype lock"))

(defun invalidate-gtypes ()
  (bt:with-lock-held (*gtype-lock*)
    (clrhash *id-to-gtype*)
    (iter (for (name gtype) in-hashtable *name-to-gtype*)
          (setf (gtype-%id gtype) nil))))

(at-finalize () (invalidate-gtypes))

;;; ----------------------------------------------------------------------------

;; gtype-id replaces the accessor gtype-%id

(defun gtype-id (gtype)
  (cond ((null gtype) +g-type-invalid+)
        ((gtype-%id gtype) (gtype-%id gtype))
        (t
         (bt:with-lock-held (*gtype-lock*)
           (let ((id (%g-type-from-name (gtype-name gtype))))
             (if (zerop id)
                 (warn-unknown-gtype (gtype-name gtype))
                 (setf (gtype-%id gtype) id
                       (gethash id *id-to-gtype*) gtype))
             id)))))

(defun warn-unknown-gtype (name)
  ;; Do not print a warning for types which are not derived from GObject
  ;; TODO: This is a hack.
  (when (or (numberp name)
            (not (member name '("LispArrayListStore"
                                "LispTreeStore")
                     :test #'equal)))
    (warn "cl-cffi-gtk: GType ~A is not known to GObject" name)))

;;; ----------------------------------------------------------------------------

;; Make a Lisp representation gtype form a name or an id

(defun gtype-from-name (name)
  (when name
    (bt:with-lock-held (*gtype-lock*)
      (let ((gtype (gethash name *name-to-gtype*)))
        (when gtype
          (unless (gtype-%id gtype)
            (let ((id (%g-type-from-name name)))
              (if (zerop id)
                  (warn-unknown-gtype name)
                  (setf (gtype-%id gtype) id
                        (gethash id *id-to-gtype*) gtype))))
          (return-from gtype-from-name gtype)))
      (let ((id (%g-type-from-name name)))
        (when (zerop id)
          (warn-unknown-gtype name)
          (setf id nil))
        (let ((gtype (make-gtype :name (copy-seq name) :%id id)))
          (setf (gethash id *id-to-gtype*) gtype
                (gethash name *name-to-gtype*) gtype)
          (return-from gtype-from-name gtype))))))

(defun gtype-from-id (id)
  (unless (zerop id)
    (bt:with-lock-held (*gtype-lock*)
      (let ((gtype (gethash id *id-to-gtype*)))
        (if gtype
            gtype
            (let (;; FIXME: This might cause a bug, because %g-type-name expects
                  ;; a valid ID. If the ID is not a valid ID the programm might
                  ;; crash. See the documentation of %g-type-name. Can we expect
                  ;; that the ID will always be a valid ID? In this case the
                  ;; check for the return value is unnecessary.
                  (name (%g-type-name id)))
              (unless name
                (warn-unknown-gtype id)
                (return-from gtype-from-id nil))
              (let ((gtype (gethash name *name-to-gtype*)))
                (when gtype
                  (setf (gtype-%id gtype) id
                        (gethash id *id-to-gtype*) gtype)
                  (return-from gtype-from-id gtype))
                (let ((gtype (make-gtype :name name :%id id)))
                  (setf (gethash id *id-to-gtype*) gtype
                        (gethash name *name-to-gtype*) gtype)
                  (return-from gtype-from-id gtype)))))))))

;;; ----------------------------------------------------------------------------

;; The function gtype converts an integer or a string representation of
;; a foreign GType to a Lisp gtype

(defun gtype (thing)
  (gtype1 thing))

(define-compiler-macro gtype (&whole whole thing)
  (if (constantp thing)
      `(load-time-value (gtype1 ,thing))
      whole))

(defun gtype1 (thing)
  (etypecase thing
    (null nil)
    (gtype thing)
    (string (gtype-from-name thing))
    (integer (gtype-from-id thing))))

;;; ----------------------------------------------------------------------------

;; Check for equality of types. This is faster than the function g-type-is-a.

;; TODO: There is a bug. The function g-type= is used to check against the type
;;       +g-type-invalid+. But this does not work.

(defun g-type= (type-1 type-2)
  (eq (gtype type-1) (gtype type-2)))

(defun g-type/= (type-1 type-2)
  (not (eq (gtype type-1) (gtype type-2))))

;;; ----------------------------------------------------------------------------

;; g-type converts automatically between the Lisp type gtype,
;; an integer or a string and the foreign type GType

(define-foreign-type g-type ()
  ((mangled-p :initarg :mangled-p
              :reader g-type-mangled-p
              :initform nil
              :documentation
              "Whether the type designator is mangled with
               the G_SIGNAL_TYPE_STATIC_SCOPE flag"))
  (:documentation
    "Values of this CFFI foreign type identify the GType. GType is designated by
     a its name (a string) or a numeric identifier. Functions accept GType
     designators as a string or integer and return them as a string. Functions
     g-type-name and g-type-from-name are used to convert between
     name and numeric identifier.
     Numeric identifier of GType may be different between different program
     runs. But string identifier of GType does not change.")
  (:actual-type %g-type)
  (:simple-parser g-type))

(defun g-type-unmangle (type)
  (logxor type (ldb (byte 1 0) type)))

(defmethod translate-from-foreign (value (type g-type))
  (gtype (if (g-type-mangled-p type)
             (g-type-unmangle value)
             value)))

(defmethod translate-to-foreign (value (type g-type))
  (gtype-id (gtype value)))

(export 'g-type)
|#

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FUNDAMENTAL()
;;;
;;; #define G_TYPE_FUNDAMENTAL(type) (g_type_fundamental (type))
;;;
;;; The fundamental type which is the ancestor of type. Fundamental types are
;;; types that serve as ultimate bases for the derived types, thus they are the
;;; roots of distinct inheritance hierarchies.
;;;
;;; type :
;;;     A GType value.
;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-make-fundamental 'function)
 "@version{2012-12-12}
  @argument[x]{the fundamental type number.}
  @return{the GType}
  Get the type ID for the fundamental type number @arg{x}. Use
  @code{g_type_fundamental_next()} instead of this macro to create new
  fundamental types.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-is-abstract 'function)
 "@version{2012-12-12}
  @argument[type]{A GType value.}
  @return{TRUE on success.}
  Checks if type is an abstract type. An abstract type cannot be instantiated
  and is normally used as an abstract base class for derived classes.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-is-derived 'function)
 "@version{2012-12-12}
  @argument[type]{A GType value.}
  @return{TRUE on success.}
  Checks if type is derived (or in object-oriented terminology: inherited)
  from another type (this holds true for all non-fundamental types).")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-is-fundamental 'function)
 "@version{2012-12-12}
  @argument[type]{A GType value.}
  @return{TRUE on success.}
  Checks if type is a fundamental type.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-is-value-type 'function)
 "@version{2012-12-16}
  @argument[type]{A GType value.}
  @return{TRUE on success.}
  Checks if type is a value type and can be used with @fun{g-value-init}.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-has-value-table 'function)
 "@version{2012-12-16}
  @argument[type]{A GType value.}
  @return{TRUE on success.}
  Checks if type has a @code{GTypeValueTable}.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-is-classed 'function)
 "@version{2012-12-16}
  @argument[type]{A GType value.}
  @return{TRUE on success.}
  Checks if type is a classed type.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-is-instantiatable 'function)
 "@version{2012-12-16}
  @argument[type]{A GType value.}
  @return{TRUE on success.}
  Checks if type can be instantiated. Instantiation is the process of creating
  an instance (object) of this type.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-is-derivable 'function)
 "@version{2012-12-16}
  @argument[type]{A GType value.}
  @return{TRUE on success.}
  Checks if type is a derivable type. A derivable type can be used as the base
  class of a flat (single-level) class hierarchy.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-is-deep-derivable 'function)
 "@version{2012-12-16}
  @argument[type]{A GType value.}
  @return{TRUE on success.}
  Checks if type is a deep derivable type. A deep derivable type can be used
  as the base class of a deep (multi-level) class hierarchy.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-is-interface 'function)
 "@version{2012-12-16}
  @argument[type]{A GType value.}
  @return{TRUE on success.}
  @begin{short}
    Checks if type is an interface type.
  @end{short}
  An interface type provides a pure API, the implementation of which is provided
  by another type (which is then said to conform to the interface). GLib
  interfaces are somewhat analogous to Java interfaces and C++ classes
  containing only pure virtual functions, with the difference that GType
  interfaces are not derivable (but see g_type_interface_add_prerequisite() for
  an alternative).")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-type-interface atdoc:*symbol-name-alias*) "CStruct")
(setf (gethash 'g-type-interface atdoc:*external-symbols*)
 "@version{2012-12-16}
  @short{An opaque structure used as the base of all interface types.}

  @begin{pre}
(defcstruct g-type-interface
  (:type g-type)
  (:instance-type g-type))

(export 'g-type-interface)
  @end{pre}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-type-instance atdoc:*symbol-name-alias*) "CStruct")
(setf (gethash 'g-type-instance atdoc:*external-symbols*)
 "@version{2012-12-16}
  @short{An opaque structure used as the base of all type instances.}

  @begin{pre}
(defcstruct g-type-instance
  (:class (:pointer g-type-class)))

(export 'g-type-instance)
  @end{pre}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-type-info atdoc:*symbol-name-alias*) "CStruct")
(setf (gethash 'g-type-info atdoc:*external-symbols*)
 "@version{2012-12-21}
  @begin{short}
    This structure is used to provide the type system with the information
    required to initialize and destruct (finalize) a type's class and its
    instances.
  @end{short}
  The initialized structure is passed to the @fun{g-type-register-static}
  function (or is copied into the provided @sym{g-type-info} structure in the
  @code{g_type_plugin_complete_type_info()}). The type system will perform a
  deep copy of this structure, so its memory does not need to be persistent
  across invocation of @fun{g-type-register-static}.
  @begin{pre}
(defcstruct g-type-info
  (:class-size :uint16)
  (:base-init-fn :pointer)
  (:base-finalize-fn :pointer)
  (:class-init-fn :pointer)
  (:class-finalize-fn :pointer)
  (:class-data :pointer)
  (:instance-size :uint16)
  (:n-preallocs :uint16)
  (:instance-init-fn :pointer)
  (:value-table :pointer))
  @end{pre}
  @begin{table}
    @begin[guint16 class_size]{entry}
      Size of the class structure (required for interface, classed and
      instantiatable types).
    @end{entry}
    @begin[GBaseInitFunc base_init]{entry}
      Location of the base initialization function (optional).
    @end{entry}
    @begin[GBaseFinalizeFunc base_finalize]{entry}
      Location of the base finalization function (optional).
    @end{entry}
    @begin[GClassInitFunc class_init]{entry}
      Location of the class initialization function for classed and
      instantiatable types. Location of the default vtable inititalization
      function for interface types. (optional) This function is used both to
      fill in virtual functions in the class or default vtable, and to do
      type-specific setup such as registering signals and object properties.
    @end{entry}
    @begin[GClassFinalizeFunc class_finalize]{entry}
      Location of the class finalization function for classed and
      instantiatable types. Location fo the default vtable finalization
      function for interface types. (optional)
    @end{entry}
    @begin[gconstpointer class_data]{entry}
      User-supplied data passed to the class init/finalize functions.
    @end{entry}
    @begin[guint16 instance_size]{entry}
      Size of the instance (object) structure (required for instantiatable
      types only).
    @end{entry}
    @begin[guint16 n_preallocs]{entry}
      Prior to GLib 2.10, it specified the number of pre-allocated (cached)
      instances to reserve memory for (0 indicates no caching). Since
      GLib 2.10, it is ignored, since instances are allocated with the slice
      allocator now.
    @end{entry}
    @begin[GInstanceInitFunc instance_init]{entry}
      Location of the instance initialization function (optional, for
      instantiatable types only).
    @end{entry}
    @begin[const GTypeValueTable *value_table]{entry}
      A GTypeValueTable function table for generic handling of GValues of this
      type (usually only useful for fundamental types).
    @end{entry}
  @end{table}
  @see-function{g-type-register-static}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-type-fundamental-info atdoc:*symbol-name-alias*) "CStruct")
(setf (gethash 'g-type-fundamental-info atdoc:*external-symbols*)
 "@version{2012-12-21}
  @begin{short}
    A structure that provides information to the type system which is used
    specifically for managing fundamental types.
  @end{short}
  @begin{pre}
(defcstruct g-type-fundamental-info
  (:type-flags g-type-fundamental-flags))
  @end{pre}
  @begin{table}
    @entry[GTypeFundamentalFlags type_flags]{GTypeFundamentalFlags describing
      the characteristics of the fundamental type}
  @end{table}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-interface-info atdoc:*symbol-name-alias*) "CStruct")
(setf (gethash 'g-interface-info atdoc:*external-symbols*)
 "@version{2012-12-21}
  @begin{short}
    A structure that provides information to the type system which is used
    specifically for managing interface types.
  @end{short}
  @begin{pre}
(defcstruct g-interface-info
  (:interface-init :pointer)
  (:interface-finalize :pointer)
  (:interface-data :pointer))
  @end{pre}
  @begin{table}
    @entry[GInterfaceInitFunc interface_init]{location of the interface
      initialization function}
    @entry[GInterfaceFinalizeFunc interface_finalize]{location of the interface
      finalization function}
    @entry[gpointer interface_data]{user-supplied data passed to the interface
      init/finalize functions}
  @end{table}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-type-value-table atdoc:*symbol-name-alias*) "CStruct")
(setf (gethash 'g-type-value-table atdoc:*external-symbols*)
 "@version{2012-12-21}
  @begin{short}
    The GTypeValueTable provides the functions required by the GValue
    implementation, to serve as a container for values of a type.
  @end{short}
  @begin{pre}
(defcstruct g-type-value-table
  (:value-init :pointer)
  (:value-free :pointer)
  (:value-copy :pointer)
  (:value-peek-pointer :pointer)
  (:collect-format (:string :free-from-foreign nil :free-to-foreign nil))
  (:collect-value :pointer)
  (:lcopy-format (:string :free-from-foreign nil :free-to-foreign nil))
  (:lcopy-value :pointer))
  @end{pre}
  @begin{table}
    @begin[value_init ()]{entry}
      Default initialize values contents by poking values directly into the
      value->data array. The data array of the GValue passed into this
      function was zero-filled with memset(), so no care has to be taken to
      free any old contents. E.g. for the implementation of a string value
      that may never be NULL, the implementation might look like:
      @begin{pre}
 value->data[0].v_pointer = g_strdup (\"\");
      @end{pre}
    @end{entry}
    @begin[value_free ()]{entry}
      Free any old contents that might be left in the data array of the passed
      in value. No resources may remain allocated through the GValue contents
      after this function returns. E.g. for our above string type:
      @begin{pre}
 // only free strings without a specific flag for static storage
 if (!(value->data[1].v_uint & G_VALUE_NOCOPY_CONTENTS))
 g_free (value->data[0].v_pointer);
      @end{pre}
    @end{entry}
    @begin[value_copy ()]{entry}
      dest_value is a GValue with zero-filled data section and src_value is a
      properly setup GValue of same or derived type. The purpose of this
      function is to copy the contents of src_value into dest_value in a way,
      that even after src_value has been freed, the contents of dest_value
      remain valid. String type example:
      @begin{pre}
 dest_value->data[0].v_pointer = g_strdup (src_value->data[0].v_pointer);
      @end{pre}
    @end{entry}
    @begin[value_peek_pointer ()]{entry}
      If the value contents fit into a pointer, such as objects or strings,
      return this pointer, so the caller can peek at the current contents. To
      extend on our above string example:
      @begin{pre}
 return value->data[0].v_pointer;
      @end{pre}
    @end{entry}
    @begin[const gchar *collect_format]{entry}
      A string format describing how to collect the contents of this value
      bit-by-bit. Each character in the format represents an argument to be
      collected, and the characters themselves indicate the type of the
      argument. Currently supported arguments are:
      @begin{table}
        @entry['i']{Integers. passed as collect_values[].v_int.}
        @entry['l']{Longs. passed as collect_values[].v_long.}
        @entry['d']{Doubles. passed as collect_values[].v_double.}
        @entry['p']{Pointers. passed as collect_values[].v_pointer.}
      @end{table}
      It should be noted that for variable argument list construction, ANSI C
      promotes every type smaller than an integer to an int, and floats to
      doubles. So for collection of short int or char, 'i' needs to be used,
      and for collection of floats 'd'.
    @end{entry}
    @begin[collect_value ()]{entry}
      The collect_value() function is responsible for converting the values
      collected from a variable argument list into contents suitable for
      storage in a GValue. This function should setup value similar to
      value_init(); e.g. for a string value that does not allow NULL pointers,
      it needs to either spew an error, or do an implicit conversion by
      storing an empty string. The value passed in to this function has a
      zero-filled data array, so just like for value_init() it is guaranteed
      to not contain any old contents that might need freeing.
      n_collect_values is exactly the string length of collect_format, and
      collect_values is an array of unions GTypeCValue with length
      n_collect_values, containing the collected values according to
      collect_format. collect_flags is an argument provided as a hint by the
      caller. It may contain the flag G_VALUE_NOCOPY_CONTENTS indicating, that
      the collected value contents may be considered \"static\" for the duration
      of the value lifetime. Thus an extra copy of the contents stored in
      collect_values is not required for assignment to value. For our above
      string example, we continue with:
      @begin{pre}
 if (!collect_values[0].v_pointer)
     value->data[0].v_pointer = g_strdup (\"\");
 else if (collect_flags & G_VALUE_NOCOPY_CONTENTS)
   {
     value->data[0].v_pointer = collect_values[0].v_pointer;
     // keep a flag for the value_free() implementation to not free this
     // string
     value->data[1].v_uint = G_VALUE_NOCOPY_CONTENTS;
   @}
   else
     value->data[0].v_pointer = g_strdup (collect_values[0].v_pointer);
   return NULL;
      @end{pre}
      It should be noted, that it is generally a bad idea to follow the
      G_VALUE_NOCOPY_CONTENTS hint for reference counted types. Due to
      reentrancy requirements and reference count assertions performed by the
      signal emission code, reference counts should always be incremented for
      reference counted contents stored in the value->data array. To deviate
      from our string example for a moment, and taking a look at an exemplary
      implementation for collect_value() of GObject:
      @begin{pre}
 if (collect_values[0].v_pointer)
 {
     GObject *object = G_OBJECT (collect_values[0].v_pointer);
     // never honour G_VALUE_NOCOPY_CONTENTS for ref-counted types
     value->data[0].v_pointer = g_object_ref (object);
     return NULL;
 @}
 else
     return g_strdup_printf (\"Object passed as invalid NULL pointer\");
 @}
      @end{pre}
      The reference count for valid objects is always incremented, regardless
      of collect_flags. For invalid objects, the example returns a newly
      allocated string without altering value. Upon success, collect_value()
      needs to return NULL. If, however, an error condition occurred,
      collect_value() may spew an error by returning a newly allocated
      non-NULL string, giving a suitable description of the error condition.
      The calling code makes no assumptions about the value contents being
      valid upon error returns, value is simply thrown away without further
      freeing. As such, it is a good idea to not allocate GValue contents,
      prior to returning an error, however, collect_values() is not obliged to
      return a correctly setup value for error returns, simply because any
      non-NULL return is considered a fatal condition so further program
      behaviour is undefined.
    @end{entry}
    @begin[const gchar *lcopy_format]{entry}
      Format description of the arguments to collect for lcopy_value,
      analogous to collect_format. Usually, lcopy_format string consists only
      of 'p's to provide lcopy_value() with pointers to storage locations.
    @end{entry}
    @begin[lcopy_value ()]{entry}
      This function is responsible for storing the value contents into
      arguments passed through a variable argument list which got collected
      into collect_values according to lcopy_format. n_collect_values equals
      the string length of lcopy_format, and collect_flags may contain
      G_VALUE_NOCOPY_CONTENTS. In contrast to collect_value(), lcopy_value()
      is obliged to always properly support G_VALUE_NOCOPY_CONTENTS. Similar
      to collect_value() the function may prematurely abort by returning a
      newly allocated string describing an error condition. To complete the
      string example:
      @begin{pre}
 gchar **string_p = collect_values[0].v_pointer;
 if (!string_p)
   return g_strdup_printf (\"string location passed as NULL\");
 if (collect_flags & G_VALUE_NOCOPY_CONTENTS)
   *string_p = value->data[0].v_pointer;
 else
   *string_p = g_strdup (value->data[0].v_pointer);
      @end{pre}
      And an illustrative version of lcopy_value() for reference-counted
      types:
      @begin{pre}
 GObject **object_p = collect_values[0].v_pointer;
 if (!object_p)
   return g_strdup_printf (\"object location passed as NULL\");
 if (!value->data[0].v_pointer)
   *object_p = NULL;
 else if (collect_flags & G_VALUE_NOCOPY_CONTENTS) /* always honour */
   *object_p = value->data[0].v_pointer;
 else
   *object_p = g_object_ref (value->data[0].v_pointer);
 return NULL;
      @end{pre}
    @end{entry}
  @end{table}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-from-instance 'function)
 "@version{2012-12-21}
  @argument[instance]{Location of a valid GTypeInstance structure.}
  @return{the GType}
  @short{Get the type identifier from a given instance structure.}

  This macro should only be used in type implementations.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-from-class 'function)
 "@version{2012-12-21}
  @argument[g_class]{Location of a valid GTypeClass structure.}
  @return{the GType}
  @short{Get the type identifier from a given class structure.}

  This macro should only be used in type implementations.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-from-interface 'function)
 "@version{2012-12-21}
  @argument[g_iface]{Location of a valid GTypeInterface structure.}
  @return{the GType}
  @short{Get the type identifier from a given interface structure.}

  This macro should only be used in type implementations.")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INSTANCE_GET_CLASS()
;;;
;;; #define G_TYPE_INSTANCE_GET_CLASS(instance, g_type, c_type)
;;;         (_G_TYPE_IGC ((instance), (g_type), c_type))
;;;
;;; Get the class structure of a given instance, casted to a specified ancestor
;;; type g_type of the instance.
;;;
;;; Note that while calling a GInstanceInitFunc(), the class pointer gets
;;; modified, so it might not always return the expected pointer.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; instance :
;;;     Location of the GTypeInstance structure.
;;;
;;; g_type :
;;;     The GType of the class to be returned.
;;;
;;; c_type :
;;;     The C type of the class structure.
;;;
;;; Returns :
;;;     a pointer to the class structure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INSTANCE_GET_INTERFACE()
;;;
;;; #define G_TYPE_INSTANCE_GET_INTERFACE(instance, g_type, c_type)
;;;         (_G_TYPE_IGI ((instance), (g_type), c_type))
;;;
;;; Get the interface structure for interface g_type of a given instance.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; instance :
;;;     Location of the GTypeInstance structure.
;;;
;;; g_type :
;;;     The GType of the interface to be returned.
;;;
;;; c_type :
;;;     The C type of the interface structure.
;;;
;;; Returns :
;;;     a pointer to the interface structure
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INSTANCE_GET_PRIVATE()
;;;
;;; #define G_TYPE_INSTANCE_GET_PRIVATE(instance, g_type, c_type)
;;;         ((c_type*) g_type_instance_get_private ((GTypeInstance*) (instance),
;;;                                                 (g_type)))
;;;
;;; Gets the private structure for a particular type. The private structure must
;;; have been registered in the class_init function with
;;; g_type_class_add_private().
;;;
;;; This macro should only be used in type implementations.
;;;
;;; instance :
;;;     the instance of a type deriving from private_type.
;;;
;;; g_type :
;;;     the type identifying which private data to retrieve.
;;;
;;; c_type :
;;;     The C type for the private structure.
;;;
;;; Returns :
;;;     a pointer to the private data structure.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CLASS_GET_PRIVATE()
;;;
;;; #define G_TYPE_CLASS_GET_PRIVATE(klass, g_type, c_type)
;;;       ((c_type*) g_type_class_get_private ((GTypeClass*) (klass), (g_type)))
;;;
;;; Gets the private class structure for a particular type. The private
;;; structure must have been registered in the get_type() function with
;;; g_type_add_class_private().
;;;
;;; This macro should only be used in type implementations.
;;;
;;; klass :
;;;     the class of a type deriving from private_type.
;;;
;;; g_type :
;;;     the type identifying which private data to retrieve.
;;;
;;; c_type :
;;;     The C type for the private structure.
;;;
;;; Returns :
;;;     a pointer to the private data structure.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE()
;;;
;;; #define G_TYPE_CHECK_INSTANCE(instance)
;;;         (_G_TYPE_CHI ((GTypeInstance*) (instance)))
;;;
;;; Checks if instance is a valid GTypeInstance structure, otherwise issues a
;;; warning and returns FALSE.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; instance :
;;;     Location of a GTypeInstance structure.
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE_CAST()
;;;
;;; #define G_TYPE_CHECK_INSTANCE_CAST(instance, g_type, c_type)
;;;         (_G_TYPE_CIC ((instance), (g_type), c_type))
;;;
;;; Checks that instance is an instance of the type identified by g_type and
;;; issues a warning if this is not the case. Returns instance casted to a
;;; pointer to c_type.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; instance :
;;;     Location of a GTypeInstance structure.
;;;
;;; g_type :
;;;     The type to be returned.
;;;
;;; c_type :
;;;     The corresponding C type of g_type.
;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-check-instance-type 'function)
 "@version{2012-12-21}
  @argument[instance]{Location of a GTypeInstance structure.}
  @argument[g-type]{The type to be checked}
  @return{TRUE on success.}
  @begin{short}
    Checks if instance is an instance of the type identified by g_type.
  @end{short}

  This macro should only be used in type implementations.")

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_CLASS_CAST()
;;;
;;; #define G_TYPE_CHECK_CLASS_CAST(g_class, g_type, c_type)
;;;         (_G_TYPE_CCC ((g_class), (g_type), c_type))
;;;
;;; Checks that g_class is a class structure of the type identified by g_type
;;; and issues a warning if this is not the case. Returns g_class casted to a
;;; pointer to c_type.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; g_class :
;;;     Location of a GTypeClass structure.
;;;
;;; g_type :
;;;     The type to be returned.
;;;
;;; c_type :
;;;     The corresponding C type of class structure of g_type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_CLASS_TYPE()
;;;
;;; #define G_TYPE_CHECK_CLASS_TYPE(g_class, g_type)
;;;         (_G_TYPE_CCT ((g_class), (g_type)))
;;;
;;; Checks if g_class is a class structure of the type identified by g_type.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; g_class :
;;;     Location of a GTypeClass structure.
;;;
;;; g_type :
;;;     The type to be checked.
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_VALUE()
;;;
;;; #define G_TYPE_CHECK_VALUE(value) (_G_TYPE_CHV ((value)))
;;;
;;; Checks if value has been initialized to hold values of a value type.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; value :
;;;     a GValue
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_VALUE_TYPE()
;;;
;;; #define G_TYPE_CHECK_VALUE_TYPE(value, g_type)
;;;         (_G_TYPE_CVH ((value), (g_type)))
;;;
;;; Checks if value has been initialized to hold values of type g_type.
;;;
;;; This macro should only be used in type implementations.
;;;
;;; value :
;;;     a GValue
;;;
;;; g_type :
;;;     The type to be checked.
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FLAG_RESERVED_ID_BIT
;;;
;;; #define G_TYPE_FLAG_RESERVED_ID_BIT ((GType) (1 << 0))
;;;
;;; A bit in the type number that's supposed to be left untouched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_init ()
;;;
;;; void g_type_init (void);
;;;
;;; Prior to any use of the type system, g_type_init() has to be called to
;;; initialize the type system and assorted other code portions (such as the
;;; various fundamental type implementations or the signal system).
;;;
;;; This function is idempotent.
;;;
;;; Since version 2.24 this also initializes the thread system
;;; ----------------------------------------------------------------------------

#|
(defcfun ("g_type_init" g-type-init) :void)

(at-init () (g-type-init))
|#

;;; ----------------------------------------------------------------------------
;;; enum GTypeDebugFlags
;;;
;;; typedef enum {
;;;   G_TYPE_DEBUG_NONE    = 0,
;;;   G_TYPE_DEBUG_OBJECTS = 1 << 0,
;;;   G_TYPE_DEBUG_SIGNALS = 1 << 1,
;;;   G_TYPE_DEBUG_MASK    = 0x03
;;; } GTypeDebugFlags;
;;;
;;; The GTypeDebugFlags enumeration values can be passed to
;;; g_type_init_with_debug_flags() to trigger debugging messages during runtime.
;;; Note that the messages can also be triggered by setting the GOBJECT_DEBUG
;;; environment variable to a ':'-separated list of "objects" and "signals".
;;;
;;; G_TYPE_DEBUG_NONE
;;;     Print no messages.
;;;
;;; G_TYPE_DEBUG_OBJECTS
;;;     Print messages about object bookkeeping.
;;;
;;; G_TYPE_DEBUG_SIGNALS
;;;     Print messages about signal emissions.
;;;
;;; G_TYPE_DEBUG_MASK
;;;     Mask covering all debug flags.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_init_with_debug_flags ()
;;;
;;; void g_type_init_with_debug_flags (GTypeDebugFlags debug_flags);
;;;
;;; Similar to g_type_init(), but additionally sets debug flags.
;;;
;;; This function is idempotent.
;;;
;;; debug_flags :
;;;     Bitwise combination of GTypeDebugFlags values for debugging purposes.
;;; ----------------------------------------------------------------------------

#|
;;; ----------------------------------------------------------------------------
;;; g_type_name ()
;;;
;;; const gchar * g_type_name (GType type);
;;;
;;; Get the unique name that is assigned to a type ID. Note that this function
;;; (like all other GType API) cannot cope with invalid type IDs. G_TYPE_INVALID
;;; may be passed to this function, as may be any other validly registered type
;;; ID, but randomized type IDs should not be passed in and will most likely
;;; lead to a crash.
;;;
;;; type :
;;;     Type to return name for.
;;;
;;; Returns :
;;;     Static type name or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_name" %g-type-name) (:string :free-from-foreign nil)
  (type %g-type)) ; Use %g-type and not g-type

;;; ----------------------------------------------------------------------------

(declaim (inline g-type-name))

(defun g-type-name (gtype)
  (gtype-name (gtype gtype)))

(export 'g-type-name)

;;; ----------------------------------------------------------------------------
;;; g_type_qname ()
;;;
;;; GQuark g_type_qname (GType type);
;;;
;;; Get the corresponding quark of the type IDs name.
;;;
;;; type :
;;;     Type to return quark of type name for.
;;;
;;; Returns :
;;;     The type names quark or 0.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_qname" g-type-qname) g-quark
  (gtype g-type))

(export 'g-type-qname)

;;; ----------------------------------------------------------------------------
;;; g_type_from_name ()
;;;
;;; GType g_type_from_name (const gchar *name);
;;;
;;; Lookup the type ID from a given type name, returning 0 if no type has been
;;; registered under this name (this is the preferred method to find out by name
;;; whether a specific type has been registered yet).
;;;
;;; name :
;;;     Type name to lookup.
;;;
;;; Returns :
;;;     Corresponding type ID or 0.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_from_name" %g-type-from-name) %g-type
  (name :string))

(defun g-type-from-name (name)
  (gtype name))

(export 'g-type-from-name)

;;; ----------------------------------------------------------------------------
;;; g_type_parent ()
;;;
;;; GType g_type_parent (GType type);
;;;
;;; Return the direct parent type of the passed in type. If the passed in type
;;; has no parent, i.e. is a fundamental type, 0 is returned.
;;;
;;; type :
;;;     The derived type.
;;;
;;; Returns :
;;;     The parent type.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_parent" g-type-parent) g-type
  (type g-type))

(export 'g-type-parent)

;;; ----------------------------------------------------------------------------
;;; g_type_depth ()
;;;
;;; guint g_type_depth (GType type);
;;;
;;; Returns the length of the ancestry of the passed in type. This includes the
;;; type itself, so that e.g. a fundamental type has depth 1.
;;;
;;; type :
;;;     A GType value.
;;;
;;; Returns :
;;;     The depth of type.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_depth" g-type-depth) :uint
  (type g-type))

(export 'g-type-depth)

;;; ----------------------------------------------------------------------------
;;; g_type_next_base ()
;;;
;;; GType g_type_next_base (GType leaf_type, GType root_type);
;;;
;;; Given a leaf_type and a root_type which is contained in its anchestry,
;;; return the type that root_type is the immediate parent of. In other words,
;;; this function determines the type that is derived directly from root_type
;;; which is also a base class of leaf_type. Given a root type and a leaf type,
;;; this function can be used to determine the types and order in which the leaf
;;; type is descended from the root type.
;;;
;;; leaf_type :
;;;     Descendant of root_type and the type to be returned.
;;;
;;; root_type :
;;;     Immediate parent of the returned type.
;;;
;;; Returns :
;;;     Immediate child of root_type and anchestor of leaf_type.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_next_base" g-type-next-base) g-type
  (leaf-type g-type)
  (root-type g-type))

(export 'g-type-next-base)

;;; ----------------------------------------------------------------------------
;;; g_type_is_a ()
;;;
;;; gboolean g_type_is_a (GType type, GType is_a_type);
;;;
;;; If is_a_type is a derivable type, check whether type is a descendant of
;;; is_a_type. If is_a_type is an interface, check whether type conforms to it.
;;;
;;; type :
;;;     Type to check anchestry for.
;;;
;;; is_a_type :
;;;     Possible anchestor of type or interface type could conform to.
;;;
;;; Returns :
;;;     TRUE if type is_a is_a_type holds true.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_is_a" g-type-is-a) :boolean
  (gtype g-type)
  (is-a-type g-type))

(export 'g-type-is-a)

;;; ----------------------------------------------------------------------------
;;; g_type_class_ref ()
;;;
;;; gpointer g_type_class_ref (GType type);
;;;
;;; Increments the reference count of the class structure belonging to type.
;;; This function will demand-create the class if it doesn't exist already.
;;;
;;; type :
;;;     Type ID of a classed type.
;;;
;;; Returns :
;;;     The GTypeClass structure for the given type ID.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_ref" g-type-class-ref) (:pointer g-type-class)
  (gtype g-type))

(export 'g-type-class-ref)

;;; ----------------------------------------------------------------------------
;;; g_type_class_peek ()
;;;
;;; gpointer g_type_class_peek (GType type);
;;;
;;; This function is essentially the same as g_type_class_ref(), except that the
;;; classes reference count isn't incremented. As a consequence, this function
;;; may return NULL if the class of the type passed in does not currently exist
;;; (hasn't been referenced before).
;;;
;;; type :
;;;     Type ID of a classed type.
;;;
;;; Returns :
;;;     The GTypeClass structure for the given type ID or NULL if the class does
;;;     not currently exist.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_peek" g-type-class-peek) (:pointer g-type-class)
  (type g-type))

(export 'g-type-class-peek)

;;; ----------------------------------------------------------------------------
;;; g_type_class_peek_static ()
;;;
;;; gpointer g_type_class_peek_static (GType type);
;;;
;;; A more efficient version of g_type_class_peek() which works only for static
;;; types.
;;;
;;; type :
;;;     Type ID of a classed type.
;;;
;;; Returns :
;;;     The GTypeClass structure for the given type ID or NULL if the class does
;;;     not currently exist or is dynamically loaded.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_peek_static" g-type-class-peek-static)
    (:pointer g-type-class)
  (gtype g-type))

(export 'g-type-class-peek-static)

;;; ----------------------------------------------------------------------------
;;; g_type_class_unref ()
;;;
;;; void g_type_class_unref (gpointer g_class);
;;;
;;; Decrements the reference count of the class structure being passed in. Once
;;; the last reference count of a class has been released, classes may be
;;; finalized by the type system, so further dereferencing of a class pointer
;;; after g_type_class_unref() are invalid.
;;;
;;; g_class :
;;;     The GTypeClass structure to unreference.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_unref" g-type-class-unref) :void
  (g-class (:pointer g-type-class)))

(export 'g-type-class-unref)

;;; ----------------------------------------------------------------------------
;;; g_type_class_peek_parent ()
;;;
;;; gpointer g_type_class_peek_parent (gpointer g_class);
;;;
;;; This is a convenience function often needed in class initializers. It
;;; returns the class structure of the immediate parent type of the class passed
;;; in. Since derived classes hold a reference count on their parent classes as
;;; long as they are instantiated, the returned class will always exist. This
;;; function is essentially equivalent to:
;;;
;;;   g_type_class_peek (g_type_parent (G_TYPE_FROM_CLASS (g_class)));
;;;
;;; g_class :
;;;     The GTypeClass structure to retrieve the parent class for.
;;;
;;; Returns :
;;;     The parent class of g_class.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_peek_parent" g-type-class-peek-parent)
    (:pointer g-type-class)
  (g-class (:pointer g-type-class)))

(export 'g-type-class-peek-parent)

|#

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-class-add-private 'function)
 "@version{2012-12-21}
  @argument[g-class]{Class structure for an instantiatable type.}
  @argument[private-size]{Size of private structure.}
  @begin{short}
    Registers a private structure for an instantiatable type.
  @end{short}

  When an object is allocated, the private structures for the type and all of
  its parent types are allocated sequentially in the same memory block as the
  public structures.

  Note that the accumulated size of the private structures of a type and all
  its parent types cannot excced 64 KiB.

  This function should be called in the type's class_init() function. The
  private structure can be retrieved using the G_TYPE_INSTANCE_GET_PRIVATE()
  macro.

  The following example shows attaching a private structure MyObjectPrivate to
  an object MyObject defined in the standard GObject fashion. type's
  class_init() function. Note the use of a structure member \"priv\" to avoid
  the overhead of repeatedly calling MY_OBJECT_GET_PRIVATE().
  @begin{pre}
 typedef struct _MyObject        MyObject;
 typedef struct _MyObjectPrivate MyObjectPrivate;

 struct _MyObject {
  GObject parent;

  MyObjectPrivate *priv;
 @};

 struct _MyObjectPrivate {
   int some_field;
 @};

 static void
 my_object_class_init (MyObjectClass *klass)
 {
   g_type_class_add_private (klass, sizeof (MyObjectPrivate));
 @}

 static void
 my_object_init (MyObject *my_object)
 {
   my_object->priv = G_TYPE_INSTANCE_GET_PRIVATE (my_object,
                                                  MY_TYPE_OBJECT,
                                                  MyObjectPrivate);
 @}

 static int
 my_object_get_some_field (MyObject *my_object)
 {
   MyObjectPrivate *priv;

   g_return_val_if_fail (MY_IS_OBJECT (my_object), 0);

   priv = my_object->priv;

   return priv->some_field;
 @}
  @end{pre}
  Since 2.4")

#|
;;; ----------------------------------------------------------------------------
;;; g_type_add_class_private ()
;;;
;;; void g_type_add_class_private (GType class_type, gsize private_size);
;;;
;;; Registers a private class structure for a classed type; when the class is
;;; allocated, the private structures for the class and all of its parent types
;;; are allocated sequentially in the same memory block as the public
;;; structures. This function should be called in the type's get_type() function
;;; after the type is registered. The private structure can be retrieved using
;;; the G_TYPE_CLASS_GET_PRIVATE() macro.
;;;
;;; class_type :
;;;     GType of an classed type.
;;;
;;; private_size :
;;;     size of private structure.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_peek ()
;;;
;;; gpointer g_type_interface_peek (gpointer instance_class, GType iface_type);
;;;
;;; Returns the GTypeInterface structure of an interface to which the passed in
;;; class conforms.
;;;
;;; instance_class :
;;;     A GTypeClass structure.
;;;
;;; iface_type :
;;;     An interface ID which this class conforms to.
;;;
;;; Returns :
;;;     The GTypeInterface structure of iface_type if implemented by
;;;     instance_class, NULL otherwise.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interface_peek" g-type-interface-peek)
    (:pointer g-type-interface)
  (instance-class (:pointer g-type-class))
  (iface-type g-type))

(export 'g-type-interface-peek)

;;; ----------------------------------------------------------------------------
;;; g_type_interface_peek_parent ()
;;;
;;; gpointer g_type_interface_peek_parent (gpointer g_iface);
;;;
;;; Returns the corresponding GTypeInterface structure of the parent type of the
;;; instance type to which g_iface belongs. This is useful when deriving the
;;; implementation of an interface from the parent type and then possibly
;;; overriding some methods.
;;;
;;; g_iface :
;;;     A GTypeInterface structure.
;;;
;;; Returns :
;;;     The corresponding GTypeInterface structure of the parent type of the
;;;     instance type to which g_iface belongs, or NULL if the parent type
;;;     doesn't conform to the interface.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_ref ()
;;;
;;; gpointer g_type_default_interface_ref (GType g_type);
;;;
;;; Increments the reference count for the interface type g_type, and returns
;;; the default interface vtable for the type.
;;;
;;; If the type is not currently in use, then the default vtable for the type
;;; will be created and initalized by calling the base interface init and
;;; default vtable init functions for the type (the @base_init and class_init
;;; members of GTypeInfo). Calling g_type_default_interface_ref() is useful when
;;; you want to make sure that signals and properties for an interface have been
;;; installed.
;;;
;;; g_type :
;;;     an interface type
;;;
;;; Returns :
;;;     the default vtable for the interface; call
;;;     g_type_default_interface_unref() when you are done using the interface.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_default_interface_ref" g-type-default-interface-ref) :pointer
  (gtype g-type))

(export 'g-type-default-interface-ref)

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_peek ()
;;;
;;; gpointer g_type_default_interface_peek (GType g_type);
;;;
;;; If the interface type g_type is currently in use, returns its default
;;; interface vtable.
;;;
;;; g_type :
;;;     an interface type
;;;
;;; Returns :
;;;     the default vtable for the interface, or NULL if the type is not
;;;     currently in use
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_unref ()
;;;
;;; void g_type_default_interface_unref (gpointer g_iface);
;;;
;;; Decrements the reference count for the type corresponding to the interface
;;; default vtable g_iface. If the type is dynamic, then when no one is using
;;; the interface and all references have been released, the finalize function
;;; for the interface's default vtable (the class_finalize member of GTypeInfo)
;;; will be called.
;;;
;;; g_iface :
;;;     the default vtable structure for a interface, as returned by
;;;     g_type_default_interface_ref()
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_default_interface_unref" g-type-default-interface-unref) :void
  (interface :pointer))

(export 'g-type-default-interface-unref)

;;; ----------------------------------------------------------------------------
;;; g_type_children ()
;;;
;;; GType * g_type_children (GType type, guint *n_children);
;;;
;;; Return a newly allocated and 0-terminated array of type IDs, listing the
;;; child types of type. The return value has to be g_free()ed after use.
;;;
;;; type :
;;;     The parent type.
;;;
;;; n_children :
;;;     Optional guint pointer to contain the number of child types.
;;;
;;; Returns :
;;;     Newly allocated and 0-terminated array of child types.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_children" %g-type-children) (:pointer %g-type)
  (gtype g-type)
  (n-children (:pointer :uint)))

(defun g-type-children (type)
  (with-foreign-object (n-children :uint)
    (let ((g-types-ptr (%g-type-children type n-children)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-children :uint)
             collect (mem-aref g-types-ptr 'g-type i))
        (g-free g-types-ptr)))))

(export 'g-type-children)

;;; ----------------------------------------------------------------------------
;;; g_type_interfaces ()
;;;
;;; GType * g_type_interfaces (GType type, guint *n_interfaces);
;;;
;;; Return a newly allocated and 0-terminated array of type IDs, listing the
;;; interface types that type conforms to. The return value has to be g_free()ed
;;; after use.
;;;
;;; type :
;;;     The type to list interface types for.
;;;
;;; n_interfaces :
;;;     Optional guint pointer to contain the number of interface types.
;;;
;;; Returns :
;;;     Newly allocated and 0-terminated array of interface types.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interfaces" %g-type-interfaces) (:pointer %g-type)
  (type g-type)
  (n-interfaces (:pointer :uint)))

(defun g-type-interfaces (type)
  (with-foreign-object (n-interfaces :uint)
    (let ((g-types-ptr (%g-type-interfaces type n-interfaces)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-interfaces :uint)
             collect (mem-aref g-types-ptr 'g-type i))
        (g-free g-types-ptr)))))

(export 'g-type-interfaces)

;;; ----------------------------------------------------------------------------
;;; g_type_interface_prerequisites ()
;;;
;;; GType * g_type_interface_prerequisites (GType interface_type,
;;;                                         guint *n_prerequisites);
;;;
;;; Returns the prerequisites of an interfaces type.
;;;
;;; interface_type :
;;;     an interface type
;;;
;;; n_prerequisites :
;;;     location to return the number of prerequisites, or NULL
;;;
;;; Returns :
;;;     a newly-allocated zero-terminated array of GType containing the
;;;     prerequisites of interface_type
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interface_prerequisites" %g-type-interface-prerequisites)
    (:pointer %g-type)
  (interface-type g-type)
  (n-prerequisites (:pointer :uint)))

(defun g-type-interface-prerequisites (interface-type)
  (with-foreign-object (n-prerequisites :uint)
    (let ((g-types-ptr (%g-type-interface-prerequisites interface-type
                                                        n-prerequisites)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-prerequisites :uint)
             collect (mem-aref g-types-ptr 'g-type i))
        (g-free g-types-ptr)))))

(export 'g-type-interface-prerequisites)

;;; ----------------------------------------------------------------------------
;;; g_type_set_qdata ()
;;;
;;; void g_type_set_qdata (GType type, GQuark quark, gpointer data);
;;;
;;; Attaches arbitrary data to a type.
;;;
;;; type :
;;;     a GType
;;;
;;; quark :
;;;     a GQuark id to identify the data
;;;
;;; data :
;;;     the data
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_set_qdata" g-type-set-qdata) :void
  (gtype g-type)
  (quark g-quark)
  (data :pointer))

(export 'g-type-set-qdata)

;;; ----------------------------------------------------------------------------
;;; g_type_get_qdata ()
;;;
;;; gpointer g_type_get_qdata (GType type, GQuark quark);
;;;
;;; Obtains data which has previously been attached to type with
;;; g_type_set_qdata().
;;;
;;; Note that this does not take subtyping into account; data attached to one
;;; type with g_type_set_qdata() cannot be retrieved from a subtype using
;;; g_type_get_qdata().
;;;
;;; type :
;;;     a GType
;;;
;;; quark :
;;;     a GQuark id to identify the data
;;;
;;; Returns :
;;;     the data, or NULL if no data was found
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_get_qdata" g-type-get-qdata) :pointer
  (gtype g-type)
  (quark g-quark))

(export 'g-type-get-qdata)

|#

(setf (gethash 'g-type-query atdoc:*symbol-name-alias*) "CStruct")
(setf (gethash 'g-type-query atdoc:*external-symbols*)
 "@version{2012-12-21}
  @begin{short}
    A structure holding information for a specific type.
  @end{short}
  It is filled in by the @fun{g-type-query} function.
  @begin{pre}
(defcstruct g-type-query
  (:type g-type)
  (:type-name (:string :free-from-foreign nil))
  (:class-size :uint)
  (:instance-size :uint))
  @end{pre}
  @begin{table}
    @entry[type]{the @class{g-type} value of the type.}
    @entry[type-name]{the name of the type.}
    @entry[class-size]{the size of the class structure.}
    @entry[instance-size]{the size of the instance structure.}
  @end{table}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-type-query 'function)
 "@version{2012-12-21}
  @argument[type]{the @class{g-type} value of a static, classed type.}
  @argument[query]{A user provided structure that is filled in with constant
    values upon success.}
  @begin{short}
    Queries the type system for information about a specific type.
  @end{short}
  This function will fill in a user-provided structure to hold type-specific
  information. If an invalid @class{g-type} is passed in, the type member of the
  @symbol{g-type-query} is 0. All members filled into the @symbol{g-type-query}
  structure should be considered constant and have to be left untouched.")

#|
;;; ----------------------------------------------------------------------------
;;; GBaseInitFunc ()
;;;
;;; void (*GBaseInitFunc) (gpointer g_class);
;;;
;;; A callback function used by the type system to do base initialization of the
;;; class structures of derived types. It is called as part of the
;;; initialization process of all derived classes and should reallocate or reset
;;; all dynamic class members copied over from the parent class. For example,
;;; class members (such as strings) that are not sufficiently handled by a plain
;;; memory copy of the parent class into the derived class have to be altered.
;;; See GClassInitFunc() for a discussion of the class intialization process.
;;;
;;; g_class :
;;;     The GTypeClass structure to initialize.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GBaseFinalizeFunc ()
;;;
;;; void (*GBaseFinalizeFunc) (gpointer g_class);
;;;
;;; A callback function used by the type system to finalize those portions of a
;;; derived types class structure that were setup from the corresponding
;;; GBaseInitFunc() function. Class finalization basically works the inverse way
;;; in which class intialization is performed. See GClassInitFunc() for a
;;; discussion of the class intialization process.
;;;
;;; g_class :
;;;     The GTypeClass structure to finalize.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClassInitFunc ()
;;;
;;; void (*GClassInitFunc) (gpointer g_class, gpointer class_data);
;;;
;;; A callback function used by the type system to initialize the class of a
;;; specific type. This function should initialize all static class members. The
;;; initialization process of a class involves:
;;;
;;;     1 - Copying common members from the parent class over to the derived
;;;         class structure.
;;;
;;;     2 - Zero initialization of the remaining members not copied over from
;;;         the parent class.
;;;
;;;     3 - Invocation of the GBaseInitFunc() initializers of all parent types
;;;         and the class' type.
;;;
;;;     4 - Invocation of the class' GClassInitFunc() initializer.
;;;
;;; Since derived classes are partially initialized through a memory copy of the
;;; parent class, the general rule is that GBaseInitFunc() and
;;; GBaseFinalizeFunc() should take care of necessary reinitialization and
;;; release of those class members that were introduced by the type that
;;; specified these GBaseInitFunc()/GBaseFinalizeFunc(). GClassInitFunc() should
;;; only care about initializing static class members, while dynamic class
;;; members (such as allocated strings or reference counted resources) are
;;; better handled by a GBaseInitFunc() for this type, so proper initialization
;;; of the dynamic class members is performed for class initialization of
;;; derived types as well. An example may help to correspond the intend of the
;;; different class initializers:
;;;
;;;   typedef struct {
;;;     GObjectClass parent_class;
;;;     gint         static_integer;
;;;     gchar       *dynamic_string;
;;;   } TypeAClass;
;;;   static void
;;;   type_a_base_class_init (TypeAClass *class)
;;;   {
;;;     class->dynamic_string = g_strdup ("some string");
;;;   }
;;;   static void
;;;   type_a_base_class_finalize (TypeAClass *class)
;;;   {
;;;     g_free (class->dynamic_string);
;;;   }
;;;   static void
;;;   type_a_class_init (TypeAClass *class)
;;;   {
;;;     class->static_integer = 42;
;;;   }
;;;
;;;   typedef struct {
;;;     TypeAClass   parent_class;
;;;     gfloat       static_float;
;;;     GString     *dynamic_gstring;
;;;   } TypeBClass;
;;;   static void
;;;   type_b_base_class_init (TypeBClass *class)
;;;   {
;;;     class->dynamic_gstring = g_string_new ("some other string");
;;;   }
;;;   static void
;;;   type_b_base_class_finalize (TypeBClass *class)
;;;   {
;;;     g_string_free (class->dynamic_gstring);
;;;   }
;;;   static void
;;;   type_b_class_init (TypeBClass *class)
;;;   {
;;;     class->static_float = 3.14159265358979323846;
;;;   }
;;;
;;; Initialization of TypeBClass will first cause initialization of TypeAClass
;;; (derived classes reference their parent classes, see g_type_class_ref() on
;;; this). Initialization of TypeAClass roughly involves zero-initializing its
;;; fields, then calling its GBaseInitFunc() type_a_base_class_init() to
;;; allocate its dynamic members (dynamic_string), and finally calling its
;;; GClassInitFunc() type_a_class_init() to initialize its static members
;;; (static_integer). The first step in the initialization process of TypeBClass
;;; is then a plain memory copy of the contents of TypeAClass into TypeBClass
;;; and zero-initialization of the remaining fields in TypeBClass. The dynamic
;;; members of TypeAClass within TypeBClass now need reinitialization which is
;;; performed by calling type_a_base_class_init() with an argument of
;;; TypeBClass. After that, the GBaseInitFunc() of TypeBClass,
;;; type_b_base_class_init() is called to allocate the dynamic members of
;;; TypeBClass (dynamic_gstring), and finally the GClassInitFunc() of
;;; TypeBClass, type_b_class_init(), is called to complete the initialization
;;; process with the static members (static_float). Corresponding finalization
;;; counter parts to the GBaseInitFunc() functions have to be provided to
;;; release allocated resources at class finalization time.
;;;
;;; g_class :
;;;     The GTypeClass structure to initialize.
;;;
;;; class_data :
;;;     The class_data member supplied via the GTypeInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GClassFinalizeFunc ()
;;;
;;; void (*GClassFinalizeFunc) (gpointer g_class, gpointer class_data);
;;;
;;; A callback function used by the type system to finalize a class. This
;;; function is rarely needed, as dynamically allocated class resources should
;;; be handled by GBaseInitFunc() and GBaseFinalizeFunc(). Also, specification
;;; of a GClassFinalizeFunc() in the GTypeInfo structure of a static type is
;;; invalid, because classes of static types will never be finalized (they are
;;; artificially kept alive when their reference count drops to zero).
;;;
;;; g_class :
;;;     The GTypeClass structure to finalize.
;;;
;;; class_data :
;;;     The class_data member supplied via the GTypeInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInstanceInitFunc ()
;;;
;;; void (*GInstanceInitFunc) (GTypeInstance *instance, gpointer g_class);
;;;
;;; A callback function used by the type system to initialize a new instance of
;;; a type. This function initializes all instance members and allocates any
;;; resources required by it. Initialization of a derived instance involves
;;; calling all its parent types instance initializers, so the class member of
;;; the instance is altered during its initialization to always point to the
;;; class that belongs to the type the current initializer was introduced for.
;;;
;;; instance :
;;;     The instance to initialize.
;;;
;;; g_class :
;;;     The class of the type the instance is created for.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInterfaceInitFunc ()
;;;
;;; void (*GInterfaceInitFunc) (gpointer g_iface, gpointer iface_data);
;;;
;;; A callback function used by the type system to initialize a new interface.
;;; This function should initialize all internal data and allocate any resources
;;; required by the interface.
;;;
;;; g_iface :
;;;     The interface structure to initialize.
;;;
;;; iface_data :
;;;     The interface_data supplied via the GInterfaceInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GInterfaceFinalizeFunc ()
;;;
;;; void (*GInterfaceFinalizeFunc) (gpointer g_iface, gpointer iface_data);
;;;
;;; A callback function used by the type system to finalize an interface. This
;;; function should destroy any internal data and release any resources
;;; allocated by the corresponding GInterfaceInitFunc() function.
;;;
;;; g_iface :
;;;     The interface structure to finalize.
;;;
;;; iface_data :
;;;     The interface_data supplied via the GInterfaceInfo structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTypeClassCacheFunc ()
;;;
;;; gboolean (*GTypeClassCacheFunc) (gpointer cache_data, GTypeClass *g_class);
;;;
;;; A callback function which is called when the reference count of a class
;;; drops to zero. It may use g_type_class_ref() to prevent the class from being
;;; freed. You should not call g_type_class_unref() from a GTypeClassCacheFunc
;;; function to prevent infinite recursion, use g_type_class_unref_uncached()
;;; instead.
;;;
;;; The functions have to check the class id passed in to figure whether they
;;; actually want to cache the class of this type, since all classes are routed
;;; through the same GTypeClassCacheFunc chain.
;;;
;;; cache_data :
;;;     data that was given to the g_type_add_class_cache_func() call
;;;
;;; g_class :
;;;     The GTypeClass structure which is unreferenced
;;;
;;; Returns :
;;;     TRUE to stop further GTypeClassCacheFuncs from being called, FALSE to
;;;     continue.
;;; ----------------------------------------------------------------------------

|#

(setf (documentation 'g-type-register-static 'function)
 "@version{2012-12-21}
  @argument[parent-type]{Type from which this type will be derived.}
  @argument[type-name]{String used as the name of the new type.}
  @argument[info]{The @symbol{g-type-info} structure for this type.}
  @argument[flags]{Bitwise combination of @symbol{g-type-flags} values.}
  @return{The new type identifier.}
  @begin{short}
    Registers @arg{type-name} as the name of a new static type derived from
    @arg{parent-type}.
  @end{short}
  The type system uses the information contained in the @symbol{g-type-info}
  structure pointed to by @arg{info} to manage the type and its instances (if
  not abstract). The value of @arg{flags} determines the nature (e. g. abstract
  or not) of the type.
  @see-symbol{g-type-info}
  @see-symbol{g-type-flags}")

#|
;;; ----------------------------------------------------------------------------
;;; g_type_register_static_simple ()
;;;
;;; GType g_type_register_static_simple (GType parent_type,
;;;                                      const gchar *type_name,
;;;                                      guint class_size,
;;;                                      GClassInitFunc class_init,
;;;                                      guint instance_size,
;;;                                      GInstanceInitFunc instance_init,
;;;                                      GTypeFlags flags);
;;;
;;; Registers type_name as the name of a new static type derived from
;;; parent_type. The value of flags determines the nature (e.g. abstract or not)
;;; of the type. It works by filling a GTypeInfo struct and calling
;;; g_type_register_static().
;;;
;;; parent_type :
;;;     Type from which this type will be derived.
;;;
;;; type_name :
;;;     0-terminated string used as the name of the new type.
;;;
;;; class_size :
;;;     Size of the class structure (see GTypeInfo)
;;;
;;; class_init :
;;;     Location of the class initialization function (see GTypeInfo)
;;;
;;; instance_size :
;;;     Size of the instance structure (see GTypeInfo)
;;;
;;; instance_init :
;;;     Location of the instance initialization function (see GTypeInfo)
;;;
;;; flags :
;;;     Bitwise combination of GTypeFlags values.
;;;
;;; Returns :
;;;     The new type identifier.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_register_static_simple"
          g-type-register-static-simple) g-type
  (parent-type g-type)
  (type-name :string)
  (class-size :uint)
  (class-init :pointer)
  (instance-size :uint)
  (instance-init :pointer)
  (flags g-type-flags))

(export 'g-type-register-static-simple)

;;; ----------------------------------------------------------------------------
;;; g_type_register_dynamic ()
;;;
;;; GType g_type_register_dynamic (GType parent_type,
;;;                                const gchar *type_name,
;;;                                GTypePlugin *plugin,
;;;                                GTypeFlags flags);
;;;
;;; Registers type_name as the name of a new dynamic type derived from
;;; parent_type. The type system uses the information contained in the
;;; GTypePlugin structure pointed to by plugin to manage the type and its
;;; instances (if not abstract). The value of flags determines the nature (e.g.
;;; abstract or not) of the type.
;;;
;;; parent_type :
;;;     Type from which this type will be derived.
;;;
;;; type_name :
;;;     0-terminated string used as the name of the new type.
;;;
;;; plugin :
;;;     The GTypePlugin structure to retrieve the GTypeInfo from.
;;;
;;; flags :
;;;     Bitwise combination of GTypeFlags values.
;;;
;;; Returns :
;;;     The new type identifier or G_TYPE_INVALID if registration failed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_register_fundamental ()
;;;
;;; GType g_type_register_fundamental (GType type_id,
;;;                                    const gchar *type_name,
;;;                                    const GTypeInfo *info,
;;;                                    const GTypeFundamentalInfo *finfo,
;;;                                    GTypeFlags flags);
;;;
;;; Registers type_id as the predefined identifier and type_name as the name of
;;; a fundamental type. If type_id is already registered, or a type named
;;; type_name is already registered, the behaviour is undefined. The type system
;;; uses the information contained in the GTypeInfo structure pointed to by info
;;; and the GTypeFundamentalInfo structure pointed to by finfo to manage the
;;; type and its instances. The value of flags determines additional
;;; characteristics of the fundamental type.
;;;
;;; type_id :
;;;     A predefined type identifier.
;;;
;;; type_name :
;;;     0-terminated string used as the name of the new type.
;;;
;;; info :
;;;     The GTypeInfo structure for this type.
;;;
;;; finfo :
;;;     The GTypeFundamentalInfo structure for this type.
;;;
;;; flags :
;;;     Bitwise combination of GTypeFlags values.
;;;
;;; Returns :
;;;     The predefined type identifier.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_static ()
;;;
;;; void g_type_add_interface_static (GType instance_type,
;;;                                   GType interface_type,
;;;                                   const GInterfaceInfo *info);
;;;
;;; Adds the static interface_type to instantiable_type. The information
;;; contained in the GInterfaceInfo structure pointed to by info is used to
;;; manage the relationship.
;;;
;;; instance_type :
;;;     GType value of an instantiable type.
;;;
;;; interface_type :
;;;     GType value of an interface type.
;;;
;;; info :
;;;     The GInterfaceInfo structure for this (instance_type, interface_type)
;;;     combination.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_add_interface_static" g-type-add-interface-static) :void
  (instance-type g-type)
  (interface-type g-type)
  (info (:pointer g-interface-info)))

(export 'g-type-add-interface-static)

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_dynamic ()
;;;
;;; void g_type_add_interface_dynamic (GType instance_type,
;;;                                    GType interface_type,
;;;                                    GTypePlugin *plugin);
;;;
;;; Adds the dynamic interface_type to instantiable_type. The information
;;; contained in the GTypePlugin structure pointed to by plugin is used to
;;; manage the relationship.
;;;
;;; instance_type :
;;;     the GType value of an instantiable type.
;;;
;;; interface_type :
;;;     the GType value of an interface type.
;;;
;;; plugin :
;;;     the GTypePlugin structure to retrieve the GInterfaceInfo from.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_add_prerequisite ()
;;;
;;; void g_type_interface_add_prerequisite (GType interface_type,
;;;                                         GType prerequisite_type);
;;;
;;; Adds prerequisite_type to the list of prerequisites of interface_type. This
;;; means that any type implementing interface_type must also implement
;;; prerequisite_type. Prerequisites can be thought of as an alternative to
;;; interface derivation (which GType doesn't support). An interface can have at
;;; most one instantiatable prerequisite type.
;;;
;;; interface_type :
;;;     GType value of an interface type.
;;;
;;; prerequisite_type :
;;;     GType value of an interface or instantiatable type.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interface_add_prerequisite" g-type-interface-add-prerequisite)
    :void
  (interface-type g-type)
  (prerequisite-type g-type))

(export 'g-type-interface-add-prerequisite)

;;; ----------------------------------------------------------------------------
;;; g_type_get_plugin ()
;;;
;;; GTypePlugin * g_type_get_plugin (GType type);
;;;
;;; Returns the GTypePlugin structure for type or NULL if type does not have a
;;; GTypePlugin structure.
;;;
;;; type :
;;;     The GType to retrieve the plugin for.
;;;
;;; Returns :
;;;     The corresponding plugin if type is a dynamic type, NULL otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_interface_get_plugin ()
;;;
;;; GTypePlugin * g_type_interface_get_plugin (GType instance_type,
;;;                                            GType interface_type);
;;;
;;; Returns the GTypePlugin structure for the dynamic interface interface_type
;;; which has been added to instance_type, or NULL if interface_type has not
;;; been added to instance_type or does not have a GTypePlugin structure. See
;;; g_type_add_interface_dynamic().
;;;
;;; instance_type :
;;;     the GType value of an instantiatable type.
;;;
;;; interface_type :
;;;     the GType value of an interface type.
;;;
;;; Returns :
;;;     the GTypePlugin for the dynamic interface interface_type of
;;;     instance_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_fundamental_next ()
;;;
;;; GType g_type_fundamental_next (void);
;;;
;;; Returns the next free fundamental type id which can be used to register a
;;; new fundamental type with g_type_register_fundamental(). The returned type
;;; ID represents the highest currently registered fundamental type identifier.
;;;
;;; Returns :
;;;     The nextmost fundamental type ID to be registered, or 0 if the type
;;;     system ran out of fundamental type IDs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_fundamental ()
;;;
;;; GType g_type_fundamental (GType type_id);
;;;
;;; Internal function, used to extract the fundamental type ID portion. Use
;;; G_TYPE_FUNDAMENTAL() instead.
;;;
;;; type_id :
;;;     valid type ID
;;;
;;; Returns :
;;;     fundamental type ID
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_fundamental" g-type-fundamental) g-type
  (gtype g-type))

(export 'g-type-fundamental)

;;; ----------------------------------------------------------------------------
;;; g_type_create_instance ()
;;;
;;; GTypeInstance * g_type_create_instance (GType type);
;;;
;;; Creates and initializes an instance of type if type is valid and can be
;;; instantiated. The type system only performs basic allocation and structure
;;; setups for instances: actual instance creation should happen through
;;; functions supplied by the type's fundamental type implementation. So use of
;;; g_type_create_instance() is reserved for implementators of fundamental types
;;; only. E.g. instances of the GObject hierarchy should be created via
;;; g_object_new() and never directly through g_type_create_instance() which
;;; doesn't handle things like singleton objects or object construction. Note:
;;; Do not use this function, unless you're implementing a fundamental type.
;;; Also language bindings should not use this function but g_object_new()
;;; instead.
;;;
;;; type :
;;;     An instantiatable type to create an instance for.
;;;
;;; Returns :
;;;     An allocated and initialized instance, subject to further treatment by
;;;     the fundamental type implementation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_free_instance ()
;;;
;;; void g_type_free_instance (GTypeInstance *instance);
;;;
;;; Frees an instance of a type, returning it to the instance pool for the type,
;;; if there is one.
;;;
;;; Like g_type_create_instance(), this function is reserved for implementors of
;;; fundamental types.
;;;
;;; instance :
;;;     an instance of a type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_class_cache_func ()
;;;
;;; void g_type_add_class_cache_func (gpointer cache_data,
;;;                                   GTypeClassCacheFunc cache_func);
;;;
;;; Adds a GTypeClassCacheFunc to be called before the reference count of a
;;; class goes from one to zero. This can be used to prevent premature class
;;; destruction. All installed GTypeClassCacheFunc functions will be chained
;;; until one of them returns TRUE. The functions have to check the class id
;;; passed in to figure whether they actually want to cache the class of this
;;; type, since all classes are routed through the same GTypeClassCacheFunc
;;; chain.
;;;
;;; cache_data :
;;;     data to be passed to cache_func
;;;
;;; cache_func :
;;;     a GTypeClassCacheFunc
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_remove_class_cache_func ()
;;;
;;; void g_type_remove_class_cache_func (gpointer cache_data,
;;;                                      GTypeClassCacheFunc cache_func);
;;;
;;; Removes a previously installed GTypeClassCacheFunc. The cache maintained by
;;; cache_func has to be empty when calling g_type_remove_class_cache_func() to
;;; avoid leaks.
;;;
;;; cache_data :
;;;     data that was given when adding cache_func
;;;
;;; cache_func :
;;;     a GTypeClassCacheFunc
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_class_unref_uncached ()
;;;
;;; void g_type_class_unref_uncached (gpointer g_class);
;;;
;;; A variant of g_type_class_unref() for use in GTypeClassCacheFunc
;;; implementations. It unreferences a class without consulting the chain of
;;; GTypeClassCacheFuncs, avoiding the recursion which would occur otherwise.
;;;
;;; g_class :
;;;     The GTypeClass structure to unreference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_add_interface_check ()
;;;
;;; void g_type_add_interface_check (gpointer check_data,
;;;                                  GTypeInterfaceCheckFunc check_func);
;;;
;;; Adds a function to be called after an interface vtable is initialized for
;;; any class (i.e. after the interface_init member of GInterfaceInfo has been
;;; called).
;;;
;;; This function is useful when you want to check an invariant that depends on
;;; the interfaces of a class. For instance, the implementation of GObject uses
;;; this facility to check that an object implements all of the properties that
;;; are defined on its interfaces.
;;;
;;; check_data :
;;;     data to pass to check_func
;;;
;;; check_func :
;;;     function to be called after each interface is initialized.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_remove_interface_check ()
;;;
;;; void g_type_remove_interface_check (gpointer check_data,
;;;                                     GTypeInterfaceCheckFunc check_func);
;;;
;;; Removes an interface check function added with g_type_add_interface_check().
;;;
;;; check_data :
;;;     callback data passed to g_type_add_interface_check()
;;;
;;; check_func :
;;;     callback function passed to g_type_add_interface_check()
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTypeInterfaceCheckFunc ()
;;;
;;; void (*GTypeInterfaceCheckFunc) (gpointer check_data, gpointer g_iface);
;;;
;;; A callback called after an interface vtable is initialized. See
;;; g_type_add_interface_check().
;;;
;;; check_data :
;;;     data passed to g_type_add_interface_check().
;;;
;;; g_iface :
;;;     the interface that has been initialized
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_type_value_table_peek ()
;;;
;;; GTypeValueTable * g_type_value_table_peek (GType type);
;;;
;;; Returns the location of the GTypeValueTable associated with type. Note that
;;; this function should only be used from source code that implements or has
;;; internal knowledge of the implementation of type.
;;;
;;; type :
;;;     A GType value.
;;;
;;; Returns :
;;;     Location of the GTypeValueTable associated with type or NULL if there is
;;;     no GTypeValueTable associated with type.
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_value_table_peek" g-type-value-table-peek)
    (:pointer g-type-value-table)
  (type g-type))

(export 'g-type-value-table-peek)

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_TYPE()
;;;
;;; #define G_DEFINE_TYPE(TN, t_n, T_P)
;;;         G_DEFINE_TYPE_EXTENDED (TN, t_n, T_P, 0, {})
;;;
;;; A convenience macro for type implementations, which declares a class
;;; initialization function, an instance initialization function (see GTypeInfo
;;; for information about these) and a static variable named t_n_parent_class
;;; pointing to the parent class. Furthermore, it defines a *_get_type()
;;; function. See G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_TYPE_WITH_CODE(TN, t_n, T_P, _C_)
;;;        _G_DEFINE_TYPE_EXTENDED_BEGIN (TN, t_n, T_P, 0) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for type implementations. Similar to G_DEFINE_TYPE(),
;;; but allows you to insert custom code into the *_get_type() function, e.g.
;;; interface implementations via G_IMPLEMENT_INTERFACE(). See
;;; G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_ABSTRACT_TYPE()
;;;
;;; #define G_DEFINE_ABSTRACT_TYPE(TN, t_n, T_P)
;;;         G_DEFINE_TYPE_EXTENDED (TN, t_n, T_P, G_TYPE_FLAG_ABSTRACT, {})
;;;
;;; A convenience macro for type implementations. Similar to G_DEFINE_TYPE(),
;;; but defines an abstract type. See G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_ABSTRACT_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_ABSTRACT_TYPE_WITH_CODE(TN, t_n, T_P, _C_)
;;;        _G_DEFINE_TYPE_EXTENDED_BEGIN (TN, t_n, T_P, G_TYPE_FLAG_ABSTRACT) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for type implementations. Similar to
;;; G_DEFINE_TYPE_WITH_CODE(), but defines an abstract type and allows you to
;;; insert custom code into the *_get_type() function, e.g. interface
;;; implementations via G_IMPLEMENT_INTERFACE(). See G_DEFINE_TYPE_EXTENDED()
;;; for an example.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the type_name_get_type() function.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_INTERFACE()
;;;
;;; #define G_DEFINE_INTERFACE(TN, t_n, T_P)
;;;         G_DEFINE_INTERFACE_WITH_CODE(TN, t_n, T_P, ;)
;;;
;;; A convenience macro for GTypeInterface definitions, which declares a default
;;; vtable initialization function and defines a *_get_type() function.
;;;
;;; The macro expects the interface initialization function to have the name
;;; t_n ## _default_init, and the interface structure to have the name
;;; TN ## Interface.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the prerequisite type for the interface, or 0
;;;     (G_TYPE_INVALID) for no prerequisite type.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_INTERFACE_WITH_CODE()
;;;
;;; #define G_DEFINE_INTERFACE_WITH_CODE(TN, t_n, T_P, _C_)
;;;        _G_DEFINE_INTERFACE_EXTENDED_BEGIN(TN, t_n, T_P) {_C_;}
;;;        _G_DEFINE_INTERFACE_EXTENDED_END()
;;;
;;; A convenience macro for GTypeInterface definitions. Similar to
;;; G_DEFINE_INTERFACE(), but allows you to insert custom code into the
;;; *_get_type() function, e.g. additional interface implementations via
;;; G_IMPLEMENT_INTERFACE(), or additional prerequisite types. See
;;; G_DEFINE_TYPE_EXTENDED() for a similar example using
;;; G_DEFINE_TYPE_WITH_CODE().
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the prerequisite type for the interface, or 0
;;;     (G_TYPE_INVALID) for no prerequisite type.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IMPLEMENT_INTERFACE()
;;;
;;; #define G_IMPLEMENT_INTERFACE(TYPE_IFACE, iface_init)
;;;
;;; A convenience macro to ease interface addition in the _C_ section of
;;; G_DEFINE_TYPE_WITH_CODE() or G_DEFINE_ABSTRACT_TYPE_WITH_CODE(). See
;;; G_DEFINE_TYPE_EXTENDED() for an example.
;;;
;;; Note that this macro can only be used together with the
;;; G_DEFINE_TYPE_* macros, since it depends on variable names from those
;;; macros.
;;;
;;; TYPE_IFACE :
;;;     The GType of the interface to add
;;;
;;; iface_init :
;;;     The interface init function
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_TYPE_EXTENDED()
;;;
;;; #define G_DEFINE_TYPE_EXTENDED(TN, t_n, T_P, _f_, _C_)
;;;        _G_DEFINE_TYPE_EXTENDED_BEGIN (TN, t_n, T_P, _f_) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; The most general convenience macro for type implementations, on which
;;; G_DEFINE_TYPE(), etc are based.
;;;
;;;   G_DEFINE_TYPE_EXTENDED (GtkGadget,
;;;                           gtk_gadget,
;;;                           GTK_TYPE_WIDGET,
;;;                           0,
;;;                           G_IMPLEMENT_INTERFACE (TYPE_GIZMO,
;;;                                                  gtk_gadget_gizmo_init));
;;;
;;; expands to
;;;
;;;   static void     gtk_gadget_init       (GtkGadget      *self);
;;;   static void     gtk_gadget_class_init (GtkGadgetClass *klass);
;;;   static gpointer gtk_gadget_parent_class = NULL;
;;;   static void     gtk_gadget_class_intern_init (gpointer klass)
;;;   {
;;;     gtk_gadget_parent_class = g_type_class_peek_parent (klass);
;;;     gtk_gadget_class_init ((GtkGadgetClass*) klass);
;;;   }
;;;
;;;   GType
;;;   gtk_gadget_get_type (void)
;;;   {
;;;     static volatile gsize g_define_type_id__volatile = 0;
;;;     if (g_once_init_enter (&g_define_type_id__volatile))
;;;       {
;;;         GType g_define_type_id =
;;;           g_type_register_static_simple
;;;                              (GTK_TYPE_WIDGET,
;;;                               g_intern_static_string ("GtkGadget"),
;;;                               sizeof (GtkGadgetClass),
;;;                               (GClassInitFunc) gtk_gadget_class_intern_init,
;;;                               sizeof (GtkGadget),
;;;                               (GInstanceInitFunc) gtk_gadget_init,
;;;                               (GTypeFlags) flags);
;;;         {
;;;           const GInterfaceInfo g_implement_interface_info = {
;;;             (GInterfaceInitFunc) gtk_gadget_gizmo_init
;;;           };
;;;           g_type_add_interface_static (g_define_type_id,
;;;                                        TYPE_GIZMO,
;;;                                        &g_implement_interface_info);
;;;         }
;;;         g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
;;;       }
;;;     return g_define_type_id__volatile;
;;;   }
;;;
;;; The only pieces which have to be manually provided are the definitions of
;;; the instance and class structure and the definitions of the instance and
;;; class init functions.
;;;
;;; TN :
;;;     The name of the new type, in Camel case.
;;;
;;; t_n :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; T_P :
;;;     The GType of the parent type.
;;;
;;; _f_ :
;;;     GTypeFlags to pass to g_type_register_static()
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_BOXED_TYPE()
;;;
;;; #define G_DEFINE_BOXED_TYPE(TypeName, type_name, copy_func, free_func)
;;;         G_DEFINE_BOXED_TYPE_WITH_CODE (TypeName,
;;;                                        type_name,
;;;                                        copy_func,
;;;                                        free_func, {})
;;;
;;; A convenience macro for boxed type implementations, which defines a
;;; type_name_get_type() function registering the boxed type.
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; copy_func :
;;;     the GBoxedCopyFunc for the new type
;;;
;;; free_func :
;;;     the GBoxedFreeFunc for the new type
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_BOXED_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_BOXED_TYPE_WITH_CODE(TypeName, type_name, copy_func, free_func, _C_)
;;;        _G_DEFINE_BOXED_TYPE_BEGIN (TypeName, type_name, copy_func, free_func) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for boxed type implementations. Similar to
;;; G_DEFINE_BOXED_TYPE(), but allows to insert custom code into the
;;; type_name_get_type() function, e.g. to register value transformations with
;;; g_value_register_transform_func().
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; copy_func :
;;;     the GBoxedCopyFunc for the new type
;;;
;;; free_func :
;;;     the GBoxedFreeFunc for the new type
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_POINTER_TYPE()
;;;
;;; #define G_DEFINE_POINTER_TYPE(TypeName, type_name)
;;;         G_DEFINE_POINTER_TYPE_WITH_CODE (TypeName, type_name, {})
;;;
;;; A convenience macro for pointer type implementations, which defines a
;;; type_name_get_type() function registering the pointer type.
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_DEFINE_POINTER_TYPE_WITH_CODE()
;;;
;;; #define G_DEFINE_POINTER_TYPE_WITH_CODE(TypeName, type_name, _C_)
;;;        _G_DEFINE_POINTER_TYPE_BEGIN (TypeName, type_name) {_C_;}
;;;        _G_DEFINE_TYPE_EXTENDED_END()
;;;
;;; A convenience macro for pointer type implementations. Similar to
;;; G_DEFINE_POINTER_TYPE(), but allows to insert custom code into the
;;; type_name_get_type() function.
;;;
;;; TypeName :
;;;     The name of the new type, in Camel case.
;;;
;;; type_name :
;;;     The name of the new type, in lowercase, with words separated by '_'.
;;;
;;; _C_ :
;;;     Custom code that gets inserted in the *_get_type() function.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

|#

;;; --- End of file gtk-atdoc.gobject.type-info.lisp ---------------------------
