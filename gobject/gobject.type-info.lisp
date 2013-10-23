;;; ----------------------------------------------------------------------------
;;; gobject.type-info.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GObject Reference Manual
;;; Version 2.36.2 and modified to document the Lisp binding to the GObject
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Type Information
;;;
;;; The GLib Runtime type identification and management system
;;;
;;; Synopsis
;;;
;;;     G_TYPE_INVALID
;;;     G_TYPE_NONE
;;;     G_TYPE_INTERFACE
;;;     G_TYPE_CHAR
;;;     G_TYPE_UCHAR
;;;     G_TYPE_BOOLEAN
;;;     G_TYPE_INT
;;;     G_TYPE_UINT
;;;     G_TYPE_LONG
;;;     G_TYPE_ULONG
;;;     G_TYPE_INT64
;;;     G_TYPE_UINT64
;;;     G_TYPE_ENUM
;;;     G_TYPE_FLAGS
;;;     G_TYPE_FLOAT
;;;     G_TYPE_DOUBLE
;;;     G_TYPE_STRING
;;;     G_TYPE_POINTER
;;;     G_TYPE_BOXED
;;;     G_TYPE_PARAM
;;;     G_TYPE_OBJECT
;;;     G_TYPE_GTYPE
;;;     G_TYPE_VARIANT
;;;
;;;     G_TYPE_RESERVED_GLIB_FIRST
;;;     G_TYPE_RESERVED_GLIB_LAST
;;;     G_TYPE_RESERVED_BSE_FIRST
;;;     G_TYPE_RESERVED_BSE_LAST
;;;     G_TYPE_RESERVED_USER_FIRST
;;;
;;;     GTypeFlags
;;;     GTypeFundamentalFlags
;;;     GType
;;;
;;;     G_TYPE_FUNDAMENTAL
;;;     G_TYPE_FUNDAMENTAL_MAX
;;;     G_TYPE_MAKE_FUNDAMENTAL
;;;     G_TYPE_IS_ABSTRACT
;;;     G_TYPE_IS_DERIVED
;;;     G_TYPE_IS_FUNDAMENTAL
;;;     G_TYPE_IS_VALUE_TYPE
;;;     G_TYPE_HAS_VALUE_TABLE
;;;     G_TYPE_IS_CLASSED
;;;     G_TYPE_IS_INSTANTIATABLE
;;;     G_TYPE_IS_DERIVABLE
;;;     G_TYPE_IS_DEEP_DERIVABLE
;;;     G_TYPE_IS_INTERFACE
;;;
;;;     GTypeInterface
;;;     GTypeClass
;;;     GTypeInstance
;;;     GTypeInfo
;;;     GTypeFundamentalInfo
;;;     GInterfaceInfo
;;;     GTypeValueTable
;;;
;;;     G_TYPE_FROM_INSTANCE
;;;     G_TYPE_FROM_CLASS
;;;     G_TYPE_FROM_INTERFACE
;;;     G_TYPE_INSTANCE_GET_CLASS
;;;     G_TYPE_INSTANCE_GET_INTERFACE            * not implemented *
;;;     G_TYPE_INSTANCE_GET_PRIVATE              * not implemented *
;;;     G_TYPE_CLASS_GET_PRIVATE                 * not implemented *
;;;     G_TYPE_CHECK_INSTANCE                    * not implemented *
;;;     G_TYPE_CHECK_INSTANCE_CAST               * not implemented *
;;;     G_TYPE_CHECK_INSTANCE_TYPE
;;;     G_TYPE_CHECK_CLASS_CAST                  * not implemented *
;;;     G_TYPE_CHECK_CLASS_TYPE
;;;     G_TYPE_CHECK_VALUE                       * not implemented *
;;;     G_TYPE_CHECK_VALUE_TYPE                  * not implemented *
;;;     G_TYPE_FLAG_RESERVED_ID_BIT              * not implemented *
;;;
;;;     g_type_init                              * deprecated *
;;;
;;;     GTypeDebugFlags                          * deprecated *
;;;
;;;     g_type_init_with_debug_flags             * deprecated *
;;;     g_type_name
;;;     g_type_qname
;;;     g_type_from_name
;;;     g_type_parent
;;;     g_type_depth
;;;     g_type_next_base
;;;     g_type_is_a
;;;     g_type_class_ref
;;;     g_type_class_peek
;;;     g_type_class_peek_static
;;;     g_type_class_unref
;;;     g_type_class_peek_parent
;;;     g_type_class_add_private
;;;     g_type_add_class_private                 * not implemented *
;;;     g_type_interface_peek
;;;     g_type_interface_peek_parent             * not implemented *
;;;     g_type_default_interface_ref
;;;     g_type_default_interface_peek
;;;     g_type_default_interface_unref
;;;     g_type_children
;;;     g_type_interfaces
;;;     g_type_interface_prerequisites
;;;     g_type_set_qdata
;;;     g_type_get_qdata
;;;
;;;     GTypeQuery
;;;
;;;     g_type_query
;;;     g_type_register_static
;;;     g_type_register_static_simple
;;;     g_type_register_dynamic                  * not implemented *
;;;     g_type_register_fundamental              * not implemented *
;;;     g_type_add_interface_static
;;;     g_type_add_interface_dynamic             * not implemented *
;;;     g_type_interface_add_prerequisite
;;;     g_type_get_plugin                        * not implemented *
;;;     g_type_interface_get_plugin              * not implemented *
;;;     g_type_fundamental_next
;;;     g_type_fundamental
;;;     g_type_create_instance                   * not implemented *
;;;     g_type_free_instance                     * not implemented *
;;;     g_type_add_class_cache_func              * not implemented *
;;;     g_type_remove_class_cache_func           * not implemented *
;;;     g_type_class_unref_uncached              * not implemented *
;;;     g_type_add_interface_check               * not implemented *
;;;     g_type_remove_interface_check            * not implemented *
;;;     g_type_value_table_peek
;;;     g_type_ensure
;;;     g_type_get_type_registration_serial
;;;
;;;     G_DEFINE_TYPE                            * not implemented *
;;;     G_DEFINE_TYPE_WITH_CODE                  * not implemented *
;;;     G_DEFINE_ABSTRACT_TYPE                   * not implemented *
;;;     G_DEFINE_ABSTRACT_TYPE_WITH_CODE         * not implemented *
;;;     G_DEFINE_INTERFACE                       * not implemented *
;;;     G_DEFINE_INTERFACE_WITH_CODE             * not implemented *
;;;     G_IMPLEMENT_INTERFACE                    * not implemented *
;;;     G_DEFINE_TYPE_EXTENDED                   * not implemented *
;;;     G_DEFINE_BOXED_TYPE                      * not implemented *
;;;     G_DEFINE_BOXED_TYPE_WITH_CODE            * not implemented *
;;;     G_DEFINE_POINTER_TYPE                    * not implemented *
;;;     G_DEFINE_POINTER_TYPE_WITH_CODE          * not implemented *
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; This constant is not exported.
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defconstant +g-type-fundamental-shift+ 2))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INVALID
;;; ----------------------------------------------------------------------------

(defconstant +g-type-invalid+ #.(ash 0 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 0)}}
  An invalid @class{g-type} used as error return value in some functions which
  return a @class{g-type}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-invalid+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_NONE
;;; ----------------------------------------------------------------------------

(defconstant +g-type-none+ #.(ash 1 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @variable-value{@code{(g-type-make-fundamental 1)}}
  A fundamental type which is used as a replacement for the C void return type.
  @see-function{g-type-make-fundamental}")

(export '+g-type-none+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INTERFACE
;;; ----------------------------------------------------------------------------

(defconstant +g-type-interface+ #.(ash 2 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 2)}}
  The fundamental type from which all interfaces are derived.
  @see-function{g-type-make-fundamental}")

(export '+g-type-interface+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHAR
;;; ----------------------------------------------------------------------------

(defconstant +g-type-char+ #.(ash 3 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 3)}}
  The fundamental type corresponding to @code{gchar}. The type designated by
  @code{G_TYPE_CHAR} is unconditionally an 8-bit signed integer. This may or may
  not be the same type a the C type \"@code{gchar}\".
  @see-function{g-type-make-fundamental}")

(export '+g-type-char+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_UCHAR
;;; ----------------------------------------------------------------------------

(defconstant +g-type-uchar+ #.(ash 4 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 4)}}
  The fundamental type corresponding to @code{guchar}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-uchar+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_BOOLEAN
;;; ----------------------------------------------------------------------------

(defconstant +g-type-boolean+ #.(ash 5 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 5)}}
  The fundamental type corresponding to @code{gboolean}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-boolean+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INT
;;; ----------------------------------------------------------------------------

(defconstant +g-type-int+ #.(ash 6 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 6)}}
  The fundamental type corresponding to @code{gint}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-int+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_UINT
;;; ----------------------------------------------------------------------------

(defconstant +g-type-uint+ #.(ash 7 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 7)}}
  The fundamental type corresponding to @code{guint}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-uint+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_LONG
;;; ----------------------------------------------------------------------------

(defconstant +g-type-long+ #.(ash 8 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 8)}}
  The fundamental type corresponding to @code{glong}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-long+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_ULONG
;;; ----------------------------------------------------------------------------

(defconstant +g-type-ulong+ #.(ash 9 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 9)}}
  The fundamental type corresponding to @code{gulong}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-ulong+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INT64
;;; ----------------------------------------------------------------------------

(defconstant +g-type-int64+ #.(ash 10 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 10)}}
  The fundamental type corresponding to @code{gint64}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-int64+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_UINT64
;;; ----------------------------------------------------------------------------

(defconstant +g-type-uint64+ #.(ash 11 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 11)}}
  The fundamental type corresponding to @code{guint64}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-uint64+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_ENUM
;;; ----------------------------------------------------------------------------

(defconstant +g-type-enum+ #.(ash 12 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 12)}}
  The fundamental type from which all enumeration types are derived.
  @see-function{g-type-make-fundamental}")

(export '+g-type-enum+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FLAGS
;;; ----------------------------------------------------------------------------

(defconstant +g-type-flags+ #.(ash 13 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 13)}}
  The fundamental type from which all flags types are derived.
  @see-function{g-type-make-fundamental}")

(export '+g-type-flags+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FLOAT
;;; ----------------------------------------------------------------------------

(defconstant +g-type-float+ #.(ash 14 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 14)}}
  The fundamental type corresponding to @code{gfloat}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-float+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_DOUBLE
;;; ----------------------------------------------------------------------------

(defconstant +g-type-double+ #.(ash 15 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 15)}}
  The fundamental type corresponding to @code{gdouble}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-double+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_STRING
;;; ----------------------------------------------------------------------------

(defconstant +g-type-string+ #.(ash 16 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 16)}}
  The fundamental type corresponding to nul-terminated C strings.
  @see-function{g-type-make-fundamental}")

(export '+g-type-string+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_POINTER
;;; ----------------------------------------------------------------------------

(defconstant +g-type-pointer+ #.(ash 17 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 17)}}
  The fundamental type corresponding to @code{gpointer}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-pointer+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_BOXED
;;; ----------------------------------------------------------------------------

(defconstant +g-type-boxed+ #.(ash 18 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 18)}}
  The fundamental type from which all boxed types are derived.
  @see-function{g-type-make-fundamental}")

(export '+g-type-boxed+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM
;;; ----------------------------------------------------------------------------

(defconstant +g-type-param+ #.(ash 19 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 19)}}
  The fundamental type from which all @code{GParamSpec} types are derived.
  @see-function{g-type-make-fundamental}")

(export '+g-type-param+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_OBJECT
;;; ----------------------------------------------------------------------------

(defconstant +g-type-object+ #.(ash 20 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 20)}}
  The fundamental type for @code{GObject}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-object+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_GTYPE
;;; ----------------------------------------------------------------------------

(defcfun ("g_gtype_get_type" %g-type-gtype) g-size)

(declaim (inline g-type-gtype))

(defun g-type-gtype ()
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @short{The type for @code{GType}.}
  @begin[Example]{dictionary}
    @begin{pre}
 (g-type-gtype)
=> #S(GTYPE :NAME \"GType\" :%ID 134819824)
    @end{pre}
  @end{dictionary}
  @see-class{g-type}"
  (gtype (%g-type-gtype)))

(export 'g-type-gtype)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VARIANT
;;; ----------------------------------------------------------------------------

(defconstant +g-type-variant+ #.(ash 21 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-6}
  @variable-value{@code{(g-type-make-fundamental 21)}}
  @short{The fundamental type corresponding to @code{GVariant}.}

  All floating @code{GVariant} instances passed through the @code{GType} system
  are consumed.

  Note that callbacks in closures, and signal handlers for signals of return
  type @code{G_TYPE_VARIANT}, must never return floating variants.

  Note: GLib 2.24 did include a boxed type with this name. It was replaced
  with this fundamental type in 2.26.

  Since 2.26
  @see-function{g-type-make-fundamental}")

(export '+g-type-variant+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_GLIB_FIRST
;;; ----------------------------------------------------------------------------

(defconstant +g-type-reserved-glib-first+ 22
 #+cl-cffi-gtk-documentation
 "@version{2013-3-31}
  @variable-value{22}
  First fundamental type number to create a new fundamental type ID with
  @fun{g-type-make-fundamental} reserved for GLib.")

(export '+g-type-reserved-glib-first+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_GLIB_LAST
;;; ----------------------------------------------------------------------------

(defconstant +g-type-reserved-glib-last+ 31
 #+cl-cffi-gtk-documentation
 "@version{2013-3-31}
  @variable-value{31}
  Last fundamental type number reserved for GLib.")

(export '+g-type-reserved-glib-last+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_BSE_FIRST
;;; ----------------------------------------------------------------------------

(defconstant +g-type-reserved-bse-first+ 32
 #+cl-cffi-gtk-documentation
 "@version{2013-3-31}
  @variable-value{32}
  First fundamental type number to create a new fundamental type ID with
  @fun{g-type-make-fundamental} reserved for BSE.")

(export '+g-type-reserved-bse-first+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_BSE_LAST
;;; ----------------------------------------------------------------------------

(defconstant +g-type-reserved-bse-last+ 48
 #+cl-cffi-gtk-documentation
 "@version{2013-3-31}
  @variable-value{48}
  Last fundamental type number reserved for BSE.")

(export '+g-type-reserved-bse-last+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_RESERVED_USER_FIRST
;;; ----------------------------------------------------------------------------

(defconstant +g-type-reserved-user-first+ 49
 #+cl-cffi-gtk-documentation
 "@version{2013-3-31}
  @variable-value{49}
  First available fundamental type number to create new fundamental type ID
  with @fun{g-type-make-fundamental}.
  @see-function{g-type-make-fundamental}")

(export '+g-type-reserved-user-first+)

;;; ----------------------------------------------------------------------------
;;; enum GTypeFlags
;;; ----------------------------------------------------------------------------

(defbitfield g-type-flags
  (:abstract #.(ash 1 4))
  (:value-abstract #.(ash 1 5)))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-type-flags atdoc:*symbol-name-alias*) "Bitfield"
      (gethash 'g-type-flags atdoc:*external-symbols*)
 "@version{2013-8-28}
  @short{Bit masks used to check or determine characteristics of a type.}
  See the function @fun{g-type-is-abstract} to check a type for the flag
  @code{:abstract} and the function @fun{g-type-is-value-abstract} to check a
  type for the flag @code{:value-abstract}.
  @begin{pre}
(defbitfield g-type-flags
  (:abstract #.(ash 1 4))
  (:value-abstract #.(ash 1 5)))
  @end{pre}
  @begin[code]{table}
    @entry[:abstract]{Indicates an abstract type. No instances can be created
      for an abstract type.}
    @entry[:value-abstract]{Indicates an abstract value type, i. e. a type that
      introduces a value table, but can not be used for the function
      @fun{g-value-init}.}
  @end{table}
  @see-class{g-type}
  @see-function{g-value-init}
  @see-function{g-type-is-abstract}
  @see-function{g-type-is-value-abstract}")

(export 'g-type-flags)

;;; ----------------------------------------------------------------------------
;;; enum GTypeFundamentalFlags
;;; ----------------------------------------------------------------------------

(defbitfield g-type-fundamental-flags
  :classed
  :instantiatable
  :derivable
  :deep-derivable)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-type-fundamental-flags atdoc:*symbol-name-alias*) "Bitfield"
      (gethash 'g-type-fundamental-flags atdoc:*external-symbols*)
 "@version{2013-8-28}
  @begin{short}
    Bit masks used to check or determine specific characteristics of a
   fundamental type.
  @end{short}
  See the functions @fun{g-type-is-classed}, @fun{g-type-is-instantiatable},
  and @fun{g-type-is-deep-variable} to check a type for these flags.
  @begin{pre}
(defbitfield g-type-fundamental-flags
  :classed
  :instantiatable
  :derivable
  :deep-derivable)
  @end{pre}
  @begin[code]{table}
    @entry[:classed]{Indicates a classed type.}
    @entry[:instantiatable]{Indicates an instantiable type (implies classed).}
    @entry[:derivable]{Indicates a flat derivable type.}
    @entry[:deep-derivable]{Indicates a deep derivable type (implies
      derivable).}
  @end{table}
  @see-class{g-type}
  @see-function{g-type-is-classed}
  @see-function{g-type-is-instantiatable}
  @see-function{g-type-is-derivable}
  @see-function{g-type-is-deep-derivable}")

(export 'g-type-fundamental-flags)

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

(defstruct (gtype
             ;; a print function to get nice output
             (:print-function
               (lambda (struct stream depth)
                 (declare (ignore depth))
                 (print-unreadable-object (struct stream)
                   (format stream "GTYPE :name \"~A\" :id ~D"
                                  (gtype-name struct)
                                  (gtype-%id struct))))))
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

(glib::at-finalize () (invalidate-gtypes))

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

;; Make a Lisp representation gtype from a name or an id

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
               the @code{G_SIGNAL_TYPE_STATIC_SCOPE} flag"))
  (:documentation
    "@version{2013-8-22}
     Values of this CFFI foreign type @sym{g-type} identify the C GType.
     @sym{g-type} is designated by its name (a string) or a numeric identifier.
     Functions accept @sym{g-type} designators as a string or integer and return
     them as a string. The functions @fun{g-type-name} and
     @fun{g-type-from-name} are used to convert between the name and the numeric
     identifier. Numeric identifier of @sym{g-type} may be different between
     different program runs. But string identifier of @sym{g-type} does not
     change.
     @see-function{g-type-name}
     @see-function{g-type-from-name}")
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

;;; See the function g_type_fundamental.

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FUNDAMENTAL_MAX
;;; ----------------------------------------------------------------------------

(defconstant +g-type-fundamental-max+
             #.(ash 255 +g-type-fundamental-shift+)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-31}
  @variable-value{1020}
  An integer constant that represents the number of identifiers reserved for
  types that are assigned at compile time.")

(export '+g-type-fundamental-max+)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MAKE_FUNDAMENTAL()
;;; ----------------------------------------------------------------------------

(defun g-type-make-fundamental (x)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[x]{a fundamental type number}
  @return{The type ID of the @class{g-type}.}
  @begin{short}
    Get the type ID for the fundamental type number @arg{x}. Use the function
    @fun{g-type-fundamental-next} instead of this function to create new
    fundamental types.
  @end{short}
  @subheading{Note}
    @sym{g-type-make-fundamental} does not return a Lisp @class{g-type} value,
    but the ID number of the @class{g-type}.
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-make-fundamental 5)
=> 20
 (gtype (g-type-make-fundamental 5))
=> <GTYPE :name \"gboolean\" :id 20>
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-function{g-type-fundamental-next}"
  (ash x +g-type-fundamental-shift+))

(export 'g-type-make-fundamental)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_ABSTRACT()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_test_flags" %g-type-test-flags) :boolean
  (type g-type)
  (flag g-type-flags))

(defun g-type-is-abstract (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[type]{a @class{g-type}}
  @return{@em{True} if @arg{type} is an abstract type.}
  @begin{short}
    Checks if @arg{type} is an abstract type. An abstract type cannot be
    instantiated and is normally used as an abstract base class for derived
    classes.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-is-abstract \"GtkWidget\")
=> T
 (g-type-is-abstract \"GtkButton\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{g-type}"
  (%g-type-test-flags type :abstract))

(export 'g-type-is-abstract)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_DERIVED()
;;; ----------------------------------------------------------------------------

(defun g-type-is-derived (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[type]{a @class{g-type}}
  @return{@em{True} if @arg{type} is a dervied type.}
  @begin{short}
    Checks if @arg{type} is derived or in object oriented terminology inherited
    from another type. This holds true for all non-fundamental types.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-is-derived \"gboolean\")
=> NIL
 (g-type-is-derived \"GObject\")
=> NIL
 (g-type-is-derived \"GtkWidget\")
=> T
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-function{g-type-is-fundamental}"
  (> (gtype-id (gtype type)) +g-type-fundamental-max+))

(export 'g-type-is-derived)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_FUNDAMENTAL()
;;; ----------------------------------------------------------------------------

(defun g-type-is-fundamental (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[type]{a @class{g-type}}
  @return{@em{True} if @arg{type} is a fundamental type.}
  @begin{short}
    Checks if @arg{type} is a fundamental type.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-is-fundamental \"gboolean\")
=> T
 (g-type-is-fundamental \"GObject\")
=> T
 (g-type-is-fundamental \"GtkWidget\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-function{g-type-is-derived}"
  (<= (gtype-id (gtype type)) +g-type-fundamental-max+))

(export 'g-type-is-fundamental)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_VALUE_TYPE()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_check_is_value_type" %g-type-check-is-value-type) :boolean
  (type g-type))

(defun g-type-is-value-type (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[type]{a @class{g-type}}
  @return{@arg{True} if @arg{type} is a value type.}
  @begin{short}
    Checks if @arg{type} is a value type and can be used with the function
    @fun{g-value-init}.
  @end{short}
  @see-class{g-type}
  @see-function{g-value-init}"
  (%g-type-check-is-value-type type))

(export 'g-type-is-value-type)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_HAS_VALUE_TABLE()
;;; ----------------------------------------------------------------------------

(defun g-type-has-value-table (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[type]{a @class{g-type}}
  @return{@em{True} if @arg{type} has a @symbol{g-type-value-table}.}
  @begin{short}
    Checks if @arg{type} has a @symbol{g-type-value-table}.
  @end{short}
  The function @sym{g-type-has-value-table} calls the function
  @fun{g-type-value-table-peek} to do the check.
  @see-class{g-type}
  @see-symbol{g-type-value-table}
  @see-function{g-type-value-table-peek}"
  (not (null-pointer-p (g-type-value-table-peek type))))

(export 'g-type-has-value-table)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_CLASSED()
;;; ----------------------------------------------------------------------------

;; We need this second variant of the function g_type_test_flags(), because
;; we can not pass a flag of type g-type-fundamental-flag to our first version.
;; See the implemenation for G_TYPE_IS_ABSTRACT.

(defcfun ("g_type_test_flags" %g-type-test-fundamental-flags) :boolean
  (type g-type)
  (flag g-type-fundamental-flags))

(defun g-type-is-classed (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[type]{a @class{g-type}}
  @return{@arg{True} if @arg{type} is a classed type.}
  Checks if @arg{type} is a classed type.
  @see-class{g-type}"
  (%g-type-test-fundamental-flags type :classed))

(export 'g-type-is-classed)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_INSTANTIATABLE()
;;; ----------------------------------------------------------------------------

(defun g-type-is-instantiatable (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[type]{a @class{g-type}}
  @return{@em{True} if @arg{type} can be instantiated.}
  @begin{short}
    Checks if @arg{type} can be instantiated.
  @end{short}
  Instantiation is the process of creating an instance (object) of this type.
  @see-class{g-type}"
  (%g-type-test-fundamental-flags type :instantiatable))

(export 'g-type-is-instantiatable)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_DERIVABLE()
;;; ----------------------------------------------------------------------------

(defun g-type-is-derivable (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[type]{a @class{g-type}}
  @return{@em{True} if @arg{type} is a derivable type.}
  @begin{short}
    Checks if @arg{type} is a derivable type.
  @end{short}
  A derivable type can be used as the base class of a flat (single-level) class
  hierarchy.
  @see-class{g-type}
  @see-function{g-type-is-deep-derivable}"
  (%g-type-test-fundamental-flags type :derivable))

(export 'g-type-is-derivable)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_DEEP_DERIVABLE()
;;; ----------------------------------------------------------------------------

(defun g-type-is-deep-derivable (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[type]{a @class{g-type}}
  @return{@em{True} if @arg{type} is a deep derivable type.}
  @begin{short}
    Checks if @arg{type} is a deep derivable type.
  @end{short}
  A deep derivable type can be used as the base class of a deep (multi-level)
  class hierarchy.
  @see-class{g-type}
  @see-function{g-type-is-derivable}"
  (%g-type-test-fundamental-flags type :deep-derivable))

(export 'g-type-is-deep-derivable)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IS_INTERFACE()
;;; ----------------------------------------------------------------------------

(defun g-type-is-interface (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[type]{a @class{g-type}}
  @return{@em{True} if @arg{type} is an interface type.}
  @begin{short}
    Checks if @arg{type} is an interface type.
  @end{short}
  An interface type provides a pure API, the implementation of which is provided
  by another type, which is then said to conform to the interface. GLib
  interfaces are somewhat analogous to Java interfaces and C++ classes
  containing only pure virtual functions, with the difference that GType
  interfaces are not derivable, but see the function
  @fun{g-type-interface-add-prerequisite} for an alternative.
  @see-class{g-type}
  @see-symbol{g-type-interface}
  @see-function{g-type-interface-add-prerequisite}"
  (eql +g-type-interface+ (gtype-id (g-type-fundamental type))))

(export 'g-type-is-interface)

;;; ----------------------------------------------------------------------------
;;; struct GTypeInterface
;;; ----------------------------------------------------------------------------

(defcstruct g-type-interface
  (:type g-type)
  (:instance-type g-type))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-type-interface atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-type-interface atdoc:*external-symbols*)
 "@version{2013-8-28}
  @short{An opaque structure used as the base of all interface types.}
  @begin{pre}
(defcstruct g-type-interface
  (:type g-type)
  (:instance-type g-type))
  @end{pre}
  @see-class{g-type}
  @see-symbol{g-type-class}
  @see-symbol{g-type-instance}
  @see-function{g-type-is-interface}")

(export 'g-type-interface)

;;; ----------------------------------------------------------------------------
;;; struct GTypeClass
;;; ----------------------------------------------------------------------------

(defcstruct g-type-class
  (:type g-type))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-type-class atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-type-class atdoc:*external-symbols*)
 "@version{2013-8-28}
  @short{An opaque structure used as the base of all classes.}
  @begin{pre}
(defcstruct g-type-class
  (:type g-type))
  @end{pre}
  @see-class{g-type}
  @see-symbol{g-type-interface}
  @see-symbol{g-type-instance}")

(export 'g-type-class)

;;; ----------------------------------------------------------------------------
;;; struct GTypeInstance
;;; ----------------------------------------------------------------------------

(defcstruct g-type-instance
  (:class (:pointer (:struct g-type-class))))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-type-instance atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-type-instance atdoc:*external-symbols*)
 "@version{2013-8-28}
  @short{An opaque structure used as the base of all type instances.}
  @begin{pre}
(defcstruct g-type-instance
  (:class (:pointer (:struct g-type-class))))
  @end{pre}
  @see-symbol{g-type-class}
  @see-symbol{g-type-interface}")

(export 'g-type-instance)

;;; ----------------------------------------------------------------------------
;;; struct GTypeInfo
;;; ----------------------------------------------------------------------------

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

#+cl-cffi-gtk-documentation
(setf (gethash 'g-type-info atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-type-info atdoc:*external-symbols*)
 "@version{2013-8-28}
  @begin{short}
    This structure is used to provide the type system with the information
    required to initialize and destruct (finalize) a type's class and its
    instances.
  @end{short}
  The initialized structure is passed to the function
  @fun{g-type-register-static}. The type system will perform a deep copy of this
  structure, so its memory does not need to be persistent across invocation of
  the function @fun{g-type-register-static}.
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
  @begin[code]{table}
    @begin[:class-size]{entry}
      Size of the class structure, required for interface, classed and
      instantiatable types.
    @end{entry}
    @begin[:base-init-fn]{entry}
      Location of the base initialization function (optional).
    @end{entry}
    @begin[:base-finalize-fn]{entry}
      Location of the base finalization function (optional).
    @end{entry}
    @begin[:class-init-fn]{entry}
      Location of the class initialization function for classed and
      instantiatable types. Location of the default vtable inititalization
      function for interface types (optional). This function is used both to
      fill in virtual functions in the class or default vtable, and to do type
      specific setup such as registering signals and object properties.
    @end{entry}
    @begin[:class-finalize-fn]{entry}
      Location of the class finalization function for classed and
      instantiatable types. Location fo the default vtable finalization
      function for interface types (optional).
    @end{entry}
    @begin[:class-data]{entry}
      User supplied data passed to the class init/finalize functions.
    @end{entry}
    @begin[:instance-size]{entry}
      Size of the instance (object) structure (required for instantiatable
      types only).
    @end{entry}
    @begin[:n-preallocs]{entry}
      Prior to GLib 2.10, it specified the number of pre-allocated (cached)
      instances to reserve memory for (0 indicates no caching). Since
      GLib 2.10, it is ignored, since instances are allocated with the slice
      allocator now.
    @end{entry}
    @begin[:instance-init-fn]{entry}
      Location of the instance initialization function (optional, for
      instantiatable types only).
    @end{entry}
    @begin[:value-table]{entry}
      A @symbol{g-type-value-table} function table for generic handling of
      @symbol{g-value}s of this type, usually only useful for fundamental
      types.
    @end{entry}
  @end{table}
  @see-symbol{g-value}
  @see-symbol{g-type-value-table}
  @see-function{g-type-register-static}")

(export 'g-type-info)

;;; ----------------------------------------------------------------------------
;;; struct GTypeFundamentalInfo
;;; ----------------------------------------------------------------------------

(defcstruct g-type-fundamental-info
  (:type-flags g-type-fundamental-flags))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-type-fundamental-info atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-type-fundamental-info atdoc:*external-symbols*)
 "@version{2013-4-1}
  @begin{short}
    A structure that provides information to the type system which is used
    specifically for managing fundamental types.
  @end{short}
  @begin{pre}
(defcstruct g-type-fundamental-info
  (:type-flags g-type-fundamental-flags))
  @end{pre}
  @begin[code]{table}
    @entry[type-flags]{@symbol{g-type-fundamental-flags} describing
      the characteristics of the fundamental type.}
  @end{table}
  @see-symbol{g-type-fundamental-flags}")

(export 'g-type-fundamental-info)

;;; ----------------------------------------------------------------------------
;;; struct GInterfaceInfo
;;; ----------------------------------------------------------------------------

(defcstruct g-interface-info
  (:interface-init :pointer)
  (:interface-finalize :pointer)
  (:interface-data :pointer))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-interface-info atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-interface-info atdoc:*external-symbols*)
 "@version{2013-8-28}
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
  @begin[@code]{table}
    @entry[:interface-init]{Location of the interface initialization function.}
    @entry[:interface-finalize]{Location of the interface finalization
      function.}
    @entry[:interface-data]{User supplied data passed to the interface
      init/finalize functions.}
  @end{table}
  @see-symbol{g-type-interface}")

(export 'g-interface-info)

;;; ----------------------------------------------------------------------------
;;; struct GTypeValueTable
;;; ----------------------------------------------------------------------------

(defcstruct g-type-value-table
  (:value-init :pointer)
  (:value-free :pointer)
  (:value-copy :pointer)
  (:value-peek-pointer :pointer)
  (:collect-format (:string :free-from-foreign nil :free-to-foreign nil))
  (:collect-value :pointer)
  (:lcopy-format (:string :free-from-foreign nil :free-to-foreign nil))
  (:lcopy-value :pointer))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-type-value-table atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-type-value-table atdoc:*external-symbols*)
 "@version{2013-8-28}
  @begin{short}
    The @sym{g-type-value-table} provides the functions required by the
    @symbol{g-value} implementation, to serve as a container for values of a
    type.
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
  @begin[code]{table}
    @begin[:value-init]{entry}
      Default initialize values contents by poking values directly into the
      @code{value->data array}. The data array of the @symbol{g-value} passed
      into this function was zero-filled with @code{memset()}, so no care has to
      be taken to free any old contents. E. g. for the implementation of a
      string value that may never be @code{NULL}, the implementation might look
      like:
      @begin{pre}
 value->data[0].v_pointer = g_strdup (\"\");
      @end{pre}
    @end{entry}
    @begin[:value-free]{entry}
      Free any old contents that might be left in the data array of the passed
      in value. No resources may remain allocated through the @symbol{g-value}
      contents after this function returns. E. g. for our above string type:
      @begin{pre}
 // only free strings without a specific flag for static storage
 if (!(value->data[1].v_uint & G_VALUE_NOCOPY_CONTENTS))
 g_free (value->data[0].v_pointer);
      @end{pre}
    @end{entry}
    @begin[:value-copy]{entry}
      @arg{dest-value} is a @symbol{g-value} with zero-filled data section and
      @arg{src-value} is a properly setup @symbol{g-value} of same or derived
      type. The purpose of this function is to copy the contents of
      @arg{src-value} into @arg{dest-value} in a way, that even after
      @arg{src-value} has been freed, the contents of @arg{dest-value} remain
      valid. String type example:
      @begin{pre}
 dest_value->data[0].v_pointer = g_strdup (src_value->data[0].v_pointer);
      @end{pre}
    @end{entry}
    @begin[:value-peek-pointer]{entry}
      If the value contents fit into a pointer, such as objects or strings,
      return this pointer, so the caller can peek at the current contents. To
      extend on our above string example:
      @begin{pre}
 return value->data[0].v_pointer;
      @end{pre}
    @end{entry}
    @begin[:collect-format]{entry}
      A string format describing how to collect the contents of this value
      bit-by-bit. Each character in the format represents an argument to be
      collected, and the characters themselves indicate the type of the
      argument. Currently supported arguments are:
      @begin{table}
        @entry['i']{Integers, passed as @code{collect_values[].v_int}.}
        @entry['l']{Longs, passed as @code{collect_values[].v_long}.}
        @entry['d']{Doubles, passed as @code{collect_values[].v_double}.}
        @entry['p']{Pointers, passed as @code{collect_values[].v_pointer}.}
      @end{table}
      It should be noted that for variable argument list construction, ANSI C
      promotes every type smaller than an integer to an int, and floats to
      doubles. So for collection of short int or char, 'i' needs to be used,
      and for collection of floats 'd'.
    @end{entry}
    @begin[:collect-value]{entry}
      The @code{collect_value()} function is responsible for converting the
      values collected from a variable argument list into contents suitable for
      storage in a @symbol{g-value}. This function should setup value similar to
      @code{value_init()}; e. g. for a string value that does not allow
      @code{NULL} pointers, it needs to either spew an error, or do an implicit
      conversion by storing an empty string. The value passed in to this
      function has a zero-filled data array, so just like for
      @code{value_init()} it is guaranteed to not contain any old contents that
      might need freeing. @code{n_collect_values} is exactly the string length
      of collect_format, and @code{collect_values} is an array of unions
      @code{GTypeCValue} with length @code{n_collect_values}, containing the
      collected values according to  @code{collect_format}. @code{collect_flags}
      is an argument provided as a hint by the caller. It may contain the flag
      @code{G_VALUE_NOCOPY_CONTENTS} indicating, that the collected value
      contents may be considered \"static\" for the duration of the value
      lifetime. Thus an extra copy of the contents stored in
      @code{collect_values} is not required for assignment to value. For our
      above string example, we continue with:
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
      @code{G_VALUE_NOCOPY_CONTENTS} hint for reference counted types. Due to
      reentrancy requirements and reference count assertions performed by the
      signal emission code, reference counts should always be incremented for
      reference counted contents stored in the @code{value->data} array. To
      deviate from our string example for a moment, and taking a look at an
      exemplary implementation for @code{collect_value()} of GObject:
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
      of @code{collect_flags}. For invalid objects, the example returns a newly
      allocated string without altering value. Upon success,
      @code{collect_value()} needs to return @code{NULL}. If, however, an error
      condition occurred, @code{collect_value()} may spew an error by returning
      a newly allocated non-@code{NULL} string, giving a suitable description of
      the error condition. The calling code makes no assumptions about the value
      contents being valid upon error returns, value is simply thrown away
      without further freeing. As such, it is a good idea to not allocate
      @symbol{g-value} contents, prior to returning an error, however,
      @code{collect_values()} is not obliged to return a correctly setup value
      for error returns, simply because any non-@code{NULL} return is considered
      a fatal condition so further program behaviour is undefined.
    @end{entry}
    @begin[:lcopy-format]{entry}
      Format description of the arguments to collect for @code{lcopy_value},
      analogous to @code{collect_format}. Usually, @code{lcopy_format} string
      consists only of 'p's to provide @code{lcopy_value()} with pointers to
      storage locations.
    @end{entry}
    @begin[:lcopy-value]{entry}
      This function is responsible for storing the value contents into
      arguments passed through a variable argument list which got collected
      into @code{collect_values} according to @code{lcopy_format}.
      @code{n_collect_values} equals the string length of @code{lcopy_format},
      and collect_flags may contain @code{G_VALUE_NOCOPY_CONTENTS}. In contrast
      to @code{collect_value()}, @code{lcopy_value()} is obliged to always
      properly support @code{G_VALUE_NOCOPY_CONTENTS}. Similar to
      @code{collect_value()} the function may prematurely abort by returning a
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
      And an illustrative version of @code{lcopy_value()} for reference-counted
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
  @end{table}
  @see-symbol{g-value}")

(export 'g-type-value-table)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FROM_INSTANCE()
;;; ----------------------------------------------------------------------------

(defun g-type-from-instance (instance)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[instance]{a valid @symbol{g-type-instance} structure}
  @return{The @class{g-type} of @arg{instance}.}
  @short{Get the type identifier from a given @arg{instance} structure.}

  This function should only be used in type implementations.
  @begin[Example]{dictionary}
    @begin{pre}
 (g-type-from-instance (make-instance 'gtk-button))
=> #<GTYPE :name \"GtkButton\" :id 134914152>
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-symbol{g-type-instance}
  @see-function{g-type-from-class}"
  (let ((ptr (if (pointerp instance) instance (pointer instance))))
    (g-type-from-class (foreign-slot-value ptr
                                           '(:struct g-type-instance) :class))))

(export 'g-type-from-instance)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FROM_CLASS()
;;; ----------------------------------------------------------------------------

(defun g-type-from-class (class)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[class]{valid @symbol{g-type-class} structure}
  @return{The @class{g-type} of @arg{class}.}
  @short{Get the type identifier from a given @arg{class} structure.}

  This macro should only be used in type implementations.
  @begin[Example]{dictionary}
    @begin{pre}
 (g-type-from-class (g-type-class-ref \"GtkWidget\"))
=> #<GTYPE :name \"GtkWidget\" :id 134921656>
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-symbol{g-type-class}
  @see-function{g-type-from-instance}"
  (foreign-slot-value class '(:struct g-type-class) :type))

(export 'g-type-from-class)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_FROM_INTERFACE()
;;; ----------------------------------------------------------------------------

(defun g-type-from-interface (interface)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[interface]{location of a valid @symbol{g-type-interface} structure}
  @return{The @class{g-type} of @arg{interface}.}
  @short{Get the type identifier from a given @arg{interface} structure.}

  This macro should only be used in type implementations.
  @begin[Example]{dictionary}
    @begin{pre}
 (g-type-from-interface (g-type-default-interface-ref \"GtkOrientable\"))
=> #<GTYPE :name \"GtkOrientable\" :id 134920864>
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-symbol{g-type-interface}
  @see-function{g-type-from-instance}
  @see-function{g-type-from-class}"
  (foreign-slot-value interface '(:struct g-type-interface) :type))

(export 'g-type-from-interface)

;;; ----------------------------------------------------------------------------
;;; G_TYPE_INSTANCE_GET_CLASS()
;;; ----------------------------------------------------------------------------

(defun g-type-instance-get-class (instance)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-29}
  @argument[instance]{the @symbol{g-type-instance} structure}
  @return{The class structure of @arg{instance}.}
  @begin{short}
    Get the class structure of a given @arg{instance}.
  @end{short}

  This function should only be used in type implementations.
  @begin[Example]{dictionary}
    @begin{pre}
 (g-type-instance-get-class (make-instance 'gtk-button))
=> #.(SB-SYS:INT-SAP #X0813E608)
 (g-type-from-class *)
=> #<GTYPE :name \"GtkButton\" :id 134914160>
    @end{pre}
  @end{dictionary}
  @see-symbol{g-type-instance}
  @see-symbol{g-type-class}"
  (let ((ptr (if (pointerp instance) instance (pointer instance))))
    (foreign-slot-value ptr '(:struct g-type-instance) :class)))

(export 'g-type-instance-get-class)

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

;;; ----------------------------------------------------------------------------
;;; G_TYPE_CHECK_INSTANCE_TYPE()
;;; ----------------------------------------------------------------------------

(defun g-type-check-instance-type (instance type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[instance]{location of a @symbol{g-type-instance} structure}
  @argument[type]{the type to be checked}
  @return{@em{True} on success.}
  @begin{short}
    Checks if @arg{instance} is an instance of the type identified by
    @arg{type} or derived.
  @end{short}

  This function should only be used in type implementations.
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-check-instance-type (make-instance 'gtk-button) \"GObject\")
=> T
 (g-type-check-instance-type (make-instance 'gtk-button) \"GtkWindow\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-symbol{g-type-instance}
  @see-function{g-type-check-class-type}"
  (g-type-is-a (g-type-from-instance instance) type))

(export 'g-type-check-instance-type)

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
;;; ----------------------------------------------------------------------------

(defun g-type-check-class-type (class type)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-28}
  @argument[class]{a @symbol{g-type-class} structure}
  @argument[type]{the type to be checked}
  @return{@em{True} on success.}
  @begin{short}
    Checks if @arg{class} is a class structure of the type identified by
    @arg{type} or derived.
  @end{short}

  This function should only be used in type implementations.
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-check-class-type (g-type-class-ref \"GtkButton\") \"GObject\")
=> T
 (g-type-check-class-type (g-type-class-ref \"GtkButton\") \"GtkWindow\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-symbol{g-type-class}
  @see-function{g-type-check-instance-type}"
  (g-type-is-a (g-type-from-class class) type))

(export 'g-type-check-class-type)

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
;;; Warning
;;;
;;; g_type_init has been deprecated since version 2.36 and should not be used in
;;; newly-written code. The type system is now initialised automatically.
;;;
;;; This function used to initialise the type system. Since GLib 2.36, the type
;;; system is initialised automatically and this function does nothing.
;;; ----------------------------------------------------------------------------

;; Not implemented because the function is deprecated.

#-glib-2-36
(defcfun ("g_type_init" %g-type-init) :void)

#-glib-2-36
(glib::at-init () (%g-type-init))

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
;;; Warning
;;;
;;; GTypeDebugFlags has been deprecated since version 2.36 and should not be
;;; used in newly-written code. g_type_init() is now done automatically
;;;
;;; These flags used to be passed to g_type_init_with_debug_flags() which is now
;;; deprecated.
;;;
;;; If you need to enable debugging features, use the GOBJECT_DEBUG environment
;;; variable.
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

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; g_type_init_with_debug_flags ()
;;;
;;; void g_type_init_with_debug_flags (GTypeDebugFlags debug_flags);
;;;
;;; Warning
;;;
;;; g_type_init_with_debug_flags has been deprecated since version 2.36 and
;;; should not be used in newly-written code. the type system is now initialised
;;; automatically
;;;
;;; This function used to initialise the type system with debugging flags. Since
;;; GLib 2.36, the type system is initialised automatically and this function
;;; does nothing.
;;;
;;; If you need to enable debugging features, use the GOBJECT_DEBUG environment
;;; variable.
;;;
;;; debug_flags :
;;;     Bitwise combination of GTypeDebugFlags values for debugging purposes.
;;; ----------------------------------------------------------------------------

;;; * deprecated *

;;; ----------------------------------------------------------------------------
;;; g_type_name ()
;;; ----------------------------------------------------------------------------

;; TODO: The type +g-type-invalid+ is special handled in gtype-id. gtype-id
;; returns nil and not an integer. Therefore this function does not accept
;; +g-type-invalid+ as an argument like the C function.

(defcfun ("g_type_name" %g-type-name) (:string :free-from-foreign nil)
  (type %g-type)) ; Use %g-type and not g-type

(declaim (inline g-type-name))

(defun g-type-name (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-1}
  @argument[type]{type to return name for}
  @return{Static type name or @code{nil}.}
  @begin{short}
    Get the unique name that is assigned to a type ID.
  @end{short}
  Note that this function, like all other GType API, cannot cope with invalid
  type IDs. Randomized type IDs should not be passed in and will most likely
  lead to a crash.
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-name +g-type-double+)
=> \"gdouble\"
 (g-type-name +g-type-enum+)
=> \"GEnum\"
 (g-type-name (gtype \"GtkButton\"))
=> \"GtkButton\"
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-function{g-type-from-name}"
  (gtype-name (gtype type)))

(export 'g-type-name)

;;; ----------------------------------------------------------------------------
;;; g_type_qname ()
;;; ----------------------------------------------------------------------------

;; This function is not exported. In the Lisp binding there is no difference
;; to the function g-type-name.

(defcfun ("g_type_qname" g-type-qname) g-quark
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{type to return quark of @arg{type} name for}
  @return{The @arg{type} names quark or 0.}
  Get the corresponding quark of the @arg{type} IDs name."
  (type g-type))

;;; ----------------------------------------------------------------------------
;;; g_type_from_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_from_name" %g-type-from-name) %g-type
  (name :string))

(defun g-type-from-name (name)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[name]{type name to lookup}
  @return{Corresponding type ID for @arg{name} or 0.}
  Lookup the type ID from a given type name, returning 0 if no type has
  been registered under this @arg{name}. This is the preferred method to find
  out by name whether a specific type has been registered yet."
  (gtype name))

(export 'g-type-from-name)

;;; ----------------------------------------------------------------------------
;;; g_type_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_parent" g-type-parent) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{the derived type}
  @return{The parent type of @arg{type}.}
  Return the direct parent type of the passed in @arg{type}. If the passed in
  @arg{type} has no parent, i. e. is a fundamental type, @code{nil} is
  returned.
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-parent \"GtkWindow\")
=> #<GTYPE :name \"GtkBin\" :id 134929288>
 (g-type-parent \"GtkContainer\")
=> #<GTYPE :name \"GtkWidget\" :id 134921664>
 (g-type-parent \"GObject\")
=> NIL
 (g-type-parent \"gdouble\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{g-type}
  @see-function{g-type-children}"
  (type g-type))

(export 'g-type-parent)

;;; ----------------------------------------------------------------------------
;;; g_type_depth ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_depth" g-type-depth) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{a @class{g-type}}
  @return{The depth of @arg{type}.}
  Returns the length of the ancestry of the passed in @arg{type}. This includes
  the @arg{type} itself, so that e. g. a fundamental type has depth 1."
  (type g-type))

(export 'g-type-depth)

;;; ----------------------------------------------------------------------------
;;; g_type_next_base ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_next_base" g-type-next-base) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[leaf-type]{descendant of @arg{root-type} and the type to be
    returned}
  @argument[root-type]{immediate parent of the returned type}
  @return{Immediate child of @arg{root-type} and anchestor of @arg{leaf-type}.}
  @begin{short}
    Given a @arg{leaf-type} and a @arg{root-type} which is contained in its
    anchestry, return the type that @arg{root-type} is the immediate parent of.
  @end{short}
  In other words, this function determines the type that is derived directly
  from @arg{root-type} which is also a base class of @arg{leaf-type}. Given a
  root type and a leaf type, this function can be used to determine the types
  and order in which the leaf type is descended from the root type."
  (leaf-type g-type)
  (root-type g-type))

(export 'g-type-next-base)

;;; ----------------------------------------------------------------------------
;;; g_type_is_a ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_is_a" g-type-is-a) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{type to check anchestry for}
  @argument[is-a-type]{possible anchestor of @arg{type} or interface
    @arg{type} could conform to}
  @return{@em{True} if @arg{type} is a @arg{is-a-type} holds true.}
  If @arg{is-a-type} is a derivable type, check whether @arg{type} is a
  descendant of @arg{is-a-type}. If @arg{is-a-type} is an interface, check
  whether type conforms to it."
  (type g-type)
  (is-a-type g-type))

(export 'g-type-is-a)

;;; ----------------------------------------------------------------------------
;;; g_type_class_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_ref" g-type-class-ref) (:pointer (:struct g-type-class))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{type ID of a classed type}
  @return{The @symbol{g-type-class} structure for the given @arg{type}.}
  Increments the reference count of the class structure belonging to @arg{type}.
  This function will create the class if it does not exist already."
  (type g-type))

(export 'g-type-class-ref)

;;; ----------------------------------------------------------------------------
;;; g_type_class_peek ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_peek" g-type-class-peek)
    (:pointer (:struct g-type-class))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{type ID of a classed type}
  @return{The @symbol{g-type-class} structure for the given @arg{type} ID or
    @code{NULL} if the class does not currently exist.}
  @begin{short}
    This function is essentially the same as @fun{g-type-class-ref}, except that
    the classes reference count is not incremented. As a consequence, this
    function may return @code{NULL} if the class of the type passed in does not
    currently exist (hasn't been referenced before).
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-class-peek \"GtkLabel\")
=> #.(SB-SYS:INT-SAP #X00000000)
 (g-type-class-ref \"GtkLabel\")
=> #.(SB-SYS:INT-SAP #X081B9760)
 (g-type-class-peek \"GtkLabel\")
=> #.(SB-SYS:INT-SAP #X081B9760)
    @end{pre}
  @end{dictionary}
  @see-function{g-type-class-ref}"
  (type g-type))

(export 'g-type-class-peek)

;;; ----------------------------------------------------------------------------
;;; g_type_class_peek_static ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_peek_static" g-type-class-peek-static)
    (:pointer (:struct g-type-class))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{type ID of a classed type}
  @return{The @symbol{g-type-class} structure for the given @arg{type} ID or
    @code{NULL} if the class does not currently exist or is dynamically loaded.}
  @begin{short}
    A more efficient version of @fun{g-type-class-peek} which works only for
    static types.
  @end{short}

  Since 2.4
  @see-function{g-type-class-peek}"
  (type g-type))

(export 'g-type-class-peek-static)

;;; ----------------------------------------------------------------------------
;;; g_type_class_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_unref" g-type-class-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[class]{the @symbol{g-type-class} structure to unreference}
  @begin{short}
    Decrements the reference count of the @arg{class} structure being passed in.
  @end{short}
  Once the last reference count of a class has been released, classes may be
  finalized by the type system, so further dereferencing of a class pointer
  after @sym{g-type-class-unref} are invalid."
  (class (:pointer (:struct g-type-class))))

(export 'g-type-class-unref)

;;; ----------------------------------------------------------------------------
;;; g_type_class_peek_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_peek_parent" g-type-class-peek-parent)
    (:pointer (:struct g-type-class))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[class]{the @symbol{g-type-class} structure to retrieve the parent
    class for}
  @return{The parent class of @arg{class}.}
  @begin{short}
    This is a convenience function often needed in class initializers. It
    returns the class structure of the immediate parent type of the @arg{class}
    passed in.
  @end{short}
  Since derived classes hold a reference count on their parent classes as long
  as they are instantiated, the returned class will always exist. This function
  is essentially equivalent to:
  @begin{pre}
 (g-type-class-peek (g-type-parent class))
  @end{pre}"
  (g-class (:pointer (:struct g-type-class))))

(export 'g-type-class-peek-parent)

;;; ----------------------------------------------------------------------------
;;; g_type_class_add_private ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_class_add_private" g-type-class-add-private) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-4-1}
  @argument[class]{class structure for an instantiatable type}
  @argument[private-size]{size of private structure}
  @begin{short}
    Registers a private structure for an instantiatable type.
  @end{short}

  When an object is allocated, the private structures for the type and all of
  its parent types are allocated sequentially in the same memory block as the
  public structures.

  Note that the accumulated size of the private structures of a type and all
  its parent types cannot excced 64 KiB.

  This function should be called in the type's @code{class_init()} function.
  The private structure can be retrieved using the
  @code{G_TYPE_INSTANCE_GET_PRIVATE()} macro.

  The following example shows attaching a private structure
  @code{MyObjectPrivate} to an object @code{MyObject} defined in the standard
  @code{GObject} fashion of type's @code{class_init()} function. Note the use of
  a structure member \"priv\" to avoid the overhead of repeatedly calling
  @code{MY_OBJECT_GET_PRIVATE()}.
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
  Since 2.4"
  (class (:pointer (:struct g-type-class)))
  (private-size g-size))

(export 'g-type-class-add-private)

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
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interface_peek" g-type-interface-peek)
    (:pointer (:struct g-type-interface))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[instance-class]{a @symbol{g-type-class} structure}
  @argument[iface-type]{an interface ID which this @arg{instance-class} conforms
    to}
  @return{The @symbol{g-type-interface} structure of @arg{iface-type} if
    implemented by @arg{instance-class}, @code{NULL} otherwise.}
  @begin{short}
    Returns the @symbol{g-type-interface} structure of an interface to which the
    passed in @arg{class} conforms.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
 (g-type-interface-peek (g-type-class-ref \"GtkBox\") \"GtkOrientable\")
=> #.(SB-SYS:INT-SAP #X080C6858)
 (g-type-from-interface *)
=> #S(GTYPE :NAME \"GtkOrientable\" :%ID 134887472)
    @end{pre}
  @end{dictionary}"
  (instance-class (:pointer (:struct g-type-class)))
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_default_interface_ref" g-type-default-interface-ref) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{an interface type}
  @return{The default vtable for the interface of type @arg{type}.}
  @begin{short}
    Increments the reference count for the interface type @arg{type}, and
    returns the default interface vtable for the type. Call
    @fun{g-type-default-interface-unref} when you are done using the interface.
  @end{short}

  If the type is not currently in use, then the default vtable for the type will
  be created and initalized by calling the base interface init and
  default vtable init functions for the type (the @code{base_init} and
  @code{class_init} members of @symbol{g-type-info}). Calling
  @sym{g-type-default-interface-ref} is useful when you want to make sure that
  signals and properties for an interface have been installed.

  Since 2.4
  @see-function{g-type-default-interface-unref}"
  (type g-type))

(export 'g-type-default-interface-ref)

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_peek ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_default_interface_peek" g-type-default-interface-peek)
    :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{an interface type}
  @begin{return}
    The default vtable for the interface of type @arg{type}, or @code{NULL} if
    the type is not currently in use.
  @end{return}
  @begin{short}
    If the interface type @arg{type} is currently in use, returns its default
    interface vtable.
  @end{short}

  Since 2.4"
  (type g-type))

(export 'g-type-default-interface-peek)

;;; ----------------------------------------------------------------------------
;;; g_type_default_interface_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_default_interface_unref" g-type-default-interface-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[interface]{the default vtable structure for a interface, as returned
    by @fun{g-type-default-interface-ref}}
  @begin{short}
    Decrements the reference count for the type corresponding to the interface
    default vtable of @arg{interface}.
  @end{short}
  If the type is dynamic, then when no one is using the interface and all
  references have been released, the finalize function for the interface's
  default vtable (the @code{class_finalize} member of @symbol{g-type-info}) will
  be called.

  Since 2.4"
  (interface :pointer))

(export 'g-type-default-interface-unref)

;;; ----------------------------------------------------------------------------
;;; g_type_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_children" %g-type-children) (:pointer %g-type)
  (type g-type)
  (n-children (:pointer :uint)))

(defun g-type-children (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{the parent type}
  @return{A list of child types.}
  @short{Return a list of type IDs, listing the child types of @arg{type}.}
  @begin[Example]{dictionary}
    @begin{pre}
 (g-type-children \"GtkButton\")
=> (#S(GTYPE :NAME \"GtkToggleButton\" :%ID 134878968)
    #S(GTYPE :NAME \"GtkLinkButton\" :%ID 134879568)
    #S(GTYPE :NAME \"GtkScaleButton\" :%ID 134844456)
    #S(GTYPE :NAME \"GtkColorButton\" :%ID 134947120)
    #S(GTYPE :NAME \"GtkFontButton\" :%ID 134953216))
    @end{pre}
  @end{dictionary}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interfaces" %g-type-interfaces) (:pointer %g-type)
  (type g-type)
  (n-interfaces (:pointer :uint)))

(defun g-type-interfaces (type)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{the type to list interface types for}
  @return{A list of interface types.}
  @begin{short}
    Return a list of type IDs, listing the interface types that @arg{type}
    conforms to.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
 (g-type-interfaces \"GtkButton\")
=> (#S(GTYPE :NAME \"AtkImplementorIface\" :%ID 134892472)
    #S(GTYPE :NAME \"GtkBuildable\" :%ID 134887048)
    #S(GTYPE :NAME \"GtkActionable\" :%ID 134885776)
    #S(GTYPE :NAME \"GtkActivatable\" :%ID 134887624))
    @end{pre}
  @end{dictionary}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interface_prerequisites" %g-type-interface-prerequisites)
    (:pointer %g-type)
  (interface-type g-type)
  (n-prerequisites (:pointer :uint)))

(defun g-type-interface-prerequisites (interface-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[interface-type]{an interface type}
  @return{A list of @class{g-type} containing the prerequisites of
    @arg{interface-type}.}
  @begin{short}
    Returns the prerequisites of an interfaces type.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
 (g-type-interface-prerequisites \"GtkOrientable\")
=> (#S(GTYPE :NAME \"GObject\" :%ID 80))
    @end{pre}
  @end{dictionary}
  Since 2.2"
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_set_qdata" %g-type-set-qdata) :void
  (type g-type)
  (quark g-quark)
  (data :pointer))

(defun g-type-set-qdata (type quark data)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{a @class{g-type}}
  @argument[quark]{a @type{g-quark} id to identify the data}
  @argument[data]{the data}
  @short{Attaches arbitrary data to a @arg{type}.}
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-set-qdata \"gboolean\" \"mydata\" \"a string\")
=>
 (g-type-get-qdata \"gboolean\" \"mydata\")
=> \"a string\"
 (g-type-set-qdata \"gboolean\" \"mydata\" '(a b c))
=>
 (g-type-get-qdata \"gboolean\" \"mydata\")
=> (A B C)
 (g-type-set-qdata \"gboolean\" \"mydata\" nil)
=> NIL
 (g-type-get-qdata \"gboolean\" \"mydata\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-function{g-type-get-qdata}"
  (let ((ptr (%g-type-get-qdata type quark)))
    (cond ((null data)
           (when (not (null-pointer-p ptr))
             (%g-type-set-qdata type quark (null-pointer))
             (glib::free-stable-pointer ptr)))
          ((null-pointer-p ptr)
           (setq ptr (glib::allocate-stable-pointer data))
           (%g-type-set-qdata type quark ptr))
          (t
           (glib::set-stable-pointer-value ptr data)
           (%g-type-set-qdata type quark ptr)))))

(export 'g-type-set-qdata)

;;; ----------------------------------------------------------------------------
;;; g_type_get_qdata ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_get_qdata" %g-type-get-qdata) :pointer
  (type g-type)
  (quark g-quark))

(defun g-type-get-qdata (type quark)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-1}
  @argument[type]{a @class{g-type}}
  @argument[quark]{a @type{g-quark} id to identify the data}
  @return{The data, or @code{NULL} if no data was found.}
  @begin{short}
    Obtains data which has previously been attached to @arg{type} with
    @fun{g-type-set-qdata}.
  @end{short}
  See @fun{g-type-set-qdata} for an example.

  Note that this does not take subtyping into account; data attached to one
  type with @fun{g-type-set-qdata} cannot be retrieved from a subtype using
  @sym{g-type-get-qdata}.
  @see-function{g-type-set-qdata}"
  (let ((ptr (%g-type-get-qdata type quark)))
    (if (null-pointer-p ptr)
        nil
        (glib::get-stable-pointer-value ptr))))

(export 'g-type-get-qdata)

;;; ----------------------------------------------------------------------------
;;; struct GTypeQuery
;;; ----------------------------------------------------------------------------

(defcstruct g-type-query
  (:type g-type)
  (:type-name (:string :free-from-foreign nil))
  (:class-size :uint)
  (:instance-size :uint))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-type-query atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-type-query atdoc:*external-symbols*)
 "@version{2013-4-2}
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
  @begin[code]{table}
    @entry[type]{The @class{g-type} value of the type.}
    @entry[type-name]{The name of the type.}
    @entry[class-size]{The size of the class structure.}
    @entry[instance-size]{The size of the instance structure.}
  @end{table}
  @see-function{g-type-query}")

(export 'g-type-query)

;;; ----------------------------------------------------------------------------
;;; g_type_query ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_query" g-type-query) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[type]{the @class{g-type} value of a static, classed type}
  @argument[query]{a user provided structure that is filled in with constant
    values upon success}
  @begin{short}
    Queries the type system for information about a specific type.
  @end{short}
  This function will fill in a user provided structure to hold type specific
  information. If an invalid @class{g-type} is passed in, the type member of the
  @symbol{g-type-query} is 0. All members filled into the @symbol{g-type-query}
  structure should be considered constant and have to be left untouched.
  @see-class{g-type-query}"
  (type g-type)
  (query (:pointer (:struct g-type-query))))

(export 'g-type-query)

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

;;; ----------------------------------------------------------------------------
;;; g_type_register_static ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_register_static" g-type-register-static) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[parent-type]{type from which this type will be derived}
  @argument[type-name]{string used as the name of the new type}
  @argument[info]{the @symbol{g-type-info} structure for this type}
  @argument[flags]{bitwise combination of @symbol{g-type-flags} values}
  @return{the new type identifier}
  @begin{short}
    Registers @arg{type-name} as the name of a new static type derived from
    @arg{parent-type}.
  @end{short}
  The type system uses the information contained in the @symbol{g-type-info}
  structure pointed to by @arg{info} to manage the type and its instances (if
  not abstract). The value of @arg{flags} determines the nature (e. g. abstract
  or not) of the type.
  @see-symbol{g-type-info}
  @see-symbol{g-type-flags}"
  (parent-type g-type)
  (type-name :string)
  (info (:pointer (:struct g-type-info)))
  (flags g-type-flags))

(export 'g-type-register-static)

;;; ----------------------------------------------------------------------------
;;; g_type_register_static_simple ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_register_static_simple"
          g-type-register-static-simple) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[parent-type]{type from which this type will be derived}
  @argument[type-name]{string used as the name of the new type}
  @argument[class-size]{size of the class structure, see @symbol{g-type-info}}
  @argument[class-init]{location of the class initialization function, see
    @symbol{g-type-info}}
  @argument[instance-size]{size of the instance structure,
    see @symbol{g-type-info}}
  @argument[instance-init]{location of the instance initialization function,
    see @symbol{g-type-info}}
  @argument[flags]{bitwise combination of @symbol{g-type-flags} values}
  @return{The new type identifier.}
  @begin{short}
    Registers @arg{type-name} as the name of a new static type derived from
    @arg{parent-type}. The value of flags determines the nature (e. g. abstract
    or not) of the type. It works by filling a @symbol{g-type-info} struct and
    calling @fun{g-type-register-static}.
  @end{short}

  Since 2.12
  @see-function{g-type-register-static}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_add_interface_static" g-type-add-interface-static) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-11}
  @argument[instance-type]{@class{g-type} value of an instantiable type}
  @argument[interface-type]{@class{g-type} value of an interface type}
  @argument[info]{the @symbol{g-interface-info} structure for this
    (@arg{instance-type}, @arg{interface-type}) combination}
  @begin{short}
    Adds the static @arg{interface-type} to @arg{instantiable-type}.
  @end{short}
  The information contained in the @symbol{g-interface-info} structure pointed
  to by info is used to manage the relationship."
  (instance-type g-type)
  (interface-type g-type)
  (info (:pointer (:struct g-interface-info))))

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
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_interface_add_prerequisite" g-type-interface-add-prerequisite)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-11}
  @argument[interface-type]{a @class{g-type} of an interface type}
  @argument[prerequisite-type]{a @class{g-type} of an interface or
    instantiatable type}
  @begin{short}
    Adds @arg{prerequisite-type} to the list of prerequisites of
    @arg{interface-type}.
  @end{short}
  This means that any type implementing @arg{interface-type} must also implement
  @arg{prerequisite-type}. Prerequisites can be thought of as an alternative to
  interface derivation (which the GType API does not support). An interface can
  have at most one instantiatable prerequisite type."
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_fundamental_next" g-type-fundamental-next) %g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @begin{return}
    The nextmost fundamental type ID to be registered, or 0 if the type
    system ran out of fundamental type IDs.
  @end{return}
  Returns the next free fundamental type ID which can be used to register a
  new fundamental type with @code{g_type_register_fundamental()}. The returned
  type ID represents the highest currently registered fundamental type
  identifier.")

(export 'g-type-fundamental-next)

;;; ----------------------------------------------------------------------------
;;; g_type_fundamental ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_fundamental" g-type-fundamental) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-6-10}
  @argument[type]{a valid @class{g-type}}
  @return{The fundamental type of the argument @arg{type}.}
  @begin{short}
    The fundamental type which is the ancestor of @arg{type}. Fundamental types
    are types that serve as ultimate bases for the derived types, thus they are
    the roots of distinct inheritance hierarchies.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
 (g-type-fundamental \"GtkButton\")
=> #<GTYPE :name \"GObject\" :id 80>
 (g-type-fundamental \"GtkOrientable\")
=> #<GTYPE :name \"GInterface\" :id 8>
 (g-type-fundamental \"GtkWindowType\")
=> #<GTYPE :name \"GEnum\" :id 48>
    @end{pre}
  @end{dictionary}
  @see-class{g-type}"
  (type g-type))

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
;;; ----------------------------------------------------------------------------

(defcfun ("g_type_value_table_peek" g-type-value-table-peek)
    (:pointer (:struct g-type-value-table))
 #+cl-cffi-gtk-documentation
 "@version{2013-6-11}
  @argument[type]{a @class{g-type} value}
  @return{Location of the @symbol{g-type-value-table} associated with
    @arg{type} or @code{null-pointer} if there is no @symbol{g-type-value-table}
    associated with @arg{type}.}
  @begin{short}
    Returns the location of the @symbol{g-type-value-table} associated with
    @arg{type}.
  @end{short}
  Note that this function should only be used from source code that implements
  or has internal knowledge of the implementation of @arg{type}.
  @see-function{g-type-has-value-table}"
  (type g-type))

(export 'g-type-value-table-peek)

;;; ----------------------------------------------------------------------------
;;; g_type_ensure ()
;;; ----------------------------------------------------------------------------

#+glib-2-34
(defcfun ("g_type_ensure" g-type-ensure) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[type]{a @class{g-type}}
  @begin{short}
    Ensures that the indicated @arg{type} has been registered with the type
    system, and its @code{_class_init()} method has been run.
  @end{short}

  In theory, simply calling the type's @code{_get_type()} method (or using the
  corresponding macro) is supposed take care of this. However,
  @code{_get_type()} methods are often marked @code{G_GNUC_CONST} for
  performance reasons, even though this is technically incorrect (since
  @code{G_GNUC_CONST} requires that the function not have side effects, which
  @code{_get_type()} methods do on the first call). As a result, if you write a
  bare call to a @code{_get_type()} macro, it may get optimized out by the
  compiler. Using @sym{g-type-ensure} guarantees that the type's
  @code{_get_type()} method is called.

  Since 2.34"
  (type g-type))

#+glib-2-34
(export 'g-type-ensure)

;;; ----------------------------------------------------------------------------
;;; g_type_get_type_registration_serial ()
;;;
;;; guint g_type_get_type_registration_serial (void);
;;;
;;; Returns an opaque serial number that represents the state of the set of
;;; registered types. Any time a type is registred this serial changes, which
;;; means you can cache information based on type lookups (such as
;;; g_type_from_name) and know if the cache is still valid at a later time by
;;; comparing the current serial with the one at the type lookup.
;;;
;;; Returns :
;;;     An unsigned int, representing the state of type registrations.
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

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

;;; --- End of file gobject.type-info.lisp -------------------------------------
