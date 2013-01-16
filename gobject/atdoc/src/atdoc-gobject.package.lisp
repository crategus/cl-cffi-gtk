;;; ----------------------------------------------------------------------------
;;; atdoc-gobject.package.lisp
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

(setf (documentation (find-package :gobject) t)
 "@em{GObject} provides the object system used for Pango and GTK+.
  @begin[Type Information]{section}
    The GLib Runtime type identification and management system.

    The GType API is the foundation of the @em{GObject} system. It provides the
    facilities for registering and managing all fundamental data types,
    user-defined object and interface types. Before using any GType or
    @em{GObject} functions, @code{g_type_init()} must be called to initialize
    the type system.

    For type creation and registration purposes, all types fall into one of two
    categories: static or dynamic. Static types are never loaded or unloaded at
    run-time as dynamic types may be. Static types are created with
    @fun{g-type-register-static} that gets type specific information passed
    in via a @symbol{g-type-info} structure. Dynamic types are created with
    @code{g_type_register_dynamic()} which takes a @code{GTypePlugin} structure
    instead. The remaining type information (the @symbol{g-type-info} structure)
    is retrieved during runtime through @code{GTypePlugin} and the 
    @code{g_type_plugin_*()} API. These registration functions are usually
    called only once from a function whose only purpose is to return the type
    identifier for a specific class. Once the type (or class or interface) is
    registered, it may be instantiated, inherited, or implemented depending on
    exactly what sort of type it is. There is also a third registration function
    for registering fundamental types called
    @code{g_type_register_fundamental()} which requires both a
    @symbol{g-type-info} structure and a @symbol{g-type-fundamental-info}
    structure but it is seldom used since most fundamental types are predefined
    rather than user-defined.

    Type instance and class structs are limited to a total of 64 KiB, including
    all parent types. Similarly, type instances' private data (as created by
    @fun{g-type-class-add-private}) are limited to a total of 64 KiB. If a
    type instance needs a large static buffer, allocate it separately (typically
    by using @code{GArray} or @code{GPtrArray}) and put a pointer to the buffer
    in the structure.

    A final word about type names. Such an identifier needs to be at least three
    characters long. There is no upper length limit. The first character needs
    to be a letter (a-z or A-Z) or an underscore '_'. Subsequent characters can
    be letters, numbers or any of '-_+'.

    @about-variable{+g-type-invalid+}
    @about-variable{+g-type-none+}
    @about-variable{+g-type-void+}
    @about-variable{+g-type-interface+}
    @about-variable{+g-type-char+}
    @about-variable{+g-type-uchar+}
    @about-variable{+g-type-boolean+}
    @about-variable{+g-type-int+}
    @about-variable{+g-type-unit+}
    @about-variable{+g-type-long+}
    @about-variable{+g-type-ulong+}
    @about-variable{+g-type-int64+}
    @about-variable{+g-type-uint64+}
    @about-variable{+g-type-enum+}
    @about-variable{+g-type-flags+}
    @about-variable{+g-type-float+}
    @about-variable{+g-type-double+}
    @about-variable{+g-type-string+}
    @about-variable{+g-type-pointer+}
    @about-variable{+g-type-boxed+}
    @about-variable{+g-type-param+}
    @about-variable{+g-type-object+}
    @about-variable{+g-type-variant+}
    @about-variable{+g-type-reserved-glib-first+}
    @about-variable{+g-type-reserved-glib-last+}
    @about-variable{+g-type-reserved-bse-first+}
    @about-variable{+g-type-reserved-bse-last+}
    @about-variable{+g-type-reserved-user-first+}
    @about-function{g-type-gtype}
    @about-symbol{g-type-flags}
    @about-symbol{g-type-fundamental-flags}
    @about-class{g-type}
    @about-function{g-type-fundamental}
    @about-variable{g-type-fundamental-max}
    @about-function{g-type-make-fundamental}
    @about-function{g-type-is-abstract}
    @about-function{g-type-is-derived}
    @about-function{g-type-is-fundamental}
    @about-function{g-type-is-value-type}
    @about-function{g-type-has-value-table}
    @about-function{g-type-is-classed}
    @about-function{g-type-is-instantiatable}
    @about-function{g-type-is-derivable}
    @about-function{g-type-is-deep-derivable}
    @about-function{g-type-is-interface}
    @about-symbol{g-type-interface}
    @about-symbol{g-type-class}
    @about-symbol{g-type-instance}
    @about-symbol{g-type-info}
    @about-symbol{g-type-fundamental-info}
    @about-symbol{g-interface-info}
    @about-symbol{g-type-value-table}
    @about-function{g-type-from-instance}
    @about-function{g-type-from-class}
    @about-function{g-type-from-interface}
    @about-function{g-type-instance-get-class}
    @about-function{g-type-instance-get-interface}
    @about-function{g-type-instance-get-private}
    @about-function{g-type-class-get-private}
    @about-function{g-type-check-instance}
    @about-function{g-type-check-instance-cast}
    @about-function{g-type-check-instance-type}
    @about-function{g-type-check-class-cast}
    @about-function{g-type-check-class-type}
    @about-function{g-type-check-value}
    @about-function{g-type-check-value-type}
    @about-function{g-type-flag-reserved-id-bit}
    @about-function{g-type-init}
    @about-symbol{GTypeDebugFlags}
    @about-function{g-type-init-with-debug-flags}
    @about-function{g-type-name}
    @about-function{g-type-qname}
    @about-function{g-type-from-name}
    @about-function{g-type-parent}
    @about-function{g-type-depth}
    @about-function{g-type-next-base}
    @about-function{g-type-is-a}
    @about-function{g-type-class-ref}
    @about-function{g-type-class-peek}
    @about-function{g-type-class-peek-static}
    @about-function{g-type-class-unref}
    @about-function{g-type-class-peek-parent}
    @about-function{g-type-class-add-private}
    @about-function{g-type-add-class-private}
    @about-function{g-type-interface-peek}
    @about-function{g-type-interface-peek-parent}
    @about-function{g-type-default-interface-ref}
    @about-function{g-type-default-interface-peek}
    @about-function{g-type-default-interface-unref}
    @about-function{g-type-children}
    @about-function{g-type-interfaces}
    @about-function{g-type-interface-prerequisites}
    @about-function{g-type-set-qdata}
    @about-function{g-type-get-qdata}
    @about-symbol{g-type-query}
    @about-function{g-type-query}
    @about-function{g-type-register-static}
    @about-function{g-type-register-static-simple}
    @about-function{g-type-register-dynamic}
    @about-function{g-type-register-fundamental}
    @about-function{g-type-add-interface-static}
    @about-function{g-type-add-interface-dynamic}
    @about-function{g-type-interface-add-prerequisite}
    @about-function{g-type-get-plugin}
    @about-function{g-type-interface-get-plugin}
    @about-function{g-type-fundamental-next}
    @about-function{g-type-fundamental}
    @about-function{g-type-create-instance}
    @about-function{g-type-free-instance}
    @about-function{g-type-add-class-cache-func}
    @about-function{g-type-remove-class-cache-func}
    @about-function{g-type-class-unref-uncached}
    @about-function{g-type-add-interface-check}
    @about-function{g-type-remove-interface-check}
    @about-function{g-type-value-table-peek}
    @about-function{G-DEFINE-TYPE}
    @about-function{G-DEFINE-TYPE-WITH-CODE}
    @about-function{G-DEFINE-ABSTRACT-TYPE}
    @about-function{G-DEFINE-ABSTRACT-TYPE-WITH-CODE}
    @about-function{G-DEFINE-INTERFACE}
    @about-function{G-DEFINE-INTERFACE-WITH-CODE}
    @about-function{G-IMPLEMENT-INTERFACE}
    @about-function{G-DEFINE-TYPE-EXTENDED}
    @about-function{G-DEFINE-BOXED-TYPE}
    @about-function{G-DEFINE-BOXED-TYPE-WITH-CODE}
    @about-function{G-DEFINE-POINTER-TYPE}
    @about-function{G-DEFINE-POINTER-TYPE-WITH-CODE}
  @end{section}
  @begin[GObject]{section}
    The base object type.

    GObject is the fundamental type providing the common attributes and methods
    for all object types in GTK+, Pango and other libraries based on GObject.
    The GObject class provides methods for object construction and destruction,
    property access methods, and signal support. Signals are described in detail
    in Signals(3).

    @about-class{g-object}
    @about-symbol{g-object-class}
    @about-symbol{g-object-construct-param}
    @about-function{g-type-is-object}
    @about-function{G_OBJECT}
    @about-function{G_IS_OBJECT}
    @about-function{G_OBJECT_CLASS}
    @about-function{G_IS_OBJECT_CLASS}
    @about-function{G_OBJECT_GET_CLASS}
    @about-function{g-object-type}
    @about-function{g-object-type-name}
    @about-function{g-object-class-type}
    @about-function{g-object-class-name}
    @about-function{g-object-class-install-property}
    @about-function{g-object-class-install-properties}
    @about-function{g-object-class-find-property}
    @about-function{g-object-class-list-properties}
    @about-function{g-object-class-override-property}
    @about-function{g-object-interface-install-property}
    @about-function{g-object-interface-find_property}
    @about-function{g-object-interface-list-properties}
    @about-function{g-object-new}
    @about-function{g-object-newv}
    @about-class{g-parameter}
    @about-function{g-object-ref}
    @about-function{g-object-unref}
    @about-function{g-object-ref-sink}
    @about-function{g-clear-object}
    @about-class{g-initially-unowned}
    @about-class{g-initially-unownedClass}

    @about-function{G_TYPE_INITIALLY_UNOWNED}

    @about-function{g_object_is_floating}
    @about-function{g_object_force_floating}
    @about-function{g_object_weak_ref}
    @about-function{g_object_weak_unref}
    @about-function{g_object_add_weak_pointer}
    @about-function{g_object_remove_weak_pointer}
    @about-function{g_object_add_toggle_ref}
    @about-function{g_object_remove_toggle_ref}
    @about-function{g_object_connect}
    @about-function{g_object_disconnect}
    @about-function{g_object_set}
    @about-function{g_object_get}
    @about-function{g_object_notify}
    @about-function{g_object_notify_by_pspec}
    @about-function{g_object_freeze_notify}
    @about-function{g_object_thaw_notify}
    @about-function{g_object_get_data}
    @about-function{g_object_set_data}
    @about-function{g_object_set_data_full}
    @about-function{g_object_steal_data}
    @about-function{g_object_get_qdata}
    @about-function{g_object_set_qdata}
    @about-function{g_object_set_qdata_full}
    @about-function{g_object_steal_qdata}
    @about-function{g_object_set_property}
    @about-function{g_object_get_property}
    @about-function{g_object_new_valist}
    @about-function{g_object_set_valist}
    @about-function{g_object_get_valist}
    @about-function{g_object_watch_closure}
    @about-function{g_object_run_dispose}

    @about-function{G_OBJECT_WARN_INVALID_PROPERTY_ID}

    @about-function{GWeakRef}

    @about-function{g_weak_ref_init}
    @about-function{g_weak_ref_clear}
    @about-function{g_weak_ref_get}
    @about-function{g_weak_ref_set}
  @end{section}
  @begin[Enumeration and Flag Types]{section}
  @end{section}
  @begin[Boxed Types]{section}
  @end{section}
  @begin[Generic Values]{section}
  @end{section}
  @begin[Parameters and Values]{section}
  @end{section}
  @begin[GParamSpec]{section}
  @end{section}
  @begin[Signals]{section}
  @end{section}
  @begin[Closures]{section}
  @end{section}
")

;;; --- End of file atdoc-gobject.lisp -----------------------------------------
