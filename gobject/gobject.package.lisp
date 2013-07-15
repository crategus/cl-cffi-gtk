;;; ----------------------------------------------------------------------------
;;; gobject.package.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file has been copied from the
;;; GObject Reference Manual Version 2.34.3. See <http://www.gtk.org>.
;;; The API documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
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

(defpackage :gobject
  (:nicknames :g)
  (:use :c2cl :glib :cffi :tg :bordeaux-threads :iter :closer-mop)
  (:export
    #:*lisp-name-exceptions*

    #:using*

    #:parse-g-value
    #:parse-g-param-spec
    #:set-g-value

    #:gtype
    #:gtype-id
    #:gtype-from-id
    #:gtype-name
    #:gtype-from-name

    #:create-fn-ref

    #:param-spec-name
    #:param-spec-readable
    #:param-spec-type
    #:param-spec-writable

    #:boxed-related-symbols
    #:copy-boxed-slots-to-foreign
    #:define-cb-methods
    #:define-g-boxed-cstruct
    #:define-g-boxed-opaque
    #:define-g-boxed-variant-cstruct
    #:define-g-enum
    #:define-g-flags
    #:define-g-interface
    #:define-g-object-class
    #:define-boxed-opaque-accessor
    #:define-vtable
    #:get-g-type-definition
    #:register-object-type
    #:register-object-type-implementation
    #:registered-object-type-by-name
    #:with-foreign-boxed-array

    ;; Symbols from gobject.signals.lisp
    #:list-signals
    #:signal-info
        ))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :gobject) t)
 "GObject provides the object system used for Pango and GTK+.
  This is the API documentation of a Lisp binding to GObject.
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
    @about-symbol{g-type-debug-flags}
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
    @about-function{g-type-ensure}
    @about-function{g-type-get-type-registration-serial}
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
    @about-function{g-is-object}
    @about-function{G_OBJECT_CLASS}
    @about-function{g-is-object-class}
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
    @about-symbol{g-parameter}
    @about-function{g-object-ref}
    @about-function{g-object-unref}
    @about-function{g-object-ref-sink}
    @about-function{g-clear-object}
    @about-class{g-initially-unowned}
    @about-class{g-initially-unowned-class}
    @about-function{G_TYPE_INITIALLY_UNOWNED}
    @about-function{g-object-is-floating}
    @about-function{g-object-force-floating}
    @about-function{g-object-weak-ref}
    @about-function{g-object-weak-unref}
    @about-function{g-object-add-weak-pointer}
    @about-function{g-object-remove-weak-pointer}
    @about-function{g-object-add-toggle-ref}
    @about-function{g-object-remove-toggle-ref}
    @about-function{g-object-connect}
    @about-function{g-object-disconnect}
    @about-function{g-object-set}
    @about-function{g-object-get}
    @about-function{g-object-notify}
    @about-function{g-object-notify-by-pspec}
    @about-function{g-object-freeze-notify}
    @about-function{g-object-thaw-notify}
    @about-function{g-object-get-data}
    @about-function{g-object-set-data}
    @about-function{g-object-set-data-full}
    @about-function{g-object-steal-data}
    @about-function{g-object-dup-data}
    @about-function{g-object-replace-data}
    @about-function{g-object-get-qdata}
    @about-function{g-object-set-qdata}
    @about-function{g-object-set-qdata-full}
    @about-function{g-object-steal-qdata}
    @about-function{g-object-dup-qdata}
    @about-function{g-object-replace-qdata}
    @about-function{g-object-set-property}
    @about-function{g-object-get-property}
    @about-function{g-object-new-valist}
    @about-function{g-object-set-valist}
    @about-function{g-object-get-valist}
    @about-function{g-object-watch-closure}
    @about-function{g-object-run-dispose}
    @about-function{G_OBJECT_WARN_INVALID_PROPERTY_ID}
    @about-function{GWeakRef}
    @about-function{g-weak-ref-init}
    @about-function{g-weak-ref-clear}
    @about-function{g-weak-ref-get}
    @about-function{g-weak-ref-set}
  @end{section}
  @begin[Enumeration and Flag Types]{section}
    The GLib type system provides fundamental types for enumeration and flags
    types. (Flags types are like enumerations, but allow their values to be
    combined by bitwise or). A registered enumeration or flags type associates a
    name and a nickname with each allowed value, and the methods
    @fun{g-enum-get-value-by-name}, @fun{g-enum-get-value-by-nick},
    @fun{g-flags-get-value-by-name} and @fun{g-flags-get-value-by-nick} can look
    up values by their name or nickname. When an enumeration or flags type is
    registered with the GLib type system, it can be used as value type for
    object properties, using @fun{g-param-spec-enum} or
    @fun{g-param-spec-flags}.

    GObject ships with a utility called @code{glib-mkenums} that can construct
    suitable type registration functions from C enumeration definitions.

    @about-symbol{g-enum-value}
    @about-symbol{g-enum-class}
    @about-symbol{g-flags-value}
    @about-symbol{g-flags-class}
    @about-function{g-enum-class-type}
    @about-function{g-enum-class-type-name}
    @about-function{g-type-is-enum}
    @about-function{g-enum-class}
    @about-function{g-is-enum-class}
    @about-function{g-type-is-flags}
    @about-function{g-flags-class}
    @about-function{g-is-flags-class}
    @about-function{g-flags-class-type}
    @about-function{g-flags-class-type-name}
    @about-function{g-enum-get-value}
    @about-function{g-enum-get-value-by-name}
    @about-function{g-enum-get-value-by-nick}
    @about-function{g-flags-get-first-value}
    @about-function{g-flags-get-value-by-name}
    @about-function{g-flags-get-value-by-nick}
    @about-function{g-enum-register-static}
    @about-function{g-flags-register-static}
    @about-function{g-enum-complete-type-info}
    @about-function{g-flags-complete-type-info}
  @end{section}
  @begin[Boxed Types]{section}
    A mechanism to wrap opaque C structures registered by the type system

    GBoxed is a generic wrapper mechanism for arbitrary C structures. The only
    thing the type system needs to know about the structures is how to copy and
    free them, beyond that they are treated as opaque chunks of memory.

    Boxed types are useful for simple value-holder structures like rectangles or
    points. They can also be used for wrapping structures defined in non-GObject
    based libraries.

    @about-function{g-boxed-copy}
    @about-function{g-boxed-free}
    @about-function{g-boxed-type-register-static}
    @about-function{g-pointer-type-register-static}
    @about-function{g-type-hash-table}
    @about-function{g-type-date}
    @about-function{g-type-gstring}
    @about-function{g-type-strv}
    @about-function{g-type-regex}
    @about-function{g-type-match-info}
    @about-function{g-type-array}
    @about-function{g-type-byte-array}
    @about-function{g-type-ptr-array}
    @about-function{g-type-bytes}
    @about-function{g-type-variant-type}
    @about-function{g-type-error}
    @about-function{g-type-date-time}
    @about-function{g-type-time-zone}
    @about-function{g-type-io-channel}
    @about-function{g-type-io-condition}
    @about-function{g-type-variant-builder}
    @about-function{g-type-key-file}
    @about-function{g-type-main-context}
    @about-function{g-type-main-loop}
    @about-function{g-type-markup-parse-context}
    @about-function{g-type-source}
    @about-function{g-type-polled}
    @about-function{g-type-thread}
    @about-symbol{GStrv}
  @end{section}
  @begin[Generic Values]{section}
    A polymorphic type that can hold values of any other type.

    @about-symbol{g-value}
    @about-function{g-value-holds}
    @about-function{g-value-type}
    @about-function{g-value-type-name}
    @about-function{g-type-is-value}
    @about-function{g-type-is-value-abstract}
    @about-function{g-is-value}
    @about-function{g-type-value}
    @about-function{g-type-value-array}
    @about-function{g-value-init}
    @about-function{g-value-copy}
    @about-function{g-value-reset}
    @about-function{g-value-unset}
    @about-function{g-value-set-instance}
    @about-function{g-value-fits-pointer}
    @about-function{g-value-peek-pointer}
    @about-function{g-value-type-compatible}
    @about-function{g-value-type-transformable}
    @about-function{g-value-transform}
    @about-function{g-value-register-transform-func}
    @about-function{g-strdup-value-contents}
  @end{section}
  @begin[Parameters and Values]{section}
    Standard Parameter and Value Types

    GValue provides an abstract container structure which can be copied,
    transformed and compared while holding a value of any (derived) type, which
    is registered as a GType with a GTypeValueTable in its GTypeInfo structure.
    Parameter specifications for most value types can be created as GParamSpec
    derived instances, to implement e.g. GObject properties which operate on
    GValue containers.

    Parameter names need to start with a letter (a-z or A-Z). Subsequent
    characters can be letters, numbers or a '-'. All other characters are
    replaced by a '-' during construction.

    @about-function{G_IS_PARAM_SPEC_BOOLEAN}
    @about-function{G_PARAM_SPEC_BOOLEAN}
    @about-function{G_VALUE_HOLDS_BOOLEAN}
    @about-function{G_TYPE_PARAM_BOOLEAN}
    @about-symbol{g-param-spec-boolean}
    @about-function{g-param-spec-boolean}
    @about-function{g-value-set-boolean}
    @about-function{g-value-get-boolean}
    @about-function{G_IS_PARAM_SPEC_CHAR}
    @about-function{G_PARAM_SPEC_CHAR}
    @about-function{G_VALUE_HOLDS_CHAR}
    @about-function{G_TYPE_PARAM_CHAR}
    @about-symbol{g-param-spec-char}
    @about-function{g-param-spec-char}
    @about-function{g-value-set-char}
    @about-function{g-value-get-char}
    @about-function{g-value-get-schar}
    @about-function{g-value-set-schar}
    @about-function{G_IS_PARAM_SPEC_UCHAR}
    @about-function{G_PARAM_SPEC_UCHAR}
    @about-function{G_VALUE_HOLDS_UCHAR}
    @about-function{G_TYPE_PARAM_UCHAR}
    @about-symbol{g-param-spec-uchar}
    @about-function{g-param-spec-uchar}
    @about-function{g-value-set-uchar}
    @about-function{g-value-get-uchar}
    @about-function{G_IS_PARAM_SPEC_INT}
    @about-function{G_PARAM_SPEC_INT}
    @about-function{G_VALUE_HOLDS_INT}
    @about-function{G_TYPE_PARAM_INT}
    @about-symbol{g-param-spec-int}
    @about-function{g-param-spec-int}
    @about-function{g-value-set-int}
    @about-function{g-value-get-int}
    @about-function{G_IS_PARAM_SPEC_UINT}
    @about-function{G_PARAM_SPEC_UINT}
    @about-function{G_VALUE_HOLDS_UINT}
    @about-function{G_TYPE_PARAM_UINT}
    @about-symbol{g-param-spec-uint}
    @about-function{g-param-spec-uint}
    @about-function{g-value-set-uint}
    @about-function{g-value-get-uint}
    @about-function{G_IS_PARAM_SPEC_LONG}
    @about-function{G_PARAM_SPEC_LONG}
    @about-function{G_VALUE_HOLDS_LONG}
    @about-function{G_TYPE_PARAM_LONG}
    @about-symbol{g-param-spec-long}
    @about-function{g-param-spec-long}
    @about-function{g-value-set-long}
    @about-function{g-value-get-long}
    @about-function{G_IS_PARAM_SPEC_ULONG}
    @about-function{G_PARAM_SPEC_ULONG}
    @about-function{G_VALUE_HOLDS_ULONG}
    @about-function{G_TYPE_PARAM_ULONG}
    @about-symbol{g-param-spec-ulong}
    @about-function{g-param-spec-ulong}
    @about-function{g-value-set-ulong}
    @about-function{g-value-get-ulong}
    @about-function{G_IS_PARAM_SPEC_INT64}
    @about-function{G_PARAM_SPEC_INT64}
    @about-function{G_VALUE_HOLDS_INT64}
    @about-function{G_TYPE_PARAM_INT64}
    @about-symbol{g-param-spec-int64}
    @about-function{g-param-spec-int64}
    @about-function{g-value-set-int64}
    @about-function{g-value-get-int64}
    @about-function{G_IS_PARAM_SPEC_UINT64}
    @about-function{G_PARAM_SPEC_UINT64}
    @about-function{G_VALUE_HOLDS_UINT64}
    @about-function{G_TYPE_PARAM_UINT64}
    @about-symbol{g-param-spec-uint64}
    @about-function{g-param-spec-uint64}
    @about-function{g-value-set-uint64}
    @about-function{g-value-get-uint64}
    @about-function{G_IS_PARAM_SPEC_FLOAT}
    @about-function{G_PARAM_SPEC_FLOAT}
    @about-function{G_VALUE_HOLDS_FLOAT}
    @about-function{G_TYPE_PARAM_FLOAT}
    @about-symbol{g-param-spec-float}
    @about-function{g-param-spec-float}
    @about-function{g-value-set-float}
    @about-function{g-value-get-float}
    @about-function{G_IS_PARAM_SPEC_DOUBLE}
    @about-function{G_PARAM_SPEC_DOUBLE}
    @about-function{G_VALUE_HOLDS_DOUBLE}
    @about-function{G_TYPE_PARAM_DOUBLE}
    @about-symbol{g-param-spec-double}
    @about-function{g-param-spec-double}
    @about-function{g-value-set-double}
    @about-function{g-value-get-double}
    @about-function{G_IS_PARAM_SPEC_ENUM}
    @about-function{G_PARAM_SPEC_ENUM}
    @about-function{G_VALUE_HOLDS_ENUM}
    @about-function{G_TYPE_PARAM_ENUM}
    @about-symbol{g-param-spec-enum}
    @about-function{g-param-spec-enum}
    @about-function{g-value-set-enum}
    @about-function{g-value-get-enum}
    @about-function{G_IS_PARAM_SPEC_FLAGS}
    @about-function{G_PARAM_SPEC_FLAGS}
    @about-function{G_VALUE_HOLDS_FLAGS}
    @about-function{G_TYPE_PARAM_FLAGS}
    @about-symbol{g-param-spec-flags}
    @about-function{g-param-spec-flags}
    @about-function{g-value-set-flags}
    @about-function{g-value-get-flags}
    @about-function{G_IS_PARAM_SPEC_STRING}
    @about-function{G_PARAM_SPEC_STRING}
    @about-function{G_VALUE_HOLDS_STRING}
    @about-function{G_TYPE_PARAM_STRING}
    @about-symbol{g-param-spec-string}
    @about-function{g-param-spec-string}
    @about-function{g-value-set-string}
    @about-function{g-value-set-static-string}
    @about-function{g-value-take-string}
    @about-function{g-value-set-string-take-ownership}
    @about-function{g-value-get-string}
    @about-function{g-value-dup-string}
    @about-function{G_IS_PARAM_SPEC_PARAM}
    @about-function{G_PARAM_SPEC_PARAM}
    @about-function{G_VALUE_HOLDS_PARAM}
    @about-function{G_TYPE_PARAM_PARAM}
    @about-symbol{g-param-spec-param}
    @about-function{g-param-spec-param}
    @about-function{g-value-set-param}
    @about-function{g-value-take-param}
    @about-function{g-value-set-param-take-ownership}
    @about-function{g-value-get-param}
    @about-function{g-value-dup-param}
    @about-function{G_IS_PARAM_SPEC_BOXED}
    @about-function{G_PARAM_SPEC_BOXED}
    @about-function{G_VALUE_HOLDS_BOXED}
    @about-function{G_TYPE_PARAM_BOXED}
    @about-symbol{g-param-spec-boxed}
    @about-function{g-param-spec-boxed}
    @about-function{g-value-set-boxed}
    @about-function{g-value-set-static-boxed}
    @about-function{g-value-take-boxed}
    @about-function{g-value-set-boxed-take-ownership}
    @about-function{g-value-get-boxed}
    @about-function{g-value-dup-boxed}
    @about-function{G_IS_PARAM_SPEC_POINTER}
    @about-function{G_PARAM_SPEC_POINTER}
    @about-function{G_VALUE_HOLDS_POINTER}
    @about-function{G_TYPE_PARAM_POINTER}
    @about-symbol{g-param-spec-pointer}
    @about-function{g-param-spec-pointer}
    @about-function{g-value-set-pointer}
    @about-function{g-value-get-pointer}
    @about-function{G_IS_PARAM_SPEC_OBJECT}
    @about-function{G_PARAM_SPEC_OBJECT}
    @about-function{G_VALUE_HOLDS_OBJECT}
    @about-function{G_TYPE_PARAM_OBJECT}
    @about-symbol{g-param-spec-object}
    @about-function{g-param-spec-object}
    @about-function{g-value-set-object}
    @about-function{g-value-take-object}
    @about-function{g-value-set-object-take-ownership}
    @about-function{g-value-get-object}
    @about-function{g-value-dup-object}
    @about-function{G_IS_PARAM_SPEC_UNICHAR}
    @about-function{G_PARAM_SPEC_UNICHAR}
    @about-function{G_TYPE_PARAM_UNICHAR}
    @about-symbol{g-param-spec-unichar}
    @about-function{g-param-spec-unichar}
    @about-function{G_IS_PARAM_SPEC_VALUE_ARRAY}
    @about-function{G_PARAM_SPEC_VALUE_ARRAY}
    @about-function{G_TYPE_PARAM_VALUE_ARRAY}
    @about-symbol{g-param-spec-value-array}
    @about-function{g-param-spec-value-array}
    @about-function{G_IS_PARAM_SPEC_OVERRIDE}
    @about-function{G_PARAM_SPEC_OVERRIDE}
    @about-function{G_TYPE_PARAM_OVERRIDE}
    @about-symbol{g-param-spec-override}
    @about-function{g-param-spec-override}
    @about-function{G_IS_PARAM_SPEC_GTYPE}
    @about-function{G_PARAM_SPEC_GTYPE}
    @about-function{G_VALUE_HOLDS_GTYPE}
    @about-function{G_TYPE_PARAM_GTYPE}
    @about-symbol{g-param-spec-g-type}
    @about-function{g-param-spec-g-type}
    @about-function{g-value-get-g-type}
    @about-function{g-value-set-g-type}
    @about-function{G_IS_PARAM_SPEC_VARIANT}
    @about-function{G_PARAM_SPEC_VARIANT}
    @about-function{G_VALUE_HOLDS_VARIANT}
    @about-function{G_TYPE_PARAM_VARIANT}
    @about-symbol{g-param-spec-variant}
    @about-function{g-param-spec-variant}
    @about-function{g-value-get-variant}
    @about-function{g-value-dup-variant}
    @about-function{g-value-set-variant}
    @about-function{g-value-take-variant}
  @end{section}
  @begin[GParamSpec]{section}
    Metadata for parameter specifications.

    @about-symbol{g-param-flags}
    @about-symbol{g-param-spec}
    @about-symbol{g-param-spec-class}
    @about-function{g-type-is-param}
    @about-function{g-param-spec}
    @about-function{g-is-param-spec}
    @about-function{g-param-spec-class}
    @about-function{g-is-param-spec-class}
    @about-function{g-param-spec-get-class}
    @about-function{g-param-spec-type}
    @about-function{g-param-spec-type-name}
    @about-function{g-param-spec-value-type}
    @about-function{g-param-readwrite}
    @about-function{g-param-static-strings}
    @about-function{g-param-mask}
    @about-function{g-param-user-shift}
    @about-function{g-param-spec-ref}
    @about-function{g-param-spec-unref}
    @about-function{g-param-spec-sink}
    @about-function{g-param-spec-ref-sink}
    @about-function{g-param-value-set-default}
    @about-function{g-param-value-defaults}
    @about-function{g-param-value-validate}
    @about-function{g-param-value-convert}
    @about-function{g-param-values-cmp}
    @about-function{g-param-spec-get-name}
    @about-function{g-param-spec-get-nick}
    @about-function{g-param-spec-get-blurb}
    @about-function{g-param-spec-get-qdata}
    @about-function{g-param-spec-set-qdata}
    @about-function{g-param-spec-set-qdata-full}
    @about-function{g-param-spec-steal-qdata}
    @about-function{g-param-spec-get-redirect-target}
    @about-function{g-param-spec-internal}
    @about-symbol{g-param-spec-type-info}
    @about-function{g-param-type-register-static}
    @about-symbol{g-param-spec-pool}
    @about-function{g-param-spec-pool-new}
    @about-function{g-param-spec-pool-insert}
    @about-function{g-param-spec-pool-remove}
    @about-function{g-param-spec-pool-lookup}
    @about-function{g-param-spec-pool-list}
    @about-function{g-param-spec-pool-list-owned}
  @end{section}
  @begin[Signals]{section}
    A means for customization of object behaviour and a general purpose
    notification mechanism.

    The basic concept of the signal system is that of the emission of a signal.
    Signals are introduced per-type and are identified through strings. Signals
    introduced for a parent type are available in derived types as well, so
    basically they are a per-type facility that is inherited. A signal emission
    mainly involves invocation of a certain set of callbacks in precisely
    defined manner. There are two main categories of such callbacks, per-object
    [10] ones and user provided ones. The per-object callbacks are most often
    referred to as \"object method handler\" or \"default (signal) handler\",
    while user provided callbacks are usually just called \"signal handler\".
    The object method handler is provided at signal creation time (this most
    frequently happens at the end of an object class' creation), while user
    provided handlers are frequently connected and disconnected to/from a
    certain signal on certain object instances.

    A signal emission consists of five stages, unless prematurely stopped:
    @begin{enumerate}
      @item{Invocation of the object method handler for G_SIGNAL_RUN_FIRST
        signals}
      @item{Invocation of normal user-provided signal handlers (after flag
        FALSE)}
      @item{Invocation of the object method handler for G_SIGNAL_RUN_LAST
        signals}
      @item{Invocation of user provided signal handlers, connected with an after
        flag of TRUE}
      @item{Invocation of the object method handler for G_SIGNAL_RUN_CLEANUP
        signals}
    @end{enumerate}
    The user-provided signal handlers are called in the order they were
    connected in. All handlers may prematurely stop a signal emission, and any
    number of handlers may be connected, disconnected, blocked or unblocked
    during a signal emission. There are certain criteria for skipping user
    handlers in stages 2 and 4 of a signal emission. First, user handlers may
    be blocked, blocked handlers are omitted during callback invocation, to
    return from the \"blocked\" state, a handler has to get unblocked exactly
    the same amount of times it has been blocked before. Second, upon emission
    of a G_SIGNAL_DETAILED signal, an additional \"detail\" argument passed in
    to g_signal_emit() has to match the detail argument of the signal handler
    currently subject to invocation. Specification of no detail argument for
    signal handlers (omission of the detail part of the signal specification
    upon connection) serves as a wildcard and matches any detail argument
    passed in to emission.

    @about-symbol{g-signal-invocation-hint}
    @about-symbol{g-signal-cmarshaller}
    @about-symbol{g-signal-flags}
    @about-symbol{g-signal-match-type}
    @about-symbol{g-signal-query}
    @about-function{G_SIGNAL_TYPE_STATIC_SCOPE}
    @about-function{G_SIGNAL_MATCH_MASK}
    @about-function{G_SIGNAL_FLAGS_MASK}
    @about-function{g-signal-new}
    @about-function{g-signal-newv}
    @about-function{g-signal-new-valist}
    @about-function{g-signal-query}
    @about-function{g-signal-lookup}
    @about-function{g-signal-name}
    @about-function{g-signal-list-ids}
    @about-function{g-signal-emit}
    @about-function{g-signal-emit-by-name}
    @about-function{g-signal-emitv}
    @about-function{g-signal-emit-valist}
    @about-function{g-signal-connect}
    @about-function{g-signal-connect-after}
    @about-function{g-signal-connect-swapped}
    @about-function{g-signal-connect-object}
    @about-symbol{g-connect-flags}
    @about-function{g-signal-connect-data}
    @about-function{g-signal-connect-closure}
    @about-function{g-signal-connect-closure-by-id}
    @about-function{g-signal-handler-block}
    @about-function{g-signal-handler-unblock}
    @about-function{g-signal-handler-disconnect}
    @about-function{g-signal-handler-find}
    @about-function{g-signal-handlers-block-matched}
    @about-function{g-signal-handlers-unblock-matched}
    @about-function{g-signal-handlers-disconnect-matched}
    @about-function{g-signal-handler-is-connected}
    @about-function{g-signal-handlers-block-by-func}
    @about-function{g-signal-handlers-unblock-by-func}
    @about-function{g-signal-handlers-disconnect-by-func}
    @about-function{g-signal-handlers-disconnect-by-data}
    @about-function{g-signal-has-handler-pending}
    @about-function{g-signal-stop-emission}
    @about-function{g-signal-stop-emission-by-name}
    @about-function{g-signal-override-class-closure}
    @about-function{g-signal-chain-from-overridden}
    @about-function{g-signal-new-class-handler}
    @about-function{g-signal-override-class-handler}
    @about-function{g-signal-chain-from-overridden-handler}
    @about-function{g-signal-add-emission-hook}
    @about-function{g-signal-remove-emission-hook}
    @about-function{g-signal-parse-name}
    @about-function{g-signal-get-invocation-hint}
    @about-function{g-signal-type-cclosure-new}
    @about-function{g-signal-accumulator-first-wins}
    @about-function{g-signal-accumulator-true-handled}
  @end{section}
  @begin[Closures]{section}
    Functions as first-class objects

    A GClosure represents a callback supplied by the programmer. It will
    generally comprise a function of some kind and a marshaller used to call it.
    It is the reponsibility of the marshaller to convert the arguments for the
    invocation from GValues into a suitable form, perform the callback on the
    converted arguments, and transform the return value back into a GValue.

    In the case of C programs, a closure usually just holds a pointer to a
    function and maybe a data argument, and the marshaller converts between
    GValue and native C types. The GObject library provides the GCClosure type
    for this purpose. Bindings for other languages need marshallers which
    convert between GValues and suitable representations in the runtime of the
    language in order to use functions written in that languages as callbacks.

    Within GObject, closures play an important role in the implementation of
    signals. When a signal is registered, the c_marshaller argument to
    g_signal_new() specifies the default C marshaller for any closure which is
    connected to this signal. GObject provides a number of C marshallers for
    this purpose, see the g_cclosure_marshal_*() functions. Additional C
    marshallers can be generated with the glib-genmarshal utility. Closures can
    be explicitly connected to signals with g_signal_connect_closure(), but it
    usually more convenient to let GObject create a closure automatically by
    using one of the g_signal_connect_*() functions which take a callback
    function/user data pair.

    Using closures has a number of important advantages over a simple callback
    function/data pointer combination:
    @begin{itemize}
      @begin{item}
        Closures allow the callee to get the types of the callback parameters,
        which means that language bindings don't have to write individual glue
        for each callback type.
      @end{item}
      @begin{item}
        The reference counting of GClosure makes it easy to handle reentrancy
        right; if a callback is removed while it is being invoked, the closure
        and its parameters won't be freed until the invocation finishes.
      @end{item}
      @begin{item}
        g_closure_invalidate() and invalidation notifiers allow callbacks to be
        automatically removed when the objects they point to go away.
      @end{item}
    @end{itemize}

    @about-symbol{G_CLOSURE_NEEDS_MARSHAL}
    @about-symbol{G_CLOSURE_N_NOTIFIERS}
    @about-symbol{G_CCLOSURE_SWAP_DATA}
    @about-symbol{G_CALLBACK}
    @about-symbol{g-closure}
    @about-function{g-type-closure}
    @about-symbol{g-cclosure}
    @about-function{g-cclosure-new}
    @about-function{g-cclosure-new-swap}
    @about-function{g-cclosure-new-object}
    @about-function{g-cclosure-new-object-swap}
    @about-function{g-cclosure-marshal-generic}
    @about-function{g-closure-new-object}
    @about-function{g-closure-ref}
    @about-function{g-closure-sink}
    @about-function{g-closure-unref}
    @about-function{g-closure-invoke}
    @about-function{g-closure-invalidate}
    @about-function{g-closure-add-finalize-notifier}
    @about-function{g-closure-add-invalidate-notifier}
    @about-function{g-closure-remove-finalize-notifier}
    @about-function{g-closure-remove-invalidate-notifier}
    @about-function{g-closure-new-simple}
    @about-function{g-closure-set-marshal}
    @about-function{g-closure-add-marshal-guards}
    @about-function{g-closure-set-meta-marshal}
    @about-function{g-closure-set-closure}
    @about-function{g-closure-set-dummy-callback}
  @end{section}")

;;; --- End of file gobject.package.lisp ---------------------------------------
