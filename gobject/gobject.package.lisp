;;; ----------------------------------------------------------------------------
;;; gobject.package.lisp
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

(defpackage :gobject
  (:nicknames :g)
  (:use :c2cl :glib :cffi :tg :bordeaux-threads :iter :closer-mop)
  (:import-from :alexandria :format-symbol)
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

    #:param-spec-type
    #:param-spec-owner-type
    #:param-spec-name
    #:param-spec-readable
    #:param-spec-writable
    #:param-spec-constructor
    #:param-spec-constructor-only

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

    #:get-flags-items
    #:get-g-flags-definition
    #:flags-item-name
    #:flags-item-value
    #:flags-item-nick

    #:get-enum-items
    #:get-g-enum-definition
    #:enum-item-name
    #:enum-item-value
    #:enum-item-nick

    #:register-object-type
    #:register-object-type-implementation
    #:registered-object-type-by-name
    #:registered-enum-type
    #:registered-flags-type
    #:with-foreign-boxed-array

    ;; Symbols from gobject.signals.lisp
    #:list-signals
    #:signal-info
        ))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :gobject) t)
 "GObject provides the object system used for Pango and GTK.
  This is the API documentation of a Lisp binding to GObject.
  @begin[Type Information]{section}
    The GLib Runtime type identification and management system.

    The GType API is the foundation of the GObject system. It provides the
    facilities for registering and managing all fundamental data types,
    user-defined objects and interface types.

    For type creation and registration purposes, all types fall into one of two
    categories: static or dynamic. Static types are never loaded or unloaded at
    run-time as dynamic types may be. Static types are created with the function
    @code{g_type_register_static()} that gets type specific information passed
    in via a @code{GTypeInfo} structure. Dynamic types are created with the
    function @code{g_type_register_dynamic()} which takes a @code{GTypePlugin}
    structure instead. The remaining type information (the @code{GTypeInfo}
    structure) is retrieved during runtime through the @code{GTypePlugin}
    structure and the @code{g_type_plugin_*()} API. These registration functions
    are usually called only once from a function whose only purpose is to return
    the type identifier for a specific class. Once the type (or class or
    interface) is registered, it may be instantiated, inherited, or implemented
    depending on exactly what sort of type it is. There is also a third
    registration function for registering fundamental types called
    @code{g_type_register_fundamental()} which requires both a @code{GTypeInfo}
    structure and a @code{GTypeFundamentalInfo} structure but it is seldom used
    since most fundamental types are predefined rather than user-defined.

    Type instance and class structures are limited to a total of 64 KiB,
    including all parent types. Similarly, type instances' private data (as
    created by the function @code{g_type_class_add_private()}) are limited to a
    total of 64 KiB. If a type instance needs a large static buffer, allocate it
    separately (typically by using a @code{GArray} or @code{GPtrArray}
    structure) and put a pointer to the buffer in the structure.

    A final word about type names. Such an identifier needs to be at least three
    characters long. There is no upper length limit. The first character needs
    to be a letter (a-z or A-Z) or an underscore '_'. Subsequent characters can
    be letters, numbers or any of '-_+'.

    @about-variable{+g-type-invalid+}
    @about-variable{+g-type-none+}
    @about-variable{+g-type-interface+}
    @about-variable{+g-type-char+}
    @about-variable{+g-type-uchar+}
    @about-variable{+g-type-boolean+}
    @about-variable{+g-type-int+}
    @about-variable{+g-type-uint+}
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
    @about-variable{+g-type-gtype+}
    @about-variable{+g-type-variant+}
    @about-variable{+g-type-checksum+}
    @about-variable{+g-type-reserved-glib-first+}
    @about-variable{+g-type-reserved-glib-last+}
    @about-variable{+g-type-reserved-bse-first+}
    @about-variable{+g-type-reserved-bse-last+}
    @about-variable{+g-type-reserved-user-first+}
    @about-variable{+g-type-fundamental-max+}
    @about-class{g-type}
    @about-symbol{g-type-interface}
    @about-symbol{g-type-instance}
    @about-symbol{g-type-class}
    @about-symbol{g-type-info}
    @about-symbol{g-type-fundamental-info}
    @about-symbol{g-interface-info}
    @about-symbol{g-type-value-table}
    @about-symbol{g-type-debug-flags}
    @about-symbol{g-type-query}
    @about-symbol{g-type-flags}
    @about-symbol{g-type-fundamental-flags}
    @about-function{g-type-fundamental}
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
    @about-function{g-type-from-instance}
    @about-function{g-type-from-class}
    @about-function{g-type-from-interface}
    @about-function{g-type-instance-class}
    @about-function{g-type-instance-interface}
    @about-function{g-type-instance-get-private}
    @about-function{g-type-class-get-private}
    @about-function{g-type-check-instance}
    @about-function{g-type-check-instance-cast}
    @about-function{g-type-check-instance-type}
    @about-function{g-type-check-instance-fundamental-type}
    @about-function{g-type-check-class-cast}
    @about-function{g-type-check-class-type}
    @about-function{g-type-check-value}
    @about-function{g-type-check-value-type}
    @about-function{g-type-init}
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
    @about-function{g-type-qdata}
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
    @about-function{g-type-get-instance-count}
    @about-function{G_DECLARE_FINAL_TYPE}
    @about-function{G_DECLARE_DERIVABLE_TYPE}
    @about-function{G_DECLARE_INTERFACE}
    @about-function{G-DEFINE-TYPE}
    @about-function{G_DEFINE_TYPE_WITH_PRIVATE}
    @about-function{G_DEFINE_ABSTRACT_TYPE_WITH_CODE}
    @about-function{G-DEFINE-TYPE-WITH-CODE}
    @about-function{G-DEFINE-ABSTRACT-TYPE}
    @about-function{G_DEFINE_ABSTRACT_TYPE_WITH_PRIVATE}
    @about-function{G-DEFINE-ABSTRACT-TYPE-WITH-CODE}
    @about-function{G_ADD_PRIVATE}
    @about-function{G_PRIVATE_OFFSET}
    @about-function{G_PRIVATE_FIELD}
    @about-function{G_PRIVATE_FIELD_P}
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

    @about-class{g-object}
    @about-generic{g-object-has-reference}
    @about-generic{g-object-pointer}
    @about-generic{g-object-signal-handlers}
    @about-function{g-type-is-object}
    @about-function{g-is-object}
    @about-function{g-object-type}
    @about-function{g-object-type-name}
    @about-function{g-object-class-find-property}
    @about-function{g-object-class-list-properties}
    @about-function{g-object-interface-find-property}
    @about-function{g-object-interface-list-properties}
    @about-function{g-object-new}
    @about-function{g-object-notify}
    @about-function{g-object-freeze-notify}
    @about-function{g-object-thaw-notify}
    @about-function{g-object-data}
    @about-function{g-object-set-data-full}
    @about-function{g-object-steal-data}
    @about-function{g-object-property}
  @end{section}
  @begin[Enumeration and Flag Types]{section}
    The GLib type system provides fundamental types for enumeration and flags
    types. Flags types are like enumerations, but allow their values to be
    combined by bitwise OR. A registered enumeration or flags type associates a
    name and a nickname with each allowed value. When an enumeration or flags
    type is registered with the GLib type system, it can be used as value type
    for object properties, using the functions @fun{g-param-spec-enum} or
    @fun{g-param-spec-flags}.

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
    @about-function{g-enum-to-string}
    @about-function{g-flags-get-first-value}
    @about-function{g-flags-get-value-by-name}
    @about-function{g-flags-get-value-by-nick}
    @about-function{g-flags-to-string}
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
    based libraries. They allow arbitrary structures to be handled in a uniform
    way, allowing uniform copying (or referencing) and freeing
    (or unreferencing) of them, and uniform representation of the type of the
    contained structure. In turn, this allows any type which can be boxed to be
    set as the data in a GValue, which allows for polymorphic handling of a much
    wider range of data types, and hence usage of such types as GObject property
    values.

    GBoxed is designed so that reference counted types can be boxed. Use the
    type’s ‘ref’ function as the @code{GBoxedCopyFunc}, and its ‘unref’ function
    as the @code{GBoxedFreeFunc}. For example, for @code{GBytes}, the
    @code{GBoxedCopyFunc} is @code{g_bytes_ref()}, and the
    @code{GBoxedFreeFunc} is @code{g_bytes_unref()}.

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

    @about-symbol{g-param-spec-boolean}
    @about-function{g-param-spec-boolean}
    @about-function{g-value-boolean}
    @about-symbol{g-param-spec-char}
    @about-function{g-param-spec-char}
    @about-function{g-value-char}
    @about-function{g-value-schar}
    @about-symbol{g-param-spec-uchar}
    @about-function{g-param-spec-uchar}
    @about-function{g-value-uchar}
    @about-symbol{g-param-spec-int}
    @about-function{g-param-spec-int}
    @about-function{g-value-int}
    @about-symbol{g-param-spec-uint}
    @about-function{g-param-spec-uint}
    @about-function{g-value-uint}
    @about-symbol{g-param-spec-long}
    @about-function{g-param-spec-long}
    @about-function{g-value-long}
    @about-symbol{g-param-spec-ulong}
    @about-function{g-param-spec-ulong}
    @about-function{g-value-ulong}
    @about-symbol{g-param-spec-int64}
    @about-function{g-param-spec-int64}
    @about-function{g-value-int64}
    @about-symbol{g-param-spec-uint64}
    @about-function{g-param-spec-uint64}
    @about-function{g-value-uint64}
    @about-symbol{g-param-spec-float}
    @about-function{g-param-spec-float}
    @about-function{g-value-float}
    @about-symbol{g-param-spec-double}
    @about-function{g-param-spec-double}
    @about-function{g-value-double}
    @about-symbol{g-param-spec-enum}
    @about-function{g-param-spec-enum}
    @about-function{g-value-enum}
    @about-symbol{g-param-spec-flags}
    @about-function{g-param-spec-flags}
    @about-function{g-value-flags}
    @about-symbol{g-param-spec-string}
    @about-function{g-param-spec-string}
    @about-function{g-value-string}
    @about-function{g-value-set-static-string}
    @about-function{g-value-take-string}
    @about-function{g-value-set-string-take-ownership}
    @about-function{g-value-dup-string}
    @about-symbol{g-param-spec-param}
    @about-function{g-param-spec-param}
    @about-function{g-value-param}
    @about-function{g-value-take-param}
    @about-function{g-value-set-param-take-ownership}
    @about-function{g-value-dup-param}
    @about-symbol{g-param-spec-boxed}
    @about-function{g-param-spec-boxed}
    @about-function{g-value-boxed}
    @about-function{g-value-set-static-boxed}
    @about-function{g-value-take-boxed}
    @about-function{g-value-set-boxed-take-ownership}
    @about-function{g-value-dup-boxed}
    @about-symbol{g-param-spec-pointer}
    @about-function{g-param-spec-pointer}
    @about-function{g-value-pointer}
    @about-symbol{g-param-spec-object}
    @about-function{g-param-spec-object}
    @about-function{g-value-object}
    @about-function{g-value-take-object}
    @about-function{g-value-set-object-take-ownership}
    @about-function{g-value-dup-object}
    @about-symbol{g-param-spec-unichar}
    @about-function{g-param-spec-unichar}
    @about-symbol{g-param-spec-value-array}
    @about-function{g-param-spec-value-array}
    @about-symbol{g-param-spec-override}
    @about-function{g-param-spec-override}
    @about-symbol{g-param-spec-gtype}
    @about-function{g-param-spec-gtype}
    @about-function{g-value-gtype}
    @about-symbol{g-param-spec-variant}
    @about-function{g-param-spec-variant}
    @about-function{g-value-variant}
    @about-function{g-value-dup-variant}
    @about-function{g-value-take-variant}
  @end{section}
  @begin[GParamSpec]{section}
    Metadata for parameter specifications.

    @about-symbol{g-param-flags}
    @about-symbol{g-param-spec}
    @about-symbol{g-param-spec-class}
    @about-symbol{g-param-spec-type-info}
    @about-symbol{g-param-spec-pool}
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
    @about-function{g-param-spec-default-value}
    @about-function{g-param-value-set-default}
    @about-function{g-param-value-defaults}
    @about-function{g-param-value-validate}
    @about-function{g-param-value-convert}
    @about-function{g-param-values-cmp}
    @about-function{g-param-spec-is-valid-name}
    @about-function{g-param-spec-name}
    @about-function{g-param-spec-name-quark}
    @about-function{g-param-spec-nick}
    @about-function{g-param-spec-blurb}
    @about-function{g-param-spec-get-qdata}
    @about-function{g-param-spec-set-qdata}
    @about-function{g-param-spec-set-qdata-full}
    @about-function{g-param-spec-steal-qdata}
    @about-function{g-param-spec-get-redirect-target}
    @about-function{g-param-spec-internal}
    @about-function{g-param-type-register-static}
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
    basically they are a per-type facility that is inherited.

    A signal emission mainly involves invocation of a certain set of callbacks
    in precisely defined manner. There are two main categories of such
    callbacks, per-object ones and user provided ones. (Although signals can
    deal with any kind of instantiatable type, I am referring to those types as
    \"object types\" in the following, simply because that is the context most
    users will encounter signals in.) The per-object callbacks are most often
    referred to as \"object method handler\" or \"default (signal) handler\",
    while user provided callbacks are usually just called \"signal handler\".

    The object method handler is provided at signal creation time (this most
    frequently happens at the end of an object class' creation), while user
    provided handlers are frequently connected and disconnected to/from a
    certain signal on certain object instances.

    A signal emission consists of five stages, unless prematurely stopped:
    @begin{enumerate}
      @begin{item}
        Invocation of the object method handler for @code{:run-first} signals.
      @end{item}
      @begin{item}
        Invocation of normal user-provided signal handlers (where the after
        flag is not set).
      @end{item}
      @begin{item}
        Invocation of the object method handler for @code{:run-last} signals.
      @end{item}
      @begin{item}
        Invocation of user provided signal handlers (where the after flag is
        set).
      @end{item}
      @begin{item}
        Invocation of the object method handler for @code{:run-cleanup} signals.
      @end{item}
    @end{enumerate}
    The user-provided signal handlers are called in the order they were
    connected in.

    All handlers may prematurely stop a signal emission, and any number of
    handlers may be connected, disconnected, blocked or unblocked during a
    signal emission.

    There are certain criteria for skipping user handlers in stages 2 and 4 of
    a signal emission.

    First, user handlers may be blocked. Blocked handlers are omitted during
    callback invocation, to return from the blocked state, a handler has to get
    unblocked exactly the same amount of times it has been blocked before.

    Second, upon emission of a @code{:detailed} signal, an additional detail
    argument passed in to the functin @fun{g-signal-emit} has to match the
    detail argument of the signal handler currently subject to invocation.
    Specification of no detail argument for signal handlers (omission of the
    detail part of the signal specification upon connection) serves as a
    wildcard and matches any detail argument passed in to emission.

    While the detail argument is typically used to pass an object property name
    (as with \"notify\"), no specific format is mandated for the detail string,
    other than that it must be non-empty.

    @subheading{Memory management of signal handlers}
    If you are connecting handlers to signals and using a @class{g-object}
    instance as your signal handler user data, you should remember to pair calls
    to the function @fun{g-signal-connect} with calls to the function
    @fun{g-signal-handler-disconnect}. While signal handlers are automatically
    disconnected when the object emitting the signal is finalised, they are not
    automatically disconnected when the signal handler user data is destroyed.
    If this user data is a @class{g-object} instance, using it from a signal
    handler after it has been finalised is an error.

    There are two strategies for managing such user data. The first is to
    disconnect the signal handler (using the function
    @fun{g-signal-handler-disconnect}) when the user data (object) is finalised;
    this has to be implemented manually. For non-threaded programs, the function
    @code{g_signal_connect_object()} can be used to implement this
    automatically. Currently, however, it is unsafe to use in threaded programs.

    The second is to hold a strong reference on the user data until after the
    signal is disconnected for other reasons. This can be implemented
    automatically using the function @code{g_signal_connect_data()}.

    The first approach is recommended, as the second approach can result in
    effective memory leaks of the user data if the signal handler is never
    disconnected for some reason.

    @about-symbol{g-signal-invocation-hint}
    @about-symbol{g-signal-cmarshaller}
    @about-symbol{g-signal-cvamarshaller}
    @about-symbol{g-signal-flags}
    @about-symbol{g-signal-match-type}
    @about-struct{g-signal-query}
    @about-function{g-signal-query-signal-id}
    @about-function{g-signal-query-signal-name}
    @about-function{g-signal-query-owner-type}
    @about-function{g-signal-query-signal-flags}
    @about-function{g-signal-query-return-type}
    @about-function{g-signal-query-param-types}
    @about-function{g-signal-query-signal-detail}
    @about-symbol{g-connect-flags}
    @about-function{g-signal-new}
    @about-function{g-signal-newv}
    @about-function{g-signal-new-valist}
    @about-function{g-signal-set-va-marshaller}
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
    @about-function{g-signal-is-valid-name}
    @about-function{g-signal-parse-name}
    @about-function{g-signal-get-invocation-hint}
    @about-function{g-signal-type-cclosure-new}
    @about-function{g-signal-accumulator-first-wins}
    @about-function{g-signal-accumulator-true-handled}
    @about-function{g-clear-signal-handler}
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
  @end{section}
  @begin[GBinding]{section}
    Bind two object properties.

    @about-symbol{g-binding-flags}
    @about-class{g-binding}
    @about-generic{g-binding-flags}
    @about-generic{g-binding-source}
    @about-generic{g-binding-source-property}
    @about-generic{g-binding-target}
    @about-generic{g-binding-target-property}
    @about-function{g-binding-unbind}
    @about-function{g-object-bind-property}
    @about-function{g-object-bind-property-full}
    @about-function{g-object-bind-property-with-closures}
  @end{section} ")

;;; --- End of file gobject.package.lisp ---------------------------------------
