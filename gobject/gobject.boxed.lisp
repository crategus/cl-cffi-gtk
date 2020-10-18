;;; ----------------------------------------------------------------------------
;;; gobject.boxed.lisp
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
;;; Boxed Types
;;;
;;;     A mechanism to wrap opaque C structures registered by the type system.
;;;
;;; Types and Values
;;;
;;;     G_TYPE_HASH_TABLE
;;;     G_TYPE_DATE
;;;     G_TYPE_GSTRING
;;;     G_TYPE_STRV
;;;     G_TYPE_REGEX
;;;     G_TYPE_MATCH_INFO
;;;     G_TYPE_ARRAY
;;;     G_TYPE_BYTE_ARRAY
;;;     G_TYPE_PTR_ARRAY
;;;     G_TYPE_BYTES
;;;     G_TYPE_VARIANT_TYPE
;;;     G_TYPE_ERROR
;;;     G_TYPE_DATE_TIME
;;;     G_Type_TIME_ZONE
;;;     G_TYPE_IO_CHANNEL
;;;     G_TYPE_IO_CONDITION
;;;     G_TYPE_VARIANT_BUILDER
;;;     G_TYPE_VARIANT_DICT
;;;     G_TYPE_KEY_FILE
;;;     G_TYPE_MAIN_CONTEXT
;;;     G_TYPE_MAIN_LOOP
;;;     G_TYPE_MAPPED_FILE
;;;     G_TYPE_MARKUP_PARSE_CONTEXT
;;;     G_TYPE_SOURCE
;;;     G_TYPE_POLLED
;;;     G_TYPE_THREAD
;;;     G_TYPE_OPTION_GROUP
;;;     G_TYPE_URI
;;;
;;; Functions
;;;
;;;     GBoxedCopyFunc
;;;     GBoxedFreeFunc
;;;
;;;     g_boxed_copy
;;;     g_boxed_free
;;;     g_boxed_type_register_static
;;;     g_pointer_type_register_static
;;;
;;;
;;; Description
;;;
;;; GBoxed is a generic wrapper mechanism for arbitrary C structures. The only
;;; thing the type system needs to know about the structures is how to copy and
;;; free them, beyond that they are treated as opaque chunks of memory.
;;;
;;; Boxed types are useful for simple value-holder structures like rectangles or
;;; points. They can also be used for wrapping structures defined in non-GObject
;;; based libraries. They allow arbitrary structures to be handled in a uniform
;;; way, allowing uniform copying (or referencing) and freeing
;;; (or unreferencing) of them, and uniform representation of the type of the
;;; contained structure. In turn, this allows any type which can be boxed to be
;;; set as the data in a GValue, which allows for polymorphic handling of a much
;;; wider range of data types, and hence usage of such types as GObject property
;;; values.
;;;
;;; GBoxed is designed so that reference counted types can be boxed. Use the
;;; type’s ‘ref’ function as the GBoxedCopyFunc, and its ‘unref’ function as
;;; the GBoxedFreeFunc. For example, for GBytes, the GBoxedCopyFunc is
;;; g_bytes_ref(), and the GBoxedFreeFunc is g_bytes_unref().
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; GBoxedCopyFunc ()
;;;
;;; gpointer (*GBoxedCopyFunc) (gpointer boxed);
;;;
;;; This function is provided by the user and should produce a copy of the
;;; passed in boxed structure.
;;;
;;; boxed :
;;;     The boxed structure to be copied.
;;;
;;; Returns :
;;;     The newly created copy of the boxed structure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GBoxedFreeFunc ()
;;;
;;; void (*GBoxedFreeFunc) (gpointer boxed);
;;;
;;; This function is provided by the user and should free the boxed structure
;;; passed.
;;;
;;; boxed :
;;;     The boxed structure to be freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_boxed_copy ()
;;; ----------------------------------------------------------------------------

;; Used internally for the implementation of a Lisp boxed type. This function
;; is not exported.

(defcfun ("g_boxed_copy" %g-boxed-copy) :pointer
  (boxed-type g-type)
  (boxed-src :pointer))

(defun g-boxed-copy (boxed-type boxed-src)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-10}
  @argument[boxed-type]{the type of @arg{src-boxed}}
  @argument[src-boxed]{the boxed structure to be copied}
  @return{The newly created copy of the boxed structure.}
  Provide a copy of a boxed structure @arg{src-boxed} which is of type
  @arg{boxed-type}."
  ;; We check for a null-pointer and return nil.
  (unless (null-pointer-p boxed-src)
    (%g-boxed-copy boxed-type boxed-src)))

;;; ----------------------------------------------------------------------------
;;; g_boxed_free ()
;;; ----------------------------------------------------------------------------

;; Used internally for the implementation of a Lisp boxed type. This function
;; is not exported.

(defcfun ("g_boxed_free" g-boxed-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-10}
  @argument[boxed-type]{the type of boxed}
  @argument[boxed]{the boxed structure to be freed}
  Free the boxed structure @arg{boxed} which is of type @arg{boxed-type}."
  (boxed-type g-type)
  (boxed :pointer))

;;; ----------------------------------------------------------------------------
;;; g_boxed_type_register_static ()
;;; ----------------------------------------------------------------------------

;; This function is not exported.

(defcfun ("g_boxed_type_register_static" g-boxed-type-register-static) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-6-10}
  @argument[name]{name of the new boxed type}
  @argument[copy-fn]{boxed structure copy function}
  @argument[free-fn]{boxed structure free function}
  @return{New @var{+g-type-boxed+} derived type ID for @arg{name}.}
  @begin{short}
    This function creates a new @var{+g-type-boxed+} derived type ID for a new
    boxed type with name @arg{name}.
  @end{short}
  Boxed type handling functions have to be provided to copy and free opaque
  boxed structures of this type."
  (name :string)
  (copy-fn :pointer)
  (free-fn :pointer))

;;; ----------------------------------------------------------------------------
;;; g_pointer_type_register_static ()
;;; ----------------------------------------------------------------------------

;; This function is not exported.

(defcfun ("g_pointer_type_register_static" g-pointer-type-register-static)
    g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-6-10}
  @argument[name]{the name of the new pointer type}
  @return{A new @var{+g-type-pointer+} derived type ID for @arg{name}.}
  Creates a new @var{+g-type-pointer+} derived type ID for a new pointer type
  with name @arg{name}."
  (name :string))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_HASH_TABLE
;;;
;;; #define G_TYPE_HASH_TABLE (g_hash_table_get_type ())
;;;
;;; The GType for a boxed type holding a GHashTable reference.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_DATE
;;;
;;; #define G_TYPE_DATE (g_date_get_type ())
;;;
;;; The GType for GDate.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_GSTRING
;;;
;;; #define G_TYPE_GSTRING (g_gstring_get_type ())
;;;
;;; The GType for GString.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_STRV
;;; ----------------------------------------------------------------------------

(defcfun ("g_strv_get_type" g-type-strv) g-type
 #+cl-cffi-gtk-documentation
 "@version{2020-10-18}
  @begin{short}
    The @class{g-type} ID for a boxed type holding a @code{NULL}-terminated
    array of strings.
  @end{short}
  @see-class{g-type}")

(export 'g-type-strv)

(glib-init::at-init nil (g-type-strv))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_REGEX
;;;
;;; #define G_TYPE_REGEX (g_regex_get_type ())
;;;
;;; The GType for a boxed type holding a GRegex reference.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MATCH_INFO
;;;
;;; #define G_TYPE_MATCH_INFO (g_match_info_get_type ())
;;;
;;; The GType for a boxed type holding a GMatchInfo reference.
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_ARRAY
;;;
;;; #define G_TYPE_ARRAY (g_array_get_type ())
;;;
;;; The GType for a boxed type holding a GArray reference.
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_BYTE_ARRAY
;;;
;;; #define G_TYPE_BYTE_ARRAY (g_byte_array_get_type ())
;;;
;;; The GType for a boxed type holding a GByteArray reference.
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PTR_ARRAY
;;;
;;; #define G_TYPE_PTR_ARRAY (g_ptr_array_get_type ())
;;;
;;; The GType for a boxed type holding a GPtrArray reference.
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_BYTES
;;;
;;; #define G_TYPE_BYTES (g_bytes_get_type ())
;;;
;;; The GType for GBytes.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VARIANT_TYPE
;;;
;;; #define G_TYPE_VARIANT_TYPE (g_variant_type_get_gtype ())
;;;
;;; The GType for a boxed type holding a GVariantType.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_ERROR
;;;
;;; #define G_TYPE_ERROR (g_error_get_type ())
;;;
;;; The GType for a boxed type holding a GError.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_DATE_TIME
;;;
;;; #define G_TYPE_DATE_TIME (g_date_time_get_type ())
;;;
;;; The GType for a boxed type holding a GDateTime.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_TIME_ZONE
;;;
;;; #define G_TYPE_TIME_ZONE (g_time_zone_get_type ())
;;;
;;; The GType for a boxed type holding a GTimeZone.
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IO_CHANNEL
;;;
;;; #define G_TYPE_IO_CHANNEL (g_io_channel_get_type ())
;;;
;;; The GType for GIOChannel.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_IO_CONDITION
;;;
;;; #define G_TYPE_IO_CONDITION (g_io_condition_get_type ())
;;;
;;; The GType for GIOCondition.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VARIANT_BUILDER
;;;
;;; #define G_TYPE_VARIANT_BUILDER (g_variant_builder_get_type ())
;;;
;;; The GType for a boxed type holding a GVariantBuilder.
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_VARIANT_DICT
;;;
;;; #define G_TYPE_VARIANT_DICT (g_variant_dict_get_type ())
;;;
;;; The GType for a boxed type holding a GVariantDict.
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_KEY_FILE
;;;
;;; #define G_TYPE_KEY_FILE (g_key_file_get_type ())
;;;
;;; The GType for a boxed type holding a GKeyFile.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MAIN_CONTEXT
;;;
;;; #define G_TYPE_MAIN_CONTEXT (g_main_context_get_type ())
;;;
;;; The GType for a boxed type holding a GMainContext.
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MAIN_LOOP
;;;
;;; #define G_TYPE_MAIN_LOOP (g_main_loop_get_type ())
;;;
;;; The GType for a boxed type holding a GMainLoop.
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MAPPED_FILE
;;;
;;; #define G_TYPE_MAPPED_FILE (g_mapped_file_get_type ())
;;;
;;; The GType for a boxed type holding a GMappedFile.
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_MARKUP_PARSE_CONTEXT
;;;
;;; #define G_TYPE_MARKUP_PARSE_CONTEXT (g_markup_parse_context_get_type ())
;;;
;;; The GType for a boxed type holding a GMarkupParseContext.
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_SOURCE
;;;
;;; #define G_TYPE_SOURCE (g_source_get_type ())
;;;
;;; The GType for a boxed type holding a GSource.
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_POLLFD
;;;
;;; #define G_TYPE_POLLFD (g_pollfd_get_type ())
;;;
;;; The GType for a boxed type holding a GPollFD.
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_THREAD
;;;
;;; #define G_TYPE_THREAD (g_thread_get_type ())
;;;
;;; The GType for a boxed type holding a GThread.
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_OPTION_GROUP
;;;
;;; #define G_TYPE_OPTION_GROUP (g_option_group_get_type ())
;;;
;;; The GType for a boxed type holding a GOptionGroup.
;;;
;;; Since 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_URI
;;;
;;; #define G_TYPE_URI (g_uri_get_type ())
;;;
;;; The GType for a boxed type holding a GUri.
;;;
;;; Since 2.66
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.boxed.lisp -----------------------------------------
