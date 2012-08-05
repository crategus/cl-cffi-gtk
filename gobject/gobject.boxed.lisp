;;; ----------------------------------------------------------------------------
;;; gobject.boxed.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GObject Reference Manual Version 2.30.3. See http://www.gtk.org
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; A mechanism to wrap opaque C structures registered by the type system
;;;
;;; Synopsis
;;;
;;;     g_boxed_copy
;;;     g_boxed_free
;;;     g_boxed_type_register_static
;;;     g_pointer_type_register_static
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
;;;     G_TYPE_VARIANT_TYPE
;;;     G_TYPE_ERROR
;;;     G_TYPE_DATE_TIME
;;;     G_TYPE_IO_CHANNEL
;;;     G_TYPE_IO_CONDITION
;;;     G_TYPE_VARIANT_BUILDER
;;;     G_TYPE_MAIN_CONTEXT
;;;     G_TYPE_MAIN_LOOP
;;;     G_TYPE_SOURCE
;;;     GStrv;
;;;
;;; Description
;;;
;;; GBoxed is a generic wrapper mechanism for arbitrary C structures. The only
;;; thing the type system needs to know about the structures is how to copy and
;;; free them, beyond that they are treated as opaque chunks of memory.
;;;
;;; Boxed types are useful for simple value-holder structures like rectangles or
;;; points. They can also be used for wrapping structures defined in non-GObject
;;; based libraries.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; GBoxedCopyFunc ()
;;;
;;; gpointer (*GBoxedCopyFunc) (gpointer boxed)
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
;;; void (*GBoxedFreeFunc) (gpointer boxed)
;;;
;;; This function is provided by the user and should free the boxed structure
;;; passed.
;;;
;;; boxed :
;;;     The boxed structure to be freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_boxed_copy ()
;;;
;;; gpointer g_boxed_copy (GType boxed_type, gconstpointer src_boxed)
;;;
;;; Provide a copy of a boxed structure src_boxed which is of type boxed_type.
;;;
;;; boxed_type :
;;;     The type of src_boxed.
;;;
;;; src_boxed :
;;;     The boxed structure to be copied.
;;;
;;; Returns :
;;;     The newly created copy of the boxed structure.
;;; ----------------------------------------------------------------------------

(defcfun ("g_boxed_copy" g-boxed-copy) :pointer
  (boxed-type g-type)
  (src-boxed :pointer))

;;; ----------------------------------------------------------------------------
;;; g_boxed_free ()
;;;
;;; void g_boxed_free (GType boxed_type, gpointer boxed)
;;;
;;; Free the boxed structure boxed which is of type boxed_type.
;;;
;;; boxed_type :
;;;     The type of boxed.
;;;
;;; boxed :
;;;     The boxed structure to be freed.
;;; ----------------------------------------------------------------------------

(defcfun ("g_boxed_free" g-boxed-free) :void
  (boxed-type g-type)
  (boxed :pointer))

;;; ----------------------------------------------------------------------------
;;; g_boxed_type_register_static ()
;;;
;;; GType g_boxed_type_register_static (const gchar *name,
;;;                                     GBoxedCopyFunc boxed_copy,
;;;                                     GBoxedFreeFunc boxed_free)
;;;
;;; This function creates a new G_TYPE_BOXED derived type id for a new boxed
;;; type with name name. Boxed type handling functions have to be provided to
;;; copy and free opaque boxed structures of this type.
;;;
;;; name :
;;;     Name of the new boxed type.
;;;
;;; boxed_copy :
;;;     Boxed structure copy function.
;;;
;;; boxed_free :
;;;     Boxed structure free function.
;;;
;;; Returns :
;;;     New G_TYPE_BOXED derived type id for name.
;;; ----------------------------------------------------------------------------

(defcfun ("g_boxed_type_register_static" g-boxed-type-register-static) g-type
  (name :string)
  (copy-fn :pointer)
  (free-fn :pointer))

;;; ----------------------------------------------------------------------------
;;; g_pointer_type_register_static ()
;;;
;;; GType g_pointer_type_register_static (const gchar *name)
;;;
;;; Creates a new G_TYPE_POINTER derived type id for a new pointer type with
;;; name name.
;;;
;;; name :
;;;     the name of the new pointer type.
;;;
;;; Returns :
;;;     a new G_TYPE_POINTER derived type id for name.
;;; ----------------------------------------------------------------------------

(defcfun ("g_pointer_type_register_static" g-pointer-type-register-static)
    g-type
  (name :string))

(export 'g-pointer-type-register-static)

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
;;;
;;; #define G_TYPE_STRV (g_strv_get_type ())
;;;
;;; The GType for a boxed type holding a NULL-terminated array of strings.
;;;
;;; The code fragments in the following example show the use of a property of
;;; type G_TYPE_STRV with g_object_class_install_property(), g_object_set() and
;;; g_object_get().
;;;
;;; g_object_class_install_property (object_class,
;;;                                  PROP_AUTHORS,
;;;                                  g_param_spec_boxed("authors",
;;;                                                     _("Authors"),
;;;                                                     _("List of authors"),
;;;                                                     G_TYPE_STRV,
;;;                                                     G_PARAM_READWRITE));
;;;
;;; gchar *authors[] = { "Owen", "Tim", NULL };
;;; g_object_set (obj, "authors", authors, NULL);
;;;
;;; gchar *writers[];
;;; g_object_get (obj, "authors", &writers, NULL);
;;; /* do something with writers */
;;; g_strfreev (writers);
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_strv_get_type" g-type-strv) g-type)

(export 'g-type-strv)

(at-init nil (g-type-strv))

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
;;; G_TYPE_SOURCE
;;;
;;; #define G_TYPE_SOURCE (g_source_get_type ())
;;;
;;; The GType for a boxed type holding a GSource.
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.boxed.lisp -----------------------------------------
