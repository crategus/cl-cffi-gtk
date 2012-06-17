;;; ----------------------------------------------------------------------------
;;; glib.variant-type.lisp
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
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
;;;
;;; GVariantType
;;; 
;;; GVariantType - introduction to the GVariant type system
;;;
;;; Synopsis
;;;
;;;     GVariantType
;;;
;;;     G_VARIANT_TYPE_BOOLEAN
;;;     G_VARIANT_TYPE_BYTE
;;;     G_VARIANT_TYPE_INT16
;;;     G_VARIANT_TYPE_UINT16
;;;     G_VARIANT_TYPE_INT32
;;;     G_VARIANT_TYPE_UINT32
;;;     G_VARIANT_TYPE_INT64
;;;     G_VARIANT_TYPE_UINT64
;;;     G_VARIANT_TYPE_HANDLE
;;;     G_VARIANT_TYPE_DOUBLE
;;;     G_VARIANT_TYPE_STRING
;;;     G_VARIANT_TYPE_OBJECT_PATH
;;;     G_VARIANT_TYPE_SIGNATURE
;;;     G_VARIANT_TYPE_VARIANT
;;;     G_VARIANT_TYPE_ANY
;;;     G_VARIANT_TYPE_BASIC
;;;     G_VARIANT_TYPE_MAYBE
;;;     G_VARIANT_TYPE_ARRAY
;;;     G_VARIANT_TYPE_TUPLE
;;;     G_VARIANT_TYPE_UNIT
;;;     G_VARIANT_TYPE_DICT_ENTRY
;;;     G_VARIANT_TYPE_DICTIONARY
;;;     G_VARIANT_TYPE_STRING_ARRAY
;;;     G_VARIANT_TYPE_OBJECT_PATH_ARRAY
;;;     G_VARIANT_TYPE_BYTESTRING
;;;     G_VARIANT_TYPE_BYTESTRING_ARRAY
;;;     G_VARIANT_TYPE_VARDICT
;;;     
;;;     G_VARIANT_TYPE
;;;     g_variant_type_free
;;;     g_variant_type_copy
;;;     g_variant_type_new
;;;     
;;;     g_variant_type_string_is_valid
;;;     g_variant_type_string_scan
;;;     g_variant_type_get_string_length
;;;     g_variant_type_peek_string
;;;     g_variant_type_dup_string
;;;     
;;;     g_variant_type_is_definite
;;;     g_variant_type_is_container
;;;     g_variant_type_is_basic
;;;     g_variant_type_is_maybe
;;;     g_variant_type_is_array
;;;     g_variant_type_is_tuple
;;;     g_variant_type_is_dict_entry
;;;     g_variant_type_is_variant
;;;     
;;;     g_variant_type_hash
;;;     g_variant_type_equal
;;;     g_variant_type_is_subtype_of
;;;     
;;;     g_variant_type_new_maybe
;;;     g_variant_type_new_array
;;;     g_variant_type_new_tuple
;;;     g_variant_type_new_dict_entry
;;;     
;;;     g_variant_type_element
;;;     g_variant_type_n_items
;;;     g_variant_type_first
;;;     g_variant_type_next
;;;     g_variant_type_key
;;;     g_variant_type_value
;;; 
;;; Description
;;; 
;;; This section introduces the GVariant type system. It is based, in large
;;; part, on the D-Bus type system, with two major changes and some minor
;;; lifting of restrictions. The DBus specification, therefore, provides a
;;; significant amount of information that is useful when working with GVariant.
;;; 
;;; The first major change with respect to the D-Bus type system is the
;;; introduction of maybe (or "nullable") types. Any type in GVariant can be
;;; converted to a maybe type, in which case, "nothing" (or "null") becomes a
;;; valid value. Maybe types have been added by introducing the character "m" to
;;; type strings.
;;; 
;;; The second major change is that the GVariant type system supports the
;;; concept of "indefinite types" -- types that are less specific than the
;;; normal types found in D-Bus. For example, it is possible to speak of "an
;;; array of any type" in GVariant, where the D-Bus type system would require
;;; you to speak of "an array of integers" or "an array of strings". Indefinite
;;; types have been added by introducing the characters "*", "?" and "r" to type
;;; strings.
;;; 
;;; Finally, all arbitrary restrictions relating to the complexity of types are
;;; lifted along with the restriction that dictionary entries may only appear
;;; nested inside of arrays.
;;; 
;;; Just as in D-Bus, GVariant types are described with strings
;;; ("type strings"). Subject to the differences mentioned above, these strings
;;; are of the same form as those found in DBus. Note, however: D-Bus always
;;; works in terms of messages and therefore individual type strings appear
;;; nowhere in its interface. Instead, "signatures" are a concatenation of the
;;; strings of the type of each argument in a message. GVariant deals with
;;; single values directly so GVariant type strings always describe the type of
;;; exactly one value. This means that a D-Bus signature string is generally not
;;; a valid GVariant type string -- except in the case that it is the signature
;;; of a message containing exactly one argument.
;;; 
;;; An indefinite type is similar in spirit to what may be called an abstract
;;; type in other type systems. No value can exist that has an indefinite type
;;; as its type, but values can exist that have types that are subtypes of
;;; indefinite types. That is to say, g_variant_get_type() will never return an
;;; indefinite type, but calling g_variant_is_of_type() with an indefinite type
;;; may return TRUE. For example, you cannot have a value that represents "an
;;; array of no particular type", but you can have an "array of integers" which
;;; certainly matches the type of "an array of no particular type", since "array
;;; of integers" is a subtype of "array of no particular type".
;;; 
;;; This is similar to how instances of abstract classes may not directly exist
;;; in other type systems, but instances of their non-abstract subtypes may. For
;;; example, in GTK, no object that has the type of GtkBin can exist (since
;;; GtkBin is an abstract class), but a GtkWindow can certainly be instantiated,
;;; and you would say that the GtkWindow is a GtkBin (since GtkWindow is a
;;; subclass of GtkBin).
;;; 
;;; A detailed description of GVariant type strings is given here:
;;; 
;;; GVariant Type Strings
;;; 
;;; A GVariant type string can be any of the following:
;;; 
;;;   * any basic type string (listed below)
;;; 
;;;   * "v", "r" or "*"
;;; 
;;;   * one of the characters 'a' or 'm', followed by another type string
;;; 
;;;   * the character '(', followed by a concatenation of zero or more other
;;;     type strings, followed by the character ')'
;;; 
;;;   * the character '{', followed by a basic type string (see below), followed
;;;     by another type string, followed by the character '}'
;;; 
;;; A basic type string describes a basic type (as per
;;; g_variant_type_is_basic()) and is always a single character in length. The
;;; valid basic type strings are "b", "y", "n", "q", "i", "u", "x", "t", "h",
;;; "d", "s", "o", "g" and "?".
;;; 
;;; The above definition is recursive to arbitrary depth. "aaaaai" and
;;; "(ui(nq((y)))s)" are both valid type strings, as is
;;; "a(aa(ui)(qna{ya(yd)}))".
;;; 
;;; The meaning of each of the characters is as follows:
;;; 
;;;  b  the type string of G_VARIANT_TYPE_BOOLEAN; a boolean value.
;;; 
;;;  y  the type string of G_VARIANT_TYPE_BYTE; a byte.
;;; 
;;;  n  the type string of G_VARIANT_TYPE_INT16; a signed 16 bit integer.
;;; 
;;;  q  the type string of G_VARIANT_TYPE_UINT16; an unsigned 16 bit integer.
;;; 
;;;  i  the type string of G_VARIANT_TYPE_INT32; a signed 32 bit integer.
;;; 
;;;  u  the type string of G_VARIANT_TYPE_UINT32; an unsigned 32 bit integer.
;;; 
;;;  x  the type string of G_VARIANT_TYPE_INT64; a signed 64 bit integer.
;;; 
;;;  t  the type string of G_VARIANT_TYPE_UINT64; an unsigned 64 bit integer.
;;; 
;;;  h  the type string of G_VARIANT_TYPE_HANDLE; a signed 32 bit value that,
;;;     by convention, is used as an index into an array of file descriptors
;;;     that are sent alongside a D-Bus message.
;;; 
;;;  d  the type string of G_VARIANT_TYPE_DOUBLE; a double precision floating
;;;     point value.
;;; 
;;;  s  the type string of G_VARIANT_TYPE_STRING; a string.
;;; 
;;;  o  the type string of G_VARIANT_TYPE_OBJECT_PATH; a string in the form of a
;;;     D-Bus object path.
;;; 
;;;  g  the type string of G_VARIANT_TYPE_STRING; a string in the form of a
;;;     D-Bus type signature.
;;; 
;;;  ?  the type string of G_VARIANT_TYPE_BASIC; an indefinite type that is a
;;;     supertype of any of the basic types.
;;; 
;;;  v  the type string of G_VARIANT_TYPE_VARIANT; a container type that contain
;;;     any other type of value.
;;; 
;;;  a  used as a prefix on another type string to mean an array of that type;
;;;     the type string "ai", for example, is the type of an array of 32 bit
;;;     signed integers.
;;; 
;;;  m  used as a prefix on another type string to mean a "maybe", or
;;;     "nullable", version of that type; the type string "ms", for example, is
;;;     the type of a value that maybe contains a string, or maybe contains
;;;     nothing.
;;; 
;;;  () used to enclose zero or more other concatenated type strings to create a
;;;     tuple type; the type string "(is)", for example, is the type of a pair
;;;     of an integer and a string.
;;; 
;;;  r  the type string of G_VARIANT_TYPE_TUPLE; an indefinite type that is a
;;;     supertype of any tuple type, regardless of the number of items.
;;; 
;;;  {} used to enclose a basic type string concatenated with another type
;;;     string to create a dictionary entry type, which usually appears inside
;;;     of an array to form a dictionary; the type string "a{sd}", for example,
;;;     is the type of a dictionary that maps strings to double precision
;;;     floating point values.
;;; 
;;;     The first type (the basic type) is the key type and the second type is
;;;     the value type. The reason that the first type is restricted to being a
;;;     basic type is so that it can easily be hashed.
;;; 
;;;  *  the type string of G_VARIANT_TYPE_ANY; the indefinite type that is a
;;;     supertype of all types. Note that, as with all type strings, this
;;;     character represents exactly one type. It cannot be used inside of
;;;     tuples to mean "any number of items".
;;; 
;;; Any type string of a container that contains an indefinite type is, itself,
;;; an indefinite type. For example, the type string "a*" (corresponding to
;;; G_VARIANT_TYPE_ARRAY) is an indefinite type that is a supertype of every
;;; array type. "(*s)" is a supertype of all tuples that contain exactly two
;;; items where the second item is a string.
;;; 
;;; "a{?*}" is an indefinite type that is a supertype of all arrays containing
;;; dictionary entries where the key is any basic type and the value is any type
;;; at all. This is, by definition, a dictionary, so this type string
;;; corresponds to G_VARIANT_TYPE_DICTIONARY. Note that, due to the restriction
;;; that the key of a dictionary entry must be a basic type, "{**}" is not a
;;; valid type string.
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GVariantType
;;; 
;;; typedef struct _GVariantType GVariantType;
;;; 
;;; A type in the GVariant type system.
;;; 
;;; Two types may not be compared by value; use g_variant_type_equal() or
;;; g_variant_type_is_subtype_of(). May be copied using g_variant_type_copy()
;;; and freed using g_variant_type_free().
;;; ----------------------------------------------------------------------------

(defctype g-variant-type :pointer)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BOOLEAN
;;; 
;;; #define G_VARIANT_TYPE_BOOLEAN ((const GVariantType *) "b")
;;; 
;;; The type of a value that can be either TRUE or FALSE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BYTE
;;; 
;;; #define G_VARIANT_TYPE_BYTE ((const GVariantType *) "y")
;;; 
;;; The type of an integer value that can range from 0 to 255.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_INT16
;;; 
;;; #define G_VARIANT_TYPE_INT16 ((const GVariantType *) "n")
;;; 
;;; The type of an integer value that can range from -32768 to 32767.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UINT16
;;; 
;;; #define G_VARIANT_TYPE_UINT16 ((const GVariantType *) "q")
;;; 
;;; The type of an integer value that can range from 0 to 65535. There were
;;; about this many people living in Toronto in the 1870s.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_INT32
;;; 
;;; #define G_VARIANT_TYPE_INT32 ((const GVariantType *) "i")
;;; 
;;; The type of an integer value that can range from -2147483648 to 2147483647.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UINT32
;;; 
;;; #define G_VARIANT_TYPE_UINT32 ((const GVariantType *) "u")
;;; 
;;; The type of an integer value that can range from 0 to 4294967295. That's one
;;; number for everyone who was around in the late 1970s.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_INT64
;;; 
;;; #define G_VARIANT_TYPE_INT64 ((const GVariantType *) "x")
;;; 
;;; The type of an integer value that can range from -9223372036854775808 to
;;; 9223372036854775807.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UINT64
;;; 
;;; #define G_VARIANT_TYPE_UINT64 ((const GVariantType *) "t")
;;; 
;;; The type of an integer value that can range from 0 to 18446744073709551616.
;;; That's a really big number, but a Rubik's cube can have a bit more than
;;; twice as many possible positions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_HANDLE
;;; 
;;; #define G_VARIANT_TYPE_HANDLE ((const GVariantType *) "h")
;;; 
;;; The type of a 32bit signed integer value, that by convention, is used as an
;;; index into an array of file descriptors that are sent alongside a D-Bus
;;; message.
;;; 
;;; If you are not interacting with D-Bus, then there is no reason to make use
;;; of this type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_DOUBLE
;;; 
;;; #define G_VARIANT_TYPE_DOUBLE ((const GVariantType *) "d")
;;; 
;;; The type of a double precision IEEE754 floating point number. These guys go
;;; up to about 1.80e308 (plus and minus) but miss out on some numbers in
;;; between. In any case, that's far greater than the estimated number of
;;; fundamental particles in the observable universe.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_STRING
;;; 
;;; #define G_VARIANT_TYPE_STRING ((const GVariantType *) "s")
;;; 
;;; The type of a string. "" is a string. NULL is not a string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_OBJECT_PATH
;;; 
;;; #define G_VARIANT_TYPE_OBJECT_PATH ((const GVariantType *) "o")
;;; 
;;; The type of a D-Bus object reference. These are strings of a specific format
;;; used to identify objects at a given destination on the bus.
;;; 
;;; If you are not interacting with D-Bus, then there is no reason to make use
;;; of this type. If you are, then the D-Bus specification contains a precise
;;; description of valid object paths.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_SIGNATURE
;;; 
;;; #define G_VARIANT_TYPE_SIGNATURE ((const GVariantType *) "g")
;;; 
;;; The type of a D-Bus type signature. These are strings of a specific format
;;; used as type signatures for D-Bus methods and messages.
;;; 
;;; If you are not interacting with D-Bus, then there is no reason to make use
;;; of this type. If you are, then the D-Bus specification contains a precise
;;; description of valid signature strings.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_VARIANT
;;; 
;;; #define G_VARIANT_TYPE_VARIANT ((const GVariantType *) "v")
;;; 
;;; The type of a box that contains any other value (including another variant).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_ANY
;;; 
;;; #define G_VARIANT_TYPE_ANY ((const GVariantType *) "*")
;;; 
;;; An indefinite type that is a supertype of every type (including itself).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BASIC
;;; 
;;; #define G_VARIANT_TYPE_BASIC ((const GVariantType *) "?")
;;; 
;;; An indefinite type that is a supertype of every basic (ie: non-container)
;;; type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_MAYBE
;;; 
;;; #define G_VARIANT_TYPE_MAYBE ((const GVariantType *) "m*")
;;; 
;;; An indefinite type that is a supertype of every maybe type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_ARRAY
;;; 
;;; #define G_VARIANT_TYPE_ARRAY ((const GVariantType *) "a*")
;;; 
;;; An indefinite type that is a supertype of every array type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_TUPLE
;;; 
;;; #define G_VARIANT_TYPE_TUPLE ((const GVariantType *) "r")
;;; 
;;; An indefinite type that is a supertype of every tuple type, regardless of
;;; the number of items in the tuple.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UNIT
;;; 
;;; #define G_VARIANT_TYPE_UNIT ((const GVariantType *) "()")
;;; 
;;; The empty tuple type. Has only one instance. Known also as "triv" or "void".
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_DICT_ENTRY
;;; 
;;; #define G_VARIANT_TYPE_DICT_ENTRY ((const GVariantType *) "{?*}")
;;; 
;;; An indefinite type that is a supertype of every dictionary entry type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_DICTIONARY
;;; 
;;; #define G_VARIANT_TYPE_DICTIONARY ((const GVariantType *) "a{?*}")
;;; 
;;; An indefinite type that is a supertype of every dictionary type -- that is,
;;; any array type that has an element type equal to any dictionary entry type.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_STRING_ARRAY
;;; 
;;; #define G_VARIANT_TYPE_STRING_ARRAY ((const GVariantType *) "as")
;;; 
;;; The type of an array of strings.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_OBJECT_PATH_ARRAY
;;; 
;;; #define G_VARIANT_TYPE_OBJECT_PATH_ARRAY ((const GVariantType *) "ao")
;;; 
;;; The type of an array of object paths.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BYTESTRING
;;; 
;;; #define G_VARIANT_TYPE_BYTESTRING ((const GVariantType *) "ay")
;;; 
;;; The type of an array of bytes. This type is commonly used to pass around
;;; strings that may not be valid utf8. In that case, the convention is that the
;;; nul terminator character should be included as the last character in the
;;; array.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BYTESTRING_ARRAY
;;; 
;;; #define G_VARIANT_TYPE_BYTESTRING_ARRAY ((const GVariantType *) "aay")
;;; 
;;; The type of an array of byte strings (an array of arrays of bytes).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_VARDICT
;;; 
;;; #define G_VARIANT_TYPE_VARDICT ((const GVariantType *) "a{sv}")
;;; 
;;; The type of a dictionary mapping strings to variants (the ubiquitous "a{sv}"
;;; type).
;;; 
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE()
;;; 
;;; # define G_VARIANT_TYPE(type_string)
;;;                                    (g_variant_type_checked_ ((type_string)))
;;; 
;;; Converts a string to a const GVariantType. Depending on the current
;;; debugging level, this function may perform a runtime check to ensure that
;;; string is a valid GVariant type string.
;;; 
;;; It is always a programmer error to use this macro with an invalid type
;;; string. If in doubt, use g_variant_type_string_is_valid() to check if the
;;; string is valid.
;;; 
;;; type_string :
;;;     a well-formed GVariantType type string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_free ()
;;; 
;;; void g_variant_type_free (GVariantType *type);
;;; 
;;; Frees a GVariantType that was allocated with g_variant_type_copy(),
;;; g_variant_type_new() or one of the container type constructor functions.
;;; 
;;; In the case that type is NULL, this function does nothing.
;;; 
;;; type :
;;;     a GVariantType, or NULL
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_copy ()
;;; 
;;; GVariantType * g_variant_type_copy (const GVariantType *type);
;;; 
;;; Makes a copy of a GVariantType. It is appropriate to call
;;; g_variant_type_free() on the return value. type may not be NULL.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     a new GVariantType
;;;
;;; Since 2.24.
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_copy" g-variant-type-copy) g-variant-type
  (type g-variant-type))

(export 'g-variant-type-copy)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new ()
;;; 
;;; GVariantType * g_variant_type_new (const gchar *type_string);
;;; 
;;; Creates a new GVariantType corresponding to the type string given by
;;; type_string. It is appropriate to call g_variant_type_free() on the return
;;; value.
;;; 
;;; It is a programmer error to call this function with an invalid type string.
;;; Use g_variant_type_string_is_valid() if you are unsure.
;;; 
;;; type_string :
;;;     a valid GVariant type string
;;; 
;;; Returns :
;;;     a new GVariantType
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_new" g-variant-type-new) :pointer
  (type-string :string))

(export 'g-variant-type-new)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_string_is_valid ()
;;; 
;;; gboolean g_variant_type_string_is_valid (const gchar *type_string);
;;; 
;;; Checks if type_string is a valid GVariant type string. This call is
;;; equivalent to calling g_variant_type_string_scan() and confirming that the
;;; following character is a nul terminator.
;;; 
;;; type_string :
;;;     a pointer to any string
;;; 
;;; Returns :
;;;     TRUE if type_string is exactly one valid type string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_string_is_valid" g-variant-type-string-is-valid)
    :boolean
  (type-string :string))

(export 'g-variant-type-string-is-valid)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_string_scan ()
;;; 
;;; gboolean g_variant_type_string_scan (const gchar *string,
;;;                                      const gchar *limit,
;;;                                      const gchar **endptr);
;;; 
;;; Scan for a single complete and valid GVariant type string in string. The
;;; memory pointed to by limit (or bytes beyond it) is never accessed.
;;; 
;;; If a valid type string is found, endptr is updated to point to the first
;;; character past the end of the string that was found and TRUE is returned.
;;; 
;;; If there is no valid type string starting at string, or if the type string
;;; does not end before limit then FALSE is returned.
;;; 
;;; For the simple case of checking if a string is a valid type string, see
;;; g_variant_type_string_is_valid().
;;; 
;;; string :
;;;     a pointer to any string
;;; 
;;; limit :
;;;     the end of string, or NULL
;;; 
;;; endptr :
;;;     location to store the end pointer, or NULL
;;; 
;;; Returns :
;;;     TRUE if a valid type string was found
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_get_string_length ()
;;; 
;;; gsize g_variant_type_get_string_length (const GVariantType *type);
;;; 
;;; Returns the length of the type string corresponding to the given type. This
;;; function must be used to determine the valid extent of the memory region
;;; returned by g_variant_type_peek_string().
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     the length of the corresponding type string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_peek_string ()
;;; 
;;; const gchar * g_variant_type_peek_string (const GVariantType *type);
;;; 
;;; Returns the type string corresponding to the given type. The result is not
;;; nul-terminated; in order to determine its length you must call
;;; g_variant_type_get_string_length().
;;; 
;;; To get a nul-terminated string, see g_variant_type_dup_string().
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     the corresponding type string (not nul-terminated)
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_peek_string" g-variant-type-peek-string) :string
  (type (:pointer g-variant-type)))

(export 'g-variant-type-peek-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_dup_string ()
;;; 
;;; gchar * g_variant_type_dup_string (const GVariantType *type);
;;; 
;;; Returns a newly-allocated copy of the type string corresponding to type. The
;;; returned string is nul-terminated. It is appropriate to call g_free() on the
;;; return value.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     the corresponding type string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_definite ()
;;; 
;;; gboolean g_variant_type_is_definite (const GVariantType *type);
;;; 
;;; Determines if the given type is definite (ie: not indefinite).
;;; 
;;; A type is definite if its type string does not contain any indefinite type
;;; characters ('*', '?', or 'r').
;;; 
;;; A GVariant instance may not have an indefinite type, so calling this
;;; function on the result of g_variant_get_type() will always result in TRUE
;;; being returned. Calling this function on an indefinite type like
;;; G_VARIANT_TYPE_ARRAY, however, will result in FALSE being returned.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     TRUE if type is definite
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_container ()
;;; 
;;; gboolean g_variant_type_is_container (const GVariantType *type);
;;; 
;;; Determines if the given type is a container type.
;;; 
;;; Container types are any array, maybe, tuple, or dictionary entry types plus
;;; the variant type.
;;; 
;;; This function returns TRUE for any indefinite type for which every definite
;;; subtype is a container -- G_VARIANT_TYPE_ARRAY, for example.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     TRUE if type is a container type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_basic ()
;;; 
;;; gboolean g_variant_type_is_basic (const GVariantType *type);
;;; 
;;; Determines if the given type is a basic type.
;;; 
;;; Basic types are booleans, bytes, integers, doubles, strings, object paths
;;; and signatures.
;;; 
;;; Only a basic type may be used as the key of a dictionary entry.
;;; 
;;; This function returns FALSE for all indefinite types except
;;; G_VARIANT_TYPE_BASIC.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     TRUE if type is a basic type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_maybe ()
;;; 
;;; gboolean g_variant_type_is_maybe (const GVariantType *type);
;;; 
;;; Determines if the given type is a maybe type. This is true if the type
;;; string for type starts with an 'm'.
;;; 
;;; This function returns TRUE for any indefinite type for which every definite
;;; subtype is a maybe type -- G_VARIANT_TYPE_MAYBE, for example.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     TRUE if type is a maybe type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_array ()
;;; 
;;; gboolean g_variant_type_is_array (const GVariantType *type);
;;; 
;;; Determines if the given type is an array type. This is true if the type
;;; string for type starts with an 'a'.
;;; 
;;; This function returns TRUE for any indefinite type for which every definite
;;; subtype is an array type -- G_VARIANT_TYPE_ARRAY, for example.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     TRUE if type is an array type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_tuple ()
;;; 
;;; gboolean g_variant_type_is_tuple (const GVariantType *type);
;;; 
;;; Determines if the given type is a tuple type. This is true if the type
;;; string for type starts with a '(' or if type is G_VARIANT_TYPE_TUPLE.
;;; 
;;; This function returns TRUE for any indefinite type for which every definite
;;; subtype is a tuple type -- G_VARIANT_TYPE_TUPLE, for example.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     TRUE if type is a tuple type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_dict_entry ()
;;; 
;;; gboolean g_variant_type_is_dict_entry (const GVariantType *type);
;;; 
;;; Determines if the given type is a dictionary entry type. This is true if the
;;; type string for type starts with a '{'.
;;; 
;;; This function returns TRUE for any indefinite type for which every definite
;;; subtype is a dictionary entry type -- G_VARIANT_TYPE_DICT_ENTRY, for
;;; example.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     TRUE if type is a dictionary entry type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_variant ()
;;; 
;;; gboolean g_variant_type_is_variant (const GVariantType *type);
;;; 
;;; Determines if the given type is the variant type.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     TRUE if type is the variant type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_hash ()
;;; 
;;; guint g_variant_type_hash (gconstpointer type);
;;; 
;;; Hashes type.
;;; 
;;; The argument type of type is only gconstpointer to allow use with GHashTable
;;; without function pointer casting. A valid GVariantType must be provided.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     the hash value
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_equal ()
;;; 
;;; gboolean g_variant_type_equal (gconstpointer type1, gconstpointer type2);
;;; 
;;; Compares type1 and type2 for equality.
;;; 
;;; Only returns TRUE if the types are exactly equal. Even if one type is an
;;; indefinite type and the other is a subtype of it, FALSE will be returned if
;;; they are not exactly equal. If you want to check for subtypes, use
;;; g_variant_type_is_subtype_of().
;;; 
;;; The argument types of type1 and type2 are only gconstpointer to allow use
;;; with GHashTable without function pointer casting. For both arguments, a
;;; valid GVariantType must be provided.
;;; 
;;; type1 :
;;;     a GVariantType
;;; 
;;; type2 :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     TRUE if type1 and type2 are exactly equal
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_subtype_of ()
;;; 
;;; gboolean g_variant_type_is_subtype_of (const GVariantType *type,
;;;                                        const GVariantType *supertype);
;;; 
;;; Checks if type is a subtype of supertype.
;;; 
;;; This function returns TRUE if type is a subtype of supertype. All types are
;;; considered to be subtypes of themselves. Aside from that, only indefinite
;;; types can have subtypes.
;;; 
;;; type :
;;;     a GVariantType
;;; 
;;; supertype :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     TRUE if type is a subtype of supertype Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_maybe ()
;;; 
;;; GVariantType * g_variant_type_new_maybe (const GVariantType *element);
;;; 
;;; Constructs the type corresponding to a maybe instance containing type type
;;; or Nothing.
;;; 
;;; It is appropriate to call g_variant_type_free() on the return value.
;;; 
;;; element :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     a new maybe GVariantType
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_array ()
;;; 
;;; GVariantType * g_variant_type_new_array (const GVariantType *element);
;;; 
;;; Constructs the type corresponding to an array of elements of the type type.
;;; 
;;; It is appropriate to call g_variant_type_free() on the return value.
;;; 
;;; element :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     a new array GVariantType
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_tuple ()
;;; 
;;; GVariantType * g_variant_type_new_tuple (const GVariantType * const *items,
;;;                                          gint length);
;;; 
;;; Constructs a new tuple type, from items.
;;; 
;;; length is the number of items in items, or -1 to indicate that items is
;;; NULL-terminated.
;;; 
;;; It is appropriate to call g_variant_type_free() on the return value.
;;; 
;;; items :
;;;     an array of GVariantTypes, one for each item
;;; 
;;; length :
;;;     the length of items, or -1
;;; 
;;; Returns :
;;;     a new tuple GVariantType
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_dict_entry ()
;;; 
;;; GVariantType * g_variant_type_new_dict_entry (const GVariantType *key,
;;;                                               const GVariantType *value);
;;; 
;;; Constructs the type corresponding to a dictionary entry with a key of type
;;; key and a value of type value.
;;; 
;;; It is appropriate to call g_variant_type_free() on the return value.
;;; 
;;; key :
;;;     a basic GVariantType
;;; 
;;; value :
;;;     a GVariantType
;;; 
;;; Returns :
;;;     a new dictionary entry GVariantType
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_element ()
;;; 
;;; const GVariantType * g_variant_type_element (const GVariantType *type);
;;; 
;;; Determines the element type of an array or maybe type.
;;; 
;;; This function may only be used with array or maybe types.
;;; 
;;; type :
;;;     an array or maybe GVariantType
;;; 
;;; Returns :
;;;     the element type of type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_n_items ()
;;; 
;;; gsize g_variant_type_n_items (const GVariantType *type);
;;; 
;;; Determines the number of items contained in a tuple or dictionary entry
;;; type.
;;; 
;;; This function may only be used with tuple or dictionary entry types, but
;;; must not be used with the generic tuple type G_VARIANT_TYPE_TUPLE.
;;; 
;;; In the case of a dictionary entry type, this function will always return 2.
;;; 
;;; type :
;;;     a tuple or dictionary entry GVariantType
;;; 
;;; Returns :
;;;     the number of items in type
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_first ()
;;; 
;;; const GVariantType * g_variant_type_first (const GVariantType *type);
;;; 
;;; Determines the first item type of a tuple or dictionary entry type.
;;; 
;;; This function may only be used with tuple or dictionary entry types, but
;;; must not be used with the generic tuple type G_VARIANT_TYPE_TUPLE.
;;; 
;;; In the case of a dictionary entry type, this returns the type of the key.
;;; 
;;; NULL is returned in case of type being G_VARIANT_TYPE_UNIT.
;;; 
;;; This call, together with g_variant_type_next() provides an iterator
;;; interface over tuple and dictionary entry types.
;;; 
;;; type :
;;;     a tuple or dictionary entry GVariantType
;;; 
;;; Returns :
;;;     the first item type of type, or NULL
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_next ()
;;; 
;;; const GVariantType * g_variant_type_next (const GVariantType *type);
;;; 
;;; Determines the next item type of a tuple or dictionary entry type.
;;; 
;;; type must be the result of a previous call to g_variant_type_first() or
;;; g_variant_type_next().
;;; 
;;; If called on the key type of a dictionary entry then this call returns the
;;; value type. If called on the value type of a dictionary entry then this call
;;; returns NULL.
;;; 
;;; For tuples, NULL is returned when type is the last item in a tuple.
;;; 
;;; type :
;;;     a GVariantType from a previous call
;;; 
;;; Returns :
;;;     the next GVariantType after type, or NULL
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_key ()
;;; 
;;; const GVariantType * g_variant_type_key (const GVariantType *type);
;;; 
;;; Determines the key type of a dictionary entry type.
;;; 
;;; This function may only be used with a dictionary entry type. Other than the
;;; additional restriction, this call is equivalent to g_variant_type_first().
;;; 
;;; type :
;;;     a dictionary entry GVariantType
;;; 
;;; Returns :
;;;     the key type of the dictionary entry
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_variant_type_value ()
;;; 
;;; const GVariantType * g_variant_type_value (const GVariantType *type);
;;; 
;;; Determines the value type of a dictionary entry type.
;;; 
;;; This function may only be used with a dictionary entry type.
;;; 
;;; type :
;;;     a dictionary entry GVariantType
;;; 
;;; Returns :
;;;     the value type of the dictionary entry
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------


;;; --- End of file glib.variant-type.lisp -------------------------------------
