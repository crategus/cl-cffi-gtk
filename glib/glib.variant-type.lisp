;;; ----------------------------------------------------------------------------
;;; glib.variant-type.lisp
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.34.3 Reference Manual. See <http://www.gtk.org>.
;;; The API documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; Introduction to the GVariant type system
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
;;;
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
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GVariantType
;;; ----------------------------------------------------------------------------

(gobject:define-g-boxed-opaque g-variant-type "GVariantType"
  :alloc (error "GVariantType can not be created from Lisp side"))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-variant-type atdoc:*class-name-alias*) "CStruct"
      (documentation 'g-variant-type 'type)
 "@version{2013-4-9}
  @begin{short}
    This section introduces the @symbol{g-variant} type system. It is based, in
    large part, on the D-Bus type system, with two major changes and some minor
    lifting of restrictions. The D-Bus specification, therefore, provides a
    significant amount of information that is useful when working with
    @symbol{g-variant}.
  @end{short}

  The first major change with respect to the D-Bus type system is the
  introduction of maybe (or \"nullable\") types. Any type in @symbol{g-variant}
  can be converted to a maybe type, in which case, \"nothing\" (or \"null\")
  becomes a valid value. Maybe types have been added by introducing the
  character \"m\" to type strings.

  The second major change is that the @symbol{g-variant} type system supports
  the concept of \"indefinite types\" -- types that are less specific than the
  normal types found in D-Bus. For example, it is possible to speak of \"an
  array of any type\" in @symbol{g-variant}, where the D-Bus type system would
  require you to speak of \"an array of integers\" or \"an array of strings\".
  Indefinite types have been added by introducing the characters \"*\", \"?\"
  and \"r\" to type strings.

  Finally, all arbitrary restrictions relating to the complexity of types are
  lifted along with the restriction that dictionary entries may only appear
  nested inside of arrays.

  Just as in D-Bus, @symbol{g-variant} types are described with strings
  (\"type strings\"). Subject to the differences mentioned above, these strings
  are of the same form as those found in D-Bus. Note, however: D-Bus always
  works in terms of messages and therefore individual type strings appear
  nowhere in its interface. Instead, \"signatures\" are a concatenation of the
  strings of the type of each argument in a message. @symbol{g-variant} deals
  with single values directly so @sym{g-variant} type strings always describe
  the type of exactly one value. This means that a D-Bus signature string is
  generally not a valid @symbol{g-variant} type string -- except in the case
  that it is the signature of a message containing exactly one argument.

  An indefinite type is similar in spirit to what may be called an abstract
  type in other type systems. No value can exist that has an indefinite type
  as its type, but values can exist that have types that are subtypes of
  indefinite types. That is to say, @fun{g-variant-get-type} will never return
  an indefinite type, but calling @fun{g-variant-is-of-type} with an indefinite
  type may return @em{true}. For example, you cannot have a value that
  represents \"an array of no particular type\", but you can have an \"array of
  integers\" which certainly matches the type of \"an array of no particular
  type\", since \"array of integers\" is a subtype of \"array of no particular
  type\".

  This is similar to how instances of abstract classes may not directly exist
  in other type systems, but instances of their non-abstract subtypes may. For
  example, in GTK+, no object that has the type of @class{gtk-bin} can exist
  (since @class{gtk-bin} is an abstract class), but a @class{gtk-window} can
  certainly be instantiated, and you would say that the @class{gtk-window} is a
  @class{gtk-bin} (since @class{gtk-window} is a subclass of @class{gtk-bin}).

  A detailed description of @symbol{g-variant} type strings is given here:

  @subheading{GVariant Type Strings}
    A @symbol{g-variant} type string can be any of the following:
    @begin{itemize}
      @item{any basic type string (listed below)}
      @item{\"v\", \"r\" or \"*\"}
      @item{one of the characters 'a' or 'm', followed by another type string}
      @item{the character '(', followed by a concatenation of zero or more other
        type strings, followed by the character ')'}
      @item{the character '{', followed by a basic type string (see below),
        followed by another type string, followed by the character '@}'}
    @end{itemize}
    A basic type string describes a basic type (as per
    @fun{g-variant-type-is-basic}) and is always a single character in length.
    The valid basic type strings are \"b\", \"y\", \"n\", \"q\", \"i\", \"u\",
    \"x\", \"t\", \"h\", \"d\", \"s\", \"o\", \"g\" and \"?\".

    The above definition is recursive to arbitrary depth. \"aaaaai\" and
    \"(ui(nq((y)))s)\" are both valid type strings, as is
    \"a(aa(ui)(qna{ya(yd)@}))\".

    The meaning of each of the characters is as follows:
    @begin[code]{table}
       @entry[b]{The type string of @var{+g-variant-type-boolean+};
         a boolean value.}
       @entry[y]{The type string of @var{+g-variant-type-byte+}; a byte.}
       @entry[n]{The type string of @var{+g-variant-type-int16+};
         a signed 16 bit integer.}
       @entry[q]{The type string of @var{+g-variant-type-uint16+};
         an unsigned 16 bit integer.}
       @entry[i]{The type string of @var{+g-variant-type-int32};
         a signed 32 bit integer.}
       @entry[u]{The type string of @var{+g-variant-type-uint32+};
         an unsigned 32 bit integer.}
       @entry[x]{The type string of @var{+g-variant-type-int64+};
         a signed 64 bit integer.}
       @entry[t]{The type string of @var{+g-variant-type-uint64+};
         an unsigned 64 bit integer.}
       @entry[h]{The type string of @var{+g-variant-type-handle+};
         a signed 32 bit value that, by convention, is used as an index into an
         array of file descriptors that are sent alongside a D-Bus message.}
       @entry[d]{The type string of @var{+g-variant-type-double+};
         a double precision floating point value.}
       @entry[s]{The type string of @var{+g-variant-type-string+}; a string.}
       @entry[o]{The type string of @var{+g-variant-type-object-path+};
         a string in the form of a D-Bus object path.}
       @entry[g]{The type string of @var{+g-variant-type-string+};
         a string in the form of a D-Bus type signature.}
       @entry[?]{The type string of @var{+g-variant-type-basic+};
         an indefinite type that is a supertype of any of the basic types.}
       @entry[v]{The type string of @var{+g-variant-type-variant+};
         a container type that contain any other type of value.}
       @entry[a]{Used as a prefix on another type string to mean an array of
         that type; the type string \"ai\", for example, is the type of an array
         of 32 bit signed integers.}
       @entry[m]{Used as a prefix on another type string to mean a \"maybe\", or
         \"nullable\", version of that type; the type string \"ms\", for
         example, is the type of a value that maybe contains a string, or maybe
         contains nothing.}
       @entry[()]{Used to enclose zero or more other concatenated type strings
         to create a tuple type; the type string \"(is)\", for example, is the
         type of a pair of an integer and a string.}
       @entry[r]{The type string of @var{+g-variant-type-tuple+};
         an indefinite type that is a supertype of any tuple type, regardless
         of the number of items.}
       @entry[{}]{Used to enclose a basic type string concatenated with another
         type string to create a dictionary entry type, which usually appears
         inside of an array to form a dictionary; the type string \"a{sd@}\",
         for example, is the type of a dictionary that maps strings to double
         precision floating point values.@br{}
         The first type (the basic type) is the key type and the second type is
         the value type. The reason that the first type is restricted to being a
         basic type is so that it can easily be hashed.}
       @entry[*]{The type string of @var{+g-variant-type-any+};
         the indefinite type that is a supertype of all types. Note that, as
         with all type strings, this character represents exactly one type. It
         cannot be used inside of tuples to mean \"any number of items\".}
    @end{table}
    Any type string of a container that contains an indefinite type is, itself,
    an indefinite type. For example, the type string \"a*\" (corresponding to
    @var{+g-variant-type-array+}) is an indefinite type that is a supertype of
    every array type. \"(*s)\" is a supertype of all tuples that contain exactly
    two items where the second item is a string.

    \"a{?*@}\" is an indefinite type that is a supertype of all arrays
    containing dictionary entries where the key is any basic type and the value
    is any type at all. This is, by definition, a dictionary, so this type
    string corresponds to @var{+g-variant-type-dictionary+}. Note that, due to
    the restriction that the key of a dictionary entry must be a basic type,
    \"{**@}\" is not a valid type string.

    Two types may not be compared by value; use @fun{g-variant-type-equal} or
    @fun{g-variant-type-is-subtype-of}. May be copied using
    @fun{g-variant-type-copy} and freed using @fun{g-variant-type-free}.
  @see-function{g-variant-get-type}
  @see-function{g-variant-is-of-type}
  @see-function{g-variant-type-is-basic}
  @see-function{g-variant-type-equal}
  @see-function{g-variant-type-is-subtype-of}
  @see-function{g-variant-type-copy}
  @see-function{g-variant-type-free}")

(export (gobject:boxed-related-symbols 'g-variant-type))

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BOOLEAN
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-boolean+ "b")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-boolean+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-boolean+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"b\"}
  The type of a value that can be either @em{true} or @code{nil}.")

(export '+g-variant-type-boolean+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BYTE
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-byte+ "y")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-byte+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-byte+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"y\"}
  The type of an integer value that can range from 0 to 255.")

(export '+g-variant-type-byte+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_INT16
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-int16+ "n")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-int16+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-int16+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"n\"}
  The type of an integer value that can range from -32768 to 32767.")

(export '+g-variant-type-int16+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UINT16
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-uint16+ "q")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-uint16+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-uint16+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"q\"}
  The type of an integer value that can range from 0 to 65535. There were
  about this many people living in Toronto in the 1870s.")

(export '+g-variant-type-uint16+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_INT32
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-int32+ "i")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-int32+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-int32+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"i\"}
  The type of an integer value that can range from -2147483648 to 2147483647.")

(export '+g-variant-type-int32+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UINT32
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-uint32+ "u")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-uint32+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-uint32+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"u\"}
  The type of an integer value that can range from 0 to 4294967295. That is one
  number for everyone who was around in the late 1970s.")

(export '+g-variant-type-uint32+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_INT64
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-int64+ "x")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-int64+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-int64+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"x\"}
  The type of an integer value that can range from -9223372036854775808 to
  9223372036854775807.")

(export '+g-variant-type-int64+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UINT64
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-uint64+ "t")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-uint64+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-uint64+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"t\"}
  The type of an integer value that can range from 0 to 18446744073709551616.
  That is a really big number, but a Rubik's cube can have a bit more than
  twice as many possible positions.")

(export '+g-variant-type-uint64+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_HANDLE
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-handle+ "h")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-handle+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-handle+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"h\"}
  @begin{short}
    The type of a 32 bit signed integer value, that by convention, is used as
    an index into an array of file descriptors that are sent alongside a D-Bus
    message.
  @end{short}

  If you are not interacting with D-Bus, then there is no reason to make use
  of this type.")

(export '+g-variant-type-handle+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_DOUBLE
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-double+ "d")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-double+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-double+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"d\"}
  The type of a double precision IEEE754 floating point number. These guys go
  up to about 1.80e308 (plus and minus) but miss out on some numbers in
  between. In any case, that is far greater than the estimated number of
  fundamental particles in the observable universe.")

(export '+g-variant-type-double+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_STRING
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-string+ "s")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-string+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-string+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"s\"}
  The type of a string. \"\" is a string. @code{NULL} is not a string.")

(export '+g-variant-type-string+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_OBJECT_PATH
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-object-path+ "o")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-object-path+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-object-path+ 'variable)
 "@version{2013-4-4}
  @variable-value{\"o\"}
  @begin{short}
    The type of a D-Bus object reference. These are strings of a specific
    format used to identify objects at a given destination on the bus.
  @end{short}

  If you are not interacting with D-Bus, then there is no reason to make use
  of this type. If you are, then the D-Bus specification contains a precise
  description of valid object paths.")

(export '+g-variant-type-object-path+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_SIGNATURE
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-signature+ "g")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-signature+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-signature+ 'variable)
 "@version{2013-2-17}
  @variable-value{\"g\"}
  @begin{short}
    The type of a D-Bus type signature. These are strings of a specific format
     used as type signatures for D-Bus methods and messages.
  @end{short}

  If you are not interacting with D-Bus, then there is no reason to make use
  of this type. If you are, then the D-Bus specification contains a precise
  description of valid signature strings.")

(export '+g-variant-type-signature+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_VARIANT
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-variant+ "v")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-variant+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-variant+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"v\"}
  The type of a box that contains any other value (including another variant).")

(export '+g-variant-type-variant+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_ANY
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-any+ "*")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-any+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-any+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"*\"}
  An indefinite type that is a supertype of every type (including itself).")

(export '+g-variant-type-any+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BASIC
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-basic+ "?")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-basic+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-basic+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"?\"}
  An indefinite type that is a supertype of every basic (i. e.: non-container)
  type.")

(export '+g-variant-type-basic+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_MAYBE
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-maybe+ "m*")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-maybe+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-maybe+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"m*\"}
  An indefinite type that is a supertype of every maybe type.")

(export '+g-variant-type-maybe+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_ARRAY
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-array+ "a*")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-array+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-array+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"a*\"}
  An indefinite type that is a supertype of every array type.")

(export '+g-variant-type-array+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_TUPLE
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-tuple+ "r")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-tuple+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-tuple+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"r\"}
  An indefinite type that is a supertype of every tuple type, regardless of
  the number of items in the tuple.")

(export '+g-variant-type-tuple+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UNIT
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-unit+ "()")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-unit+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-unit+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"()\"}
  The empty tuple type. Has only one instance. Known also as \"triv\" or
  \"void\".")

(export '+g-variant-type-unit+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_DICT_ENTRY
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-dict-entry+ "{?*}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-dict-entry+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-dict-entry+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"{?*@}\"}
  An indefinite type that is a supertype of every dictionary entry type.")

(export '+g-variant-type-dict-entry+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_DICTIONARY
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-dictionary+ "a{?*}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-dictionary+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-dictionary+ 'variable)
 "@version{2013-6-29}
  @variable-value{\"a{?*@}\"}
  An indefinite type that is a supertype of every dictionary type. That is,
  any array type that has an element type equal to any dictionary entry type.")

(export '+g-variant-type-dictionary+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_STRING_ARRAY
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-string-array+ "as")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-string-array+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-string-array+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"as\"}
  The type of an array of strings.")

(export '+g-variant-type-string-array+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_OBJECT_PATH_ARRAY
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-object-path-array+ "ao")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-object-path-array+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-object-path-array+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"ao\"}
  The type of an array of object paths.")

(export '+g-variant-type-object-path-array+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BYTESTRING
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-bytestring+ "ay")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-bytestring+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-bytestring+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"ay\"}
  The type of an array of bytes. This type is commonly used to pass around
  strings that may not be valid utf8. In that case, the convention is that the
  nul terminator character should be included as the last character in the
  array.")

(export '+g-variant-type-bytestring+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BYTESTRING_ARRAY
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-bytestring-array+ "aay")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-bytestring-array+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-bytestring-array+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"aay\"}
  The type of an array of byte strings (an array of arrays of bytes).")

(export '+g-variant-type-bytestring-array+)

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_VARDICT
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-vardict+ "a(sv)")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-vardict+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-vardict+ 'variable)
 "@version{2013-4-9}
  @variable-value{\"a(sv)\"}
  @begin{short}
    The type of a dictionary mapping strings to variants (the ubiquitous
    \"a{sv@}\" type).
  @end{short}

  Since 2.30")

(export '+g-variant-type-vardict+)

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
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_free" g-variant-type-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}, or @code{NULL}}
  @begin{short}
    Frees a @symbol{g-variant-type} that was allocated with
    @fun{g-variant-type-copy}, @fun{g-variant-type-new} or one of the container
    type constructor functions.
  @end{short}

  In the case that type is @code{NULL}, this function does nothing.

  Since 2.24
  @see-function{g-variant-type-copy}
  @see-function{g-variant-type-new}"
  (type (gobject::g-boxed-foreign g-variant-type)))

(export 'g-variant-type-free)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_copy" g-variant-type-copy)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{A new @symbol{g-variant-type}.}
  @begin{short}
    Makes a copy of a @symbol{g-variant-type}. It is appropriate to call
    @fun{g-variant-type-free} on the return value. @arg{type} may not be
    @code{NULL}.
  @end{short}

  Since 2.24
  @see-function{g-variant-type-free}"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-copy)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_new" g-variant-type-new)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type-string]{a valid @symbol{g-variant} type string}
  @return{A new @symbol{g-variant-type}.}
  @begin{short}
    Creates a new @symbol{g-variant-type} corresponding to the type string
    given by @arg{type-string}.
  @end{short}
  It is appropriate to call @fun{g-variant-type-free} on the return value.

  It is a programmer error to call this function with an invalid type string.
  Use @fun{g-variant-type-string-is-valid} if you are unsure.

  Since 2.24
  @see-function{g-variant-type-free}
  @see-function{g-variant-type-string-is-valid}"
  (type-string :string))

(export 'g-variant-type-new)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_string_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_string_is_valid" g-variant-type-string-is-valid)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type-string]{a pointer to any string}
  @return{@em{True} if @arg{type-string} is exactly one valid type string.}
  @begin{short}
    Checks if @arg{type-string} is a valid @symbol{g-variant} type string.
  @end{short}
  This call is equivalent to calling @fun{g-variant-type-string-scan} and
  confirming that the following character is a nul terminator.

  Since 2.24
  @see-function{g-variant-type-string-scan}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_get_string_length" g-variant-type-get-string-length)
    g-size
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{The length of the corresponding type string.}
  @begin{short}
    Returns the length of the type string corresponding to the given type.
  @end{short}
  This function must be used to determine the valid extent of the memory region
  returned by @fun{g-variant-type-peek-string}.

  Since 2.24
  @see-function{g-variant-type-peek-string}"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-get-string-length)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_peek_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_peek_string" g-variant-type-peek-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{The corresponding type string (not nul-terminated)}
  @begin{short}
    Returns the type string corresponding to the given @arg{type}.
  @end{short}
  The result is not nul-terminated; in order to determine its length you must
  call @fun{g-variant-type-get-string-length}.

  To get a nul-terminated string, see @fun{g-variant-type-dup-string}.

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-peek-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_dup_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_dup_string" g-variant-type-dup-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{The corresponding type string.}
  @begin{short}
    Returns a newly allocated copy of the type string corresponding to type.
  @end{short}
  The returned string is nul-terminated. It is appropriate to call @fun{g-free}
  on the return value.

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-dup-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_definite ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_definite" g-variant-type-is-definite) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{@em{True} if type is definite.}
  @begin{short}
    Determines if the given type is definite (i. e.: not indefinite).
  @end{short}

  A type is definite if its type string does not contain any indefinite type
  characters ('*', '?', or 'r').

  A @symbol{g-variant} instance may not have an indefinite type, so calling this
  function on the result of @fun{g-variant-get-type} will always result in
  @em{true} being returned. Calling this function on an indefinite type like
  @var{+g-variant-type-array+}, however, will result in @code{nil} being
  returned.

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-definite)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_container ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_container" g-variant-type-is-container) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{@em{True} if type is a container type.}
  @begin{short}
    Determines if the given type is a container type.
  @end{short}

  Container types are any array, maybe, tuple, or dictionary entry types plus
  the variant type.

  This function returns @em{true} for any indefinite type for which every
  definite subtype is a container -- @var{+g-variant-type-array+}, for example.

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-container)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_basic ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_basic" g-variant-type-is-basic) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{@em{True} if type is a basic type.}
  @begin{short}
    Determines if the given type is a basic type.
  @end{short}

  Basic types are booleans, bytes, integers, doubles, strings, object paths
  and signatures.

  Only a basic type may be used as the key of a dictionary entry.

  This function returns @code{nil} for all indefinite types except
  @var{+g-variant-type-basic+}.

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-basic)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_maybe ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_maybe" g-variant-type-is-maybe) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{@em{True} if type is a maybe type.}
  @begin{short}
    Determines if the given type is a maybe type. This is true if the type
    string for type starts with an 'm'.
  @end{short}

  This function returns @em{true} for any indefinite type for which every
  definite subtype is a maybe type -- @var{+g-variant-type-maybe+}, for example.

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-maybe)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_array ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_array" g-variant-type-is-array) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{@em{True} if type is an array type.}
  @begin{short}
    Determines if the given type is an array type. This is true if the type
    string for type starts with an 'a'.
  @end{short}

  This function returns @em{true} for any indefinite type for which every
  definite subtype is an array type -- @var{+g-variant-type-array+}, for
  example.

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-array)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_tuple ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_tuple" g-variant-type-is-tuple) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{@em{True} if type is a tuple type.}
  @begin{short}
    Determines if the given type is a tuple type. This is true if the type
    string for type starts with a '(' or if type is
    @var{+g-variant-type-tuple+}.
  @end{short}

  This function returns @em{true} for any indefinite type for which every
  definite subtype is a tuple type -- @var{+g-variant-type-tuple+}, for example.

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-tuple)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_dict_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_dict_entry" g-variant-type-is-dict-entry) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{@em{True} if type is a dictionary entry type.}
  @begin{short}
    Determines if the given type is a dictionary entry type. This is true if the
    type string for type starts with a '{'.
  @end{short}

  This function returns @em{true} for any indefinite type for which every
  definite subtype is a dictionary entry type --
  @var{+g-variant-type-dict-entry+}, for example.

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-dict-entry)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_variant ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_variant" g-variant-type-is-variant) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[type]{a @symbol{g-variant-type}}
  @return{@em{True} if type is the variant type.}
  @begin{short}
    Determines if the given type is the variant type.
  @end{short}

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-variant)

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
;;; ----------------------------------------------------------------------------


(defcfun ("g_variant_type_equal" g-variant-type-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-2}
  @argument[type1]{a @symbol{g-variant-type} structure}
  @argument[type2]{a @symbol{g-variant-type} structure}
  @return{@em{True} if @arg{type1} and @arg{type2} are exactly equal.}
  @begin{short}
    Compares @arg{type1} and @arg{type2} for equality.
  @end{short}

  Only returns @em{true} if the types are exactly equal. Even if one type is an
  indefinite type and the other is a subtype of it, @code{nil} will be returned
  if they are not exactly equal. If you want to check for subtypes, use the
  function @fun{g-variant-type-is-subtype-of}.

  The argument types of @arg{type1} and @arg{type2} are only in the C
  implementation @code{gconstpointer} to allow use with @code{GHashTable}
  without function pointer casting. For both arguments, a valid
  @symbol{g-variant-type} must be provided.

  Since 2.24
  @see-function{g-variant-type-is-subtype-of}"
  (type1 (gobject:g-boxed-foreign g-variant-type))
  (type2 (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-equal)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_subtype_of ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_subtype_of" g-variant-type-is-subtype-of) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-2}
  @argument[type]{a @symbol{g-variant-type} structure}
  @argument[supertype]{a @symbol{g-variant-type} structure}
  @return{@em{True} if @arg{type} is a subtype of @arg{supertype}.}
  @begin{short}
    Checks if @arg{type} is a subtype of @arg{supertype}.
  @end{short}

  This function returns @em{true} if @arg{type} is a subtype of @arg{supertype}.
  All types are considered to be subtypes of themselves. Aside from that, only
  indefinite types can have subtypes.

  Since 2.24"
  (type (gobject:g-boxed-foreign g-variant-type))
  (supertype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-subtype-of)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_maybe ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_new_maybe" g-variant-type-new-maybe)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-2}
  @argument[element]{a @symbol{g-variant-type} structure}
  @return{A new maybe @symbol{g-variant-type} structure.}
  @begin{short}
    Constructs the type corresponding to a maybe instance containing type
    @arg{type} or nothing.
  @end{short}

  It is appropriate to call @fun{g-variant-type-free} on the return value.

  Since 2.24
  @see-function{g-variant-type-free}"
  (element (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-new-maybe)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_array ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_new_array" g-variant-type-new-array)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-2}
  @argument[element]{a @symbol{g-variant-type} structure}
  @return{A new array @symbol{g-variant-type} structure.}
  @begin{short}
    Constructs the type corresponding to an array of elements of the type
    @arg{type}.
  @end{short}

  It is appropriate to call the function @fun{g-variant-type-free} on the
  return value.

  Since 2.24
  @see-function{g-variant-type-free}"
  (element (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-new-array)

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

#|
(defcfun ("g_variant_type_new_tuple" %g-variant-type-new-tuple) g-variant-type
  (items (:pointer g-variant-type))
  (length :int))

;; This code is an example for passing an array to a C function.

(defun g-variant-type-new-tuple (items)
  (let ((n (length items)))
    (with-foreign-object (array 'g-variant-type (+ n 1))
      (loop for i from 0 and item in items
         do (setf (mem-aref array 'g-variant-type i) item))
      ;; The last element of array is a null-pointer
      (setf (mem-aref array 'g-variant-type n) (null-pointer))
      (%g-variant-type-new-tuple array -1))))

(export 'g-variant-type-new-tuple)
|#

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
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_n_items" g-variant-type-n-items) g-size
 #+cl-cffi-gtk-documentation
 "@version{2013-6-16}
  @argument[type]{a tuple or dictionary entry @class{g-variant-type}}
  @return{The number of items in @arg{type}.}
  @begin{short}
    Determines the number of items contained in a tuple or dictionary entry
    @arg{type}.
  @end{short}

  This function may only be used with tuple or dictionary entry types, but
  must not be used with the generic tuple type @var{+g-variant-type-tuple+}.

  In the case of a dictionary entry type, this function will always return 2.

  Since 2.24
  @see-variable{+g-variant-type-tuple+}"
  (type (gobject::g-boxed-foreign g-variant-type)))

(export 'g-variant-type-n-items)

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
