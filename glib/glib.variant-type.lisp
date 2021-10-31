;;; ----------------------------------------------------------------------------
;;; glib.variant-type.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.68 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;;     Introduction to the GVariant type system
;;;
;;; Types and Values
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
;;; Functions
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

;; Call the type intializer at this point.

(glib-init::at-init () (foreign-funcall "g_variant_type_get_gtype" g-size))

;; No g-variant-type form the Lisp side, we return a NULL-Pointer
(gobject::define-g-boxed-opaque g-variant-type "GVariantType"
  :alloc (error "GVariantType cannot be created from the Lisp side."))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-variant-type atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'g-variant-type 'type)
 "@version{2021-7-31}
  @begin{short}
    The GVariant type system is based, in large part, on the D-Bus type system,
    with two major changes and some minor lifting of restrictions.
  @end{short}
  The D-Bus specification, therefore, provides a significant amount of
  information that is useful when working with @type{g-variant} values.

  The first major change with respect to the D-Bus type system is the
  introduction of maybe (or \"nullable\") types. Any type in the GVariant type
  system can be converted to a maybe type, in which case, \"nothing\" (or
  \"null\") becomes a valid value. Maybe types have been added by introducing
  the character \"m\" to type strings.

  The second major change is that the GVariant type system supports the concept
  of \"indefinite types\" - types that are less specific than the normal types
  found in D-Bus. For example, it is possible to speak of \"an array of any
  type\" in the GVariant type system, where the D-Bus type system would require
  you to speak of \"an array of integers\" or \"an array of strings\".
  Indefinite types have been added by introducing the characters \"*\", \"?\"
  and \"r\" to type strings.

  Finally, all arbitrary restrictions relating to the complexity of types are
  lifted along with the restriction that dictionary entries may only appear
  nested inside of arrays.

  Just as in D-Bus, GVariant types are described with type strings. Subject to
  the differences mentioned above, these strings are of the same form as those
  found in D-Bus. Note, however: D-Bus always works in terms of messages and
  therefore individual type strings appear nowhere in its interface. Instead,
  \"signatures\" are a concatenation of the strings of the type of each argument
  in a message. The GVariant type system deals with single values directly so
  GVariant type strings always describe the type of exactly one value. This
  means that a D-Bus signature string is generally not a valid GVariant type
  string - except in the case that it is the signature of a message containing
  exactly one argument.

  An indefinite type is similar in spirit to what may be called an abstract
  type in other type systems. No value can exist that has an indefinite type
  as its type, but values can exist that have types that are subtypes of
  indefinite types. That is to say, the function @fun{g-variant-type} will never
  return an indefinite type, but calling the function @fun{g-variant-is-of-type}
  with an indefinite type may return @em{true}. For example, you cannot have a
  value that represents \"an array of no particular type\", but you can have an
  \"array of integers\" which certainly matches the type of \"an array of no
  particular type\", since \"array of integers\" is a subtype of \"array of no
  particular type\".

  This is similar to how instances of abstract classes may not directly exist
  in other type systems, but instances of their non-abstract subtypes may. For
  example, in GTK, no object that has the type of @class{gtk-bin} can exist,
  since @class{gtk-bin} is an abstract class, but a @class{gtk-window} can
  certainly be instantiated, and you would say that the @class{gtk-window} is a
  @class{gtk-bin}, since @class{gtk-window} is a subclass of @class{gtk-bin}.

  A detailed description of GVariant type strings is given here:

  @subheading{GVariant Type Strings}
  A GVariant type string can be any of the following:
  @begin{itemize}
    @item{any basic type string (listed below)}
    @item{\"v\", \"r\" or \"*\"}
    @item{one of the characters 'a' or 'm', followed by another type string}
    @item{the character '(', followed by a concatenation of zero or more other
      type strings, followed by the character ')'}
    @item{the character '{', followed by a basic type string (see below),
      followed by another type string, followed by the character '@}'}
  @end{itemize}
  A basic type string describes a basic type as per the function
  @fun{g-variant-type-is-basic} and is always a single character in length.
  The valid basic type strings are \"b\", \"y\", \"n\", \"q\", \"i\", \"u\",
  \"x\", \"t\", \"h\", \"d\", \"s\", \"o\", \"g\" and \"?\".

  The above definition is recursive to arbitrary depth. \"aaaaai\" and
  \"(ui(nq((y)))s)\" are both valid type strings, as is
  \"a(aa(ui)(qna{ya(yd)@}))\".

  The meaning of each of the characters is as follows:
  @begin[code]{table}
     @entry[b]{The type string of a boolean value.}
     @entry[y]{The type string of a byte.}
     @entry[n]{The type string of a signed 16 bit integer.}
     @entry[q]{The type string of an unsigned 16 bit integer.}
     @entry[i]{The type string of a signed 32 bit integer.}
     @entry[u]{The type string of an unsigned 32 bit integer.}
     @entry[x]{The type string of a signed 64 bit integer.}
     @entry[t]{The type string of an unsigned 64 bit integer.}
     @entry[h]{The type string of a signed 32 bit value that, by convention,
       is used as an index into an array of file descriptors that are sent
       alongside a D-Bus message.}
     @entry[d]{The type string of a double precision floating point value.}
     @entry[s]{The type string of a string.}
     @entry[o]{The type string of a string in the form of a D-Bus object path.}
     @entry[g]{The type string of a string in the form of a D-Bus type
       signature.}
     @entry[?]{The type string of an indefinite type that is a supertype of any
       of the basic types.}
     @entry[v]{The type string of a container type that contain any other type
       of value.}
     @entry[a]{Used as a prefix on another type string to mean an array of
       that type. The type string \"ai\", for example, is the type of an array
       of 32 bit signed integers.}
     @entry[m]{Used as a prefix on another type string to mean a \"maybe\", or
       \"nullable\", version of that type. The type string \"ms\", for
       example, is the type of a value that maybe contains a string, or maybe
       contains nothing.}
     @entry[()]{Used to enclose zero or more other concatenated type strings
       to create a tuple type. The type string \"(is)\", for example, is the
       type of a pair of an integer and a string.}
     @entry[r]{The type string of an indefinite type that is a supertype of any
       tuple type, regardless of the number of items.}
     @entry[{}]{Used to enclose a basic type string concatenated with another
       type string to create a dictionary entry type, which usually appears
       inside of an array to form a dictionary. The type string \"a{sd@}\",
       for example, is the type of a dictionary that maps strings to double
       precision floating point values. The first type (the basic type) is the
       key type and the second type is the value type. The reason that the
       first type is restricted to being a basic type is so that it can easily
       be hashed.}
     @entry[*]{The type string of the indefinite type that is a supertype of all
       types. Note that, as with all type strings, this character represents
       exactly one type. It cannot be used inside of tuples to mean \"any number
       of items\".}
  @end{table}
  Any type string of a container that contains an indefinite type is, itself,
  an indefinite type. For example, the type string \"a*\" (corresponding to
  an array) is an indefinite type that is a supertype of every array type.
  \"(*s)\" is a supertype of all tuples that contain exactly two items where
  the second item is a string.

  \"a{?*@}\" is an indefinite type that is a supertype of all arrays
  containing dictionary entries where the key is any basic type and the value
  is any type at all. This is, by definition, a dictionary. Note that, due to
  the restriction that the key of a dictionary entry must be a basic type,
  \"{**@}\" is not a valid type string.

  Two types may not be compared by value. Use the functions
  @fun{g-variant-type-equal} or @fun{g-variant-type-is-subtype-of}. May be
  copied using the function @fun{g-variant-type-copy} and freed using the
  function @fun{g-variant-type-free}.
  @see-type{g-variant}
  @see-function{g-variant-type}
  @see-function{g-variant-is-of-type}
  @see-function{g-variant-type-is-basic}
  @see-function{g-variant-type-equal}
  @see-function{g-variant-type-is-subtype-of}
  @see-function{g-variant-type-copy}
  @see-function{g-variant-type-free}")

(export (gobject::boxed-related-symbols 'g-variant-type))

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BOOLEAN                                 not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-boolean+ "b")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-boolean+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-boolean+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"b\"}
  @begin{short}
    The type of a value that can be either @em{true} or @code{nil}.
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BYTE                                    not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-byte+ "y")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-byte+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-byte+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"y\"}
  @short{The type of an integer value that can range from 0 to 255.}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_INT16                                   not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-int16+ "n")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-int16+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-int16+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"n\"}
  @short{The type of an integer value that can range from -32768 to 32767.}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UINT16                                  not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-uint16+ "q")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-uint16+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-uint16+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"q\"}
  @short{The type of an integer value that can range from 0 to 65535.}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_INT32                                   not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-int32+ "i")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-int32+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-int32+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"i\"}
  @begin{short}
    The type of an integer value that can range from -2147483648 to 2147483647.
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UINT32                                  not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-uint32+ "u")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-uint32+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-uint32+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"u\"}
  @short{The type of an integer value that can range from 0 to 4294967295.}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_INT64                                   not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-int64+ "x")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-int64+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-int64+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"x\"}
  @begin{short}
    The type of an integer value that can range from -9223372036854775808 to
    9223372036854775807.
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UINT64                                  not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-uint64+ "t")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-uint64+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-uint64+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"t\"}
  @begin{short}
    The type of an integer value that can range from 0 to 18446744073709551616.
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_HANDLE                                  not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-handle+ "h")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-handle+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-handle+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"h\"}
  @begin{short}
    The type of a 32 bit signed integer value, that by convention, is used as
    an index into an array of file descriptors that are sent alongside a D-Bus
    message.
  @end{short}
  If you are not interacting with D-Bus, then there is no reason to make use
  of this type.
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_DOUBLE                                  not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-double+ "d")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-double+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-double+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"d\"}
  @short{The type of a double precision IEEE754 floating point number.}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_STRING                                  not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-string+ "s")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-string+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-string+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"s\"}
  @short{The type of a string.}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_OBJECT_PATH                             not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-object-path+ "o")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-object-path+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-object-path+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"o\"}
  @begin{short}
    The type of a D-Bus object reference.
  @end{short}
  These are strings of a specific format used to identify objects at a given
  destination on the bus. If you are not interacting with D-Bus, then there is
  no reason to make use of this type. If you are, then the D-Bus specification
  contains a precise description of valid object paths.
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_SIGNATURE                               not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-signature+ "g")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-signature+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-signature+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"g\"}
  @begin{short}
    The type of a D-Bus type signature.
  @end{short}
  These are strings of a specific format used as type signatures for D-Bus
  methods and messages. If you are not interacting with D-Bus, then there is no
  reason to make use of this type. If you are, then the D-Bus specification
  contains a precise description of valid signature strings.
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_VARIANT                                 not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-variant+ "v")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-variant+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-variant+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"v\"}
  @begin{short}
    The type of a box that contains any other value (including another
    variant).
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_ANY                                     not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-any+ "*")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-any+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-any+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"*\"}
  @begin{short}
    An indefinite type that is a supertype of every type (including itself).
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BASIC                                   not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-basic+ "?")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-basic+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-basic+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"?\"}
  @begin{short}
    An indefinite type that is a supertype of every basic (i.e. non-container)
    type.
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_MAYBE                                   not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-maybe+ "m*")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-maybe+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-maybe+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"m*\"}
  @short{An indefinite type that is a supertype of every maybe type.}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_ARRAY                                   not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-array+ "a*")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-array+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-array+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"a*\"}
  @short{An indefinite type that is a supertype of every array type.}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_TUPLE                                   not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-tuple+ "r")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-tuple+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-tuple+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"r\"}
  @begin{short}
    An indefinite type that is a supertype of every tuple type, regardless of
    the number of items in the tuple.
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_UNIT                                    not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-unit+ "()")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-unit+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-unit+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"()\"}
  @begin{short}
    The empty tuple type. Has only one instance. Known also as \"triv\" or
    \"void\".
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_DICT_ENTRY                              not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-dict-entry+ "{?*}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-dict-entry+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-dict-entry+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"{?*@}\"}
  @begin{short}
    An indefinite type that is a supertype of every dictionary entry type.
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_DICTIONARY                              not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-dictionary+ "a{?*}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-dictionary+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-dictionary+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"a{?*@}\"}
  @begin{short}
    An indefinite type that is a supertype of every dictionary type.
  @end{short}
  That is, any array type that has an element type equal to any dictionary
  entry type.
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_STRING_ARRAY                            not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-string-array+ "as")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-string-array+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-string-array+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"as\"}
  @short{The type of an array of strings.}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_OBJECT_PATH_ARRAY                       not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-object-path-array+ "ao")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-object-path-array+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-object-path-array+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"ao\"}
  @short{The type of an array of object paths.}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BYTESTRING                              not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-bytestring+ "ay")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-bytestring+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-bytestring+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"ay\"}
  @begin{short}
    The type of an array of bytes.
  @end{short}
  This type is commonly used to pass around strings that may not be valid UTF-8.
  In that case, the convention is that the nul terminator character should be
  included as the last character in the array.
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_BYTESTRING_ARRAY                        not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-bytestring-array+ "aay")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-bytestring-array+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-bytestring-array+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"aay\"}
  @short{The type of an array of byte strings (an array of arrays of bytes).}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE_VARDICT                                 not exported
;;; ----------------------------------------------------------------------------

(defparameter +g-variant-type-vardict+ "a(sv)")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-variant-type-vardict+ atdoc:*variable-name-alias*)
      "Constant"
      (documentation '+g-variant-type-vardict+ 'variable)
 "@version{2020-11-29}
  @variable-value{\"a(sv)\"}
  @begin{short}
    The type of a dictionary mapping strings to variants (the ubiquitous
    \"a{sv@}\" type).
  @end{short}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; G_VARIANT_TYPE -> g-variant-type-checked               not exported
;;; ----------------------------------------------------------------------------

;; We call the private C function g_variant_type_checked_ to define a Lisp
;; function which returns a GVariantType from a valid type string.

(defcfun ("g_variant_type_checked_" g-variant-type-checked)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[string]{a well-formed @class{g-variant-type} type string}
  @return{A @class{g-variant-type} instance.}
  @begin{short}
    Converts a string to a @class{g-variant-type} instance.
  @end{short}
  Depending on the current debugging level, this function may perform a runtime
  check to ensure that the type string is a valid GVariant type string.

  It is always a programmer error to use this function with an invalid type
  string. If in doubt, use the function @fun{g-variant-type-string-is-valid} to
  check if the string is valid.
  @see-class{g-variant-type}
  @see-function{g-variant-type-string-is-valid}"
  (string :string))

;;; ----------------------------------------------------------------------------
;;; g_variant_type_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_free" g-variant-type-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @begin{short}
    Frees a @class{g-variant-type} instance that was allocated with the
    functions @fun{g-variant-type-copy}, @fun{g-variant-type-new} or one of the
    container type constructor functions.
  @end{short}
  @see-class{g-variant-type}
  @see-function{g-variant-type-copy}
  @see-function{g-variant-type-new}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-free)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_copy" g-variant-type-copy)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{A new @class{g-variant-type} instance.}
  @begin{short}
    Makes a copy of a @class{g-variant-type} instance.
  @end{short}
  It is appropriate to call the function @fun{g-variant-type-free} on the
  return value.
  @see-class{g-variant-type}
  @see-function{g-variant-type-free}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-copy)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_new" g-variant-type-new)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{*2021-10-31}
  @argument[string]{a valid @class{g-variant-type} type string}
  @return{A new @class{g-variant-type} instance.}
  @begin{short}
    Creates a new @class{g-variant-type} instance corresponding to the type
    string given by @arg{string}.
  @end{short}
  It is appropriate to call the @fun{g-variant-type-free} function on the
  return value.

  It is a programmer error to call this function with an invalid type string.
  Use the @fun{g-variant-type-string-is-valid} function if you are unsure.
  @see-class{g-variant-type}
  @see-function{g-variant-type-free}
  @see-function{g-variant-type-string-is-valid}"
  (string :string))

(export 'g-variant-type-new)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_string_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_string_is_valid" g-variant-type-string-is-valid)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[string]{a @class{g-variant-type} type string}
  @return{@em{True} if @arg{string} is exactly one valid type string.}
  @begin{short}
    Checks if @arg{string} is a valid @class{g-variant-type} type string.
  @end{short}
  @see-class{g-variant-type}"
  (string :string))

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

;; not implemented, is equivalent to g-variant-type-string-is-valid

;;; ----------------------------------------------------------------------------
;;; g_variant_type_get_string_length ()
;;; -> g-variant-type-string-length                        not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_get_string_length" g-variant-type-string-length)
    g-size
 #+cl-cffi-gtk-documentation
 "@version{2020-11-29}
  @argument[variant-type]{a @class{g-variant-type} instance}
  @return{The length of the corresponding type string.}
  @begin{short}
    Returns the length of the type string corresponding to the given type.
  @end{short}
  This function must be used to determine the valid extent of the memory region
  returned by the function @fun{g-variant-type-peek-string}.
  @see-class{g-variant-type}
  @see-function{g-variant-type-peek-string}"
  (variant-type (gobject:g-boxed-foreign g-variant-type)))

;;; ----------------------------------------------------------------------------
;;; g_variant_type_peek_string ()                          not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_peek_string" g-variant-type-peek-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-11-29}
  @argument[variant-type]{a @class{g-variant-type} instance}
  @return{The corresponding type string.}
  @begin{short}
    Returns the type string corresponding to the given @arg{variant-type}.
  @end{short}
  The result is not nul-terminated. In order to determine its length you must
  call the function @fun{g-variant-type-string-length}.

  To get a nul-terminated string, see the function
  @fun{g-variant-type-dup-string}.
  @see-class{g-variant-type}"
  (variant-type (gobject:g-boxed-foreign g-variant-type)))

;;; ----------------------------------------------------------------------------
;;; g_variant_type_dup_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_dup_string" g-variant-type-dup-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{The corresponding type string.}
  @begin{short}
    Returns a newly allocated copy of the type string corresponding to
    @arg{vtype}.
  @end{short}
  The returned string is nul-terminated. It is appropriate to call the function
  @fun{g-variant-type-free} on the return value.
  @see-class{g-variant-type}
  @see-function{g-variant-type-free}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-dup-string)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_definite ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_definite" g-variant-type-is-definite) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{@em{True} if @arg{vtype} is definite.}
  @begin{short}
    Determines if the given variant type is definite, i.e. not indefinite.
  @end{short}
  A type is definite if its type string does not contain any indefinite type
  characters ('*', '?', or 'r').

  A @class{g-variant-type} instance may not have an indefinite type, so calling
  this function on the result of the function @fun{g-variant-type-new} will
  always result in @em{true} being returned. Calling this function on an
  indefinite type like \"a*\", however, will result in @em{false} being
  returned.
  @see-class{g-variant-type}
  @see-function{g-variant-type-new}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-definite)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_container ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_container" g-variant-type-is-container) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[type]{a @class{g-variant-type} instance}
  @return{@em{True} if @arg{vtype} is a container type.}
  @begin{short}
    Determines if the given variant type is a container type.
  @end{short}
  Container types are any array, maybe, tuple, or dictionary entry types plus
  the variant type.

  This function returns @em{true} for any indefinite type for which every
  definite subtype is a container - \"a*\", for example.
  @see-class{g-variant-type}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-container)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_basic ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_basic" g-variant-type-is-basic) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{@em{True} if @arg{vtype} is a basic type.}
  @begin{short}
    Determines if the given variant type is a basic type.
  @end{short}
  Basic types are booleans, bytes, integers, doubles, strings, object paths
  and signatures. Only a basic type may be used as the key of a dictionary
  entry.

  This function returns @em{false} for all indefinite types except \"?\".
  @see-class{g-variant-type}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-basic)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_maybe ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_maybe" g-variant-type-is-maybe) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{@em{True} if @arg{vtype} is a maybe type.}
  @begin{short}
    Determines if the given variant type is a maybe type.
  @end{short}
  This is @em{true} if the type string for @arg{vtype} starts with an 'm'.

  This function returns @em{true} for any indefinite type for which every
  definite subtype is a maybe type - \"m*\", for example.
  @see-class{g-variant-type}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-maybe)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_array ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_array" g-variant-type-is-array) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{@em{True} if @arg{vtype} is an array type.}
  @begin{short}
    Determines if the given variant type is an array type.
  @end{short}
  This is @em{true} if the variant type string for type starts with an 'a'.

  This function returns @em{true} for any indefinite type for which every
  definite subtype is an array type - \"a*\", for example.
  @see-class{g-variant-type}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-array)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_tuple ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_tuple" g-variant-type-is-tuple) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{@em{True} if @arg{vtype} is a tuple type.}
  @begin{short}
    Determines if the given type is a tuple type.
  @end{short}
  This is @em{true} if the variant type string for type starts with a '(' or if
  type is \"r\".

  This function returns @em{true} for any indefinite type for which every
  definite subtype is a tuple type - \"r\", for example.
  @see-class{g-variant-type}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-tuple)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_dict_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_dict_entry" g-variant-type-is-dict-entry) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{@em{True} if @arg{vtype} is a dictionary entry type.}
  @begin{short}
    Determines if the given variant type is a dictionary entry type.
  @end{short}
  This is @em{true} if the type string for type starts with a '{'.

  This function returns @em{true} for any indefinite type for which every
  definite subtype is a dictionary entry type - \"{?*@}\", for example.
  @see-class{g-variant-type}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-dict-entry)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_variant ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_variant" g-variant-type-is-variant) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{@em{True} if @arg{vtype} is the variant type.}
  @begin{short}
    Determines if the given variant type is the variant type.
  @end{short}
  @see-class{g-variant-type}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-variant)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_hash ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_hash" g-variant-type-hash) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{An unsigned integer with the hash value.}
  @begin{short}
    The has value of the given variant type.
  @end{short}
  A valid @class{g-variant-type} instance must be provided.
  @see-class{g-variant-type}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-hash)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_equal" g-variant-type-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype1]{a @class{g-variant-type} instance}
  @argument[vtype2]{a @class{g-variant-type} instance}
  @begin{return}
    @em{True} if @arg{vtype1} and @arg{vtype2} are exactly equal.
  @end{return}
  @begin{short}
    Compares two variant types for equality.
  @end{short}
  Only returns @em{true} if the types are exactly equal. Even if one type is an
  indefinite type and the other is a subtype of it, @em{false} will be returned
  if they are not exactly equal. If you want to check for subtypes, use the
  function @fun{g-variant-type-is-subtype-of}.

  For both arguments, a valid @class{g-variant-type} instance must be provided.
  @see-class{g-variant-type}
  @see-function{g-variant-type-is-subtype-of}"
  (vtype1 (gobject:g-boxed-foreign g-variant-type))
  (vtype2 (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-equal)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_is_subtype_of ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_is_subtype_of" g-variant-type-is-subtype-of) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @argument[supertype]{a @class{g-variant-type} instance}
  @return{@em{True} if @arg{vtype} is a subtype of @arg{supertype}.}
  @begin{short}
    Checks if @arg{vtype} is a subtype of @arg{supertype}.
  @end{short}

  This function returns @em{true} if @arg{vtype} is a subtype of
  @arg{supertype}. All types are considered to be subtypes of themselves. Aside
  from that, only indefinite types can have subtypes.
  @see-class{g-variant-type}"
  (vtype (gobject:g-boxed-foreign g-variant-type))
  (supertype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-is-subtype-of)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_maybe ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_new_maybe" g-variant-type-new-maybe)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{A new maybe @class{g-variant-type} instance.}
  @begin{short}
    Constructs the type corresponding to a maybe instance containing type
    @arg{vtype} or nothing.
  @end{short}

  It is appropriate to call the function @fun{g-variant-type-free} on the
  return value.
  @see-class{g-variant-type}
  @see-function{g-variant-type-free}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-new-maybe)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_array ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_new_array" g-variant-type-new-array)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance}
  @return{A new array @class{g-variant-type} instance.}
  @begin{short}
    Constructs the type corresponding to an array of elements of the type
    @arg{vtype}.
  @end{short}

  It is appropriate to call the function @fun{g-variant-type-free} on the
  return value.
  @see-class{g-variant-type}
  @see-function{g-variant-type-free}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-new-array)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_tuple ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_new_tuple" %g-variant-type-new-tuple)
    (gobject:g-boxed-foreign g-variant-type)
  (items :pointer)
  (length :int))

(defun g-variant-type-new-tuple (&rest items)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[items]{a list of @class{g-variant-type} types, one for each item}
  @return{A new tuple @class{g-variant-type} instance.}
  @begin{short}
    Constructs a new tuple type, from @arg{items}.
  @end{short}

  It is appropriate to call the function @fun{g-variant-type-free} on the
  return value.
  @see-class{g-variant-type}
  @see-function{g-variant-type-free}"
  (let ((n-items (length items)))
    (with-foreign-object (items-ar :pointer n-items)
      (iter
        (for i from 0 below n-items)
        (for item in items)
        (setf (mem-aref items-ar :pointer i)
              (convert-to-foreign item
                                  '(gobject:g-boxed-foreign g-variant-type))))
      (%g-variant-type-new-tuple items-ar n-items))))

(export 'g-variant-type-new-tuple)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_new_dict_entry ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_new_dict_entry" g-variant-type-new-dict-entry)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[key]{a basic @class{g-variant-type} instance}
  @argument[value]{a @class{g-variant-type} instance}
  @return{A new dictionary entry @class{g-variant-type} instance.}
  @begin{short}
    Constructs the type corresponding to a dictionary entry with a key of type
    @arg{key} and a value of type @arg{value}.
  @end{short}

  It is appropriate to call the function @fun{g-variant-type-free} on the
  return value.
  @see-class{g-variant-type}
  @see-function{g-variant-type-free}"
  (key (gobject:g-boxed-foreign g-variant-type))
  (value (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-new-dict-entry)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_element ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_element" g-variant-type-element)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{an array or maybe @class{g-variant-type} instance}
  @return{The element type of @arg{vtype}.}
  @begin{short}
    Determines the element type of an array or maybe type.
  @end{short}
  This function may only be used with array or maybe types.
  @see-class{g-variant-type}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-element)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_n_items ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_n_items" g-variant-type-n-items) g-size
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a tuple or dictionary entry @class{g-variant-type}
    instance}
  @return{The number of items in @arg{vtype}.}
  @begin{short}
    Determines the number of items contained in a tuple or dictionary entry
    @arg{vtype}.
  @end{short}

  This function may only be used with tuple or dictionary entry types, but
  must not be used with the generic tuple type \"r\". In the case of a
  dictionary entry type, this function will always return 2.
  @see-class{g-variant-type}"
  (vtype (gobject::g-boxed-foreign g-variant-type)))

(export 'g-variant-type-n-items)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_first ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_first" g-variant-type-first)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a tuple or dictionary entry @class{g-variant-type} instance}
  @return{The first item type of @arg{vtype}.}
  @begin{short}
    Determines the first item type of a tuple or dictionary entry type.
  @end{short}
  This function may only be used with tuple or dictionary entry types, but
  must not be used with the generic tuple type \"r\".

  In the case of a dictionary entry type, this returns the type of the key.
  @code{NULL} is returned in case of type being \"()\".

  This call, together with the function @fun{g-variant-type-next} provides an
  iterator interface over tuple and dictionary entry types.
  @see-class{g-variant-type}
  @see-function{g-variant-type-next}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-first)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_next ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_next" g-variant-type-next)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a @class{g-variant-type} instance from a previous call}
  @return{The next @class{g-variant-type} instance after type, or @code{NULL}.}
  @begin{short}
    Determines the next item type of a tuple or dictionary entry type.
  @end{short}
  The argument @arg{vtype} must be the result of a previous call to the
  functions @fun{g-variant-type-first} or @sym{g-variant-type-next}.

  If called on the key type of a dictionary entry then this call returns the
  value type. If called on the value type of a dictionary entry then this call
  returns @code{NULL}.

  For tuples, @code{NULL} is returned when @arg{vtype} is the last item
  in a tuple.
  @see-class{g-variant-type}
  @see-function{g-variant-type-first}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-next)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_key ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_key" g-variant-type-key)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a dictionary entry @class{g-variant-type} instance}
  @return{The key type of the dictionary entry.}
  @begin{short}
    Determines the key type of a dictionary entry type.
  @end{short}

  This function may only be used with a dictionary entry type. Other than the
  additional restriction, this call is equivalent to the function
  @fun{g-variant-type-first}.
  @see-class{g-variant-type}
  @see-function{g-variant-type-first}
  @see-function{g-variant-type-value}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-key)

;;; ----------------------------------------------------------------------------
;;; g_variant_type_value ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_variant_type_value" g-variant-type-value)
    (gobject:g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-31}
  @argument[vtype]{a dictionary entry @class{g-variant-type} instance}
  @return{The value type of the dictionary entry.}
  @begin{short}
    Determines the value type of a dictionary entry type.
  @end{short}
  This function may only be used with a dictionary entry type.
  @see-class{g-variant-type}
  @see-function{g-variant-type-key}"
  (vtype (gobject:g-boxed-foreign g-variant-type)))

(export 'g-variant-type-value)

;;; --- End of file glib.variant-type.lisp -------------------------------------
