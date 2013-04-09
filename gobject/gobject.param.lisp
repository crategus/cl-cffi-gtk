;;; ----------------------------------------------------------------------------
;;; gobject.param.lisp
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
;;;
;;; Parameters and Values
;;;
;;; Standard Parameter and Value Types
;;;
;;; Synopsis
;;;
;;;     G_IS_PARAM_SPEC_BOOLEAN
;;;     G_PARAM_SPEC_BOOLEAN
;;;     G_VALUE_HOLDS_BOOLEAN
;;;     G_TYPE_PARAM_BOOLEAN
;;;
;;;     GParamSpecBoolean
;;;
;;;     g_param_spec_boolean
;;;     g_value_set_boolean
;;;     g_value_get_boolean
;;;
;;;     G_IS_PARAM_SPEC_CHAR
;;;     G_PARAM_SPEC_CHAR
;;;     G_VALUE_HOLDS_CHAR
;;;     G_TYPE_PARAM_CHAR
;;;
;;;     GParamSpecChar
;;;
;;;     g_param_spec_char
;;;     g_value_set_char
;;;     g_value_get_char
;;;     g_value_get_schar
;;;     g_value_set_schar
;;;
;;;     G_IS_PARAM_SPEC_UCHAR
;;;     G_PARAM_SPEC_UCHAR
;;;     G_VALUE_HOLDS_UCHAR
;;;     G_TYPE_PARAM_UCHAR
;;;
;;;     GParamSpecUChar
;;;
;;;     g_param_spec_uchar
;;;     g_value_set_uchar
;;;     g_value_get_uchar
;;;
;;;     G_IS_PARAM_SPEC_INT
;;;     G_PARAM_SPEC_INT
;;;     G_VALUE_HOLDS_INT
;;;     G_TYPE_PARAM_INT
;;;
;;;     GParamSpecInt
;;;
;;;     g_param_spec_int
;;;     g_value_set_int
;;;     g_value_get_int
;;;
;;;     G_IS_PARAM_SPEC_UINT
;;;     G_PARAM_SPEC_UINT
;;;     G_VALUE_HOLDS_UINT
;;;     G_TYPE_PARAM_UINT
;;;
;;;     GParamSpecUInt
;;;
;;;     g_param_spec_uint
;;;     g_value_set_uint
;;;     g_value_get_uint
;;;
;;;     G_IS_PARAM_SPEC_LONG
;;;     G_PARAM_SPEC_LONG
;;;     G_VALUE_HOLDS_LONG
;;;     G_TYPE_PARAM_LONG
;;;
;;;     GParamSpecLong
;;;
;;;     g_param_spec_long
;;;     g_value_set_long
;;;     g_value_get_long
;;;
;;;     G_IS_PARAM_SPEC_ULONG
;;;     G_PARAM_SPEC_ULONG
;;;     G_VALUE_HOLDS_ULONG
;;;     G_TYPE_PARAM_ULONG
;;;
;;;     GParamSpecULong
;;;
;;;     g_param_spec_ulong
;;;     g_value_set_ulong
;;;     g_value_get_ulong
;;;
;;;     G_IS_PARAM_SPEC_INT64
;;;     G_PARAM_SPEC_INT64
;;;     G_VALUE_HOLDS_INT64
;;;     G_TYPE_PARAM_INT64
;;;
;;;     GParamSpecInt64
;;;
;;;     g_param_spec_int64
;;;     g_value_set_int64
;;;     g_value_get_int64
;;;
;;;     G_IS_PARAM_SPEC_UINT64
;;;     G_PARAM_SPEC_UINT64
;;;     G_VALUE_HOLDS_UINT64
;;;     G_TYPE_PARAM_UINT64
;;;
;;;     GParamSpecUInt64
;;;
;;;     g_param_spec_uint64
;;;     g_value_set_uint64
;;;     g_value_get_uint64
;;;
;;;     G_IS_PARAM_SPEC_FLOAT
;;;     G_PARAM_SPEC_FLOAT
;;;     G_VALUE_HOLDS_FLOAT
;;;     G_TYPE_PARAM_FLOAT
;;;
;;;     GParamSpecFloat
;;;
;;;     g_param_spec_float
;;;     g_value_set_float
;;;     g_value_get_float
;;;
;;;     G_IS_PARAM_SPEC_DOUBLE
;;;     G_PARAM_SPEC_DOUBLE
;;;     G_VALUE_HOLDS_DOUBLE
;;;     G_TYPE_PARAM_DOUBLE
;;;
;;;     GParamSpecDouble
;;;
;;;     g_param_spec_double
;;;     g_value_set_double
;;;     g_value_get_double
;;;
;;;     G_IS_PARAM_SPEC_ENUM
;;;     G_PARAM_SPEC_ENUM
;;;     G_VALUE_HOLDS_ENUM
;;;     G_TYPE_PARAM_ENUM
;;;
;;;     GParamSpecEnum
;;;
;;;     g_param_spec_enum
;;;     g_value_set_enum
;;;     g_value_get_enum
;;;
;;;     G_IS_PARAM_SPEC_FLAGS
;;;     G_PARAM_SPEC_FLAGS
;;;     G_VALUE_HOLDS_FLAGS
;;;     G_TYPE_PARAM_FLAGS
;;;
;;;     GParamSpecFlags
;;;
;;;     g_param_spec_flags
;;;     g_value_set_flags
;;;     g_value_get_flags
;;;
;;;     G_IS_PARAM_SPEC_STRING
;;;     G_PARAM_SPEC_STRING
;;;     G_VALUE_HOLDS_STRING
;;;     G_TYPE_PARAM_STRING
;;;
;;;     GParamSpecString
;;;     gchararray
;;;
;;;     g_param_spec_string
;;;     g_value_set_string
;;;     g_value_set_static_string
;;;     g_value_take_string
;;;     g_value_set_string_take_ownership
;;;     g_value_get_string
;;;     g_value_dup_string
;;;
;;;     G_IS_PARAM_SPEC_PARAM
;;;     G_PARAM_SPEC_PARAM
;;;     G_VALUE_HOLDS_PARAM
;;;     G_TYPE_PARAM_PARAM
;;;
;;;     GParamSpecParam
;;;
;;;     g_param_spec_param
;;;     g_value_set_param
;;;     g_value_take_param
;;;     g_value_set_param_take_ownership
;;;     g_value_get_param
;;;     g_value_dup_param
;;;
;;;     G_IS_PARAM_SPEC_BOXED
;;;     G_PARAM_SPEC_BOXED
;;;     G_VALUE_HOLDS_BOXED
;;;     G_TYPE_PARAM_BOXED
;;;
;;;     GParamSpecBoxed
;;;
;;;     g_param_spec_boxed
;;;     g_value_set_boxed
;;;     g_value_set_static_boxed
;;;     g_value_take_boxed
;;;     g_value_set_boxed_take_ownership
;;;     g_value_get_boxed
;;;     g_value_dup_boxed
;;;
;;;     G_IS_PARAM_SPEC_POINTER
;;;     G_PARAM_SPEC_POINTER
;;;     G_VALUE_HOLDS_POINTER
;;;     G_TYPE_PARAM_POINTER
;;;
;;;     GParamSpecPointer
;;;
;;;     g_param_spec_pointer
;;;     g_value_set_pointer
;;;     g_value_get_pointer
;;;
;;;     G_IS_PARAM_SPEC_OBJECT
;;;     G_PARAM_SPEC_OBJECT
;;;     G_VALUE_HOLDS_OBJECT
;;;     G_TYPE_PARAM_OBJECT
;;;
;;;     GParamSpecObject
;;;
;;;     g_param_spec_object
;;;     g_value_set_object
;;;     g_value_take_object
;;;     g_value_set_object_take_ownership
;;;     g_value_get_object
;;;     g_value_dup_object
;;;
;;;     G_IS_PARAM_SPEC_UNICHAR
;;;     G_PARAM_SPEC_UNICHAR
;;;     G_TYPE_PARAM_UNICHAR
;;;
;;;     GParamSpecUnichar
;;;
;;;     g_param_spec_unichar
;;;
;;;     G_IS_PARAM_SPEC_VALUE_ARRAY
;;;     G_PARAM_SPEC_VALUE_ARRAY
;;;     G_TYPE_PARAM_VALUE_ARRAY
;;;
;;;     GParamSpecValueArray
;;;
;;;     g_param_spec_value_array
;;;
;;;     G_IS_PARAM_SPEC_OVERRIDE
;;;     G_PARAM_SPEC_OVERRIDE
;;;     G_TYPE_PARAM_OVERRIDE
;;;
;;;     GParamSpecOverride
;;;
;;;     g_param_spec_override
;;;
;;;     G_IS_PARAM_SPEC_GTYPE
;;;     G_PARAM_SPEC_GTYPE
;;;     G_VALUE_HOLDS_GTYPE
;;;     G_TYPE_PARAM_GTYPE
;;;
;;;     GParamSpecGType
;;;
;;;     g_param_spec_gtype
;;;     g_value_get_gtype
;;;     g_value_set_gtype
;;;
;;;     G_IS_PARAM_SPEC_VARIANT
;;;     G_PARAM_SPEC_VARIANT
;;;     G_VALUE_HOLDS_VARIANT
;;;     G_TYPE_PARAM_VARIANT
;;;
;;;     GParamSpecVariant
;;;
;;;     g_param_spec_variant
;;;     g_value_get_variant
;;;     g_value_dup_variant
;;;     g_value_set_variant
;;;     g_value_take_variant
;;;
;;; Description
;;;
;;; GValue provides an abstract container structure which can be copied,
;;; transformed and compared while holding a value of any (derived) type, which
;;; is registered as a GType with a GTypeValueTable in its GTypeInfo structure.
;;; Parameter specifications for most value types can be created as GParamSpec
;;; derived instances, to implement e.g. GObject properties which operate on
;;; GValue containers.
;;;
;;; Parameter names need to start with a letter (a-z or A-Z). Subsequent
;;; characters can be letters, numbers or a '-'. All other characters are
;;; replaced by a '-' during construction.
;;; ----------------------------------------------------------------------------

(in-package :gobject)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_BOOLEAN()
;;;
;;; #define G_IS_PARAM_SPEC_BOOLEAN(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_BOOLEAN))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_BOOLEAN.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_BOOLEAN()
;;;
;;; #define G_PARAM_SPEC_BOOLEAN(pspec)
;;;;        (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_BOOLEAN, GParamSpecBoolean))
;;;
;;; Cast a GParamSpec instance into a GParamSpecBoolean.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_BOOLEAN()
;;;
;;; #define G_VALUE_HOLDS_BOOLEAN(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_BOOLEAN))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_BOOLEAN.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_BOOLEAN
;;;
;;; #define G_TYPE_PARAM_BOOLEAN (g_param_spec_types[2])
;;;
;;; The GType of GParamSpecBoolean.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecBoolean
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-boolean
  (:parent-instance g-param-spec)
  (:default-value :boolean))

(export 'g-param-spec-boolean)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-boolean atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-boolean atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    boolean properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-boolean
  (:parent-instance g-param-spec)
  (:default-value :boolean))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_boolean ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_boolean" g-param-spec-boolean)
    (:pointer g-param-spec-boolean)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-boolean} instance specifying a
    @var{+g-type-boolean+} property.
  @end{short}
  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (default-value :boolean)
  (flags g-param-flags))

(export 'g-param-spec-boolean)

;;; ----------------------------------------------------------------------------
;;; g_value_set_boolean ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_boolean" g-value-set-boolean) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gboolean}}
  @argument[v-boolean]{boolean value to be set}
  Set the contents of a @code{gboolean} @symbol{g-value} to @arg{v-boolean}."
  (value (:pointer g-value))
  (v-boolean :boolean))

(export 'g-value-set-boolean)

;;; ----------------------------------------------------------------------------
;;; g_value_get_boolean ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_boolean" g-value-get-boolean) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gboolean}}
  @return{Boolean contents of @arg{value}.}
  @short{Get the contents of a @code{gboolean} @symbol{g-value}.}"
  (value (:pointer g-value)))

(export 'g-value-get-boolean)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_CHAR()
;;;
;;; #define G_IS_PARAM_SPEC_CHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_CHAR))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_CHAR.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_CHAR()
;;;
;;; #define G_PARAM_SPEC_CHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_CHAR, GParamSpecChar))
;;;
;;; Cast a GParamSpec instance into a GParamSpecChar.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_CHAR()
;;;
;;; #define G_VALUE_HOLDS_CHAR(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_CHAR))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_CHAR.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_CHAR
;;;
;;; #define G_TYPE_PARAM_CHAR (g_param_spec_types[0])
;;;
;;; The GType of GParamSpecChar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecChar
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-char
  (:parent-instance g-param-spec)
  (:minimum :int8)
  (:maximum :int8)
  (:default-value :int8))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-char atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-char atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    character properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-char
  (:parent-instance g-param-spec)
  (:minimum :int8)
  (:maximum :int8)
  (:default-value :int8))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:minimum]{minimum value for the property specified}
    @entry[:maximum]{maximum value for the property specified}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_char" g-param-spec-char) (:pointer g-param-spec-char)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[minimum]{minimum value for the property specified}
  @argument[maximum]{maximum value for the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  Creates a new @symbol{g-param-spec-char} instance specifying a
  @var{+g-type-char+} property."
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int8)
  (maximum :int8)
  (default-value :int8)
  (flags g-param-flags))

(export 'g-param-spec-char)

;;; ----------------------------------------------------------------------------
;;; g_value_set_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_char" g-value-set-char) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gchar}}
  @argument[v-char]{character value to be set}
  @subheading{Warning}
    @sym{g-value-set-char} has been deprecated since version 2.32 and should not
    be used in newly-written code. This function's input type is broken, see
    @fun{g-value-set-schar}.

  @begin{short}
    Set the contents of a @code{gchar} @symbol{g-value} to @arg{v-char}.
  @end{short}"
  (value (:pointer g-value))
  (v-char :char))

(export 'g-value-set-char)

;;; ----------------------------------------------------------------------------
;;; g_value_get_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_char" g-value-get-char) :char
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gchar}}
  @return{Character contents of @arg{value}.}
  @subheading{Warning}
    @sym{g-value-get-char} has been deprecated since version 2.32 and should not
    be used in newly-written code. This function's return type is broken, see
    @fun{g-value-get-schar}.

    Do not use this function; it is broken on platforms where the char type is
    unsigned, such as ARM and PowerPC. See @fun{g-value-get-schar}.

    @short{Get the contents of a @code{gchar} @symbol{g-value}.}"
  (value (:pointer g-value)))

(export 'g-value-get-char)

;;; ----------------------------------------------------------------------------
;;; g_value_get_schar ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_schar" g-value-get-schar) :int8
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gchar}}
  @return{Signed 8 bit integer contents of @arg{value}.}
  @short{Get the contents of a @code{gchar} @symbol{g-value}.}

  Since 2.32"
  (value (:pointer g-value)))

(export 'g-value-get-schar)

;;; ----------------------------------------------------------------------------
;;; g_value_set_schar ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_schar" g-value-set-schar) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gchar}}
  @argument[v-char]{signed 8 bit integer to be set}
  @short{Set the contents of a @code{gchar} @symbol{g-value} to @arg{v-char}.}

  Since 2.32"
  (value (:pointer g-value))
  (v-char :int8))

(export 'g-value-set-schar)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_UCHAR()
;;;
;;; #define G_IS_PARAM_SPEC_UCHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_UCHAR))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_UCHAR.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_UCHAR()
;;;
;;; #define G_PARAM_SPEC_UCHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_UCHAR, GParamSpecUChar))
;;;
;;; Cast a GParamSpec instance into a GParamSpecUChar.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_UCHAR()
;;;
;;; #define G_VALUE_HOLDS_UCHAR(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_UCHAR))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_UCHAR.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_UCHAR
;;;
;;; #define G_TYPE_PARAM_UCHAR           (g_param_spec_types[1])
;;;
;;; The GType of GParamSpecUChar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecUChar
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-uchar
  (:parent-instance g-param-spec)
  (:minimum :uint8)
  (:maximum :uint8)
  (:default-value :uint8))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-uchar atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-uchar atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    unsigned character properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-uchar
  (:parent-instance g-param-spec)
  (:minimum :uint8)
  (:maximum :uint8)
  (:default-value :uint8))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:minimum]{minimum value for the property specified}
    @entry[:maximum]{maximum value for the property specified}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uchar ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_uchar" g-param-spec-uchar) (:pointer g-param-spec-uchar)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[minimum]{minimum value for the property specified}
  @argument[maximum]{maximum value for the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{a newly created parameter specification}
  Creates a new @symbol{g-param-spec-uchar} instance specifying a
  @var{+g-type-uchar+} property."
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint8)
  (maximum :uint8)
  (default-value :uint8)
  (flags g-param-flags))

(export 'g-param-spec-uchar)

;;; ----------------------------------------------------------------------------
;;; g_value_set_uchar ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_uchar" g-value-set-uchar) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{guchar}}
  @argument[v-uchar]{unsigned character value to be set}
  Set the contents of a @code{guchar} @symbol{g-value} to @arg{v-uchar}."
  (value (:pointer g-value))
  (v-uchar :uchar))

(export 'g-value-set-uchar)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uchar ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_uchar" g-value-get-uchar) :uchar
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{guchar}}
  @return{Unsigned character contents of @arg{value}.}
  Get the contents of a @code{guchar} @symol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-uchar)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_INT()
;;;
;;; #define G_IS_PARAM_SPEC_INT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_INT))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_INT.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_INT()
;;;
;;; #define G_PARAM_SPEC_INT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_INT, GParamSpecInt))
;;;
;;; Cast a GParamSpec instance into a GParamSpecInt.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_INT()
;;;
;;; #define G_VALUE_HOLDS_INT(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_INT))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_INT.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_INT
;;;
;;; #define G_TYPE_PARAM_INT (g_param_spec_types[3])
;;;
;;; The GType of GParamSpecInt.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecInt
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-int
  (:parent-instance g-param-spec)
  (:minimum :int)
  (:maximum :int)
  (:default-value :int))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-int atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-int atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-int
  (:parent-instance g-param-spec)
  (:minimum :int)
  (:maximum :int)
  (:default-value :int))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:minimum]{minimum value for the property specified}
    @entry[:maximum]{maximum value for the property specified}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_int ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_int" g-param-spec-int) (:pointer g-param-spec-int)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[minimum]{minimum value for the property specified}
  @argument[maximum]{maximum value for the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-int} instance specifying a
    @var{+g-type-int+} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int)
  (maximum :int)
  (default-value :int)
  (flags g-param-flags))

(export 'g-param-spec-int)

;;; ----------------------------------------------------------------------------
;;; g_value_set_int ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_int" g-value-set-int) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gint}}
  @argument[v-int]{integer value to be set}
  Set the contents of a @code{gint} @symbol{g-value} to @arg{v-int}."
  (value (:pointer g-value))
  (v-int :int))

(export 'g-value-set-int)

;;; ----------------------------------------------------------------------------
;;; g_value_get_int ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_int" g-value-get-int) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gint}}
  @return{Integer contents of @arg{value}.}
  Get the contents of a @code{gint} @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-int)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_UINT()
;;;
;;; #define G_IS_PARAM_SPEC_UINT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_UINT))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_UINT.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_UINT()
;;;
;;; #define G_PARAM_SPEC_UINT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_UINT, GParamSpecUInt))
;;;
;;; Cast a GParamSpec instance into a GParamSpecUInt.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; G_VALUE_HOLDS_UINT()
;;;
;;; #define G_VALUE_HOLDS_UINT(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_UINT))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_UINT.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_UINT
;;;
;;; #define G_TYPE_PARAM_UINT (g_param_spec_types[4])
;;;
;;; The GType of GParamSpecUInt.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecUInt
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-uint
  (:parent-instance g-param-spec)
  (:minimum :uint)
  (:maximum :uint)
  (:default-value :uint))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-uint atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-uint atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    unsigned integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-uint
  (:parent-instance g-param-spec)
  (:minimum :uint)
  (:maximum :uint)
  (:default-value :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:minimum]{minimum value for the property specified}
    @entry[:maximum]{maximum value for the property specified}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uint ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_uint" g-param-spec-uint) (:pointer g-param-spec-uint)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[minimum]{minimum value for the property specified}
  @argument[maximum]{maximum value for the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-uint} instance specifying a
    @var{+g-type-uint+} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint)
  (maximum :uint)
  (default-value :uint)
  (flags g-param-flags))

(export 'g-param-spec-uint)

;;; ----------------------------------------------------------------------------
;;; g_value_set_uint ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_uint" g-value-set-uint) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{guint}}
  @argument[v-uint]{unsigned integer value to be set}
  Set the contents of a @code{guint} @symbol{g-value} to @arg{v-uint}."
  (value (:pointer g-value))
  (v-uint :uint))

(export 'g-value-set-uint)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uint ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_uint" g-value-get-uint) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{guint}}
  @return{Unsigned integer contents of @arg{value}.}
  Get the contents of a @code{guint} @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-uint)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_LONG()
;;;
;;; #define G_IS_PARAM_SPEC_LONG(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_LONG))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_LONG.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_LONG()
;;;
;;; #define G_PARAM_SPEC_LONG(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_LONG, GParamSpecLong))
;;;
;;; Cast a GParamSpec instance into a GParamSpecLong.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_LONG()
;;;
;;; #define G_VALUE_HOLDS_LONG(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_LONG))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_LONG.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_LONG
;;;
;;; #define G_TYPE_PARAM_LONG           (g_param_spec_types[5])
;;;
;;; The GType of GParamSpecLong.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecLong
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-long
  (:parent-instance g-param-spec)
  (:minimum :long)
  (:maximum :long)
  (:default-value :ulong))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-long atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-long atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    long integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-long
  (:parent-instance g-param-spec)
  (:minimum :long)
  (:maximum :long)
  (:default-value :ulong))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:minimum]{minimum value for the property specified}
    @entry[:maximum]{maximum value for the property specified}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_long ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_long" g-param-spec-long) (:pointer g-param-spec-long)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[minimum]{minimum value for the property specified}
  @argument[maximum]{maximum value for the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-long} instance specifying a
    @var{+g-type-long+} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :long)
  (maximum :long)
  (default-value :long)
  (flags g-param-flags))

(export 'g-param-spec-long)

;;; ----------------------------------------------------------------------------
;;; g_value_set_long ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_long" g-value-set-long) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{glong}}
  @argument[v-long]{long integer value to be set}
  Set the contents of a @code{glong} @symbol{g-value} to @arg{v-long}."
  (value (:pointer g-value))
  (v-long :long))

(export 'g-value-set-long)

;;; ----------------------------------------------------------------------------
;;; g_value_get_long ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_long" g-value-get-long) :long
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[value]{a valid @symbol{g-value} of type @code{glong}}
  @return{Long integer contents of @arg{value}.}
  Get the contents of a @code{glong} @symbol{g-value}."
  (g-value (:pointer g-value)))

(export 'g-value-get-long)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_ULONG()
;;;
;;; #define G_IS_PARAM_SPEC_ULONG(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_ULONG))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_ULONG.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_ULONG()
;;;
;;; #define G_PARAM_SPEC_ULONG(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_ULONG, GParamSpecULong))
;;;
;;; Cast a GParamSpec instance into a GParamSpecULong.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_ULONG()
;;;
;;; #define G_VALUE_HOLDS_ULONG(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_ULONG))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_ULONG.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_ULONG
;;;
;;; #define G_TYPE_PARAM_ULONG (g_param_spec_types[6])
;;;
;;; The GType of GParamSpecULong.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecULong
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-ulong
  (:parent-instance g-param-spec)
  (:minimum :ulong)
  (:maximum :ulong)
  (:default-value :ulong))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-ulong atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-ulong atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    unsigned long integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-ulong
  (:parent-instance g-param-spec)
  (:minimum :ulong)
  (:maximum :ulong)
  (:default-value :ulong))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:minimum]{minimum value for the property specified}
    @entry[:maximum]{maximum value for the property specified}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ulong ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_ulong" g-param-spec-ulong) (:pointer g-param-spec-ulong)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[minimum]{minimum value for the property specified}
  @argument[maximum]{maximum value for the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-ulong} instance specifying a
    @code{gulong} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :ulong)
  (maximum :ulong)
  (default-value :ulong)
  (flags g-param-flags))

(export 'g-param-spec-ulong)

;;; ----------------------------------------------------------------------------
;;; g_value_set_ulong ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_ulong" g-value-set-ulong) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gulong}}
  @argument[v-ulong]{unsigned long integer value to be set}
  Set the contents of a @code{gulong} @symbol{g-value} to @arg{v-ulong}."
  (value (:pointer g-value))
  (v-ulong :ulong))

(export 'g-value-set-ulong)

;;; ----------------------------------------------------------------------------
;;; g_value_get_ulong ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_ulong" g-value-get-ulong) :ulong
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gulong}}
  @return{Unsigned long integer contents of @arg{value}.}
  Get the contents of a @code{gulong} @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-ulong)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_INT64()
;;;
;;; #define G_IS_PARAM_SPEC_INT64(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_INT64))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_INT64.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_INT64()
;;;
;;; #define G_PARAM_SPEC_INT64(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_INT64, GParamSpecInt64))
;;;
;;; Cast a GParamSpec instance into a GParamSpecInt64.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_INT64()
;;;
;;; #define G_VALUE_HOLDS_INT64(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_INT64))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_INT64.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_INT64
;;;
;;; #define G_TYPE_PARAM_INT64 (g_param_spec_types[7])
;;;
;;; The GType of GParamSpecInt64.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecInt64
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-int64
  (:parent-instance g-param-spec)
  (:minimum :int64)
  (:maximum :int64)
  (:default-value :int64))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-int64 atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-int64 atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    64bit integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-int64
  (:parent-instance g-param-spec)
  (:minimum :int64)
  (:maximum :int64)
  (:default-value :int64))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:minimum]{minimum value for the property specified}
    @entry[:maximum]{maximum value for the property specified}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_int64 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_int64" g-param-spec-int64) (:pointer g-param-spec-int64)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[minimum]{minimum value for the property specified}
  @argument[maximum]{maximum value for the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-int64} instance specifying a
    @code{gint64} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int64)
  (maximum :int64)
  (default-value :int64)
  (flags g-param-flags))

(export 'g-param-spec-int64)

;;; ----------------------------------------------------------------------------
;;; g_value_set_int64 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_int64" g-value-set-int64) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gint64}}
  @argument[v-int64]{64bit integer value to be set}
  Set the contents of a @code{gint64} @symbol{g-value} to @arg{v-int64}."
  (value (:pointer g-value))
  (v-int64 :int64))

(export 'g-value-set-int64)

;;; ----------------------------------------------------------------------------
;;; g_value_get_int64 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_int64" g-value-get-int64) :int64
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gint64}}
  @return{64bit integer contents of @arg{value}.}
  Get the contents of a @code{gint64} @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-int64)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_UINT64()
;;;
;;; #define G_IS_PARAM_SPEC_UINT64(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_UINT64))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_UINT64.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_UINT64()
;;;
;;; #define G_PARAM_SPEC_UINT64(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_UINT64, GParamSpecUInt64))
;;;
;;; Cast a GParamSpec instance into a GParamSpecUInt64.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_UINT64()
;;;
;;; #define G_VALUE_HOLDS_UINT64(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_UINT64))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_UINT64.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_UINT64
;;;
;;; #define G_TYPE_PARAM_UINT64 (g_param_spec_types[8])
;;;
;;; The GType of GParamSpecUInt64.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecUInt64
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-uint64
  (:parent-instance g-param-spec)
  (:minimum :uint64)
  (:maximum :uint64)
  (:default-value :uint64))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-uint64 atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-uint64 atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    unsigned 64bit integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-uint64
  (:parent-instance g-param-spec)
  (:minimum :uint64)
  (:maximum :uint64)
  (:default-value :uint64))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:minimum]{minimum value for the property specified}
    @entry[:maximum]{maximum value for the property specified}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uint64 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_uint64" g-param-spec-uint64)
    (:pointer g-param-spec-uint64)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[minimum]{minimum value for the property specified}
  @argument[maximum]{maximum value for the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-uint64} instance specifying a
    @code{guint64} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint64)
  (maximum :uint64)
  (default-value :uint64)
  (flags g-param-flags))

(export 'g-param-spec-uint64)

;;; ----------------------------------------------------------------------------
;;; g_value_set_uint64 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_uint64" g-value-set-uint64) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{guint64}}
  @argument[v-uint64]{unsigned 64bit integer value to be set}
  Set the contents of a @code{guint64} @symbol{g-value} to @arg{v-uint64}."
  (value (:pointer g-value))
  (v-uint64 :uint64))

(export 'g-value-set-uint64)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uint64 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_uint64" g-value-get-uint64) :uint64
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{guint64}}
  @return{Unsigned 64bit integer contents of @arg{value}.}
  Get the contents of a @code{guint64} @symbol{g-value}."
  (g-value (:pointer g-value)))

(export 'g-value-get-uint64)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_FLOAT()
;;;
;;; #define G_IS_PARAM_SPEC_FLOAT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_FLOAT))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_FLOAT.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_FLOAT()
;;;
;;; #define G_PARAM_SPEC_FLOAT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_FLOAT, GParamSpecFloat))
;;;
;;; Cast a GParamSpec instance into a GParamSpecFloat.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_FLOAT()
;;;
;;; #define G_VALUE_HOLDS_FLOAT(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_FLOAT))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_FLOAT.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_FLOAT
;;;
;;; #define G_TYPE_PARAM_FLOAT (g_param_spec_types[12])
;;;
;;; The GType of GParamSpecFloat.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecFloat
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-float
  (:parent-instance g-param-spec)
  (:minimum :float)
  (:maximum :float)
  (:default-value :float)
  (:epsilon :float))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-float atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-float atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    float properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-float
  (:parent-instance g-param-spec)
  (:minimum :float)
  (:maximum :float)
  (:default-value :float)
  (:epsilon :float))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:minimum]{minimum value for the property specified}
    @entry[:maximum]{maximum value for the property specified}
    @entry[:default-value]{default value for the property specified}
    @entry[:epsilon]{values closer than epsilon will be considered identical by
      @fun{g-param-values-cmp} the default value is 1e-30.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_float ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_float" g-param-spec-float) (:pointer g-param-spec-float)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[minimum]{minimum value for the property specified}
  @argument[maximum]{maximum value for the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-float} instance specifying a
    @code{gfloat} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :float)
  (maximum :float)
  (default-value :float)
  (flags g-param-flags))

(export 'g-param-spec-float)

;;; ----------------------------------------------------------------------------
;;; g_value_set_float ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_float" g-value-set-float) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gfloat}}
  @argument[v-float]{float value to be set}
  Set the contents of a @code{gfloat} @symbol{g-value} to @arg{v-float}."
  (value (:pointer g-value))
  (v-float :float))

(export 'g-value-set-float)

;;; ----------------------------------------------------------------------------
;;; g_value_get_float ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_float" g-value-get-float) :float
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gfloat}}
  @return{Float contents of @arg{value}.}
  Get the contents of a @code{gfloat} @symbol{g-value}."
  (g-value (:pointer g-value)))

(export 'g-value-get-float)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_DOUBLE()
;;;
;;; #define G_IS_PARAM_SPEC_DOUBLE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_DOUBLE))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_DOUBLE.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_DOUBLE()
;;;
;;; #define G_PARAM_SPEC_DOUBLE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_DOUBLE, GParamSpecDouble))
;;;
;;; Cast a GParamSpec instance into a GParamSpecDouble.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_DOUBLE()
;;;
;;; #define G_VALUE_HOLDS_DOUBLE(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_DOUBLE))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_DOUBLE.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_DOUBLE
;;;
;;; #define G_TYPE_PARAM_DOUBLE (g_param_spec_types[13])
;;;
;;; The GType of GParamSpecDouble.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecDouble
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-double
  (:parent-instance g-param-spec)
  (:minimum :double)
  (:maximum :double)
  (:default-value :double)
  (:epsilon :double))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-double atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-double atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    double properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-double
  (:parent-instance g-param-spec)
  (:minimum :double)
  (:maximum :double)
  (:default-value :double)
  (:epsilon :double))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:minimum]{minimum value for the property specified}
    @entry[:maximum]{maximum value for the property specified}
    @entry[:default-value]{default value for the property specified}
    @entry[:epsilon]{values closer than epsilon will be considered identical by
      @fun{g-param-values-cmp} the default value is 1e-90.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_double ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_double" g-param-spec-double)
    (:pointer g-param-spec-double)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[minimum]{minimum value for the property specified}
  @argument[maximum]{maximum value for the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-double} instance specifying a
    @code{gdouble} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :double)
  (maximum :double)
  (default-value :double)
  (flags g-param-flags))

(export 'g-param-spec-double)

;;; ----------------------------------------------------------------------------
;;; g_value_set_double ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_double" g-value-set-double) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gdouble}}
  @argument[v-double]{double value to be set}
  Set the contents of a @code{gdouble} to @arg{v-double}."
  (value (:pointer g-value))
  (v-double :double))

(export 'g-value-set-double)

;;; ----------------------------------------------------------------------------
;;; g_value_get_double ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_double" g-value-get-double) :double
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gdouble}}
  @return{Double contents of @arg{value}.}
  Get the contents of a @code{gdouble} @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-double)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_ENUM()
;;;
;;; #define G_IS_PARAM_SPEC_ENUM(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_ENUM))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_ENUM.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_ENUM()
;;;
;;; #define G_PARAM_SPEC_ENUM(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_ENUM, GParamSpecEnum))
;;;
;;; Cast a GParamSpec instance into a GParamSpecEnum.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_ENUM()
;;;
;;; #define G_VALUE_HOLDS_ENUM(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_ENUM))
;;;
;;; Checks whether the given GValue can hold values derived from type
;;; G_TYPE_ENUM.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_ENUM
;;;
;;; #define G_TYPE_PARAM_ENUM (g_param_spec_types[10])
;;;
;;; The GType of GParamSpecEnum.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecEnum
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-enum
  (:parent-instance g-param-spec)
  (:enum-class (:pointer g-enum-class))
  (:default-value :int))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-enum atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-enum atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    enum properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-enum
  (:parent-instance g-param-spec)
  (:enum-class (:pointer g-enum-class))
  (:default-value :int))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:enum-class]{the @symbol{g-enum-class} for the enum}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_enum ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_enum" g-param-spec-enum) (:pointer g-param-spec-enum)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[enum-type]{a @class{g-type} derived from @code{GEnum}}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-enum} instance specifying a
    @code{GEnum} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (enum-type g-type)
  (default-value :int)
  (flags g-param-flags))

(export 'g-param-spec-enum)

;;; ----------------------------------------------------------------------------
;;; g_value_set_enum ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_enum" g-value-set-enum) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} whose type is derived from
    @code{GEnum}}
  @argument[v-enum]{enum value to be set}
  Set the contents of a @code{GEnum} @symbol{g-value} to @arg{v-enum}."
  (value (:pointer g-value))
  (v-enum :int))

(export 'g-value-set-enum)

;;; ----------------------------------------------------------------------------
;;; g_value_get_enum ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_enum" g-value-get-enum) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} whose type is derived from
    @code{GEnum}}
  @return{Enum contents of @arg{value}.}
  Get the contents of a @code{GEnum} @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-enum)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_FLAGS()
;;;
;;; #define G_IS_PARAM_SPEC_FLAGS(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_FLAGS))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_FLAGS.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_FLAGS()
;;;
;;; #define G_PARAM_SPEC_FLAGS(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_FLAGS, GParamSpecFlags))
;;;
;;; Cast a GParamSpec instance into a GParamSpecFlags.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_FLAGS()
;;;
;;; #define G_VALUE_HOLDS_FLAGS(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_FLAGS))
;;;
;;; Checks whether the given GValue can hold values derived from type
;;; G_TYPE_FLAGS.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_FLAGS
;;;
;;; #define G_TYPE_PARAM_FLAGS (g_param_spec_types[11])
;;;
;;; The GType of GParamSpecFlags.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecFlags
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-flags
  (:parent-instance g-param-spec)
  (:flags-class (:pointer g-flags-class))
  (:default-value :uint))

(export 'g-param-spec-flags)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-flags atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-flags atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    flags properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-flags
  (:parent-instance g-param-spec)
  (:flags-class (:pointer g-flags-class))
  (:default-value :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:flags-class]{the @symbol{g-flags-class} for the flags}
    @entry[:default-value]{default value for the property specified}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_flags ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_flags" g-param-spec-flags) (:pointer g-param-spec-flags)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-2}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[flags-type]{a @class{g-type} derived from @code{GFlags}}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-flags} instance specifying a
    @code{GFlags} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (flags-type g-type)
  (default-value :int)
  (flags g-param-flags))

(export 'g-param-spec-flags)

;;; ----------------------------------------------------------------------------
;;; g_value_set_flags ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_flags" g-value-set-flags) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} whose type is derived from
    @code{GFlags}}
  @argument[v-flags]{flags value to be set}
  Set the contents of a @code{GFlags} @symbol{g-value} to @arg{v-flags}."
  (value (:pointer g-value))
  (v-flags :int))

(export 'g-value-set-flags)

;;; ----------------------------------------------------------------------------
;;; g_value_get_flags ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_flags" g-value-get-flags) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} whose type is derived from
    @code{GFlags}}
  @return{Flags contents of @arg{value}.}
  Get the contents of a @code{GFlags} @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-flags)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_STRING()
;;;
;;; #define G_IS_PARAM_SPEC_STRING(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_STRING))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_STRING.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_STRING()
;;;
;;; #define G_PARAM_SPEC_STRING(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_STRING, GParamSpecString))
;;;
;;; Casts a GParamSpec instance into a GParamSpecString.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_STRING()
;;;
;;; #define G_VALUE_HOLDS_STRING(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_STRING))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_STRING.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_STRING
;;;
;;; #define G_TYPE_PARAM_STRING (g_param_spec_types[14])
;;;
;;; The GType of GParamSpecString.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecString
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-string
  (:parent-instance g-param-spec)
  (:default-value (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-first (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-nth (:string :free-to-foreign nil :free-from-foreign nil))
  (:substitutor :char)
  ;; TODO: The definition in the C API is different. Check this.
  ;;;   guint         null_fold_if_empty : 1;
  ;;;   guint         ensure_non_null : 1;
  (:flags-for-null :uint))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-string atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-string atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    string properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-string
  (:parent-instance g-param-spec)
  (:default-value (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-first (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-nth (:string :free-to-foreign nil :free-from-foreign nil))
  (:substitutor :char)
  (:flags-for-null :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:default-value]{default value for the property specified}
    @entry[:cset-frist]{a string containing the allowed values for the first
      byte}
    @entry[:cset-nth]{a string containing the allowed values for the subsequent
      bytes}
    @entry[:substitutor]{the replacement byte for bytes which do not match
      @code{:cset-first} or @code{cset-nth}.}
    @entry[:flags-for-null]{replace empty string by @code{NULL} and
      @code{NULL} strings by an empty string}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gchararray
;;;
;;; typedef gchar* gchararray;
;;;
;;; A C representable type name for G_TYPE_STRING.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_string ()
;;; ----------------------------------------------------------------------------

(defcfun g-param-spec-string (:pointer g-param-spec-string)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[default-value]{default value for the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-string} instance.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (default-value :string)
  (flags g-param-flags))

(export 'g-param-spec-string)

;;; ----------------------------------------------------------------------------
;;; g_value_set_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_string" g-value-set-string) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gstring}}
  @argument[v-string]{caller-owned string to be duplicated for the
    @symbol{g-value}}
  Set the contents of a @code{gstring} @symbol{g-value} to @arg{v-string}."
  (value (:pointer g-value))
  (v-string :string))

(export 'g-value-set-string)

;;; ----------------------------------------------------------------------------
;;; g_value_set_static_string ()
;;;
;;; void g_value_set_static_string (GValue *value, const gchar *v_string);
;;;
;;; Set the contents of a G_TYPE_STRING GValue to v_string. The string is
;;; assumed to be static, and is thus not duplicated when setting the GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; v_string :
;;;     static string to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_take_string ()
;;;
;;; void g_value_take_string (GValue *value, gchar *v_string);
;;;
;;; Sets the contents of a G_TYPE_STRING GValue to v_string.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; v_string :
;;;     string to take ownership of
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_string_take_ownership ()
;;;
;;; void g_value_set_string_take_ownership (GValue *value, gchar *v_string);
;;;
;;; Warning
;;;
;;; g_value_set_string_take_ownership has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use g_value_take_string() instead.
;;;
;;; This is an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; v_string :
;;;     duplicated unowned string to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_get_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_string" g-value-get-string)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{gstring}}
  @return{String content of @arg{value}.}
  Get the contents of a @code{gstring} @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-string)

;;; ----------------------------------------------------------------------------
;;; g_value_dup_string ()
;;;
;;; gchar * g_value_dup_string (const GValue *value);
;;;
;;; Get a copy the contents of a G_TYPE_STRING GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; Returns :
;;;     a newly allocated copy of the string content of value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_PARAM()
;;;
;;; #define G_IS_PARAM_SPEC_PARAM(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_PARAM))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_PARAM.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_PARAM()
;;;
;;; #define G_PARAM_SPEC_PARAM(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_PARAM, GParamSpecParam))
;;;
;;; Casts a GParamSpec instance into a GParamSpecParam.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_PARAM()
;;;
;;; #define G_VALUE_HOLDS_PARAM(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_PARAM))
;;;
;;; Checks whether the given GValue can hold values derived from type
;;; G_TYPE_PARAM.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_PARAM
;;;
;;; #define G_TYPE_PARAM_PARAM (g_param_spec_types[15])
;;;
;;; The GType of GParamSpecParam.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecParam
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-param
  (:parent-instance g-param-spec))

(export 'g-param-spec-param)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-param atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-param atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    @var{+g-type-param+} properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-param
  (:parent-instance g-param-spec))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_param ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_param" g-param-spec-param) (:pointer g-param-spec-param)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[param-type]{a @class{g-type} derived from @code{GParam}}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-param} instance specifying a
    @code{GParam} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (param-type g-type)
  (flags g-param-flags))

(export 'g-param-spec-param)

;;; ----------------------------------------------------------------------------
;;; g_value_set_param ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_param" g-value-set-param) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{GParam}}
  @argument[param]{the @symbol{g-param-spec} to be set}
  Set the contents of a @code{GParam} @symbol{g-value} to @arg{param}."
  (value (:pointer g-value))
  (param (:pointer g-param-spec)))

(export 'g-value-set-param)

;;; ----------------------------------------------------------------------------
;;; g_value_take_param ()
;;;
;;; void g_value_take_param (GValue *value, GParamSpec *param);
;;;
;;; Sets the contents of a G_TYPE_PARAM GValue to param and takes over the
;;; ownership of the callers reference to param; the caller doesn't have to
;;; unref it any more.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_PARAM
;;;
;;; param :
;;;     the GParamSpec to be set
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_param_take_ownership ()
;;;
;;; void g_value_set_param_take_ownership (GValue *value, GParamSpec *param);
;;;
;;; Warning
;;;
;;; g_value_set_param_take_ownership has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use g_value_take_param() instead.
;;;
;;; This is an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_PARAM
;;;
;;; param :
;;;     the GParamSpec to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_get_param ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_param" g-value-get-param) (:pointer g-param-spec)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} whose type is derived from
    @code{GParam}}
  @return{@symbol{g-param-spec} content of @arg{value}.}
  Get the contents of a @code{GParam} @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-param)

;;; ----------------------------------------------------------------------------
;;; g_value_dup_param ()
;;;
;;; GParamSpec * g_value_dup_param (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_PARAM GValue, increasing its reference count.
;;;
;;; value :
;;;     a valid GValue whose type is derived from G_TYPE_PARAM
;;;
;;; Returns :
;;;     GParamSpec content of value, should be unreferenced when no longer
;;;     needed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_BOXED()
;;;
;;; #define G_IS_PARAM_SPEC_BOXED(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_BOXED))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_BOXED.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_BOXED()
;;;
;;; #define G_PARAM_SPEC_BOXED(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_BOXED, GParamSpecBoxed))
;;;
;;; Cast a GParamSpec instance into a GParamSpecBoxed.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_BOXED()
;;;
;;; #define G_VALUE_HOLDS_BOXED(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_BOXED))
;;;
;;; Checks whether the given GValue can hold values derived from type
;;; G_TYPE_BOXED.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_BOXED
;;;
;;; #define G_TYPE_PARAM_BOXED (g_param_spec_types[16])
;;;
;;; The GType of GParamSpecBoxed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecBoxed
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-boxed
  (:parent-instance g-param-spec))

(export 'g-param-spec-boxed)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-boxed atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-boxed atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    boxed properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-boxed
  (:parent-instance g-param-spec))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_boxed ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_boxed" g-param-spec-boxed) (:pointer g-param-spec-boxed)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[boxed-type]{@code{GBoxed} derived type of this property}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-boxed} instance specifying a
    @code{GBoxed} derived property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (boxed-type g-type)
  (flags g-param-flags))

(export 'g-param-spec-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_set_boxed ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_boxed" g-value-set-boxed) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of @code{GBoxed} derived type}
  @argument[v-boxed]{boxed value to be set}
  Set the contents of a @code{GBoxed} derived @symbol{g-value} to
  @arg{v-boxed}."
  (value (:pointer g-value))
  (v-boxed :pointer))

(export 'g-value-set-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_set_static_boxed ()
;;;
;;; void g_value_set_static_boxed (GValue *value, gconstpointer v_boxed);
;;;
;;; Set the contents of a G_TYPE_BOXED derived GValue to v_boxed. The boxed
;;; value is assumed to be static, and is thus not duplicated when setting the
;;; GValue.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_BOXED derived type
;;;
;;; v_boxed :
;;;     static boxed value to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_take_boxed ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_take_boxed" g-value-take-boxed) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of @code{GBoxed} derived type}
  @argument[v-boxed]{duplicated unowned boxed value to be set}
  @begin{short}
    Sets the contents of a @code{GBoxed} derived @symbol{g-value} to
    @arg{v-boxed} and takes over the ownership of the callers reference to
    @arg{v-boxed}; the caller doesn't have to unref it any more.
  @end{short}

  Since 2.4"
  (value (:pointer g-value))
  (v-boxed :pointer))

(export 'g-value-take-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_set_boxed_take_ownership ()
;;;
;;; void g_value_set_boxed_take_ownership (GValue *value, gconstpointer v_boxed)
;;;
;;; Warning
;;;
;;; g_value_set_boxed_take_ownership has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use g_value_take_boxed() instead.
;;;
;;; This is an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_BOXED derived type
;;;
;;; v_boxed :
;;;     duplicated unowned boxed value to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_get_boxed ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_boxed" g-value-get-boxed) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of @code{GBoxed} derived type}
  @return{Boxed contents of @arg{value}.}
  Get the contents of a @code{GBoxed} derived @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_dup_boxed ()
;;;
;;; gpointer g_value_dup_boxed (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_BOXED derived GValue. Upon getting, the boxed
;;; value is duplicated and needs to be later freed with g_boxed_free(), e.g.
;;; like: g_boxed_free (G_VALUE_TYPE (value), return_value);
;;;
;;; value :
;;;     a valid GValue of G_TYPE_BOXED derived type
;;;
;;; Returns :
;;;     boxed contents of value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_POINTER()
;;;
;;; #define G_IS_PARAM_SPEC_POINTER(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_POINTER))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_POINTER.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_POINTER()
;;;
;;; #define G_PARAM_SPEC_POINTER(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_POINTER, GParamSpecPointer))
;;;
;;; Casts a GParamSpec instance into a GParamSpecPointer.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_POINTER()
;;;
;;; #define G_VALUE_HOLDS_POINTER(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_POINTER))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_POINTER.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_POINTER
;;;
;;; #define G_TYPE_PARAM_POINTER (g_param_spec_types[17])
;;;
;;; The GType of GParamSpecPointer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecPointer
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-pointer
  (:parent-instance g-param-spec))

(export 'g-param-spec-pointer)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-pointer atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-pointer atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    pointer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-pointer
  (:parent-instance g-param-spec))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_pointer" g-param-spec-pointer)
    (:pointer g-param-spec-pointer)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-pointer} instance specifying a pointer
    property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (flags g-param-flags))

(export 'g-param-spec-pointer)

;;; ----------------------------------------------------------------------------
;;; g_value_set_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_pointer" g-value-set-pointer) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of @code{gpointer}}
  @argument[v-pointer]{pointer value to be set}
  Set the contents of a pointer @symbol{g-value} to @arg{v-pointer}."
  (value (:pointer g-value))
  (v-pointer :pointer))

(export 'g-value-set-pointer)

;;; ----------------------------------------------------------------------------
;;; g_value_get_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_pointer" g-value-get-pointer) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of @code{gpointer}}
  @return{Pointer contents of @arg{value}.}
  Get the contents of a pointer @symbol{g-value}."
  (value (:pointer g-value)))

(export 'g-value-get-pointer)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_OBJECT()
;;;
;;; #define G_IS_PARAM_SPEC_OBJECT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_OBJECT))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_OBJECT.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_OBJECT()
;;;
;;; #define G_PARAM_SPEC_OBJECT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_OBJECT, GParamSpecObject))
;;;
;;; Casts a GParamSpec instance into a GParamSpecObject.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_OBJECT()
;;;
;;; #define G_VALUE_HOLDS_OBJECT(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_OBJECT))
;;;
;;; Checks whether the given GValue can hold values derived from type
;;; G_TYPE_OBJECT.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_OBJECT
;;;
;;; #define G_TYPE_PARAM_OBJECT (g_param_spec_types[19])
;;;
;;; The GType of GParamSpecObject.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecObject
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-object
  (:parent-instance g-param-spec))

(export 'g-param-spec-object)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-object atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-object atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    object properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-object
  (:parent-instance g-param-spec))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_object ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_object" g-param-spec-object)
    (:pointer g-param-spec-object)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[object-type]{@code{GObject} derived type of this property}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-boxed} instance specifying a
    @code{GObject} derived property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (object-type g-type)
  (flags g-param-flags))

(export 'g-param-spec-object)

;;; ----------------------------------------------------------------------------
;;; g_value_set_object ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_object" %g-value-set-object) :void
  (value (:pointer g-value))
  (v-object :pointer))

(defun g-value-set-object (value v-object)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of @code{GObject} derived type}
  @argument[v-object]{object value to be set}
  @begin{short}
    Set the contents of a @code{GObject} derived @symbol{g-value} to
    @arg{v-object}.
  @end{short}

  @sym{g-value-set-object} increases the reference count of @arg{v-object} (the
  @symbol{g-value} holds a reference to @arg{v-object}). If you do not wish to
  increase the reference count of the object (i. e. you wish to pass your
  current reference to the @symbol{g-value} because you no longer need it), use
  @fun{g-value-take-object} instead.

  It is important that your @symbol{g-value} holds a reference to @arg{v-object}
  (either its own, or one it has taken) to ensure that the object won't be
  destroyed while the @symbol{g-value} still exists)."
  (%g-value-set-object value (if v-object (pointer v-object) (null-pointer))))

(export 'g-value-set-object)

;;; ----------------------------------------------------------------------------
;;; g_value_take_object ()
;;;
;;; void g_value_take_object (GValue *value, gpointer v_object);
;;;
;;; Sets the contents of a G_TYPE_OBJECT derived GValue to v_object and takes
;;; over the ownership of the callers reference to v_object; the caller doesn't
;;; have to unref it any more (i.e. the reference count of the object is not
;;; increased).
;;;
;;; If you want the GValue to hold its own reference to v_object, use
;;; g_value_set_object() instead.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_OBJECT derived type
;;;
;;; v_object :
;;;     object value to be set
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_object_take_ownership ()
;;;
;;; void g_value_set_object_take_ownership (GValue *value, gpointer v_object);
;;;
;;; Warning
;;;
;;; g_value_set_object_take_ownership has been deprecated since version 2.4 and
;;; should not be used in newly-written code. Use g_value_take_object() instead.
;;;
;;; This is an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_OBJECT derived type
;;;
;;; v_object :
;;;     object value to be set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_get_object ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_object" %g-value-get-object) :pointer
  (value (:pointer g-value)))

;; TODO: %g-value-get-object returns a pointer. The pointer is translated
;;       with get-object-for-pointer to a Lisp object. The transformation
;;       should be done automatically when specifing the type g-object for
;;       the return value.

(defun g-value-get-object (value)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of @code{GObject} derived type}
  @return{Object contents of @arg{value}.}
  Get the contents of a @code{GObject} derived @symbol{g-value}."
  (get-g-object-for-pointer (%g-value-get-object value)))

(export 'g-value-get-object)

;;; ----------------------------------------------------------------------------
;;; g_value_dup_object ()
;;;
;;; gpointer g_value_dup_object (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_OBJECT derived GValue, increasing its reference
;;; count. If the contents of the GValue are NULL, then NULL will be returned.
;;;
;;; value :
;;;     a valid GValue whose type is derived from G_TYPE_OBJECT
;;;
;;; Returns :
;;;     object content of value, should be unreferenced when no longer needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_UNICHAR()
;;;
;;; #define G_IS_PARAM_SPEC_UNICHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_UNICHAR))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_UNICHAR.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_UNICHAR()
;;;
;;; #define G_PARAM_SPEC_UNICHAR(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_UNICHAR, GParamSpecUnichar))
;;;
;;; Cast a GParamSpec instance into a GParamSpecUnichar.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_UNICHAR
;;;
;;; #define G_TYPE_PARAM_UNICHAR (g_param_spec_types[9])
;;;
;;; The GType of GParamSpecUnichar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecUnichar
;;;
;;; struct GParamSpecUnichar {
;;;   GParamSpec    parent_instance;
;;;
;;;   gunichar      default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for unichar
;;; (unsigned integer) properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; gunichar default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_unichar ()
;;;
;;; GParamSpec * g_param_spec_unichar (const gchar *name,
;;;                                    const gchar *nick,
;;;                                    const gchar *blurb,
;;;                                    gunichar default_value,
;;;                                    GParamFlags flags);
;;;
;;; Creates a new GParamSpecUnichar instance specifying a G_TYPE_UINT property.
;;; GValue structures for this property can be accessed with g_value_set_uint()
;;; and g_value_get_uint().
;;;
;;; See g_param_spec_internal() for details on property names.
;;;
;;; name :
;;;     canonical name of the property specified
;;;
;;; nick :
;;;     nick name for the property specified
;;;
;;; blurb :
;;;     description of the property specified
;;;
;;; default_value :
;;;     default value for the property specified
;;;
;;; flags :
;;;     flags for the property specified
;;;
;;; Returns :
;;;     a newly created parameter specification
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_VALUE_ARRAY()
;;;
;;; #define G_IS_PARAM_SPEC_VALUE_ARRAY(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_VALUE_ARRAY))
;;;
;;; Warning
;;;
;;; G_IS_PARAM_SPEC_VALUE_ARRAY has been deprecated since version 2.32 and
;;; should not be used in newly-written code. Use GArray instead of GValueArray
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_VALUE_ARRAY.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;;
;;; Returns :
;;;     TRUE on success.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_VALUE_ARRAY()
;;;
;;; #define G_PARAM_SPEC_VALUE_ARRAY(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_VALUE_ARRAY, GParamSpecValueArray))
;;;
;;; Warning
;;;
;;; G_PARAM_SPEC_VALUE_ARRAY has been deprecated since version 2.32 and should
;;; not be used in newly-written code. Use GArray instead of GValueArray
;;;
;;; Cast a GParamSpec instance into a GParamSpecValueArray.
;;;
;;; pspec :
;;;     a valid GParamSpec instance
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_VALUE_ARRAY
;;;
;;; #define G_TYPE_PARAM_VALUE_ARRAY (g_param_spec_types[18])
;;;
;;; Warning
;;;
;;; G_TYPE_PARAM_VALUE_ARRAY has been deprecated since version 2.32 and should
;;; not be used in newly-written code. Use GArray instead of GValueArray
;;;
;;; The GType of GParamSpecValueArray.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecValueArray
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-value-array
  (:parent-instance g-param-spec)
  (:element-spec (:pointer g-param-spec))
  (:fixed-n-elements :uint))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-value-array atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-value-array atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    @code{GValueArray} properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-value-array
  (:parent-instance g-param-spec)
  (:element-spec (:pointer g-param-spec))
  (:fixed-n-elements :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:private-instance]{private @symbol{g-param-spec} portion}
    @entry[:element-spec]{a @symbol{g-param-spec} describing the elements
      contained in arrays of this property, may be @code{NULL}}
    @entry[:fixed-n-elements]{if greater than 0, arrays of this property will
      always have this many elements}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_value_array ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_value_array" g-param-spec-value-array)
    (:pointer g-param-spec-value-array)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[element-spec]{ a @symbol{g-param-spec} describing the elements
    contained in arrays of this property, may be @code{NULL}}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-value-array} instance specifying a
    @code{GValueArray} property. @code{GValueArray} is a @code{GBoxed} type, as
    such, @symbol{g-value} structures for this property can be accessed with
    @fun{g-value-set-boxed} and @fun{g-value-get-boxed}.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names."
  (name :string)
  (nick :string)
  (blurb :string)
  (element-spec (:pointer g-param-spec))
  (flags g-param-flags))

(export 'g-param-spec-value-array)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_OVERRIDE()
;;;
;;; #define G_IS_PARAM_SPEC_OVERRIDE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_OVERRIDE))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_OVERRIDE.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Returns :
;;;     TRUE on success.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_OVERRIDE()
;;;
;;; #define G_PARAM_SPEC_OVERRIDE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_OVERRIDE, GParamSpecOverride))
;;;
;;; Casts a GParamSpec into a GParamSpecOverride.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_OVERRIDE
;;;
;;; #define G_TYPE_PARAM_OVERRIDE (g_param_spec_types[20])
;;;
;;; The GType of GParamSpecOverride.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecOverride
;;;
;;; struct GParamSpecOverride {
;;; };
;;;
;;; This is a type of GParamSpec type that simply redirects operations to
;;; another paramspec. All operations other than getting or setting the value
;;; are redirected, including accessing the nick and blurb, validating a value,
;;; and so forth. See g_param_spec_get_redirect_target() for retrieving the
;;; overidden property. GParamSpecOverride is used in implementing
;;; g_object_class_override_property(), and will not be directly useful unless
;;; you are implementing a new base type similar to GObject.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_override ()
;;;
;;; GParamSpec * g_param_spec_override (const gchar *name,
;;;                                     GParamSpec *overridden);
;;;
;;; Creates a new property of type GParamSpecOverride. This is used to direct
;;; operations to another paramspec, and will not be directly useful unless you
;;; are implementing a new base type similar to GObject.
;;;
;;; name :
;;;     the name of the property.
;;;
;;; overridden :
;;;     The property that is being overridden
;;;
;;; Returns :
;;;     the newly created GParamSpec
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_GTYPE()
;;;
;;; #define G_IS_PARAM_SPEC_GTYPE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_GTYPE))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_GTYPE.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Returns :
;;;     TRUE on success.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_GTYPE()
;;;
;;; #define G_PARAM_SPEC_GTYPE(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_GTYPE, GParamSpecGType))
;;;
;;; Casts a GParamSpec into a GParamSpecGType.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_GTYPE()
;;;
;;; #define G_VALUE_HOLDS_GTYPE(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_GTYPE))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_GTYPE.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_GTYPE
;;;
;;; #define G_TYPE_PARAM_GTYPE (g_param_spec_types[21])
;;;
;;; The GType of GParamSpecGType.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecGType
;;; ----------------------------------------------------------------------------

;; TODO: The name is changed to g-param-spec-g-type !?

(defcstruct g-param-spec-g-type
  (:parent-instance g-param-spec)
  (:is-a-type g-type))

(export 'g-param-spec-g-type)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-g-type atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-g-type atdoc:*external-symbols*)
 "@version{2013-4-2}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    @class{g-type} properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-g-type
  (:parent-instance g-param-spec)
  (:is-a-type g-type))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{private @symbol{g-param-spec} portion}
    @entry[:is-a-type]{a @class{g-type} whose subtypes can occur as values}
  @end{table}
  Since 2.10")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_gtype ()
;;; ----------------------------------------------------------------------------

;; TODO: The name is changed to g-param-spec-g-type !?

(defcfun ("g_param_spec_gtype" g-param-spec-g-type)
    (:pointer g-param-spec-g-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[name]{canonical name of the property specified}
  @argument[nick]{nick name for the property specified}
  @argument[blurb]{description of the property specified}
  @argument[is-a-type]{a @class{g-type} whose subtypes are allowed as values of
    the property (use @code{G_TYPE_NONE} for any type)}
  @argument[flags]{flags for the property specified}
  @return{A newly created parameter specification.}
  @begin{short}
    Creates a new @symbol{g-param-spec-g-type} instance specifying a
    @code{G_TYPE_GTYPE} property.
  @end{short}

  See @fun{g-param-spec-internal} for details on property names.

  Since 2.10"
  (name :string)
  (nick :string)
  (blurb :string)
  (types-root g-type)
  (flags g-param-flags))

(export 'g-param-spec-g-type)

;;; ----------------------------------------------------------------------------
;;; g_value_get_gtype ()
;;; ----------------------------------------------------------------------------

;; TODO: The name is changed to g-value-get-g-type !?

(defcfun ("g_value_get_gtype" g-value-get-g-type) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{G_TYPE_GTYPE}}
  @return{The @class{g-type} stored in @arg{value}.}
  @short{Get the contents of a @code{G_TYPE_GTYPE} @symbol{g-value}.}

  Since 2.12"
  (g-value (:pointer g-value)))

(export 'g-value-get-g-type)

;;; ----------------------------------------------------------------------------
;;; g_value_set_gtype ()
;;; ----------------------------------------------------------------------------

;; TODO: The name is changed to g-value-set-g-type !?

(defcfun ("g_value_set_gtype" g-value-set-g-type) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{G_TYPE_GTYPE}}
  @argument[v-gtype]{@class{g-type} to be set}
  @begin{short}
    Set the contents of a @code{G_TYPE_GTYPE} @symbol{g-value} to @arg{v-gtype}.
  @end{short}

  Since 2.12"
  (value (:pointer g-value))
  (v-gtype g-type))

(export 'g-value-set-g-type)

;;; ----------------------------------------------------------------------------
;;; G_IS_PARAM_SPEC_VARIANT()
;;;
;;; #define G_IS_PARAM_SPEC_VARIANT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), G_TYPE_PARAM_VARIANT))
;;;
;;; Checks whether the given GParamSpec is of type G_TYPE_PARAM_VARIANT.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Returns :
;;;     TRUE on success
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PARAM_SPEC_VARIANT()
;;;
;;; #define G_PARAM_SPEC_VARIANT(pspec)
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
;;;          G_TYPE_PARAM_VARIANT, GParamSpecVariant))
;;;
;;; Casts a GParamSpec into a GParamSpecVariant.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_HOLDS_VARIANT()
;;;
;;; #define G_VALUE_HOLDS_VARIANT(value)
;;;         (G_TYPE_CHECK_VALUE_TYPE ((value), G_TYPE_VARIANT))
;;;
;;; Checks whether the given GValue can hold values of type G_TYPE_VARIANT.
;;;
;;; value :
;;;     a valid GValue structure
;;;
;;; Returns :
;;;     TRUE on success.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_VARIANT
;;;
;;; #define G_TYPE_PARAM_VARIANT (g_param_spec_types[22])
;;;
;;; The GType of GParamSpecVariant.
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecVariant
;;;
;;; struct GParamSpecVariant {
;;;   GParamSpec    parent_instance;
;;;   GVariantType *type;
;;;   GVariant     *default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for GVariant
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; GVariantType *type;
;;;     a GVariantType, or NULL
;;;
;;; GVariant *default_value;
;;;     a GVariant, or NULL
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_variant ()
;;;
;;; GParamSpec * g_param_spec_variant (const gchar *name,
;;;                                    const gchar *nick,
;;;                                    const gchar *blurb,
;;;                                    const GVariantType *type,
;;;                                    GVariant *default_value,
;;;                                    GParamFlags flags);
;;;
;;; Creates a new GParamSpecVariant instance specifying a GVariant property.
;;;
;;; If default_value is floating, it is consumed.
;;;
;;; See g_param_spec_internal() for details on property names.
;;;
;;; name :
;;;     canonical name of the property specified
;;;
;;; nick :
;;;     nick name for the property specified
;;;
;;; blurb :
;;;     description of the property specified
;;;
;;; type :
;;;     a GVariantType
;;;
;;; default_value :
;;;     a GVariant of type type to use as the default value, or NULL
;;;
;;; flags :
;;;     flags for the property specified
;;;
;;; Returns :
;;;     the newly created GParamSpec
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_get_variant ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_variant" g-value-get-variant) (:pointer g-variant)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{GVariant}}
  @return{Variant contents of @arg{value}.}
  @short{Get the contents of a variant @symbol{g-value}.}

  Since 2.26"
  (value (:pointer g-value)))

(export 'g-value-get-variant)

;;; ----------------------------------------------------------------------------
;;; g_value_dup_variant ()
;;;
;;; GVariant * g_value_dup_variant (const GValue *value);
;;;
;;; Get the contents of a variant GValue, increasing its refcount.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_VARIANT
;;;
;;; Returns :
;;;     variant contents of value, should be unrefed using g_variant_unref()
;;;     when no longer needed
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_variant ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_variant" g-value-set-variant) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[value]{a valid @symbol{g-value} of type @code{GVariant}}
  @argument[variant]{a @code{GVariant}, or @code{NULL}}
  @begin{short}
    Set the contents of a variant @symbol{g-value} to @arg{variant}. If the
    @arg{variant} is floating, it is consumed.
  @end{short}

  Since 2.26"
  (value (:pointer g-value))
  (variant (:pointer g-variant)))

(export 'g-value-set-variant)

;;; ----------------------------------------------------------------------------
;;; g_value_take_variant ()
;;;
;;; void g_value_take_variant (GValue *value, GVariant *variant);
;;;
;;; Set the contents of a variant GValue to variant, and takes over the
;;; ownership of the caller's reference to variant; the caller doesn't have to
;;; unref it any more (i.e. the reference count of the variant is not
;;; increased).
;;;
;;; If variant was floating then its floating reference is converted to a hard
;;; reference.
;;;
;;; If you want the GValue to hold its own reference to variant, use
;;; g_value_set_variant() instead.
;;;
;;; This is an internal function introduced mainly for C marshallers.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_VARIANT
;;;
;;; variant :
;;;     a GVariant, or NULL
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; --- End of file gobject.param.lisp -----------------------------------------
