;;; ----------------------------------------------------------------------------
;;; gobject.param.lisp
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
;;; Parameters and Values
;;;
;;;     Standard Parameter and Value Types
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
;;;     G_VALUE_IS_INTERNED_STRING
;;;     G_TYPE_PARAM_STRING
;;;     G_VALUE_INTERNED_STRING
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
;;;     g_value_set_interned_string
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
;;;         (G_TYPE_CHECK_INSTANCE_CAST ((pspec),
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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:default-value :boolean))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-boolean atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-boolean atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    boolean properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-boolean
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:default-value :boolean))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:default-value]{A @code{:boolean} default value for the property
      specified.}
  @end{table}
  @see-symbol{g-param-spec}
  @see-function{g-param-spec-boolean}")

(export 'g-param-spec-boolean)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_boolean ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_boolean" g-param-spec-boolean)
    (:pointer (:struct g-param-spec-boolean))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[default-value]{a @code{:boolean} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-boolean} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    type @var{+g-type-boolean+}.
  @end{short}
  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-boolean}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-boolean+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (default-value :boolean)
  (flags g-param-flags))

(export 'g-param-spec-boolean)

;;; ----------------------------------------------------------------------------
;;; g_value_set_boolean ()
;;; g_value_get_boolean () -> g-value-boolean
;;; ----------------------------------------------------------------------------

(defun (setf g-value-boolean) (value gvalue)
  (foreign-funcall "g_value_set_boolean"
                   (:pointer (:struct g-value)) gvalue
                   :boolean value
                   :void)
  value)

(defcfun ("g_value_get_boolean" g-value-boolean) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-boolean gvalue) => value}
  @syntax[]{(setf (g-value-boolan gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-boolean+}}
  @argument[value]{a @code{:boolean} value}
  @begin{short}
    Boolean contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-boolean} gets the contents of a @symbol{g-value}
  of type @var{+g-type-boolean+}. The function @sym{(setf g-value-boolean)}
  sets the contents of the @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-boolean)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :int8)
  (:maximum :int8)
  (:default-value :int8))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-char atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-char atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    character properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-char
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :int8)
  (:maximum :int8)
  (:default-value :int8))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:minimum]{A @code{:int8} minimum value for the property specified.}
    @entry[:maximum]{A @code{:int8} maximum value for the property specified.}
    @entry[:default-value]{A @code{:int8} default value for the property
      specified.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-char)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_char" g-param-spec-char)
    (:pointer (:struct g-param-spec-char))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[minimum]{a @code{:int8} minimum value for the property specified}
  @argument[maximum]{a @code{:int8} maximum value for the property specified}
  @argument[default-value]{a @code{:int8} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-char} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    type @var{+g-type-char+}.
  @end{short}
  @see-symbol{g-param-spec-char}
  @see-variable{+g-type-char+}"
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
;;; g_value_get_char () -> g-value-char
;;; ----------------------------------------------------------------------------

(defun (setf g-value-char) (value gvalue)
  (foreign-funcall "g_value_set_char"
                   (:pointer (:struct g-value)) gvalue
                   :int8 value
                   :void)
  value)

(defcfun ("g_value_get_char" g-value-char) :int8
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-char gvalue) => value}
  @syntax[]{(setf (g-value-char gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-char+}}
  @argument[value]{a @code{:int8} character value}
  @begin{short}
     Character contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-char} gets the contents of a @symbol{g-value}
  of type @var{+g-type-char+}. The function @sym{(setf g-value-char)} sets the
  contents of a @symbol{g-value} to @arg{value}.
  @begin[Warning]{dictionary}
    The function @sym{g-value-char} has been deprecated since version 2.32 and
    should not be used in newly-written code. The function return type is
    broken, see the function @fun{g-value-schar}.
  @end{dictionary}
  @see-symbol{g-value}
  @see-function{g-value-schar}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-char)

;;; ----------------------------------------------------------------------------
;;; g_value_get_schar ()
;;; g_value_set_schar () -> g-value-schar
;;; ----------------------------------------------------------------------------

(defun (setf g-value-schar) (value gvalue)
  (foreign-funcall "g_value_set_schar"
                   (:pointer (:struct g-value)) gvalue
                   :int8 value
                   :void)
  value)

(defcfun ("g_value_get_schar" g-value-schar) :int8
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-schar gvalue) => value}
  @syntax[]{(setf (g-value-schar gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-char+}}
  @argument[value]{a @code{:int8} character}
  @begin{short}
    Signed 8 bit integer contents of @arg{gvalue}.
  @end{short}

  The function @fun{g-value-schar} gets the contents of a @symbol{g-value}
  of type @var{+g-type-char+}. The function @sym{(setf g-value-schar)} sets
  the contents of a @symbol{g-value} to @arg{value}.

  Since 2.32
  @see-symbol{g-value}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-schar)

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
;;; #define G_TYPE_PARAM_UCHAR (g_param_spec_types[1])
;;;
;;; The GType of GParamSpecUChar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecUChar
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-uchar
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :uint8)
  (:maximum :uint8)
  (:default-value :uint8))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-uchar atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-uchar atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    unsigned character properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-uchar
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :uint8)
  (:maximum :uint8)
  (:default-value :uint8))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:minimum]{A @code{:uint8} minimum value for the property specified.}
    @entry[:maximum]{A @code{:uint8} maximum value for the property specified.}
    @entry[:default-value]{A @code{:uint8} default value for the property
      specified.}
  @end{table}
  @see-symbol{g-param-spec-uchar}
  @see-variable{+g-type-uchar+}")

(export 'g-param-spec-uchar)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uchar ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_uchar" g-param-spec-uchar)
    (:pointer (:struct g-param-spec-uchar))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[minimum]{a @code{:uint8} minimum value for the property specified}
  @argument[maximum]{a @code{:uint8} maximum value for the property specified}
  @argument[default-value]{a @code{:uint8} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-uchar} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    type @var{+g-type-uchar+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-uchar}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-uchar+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint8)
  (maximum :uint8)
  (default-value :uint8)
  (flags g-param-flags))

(export 'g-param-spec-uchar)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uchar ()
;;; g_value_set_uchar () -> g-value-uchar
;;; ----------------------------------------------------------------------------

(defun (setf g-value-uchar) (value gvalue)
  (foreign-funcall "g_value_set_uchar"
                   (:pointer (:struct g-value)) gvalue
                   :uint8 value
                   :void)
  value)

(defcfun ("g_value_get_uchar" g-value-uchar) :uint8
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-uchar gvalue) => value}
  @syntax[]{(setf (g-value-uchar gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-uchar+}}
  @argument[value]{a @code{:uint8} unsigned character value}
  @begin{short}
    Unsigned character contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-uchar} gets the contents of a @symol{g-value} of
  type @var{+g-type-uchar+}. The function @sym{(setf g-value-uchar)} sets the
  contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-uchar+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-uchar)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :int)
  (:maximum :int)
  (:default-value :int))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-int atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-int atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-int
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :int)
  (:maximum :int)
  (:default-value :int))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:minimum]{A @code{:int} minimum value for the property specified.}
    @entry[:maximum]{A @code{:int} maximum value for the property specified.}
    @entry[:default-value]{A @code{:int} default value for the property
      specified.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-int)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_int ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_int" g-param-spec-int)
    (:pointer (:struct g-param-spec-int))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[minimum]{a @code{:int} minimum value for the property specified}
  @argument[maximum]{a @code{:int} maximum value for the property specified}
  @argument[default-value]{a @code{:int} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-int} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-int+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-int}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-int+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int)
  (maximum :int)
  (default-value :int)
  (flags g-param-flags))

(export 'g-param-spec-int)

;;; ----------------------------------------------------------------------------
;;; g_value_get_int ()
;;; g_value_set_int () -> g-value-int
;;; ----------------------------------------------------------------------------

(defun (setf g-value-int) (value gvalue)
  (foreign-funcall "g_value_set_int"
                   (:pointer (:struct g-value)) gvalue
                   :int value
                   :void)
  value)

(defcfun ("g_value_get_int" g-value-int) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-int gvalue) => value}
  @syntax[]{(setf (g-value-int gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-int+}}
  @argument[value]{a @code{:int} integer value}
  @begin{short}
    Integer contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-int} gets the contents of a @symbol{g-value} of
  type @var{+g-type-int+}. The function @sym{(setf g-value-int)} sets the
  contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-int+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-int)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :uint)
  (:maximum :uint)
  (:default-value :uint))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-uint atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-uint atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    unsigned integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-uint
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :uint)
  (:maximum :uint)
  (:default-value :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:minimum]{a @code{:uint} minimum value for the property specified}
    @entry[:maximum]{a @code{:uint} maximum value for the property specified}
    @entry[:default-value]{a @code{:uint} default value for the property
      specified}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-uint)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uint ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_uint" g-param-spec-uint)
    (:pointer (:struct g-param-spec-uint))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[minimum]{a @code{:uint} minimum value for the property specified}
  @argument[maximum]{a @code{:uint} maximum value for the property specified}
  @argument[default-value]{a @code{:uint} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-uint} parameter specification.}
  @begin{short}
    Creates a new parameter specificaton instance specifying a property
    of type @var{+g-type-uint+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-parm-spec-flags}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-uint}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint)
  (maximum :uint)
  (default-value :uint)
  (flags g-param-flags))

(export 'g-param-spec-uint)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uint ()
;;; g_value_set_uint () -> g-value-uint
;;; ----------------------------------------------------------------------------

(defun (setf g-value-uint) (value gvalue)
  (foreign-funcall "g_value_set_uint"
                   (:pointer (:struct g-value)) gvalue
                   :uint value
                   :void)
  value)

(defcfun ("g_value_get_uint" g-value-uint) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-uint gvalue) => value}
  @syntax[]{(setf (g-value-uint gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-uint+}}
  @argument[value]{a @code{:uint} unsigned integer value}
  @begin{short}
    Unsigned integer contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-uint} gets the contents of a @symbol{g-value}
  of type @var{+g-type-uint+}. The function @sym{(setf g-value-uint)} sets the
  contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-uint+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-uint)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :long)
  (:maximum :long)
  (:default-value :long))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-long atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-long atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    long integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-long
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :long)
  (:maximum :long)
  (:default-value :long))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:minimum]{A @code{:long} minimum value for the property specified.}
    @entry[:maximum]{A @code{:long} maximum value for the property specified.}
    @entry[:default-value]{A @code{:long} default value for the property
      specified.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-long)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_long ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_long" g-param-spec-long)
    (:pointer (:struct g-param-spec-long))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[minimum]{a @code{:long} minimum value for the property specified}
  @argument[maximum]{a @code{:long} maximum value for the property specified}
  @argument[default-value]{a @code{:long} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-long} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-long+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-long}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-long+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :long)
  (maximum :long)
  (default-value :long)
  (flags g-param-flags))

(export 'g-param-spec-long)

;;; ----------------------------------------------------------------------------
;;; g_value_get_long ()
;;; g_value_set_long () -> g-value-long
;;; ----------------------------------------------------------------------------

(defun (setf g-value-long) (value gvalue)
  (foreign-funcall "g_value_set_long"
                   (:pointer (:struct g-value)) gvalue
                   :long value
                   :void)
  value)

(defcfun ("g_value_get_long" g-value-long) :long
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-long gvalue) => value}
  @syntax[]{(setf (g-value-long gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-long+}}
  @argument[value]{a @code{:long} long integer value}
  @begin{short}
    Long integer contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-long} gets the contents of a @symbol{g-value}
  of type @var{+g-type-long+}. The function @sym{(setf g-value-long)} sets the
  contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-long+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-long)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :ulong)
  (:maximum :ulong)
  (:default-value :ulong))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-ulong atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-ulong atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    unsigned long integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-ulong
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :ulong)
  (:maximum :ulong)
  (:default-value :ulong))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:minimum]{A @code{:ulong} minimum value for the property specified.}
    @entry[:maximum]{A @code{:ulong} maximum value for the property specified.}
    @entry[:default-value]{A @code{:ulong} default value for the property
      specified.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-ulong)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ulong ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_ulong" g-param-spec-ulong)
    (:pointer (:struct g-param-spec-ulong))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[minimum]{a @code{:ulong} minimum value for the property specified}
  @argument[maximum]{a @code{:ulong} maximum value for the property specified}
  @argument[default-value]{a @code{:ulong} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-ulong} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-ulong+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-ulong}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-ulong+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :ulong)
  (maximum :ulong)
  (default-value :ulong)
  (flags g-param-flags))

(export 'g-param-spec-ulong)

;;; ----------------------------------------------------------------------------
;;; g_value_get_ulong ()
;;; g_value_set_ulong () -> g-value-ulong
;;; ----------------------------------------------------------------------------

(defun (setf g-value-ulong) (value gvalue)
  (foreign-funcall "g_value_set_ulong"
                   (:pointer (:struct g-value)) gvalue
                   :ulong value
                   :void)
  value)

(defcfun ("g_value_get_ulong" g-value-ulong) :ulong
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-ulong gvalue) => value}
  @syntax[]{(setf (g-value-ulong gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-ulong+}}
  @argument[value]{a @code{:ulong} unsigned long integer value}
  @begin{short}
    Unsigned long integer contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-ulong} gets the contents of a @symbol{g-value}
  of type @var{+g-type-ulong+}. The function @sym{(setf g-value-ulong)} sets
  the contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-ulong+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-ulong)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :int64)
  (:maximum :int64)
  (:default-value :int64))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-int64 atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-int64 atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    64 bit integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-int64
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :int64)
  (:maximum :int64)
  (:default-value :int64))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:minimum]{A @code{:int64} minimum value for the property specified.}
    @entry[:maximum]{A @code{:int64} maximum value for the property specified.}
    @entry[:default-value]{A @code{:int64} default value for the property
      specified.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-int64)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_int64 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_int64" g-param-spec-int64)
    (:pointer (:struct g-param-spec-int64))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[minimum]{a @code{:int64} minimum value for the property specified}
  @argument[maximum]{a @code{:int64} maximum value for the property specified}
  @argument[default-value]{a @code{:int64} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-int64} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-int64+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-int64}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-int64+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int64)
  (maximum :int64)
  (default-value :int64)
  (flags g-param-flags))

(export 'g-param-spec-int64)

;;; ----------------------------------------------------------------------------
;;; g_value_get_int64 ()
;;; g_value_set_int64 () -> g-value-int64
;;; ----------------------------------------------------------------------------

(defun (setf g-value-int64) (value gvalue)
  (foreign-funcall "g_value_set_int64"
                   (:pointer (:struct g-value)) gvalue
                   :int64 value
                   :void)
  value)

(defcfun ("g_value_get_int64" g-value-int64) :int64
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-int64 gvalue) => value}
  @syntax[]{(setf (g-value-int64 gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-int64+}}
  @argument[value]{a @code{:int64} 64 bit integer value}
  @begin{short}
    64 bit integer contents of @arg{gvalue}.
  @end{short}

  The funcion @sym{g-value-int64} gets the contents of a @symbol{g-value}
  of type @var{+g-type-int64+}. The function @sym{(setf g-value-int64)} set the
  contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-int64+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-int64)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :uint64)
  (:maximum :uint64)
  (:default-value :uint64))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-uint64 atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-uint64 atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    unsigned 64 bit integer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-uint64
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :uint64)
  (:maximum :uint64)
  (:default-value :uint64))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:minimum]{A @code{:uint64} minimum value for the property specified.}
    @entry[:maximum]{A @code{:uint64} maximum value for the property specified.}
    @entry[:default-value]{A @code{:uint64} default value for the property
      specified.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-uint64)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uint64 ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_uint64" g-param-spec-uint64)
    (:pointer (:struct g-param-spec-uint64))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[minimum]{a @code{:uint64} minimum value for the property specified}
  @argument[maximum]{a @code{:uint64} maximum value for the property specified}
  @argument[default-value]{a @code{:uint64} default value for the property
    specified}
  @argument[flags]{flags of @symbol{g-param-flags} for the property specified}
  @return{A newly created @symbol{g-param-spec-uint64} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-uint+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-uint64}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-uint64+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint64)
  (maximum :uint64)
  (default-value :uint64)
  (flags g-param-flags))

(export 'g-param-spec-uint64)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uint64 ()
;;; g_value_set_uint64 () -> g-value-uint64
;;; ----------------------------------------------------------------------------

(defun (setf g-value-uint64) (value gvalue)
  (foreign-funcall "g_value_set_uint64"
                   (:pointer (:struct g-value)) gvalue
                   :uint64 value
                   :void)
  value)

(defcfun ("g_value_get_uint64" g-value-uint64) :uint64
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-uint64 gvalue) => value}
  @syntax[]{(setf (g-value-uint64 gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-uint64}}
  @argument[value]{a @code{:uint64} unsigned 64 bit integer value}
  @begin{short}
    Unsigned 64 bit integer contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-uint64} gets the contents of a @symbol{g-value}
  of type @var{+g-type-uint64+}. The function @sym{(setf g-value-uint64)} sets
  the contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-uint64+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-uint64)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :float)
  (:maximum :float)
  (:default-value :float)
  (:epsilon :float))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-float atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-float atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    float properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-float
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :float)
  (:maximum :float)
  (:default-value :float)
  (:epsilon :float))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:minimum]{A @code{:float} minimum value for the property specified.}
    @entry[:maximum]{A @code{:float} maximum value for the property specified.}
    @entry[:default-value]{A @code{:float} default value for the property
      specified.}
    @entry[:epsilon]{a @code{:float}, values closer than epsilon will be
      considered identical by the function @fun{g-param-values-cmp} the default
      value is 1e-30.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-float)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_float ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_float" g-param-spec-float)
    (:pointer (:struct g-param-spec-float))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[minimum]{a @code{:float} minimum value for the property specified}
  @argument[maximum]{a @code{:float} maximum value for the property specified}
  @argument[default-value]{a @code{:float} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-float} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+-g-type-float+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-float}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-float+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :float)
  (maximum :float)
  (default-value :float)
  (flags g-param-flags))

(export 'g-param-spec-float)

;;; ----------------------------------------------------------------------------
;;; g_value_get_float ()
;;; g_value_set_float () -> g-value-float
;;; ----------------------------------------------------------------------------

(defun (setf g-value-float) (value gvalue)
  (foreign-funcall "g_value_set_float"
                   (:pointer (:struct g-value)) gvalue
                   :float value
                   :void)
  value)

(defcfun ("g_value_get_float" g-value-float) :float
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-float gvalue) => value}
  @syntax[]{(setf (g-value-float gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-float+}}
  @argument[value]{a @code{:float} value}
  @begin{short}
    Float contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-float} gets the contents of a @symbol{g-value}
  of type @var{+g-type-float+}. The function @sym{(setf g-value-float)} sets
  the contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-float+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-float)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :double)
  (:maximum :double)
  (:default-value :double)
  (:epsilon :double))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-double atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-double atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    double properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-double
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:minimum :double)
  (:maximum :double)
  (:default-value :double)
  (:epsilon :double))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:minimum]{A @code{:double} minimum value for the property specified.}
    @entry[:maximum]{A @code{:double} maximum value for the property specified.}
    @entry[:default-value]{A @code{:double} default value for the property
      specified.}
    @entry[:epsilon]{A @code{:double}, values closer than epsilon will be
      considered identical by the function @fun{g-param-values-cmp}, the default
      value is 1e-90.}
  @end{table}
  @see-symbol{g-param-spec-double}
  @see-variable{+g-type-double+}")

(export 'g-param-spec-double)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_double ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_double" g-param-spec-double)
    (:pointer (:struct g-param-spec-double))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[minimum]{a @code{:double} minimum value for the property specified}
  @argument[maximum]{a @code{:double} maximum value for the property specified}
  @argument[default-value]{a @code{:double} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-double} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-double+} property.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-double}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-double+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :double)
  (maximum :double)
  (default-value :double)
  (flags g-param-flags))

(export 'g-param-spec-double)

;;; ----------------------------------------------------------------------------
;;; g_value_get_double ()
;;; g_value_set_double () -> g-value-double
;;; ----------------------------------------------------------------------------

(defun (setf g-value-double) (value gvalue)
  (foreign-funcall "g_value_set_double"
                   (:pointer (:struct g-value)) gvalue
                   :double value
                   :void)
  value)

(defcfun ("g_value_get_double" g-value-double) :double
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-double gvalue) => value}
  @syntax[]{(setf (g-value-double gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-double+}}
  @argument[value]{a @code{:double} double value}
  @begin{short}
    Double contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-double} gets the contents of a @symbol{g-value}
  of type @var{+g-type-double+}. The function @sym{(setf g-value-double)} sets
  the contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-double+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-double)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:enum-class (:pointer (:struct g-enum-class)))
  (:default-value :int))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-enum atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-enum atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    enum properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-enum
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:enum-class (:pointer g-enum-class))
  (:default-value :int))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:enum-class]{the @symbol{g-enum-class} structure for the enum.}
    @entry[:default-value]{a @code{:int} default value for the property
      specified.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-enum)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_enum ()
;;; ----------------------------------------------------------------------------

;; TODO:
;; This accepts any integer for default-value, but does not check for a valid
;; enum parameter. Can this be implemented much better?

(defcfun ("g_param_spec_enum" g-param-spec-enum)
    (:pointer (:struct g-param-spec-enum))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[enum-type]{a @class{g-type} derived from @var{+g-type-enum+}}
  @argument[default-value]{a @code{:int} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-enum} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-enum+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-enum}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-enum+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (enum-type g-type)
  (default-value :int)
  (flags g-param-flags))

(export 'g-param-spec-enum)

;;; ----------------------------------------------------------------------------
;;; g_value_get_enum ()
;;; g_value_set_enum () -> g-value-enum
;;; ----------------------------------------------------------------------------

;; TODO:
;; This accepts any integer, but does not check for a valid enum parameter.
;; Can this be implemented much better?

(defun (setf g-value-enum) (value gvalue)
  (foreign-funcall "g_value_set_enum"
                   (:pointer (:struct g-value)) gvalue
                   :int value
                   :void)
  value)

(defcfun ("g_value_get_enum" g-value-enum) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-enum gvalue) => value}
  @syntax[]{(setf (g-value-enum gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} whose type is derived from
    @var{+g-type-enum+}}
  @argument[value]{a @code{:int} enum value}
  @begin{short}
    Enum contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-enum} gets the contents of a @symbol{g-value}
  of type @var{+g-type-enum+}. The function @sym{(setf g-value-enum)} sets the
  contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-enum+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-enum)

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:flags-class (:pointer (:struct g-flags-class)))
  (:default-value :uint))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-flags atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-flags atdoc:*external-symbols*)
 "@version{2020-8-26}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    flags properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-flags
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:flags-class (:pointer g-flags-class))
  (:default-value :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:flags-class]{The @symbol{g-flags-class} structure for the flags.}
    @entry[:default-value]{a @code{:uint} default value for the property
      specified.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-flags)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_flags ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_flags" g-param-spec-flags)
    (:pointer (:struct g-param-spec-flags))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[flags-type]{a @class{g-type} derived from @var{+g-type-flags+}}
  @argument[default-value]{a @code{:uint} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-flags} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-flags+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-flags}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-flags+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (flags-type g-type)
  (default-value :uint)
  (flags g-param-flags))

(export 'g-param-spec-flags)

;;; ----------------------------------------------------------------------------
;;; g_value_get_flags ()
;;; g_value_set_flags () -> g-value-flags
;;; ----------------------------------------------------------------------------

(defun (setf g-value-flags) (value gvalue)
  (foreign-funcall "g_value_set_flags"
                   (:pointer (:struct g-value)) gvalue
                   :uint value
                   :void)
  value)

(defcfun ("g_value_get_flags" g-value-flags) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-flags gvalue) => value}
  @syntax[]{(setf (g-value-flags gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} whose type is derived from
    @var{+g-type-flags+}}
  @argument[value]{a @code{:uint} flags value}
  @begin{short}
    Flags contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-flags} gets the contents of a @symbol{g-value}
  of type @var{+g-type-flags+}. The function @sym{(setf g-value-flags)} sets
  the contents of a @symbol{g-value} to @arg{vaiue}.
  @see-symbol{g-value}
  @see-variable{+g-type-flags+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-flags)

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
;;; G_VALUE_IS_INTERNED_STRING()
;;;
;;; #define G_VALUE_IS_INTERNED_STRING(value)
;;;         (G_VALUE_HOLDS_STRING
;;;           (value) && ((value)->data[1].v_uint
;;;                   & G_VALUE_INTERNED_STRING)) GLIB_AVAILABLE_MACRO_IN_2_66
;;;
;;; Checks whether value contains a string which is canonical.
;;;
;;; value:
;;;     a valid GValue structure
;;;
;;; Returns:
;;;     TRUE if the value contains a string in its canonical representation, as
;;;     returned by g_intern_string(). See also g_value_set_interned_string().
;;;
;;; Since 2.66
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_STRING
;;;
;;; #define G_TYPE_PARAM_STRING (g_param_spec_types[14])
;;;
;;; The GType of GParamSpecString.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_VALUE_INTERNED_STRING
;;;
;;; #define G_VALUE_INTERNED_STRING (1 << 28) GLIB_AVAILABLE_MACRO_IN_2_66
;;;
;;; For string values, indicates that the string contained is canonical and will
;;; exist for the duration of the process. See g_value_set_interned_string().
;;;
;;; Since 2.66
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GParamSpecString
;;;
;;; TODO: This is taken from the C documentation. Check the implementation.
;;;
;;; struct GParamSpecString {
;;;   GParamSpec    parent_instance;
;;;   gchar        *default_value;
;;;   gchar        *cset_first;
;;;   gchar        *cset_nth;
;;;   gchar         substitutor;
;;;   guint         null_fold_if_empty : 1;
;;;   guint         ensure_non_null : 1;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for string
;;; properties.
;;;
;;; gchar *default_value :
;;;     default value for the property specified
;;;
;;; gchar *cset_first :
;;;     a string containing the allowed values for the first byte
;;;
;;; gchar *cset_nth :
;;;     a string containing the allowed values for the subsequent bytes
;;;
;;; gchar substitutor :
;;;     the replacement byte for bytes which don't match cset_first or cset_nth
;;;
;;; guint null_fold_if_empty : 1;
;;;     replace empty string by NULL
;;;
;;; guint ensure_non_null : 1;
;;;     replace NULL strings by an empty string
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-string
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:default-value (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-first (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-nth (:string :free-to-foreign nil :free-from-foreign nil))
  (:substitutor :char)
  ;; TODO: The definition in the C API is different. Check this.
  ;;;   guint         null_fold_if_empty : 1;
  ;;;   guint         ensure_non_null : 1;
  (:flags-for-null :uint))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-string atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-string atdoc:*external-symbols*)
 "@version{2020-8-27}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    string properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-string
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:default-value (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-first (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-nth (:string :free-to-foreign nil :free-from-foreign nil))
  (:substitutor :char)
  (:flags-for-null :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:default-value]{A @code{:string} default value for the property
      specified.}
    @entry[:cset-frist]{A @code{:string} containing the allowed values for the
      first byte.}
    @entry[:cset-nth]{A @code{:string} containing the allowed values for the
      subsequent bytes.}
    @entry[:substitutor]{A @code{:char} with the replacement byte for bytes
      which do not match @code{:cset-first} or @code{cset-nth}.}
    @entry[:flags-for-null]{A @code{:uint}, replace empty string by @code{NULL}
      and @code{NULL} strings by an empty string.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-string)

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

(defcfun ("g_param_spec_string" g-param-spec-string)
    (:pointer (:struct g-param-spec-string))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[default-value]{a @code{:string} default value for the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-string} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    type @var{+g-type-string+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-string}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-string+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (default-value :string)
  (flags g-param-flags))

(export 'g-param-spec-string)

;;; ----------------------------------------------------------------------------
;;; g_value_get_string ()
;;; g_value_set_string () -> g-value-string
;;; ----------------------------------------------------------------------------

(defun (setf g-value-string) (value gvalue)
  (foreign-funcall "g_value_set_string"
                   (:pointer (:struct g-value)) gvalue
                   :string value
                   :void)
  value)

(defcfun ("g_value_get_string" g-value-string) (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @syntax[]{(g-value-string gvalue) => value}
  @syntax[]{(setf (g-value-string gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-string+}}
  @argument[value]{caller-owned string to be duplicated for the
    @symbol{g-value}}
  @begin{short}
    String content of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-string} gets the contents of a @symbol{g-value}
  of type @var{+g-type-string+}. The function @sym{(setf g-value-string)} sets
  the contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-string+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-string)

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
;;; g_value_set_interned_string ()
;;;
;;; void
;;; g_value_set_interned_string (GValue *value,
;;;                              const gchar *v_string);
;;;
;;; Set the contents of a G_TYPE_STRING GValue to v_string . The string is
;;; assumed to be static and interned (canonical, for example from
;;; g_intern_string()), and is thus not duplicated when setting the GValue.
;;;
;;; value:
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; v_string:
;;;     static string to be set
;;;
;;; Since 2.66
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
  (:parent-instance (:pointer (:struct g-param-spec))))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-param atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-param atdoc:*external-symbols*)
 "@version{2020-8-27}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    properties of type @var{+g-type-param+}.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-param
  (:parent-instance (:pointer (:struct g-param-spec))))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-param)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_param ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_param" g-param-spec-param)
    (:pointer (:struct g-param-spec-param))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[param-type]{a @class{g-type} derived from @code{GParam}}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-param} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-param+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-param}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-param+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (param-type g-type)
  (flags g-param-flags))

(export 'g-param-spec-param)

;;; ----------------------------------------------------------------------------
;;; g_value_get_param ()
;;; g_value_set_param () -> g-value-param
;;; ----------------------------------------------------------------------------

(defun (setf g-value-param) (value gvalue)
  (foreign-funcall "g_value_set_param"
                   (:pointer (:struct g-value)) gvalue
                   (:pointer (:struct g-param-spec)) value
                   :void)
  value)

(defcfun ("g_value_get_param" g-value-param) (:pointer (:struct g-param-spec))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @syntax[]{(g-value-param gvalue) => value}
  @syntax[]{(setf (g-value-param gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} whose type is derived from
    @var{+g-value-param+}}
  @argument[value]{a @symbol{g-param-spec} value}
  @begin{short}
    @symbol{g-param-spec} content of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-param} gets the contents of a @symbol{g-value}
  of type @var{+g-type-param+}. The function @sym{(setf g-value-param)} sets
  the contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-param+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-param)

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
  (:parent-instance (:pointer (:struct g-param-spec))))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-boxed atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-boxed atdoc:*external-symbols*)
 "@version{2020-8-27}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    boxed properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-boxed
  (:parent-instance (:pointer (:struct g-param-spec))))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-boxed)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_boxed ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_boxed" g-param-spec-boxed)
    (:pointer (:struct g-param-spec-boxed))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @argument[name]{a @code{:string} canonical name of the property specified}
  @argument[nick]{a @code{:string} nick name for the property specified}
  @argument[blurb]{a @code{:string} description of the property specified}
  @argument[boxed-type]{@var{+g-type-boxed+} derived type of this property}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-boxed} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    derived of type @var{+g-type-boxed+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-boxed}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-boxed+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (boxed-type g-type)
  (flags g-param-flags))

(export 'g-param-spec-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_get_boxed ()
;;; g_value_set_boxed () -> g-value-boxed
;;; ----------------------------------------------------------------------------

(defun (setf g-value-boxed) (value gvalue)
  (foreign-funcall "g_value_set_boxed"
                   (:pointer (:struct g-value)) gvalue
                   :pointer value
                   :void)
  value)

(defcfun ("g_value_get_boxed" g-value-boxed) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @syntax[]{(g-value-boxed gvalue) => value}
  @syntax[]{(setf (g-value-boxed gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of @var{+g-type-boxed+} derived type}
  @argument[value]{a boxed value}
  @begin{short}
    Boxed contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-boxed} gets the contents of a @symbol{g-value}
  derived of the type @var{+g-type-boxed+}. The function
  @sym{(setf g-value-boxed)} sets the contents of a @symbol{g-value} to
  @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-boxed+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-boxed)

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
 "@version{2020-8-27}
  @argument[gvalue]{a @symbol{g-value} of @var{+g-type-boxed+} derived type}
  @argument[value]{duplicated unowned boxed value}
  @begin{short}
    Sets the contents of a @symbol{g-value} of dervied type @var{+g-type-boxed}
    to @arg{value}.
  @end{short}
  This function takes over the ownership of the callers reference to
  @arg{value}; the caller does not have to unref it any more.
  @see-symbol{g-value}
  @see-variable{+g-type-boxed+}
  @see-function{g-value-boxed}"
  (gvalue (:pointer (:struct g-value)))
  (value :pointer))

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
  (:parent-instance (:pointer (:struct g-param-spec))))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-pointer atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-pointer atdoc:*external-symbols*)
 "@version{2020-8-27}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    pointer properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-pointer
  (:parent-instance (:pointer (:struct g-param-spec))))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-pointer)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_pointer" g-param-spec-pointer)
    (:pointer (:struct g-param-spec-pointer))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @argument[name]{a @code{:string} with the canonical name of the property
    specified}
  @argument[nick]{a @code{:string} with the nick name for the property
    specified}
  @argument[blurb]{a @code{:string} with the description of the property
    specified}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-pointer} parameter
    specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of type @var{+g-type-pointer+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-pointer}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-pointer+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (flags g-param-flags))

(export 'g-param-spec-pointer)

;;; ----------------------------------------------------------------------------
;;; g_value_get_pointer ()
;;; g_value_set_pointer () -> g-value-pointer
;;; ----------------------------------------------------------------------------

(defun (setf g-value-pointer) (value gvalue)
  (foreign-funcall "g_value_set_pointer"
                   (:pointer (:struct g-value)) gvalue
                   :pointer value
                   :void)
  value)

(defcfun ("g_value_get_pointer" g-value-pointer) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @syntax[]{(g-value-pointer gvalue) => value}
  @syntax[]{(setf (g-value-pointer gvalue) value)}
  @argument[gvalue]{a valid @symbol{g-value} of @code{gpointer}}
  @argument[value]{pointer value to be set}
  @begin{short}
    Pointer contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-pointer} gets the contents of a @symbol{g-value}
  of type @var{+g-type-pointer+}. The function @sym{(setf g-value-pointer)}
  sets the contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-variable{+g-type-pointer+}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-pointer)

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
  (:parent-instance (:pointer (:struct g-param-spec))))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-object atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-object atdoc:*external-symbols*)
 "@version{2020-8-27}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    object properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-object
  (:parent-instance (:pointer (:struct g-param-spec))))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-object)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_object ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_object" g-param-spec-object)
    (:pointer (:struct g-param-spec-object))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @argument[name]{a @code{:string} with the canonical name of the property
    specified}
  @argument[nick]{a @code{:string} with the nick name for the property
    specified}
  @argument[blurb]{a @code{:string} with the description of the property
    specified}
  @argument[object-type]{@var{+g-type-object+} derived type of this property}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-object} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property
    of a dervived type @var{+g-type-object+}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-object}
  @see-symbol{g-param-flags}
  @see-variable{+g-type-object+}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (object-type g-type)
  (flags g-param-flags))

(export 'g-param-spec-object)

;;; ----------------------------------------------------------------------------
;;; g_value_get_object ()
;;; g_value_set_object () -> g-value-object
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_object" %g-value-set-object) :void
  (gvalue (:pointer (:struct g-value)))
  (value :pointer))

(defun (setf g-value-object) (value gvalue)
  (%g-value-set-object gvalue
                       (if value
                           (pointer value)
                           (null-pointer)))
  value)

(defcfun ("g_value_get_object" %g-value-get-object) :pointer
  (value (:pointer (:struct g-value))))

(defun g-value-object (gvalue)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-26}
  @syntax[]{(g-value-object gvalue) => value}
  @syntax[]{(setf (g-value-object gvalue) value)}
  @argument[gvalue]{a valid @symbol{g-value} of @var{+g-type-object+}
    derived type}
  @argument[value]{object value of derived type @var{+g-type-object+}}
  @begin{short}
    Object contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-object} gets the contents of a @symbol{g-value}
  of a derived type @var{+g-type-object+}. The function
  @sym{(setf g-value-object)} sets the contents of a @symbol{g-value} to
  @arg{value}.

  @sym{(setf g-value-object)} increases the reference count of @arg{value} (the
  @symbol{g-value} holds a reference to @arg{value}). If you do not wish to
  increase the reference count of the object (i.e. you wish to pass your
  current reference to the @symbol{g-value} because you no longer need it), use
  the function @fun{g-value-take-object} instead.

  It is important that your @symbol{g-value} holds a reference to @arg{value}
  (either its own, or one it has taken) to ensure that the object won't be
  destroyed while the @symbol{g-value} still exists).
  @see-symbol{g-value}
  @see-variable{+g-type-object+}"
  (get-g-object-for-pointer (%g-value-get-object gvalue)))

(export 'g-value-object)

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

;; We do not expot this structure.

(defcstruct g-param-spec-value-array
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:element-spec (:pointer (:struct g-param-spec)))
  (:fixed-n-elements :uint))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-value-array atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-value-array atdoc:*external-symbols*)
 "@version{2020-8-27}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    @code{GValueArray} properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-value-array
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:element-spec (:pointer g-param-spec))
  (:fixed-n-elements :uint))
  @end{pre}
  @begin[code]{table}
    @entry[:private-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:element-spec]{a @symbol{g-param-spec} describing the elements
      contained in arrays of this property, may be @code{NULL}}
    @entry[:fixed-n-elements]{a @code{:uint}, if greater than 0, arrays of this
      property will always have this many elements}
  @end{table}
  @see-symbol{g-param-spec}")

;;; ----------------------------------------------------------------------------
;;; g_param_spec_value_array ()
;;; ----------------------------------------------------------------------------

;; We dot not export this function.

(defcfun ("g_param_spec_value_array" g-param-spec-value-array)
    (:pointer (:struct g-param-spec-value-array))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @argument[name]{a @code{:string} with the canonical name of the property
    specified}
  @argument[nick]{a @code{:string} with the nick name for the property
    specified}
  @argument[blurb]{a @code{:string} with the description of the property
    specified}
  @argument[element-spec]{a @symbol{g-param-spec} describing the elements
    contained in arrays of this property, may be @code{NULL}}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-value-array} parameter
    specification.}
  @begin{short}
    Creates a new parameter specificytion instance specifying a
    @code{GValueArray} property. @code{GValueArray} is a @code{GBoxed} type, as
    such, @symbol{g-value} structures for this property can be accessed with
    @fun{g-value-set-boxed} and @fun{g-value-get-boxed}.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-value-array}
  @see-symbol{g-param-flags}
  @see-function{g-param-spec-internal}"
  (name :string)
  (nick :string)
  (blurb :string)
  (element-spec (:pointer (:struct g-param-spec)))
  (flags g-param-flags))

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
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:is-a-type g-type))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-param-spec-g-type atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'g-param-spec-g-type atdoc:*external-symbols*)
 "@version{2020-8-27}
  @begin{short}
    A @symbol{g-param-spec} derived structure that contains the meta data for
    @class{g-type} properties.
  @end{short}
  @begin{pre}
(defcstruct g-param-spec-g-type
  (:parent-instance (:pointer (:struct g-param-spec)))
  (:is-a-type g-type))
  @end{pre}
  @begin[code]{table}
    @entry[:parent-instance]{Private @symbol{g-param-spec} portion.}
    @entry[:is-a-type]{A @class{g-type} whose subtypes can occur as values.}
  @end{table}
  @see-symbol{g-param-spec}")

(export 'g-param-spec-g-type)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_gtype ()
;;; ----------------------------------------------------------------------------

;; TODO: The name is changed to g-param-spec-g-type !?

(defcfun ("g_param_spec_gtype" g-param-spec-g-type)
    (:pointer (:struct g-param-spec-g-type))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @argument[name]{a @code{:string} with the canonical name of the property
    specified}
  @argument[nick]{a @code{:string} with the nick name for the property
    specified}
  @argument[blurb]{a @code{:string} with the description of the property
    specified}
  @argument[is-a-type]{a @class{g-type} whose subtypes are allowed as values of
    the property (use @var{+g-type-none+} for any type)}
  @argument[flags]{flags of type @symbol{g-param-flags} for the property
    specified}
  @return{A newly created @symbol{g-param-spec-g-type} parameter specification.}
  @begin{short}
    Creates a new parameter specification instance specifying a property of
    type @var{G_TYPE_GTYPE} property.
  @end{short}

  See the function @fun{g-param-spec-internal} for details on property names.
  @see-symbol{g-param-spec-g-type}
  @see-symbol{g-param-flags}
  @see-function{g-param-spec-internal}"
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
  @argument[gvalue]{a valid @symbol{g-value} of type @code{G_TYPE_GTYPE}}
  @return{The @class{g-type} stored in @arg{gvalue}.}
  @short{Get the contents of a @code{G_TYPE_GTYPE} @symbol{g-value}.}

  Since 2.12"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-get-g-type)

;;; ----------------------------------------------------------------------------
;;; g_value_set_gtype ()
;;; ----------------------------------------------------------------------------

;; TODO: The name is changed to g-value-set-g-type !?

(defcfun ("g_value_set_gtype" g-value-set-g-type) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[gvalue]{a valid @symbol{g-value} of type @code{G_TYPE_GTYPE}}
  @argument[value]{@class{g-type} to be set}
  @begin{short}
    Set the contents of a @code{G_TYPE_GTYPE} @symbol{g-value} to @arg{value}.
  @end{short}

  Since 2.12"
  (gvalue (:pointer (:struct g-value)))
  (value g-type))

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
;;; g_value_set_variant () -> g-value-variant
;;; ----------------------------------------------------------------------------

(defun (setf g-value-variant) (value gvalue)
  (foreign-funcall "g_value_set_variant"
                   (:pointer (:struct g-value)) gvalue
                   (:pointer (:struct g-variant)) value
                   :void)
  value)

(defcfun ("g_value_get_variant" g-value-variant) (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-8-27}
  @syntax[]{(g-value-variant gvalue) => value}
  @syntax[]{(setf (g-value-variant gvalue) value)}
  @argument[gvalue]{a @symbol{g-value} of type @var{+g-type-variant+}}
  @argument[value]{a @sybmol{g-variant} value}
  @begin{short}
    Variant contents of @arg{gvalue}.
  @end{short}

  The function @sym{g-value-variant} gets the contents of a @symbol{g-value}
  of type @symbol{g-variant}. The function @sym{(setf g-value-variant)} sets
  the contents of a @symbol{g-value} to @arg{value}.
  @see-symbol{g-value}
  @see-symbol{g-variant}"
  (gvalue (:pointer (:struct g-value))))

(export 'g-value-variant)

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
