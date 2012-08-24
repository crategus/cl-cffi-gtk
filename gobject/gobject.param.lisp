;;; ----------------------------------------------------------------------------
;;; gobject.param.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GObject Reference Manual Version 2.32.4. See http://www.gtk.org
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
;;;
;;; struct GParamSpecBoolean {
;;;   GParamSpec    parent_instance;
;;;
;;;   gboolean      default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for boolean
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; gboolean default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-boolean
  (:parent-instance g-param-spec)
  (:default-value :boolean))

(export 'g-param-spec-boolean)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_boolean ()
;;;
;;; GParamSpec * g_param_spec_boolean (const gchar *name,
;;;                                    const gchar *nick,
;;;                                    const gchar *blurb,
;;;                                    gboolean default_value,
;;;                                    GParamFlags flags);
;;;
;;; Creates a new GParamSpecBoolean instance specifying a G_TYPE_BOOLEAN
;;; property.
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

(defcfun ("g_param_spec_boolean" g-param-spec-boolean)
    (:pointer g-param-spec-boolean)
  (name :string)
  (nick :string)
  (blurb :string)
  (default-value :boolean)
  (flags g-param-flags))

(export 'g-param-spec-boolean)

;;; ----------------------------------------------------------------------------
;;; g_value_set_boolean ()
;;;
;;; void g_value_set_boolean (GValue *value, gboolean v_boolean);
;;;
;;; Set the contents of a G_TYPE_BOOLEAN GValue to v_boolean.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_BOOLEAN
;;;
;;; v_boolean :
;;;     boolean value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_boolean" g-value-set-boolean) :void
  (value (:pointer g-value))
  (v-boolean :boolean))

(export 'g-value-set-boolean)

;;; ----------------------------------------------------------------------------
;;; g_value_get_boolean ()
;;;
;;; gboolean g_value_get_boolean (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_BOOLEAN GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_BOOLEAN
;;;
;;; Returns :
;;;     boolean contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_boolean" g-value-get-boolean) :boolean
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
;;;
;;; struct GParamSpecChar {
;;;   GParamSpec    parent_instance;
;;;
;;;   gint8         minimum;
;;;   gint8         maximum;
;;;   gint8         default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for character
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; gint8 minimum;
;;;     minimum value for the property specified
;;;
;;; gint8 maximum;
;;;     maximum value for the property specified
;;;
;;; gint8 default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-char
  (:parent-instance g-param-spec)
  (:minimum :int8)
  (:maximum :int8)
  (:default-value :int8))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_char ()
;;;
;;; GParamSpec * g_param_spec_char (const gchar *name,
;;;                                 const gchar *nick,
;;;                                 const gchar *blurb,
;;;                                 gint8 minimum,
;;;                                 gint8 maximum,
;;;                                 gint8 default_value,
;;;                                 GParamFlags flags);
;;;
;;; Creates a new GParamSpecChar instance specifying a G_TYPE_CHAR property.
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
;;; minimum :
;;;     minimum value for the property specified
;;;
;;; maximum :
;;;     maximum value for the property specified
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

(defcfun ("g_param_spec_char" g-param-spec-char) (:pointer g-param-spec-char)
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
;;;
;;; void g_value_set_char (GValue *value, gchar v_char);
;;;
;;; Warning
;;;
;;; g_value_set_char has been deprecated since version 2.32 and should not be
;;; used in newly-written code. This function's input type is broken, see
;;; g_value_set_schar()
;;;
;;; Set the contents of a G_TYPE_CHAR GValue to v_char.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_CHAR
;;;
;;; v_char :
;;;     character value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_char" g-value-set-char) :void
  (value (:pointer g-value))
  (v-char :char))

(export 'g-value-set-char)

;;; ----------------------------------------------------------------------------
;;; g_value_get_char ()
;;;
;;; gchar g_value_get_char (const GValue *value);
;;;
;;; Warning
;;;
;;; g_value_get_char has been deprecated since version 2.32 and should not be
;;; used in newly-written code. This function's return type is broken, see
;;; g_value_get_schar()
;;;
;;; Do not use this function; it is broken on platforms where the char type is
;;; unsigned, such as ARM and PowerPC. See g_value_get_schar().
;;;
;;; Get the contents of a G_TYPE_CHAR GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_CHAR
;;;
;;; Returns :
;;;     character contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_char" g-value-get-char) :char
  (value (:pointer g-value)))

(export 'g-value-get-char)

;;; ----------------------------------------------------------------------------
;;; g_value_get_schar ()
;;;
;;; gint8 g_value_get_schar (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_CHAR GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_CHAR
;;;
;;; Returns :
;;;     signed 8 bit integer contents of value
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_value_set_schar ()
;;;
;;; void g_value_set_schar (GValue *value, gint8 v_char);
;;;
;;; Set the contents of a G_TYPE_CHAR GValue to v_char.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_CHAR
;;;
;;; v_char :
;;;     signed 8 bit integer to be set
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

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
;;;
;;; struct GParamSpecUChar {
;;;   GParamSpec    parent_instance;
;;;
;;;   guint8        minimum;
;;;   guint8        maximum;
;;;   guint8        default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for unsigned
;;; character properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; guint8 minimum;
;;;     minimum value for the property specified
;;;
;;; guint8 maximum;
;;;     maximum value for the property specified
;;;
;;; guint8 default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-uchar
  (:parent-instance g-param-spec)
  (:minimum :uint8)
  (:maximum :uint8)
  (:default-value :uint8))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uchar ()
;;;
;;; GParamSpec * g_param_spec_uchar (const gchar *name,
;;;                                  const gchar *nick,
;;;                                  const gchar *blurb,
;;;                                  guint8 minimum,
;;;                                  guint8 maximum,
;;;                                  guint8 default_value,
;;;                                  GParamFlags flags);
;;;
;;; Creates a new GParamSpecUChar instance specifying a G_TYPE_UCHAR property.
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
;;; minimum :
;;;     minimum value for the property specified
;;;
;;; maximum :
;;;     maximum value for the property specified
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

(defcfun ("g_param_spec_uchar" g-param-spec-uchar) (:pointer g-param-spec-uchar)
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
;;;
;;; void g_value_set_uchar (GValue *value, guchar v_uchar);
;;;
;;; Set the contents of a G_TYPE_UCHAR GValue to v_uchar.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_UCHAR
;;;
;;; v_uchar :
;;;     unsigned character value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_uchar" g-value-set-uchar) :void
  (value (:pointer g-value))
  (v-uchar :uchar))

(export 'g-value-set-uchar)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uchar ()
;;;
;;; guchar g_value_get_uchar (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_UCHAR GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_UCHAR
;;;
;;; Returns :
;;;     unsigned character contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_uchar" g-value-get-uchar) :uchar
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
;;;
;;; struct GParamSpecInt {
;;;   GParamSpec    parent_instance;
;;;
;;;   gint          minimum;
;;;   gint          maximum;
;;;   gint          default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for integer
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; gint minimum;
;;;     minimum value for the property specified
;;;
;;; gint maximum;
;;;     maximum value for the property specified
;;;
;;; gint default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-int
  (:parent-instance g-param-spec)
  (:minimum :int)
  (:maximum :int)
  (:default-value :int))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_int ()
;;;
;;; GParamSpec * g_param_spec_int (const gchar *name,
;;;                                const gchar *nick,
;;;                                const gchar *blurb,
;;;                                gint minimum,
;;;                                gint maximum,
;;;                                gint default_value,
;;;                                GParamFlags flags);
;;;
;;; Creates a new GParamSpecInt instance specifying a G_TYPE_INT property.
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
;;; minimum :
;;;     minimum value for the property specified
;;;
;;; maximum :
;;;     maximum value for the property specified
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

(defcfun ("g_param_spec_int" g-param-spec-int) (:pointer g-param-spec-int)
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
;;;
;;; void g_value_set_int (GValue *value, gint v_int);
;;;
;;; Set the contents of a G_TYPE_INT GValue to v_int.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_INT
;;;
;;; v_int :
;;;     integer value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_int" g-value-set-int) :void
  (value (:pointer g-value))
  (v_int :int))

(export 'g-value-set-int)

;;; ----------------------------------------------------------------------------
;;; g_value_get_int ()
;;;
;;; gint g_value_get_int (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_INT GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_INT
;;;
;;; Returns :
;;;     integer contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_int" g-value-get-int) :int
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

(defcstruct g-param-spec-uint
  (:parent-instance g-param-spec)
  (:minimum :uint)
  (:maximum :uint)
  (:default-value :uint))

;;; ----------------------------------------------------------------------------
;;; G_TYPE_PARAM_UINT
;;;
;;; #define G_TYPE_PARAM_UINT (g_param_spec_types[4])
;;;
;;; The GType of GParamSpecUInt.
;;; struct GParamSpecUInt
;;;
;;; struct GParamSpecUInt {
;;;   GParamSpec    parent_instance;
;;;
;;;   guint         minimum;
;;;   guint         maximum;
;;;   guint         default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for unsigned
;;; integer properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; guint minimum;
;;;     minimum value for the property specified
;;;
;;; guint maximum;
;;;     maximum value for the property specified
;;;
;;; guint default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uint ()
;;;
;;; GParamSpec * g_param_spec_uint (const gchar *name,
;;;                                 const gchar *nick,
;;;                                 const gchar *blurb,
;;;                                 guint minimum,
;;;                                 guint maximum,
;;;                                 guint default_value,
;;;                                 GParamFlags flags);
;;;
;;; Creates a new GParamSpecUInt instance specifying a G_TYPE_UINT property.
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
;;; minimum :
;;;     minimum value for the property specified
;;;
;;; maximum :
;;;     maximum value for the property specified
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

(defcfun ("g_param_spec_uint" g-param-spec-uint) (:pointer g-param-spec-uint)
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
;;;
;;; void g_value_set_uint (GValue *value, guint v_uint);
;;;
;;; Set the contents of a G_TYPE_UINT GValue to v_uint.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_UINT
;;;
;;; v_uint :
;;;     unsigned integer value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_uint" g-value-set-uint) :void
  (value (:pointer g-value))
  (v-uint :uint))

(export 'g-value-set-uint)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uint ()
;;;
;;; guint g_value_get_uint (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_UINT GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_UINT
;;;
;;; Returns :
;;;     unsigned integer contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_uint" g-value-get-uint) :uint
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
;;;
;;; struct GParamSpecLong {
;;;   GParamSpec    parent_instance;
;;;
;;;   glong         minimum;
;;;   glong         maximum;
;;;   glong         default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for long integer
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; glong minimum;
;;;     minimum value for the property specified
;;;
;;; glong maximum;
;;;     maximum value for the property specified
;;;
;;; glong default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-long
  (:parent-instance g-param-spec)
  (:minimum :long)
  (:maximum :long)
  (:default-value :ulong))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_long ()
;;;
;;; GParamSpec * g_param_spec_long (const gchar *name,
;;;                                 const gchar *nick,
;;;                                 const gchar *blurb,
;;;                                 glong minimum,
;;;                                 glong maximum,
;;;                                 glong default_value,
;;;                                 GParamFlags flags);
;;;
;;; Creates a new GParamSpecLong instance specifying a G_TYPE_LONG property.
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
;;; minimum :
;;;     minimum value for the property specified
;;;
;;; maximum :
;;;     maximum value for the property specified
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

(defcfun ("g_param_spec_long" g-param-spec-long) (:pointer g-param-spec-long)
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
;;;
;;; void g_value_set_long (GValue *value, glong v_long);
;;;
;;; Set the contents of a G_TYPE_LONG GValue to v_long.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_LONG
;;;
;;; v_long :
;;;     long integer value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_long" g-value-set-long) :void
  (value (:pointer g-value))
  (v-long :long))

(export 'g-value-set-long)

;;; ----------------------------------------------------------------------------
;;; g_value_get_long ()
;;;
;;; glong g_value_get_long (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_LONG GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_LONG
;;;
;;; Returns :
;;;     long integer contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_long" g-value-get-long) :long
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
;;;
;;; struct GParamSpecULong {
;;;   GParamSpec    parent_instance;
;;;
;;;   gulong        minimum;
;;;   gulong        maximum;
;;;   gulong        default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for unsigned long
;;; integer properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; gulong minimum;
;;;     minimum value for the property specified
;;;
;;; gulong maximum;
;;;     maximum value for the property specified
;;;
;;; gulong default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-ulong
  (:parent-instance g-param-spec)
  (:minimum :ulong)
  (:maximum :ulong)
  (:default-value :ulong))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_ulong ()
;;;
;;; GParamSpec * g_param_spec_ulong (const gchar *name,
;;;                                  const gchar *nick,
;;;                                  const gchar *blurb,
;;;                                  gulong minimum,
;;;                                  gulong maximum,
;;;                                  gulong default_value,
;;;                                  GParamFlags flags);
;;;
;;; Creates a new GParamSpecULong instance specifying a G_TYPE_ULONG property.
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
;;; minimum :
;;;     minimum value for the property specified
;;;
;;; maximum :
;;;     maximum value for the property specified
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

(defcfun ("g_param_spec_ulong" g-param-spec-ulong) (:pointer g-param-spec-ulong)
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
;;;
;;; void g_value_set_ulong (GValue *value, gulong v_ulong);
;;;
;;; Set the contents of a G_TYPE_ULONG GValue to v_ulong.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_ULONG
;;;
;;; v_ulong :
;;;     unsigned long integer value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_ulong" g-value-set-ulong) :void
  (value (:pointer g-value))
  (v-long :ulong))

(export 'g-value-set-ulong)

;;; ----------------------------------------------------------------------------
;;; g_value_get_ulong ()
;;;
;;; gulong g_value_get_ulong (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_ULONG GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_ULONG
;;;
;;; Returns :
;;;     unsigned long integer contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_ulong" g-value-get-ulong) :ulong
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
;;;
;;; struct GParamSpecInt64 {
;;;   GParamSpec    parent_instance;
;;;
;;;   gint64        minimum;
;;;   gint64        maximum;
;;;   gint64        default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for 64bit integer
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; gint64 minimum;
;;;     minimum value for the property specified
;;;
;;; gint64 maximum;
;;;     maximum value for the property specified
;;;
;;; gint64 default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-int64
  (:parent-instance g-param-spec)
  (:minimum :int64)
  (:maximum :int64)
  (:default-value :int64))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_int64 ()
;;;
;;; GParamSpec * g_param_spec_int64 (const gchar *name,
;;;                                  const gchar *nick,
;;;                                  const gchar *blurb,
;;;                                  gint64 minimum,
;;;                                  gint64 maximum,
;;;                                  gint64 default_value,
;;;                                  GParamFlags flags);
;;;
;;; Creates a new GParamSpecInt64 instance specifying a G_TYPE_INT64 property.
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
;;; minimum :
;;;     minimum value for the property specified
;;;
;;; maximum :
;;;     maximum value for the property specified
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

(defcfun ("g_param_spec_int64" g-param-spec-int64) (:pointer g-param-spec-int64)
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
;;;
;;; void g_value_set_int64 (GValue *value, gint64 v_int64);
;;;
;;; Set the contents of a G_TYPE_INT64 GValue to v_int64.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_INT64
;;;
;;; v_int64 :
;;;     64bit integer value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_int64" g-value-set-int64) :void
  (value (:pointer g-value))
  (v-int64 :int64))

(export 'g-value-set-int64)

;;; ----------------------------------------------------------------------------
;;; g_value_get_int64 ()
;;;
;;; gint64 g_value_get_int64 (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_INT64 GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_INT64
;;;
;;; Returns :
;;;     64bit integer contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_int64" g-value-get-int64) :int64
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
;;;
;;; struct GParamSpecUInt64 {
;;;   GParamSpec    parent_instance;
;;;
;;;   guint64       minimum;
;;;   guint64       maximum;
;;;   guint64       default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for unsigned
;;; 64bit integer properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; guint64 minimum;
;;;     minimum value for the property specified
;;;
;;; guint64 maximum;
;;;     maximum value for the property specified
;;;
;;; guint64 default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-uint64
  (:parent-instance g-param-spec)
  (:minimum :uint64)
  (:maximum :uint64)
  (:default-value :uint64))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_uint64 ()
;;;
;;; GParamSpec * g_param_spec_uint64 (const gchar *name,
;;;                                   const gchar *nick,
;;;                                   const gchar *blurb,
;;;                                   guint64 minimum,
;;;                                   guint64 maximum,
;;;                                   guint64 default_value,
;;;                                   GParamFlags flags);
;;;
;;; Creates a new GParamSpecUInt64 instance specifying a G_TYPE_UINT64 property.
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
;;; minimum :
;;;     minimum value for the property specified
;;;
;;; maximum :
;;;     maximum value for the property specified
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

(defcfun ("g_param_spec_uint64" g-param-spec-uint64)
    (:pointer g-param-spec-uint64)
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
;;;
;;; void g_value_set_uint64 (GValue *value, guint64 v_uint64);
;;;
;;; Set the contents of a G_TYPE_UINT64 GValue to v_uint64.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_UINT64
;;;
;;; v_uint64 :
;;;     unsigned 64bit integer value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_uint64" g-value-set-uint64) :void
  (value (:pointer g-value))
  (v-uint64 :uint64))

(export 'g-value-set-uint64)

;;; ----------------------------------------------------------------------------
;;; g_value_get_uint64 ()
;;;
;;; guint64 g_value_get_uint64 (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_UINT64 GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_UINT64
;;;
;;; Returns :
;;;     unsigned 64bit integer contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_uint64" g-value-get-uint64) :uint64
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
;;;
;;; struct GParamSpecFloat {
;;;   GParamSpec    parent_instance;
;;;
;;;   gfloat        minimum;
;;;   gfloat        maximum;
;;;   gfloat        default_value;
;;;   gfloat        epsilon;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for float
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; gfloat minimum;
;;;     minimum value for the property specified
;;;
;;; gfloat maximum;
;;;     maximum value for the property specified
;;;
;;; gfloat default_value;
;;;     default value for the property specified
;;;
;;; gfloat epsilon;
;;;     values closer than epsilon will be considered identical by
;;;     g_param_values_cmp(); the default value is 1e-30.
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-float
  (:parent-instance g-param-spec)
  (:minimum :float)
  (:maximum :float)
  (:default-value :float)
  (:epsilon :float))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_float ()
;;;
;;; GParamSpec * g_param_spec_float (const gchar *name,
;;;                                  const gchar *nick,
;;;                                  const gchar *blurb,
;;;                                  gfloat minimum,
;;;                                  gfloat maximum,
;;;                                  gfloat default_value,
;;;                                  GParamFlags flags);
;;;
;;; Creates a new GParamSpecFloat instance specifying a G_TYPE_FLOAT property.
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
;;; minimum :
;;;     minimum value for the property specified
;;;
;;; maximum :
;;;     maximum value for the property specified
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

(defcfun ("g_param_spec_float" g-param-spec-float) (:pointer g-param-spec-float)
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
;;;
;;; void g_value_set_float (GValue *value, gfloat v_float);
;;;
;;; Set the contents of a G_TYPE_FLOAT GValue to v_float.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_FLOAT
;;;
;;; v_float :
;;;     float value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_float" g-value-set-float) :void
  (value (:pointer g-value))
  (v-float :float))

(export 'g-value-set-float)

;;; ----------------------------------------------------------------------------
;;; g_value_get_float ()
;;;
;;; gfloat g_value_get_float (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_FLOAT GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_FLOAT
;;;
;;; Returns :
;;;     float contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_float" g-value-get-float) :float
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
;;;
;;; struct GParamSpecDouble {
;;;   GParamSpec    parent_instance;
;;;
;;;   gdouble       minimum;
;;;   gdouble       maximum;
;;;   gdouble       default_value;
;;;   gdouble       epsilon;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for double
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; gdouble minimum;
;;;     minimum value for the property specified
;;;
;;; gdouble maximum;
;;;     maximum value for the property specified
;;;
;;; gdouble default_value;
;;;     default value for the property specified
;;;
;;; gdouble epsilon;
;;;     values closer than epsilon will be considered identical by
;;;     g_param_values_cmp(); the default value is 1e-90.
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-double
  (:parent-instance g-param-spec)
  (:minimum :double)
  (:maximum :double)
  (:default-value :double)
  (:epsilon :double))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_double ()
;;;
;;; GParamSpec * g_param_spec_double (const gchar *name,
;;;                                   const gchar *nick,
;;;                                   const gchar *blurb,
;;;                                   gdouble minimum,
;;;                                   gdouble maximum,
;;;                                   gdouble default_value,
;;;                                   GParamFlags flags);
;;;
;;; Creates a new GParamSpecDouble instance specifying a G_TYPE_DOUBLE property.
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
;;; minimum :
;;;     minimum value for the property specified
;;;
;;; maximum :
;;;     maximum value for the property specified
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

(defcfun ("g_param_spec_double" g-param-spec-double)
    (:pointer g-param-spec-double)
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
;;;
;;; void g_value_set_double (GValue *value, gdouble v_double);
;;;
;;; Set the contents of a G_TYPE_DOUBLE GValue to v_double.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_DOUBLE
;;;
;;; v_double :
;;;     double value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_double" g-value-set-double) :void
  (value (:pointer g-value))
  (v-double :double))

(export 'g-value-set-double)

;;; ----------------------------------------------------------------------------
;;; g_value_get_double ()
;;;
;;; gdouble g_value_get_double (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_DOUBLE GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_DOUBLE
;;;
;;; Returns :
;;;     double contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_double" g-value-get-double) :double
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
;;;
;;; struct GParamSpecEnum {
;;;   GParamSpec    parent_instance;
;;;
;;;   GEnumClass   *enum_class;
;;;   gint          default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for enum
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; GEnumClass *enum_class;
;;;     the GEnumClass for the enum
;;;
;;; gint default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-enum
  (:parent-instance g-param-spec)
  (:enum-class (:pointer g-enum-class))
  (:default-value :int))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_enum ()
;;;
;;; GParamSpec * g_param_spec_enum (const gchar *name,
;;;                                 const gchar *nick,
;;;                                 const gchar *blurb,
;;;                                 GType enum_type,
;;;                                 gint default_value,
;;;                                 GParamFlags flags);
;;;
;;; Creates a new GParamSpecEnum instance specifying a G_TYPE_ENUM property.
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
;;; enum_type :
;;;     a GType derived from G_TYPE_ENUM
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

(defcfun ("g_param_spec_enum" g-param-spec-enum) (:pointer g-param-spec-enum)
  (name :string)
  (nick :string)
  (blurb :string)
  (enum-type g-type)
  (default-value :int)
  (flags g-param-flags))

(export 'g-param-spec-enum)

;;; ----------------------------------------------------------------------------
;;; g_value_set_enum ()
;;;
;;; void g_value_set_enum (GValue *value, gint v_enum);
;;;
;;; Set the contents of a G_TYPE_ENUM GValue to v_enum.
;;;
;;; value :
;;;     a valid GValue whose type is derived from G_TYPE_ENUM
;;;
;;; v_enum :
;;;     enum value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_enum" g-value-set-enum) :void
  (value (:pointer g-value))
  (v-enum :int))

(export 'g-value-set-enum)

;;; ----------------------------------------------------------------------------
;;; g_value_get_enum ()
;;;
;;; gint g_value_get_enum (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_ENUM GValue.
;;;
;;; value :
;;;     a valid GValue whose type is derived from G_TYPE_ENUM
;;;
;;; Returns :
;;;     enum contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_enum" g-value-get-enum) :int
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
;;;
;;; struct GParamSpecFlags {
;;;   GParamSpec    parent_instance;
;;;
;;;   GFlagsClass  *flags_class;
;;;   guint         default_value;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for flags
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; GFlagsClass *flags_class;
;;;     the GFlagsClass for the flags
;;;
;;; guint default_value;
;;;     default value for the property specified
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-flags
  (:parent-instance g-param-spec)
  (:flags-class (:pointer g-flags-class))
  (:default-value :uint))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_flags ()
;;;
;;; GParamSpec * g_param_spec_flags (const gchar *name,
;;;                                  const gchar *nick,
;;;                                  const gchar *blurb,
;;;                                  GType flags_type,
;;;                                  guint default_value,
;;;                                  GParamFlags flags);
;;;
;;; Creates a new GParamSpecFlags instance specifying a G_TYPE_FLAGS property.
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
;;; flags_type :
;;;     a GType derived from G_TYPE_FLAGS
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

(defcfun ("g_param_spec_flags" g-param-spec-flags) (:pointer g-param-spec-flags)
  (name :string)
  (nick :string)
  (blurb :string)
  (flags-type g-type)
  (default-value :int)
  (flags g-param-flags))

(export 'g-param-spec-flags)

;;; ----------------------------------------------------------------------------
;;; g_value_set_flags ()
;;;
;;; void g_value_set_flags (GValue *value, guint v_flags);
;;;
;;; Set the contents of a G_TYPE_FLAGS GValue to v_flags.
;;;
;;; value :
;;;     a valid GValue whose type is derived from G_TYPE_FLAGS
;;;
;;; v_flags :
;;;     flags value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_flags" g-value-set-flags) :void
  (value (:pointer g-value))
  (v-flags :int))

(export 'g-value-set-flags)

;;; ----------------------------------------------------------------------------
;;; g_value_get_flags ()
;;;
;;; guint g_value_get_flags (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_FLAGS GValue.
;;;
;;; value :
;;;     a valid GValue whose type is derived from G_TYPE_FLAGS
;;;
;;; Returns :
;;;     flags contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_flags" g-value-get-flags) :int
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
;;;
;;; struct GParamSpecString {
;;;   GParamSpec    parent_instance;
;;;
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
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; gchar *default_value;
;;;     default value for the property specified
;;;
;;; gchar *cset_first;
;;;     a string containing the allowed values for the first byte
;;;
;;; gchar *cset_nth;
;;;     a string containing the allowed values for the subsequent bytes
;;;
;;; gchar substitutor;
;;;     the replacement byte for bytes which don't match cset_first or cset_nth.
;;;
;;; guint null_fold_if_empty : 1;
;;;     replace empty string by NULL
;;;
;;; guint ensure_non_null : 1;
;;;     replace NULL strings by an empty string
;;; gchararray
;;;
;;; typedef gchar* gchararray;
;;;
;;; A C representable type name for G_TYPE_STRING.
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-string
  (:parent-instance g-param-spec)
  (:default-value (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-first (:string :free-to-foreign nil :free-from-foreign nil))
  (:cset-nth (:string :free-to-foreign nil :free-from-foreign nil))
  (:substitutor :char)
  (:flags-for-null :uint))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_string ()
;;;
;;; GParamSpec * g_param_spec_string (const gchar *name,
;;;                                   const gchar *nick,
;;;                                   const gchar *blurb,
;;;                                   const gchar *default_value,
;;;                                   GParamFlags flags);
;;;
;;; Creates a new GParamSpecString instance.
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

(defcfun g-param-spec-string (:pointer g-param-spec-string)
  (name :string)
  (nick :string)
  (blurb :string)
  (default-value :string)
  (flags g-param-flags))

(export 'g-param-spec-string)

;;; ----------------------------------------------------------------------------
;;; g_value_set_string ()
;;;
;;; void g_value_set_string (GValue *value, const gchar *v_string);
;;;
;;; Set the contents of a G_TYPE_STRING GValue to v_string.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; v_string :
;;;     caller-owned string to be duplicated for the GValue
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_string" g-value-set-string) :void
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
;;;
;;; const gchar * g_value_get_string (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_STRING GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_STRING
;;;
;;; Returns :
;;;     string content of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_string" g-value-get-string)
    (:string :free-from-foreign nil)
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
;;;
;;; struct GParamSpecParam {
;;;   GParamSpec    parent_instance;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for G_TYPE_PARAM
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-param
  (:parent-instance g-param-spec))

(export 'g-param-spec-param)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_param ()
;;;
;;; GParamSpec * g_param_spec_param (const gchar *name,
;;;                                  const gchar *nick,
;;;                                  const gchar *blurb,
;;;                                  GType param_type,
;;;                                  GParamFlags flags);
;;;
;;; Creates a new GParamSpecParam instance specifying a G_TYPE_PARAM property.
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
;;; param_type :
;;;     a GType derived from G_TYPE_PARAM
;;;
;;; flags :
;;;     flags for the property specified
;;;
;;; Returns :
;;;     a newly created parameter specification
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_param" g-param-spec-param) (:pointer g-param-spec-param)
  (name :string)
  (nick :string)
  (blurb :string)
  (param-type g-type)
  (flags g-param-flags))

(export 'g-param-spec-param)

;;; ----------------------------------------------------------------------------
;;; g_value_set_param ()
;;;
;;; void g_value_set_param (GValue *value, GParamSpec *param);
;;;
;;; Set the contents of a G_TYPE_PARAM GValue to param.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_PARAM
;;;
;;; param :
;;;     the GParamSpec to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_param" g-value-set-param) :void
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
;;;
;;; GParamSpec * g_value_get_param (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_PARAM GValue.
;;;
;;; value :
;;;     a valid GValue whose type is derived from G_TYPE_PARAM
;;;
;;; Returns :
;;;     GParamSpec content of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_param" g-value-get-param) (:pointer g-param-spec)
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
;;;
;;; struct GParamSpecBoxed {
;;;   GParamSpec    parent_instance;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for boxed
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-boxed
  (:parent-instance g-param-spec))

(export 'g-param-spec-boxed)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_boxed ()
;;;
;;; GParamSpec * g_param_spec_boxed (const gchar *name,
;;;                                  const gchar *nick,
;;;                                  const gchar *blurb,
;;;                                  GType boxed_type,
;;;                                  GParamFlags flags);
;;;
;;; Creates a new GParamSpecBoxed instance specifying a G_TYPE_BOXED derived
;;; property.
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
;;; boxed_type :
;;;     G_TYPE_BOXED derived type of this property
;;;
;;; flags :
;;;     flags for the property specified
;;;
;;; Returns :
;;;     a newly created parameter specification
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_boxed" g-param-spec-boxed) (:pointer g-param-spec-boxed)
  (name :string)
  (nick :string)
  (blurb :string)
  (boxed-type g-type)
  (flags g-param-flags))

(export 'g-param-spec-boxed)

;;; ----------------------------------------------------------------------------
;;; g_value_set_boxed ()
;;;
;;; void g_value_set_boxed (GValue *value, gconstpointer v_boxed);
;;;
;;; Set the contents of a G_TYPE_BOXED derived GValue to v_boxed.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_BOXED derived type
;;;
;;; v_boxed :
;;;     boxed value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_boxed" g-value-set-boxed) :void
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
;;;
;;; void g_value_take_boxed (GValue *value, gconstpointer v_boxed);
;;;
;;; Sets the contents of a G_TYPE_BOXED derived GValue to v_boxed and takes over
;;; the ownership of the callers reference to v_boxed; the caller doesn't have
;;; to unref it any more.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_BOXED derived type
;;;
;;; v_boxed :
;;;     duplicated unowned boxed value to be set
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_take_boxed" g-value-take-boxed) :void
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
;;;
;;; gpointer g_value_get_boxed (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_BOXED derived GValue.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_BOXED derived type
;;;
;;; Returns :
;;;     boxed contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_boxed" g-value-get-boxed) :pointer
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
;;;
;;; struct GParamSpecPointer {
;;;   GParamSpec    parent_instance;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for pointer
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-pointer
  (:parent-instance g-param-spec))

(export 'g-param-spec-pointer)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_pointer ()
;;;
;;; GParamSpec * g_param_spec_pointer (const gchar *name,
;;;                                    const gchar *nick,
;;;                                    const gchar *blurb,
;;;                                    GParamFlags flags);
;;;
;;; Creates a new GParamSpecPointer instance specifying a pointer property.
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
;;; flags :
;;;     flags for the property specified
;;;
;;; Returns :
;;;     a newly created parameter specification
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_pointer" g-param-spec-pointer)
    (:pointer g-param-spec-pointer)
  (name :string)
  (nick :string)
  (blurb :string)
  (flags g-param-flags))

(export 'g-param-spec-pointer)

;;; ----------------------------------------------------------------------------
;;; g_value_set_pointer ()
;;;
;;; void g_value_set_pointer (GValue *value, gpointer v_pointer);
;;;
;;; Set the contents of a pointer GValue to v_pointer.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_POINTER
;;;
;;; v_pointer :
;;;     pointer value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_pointer" g-value-set-pointer) :void
  (value (:pointer g-value))
  (v-pointer :pointer))

(export 'g-value-set-pointer)

;;; ----------------------------------------------------------------------------
;;; g_value_get_pointer ()
;;;
;;; gpointer g_value_get_pointer (const GValue *value);
;;;
;;; Get the contents of a pointer GValue.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_POINTER
;;;
;;; Returns :
;;;     pointer contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_pointer" g-value-get-pointer) :pointer
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
;;;
;;; struct GParamSpecObject {
;;;   GParamSpec    parent_instance;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for object
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-object
  (:parent-instance g-param-spec))

(export 'g-param-spec-object)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_object ()
;;;
;;; GParamSpec * g_param_spec_object (const gchar *name,
;;;                                   const gchar *nick,
;;;                                   const gchar *blurb,
;;;                                   GType object_type,
;;;                                   GParamFlags flags);
;;;
;;; Creates a new GParamSpecBoxed instance specifying a G_TYPE_OBJECT derived
;;; property.
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
;;; object_type :
;;;     G_TYPE_OBJECT derived type of this property
;;;
;;; flags :
;;;     flags for the property specified
;;;
;;; Returns :
;;;     a newly created parameter specification
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_object" g-param-spec-object)
    (:pointer g-param-spec-object)
  (name :string)
  (nick :string)
  (blurb :string)
  (object-type g-type)
  (flags g-param-flags))

(export 'g-param-spec-object)

;;; ----------------------------------------------------------------------------
;;; g_value_set_object ()
;;;
;;; void g_value_set_object (GValue *value, gpointer v_object);
;;;
;;; Set the contents of a G_TYPE_OBJECT derived GValue to v_object.
;;;
;;; g_value_set_object() increases the reference count of v_object (the GValue
;;; holds a reference to v_object). If you do not wish to increase the reference
;;; count of the object (i.e. you wish to pass your current reference to the
;;; GValue because you no longer need it), use g_value_take_object() instead.
;;;
;;; It is important that your GValue holds a reference to v_object (either its
;;; own, or one it has taken) to ensure that the object won't be destroyed while
;;; the GValue still exists).
;;;
;;; value :
;;;     a valid GValue of G_TYPE_OBJECT derived type
;;;
;;; v_object :
;;;     object value to be set
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_object" %g-value-set-object) :void
  (value (:pointer g-value))
  (v-object :pointer))

(defun g-value-set-object (value v-object)
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
;;;
;;; gpointer g_value_get_object (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_OBJECT derived GValue.
;;;
;;; value :
;;;     a valid GValue of G_TYPE_OBJECT derived type
;;;
;;; Returns :
;;;     object contents of value
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_object" %g-value-get-object) :pointer
  (value (:pointer g-value)))

;; TODO: %g-value-get-object returns a pointer. The pointer is translated
;;       with get-object-for-pointer to a Lisp object. The transformation
;;       should be done automatically when specifing the type g-object for
;;       the return value.

(defun g-value-get-object (value)
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
;;;
;;; struct GParamSpecValueArray {
;;;   GParamSpec    parent_instance;
;;;   GParamSpec   *element_spec;
;;;   guint        fixed_n_elements;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for GValueArray
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; GParamSpec *element_spec;
;;;     a GParamSpec describing the elements contained in arrays of this
;;;     property, may be NULL
;;;
;;; guint fixed_n_elements;
;;;     if greater than 0, arrays of this property will always have this many
;;;     elements
;;; ----------------------------------------------------------------------------

(defcstruct g-param-spec-value-array
  (:parent-instance g-param-spec)
  (:element-spec (:pointer g-param-spec))
  (:fixed-n-elements :uint))

;;; ----------------------------------------------------------------------------
;;; g_param_spec_value_array ()
;;;
;;; GParamSpec * g_param_spec_value_array (const gchar *name,
;;;                                        const gchar *nick,
;;;                                        const gchar *blurb,
;;;                                        GParamSpec *element_spec,
;;;                                        GParamFlags flags);
;;;
;;; Creates a new GParamSpecValueArray instance specifying a G_TYPE_VALUE_ARRAY
;;; property. G_TYPE_VALUE_ARRAY is a G_TYPE_BOXED type, as such, GValue
;;; structures for this property can be accessed with g_value_set_boxed() and
;;; g_value_get_boxed().
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
;;; element_spec :
;;;     a GParamSpec describing the elements contained in arrays of this
;;;     property, may be NULL
;;;
;;; flags :
;;;     flags for the property specified
;;;
;;; Returns :
;;;     a newly created parameter specification
;;; ----------------------------------------------------------------------------

(defcfun ("g_param_spec_value_array" g-param-spec-value-array)
    (:pointer g-param-spec-value-array)
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
;;;
;;; struct GParamSpecGType {
;;;   GParamSpec    parent_instance;
;;;   GType         is_a_type;
;;; };
;;;
;;; A GParamSpec derived structure that contains the meta data for GType
;;; properties.
;;;
;;; GParamSpec parent_instance;
;;;     private GParamSpec portion
;;;
;;; GType is_a_type;
;;;     a GType whose subtypes can occur as values
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;; TODO: The name is changed to g-param-spec-g-type !?

(defcstruct g-param-spec-g-type
  (:parent-instance g-param-spec)
  (:types-root g-type))

(export 'g-param-spec-g-type)

;;; ----------------------------------------------------------------------------
;;; g_param_spec_gtype ()
;;;
;;; GParamSpec * g_param_spec_gtype (const gchar *name,
;;;                                  const gchar *nick,
;;;                                  const gchar *blurb,
;;;                                  GType is_a_type,
;;;                                  GParamFlags flags);
;;;
;;; Creates a new GParamSpecGType instance specifying a G_TYPE_GTYPE property.
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
;;; is_a_type :
;;;     a GType whose subtypes are allowed as values of the property (use
;;;     G_TYPE_NONE for any type)
;;;
;;; flags :
;;;     flags for the property specified
;;;
;;; Returns :
;;;     a newly created parameter specification
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;; TODO: The name is changed to g-param-spec-g-type !?

(defcfun ("g_param_spec_gtype" g-param-spec-g-type)
    (:pointer g-param-spec-g-type)
  (name :string)
  (nick :string)
  (blurb :string)
  (types-root g-type)
  (flags g-param-flags))

(export 'g-param-spec-g-type)

;;; ----------------------------------------------------------------------------
;;; g_value_get_gtype ()
;;;
;;; GType g_value_get_gtype (const GValue *value);
;;;
;;; Get the contents of a G_TYPE_GTYPE GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_GTYPE
;;;
;;; Returns :
;;;     the GType stored in value
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;; TODO: The name is changed to g-value-get-g-type !?

(defcfun ("g_value_get_gtype" g-value-get-g-type) g-type
  (g-value (:pointer g-value)))

(export 'g-value-get-g-type)

;;; ----------------------------------------------------------------------------
;;; g_value_set_gtype ()
;;;
;;; void g_value_set_gtype (GValue *value, GType v_gtype);
;;;
;;; Set the contents of a G_TYPE_GTYPE GValue to v_gtype.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_GTYPE
;;;
;;; v_gtype :
;;;     GType to be set
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;; TODO: The name is changed to g-value-set-g-type !?

(defcfun ("g_value_set_gtype" g-value-set-g-type) :void
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
;;;
;;; GVariant * g_value_get_variant (const GValue *value);
;;;
;;; Get the contents of a variant GValue.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_VARIANT
;;;
;;; Returns :
;;;     variant contents of value
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_get_variant" g-value-get-variant) (:pointer g-variant)
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
;;;
;;; void g_value_set_variant (GValue *value, GVariant *variant);
;;;
;;; Set the contents of a variant GValue to variant. If the variant is floating,
;;; it is consumed.
;;;
;;; value :
;;;     a valid GValue of type G_TYPE_VARIANT
;;;
;;; variant :
;;;     a GVariant, or NULL
;;;
;;; Since 2.26
;;; ----------------------------------------------------------------------------

(defcfun ("g_value_set_variant" g-value-set-variant) :void
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
