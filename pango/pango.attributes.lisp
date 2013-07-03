;;; ----------------------------------------------------------------------------
;;; pango.attributes.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the Pango Reference Manual
;;; for Pango 1.30.0. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Text Attributes - Font and other attributes for annotating text
;;;
;;; Synopsis
;;;
;;;     PangoAttrType
;;;     PangoAttrClass
;;;     PangoAttribute
;;;     PangoAttrString
;;;     PangoAttrLanguage
;;;     PangoAttrColor
;;;     PangoAttrInt
;;;     PangoAttrFloat
;;;     PangoAttrFontDesc
;;;     PangoAttrShape
;;;     PangoAttrSize
;;;
;;;     pango_parse_markup
;;;     pango_attr_type_register
;;;     pango_attr_type_get_name
;;;     pango_attribute_init
;;;     pango_attribute_copy
;;;     pango_attribute_equal
;;;     pango_attribute_destroy
;;;
;;;     pango_attr_language_new
;;;     pango_attr_family_new
;;;     pango_attr_style_new
;;;     pango_attr_variant_new
;;;     pango_attr_stretch_new
;;;     pango_attr_weight_new
;;;     pango_attr_size_new
;;;     pango_attr_size_new_absolute
;;;     pango_attr_font_desc_new
;;;     pango_attr_foreground_new
;;;     pango_attr_background_new
;;;     pango_attr_strikethrough_new
;;;     pango_attr_strikethrough_color_new
;;;     pango_attr_underline_new
;;;     pango_attr_underline_color_new
;;;
;;;     PangoUnderline
;;;
;;;     pango_attr_shape_new
;;;     pango_attr_shape_new_with_data
;;;     pango_attr_scale_new
;;;     PANGO_SCALE_XX_SMALL
;;;     PANGO_SCALE_X_SMALL
;;;     PANGO_SCALE_SMALL
;;;     PANGO_SCALE_MEDIUM
;;;     PANGO_SCALE_LARGE
;;;     PANGO_SCALE_X_LARGE
;;;     PANGO_SCALE_XX_LARGE
;;;     pango_attr_rise_new
;;;     pango_attr_letter_spacing_new
;;;     pango_attr_fallback_new
;;;     pango_attr_gravity_new
;;;     pango_attr_gravity_hint_new
;;;
;;;     PangoColor
;;;
;;;     pango_color_parse
;;;     pango_color_copy
;;;     pango_color_free
;;;     pango_color_to_string
;;;
;;;     PangoAttrList
;;;
;;;     pango_attr_list_new
;;;     pango_attr_list_ref
;;;     pango_attr_list_unref
;;;     pango_attr_list_copy
;;;     pango_attr_list_insert
;;;     pango_attr_list_insert_before
;;;     pango_attr_list_change
;;;     pango_attr_list_splice
;;;     pango_attr_list_filter
;;;     pango_attr_list_get_iterator
;;;
;;;     PangoAttrIterator
;;;
;;;     pango_attr_iterator_copy
;;;     pango_attr_iterator_next
;;;     pango_attr_iterator_range
;;;     pango_attr_iterator_get
;;;     pango_attr_iterator_get_font
;;;     pango_attr_iterator_get_attrs
;;;     pango_attr_iterator_destroy
;;;
;;; Object Hierarchy
;;;
;;;   GEnum
;;;    +----PangoAttrType
;;;
;;;   GEnum
;;;    +----PangoUnderline
;;;
;;;   GBoxed
;;;    +----PangoColor
;;;
;;;   GBoxed
;;;    +----PangoAttrList
;;;
;;; Description
;;;
;;; Attributed text is used in a number of places in Pango. It is used as the
;;; input to the itemization process and also when creating a PangoLayout. The
;;; data types and functions in this section are used to represent and
;;; manipulate sets of attributes applied to a portion of text.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; enum PangoAttrType
;;;
;;; typedef enum {
;;;   PANGO_ATTR_INVALID,             /* 0 is an invalid attribute type */
;;;   PANGO_ATTR_LANGUAGE,            /* PangoAttrLanguage */
;;;   PANGO_ATTR_FAMILY,              /* PangoAttrString */
;;;   PANGO_ATTR_STYLE,               /* PangoAttrInt */
;;;   PANGO_ATTR_WEIGHT,              /* PangoAttrInt */
;;;   PANGO_ATTR_VARIANT,             /* PangoAttrInt */
;;;   PANGO_ATTR_STRETCH,             /* PangoAttrInt */
;;;   PANGO_ATTR_SIZE,                /* PangoAttrSize */
;;;   PANGO_ATTR_FONT_DESC,           /* PangoAttrFontDesc */
;;;   PANGO_ATTR_FOREGROUND,          /* PangoAttrColor */
;;;   PANGO_ATTR_BACKGROUND,          /* PangoAttrColor */
;;;   PANGO_ATTR_UNDERLINE,           /* PangoAttrInt */
;;;   PANGO_ATTR_STRIKETHROUGH,       /* PangoAttrInt */
;;;   PANGO_ATTR_RISE,                /* PangoAttrInt */
;;;   PANGO_ATTR_SHAPE,               /* PangoAttrShape */
;;;   PANGO_ATTR_SCALE,               /* PangoAttrFloat */
;;;   PANGO_ATTR_FALLBACK,            /* PangoAttrInt */
;;;   PANGO_ATTR_LETTER_SPACING,      /* PangoAttrInt */
;;;   PANGO_ATTR_UNDERLINE_COLOR,     /* PangoAttrColor */
;;;   PANGO_ATTR_STRIKETHROUGH_COLOR, /* PangoAttrColor */
;;;   PANGO_ATTR_ABSOLUTE_SIZE,       /* PangoAttrSize */
;;;   PANGO_ATTR_GRAVITY,             /* PangoAttrInt */
;;;   PANGO_ATTR_GRAVITY_HINT         /* PangoAttrInt */
;;; } PangoAttrType;
;;;
;;; The PangoAttrType distinguishes between different types of attributes.
;;; Along with the predefined values, it is possible to allocate additional
;;; values for custom attributes using pango_attr_type_register(). The
;;; predefined values are given below. The type of structure used to store the
;;; attribute is listed in parentheses after the description.
;;;
;;; PANGO_ATTR_INVALID
;;;     does not happen
;;;
;;; PANGO_ATTR_LANGUAGE
;;;     language (PangoAttrLanguage)
;;;
;;; PANGO_ATTR_FAMILY
;;;     font family name list (PangoAttrString)
;;;
;;; PANGO_ATTR_STYLE
;;;     font slant style (PangoAttrInt)
;;;
;;; PANGO_ATTR_WEIGHT
;;;     font weight (PangoAttrInt)
;;;
;;; PANGO_ATTR_VARIANT
;;;     font variant (normal or small caps) (PangoAttrInt)
;;;
;;; PANGO_ATTR_STRETCH
;;;     font stretch (PangoAttrInt)
;;;
;;; PANGO_ATTR_SIZE
;;;     font size in points scaled by PANGO_SCALE (PangoAttrInt)
;;;
;;; PANGO_ATTR_FONT_DESC
;;;     font description (PangoAttrFontDesc)
;;;
;;; PANGO_ATTR_FOREGROUND
;;;     foreground color (PangoAttrColor)
;;;
;;; PANGO_ATTR_BACKGROUND
;;;     background color (PangoAttrColor)
;;;
;;; PANGO_ATTR_UNDERLINE
;;;     whether the text has an underline (PangoAttrInt)
;;;
;;; PANGO_ATTR_STRIKETHROUGH
;;;     whether the text is struck-through (PangoAttrInt)
;;;
;;; PANGO_ATTR_RISE
;;;     baseline displacement (PangoAttrInt)
;;;
;;; PANGO_ATTR_SHAPE
;;;     shape (PangoAttrShape)
;;;
;;; PANGO_ATTR_SCALE
;;;     font size scale factor (PangoAttrFloat)
;;;
;;; PANGO_ATTR_FALLBACK
;;;     whether fallback is enabled (PangoAttrInt)
;;;
;;; PANGO_ATTR_LETTER_SPACING
;;;     letter spacing (PangoAttrInt)
;;;
;;; PANGO_ATTR_UNDERLINE_COLOR
;;;     underline color (PangoAttrColor)
;;;
;;; PANGO_ATTR_STRIKETHROUGH_COLOR
;;;     strikethrough color (PangoAttrColor)
;;;
;;; PANGO_ATTR_ABSOLUTE_SIZE
;;;     font size in pixels scaled by PANGO_SCALE (PangoAttrInt)
;;;
;;; PANGO_ATTR_GRAVITY
;;;     base text gravity (PangoAttrInt)
;;;
;;; PANGO_ATTR_GRAVITY_HINT
;;;     gravity hint (PangoAttrInt)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrClass
;;;
;;; struct PangoAttrClass {
;;;   PangoAttrType type;
;;;   PangoAttribute * (*copy) (const PangoAttribute *attr);
;;;   void             (*destroy) (PangoAttribute *attr);
;;;   gboolean         (*equal) (const PangoAttribute *attr1,
;;;                              const PangoAttribute *attr2);
;;; };
;;;
;;; The PangoAttrClass structure stores the type and operations for a particular
;;; type of attribute. The functions in this structure should not be called
;;; directly. Instead, one should use the wrapper functions provided for
;;; PangoAttribute.
;;;
;;; PangoAttrType type;
;;;     the type ID for this attribute
;;;
;;; copy ()
;;;     function to duplicate an attribute of this type
;;;     (see pango_attribute_copy())
;;;
;;; destroy ()
;;;     function to free an attribute of this type
;;;     (see pango_attribute_destroy())
;;;
;;; equal ()
;;;     function to check two attributes of this type for equality
;;;     (see pango_attribute_equal())
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttribute
;;;
;;; struct PangoAttribute {
;;;   const PangoAttrClass *klass;
;;;   guint start_index; /* in bytes */
;;;   guint end_index; /*in bytes. The character at this index is not included*/
;;; };
;;;
;;; The PangoAttribute structure represents the common portions of all
;;; attributes. Particular types of attributes include this structure as their
;;; initial portion. The common portion of the attribute holds the range to
;;; which the value in the type-specific part of the attribute applies and
;;; should be initialized using pango_attribute_init(). By default an attribute
;;; will have an all-inclusive range of [0,G_MAXUINT].
;;;
;;; const PangoAttrClass *klass;
;;;     the class structure holding information about the type of the attribute
;;;
;;; guint start_index;
;;;     the start index of the range (in bytes).
;;;
;;; guint end_index;
;;;     end index of the range (in bytes). The character at this index is not
;;;     included in the range
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING
;;;
;;; #define PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING 0
;;;
;;; This value can be used to set the start_index member of a PangoAttribute
;;; such that the attribute covers from the beginning of the text.
;;; Since: 1.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_ATTR_INDEX_TO_TEXT_END
;;;
;;; #define PANGO_ATTR_INDEX_TO_TEXT_END        G_MAXUINT
;;;
;;; This value can be used to set the end_index member of a PangoAttribute such
;;; that the attribute covers to the end of the text.
;;; Since: 1.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrString
;;;
;;; struct PangoAttrString {
;;;   PangoAttribute attr;
;;;   char *value;
;;; };
;;;
;;; The PangoAttrString structure is used to represent attributes with a string
;;; value.
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; char *value;
;;;     the string which is the value of the attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrLanguage
;;;
;;; struct PangoAttrLanguage {
;;;   PangoAttribute attr;
;;;   PangoLanguage *value;
;;; };
;;;
;;; The PangoAttrLanguage structure is used to represent attributes that are
;;; languages.
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; PangoLanguage *value;
;;;     the PangoLanguage which is the value of the attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrColor
;;;
;;; struct PangoAttrColor {
;;;   PangoAttribute attr;
;;;   PangoColor color;
;;; };
;;;
;;; The PangoAttrColor structure is used to represent attributes that are
;;; colors.
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; PangoColor color;
;;;     the PangoColor which is the value of the attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrInt
;;;
;;; struct PangoAttrInt {
;;;   PangoAttribute attr;
;;;   int value;
;;; };
;;;
;;; The PangoAttrInt structure is used to represent attributes with an integer
;;; or enumeration value.
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; int value;
;;;     the value of the attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrFloat
;;;
;;; struct PangoAttrFloat {
;;;   PangoAttribute attr;
;;;   double value;
;;; };
;;;
;;; The PangoAttrFloat structure is used to represent attributes with a float
;;; or double value.
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; double value;
;;;     the value of the attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrFontDesc
;;;
;;; struct PangoAttrFontDesc {
;;;   PangoAttribute attr;
;;;   PangoFontDescription *desc;
;;; };
;;;
;;; The PangoAttrFontDesc structure is used to store an attribute that sets all
;;; aspects of the font description at once.
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; PangoFontDescription *desc;
;;;     the font description which is the value of this attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrShape
;;;
;;; struct PangoAttrShape {
;;;   PangoAttribute attr;
;;;   PangoRectangle ink_rect;
;;;   PangoRectangle logical_rect;
;;;
;;;   gpointer              data;
;;;   PangoAttrDataCopyFunc copy_func;
;;;   GDestroyNotify        destroy_func;
;;; };
;;;
;;; The PangoAttrShape structure is used to represent attributes which impose
;;; shape restrictions.
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; PangoRectangle ink_rect;
;;;     the ink rectangle to restrict to
;;;
;;; PangoRectangle logical_rect;
;;;     the logical rectangle to restrict to
;;;
;;; gpointer data;
;;;     user data set (see pango_attr_shape_new_with_data())
;;;
;;; PangoAttrDataCopyFunc copy_func;
;;;     copy function for the user data
;;;
;;; GDestroyNotify destroy_func;
;;;     destroy function for the user data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrSize
;;;
;;; struct PangoAttrSize {
;;;   PangoAttribute attr;
;;;   int size;
;;;   guint absolute : 1;
;;; };
;;;
;;; The PangoAttrShape structure is used to represent attributes which set font
;;; size.
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; int size;
;;;     size of font, in units of 1/PANGO_SCALE of a point (for PANGO_ATTR_SIZE)
;;;     or of a device uni (for PANGO_ATTR_ABSOLUTE_SIZE)
;;;
;;; guint absolute : 1;
;;;     whether the font size is in device units or points. This field is only
;;;     present for compatibility with Pango-1.8.0 (PANGO_ATTR_ABSOLUTE_SIZE
;;;     was added in 1.8.1); and always will be FALSE for PANGO_ATTR_SIZE and
;;;     TRUE for PANGO_ATTR_ABSOLUTE_SIZE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_parse_markup ()
;;;
;;; gboolean pango_parse_markup (const char *markup_text,
;;;                              int length,
;;;                              gunichar accel_marker,
;;;                              PangoAttrList **attr_list,
;;;                              char **text,
;;;                              gunichar *accel_char,
;;;                              GError **error);
;;;
;;; Parses marked-up text (see markup format) to create a plain-text string and
;;; an attribute list.
;;;
;;; If accel_marker is nonzero, the given character will mark the character
;;; following it as an accelerator. For example, accel_marker might be an
;;; ampersand or underscore. All characters marked as an accelerator will
;;; receive a PANGO_UNDERLINE_LOW attribute, and the first character so marked
;;; will be returned in accel_char. Two accel_marker characters following each
;;; other produce a single literal accel_marker character.
;;;
;;; If any error happens, none of the output arguments are touched except for
;;; error.
;;;
;;; markup_text :
;;;     markup to parse (see markup format)
;;;
;;; length :
;;;     length of markup_text, or -1 if nul-terminated
;;;
;;; accel_marker :
;;;     character that precedes an accelerator, or 0 for none
;;;
;;; attr_list :
;;;     address of return location for a PangoAttrList, or NULL
;;;
;;; text :
;;;     address of return location for text with tags stripped, or NULL
;;;
;;; accel_char :
;;;     address of return location for accelerator char, or NULL
;;;
;;; error :
;;;     address of return location for errors, or NULL
;;;
;;; Returns :
;;;     FALSE if error is set, otherwise TRUE
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-parse-markup atdoc:*symbol-name-alias*)
      "Function"
      (gethash 'pango-parse-markup atdoc:*external-symbols*)
 "@version{2013-4-20}
  At the current time this function is not implemented in the Lisp binding to
  GTK+.")

#+cl-cffi-gtk-documentation
(export 'pango-parse-markup)

;;; ----------------------------------------------------------------------------
;;; pango_attr_type_register ()
;;;
;;; PangoAttrType pango_attr_type_register (const gchar *name);
;;;
;;; Allocate a new attribute type ID. The attribute type name can be accessed
;;; later by using pango_attr_type_get_name().
;;;
;;; name :
;;;     an identifier for the type
;;;
;;; Returns :
;;;     the new type ID
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_type_get_name ()
;;;
;;; const char * pango_attr_type_get_name (PangoAttrType type);
;;;
;;; Fetches the attribute type name passed in when registering the type using
;;; pango_attr_type_register().
;;;
;;; The returned value is an interned string (see g_intern_string() for what
;;; that means) that should not be modified or freed.
;;;
;;; type :
;;;     an attribute type ID to fetch the name for
;;;
;;; Returns :
;;;     the type ID name (which may be NULL), or NULL if type is a built-in
;;;     Pango attribute type or invalid.
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attribute_init ()
;;;
;;; void pango_attribute_init (PangoAttribute *attr,
;;;                            const PangoAttrClass *klass);
;;;
;;; Initializes attr's klass to klass, it's start_index to
;;; PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING and end_index to
;;; PANGO_ATTR_INDEX_TO_TEXT_END such that the attribute applies to the entire
;;; text by default.
;;;
;;; attr :
;;;     a PangoAttribute
;;;
;;; klass :
;;;     a PangoAttributeClass
;;;
;;; Since 1.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attribute_copy ()
;;;
;;; PangoAttribute * pango_attribute_copy (const PangoAttribute *attr);
;;;
;;; Make a copy of an attribute.
;;;
;;; attr :
;;;     a PangoAttribute
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attribute_equal ()
;;;
;;; gboolean pango_attribute_equal (const PangoAttribute *attr1,
;;;                                 const PangoAttribute *attr2);
;;;
;;; Compare two attributes for equality. This compares only the actual value of
;;; the two attributes and not the ranges that the attributes apply to.
;;;
;;; attr1 :
;;;     a PangoAttribute
;;;
;;; attr2 :
;;;     another PangoAttribute
;;;
;;; Returns :
;;;     TRUE if the two attributes have the same value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attribute_destroy ()
;;;
;;; void pango_attribute_destroy (PangoAttribute *attr);
;;;
;;; Destroy a PangoAttribute and free all associated memory.
;;;
;;; attr :
;;;     a PangoAttribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_language_new ()
;;;
;;; PangoAttribute * pango_attr_language_new (PangoLanguage *language);
;;;
;;; Create a new language tag attribute.
;;;
;;; language :
;;;     language tag
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_family_new ()
;;;
;;; PangoAttribute * pango_attr_family_new (const char *family);
;;;
;;; Create a new font family attribute.
;;;
;;; family :
;;;     the family or comma separated list of families
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_style_new ()
;;;
;;; PangoAttribute * pango_attr_style_new (PangoStyle style);
;;;
;;; Create a new font slant style attribute.
;;;
;;; style :
;;;     the slant style
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_variant_new ()
;;;
;;; PangoAttribute * pango_attr_variant_new (PangoVariant variant);
;;;
;;; Create a new font variant attribute (normal or small caps)
;;;
;;; variant :
;;;     the variant
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_stretch_new ()
;;;
;;; PangoAttribute * pango_attr_stretch_new (PangoStretch stretch);
;;;
;;; Create a new font stretch attribute
;;;
;;; stretch :
;;;     the stretch
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_weight_new ()
;;;
;;; PangoAttribute * pango_attr_weight_new (PangoWeight weight);
;;;
;;; Create a new font weight attribute.
;;;
;;; weight :
;;;     the weight
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_size_new ()
;;;
;;; PangoAttribute * pango_attr_size_new (int size);
;;;
;;; Create a new font-size attribute in fractional points.
;;;
;;; size :
;;;     the font size, in PANGO_SCALEths of a point.
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_size_new_absolute ()
;;;
;;; PangoAttribute * pango_attr_size_new_absolute (int size);
;;;
;;; Create a new font-size attribute in device units.
;;;
;;; size :
;;;     the font size, in PANGO_SCALEths of a device unit.
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_font_desc_new ()
;;;
;;; PangoAttribute * pango_attr_font_desc_new (const PangoFontDescription *desc)
;;;
;;; Create a new font description attribute. This attribute allows setting
;;; family, style, weight, variant, stretch, and size simultaneously.
;;;
;;; desc :
;;;     the font description
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_foreground_new ()
;;;
;;; PangoAttribute * pango_attr_foreground_new (guint16 red,
;;;                                             guint16 green,
;;;                                             guint16 blue);
;;;
;;; Create a new foreground color attribute.
;;;
;;; red :
;;;     the red value (ranging from 0 to 65535)
;;;
;;; green :
;;;     the green value
;;;
;;; blue :
;;;     the blue value
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_background_new ()
;;;
;;; PangoAttribute * pango_attr_background_new (guint16 red,
;;;                                             guint16 green,
;;;                                             guint16 blue);
;;;
;;; Create a new background color attribute.
;;;
;;; red :
;;;     the red value (ranging from 0 to 65535)
;;;
;;; green :
;;;     the green value
;;;
;;; blue :
;;;     the blue value
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_strikethrough_new ()
;;;
;;; PangoAttribute * pango_attr_strikethrough_new (gboolean strikethrough);
;;;
;;; Create a new strike-through attribute.
;;;
;;; strikethrough :
;;;     TRUE if the text should be struck-through.
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_strikethrough_color_new ()
;;;
;;; PangoAttribute * pango_attr_strikethrough_color_new (guint16 red,
;;;                                                      guint16 green,
;;;                                                      guint16 blue);
;;;
;;; Create a new strikethrough color attribute. This attribute modifies the
;;; color of strikethrough lines. If not set, strikethrough lines will use the
;;; foreground color.
;;;
;;; red :
;;;     the red value (ranging from 0 to 65535)
;;;
;;; green :
;;;     the green value
;;;
;;; blue :
;;;     the blue value
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_underline_new ()
;;;
;;; PangoAttribute * pango_attr_underline_new (PangoUnderline underline);
;;;
;;; Create a new underline-style attribute.
;;;
;;; underline :
;;;     the underline style.
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_underline_color_new ()
;;;
;;; PangoAttribute * pango_attr_underline_color_new (guint16 red,
;;;                                                  guint16 green,
;;;                                                  guint16 blue);
;;;
;;; Create a new underline color attribute. This attribute modifies the color
;;; of underlines. If not set, underlines will use the foreground color.
;;;
;;; red :
;;;     the red value (ranging from 0 to 65535)
;;;
;;; green :
;;;     the green value
;;;
;;; blue :
;;;     the blue value
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum PangoUnderline
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoUnderline" pango-underline
  (:export t
   :type-initializer "pango_underline_get_type")
  (:none 0)
  (:single 1)
  (:double 2)
  (:low 3)
  (:error 4))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-underline atdoc:*symbol-name-alias*) "Enum"
      (gethash 'pango-underline atdoc:*external-symbols*)
 "@version{2013-6-30}
  @begin{short}
    The @sym{pango-underline} enumeration is used to specify whether text should
    be underlined, and if so, the type of underlining.
  @end{short}
  @begin{pre}
(define-g-enum \"PangoUnderline\" pango-underline
  (:export t
   :type-initializer \"pango_underline_get_type\")
  (:none 0)
  (:single 1)
  (:double 2)
  (:low 3)
  (:error 4))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No underline should be drawn.}
    @entry[:single]{A single underline should be drawn.}
    @entry[:double]{A double underline should be drawn.}
    @entry[:low]{A single underline should be drawn at a position beneath the
      ink extents of the text being underlined. This should be used only for
      underlining single characters, such as for keyboard accelerators.
      @code{:single} should be used for extended portions of text.}
    @entry[:error]{A wavy underline should be drawn below. This underline is
      typically used to indicate an error such as a possible mispelling; in some
      cases a contrasting color may automatically be used. This type of
      underlining is available since Pango 1.4.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_UNDERLINE
;;;
;;; #define PANGO_TYPE_UNDERLINE (pango_underline_get_type())
;;;
;;; The GObject type for PangoUnderline.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_shape_new ()
;;;
;;; PangoAttribute * pango_attr_shape_new (const PangoRectangle *ink_rect,
;;;                                        const PangoRectangle *logical_rect);
;;;
;;; Create a new shape attribute. A shape is used to impose a particular ink
;;; and logical rectangle on the result of shaping a particular glyph. This
;;; might be used, for instance, for embedding a picture or a widget inside a
;;; PangoLayout.
;;;
;;; ink_rect :
;;;     ink rectangle to assign to each character
;;;
;;; logical_rect :
;;;     logical rectangle to assign to each character
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_shape_new_with_data ()
;;;
;;; PangoAttribute * pango_attr_shape_new_with_data
;;;                                         (const PangoRectangle *ink_rect,
;;;                                          const PangoRectangle *logical_rect,
;;;                                          gpointer data,
;;;                                          PangoAttrDataCopyFunc copy_func,
;;;                                          GDestroyNotify destroy_func);
;;;
;;; Like pango_attr_shape_new(), but a user data pointer is also provided; this
;;; pointer can be accessed when later rendering the glyph.
;;;
;;; ink_rect :
;;;     ink rectangle to assign to each character
;;;
;;; logical_rect :
;;;     logical rectangle to assign to each character
;;;
;;; data :
;;;     user data pointer
;;;
;;; copy_func :
;;;     function to copy data when the attribute is copied. If NULL, data is
;;;     simply copied as a pointer
;;;
;;; destroy_func :
;;;     function to free data when the attribute is freed, or NULL
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoAttrDataCopyFunc ()
;;;
;;; gpointer (*PangoAttrDataCopyFunc) (gconstpointer data);
;;;
;;; A copy function passed to attribute new functions that take user data.
;;;
;;; data :
;;;     the user data
;;;
;;; Returns :
;;;     a new copy of data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_scale_new ()
;;;
;;; PangoAttribute * pango_attr_scale_new (double scale_factor);
;;;
;;; Create a new font size scale attribute. The base font for the affected text
;;; will have its size multiplied by scale_factor.
;;;
;;; scale_factor :
;;;     factor to scale the font
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_XX_SMALL
;;;
;;; #define PANGO_SCALE_XX_SMALL ((double)0.5787037037037)
;;;
;;; The scale factor for three shrinking steps (1 / (1.2 * 1.2 * 1.2)).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_X_SMALL
;;;
;;; #define PANGO_SCALE_X_SMALL  ((double)0.6444444444444)
;;;
;;; The scale factor for two shrinking steps (1 / (1.2 * 1.2)).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_SMALL
;;;
;;; #define PANGO_SCALE_SMALL    ((double)0.8333333333333)
;;;
;;; The scale factor for one shrinking step (1 / 1.2).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_MEDIUM
;;;
;;; #define PANGO_SCALE_MEDIUM   ((double)1.0)
;;;
;;; The scale factor for normal size (1.0).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_LARGE
;;;
;;; #define PANGO_SCALE_LARGE    ((double)1.2)
;;;
;;; The scale factor for one magnification step (1.2).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_X_LARGE
;;;
;;; #define PANGO_SCALE_X_LARGE  ((double)1.4399999999999)
;;;
;;; The scale factor for two magnification steps (1.2 * 1.2).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_XX_LARGE
;;;
;;; #define PANGO_SCALE_XX_LARGE ((double)1.728)
;;;
;;; The scale factor for three magnification steps (1.2 * 1.2 * 1.2).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_rise_new ()
;;;
;;; PangoAttribute * pango_attr_rise_new (int rise);
;;;
;;; Create a new baseline displacement attribute.
;;;
;;; rise :
;;;     the amount that the text should be displaced vertically, in Pango units.
;;;     Positive values displace the text upwards.
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_letter_spacing_new ()
;;;
;;; PangoAttribute * pango_attr_letter_spacing_new (int letter_spacing);
;;;
;;; Create a new letter-spacing attribute.
;;;
;;; letter_spacing :
;;;     amount of extra space to add between graphemes of the text, in Pango
;;;     units.
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_fallback_new ()
;;;
;;; PangoAttribute * pango_attr_fallback_new (gboolean enable_fallback);
;;;
;;; Create a new font fallback attribute.
;;;
;;; If fallback is disabled, characters will only be used from the closest
;;; matching font on the system. No fallback will be done to other fonts on the
;;; system that might contain the characters in the text.
;;;
;;; enable_fallback :
;;;     TRUE if we should fall back on other fonts for characters the active
;;;     font is missing.
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_gravity_new ()
;;;
;;; PangoAttribute * pango_attr_gravity_new (PangoGravity gravity);
;;;
;;; Create a new gravity attribute.
;;;
;;; gravity :
;;;     the gravity value; should not be PANGO_GRAVITY_AUTO.
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_gravity_hint_new ()
;;;
;;; PangoAttribute * pango_attr_gravity_hint_new (PangoGravityHint hint);
;;;
;;; Create a new gravity hint attribute.
;;;
;;; hint :
;;;     the gravity hint value.
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoColor
;;;
;;; struct PangoColor {
;;;   guint16 red;
;;;   guint16 green;
;;;   guint16 blue;
;;; };
;;;
;;; The PangoColor structure is used to represent a color in an uncalibrated
;;; RGB color-space.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_color_parse ()
;;;
;;; gboolean pango_color_parse (PangoColor *color, const char *spec);
;;;
;;; Fill in the fields of a color from a string specification. The string can
;;; either one of a large set of standard names. (Taken from the X11 rgb.txt
;;; file), or it can be a hex value in the form '#rgb' '#rrggbb' '#rrrgggbbb'
;;; or '#rrrrggggbbbb' where 'r', 'g' and 'b' are hex digits of the red, green,
;;; and blue components of the color, respectively. (White in the four forms is
;;; '#fff' '#ffffff' '#fffffffff' and '#ffffffffffff')
;;;
;;; color :
;;;     a PangoColor structure in which to store the result, or NULL
;;;
;;; spec :
;;;     a string specifying the new color
;;;
;;; Returns :
;;;     TRUE if parsing of the specifier succeeded, otherwise false
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_color_copy ()
;;;
;;; PangoColor * pango_color_copy (const PangoColor *src);
;;;
;;; Creates a copy of src, which should be freed with pango_color_free().
;;; Primarily used by language bindings, not that useful otherwise (since colors
;;; can just be copied by assignment in C).
;;;
;;; src :
;;;     color to copy, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoColor, which should be freed with
;;;     pango_color_free(), or NULL if src was NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_color_free ()
;;;
;;; void pango_color_free (PangoColor *color);
;;;
;;; Frees a color allocated by pango_color_copy().
;;;
;;; color :
;;;     an allocated PangoColor, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_color_to_string ()
;;;
;;; gchar * pango_color_to_string (const PangoColor *color);
;;;
;;; Returns a textual specification of color in the hexadecimal form
;;; #rrrrggggbbbb, where r, g and b are hex digits representing the red, green,
;;; and blue components respectively.
;;;
;;; color :
;;;     a PangoColor
;;;
;;; Returns :
;;;     a newly-allocated text string that must be freed with g_free()
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoAttrList
;;; ----------------------------------------------------------------------------

(glib::at-init () (foreign-funcall "pango_attr_list_get_type" :int))

(define-g-boxed-opaque pango-attr-list "PangoAttrList"
  :alloc (%pango-attr-list-new))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-attr-list atdoc:*class-name-alias*) "CStruct"
      (documentation 'pango-attr-list 'type)
 "@version{2013-6-30}
  @begin{short}
    The @sym{pango-attr-list} structure represents a list of attributes that
    apply to a section of text.
  @end{short}
  The attributes are, in general, allowed to overlap in an arbitrary fashion,
  however, if the attributes are manipulated only through the function
  @fun{pango-attr-list-change}, the overlap between properties will meet
  stricter criteria.

  Since the @sym{pango-attr-list} structure is stored as a linear list, it is
  not suitable for storing attributes for large amounts of text. In general, you
  should not use a single @sym{pango-attr-list} for more than one paragraph of
  text.
  @see-function{pango-attr-list-change}")

(export (boxed-related-symbols 'pango-attr-list))

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_new ()
;;;
;;; PangoAttrList * pango_attr_list_new (void);
;;;
;;; Create a new empty attribute list with a reference count of one.
;;;
;;; Returns :
;;;     the newly allocated PangoAttrList, which should be freed with
;;;     pango_attr_list_unref()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_attr_list_new" %pango-attr-list-new) :pointer)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_ref ()
;;;
;;; PangoAttrList * pango_attr_list_ref (PangoAttrList *list);
;;;
;;; Increase the reference count of the given attribute list by one.
;;;
;;; list :
;;;     a PangoAttrList, may be NULL
;;;
;;; Returns :
;;;     The attribute list passed in
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_unref ()
;;;
;;; void pango_attr_list_unref (PangoAttrList *list);
;;;
;;; Decrease the reference count of the given attribute list by one. If the
;;; result is zero, free the attribute list and the attributes it contains.
;;;
;;; list :
;;;     a PangoAttrList, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_copy ()
;;;
;;; PangoAttrList * pango_attr_list_copy (PangoAttrList *list);
;;;
;;; Copy list and return an identical new list.
;;;
;;; list :
;;;     a PangoAttrList, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoAttrList, with a reference count of one, which
;;;     should be freed with pango_attr_list_unref(). Returns NULL if list was
;;;     NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_insert ()
;;;
;;; void pango_attr_list_insert (PangoAttrList *list, PangoAttribute *attr);
;;;
;;; Insert the given attribute into the PangoAttrList. It will be inserted
;;; after all other attributes with a matching start_index.
;;;
;;; list :
;;;     a PangoAttrList
;;;
;;; attr :
;;;     the attribute to insert. Ownership of this value is assumed by the list
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_insert_before ()
;;;
;;; void pango_attr_list_insert_before (PangoAttrList *list,
;;;                                     PangoAttribute *attr);
;;;
;;; Insert the given attribute into the PangoAttrList. It will be inserted
;;; before all other attributes with a matching start_index.
;;;
;;; list :
;;;     a PangoAttrList
;;;
;;; attr :
;;;     the attribute to insert. Ownership of this value is assumed by the list
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_change ()
;;;
;;; void pango_attr_list_change (PangoAttrList *list, PangoAttribute *attr);
;;;
;;; Insert the given attribute into the PangoAttrList. It will replace any
;;; attributes of the same type on that segment and be merged with any adjoining
;;; attributes that are identical.
;;;
;;; This function is slower than pango_attr_list_insert() for creating a
;;; attribute list in order (potentially much slower for large lists). However,
;;; pango_attr_list_insert() is not suitable for continually changing a set of
;;; attributes since it never removes or combines existing attributes.
;;;
;;; list :
;;;     a PangoAttrList
;;;
;;; attr :
;;;     the attribute to insert. Ownership of this value is assumed by the list
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_splice ()
;;;
;;; void pango_attr_list_splice (PangoAttrList *list,
;;;                              PangoAttrList *other,
;;;                              gint pos,
;;;                              gint len);
;;;
;;; This function opens up a hole in list, fills it in with attributes from the
;;; left, and then merges other on top of the hole.
;;;
;;; This operation is equivalent to stretching every attribute that applies at
;;; position pos in list by an amount len, and then calling
;;; pango_attr_list_change() with a copy of each attribute in other in sequence
;;; (offset in position by pos).
;;;
;;; This operation proves useful for, for instance, inserting a pre-edit string
;;; in the middle of an edit buffer.
;;;
;;; list :
;;;     a PangoAttrList
;;;
;;; other :
;;;     another PangoAttrList
;;;
;;; pos :
;;;     the position in list at which to insert other
;;;
;;; len :
;;;     the length of the spliced segment. (Note that this must be specified
;;;     since the attributes in other may only be present at some subsection
;;;     of this range)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_filter ()
;;;
;;; PangoAttrList * pango_attr_list_filter (PangoAttrList *list,
;;;                                         PangoAttrFilterFunc func,
;;;                                         gpointer data);
;;;
;;; Given a PangoAttrList and callback function, removes any elements of list
;;; for which func returns TRUE and inserts them into a new list.
;;;
;;; list :
;;;     a PangoAttrList
;;;
;;; func :
;;;     callback function; returns TRUE if an attribute should be filtered out
;;;
;;; data :
;;;     Data to be passed to func
;;;
;;; Returns :
;;;     the new PangoAttrList or NULL if no attributes of the given types were
;;;     found
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoAttrFilterFunc ()
;;;
;;; gboolean (*PangoAttrFilterFunc) (PangoAttribute *attribute,
;;;                                  gpointer data);
;;;
;;; A predicate function used by pango_attr_list_filter() to filter out a
;;; subset of attributes for a list.
;;;
;;; attribute :
;;;     a PangoAttribute
;;;
;;; data :
;;;     callback data passed to pango_attr_list_filter()
;;;
;;; Returns :
;;;     TRUE if the attribute should be filtered out
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_get_iterator ()
;;;
;;; PangoAttrIterator * pango_attr_list_get_iterator (PangoAttrList *list);
;;;
;;; Create a iterator initialized to the beginning of the list. list must not
;;; be modified until this iterator is freed.
;;;
;;; list :
;;;     a PangoAttrList
;;;
;;; Returns :
;;;     the newly allocated PangoAttrIterator, which should be freed with
;;;     pango_attr_iterator_destroy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoAttrIterator
;;;
;;; typedef struct _PangoAttrIterator PangoAttrIterator;
;;;
;;; The PangoAttrIterator structure is used to represent an iterator through a
;;; PangoAttrList. A new iterator is created with
;;; pango_attr_list_get_iterator(). Once the iterator is created, it can be
;;; advanced through the style changes in the text using
;;; pango_attr_iterator_next(). At each style change, the range of the current
;;; style segment and the attributes currently in effect can be queried.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_copy ()
;;;
;;; PangoAttrIterator * pango_attr_iterator_copy (PangoAttrIterator *iterator);
;;;
;;; Copy a PangoAttrIterator
;;;
;;; iterator :
;;;     a PangoAttrIterator.
;;;
;;; Returns :
;;;     the newly allocated PangoAttrIterator, which should be freed with
;;;     pango_attr_iterator_destroy().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_next ()
;;;
;;; gboolean pango_attr_iterator_next (PangoAttrIterator *iterator);
;;;
;;; Advance the iterator until the next change of style.
;;;
;;; iterator :
;;;     a PangoAttrIterator
;;;
;;; Returns :
;;;     FALSE if the iterator is at the end of the list, otherwise TRUE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_range ()
;;;
;;; void pango_attr_iterator_range (PangoAttrIterator *iterator,
;;;                                 gint *start,
;;;                                 gint *end);
;;;
;;; Get the range of the current segment. Note that the stored return values
;;; are signed, not unsigned like the values in PangoAttribute. To deal with
;;; this API oversight, stored return values that wouldn't fit into a signed
;;; integer are clamped to G_MAXINT.
;;;
;;; iterator :
;;;     a PangoAttrIterator
;;;
;;; start :
;;;     location to store the start of the range
;;;
;;; end :
;;;     location to store the end of the range
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_get ()
;;;
;;; PangoAttribute * pango_attr_iterator_get (PangoAttrIterator *iterator,
;;;                                           PangoAttrType type);
;;;
;;; Find the current attribute of a particular type at the iterator location.
;;; When multiple attributes of the same type overlap, the attribute whose
;;; range starts closest to the current location is used.
;;;
;;; iterator :
;;;     a PangoAttrIterator
;;;
;;; type :
;;;     the type of attribute to find.
;;;
;;; Returns :
;;;     the current attribute of the given type, or NULL if no attribute of
;;;     that type applies to the current location
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_get_font ()
;;;
;;; void pango_attr_iterator_get_font (PangoAttrIterator *iterator,
;;;                                    PangoFontDescription *desc,
;;;                                    PangoLanguage **language,
;;;                                    GSList **extra_attrs);
;;;
;;; Get the font and other attributes at the current iterator position.
;;;
;;; iterator :
;;;     a PangoAttrIterator
;;;
;;; desc :
;;;     a PangoFontDescription to fill in with the current values. The family
;;;     name in this structure will be set using
;;;     pango_font_description_set_family_static() using values from an
;;;     attribute in the PangoAttrList associated with the iterator, so if you
;;;     plan to keep it around, you must call: pango_font_description_set_family
;;;     (desc, pango_font_description_get_family (desc)).
;;;
;;; language :
;;;     if non-NULL, location to store language tag for item, or NULL if none
;;;     is found
;;;
;;; extra_attrs :
;;;     if non-NULL, location in which to store a list of non-font attributes
;;;     at the the current position; only the highest priority value of each
;;;     attribute will be added to this list. In order to free this value, you
;;;     must call pango_attribute_destroy() on each member
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_get_attrs ()
;;;
;;; GSList * pango_attr_iterator_get_attrs (PangoAttrIterator *iterator);
;;;
;;; Gets a list of all attributes at the current position of the iterator.
;;;
;;; iterator :
;;;     a PangoAttrIterator
;;;
;;; Returns :
;;;     a list of all attributes for the current range. To free this value,
;;;     call pango_attribute_destroy() on each value and g_slist_free() on the
;;;     list
;;;
;;; Since 1.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_destroy ()
;;;
;;; void pango_attr_iterator_destroy (PangoAttrIterator *iterator);
;;;
;;; Destroy a PangoAttrIterator and free all associated memory.
;;;
;;; iterator :
;;;     a PangoAttrIterator
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.attributes.lisp --------------------------------------
