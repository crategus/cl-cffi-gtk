;;; ----------------------------------------------------------------------------
;;; pango.fonts.lisp
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
;;; Fonts
;;;
;;; Structures representing abstract fonts
;;;
;;; Synopsis
;;;
;;;     PangoFontDescription
;;;     PangoStyle
;;;     PangoWeight
;;;     PangoVariant
;;;     PangoStretch
;;;     PangoFontMask
;;;
;;;     pango_font_description_new
;;;     pango_font_description_copy
;;;     pango_font_description_copy_static
;;;     pango_font_description_hash
;;;     pango_font_description_equal
;;;     pango_font_description_free
;;;     pango_font_descriptions_free
;;;     pango_font_description_set_family
;;;     pango_font_description_set_family_static
;;;     pango_font_description_get_family
;;;     pango_font_description_set_style
;;;     pango_font_description_get_style
;;;     pango_font_description_set_variant
;;;     pango_font_description_get_variant
;;;     pango_font_description_set_weight
;;;     pango_font_description_get_weight
;;;     pango_font_description_set_stretch
;;;     pango_font_description_get_stretch
;;;     pango_font_description_set_size
;;;     pango_font_description_get_size
;;;     pango_font_description_set_absolute_size
;;;     pango_font_description_get_size_is_absolute
;;;     pango_font_description_set_gravity
;;;     pango_font_description_get_gravity
;;;     pango_font_description_get_set_fields
;;;     pango_font_description_unset_fields
;;;     pango_font_description_merge
;;;     pango_font_description_merge_static
;;;     pango_font_description_better_match
;;;     pango_font_description_from_string
;;;     pango_font_description_to_string
;;;     pango_font_description_to_filename
;;;
;;;     PangoFontMetrics
;;;
;;;     pango_font_metrics_ref
;;;     pango_font_metrics_unref
;;;     pango_font_metrics_get_ascent
;;;     pango_font_metrics_get_descent
;;;     pango_font_metrics_get_approximate_char_width
;;;     pango_font_metrics_get_approximate_digit_width
;;;     pango_font_metrics_get_underline_thickness
;;;     pango_font_metrics_get_underline_position
;;;     pango_font_metrics_get_strikethrough_thickness
;;;     pango_font_metrics_get_strikethrough_position
;;;
;;;     PangoFont
;;;
;;;     pango_font_find_shaper
;;;     pango_font_describe
;;;     pango_font_describe_with_absolute_size
;;;     pango_font_get_coverage
;;;     pango_font_get_glyph_extents
;;;     pango_font_get_metrics
;;;     pango_font_get_font_map
;;;
;;;     PangoFontFamily
;;;
;;;     pango_font_family_get_name
;;;     pango_font_family_is_monospace
;;;     pango_font_family_list_faces
;;;
;;;     PangoFontFace
;;;
;;;     pango_font_face_get_face_name
;;;     pango_font_face_list_sizes
;;;     pango_font_face_describe
;;;     pango_font_face_is_synthesized
;;;
;;;     PangoFontMap
;;;     PangoFontMapClass
;;;
;;;     pango_font_map_create_context
;;;     pango_font_map_load_font
;;;     pango_font_map_load_fontset
;;;     pango_font_map_list_families
;;;     pango_font_map_get_shape_engine_type
;;;
;;;     PangoFontset
;;;     PangoFontsetClass;
;;;
;;;     pango_fontset_get_font
;;;     pango_fontset_get_metrics
;;;     pango_fontset_foreach
;;;
;;;     PangoFontsetSimple
;;;
;;;     pango_fontset_simple_new
;;;     pango_fontset_simple_append
;;;     pango_fontset_simple_size
;;;
;;; Object Hierarchy
;;;
;;;   GBoxed
;;;    +----PangoFontDescription
;;;
;;;   GEnum
;;;    +----PangoStyle
;;;
;;;   GEnum
;;;    +----PangoWeight
;;;
;;;   GEnum
;;;    +----PangoVariant
;;;
;;;   GEnum
;;;    +----PangoStretch
;;;
;;;   GFlags
;;;    +----PangoFontMask
;;;
;;;   GBoxed
;;;    +----PangoFontMetrics
;;;
;;;   GObject
;;;    +----PangoFont
;;;          +----PangoFcFont
;;;
;;;   GObject
;;;    +----PangoFontFamily
;;;
;;;   GObject
;;;    +----PangoFontFace
;;;
;;;   GObject
;;;    +----PangoFontMap
;;;          +----PangoFcFontMap
;;;
;;;   GObject
;;;    +----PangoFontset
;;;          +----PangoFontsetSimple
;;;
;;;   GObject
;;;    +----PangoFontset
;;;          +----PangoFontsetSimple
;;;
;;; Known Derived Interfaces
;;;
;;; PangoFont is required by PangoCairoFont.
;;;
;;; PangoFontMap is required by PangoCairoFontMap.
;;;
;;; Description
;;;
;;; Pango supports a flexible architecture where a particular rendering
;;; architecture can supply an implementation of fonts. The PangoFont structure
;;; represents an abstract rendering-system-independent font. Pango provides
;;; routines to list available fonts, and to load a font of a given description.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoFontDescription
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque pango-font-description "PangoFontDescription"
  :alloc (error "PangoFontDescription can not be created from Lisp side"))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-font-description atdoc:*class-name-alias*) "CStruct"
      (documentation 'pango-font-description 'type)
 "@version{2013-6-26}
  @begin{short}
    The @sym{pango-font-description} structure represents the description of an
    ideal font. These structures are used both to list what fonts are available
    on the system and also for specifying the characteristics of a font to load.
  @end{short}
  @begin{pre}
(define-g-boxed-opaque pango-font-description \"PangoFontDescription\"
  :alloc (error \"PangoFontDescription can not be created from Lisp side\"))
  @end{pre}")

(export (boxed-related-symbols 'pango-font-description))

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_FONT_DESCRIPTION
;;;
;;; #define PANGO_TYPE_FONT_DESCRIPTION (pango_font_description_get_type ())
;;;
;;; The GObject type for PangoFontDescription.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum PangoStyle
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoStyle" pango-style
  (:export t
   :type-initializer "pango_style_get_type")
  (:normal 0)
  (:oblique 1)
  (:italic 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-style atdoc:*symbol-name-alias*) "Enum"
      (gethash 'pango-style atdoc:*external-symbols*)
 "@version{2013-6-29}
  @begin{short}
    An enumeration specifying the various slant styles possible for a font.
  @end{short}
  @begin{pre}
(define-g-enum \"PangoStyle\" pango-style
  (:export t
   :type-initializer \"pango_style_get_type\")
  (:normal 0)
  (:oblique 1)
  (:italic 2))
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{The font is upright.}
    @entry[:oblique]{The font is slanted, but in a roman style.}
    @entry[:italic]{The font is slanted in an italic style.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_STYLE
;;;
;;; #define PANGO_TYPE_STYLE (pango_style_get_type())
;;;
;;; The GObject type for PangoStyle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum PangoWeight
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoWeight" pango-weight
  (:export t
   :type-initializer "pango_weight_get_type")
  (:thin 100)
  (:ultralight 200)
  (:light 300)
  (:book 380)
  (:normal 400)
  (:medium 500)
  (:semibold 600)
  (:bold 700)
  (:ultrabold 800)
  (:heavy 900)
  (:ultraheavy 1000))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-weight atdoc:*symbol-name-alias*) "Enum"
      (gethash 'pango-weight atdoc:*external-symbols*)
 "@version{2013-6-29}
  @begin{short}
    An enumeration specifying the weight (boldness) of a font. This is a
    numerical value ranging from 100 to 900, but there are some predefined
    values.
  @end{short}
  @begin{pre}
(define-g-enum \"PangoWeight\" pango-weight
  (:export t
   :type-initializer \"pango_weight_get_type\")
  (:thin 100)
  (:ultralight 200)
  (:light 300)
  (:book 380)
  (:normal 400)
  (:medium 500)
  (:semibold 600)
  (:bold 700)
  (:ultrabold 800)
  (:heavy 900)
  (:ultraheavy 1000))
  @end{pre}
  @begin[code]{table}
    @entry[:thin]{The thin weight. Since 1.24.}
    @entry[:ultralight]{The ultralight weight.}
    @entry[:light]{The light weight.}
    @entry[:book]{The book weight. Since 1.24.}
    @entry[:normal]{The default weight.}
    @entry[:medium]{The normal weight. Since 1.24.}
    @entry[:semibold]{The semibold weight.}
    @entry[:bold]{The bold weight.}
    @entry[:ultrabold]{The ultrabold weight.}
    @entry[:heavy]{The heavy weight.}
    @entry[:ultraheavy]{The ultraheavy weight. Since 1.24.}
  @end{table}")

;(defconstant +pango-weight-thin+ 100)
;(defconstant +pango-weight-ultralight+ 200)
;(defconstant +pango-weight-light+ 300)
;(defconstant +pango-weight-book+ 380)
;(defconstant +pango-weight-normal+ 400)
;(defconstant +pango-weight-medium+ 500)
;(defconstant +pango-weight-semibold+ 600)
;(defconstant +pango-weight-bold+ 700)
;(defconstant +pango-weight-ultrabold+ 800)
;(defconstant +pango-weight-heavy+ 900)
;(defconstant +pango-weight-ultraheavy+ 1000)

;(export '+pango-weight-thin+)
;(export '+pango-weight-ultralight+)
;(export '+pango-weight-light+)
;(export '+pango-weight-book+)
;(export '+pango-weight-normal+)
;(export '+pango-weight-medium+)
;(export '+pango-weight-semibold+)
;(export '+pango-weight-bold+)
;(export '+pango-weight-ultrabold+)
;(export '+pango-weight-heavy+)
;(export '+pango-weight-ultraheavy+)

;;; ----------------------------------------------------------------------------
;;; enum PangoVariant
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoVariant" pango-variant
  (:export t
   :type-initializer "pango_variant_get_type")
  (:normal 0)
  (:small-caps 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-weight atdoc:*symbol-name-alias*) "Enum"
      (gethash 'pango-weight atdoc:*external-symbols*)
 "@version{2020-3-20}
  @begin{short}
    An enumeration specifying capitalization variant of the font.
  @end{short}
  @begin{pre}
(define-g-enum \"PangoVariant\" pango-variant
  (:export t
   :type-initializer \"pango_variant_get_type\")
  (:normal 0)
  (:small-caps 1))
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{A normal font.}
    @entry[:small-caps]{A font with the lower case characters replaced by
      smaller variants of the capital characters.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; enum PangoStretch
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "pango_stretch_get_type" :int))

(define-g-enum "PangoStretch" pango-stretch
  (:export t
   :type-initializer "pango_stretch_type_get_type")
  (:ultra-condensed 0)
  (:extra-condensed 1)
  (:condensed 2)
  (:semi-condensed 3)
  (:normal 4)
  (:semi-expanded 5)
  (:expanded 6)
  (:extra-expanded 7)
  (:ultra-expanded 8))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-stretch atdoc:*symbol-name-alias*) "Enum"
      (gethash 'pango-stretch atdoc:*external-symbols*)
 "@version{2020-3-20}
  @begin{short}
    An enumeration specifying the width of the font relative to other designs
    within a family.
  @end{short}
  @begin{pre}
(define-g-enum \"PangoStretch\" pango-stretch
  (:export t
   :type-initializer \"pango_stretch_type_get_type\")
  (:ultra-condensed 0)
  (:extra-condensed 1)
  (:condensed 2)
  (:semi-condensed 3)
  (:normal 4)
  (:semi-expanded 5)
  (:expanded 6)
  (:extra-expanded 7)
  (:ultra-expanded 8))
  @end{pre}
  @begin[code]{table}
    @entry[:ultra-condensed]{Ultra condensed width.}
    @entry[:extra-condensed]{Extra condensed width.}
    @entry[:condensed]{Condensed width.}
    @entry[:semi-condensed]{Semi condensed width.}
    @entry[:normal]{The normal widt.}
    @entry[:semi-expanded]{Semi expanded width.}
    @entry[:expanded]{Expanded width.}
    @entry[:extra-expanded]{Extra expanded width.}
    @endtry[:ultra-expanded]{Ultra expanded width.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; enum PangoFontMask
;;;
;;; typedef enum {
;;;   PANGO_FONT_MASK_FAMILY  = 1 << 0,
;;;   PANGO_FONT_MASK_STYLE   = 1 << 1,
;;;   PANGO_FONT_MASK_VARIANT = 1 << 2,
;;;   PANGO_FONT_MASK_WEIGHT  = 1 << 3,
;;;   PANGO_FONT_MASK_STRETCH = 1 << 4,
;;;   PANGO_FONT_MASK_SIZE    = 1 << 5,
;;;   PANGO_FONT_MASK_GRAVITY = 1 << 6
;;; } PangoFontMask;
;;;
;;; The bits in a PangoFontMask correspond to fields in a PangoFontDescription
;;; that have been set.
;;;
;;; PANGO_FONT_MASK_FAMILY
;;;     the font family is specified.
;;;
;;; PANGO_FONT_MASK_STYLE
;;;     the font style is specified.
;;;
;;; PANGO_FONT_MASK_VARIANT
;;;     the font variant is specified.
;;;
;;; PANGO_FONT_MASK_WEIGHT
;;;     the font weight is specified.
;;;
;;; PANGO_FONT_MASK_STRETCH
;;;     the font stretch is specified.
;;;
;;; PANGO_FONT_MASK_SIZE
;;;     the font size is specified.
;;;
;;; PANGO_FONT_MASK_GRAVITY
;;;     the font gravity is specified (Since: 1.16.)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_new ()
;;;
;;; PangoFontDescription * pango_font_description_new (void);
;;;
;;; Creates a new font description structure with all fields unset.
;;;
;;; Returns :
;;;     the newly allocated PangoFontDescription, which should be freed using
;;;     pango_font_description_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_copy ()
;;;
;;; PangoFontDescription * pango_font_description_copy
;;;                                           (const PangoFontDescription *desc)
;;;
;;; Make a copy of a PangoFontDescription.
;;;
;;; desc :
;;;     a PangoFontDescription, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoFontDescription, which should be freed with
;;;     pango_font_description_free(), or NULL if desc was NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_copy_static ()
;;;
;;; PangoFontDescription * pango_font_description_copy_static
;;;                                           (const PangoFontDescription *desc)
;;;
;;; Like pango_font_description_copy(), but only a shallow copy is made of the
;;; family name and other allocated fields. The result can only be used until
;;; desc is modified or freed. This is meant to be used when the copy is only
;;; needed temporarily.
;;;
;;; desc :
;;;     a PangoFontDescription, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoFontDescription, which should be freed with
;;;     pango_font_description_free(), or NULL if desc was NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_hash ()
;;;
;;; guint pango_font_description_hash (const PangoFontDescription *desc);
;;;
;;; Computes a hash of a PangoFontDescription structure suitable to be used,
;;; for example, as an argument to g_hash_table_new(). The hash value is
;;; independent of desc->mask.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; Returns :
;;;     the hash value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_equal" pango-font-description-equal) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[desc1]{a @class{pango-font-description} structure}
  @argument[desc2]{another @class{pango-font-description} structure}
  @begin{return}
    @em{True} if the two font descriptions are identical, @code{nil} otherwise.
  @end{return}
  @begin{short}
    Compares two font descriptions for equality.
  @end{short}
  Two font descriptions are considered equal if the fonts they describe are
  provably identical. This means that their masks do not have to match, as long
  as other fields are all the same. Two font descriptions may result in
  identical fonts being loaded, but still compare @code{nil}."
  (desc1 (g-boxed-foreign pango-font-description))
  (desc2 (g-boxed-foreign pango-font-description)))

(export 'pango-font-description-equal)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_free ()
;;;
;;; void pango_font_description_free (PangoFontDescription *desc);
;;;
;;; Frees a font description.
;;;
;;; desc :
;;;     a PangoFontDescription, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_descriptions_free ()
;;;
;;; void pango_font_descriptions_free (PangoFontDescription **descs,
;;;                                    int n_descs);
;;;
;;; Frees an array of font descriptions.
;;;
;;; descs :
;;;     a pointer to an array of PangoFontDescription, may be NULL
;;;
;;; n_descs :
;;;     number of font descriptions in descs
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_family ()
;;;
;;; void pango_font_description_set_family (PangoFontDescription *desc,
;;;                                         const char *family);
;;;
;;; Sets the family name field of a font description. The family name represents
;;; a family of related font styles, and will resolve to a particular
;;; PangoFontFamily. In some uses of PangoFontDescription, it is also possible
;;; to use a comma separated list of family names for this field.
;;;
;;; desc :
;;;     a PangoFontDescription.
;;;
;;; family :
;;;     a string representing the family name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_family_static ()
;;;
;;; void pango_font_description_set_family_static (PangoFontDescription *desc,
;;;                                                const char *family);
;;;
;;; Like pango_font_description_set_family(), except that no copy of family is
;;; made. The caller must make sure that the string passed in stays around until
;;; desc has been freed or the name is set again. This function can be used if
;;; family is a static string such as a C string literal, or if desc is only
;;; needed temporarily.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; family :
;;;     a string representing the family name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_family ()
;;;
;;; const char * pango_font_description_get_family
;;;                                           (const PangoFontDescription *desc)
;;;
;;; Gets the family name field of a font description.
;;; See pango_font_description_set_family().
;;;
;;; desc :
;;;     a PangoFontDescription.
;;;
;;; Returns :
;;;     the family name field for the font description, or NULL if not
;;;     previously set. This has the same life-time as the font description
;;;     itself and should not be freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_style ()
;;;
;;; void pango_font_description_set_style (PangoFontDescription *desc,
;;;                                        PangoStyle style);
;;;
;;; Sets the style field of a PangoFontDescription. The PangoStyle enumeration
;;; describes whether the font is slanted and the manner in which it is slanted;
;;; it can be either PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC, or
;;; PANGO_STYLE_OBLIQUE. Most fonts will either have a italic style or an
;;; oblique style, but not both, and font matching in Pango will match italic
;;; specifications with oblique fonts and vice-versa if an exact match is not
;;; found.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; style :
;;;     the style for the font description
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_style ()
;;;
;;; PangoStyle pango_font_description_get_style
;;;                                           (const PangoFontDescription *desc)
;;;
;;; Gets the style field of a PangoFontDescription.
;;; See pango_font_description_set_style().
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; Returns :
;;;     the style field for the font description. Use
;;;     pango_font_description_get_set_fields() to find out if the field was
;;;     explicitly set or not.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_variant ()
;;;
;;; void pango_font_description_set_variant (PangoFontDescription *desc,
;;;                                          PangoVariant variant);
;;;
;;; Sets the variant field of a font description. The PangoVariant can either
;;; be PANGO_VARIANT_NORMAL or PANGO_VARIANT_SMALL_CAPS.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; variant :
;;;     the variant type for the font description
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_variant ()
;;;
;;; PangoVariant pango_font_description_get_variant
;;;                                           (const PangoFontDescription *desc)
;;;
;;; Gets the variant field of a PangoFontDescription.
;;; See pango_font_description_set_variant().
;;;
;;; desc :
;;;     a PangoFontDescription.
;;;
;;; Returns :
;;;     the variant field for the font description. Use
;;;     pango_font_description_get_set_fields() to find out if the field was
;;;     explicitly set or not.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_weight ()
;;;
;;; void pango_font_description_set_weight (PangoFontDescription *desc,
;;;                                         PangoWeight weight);
;;;
;;; Sets the weight field of a font description. The weight field specifies how
;;; bold or light the font should be. In addition to the values of the
;;; PangoWeight enumeration, other intermediate numeric values are possible.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; weight :
;;;     the weight for the font description
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_weight ()
;;;
;;; PangoWeight pango_font_description_get_weight
;;;                                           (const PangoFontDescription *desc)
;;;
;;; Gets the weight field of a font description.
;;; See pango_font_description_set_weight().
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; Returns :
;;;     the weight field for the font description. Use
;;;     pango_font_description_get_set_fields() to find out if the field was
;;;     explicitly set or not.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_stretch ()
;;;
;;; void pango_font_description_set_stretch (PangoFontDescription *desc,
;;;                                          PangoStretch stretch);
;;;
;;; Sets the stretch field of a font description. The stretch field specifies
;;; how narrow or wide the font should be.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; stretch :
;;;     the stretch for the font description
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_stretch ()
;;;
;;; PangoStretch pango_font_description_get_stretch
;;;                                           (const PangoFontDescription *desc)
;;;
;;; Gets the stretch field of a font description.
;;; See pango_font_description_set_stretch().
;;;
;;; desc :
;;;     a PangoFontDescription.
;;;
;;; Returns :
;;;     the stretch field for the font description. Use
;;;     pango_font_description_get_set_fields() to find out if the field was
;;;     explicitly set or not.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_size ()
;;;
;;; void pango_font_description_set_size (PangoFontDescription *desc, gint size)
;;;
;;; Sets the size field of a font description in fractional points. This is
;;; mutually exclusive with pango_font_description_set_absolute_size().
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; size :
;;;     the size of the font in points, scaled by PANGO_SCALE. (That is, a size
;;;     value of 10 * PANGO_SCALE is a 10 point font. The conversion factor
;;;     between points and device units depends on system configuration and the
;;;     output device. For screen display, a logical DPI of 96 is common, in
;;;     which case a 10 point font corresponds to a 10 * (96 / 72) = 13.3 pixel
;;;     font. Use pango_font_description_set_absolute_size() if you need a
;;;     particular size in device units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_size ()
;;;
;;; gint pango_font_description_get_size (const PangoFontDescription *desc);
;;;
;;; Gets the size field of a font description.
;;; See pango_font_description_set_size().
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; Returns :
;;;     the size field for the font description in points or device units. You
;;;     must call pango_font_description_get_size_is_absolute() to find out
;;;     which is the case. Returns 0 if the size field has not previously been
;;;     set or it has been set to 0 explicitly. Use
;;;     pango_font_description_get_set_fields() to find out if the field was
;;;     explicitly set or not.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_absolute_size ()
;;;
;;; void pango_font_description_set_absolute_size (PangoFontDescription *desc,
;;;                                                double size);
;;;
;;; Sets the size field of a font description, in device units. This is
;;; mutually exclusive with pango_font_description_set_size() which sets the
;;; font size in points.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; size :
;;;     the new size, in Pango units. There are PANGO_SCALE Pango units in one
;;;     device unit. For an output backend where a device unit is a pixel, a
;;;     size value of 10 * PANGO_SCALE gives a 10 pixel font.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_size_is_absolute ()
;;;
;;; gboolean pango_font_description_get_size_is_absolute
;;;                                           (const PangoFontDescription *desc)
;;;
;;; Determines whether the size of the font is in points (not absolute) or
;;; device units (absolute). See pango_font_description_set_size() and
;;; pango_font_description_set_absolute_size().
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; Returns :
;;;     whether the size for the font description is in points or device units.
;;;     Use pango_font_description_get_set_fields() to find out if the size
;;;     field of the font description was explicitly set or not.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_gravity ()
;;;
;;; void pango_font_description_set_gravity (PangoFontDescription *desc,
;;;                                          PangoGravity gravity);
;;;
;;; Sets the gravity field of a font description. The gravity field specifies
;;; how the glyphs should be rotated. If gravity is PANGO_GRAVITY_AUTO, this
;;; actually unsets the gravity mask on the font description.
;;;
;;; This function is seldom useful to the user. Gravity should normally be set
;;; on a PangoContext.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; gravity :
;;;     the gravity for the font description.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_gravity ()
;;;
;;; PangoGravity pango_font_description_get_gravity
;;;                                           (const PangoFontDescription *desc)
;;;
;;; Gets the gravity field of a font description.
;;; See pango_font_description_set_gravity().
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; Returns :
;;;     the gravity field for the font description. Use
;;;     pango_font_description_get_set_fields() to find out if the field was
;;;     explicitly set or not.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_set_fields ()
;;;
;;; PangoFontMask pango_font_description_get_set_fields
;;;                                           (const PangoFontDescription *desc)
;;;
;;; Determines which fields in a font description have been set.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; Returns :
;;;     a bitmask with bits set corresponding to the fields in desc that have
;;;     been set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_unset_fields ()
;;;
;;; void pango_font_description_unset_fields (PangoFontDescription *desc,
;;;                                           PangoFontMask to_unset);
;;;
;;; Unsets some of the fields in a PangoFontDescription. The unset fields will
;;; get back to their default values.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; to_unset :
;;;     bitmask of fields in the desc to unset.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_merge ()
;;;
;;; void pango_font_description_merge(PangoFontDescription *desc,
;;;                                   const PangoFontDescription *desc_to_merge,
;;;                                   gboolean replace_existing);
;;;
;;; Merges the fields that are set in desc_to_merge into the fields in desc. If
;;; replace_existing is FALSE, only fields in desc that are not already set are
;;; affected. If TRUE, then fields that are already set will be replaced as
;;; well.
;;;
;;; If desc_to_merge is NULL, this function performs nothing.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; desc_to_merge :
;;;     the PangoFontDescription to merge from, or NULL
;;;
;;; replace_existing :
;;;     if TRUE, replace fields in desc with the corresponding values from
;;;     desc_to_merge, even if they are already exist.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_merge_static ()
;;;
;;; void pango_font_description_merge_static
;;;                                  (PangoFontDescription *desc,
;;;                                   const PangoFontDescription *desc_to_merge,
;;;                                   gboolean replace_existing);
;;;
;;; Like pango_font_description_merge(), but only a shallow copy is made of the
;;; family name and other allocated fields. desc can only be used until
;;; desc_to_merge is modified or freed. This is meant to be used when the merged
;;; font description is only needed temporarily.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; desc_to_merge :
;;;     the PangoFontDescription to merge from
;;;
;;; replace_existing :
;;;     if TRUE, replace fields in desc with the corresponding values from
;;;     desc_to_merge, even if they are already exist.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_better_match ()
;;;
;;; gboolean pango_font_description_better_match
;;;                                      (const PangoFontDescription *desc,
;;;                                       const PangoFontDescription *old_match,
;;;                                       const PangoFontDescription *new_match)
;;;
;;; Determines if the style attributes of new_match are a closer match for desc
;;; than those of old_match are, or if old_match is NULL, determines if
;;; new_match is a match at all. Approximate matching is done for weight and
;;; style; other style attributes must match exactly. Style attributes are all
;;; attributes other than family and size-related attributes. Approximate
;;; matching for style considers PANGO_STYLE_OBLIQUE and PANGO_STYLE_ITALIC as
;;; matches, but not as good a match as when the styles are equal.
;;;
;;; Note that old_match must match desc.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; old_match :
;;;     a PangoFontDescription, or NULL
;;;
;;; new_match :
;;;     a PangoFontDescription
;;;
;;; Returns :
;;;     TRUE if new_match is a better match
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_from_string"
           pango-font-description-from-string)
    (g-boxed-foreign pango-font-description)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-29}
  @argument[str]{string representation of a font description}
  @return{A new @class{pango-font-description}.}
  @begin{short}
    Creates a new font description from a string representation in the form
    @code{[FAMILY-LIST] [STYLE-OPTIONS] [SIZE]}, where @code{FAMILY-LIST} is a
    comma separated list of families optionally terminated by a comma,
    @code{STYLE_OPTIONS} is a whitespace separated list of words where each
    @code{WORD} describes one of style, variant, weight, stretch, or gravity,
    and @code{SIZE} is a decimal number (size in points) or optionally followed
    by the unit modifier @code{px} for absolute size.
  @end{short}
  Any one of the options may be absent. If @code{FAMILY-LIST} is absent, then
  the @code{family_name} field of the resulting font description will
  be initialized to @code{NULL}. If @code{STYLE-OPTIONS} is missing, then all
  style options will be set to the default values. If @code{SIZE} is missing,
  the size in the resulting font description will be set to 0."
  (str :string))

(export 'pango-font-description-from-string)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_to_string"
           pango-font-description-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-6-29}
  @argument[desc]{a @class{pango-font-description} object}
  @return{A new string that must be freed with @code{g_free()}.}
  @begin{short}
    Creates a string representation of a font description.
  @end{short}
  See the function @fun{pango-font-description-from-string} for a description of
  the format of the string representation. The family list in the string
  description will only have a terminating comma if the last word of the list
  is a valid style option.
  @see-function{pango-font-description-from-string}"
  (desc (g-boxed-foreign pango-font-description)))

(export 'pango-font-description-to-string)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_to_filename ()
;;;
;;; char * pango_font_description_to_filename (const PangoFontDescription *desc)
;;;
;;; Creates a filename representation of a font description. The filename is
;;; identical to the result from calling pango_font_description_to_string(),
;;; but with underscores instead of characters that are untypical in filenames,
;;; and in lower case only.
;;;
;;; desc :
;;;     a PangoFontDescription
;;;
;;; Returns :
;;;     a new string that must be freed with g_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoFontMetrics
;;;
;;; struct PangoFontMetrics {
;;;   guint ref_count;
;;;
;;;   int ascent;
;;;   int descent;
;;;   int approximate_char_width;
;;;   int approximate_digit_width;
;;;   int underline_position;
;;;   int underline_thickness;
;;;   int strikethrough_position;
;;;   int strikethrough_thickness;
;;; };
;;;
;;; A PangoFontMetrics structure holds the overall metric information for a
;;; font (possibly restricted to a script). The fields of this structure are
;;; private to implementations of a font backend. See the documentation of the
;;; corresponding getters for documentation of their meaning.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_ref ()
;;;
;;; PangoFontMetrics * pango_font_metrics_ref (PangoFontMetrics *metrics);
;;;
;;; Increase the reference count of a font metrics structure by one.
;;;
;;; metrics :
;;;     a PangoFontMetrics structure, may be NULL
;;;
;;; Returns :
;;;     metrics
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_unref ()
;;;
;;; void pango_font_metrics_unref (PangoFontMetrics *metrics);
;;;
;;; Decrease the reference count of a font metrics structure by one. If the
;;; result is zero, frees the structure and any associated memory.
;;;
;;; metrics :
;;;     a PangoFontMetrics structure, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_ascent ()
;;;
;;; int pango_font_metrics_get_ascent (PangoFontMetrics *metrics);
;;;
;;; Gets the ascent from a font metrics structure. The ascent is the distance
;;; from the baseline to the logical top of a line of text. (The logical top
;;; may be above or below the top of the actual drawn ink. It is necessary to
;;; lay out the text to figure where the ink will be.)
;;;
;;; metrics :
;;;     a PangoFontMetrics structure
;;;
;;; Returns :
;;;     the ascent, in Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_descent ()
;;;
;;; int pango_font_metrics_get_descent (PangoFontMetrics *metrics);
;;;
;;; Gets the descent from a font metrics structure. The descent is the distance
;;; from the baseline to the logical bottom of a line of text. (The logical
;;; bottom may be above or below the bottom of the actual drawn ink. It is
;;; necessary to lay out the text to figure where the ink will be.)
;;;
;;; metrics :
;;;     a PangoFontMetrics structure
;;;
;;; Returns :
;;;     the descent, in Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_approximate_char_width ()
;;;
;;; int pango_font_metrics_get_approximate_char_width
;;;                                                  (PangoFontMetrics *metrics)
;;;
;;; Gets the approximate character width for a font metrics structure. This is
;;; merely a representative value useful, for example, for determining the
;;; initial size for a window. Actual characters in text will be wider and
;;; narrower than this.
;;;
;;; metrics :
;;;     a PangoFontMetrics structure
;;;
;;; Returns :
;;;     the character width, in Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_approximate_digit_width ()
;;;
;;; int pango_font_metrics_get_approximate_digit_width
;;;                                                  (PangoFontMetrics *metrics)
;;;
;;; Gets the approximate digit width for a font metrics structure. This is
;;; merely a representative value useful, for example, for determining the
;;; initial size for a window. Actual digits in text can be wider or narrower
;;; than this, though this value is generally somewhat more accurate than the
;;; result of pango_font_metrics_get_approximate_char_width() for digits.
;;;
;;; metrics :
;;;     a PangoFontMetrics structure
;;;
;;; Returns :
;;;     the digit width, in Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_underline_thickness ()
;;;
;;; int pango_font_metrics_get_underline_thickness (PangoFontMetrics *metrics);
;;;
;;; Gets the suggested thickness to draw for the underline.
;;;
;;; metrics :
;;;     a PangoFontMetrics structure
;;;
;;; Returns :
;;;     the suggested underline thickness, in Pango units.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_underline_position ()
;;;
;;; int pango_font_metrics_get_underline_position (PangoFontMetrics *metrics);
;;;
;;; Gets the suggested position to draw the underline. The value returned is
;;; the distance above the baseline of the top of the underline. Since most
;;; fonts have underline positions beneath the baseline, this value is
;;; typically negative.
;;;
;;; metrics :
;;;     a PangoFontMetrics structure
;;;
;;; Returns :
;;;     the suggested underline position, in Pango units.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_strikethrough_thickness ()
;;;
;;; int pango_font_metrics_get_strikethrough_thickness
;;;                                                  (PangoFontMetrics *metrics)
;;;
;;; Gets the suggested thickness to draw for the strikethrough.
;;;
;;; metrics :
;;;     a PangoFontMetrics structure
;;;
;;; Returns :
;;;     the suggested strikethrough thickness, in Pango units.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_strikethrough_position ()
;;;
;;; int pango_font_metrics_get_strikethrough_position
;;;                                                  (PangoFontMetrics *metrics)
;;;
;;; Gets the suggested position to draw the strikethrough. The value returned
;;; is the distance above the baseline of the top of the strikethrough.
;;;
;;; metrics :
;;;     a PangoFontMetrics structure
;;;
;;; Returns :
;;;     the suggested strikethrough position, in Pango units.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoFont
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoFont" pango-font
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "pango_font_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'pango-font 'type)
 "@version{2013-6-29}
  @begin{short}
    The @sym{pango-font} structure is used to represent a font in a
    rendering-system-independent matter.
  @end{short}
  To create an implementation of a @sym{pango-font}, the rendering-system
  specific code should allocate a larger structure that contains a nested
  @sym{pango-font}, fill in the klass member of the nested @sym{pango-font} with
  a pointer to a appropriate @code{PangoFontClass}, then call the function
  @code{pango_font_init ()} on the structure.

  The @sym{pango-font} structure contains one member which the implementation
  fills in.")

;;; ----------------------------------------------------------------------------
;;; pango_font_find_shaper ()
;;;
;;; PangoEngineShape * pango_font_find_shaper (PangoFont *font,
;;;                                            PangoLanguage *language,
;;;                                            guint32 ch);
;;;
;;; Finds the best matching shaper for a font for a particular language tag and
;;; character point.
;;;
;;; font :
;;;     a PangoFont
;;;
;;; language :
;;;     the language tag
;;;
;;; ch :
;;;     a Unicode character.
;;;
;;; Returns :
;;;     the best matching shaper.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_describe ()
;;;
;;; PangoFontDescription * pango_font_describe (PangoFont *font);
;;;
;;; Returns a description of the font, with font size set in points. Use
;;; pango_font_describe_with_absolute_size() if you want the font size in
;;; device units.
;;;
;;; font :
;;;     a PangoFont
;;;
;;; Returns :
;;;     a newly-allocated PangoFontDescription object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_describe_with_absolute_size ()
;;;
;;; PangoFontDescription * pango_font_describe_with_absolute_size
;;;                                                            (PangoFont *font)
;;;
;;; Returns a description of the font, with absolute font size set (in device
;;; units). Use pango_font_describe() if you want the font size in points.
;;;
;;; font :
;;;     a PangoFont
;;;
;;; Returns :
;;;     a newly-allocated PangoFontDescription object.
;;;
;;; Since 1.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_get_coverage ()
;;;
;;; PangoCoverage * pango_font_get_coverage (PangoFont *font,
;;;                                          PangoLanguage *language);
;;;
;;; Computes the coverage map for a given font and language tag.
;;;
;;; font :
;;;     a PangoFont
;;;
;;; language :
;;;     the language tag
;;;
;;; Returns :
;;;     a newly-allocated PangoCoverage object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_get_glyph_extents ()
;;;
;;; void pango_font_get_glyph_extents (PangoFont *font,
;;;                                    PangoGlyph glyph,
;;;                                    PangoRectangle *ink_rect,
;;;                                    PangoRectangle *logical_rect);
;;;
;;; Gets the logical and ink extents of a glyph within a font. The coordinate
;;; system for each rectangle has its origin at the base line and horizontal
;;; origin of the character with increasing coordinates extending to the right
;;; and down. The macros PANGO_ASCENT(), PANGO_DESCENT(), PANGO_LBEARING(), and
;;; PANGO_RBEARING() can be used to convert from the extents rectangle to more
;;; traditional font metrics. The units of the rectangles are in 1/PANGO_SCALE
;;; of a device unit.
;;;
;;; If font is NULL, this function gracefully sets some sane values in the
;;; output variables and returns.
;;;
;;; font :
;;;     a PangoFont
;;;
;;; glyph :
;;;     the glyph index
;;;
;;; ink_rect :
;;;     rectangle used to store the extents of the glyph as drawn or NULL to
;;;     indicate that the result is not needed
;;;
;;; logical_rect :
;;;     rectangle used to store the logical extents of the glyph or NULL to
;;;     indicate that the result is not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_get_metrics ()
;;;
;;; PangoFontMetrics * pango_font_get_metrics (PangoFont *font,
;;;                                            PangoLanguage *language);
;;;
;;; Gets overall metric information for a font. Since the metrics may be
;;; substantially different for different scripts, a language tag can be
;;; provided to indicate that the metrics should be retrieved that correspond
;;; to the script(s) used by that language.
;;;
;;; If font is NULL, this function gracefully sets some sane values in the
;;; output variables and returns.
;;;
;;; font :
;;;     a PangoFont
;;;
;;; language :
;;;     language tag used to determine which script to get the metrics for, or
;;;     NULL to indicate to get the metrics for the entire font
;;;
;;; Returns :
;;;     a PangoFontMetrics object. The caller must call
;;;     pango_font_metrics_unref() when finished using the object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_get_font_map ()
;;;
;;; PangoFontMap * pango_font_get_font_map (PangoFont *font);
;;;
;;; Gets the font map for which the font was created.
;;;
;;; Note that the font maintains a weak reference to the font map, so if all
;;; references to font map are dropped, the font map will be finalized even if
;;; there are fonts created with the font map that are still alive. In that case
;;; this function will return NULL. It is the responsibility of the user to
;;; ensure that the font map is kept alive. In most uses this is not an issue
;;; as a PangoContext holds a reference to the font map.
;;;
;;; font :
;;;     a PangoFont, or NULL
;;;
;;; Returns :
;;;     the PangoFontMap for the font, or NULL if font is NULL
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoFontFamily
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoFontFamily" pango-font-family
  (:superclass g-object
    :export t
    :interfaces nil
    :type-initializer "pango_font_family_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'pango-font-family 'type)
 "@version{2013-6-29}
  @begin{short}
    The @sym{pango-font-family} structure is used to represent a family of
    related font faces.
  @end{short}
  The faces in a family share a common design, but differ in slant, weight,
  width and other aspects.")

;;; ----------------------------------------------------------------------------
;;; pango_font_family_get_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_family_get_name" pango-font-family-get-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-6-29}
  @argument[family]{a @class{pango-font-family} object}
  @begin{return}
    The name of the family. This string is owned by the family object and
    must not be modified or freed.
  @end{return}
  @begin{short}
    Gets the name of the family.
  @end{short}
  The name is unique among all fonts for the font backend and can be used in a
  PangoFontDescription to specify that a face from this family is desired."
  (family (g-object pango-font-family)))

(export 'pango-font-family-get-name)

;;; ----------------------------------------------------------------------------
;;; pango_font_family_is_monospace ()
;;;
;;; gboolean pango_font_family_is_monospace (PangoFontFamily *family);
;;;
;;; A monospace font is a font designed for text display where the the
;;; characters form a regular grid. For Western languages this would mean that
;;; the advance width of all characters are the same, but this categorization
;;; also includes Asian fonts which include double-width characters: characters
;;; that occupy two grid cells. g_unichar_iswide() returns a result that
;;; indicates whether a character is typically double-width in a monospace font.
;;;
;;; The best way to find out the grid-cell size is to call
;;; pango_font_metrics_get_approximate_digit_width(), since the results of
;;; pango_font_metrics_get_approximate_char_width() may be affected by
;;; double-width characters.
;;;
;;; family :
;;;     a PangoFontFamily
;;;
;;; Returns :
;;;     TRUE if the family is monospace.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_family_list_faces ()
;;;
;;; void pango_font_family_list_faces (PangoFontFamily *family,
;;;                                    PangoFontFace ***faces,
;;;                                    int *n_faces);
;;;
;;; Lists the different font faces that make up family. The faces in a family
;;; share a common design, but differ in slant, weight, width and other aspects.
;;;
;;; family :
;;;     a PangoFontFamily
;;;
;;; faces :
;;;     location to store an array of pointers to PangoFontFace objects, or
;;;     NULL. This array should be freed with g_free() when it is no longer
;;;     needed
;;;
;;; n_faces :
;;;     location to store number of elements in faces
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoFontFace
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoFontFace" pango-font-face
  (:superclass g-object
    :export t
    :interfaces nil
    :type-initializer "pango_font_face_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'pango-font-face 'type)
 "@version{2013-6-29}
  The @sym{pango-font-face} structure is used to represent a group of fonts with
  the same family, slant, weight, width, but varying sizes.")

;;; ----------------------------------------------------------------------------
;;; pango_font_face_get_face_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_face_get_face_name" pango-font-face-get-face-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-6-29}
  @argument[face]{a @class{pango-font-face} object}
  @begin{return}
    The face name for the face. This string is owned by the face object and
    must not be modified or freed.
  @end{return}
  @begin{short}
    Gets a name representing the style of this face among the different faces
    in the @class{pango-font-family} for the face.
  @end{short}
  This name is unique among all faces in the family and is suitable for
  displaying to users."
  (face (g-object pango-font-face)))

(export 'pango-font-face-get-face-name)

;;; ----------------------------------------------------------------------------
;;; pango_font_face_list_sizes ()
;;;
;;; void pango_font_face_list_sizes (PangoFontFace *face,
;;;                                  int **sizes,
;;;                                  int *n_sizes);
;;;
;;; List the available sizes for a font. This is only applicable to bitmap
;;; fonts. For scalable fonts, stores NULL at the location pointed to by sizes
;;; and 0 at the location pointed to by n_sizes. The sizes returned are in
;;; Pango units and are sorted in ascending order.
;;;
;;; face :
;;;     a PangoFontFace.
;;;
;;; sizes :
;;;     location to store a pointer to an array of int. This array should be
;;;     freed with g_free().
;;;
;;; n_sizes :
;;;     location to store the number of elements in sizes
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_face_describe ()
;;;
;;; PangoFontDescription * pango_font_face_describe (PangoFontFace *face);
;;;
;;; Returns the family, style, variant, weight and stretch of a PangoFontFace.
;;; The size field of the resulting font description will be unset.
;;;
;;; face :
;;;     a PangoFontFace
;;;
;;; Returns :
;;;     a newly-created PangoFontDescription structure holding the description
;;;     of the face. Use pango_font_description_free() to free the result.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_face_is_synthesized ()
;;;
;;; gboolean pango_font_face_is_synthesized (PangoFontFace *face);
;;;
;;; Returns whether a PangoFontFace is synthesized by the underlying font
;;; rendering engine from another face, perhaps by shearing, emboldening, or
;;; lightening it.
;;;
;;; face :
;;;     a PangoFontFace
;;;
;;; Returns :
;;;     whether face is synthesized.
;;;
;;; Since 1.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoFontMap
;;;
;;; typedef struct _PangoFontMap PangoFontMap;
;;;
;;; The PangoFontMap represents the set of fonts available for a particular
;;; rendering system. This is a virtual object with implementations being
;;; specific to particular rendering systems. To create an implementation of a
;;; PangoFontMap, the rendering-system specific code should allocate a larger
;;; structure that contains a nested PangoFontMap, fill in the klass member of
;;; the nested PangoFontMap with a pointer to a appropriate PangoFontMapClass,
;;; then call pango_font_map_init() on the structure.
;;;
;;; The PangoFontMap structure contains one member which the implementation
;;; fills in.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoFontMapClass
;;;
;;; struct PangoFontMapClass {
;;;   GObjectClass parent_class;
;;;
;;;
;;;   PangoFont *   (*load_font)     (PangoFontMap               *fontmap,
;;;                                   PangoContext               *context,
;;;                                   const PangoFontDescription *desc);
;;;   void          (*list_families) (PangoFontMap               *fontmap,
;;;                                   PangoFontFamily          ***families,
;;;                                   int                        *n_families);
;;;   PangoFontset *(*load_fontset)  (PangoFontMap               *fontmap,
;;;                                   PangoContext               *context,
;;;                                   const PangoFontDescription *desc,
;;;                                   PangoLanguage              *language);
;;;
;;;   const char *shape_engine_type;
;;; };
;;;
;;; The PangoFontMapClass structure holds the virtual functions for a
;;; particular PangoFontMap implementation.
;;;
;;; GObjectClass parent_class;
;;;     parent GObjectClass.
;;;
;;; load_font ()
;;;     a function to load a font with a given description.
;;;     See pango_font_map_load_font().
;;;
;;; list_families ()
;;;     A function to list available font families.
;;;     See pango_font_map_list_families().
;;;
;;; load_fontset ()
;;;     a function to load a fontset with a given given description suitable
;;;     for a particular language. See pango_font_map_load_fontset().
;;;
;;; const char *shape_engine_type;
;;;     the type of rendering-system-dependent engines that can handle fonts of
;;;     this fonts loaded with this fontmap.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_map_create_context ()
;;;
;;; PangoContext * pango_font_map_create_context (PangoFontMap *fontmap);
;;;
;;; Creates a PangoContext connected to fontmap. This is equivalent to
;;; pango_context_new() followed by pango_context_set_font_map().
;;;
;;; If you are using Pango as part of a higher-level system, that system may
;;; have it's own way of create a PangoContext. For instance, the GTK+ toolkit
;;; has, among others, gdk_pango_context_get_for_screen(), and
;;; gtk_widget_get_pango_context(). Use those instead.
;;;
;;; fontmap :
;;;     a PangoFontMap
;;;
;;; Returns :
;;;     the newly allocated PangoContext, which should be freed with
;;;     g_object_unref()
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_map_load_font ()
;;;
;;; PangoFont * pango_font_map_load_font (PangoFontMap *fontmap,
;;;                                       PangoContext *context,
;;;                                       const PangoFontDescription *desc);
;;;
;;; Load the font in the fontmap that is the closest match for desc.
;;;
;;; fontmap :
;;;     a PangoFontMap
;;;
;;; context :
;;;     the PangoContext the font will be used with
;;;
;;; desc :
;;;     a PangoFontDescription describing the font to load
;;;
;;; Returns :
;;;     the newly allocated PangoFont loaded, or NULL if no font matched
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_map_load_fontset ()
;;;
;;; PangoFontset * pango_font_map_load_fontset
;;;                                           (PangoFontMap *fontmap,
;;;                                            PangoContext *context,
;;;                                            const PangoFontDescription *desc,
;;;                                            PangoLanguage *language);
;;;
;;; Load a set of fonts in the fontmap that can be used to render a font
;;; matching desc.
;;;
;;; fontmap :
;;;     a PangoFontMap
;;;
;;; context :
;;;     the PangoContext the font will be used with
;;;
;;; desc :
;;;     a PangoFontDescription describing the font to load
;;;
;;; language :
;;;     a PangoLanguage the fonts will be used for
;;;
;;; Returns :
;;;     the newly allocated PangoFontset loaded, or NULL if no font matched
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_map_list_families ()
;;;
;;; void pango_font_map_list_families (PangoFontMap *fontmap,
;;;                                    PangoFontFamily ***families,
;;;                                    int *n_families);
;;;
;;; List all families for a fontmap.
;;;
;;; fontmap :
;;;     a PangoFontMap
;;;
;;; families :
;;;     location to store a pointer to an array of PangoFontFamily *. This
;;;     array should be freed with g_free().
;;;
;;; n_families :
;;;     location to store the number of elements in families.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_map_get_shape_engine_type ()
;;;
;;; const char * pango_font_map_get_shape_engine_type (PangoFontMap *fontmap);
;;;
;;; Returns the render ID for shape engines for this fontmap. See
;;; the render_type field of PangoEngineInfo.
;;;
;;; fontmap :
;;;     a PangoFontMap
;;;
;;; Returns :
;;;     the ID string for shape engines for this fontmap. Owned by Pango,
;;;     should not be modified or freed.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoFontset
;;;
;;; struct PangoFontset;
;;;
;;; A PangoFontset represents a set of PangoFont to use when rendering text. It
;;; is the result of resolving a PangoFontDescription against a particular
;;; PangoContext. It has operations for finding the component font for a
;;; particular Unicode character, and for finding a composite set of metrics
;;; for the entire fontset.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoFontsetClass
;;;
;;; struct PangoFontsetClass {
;;;   GObjectClass parent_class;
;;;
;;;
;;;   PangoFont *       (*get_font)     (PangoFontset     *fontset,
;;;                      guint             wc);
;;;
;;;   PangoFontMetrics *(*get_metrics)  (PangoFontset     *fontset);
;;;   PangoLanguage *   (*get_language) (PangoFontset     *fontset);
;;;   void              (*foreach)      (PangoFontset           *fontset,
;;;                      PangoFontsetForeachFunc func,
;;;                      gpointer                data);
;;; };
;;;
;;; The PangoFontsetClass structure holds the virtual functions for a particular
;;; PangoFontset implementation.
;;;
;;; GObjectClass parent_class;
;;;     parent GObjectClass.
;;;
;;; get_font ()
;;;     a function to get the font in the fontset that contains the best glyph
;;;     for the given Unicode character; see pango_fontset_get_font().
;;;
;;; get_metrics ()
;;;     a function to get overall metric information for the fonts in the
;;;     fontset; see pango_fontset_get_metrics().
;;;
;;; get_language ()
;;;     a function to get the language of the fontset.
;;;
;;; foreach ()
;;;     a function to loop over the fonts in the fontset.
;;;     See pango_fontset_foreach().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_fontset_get_font ()
;;;
;;; PangoFont * pango_fontset_get_font (PangoFontset *fontset, guint wc);
;;;
;;; Returns the font in the fontset that contains the best glyph for the
;;; Unicode character wc.
;;;
;;; fontset :
;;;     a PangoFontset
;;;
;;; wc :
;;;     a Unicode character
;;;
;;; Returns :
;;;     a PangoFont. The caller must call g_object_unref when finished with
;;;     the font
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_fontset_get_metrics ()
;;;
;;; PangoFontMetrics * pango_fontset_get_metrics (PangoFontset *fontset);
;;;
;;; Get overall metric information for the fonts in the fontset.
;;;
;;; fontset :
;;;     a PangoFontset
;;;
;;; Returns :
;;;     a PangoFontMetrics object. The caller must call
;;;     pango_font_metrics_unref() when finished using the object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoFontsetForeachFunc ()
;;;
;;; gboolean (*PangoFontsetForeachFunc) (PangoFontset *fontset,
;;;                                      PangoFont *font,
;;;                                      gpointer data);
;;;
;;; A callback function used by pango_fontset_foreach() when enumerating the
;;; fonts in a fontset.
;;;
;;; fontset :
;;;     a PangoFontset
;;;
;;; font :
;;;     a font from fontset
;;;
;;; data :
;;;     callback data
;;;
;;; Returns :
;;;     if TRUE, stop iteration and return immediately.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_fontset_foreach ()
;;;
;;; void pango_fontset_foreach (PangoFontset *fontset,
;;;                             PangoFontsetForeachFunc func,
;;;                             gpointer data);
;;;
;;; Iterates through all the fonts in a fontset, calling func for each one. If
;;; func returns TRUE, that stops the iteration.
;;;
;;; fontset :
;;;     a PangoFontset
;;;
;;; func :
;;;     Callback function
;;;
;;; data :
;;;     data to pass to the callback function
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoFontsetSimple
;;;
;;; typedef struct _PangoFontsetSimple PangoFontsetSimple;
;;;
;;; PangoFontsetSimple is a implementation of the abstract PangoFontset base
;;; class in terms of an array of fonts, which the creator provides when
;;; constructing the PangoFontsetSimple.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_fontset_simple_new ()
;;;
;;; PangoFontsetSimple * pango_fontset_simple_new(PangoLanguage *language);
;;;
;;; Creates a new PangoFontsetSimple for the given language.
;;;
;;; language :
;;;     a PangoLanguage tag
;;;
;;; Returns :
;;;     The newly allocated PangoFontsetSimple, which should be freed with
;;;     g_object_unref().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_fontset_simple_append ()
;;;
;;; void pango_fontset_simple_append (PangoFontsetSimple *fontset,
;;;                                   PangoFont *font);
;;;
;;; Adds a font to the fontset
;;;
;;; fontset :
;;;     a PangoFontsetSimple
;;;
;;; font :
;;;     a PangoFont
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_fontset_simple_size ()
;;;
;;; int pango_fontset_simple_size (PangoFontsetSimple *fontset);
;;;
;;; Returns the number of fonts in the fontset.
;;;
;;; fontset :
;;;     a PangoFontsetSimple
;;;
;;; Returns :
;;;     The size of fontset.
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.fonts.lisp -------------------------------------------
