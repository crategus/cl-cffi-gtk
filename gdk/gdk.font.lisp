;;; ----------------------------------------------------------------------------
;;; gdk.font.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; Version 2.24.10. See http://www.gtk.org.
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
;;; Fonts
;;; 
;;; Loading and manipulating fonts
;;; 
;;; Synopsis
;;; 
;;;     GdkFont
;;;     GdkFontType
;;;
;;;     gdk_font_load
;;;     gdk_font_load_for_display
;;;     gdk_fontset_load
;;;     gdk_fontset_load_for_display
;;;     gdk_font_from_description
;;;     gdk_font_from_description_for_display
;;;     gdk_font_get_display
;;;     gdk_font_ref
;;;     gdk_font_unref
;;;     gdk_font_id
;;;     gdk_font_equal
;;; 
;;;     gdk_string_extents
;;;     gdk_text_extents
;;;     gdk_text_extents_wc
;;;     gdk_string_width
;;;     gdk_text_width
;;;     gdk_text_width_wc
;;;     gdk_char_width
;;;     gdk_char_width_wc
;;;     gdk_string_measure
;;;     gdk_text_measure
;;;     gdk_char_measure
;;;     gdk_string_height
;;;     gdk_text_height
;;;     gdk_char_height
;;; 
;;;     GdkWChar
;;;
;;;     gdk_wcstombs
;;;     gdk_mbstowcs
;;; 
;;; Description
;;; 
;;; The GdkFont data type represents a font for drawing on the screen. These
;;; functions provide support for loading fonts, and also for determining the
;;; dimensions of characters and strings when drawn with a particular font.
;;; 
;;; Fonts in X are specified by a X Logical Font Description. The following
;;; description is considerably simplified. For definitive information about
;;; XLFD's see the X reference documentation. A X Logical Font Description
;;; (XLFD) consists of a sequence of fields separated (and surrounded by) '-'
;;; characters. For example, Adobe Helvetica Bold 12 pt, has the full
;;; description:
;;; 
;;; "-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1"
;;; 
;;; The fields in the XLFD are:
;;;
;;; Foundry
;;;    the company or organization where the font originated.
;;;
;;; Family 
;;;     the font family (a group of related font designs).
;;;
;;; Weight 
;;;     A name for the font's typographic weight For example, 'bold' or
;;;     'medium').
;;;
;;; Slant 
;;;     The slant of the font. Common values are 'R' for Roman, 'I' for italoc,
;;;     and 'O' for oblique.
;;;
;;; Set Width 
;;;     A name for the width of the font. For example, 'normal' or 'condensed'.
;;;
;;; Add Style 
;;;     Additional information to distinguish a font from other fonts of the
;;;     same family.
;;;
;;; Pixel Size 
;;;     The body size of the font in pixels.
;;;
;;; Point Size 
;;;     The body size of the font in 10ths of a point. (A point is 1/72.27 inch)
;;;
;;; Resolution X 
;;;     The horizontal resolution that the font was designed for.
;;;
;;; Resolution Y 
;;;     The vertical resolution that the font was designed for .
;;;
;;; Spacing 
;;;     The type of spacing for the font - can be 'p' for proportional, 'm' for
;;;     monospaced or 'c' for charcell.
;;;
;;; Average Width 
;;;     The average width of a glyph in the font. For monospaced and charcell
;;;     fonts, all glyphs in the font have this width
;;;
;;; Charset Registry 
;;;     The registration authority that owns the encoding for the font.
;;;     Together with the Charset Encoding field, this defines the character
;;;     set for the font.
;;;
;;; Charset Encoding 
;;;     An identifier for the particular character set encoding.
;;; 
;;; When specifying a font via a X logical Font Description, '*' can be used as
;;; a wildcard to match any portion of the XLFD. For instance, the above example
;;; could also be specified as
;;; 
;;; "-*-helvetica-bold-r-normal--*-120-*-*-*-*-iso8859-1"
;;; 
;;; It is generally a good idea to use wildcards for any portion of the XLFD
;;; that your program does not care about specifically, since that will improve
;;; the chances of finding a matching font.
;;; 
;;; A fontset is a list of fonts that is used for drawing international text
;;; that may contain characters from a number of different character sets. It
;;; is represented by a list of XLFD's.
;;; 
;;; The font for a given character set is determined by going through the list
;;; of XLFD's in order. For each one, if the registry and and encoding fields
;;; match the desired character set, then that font is used, otherwise if the
;;; XLFD contains wild-cards for the registry and encoding fields, the registry
;;; and encoding for the desired character set are substituted in and a lookup
;;; is done. If a match is found that font is used. Otherwise, processing
;;; continues on to the next font in the list.
;;; 
;;; The functions for determining the metrics of a string come in several
;;; varieties that can take a number of forms of string input:
;;; 
;;; 8-bit string
;;;     When using functions like gdk_string_width() that take a gchar *, if the
;;;     font is of type GDK_FONT_FONT and is an 8-bit font, then each gchar
;;;     indexes the glyphs in the font directly.
;;; 
;;; 16-bit string
;;;     For functions taking a gchar *, if the font is of type GDK_FONT_FONT,
;;;     and is a 16-bit font, then the gchar * argument is interpreted as a
;;;     guint16 * cast to a gchar * and each guint16 indexes the glyphs in the
;;;     font directly.
;;; 
;;; Multibyte string
;;;     For functions taking a gchar *, if the font is of type GDK_FONT_FONTSET,
;;;     then the input string is interpreted as a multibyte encoded according to
;;;     the current locale. (A multibyte string is one in which each character
;;;     may consist of one or more bytes, with different lengths for different
;;;     characters in the string). They can be converted to and from wide
;;;     character strings (see below) using gdk_wcstombs() and gdk_mbstowcs().)
;;;     The string will be rendered using one or more different fonts from the
;;;     fontset.
;;; 
;;; Wide character string
;;;     For a number of the text-measuring functions, GDK provides a variant
;;;     (such as gdk_text_width_wc()) which takes a GdkWChar * instead of a
;;;     gchar *. The input is then taken to be a wide character string in the
;;;     encoding of the current locale. (A wide character string is a string in
;;;     which each character consists of several bytes, and the width of each
;;;     character in the string is constant.)
;;; 
;;; GDK provides functions to determine a number of different measurements
;;; (metrics) for a given string. (Need diagram here).
;;; 
;;; ascent
;;;     The vertical distance from the origin of the drawing opereration to the
;;;     top of the drawn character.
;;; 
;;; descent
;;;     The vertical distance from the origin of the drawing opereration to the
;;;     bottom of the drawn character.
;;; 
;;; left bearing
;;;     The horizontal distance from the origin of the drawing operation to the
;;;     left-most part of the drawn character.
;;; 
;;; right bearing
;;;     The horizontal distance from the origin of the drawing operation to the
;;;     right-most part of the drawn character.
;;; 
;;; width bearing
;;;     The horizontal distance from the origin of the drawing operation to the
;;;     correct origin for drawing another string to follow the current one.
;;;     Depending on the font, this could be greater than or less than the right
;;;     bearing.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkFontType
;;; 
;;; typedef enum
;;; {
;;;   GDK_FONT_FONT,
;;;   GDK_FONT_FONTSET
;;; } GdkFontType;
;;; 
;;; Warning
;;; 
;;; GdkFontType is deprecated and should not be used in newly-written code.
;;; 
;;; Indicates the type of a font. The possible values are currently:
;;; 
;;; GDK_FONT_FONT
;;;     the font is a single font.
;;; 
;;; GDK_FONT_FONTSET
;;;     the font is a fontset.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkFontType" gdk-font-type () :font :fontset)

;;; ----------------------------------------------------------------------------
;;; GdkFont
;;; 
;;; typedef struct {
;;;   GdkFontType type;
;;;   gint ascent;
;;;   gint descent;
;;; } GdkFont;
;;; 
;;; Warning
;;; 
;;; GdkFont is deprecated and should not be used in newly-written code.
;;; 
;;; The GdkFont structure represents a font or fontset. It contains the
;;; following public fields. A new GdkFont structure is returned by
;;; gdk_font_load() or gdk_fontset_load(), and is reference counted with
;;; gdk_font_ref() and gdk_font_unref()
;;; 
;;; GdkFontType type;
;;;     a value of type GdkFontType which indicates whether this font is a
;;;     single font or a fontset.
;;; 
;;; gint ascent;
;;;     the maximum distance that the font, when drawn, ascends above the
;;;     baseline.
;;; 
;;; gint descent;
;;;     the maximum distance that the font, when drawn, descends below the
;;;     baseline.
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque gdk-font "GdkFont"
  :alloc (error "GDK:FONT objects may not be allocated directly"))

(export (boxed-related-symbols 'gdk-font))

;;; ----------------------------------------------------------------------------
;;; gdk_font_load ()
;;; 
;;; GdkFont * gdk_font_load (const gchar *font_name);
;;; 
;;; Warning
;;; 
;;; gdk_font_load is deprecated and should not be used in newly-written code.
;;; 
;;; Loads a font.
;;; 
;;; The font may be newly loaded or looked up the font in a cache. You should
;;; make no assumptions about the initial reference count.
;;; 
;;; font_name :
;;;     a XLFD describing the font to load.
;;; 
;;; Returns :
;;;     a GdkFont, or NULL if the font could not be loaded.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_font_load_for_display ()
;;; 
;;; GdkFont * gdk_font_load_for_display (GdkDisplay *display,
;;;                                      const gchar *font_name);
;;; 
;;; Warning
;;; 
;;; gdk_font_load_for_display is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Loads a font for use on display.
;;; 
;;; The font may be newly loaded or looked up the font in a cache. You should
;;; make no assumptions about the initial reference count.
;;; 
;;; display :
;;;     a GdkDisplay
;;; 
;;; font_name :
;;;     a XLFD describing the font to load.
;;; 
;;; Returns :
;;;     a GdkFont, or NULL if the font could not be loaded.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_fontset_load ()
;;; 
;;; GdkFont * gdk_fontset_load (const gchar *fontset_name);
;;; 
;;; Warning
;;; 
;;; gdk_fontset_load is deprecated and should not be used in newly-written code.
;;; 
;;; Loads a fontset.
;;; 
;;; The fontset may be newly loaded or looked up in a cache. You should make no
;;; assumptions about the initial reference count.
;;; 
;;; fontset_name :
;;;     a comma-separated list of XLFDs describing the component fonts of the
;;;     fontset to load.
;;; 
;;; Returns :
;;;     a GdkFont, or NULL if the fontset could not be loaded.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_fontset_load_for_display ()
;;; 
;;; GdkFont * gdk_fontset_load_for_display (GdkDisplay *display,
;;;                                         const gchar *fontset_name);
;;; 
;;; Warning
;;; 
;;; gdk_fontset_load_for_display is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Loads a fontset for use on display.
;;; 
;;; The fontset may be newly loaded or looked up in a cache. You should make
;;; no assumptions about the initial reference count.
;;; 
;;; display :
;;;     a GdkDisplay
;;; 
;;; fontset_name :
;;;     a comma-separated list of XLFDs describing the component fonts of the
;;;     fontset to load.
;;; 
;;; Returns :
;;;     a GdkFont, or NULL if the fontset could not be loaded.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_font_from_description ()
;;; 
;;; GdkFont * gdk_font_from_description  (PangoFontDescription *font_desc);
;;; 
;;; Warning
;;; 
;;; gdk_font_from_description is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Load a GdkFont based on a Pango font description. This font will only be
;;; an approximation of the Pango font, and internationalization will not be
;;; handled correctly. This function should only be used for legacy code that
;;; cannot be easily converted to use Pango. Using Pango directly will produce
;;; better results.
;;; 
;;; font_desc :
;;;     a PangoFontDescription.
;;; 
;;; Returns :
;;;     the newly loaded font, or NULL if the font cannot be loaded.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_font_from_description_for_display ()
;;; 
;;; GdkFont * gdk_font_from_description_for_display
;;;                                            (GdkDisplay *display,
;;;                                             PangoFontDescription *font_desc)
;;; 
;;; Warning
;;; 
;;; gdk_font_from_description_for_display is deprecated and should not be used
;;; in newly-written code.
;;; 
;;; Loads a GdkFont based on a Pango font description for use on display. This
;;; font will only be an approximation of the Pango font, and
;;; internationalization will not be handled correctly. This function should
;;; only be used for legacy code that cannot be easily converted to use Pango.
;;; Using Pango directly will produce better results.
;;; 
;;; display :
;;;     a GdkDisplay
;;; 
;;; font_desc :
;;;     a PangoFontDescription.
;;; 
;;; Returns :
;;;     the newly loaded font, or NULL if the font cannot be loaded.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_font_get_display ()
;;; 
;;; GdkDisplay * gdk_font_get_display (GdkFont *font);
;;; 
;;; Warning
;;; 
;;; gdk_font_get_display is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Returns the GdkDisplay for font.
;;; 
;;; font :
;;;     the GdkFont.
;;; 
;;; Returns :
;;;     the corresponding GdkDisplay.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_font_ref ()
;;; 
;;; GdkFont * gdk_font_ref (GdkFont *font);
;;; 
;;; Warning
;;; 
;;; gdk_font_ref is deprecated and should not be used in newly-written code.
;;; 
;;; Increases the reference count of a font by one.
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; Returns :
;;;     font
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_font_unref ()
;;; 
;;; void gdk_font_unref (GdkFont *font);
;;; 
;;; Warning
;;; 
;;; gdk_font_unref is deprecated and should not be used in newly-written code.
;;; 
;;; Decreases the reference count of a font by one. If the result is zero,
;;; destroys the font.
;;; 
;;; font :
;;;     a GdkFont
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_font_id ()
;;; 
;;; gint gdk_font_id (const GdkFont *font);
;;; 
;;; Warning
;;; 
;;; gdk_font_id is deprecated and should not be used in newly-written code.
;;; 
;;; Returns the X Font ID for the given font.
;;; 
;;; font :
;;;     a GdkFont.
;;; 
;;; Returns :
;;;     the numeric X Font ID
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_font_equal ()
;;; 
;;; gboolean gdk_font_equal (const GdkFont *fonta, const GdkFont *fontb);
;;; 
;;; Warning
;;; 
;;; gdk_font_equal is deprecated and should not be used in newly-written code.
;;; 
;;; Compares two fonts for equality. Single fonts compare equal if they have
;;; the same X font ID. This operation does not currently work correctly for
;;; fontsets.
;;; 
;;; fonta :
;;;     a GdkFont.
;;; 
;;; fontb :
;;;     another GdkFont.
;;; 
;;; Returns :
;;;     TRUE if the fonts are equal.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_string_extents ()
;;; 
;;; void gdk_string_extents (GdkFont *font,
;;;                          const gchar *string,
;;;                          gint *lbearing,
;;;                          gint *rbearing,
;;;                          gint *width,
;;;                          gint *ascent,
;;;                          gint *descent);
;;; 
;;; Warning
;;; 
;;; gdk_string_extents is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Gets the metrics of a nul-terminated string.
;;; 
;;; font :
;;;     a GdkFont.
;;; 
;;; string :
;;;     the nul-terminated string to measure.
;;; 
;;; lbearing :
;;;     the left bearing of the string.
;;; 
;;; rbearing :
;;;     the right bearing of the string.
;;; 
;;; width :
;;;     the width of the string.
;;; 
;;; ascent :
;;;     the ascent of the string.
;;; 
;;; descent :
;;;     the descent of the string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_text_extents ()
;;; 
;;; void gdk_text_extents (GdkFont *font,
;;;                        const gchar *text,
;;;                        gint text_length,
;;;                        gint *lbearing,
;;;                        gint *rbearing,
;;;                        gint *width,
;;;                        gint *ascent,
;;;                        gint *descent);
;;; 
;;; Warning
;;; 
;;; gdk_text_extents is deprecated and should not be used in newly-written code.
;;; 
;;; Gets the metrics of a string.
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; text :
;;;     the text to measure
;;; 
;;; text_length :
;;;     the length of the text in bytes. (If the font is a 16-bit font, this is
;;;     twice the length of the text in characters.)
;;; 
;;; lbearing :
;;;     the left bearing of the string.
;;; 
;;; rbearing :
;;;     the right bearing of the string.
;;; 
;;; width :
;;;     the width of the string.
;;; 
;;; ascent :
;;;     the ascent of the string.
;;; 
;;; descent :
;;;     the descent of the string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_text_extents_wc ()
;;; 
;;; void gdk_text_extents_wc (GdkFont *font,
;;;                           const GdkWChar *text,
;;;                           gint text_length,
;;;                           gint *lbearing,
;;;                           gint *rbearing,
;;;                           gint *width,
;;;                           gint *ascent,
;;;                           gint *descent);
;;; 
;;; Warning
;;; 
;;; gdk_text_extents_wc is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Gets the metrics of a string of wide characters.
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; text :
;;;     the text to measure.
;;; 
;;; text_length :
;;;     the length of the text in character.
;;; 
;;; lbearing :
;;;     the left bearing of the string.
;;; 
;;; rbearing :
;;;     the right bearing of the string.
;;; 
;;; width :
;;;     the width of the string.
;;; 
;;; ascent :
;;;     the ascent of the string.
;;; 
;;; descent :
;;;     the descent of the string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_string_width ()
;;; 
;;; gint gdk_string_width (GdkFont *font, const gchar *string);
;;; 
;;; Warning
;;; 
;;; gdk_string_width is deprecated and should not be used in newly-written code.
;;; 
;;; Determines the width of a nul-terminated string. (The distance from the
;;; origin of the string to the point where the next string in a sequence of
;;; strings should be drawn)
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; string :
;;;     the nul-terminated string to measure
;;; 
;;; Returns :
;;;     the width of the string in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_text_width ()
;;; 
;;; gint gdk_text_width (GdkFont *font, const gchar *text, gint text_length);
;;; 
;;; Warning
;;; 
;;; gdk_text_width is deprecated and should not be used in newly-written code.
;;; 
;;; Determines the width of a given string.
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; text :
;;;     the text to measure.
;;; 
;;; text_length :
;;;     the length of the text in bytes.
;;; 
;;; Returns :
;;;     the width of the string in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_text_width_wc ()
;;; 
;;; gint gdk_text_width_wc (GdkFont *font,
;;;                         const GdkWChar *text,
;;;                         gint text_length);
;;; 
;;; Warning
;;; 
;;; gdk_text_width_wc is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Determines the width of a given wide-character string.
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; text :
;;;     the text to measure.
;;; 
;;; text_length :
;;;     the length of the text in characters.
;;; 
;;; Returns :
;;;     the width of the string in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_char_width ()
;;; 
;;; gint gdk_char_width (GdkFont *font, gchar character);
;;; 
;;; Warning
;;; 
;;; gdk_char_width has been deprecated since version 2.2 and should not be used
;;; in newly-written code. Use gdk_text_extents() instead.
;;; 
;;; Determines the width of a given character.
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; character :
;;;     the character to measure.
;;; 
;;; Returns :
;;;     the width of the character in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_char_width_wc ()
;;; 
;;; gint gdk_char_width_wc (GdkFont *font, GdkWChar character);
;;; 
;;; Warning
;;; 
;;; gdk_char_width_wc is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Determines the width of a given wide character. (Encoded in the
;;; wide-character encoding of the current locale).
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; character :
;;;     the character to measure.
;;; 
;;; Returns :
;;;     the width of the character in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_string_measure ()
;;; 
;;; gint gdk_string_measure (GdkFont *font, const gchar *string);
;;; 
;;; Warning
;;; 
;;; gdk_string_measure is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Determines the distance from the origin to the rightmost portion of a
;;; nul-terminated string when drawn. This is not the correct value for
;;; determining the origin of the next portion when drawing text in multiple
;;; pieces. See gdk_string_width().
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; string :
;;;     the nul-terminated string to measure.
;;; 
;;; Returns :
;;;     the right bearing of the string in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_text_measure ()
;;; 
;;; gint gdk_text_measure (GdkFont *font, const gchar *text, gint text_length)
;;; 
;;; Warning
;;; 
;;; gdk_text_measure is deprecated and should not be used in newly-written code.
;;; 
;;; Determines the distance from the origin to the rightmost portion of a
;;; string when drawn. This is not the correct value for determining the origin
;;; of the next portion when drawing text in multiple pieces.
;;; See gdk_text_width().
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; text :
;;;     the text to measure.
;;; 
;;; text_length :
;;;     the length of the text in bytes.
;;; 
;;; Returns :
;;;     the right bearing of the string in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_char_measure ()
;;; 
;;; gint gdk_char_measure (GdkFont *font, gchar character);
;;; 
;;; Warning
;;; 
;;; gdk_char_measure is deprecated and should not be used in newly-written code.
;;; 
;;; Determines the distance from the origin to the rightmost portion of a
;;; character when drawn. This is not the correct value for determining the
;;; origin of the next portion when drawing text in multiple pieces.
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; character :
;;;     the character to measure.
;;; 
;;; Returns :
;;;     the right bearing of the character in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_string_height ()
;;; 
;;; gint gdk_string_height (GdkFont *font, const gchar *string);
;;; 
;;; Warning
;;; 
;;; gdk_string_height is deprecated and should not be used in newly-written
;;; code.
;;; 
;;; Determines the total height of a given nul-terminated string. This value
;;; is not generally useful, because you cannot determine how this total height
;;; will be drawn in relation to the baseline. See gdk_string_extents().
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; string :
;;;     the nul-terminated string to measure.
;;; 
;;; Returns :
;;;     the height of the string in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_text_height ()
;;; 
;;; gint gdk_text_height (GdkFont *font, const gchar *text, gint text_length);
;;; 
;;; Warning
;;; 
;;; gdk_text_height is deprecated and should not be used in newly-written code.
;;; 
;;; Determines the total height of a given string. This value is not generally
;;; useful, because you cannot determine how this total height will be drawn in
;;; relation to the baseline. See gdk_text_extents().
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; text :
;;;     the text to measure.
;;; 
;;; text_length :
;;;     the length of the text in bytes.
;;; 
;;; Returns :
;;;     the height of the string in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_char_height ()
;;; 
;;; gint gdk_char_height (GdkFont *font, gchar character);
;;; 
;;; Warning
;;; 
;;; gdk_char_height has been deprecated since version 2.2 and should not be
;;; used in newly-written code. Use gdk_text_extents() instead.
;;; 
;;; Determines the total height of a given character. This value is not
;;; generally useful, because you cannot determine how this total height will
;;; be drawn in relation to the baseline. See gdk_text_extents().
;;; 
;;; font :
;;;     a GdkFont
;;; 
;;; character :
;;;     the character to measure.
;;; 
;;; Returns :
;;;     the height of the character in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkWChar
;;; 
;;; typedef guint32 GdkWChar;
;;; 
;;; Specifies a wide character type, used to represent character codes. This is
;;; needed since some native languages have character sets which have more than
;;; 256 characters (Japanese and Chinese, for example).
;;; 
;;; Wide character values between 0 and 127 are always identical in meaning to
;;; the ASCII character codes. The wide character value 0 is often used to
;;; terminate strings of wide characters in a similar way to normal strings
;;; using the char type.
;;; 
;;; An alternative to wide characters is multi-byte characters, which extend
;;; normal char strings to cope with larger character sets. As the name
;;; suggests, multi-byte characters use a different number of bytes to store
;;; different character codes. For example codes 0-127 (i.e. the ASCII codes)
;;; often use just one byte of memory, while other codes may use 2, 3 or even
;;; 4 bytes. Multi-byte characters have the advantage that they can often be
;;; used in an application with little change, since strings are still
;;; represented as arrays of char values. However multi-byte strings are much
;;; easier to manipulate since the character are all of the same size.
;;; 
;;; Applications typically use wide characters to represent character codes
;;; internally, and multi-byte strings when saving the characters to a file.
;;; The gdk_wcstombs() and gdk_mbstowcs() functions can be used to convert from
;;; one representation to the other.
;;; 
;;; See the 'Extended Characters' section of the GNU C Library Reference Manual
;;; for more detailed information on wide and multi-byte characters.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_wcstombs ()
;;; 
;;; gchar * gdk_wcstombs (const GdkWChar *src);
;;; 
;;; Warning
;;; 
;;; gdk_wcstombs is deprecated and should not be used in newly-written code.
;;; 
;;; Converts a wide character string to a multi-byte string. (The function name
;;; comes from an acronym of 'Wide Character String TO Multi-Byte String').
;;; 
;;; src :
;;;     a wide character string.
;;; 
;;; Returns :
;;;     the multi-byte string corresponding to src, or NULL if the conversion
;;;     failed. The returned string should be freed with g_free() when no longer
;;;     needed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_mbstowcs ()
;;; 
;;; gint  gdk_mbstowcs (GdkWChar *dest, const gchar *src, gint dest_max);
;;; 
;;; Warning
;;; 
;;; gdk_mbstowcs is deprecated and should not be used in newly-written code.
;;; 
;;; Converts a multi-byte string to a wide character string. (The function name
;;; comes from an acronym of 'Multi-Byte String TO Wide Character String').
;;; 
;;; dest :
;;;     the space to place the converted wide character string into.
;;; 
;;; src :
;;;     the multi-byte string to convert, which must be nul-terminated.
;;; 
;;; dest_max :
;;;     the maximum number of wide characters to place in dest.
;;; 
;;; Returns :
;;;     the number of wide characters written into dest, or -1 if the
;;;     conversion failed.
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.font.lisp ----------------------------------------------
