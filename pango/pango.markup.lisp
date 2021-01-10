;;; ----------------------------------------------------------------------------
;;; pango.markup.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.48 and modified to document the Lisp binding to the Pango library.
;;; See <http://www.pango.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 Dieter Kaiser
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
;;; Markup
;;;
;;;     Simple markup language for text with attributes
;;;
;;; Functions
;;;
;;;     pango_parse_markup
;;;     pango_markup_parser_new
;;;     pango_markup_parser_finish
;;;
;;; Description
;;;
;;; Frequently, you want to display some text to the user with attributes
;;; applied to part of the text (for example, you might want bold or italicized
;;; words). With the base Pango interfaces, you could create a PangoAttrList and
;;; apply it to the text; the problem is that you'd need to apply attributes to
;;; some numeric range of characters, for example "characters 12-17." This is
;;; broken from an internationalization standpoint; once the text is translated,
;;; the word you wanted to italicize could be in a different position.
;;;
;;; The solution is to include the text attributes in the string to be
;;; translated. Pango provides this feature with a small markup language. You
;;; can parse a marked-up string into the string text plus a PangoAttrList using
;;; either of pango_parse_markup() or pango_markup_parser_new().
;;;
;;; A simple example of a marked-up string might be:
;;;
;;; <span foreground="blue" size="x-large">Blue text</span> is <i>cool</i>!
;;;
;;; Pango uses GMarkup to parse this language, which means that XML features
;;; such as numeric character entities such as &amp;#169; for © can be used too.
;;;
;;; The root tag of a marked-up document is <markup>, but pango_parse_markup()
;;; allows you to omit this tag, so you will most likely never need to use it.
;;; The most general markup tag is <span>, then there are some convenience tags.
;;;
;;; Span attributes
;;;
;;; <span> has the following attributes:
;;;
;;; font_desc: A font description string, such as "Sans Italic 12". See
;;; pango_font_description_from_string() for a description of the format of the
;;; string representation . Note that any other span attributes will override
;;; this description. So if you have "Sans Italic" and also a style="normal"
;;; attribute, you will get Sans normal, not italic.
;;;
;;; font_family: A font family name
;;;
;;; font_size, size: Font size in 1024ths of a point, or one of the absolute
;;; sizes xx-small, x-small, small, medium, large, x-large, xx-large, or one of
;;; the relative sizes smaller or larger. If you want to specify a absolute
;;; size, it's usually easier to take advantage of the ability to specify a
;;; partial font description using font; you can use font='12.5' rather than
;;; size='12800'.
;;;
;;; font_style: One of normal, oblique, italic
;;;
;;; font_weight: One of ultralight, light, normal, bold, ultrabold, heavy, or a
;;; numeric weight
;;;
;;; font_variant: One of normal or smallcaps
;;;
;;; font_stretch, stretch: One of ultracondensed, extracondensed, condensed,
;;; semicondensed, normal, semiexpanded, expanded, extraexpanded, ultraexpanded
;;;
;;; font_features: A comma-separated list of OpenType font feature settings, in
;;; the same syntax as accepted by CSS. E.g: font_features='dlig=1, -kern,
;;; afrc on'
;;;
;;; foreground, fgcolor: An RGB color specification such as #00FF00 or a color
;;; name such as red. Since 1.38, an RGBA color specification such as #00FF007F
;;; will be interpreted as specifying both a foreground color and foreground
;;; alpha.
;;;
;;; background, bgcolor: An RGB color specification such as #00FF00 or a color
;;; name such as red. Since 1.38, an RGBA color specification such as #00FF007F
;;; will be interpreted as specifying both a background color and background
;;; alpha.
;;;
;;; alpha, fgalpha: An alpha value for the foreground color, either a plain
;;; integer between 1 and 65536 or a percentage value like 50%.
;;;
;;; background_alpha, bgalpha: An alpha value for the background color, either
;;; a plain integer between 1 and 65536 or a percentage value like 50%.
;;;
;;; underline: One of none, single, double, low, error, single-line, double-line
;;; or error-line.
;;;
;;; underline_color: The color of underlines; an RGB color specification such as
;;; #00FF00 or a color name such as red
;;;
;;; overline: One of none or single
;;;
;;; overline_color: The color of overlines; an RGB color specification such as
;;; #00FF00 or a color name such as red
;;;
;;; rise: Vertical displacement, in Pango units. Can be negative for subscript,
;;; positive for superscript.
;;;
;;; strikethrough true or false whether to strike through the text
;;;
;;; strikethrough_color: The color of strikethrough lines; an RGB color
;;; specification such as #00FF00 or a color name such as red
;;;
;;; fallback: true or false whether to enable fallback. If disabled, then
;;; characters will only be used from the closest matching font on the system.
;;; No fallback will be done to other fonts on the system that might contain the
;;; characters in the text. Fallback is enabled by default. Most applications
;;; should not disable fallback.
;;;
;;; allow_breaks: true or false whether to allow line breaks or not. If not
;;; allowed, the range will be kept in a single run as far as possible. Breaks
;;; are allowed by default.
;;;
;;; insert_hyphens: true or false` whether to insert hyphens when breaking lines
;;; in the middle of a word. Hyphens are inserted by default.
;;;
;;; show: A value determining how invisible characters are treated. Possible
;;; values are spaces, line-breaks, ignorables or combinations, such as
;;; spaces|line-breaks.
;;;
;;; lang: A language code, indicating the text language
;;;
;;; letter_spacing: Inter-letter spacing in 1024ths of a point.
;;;
;;; gravity: One of south, east, north, west, auto.
;;;
;;; gravity_hint: One of natural, strong, line.
;;;
;;; Convenience tags
;;;
;;; The following convenience tags are provided:
;;;
;;; <b>: Bold
;;; <big>: Makes font relatively larger, equivalent to <span size="larger">
;;; <i>: Italic
;;; <s>: Strikethrough
;;; <sub>: Subscript
;;; <sup>: Superscript
;;; <small>: Makes font relatively smaller, equivalent to <span size="smaller">
;;; <tt>: Monospace
;;; <u>: Underline
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; pango_parse_markup ()
;;;
;;; gboolean
;;; pango_parse_markup (const char *markup_text,
;;;                     int length,
;;;                     gunichar accel_marker,
;;;                     PangoAttrList **attr_list,
;;;                     char **text,
;;;                     gunichar *accel_char,
;;;                     GError **error);
;;;
;;; Parses marked-up text (see markup format) to create a plain-text string and
;;; an attribute list.
;;;
;;; If accel_marker is nonzero, the given character will mark the character
;;; following it as an accelerator. For example, accel_marker might be an
;;; ampersand or underscore. All characters marked as an accelerator will
;;; receive a PANGO_UNDERLINE_LOW attribute, and the first character so marked
;;; will be returned in accel_char . Two accel_marker characters following each
;;; other produce a single literal accel_marker character.
;;;
;;; To parse a stream of pango markup incrementally, use
;;; pango_markup_parser_new().
;;;
;;; If any error happens, none of the output arguments are touched except for
;;; error .
;;;
;;; markup_text :
;;;     markup to parse (see markup format)
;;;
;;; length :
;;;     length of markup_text , or -1 if nul-terminated
;;;
;;; accel_marker :
;;;     character that precedes an accelerator, or 0 for none
;;;
;;; attr_list :
;;;     address of return location for a PangoAttrList, or NULL.
;;;
;;; text :
;;;     address of return location for text with tags stripped, or NULL.
;;;
;;; accel_char :
;;;     address of return location for accelerator char, or NULL.
;;;
;;; error :
;;;     address of return location for errors, or NULL
;;;
;;; Returns :
;;;     FALSE if error is set, otherwise TRUE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_markup_parser_new ()
;;;
;;; GMarkupParseContext *
;;; pango_markup_parser_new (gunichar accel_marker);
;;;
;;; Parses marked-up text (see markup format) to create a plain-text string and
;;; an attribute list.
;;;
;;; If accel_marker is nonzero, the given character will mark the character
;;; following it as an accelerator. For example, accel_marker might be an
;;; ampersand or underscore. All characters marked as an accelerator will
;;; receive a PANGO_UNDERLINE_LOW attribute, and the first character so marked
;;; will be returned in accel_char , when calling finish(). Two accel_marker
;;; characters following each other produce a single literal accel_marker
;;; character.
;;;
;;; To feed markup to the parser, use g_markup_parse_context_parse() on the
;;; returned GMarkupParseContext. When done with feeding markup to the parser,
;;; use pango_markup_parser_finish() to get the data out of it, and then use
;;; g_markup_parse_context_free() to free it.
;;;
;;; This function is designed for applications that read pango markup from
;;; streams. To simply parse a string containing pango markup, the simpler
;;; pango_parse_markup() API is recommended instead.
;;;
;;; accel_marker :
;;;     character that precedes an accelerator, or 0 for none
;;;
;;; Returns :
;;;     a GMarkupParseContext that should be destroyed with
;;;     g_markup_parse_context_free().
;;;
;;; Since 1.31
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_markup_parser_finish ()
;;;
;;; gboolean
;;; pango_markup_parser_finish (GMarkupParseContext *context,
;;;                             PangoAttrList **attr_list,
;;;                             char **text,
;;;                             gunichar *accel_char,
;;;                             GError **error);
;;;
;;; After feeding a pango markup parser some data with
;;; g_markup_parse_context_parse(), use this function to get the list of pango
;;; attributes and text out of the markup. This function will not free context ,
;;; use g_markup_parse_context_free() to do so.
;;;
;;; context :
;;;     A valid parse context that was returned from pango_markup_parser_new()
;;;
;;; attr_list :
;;;     address of return location for a PangoAttrList, or NULL.
;;;
;;; text :
;;;     address of return location for text with tags stripped, or NULL.
;;;
;;; accel_char :
;;;     address of return location for accelerator char, or NULL.
;;;
;;; error :
;;;     address of return location for errors, or NULL
;;;
;;; Returns :
;;;     FALSE if error is set, otherwise TRUE
;;;
;;; Since 1.31
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.markup.lisp ------------------------------------------
