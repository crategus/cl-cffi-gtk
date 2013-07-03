;;; ----------------------------------------------------------------------------
;;; pango.bidirectional.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the Pango Reference Manual
;;; for Pango 1.32.6. See <http://www.gtk.org>. The API documentation of the
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
;;; Bidirectional Text
;;;
;;; Types and functions to help with handling bidirectional text
;;;
;;; Synopsis
;;;
;;;     PangoDirection
;;;
;;;     pango_unichar_direction
;;;     pango_find_base_dir
;;;     pango_get_mirror_char
;;;
;;;     PangoBidiType
;;;
;;;     pango_bidi_type_for_unichar
;;;
;;; Object Hierarchy
;;;
;;;   GEnum
;;;    +----PangoDirection
;;;
;;;   GEnum
;;;    +----PangoBidiType
;;;
;;; Description
;;;
;;; Pango supports bidirectional text (like Arabic and Hebrew) automatically.
;;; Some applications however, need some help to correctly handle bidirectional
;;; text.
;;;
;;; The PangoDirection type can be used with pango_context_set_base_dir() to
;;; instruct Pango about direction of text, though in most cases Pango detects
;;; that correctly and automatically. The rest of the facilities in this section
;;; are used internally by Pango already, and are provided to help applications
;;; that need more direct control over bidirectional setting of text.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; enum PangoDirection
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoDirection" pango-direction
  (:export t
   :type-initializer "pango_direction_get_type")
  (:ltr 0)
  (:rtl 1)
  (:ttb-ltr 2)
  (:ttb-rtl 3)
  (:weak-ltr 4)
  (:weak-rtl 5)
  (:neutral 6))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-direction atdoc:*symbol-name-alias*) "Enum"
      (gethash 'pango-direction atdoc:*external-symbols*)
 "@version{2013-6-29}
  @begin{short}
    The @sym{pango-direction} enumeration represents a direction in the Unicode
    bidirectional algorithm.
  @end{short}
  Not every value in this enumeration makes sense for every usage of
  @sym{pango-direction}. For example, the return value of the functions
  @fun{pango-unichar-direction} and @fun{pango-find-base-dir} cannot be
  @code{:weak-ltr} or @code{:weak-rtl}, since every character is either neutral
  or has a strong direction; on the other hand @code{:neutral} does not make
  sense to pass to the function @fun{pango-itemize-with-base-dir}.

  The @code{:ttb-ltr}, @code{:ttb-rtl} values come from an earlier
  interpretation of this enumeration as the writing direction of a block of
  text and are no longer used. See @symbol{pango-gravity} for how vertical text
  is handled in Pango.
  @begin{pre}
(define-g-enum \"PangoDirection\" pango-direction
  (:export t
   :type-initializer \"pango_direction_get_type\")
  (:ltr 0)
  (:rtl 1)
  (:ttb-ltr 2)
  (:ttb-rtl 3)
  (:weak-ltr 4)
  (:weak-rtl 5)
  (:neutral 6))
  @end{pre}
  @begin[code]{table}
    @entry[:ltr]{A strong left-to-right direction.}
    @entry[:rtl]{A strong right-to-left direction.}
    @entry[:ttb-ltr]{Deprecated value; treated the same as @code{:rtl}.}
    @entry{:ttb-rtl]{Deprecated value; treated the same as @code{:ltr}.}
    @entry[:weak-ltr]{A weak left-to-right direction.}
    @entry[:wek-rtl]{A weak right-to-left direction.}
    @entry[:neutral]{No direction specified.}
  @end{table}
  @see-symbol{pango-gravity}
  @see-function{pango-unichar-direction}
  @see-function{pango-find-base-dir}
  @see-function{pango-itemize-with-base-dir}")

;;; ----------------------------------------------------------------------------
;;; pango_unichar_direction ()
;;;
;;; PangoDirection pango_unichar_direction (gunichar ch);
;;;
;;; Determines the inherent direction of a character; either
;;; PANGO_DIRECTION_LTR, PANGO_DIRECTION_RTL, or PANGO_DIRECTION_NEUTRAL.
;;;
;;; This function is useful to categorize characters into left-to-right letters,
;;; right-to-left letters, and everything else. If full Unicode bidirectional
;;; type of a character is needed, pango_bidi_type_for_gunichar() can be used
;;; instead.
;;;
;;; ch :
;;;     a Unicode character
;;;
;;; Returns :
;;;     the direction of the character.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_find_base_dir ()
;;;
;;; PangoDirection pango_find_base_dir (const gchar *text, gint length);
;;;
;;; Searches a string the first character that has a strong direction, according
;;; to the Unicode bidirectional algorithm.
;;;
;;; text :
;;;     the text to process
;;;
;;; length :
;;;     length of text in bytes (may be -1 if text is nul-terminated)
;;;
;;; Returns :
;;;     The direction corresponding to the first strong character. If no such
;;;     character is found, then PANGO_DIRECTION_NEUTRAL is returned.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_get_mirror_char ()
;;;
;;; gboolean pango_get_mirror_char (gunichar ch, gunichar *mirrored_ch);
;;;
;;; Warning
;;;
;;; pango_get_mirror_char is deprecated and should not be used in newly-written
;;; code.
;;;
;;; If ch has the Unicode mirrored property and there is another Unicode
;;; character that typically has a glyph that is the mirror image of ch's glyph,
;;; puts that character in the address pointed to by mirrored_ch.
;;;
;;; Use g_unichar_get_mirror_char() instead; the docs for that function provide
;;; full details.
;;;
;;; ch :
;;;     a Unicode character
;;;
;;; mirrored_ch :
;;;     location to store the mirrored character
;;;
;;; Returns :
;;;     TRUE if ch has a mirrored character and mirrored_ch is filled in, FALSE
;;;     otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum PangoBidiType
;;;
;;; typedef enum {
;;;   /* Strong types */
;;;   PANGO_BIDI_TYPE_L,
;;;   PANGO_BIDI_TYPE_LRE,
;;;   PANGO_BIDI_TYPE_LRO,
;;;   PANGO_BIDI_TYPE_R,
;;;   PANGO_BIDI_TYPE_AL,
;;;   PANGO_BIDI_TYPE_RLE,
;;;   PANGO_BIDI_TYPE_RLO,
;;;
;;;   /* Weak types */
;;;   PANGO_BIDI_TYPE_PDF,
;;;   PANGO_BIDI_TYPE_EN,
;;;   PANGO_BIDI_TYPE_ES,
;;;   PANGO_BIDI_TYPE_ET,
;;;   PANGO_BIDI_TYPE_AN,
;;;   PANGO_BIDI_TYPE_CS,
;;;   PANGO_BIDI_TYPE_NSM,
;;;   PANGO_BIDI_TYPE_BN,
;;;
;;;   /* Neutral types */
;;;   PANGO_BIDI_TYPE_B,
;;;   PANGO_BIDI_TYPE_S,
;;;   PANGO_BIDI_TYPE_WS,
;;;   PANGO_BIDI_TYPE_ON
;;; } PangoBidiType;
;;;
;;; The PangoBidiType type represents the bidirectional character type of a
;;; Unicode character as specified by the Unicode bidirectional algorithm.
;;;
;;; PANGO_BIDI_TYPE_L
;;;     Left-to-Right
;;;
;;; PANGO_BIDI_TYPE_LRE
;;;     Left-to-Right Embedding
;;;
;;; PANGO_BIDI_TYPE_LRO
;;;     Left-to-Right Override
;;;
;;; PANGO_BIDI_TYPE_R
;;;     Right-to-Left
;;;
;;; PANGO_BIDI_TYPE_AL
;;;     Right-to-Left Arabic
;;;
;;; PANGO_BIDI_TYPE_RLE
;;;     Right-to-Left Embedding
;;;
;;; PANGO_BIDI_TYPE_RLO
;;;     Right-to-Left Override
;;;
;;; PANGO_BIDI_TYPE_PDF
;;;     Pop Directional Format
;;;
;;; PANGO_BIDI_TYPE_EN
;;;     European Number
;;;
;;; PANGO_BIDI_TYPE_ES
;;;     European Number Separator
;;;
;;; PANGO_BIDI_TYPE_ET
;;;     European Number Terminator
;;;
;;; PANGO_BIDI_TYPE_AN
;;;     Arabic Number
;;;
;;; PANGO_BIDI_TYPE_CS
;;;     Common Number Separator
;;;
;;; PANGO_BIDI_TYPE_NSM
;;;     Nonspacing Mark
;;;
;;; PANGO_BIDI_TYPE_BN
;;;     Boundary Neutral
;;;
;;; PANGO_BIDI_TYPE_B
;;;     Paragraph Separator
;;;
;;; PANGO_BIDI_TYPE_S
;;;     Segment Separator
;;;
;;; PANGO_BIDI_TYPE_WS
;;;     Whitespace
;;;
;;; PANGO_BIDI_TYPE_ON
;;;     Other Neutrals
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_bidi_type_for_unichar ()
;;;
;;; PangoBidiType pango_bidi_type_for_unichar (gunichar ch);
;;;
;;; Determines the normative bidirectional character type of a character, as
;;; specified in the Unicode Character Database.
;;;
;;; A simplified version of this function is available as
;;; pango_unichar_get_direction().
;;;
;;; ch :
;;;     a Unicode character
;;;
;;; Returns :
;;;     the bidirectional character type, as used in the Unicode bidirectional
;;;     algorithm.
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; --- pango.bidirectional.lisp -----------------------------------------------
