;;; ----------------------------------------------------------------------------
;;; pango.script.lisp
;;;
;;; The documentation has been copied from the Pango Reference Manual
;;; for Pango 1.29.5. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Scripts and Languages
;;;
;;; Identifying writing systems and languages
;;;
;;; Synopsis
;;;
;;;     PangoScript
;;;
;;;     PANGO_TYPE_SCRIPT
;;;
;;;     PangoScriptIter
;;;
;;;     pango_script_for_unichar
;;;     pango_script_get_sample_language
;;;     pango_script_iter_new
;;;     pango_script_iter_get_range
;;;     pango_script_iter_next
;;;     pango_script_iter_free
;;;
;;;     PangoLanguage
;;;
;;;     PANGO_TYPE_LANGUAGE
;;;
;;;     pango_language_from_string
;;;     pango_language_to_string
;;;     pango_language_matches
;;;     pango_language_includes_script
;;;     pango_language_get_scripts
;;;     pango_language_get_default
;;;     pango_language_get_sample_string
;;;
;;; Object Hierarchy
;;;
;;;   GEnum
;;;    +----PangoScript
;;;
;;;   GBoxed
;;;    +----PangoLanguage
;;;
;;; Description
;;;
;;; The functions in this section are used to identify the writing system, or
;;; script of individual characters and of ranges within a larger text string.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; enum PangoScript
;;;
;;; typedef enum {
;;;                                        /* ISO 15924 code */
;;;       PANGO_SCRIPT_INVALID_CODE = -1,
;;;       PANGO_SCRIPT_COMMON       = 0,   /* Zyyy */
;;;       PANGO_SCRIPT_INHERITED,          /* Qaai */
;;;       PANGO_SCRIPT_ARABIC,             /* Arab */
;;;       PANGO_SCRIPT_ARMENIAN,           /* Armn */
;;;       PANGO_SCRIPT_BENGALI,            /* Beng */
;;;       PANGO_SCRIPT_BOPOMOFO,           /* Bopo */
;;;       PANGO_SCRIPT_CHEROKEE,           /* Cher */
;;;       PANGO_SCRIPT_COPTIC,             /* Qaac */
;;;       PANGO_SCRIPT_CYRILLIC,           /* Cyrl (Cyrs) */
;;;       PANGO_SCRIPT_DESERET,            /* Dsrt */
;;;       PANGO_SCRIPT_DEVANAGARI,         /* Deva */
;;;       PANGO_SCRIPT_ETHIOPIC,           /* Ethi */
;;;       PANGO_SCRIPT_GEORGIAN,           /* Geor (Geon, Geoa) */
;;;       PANGO_SCRIPT_GOTHIC,             /* Goth */
;;;       PANGO_SCRIPT_GREEK,              /* Grek */
;;;       PANGO_SCRIPT_GUJARATI,           /* Gujr */
;;;       PANGO_SCRIPT_GURMUKHI,           /* Guru */
;;;       PANGO_SCRIPT_HAN,                /* Hani */
;;;       PANGO_SCRIPT_HANGUL,             /* Hang */
;;;       PANGO_SCRIPT_HEBREW,             /* Hebr */
;;;       PANGO_SCRIPT_HIRAGANA,           /* Hira */
;;;       PANGO_SCRIPT_KANNADA,            /* Knda */
;;;       PANGO_SCRIPT_KATAKANA,           /* Kana */
;;;       PANGO_SCRIPT_KHMER,              /* Khmr */
;;;       PANGO_SCRIPT_LAO,                /* Laoo */
;;;       PANGO_SCRIPT_LATIN,              /* Latn (Latf, Latg) */
;;;       PANGO_SCRIPT_MALAYALAM,          /* Mlym */
;;;       PANGO_SCRIPT_MONGOLIAN,          /* Mong */
;;;       PANGO_SCRIPT_MYANMAR,            /* Mymr */
;;;       PANGO_SCRIPT_OGHAM,              /* Ogam */
;;;       PANGO_SCRIPT_OLD_ITALIC,         /* Ital */
;;;       PANGO_SCRIPT_ORIYA,              /* Orya */
;;;       PANGO_SCRIPT_RUNIC,              /* Runr */
;;;       PANGO_SCRIPT_SINHALA,            /* Sinh */
;;;       PANGO_SCRIPT_SYRIAC,             /* Syrc (Syrj, Syrn, Syre) */
;;;       PANGO_SCRIPT_TAMIL,              /* Taml */
;;;       PANGO_SCRIPT_TELUGU,             /* Telu */
;;;       PANGO_SCRIPT_THAANA,             /* Thaa */
;;;       PANGO_SCRIPT_THAI,               /* Thai */
;;;       PANGO_SCRIPT_TIBETAN,            /* Tibt */
;;;       PANGO_SCRIPT_CANADIAN_ABORIGINAL, /* Cans */
;;;       PANGO_SCRIPT_YI,                 /* Yiii */
;;;       PANGO_SCRIPT_TAGALOG,            /* Tglg */
;;;       PANGO_SCRIPT_HANUNOO,            /* Hano */
;;;       PANGO_SCRIPT_BUHID,              /* Buhd */
;;;       PANGO_SCRIPT_TAGBANWA,           /* Tagb */
;;;
;;;       /* Unicode-4.0 additions */
;;;       PANGO_SCRIPT_BRAILLE,            /* Brai */
;;;       PANGO_SCRIPT_CYPRIOT,            /* Cprt */
;;;       PANGO_SCRIPT_LIMBU,              /* Limb */
;;;       PANGO_SCRIPT_OSMANYA,            /* Osma */
;;;       PANGO_SCRIPT_SHAVIAN,            /* Shaw */
;;;       PANGO_SCRIPT_LINEAR_B,           /* Linb */
;;;       PANGO_SCRIPT_TAI_LE,             /* Tale */
;;;       PANGO_SCRIPT_UGARITIC,           /* Ugar */
;;;
;;;       /* Unicode-4.1 additions */
;;;       PANGO_SCRIPT_NEW_TAI_LUE,        /* Talu */
;;;       PANGO_SCRIPT_BUGINESE,           /* Bugi */
;;;       PANGO_SCRIPT_GLAGOLITIC,         /* Glag */
;;;       PANGO_SCRIPT_TIFINAGH,           /* Tfng */
;;;       PANGO_SCRIPT_SYLOTI_NAGRI,       /* Sylo */
;;;       PANGO_SCRIPT_OLD_PERSIAN,        /* Xpeo */
;;;       PANGO_SCRIPT_KHAROSHTHI,         /* Khar */
;;;
;;;       /* Unicode-5.0 additions */
;;;       PANGO_SCRIPT_UNKNOWN,            /* Zzzz */
;;;       PANGO_SCRIPT_BALINESE,           /* Bali */
;;;       PANGO_SCRIPT_CUNEIFORM,          /* Xsux */
;;;       PANGO_SCRIPT_PHOENICIAN,         /* Phnx */
;;;       PANGO_SCRIPT_PHAGS_PA,           /* Phag */
;;;       PANGO_SCRIPT_NKO,                /* Nkoo */
;;;
;;;       /* Unicode-5.1 additions */
;;;       PANGO_SCRIPT_KAYAH_LI,           /* Kali */
;;;       PANGO_SCRIPT_LEPCHA,             /* Lepc */
;;;       PANGO_SCRIPT_REJANG,             /* Rjng */
;;;       PANGO_SCRIPT_SUNDANESE,          /* Sund */
;;;       PANGO_SCRIPT_SAURASHTRA,         /* Saur */
;;;       PANGO_SCRIPT_CHAM,               /* Cham */
;;;       PANGO_SCRIPT_OL_CHIKI,           /* Olck */
;;;       PANGO_SCRIPT_VAI,                /* Vaii */
;;;       PANGO_SCRIPT_CARIAN,             /* Cari */
;;;       PANGO_SCRIPT_LYCIAN,             /* Lyci */
;;;       PANGO_SCRIPT_LYDIAN              /* Lydi */
;;; } PangoScript;
;;;
;;; The PangoScript enumeration identifies different writing systems. The values
;;; correspond to the names as defined in the Unicode standard. Note that new
;;; types may be added in the future. Applications should be ready to handle
;;; unknown values. This enumeration is interchangeable with GUnicodeScript.
;;; See Unicode Standard Annex #24: Script names.
;;;
;;; PANGO_SCRIPT_INVALID_CODE
;;;     a value never returned from pango_script_for_unichar()
;;;
;;; PANGO_SCRIPT_COMMON
;;;     a character used by multiple different scripts
;;;
;;; PANGO_SCRIPT_INHERITED
;;;     a mark glyph that takes its script from the base glyph to which it is
;;;     attached
;;;
;;; PANGO_SCRIPT_ARABIC
;;;     Arabic
;;;
;;; PANGO_SCRIPT_ARMENIAN
;;;     Armenian
;;;
;;; PANGO_SCRIPT_BENGALI
;;;     Bengali
;;;
;;; PANGO_SCRIPT_BOPOMOFO
;;;     Bopomofo
;;;
;;; PANGO_SCRIPT_CHEROKEE
;;;     Cherokee
;;;
;;; PANGO_SCRIPT_COPTIC
;;;     Coptic
;;;
;;; PANGO_SCRIPT_CYRILLIC
;;;     Cyrillic
;;;
;;; PANGO_SCRIPT_DESERET
;;;     Deseret
;;;
;;; PANGO_SCRIPT_DEVANAGARI
;;;     Devanagari
;;;
;;; PANGO_SCRIPT_ETHIOPIC
;;;     Ethiopic
;;;
;;; PANGO_SCRIPT_GEORGIAN
;;;     Georgian
;;;
;;; PANGO_SCRIPT_GOTHIC
;;;     Gothic
;;;
;;; PANGO_SCRIPT_GREEK
;;;     Greek
;;;
;;; PANGO_SCRIPT_GUJARATI
;;;     Gujarati
;;;
;;; PANGO_SCRIPT_GURMUKHI
;;;     Gurmukhi
;;;
;;; PANGO_SCRIPT_HAN
;;;     Han
;;;
;;; PANGO_SCRIPT_HANGUL
;;;     Hangul
;;;
;;; PANGO_SCRIPT_HEBREW
;;;     Hebrew
;;;
;;; PANGO_SCRIPT_HIRAGANA
;;;     Hiragana
;;;
;;; PANGO_SCRIPT_KANNADA
;;;     Kannada
;;;
;;; PANGO_SCRIPT_KATAKANA
;;;     Katakana
;;;
;;; PANGO_SCRIPT_KHMER
;;;     Khmer
;;;
;;; PANGO_SCRIPT_LAO
;;;     Lao
;;;
;;; PANGO_SCRIPT_LATIN
;;;     Latin
;;;
;;; PANGO_SCRIPT_MALAYALAM
;;;     Malayalam
;;;
;;; PANGO_SCRIPT_MONGOLIAN
;;;     Mongolian
;;;
;;; PANGO_SCRIPT_MYANMAR
;;;     Myanmar
;;;
;;; PANGO_SCRIPT_OGHAM
;;;     Ogham
;;;
;;; PANGO_SCRIPT_OLD_ITALIC
;;;     Old Italic
;;;
;;; PANGO_SCRIPT_ORIYA
;;;     Oriya
;;;
;;; PANGO_SCRIPT_RUNIC
;;;     Runic
;;;
;;; PANGO_SCRIPT_SINHALA
;;;     Sinhala
;;;
;;; PANGO_SCRIPT_SYRIAC
;;;     Syriac
;;;
;;; PANGO_SCRIPT_TAMIL
;;;     Tamil
;;;
;;; PANGO_SCRIPT_TELUGU
;;;     Telugu
;;;
;;; PANGO_SCRIPT_THAANA
;;;     Thaana
;;;
;;; PANGO_SCRIPT_THAI
;;;     Thai
;;;
;;; PANGO_SCRIPT_TIBETAN
;;;     Tibetan
;;;
;;; PANGO_SCRIPT_CANADIAN_ABORIGINAL
;;;     Canadian Aboriginal
;;;
;;; PANGO_SCRIPT_YI
;;;     Yi
;;;
;;; PANGO_SCRIPT_TAGALOG
;;;     Tagalog
;;;
;;; PANGO_SCRIPT_HANUNOO
;;;     Hanunoo
;;;
;;; PANGO_SCRIPT_BUHID
;;;     Buhid
;;;
;;; PANGO_SCRIPT_TAGBANWA
;;;     Tagbanwa
;;;
;;; PANGO_SCRIPT_BRAILLE
;;;     Braille
;;;
;;; PANGO_SCRIPT_CYPRIOT
;;;     Cypriot
;;;
;;; PANGO_SCRIPT_LIMBU
;;;     Limbu
;;;
;;; PANGO_SCRIPT_OSMANYA
;;;     Osmanya
;;;
;;; PANGO_SCRIPT_SHAVIAN
;;;     Shavian
;;;
;;; PANGO_SCRIPT_LINEAR_B
;;;     Linear B
;;;
;;; PANGO_SCRIPT_TAI_LE
;;;     Tai Le
;;;
;;; PANGO_SCRIPT_UGARITIC
;;;     Ugaritic
;;;
;;; PANGO_SCRIPT_NEW_TAI_LUE
;;;     New Tai Lue. Since 1.10
;;;
;;; PANGO_SCRIPT_BUGINESE
;;;     Buginese. Since 1.10
;;;
;;; PANGO_SCRIPT_GLAGOLITIC
;;;     Glagolitic. Since 1.10
;;;
;;; PANGO_SCRIPT_TIFINAGH
;;;     Tifinagh. Since 1.10
;;;
;;; PANGO_SCRIPT_SYLOTI_NAGRI
;;;     Syloti Nagri. Since 1.10
;;;
;;; PANGO_SCRIPT_OLD_PERSIAN
;;;     Old Persian. Since 1.10
;;;
;;; PANGO_SCRIPT_KHAROSHTHI
;;;     Kharoshthi. Since 1.10
;;;
;;; PANGO_SCRIPT_UNKNOWN
;;;     an unassigned code point. Since 1.14
;;;
;;; PANGO_SCRIPT_BALINESE
;;;     Balinese. Since 1.14
;;;
;;; PANGO_SCRIPT_CUNEIFORM
;;;     Cuneiform. Since 1.14
;;;
;;; PANGO_SCRIPT_PHOENICIAN
;;;     Phoenician. Since 1.14
;;;
;;; PANGO_SCRIPT_PHAGS_PA
;;;     Phags-pa. Since 1.14
;;;
;;; PANGO_SCRIPT_NKO
;;;     N'Ko. Since 1.14
;;;
;;; PANGO_SCRIPT_KAYAH_LI
;;;     Kayah Li. Since 1.20.1
;;;
;;; PANGO_SCRIPT_LEPCHA
;;;     Lepcha. Since 1.20.1
;;;
;;; PANGO_SCRIPT_REJANG
;;;     Rejang. Since 1.20.1
;;;
;;; PANGO_SCRIPT_SUNDANESE
;;;     Sundanese. Since 1.20.1
;;;
;;; PANGO_SCRIPT_SAURASHTRA
;;;     Saurashtra. Since 1.20.1
;;;
;;; PANGO_SCRIPT_CHAM
;;;     Cham. Since 1.20.1
;;;
;;; PANGO_SCRIPT_OL_CHIKI
;;;     Ol Chiki. Since 1.20.1
;;;
;;; PANGO_SCRIPT_VAI
;;;     Vai. Since 1.20.1
;;;
;;; PANGO_SCRIPT_CARIAN
;;;     Carian. Since 1.20.1
;;;
;;; PANGO_SCRIPT_LYCIAN
;;;     Lycian. Since 1.20.1
;;;
;;; PANGO_SCRIPT_LYDIAN
;;;     Lydian. Since 1.20.1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_SCRIPT
;;;
;;; #define PANGO_TYPE_SCRIPT (pango_script_get_type())
;;;
;;; The GObject type for PangoScript
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoScriptIter
;;;
;;; typedef struct _PangoScriptIter PangoScriptIter;
;;;
;;; A PangoScriptIter is used to iterate through a string and identify ranges
;;; in different scripts.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_for_unichar ()
;;;
;;; PangoScript pango_script_for_unichar (gunichar ch);
;;;
;;; Looks up the PangoScript for a particular character (as defined by Unicode
;;; Standard Annex 24). No check is made for ch being a valid Unicode character;
;;; if you pass in invalid character, the result is undefined.
;;;
;;; As of Pango 1.18, this function simply returns the return value of
;;; g_unichar_get_script().
;;;
;;; ch :
;;;     a Unicode character
;;;
;;; Returns :
;;;     the PangoScript for the character
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_get_sample_language ()
;;;
;;; PangoLanguage * pango_script_get_sample_language (PangoScript script);
;;;
;;; Given a script, finds a language tag that is reasonably representative of
;;; that script. This will usually be the most widely spoken or used language
;;; written in that script: for instance, the sample language for
;;; PANGO_SCRIPT_CYRILLIC is ru (Russian), the sample language for
;;; PANGO_SCRIPT_ARABIC is ar.
;;;
;;; For some scripts, no sample language will be returned because there is no
;;; language that is sufficiently representative. The best example of this is
;;; PANGO_SCRIPT_HAN, where various different variants of written Chinese,
;;; Japanese, and Korean all use significantly different sets of Han characters
;;; and forms of shared characters. No sample language can be provided for many
;;; historical scripts as well.
;;;
;;; As of 1.18, this function checks the environment variables PANGO_LANGUAGE
;;; and LANGUAGE (checked in that order) first. If one of them is set, it is
;;; parsed as a list of language tags separated by colons or other separators.
;;; This function will return the first language in the parsed list that Pango
;;; believes may use script for writing. This last predicate is tested using
;;; pango_language_includes_script(). This can be used to control Pango's font
;;; selection for non-primary languages. For example, a PANGO_LANGUAGE
;;; enviroment variable set to "en:fa" makes Pango choose fonts suitable for
;;; Persian (fa) instead of Arabic (ar) when a segment of Arabic text is found
;;; in an otherwise non-Arabic text. The same trick can be used to choose a
;;; default language for PANGO_SCRIPT_HAN when setting context language is not
;;; feasible.
;;;
;;; script :
;;;     a PangoScript
;;;
;;; Returns :
;;;     a PangoLanguage that is representative of the script, or NULL if no
;;;     such language exists.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_iter_new ()
;;;
;;; PangoScriptIter * pango_script_iter_new (const char *text, int length);
;;;
;;; Create a new PangoScriptIter, used to break a string of Unicode into runs
;;; by text. No copy is made of text, so the caller needs to make sure it
;;; remains valid until the iterator is freed with pango_script_iter_free().
;;;
;;; text :
;;;     a UTF-8 string
;;;
;;; length :
;;;     length of text, or -1 if text is nul-terminated
;;;
;;; Returns :
;;;     The new script iterator, initialized to point at the first range in the
;;      text, which should be freed with pango_script_iter_free(). If the string
;;;     is empty, it will point at an empty range.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_iter_get_range ()
;;;
;;; void pango_script_iter_get_range (PangoScriptIter *iter,
;;;                                   const char **start,
;;;                                   const char **end,
;;;                                   PangoScript *script);
;;;
;;; Gets information about the range to which iter currently points. The range
;;; is the set of locations p where *start <= p < *end. (That is, it doesn't
;;; include the character stored at *end)
;;;
;;; iter :
;;;     a PangoScriptIter
;;;
;;; start :
;;;     location to store start position of the range, or NULL
;;;
;;; end :
;;;     location to store end position of the range, or NULL
;;;
;;; script :
;;;     location to store script for range, or NULL
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_iter_next ()
;;;
;;; gboolean pango_script_iter_next (PangoScriptIter *iter);
;;;
;;; Advances a PangoScriptIter to the next range. If iter is already at the end,
;;; it is left unchanged and FALSE is returned.
;;;
;;; iter :
;;;     a PangoScriptIter
;;;
;;; Returns :
;;;     TRUE if iter was successfully advanced
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_iter_free ()
;;;
;;; void pango_script_iter_free (PangoScriptIter *iter);
;;;
;;; Frees a PangoScriptIter created with pango_script_iter_new().
;;;
;;; iter :
;;;     a PangoScriptIter
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoLanguage
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque pango-language "PangoLanguage"
  :alloc (error "PangoLanguage can not be created from Lisp side."))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-language atdoc:*class-name-alias*) "CStruct"
      (documentation 'pango-language 'type)
 "@version{2013-3-9}
  @begin{short}
    The @sym{pango-language} structure is used to represent a language.
  @end{short}

  @sym{pango-language} pointers can be efficiently copied and compared with each
  other.")

(export (boxed-related-symbols 'pango-language))

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_LANGUAGE
;;; ----------------------------------------------------------------------------

(defcfun ("pango_language_get_type" pango-language-get-type) g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-3-9}
  @short{The GObject type for @class{pango-language} structure.}")

(export 'pango-language-get-type)

;;; ----------------------------------------------------------------------------
;;; pango_language_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_language_from_string" pango-language-from-string)
    (g-boxed-foreign pango-language)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[language]{a string representing a language tag, or @code{nil}}
  @begin{return}
    An opaque pointer to a @class{pango-language} structure, or @code{nil} if
    language was @code{NULL}. The returned pointer will be valid forever after,
    and should not be freed.
  @end{return}
  @begin{short}
    Take a RFC-3066 format language tag as a string and convert it to a
    @class{pango-language} pointer that can be efficiently copied (copy the
    pointer) and compared with other language tags (compare the pointer.)
  @end{short}

  This function first canonicalizes the string by converting it to lowercase,
  mapping '_' to '-', and stripping all characters other than letters and '-'.

  Use the function @fun{pango-language-default} if you want to get the
  @class{pango-language} structure for the current locale of the process.
  @see-function{pango-language-default}"
  (language :string))

(export 'pango-language-from-string)

;;; ----------------------------------------------------------------------------
;;; pango_language_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_language_to_string" pango-language-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[language]{a language tag}
  @begin{return}
    A string representing the language tag. This is owned by Pango and
    should not be freed.
  @end{return}
  Gets the RFC-3066 format string representing the given language tag."
  (language (g-boxed-foreign pango-language)))

(export 'pango-language-to-string)

;;; ----------------------------------------------------------------------------
;;; pango_language_matches ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_language_matches" pango-language-matches) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-30}
  @argument[language]{a language tag (see the function
    @fun{pango-language-from-string}), @code{nil} is allowed and matches
    nothing but '*'}
  @argument[range-list]{a list of language ranges, separated by ';', ':', ',',
    or space characters. Each element must either be '*', or a RFC 3066 language
    range canonicalized as by the function @fun{pango-language-from-string}}
  @return{@em{True} if a match was found.}
  @begin{short}
    Checks if a language tag matches one of the elements in a list of language
    ranges.
  @end{short}
  A language tag is considered to match a range in the list if the range is '*',
  the range is exactly the tag, or the range is a prefix of the tag, and the
  character after it in the tag is '-'.
  @see-function{pango-language-from-string}"
  (language (g-boxed-foreign pango-language))
  (range-list :string))

(export 'pango-language-matches)

;;; ----------------------------------------------------------------------------
;;; pango_language_includes_script ()
;;;
;;; gboolean pango_language_includes_script (PangoLanguage *language,
;;;                                          PangoScript script);
;;;
;;; Determines if script is one of the scripts used to write language. The
;;; returned value is conservative; if nothing is known about the language tag
;;; language, TRUE will be returned, since, as far as Pango knows, script might
;;; be used to write language.
;;;
;;; This routine is used in Pango's itemization process when determining if a
;;; supplied language tag is relevant to a particular section of text. It
;;; probably is not useful for applications in most circumstances.
;;;
;;; This function uses pango_language_get_scripts() internally.
;;;
;;; language :
;;;     a PangoLanguage, or NULL
;;;
;;; script :
;;;     a PangoScript
;;;
;;; Returns :
;;;     TRUE if script is one of the scripts used to write language or if
;;;     nothing is known about language (including the case that language is
;;;     NULL), FALSE otherwise.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_language_get_scripts ()
;;;
;;; const PangoScript * pango_language_get_scripts (PangoLanguage *language,
;;;                                                 int *num_scripts);
;;;
;;; Determines the scripts used to to write language. If nothing is known about
;;; the language tag language, or if language is NULL, then NULL is returned.
;;; The list of scripts returned starts with the script that the language uses
;;; most and continues to the one it uses least.
;;;
;;; The value num_script points at will be set to the number of scripts in the
;;; returned array (or zero if NULL is returned).
;;;
;;; Most languages use only one script for writing, but there are some that use
;;; two (Latin and Cyrillic for example), and a few use three (Japanese for
;;; example). Applications should not make any assumptions on the maximum number
;;; of scripts returned though, except that it is positive if the return value
;;; is not NULL, and it is a small number.
;;;
;;; The pango_language_includes_script() function uses this function internally.
;;;
;;; language :
;;;     a PangoLanguage, or NULL
;;;
;;; num_scripts :
;;;     location to return number of scripts, or NULL
;;;
;;; Returns :
;;;     An array of PangoScript values, with the number of entries in the array
;;;     stored in num_scripts, or NULL if Pango does not have any information
;;;     about this particular language tag (also the case if language is NULL).
;;;     The returned array is owned by Pango and should not be modified or
;;;     freed.
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_language_get_default () -> pango-language-default
;;; ----------------------------------------------------------------------------

(defcfun ("pango_language_get_default" pango-language-default)
    (g-boxed-foreign pango-language)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-28}
  @return{The default language as a @class{pango-language} structure.}
  @begin{short}
    Returns the @class{pango-language} structure for the current locale of the
    process.
  @end{short}
  Note that this can change over the life of an application.

  On Unix systems, this is the return value derived from
  @code{setlocale(LC_CTYPE, NULL)}, and the user can affect this through the
  environment variables @code{LC_ALL}, @code{LC_CTYPE} or @code{LANG} (checked
  in that order). The locale string typically is in the form
  @code{lang_COUNTRY}, where @code{lang} is an ISO-639 language code, and
  @code{COUNTRY} is an ISO-3166 country code. For instance, @code{sv_FI} for
  Swedish as written in Finland or @code{pt_BR} for Portuguese as written in
  Brazil.

  On Windows, the C library does not use any such environment variables, and
  setting them won't affect the behavior of functions like @code{ctime()}. The
  user sets the locale through the Regional Options in the Control Panel. The
  C library (in the @code{setlocale()} function) does not use country and
  language codes, but country and language names spelled out in English.
  However, this function does check the above environment variables, and does
  return a Unix-style locale string based on either said environment variables
  or the thread's current locale.

  Your application should call @code{setlocale(LC_ALL, \"\");} for the user
  settings to take effect. Gtk+ does this in its initialization functions
  automatically by calling @code{gtk_set_locale()}. See @code{man setlocale}
  for more details.

  Since 1.16
  @begin[Example]{dictionary}
    @begin{pre}
  (pango-language-default)
=> #<PANGO-LANGUAGE {10019E6973@}>
  (pango-language-to-string *)
=> \"de-de\"
    @end{pre}
  @end{dictionary}
  @see-class{pango-language}
  @see-function{pango-language-sample-string}")

(export 'pango-language-default)

;;; ----------------------------------------------------------------------------
;;; pango_language_get_sample_string () -> pango-language-sample-string
;;; ----------------------------------------------------------------------------

(defcfun ("pango_language_get_sample_string" pango-language-sample-string)
    g-string
 #+cl-cffi-gtk-documentation
 "@version{2020-4-28}
  @argument[language]{a @class{pango-language} structure, or @code{nil}}
  @begin{return}
    The sample string.
  @end{return}
  @begin{short}
    Get a string that is representative of the characters needed to render
    a particular language.
  @end{short}

  The sample text may be a pangram, but is not necessarily. It is chosen to
  be demonstrative of normal text in the language, as well as exposing font
  feature requirements unique to the language. It is suitable for use as
  sample text in a font selection dialog.

  If @arg{language} is @code{nil}, the default language as found by the
  function @fun{pango-language-default} is used.

  If Pango does not have a sample string for @arg{language}, the classic
  \"The quick brown fox...\" is returned.
  @begin[Example]{dictionary}
    @begin{pre}
  (pango-language-sample-string (pango-language-default))
=> \"Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich.\"
    @end{pre}
  @end{dictionary}
  @see-class{pango-language}
  @see-function{pango-language-default}"
  (language (g-boxed-foreign pango-language)))

(export 'pango-language-sample-string)

;;; --- End of file pango.script.lisp ------------------------------------------
