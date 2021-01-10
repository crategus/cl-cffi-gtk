;;; ----------------------------------------------------------------------------
;;; pango.rendering.lisp
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
;;; Rendering
;;;
;;;     Functions to run the rendering pipeline
;;;
;;; Types and Values
;;;
;;;     PangoItem
;;;     PangoAnalysis
;;;
;;;     PANGO_ANALYSIS_FLAG_CENTERED_BASELINE
;;;     PANGO_ANALYSIS_FLAG_IS_ELLIPSIS
;;;     PANGO_ANALYSIS_FLAG_NEED_HYPHEN
;;;
;;;     PangoLogAttr
;;;     PangoShapeFlags
;;;
;;; Functions
;;;
;;;     pango_itemize
;;;     pango_itemize_with_base_dir
;;;     pango_item_free
;;;     pango_item_copy
;;;     pango_item_new
;;;     pango_item_split
;;;     pango_item_apply_attrs
;;;     pango_reorder_items
;;;     pango_break
;;;     pango_get_log_attrs
;;;     pango_find_paragraph_boundary
;;;     pango_default_break
;;;     pango_tailor_break
;;;     pango_shape
;;;     pango_shape_full
;;;     pango_shape_with_flags
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── PangoItem
;;;
;;;     GFlags
;;;     ╰── PangoShapeFlags
;;;
;;; Description
;;;
;;;     The Pango rendering pipeline takes a string of Unicode characters and
;;;     converts it into glyphs. The functions described in this section
;;;     accomplish various steps of this process.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; struct PangoAnalysis
;;;
;;; struct PangoAnalysis {
;;;   PangoEngineShape *shape_engine;
;;;   PangoEngineLang  *lang_engine;
;;;   PangoFont *font;
;;;
;;;   guint8 level;
;;;   guint8 gravity;
;;;   guint8 flags;
;;;
;;;   guint8 script;
;;;   PangoLanguage *language;
;;;
;;;   GSList *extra_attrs;
;;; };
;;;
;;; The PangoAnalysis structure stores information about the properties of a
;;; segment of text.
;;;
;;; PangoEngineShape *shape_engine :
;;;     unused
;;;
;;; PangoEngineLang *lang_engine :
;;;     unused
;;;
;;; PangoFont *font :
;;;     the font for this segment.
;;;
;;; guint8 level :
;;;     the bidirectional level for this segment.
;;;
;;; guint8 gravity :
;;;     the glyph orientation for this segment (A PangoGravity).
;;;
;;; guint8 flags :
;;;     boolean flags for this segment (Since: 1.16).
;;;
;;; guint8 script:
;;;     the detected script for this segment (A PangoScript) (Since: 1.18).
;;;
;;; PangoLanguage *language :
;;;     the detected language for this segment.
;;;
;;; GSList *extra_attrs :
;;;     extra attributes for this segment.
;;; ----------------------------------------------------------------------------

(defcstruct pango-analysis
  (shape-engine :pointer)
  (lang-engine :pointer)
  (font (g-object pango-font))
  (level :uint8)
  (gravity :uint8)
  (flags :uint8)
  (script :uint8)
  (language (g-boxed-foreign pango-language))
  (extra-attrs :pointer))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-analysis atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'pango-analysis atdoc:*external-symbols*)
 "@version{2021-1-10}
")

;;; ----------------------------------------------------------------------------
;;; struct PangoItem
;;;
;;; struct PangoItem {
;;;   gint offset;
;;;   gint length;
;;;   gint num_chars;
;;;   PangoAnalysis analysis;
;;; };
;;;
;;; The PangoItem structure stores information about a segment of text.
;;;
;;; gint offset :
;;;     byte offset of the start of this item in text.
;;;
;;; gint length :
;;;     length of this item in bytes.
;;;
;;; gint num_chars :
;;;     number of Unicode characters in the item.
;;;
;;; PangoAnalysis analysis :
;;;     analysis results for the item.
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct pango-item "PangoItem"
  (offset :int)
  (length :int)
  (num-chars :int)
  (analysis (:pointer (:struct pango-analysis))))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-item atdoc:*class-name-alias*)
      "CStruct"
      (documentation 'pango-item 'type)
 "@version{2021-1-9}
")

;;; ----------------------------------------------------------------------------
;;; PANGO_ANALYSIS_FLAG_CENTERED_BASELINE
;;;
;;; #define PANGO_ANALYSIS_FLAG_CENTERED_BASELINE (1 << 0)
;;;
;;; Whether the segment should be shifted to center around the baseline. Used
;;; in vertical writing directions mostly.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_ANALYSIS_FLAG_IS_ELLIPSIS
;;;
;;; #define PANGO_ANALYSIS_FLAG_IS_ELLIPSIS (1 << 1)
;;;
;;; This flag is used to mark runs that hold ellipsized text, in an ellipsized
;;; layout.
;;;
;;; Since 1.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_ANALYSIS_FLAG_NEED_HYPHEN
;;;
;;; #define PANGO_ANALYSIS_FLAG_NEED_HYPHEN (1 << 2)
;;;
;;; This flag tells Pango to add a hyphen at the end of the run during shaping.
;;;
;;; Since 1.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoLogAttr
;;; ----------------------------------------------------------------------------

(defcstruct pango-log-attr
  (is-line-break :uint)
  (is-mandatory-break :uint)
  (is-char-break :uint)
  (is-white :uint)
  (is-cursor-position :uint)
  (is-word-start :uint)
  (is-word-end :uint)
  (is-sentence-boundary :uint)
  (is-sentence-start :uint)
  (is-sentence-end :uint)
  (backspcaces-deletes-character :uint)
  (is-expandable-space :uint)
  (is-word-boundary :uint))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-log-attr atdoc:*symbol-name-alias*)
      "CStruct"
      (gethash 'pango-log-attr atdoc:*external-symbols*)
 "@version{2021-1-6}
  @begin{short}
    The @sym{pango-log-attr} structure stores information about the attributes
    of a single character.
  @end{short}
  @begin{pre}
(defcstruct pango-log-attr
  (is-line-break :uint)
  (is-mandatory-break :uint)
  (is-char-break :uint)
  (is-white :uint)
  (is-cursor-position :uint)
  (is-word-start :uint)
  (is-word-end :uint)
  (is-sentence-boundary :uint)
  (is-sentence-start :uint)
  (is-sentence-end :uint)
  (backspcaces-deletes-character :uint)
  (is-expandable-space :uint)
  (is-word-boundary :uint))
  @end{pre}
  @begin[code]{table}
    @entry[is-line-break]{If set, can break line in front of character.}
    @entry[is-mandatory-break]{If set, must break line in front of character.}
    @entry[is-char-break]{If set, can break here when doing character wrapping.}
    @entry[is-white]{Is whitespace character.}
    @entry[is-cursor-position]{If set, cursor can appear in front of character.
      I.e. this is a grapheme boundary, or the first character in the text.
      This flag implements Unicode's Grapheme Cluster Boundaries semantics.}
    @entry[is-word-start]{Is first character in a word.}
    @entry[is-word-end]{Is first non-word char after a word. Note that in
      degenerate cases, you could have both @code{is-word-start} and
      @code{is-word-end} set for some character.}
    @entry[is-sentence-boundary]{Is a sentence boundary. There are two ways to
      divide sentences. The first assigns all inter-sentence
      whitespace/control/format chars to some sentence, so all chars are in
      some sentence; @code{is-sentence-boundary} denotes the boundaries there.
      The second way does not assign between-sentence spaces, etc. to any
      sentence, so @code{is-sentence_start}/@code{is-sentence-end} mark the
      boundaries of those sentences.}
    @entry[is-sentence-start]{Is first character in a sentence.}
    @entry[is-sentence-end]{Is first char after a sentence. Note that in
      degenerate cases, you could have both @code{is-sentence-start} and
      @code{is-sentence-end} set for some character, e.g. no space after a
      period, so the next sentence starts right away.}
    @entry[backspace-deletes-character]{If set, backspace deletes one character
      rather than the entire grapheme cluster. This field is only meaningful on
      grapheme boundaries, where @code{is-cursor-position} is set. In some
      languages, the full grapheme, e.g. letter + diacritics, is considered a
      unit, while in others, each decomposed character in the grapheme is a
      unit. In the default implementation of @fun{pango-break}, this bit is set
      on all grapheme boundaries except those following Latin, Cyrillic or
      Greek base characters.}
    @entry[is-expandable-space]{Is a whitespace character that can possibly be
      expanded for justification purposes.}
    @entry[is-word-boundary]{Is a word boundary. More specifically, means that
      this is not a position in the middle of a word. For example, both sides
      of a punctuation mark are considered word boundaries. This flag is
      particularly useful when selecting text word-by-word. This flag
      implements Unicode's Word Boundaries semantics.}
  @end{table}
  @see-class{pango-layout}
  @see-function{pango-break}")

(export 'pango-log-attr)

;;; ----------------------------------------------------------------------------
;;; enum PangoShapeFlags
;;;
;;; Flags influencing the shaping process. These can be passed to
;;; pango_shape_with_flags().
;;;
;;; PANGO_SHAPE_NONE :
;;;     Default value.
;;;
;;; PANGO_SHAPE_ROUND_POSITIONS :
;;;     Round glyph positions and widths to whole device units. This option
;;;     should be set if the target renderer can't do subpixel positioning of
;;;     glyphs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_itemize ()
;;;
;;; GList *
;;; pango_itemize (PangoContext *context,
;;;                const char *text,
;;;                int start_index,
;;;                int length,
;;;                PangoAttrList *attrs,
;;;                PangoAttrIterator *cached_iter);
;;;
;;; Breaks a piece of text into segments with consistent directional level and
;;; shaping engine. Each byte of text will be contained in exactly one of the
;;; items in the returned list; the generated list of items will be in logical
;;; order (the start offsets of the items are ascending).
;;;
;;; cached_iter should be an iterator over attrs currently positioned at a range
;;; before or containing start_index ; cached_iter will be advanced to the range
;;; covering the position just after start_index + length . (i.e. if itemizing
;;; in a loop, just keep passing in the same cached_iter ).
;;;
;;; context :
;;;     a structure holding information that affects the itemization process.
;;;
;;; text :
;;;     the text to itemize. Must be valid UTF-8
;;;
;;; start_index :
;;;     first byte in text to process
;;;
;;; length :
;;;     the number of bytes (not characters) to process after start_index .
;;;     This must be >= 0.
;;;
;;; attrs :
;;;     the set of attributes that apply to text .
;;;
;;; cached_iter :
;;;     Cached attribute iterator, or NULL.
;;;
;;; Returns :
;;;     a GList of PangoItem structures. The items should be freed using
;;;     pango_item_free() probably in combination with g_list_foreach(), and
;;;     the list itself using g_list_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_itemize_with_base_dir ()
;;;
;;; GList *
;;; pango_itemize_with_base_dir (PangoContext *context,
;;;                              PangoDirection base_dir,
;;;                              const char *text,
;;;                              int start_index,
;;;                              int length,
;;;                              PangoAttrList *attrs,
;;;                              PangoAttrIterator *cached_iter);
;;;
;;; Like pango_itemize(), but the base direction to use when computing
;;; bidirectional levels (see pango_context_set_base_dir()), is specified
;;; explicitly rather than gotten from the PangoContext.
;;;
;;; context :
;;;     a structure holding information that affects the itemization process.
;;;
;;; base_dir :
;;;     base direction to use for bidirectional processing
;;;
;;; text :
;;;     the text to itemize.
;;;
;;; start_index :
;;;     first byte in text to process
;;;
;;; length :
;;;     the number of bytes (not characters) to process after start_index .
;;;     This must be >= 0.
;;;
;;; attrs :
;;;     the set of attributes that apply to text .
;;;
;;; cached_iter :
;;;     Cached attribute iterator, or NULL.
;;;
;;; Returns :
;;;     a GList of PangoItem structures. The items should be freed using
;;;     pango_item_free() probably in combination with g_list_foreach(), and
;;;     the list itself using g_list_free().
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_item_free ()
;;;
;;; void
;;; pango_item_free (PangoItem *item);
;;;
;;; Free a PangoItem and all associated memory.
;;;
;;; item :
;;;     a PangoItem, may be NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_item_copy ()
;;;
;;; PangoItem *
;;; pango_item_copy (PangoItem *item);
;;;
;;; Copy an existing PangoItem structure.
;;;
;;; item :
;;;     a PangoItem, may be NULL.
;;;
;;; Returns :
;;;     the newly allocated PangoItem, which should be freed with
;;;     pango_item_free(), or NULL if item was NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_item_new ()
;;;
;;; PangoItem *
;;; pango_item_new (void);
;;;
;;; Creates a new PangoItem structure initialized to default values.
;;;
;;; Returns :
;;;     the newly allocated PangoItem, which should be freed with
;;;     pango_item_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_item_split ()
;;;
;;; PangoItem *
;;; pango_item_split (PangoItem *orig,
;;;                   int split_index,
;;;                   int split_offset);
;;;
;;; Modifies orig to cover only the text after split_index , and returns a new
;;; item that covers the text before split_index that used to be in orig . You
;;; can think of split_index as the length of the returned item. split_index
;;; may not be 0, and it may not be greater than or equal to the length of orig
;;; (that is, there must be at least one byte assigned to each item, you can't
;;; create a zero-length item). split_offset is the length of the first item in
;;; chars, and must be provided because the text used to generate the item
;;; isn't available, so pango_item_split() can't count the char length of the
;;; split items itself.
;;;
;;; orig :
;;;     a PangoItem
;;;
;;; split_index :
;;;     byte index of position to split item, relative to the start of the item
;;;
;;; split_offset :
;;;     number of chars between start of orig and split_index
;;;
;;; Returns :
;;;     new item representing text before split_index , which should be freed
;;;     with pango_item_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_item_apply_attrs ()
;;;
;;; void
;;; pango_item_apply_attrs (PangoItem *item,
;;;                         PangoAttrIterator *iter);
;;;
;;; Add attributes to a PangoItem. The idea is that you have attributes that
;;; don't affect itemization, such as font features, so you filter them out
;;; using pango_attr_list_filter(), itemize your text, then reapply the
;;; attributes to the resulting items using this function.
;;;
;;; The iter should be positioned before the range of the item, and will be
;;; advanced past it. This function is meant to be called in a loop over the
;;; items resulting from itemization, while passing the iter to each call.
;;;
;;; item :
;;;     a PangoItem
;;;
;;; iter :
;;;     a PangoAttrIterator
;;;
;;; Since 1.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_reorder_items ()
;;;
;;; GList *
;;; pango_reorder_items (GList *logical_items);
;;;
;;; From a list of items in logical order and the associated directional levels,
;;; produce a list in visual order. The original list is unmodified.
;;;
;;; logical_items :
;;;     a GList of PangoItem in logical order.
;;;
;;; Returns :
;;;     a GList of PangoItem structures in visual order.
;;;
;;; (Please open a bug if you use this function. It is not a particularly
;;; convenient interface, and the code is duplicated elsewhere in Pango for
;;; that reason.).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_break ()
;;;
;;; void
;;; pango_break (const gchar *text,
;;;              int length,
;;;              PangoAnalysis *analysis,
;;;              PangoLogAttr *attrs,
;;;              int attrs_len);
;;;
;;; pango_break has been deprecated since version 1.44 and should not be used
;;; in newly-written code.
;;;
;;; Use pango_default_break() and pango_tailor_break()
;;;
;;; Determines possible line, word, and character breaks for a string of
;;; Unicode text with a single analysis. For most purposes you may want to use
;;; pango_get_log_attrs().
;;;
;;; text :
;;;     the text to process. Must be valid UTF-8
;;;
;;; length :
;;;     length of text in bytes (may be -1 if text is nul-terminated)
;;;
;;; analysis :
;;;     PangoAnalysis structure from pango_itemize()
;;;
;;; attrs :
;;;     an array to store character information in.
;;;
;;; attrs_len :
;;;     size of the array passed as attrs
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_get_log_attrs ()
;;;
;;; void
;;; pango_get_log_attrs (const char *text,
;;;                      int length,
;;;                      int level,
;;;                      PangoLanguage *language,
;;;                      PangoLogAttr *log_attrs,
;;;                      int attrs_len);
;;;
;;; Computes a PangoLogAttr for each character in text . The log_attrs array
;;; must have one PangoLogAttr for each position in text ; if text contains N
;;; characters, it has N+1 positions, including the last position at the end of
;;; the text. text should be an entire paragraph; logical attributes can't be
;;; computed without context (for example you need to see spaces on either side
;;; of a word to know the word is a word).
;;;
;;; text :
;;;     text to process. Must be valid UTF-8
;;;
;;; length :
;;;     length in bytes of text
;;;
;;; level :
;;;     embedding level, or -1 if unknown
;;;
;;; language :
;;;     language tag
;;;
;;; log_attrs :
;;;     array with one PangoLogAttr per character in text , plus one extra, to
;;;     be filled in.
;;;
;;; attrs_len :
;;;     length of log_attrs array
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_find_paragraph_boundary ()
;;;
;;; void
;;; pango_find_paragraph_boundary (const gchar *text,
;;;                                gint length,
;;;                                gint *paragraph_delimiter_index,
;;;                                gint *next_paragraph_start);
;;;
;;; Locates a paragraph boundary in text . A boundary is caused by delimiter
;;; characters, such as a newline, carriage return, carriage return-newline
;;; pair, or Unicode paragraph separator character. The index of the run of
;;; delimiters is returned in paragraph_delimiter_index . The index of the
;;; start of the paragraph (index after all delimiters) is stored in
;;; next_paragraph_start .
;;;
;;; If no delimiters are found, both paragraph_delimiter_index and
;;; next_paragraph_start are filled with the length of text (an index one off
;;; the end).
;;;
;;; text :
;;;     UTF-8 text
;;;
;;; length :
;;;     length of text in bytes, or -1 if nul-terminated
;;;
;;; paragraph_delimiter_index :
;;;     return location for index of delimiter.
;;;
;;; next_paragraph_start :
;;;     return location for start of next paragraph.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_default_break ()
;;;
;;; void
;;; pango_default_break (const gchar *text,
;;;                      int length,
;;;                      PangoAnalysis *analysis,
;;;                      PangoLogAttr *attrs,
;;;                      int attrs_len);
;;;
;;; This is the default break algorithm. It applies Unicode rules without
;;; language-specific tailoring, therefore the analyis argument is unused and
;;; can be NULL.
;;;
;;; See pango_tailor_break() for language-specific breaks.
;;;
;;; text :
;;;     text to break. Must be valid UTF-8
;;;
;;; length :
;;;     length of text in bytes (may be -1 if text is nul-terminated)
;;;
;;; analysis :
;;;     a PangoAnalysis for the text .
;;;
;;; attrs :
;;;     logical attributes to fill in
;;;
;;; attrs_len :
;;;     size of the array passed as attrs
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tailor_break ()
;;;
;;; void
;;; pango_tailor_break (const char *text,
;;;                     int length,
;;;                     PangoAnalysis *analysis,
;;;                     int offset,
;;;                     PangoLogAttr *log_attrs,
;;;                     int log_attrs_len);
;;;
;;; Apply language-specific tailoring to the breaks in log_attrs , which are
;;; assumed to have been produced by pango_default_break().
;;;
;;; If offset is not -1, it is used to apply attributes from analysis that are
;;; relevant to line breaking.
;;;
;;; text :
;;;     text to process. Must be valid UTF-8
;;;
;;; length :
;;;     length in bytes of text
;;;
;;; analysis :
;;;     PangoAnalysis structure from pango_itemize() for text
;;;
;;; offset :
;;;     Byte offset of text from the beginning of the paragraph, or -1 to
;;;     ignore attributes from analysis
;;;
;;; log_attrs :
;;;     array with one PangoLogAttr per character in text , plus one extra, to
;;;     be filled in.
;;;
;;; log_attrs_len :
;;;     length of log_attrs array
;;;
;;; Since 1.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_shape ()
;;;
;;; void
;;; pango_shape (const char *text,
;;;              int length,
;;;              const PangoAnalysis *analysis,
;;;              PangoGlyphString *glyphs);
;;;
;;; Given a segment of text and the corresponding PangoAnalysis structure
;;; returned from pango_itemize(), convert the characters into glyphs. You may
;;; also pass in only a substring of the item from pango_itemize().
;;;
;;; It is recommended that you use pango_shape_full() instead, since that API
;;; allows for shaping interaction happening across text item boundaries.
;;;
;;; Note that the extra attributes in the analyis that is returned from
;;; pango_itemize() have indices that are relative to the entire paragraph, so
;;; you need to subtract the item offset from their indices before calling
;;; pango_shape().
;;;
;;; text :
;;;     the text to process
;;;
;;; length :
;;;     the length (in bytes) of text
;;;
;;; analysis :
;;;     PangoAnalysis structure from pango_itemize()
;;;
;;; glyphs :
;;;     glyph string in which to store results
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_shape_full ()
;;;
;;; void
;;; pango_shape_full (const char *item_text,
;;;                   int item_length,
;;;                   const char *paragraph_text,
;;;                   int paragraph_length,
;;;                   const PangoAnalysis *analysis,
;;;                   PangoGlyphString *glyphs);
;;;
;;; Given a segment of text and the corresponding PangoAnalysis structure
;;; returned from pango_itemize(), convert the characters into glyphs. You may
;;; also pass in only a substring of the item from pango_itemize().
;;;
;;; This is similar to pango_shape(), except it also can optionally take the
;;; full paragraph text as input, which will then be used to perform certain
;;; cross-item shaping interactions. If you have access to the broader text of
;;; which item_text is part of, provide the broader text as paragraph_text . If
;;; paragraph_text is NULL, item text is used instead.
;;;
;;; Note that the extra attributes in the analyis that is returned from
;;; pango_itemize() have indices that are relative to the entire paragraph, so
;;; you do not pass the full paragraph text as paragraph_text , you need to
;;; subtract the item offset from their indices before calling
;;; pango_shape_full().
;;;
;;; item_text :
;;;     valid UTF-8 text to shape.
;;;
;;; item_length :
;;;     the length (in bytes) of item_text . -1 means nul-terminated text.
;;;
;;; paragraph_text :
;;;     text of the paragraph (see details). May be NULL.
;;;
;;; paragraph_length :
;;;     the length (in bytes) of paragraph_text . -1 means nul-terminated text.
;;;
;;; analysis :
;;;     PangoAnalysis structure from pango_itemize().
;;;
;;; glyphs :
;;;     glyph string in which to store results.
;;;
;;; Since 1.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_shape_with_flags ()
;;;
;;; void
;;; pango_shape_with_flags (const char *item_text,
;;;                         int item_length,
;;;                         const char *paragraph_text,
;;;                         int paragraph_length,
;;;                         const PangoAnalysis *analysis,
;;;                         PangoGlyphString *glyphs,
;;;                         PangoShapeFlags flags);
;;;
;;; Given a segment of text and the corresponding PangoAnalysis structure
;;; returned from pango_itemize(), convert the characters into glyphs. You may
;;; also pass in only a substring of the item from pango_itemize().
;;;
;;; This is similar to pango_shape_full(), except it also takes flags that can
;;; influence the shaping process.
;;;
;;; Note that the extra attributes in the analyis that is returned from
;;; pango_itemize() have indices that are relative to the entire paragraph, so
;;; you do not pass the full paragraph text as paragraph_text , you need to
;;; subtract the item offset from their indices before calling
;;; pango_shape_with_flags().
;;;
;;; item_text :
;;;     valid UTF-8 text to shape
;;;
;;; item_length :
;;;     the length (in bytes) of item_text . -1 means nul-terminated text.
;;;
;;; paragraph_text :
;;;     text of the paragraph (see details). May be NULL.
;;;
;;; paragraph_length :
;;;     the length (in bytes) of paragraph_text . -1 means nul-terminated text.
;;;
;;; analysis :
;;;     PangoAnalysis structure from pango_itemize()
;;;
;;; glyphs :
;;;     glyph string in which to store results
;;;
;;; flags :
;;;     flags influencing the shaping process
;;;
;;; Since 1.44
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.rendering.lisp ---------------------------------------
