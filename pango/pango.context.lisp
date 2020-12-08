;;; ----------------------------------------------------------------------------
;;; pango.context.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; for Pango 1.32.6 and modified to document the Lisp binding to the Pango
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;; Contexts
;;;
;;;     Global context object
;;;
;;; Synopsis
;;;
;;;     PangoContext
;;;     PangoItem
;;;     PangoAnalysis
;;;
;;;     PANGO_ANALYSIS_FLAG_CENTERED_BASELINE
;;;     PANGO_TYPE_DIRECTION
;;;
;;;     pango_itemize
;;;     pango_itemize_with_base_dir
;;;     pango_item_free
;;;     pango_item_copy
;;;     pango_item_new
;;;     pango_item_split
;;;     pango_reorder_items
;;;
;;;     pango_context_new
;;;     pango_context_changed
;;;     pango_context_get_serial
;;;     pango_context_set_font_map
;;;     pango_context_get_font_map
;;;     pango_context_get_font_description
;;;     pango_context_set_font_description
;;;     pango_context_get_language
;;;     pango_context_set_language
;;;     pango_context_get_base_dir
;;;     pango_context_set_base_dir
;;;     pango_context_get_base_gravity
;;;     pango_context_set_base_gravity
;;;     pango_context_get_gravity
;;;     pango_context_get_gravity_hint
;;;     pango_context_set_gravity_hint
;;;     pango_context_get_matrix
;;;     pango_context_set_matrix
;;;     pango_context_load_font
;;;     pango_context_load_fontset
;;;     pango_context_get_metrics
;;;     pango_context_list_families
;;;
;;;     pango_break
;;;     pango_get_log_attrs
;;;     pango_find_paragraph_boundary
;;;     pango_default_break
;;;
;;;     PangoLogAttr
;;;
;;;     pango_shape
;;;     pango_shape_full
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoContext" pango-context
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "pango_context_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'pango-context 'type)
 "@version{2013-6-29}
  The @sym{pango-context} object stores global information used to control the
  itemization process.
  @see-function{pango-context-new}")

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
;;; It contains the following fields:
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAnalysis
;;;
;;; struct PangoAnalysis {
;;;   PangoEngineShape *shape_engine;
;;;   PangoEngineLang  *lang_engine;
;;;   PangoFont *font;
;;;
;;;   guint8 level;
;;;   guint8 gravity; /* PangoGravity */
;;;   guint8 flags;
;;;
;;;   guint8 script; /* PangoScript */
;;;   PangoLanguage *language;
;;;
;;;   GSList *extra_attrs;
;;; };
;;;
;;; The PangoAnalysis structure stores information about the properties of a
;;; segment of text. It has the following fields:
;;;
;;; PangoEngineShape *shape_engine;
;;;     the engine for doing rendering-system-dependent processing.
;;;
;;; PangoEngineLang *lang_engine;
;;;     the engine for doing rendering-system-independent processing.
;;;
;;; PangoFont *font;
;;;     the font for this segment.
;;;
;;; guint8 level;
;;;     the bidirectional level for this segment.
;;;
;;; guint8 gravity;
;;;     the glyph orientation for this segment (A PangoGravity).
;;;
;;; guint8 flags;
;;;     boolean flags for this segment (currently only one) (Since: 1.16).
;;;
;;; guint8 script;
;;;     the detected script for this segment (A PangoScript) (Since: 1.18).
;;;
;;; PangoLanguage *language;
;;;     the detected language for this segment.
;;;
;;; GSList *extra_attrs;
;;;     extra attributes for this segment.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_ANALYSIS_FLAG_CENTERED_BASELINE
;;;
;;; #define PANGO_ANALYSIS_FLAG_CENTERED_BASELINE (1 << 0)
;;;
;;; Whether the segment should be shifted to center around the baseline. Used
;;; in vertical writing directions mostly. Since: 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_TYPE_DIRECTION
;;;
;;; #define PANGO_TYPE_DIRECTION (pango_direction_get_type ())
;;;
;;; The GObject type for PangoDirection.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_itemize ()
;;;
;;; GList * pango_itemize (PangoContext *context,
;;;                        const char *text,
;;;                        int start_index,
;;;                        int length,
;;;                        PangoAttrList *attrs,
;;;                        PangoAttrIterator *cached_iter);
;;;
;;; Breaks a piece of text into segments with consistent directional level and
;;; shaping engine. Each byte of text will be contained in exactly one of the
;;; items in the returned list; the generated list of items will be in logical
;;; order (the start offsets of the items are ascending).
;;;
;;; cached_iter should be an iterator over attrs currently positioned at a range
;;; before or containing start_index; cached_iter will be advanced to the range
;;; covering the position just after start_index + length. (i.e. if itemizing in
;;; a loop, just keep passing in the same cached_iter).
;;;
;;; context :
;;;     a structure holding information that affects the itemization process.
;;;
;;; text :
;;;     the text to itemize.
;;;
;;; start_index :
;;;     first byte in text to process
;;;
;;; length :
;;;     the number of bytes (not characters) to process after start_index.
;;;     This must be >= 0.
;;;
;;; attrs :
;;;     the set of attributes that apply to text.
;;;
;;; cached_iter :
;;;     Cached attribute iterator, or NULL
;;;
;;; Returns :
;;;     A GList of PangoItem structures. The items should be freed using
;;;     pango_item_free() probably in combination with g_list_foreach(), and
;;;     the list itself using g_list_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_itemize_with_base_dir ()
;;;
;;; GList * pango_itemize_with_base_dir (PangoContext *context,
;;;                                      PangoDirection base_dir,
;;;                                      const char *text,
;;;                                      int start_index,
;;;                                      int length,
;;;                                      PangoAttrList *attrs,
;;;                                      PangoAttrIterator *cached_iter);
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
;;;     the number of bytes (not characters) to process after start_index.
;;;     This must be >= 0.
;;;
;;; attrs :
;;;     the set of attributes that apply to text.
;;;
;;; cached_iter :
;;;     Cached attribute iterator, or NULL
;;;
;;; Returns :
;;;     a GList of PangoItem structures. The items should be freed using
;;;     pango_item_free() probably in combination with g_list_foreach(), and the
;;;     list itself using g_list_free().
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_item_free ()
;;;
;;; void pango_item_free (PangoItem *item);
;;;
;;; Free a PangoItem and all associated memory.
;;;
;;; item :
;;;     a PangoItem, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_item_copy ()
;;;
;;; PangoItem * pango_item_copy (PangoItem *item);
;;;
;;; Copy an existing PangoItem structure.
;;;
;;; item :
;;;     a PangoItem, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoItem, which should be freed with
;;;     pango_item_free(), or NULL if item was NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_item_new ()
;;;
;;; PangoItem * pango_item_new (void);
;;;
;;; Creates a new PangoItem structure initialized to default values.
;;;
;;; Returns :
;;;     the newly allocated PangoItem, which should be freed with pango_item_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_item_split ()
;;;
;;; PangoItem * pango_item_split (PangoItem *orig,
;;;                               int split_index,
;;;                               int split_offset);
;;;
;;; Modifies orig to cover only the text after split_index, and returns a new
;;; item that covers the text before split_index that used to be in orig. You
;;; can think of split_index as the length of the returned item. split_index may
;;; not be 0, and it may not be greater than or equal to the length of orig
;;; (that is, there must be at least one byte assigned to each item, you can't
;;; create a zero-length item). split_offset is the length of the first item in
;;; chars, and must be provided because the text used to generate the item isn't
;;; available, so pango_item_split() can't count the char length of the split
;;; items itself.
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
;;;     new item representing text before split_index, which should be freed
;;;     with pango_item_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_reorder_items ()
;;;
;;; GList * pango_reorder_items (GList *logical_items);
;;;
;;; From a list of items in logical order and the associated directional levels,
;;; produce a list in visual order. The original list is unmodified.
;;;
;;; logical_items :
;;;     a GList of PangoItem in logical order. [element-type Pango.Item]
;;;
;;; Returns :
;;;     a GList of PangoItem structures in visual order. (Please open a bug if
;;;     you use this function. It is not a particularly convenient interface,
;;;     and the code is duplicated elsewhere in Pango for that reason.).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline pango-context-new))

(defun pango-context-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @return{The newly allocated @class{pango-context}.}
  @begin{short}
    Creates a new @class{pango-context} initialized to default values.
  @end{short}

  This function is not particularly useful as it should always be followed by
  a @fun{pango-context-set-font-map} call, and the function
  @fun{pango-font-map-create-context} does these two steps together and hence
  users are recommended to use that.

  If you are using Pango as part of a higher-level system, that system may
  have it's own way of create a @class{pango-context}. For instance, the GTK+
  toolkit has, among others, the functions @fun{gdk-pango-context-for-screen},
  and @fun{gtk-widget-pango-context}. Use those instead.
  @see-class{pango-context}
  @see-function{pango-context-set-font-map}
  @see-function{gdk-pango-context-for-screen}
  @see-function{gtk-widget-pango-context}"
  (make-instance 'pango-context))

(export 'pango-context-new)

;;; ----------------------------------------------------------------------------
;;; pango_context_changed ()
;;;
;;; void pango_context_changed (PangoContext *context);
;;;
;;; Forces a change in the context, which will cause any PangoLayout using this
;;; context to re-layout.
;;;
;;; This function is only useful when implementing a new backend for Pango,
;;; something applications won't do. Backends should call this function if they
;;; have attached extra data to the context and such data is changed.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Since 1.32.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_get_serial ()
;;;
;;; guint pango_context_get_serial (PangoContext *context);
;;;
;;; Returns the current serial number of context. The serial number is
;;; initialized to an small number larger than zero when a new context is
;;; created and is increased whenever the context is changed using any of the
;;; setter functions, or the PangoFontMap it uses to find fonts has changed. The
;;; serial may wrap, but will never have the value 0. Since it can wrap, never
;;; compare it with "less than", always use "not equals".
;;;
;;; This can be used to automatically detect changes to a PangoContext, and is
;;; only useful when implementing objects that need update when their
;;; PangoContext changes, like PangoLayout.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Returns :
;;;     The current serial number of context.
;;;
;;; Since 1.32.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_set_font_map ()
;;;
;;; void pango_context_set_font_map (PangoContext *context,
;;;                                  PangoFontMap *font_map);
;;;
;;; Sets the font map to be searched when fonts are looked-up in this context.
;;; This is only for internal use by Pango backends, a PangoContext obtained via
;;; one of the recommended methods should already have a suitable font map.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; font_map :
;;;     the PangoFontMap to set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_get_font_map ()
;;;
;;; PangoFontMap * pango_context_get_font_map (PangoContext *context);
;;;
;;; Gets the PangoFontmap used to look up fonts for this context.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Returns :
;;;     the font map for the PangoContext. This value is owned by Pango and
;;;     should not be unreferenced.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_get_font_description ()
;;;
;;; PangoFontDescription * pango_context_get_font_description
;;;                                                     (PangoContext *context);
;;;
;;; Retrieve the default font description for the context.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Returns :
;;;     a pointer to the context's default font description. This value must
;;;     not be modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_set_font_description ()
;;;
;;; void pango_context_set_font_description (PangoContext *context,
;;;                                          const PangoFontDescription *desc);
;;;
;;; Set the default font description for the context
;;;
;;; context :
;;;     a PangoContext
;;;
;;; desc :
;;;     the new pango font description
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_get_language ()
;;;
;;; PangoLanguage * pango_context_get_language (PangoContext *context);
;;;
;;; Retrieves the global language tag for the context.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Returns :
;;;     the global language tag.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_set_language ()
;;;
;;; void pango_context_set_language (PangoContext *context,
;;;                                  PangoLanguage *language);
;;;
;;; Sets the global language tag for the context. The default language for the
;;; locale of the running process can be found using
;;; pango_language_get_default().
;;;
;;; context :
;;;     a PangoContext
;;;
;;; language :
;;;     the new language tag.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_get_base_dir ()
;;;
;;; PangoDirection pango_context_get_base_dir (PangoContext *context);
;;;
;;; Retrieves the base direction for the context.
;;; See pango_context_set_base_dir().
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Returns :
;;;     the base direction for the context.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_set_base_dir ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_context_set_base_dir" pango-context-set-base-dir) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-1-24}
  @argument[context]{a @class{pango-context}}
  @argument[direction]{the new base direction of type @symbol{pango-direction}}
  @begin{short}
    Sets the base direction for the context.
  @end{short}

  The base direction is used in applying the Unicode bidirectional algorithm;
  if the direction is @code{:ltr} or @code{:rtl}, then the value will be used as
  the paragraph direction in the Unicode bidirectional algorithm. A value of
  @code{:weak-ltr} or @code{:weak-rtl} is used only for paragraphs that do not
  contain any strong characters themselves.
  @see-class{pango-context}
  @see-symbol{pango-direction}
  @see-function{pango-context-get-base-dir}"
  (context (g-object pango-context))
  (direction pango-direction))

(export 'pango-context-set-base-dir)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_base_gravity ()
;;;
;;; PangoGravity pango_context_get_base_gravity (PangoContext *context);
;;;
;;; Retrieves the base gravity for the context. See
;;; pango_context_set_base_gravity().
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Returns :
;;;     the base gravity for the context.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_set_base_gravity ()
;;;
;;; void pango_context_set_base_gravity (PangoContext *context,
;;;                                      PangoGravity gravity);
;;;
;;; Sets the base gravity for the context.
;;;
;;; The base gravity is used in laying vertical text out.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; gravity :
;;;     the new base gravity
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_get_gravity ()
;;;
;;; PangoGravity pango_context_get_gravity (PangoContext *context);
;;;
;;; Retrieves the gravity for the context. This is similar to
;;; pango_context_get_base_gravity(), except for when the base gravity is
;;; PANGO_GRAVITY_AUTO for which pango_gravity_get_for_matrix() is used to
;;; return the gravity from the current context matrix.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Returns :
;;;     the resolved gravity for the context.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_get_gravity_hint ()
;;;
;;; PangoGravityHint pango_context_get_gravity_hint (PangoContext *context);
;;;
;;; Retrieves the gravity hint for the context. See
;;; pango_context_set_gravity_hint() for details.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Returns :
;;;     the gravity hint for the context.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_set_gravity_hint ()
;;;
;;; void pango_context_set_gravity_hint (PangoContext *context,
;;;                                      PangoGravityHint hint);
;;;
;;; Sets the gravity hint for the context.
;;;
;;; The gravity hint is used in laying vertical text out, and is only relevant
;;; if gravity of the context as returned by pango_context_get_gravity() is set
;;; PANGO_GRAVITY_EAST or PANGO_GRAVITY_WEST.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; hint :
;;;     the new gravity hint
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_get_matrix ()
;;;
;;; const PangoMatrix * pango_context_get_matrix (PangoContext *context);
;;;
;;; Gets the transformation matrix that will be applied when rendering with this
;;; context. See pango_context_set_matrix().
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Returns :
;;;     the matrix, or NULL if no matrix has been set (which is the same as the
;;;     identity matrix). The returned matrix is owned by Pango and must not be
;;;     modified or freed.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_set_matrix ()
;;;
;;; void pango_context_set_matrix (PangoContext *context,
;;;                                const PangoMatrix *matrix);
;;;
;;; Sets the transformation matrix that will be applied when rendering with this
;;; context. Note that reported metrics are in the user space coordinates before
;;; the application of the matrix, not device-space coordinates after the
;;; application of the matrix. So, they don't scale with the matrix, though they
;;; may change slightly for different matrices, depending on how the text is fit
;;; to the pixel grid.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; matrix :
;;;     a PangoMatrix, or NULL to unset any existing matrix. (No matrix set is
;;;     the same as setting the identity matrix.).
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_load_font ()
;;;
;;; PangoFont * pango_context_load_font (PangoContext *context,
;;;                                      const PangoFontDescription *desc);
;;;
;;; Loads the font in one of the fontmaps in the context that is the closest
;;; match for desc.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; desc :
;;;     a PangoFontDescription describing the font to load
;;;
;;; Returns :
;;;     the newly allocated PangoFont that was loaded, or NULL if no font
;;;     matched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_load_fontset ()
;;;
;;; PangoFontset * pango_context_load_fontset (PangoContext *context,
;;;                                            const PangoFontDescription *desc,
;;;                                            PangoLanguage *language);
;;;
;;; Load a set of fonts in the context that can be used to render a font
;;; matching desc.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; desc :
;;;     a PangoFontDescription describing the fonts to load
;;;
;;; language :
;;;     a PangoLanguage the fonts will be used for
;;;
;;; Returns :
;;;     The newly allocated PangoFontset loaded, or NULL if no font matched.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_get_metrics ()
;;;
;;; PangoFontMetrics * pango_context_get_metrics (
;;;                                            PangoContext *context,
;;;                                            const PangoFontDescription *desc,
;;;                                            PangoLanguage *language);
;;;
;;; Get overall metric information for a particular font description. Since the
;;; metrics may be substantially different for different scripts, a language tag
;;; can be provided to indicate that the metrics should be retrieved that
;;; correspond to the script(s) used by that language.
;;;
;;; The PangoFontDescription is interpreted in the same way as by
;;; pango_itemize(), and the family name may be a comma separated list of
;;; figures. If characters from multiple of these families would be used to
;;; render the string, then the returned fonts would be a composite of the
;;; metrics for the fonts loaded for the individual families.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; desc :
;;;     a PangoFontDescription structure. NULL means that the font description
;;;     from the context will be used.
;;;
;;; language :
;;;     language tag used to determine which script to get the metrics for. NULL
;;;     means that the language tag from the context will be used. If no
;;;     language tag is set on the context, metrics for the default language
;;;     (as determined by pango_language_get_default()) will be returned.
;;;
;;; Returns :
;;;     a PangoFontMetrics object. The caller must call
;;;     pango_font_metrics_unref() when finished using the object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_context_list_families ()
;;;
;;; void pango_context_list_families (PangoContext *context,
;;;                                   PangoFontFamily ***families,
;;;                                   int *n_families);
;;;
;;; List all families for a context.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; families :
;;;     location to store a pointer to an array of PangoFontFamily *. This
;;;     array should be freed with g_free().
;;;
;;; n_families :
;;;     location to store the number of elements in descs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_break ()
;;;
;;; void pango_break (const gchar *text,
;;;                   int length,
;;;                   PangoAnalysis *analysis,
;;;                   PangoLogAttr *attrs,
;;;                   int attrs_len);
;;;
;;; Determines possible line, word, and character breaks for a string of Unicode
;;; text with a single analysis. For most purposes you may want to use
;;; pango_get_log_attrs().
;;;
;;; text :
;;;     the text to process
;;;
;;; length :
;;;     length of text in bytes (may be -1 if text is nul-terminated)
;;;
;;; analysis :
;;;     PangoAnalysis structure from pango_itemize()
;;;
;;; attrs :
;;;     an array to store character information in
;;;
;;; attrs_len :
;;;     size of the array passed as attrs
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_get_log_attrs ()
;;;
;;; void pango_get_log_attrs (const char *text,
;;;                           int length,
;;;                           int level,
;;;                           PangoLanguage *language,
;;;                           PangoLogAttr *log_attrs,
;;;                           int attrs_len);
;;;
;;; Computes a PangoLogAttr for each character in text. The log_attrs array must
;;; have one PangoLogAttr for each position in text; if text contains N
;;; characters, it has N+1 positions, including the last position at the end of
;;; the text. text should be an entire paragraph; logical attributes can't be
;;; computed without context (for example you need to see spaces on either side
;;; of a word to know the word is a word).
;;;
;;; text :
;;;     text to process
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
;;;     array with one PangoLogAttr per character in text, plus one extra, to be
;;;     filled in
;;;
;;; attrs_len :
;;;     length of log_attrs array
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_find_paragraph_boundary ()
;;;
;;; void pango_find_paragraph_boundary (const gchar *text,
;;;                                     gint length,
;;;                                     gint *paragraph_delimiter_index,
;;;                                     gint *next_paragraph_start);
;;;
;;; Locates a paragraph boundary in text. A boundary is caused by delimiter
;;; characters, such as a newline, carriage return, carriage return-newline
;;; pair, or Unicode paragraph separator character. The index of the run of
;;; delimiters is returned in paragraph_delimiter_index. The index of the start
;;; of the paragraph (index after all delimiters) is stored in
;;; next_paragraph_start.
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
;;;     return location for index of delimiter
;;;
;;; next_paragraph_start :
;;;     return location for start of next paragraph
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_default_break ()
;;;
;;; void pango_default_break (const gchar *text,
;;;                           int length,
;;;                           PangoAnalysis *analysis,
;;;                           PangoLogAttr *attrs,
;;;                           int attrs_len);
;;;
;;; This is the default break algorithm, used if no language engine overrides
;;; it. Normally you should use pango_break() instead. Unlike pango_break(),
;;; analysis can be NULL, but only do that if you know what you're doing. If you
;;; need an analysis to pass to pango_break(), you need to pango_itemize(). In
;;; most cases however you should simply use pango_get_log_attrs().
;;;
;;; text :
;;;     text to break
;;;
;;; length :
;;;     length of text in bytes (may be -1 if text is nul-terminated)
;;;
;;; analysis :
;;;     a PangoAnalysis for the text
;;;
;;; attrs :
;;;     logical attributes to fill in
;;;
;;; attrs_len :
;;;     size of the array passed as attrs
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoLogAttr
;;; ----------------------------------------------------------------------------

(defcstruct pango-log-attr
  (is-line-break :uint) ; Can break line in front of character
  (is-mandatory-break :uint) ; Must break line in front of character
  (is-char-break :uint) ;Can break here when doing char wrap
  (is-white :uint) ; Whitespace character
  ;; Cursor can appear in front of character (i.e. this is a grapheme
  ;; boundary, or the first character in the text).
  (is-cursor-position :uint)
  ;; Note that in degenerate cases, you could have both start/end set on
  ;; some text, most likely for sentences (e.g. no space after a period, so
  ;; the next sentence starts right away).
  (is-word-start :uint) ; first character in a word
  (is-word-end :uint) ; is first non-word char after a word
  ;; There are two ways to divide sentences. The first assigns all
  ;; intersentence whitespace/control/format chars to some sentence,
  ;; so all chars are in some sentence; is_sentence_boundary denotes
  ;; the boundaries there. The second way doesn't assign
  ;; between-sentence spaces, etc. to any sentence, so
  ;; is_sentence_start/is_sentence_end mark the boundaries of those
  ;; sentences.
  (is-sentence-boundary :uint)
  (is-sentence-start :uint) ; first character in a sentence
  (is-sentence-end :uint) ; first non-sentence char after a sentence
  ;; If set, backspace deletes one character rather than
  ;; the entire grapheme cluster.
  (backspcaces-deletes-character :uint)
  ;; Only few space variants (U+0020 and U+00A0) have variable
  ;; width during justification.
  (is-expandable-space :uint)
  ;; Word boundary as defined by UAX#29
  (is-word-boundary :uint) ; is NOT in the middle of a word
)

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-log-attr atdoc:*symbol-name-alias*) "CStruct"
      (gethash 'pango-log-attr atdoc:*external-symbols*)
 "@version{2013-8-2}
  @begin{short}
    The @sym{pango-log-attr} structure stores information about the attributes
    of a single character.
  @end{short}
  @begin{pre}
(defcstruct pango-log-attr
  (is-line-break :uint) ; Can break line in front of character
  (is-mandatory-break :uint) ; Must break line in front of character
  (is-char-break :uint) ;Can break here when doing char wrap
  (is-white :uint) ; Whitespace character
  ;; Cursor can appear in front of character (i.e. this is a grapheme
  ;; boundary, or the first character in the text).
  (is-cursor-position :uint)
  ;; Note that in degenerate cases, you could have both start/end set on
  ;; some text, most likely for sentences (e.g. no space after a period, so
  ;; the next sentence starts right away).
  (is-word-start :uint) ; first character in a word
  (is-word-end :uint) ; is first non-word char after a word
  ;; There are two ways to divide sentences. The first assigns all
  ;; intersentence whitespace/control/format chars to some sentence,
  ;; so all chars are in some sentence; is_sentence_boundary denotes
  ;; the boundaries there. The second way doesn't assign
  ;; between-sentence spaces, etc. to any sentence, so
  ;; is_sentence_start/is_sentence_end mark the boundaries of those
  ;; sentences.
  (is-sentence-boundary :uint)
  (is-sentence-start :uint) ; first character in a sentence
  (is-sentence-end :uint) ; first non-sentence char after a sentence
  ;; If set, backspace deletes one character rather than
  ;; the entire grapheme cluster.
  (backspcaces-deletes-character :uint)
  ;; Only few space variants (U+0020 and U+00A0) have variable
  ;; width during justification.
  (is-expandable-space :uint)
  ;; Word boundary as defined by UAX#29
  (is-word-boundary :uint) ; is NOT in the middle of a word
)
  @end{pre}
  @begin[code]{table}
    @entry[is-line-break]{If set, can break line in front of character.}
    @entry[is-mandatory-break]{If set, must break line in front of character.}
    @entry[is-char-break]{If set, can break here when doing character wrapping.}
    @entry[is-white]{Is whitespace character.}
    @entry[is-cursor-position]{If set, cursor can appear in front of character.
      I. e. this is a grapheme boundary, or the first character in the text.
      This flag implements Unicode's Grapheme Cluster Boundaries semantics.}
    @entry[is-word-start]{Is first character in a word.}
    @entry[is-word-end]{Is first non-word char after a word. Note that in
      degenerate cases, you could have both @code{is-word-start} and
      @code{is-word-end} set for some character.}
    @entry[is-sentence-boundary]{Is a sentence boundary. There are two ways to
      divide sentences. The first assigns all inter-sentence
      whitespace/control/format chars to some sentence, so all chars are in some
      sentence; @code{is-sentence-boundary} denotes the boundaries there. The
      second way does not assign between-sentence spaces, etc. to any sentence, so
      @code{is-sentence_start}/@code{is-sentence-end} mark the boundaries of
      those sentences.}
    @entry[is-sentence-start]{Is first character in a sentence.}
    @entry[is-sentence-end]{Is first char after a sentence. Note that in
      degenerate cases, you could have both @code{is-sentence-start} and
      @code{is-sentence-end} set for some character, e. g. no space after a
      period, so the next sentence starts right away.}
    @entry[backspace-deletes-character]{If set, backspace deletes one character
      rather than the entire grapheme cluster. This field is only meaningful on
      grapheme boundaries, where @code{is-cursor-position} is set. In some
      languages, the full grapheme, e. g. letter + diacritics, is considered a
      unit, while in others, each decomposed character in the grapheme is a
      unit. In the default implementation of @fun{pango-break}, this bit is set
      on all grapheme boundaries except those following Latin, Cyrillic or Greek
      base characters.}
    @entry[is-expandable-space]{Is a whitespace character that can possibly be
      expanded for justification purposes. Since 1.18}
    @entry[is-word-boundary]{Is a word boundary. More specifically, means that
      this is not a position in the middle of a word. For example, both sides of
      a punctuation mark are considered word boundaries. This flag is
      particularly useful when selecting text word-by-word. This flag implements
      Unicode's Word Boundaries semantics. Since 1.22}
  @end{table}
  @see-function{pango-break}")

(export 'pango-log-attr)

;;; ----------------------------------------------------------------------------
;;; pango_shape ()
;;;
;;; void pango_shape (const gchar *text,
;;;                   gint length,
;;;                   const PangoAnalysis *analysis,
;;;                   PangoGlyphString *glyphs);
;;;
;;; Given a segment of text and the corresponding PangoAnalysis structure
;;; returned from pango_itemize(), convert the characters into glyphs. You may
;;; also pass in only a substring of the item from pango_itemize().
;;;
;;; It is recommended that you use pango_shape_full() instead, since that API
;;; allows for shaping interaction happening across text item boundaries.
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
;;; void pango_shape_full (const gchar *item_text,
;;;                        gint item_length,
;;;                        const gchar *paragraph_text,
;;;                        gint paragraph_length,
;;;                        const PangoAnalysis *analysis,
;;;                        PangoGlyphString *glyphs);
;;;
;;; Given a segment of text and the corresponding PangoAnalysis structure
;;; returned from pango_itemize(), convert the characters into glyphs. You may
;;; also pass in only a substring of the item from pango_itemize().
;;;
;;; This is similar to pango_shape(), except it also can optionally take the
;;; full paragraph text as input, which will then be used to perform certain
;;; cross-item shaping interactions. If you have access to the broader text of
;;; which item_text is part of, provide the broader text as paragraph_text. If
;;; paragraph_text is NULL, item text is used instead.
;;;
;;; item_text :
;;;     valid UTF-8 text to shape
;;;
;;; item_length :
;;;     the length (in bytes) of item_text. -1 means nul-terminated text
;;;
;;; paragraph_text :
;;;     (allow-none) text of the paragraph (see details). May be NULL
;;;
;;; paragraph_length :
;;;     the length (in bytes) of paragraph_text. -1 means nul-terminated text
;;;
;;; analysis :
;;;     PangoAnalysis structure from pango_itemize()
;;;
;;; glyphs :
;;;     glyph string in which to store results
;;;
;;; Since 1.32
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.context.lisp -----------------------------------------
