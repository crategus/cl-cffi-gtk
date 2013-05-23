;;; ----------------------------------------------------------------------------
;;; pango.layout.lisp
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
;;; Layout Objects
;;;
;;; High-level layout driver objects
;;;
;;; Synopsis
;;;
;;;     PangoLayout
;;;     PangoLayoutIter
;;;
;;;     pango_layout_new
;;;     pango_layout_copy
;;;     pango_layout_get_context
;;;     pango_layout_context_changed
;;;     pango_layout_set_text
;;;     pango_layout_get_text
;;;     pango_layout_get_character_count
;;;     pango_layout_set_markup
;;;     pango_layout_set_markup_with_accel
;;;     pango_layout_set_attributes
;;;     pango_layout_get_attributes
;;;     pango_layout_set_font_description
;;;     pango_layout_get_font_description
;;;     pango_layout_set_width
;;;     pango_layout_get_width
;;;     pango_layout_set_height
;;;     pango_layout_get_height
;;;     pango_layout_set_wrap
;;;     pango_layout_get_wrap
;;;     pango_layout_is_wrapped
;;;
;;;     PangoWrapMode
;;;
;;;     pango_layout_set_ellipsize
;;;     pango_layout_get_ellipsize
;;;     pango_layout_is_ellipsized
;;;
;;;     PangoEllipsizeMode
;;;
;;;     pango_layout_set_indent
;;;     pango_layout_get_indent
;;;     pango_layout_get_spacing
;;;     pango_layout_set_spacing
;;;     pango_layout_set_justify
;;;     pango_layout_get_justify
;;;     pango_layout_set_auto_dir
;;;     pango_layout_get_auto_dir
;;;     pango_layout_set_alignment
;;;     pango_layout_get_alignment
;;;     pango_layout_set_tabs
;;;     pango_layout_get_tabs
;;;     pango_layout_set_single_paragraph_mode
;;;     pango_layout_get_single_paragraph_mode
;;;
;;;     PangoAlignment
;;;
;;;     pango_layout_get_unknown_glyphs_count
;;;     pango_layout_get_log_attrs
;;;     pango_layout_get_log_attrs_readonly
;;;     pango_layout_index_to_pos
;;;     pango_layout_index_to_line_x
;;;     pango_layout_xy_to_index
;;;     pango_layout_get_cursor_pos
;;;     pango_layout_move_cursor_visually
;;;     pango_layout_get_extents
;;;     pango_layout_get_pixel_extents
;;;     pango_layout_get_size
;;;     pango_layout_get_pixel_size
;;;     pango_layout_get_baseline
;;;     pango_layout_get_line_count
;;;     pango_layout_get_line
;;;     pango_layout_get_line_readonly
;;;     pango_layout_get_lines
;;;     pango_layout_get_lines_readonly
;;;     pango_layout_get_iter
;;;     pango_layout_iter_copy
;;;     pango_layout_iter_free
;;;     pango_layout_iter_next_run
;;;     pango_layout_iter_next_char
;;;     pango_layout_iter_next_cluster
;;;     pango_layout_iter_next_line
;;;     pango_layout_iter_at_last_line
;;;     pango_layout_iter_get_index
;;;     pango_layout_iter_get_baseline
;;;     pango_layout_iter_get_run
;;;     pango_layout_iter_get_run_readonly
;;;     pango_layout_iter_get_line
;;;     pango_layout_iter_get_line_readonly
;;;     pango_layout_iter_get_layout
;;;     pango_layout_iter_get_char_extents
;;;     pango_layout_iter_get_cluster_extents
;;;     pango_layout_iter_get_run_extents
;;;     pango_layout_iter_get_line_yrange
;;;     pango_layout_iter_get_line_extents
;;;     pango_layout_iter_get_layout_extents
;;;
;;;     PangoLayoutLine
;;;     PangoLayoutRun
;;;
;;;     pango_layout_line_ref
;;;     pango_layout_line_unref
;;;     pango_layout_line_get_extents
;;;     pango_layout_line_get_pixel_extents
;;;     pango_layout_line_index_to_x
;;;     pango_layout_line_x_to_index
;;;     pango_layout_line_get_x_ranges
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----PangoLayout
;;;
;;;   GBoxed
;;;    +----PangoLayoutIter
;;;
;;;   GEnum
;;;    +----PangoWrapMode
;;;
;;;   GEnum
;;;    +----PangoEllipsizeMode
;;;
;;;   GEnum
;;;    +----PangoAlignment
;;;
;;;   GBoxed
;;;    +----PangoLayoutLine
;;;
;;; Description
;;;
;;; While complete access to the layout capabilities of Pango is provided using
;;; the detailed interfaces for itemization and shaping, using that
;;; functionality directly involves writing a fairly large amount of code. The
;;; objects and functions in this section provide a high-level driver for
;;; formatting entire paragraphs of text at once.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoLayout
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoLayout" pango-layout
  (:type-initializer "pango_layout_get_type")
  ())

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'pango-layout 'type)
 "@version{2013-4-12}
  @begin{short}
    The @sym{pango-layout} structure represents an entire paragraph of text. It
    is initialized with a @class{pango-context}, UTF-8 string and set of
    attributes for that string. Once that is done, the set of formatted lines
    can be extracted from the object, the layout can be rendered, and conversion
    between logical character positions within the layout's text, and the
    physical position of the resulting glyphs can be made.
  @end{short}

  There are also a number of parameters to adjust the formatting of a
  @sym{pango-layout}, which are illustrated in Figure 1, \"Adjustable parameters
  for a PangoLayout\". It is possible, as well, to ignore the 2-D setup, and
  simply treat the results of a @sym{pango-layout} as a list of lines.

  The @sym{pango-layout} structure is opaque, and has no user-visible fields.")

;;; ----------------------------------------------------------------------------
;;; PangoLayoutIter
;;;
;;; typedef struct _PangoLayoutIter PangoLayoutIter;
;;;
;;; A PangoLayoutIter structure can be used to iterate over the visual extents
;;; of a PangoLayout.
;;;
;;; The PangoLayoutIter structure is opaque, and has no user-visible fields.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_new ()
;;;
;;; PangoLayout * pango_layout_new (PangoContext *context);
;;;
;;; Create a new PangoLayout object with attributes initialized to default
;;; values for a particular PangoContext.
;;;
;;; context :
;;;     a PangoContext
;;;
;;; Returns :
;;;     the newly allocated PangoLayout, with a reference count of one, which
;;;     should be freed with g_object_unref().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_copy ()
;;;
;;; PangoLayout * pango_layout_copy (PangoLayout *src);
;;;
;;; Does a deep copy-by-value of the src layout. The attribute list, tab array,
;;; and text from the original layout are all copied by value.
;;;
;;; src :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the newly allocated PangoLayout, with a reference count of one, which
;;;     should be freed with g_object_unref()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_context ()
;;;
;;; PangoContext * pango_layout_get_context (PangoLayout *layout);
;;;
;;; Retrieves the PangoContext used for this layout.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the PangoContext for the layout. This does not have an additional
;;;     refcount added, so if you want to keep a copy of this around, you must
;;;     reference it yourself
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_context_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_context_changed" pango-layout-context-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-4}
  @argument[layout]{a @class{pango-layout} object}
  @begin{short}
    Forces recomputation of any state in the PangoLayout that might depend on
    the layout's context. This function should be called if you make changes to
    the context subsequent to creating the layout.
  @end{short}"
  (layout (g-object pango-layout)))

(export 'pango-layout-context-changed)

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_text ()
;;;
;;; void pango_layout_set_text (PangoLayout *layout,
;;;                             const char *text,
;;;                             int length);
;;;
;;; Sets the text of the layout.
;;;
;;; Note that if you have used pango_layout_set_markup() or
;;; pango_layout_set_markup_with_accel() on layout before, you may want to call
;;; pango_layout_set_attributes() to clear the attributes set on the layout
;;; from the markup as this function does not clear attributes.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; text :
;;;     a valid UTF-8 string
;;;
;;; length :
;;;     maximum length of text, in bytes. -1 indicates that the string is
;;;     nul-terminated and the length should be calculated. The text will also
;;;     be truncated on encountering a nul-termination even when length is
;;;     positive.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_text ()
;;;
;;; const char * pango_layout_get_text (PangoLayout *layout);
;;;
;;; Gets the text in the layout. The returned text should not be freed or
;;; modified.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the text in the layout.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_character_count ()
;;;
;;; gint pango_layout_get_character_count (PangoLayout *layout);
;;;
;;; Returns the number of Unicode characters in the the text of layout.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the number of Unicode characters in the text of layout
;;;
;;; Since 1.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_markup ()
;;;
;;; void pango_layout_set_markup (PangoLayout *layout,
;;;                               const char *markup,
;;;                               int length);
;;;
;;; Same as pango_layout_set_markup_with_accel(), but the markup text isn't
;;; scanned for accelerators.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; markup :
;;;     marked-up text
;;;
;;; length :
;;;     length of marked-up text in bytes, or -1 if markup is null-terminated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_markup_with_accel ()
;;;
;;; void pango_layout_set_markup_with_accel (PangoLayout *layout,
;;;                                          const char *markup,
;;;                                          int length,
;;;                                          gunichar accel_marker,
;;;                                          gunichar *accel_char);
;;;
;;; Sets the layout text and attribute list from marked-up text (see markup
;;; format). Replaces the current text and attribute list.
;;;
;;; If accel_marker is nonzero, the given character will mark the character
;;; following it as an accelerator. For example, accel_marker might be an
;;; ampersand or underscore. All characters marked as an accelerator will
;;; receive a PANGO_UNDERLINE_LOW attribute, and the first character so marked
;;; will be returned in accel_char. Two accel_marker characters following each
;;; other produce a single literal accel_marker character.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; markup :
;;;     marked-up text (see markup format)
;;;
;;; length :
;;;     length of marked-up text in bytes, or -1 if markup is null-terminated
;;;
;;; accel_marker :
;;;     marker for accelerators in the text
;;;
;;; accel_char :
;;;     return location for first located accelerator, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_attributes ()
;;;
;;; void pango_layout_set_attributes (PangoLayout *layout,
;;;                                   PangoAttrList *attrs);
;;;
;;; Sets the text attributes for a layout object. References attrs, so the
;;; caller can unref its reference.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; attrs :
;;;     a PangoAttrList, can be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_attributes ()
;;;
;;; PangoAttrList * pango_layout_get_attributes (PangoLayout *layout);
;;;
;;; Gets the attribute list for the layout, if any.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     a PangoAttrList
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_font_description ()
;;;
;;; void pango_layout_set_font_description (PangoLayout *layout,
;;;                                         const PangoFontDescription *desc);
;;;
;;; Sets the default font description for the layout. If no font description is
;;; set on the layout, the font description from the layout's context is used.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; desc :
;;;     the new PangoFontDescription, or NULL to unset the current font
;;;     description
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_font_description ()
;;;
;;; const PangoFontDescription * pango_layout_get_font_description
;;;                                                        (PangoLayout *layout)
;;;
;;; Gets the font description for the layout, if any.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     a pointer to the layout's font description, or NULL if the font
;;;     description from the layout's context is inherited. This value is owned
;;;     by the layout and must not be modified or freed.
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_width ()
;;;
;;; void pango_layout_set_width (PangoLayout *layout, int width);
;;;
;;; Sets the width to which the lines of the PangoLayout should wrap or
;;; ellipsized. The default value is -1: no width set.
;;;
;;; layout :
;;;     a PangoLayout.
;;;
;;; width :
;;;     the desired width in Pango units, or -1 to indicate that no wrapping or
;;;     ellipsization should be performed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_width ()
;;;
;;; int pango_layout_get_width (PangoLayout *layout);
;;;
;;; Gets the width to which the lines of the PangoLayout should wrap.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the width in Pango units, or -1 if no width set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_height ()
;;;
;;; void pango_layout_set_height (PangoLayout *layout, int height);
;;;
;;; Sets the height to which the PangoLayout should be ellipsized at. There are
;;; two different behaviors, based on whether height is positive or negative.
;;;
;;; If height is positive, it will be the maximum height of the layout. Only
;;; lines would be shown that would fit, and if there is any text omitted, an
;;; ellipsis added. At least one line is included in each paragraph regardless
;;; of how small the height value is. A value of zero will render exactly one
;;; line for the entire layout.
;;;
;;; If height is negative, it will be the (negative of) maximum number of lines
;;; per paragraph. That is, the total number of lines shown may well be more
;;; than this value if the layout contains multiple paragraphs of text. The
;;; default value of -1 means that first line of each paragraph is ellipsized.
;;; This behvaior may be changed in the future to act per layout instead of per
;;; paragraph. File a bug against pango at http://bugzilla.gnome.org/ if your
;;; code relies on this behavior.
;;;
;;; Height setting only has effect if a positive width is set on layout and
;;; ellipsization mode of layout is not PANGO_ELLIPSIZE_NONE. The behavior is
;;; undefined if a height other than -1 is set and ellipsization mode is set to
;;; PANGO_ELLIPSIZE_NONE, and may change in the future.
;;;
;;; layout :
;;;     a PangoLayout.
;;;
;;; height :
;;;     the desired height of the layout in Pango units if positive, or desired
;;;     number of lines if negative.
;;;
;;; Since 1.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_height ()
;;;
;;; int pango_layout_get_height (PangoLayout *layout);
;;;
;;; Gets the height of layout used for ellipsization.
;;; See pango_layout_set_height() for details.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the height, in Pango units if positive, or number of lines if negative.
;;;
;;; Since 1.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_wrap ()
;;;
;;; void pango_layout_set_wrap (PangoLayout *layout, PangoWrapMode wrap);
;;;
;;; Sets the wrap mode; the wrap mode only has effect if a width is set on the
;;; layout with pango_layout_set_width(). To turn off wrapping, set the width
;;; to -1.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; wrap :
;;;     the wrap mode
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_wrap ()
;;;
;;; PangoWrapMode pango_layout_get_wrap (PangoLayout *layout);
;;;
;;; Gets the wrap mode for the layout.
;;;
;;; Use pango_layout_is_wrapped() to query whether any paragraphs were actually
;;; wrapped.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     active wrap mode
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_is_wrapped ()
;;;
;;; gboolean pango_layout_is_wrapped (PangoLayout *layout);
;;;
;;; Queries whether the layout had to wrap any paragraphs.
;;;
;;; This returns TRUE if a positive width is set on layout, ellipsization mode
;;; of layout is set to PANGO_ELLIPSIZE_NONE, and there are paragraphs exceeding
;;; the layout width that have to be wrapped.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     TRUE if any paragraphs had to be wrapped, FALSE otherwise.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum PangoWrapMode
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoWrapMode" pango-wrap-mode
  (:export t
   :type-initializer "pango_wrap_mode_get_type")
  (:word 0)
  (:char 1)
  (:word-char 2))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-wrap-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'pango-wrap-mode atdoc:*external-symbols*)
 "@version{2013-4-12}
  @begin{short}
    A @sym{pango-wrap-mode} describes how to wrap the lines of a
    @class{pango-layout} to the desired width.
  @end{short}
  @begin{pre}
(define-g-enum \"PangoWrapMode\" pango-wrap-mode
  (:export t
   :type-initializer \"pango_wrap_mode_get_type\")
  (:word 0)
  (:char 1)
  (:word-char 2))
  @end{pre}
  @begin[code]{table}
    @entry[:word]{Wrap lines at word boundaries.}
    @entry[:char]{Wrap lines at character boundaries.}
    @entry[:word-char]{Wrap lines at word boundaries, but fall back to
      character boundaries if there is not enough space for a full word.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_ellipsize ()
;;;
;;; void pango_layout_set_ellipsize (PangoLayout *layout,
;;;                                  PangoEllipsizeMode ellipsize);
;;;
;;; Sets the type of ellipsization being performed for layout. Depending on the
;;; ellipsization mode ellipsize text is removed from the start, middle, or end
;;; of text so they fit within the width and height of layout set with
;;; pango_layout_set_width() and pango_layout_set_height().
;;;
;;; If the layout contains characters such as newlines that force it to be
;;; layed out in multiple paragraphs, then whether each paragraph is ellipsized
;;; separately or the entire layout is ellipsized as a whole depends on the set
;;; height of the layout. See pango_layout_set_height() for details.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; ellipsize :
;;;     the new ellipsization mode for layout
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_ellipsize ()
;;;
;;; PangoEllipsizeMode pango_layout_get_ellipsize (PangoLayout *layout);
;;;
;;; Gets the type of ellipsization being performed for layout.
;;; See pango_layout_set_ellipsize()
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the current ellipsization mode for layout. Use
;;;     pango_layout_is_ellipsized() to query whether any paragraphs were
;;;     actually ellipsized.
;;;
;;; Since 1.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_is_ellipsized ()
;;;
;;; gboolean pango_layout_is_ellipsized (PangoLayout *layout);
;;;
;;; Queries whether the layout had to ellipsize any paragraphs.
;;;
;;; This returns TRUE if the ellipsization mode for layout is not
;;; PANGO_ELLIPSIZE_NONE, a positive width is set on layout, and there are
;;; paragraphs exceeding that width that have to be ellipsized.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     TRUE if any paragraphs had to be ellipsized, FALSE otherwise.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum PangoEllipsizeMode
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoEllipsizeMode" pango-ellipsize-mode
  (:export t
   :type-initializer "pango_ellipsize_mode_get_type")
  (:none 0)
  (:start 1)
  (:middle 2)
  (:end 3))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-ellipsize-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'pango-ellipsize-mode atdoc:*external-symbols*)
 "@version{2013-4-12}
  @begin{short}
    The @sym{pango-ellipsize-mode} enumeration describes what sort of (if any)
    ellipsization should be applied to a line of text. In the ellipsization
    process characters are removed from the text in order to make it fit to a
    given width and replaced with an ellipsis.
  @end{short}
  @begin{pre}
(define-g-enum \"PangoEllipsizeMode\" pango-ellipsize-mode
  (:export t
   :type-initializer \"pango_ellipsize_mode_get_type\")
  (:none 0)
  (:start 1)
  (:middle 2)
  (:end 3))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No ellipsization.}
    @entry[:start]{Omit characters at the start of the text.}
    @entry[:middle]{Omit characters in the middle of the text.}
    @entry[:end]{Omit characters at the end of the text.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_indent ()
;;;
;;; void pango_layout_set_indent (PangoLayout *layout, int indent);
;;;
;;; Sets the width in Pango units to indent each paragraph. A negative value of
;;; indent will produce a hanging indentation. That is, the first line will have
;;; the full width, and subsequent lines will be indented by the absolute value
;;; of indent.
;;;
;;; The indent setting is ignored if layout alignment is set to
;;; PANGO_ALIGN_CENTER.
;;;
;;; layout :
;;;     a PangoLayout.
;;;
;;; indent :
;;;     the amount by which to indent
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_indent ()
;;;
;;; int pango_layout_get_indent (PangoLayout *layout);
;;;
;;; Gets the paragraph indent width in Pango units. A negative value indicates
;;; a hanging indentation.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the indent in Pango units
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_spacing ()
;;;
;;; int pango_layout_get_spacing (PangoLayout *layout);
;;;
;;; Gets the amount of spacing between the lines of the layout.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the spacing in Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_spacing ()
;;;
;;; void pango_layout_set_spacing (PangoLayout *layout, int spacing);
;;;
;;; Sets the amount of spacing in Pango unit between the lines of the layout.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; spacing :
;;;     the amount of spacing
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_justify ()
;;;
;;; void pango_layout_set_justify (PangoLayout *layout, gboolean justify);
;;;
;;; Sets whether each complete line should be stretched to fill the entire
;;; width of the layout. This stretching is typically done by adding whitespace,
;;; but for some scripts (such as Arabic), the justification may be done in more
;;; complex ways, like extending the characters.
;;;
;;; Note that this setting is not implemented and so is ignored in Pango older
;;; than 1.18.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; justify :
;;;     whether the lines in the layout should be justified.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_justify ()
;;;
;;; gboolean pango_layout_get_justify (PangoLayout *layout);
;;;
;;; Gets whether each complete line should be stretched to fill the entire
;;; width of the layout.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the justify
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_auto_dir ()
;;;
;;; void pango_layout_set_auto_dir (PangoLayout *layout, gboolean auto_dir);
;;;
;;; Sets whether to calculate the bidirectional base direction for the layout
;;; according to the contents of the layout; when this flag is on (the default),
;;; then paragraphs in layout that begin with strong right-to-left characters
;;; (Arabic and Hebrew principally), will have right-to-left layout, paragraphs
;;; with letters from other scripts will have left-to-right layout. Paragraphs
;;; with only neutral characters get their direction from the surrounding
;;; paragraphs.
;;;
;;; When FALSE, the choice between left-to-right and right-to-left layout is
;;; done according to the base direction of the layout's PangoContext. (See
;;; pango_context_set_base_dir()).
;;;
;;; When the auto-computed direction of a paragraph differs from the base
;;; direction of the context, the interpretation of PANGO_ALIGN_LEFT and
;;; PANGO_ALIGN_RIGHT are swapped.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; auto_dir :
;;;     if TRUE, compute the bidirectional base direction from the layout's
;;;     contents.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_auto_dir ()
;;;
;;; gboolean pango_layout_get_auto_dir (PangoLayout *layout);
;;;
;;; Gets whether to calculate the bidirectional base direction for the layout
;;; according to the contents of the layout. See pango_layout_set_auto_dir().
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     TRUE if the bidirectional base direction is computed from the layout's
;;;     contents, FALSE otherwise.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_alignment ()
;;;
;;; void pango_layout_set_alignment (PangoLayout *layout,
;;;                                  PangoAlignment alignment);
;;;
;;; Sets the alignment for the layout: how partial lines are positioned within
;;; the horizontal space available.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; alignment :
;;;     the alignment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_alignment ()
;;;
;;; PangoAlignment pango_layout_get_alignment (PangoLayout *layout);
;;;
;;; Gets the alignment for the layout: how partial lines are positioned within
;;; the horizontal space available.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the alignment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_tabs ()
;;;
;;; void pango_layout_set_tabs (PangoLayout *layout,
;;;                             PangoTabArray *tabs);
;;;
;;; Sets the tabs to use for layout, overriding the default tabs (by default,
;;; tabs are every 8 spaces). If tabs is NULL, the default tabs are reinstated.
;;; tabs is copied into the layout; you must free your copy of tabs yourself.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; tabs :
;;;     a PangoTabArray, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_tabs ()
;;;
;;; PangoTabArray * pango_layout_get_tabs (PangoLayout *layout);
;;;
;;; Gets the current PangoTabArray used by this layout. If no PangoTabArray has
;;; been set, then the default tabs are in use and NULL is returned. Default
;;; tabs are every 8 spaces. The return value should be freed with
;;; pango_tab_array_free().
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     a copy of the tabs for this layout, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_single_paragraph_mode ()
;;;
;;; void pango_layout_set_single_paragraph_mode (PangoLayout *layout,
;;;                                              gboolean setting);
;;;
;;; If setting is TRUE, do not treat newlines and similar characters as
;;; paragraph separators; instead, keep all text in a single paragraph, and
;;; display a glyph for paragraph separator characters. Used when you want to
;;; allow editing of newlines on a single text line.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; setting :
;;;     new setting
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_single_paragraph_mode ()
;;;
;;; gboolean pango_layout_get_single_paragraph_mode (PangoLayout *layout);
;;;
;;; Obtains the value set by pango_layout_set_single_paragraph_mode().
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     TRUE if the layout does not break paragraphs at paragraph separator
;;;     characters, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum PangoAlignment
;;;
;;; typedef enum {
;;;   PANGO_ALIGN_LEFT,
;;;   PANGO_ALIGN_CENTER,
;;;   PANGO_ALIGN_RIGHT
;;; } PangoAlignment;
;;;
;;; A PangoAlignment describes how to align the lines of a PangoLayout within
;;; the available space. If the PangoLayout is set to justify using
;;; pango_layout_set_justify(), this only has effect for partial lines.
;;;
;;; PANGO_ALIGN_LEFT
;;;     Put all available space on the right
;;;
;;; PANGO_ALIGN_CENTER
;;;     Center the line within the available space
;;;
;;; PANGO_ALIGN_RIGHT
;;;     Put all available space on the left
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_unknown_glyphs_count ()
;;;
;;; int pango_layout_get_unknown_glyphs_count (PangoLayout *layout);
;;;
;;; Counts the number unknown glyphs in layout. That is, zero if glyphs for all
;;; characters in the layout text were found, or more than zero otherwise.
;;;
;;; This function can be used to determine if there are any fonts available to
;;; render all characters in a certain string, or when used in combination with
;;; PANGO_ATTR_FALLBACK, to check if a certain font supports all the characters
;;; in the string.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     The number of unknown glyphs in layout.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_log_attrs ()
;;;
;;; void pango_layout_get_log_attrs (PangoLayout *layout,
;;;                                  PangoLogAttr **attrs,
;;;                                  gint *n_attrs);
;;;
;;; Retrieves an array of logical attributes for each character in the layout.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; attrs :
;;;     Location to store a pointer to an array of logical attributes.
;;;     This value must be freed with g_free().
;;;
;;; n_attrs :
;;;     location to store the number of the attributes in the array. (The
;;;     stored value will be one more than the total number of characters in
;;;     the layout, since there need to be attributes corresponding to both the
;;;     position before the first character and the position after the last
;;;     character.)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_log_attrs_readonly ()
;;;
;;; const PangoLogAttr * pango_layout_get_log_attrs_readonly
;;;                                                        (PangoLayout *layout,
;;;                                                         gint *n_attrs);
;;;
;;; Retrieves an array of logical attributes for each character in the layout.
;;;
;;; This is a faster alternative to pango_layout_get_log_attrs(). The returned
;;; array is part of layout and must not be modified. Modifying the layout will
;;; invalidate the returned array.
;;;
;;; The number of attributes returned in n_attrs will be one more than the total
;;; number of characters in the layout, since there need to be attributes
;;; corresponding to both the position before the first character and the
;;; position after the last character.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; n_attrs :
;;;     location to store the number of the attributes in the array
;;;
;;; Returns :
;;;     an array of logical attributes
;;;
;;; Since 1.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_index_to_pos ()
;;;
;;; void pango_layout_index_to_pos (PangoLayout *layout,
;;;                                 int index_,
;;;                                 PangoRectangle *pos);
;;;
;;; Converts from an index within a PangoLayout to the onscreen position
;;; corresponding to the grapheme at that index, which is represented as
;;; rectangle. Note that pos->x is always the leading edge of the grapheme and
;;; pos->x + pos->width the trailing edge of the grapheme. If the directionality
;;; of the grapheme is right-to-left, then pos->width will be negative.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; index_ :
;;;     byte index within layout
;;;
;;; pos :
;;;     rectangle in which to store the position of the grapheme
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_index_to_line_x ()
;;;
;;; void pango_layout_index_to_line_x (PangoLayout *layout,
;;;                                    int index_,
;;;                                    gboolean trailing,
;;;                                    int *line,
;;;                                    int *x_pos);
;;;
;;; Converts from byte index_ within the layout to line and X position. (X
;;; position is measured from the left edge of the line)
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; index_ :
;;;     the byte index of a grapheme within the layout.
;;;
;;; trailing :
;;;     an integer indicating the edge of the grapheme to retrieve the position
;;;     of. If 0, the trailing edge of the grapheme, if > 0, the leading of the
;;;     grapheme.
;;;
;;; line :
;;;     location to store resulting line index. (which will between 0 and
;;;     pango_layout_get_line_count(layout) - 1), or NULL
;;;
;;; x_pos :
;;;     location to store resulting position within line (PANGO_SCALE units per
;;;     device unit), or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_xy_to_index ()
;;;
;;; gboolean pango_layout_xy_to_index (PangoLayout *layout,
;;;                                    int x,
;;;                                    int y,
;;;                                    int *index_,
;;;                                    int *trailing);
;;;
;;; Converts from X and Y position within a layout to the byte index to the
;;; character at that logical position. If the Y position is not inside the
;;; layout, the closest position is chosen (the position will be clamped inside
;;; the layout). If the X position is not within the layout, then the start or
;;; the end of the line is chosen as described for pango_layout_x_to_index().
;;; If either the X or Y positions were not inside the layout, then the function
;;; returns FALSE; on an exact hit, it returns TRUE.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; x :
;;;     the X offset (in Pango units) from the left edge of the layout.
;;;
;;; y :
;;;     the Y offset (in Pango units) from the top edge of the layout
;;;
;;; index_ :
;;;     location to store calculated byte index
;;;
;;; trailing :
;;;     location to store a integer indicating where in the grapheme the user
;;;     clicked. It will either be zero, or the number of characters in the
;;;     grapheme. 0 represents the trailing edge of the grapheme
;;;
;;; Returns :
;;;     TRUE if the coordinates were inside text, FALSE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_cursor_pos ()
;;;
;;; void pango_layout_get_cursor_pos (PangoLayout *layout,
;;;                                   int index_,
;;;                                   PangoRectangle *strong_pos,
;;;                                   PangoRectangle *weak_pos);
;;;
;;; Given an index within a layout, determines the positions that of the strong
;;; and weak cursors if the insertion point is at that index. The position of
;;; each cursor is stored as a zero-width rectangle. The strong cursor location
;;; is the location where characters of the directionality equal to the base
;;; direction of the layout are inserted. The weak cursor location is the
;;; location where characters of the directionality opposite to the base
;;; direction of the layout are inserted.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; index_ :
;;;     the byte index of the cursor
;;;
;;; strong_pos :
;;;     location to store the strong cursor position (may be NULL)
;;;
;;; weak_pos :
;;;     location to store the weak cursor position (may be NULL)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_move_cursor_visually ()
;;;
;;; void pango_layout_move_cursor_visually (PangoLayout *layout,
;;;                                         gboolean strong,
;;;                                         int old_index,
;;;                                         int old_trailing,
;;;                                         int direction,
;;;                                         int *new_index,
;;;                                         int *new_trailing);
;;;
;;; Computes a new cursor position from an old position and a count of positions
;;; to move visually. If direction is positive, then the new strong cursor
;;; position will be one position to the right of the old cursor position. If
;;; direction is negative, then the new strong cursor position will be one
;;; position to the left of the old cursor position.
;;;
;;; In the presence of bidirectional text, the correspondence between logical
;;; and visual order will depend on the direction of the current run, and there
;;; may be jumps when the cursor is moved off of the end of a run.
;;;
;;; Motion here is in cursor positions, not in characters, so a single call to
;;; pango_layout_move_cursor_visually() may move the cursor over multiple
;;; characters when multiple characters combine to form a single grapheme.
;;;
;;; layout :
;;;     a PangoLayout.
;;;
;;; strong :
;;;     whether the moving cursor is the strong cursor or the weak cursor. The
;;;     strong cursor is the cursor corresponding to text insertion in the base
;;;     direction for the layout.
;;;
;;; old_index :
;;;     the byte index of the grapheme for the old index
;;;
;;; old_trailing :
;;;     if 0, the cursor was at the trailing edge of the grapheme indicated by
;;;     old_index, if > 0, the cursor was at the leading edge.
;;;
;;; direction :
;;;     direction to move cursor. A negative value indicates motion to the left.
;;;
;;; new_index :
;;;     location to store the new cursor byte index. A value of -1 indicates
;;;     that the cursor has been moved off the beginning of the layout. A value
;;;     of G_MAXINT indicates that the cursor has been moved off the end of the
;;;     layout
;;;
;;; new_trailing :
;;;     number of characters to move forward from the location returned for
;;;     new_index to get the position where the cursor should be displayed.
;;;     This allows distinguishing the position at the beginning of one line
;;;     from the position at the end of the preceding line. new_index is always
;;;     on the line where the cursor should be displayed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_extents ()
;;;
;;; void pango_layout_get_extents (PangoLayout *layout,
;;;                                PangoRectangle *ink_rect,
;;;                                PangoRectangle *logical_rect);
;;;
;;; Computes the logical and ink extents of layout. Logical extents are usually
;;; what you want for positioning things. Note that both extents may have
;;; non-zero x and y. You may want to use those to offset where you render the
;;; layout. Not doing that is a very typical bug that shows up as right-to-left
;;; layouts not being correctly positioned in a layout with a set width.
;;;
;;; The extents are given in layout coordinates and in Pango units; layout
;;; coordinates begin at the top left corner of the layout.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; ink_rect :
;;;     rectangle used to store the extents of the layout as drawn or NULL to
;;;     indicate that the result is not needed
;;;
;;; logical_rect :
;;;     rectangle used to store the logical extents of the layout or NULL to
;;;     indicate that the result is not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_pixel_extents ()
;;;
;;; void pango_layout_get_pixel_extents (PangoLayout *layout,
;;;                                      PangoRectangle *ink_rect,
;;;                                      PangoRectangle *logical_rect);
;;;
;;; Computes the logical and ink extents of layout in device units. This
;;; function just calls pango_layout_get_extents() followed by two
;;; pango_extents_to_pixels() calls, rounding ink_rect and logical_rect such
;;; that the rounded rectangles fully contain the unrounded one (that is, passes
;;; them as first argument to pango_extents_to_pixels()).
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; ink_rect :
;;;     rectangle used to store the extents of the layout as drawn or NULL to
;;;     indicate that the result is not needed
;;;
;;; logical_rect :
;;;     rectangle used to store the logical extents of the layout or NULL to
;;;     indicate that the result is not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_size ()
;;;
;;; void pango_layout_get_size (PangoLayout *layout, int *width, int *height);
;;;
;;; Determines the logical width and height of a PangoLayout in Pango units
;;; (device units scaled by PANGO_SCALE). This is simply a convenience function
;;; around pango_layout_get_extents().
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; width :
;;;     location to store the logical width, or NULL
;;;
;;; height :
;;;     location to store the logical height, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_pixel_size ()
;;;
;;; void pango_layout_get_pixel_size (PangoLayout *layout,
;;;                                   int *width,
;;;                                   int *height);
;;;
;;; Determines the logical width and height of a PangoLayout in device units.
;;; (pango_layout_get_size() returns the width and height scaled by
;;; PANGO_SCALE.) This is simply a convenience function around
;;; pango_layout_get_pixel_extents().
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; width :
;;;     location to store the logical width, or NULL
;;;
;;; height :
;;;     location to store the logical height, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_baseline ()
;;;
;;; int pango_layout_get_baseline (PangoLayout *layout);
;;;
;;; Gets the Y position of baseline of the first line in layout.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     baseline of first line, from top of layout.
;;;
;;; Since 1.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line_count ()
;;;
;;; int pango_layout_get_line_count (PangoLayout *layout);
;;;
;;; Retrieves the count of lines for the layout.
;;;
;;; layout :
;;;     PangoLayout
;;;
;;; Returns :
;;;     the line count
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line ()
;;;
;;; PangoLayoutLine * pango_layout_get_line (PangoLayout *layout, int line);
;;;
;;; Retrieves a particular line from a PangoLayout.
;;;
;;; Use the faster pango_layout_get_line_readonly() if you do not plan to modify
;;; the contents of the line (glyphs, glyph widths, etc.).
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; line :
;;;     the index of a line, which must be between 0 and
;;;     pango_layout_get_line_count(layout) - 1, inclusive.
;;;
;;; Returns :
;;;     the requested PangoLayoutLine, or NULL if the index is out of range.
;;;     This layout line can be ref'ed and retained, but will become invalid
;;;     if changes are made to the PangoLayout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line_readonly ()
;;;
;;; PangoLayoutLine * pango_layout_get_line_readonly (PangoLayout *layout,
;;;                                                   int line);
;;;
;;; Retrieves a particular line from a PangoLayout.
;;;
;;; This is a faster alternative to pango_layout_get_line(), but the user is not
;;; expected to modify the contents of the line (glyphs, glyph widths, etc.).
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; line :
;;;     the index of a line, which must be between 0 and
;;;     pango_layout_get_line_count(layout) - 1, inclusive.
;;;
;;; Returns :
;;;     the requested PangoLayoutLine, or NULL if the index is out of range.
;;;     This layout line can be ref'ed and retained, but will become invalid if
;;;     changes are made to the PangoLayout. No changes should be made to the
;;;     line
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_lines ()
;;;
;;; GSList * pango_layout_get_lines (PangoLayout *layout);
;;;
;;; Returns the lines of the layout as a list.
;;;
;;; Use the faster pango_layout_get_lines_readonly() if you do not plan to
;;; modify the contents of the lines (glyphs, glyph widths, etc.).
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     a GSList containing the lines in the layout. This points to internal
;;;     data of the PangoLayout and must be used with care. It will become
;;;     invalid on any change to the layout's text or properties
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_lines_readonly ()
;;;
;;; GSList * pango_layout_get_lines_readonly (PangoLayout *layout);
;;;
;;; Returns the lines of the layout as a list.
;;;
;;; This is a faster alternative to pango_layout_get_lines(), but the user is
;;; not expected to modify the contents of the lines (glyphs, glyph widths,
;;; etc.).
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     a GSList containing the lines in the layout. This points to internal
;;;     data of the PangoLayout and must be used with care. It will become
;;;     invalid on any change to the layout's text or properties. No changes
;;;     should be made to the lines
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_iter ()
;;;
;;; PangoLayoutIter * pango_layout_get_iter (PangoLayout *layout);
;;;
;;; Returns an iterator to iterate over the visual extents of the layout.
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; Returns :
;;;     the new PangoLayoutIter that should be freed using
;;;     pango_layout_iter_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_copy ()
;;;
;;; PangoLayoutIter * pango_layout_iter_copy (PangoLayoutIter *iter);
;;;
;;; Copies a PangoLayoutIter.
;;;
;;; iter :
;;;     a PangoLayoutIter, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoLayoutIter, which should be freed with
;;;     pango_layout_iter_free(), or NULL if iter was NULL.
;;;
;;; Since 1.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_free ()
;;;
;;; void pango_layout_iter_free (PangoLayoutIter *iter);
;;;
;;; Frees an iterator that's no longer in use.
;;;
;;; iter :
;;;     a PangoLayoutIter, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_run ()
;;;
;;; gboolean pango_layout_iter_next_run (PangoLayoutIter *iter);
;;;
;;; Moves iter forward to the next run in visual order. If iter was already at
;;; the end of the layout, returns FALSE.
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     whether motion was possible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_char ()
;;;
;;; gboolean pango_layout_iter_next_char (PangoLayoutIter *iter);
;;;
;;; Moves iter forward to the next character in visual order. If iter was
;;; already at the end of the layout, returns FALSE.
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     whether motion was possible
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_cluster ()
;;;
;;; gboolean pango_layout_iter_next_cluster (PangoLayoutIter *iter);
;;;
;;; Moves iter forward to the next cluster in visual order. If iter was already
;;; at the end of the layout, returns FALSE.
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     whether motion was possible
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_line ()
;;;
;;; gboolean pango_layout_iter_next_line (PangoLayoutIter *iter);
;;;
;;; Moves iter forward to the start of the next line. If iter is already on the
;;; last line, returns FALSE.
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     whether motion was possible
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_at_last_line ()
;;;
;;; gboolean pango_layout_iter_at_last_line (PangoLayoutIter *iter);
;;;
;;; Determines whether iter is on the last line of the layout.
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     TRUE if iter is on the last line
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_index ()
;;;
;;; int pango_layout_iter_get_index (PangoLayoutIter *iter);
;;;
;;; Gets the current byte index. Note that iterating forward by char moves in
;;; visual order, not logical order, so indexes may not be sequential. Also,
;;; the index may be equal to the length of the text in the layout, if on the
;;; NULL run (see pango_layout_iter_get_run()).
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     current byte index.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_baseline ()
;;;
;;; int pango_layout_iter_get_baseline (PangoLayoutIter *iter);
;;;
;;; Gets the Y position of the current line's baseline, in layout coordinates
;;; (origin at top left of the entire layout).
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     baseline of current line
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_run ()
;;;
;;; PangoLayoutRun * pango_layout_iter_get_run (PangoLayoutIter *iter);
;;;
;;; Gets the current run. When iterating by run, at the end of each line,
;;; there's a position with a NULL run, so this function can return NULL. The
;;; NULL run at the end of each line ensures that all lines have at least one
;;; run, even lines consisting of only a newline.
;;;
;;; Use the faster pango_layout_iter_get_run_readonly() if you do not plan to
;;; modify the contents of the run (glyphs, glyph widths, etc.).
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     the current run
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_run_readonly ()
;;;
;;; PangoLayoutRun * pango_layout_iter_get_run_readonly (PangoLayoutIter *iter);
;;;
;;; Gets the current run. When iterating by run, at the end of each line,
;;; there's a position with a NULL run, so this function can return NULL. The
;;; NULL run at the end of each line ensures that all lines have at least one
;;; run, even lines consisting of only a newline.
;;;
;;; This is a faster alternative to pango_layout_iter_get_run(), but the user
;;; is not expected to modify the contents of the run (glyphs, glyph widths,
;;; etc.).
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     the current run, that should not be modified
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line ()
;;;
;;; PangoLayoutLine * pango_layout_iter_get_line (PangoLayoutIter *iter);
;;;
;;; Gets the current line.
;;;
;;; Use the faster pango_layout_iter_get_line_readonly() if you do not plan to
;;; modify the contents of the line (glyphs, glyph widths, etc.).
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     the current line
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line_readonly ()
;;;
;;; PangoLayoutLine * pango_layout_iter_get_line_readonly
;;;                                                      (PangoLayoutIter *iter)
;;;
;;; Gets the current line for read-only access.
;;;
;;; This is a faster alternative to pango_layout_iter_get_line(), but the user
;;; is not expected to modify the contents of the line (glyphs, glyph widths,
;;; etc.).
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     the current line, that should not be modified.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_layout ()
;;;
;;; PangoLayout * pango_layout_iter_get_layout (PangoLayoutIter *iter);
;;;
;;; Gets the layout associated with a PangoLayoutIter.
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; Returns :
;;;     the layout associated with iter
;;;
;;; Since 1.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_char_extents ()
;;;
;;; void pango_layout_iter_get_char_extents (PangoLayoutIter *iter,
;;;                                          PangoRectangle *logical_rect);
;;;
;;; Gets the extents of the current character, in layout coordinates (origin is
;;; the top left of the entire layout). Only logical extents can sensibly be
;;; obtained for characters; ink extents make sense only down to the level of
;;; clusters.
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; logical_rect :
;;;     rectangle to fill with logical extents
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_cluster_extents ()
;;;
;;; void pango_layout_iter_get_cluster_extents (PangoLayoutIter *iter,
;;;                                             PangoRectangle *ink_rect,
;;;                                             PangoRectangle *logical_rect);
;;;
;;; Gets the extents of the current cluster, in layout coordinates (origin is
;;; the top left of the entire layout).
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; ink_rect :
;;;     rectangle to fill with ink extents, or NULL
;;;
;;; logical_rect :
;;;     rectangle to fill with logical extents, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_run_extents ()
;;;
;;; void pango_layout_iter_get_run_extents (PangoLayoutIter *iter,
;;;                                         PangoRectangle *ink_rect,
;;;                                         PangoRectangle *logical_rect);
;;;
;;; Gets the extents of the current run in layout coordinates (origin is the
;;; top left of the entire layout).
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; ink_rect :
;;;     rectangle to fill with ink extents, or NULL
;;;
;;; logical_rect :
;;;     rectangle to fill with logical extents, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line_yrange ()
;;;
;;; void pango_layout_iter_get_line_yrange (PangoLayoutIter *iter,
;;;                                         int *y0_,
;;;                                         int *y1_);
;;;
;;; Divides the vertical space in the PangoLayout being iterated over between
;;; the lines in the layout, and returns the space belonging to the current
;;; line. A line's range includes the line's logical extents, plus half of the
;;; spacing above and below the line, if pango_layout_set_spacing() has been
;;; called to set layout spacing. The Y positions are in layout coordinates
;;; (origin at top left of the entire layout).
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; y0_ :
;;;     start of line, or NULL
;;;
;;; y1_ :
;;;     end of line, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line_extents ()
;;;
;;; void pango_layout_iter_get_line_extents (PangoLayoutIter *iter,
;;;                                          PangoRectangle *ink_rect,
;;;                                          PangoRectangle *logical_rect);
;;;
;;; Obtains the extents of the current line. ink_rect or logical_rect can be
;;; NULL if you aren't interested in them. Extents are in layout coordinates
;;; (origin is the top-left corner of the entire PangoLayout). Thus the extents
;;; returned by this function will be the same width/height but not at the same
;;; x/y as the extents returned from pango_layout_line_get_extents().
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; ink_rect :
;;;     rectangle to fill with ink extents, or NULL
;;;
;;; logical_rect :
;;;     rectangle to fill with logical extents, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_layout_extents ()
;;;
;;; void pango_layout_iter_get_layout_extents (PangoLayoutIter *iter,
;;;                                            PangoRectangle *ink_rect,
;;;                                            PangoRectangle *logical_rect);
;;;
;;; Obtains the extents of the PangoLayout being iterated over. ink_rect or
;;; logical_rect can be NULL if you aren't interested in them.
;;;
;;; iter :
;;;     a PangoLayoutIter
;;;
;;; ink_rect :
;;;     rectangle to fill with ink extents, or NULL
;;;
;;; logical_rect :
;;;     rectangle to fill with logical extents, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoLayoutLine
;;;
;;; struct PangoLayoutLine {
;;;   PangoLayout *layout;
;;;   /* start of line as byte index into layout->text */
;;;   gint         start_index;
;;;   /* length of line in bytes */
;;;   gint         length;
;;;   GSList      *runs;
;;;   /* TRUE if this is the first line of the paragraph */
;;;   guint        is_paragraph_start : 1;
;;;   /* Resolved PangoDirection of line */
;;;   guint        resolved_dir : 3;
;;; };
;;;
;;; The PangoLayoutLine structure represents one of the lines resulting from
;;; laying out a paragraph via PangoLayout. PangoLayoutLine structures are
;;; obtained by calling pango_layout_get_line() and are only valid until the
;;; text, attributes, or settings of the parent PangoLayout are modified.
;;;
;;; Routines for rendering PangoLayout objects are provided in code specific
;;; to each rendering system.
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque pango-layout-line "PangoLayoutLine"
  :alloc (error "Use Pango to create PANGO-LAYOUT-LINEs"))

(export (boxed-related-symbols 'pango-layout-line))

;;; ----------------------------------------------------------------------------
;;; PangoLayoutRun
;;;
;;; typedef PangoGlyphItem PangoLayoutRun;
;;;
;;; The PangoLayoutRun structure represents a single run within a
;;; PangoLayoutLine; it is simply an alternate name for PangoGlyphItem. See the
;;; PangoGlyphItem docs for details on the fields.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_ref ()
;;;
;;; PangoLayoutLine * pango_layout_line_ref (PangoLayoutLine *line);
;;;
;;; Increase the reference count of a PangoLayoutLine by one.
;;;
;;; line :
;;;     a PangoLayoutLine, may be NULL
;;;
;;; Returns :
;;;     the line passed in
;;;
;;; Since 1.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_unref ()
;;;
;;; void pango_layout_line_unref (PangoLayoutLine *line);
;;;
;;; Decrease the reference count of a PangoLayoutLine by one. If the result is
;;; zero, the line and all associated memory will be freed.
;;;
;;; line :
;;;     a PangoLayoutLine
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_extents ()
;;;
;;; void pango_layout_line_get_extents (PangoLayoutLine *line,
;;;                                     PangoRectangle *ink_rect,
;;;                                     PangoRectangle *logical_rect);
;;;
;;; Computes the logical and ink extents of a layout line. See
;;; pango_font_get_glyph_extents() for details about the interpretation of the
;;; rectangles.
;;;
;;; line :
;;;     a PangoLayoutLine
;;;
;;; ink_rect :
;;;     rectangle used to store the extents of the glyph string as drawn, or
;;;     NULL
;;;
;;; logical_rect :
;;;     rectangle used to store the logical extents of the glyph string, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_pixel_extents ()
;;;
;;; void pango_layout_line_get_pixel_extents (PangoLayoutLine *layout_line,
;;;                                           PangoRectangle *ink_rect,
;;;                                           PangoRectangle *logical_rect);
;;;
;;; Computes the logical and ink extents of layout_line in device units. This
;;; function just calls pango_layout_line_get_extents() followed by two
;;; pango_extents_to_pixels() calls, rounding ink_rect and logical_rect such
;;; that the rounded rectangles fully contain the unrounded one (that is, passes
;;; them as first argument to pango_extents_to_pixels()).
;;;
;;; layout_line :
;;;     a PangoLayoutLine
;;;
;;; ink_rect :
;;;     rectangle used to store the extents of the glyph string as drawn, or
;;;     NULL
;;;
;;; logical_rect :
;;;     rectangle used to store the logical extents of the glyph string, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_index_to_x ()
;;;
;;; void pango_layout_line_index_to_x (PangoLayoutLine *line,
;;;                                    int index_,
;;;                                    gboolean trailing,
;;;                                    int *x_pos);
;;;
;;; Converts an index within a line to a X position.
;;;
;;; line :
;;;     a PangoLayoutLine
;;;
;;; index_ :
;;;     byte offset of a grapheme within the layout
;;;
;;; trailing :
;;;     an integer indicating the edge of the grapheme to retrieve the position
;;;     of. If > 0, the trailing edge of the grapheme, if 0, the leading of the
;;;     grapheme.
;;;
;;; x_pos :
;;;     location to store the x_offset (in Pango unit)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_x_to_index ()
;;;
;;; gboolean pango_layout_line_x_to_index (PangoLayoutLine *line,
;;;                                        int x_pos,
;;;                                        int *index_,
;;;                                        int *trailing);
;;;
;;; Converts from x offset to the byte index of the corresponding character
;;; within the text of the layout. If x_pos is outside the line, index_ and
;;; trailing will point to the very first or very last position in the line.
;;; This determination is based on the resolved direction of the paragraph; for
;;; example, if the resolved direction is right-to-left, then an X position to
;;; the right of the line (after it) results in 0 being stored in index_ and
;;; trailing. An X position to the left of the line results in index_ pointing
;;; to the (logical) last grapheme in the line and trailing being set to the
;;; number of characters in that grapheme. The reverse is true for a
;;; left-to-right line.
;;;
;;; line :
;;;     a PangoLayoutLine
;;;
;;; x_pos :
;;;     the X offset (in Pango units) from the left edge of the line.
;;;
;;; index_ :
;;;     location to store calculated byte index for the grapheme in which the
;;;     user clicked
;;;
;;; trailing :
;;;     location to store an integer indicating where in the grapheme the user
;;;     clicked. It will either be zero, or the number of characters in the
;;;     grapheme. 0 represents the leading edge of the grapheme
;;;
;;; Returns :
;;;     FALSE if x_pos was outside the line, TRUE if inside
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_x_ranges ()
;;;
;;; void pango_layout_line_get_x_ranges (PangoLayoutLine *line,
;;;                                      int start_index,
;;;                                      int end_index,
;;;                                      int **ranges,
;;;                                      int *n_ranges);
;;;
;;; Gets a list of visual ranges corresponding to a given logical range. This
;;; list is not necessarily minimal - there may be consecutive ranges which are
;;; adjacent. The ranges will be sorted from left to right. The ranges are with
;;; respect to the left edge of the entire layout, not with respect to the line.
;;;
;;; line :
;;;     a PangoLayoutLine
;;;
;;; start_index :
;;;     Start byte index of the logical range. If this value is less than the
;;;     start index for the line, then the first range will extend all the way
;;;     to the leading edge of the layout. Otherwise it will start at the
;;;     leading edge of the first character.
;;;
;;; end_index :
;;;     Ending byte index of the logical range. If this value is greater than
;;;     the end index for the line, then the last range will extend all the way
;;;     to the trailing edge of the layout. Otherwise, it will end at the
;;;     trailing edge of the last character.
;;;
;;; ranges :
;;;     location to store a pointer to an array of ranges. The array will be of
;;;     length 2*n_ranges, with each range starting at (*ranges)[2*n] and of
;;;     width (*ranges)[2*n + 1] - (*ranges)[2*n]. This array must be freed
;;;     with g_free(). The coordinates are relative to the layout and are in
;;;     Pango units
;;;
;;; n_ranges :
;;;     The number of ranges stored in ranges.
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.layout.lisp ------------------------------------------
