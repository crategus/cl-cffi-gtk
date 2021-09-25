;;; ----------------------------------------------------------------------------
;;; pango.layout.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.48 and modified to document the Lisp binding to the Pango library.
;;; See <http://www.pango.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     High-level layout driver objects
;;;
;;; Types and Values
;;;
;;;     PangoWrapMode
;;;     PangoEllipsizeMode
;;;     PangoAlignment
;;;     PangoLayoutLine
;;;     PangoLayoutRun
;;;
;;;     PangoLayout
;;;     PangoLayoutIter
;;;
;;; Functions
;;;
;;;     pango_layout_new
;;;     pango_layout_copy
;;;     pango_layout_get_context
;;;     pango_layout_context_changed
;;;     pango_layout_get_serial
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
;;;     pango_layout_set_ellipsize
;;;     pango_layout_get_ellipsize
;;;     pango_layout_is_ellipsized
;;;     pango_layout_set_indent
;;;     pango_layout_get_indent
;;;     pango_layout_get_spacing
;;;     pango_layout_set_spacing
;;;     pango_layout_set_line_spacing
;;;     pango_layout_get_line_spacing
;;;     pango_layout_set_justify
;;;     pango_layout_get_justify
;;;     pango_layout_set_auto_dir
;;;     pango_layout_get_auto_dir
;;;     pango_layout_get_direction
;;;     pango_layout_set_alignment
;;;     pango_layout_get_alignment
;;;     pango_layout_set_tabs
;;;     pango_layout_get_tabs
;;;     pango_layout_set_single_paragraph_mode
;;;     pango_layout_get_single_paragraph_mode
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
;;;     pango_layout_line_ref
;;;     pango_layout_line_unref
;;;     pango_layout_line_get_extents
;;;     pango_layout_line_get_pixel_extents
;;;     pango_layout_line_index_to_x
;;;     pango_layout_line_x_to_index
;;;     pango_layout_line_get_x_ranges
;;;     pango_layout_line_get_height
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ├── PangoLayoutIter
;;;     ╰── PangoLayoutLine
;;;
;;;     GEnum
;;;     ├── PangoAlignment
;;;     ├── PangoEllipsizeMode
;;;     ╰── PangoWrapMode
;;;
;;;     GObject
;;;     ╰── PangoLayout
;;;
;;; Description
;;;
;;;     While complete access to the layout capabilities of Pango is provided
;;;     using the detailed interfaces for itemization and shaping, using that
;;;     functionality directly involves writing a fairly large amount of code.
;;;     The objects and functions in this section provide a high-level driver
;;;     for formatting entire paragraphs of text at once. This includes
;;;     paragraph-level functionality such as line-breaking, justification,
;;;     alignment and ellipsization.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; enum PangoWrapMode
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoWrapMode" pango-wrap-mode
  (:export t
   :type-initializer "pango_wrap_mode_get_type")
  (:word 0)
  (:char 1)
  (:word-char 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-wrap-mode atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'pango-wrap-mode atdoc:*external-symbols*)
 "@version{2021-1-5}
  @begin{short}
    A @sym{pango-wrap-mode} enumeration describes how to wrap the lines of a
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
  @end{table}
  @see-class{pango-layout}")

(export 'pango-wrap-mode)

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

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-ellipsize-mode atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'pango-ellipsize-mode atdoc:*external-symbols*)
 "@version{2021-5-2}
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
  @end{table}
  @see-class{pango-layout}")

(export 'pango-ellipsize-mode)

;;; ----------------------------------------------------------------------------
;;; enum PangoAlignment
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoAlignment" pango-alignment
  (:export t
   :type-initializer "pango_alignment_get_type")
  (:left 0)
  (:center 1)
  (:right 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-alignment atdoc:*symbol-name-alias*)
      "Enum"
      (gethash 'pango-alignment atdoc:*external-symbols*)
 "@version{2021-1-5}
  @begin{short}
    A @sym{pango-alignment} enumeration describes how to align the lines of a
    @class{pango-layout} object within the available space.
  @end{short}
  If the @class{pango-layout} object is set to justify using the function
  @fun{pango-layout-justify}, this only has effect for partial lines.
  @begin{pre}
(define-g-enum \"PangoAlignment\" pango-alignment
  (:export t
   :type-initializer \"pango_alignment_get_type\")
  (:left 0)
  (:center 1)
  (:right 2))
  @end{pre}
  @begin[code]{table}
    @entry[:left]{Put all available space on the right.}
    @entry[:center]{Center the line within the available space.}
    @entry[:right]{Put all available space on the left.}
  @end{table}
  @see-class{pango-layout}
  @see-function{pango-layout-justify}")

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
;;; ----------------------------------------------------------------------------

;; TODO: Implement PangoLayoutLine as a Gboxed Cstruct

(define-g-boxed-opaque pango-layout-line "PangoLayoutLine"
  :alloc (error "Use Pango to create PANGO-LAYOUT-LINEs"))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-layout-line atdoc:*class-name-alias*)
      "CStruct"
      (documentation 'pango-layout-line 'type)
 "@version{2021-1-5}
  @begin{short}
    The @sym{pango-layout-line} structure represents one of the lines resulting
    from laying out a paragraph via a @class{pango-layout} object.
  @end{short}
  @sym{pango-layout-line} structures are obtained by calling the function
  @fun{pango-layout-line} and are only valid until the text, attributes,
  or settings of the parent @class{pango-layout} object are modified.

  Routines for rendering @class{pango-layout} objects are provided in code
  specific to each rendering system.
  @begin{pre}
(define-g-boxed-opaque pango-layout-line \"PangoLayoutLine\"
  :alloc (error \"Use Pango to create PANGO-LAYOUT-LINEs\"))
  @end{pre}
  @see-class{pango-layout}
  @see-function{pango-layout-line}")

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
;;; PangoLayout
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoLayout" pango-layout
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "pango_layout_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'pango-layout 'type)
 "@version{2021-1-18}
  @begin{short}
    The @sym{pango-layout} class represents an entire paragraph of text.
  @end{short}
  It is initialized with a @class{pango-context} object, UTF-8 string and set
  of attributes for that string. Once that is done, the set of formatted lines
  can be extracted from the object, the layout can be rendered, and conversion
  between logical character positions within the layout's text, and the
  physical position of the resulting glyphs can be made.

  There are also a number of parameters to adjust the formatting of a
  @sym{pango-layout} object. It is possible, as well, to ignore the 2-D setup,
  and simply treat the results of a @sym{pango-layout} object as a list of
  lines.
  @image[layout]{Figure. Adjustable parameters (on the left) and font metrics
    (on the right) for a Pango layout}

  The @sym{pango-layout} class is opaque, and has no user visible fields.
  @see-class{pango-context}")

;;; ----------------------------------------------------------------------------
;;; PangoLayoutIter
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque pango-layout-iter "PangoLayoutIter"
  :alloc (error "Use Pango to create PANGO-LAYOUT-ITER"))

#+cl-cffi-gtk-documentation
(setf (gethash 'pango-layout-iter atdoc:*class-name-alias*)
      "CStruct"
      (documentation 'pango-layout-iter 'type)
 "@version{2021-1-5}
  @begin{short}
    A @sym{pango-layout-iter} structure can be used to iterate over the visual
    extents of a @class{pango-layout} object.
  @end{short}

  The @sym{pango-layout-iter} structure is opaque, and has no user visible
  fields.
  @begin{pre}
(define-g-boxed-opaque pango-layout-iter \"PangoLayoutIter\"
  :alloc (error \"Use Pango to create PANGO-LAYOUT-ITER\"))
  @end{pre}
  @see-class{pango-layout}")

(export 'pango-layout-iter)

;;; ----------------------------------------------------------------------------
;;; pango_layout_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_new" pango-layout-new) (g-object pango-layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-5}
  @argument[context]{a @class{pango-context} object}
  @return{The newly allocated @class{pango-layout} object.}
  @begin{short}
    Create a new Pango layout with attributes initialized to default values for
    a particular @class{pango-context} object.
  @end{short}
  @see-class{pango-layout}
  @see-class{pango-context}"
  (context (g-object pango-context)))

(export 'pango-layout-new)

;;; ----------------------------------------------------------------------------
;;; pango_layout_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_copy" pango-layout-copy) (g-object pango-layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @argument[src]{a @class{pango-layout} object}
  @begin{return}
    The newly allocated @class{pango-layout} object.
  @end{return}
  @begin{short}
    Does a deep copy-by-value of the src layout.
  @end{short}
  The attribute list, tab array, and text from the original layout are all
  copied by value.
  @see-class{pango-layout}"
  (src (g-object pango-layout)))

(export 'pango-layout-copy)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_context () -> pango-layout-context
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_context" pango-layout-context)
    (g-object pango-context)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    The @class{pango-context} object for the layout.
  @end{return}
  @begin{short}
    Retrieves the Pango context used for this layout.
  @end{short}
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-context)

;;; ----------------------------------------------------------------------------
;;; pango_layout_context_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_context_changed" pango-layout-context-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-1-5}
  @argument[layout]{a @class{pango-layout} object}
  @begin{short}
    Forces recomputation of any state in the Pango layout that might depend
    on the @arg{layout}'s context.
  @end{short}
  This function should be called if you make changes to the context subsequent
  to creating the Pango layout.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-context-changed)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_serial () -> pango-layout-serial
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_serial" pango-layout-serial) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    The current serial number of @arg{layout}.
  @end{return}
  @begin{short}
    Returns the current serial number of @arg{layout}.
  @end{short}
  The serial number is initialized to an small number larger than zero when a
  new layout is created and is increased whenever the layout is changed using
  any of the setter functions, or the @class{pango-context} object it uses has
  changed. The serial may wrap, but will never have the value 0. Since it can
  wrap, never compare it with \"less than\", always use \"not equals\".

  This can be used to automatically detect changes to a Pango layout, and is
  useful for example to decide whether a layout needs redrawing. To force the
  serial to be increased, use the function @fun{pango-layout-context-changed}.
  @see-class{pango-layout}
  @see-class{pango-context}
  @see-function{pango-layout-context-changed}"
  (layout (g-object pango-layout)))

(export 'pango-layout-serial)

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_text ()
;;; pango_layout_get_text () -> pango-layout-text
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-text) (text layout)
  (foreign-funcall "pango_layout_set_text"
                   (g-object pango-layout) layout
                   :string text
                   :int -1)  ;; (length text) is wrong, byte length is needed
  text)

(defcfun ("pango_layout_get_text" pango-layout-text) :string
 #+cl-cffi-gtk-documentation
 "@version{2020-10-18}
  @syntax[]{(pango-layout-text layout) => text}
  @syntax[]{(setf (pango-layout-text layout) text)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[text]{a string with a valid UTF-8 string}
  @begin{short}
    Accessor of the text of a @class{pango-layout} object.
  @end{short}

  The function @sym{pango-layout-text} gets the text in the Pango layout.
  The function @sym{(setf pango-layout-text)} sets the text of the Pango layout.

  Note that if you have used the functions @fun{pango-layout-set-markup} or
  @fun{pango-layout-set-markup-with-accel} on the Pango layout before, you may
  want to call the function @fun{pango-layout-attributes} to clear the
  attributes set on the layout from the markup as this function does not clear
  attributes.
  @see-class{pango-layout}
  @see-function{pango-layout-set-markup}
  @see-function{pango-layout-set-markup-with-accel}
  @see-function{pango-layout-attributes}"
  (layout (g-object pango-layout)))

(export 'pango-layout-text)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_character_count () -> pango-layout-character-count
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_character_count" pango-layout-character-count) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    An integer with the number of Unicode characters in the text of
    @arg{layout}.
  @end{return}
  @begin{short}
    Returns the number of Unicode characters in the the text of @arg{layout}.
  @end{short}
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-character-count)

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_set_markup" %pango-layout-set-markup) :void
  (layout (g-object pango-layout))
  (markup :string)
  (length :int))

(defun pango-layout-set-markup (layout markup)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @argument[layout]{a @class{pango-layout} object}
  @argument[markup]{a string with the marked-up text}
  @begin{short}
    Same as the function @fun{pango-layout-set-markup-with-accel}, but the
    markup text is not scanned for accelerators.
  @end{short}
  @see-class{pango-layout}
  @see-function{pango-layout-set-markup-with-accel}"
  (%pango-layout-set-markup layout markup -1))

(export 'pango-layout-set-markup)

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_markup_with_accel ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_set_markup_with_accel"
          %pango-layout-set-markup-with-accel) :void
  (layout (g-object pango-layout))
  (markup :string)
  (length :int)
  (accel-marker :uint32) ; for gunichar (see Glib Unicode Manipulation)
  (accel-char (:pointer :uint32)))

(defun pango-layout-set-markup-with-accel (layout markup accel-marker)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-13}
  @argument[layout]{a @class{pango-layout} object}
  @argument[markup]{a string with the marked-up text (see markup format)}
  @argument[accel-marker]{an unsigned integer with the marker for accelerators
    in the text}
  @begin{return}
    Returns the first located accelerator.
  @end{return}
  @begin{short}
    Sets the layout text and attribute list from marked-up text (see markup
    format).
  @end{short}
  Replaces the current text and attribute list.

  If @arg{accel-marker} is nonzero, the given character will mark the character
  following it as an accelerator. For example, @arg{accel-marker} might be an
  ampersand or underscore. All characters marked as an accelerator will receive
  a @code{PANGO_UNDERLINE_LOW} attribute, and the first character so marked
  will be returned. Two @arg{accel-marker} characters following each other
  produce a single literal @arg{accel-marker} character.
  @see-class{pango-layout}"
  (with-foreign-object (accel-char :uint32)
    (%pango-layout-set-markup-with-accel layout
                                         markup
                                         -1
                                         accel-marker
                                         accel-char)
      (values (mem-ref accel-char :uint32))))

(export 'pango-layout-set-markup-with-accel)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_attributes ()
;;; pango_layout_set_attributes () -> pango-layout-attributes
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-attributes) (attrs layout)
  (foreign-funcall "pango_layout_set_attributes"
                   (g-object pango-layout) layout
                   (g-boxed-foreign pango-attr-list) attrs
                   :void)
  attrs)

(defcfun ("pango_layout_get_attributes" pango-layout-attributes)
    (g-boxed-foreign pango-attr-list)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @syntax[]{(pango-layout-attributes layout) => attrs}
  @syntax[]{(setf (pango-layout-attributes layout) attrs)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[attrs]{a @class{pango-attr-list} instance, can be @code{NULL}}
  @begin{short}
    Accessor of the attribute list for the layout.
  @end{short}

  The function @sym{pango-layout-attributes} gets the attribute list for the
  layout, if any. The function @sym{(setf pango-layout-attributes)} sets the
  text attributes for a layout object. References @arg{attrs}, so the caller
  can unref its reference.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-attributes)

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_font_description ()
;;; pango_layout_get_font_description () -> pango-layout-font-description
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-font-description) (desc layout)
  (foreign-funcall "pango_layout_set_font_description"
                   (g-object pango-layout) layout
                   (g-boxed-foreign pango-font-description) desc
                   :void)
  desc)

(defcfun ("pango_layout_get_font_description" pango-layout-font-description)
    (g-boxed-foreign pango-font-description)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-11}
  @syntax[]{(pango-layout-font-description layout) => desc}
  @syntax[]{(setf (pango-layout-font-description layout) desc)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[desc]{the new @class{pango-font-description} structure, or
    @code{nil} to unset the current font description}
  @begin{short}
    Accessor of the font description of the Pango layout.
  @end{short}

  The function @sym{pango-layout-font-description} gets the font description for
  the Pango layout. The function @sym{(setf pango-layout-font-description)} sets
  the default font description for the Pango layout.

  If no font description is set on the layout, the font description from the
  layout's context is used.
  @see-class{pango-layout}
  @see-class{pango-font-description}"
  (layout (g-object pango-layout)))

(export 'pango-layout-font-description)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_width ()
;;; pango_layout_set_width () -> pango-layout-width
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-width) (width layout)
  (foreign-funcall "pango_layout_set_width"
                   (g-object pango-layout) layout
                   :int width
                   :void)
  width)

(defcfun ("pango_layout_get_width" pango-layout-width) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @syntax[]{(pango-layout-width layout) => width}
  @syntax[]{(setf (pango-layout-width layout) width)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[width]{an integer with the desired width in Pango units, or -1 to
    indicate that no wrapping or ellipsization should be performed}
  @begin{short}
    Accessor of the width in Pango units of a Pango layout.
  @end{short}

  The function @sym{pango-layout-width} gets the width to which the lines of
  the Pango layout should wrap. The function @sym{(setf pango-layout-width)}
  sets the width to which the lines of the Pango layout should wrap or
  ellipsized. The default value is -1: no width set.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-width)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_height ()
;;; pango_layout_set_height () -> pango-layout-height
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-height) (height layout)
  (foreign-funcall "pango_layout_set_height"
                   (g-object pango-layout) layout
                   :int height
                   :void)
  height)

(defcfun ("pango_layout_get_height" pango-layout-height) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @syntax[]{(pango-layout-height layout) => height}
  @syntax[]{(setf (pango-layout-height layout) height)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[height]{an integer with the desired height of the layout in Pango
    units if positive, or desired number of lines if negative}
  @begin{short}
    Accessor of the height in Pango units of the Pango layout.
  @end{short}

  The function @sym{pango-layout-height} gets the height of @arg{layout} used
  for ellipsization. The function @sym{(setf pango-layout-height)} sets the
  height to which the Pango layout should be ellipsized at. There are two
  different behaviors, based on whether @arg{height} is positive or negative.

  If @arg{height} is positive, it will be the maximum height of the layout.
  Only lines would be shown that would fit, and if there is any text omitted,
  an ellipsis added. At least one line is included in each paragraph regardless
  of how small the height value is. A value of zero will render exactly one
  line for the entire layout.

  If @arg{height} is negative, it will be the (negative of) maximum number of
  lines per paragraph. That is, the total number of lines shown may well be
  more than this value if the layout contains multiple paragraphs of text. The
  default value of -1 means that first line of each paragraph is ellipsized.
  This behvaior may be changed in the future to act per layout instead of per
  paragraph. File a bug against pango at http://bugzilla.gnome.org/ if your
  code relies on this behavior.

  Height setting only has effect if a positive width is set on layout and
  ellipsization mode of layout is not @code{PANGO_ELLIPSIZE_NONE}. The behavior
  is undefined if a height other than -1 is set and ellipsization mode is set
  to @code{PANGO_ELLIPSIZE_NONE}, and may change in the future.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-height)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_wrap ()
;;; pango_layout_set_wrap () -> pango-layout-wrap
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-wrap) (wrap layout)
  (foreign-funcall "pango_layout_set_wrap"
                   (g-object pango-layout) layout
                   pango-wrap-mode wrap
                   :void)
  wrap)

(defcfun ("pango_layout_get_wrap" pango-layout-wrap) pango-wrap-mode
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @syntax[]{(pango-layout-wrap layout) => wrap}
  @syntax[]{(setf (pango-layout-wrap layout) wrap)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[wrap]{a value of the @symbol{pango-wrap-mode} enumeration}
  @begin{short}
    Accessor of the wrap mode of the Pango layout.
  @end{short}

  The function @sym{pango-layout-wrap} gets the wrap mode for the layout. The
  function @sym{(setf pango-layout-wrap)} sets the wrap mode. The wrap mode
  only has effect if a width is set on the layout with the function
  @fun{pango-layout-width}. To turn off wrapping, set the width to -1.

  Use the function @fun{pango-layout-is-wrapped} to query whether any
  paragraphs were actually wrapped.
  @see-class{pango-layout}
  @see-function{pango-layout-width}
  @see-function{pango-layout-is-wrapped}"
  (layout (g-object pango-layout)))

(export 'pango-layout-wrap)

;;; ----------------------------------------------------------------------------
;;; pango_layout_is_wrapped ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_is_wrapped" pango-layout-is-wrapped) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    @em{True} if any paragraphs had to be wrapped, @em{false} otherwise.
  @end{return}
  @begin{short}
    Queries whether the layout had to wrap any paragraphs.
  @end{short}

  This returns @em{true} if a positive width is set on the layout, ellipsization
  mode of the layout is set to @code{PANGO_ELLIPSIZE_NONE}, and there are
  paragraphs exceeding the layout width that have to be wrapped.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-is-wrapped)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_ellipsize ()
;;; pango_layout_set_ellipsize () -> pango-layout-ellipsize
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-ellipsize) (ellipsize layout)
  (foreign-funcall "pango_layout_set_ellipsize"
                   (g-object pango-layout) layout
                   pango-ellipsize-mode ellipsize
                   :void)
  ellipsize)

(defcfun ("pango_layout_get_ellipsize" pango-layout-ellipsize)
    pango-ellipsize-mode
 #+cl-cffi-gtk-documentation
 "@version{2021-1-14}
  @syntax[]{(pango-layout-ellipsize layout) => ellipsize}
  @syntax[]{(setf (pango-layout-ellipsize layout) ellipsize)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[ellipsize]{a value of the @symbol{pango-ellipsize-mode} enumeration}
  @begin{short}
    Accessor of the ellipsization mode for the layout.
  @end{short}

  The function @sym{pango-layout-ellispze} gets the type of ellipsization being
  performed for the layout. The function @sym{(setf pango-layout-ellipsize}
  sets the type of ellipsization being performed for the layout. Depending on
  the ellipsization mode ellipsize text is removed from the start, middle, or
  end of text so they fit within the width and height of layout set with the
  functions @fun{pango-layout-width} and @fun{pango-layout-height}.

  If the layout contains characters such as newlines that force it to be layed
  out in multiple paragraphs, then whether each paragraph is ellipsized
  separately or the entire layout is ellipsized as a whole depends on the set
  height of the layout. See the function @fun{pango-layout-height} for details.

  Use the function @fun{pango-layout-is-ellipsized} to query whether any
  paragraphs were actually ellipsized.
  @see-class{pango-layout}
  @see-function{pango-layout-width}
  @see-function{pango-layout-height}
  @see-function{pango-layout-is-ellipsized}"
  (layout (g-object pango-layout)))

(export 'pango-layout-ellipsize)

;;; ----------------------------------------------------------------------------
;;; pango_layout_is_ellipsized ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_is_ellipsized" pango-layout-is-ellipsized) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-13}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    @em{True} if any paragraphs had to be ellipsized, @em{false} otherwise.
  @end{return}
  @begin{short}
    Queries whether the layout had to ellipsize any paragraphs.
  @end{short}

  This returns @em{true} if the ellipsization mode for the layout is not
  @code{PANGO_ELLIPSIZE_NONE}, a positive width is set on the layout, and there
  are paragraphs exceeding that width that have to be ellipsized.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-is-ellipsized)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_indent ()
;;; pango_layout_set_indent () -> pango-layout-indent
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-indent) (indent layout)
  (foreign-funcall "pango_layout_set_indent"
                   (g-object pango-layout) layout
                   :int indent
                   :void)
  indent)

(defcfun ("pango_layout_get_ident" pango-layout-indent) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @syntax[]{(pango-layout-indent layout) => indent}
  @syntax[]{(setf (pango-layout-indent layout) indent)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[indent]{an integer with the amount by which to indent}
  @begin{short}
    Accessor of the indent in Pango units of the Pango layout.
  @end{short}

  The function @sym{pango-layout-indent} gets the paragraph indent width in
  Pango units. A negative value indicates a hanging indentation. The function
  @sym{(setf pango-layout-indent)} sets the width in Pango units to indent each
  paragraph. A negative value of indent will produce a hanging indentation.
  That is, the first line will have the full width, and subsequent lines will
  be indented by the absolute value of indent.

  The indent setting is ignored if the layout alignment is set to
  @code{PANGO_ALIGN_CENTER}.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-indent)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_spacing ()
;;; pango_layout_set_spacing () -> pango-layout-spacing
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-spacing) (spacing layout)
  (foreign-funcall "pango_layout_set_spacing"
                   (g-object pango-layout) layout
                   :int spacing
                   :void)
  spacing)

(defcfun ("pango_layout_get_spacing" pango-layout-spacing) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-1-19}
  @syntax[]{(pango-layout-spacing layout) => spacing}
  @syntax[]{(setf (pango-layout-spacing layout) spacing)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[spacing]{an integer with the amount of spacing}
  @begin{short}
    Accessor of the spacing in Pango units of the Pango layout.
  @end{short}

  The function @sym{pango-layout-spacing} gets the amount of spacing in Pango
  units between the lines of the layout. The function
  @sym{(setf pango-layout-spacing)} sets the amount of spacing. When placing
  lines with spacing, Pango arranges things so that
  @begin{pre}
line2.top = line1.bottom + spacing
  @end{pre}
  @begin[Note]{dictionary}
    Since 1.44, Pango defaults to using the line height (as determined by the
    font) for placing lines. The spacing set with this function is only taken
    into account when the line-height factor is set to zero with the function
    @fun{pango-layout-line-spacing}.
  @end{dictionary}
  @see-class{pango-layout-spacing}
  @see-function{pango-layout-line-spacing}"
  (layout (g-object pango-layout)))

(export 'pango-layout-spacing)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line_spacing ()
;;; pango_layout_set_line_spacing () -> pango-layout-line-spacing
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defun (setf pango-layout-line-spacing) (factor layout)
  (foreign-funcall "pango_layout_set_line_spacing"
                   (g-object pango-layout) layout
                   :float (coerce factor 'float)
                   :void)
  factor)

#+pango-1-44
(defcfun ("pango_layout_get_line_spacing" pango-layout-line-spacing) :float
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @syntax[]{(pango-layout-line-spacing layout) => factor}
  @syntax[]{(setf (pango-layout-line-spacing layout) factor)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[factor]{a float with the new line spacing factor}
  @begin{short}
    Accessor of the line spacing for the Pango layout.
  @end{short}

  The function @sym{pango-layout-line-spacing} gets the value of the line
  spacing. The function @sym{(setf pango-layout-line-spacing)} sets a factor
  for line spacing. Typical values are: 0, 1, 1.5, 2. The default values is 0.

  If @arg{factor} is non-zero, lines are placed so that
  @begin{pre}
baseline2 = baseline1 + factor * height2
  @end{pre}
  where @code{height2} is the line height of the second line (as determined by
  the font(s)). In this case, the spacing set with the function
  @fun{pango-layout-spacing} is ignored.

  If @arg{factor} is zero, spacing is applied as before.

  Since 1.44
  @see-class{pango-laoyut}
  @see-function{pango-layout-spacing}"
  (layout (g-object pango-layout)))

#+pango-1-44
(export 'pango-layout-line-spacing)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_justify ()
;;; pango_layout_set_justify () -> pango-layout-justify
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-justify) (justify layout)
  (foreign-funcall "pango_layout_set_justify"
                   (g-object pango-layout) layout
                   :boolean justify
                   :void)
  justify)

(defcfun ("pango_layout_get_justify" pango-layout-justify) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @syntax[]{(pango-layout-justify layout) => justify}
  @syntax[]{(setf (pango-layout-justify layout) justify)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[justify]{a boolean whether the lines in the layout should be
    justified}
  @begin{short}
    Accessor of the justify value of the Pango layout.
  @end{short}

  The function @sym{pango-layout-justify} gets whether each complete line should
  be stretched to fill the entire width of the layout. The function
  @sym{(setf pango-layout-justify)} sets the justify. This stretching is
  typically done by adding whitespace, but for some scripts (such as Arabic),
  the justification may be done in more complex ways, like extending the
  characters.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-justify)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_auto_dir ()
;;; pango_layout_set_auto_dir () -> pango-layout-auto-dir
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-auto-dir) (auto-dir layout)
  (foreign-funcall "pango_layout_set_auto_dir"
                   (g-object pango-layout) layout
                   :boolean auto-dir
                   :void)
  auto-dir)

(defcfun ("pango_layout_get_auto_dir" pango-layout-auto-dir) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @syntax[]{(pango-layout-auto-dir layout) => auto-dir}
  @syntax[]{(setf (pango-layout-auto-dir layout) auto-dir)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[auto-dir]{if @em{true}, compute the bidirectional base direction
    from the layout's contents}
  @begin{short}
    The function @sym{pango-layout-auto-dir} gets whether to calculate the
    bidirectional base direction for the layout according to the contents of
    the layout.
  @end{short}

  The function @sym{(setf pango-layout-auto-dir)} sets whether to calculate the
  bidirectional base direction for the layout according to the contents of the
  layout. When this flag is on (the default), then paragraphs in layout that
  begin with strong right-to-left characters (Arabic and Hebrew principally),
  will have right-to-left layout, paragraphs with letters from other scripts
  will have left-to-right layout. Paragraphs with only neutral characters get
  their direction from the surrounding paragraphs.

  When @em{false}, the choice between left-to-right and right-to-left layout is
  done according to the base direction of the layout's Pango context. (See the
  function @fun{pango-context-base-dir}).

  When the auto-computed direction of a paragraph differs from the base
  direction of the context, the interpretation of @code{PANGO_ALIGN_LEFT} and
  @code{PANGO_ALIGN_RIGHT} are swapped.
  @see-class{pango-layout}
  @see-function{pango-context-base-dir}"
  (layout (g-object pango-layout)))

(export 'pango-layout-auto-dir)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_direction () -> pango-layout-direction
;;; ----------------------------------------------------------------------------

#+pango-1-46
(defcfun ("pango_layout_get_direction" pango-layout-direction) pango-direction
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @argument[index]{an integer with the byte index of the char}
  @return{a @symbol{pango-direction} value for the text direction at
    @arg{index}}
  @begin{short}
    Gets the text direction at the given character position in the layout.
  @end{short}

  Since 1.46
  @see-class{pango-layout}"
  (layout (g-object pango-layout))
  (index :int))

#+pango-1-46
(export 'pango-layout-direction)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_alignment ()
;;; pango_layout_set_alignment () -> pango-layout-alignment
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-alignment) (alignment layout)
  (foreign-funcall "pango_layout_set_alignment"
                   (g-object pango-layout) layout
                   pango-alignment alignment
                   :void)
  alignment)

(defcfun ("pango_layout_get_alignment" pango-layout-alignment) pango-alignment
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @syntax[]{(pango-layout-alignment layout) => alignment}
  @syntax[]{(setf (pango-layout-alignment layout) alignment)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[alignment]{a @symbol{pango-alignment} value}
  @begin{short}
    Accessor of the alignement of the layout.
  @end{short}

  The function @sym{pango-layout-alignment} gets the alignment for the layout:
  how partial lines are positioned within the horizontal space available. The
  function @sym{(setf pango-layout-alignment)} sets the alignment.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-alignment)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_tabs ()
;;; pango_layout_set_tabs () -> pango-layout-tabs
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-tabs) (tabs layout)
  (foreign-funcall "pango_layout_set_tabs"
                   (g-object pango-layout) layout
                   (g-boxed-foreign pango-tab-array) tabs
                   :void)
  tabs)

(defcfun ("pango_layout_get_tabs" pango-layout-tabs)
    (g-boxed-foreign pango-tab-array)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @syntax[]{(pango-layout-tabs layout) => tabs}
  @syntax[]{(setf (pango-layout-tabs layout) tabs)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[tabs]{a @class{pango-tab-array}, or @code{nil}}
  @begin{short}
    Accessor of the tabs for the Pango layout.
  @end{short}

  The function @sym{pango-layout-tabs} gets the current @symbol{pango-tab-array}
  instance used by this layout. The function @sym{(setf pango-layout-tabs)} sets
  the tabs to use for the layout.

  If no PangoTabArray has been set, then the default tabs are in use and
  @code{nil} is returned. Default tabs are every 8 spaces. The return value
  should be freed with the function @fun{pango-tab-array-free}.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-tabs)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_single_paragraph_mode ()
;;; pango_layout_set_single_paragraph_mode ()
;;;   -> pango-layout-single-paragraph-mode
;;; ----------------------------------------------------------------------------

(defun (setf pango-layout-single-paragraph-mode) (setting layout)
  (foreign-funcall "pango-layout-single-paragraph-mode"
                   (g-object pango-layout) layout
                   :boolean setting
                   :void)
  setting)

(defcfun ("pango_layout_get_single_paragraph_mode"
           pango-layout-single-paragraph-mode) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @syntax[]{(pango-layout-tabs layout) => tabs}
  @syntax[]{(setf (pango-layout-tabs layout) tabs)}
  @argument[layout]{a @class{pango-layout} object}
  @argument[setting]{whether the layout does not break paragraphs at paragraph
    separator characters}
  @begin{short}
    Accessor of the single paragraph mode of the Pango Layout.
  @end{short}

  If @arg{setting} is @em{true}, do not treat newlines and similar characters
  as paragraph separators. Instead, keep all text in a single paragraph, and
  display a glyph for paragraph separator characters. Used when you want to
  allow editing of newlines on a single text line.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-single-paragraph-mode)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_unknown_glyphs_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_unknown_glyphs_count"
           pango-layout-unknown-glyphs-count) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @return{An integer with the number of unknown glyphs in @arg{layout}.}
  @begin{short}
    Counts the number unknown glyphs in layout.
  @end{short}
  That is, zero if glyphs for all characters in the layout text were found, or
  more than zero otherwise.

  This function can be used to determine if there are any fonts available to
  render all characters in a certain string, or when used in combination with
  @code{PANGO_ATTR_FALLBACK}, to check if a certain font supports all the
  characters in the string.
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-unknown-glyphs-count)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_log_attrs () -> pango-layout-log-attrs
;;; ----------------------------------------------------------------------------

;; FIXME: This implementation does not work.

(defcfun ("pango_layout_get_log_attrs" %pango-layout-log-attrs) :void
  (layout (g-object pango-layout))
  (attrs (:pointer (:struct pango-log-attr)))
  (n-attrs (:pointer :int)))

(defun pango-layout-log-attrs (layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    A list of logical attributes of type @symbol{pango-log-attr}.
  @end{return}
  @begin{short}
    Retrieves a list of logical attributes for each character in the layout.
  @end{short}
  @see-class{pango-layout}
  @see-symbol{pango-log-attr}"
  (with-foreign-objects ((attrs-ptr :pointer) (n-attrs :int))
    (%pango-layout-log-attrs layout attrs-ptr n-attrs)
    (loop with attrs-ar = (mem-ref attrs-ptr :pointer)
          for i from 0 below (mem-ref n-attrs :int)
          for attr = (mem-aref attrs-ar :pointer i)
          collect attr
          finally (g-free attrs-ptr))))

(export 'pango-layout-log-attrs)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_log_attrs_readonly () -> pango-layout-log-attrs-readonly
;;; ----------------------------------------------------------------------------

;; FIXME: This implementation does not work.

(defcfun ("pango_layout_get_log_attrs_readonly"
          %pango-layout-log-attrs-readonly) (:pointer (:struct pango-log-attr))
  (layout (g-object pango-layout))
  (n-attrs (:pointer :int)))

(defun pango-layout-log-attrs-readonly (layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @return{a list of logical attributes of type @symbol{pango-log-attr}}
  @begin{short}
    Retrieves a list of logical attributes for each character in the layout.
  @end{short}

  This is a faster alternative to the function @fun{pango-layout-log-attrs}. The
  returned list is part of @arg{layout} and must not be modified. Modifying the
  layout will invalidate the returned list.
  @see-class{pango-layout}
  @see-function{pango-layout-log-attrs}"
  (with-foreign-object (n-attrs :int)
    (let ((attrs-ptr (%pango-layout-log-attrs-readonly layout n-attrs)))
      (loop with attrs-ar = (mem-ref attrs-ptr :pointer)
            for i from 0 below (mem-ref n-attrs :int)
            for attr = (mem-ref attrs-ar :pointer i)
            collect attr
            finally (g-free attrs-ptr)))))

(export 'pango-layout-log-attrs-readonly)

;;; ----------------------------------------------------------------------------
;;; pango_layout_index_to_pos ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_index_to_pos" %pango-layout-index-to-pos) :void
  (layout (g-object pango-layout))
  (index :int)
  (pos (:pointer (:struct pango-rectangle))))

(defun pango-layout-index-to-pos (layout index)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @argument[index]{an integer with the byte index within @arg{layout}}
  @begin{return}
    A @symbol{pango-rectangle} instance with the position of the grapheme.
  @end{return}
  @begin{short}
    Converts from an index within a Pango layout to the onscreen position
    corresponding to the grapheme at that index, which is represented as
    rectangle.
  @end{short}
  Note that pos->x is always the leading edge of the grapheme and
  pos->x + pos->width the trailing edge of the grapheme. If the directionality
  of the grapheme is right-to-left, then pos->width will be negative.
  @see-class{pango-layout}"
  (with-foreign-object (pos '(:pointer (:struct pango-rectangle)))
    (%pango-layout-index-to-pos layout index pos)
    pos))

(export 'pango-layout-index-to-pos)

;;; ----------------------------------------------------------------------------
;;; pango_layout_index_to_line_x ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_index_to_line_x" %pango-layout-index-to-line-x) :void
  (layout (g-object pango-layout))
  (index :int)
  (trailing :boolean)
  (line (:pointer :int))
  (x-pos (:pointer :int)))

(defun pango-layout-index-to-line-x (layout index trailing)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-18}
  @argument[layout]{a @class{pango-layout} object}
  @argument[index]{an integer with the byte index of a grapheme within the
    layout}
  @argument[trailing]{an integer indicating the edge of the grapheme to
    retrieve the position of, if 0, the trailing edge of the grapheme, if > 0,
    the leading of the grapheme}
  @begin{return}
    @code{line} -- an integer with the resulting line index, (which will between
    0 and @code{(pango-layout-line-count layout)} - 1, or @code{nil} @br{}
    @code{x-pos} -- an integer with the resulting position within line
    (@code{+pango-scale+} units per device unit), or @code{nil}
  @end{return}
  @begin{short}
    Converts from byte @arg{index} within the layout to line and x position.
  @end{short}
  x position is measured from the left edge of the line.
  @see-class{pango-layout}"
  (with-foreign-objects ((line :int) (x-pos :int))
    (%pango-layout-index-to-line-x layout index trailing line x-pos)
    (values (mem-ref line :int)
            (mem-ref x-pos :int))))

(export 'pango-layout-index-to-line-x)

;;; ----------------------------------------------------------------------------
;;; pango_layout_xy_to_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_xy_to_index" %pango-layout-xy-to-index) :boolean
  (layout (g-object pango-layout))
  (x :int)
  (y :int)
  (index (:pointer :int))
  (trailing (:pointer :int)))

(defun pango-layout-xy-to-index (layout x y)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @argument[x]{the integer y offset (in Pango units) from the left edge of the
    layout}
  @argument[y]{the integer y offset (in Pango units) from the top edge of the
    layout}
  @begin{return}
    @code{index} -- an integer with the calculated byte index @br{}
    @code{trailing} -- an integer indicating where in the grapheme the user
    clicked, it will either be zero, or the number of characters in the
    grapheme, 0 represents the trailing edge of the grapheme @br{}
    @em{true} -- if the coordinates were inside text, @em{false} otherwise
  @end{return}
  @begin{short}
    Converts from x and y position within a layout to the byte index to the
    character at that logical position.
  @end{short}
  If the y position is not inside the layout, the closest position is chosen
  (the position will be clamped inside the layout). If the x position is not
  within the layout, then the start or the end of the line is chosen as
  described for the function @fun{pango-layout-x-to-index}. If either the x or
  y positions were not inside the layout, then the function returns @em{false};
  on an exact hit, it returns @em{true}.
  @see-class{pango-layout}"
  (with-foreign-objects ((index :int) (trailing :int))
    (let ((bool (%pango-layout-xy-to-index layout x y index trailing)))
      (values (mem-ref index :int)
              (mem-ref trailing :int)
              bool))))

(export 'pango-layout-xy-to-index)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_cursor_pos () -> pango-layout-cursor-pos
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_cursor_pos" %pango-layout-cursor-pos) :void
  (layout (g-object pango-layout))
  (index :int)
  (strong-pos (:pointer (:struct pango-rectangle)))
  (weak-pos (:pointer (:struct pango-rectangle))))

(defun pango-layout-cursor-pos (layout index)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @argument[index]{an integer with the byte index of the cursor}
  @begin{return}
    @code{strong-pos} -- a @symbol{pango-rectangle} instance with the strong
    cursor position (may be NULL) @br{}
    @code{weak-pos} -- a @symbol{pango-rectangle} instance the weak cursor
    position (may be NULL)
  @end{return}
  @begin{short}
    Given an index within a layout, determines the positions that of the strong
    and weak cursors if the insertion point is at that index.
  @end{short}
  The position of each cursor is stored as a zero-width rectangle. The strong
  cursor location is the location where characters of the directionality equal
  to the base direction of the layout are inserted. The weak cursor location is
  the location where characters of the directionality opposite to the base
  direction of the layout are inserted.
  @see-class{pango-layout}"
  (with-foreign-objects ((strong-pos '(:pointer (:struct pango-rectangle)))
                         (weak-pos '(:pointer (:struct pango-rectangle))))
    (%pango-layout-cursor-pos layout index strong-pos weak-pos)
    (values strong-pos weak-pos)))

(export 'pango-layout-cursor-pos)

;;; ----------------------------------------------------------------------------
;;; pango_layout_move_cursor_visually ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_move_cursor_visually"
          %pango-layout-move-cursor-visually) :void
  (layout (g-object pango-layout))
  (strong :boolean)
  (old-index :int)
  (old-trailing :int)
  (direction :int)
  (new-index (:pointer :int))
  (new-traling (:pointer :int)))

(defun pango-layout-move-cursor-visually (layout
                                          strong
                                          old-index
                                          old-trailing
                                          direction)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @argument[strong]{a boolean whether the moving cursor is the strong cursor or
    the weak cursor, the strong cursor is the cursor corresponding to text
    insertion in the base direction for the layout}
  @argument[old-index]{an integer with the byte index of the grapheme for the
    old index}
  @argument[old-trailing]{an integer, if 0, the cursor was at the trailing edge
    of the grapheme indicated by @arg{old-index}, if > 0, the cursor was at the
    leading edge}
  @argument[direction]{an integer with the direction to move cursor, a negative
    value indicates motion to the left}
  @begin{return}
    @code{new-index} -- an integer with the new cursor byte index, a value of
    -1 indicates that the cursor has been moved off the beginning of the layout,
    a value of @code{G_MAXINT} indicates that the cursor has been moved off the
    end of the layout @br{}
    @code{new-trailing} --an integer with the number of characters to move
    forward from the location returned for new_index to get the position where
    the cursor should be displayed, this allows distinguishing the position at
    the beginning of one line from the position at the end of the preceding
    line, @arg{new-index} is always on the line where the cursor should be
    displayed
  @end{return}
  @begin{short}
    Computes a new cursor position from an old position and a count of positions
    to move visually.
  @end{short}
  If @arg{direction} is positive, then the new strong cursor position will be
  one position to the right of the old cursor position. If @arg{direction} is
  negative, then the new strong cursor position will be one position to the
  left of the old cursor position.

  In the presence of bidirectional text, the correspondence between logical
  and visual order will depend on the direction of the current run, and there
  may be jumps when the cursor is moved off of the end of a run.

  Motion here is in cursor positions, not in characters, so a single call to
  the function @sym{pango-layout-move-cursor-visually} may move the cursor over
  multiple characters when multiple characters combine to form a single
  grapheme.
  @see-class{pango-layout}"
  (with-foreign-objects ((new-index :int) (new-trailing :int))
    (%pango-layout-move-cursor-visually layout
                                        strong
                                        old-index
                                        old-trailing
                                        direction
                                        new-index
                                        new-trailing)
    (values (mem-ref new-index :int)
            (mem-ref new-trailing :int))))

(export 'pango-layout-move-cursor-visually)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_extents" %pango-layout-extents) :void
  (layout (g-object pango-layout))
  (ink-rect (:pointer (:struct pango-rectangle)))
  (logical-rect (:pointer (:struct pango-rectangle))))

(defun pango-layout-extents (layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    @code{ink-rect} -- a @symbol{pango-rectangle} instance with the extents of
    the layout as drawn or NULL to indicate that the result is not needed @br{}
    @code{logical-rect} -- a @symbol{pango-rectangle} instance with the logical
    extents of the layout or NULL to indicate that the result is not needed
  @end{return}
  @begin{short}
    Computes the logical and ink extents of layout.
  @end{short}
  Logical extents are usually what you want for positioning things. Note that
  both extents may have non-zero x and y. You may want to use those to offset
  where you render the layout. Not doing that is a very typical bug that shows
  up as right-to-left layouts not being correctly positioned in a layout with
  a set width.

  The extents are given in layout coordinates and in Pango units; layout
  coordinates begin at the top left corner of the layout.
  @see-class{pango-layout}"
  (with-foreign-objects ((ink-rect '(:pointer (:struct pango-rectangle)))
                         (logical-rect '(:pointer (:struct pango-rectangle))))
    (%pango-layout-extents layout ink-rect logical-rect)
    (values ink-rect logical-rect)))

(export 'pango-layout-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_pixel_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_pixel_extents" %pango-layout-pixel-extents) :void
  (layout (g-object pango-layout))
  (ink-rect (:pointer (:struct pango-rectangle)))
  (logical-rect (:pointer (:struct pango-rectangle))))

(defun pango-layout-pixel-extents (layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    @code{ink-rect} -- a @symbol{pango-rectangle} instance with the extents of
    the layout as drawn or NULL to indicate that the result is not needed @br{}
    @code{logical-rect} -- a @symbol{pango-rectangle} instance with the logical
    extents of the layout or NULL to indicate that the result is not needed
  @end{return}
  @begin{short}
    Computes the logical and ink extents of layout in device units.
  @end{short}
  This function just calls the function @fun{pango-layout-extents} followed by
  two @fun{pango-extents-to-pixels} calls, rounding @arg{ink-rect} and
  @arg{logical-rect} such that the rounded rectangles fully contain the
  unrounded one (that is, passes them as first argument to the function
  @fun{pango-extents-to-pixels}).
  @see-class{pango-layout}
  @see-function{pango-layout-extents}
  @see-function{pango-extents-to-pixels}"
  (with-foreign-objects ((ink-rect '(:pointer (:struct pango-rectangle)))
                         (logical-rect '(:pointer (:struct pango-rectangle))))
    (%pango-layout-pixel-extents layout ink-rect logical-rect)
    (values ink-rect logical-rect)))

(export 'pango-layout-pixel-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_size () -> pango-layout-size
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_size" %pango-layout-size) :void
  (layout (g-object pango-layout))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun pango-layout-size (layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    @code{width}  -- an integer with the logical width, or @code{nil} @br{}
    @code{height} -- an integer with the logical height, or @code{nil}
  @end{return}
  @begin{short}
    Determines the logical width and height of a layout in Pango units, device
    units scaled by the constant @var{+pango-scale+}.
  @end{short}
  This is simply a convenience function around the function
  @fun{pango-layout-extents}.
  @see-class{pango-layout}
  @see-function{pango-layout-extents}
  @see-variable{+pango-scale+}"
  (with-foreign-objects ((width :int) (height :int))
    (%pango-layout-size layout width height)
    (values (mem-ref width :int)
            (mem-ref height :int))))

(export 'pango-layout-size)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_pixel_size () -> pango-layout-pixel-size
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_pixel_size" %pango-layout-get-pixel-size) :void
  (layout (g-object pango-layout))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun pango-layout-pixel-size (layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    @code{width} - an integer with the logical width @br{}
    @code{height} - an integer with the logical height
  @end{return}
  @begin{short}
    Determines the logical width and height of a Pango layout in device units.
  @end{short}
  The function @fun{pango-layout-size} returns the width and height scaled by
  @code{+pango-scale+}. This is simply a convenience function around the
  function @fun{pango-layout-pixel-extents}.
  @see-class{pango-layout}
  @see-funciton{pango-layout-size}
  @see-function{pango-layout-pixel-extents}"
  (with-foreign-objects ((width :int) (height :int))
    (%pango-layout-get-pixel-size layout width height)
    (values (mem-ref width :int)
            (mem-ref height :int))))

(export 'pango-layout-pixel-size)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_baseline () -> pango-layout-baseline
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_baseline" pango-layout-baseline) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @return{An integer with the baseline of first line, from top of @arg{layout}.}
  @begin{short}
    Gets the y position of baseline of the first line in layout.
  @end{short}
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-baseline)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line_count () -> pango-layout-line-count
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_line_count" pango-layout-line-count) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @return{An integer witht the line count.}
  @begin{short}
    Retrieves the count of lines for the layout.
  @end{short}
  @see-class{pango-layout}"
  (layout (g-object pango-layout)))

(export 'pango-layout-line-count)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line () -> pango-layout-line
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_line" pango-layout-line)
    (g-boxed-foreign pango-layout-line)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @argument[line]{an integer with the index of a line, which must be between 0
    and @code{(pango-layout-line-count layout)} - 1, inclusive}
  @begin{return}
    The requested @class{pango-layout-line} instance, or @code{nil} if the
    index is out of range. This layout line can be ref'ed and retained, but
    will become invalid if changes are made to the Pango layout.
  @end{return}
  @begin{short}
    Retrieves a particular line from a Pango layout.
  @end{short}

  Use the faster function @fun{pango-layout-line-readonly} if you do not plan
  to modify the contents of the line (glyphs, glyph widths, etc.).
  @see-class{pango-layout}
  @see-class{pango-layout-line}
  @see-function{pango-layout-line-readonly}"
  (layout (g-object pango-layout))
  (line :int))

(export 'pango-layout-line)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line_readonly () -> pango-layout-line-readonly
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_line_readonly" pango-layout-line-readonly)
    (g-boxed-foreign pango-layout-line)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @argument[line]{an integer with the index of a line, which must be between 0
    and (@code{pango-layout-line-count layout)} - 1, inclusive}
  @begin{return}
    The requested @class{pango-layout-line} instance, or NULL if the index is
    out of range. This layout line can be ref'ed and retained, but will become
    invalid if changes are made to the PangoLayout. No changes should be made
    to the line.
  @end{return}
  @begin{short}
    Retrieves a particular line from a Pango layout.
  @end{short}

  This is a faster alternative to the function @fun{pango-layout-line}, but the
  user is not expected to modify the contents of the line (glyphs, glyph widths,
  etc.).
  @see-class{pango-layout}
  @see-function{pango-layout-line}"
  (layout (g-object pango-layout))
  (line :int))

(export 'pango-layout-line-readonly)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_lines () -> pango-layout-lines
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_lines" pango-layout-lines)
    (g-slist (g-boxed-foreign pango-layout-line :free-from-foreign nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    A list containing the @class{pango-layout-line} instances in the layout.
    This points to internal data of the Pango layout and must be used with
    care. It will become invalid on any change to the layout's text or
    properties.
  @end{return}
  @begin{short}
    Returns the lines of the layout as a list.
  @end{short}

  Use the faster function @fun{pango-layout-lines-readonly} if you do not plan
  to modify the contents of the lines (glyphs, glyph widths, etc.).
  @see-class{pango-layout}
  @see-class{pango-layout-line}"
  (layout (g-object pango-layout)))

(export 'pango-layout-lines)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_lines_readonly () -> pango-layout-lines-readonly
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_lines_readonly" pango-layout-lines-readonly)
    (g-slist (g-boxed-foreign pango-layout-line :free-from-foreign nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    A list containing the @class{pango-layout-line} instances in the layout.
    This points to internal data of the Pango layout and must be used with care.
    It will become invalid on any change to the layout's text or properties. No
    changes should be made to the lines.
  @end{return}
  @begin{short}
    Returns the lines of the layout as a list.
  @end{short}

  This is a faster alternative to the function @fun{pango-layout-lines}, but
  the user is not expected to modify the contents of the lines (glyphs, glyph
  widths, etc.).
  @see-class{pango-layout}
  @see-class{pango-layout-line}
  @see-function{pango-layout-lines}"
  (layout (g-object pango-layout)))

(export 'pango-layout-lines-readonly)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_iter () -> pango-layout-iter
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_iter" pango-layout-iter)
    (g-boxed-foreign pango-layout-iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[layout]{a @class{pango-layout} object}
  @begin{return}
    The new @symbol{pango-layout-iter} that should be freed using the function
    @fun{pango-layout-iter-free}.
  @end{return}
  @begin{short}
    Returns an iterator to iterate over the visual extents of the layout.
  @end{short}
  @see-class{pango-layout}
  @see-function{pango-layout-iter-free}"
  (layout (g-object pango-layout)))

(export 'pango-layout-iter)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_copy" pango-layout-iter-copy)
    (g-boxed-foreign pango-layout-iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[iter]{a @class{pango-layout-iter} instance, may be NULL}
  @begin{return}
    The newly allocated @class{pango-layout-iter} instance, which should be
    freed with the function @fun{pango-layout-iter-free}, or NULL if @arg{iter}
    was NULL.
  @end{return}
  @begin{short}
    Copies a @class{pango-layout-iter} instance.
  @end{short}
  @see-class{pango-layout-iter}
  @see-function{pango-layout-iter-free}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-copy)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_free" pango-layout-iter-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[iter]{a @class{pango-layout-iter} instance, may be NULL}
  @begin{short}
    Frees an iterator that is no longer in use.
  @end{short}
  @see-class{pango-layout-iter}"
  (iter (g-object pango-layout-iter)))

(export 'pango-layout-iter-free)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_run ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_next_run" pango-layout-iter-next-run) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{A boolean whether motion was possible.}
  @begin{short}
    Moves @arg{iter} forward to the next run in visual order.
  @end{short}
  If @arg{iter} was already at the end of the layout, returns @em{false}.
  @see-class{pango-layout-iter}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-next-run)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_char ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_next_char" pango-layout-iter-next-char) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{A boolean whether motion was possible.}
  @begin{short}
    Moves @arg{iter} forward to the next character in visual order.
  @end{short}
  If @arg{iter} was already at the end of the layout, returns @em{false}.
  @see-class{pango-layout-iter}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-next-char)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_cluster ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_next_cluster" pango-layout-iter-next-cluster)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{A boolean whether motion was possible.}
  @begin{short}
    Moves @arg{iter} forward to the next cluster in visual order.
  @end{short}
  If @arg{iter} was already at the end of the layout, returns @em{false}.
  @see-class{pango-layout-iter}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-next-cluster)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_next_line" pango-layout-iter-next-line) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{A boolean whether motion was possible.}
  @begin{short}
    Moves @arg{iter} forward to the start of the next line.
  @end{short}
  If @arg{iter} is already on the last line, returns @em{false}.
  @see-class{pango-layout-iter}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-next-line)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_at_last_line ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_at_last_line" pango-layout-iter-at-last-line)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{@em{True} if @arg{iter} is on the last line.}
  @begin{short}
    Determines whether @arg{iter} is on the last line of the layout.
  @end{short}
  @see-class{pango-layout-iter}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-at-last-line)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_index () -> pango-layout-iter-index
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_get_iter_index" pango-layout-iter-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{An integer with the current byte index.}
  @begin{short}
    Gets the current byte index.
  @end{short}
  Note that iterating forward by char moves in visual order, not logical order,
  so indexes may not be sequential. Also, the index may be equal to the length
  of the text in the layout, if on the NULL run (see the function
  @fun{pango-layout-iter-run}).
  @see-class{pango-layout-iter}
  @see-function{pango-layout-iter-run}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-index)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_baseline () -> pango-layout-iter-baseline
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_baseline" pango-layout-iter-baseline) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{An integer with the baseline of current line.}
  @begin{short}
    Gets the y position of the current line's baseline, in layout coordinates
    (origin at top left of the entire layout).
  @end{short}
  @see-class{pango-layout-iter}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-baseline)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_run () -> pango-layout-iter-run
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_run" pango-layout-iter-run)
    (g-boxed-foreign pango-glyph-item)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{A @class{pango-glyph-item} instance with the current run.}
  @begin{short}
    Gets the current run.
  @end{short}
  When iterating by run, at the end of each line, there is a position with a
  NULL run, so this function can return NULL. The NULL run at the end of each
  line ensures that all lines have at least one run, even lines consisting of
  only a newline.

  Use the faster function @fun{pango-layout-iter-run-readonly} if you do not
  plan to modify the contents of the run (glyphs, glyph widths, etc.).
  @see-class{pango-layout-iter}
  @see-class{pango-glyph-item}
  @see-function{pango-layout-iter-run-readonly}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-run)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_run_readonly () -> pango-layout-iter-run-readonly
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_run_readonly" pango-layout-iter-run-readonly)
    (g-boxed-foreign pango-glyph-item)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{A @class{pango-layout-iter} instance with the current run, that
    should not be modified.}
  @begin{short}
    Gets the current run.
  @end{short}
  When iterating by run, at the end of each line, there is a position with a
  NULL run, so this function can return NULL. The NULL run at the end of each
  line ensures that all lines have at least one run, even lines consisting of
  only a newline.

  This is a faster alternative to the function @fun{pango-layout-iter-run}, but
  the user is not expected to modify the contents of the run (glyphs,
  glyph widths, etc.).
  @see-class{pango-layout-iter}
  @see-function{pango-layout-iter-run}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-run-readonly)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line () -> pango-layout-iter-line
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_line" pango-layout-iter-line)
    (g-boxed-foreign pango-layout-line)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{A @class{pango-layout-line} instance with the current line.}
  @begin{short}
    Gets the current line.
  @end{short}

  Use the faster function @fun{pango-layout-iter-line-readonly} if you do not
  plan to modify the contents of the line (glyphs, glyph widths, etc.).
  @see-class{pango-layout-iter}
  @see-class{pango-layout-line}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-line)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line_readonly () -> pango-layout-iter-line-readonly
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_line_readonly" pango-layout-iter-line-readonly)
    (g-boxed-foreign pango-layout-line)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{A @class{pango-layout-line} instance with the current line, that
    should not be modified.}
  @begin{short}
    Gets the current line for read-only access.
  @end{short}

  This is a faster alternative to the function @fun{pango-layout-iter-line},
  but the user is not expected to modify the contents of the line (glyphs,
  glyph widths, etc.).
  @see-class{pango-layout-iter}
  @see-class{pango-layout-line}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-line-readonly)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_layout () -> pango-layout-iter-layout
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_layout" pango-layout-iter-layout)
    (g-object pango-layout)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{A @class{pango-layout} object with the layout associated with
    @arg{iter}.}
  @begin{short}
    Gets the layout associated with a Pango layout iterator.
  @end{short}
  @see-class{pango-layout-iter}"
  (iter (g-boxed-foreign pango-layout-iter)))

(export 'pango-layout-iter-layout)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_char_extents () -> pango-layout-iter-char-extents
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_char_extents" %pango-layout-iter-char-extents)
    :void
  (iter (g-boxed-foreign pango-layout-iter))
  (logical-rect (:pointer (:struct pango-rectangle))))

(defun pango-layout-iter-char-extents (iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @return{A @symbol{pango-rectangle} instance with logical extents.}
  @begin{short}
    Gets the extents of the current character, in layout coordinates (origin is
    the top left of the entire layout).
  @end{short}
  Only logical extents can sensibly be obtained for characters; ink extents
  make sense only down to the level of clusters.
  @see-class{pango-layout-iter}
  @see-symbol{pango-rectangle}"
  (with-foreign-object (logical-rect '(:pointer (:struct pango-rectangle)))
    (%pango-layout-iter-char-extents iter logical-rect)
    (values logical-rect)))

(export 'pango-layout-iter-char-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_cluster_extents ()
;;;   -> pango-layout-iter-cluster-extents
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_cluster_extents"
          %pango-layout-iter-cluster-extents) :void
  (iter (g-boxed-foreign pango-layout-iter))
  (ink-rect (:pointer (:struct pango-rectangle)))
  (logical-rect (:pointer (:struct pango-rectangle))))

(defun pango-layout-iter-cluster-extents (iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @begin{return}
    @code{ink-rect} -- a @symbol{pango-rectangle} instance with ink extents
    @br{}
    @code{logical-rect} -- a @symbol{pango-rectangle} instance with logical
    extents
  @end{return}
  @begin{short}
    Gets the extents of the current cluster, in layout coordinates (origin is
    the top left of the entire layout).
  @end{short}
  @see-class{pango-layout-iter}
  @see-symbol{pango-rectangle}"
  (with-foreign-objects ((ink-rect '(:pointer (:struct pango-rectangle)))
                         (logical-rect '(:pointer (:struct pango-rectangle))))
    (%pango-layout-iter-cluster-extents iter ink-rect logical-rect)
    (values ink-rect logical-rect)))

(export 'pango-layout-iter-cluster-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_run_extents () -> pango-layout-iter-run-extents
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_run_extents" %pango-layout-iter-run-extents)
    :void
  (iter (g-boxed-foreign pango-layout-iter))
  (ink-rect (:pointer (:struct pango-rectangle)))
  (logical-rect (:pointer (:struct pango-rectangle))))

(defun pango-layout-iter-run-extents (iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @begin{return}
    @code{ink-rect} -- a @symbol{pango-rectangle} instance with ink extents
    @br{}
    @code{logical-rect} -- a @symbol{pango-rectangle} instance with logical
    extents
  @end{return}
  @begin{short}
    Gets the extents of the current run in layout coordinates (origin is the
    top left of the entire layout).
  @end{short}
  @see-class{pango-layout-iter}
  @see-symbol{pango-rectangle}"
  (with-foreign-objects ((ink-rect '(:pointer (:struct pango-rectangle)))
                         (logical-rect '(:pointer (:struct pango-rectangle))))
    (%pango-layout-iter-run-extents iter ink-rect logical-rect)
    (values ink-rect logical-rect)))

(export 'pango-layout-iter-run-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line_yrange () -> pango-layout-iter-line-yrange
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_line_yrange" %pango-layout-iter-line-yrange)
    :void
  (iter (g-boxed-foreign pango-layout-iter))
  (y0 (:pointer :int))
  (y1 (:pointer :int)))

(defun pango-layout-iter-line-yrange (iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @begin{return}
    @code{y0} -- an integer with the start of line, or NULL @br{}
    @code{y1} -- an integer with the end of line, or NULL
  @end{return}
  @begin{short}
    Divides the vertical space in the Pango layout being iterated over between
    the lines in the layout, and returns the space belonging to the current
    line.
  @end{short}
  A line's range includes the line's logical extents, plus half of the spacing
  above and below the line, if the function @fun{pango-layout-spacing} has been
  called to set layout spacing. The y positions are in layout coordinates
  (origin at top left of the entire layout).
  @begin[Note]{dictionary}
    Since 1.44, Pango uses line heights for placing lines, and there may be
    gaps between the ranges returned by this function.
  @end{dictionary}
  @see-class{pango-layout-iter}
  @see-function{pango-layout-spacing}"
  (with-foreign-objects ((y0 :int) (y1 :int))
    (%pango-layout-iter-line-yrange iter y0 y1)
    (values (mem-ref y0 :int)
            (mem-ref y1 :int))))

(export 'pango-layout-iter-line-yrange)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line_extents () -> pango-layout-iter-line-extents
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_line_extents" %pango-layout-iter-line-extents)
    :void
  (iter (g-boxed-foreign pango-layout-iter))
  (ink-rect (:pointer (:struct pango-rectangle)))
  (logical-rect (:pointer (:struct pango-rectangle))))

(defun pango-layout-iter-line-extents (iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @begin{return}
    @code{ink-rect} -- a @symbol{pango-rectangle} instance with ink extents
    @br{}
    @code{logical-rect} -- a @symbol{pango-rectangle} instance with logical
    extents
  @end{return}
  @begin{short}
    Obtains the extents of the current line.
  @end{short}
  @arg{ink-rect} or @arg{logical-rect} can be NULL if you are not interested in
  them. Extents are in layout coordinates (origin is the top-left corner of the
  entire Pango layout). Thus the extents returned by this function will be the
  same width/height but not at the same x/y as the extents returned from the
  function @fun{pango-layout-line-extents}.
  @see-class{pango-layout-iter}
  @see-symbol{pango-rectangle}
  @see-function{pango-layout-line-extents}"
  (with-foreign-objects ((ink-rect '(:pointer (:struct pango-rectangle)))
                         (logical-rect '(:pointer (:struct pango-rectangle))))
    (%pango-layout-iter-line-extents iter ink-rect logical-rect)
    (values ink-rect logical-rect)))

(export 'pango-layout-iter-line-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_layout_extents () -> pango-layout-iter-layout-extents
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_iter_get_layout_extents"
          %pango-layout-iter-layout-extents) :void
  (iter (g-boxed-foreign pango-layout-iter))
  (ink-rect (:pointer (:struct pango-rectangle)))
  (logical-rect (:pointer (:struct pango-rectangle))))

(defun pango-layout-iter-layout-extents (iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @begin{return}
    @code{ink-rect} -- a @symbol{pango-rectangle} instance with ink extents
    @br{}
    @code{logical-rect} -- a @symbol{pango-rectangle} instance with logical
    extents
  @end{return}
  @begin{short}
    Obtains the extents of the Pango layout being iterated over.
  @end{short}
  @arg{ink-rect} or @arg{logical-rect} can be NULL if you are not interested in
  them.
  @see-class{pango-layout-iter}
  @see-symbol{pango-rectangle}"
  (with-foreign-objects ((ink-rect '(:pointer (:struct pango-rectangle)))
                         (logical-rect '(:pointer (:struct pango-rectangle))))
    (%pango-layout-iter-layout-extents iter ink-rect logical-rect)
    (values ink-rect logical-rect)))

(export 'pango-layout-iter-layout-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_line_ref" pango-layout-line-ref)
    (g-boxed-foreign pango-layout-line)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[line]{a @class{pango-layout-line} instance, may be NULL}
  @return{A @class{pango-layout-line} instance with the line passed in.}
  @begin{short}
    Increase the reference count of a @class{pango-layout-line} instance by one.
  @end{short}
  @see-class{pango-layout-line}"
  (line (g-boxed-foreign pango-layout-line)))

(export 'pango-layout-line-ref)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_line_unref" pango-layout-line-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[line]{a @class{pango-layout-line} instance}
  @begin{short}
    Decrease the reference count of a @class{pango-layout-line} instance by one.
  @end{short}
  If the result is zero, the line and all associated memory will be freed.
  @see-class{pango-layout-line}"
  (line (g-boxed-foreign pango-layout-line)))

(export 'pango-layout-line-unref)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_extents () -> pango-layout-line-extents
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_line_get_extents" %pango-layout-line-extents) :void
  (line (g-boxed-foreign pango-layout-line))
  (ink-rect (:pointer (:struct pango-rectangle)))
  (logical-rect (:pointer (:struct pango-rectangle))))

(defun pango-layout-line-extents (line)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @begin{return}
    @code{ink-rect} -- a @symbol{pango-rectangle} instance with the extents of
    the glyph string as drawn, or NULL @br{}
    @code{logical-rect} -- a @symbol{pango-rectangle} instance with extents of
    the glyph string, or NULL
  @end{return}
  @begin{short}
    Computes the logical and ink extents of a layout line.
  @end{short}
  See the function @fun{pango-font-glyph-extents} for details about the
  interpretation of the rectangles.
  @see-class{pango-layout-line}
  @see-symbol{pango-rectangle}
  @see-function{pango-font-glyph-extents}"
  (with-foreign-objects ((ink-rect '(:pointer (:struct pango-rectangle)))
                         (logical-rect '(:pointer (:struct pango-rectangle))))
    (%pango-layout-line-extents line ink-rect logical-rect)
    (values ink-rect logical-rect)))

(export 'pango-layout-line-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_pixel_extents () -> pango-layout-line-pixel-extents
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_line_get_pixel_extents"
          %pango-layout-line-pixel-extents) :void
  (line (g-boxed-foreign pango-layout-line))
  (ink-rect (:pointer (:struct pango-rectangle)))
  (logical-rect (:pointer (:struct pango-rectangle))))

(defun pango-layout-line-pixel-extents (line)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @begin{return}
    @code{ink-rect} -- a @symbol{pango-rectangle} instance with the extents of
    the glyph string as drawn, or NULL @br{}
    @code{logical-rect} -- a @symbol{pango-rectangle} instance with extents of
    the glyph string, or NULL
  @end{return}
  @begin{short}
    Computes the logical and ink extents of @arg{line} in device units.
  @end{short}
  This function just calls the function @fun{pango-layout-line-extents}
  followed by two @fun{pango-extents-to-pixels} calls, rounding @arg{ink-rect}
  and @arg{logical-rect} such that the rounded rectangles fully contain the
  unrounded one (that is, passes them as first argument to the function
  @fun{pango-extents-to-pixels}).
  @see-class{pango-layout-line}
  @see-symbol{pango-rectangle}
  @see-function{pango-layout-line-extents}
  @see-function{pango-extents-to-pixels}"
  (with-foreign-objects ((ink-rect '(:pointer (:struct pango-rectangle)))
                         (logical-rect '(:pointer (:struct pango-rectangle))))
    (%pango-layout-line-pixel-extents line ink-rect logical-rect)
    (values ink-rect logical-rect)))

(export 'pango-layout-line-pixel-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_index_to_x ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_line_index_to_x" %pango-layout-line-index-to-x) :void
  (line (g-boxed-foreign pango-layout-line))
  (index :int)
  (trailing :boolean)
  (x-pos (:pointer :int)))

(defun pango-layout-line-index-to-x (line index trailing)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @argument[index]{an integer with the byte offset of a grapheme within the
    layout}
  @argument[trailing]{a boolean indicating the edge of the grapheme to retrieve
    the position of, if @em{true}, the trailing edge of the grapheme, if
    @em{false}, the leading of the grapheme}
  @begin{return}
    An integer with the x offset (in Pango unit).
  @end{return}
  @begin{short}
    Converts an index within a line to a x position.
  @end{short}
  @see-class{pango-layout-line}"
  (with-foreign-object (x-pos :int)
    (%pango-layout-line-index-to-x line index trailing x-pos)
    (values (mem-ref x-pos :int))))

(export 'pango-layout-line-index-to-x)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_x_to_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_layout_line_x_to_index" %pango-layout-line-x-to-index) :boolean
  (line  (g-boxed-foreign pango-layout-line))
  (x-pos :int)
  (index (:pointer :int))
  (trailing (:pointer :int)))

(defun pango-layout-line-x-to-index (line x-pos)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @argument[x-pos]{an integer with the x offset (in Pango units) from the left
    edge of the line}
  @begin{return}
    @code{index} -- an integer with the calculated byte index for the grapheme
    in which the user clicked @br{}
    @code{trailing} -- an integer indicating where in the grapheme the user
    clicked. It will either be zero, or the number of characters in the
    grapheme. 0 represents the leading edge of the grapheme.
    @code{bool} -- @em{false} if @arg{x-pos} was outside the line, @em{true}
    if inside
  @end{return}
  @begin{short}
    Converts from x offset to the byte index of the corresponding character
    within the text of the layout.
  @end{short}
  If @arg{x-pos} is outside the line, @arg{index} and @arg{trailing} will point
  to the very first or very last position in the line. This determination is
  based on the resolved direction of the paragraph; for example, if the resolved
  direction is right-to-left, then an x position to the right of the line (after
  it) results in 0 being stored in @arg{index} and @arg{trailing}. An x position
  to the left of the line results in @arg{index} pointing to the (logical) last
  grapheme in the line and trailing being set to the number of characters in
  that grapheme. The reverse is true for a left-to-right line.
  @see-class{pango-layout-line-x-to-index}"
  (with-foreign-objects ((index :int) (trailing :int))
    (let ((bool (%pango-layout-line-x-to-index line x-pos index trailing)))
      (values (mem-ref index :int)
              (mem-ref trailing :int)
              bool))))

(export 'pango-layout-line-x-to-index)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_x_ranges () -> pango-layout-line-x-ranges
;;; ----------------------------------------------------------------------------

;; TODO: Return a Lisp with the values.

(defcfun ("pango_layout_line_get_x_ranges" pango-layout-line-x-ranges) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-1-16}
  @argument[iter]{a @class{pango-layout-iter} instance}
  @argument[start-index]{an integer with the start byte index of the logical
    range. If this value is less than the start index for the line, then the
    first range will extend all the way to the leading edge of the layout.
    Otherwise it will start at the leading edge of the first character.}
  @argument[end-index]{an integer with the ending byte index of the logical
    range. If this value is greater than the end index for the line, then the
    last range will extend all the way to the trailing edge of the layout.
    Otherwise, it will end at the trailing edge of the last character.}
  @begin{return}
    @code{ranges} -- a pointer to an array of ranges. The array will be of
    length 2*n_ranges, with each range starting at (*ranges)[2*n] and of
    width (*ranges)[2*n + 1] - (*ranges)[2*n]. This array must be freed with
    @fun{g-free}. The coordinates are relative to the layout and are in
    Pango units @br{}
    @code{n-ranges} -- The number of ranges stored in ranges.
  @end{return}
  @begin{short}
    Gets a list of visual ranges corresponding to a given logical range.
  @end{short}
  This list is not necessarily minimal - there may be consecutive ranges which
  are adjacent. The ranges will be sorted from left to right. The ranges are
  with respect to the left edge of the entire layout, not with respect to the
  line.
  @see-class{pango-layout-line}"
  (line (g-boxed-foreign pango-layout-line))
  (start-index :int)
  (end-index :int)
  (ranges :pointer)
  (n-ranges (:pointer :int)))

(export 'pango-layout-line-x-ranges)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_height () -> pango-layout-line-height
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defcfun ("pango_layout_line_get_height" %pango-layout-line-height) :void
  (line (g-boxed-foreign pango-layout-line))
  (height (:pointer :int)))

#+pango-1-44
(defun pango-layout-line-height (line)
 #+cl-cffi-gtk-documentation
 "@version{2021-1-15}
  @argument[line]{a @class{pango-layout-line} instance}
  @return{An integer with the line height.}
  @begin{short}
    Computes the height of the line, i.e. the distance between this and the
    previous lines baseline.
  @end{short}

  Since 1.44
  @see-class{pango-layout-line}"
  (with-foreign-object (height :int)
    (%pango-layout-line-height line height)
    (values (mem-ref height :int))))

#+pango-1-44
(export 'pango-layout-line-height)

;;; --- End of file pango.layout.lisp ------------------------------------------
