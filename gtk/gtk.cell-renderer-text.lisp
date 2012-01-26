;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-text.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; --------------------------------------------------------------------------
;;;
;;; GtkCellRendererText
;;; 
;;; Renders text in a cell
;;; 	
;;; Synopsis
;;; 
;;;     GtkCellRendererText
;;;
;;;     gtk_cell_renderer_text_new
;;;     gtk_cell_renderer_text_set_fixed_height_from_font
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkCellRenderer
;;;                      +----GtkCellRendererText
;;;                            +----GtkCellRendererAccel
;;;                            +----GtkCellRendererCombo
;;;                            +----GtkCellRendererSpin
;;; 
;;; Properties
;;; 
;;;   "align-set"                gboolean              : Read / Write
;;;   "alignment"                PangoAlignment        : Read / Write
;;;   "attributes"               PangoAttrList*        : Read / Write
;;;   "background"               gchar*                : Write
;;;   "background-gdk"           GdkColor*             : Read / Write
;;;   "background-set"           gboolean              : Read / Write
;;;   "editable"                 gboolean              : Read / Write
;;;   "editable-set"             gboolean              : Read / Write
;;;   "ellipsize"                PangoEllipsizeMode    : Read / Write
;;;   "ellipsize-set"            gboolean              : Read / Write
;;;   "family"                   gchar*                : Read / Write
;;;   "family-set"               gboolean              : Read / Write
;;;   "font"                     gchar*                : Read / Write
;;;   "font-desc"                PangoFontDescription* : Read / Write
;;;   "foreground"               gchar*                : Write
;;;   "foreground-gdk"           GdkColor*             : Read / Write
;;;   "foreground-set"           gboolean              : Read / Write
;;;   "language"                 gchar*                : Read / Write
;;;   "language-set"             gboolean              : Read / Write
;;;   "markup"                   gchar*                : Write
;;;   "rise"                     gint                  : Read / Write
;;;   "rise-set"                 gboolean              : Read / Write
;;;   "scale"                    gdouble               : Read / Write
;;;   "scale-set"                gboolean              : Read / Write
;;;   "single-paragraph-mode"    gboolean              : Read / Write
;;;   "size"                     gint                  : Read / Write
;;;   "size-points"              gdouble               : Read / Write
;;;   "size-set"                 gboolean              : Read / Write
;;;   "stretch"                  PangoStretch          : Read / Write
;;;   "stretch-set"              gboolean              : Read / Write
;;;   "strikethrough"            gboolean              : Read / Write
;;;   "strikethrough-set"        gboolean              : Read / Write
;;;   "style"                    PangoStyle            : Read / Write
;;;   "style-set"                gboolean              : Read / Write
;;;   "text"                     gchar*                : Read / Write
;;;   "underline"                PangoUnderline        : Read / Write
;;;   "underline-set"            gboolean              : Read / Write
;;;   "variant"                  PangoVariant          : Read / Write
;;;   "variant-set"              gboolean              : Read / Write
;;;   "weight"                   gint                  : Read / Write
;;;   "weight-set"               gboolean              : Read / Write
;;;   "width-chars"              gint                  : Read / Write
;;;   "wrap-mode"                PangoWrapMode         : Read / Write
;;;   "wrap-width"               gint                  : Read / Write
;;; 
;;; Signals
;;; 
;;;   "edited"                                         : Run Last
;;; 
;;; Description
;;; 
;;; A GtkCellRendererText renders a given text in its cell, using the font,
;;; color and style information provided by its properties. The text will be
;;; ellipsized if it is too long and the ellipsize property allows it.
;;; 
;;; If the mode is GTK_CELL_RENDERER_MODE_EDITABLE, the GtkCellRendererText
;;; allows to edit its text using an entry.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "align-set" property
;;; 
;;;   "align-set"                gboolean              : Read / Write
;;; 
;;; Whether this tag affects the alignment mode.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "alignment" property
;;; 
;;;   "alignment"                PangoAlignment        : Read / Write
;;; 
;;; Specifies how to align the lines of text with respect to each other.
;;; 
;;; Note that this property describes how to align the lines of text in case
;;; there are several of them. The "xalign" property of GtkCellRenderer, on
;;; the other hand, sets the horizontal alignment of the whole text.
;;; 
;;; Default value: PANGO_ALIGN_LEFT
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "attributes" property
;;; 
;;;   "attributes"               PangoAttrList*        : Read / Write
;;; 
;;; A list of style attributes to apply to the text of the renderer.
;;;
;;; ----------------------------------------------------------------------------
;;; The "background" property
;;; 
;;;   "background"               gchar*                : Write
;;; 
;;; Background color as a string.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "background-gdk" property
;;; 
;;;   "background-gdk"           GdkColor*             : Read / Write
;;; 
;;; Background color as a GdkColor.
;;;
;;; ----------------------------------------------------------------------------
;;; The "background-set" property
;;; 
;;;   "background-set"           gboolean              : Read / Write
;;; 
;;; Whether this tag affects the background color.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "editable" property
;;; 
;;;   "editable"                 gboolean              : Read / Write
;;; 
;;; Whether the text can be modified by the user.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "editable-set" property
;;; 
;;;   "editable-set"             gboolean              : Read / Write
;;; 
;;; Whether this tag affects text editability.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "ellipsize" property
;;; 
;;;   "ellipsize"                PangoEllipsizeMode    : Read / Write
;;; 
;;; Specifies the preferred place to ellipsize the string, if the cell renderer
;;; does not have enough room to display the entire string. Setting it to
;;; PANGO_ELLIPSIZE_NONE turns off ellipsizing. See the wrap-width property for
;;; another way of making the text fit into a given width.
;;; 
;;; Default value: PANGO_ELLIPSIZE_NONE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "ellipsize-set" property
;;; 
;;;   "ellipsize-set"            gboolean              : Read / Write
;;; 
;;; Whether this tag affects the ellipsize mode.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "family" property
;;; 
;;;   "family"                   gchar*                : Read / Write
;;; 
;;; Name of the font family, e.g. Sans, Helvetica, Times, Monospace.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "family-set" property
;;; 
;;;   "family-set"               gboolean              : Read / Write
;;; 
;;; Whether this tag affects the font family.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "font" property
;;; 
;;;   "font"                     gchar*                : Read / Write
;;; 
;;; Font description as a string, e.g. "Sans Italic 12".
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "font-desc" property
;;; 
;;;   "font-desc"                PangoFontDescription*  : Read / Write
;;; 
;;; Font description as a PangoFontDescription struct.
;;;
;;; ----------------------------------------------------------------------------
;;; The "foreground" property
;;; 
;;;   "foreground"               gchar*                : Write
;;; 
;;; Foreground color as a string.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "foreground-gdk" property
;;; 
;;;   "foreground-gdk"           GdkColor*             : Read / Write
;;; 
;;; Foreground color as a GdkColor.
;;;
;;; ----------------------------------------------------------------------------
;;; The "foreground-set" property
;;; 
;;;   "foreground-set"           gboolean              : Read / Write
;;; 
;;; Whether this tag affects the foreground color.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "language" property
;;; 
;;;   "language"                 gchar*                : Read / Write
;;; 
;;; The language this text is in, as an ISO code. Pango can use this as a hint
;;; when rendering the text. If you don't understand this parameter, you
;;; probably don't need it.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "language-set" property
;;; 
;;;   "language-set"             gboolean              : Read / Write
;;; 
;;; Whether this tag affects the language the text is rendered as.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "markup" property
;;; 
;;;   "markup"                   gchar*                : Write
;;; 
;;; Marked up text to render.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "rise" property
;;; 
;;;   "rise"                     gint                  : Read / Write
;;; 
;;; Offset of text above the baseline (below the baseline if rise is negative).
;;; 
;;; Allowed values: >= -2147483647
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "rise-set" property
;;; 
;;;   "rise-set"                 gboolean              : Read / Write
;;; 
;;; Whether this tag affects the rise.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "scale" property
;;; 
;;;   "scale"                    gdouble               : Read / Write
;;; 
;;; Font scaling factor.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "scale-set" property
;;; 
;;;   "scale-set"                gboolean              : Read / Write
;;; 
;;; Whether this tag scales the font size by a factor.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "single-paragraph-mode" property
;;; 
;;;   "single-paragraph-mode"    gboolean              : Read / Write
;;; 
;;; Whether or not to keep all text in a single paragraph.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "size" property
;;; 
;;;   "size"                     gint                  : Read / Write
;;; 
;;; Font size.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "size-points" property
;;; 
;;;   "size-points"              gdouble               : Read / Write
;;; 
;;; Font size in points.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "size-set" property
;;; 
;;;   "size-set"                 gboolean              : Read / Write
;;; 
;;; Whether this tag affects the font size.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "stretch" property
;;; 
;;;   "stretch"                  PangoStretch          : Read / Write
;;; 
;;; Font stretch.
;;; 
;;; Default value: PANGO_STRETCH_NORMAL
;;;
;;; ----------------------------------------------------------------------------
;;; The "stretch-set" property
;;; 
;;;   "stretch-set"              gboolean              : Read / Write
;;; 
;;; Whether this tag affects the font stretch.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "strikethrough" property
;;; 
;;;   "strikethrough"            gboolean              : Read / Write
;;; 
;;; Whether to strike through the text.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "strikethrough-set" property
;;; 
;;;   "strikethrough-set"        gboolean              : Read / Write
;;; 
;;; Whether this tag affects strikethrough.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "style" property
;;; 
;;;   "style"                    PangoStyle            : Read / Write
;;; 
;;; Font style.
;;; 
;;; Default value: PANGO_STYLE_NORMAL
;;;
;;; ----------------------------------------------------------------------------
;;; The "style-set" property
;;; 
;;;   "style-set"                gboolean              : Read / Write
;;; 
;;; Whether this tag affects the font style.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "text" property
;;; 
;;;   "text"                     gchar*                : Read / Write
;;; 
;;; Text to render.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "underline" property
;;; 
;;;   "underline"                PangoUnderline        : Read / Write
;;; 
;;; Style of underline for this text.
;;; 
;;; Default value: PANGO_UNDERLINE_NONE
;;;
;;; ----------------------------------------------------------------------------
;;; The "underline-set" property
;;; 
;;;   "underline-set"            gboolean              : Read / Write
;;; 
;;; Whether this tag affects underlining.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "variant" property
;;; 
;;;   "variant"                  PangoVariant          : Read / Write
;;; 
;;; Font variant.
;;; 
;;; Default value: PANGO_VARIANT_NORMAL
;;;
;;; ----------------------------------------------------------------------------
;;; The "variant-set" property
;;; 
;;;   "variant-set"              gboolean              : Read / Write
;;; 
;;; Whether this tag affects the font variant.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "weight" property
;;; 
;;;   "weight"                   gint                  : Read / Write
;;; 
;;; Font weight.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 400
;;;
;;; ----------------------------------------------------------------------------
;;; The "weight-set" property
;;; 
;;;   "weight-set"               gboolean              : Read / Write
;;; 
;;; Whether this tag affects the font weight.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "width-chars" property
;;; 
;;;   "width-chars"              gint                  : Read / Write
;;; 
;;; The desired width of the cell, in characters. If this property is set to -1,
;;; the width will be calculated automatically, otherwise the cell will request
;;; either 3 characters or the property value, whichever is greater.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "wrap-mode" property
;;; 
;;;   "wrap-mode"                PangoWrapMode         : Read / Write
;;; 
;;; Specifies how to break the string into multiple lines, if the cell renderer
;;; does not have enough room to display the entire string. This property has
;;; no effect unless the wrap-width property is set.
;;; 
;;; Default value: PANGO_WRAP_CHAR
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "wrap-width" property
;;; 
;;;   "wrap-width"               gint                  : Read / Write
;;; 
;;; Specifies the width at which the text is wrapped. The wrap-mode property can
;;; be used to influence at what character positions the line breaks can be
;;; placed. Setting wrap-width to -1 turns wrapping off.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "edited" signal
;;; 
;;; void user_function (GtkCellRendererText *renderer,
;;;                     gchar               *path,
;;;                     gchar               *new_text,
;;;                     gpointer             user_data)      : Run Last
;;; 
;;; This signal is emitted after renderer has been edited.
;;; 
;;; It is the responsibility of the application to update the model and store
;;; new_text at the position indicated by path.
;;; 
;;; renderer :
;;; 	the object which received the signal
;;; 
;;; path :
;;; 	the path identifying the edited cell
;;; 
;;; new_text :
;;; 	the new text
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererText
;;; 
;;; struct GtkCellRendererText;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererText" gtk-cell-renderer-text
  (:superclass gtk-cell-renderer
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_text_get_type")
  ((align-set gtk-cell-renderer-text-align-set
    "align-set" "gboolean" t t)
   (alignment gtk-cell-renderer-text-alignment
    "alignment" "PangoAlignment" t t)
   (attributes gtk-cell-renderer-text-attributes
    "attributes" "PangoAttrList" t t)
   (background gtk-cell-renderer-text-background
    "background" "gchararray" nil t)
   (background-gdk gtk-cell-renderer-text-background-gdk
    "background-gdk" "GdkColor" t t)
   (background-set gtk-cell-renderer-text-background-set
    "background-set" "gboolean" t t)
   (editable gtk-cell-renderer-text-editable
    "editable" "gboolean" t t)
   (editable-set gtk-cell-renderer-text-editable-set
    "editable-set" "gboolean" t t)
   (ellipsize gtk-cell-renderer-text-ellipsize
    "ellipsize" "PangoEllipsizeMode" t t)
   (ellipsize-set gtk-cell-renderer-text-ellipsize-set
    "ellipsize-set" "gboolean" t t)
   (family gtk-cell-renderer-text-family
    "family" "gchararray" t t)
   (family-set gtk-cell-renderer-text-family-set
    "family-set" "gboolean" t t)
   (font gtk-cell-renderer-text-font
    "font" "gchararray" t t)
   (font-desc gtk-cell-renderer-text-font-desc
    "font-desc" "PangoFontDescription" t t)
   (foreground gtk-cell-renderer-text-foreground
    "foreground" "gchararray" nil t)
   (foreground-gdk gtk-cell-renderer-text-foreground-gdk
    "foreground-gdk" "GdkColor" t t)
   (foreground-set gtk-cell-renderer-text-foreground-set
    "foreground-set" "gboolean" t t)
   (language gtk-cell-renderer-text-language
    "language" "gchararray" t t)
   (language-set gtk-cell-renderer-text-language-set
    "language-set" "gboolean" t t)
   (markup gtk-cell-renderer-text-markup
    "markup" "gchararray" nil t)
   (rise gtk-cell-renderer-text-rise
    "rise" "gint" t t)
   (rise-set gtk-cell-renderer-text-rise-set
    "rise-set" "gboolean" t t)
   (scale gtk-cell-renderer-text-scale
    "scale" "gdouble" t t)
   (scale-set gtk-cell-renderer-text-scale-set
    "scale-set" "gboolean" t t)
   (single-paragraph-mode gtk-cell-renderer-text-single-paragraph-mode
    "single-paragraph-mode" "gboolean" t t)
   (size gtk-cell-renderer-text-size
    "size" "gint" t t)
   (size-points gtk-cell-renderer-text-size-points
    "size-points" "gdouble" t t)
   (size-set gtk-cell-renderer-text-size-set
    "size-set" "gboolean" t t)
   (stretch gtk-cell-renderer-text-stretch
    "stretch" "PangoStretch" t t)
   (stretch-set gtk-cell-renderer-text-stretch-set
    "stretch-set" "gboolean" t t)
   (strikethrough gtk-cell-renderer-text-strikethrough
    "strikethrough" "gboolean" t t)
   (strikethrough-set gtk-cell-renderer-text-strikethrough-set
    "strikethrough-set" "gboolean" t t)
   (style gtk-cell-renderer-text-style
    "style" "PangoStyle" t t)
   (style-set gtk-cell-renderer-text-style-set
    "style-set" "gboolean" t t)
   (text gtk-cell-renderer-text-text
    "text" "gchararray" t t)
   (underline gtk-cell-renderer-text-underline
    "underline" "PangoUnderline" t t)
   (underline-set gtk-cell-renderer-text-underline-set
    "underline-set" "gboolean" t t)
   (variant gtk-cell-renderer-text-variant
    "variant" "PangoVariant" t t)
   (variant-set gtk-cell-renderer-text-variant-set
    "variant-set" "gboolean" t t)
   (weight gtk-cell-renderer-text-weight
    "weight" "gint" t t)
   (weight-set gtk-cell-renderer-text-weight-set
    "weight-set" "gboolean" t t)
   (width-chars gtk-cell-renderer-text-width-chars
    "width-chars" "gint" t t)
   (wrap-mode gtk-cell-renderer-text-wrap-mode
    "wrap-mode" "PangoWrapMode" t t)
   (wrap-width gtk-cell-renderer-text-wrap-width
    "wrap-width" "gint" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_text_new ()
;;; 
;;; GtkCellRenderer * gtk_cell_renderer_text_new (void);
;;; 
;;; Creates a new GtkCellRendererText. Adjust how text is drawn using object
;;; properties. Object properties can be set globally (with g_object_set()).
;;; Also, with GtkTreeViewColumn, you can bind a property to a value in a
;;; GtkTreeModel. For example, you can bind the "text" property on the cell
;;; renderer to a string value in the model, thus rendering a different string
;;; in each row of the GtkTreeView
;;; 
;;; Returns :
;;; 	the new cell renderer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_text_set_fixed_height_from_font ()
;;; 
;;; void gtk_cell_renderer_text_set_fixed_height_from_font
;;;                                              (GtkCellRendererText *renderer,
;;;                                               gint number_of_rows);
;;; 
;;; Sets the height of a renderer to explicitly be determined by the "font" and
;;; "y_pad" property set on it. Further changes in these properties do not
;;; affect the height, so they must be accompanied by a subsequent call to this
;;; function. Using this function is unflexible, and should really only be used
;;; if calculating the size of a cell is too slow (ie, a massive number of
;;; cells displayed). If number_of_rows is -1, then the fixed height is unset,
;;; and the height is determined by the properties again.
;;; 
;;; renderer :
;;; 	A GtkCellRendererText
;;; 
;;; number_of_rows :
;;; 	Number of rows of text each cell renderer is allocated, or -1
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_text_set_fixed_height_from_font"
          gtk-cell-renderer-text-set-fixed-height-from-font) :void
  (renderer (g-object gtk-cell-renderer-text))
  (number-of-rows :int))

(export 'gtk-cell-renderer-text-set-fixed-height-from-font)

;;; --- End of file gtk.cell-renderer-text.lisp --------------------------------
