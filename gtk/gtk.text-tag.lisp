;;; ----------------------------------------------------------------------------
;;; gtk.text-tag.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkTextTag
;;; 
;;; A tag that can be applied to text in a GtkTextBuffer
;;; 	
;;; Synopsis
;;; 
;;;     GtkTextTag
;;;     GtkWrapMode
;;;     GtkTextAttributes
;;;
;;;     gtk_text_tag_new
;;;     gtk_text_tag_get_priority
;;;     gtk_text_tag_set_priority
;;;     gtk_text_tag_event
;;;
;;;     GtkTextAppearance
;;;
;;;     gtk_text_attributes_new
;;;     gtk_text_attributes_copy
;;;     gtk_text_attributes_copy_values
;;;     gtk_text_attributes_unref
;;;     gtk_text_attributes_ref
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkTextTag
;;; 
;;; Properties
;;; 
;;;   "accumulative-margin"      gboolean              : Read / Write
;;;   "background"               gchar*                : Write
;;;   "background-full-height"   gboolean              : Read / Write
;;;   "background-full-height-set" gboolean            : Read / Write
;;;   "background-gdk"           GdkColor*             : Read / Write
;;;   "background-rgba"          GdkRGBA*              : Read / Write
;;;   "background-set"           gboolean              : Read / Write
;;;   "direction"                GtkTextDirection      : Read / Write
;;;   "editable"                 gboolean              : Read / Write
;;;   "editable-set"             gboolean              : Read / Write
;;;   "family"                   gchar*                : Read / Write
;;;   "family-set"               gboolean              : Read / Write
;;;   "font"                     gchar*                : Read / Write
;;;   "font-desc"                PangoFontDescription*  : Read / Write
;;;   "foreground"               gchar*                : Write
;;;   "foreground-gdk"           GdkColor*             : Read / Write
;;;   "foreground-rgba"          GdkRGBA*              : Read / Write
;;;   "foreground-set"           gboolean              : Read / Write
;;;   "indent"                   gint                  : Read / Write
;;;   "indent-set"               gboolean              : Read / Write
;;;   "invisible"                gboolean              : Read / Write
;;;   "invisible-set"            gboolean              : Read / Write
;;;   "justification"            GtkJustification      : Read / Write
;;;   "justification-set"        gboolean              : Read / Write
;;;   "language"                 gchar*                : Read / Write
;;;   "language-set"             gboolean              : Read / Write
;;;   "left-margin"              gint                  : Read / Write
;;;   "left-margin-set"          gboolean              : Read / Write
;;;   "name"                     gchar*                : Read / Write /Construct
;;;   "paragraph-background"     gchar*                : Write
;;;   "paragraph-background-gdk" GdkColor*             : Read / Write
;;;   "paragraph-background-rgba" GdkRGBA*             : Read / Write
;;;   "paragraph-background-set" gboolean              : Read / Write
;;;   "pixels-above-lines"       gint                  : Read / Write
;;;   "pixels-above-lines-set"   gboolean              : Read / Write
;;;   "pixels-below-lines"       gint                  : Read / Write
;;;   "pixels-below-lines-set"   gboolean              : Read / Write
;;;   "pixels-inside-wrap"       gint                  : Read / Write
;;;   "pixels-inside-wrap-set"   gboolean              : Read / Write
;;;   "right-margin"             gint                  : Read / Write
;;;   "right-margin-set"         gboolean              : Read / Write
;;;   "rise"                     gint                  : Read / Write
;;;   "rise-set"                 gboolean              : Read / Write
;;;   "scale"                    gdouble               : Read / Write
;;;   "scale-set"                gboolean              : Read / Write
;;;   "size"                     gint                  : Read / Write
;;;   "size-points"              gdouble               : Read / Write
;;;   "size-set"                 gboolean              : Read / Write
;;;   "stretch"                  PangoStretch          : Read / Write
;;;   "stretch-set"              gboolean              : Read / Write
;;;   "strikethrough"            gboolean              : Read / Write
;;;   "strikethrough-set"        gboolean              : Read / Write
;;;   "style"                    PangoStyle            : Read / Write
;;;   "style-set"                gboolean              : Read / Write
;;;   "tabs"                     PangoTabArray*        : Read / Write
;;;   "tabs-set"                 gboolean              : Read / Write
;;;   "underline"                PangoUnderline        : Read / Write
;;;   "underline-set"            gboolean              : Read / Write
;;;   "variant"                  PangoVariant          : Read / Write
;;;   "variant-set"              gboolean              : Read / Write
;;;   "weight"                   gint                  : Read / Write
;;;   "weight-set"               gboolean              : Read / Write
;;;   "wrap-mode"                GtkWrapMode           : Read / Write
;;;   "wrap-mode-set"            gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "event"                                          : Run Last
;;; 
;;; Description
;;; 
;;; You may wish to begin by reading the text widget conceptual overview which
;;; gives an overview of all the objects and data types related to the text
;;; widget and how they work together.
;;; 
;;; Tags should be in the GtkTextTagTable for a given GtkTextBuffer before
;;; using them with that buffer.
;;; 
;;; gtk_text_buffer_create_tag() is the best way to create tags. See gtk3-demo
;;; for numerous examples.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "accumulative-margin" property
;;; 
;;;   "accumulative-margin"      gboolean              : Read / Write
;;; 
;;; Whether the margins accumulate or override each other.
;;; 
;;; When set to TRUE the margins of this tag are added to the margins of any
;;; other non-accumulative margins present. When set to FALSE the margins
;;; override one another (the default).
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.12
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
;;; The "background-full-height" property
;;; 
;;;   "background-full-height"   gboolean              : Read / Write
;;; 
;;; Whether the background color fills the entire line height or only the
;;; height of the tagged characters.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "background-full-height-set" property
;;; 
;;;   "background-full-height-set" gboolean              : Read / Write
;;; 
;;; Whether this tag affects background height.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "background-gdk" property
;;; 
;;;   "background-gdk"           GdkColor*             : Read / Write
;;; 
;;; Background color as a GdkColor.
;;;
;;; ----------------------------------------------------------------------------
;;; The "background-rgba" property
;;; 
;;;   "background-rgba"          GdkRGBA*              : Read / Write
;;; 
;;; Background color as a GdkRGBA.
;;; 
;;; Since 3.2
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
;;; The "direction" property
;;; 
;;;   "direction"                GtkTextDirection      : Read / Write
;;; 
;;; Text direction, e.g. right-to-left or left-to-right.
;;; 
;;; Default value: GTK_TEXT_DIR_NONE
;;;
;;; ----------------------------------------------------------------------------
;;; The "editable" property
;;; 
;;;   "editable"                 gboolean              : Read / Write
;;; 
;;; Whether the text can be modified by the user.
;;; 
;;; Default value: TRUE
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
;;; Font description as string, e.g. \"Sans Italic 12\".
;;; 
;;; Note that the initial value of this property depends on the internals of
;;; PangoFontDescription.
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
;;; The "foreground-rgba" property
;;; 
;;;   "foreground-rgba"          GdkRGBA*              : Read / Write
;;; 
;;; Foreground color as a GdkRGBA.
;;; 
;;; Since 3.2
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
;;; The "indent" property
;;; 
;;;   "indent"                   gint                  : Read / Write
;;; 
;;; Amount to indent the paragraph, in pixels.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "indent-set" property
;;; 
;;;   "indent-set"               gboolean              : Read / Write
;;; 
;;; Whether this tag affects indentation.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "invisible" property
;;; 
;;;   "invisible"                gboolean              : Read / Write
;;; 
;;; Whether this text is hidden.
;;; 
;;; Note that there may still be problems with the support for invisible text,
;;; in particular when navigating programmatically inside a buffer containing
;;; invisible segments.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "invisible-set" property
;;; 
;;;   "invisible-set"            gboolean              : Read / Write
;;; 
;;; Whether this tag affects text visibility.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "justification" property
;;; 
;;;   "justification"            GtkJustification      : Read / Write
;;; 
;;; Left, right, or center justification.
;;; 
;;; Default value: GTK_JUSTIFY_LEFT
;;;
;;; ----------------------------------------------------------------------------
;;; The "justification-set" property
;;; 
;;;   "justification-set"        gboolean              : Read / Write
;;; 
;;; Whether this tag affects paragraph justification.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "language" property
;;; 
;;;   "language"                 gchar*                : Read / Write
;;; 
;;; The language this text is in, as an ISO code. Pango can use this as a hint
;;; when rendering the text. If not set, an appropriate default will be used.
;;; 
;;; Note that the initial value of this property depends on the current locale,
;;; see also gtk_get_default_language().
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
;;; The "left-margin" property
;;; 
;;;   "left-margin"              gint                  : Read / Write
;;; 
;;; Width of the left margin in pixels.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "left-margin-set" property
;;; 
;;;   "left-margin-set"          gboolean              : Read / Write
;;; 
;;; Whether this tag affects the left margin.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "name" property
;;; 
;;;   "name"               gchar*                : Read / Write / Construct Only
;;; 
;;; Name used to refer to the text tag. NULL for anonymous tags.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "paragraph-background" property
;;; 
;;;   "paragraph-background"     gchar*                : Write
;;; 
;;; The paragraph background color as a string.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "paragraph-background-gdk" property
;;; 
;;;   "paragraph-background-gdk" GdkColor*             : Read / Write
;;; 
;;; The paragraph background color as a as a GdkColor.
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "paragraph-background-rgba" property
;;; 
;;;   "paragraph-background-rgba" GdkRGBA*              : Read / Write
;;; 
;;; The paragraph background color as a as a GdkRGBA.
;;; 
;;; Since 3.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "paragraph-background-set" property
;;; 
;;;   "paragraph-background-set" gboolean              : Read / Write
;;; 
;;; Whether this tag affects the paragraph background color.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixels-above-lines" property
;;; 
;;;   "pixels-above-lines"       gint                  : Read / Write
;;; 
;;; Pixels of blank space above paragraphs.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixels-above-lines-set" property
;;; 
;;;   "pixels-above-lines-set"   gboolean              : Read / Write
;;; 
;;; Whether this tag affects the number of pixels above lines.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixels-below-lines" property
;;; 
;;;   "pixels-below-lines"       gint                  : Read / Write
;;; 
;;; Pixels of blank space below paragraphs.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixels-below-lines-set" property
;;; 
;;;   "pixels-below-lines-set"   gboolean              : Read / Write
;;; 
;;; Whether this tag affects the number of pixels above lines.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixels-inside-wrap" property
;;; 
;;;   "pixels-inside-wrap"       gint                  : Read / Write
;;; 
;;; Pixels of blank space between wrapped lines in a paragraph.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixels-inside-wrap-set" property
;;; 
;;;   "pixels-inside-wrap-set"   gboolean              : Read / Write
;;; 
;;; Whether this tag affects the number of pixels between wrapped lines.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "right-margin" property
;;; 
;;;   "right-margin"             gint                  : Read / Write
;;; 
;;; Width of the right margin in pixels.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "right-margin-set" property
;;; 
;;;   "right-margin-set"         gboolean              : Read / Write
;;; 
;;; Whether this tag affects the right margin.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "rise" property
;;; 
;;;   "rise"                     gint                  : Read / Write
;;; 
;;; Offset of text above the baseline (below the baseline if rise is negative)
;;; in Pango units.
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
;;; Font size as a scale factor relative to the default font size. This
;;; properly adapts to theme changes etc. so is recommended. Pango predefines
;;; some scales such as PANGO_SCALE_X_LARGE.
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
;;; The "size" property
;;; 
;;;   "size"                     gint                  : Read / Write
;;; 
;;; Font size in Pango units.
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
;;; Font stretch as a PangoStretch, e.g. PANGO_STRETCH_CONDENSED.
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
;;; Font style as a PangoStyle, e.g. PANGO_STYLE_ITALIC.
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
;;; The "tabs" property
;;; 
;;;   "tabs"                     PangoTabArray*        : Read / Write
;;; 
;;; Custom tabs for this text.
;;; The "tabs-set" property
;;; 
;;;   "tabs-set"                 gboolean              : Read / Write
;;; 
;;; Whether this tag affects tabs.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "underline" property
;;; 
;;;   "underline"                PangoUnderline        : Read / Write
;;; 
;;; Style of underline for this text.
;;; 
;;; Default value: PANGO_UNDERLINE_NONE
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
;;; Font variant as a PangoVariant, e.g. PANGO_VARIANT_SMALL_CAPS.
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
;;; Font weight as an integer, see predefined values in PangoWeight; for
;;; example, PANGO_WEIGHT_BOLD.
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
;;; The "wrap-mode" property
;;; 
;;;   "wrap-mode"                GtkWrapMode           : Read / Write
;;; 
;;; Whether to wrap lines never, at word boundaries, or at character boundaries.
;;; 
;;; Default value: GTK_WRAP_NONE
;;;
;;; ----------------------------------------------------------------------------
;;; The "wrap-mode-set" property
;;; 
;;;   "wrap-mode-set"            gboolean              : Read / Write
;;; 
;;; Whether this tag affects line wrap mode.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "event" signal
;;; 
;;; gboolean user_function (GtkTextTag  *tag,
;;;                         GObject     *object,
;;;                         GdkEvent    *event,
;;;                         GtkTextIter *iter,
;;;                         gpointer     user_data)      : Run Last
;;; 
;;; The ::event signal is emitted when an event occurs on a region of the
;;; buffer marked with this tag.
;;; 
;;; tag :
;;; 	the GtkTextTag on which the signal is emitted
;;; 
;;; object :
;;; 	the object the event was fired from (typically a GtkTextView)
;;; 
;;; event :
;;; 	the event which triggered the signal
;;; 
;;; iter :
;;; 	a GtkTextIter pointing at the location the event occured
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; 
;;; Returns :
;;; 	TRUE to stop other handlers from being invoked for the event.
;;;     FALSE to propagate the event further.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTextTag
;;; 
;;; struct GtkTextTag;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTextTag" gtk-text-tag
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_text_tag_get_type")
  ((accumulative-margin
    gtk-text-tag-accumulative-margin
    "accumulative-margin" "gboolean" t t)
   (background
    gtk-text-tag-background
    "background" "gchararray" nil t)
   (background-full-height
    gtk-text-tag-background-full-height
    "background-full-height" "gboolean" t t)
   (background-full-height-set
    gtk-text-tag-background-full-height-set
    "background-full-height-set" "gboolean" t t)
   (background-gdk
    gtk-text-tag-background-gdk
    "background-gdk" "GdkColor" t t)
   (background-set
    gtk-text-tag-background-set
    "background-set" "gboolean" t t)
   (background-stipple
    gtk-text-tag-background-stipple
    "background-stipple" "GdkPixmap" t t)
   (background-stipple-set
    gtk-text-tag-background-stipple-set
    "background-stipple-set" "gboolean" t t)
   (direction
    gtk-text-tag-direction
    "direction" "GtkTextDirection" t t)
   (editable
    gtk-text-tag-editable
    "editable" "gboolean" t t)
   (editable-set
    gtk-text-tag-editable-set
    "editable-set" "gboolean" t t)
   (family
    gtk-text-tag-family
    "family" "gchararray" t t)
   (family-set
    gtk-text-tag-family-set
    "family-set" "gboolean" t t)
   (font
    gtk-text-tag-font
    "font" "gchararray" t t)
   (font-desc
    gtk-text-tag-font-desc
    "font-desc" "PangoFontDescription" t t)
   (foreground
    gtk-text-tag-foreground
    "foreground" "gchararray" nil t)
   (foreground-gdk
    gtk-text-tag-foreground-gdk
    "foreground-gdk" "GdkColor" t t)
   (foreground-set
    gtk-text-tag-foreground-set
    "foreground-set" "gboolean" t t)
   (foreground-stipple
    gtk-text-tag-foreground-stipple
    "foreground-stipple" "GdkPixmap" t t)
   (foreground-stipple-set
    gtk-text-tag-foreground-stipple-set
    "foreground-stipple-set" "gboolean" t t)
   (indent
    gtk-text-tag-indent
    "indent" "gint" t t)
   (indent-set
    gtk-text-tag-indent-set
    "indent-set" "gboolean" t t)
   (invisible
    gtk-text-tag-invisible
    "invisible" "gboolean" t t)
   (invisible-set
    gtk-text-tag-invisible-set
    "invisible-set" "gboolean" t t)
   (justification
    gtk-text-tag-justification
    "justification" "GtkJustification" t t)
   (justification-set
    gtk-text-tag-justification-set
    "justification-set" "gboolean" t t)
   (language
    gtk-text-tag-language
    "language" "gchararray" t t)
   (language-set
    gtk-text-tag-language-set
    "language-set" "gboolean" t t)
   (left-margin
    gtk-text-tag-left-margin
    "left-margin" "gint" t t)
   (left-margin-set
    gtk-text-tag-left-margin-set
    "left-margin-set" "gboolean" t t)
   (name
    gtk-text-tag-name
    "name" "gchararray" t nil)
   (paragraph-background
    gtk-text-tag-paragraph-background
    "paragraph-background" "gchararray" nil t)
   (paragraph-background-gdk
    gtk-text-tag-paragraph-background-gdk
    "paragraph-background-gdk" "GdkColor" t t)
   (paragraph-background-set
    gtk-text-tag-paragraph-background-set
    "paragraph-background-set" "gboolean" t t)
   (pixels-above-lines
    gtk-text-tag-pixels-above-lines
    "pixels-above-lines" "gint" t t)
   (pixels-above-lines-set
    gtk-text-tag-pixels-above-lines-set
    "pixels-above-lines-set" "gboolean" t t)
   (pixels-below-lines
    gtk-text-tag-pixels-below-lines
    "pixels-below-lines" "gint" t t)
   (pixels-below-lines-set
    gtk-text-tag-pixels-below-lines-set
    "pixels-below-lines-set" "gboolean" t t)
   (pixels-inside-wrap
    gtk-text-tag-pixels-inside-wrap
    "pixels-inside-wrap" "gint" t t)
   (pixels-inside-wrap-set
    gtk-text-tag-pixels-inside-wrap-set
    "pixels-inside-wrap-set" "gboolean" t t)
   (right-margin
    gtk-text-tag-right-margin
    "right-margin" "gint" t t)
   (right-margin-set
    gtk-text-tag-right-margin-set
    "right-margin-set" "gboolean" t t)
   (rise
    gtk-text-tag-rise
    "rise" "gint" t t)
   (rise-set
    gtk-text-tag-rise-set
    "rise-set" "gboolean" t t)
   (scale
    gtk-text-tag-scale
    "scale" "gdouble" t t)
   (scale-set
    gtk-text-tag-scale-set
    "scale-set" "gboolean" t t)
   (size
    gtk-text-tag-size
    "size" "gint" t t)
   (size-points
    gtk-text-tag-size-points
    "size-points" "gdouble" t t)
   (size-set
    gtk-text-tag-size-set
    "size-set" "gboolean" t t)
   (stretch
    gtk-text-tag-stretch
    "stretch" "PangoStretch" t t)
   (stretch-set
    gtk-text-tag-stretch-set
    "stretch-set" "gboolean" t t)
   (strikethrough
    gtk-text-tag-strikethrough
    "strikethrough" "gboolean" t t)
   (strikethrough-set
    gtk-text-tag-strikethrough-set
    "strikethrough-set" "gboolean" t t)
   (style
    gtk-text-tag-style
    "style" "PangoStyle" t t)
   (style-set
    gtk-text-tag-style-set
    "style-set" "gboolean" t t)
   (tabs
    gtk-text-tag-tabs
    "tabs" "PangoTabArray" t t)
   (tabs-set
    gtk-text-tag-tabs-set
    "tabs-set" "gboolean" t t)
   (underline
    gtk-text-tag-underline
    "underline" "PangoUnderline" t t)
   (underline-set
    gtk-text-tag-underline-set
    "underline-set" "gboolean" t t)
   (variant
    gtk-text-tag-variant
    "variant" "PangoVariant" t t)
   (variant-set
    gtk-text-tag-variant-set
    "variant-set" "gboolean" t t)
   (weight
    gtk-text-tag-weight
    "weight" "gint" t t)
   (weight-set
    gtk-text-tag-weight-set
    "weight-set" "gboolean" t t)
   (wrap-mode
    gtk-text-tag-wrap-mode
    "wrap-mode" "GtkWrapMode" t t)
   (wrap-mode-set
    gtk-text-tag-wrap-mode-set
    "wrap-mode-set" "gboolean" t t)
   (:cffi priority
          gtk-text-tag-priority :int
          "gtk_text_tag_get_priority" "gtk_text_tag_set_priority")))

;;; ----------------------------------------------------------------------------
;;; enum GtkWrapMode
;;; 
;;; typedef enum {
;;;   GTK_WRAP_NONE,
;;;   GTK_WRAP_CHAR,
;;;   GTK_WRAP_WORD,
;;;   GTK_WRAP_WORD_CHAR
;;; } GtkWrapMode;
;;; 
;;; Describes a type of line wrapping.
;;; 
;;; GTK_WRAP_NONE
;;; 	do not wrap lines; just make the text area wider
;;; 
;;; GTK_WRAP_CHAR
;;; 	wrap text, breaking lines anywhere the cursor can appear (between
;;;     characters, usually - if you want to be technical, between graphemes,
;;;     see pango_get_log_attrs())
;;; 
;;; GTK_WRAP_WORD
;;; 	wrap text, breaking lines in between words
;;; 
;;; GTK_WRAP_WORD_CHAR
;;; 	wrap text, breaking lines in between words, or if that is not enough,
;;;     also between graphemes
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkWrapMode" gtk-wrap-mode
  (:export t
   :type-initializer "gtk_wrap_mode_get_type")
  (:none 0)
  (:char 1)
  (:word 2)
  (:word-char 3))

;;; ----------------------------------------------------------------------------
;;; struct GtkTextAttributes
;;; 
;;; struct GtkTextAttributes {
;;;   GtkTextAppearance appearance;
;;; 
;;;   GtkJustification justification;
;;;   GtkTextDirection direction;
;;; 
;;;   /* Individual chunks of this can be set/unset as a group */
;;;   PangoFontDescription *font;
;;; 
;;;   gdouble font_scale;
;;; 
;;;   gint left_margin;
;;;   gint right_margin;
;;;   gint indent;
;;; 
;;;   gint pixels_above_lines;
;;;   gint pixels_below_lines;
;;;   gint pixels_inside_wrap;
;;; 
;;;   PangoTabArray *tabs;
;;; 
;;;   GtkWrapMode wrap_mode;        /* How to handle wrap-around for this tag.
;;;                                  * Must be GTK_WRAPMODE_CHAR,
;;;                                  * GTK_WRAPMODE_NONE, GTK_WRAPMODE_WORD
;;;                                  */
;;; 
;;;   PangoLanguage *language;
;;; 
;;;   /* hide the text  */
;;;   guint invisible : 1;
;;; 
;;;   /* Background is fit to full line height rather than
;;;    * baseline +/- ascent/descent (font height)
;;;    */
;;;   guint bg_full_height : 1;
;;; 
;;;   /* can edit this text */
;;;   guint editable : 1;
;;; };
;;; 
;;; Using GtkTextAttributes directly should rarely be necessary. It's primarily
;;; useful with gtk_text_iter_get_attributes(). As with most GTK+ structs, the
;;; fields in this struct should only be read, never modified directly.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_new ()
;;; 
;;; GtkTextTag * gtk_text_tag_new (const gchar *name);
;;; 
;;; Creates a GtkTextTag. Configure the tag using object arguments, i.e. using
;;; g_object_set().
;;; 
;;; name :
;;; 	tag name, or NULL. [allow-none]
;;; 
;;; Returns :
;;; 	a new GtkTextTag
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_get_priority ()
;;; 
;;; gint gtk_text_tag_get_priority (GtkTextTag *tag);
;;; 
;;; Get the tag priority.
;;; 
;;; tag :
;;; 	a GtkTextTag
;;; 
;;; Returns :
;;; 	The tag's priority
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_set_priority ()
;;; 
;;; void gtk_text_tag_set_priority (GtkTextTag *tag, gint priority);
;;; 
;;; Sets the priority of a GtkTextTag. Valid priorities are start at 0 and go
;;; to one less than gtk_text_tag_table_get_size(). Each tag in a table has a
;;; unique priority; setting the priority of one tag shifts the priorities of
;;; all the other tags in the table to maintain a unique priority for each tag.
;;; Higher priority tags "win" if two tags both set the same text attribute.
;;; When adding a tag to a tag table, it will be assigned the highest priority
;;; in the table by default; so normally the precedence of a set of tags is the
;;; order in which they were added to the table, or created with
;;; gtk_text_buffer_create_tag(), which adds the tag to the buffer's table
;;; automatically.
;;; 
;;; tag :
;;; 	a GtkTextTag
;;; 
;;; priority :
;;; 	the new priority
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_event ()
;;; 
;;; gboolean gtk_text_tag_event (GtkTextTag *tag,
;;;                              GObject *event_object,
;;;                              GdkEvent *event,
;;;                              const GtkTextIter *iter);
;;; 
;;; Emits the "event" signal on the GtkTextTag.
;;; 
;;; tag :
;;; 	a GtkTextTag
;;; 
;;; event_object :
;;; 	object that received the event, such as a widget
;;; 
;;; event :
;;; 	the event
;;; 
;;; iter :
;;; 	location where the event was received
;;; 
;;; Returns :
;;; 	result of signal emission (whether the event was handled)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkTextAppearance
;;; 
;;; struct GtkTextAppearance {
;;;   GdkColor bg_color;
;;;   GdkColor fg_color;
;;; 
;;;   /* super/subscript rise, can be negative */
;;;   gint rise;
;;; 
;;;   guint underline : 4;          /* PangoUnderline */
;;;   guint strikethrough : 1;
;;; 
;;;   /* Whether to use background-related values; this is irrelevant for
;;;    * the values struct when in a tag, but is used for the composite
;;;    * values struct; it's true if any of the tags being composited
;;;    * had background stuff set.
;;;    */
;;;   guint draw_bg : 1;
;;; 
;;;   /* These are only used when we are actually laying out and rendering
;;;    * a paragraph; not when a GtkTextAppearance is part of a
;;;    * GtkTextAttributes.
;;;    */
;;;   guint inside_selection : 1;
;;;   guint is_text : 1;
;;; 
;;;   GdkRGBA *rgba[2];
;;; 
;;; #if __SIZEOF_INT__ == __SIZEOF_POINTER__
;;;   /* unusable, just for ABI compat */
;;;   guint padding[2];
;;; #endif
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_new ()
;;; 
;;; GtkTextAttributes * gtk_text_attributes_new (void);
;;; 
;;; Creates a GtkTextAttributes, which describes a set of properties on some
;;; text.
;;; 
;;; Returns :
;;; 	a new GtkTextAttributes, free with gtk_text_attributes_unref().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_copy ()
;;; 
;;; GtkTextAttributes * gtk_text_attributes_copy (GtkTextAttributes *src);
;;; 
;;; Copies src and returns a new GtkTextAttributes.
;;; 
;;; src :
;;; 	a GtkTextAttributes to be copied
;;; 
;;; Returns :
;;; 	a copy of src, free with gtk_text_attributes_unref()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_copy_values ()
;;; 
;;; void gtk_text_attributes_copy_values (GtkTextAttributes *src,
;;;                                       GtkTextAttributes *dest);
;;; 
;;; Copies the values from src to dest so that dest has the same values as src.
;;; Frees existing values in dest.
;;; 
;;; src :
;;; 	a GtkTextAttributes
;;; 
;;; dest :
;;; 	another GtkTextAttributes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_unref ()
;;; 
;;; void gtk_text_attributes_unref (GtkTextAttributes *values);
;;; 
;;; Decrements the reference count on values, freeing the structure if the
;;; reference count reaches 0.
;;; 
;;; values :
;;; 	a GtkTextAttributes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_ref ()
;;; 
;;; GtkTextAttributes * gtk_text_attributes_ref (GtkTextAttributes *values);
;;; 
;;; Increments the reference count on values.
;;; 
;;; values :
;;; 	a GtkTextAttributes
;;; 
;;; Returns :
;;; 	the GtkTextAttributes that were passed in
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.text-tag.lisp ------------------------------------------
