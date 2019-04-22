;;; ----------------------------------------------------------------------------
;;; gtk.text-tag.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     A tag that can be applied to text in a GtkTextBuffer
;;;
;;; Types and Values
;;;
;;;     GtkTextTag
;;;     GtkTextAttributes
;;;     GtkTextAppearance
;;;
;;;     GtkWrapMode          ---> GtkTextView ?
;;;
;;; Functions
;;;
;;;     gtk_text_tag_new
;;;     gtk_text_tag_get_priority
;;;     gtk_text_tag_set_priority
;;;     gtk_text_tag_event
;;;     gtk_text_tag_changed
;;;
;;;     gtk_text_attributes_new
;;;     gtk_text_attributes_copy
;;;     gtk_text_attributes_copy_values
;;;     gtk_text_attributes_unref
;;;     gtk_text_attributes_ref
;;;
;;; Properties
;;;
;;;             gboolean   accumulative-margin          Read / Write
;;;                gchar*  background                   Write
;;;             gboolean   background-full-height       Read / Write
;;;             gboolean   background-full-height-set   Read / Write
;;;             GdkColor*  background-gdk               Read / Write
;;;              GdkRGBA*  background-rgba              Read / Write
;;;             gboolean   background-set               Read / Write
;;;     GtkTextDirection   direction                    Read / Write
;;;             gboolean   editable                     Read / Write
;;;             gboolean   editable-set                 Read / Write
;;;             gboolean   fallback                     Read / Write
;;;             gboolean   fallback-set                 Read / Write
;;;                gchar*  family                       Read / Write
;;;             gboolean   family-set                   Read / Write
;;;                gchar*  font                         Read / Write
;;; PangoFontDescription*  font-desc                    Read / Write
;;;                gchar*  font-features                Read / Write
;;;             gboolean   font-features-set            Read / Write
;;;                gchar*  foreground                   Write
;;;             GdkColor*  foreground-gdk               Read / Write
;;;              GdkRGBA*  foreground-rgba              Read / Write
;;;             gboolean   foreground-set               Read / Write
;;;                 gint   indent                       Read / Write
;;;             gboolean   indent-set                   Read / Write
;;;             gboolean   invisible                    Read / Write
;;;             gboolean   invisible-set                Read / Write
;;;     GtkJustification   justification                Read / Write
;;;             gboolean   justification-set            Read / Write
;;;                gchar*  language                     Read / Write
;;;             gboolean   language-set                 Read / Write
;;;                 gint   left-margin                  Read / Write
;;;             gboolean   left-margin-set              Read / Write
;;;                 gint   letter-spacing               Read / Write
;;;             gboolean   letter-spacing-set           Read / Write
;;;                gchar*  name                         Read / Write / Construct
;;;                gchar*  paragraph-background         Write
;;;             GdkColor*  paragraph-background-gdk     Read / Write
;;;              GdkRGBA*  paragraph-background-rgba    Read / Write
;;;             gboolean   paragraph-background-set     Read / Write
;;;                 gint   pixels-above-lines           Read / Write
;;;             gboolean   pixels-above-lines-set       Read / Write
;;;                 gint   pixels-below-lines           Read / Write
;;;             gboolean   pixels-below-lines-set       Read / Write
;;;                 gint   pixels-inside-wrap           Read / Write
;;;             gboolean   pixels-inside-wrap-set       Read / Write
;;;                 gint   right-margin                 Read / Write
;;;             gboolean   right-margin-set             Read / Write
;;;                 gint   rise                         Read / Write
;;;             gboolean   rise-set                     Read / Write
;;;              gdouble   scale                        Read / Write
;;;             gboolean   scale-set                    Read / Write
;;;                 gint   size                         Read / Write
;;;              gdouble   size-points                  Read / Write
;;;             gboolean   size-set                     Read / Write
;;;         PangoStretch   stretch                      Read / Write
;;;             gboolean   stretch-set                  Read / Write
;;;             gboolean   strikethrough                Read / Write
;;;              GdkRGBA*  strikethrough-rgba           Read / Write
;;;             gboolean   strikethrough-rgba-set       Read / Write
;;;             gboolean   strikethrough-set            Read / Write
;;;           PangoStyle   style                        Read / Write
;;;             gboolean   style-set                    Read / Write
;;;        PangoTabArray*  tabs                         Read / Write
;;;             gboolean   tabs-set                     Read / Write
;;;       PangoUnderline   underline                    Read / Write
;;;              GdkRGBA*  underline-rgba               Read / Write
;;;             gboolean   underline-rgba-set           Read / Write
;;;             gboolean   underline-set                Read / Write
;;;         PangoVariant   variant                      Read / Write
;;;             gboolean   variant-set                  Read / Write
;;;                 gint   weight                       Read / Write
;;;             gboolean   weight-set                   Read / Write
;;;          GtkWrapMode   wrap-mode                    Read / Write
;;;             gboolean   wrap-mode-set                Read / Write
;;;
;;; Signals
;;;
;;;             gboolean   event                        Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTextTag
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTextTag
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
   (background-rgba
    gtk-text-tag-background-rgba
    "background-rgba" "GdkRGBA" t t)
   (background-set
    gtk-text-tag-background-set
    "background-set" "gboolean" t t)
   (direction
    gtk-text-tag-direction
    "direction" "GtkTextDirection" t t)
   (editable
    gtk-text-tag-editable
    "editable" "gboolean" t t)
   (editable-set
    gtk-text-tag-editable-set
    "editable-set" "gboolean" t t)
   #+gtk-3-16
   (fallback
    gtk-text-tag-fallback
    "fallback" "gboolean" t t)
   #+gtk-3-16
   (fallback-set
    gtk-text-tag-fallback-set
    "fallback-set" "gboolean" t t)
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
   #+gtk-3-18
   (font-features
    gtk-text-tag-font-features
    "font-features" "gchar" t t)
   #+gtk-3-18
   (font-features-set
    gtk-text-tag-font-features-set
    "font-features-set" "gboolean" t t)
   (foreground
    gtk-text-tag-foreground
    "foreground" "gchararray" nil t)
   (foreground-gdk
    gtk-text-tag-foreground-gdk
    "foreground-gdk" "GdkColor" t t)
   (foreground-rgba
    gtk-text-tag-foreground-rgba
    "foreground-rgba" "GdkRGBA" t t)
   (foreground-set
    gtk-text-tag-foreground-set
    "foreground-set" "gboolean" t t)
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
   #+gtk-3-16
   (letter-spacing
    gtk-text-tag-letter-spacing
    "letter-spacing" "gint" t t)
   #+gtk-3-16
   (letter-spacing-set
    gtk-text-tag-letter-spacing-set
    "letter-spacing-set" "gboolean" t t)
   (name
    gtk-text-tag-name
    "name" "gchararray" t nil)
   (paragraph-background
    gtk-text-tag-paragraph-background
    "paragraph-background" "gchararray" nil t)
   (paragraph-background-gdk
    gtk-text-tag-paragraph-background-gdk
    "paragraph-background-gdk" "GdkColor" t t)
   (paragraph-background-rgba
    gtk-text-tag-paragraph-background-rgba
    "paragraph-background-rgba" "GdkRGBA" t t)
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
   #+gtk-3-16
   (strikethrough-rgba
    gtk-text-tag-strikethrough-rgba
    "strikethrough-rgba" "GdkRGBA" t t)
   #+gtk-3-16
   (strikethrough-rgba-set
    gtk-text-tag-strikethrough-rgba-set
    "strikethrough-rgba-set" "gboolean" t t)
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
   #+gtk-3-16
   (underline-rgba
    gtk-text-tag-underline-rgba
    "underline-rgba" "GdkRGBA" t t)
   #+gtk-3-16
   (underline-rgba-set
    gtk-text-tag-underline-rgba-set
    "underline-rgba-set" "gboolean" t t)
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
    "wrap-mode-set" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-text-tag 'type)
 "@version{2013-5-5}
  @begin{short}
    You may wish to begin by reading the text widget conceptual overview which
    gives an overview of all the objects and data types related to the text
    widget and how they work together.
  @end{short}

  Tags should be in the @class{gtk-text-tag-table} object for a given
  @class{gtk-text-buffer} object before using them with that buffer.

  The function @fun{gtk-text-buffer-create-tag} is the best way to create tags.
  See gtk3-demo for numerous examples.
  @begin[Signal Details]{dictionary}
    @subheading{The \"event\" signal}
      @begin{pre}
 lambda (tag object event iter)    : Run Last
      @end{pre}
      The \"event\" signal is emitted when an event occurs on a region of the
      buffer marked with this tag.
      @begin[code]{table}
        @entry[tag]{The @class{gtk-text-tag} object on which the signal is
          emitted.}
        @entry[object]{The object the event was fired from (typically a
          @class{gtk-text-view} object).}
        @entry[event]{The event which triggered the signal.}
        @entry[iter]{A @class{gtk-text-iter} pointing at the location the event
          occured.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @code{Nil} to propagate the event further.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-text-tag-accumulative-margin}
  @see-slot{gtk-text-tag-background}
  @see-slot{gtk-text-tag-background-full-height}
  @see-slot{gtk-text-tag-background-full-height-set}
  @see-slot{gtk-text-tag-background-gdk}
  @see-slot{gtk-text-tag-background-rgba}
  @see-slot{gtk-text-tag-background-set}
  @see-slot{gtk-text-tag-direction}
  @see-slot{gtk-text-tag-editable}
  @see-slot{gtk-text-tag-editable-set}
  @see-slot{gtk-text-tag-family}
  @see-slot{gtk-text-tag-family-set}
  @see-slot{gtk-text-tag-font}
  @see-slot{gtk-text-tag-font-desc}
  @see-slot{gtk-text-tag-foreground}
  @see-slot{gtk-text-tag-foreground-gdk}
  @see-slot{gtk-text-tag-foreground-rgba}
  @see-slot{gtk-text-tag-foreground-set}
  @see-slot{gtk-text-tag-indent}
  @see-slot{gtk-text-tag-indent-set}
  @see-slot{gtk-text-tag-invisible}
  @see-slot{gtk-text-tag-invisible-set}
  @see-slot{gtk-text-tag-justification}
  @see-slot{gtk-text-tag-justification-set}
  @see-slot{gtk-text-tag-language}
  @see-slot{gtk-text-tag-language-set}
  @see-slot{gtk-text-tag-left-margin}
  @see-slot{gtk-text-tag-left-margin-set}
  @see-slot{gtk-text-tag-name}
  @see-slot{gtk-text-tag-paragraph-background}
  @see-slot{gtk-text-tag-paragraph-background-gdk}
  @see-slot{gtk-text-tag-paragraph-background-rgba}
  @see-slot{gtk-text-tag-paragraph-background-set}
  @see-slot{gtk-text-tag-pixels-above-lines}
  @see-slot{gtk-text-tag-pixels-above-lines-set}
  @see-slot{gtk-text-tag-pixels-below-lines}
  @see-slot{gtk-text-tag-pixels-below-lines-set}
  @see-slot{gtk-text-tag-pixels-inside-wrap}
  @see-slot{gtk-text-tag-pixels-inside-wrap-set}
  @see-slot{gtk-text-tag-right-margin}
  @see-slot{gtk-text-tag-right-margin-set}
  @see-slot{gtk-text-tag-rise}
  @see-slot{gtk-text-tag-rise-set}
  @see-slot{gtk-text-tag-scale}
  @see-slot{gtk-text-tag-scale-set}
  @see-slot{gtk-text-tag-size}
  @see-slot{gtk-text-tag-size-points}
  @see-slot{gtk-text-tag-size-set}
  @see-slot{gtk-text-tag-stretch}
  @see-slot{gtk-text-tag-stretch-set}
  @see-slot{gtk-text-tag-strikethrough}
  @see-slot{gtk-text-tag-strikethrough-set}
  @see-slot{gtk-text-tag-style}
  @see-slot{gtk-text-tag-style-set}
  @see-slot{gtk-text-tag-tabs}
  @see-slot{gtk-text-tag-tabs-set}
  @see-slot{gtk-text-tag-underline}
  @see-slot{gtk-text-tag-underline-set}
  @see-slot{gtk-text-tag-variant}
  @see-slot{gtk-text-tag-variant-set}
  @see-slot{gtk-text-tag-weight}
  @see-slot{gtk-text-tag-weight-set}
  @see-slot{gtk-text-tag-wrap-mode}
  @see-slot{gtk-text-tag-wrap-mode-set}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-text-tag-accumulative-margin ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "accumulative-margin"
                                               'gtk-text-tag) 't)
 "The @code{accumulative-margin} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the margins accumulate or override each other.
  When set to @em{true} the margins of this tag are added to the margins of any
  other non-accumulative margins present. When set to @code{nil} the margins
  override one another (the default). @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-accumulative-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-accumulative-margin 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{accumulative-margin} of the
  @class{gtk-text-tag} class.")

;;; --- gtk-text-tag-background ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background" 'gtk-text-tag) 't)
 "The @code{background} property of type @code{:string} (Write) @br{}
  Background color as a string. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-background atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-background 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{background} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-background-full-height ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-full-height"
                                               'gtk-text-tag) 't)
 "The @code{background-full-height} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the background color fills the entire line height or only the height
  of the tagged characters. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-background-full-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-background-full-height 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{background-full-height} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-background-full-height-set --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-full-height-set"
                                               'gtk-text-tag) 't)
 "The @code{background-full-height-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects background height. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-background-full-height-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-background-full-height-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{background-full-height-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-background-gdk --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-gdk"
                                               'gtk-text-tag) 't)
 "The @code{background-gdk} property of type @class{gdk-color}
  (Read / Write) @br{}
  @b{Warning:}
  @code{\"background-gdk\"} has been deprecated since version 3.4 and should
  not be used in newly-written code. Use @code{\"background-rgba\"}
  instead. @br{}
  Background color as a @class{gdk-color}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-background-gdk atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-background-gdk 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{background-gdk} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-background-rgba -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-rgba"
                                               'gtk-text-tag) 't)
 "The @code{background-rgba} property of type @class{gdk-rgba}
  (Read / Write) @br{}
  Background color as a @class{gdk-rgba}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-background-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-background-rgba 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{background-rgba} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-background-set --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-set"
                                               'gtk-text-tag) 't)
 "The @code{background-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the background color. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-background-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-background-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{background-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-direction -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "direction" 'gtk-text-tag) 't)
 "The @code{direction} property of type @symbol{gtk-text-direction}
  (Read / Write) @br{}
  Text direction, e. g. right-to-left or left-to-right. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-direction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-direction 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{direction} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-editable --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editable" 'gtk-text-tag) 't)
 "The @code{editable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the text can be modified by the user. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-editable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-editable 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{editable} of the @class{gtk-text-tag}
  class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-editable-set ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editable-set" 'gtk-text-tag) 't)
 "The @code{editable-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects text editability. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-editable-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-editable-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{editable-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-fallback --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "fallback" 'gtk-text-tag) 't)
 "The @code{fallback} property of type @code{:boolean} (Read / Write) @br{}
  Whether font fallback is enabled. When set to @em{true}, other fonts will be
  substituted where the current font is missing glyphs. @br{}
  Default value: @em{true} @br{}
  Since 3.16")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-fallback atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-fallback 'function)
 "@version{2019-4-12}
  Accessor of the slot @slot[gtk-text-tag]{fallback} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-fallback-set ----------------------------------------------

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "fallback-set" 'gtk-text-tag) 't)
 "The @code{fallback-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects font fallback. @br{}
  Default value: @code{nil} @br{}
  Since 3.16")

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-tag-fallback-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-fallback-set 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-text-tag]{fallback-set} of the
    @class{gtk-text-tag} class.
  @end{short}

  Since 3.16
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-family ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "family" 'gtk-text-tag) 't)
 "The @code{family} property of type @code{:string} (Read / Write) @br{}
  Name of the font family, e. g. Sans, Helvetica, Times, Monospace. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-family atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-family 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{family} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-family-set ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "family-set" 'gtk-text-tag) 't)
 "The @code{family-set} property of type @code{:boolean} (Read / Write)@br{}
  Whether this tag affects the font family. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-family-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-family-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{family-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-font ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font" 'gtk-text-tag) 't)
 "The @code{font} property of type @code{:string} (Read / Write) @br{}
  Font description as string, e. g. \"Sans Italic 12\".
  Note that the initial value of this property depends on the internals of
  @class{pango-font-description} structure. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-font atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-font 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{font} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-font-desc -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font-desc" 'gtk-text-tag) 't)
 "The @code{font-desc} property of type @class{pango-font-description}
  (Read / Write) @br{}
  Font description as a @class{pango-font-description} structure.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-font-desc atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-font-desc 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{font-desc} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-font-features ---------------------------------------------

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "font-features" 'gtk-text-tag)
      't)
 "The @code{font-features} property of type @code{:string} (Read / Write) @br{}
  OpenType font features, as a string. @br{}
  Default value: @code{nil} @br{}
  Since 3.18")

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-tag-font-features atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-font-features 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-text-tag]{font-features} of the
    @class{gtk-text-tag} class.
  @end{short}

  Since 3.18
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-font-features-set -----------------------------------------

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "font-features-set"
                                               'gtk-text-tag) 't)
 "The @code{font-features-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects font features. @br{}
  Default value: @code{nil} @br{}
  Since 3.18")

#+(and gtk-3-18 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-tag-font-features-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-font-features-set 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-text-tag]{font-features-set} of the
    @class{gtk-text-tag} class.
  @end{short}

  Since 3.18
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-foreground ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "foreground" 'gtk-text-tag) 't)
 "The @code{foreground} property of type @code{:string} (Write) @br{}
  Foreground color as a string. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-foreground atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-foreground 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{foreground} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-foreground-gdk --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "foreground-gdk"
                                               'gtk-text-tag) 't)
 "The @code{foreground-gdk} property of type @class{gdk-color}
  (Read / Write) @br{}
  @b{Warning:} @code{\"foreground-gdk\"} has been deprecated since version 3.4
  and should not be used in newly-written code. Use @code{\"foreground-rgba\"}
  instead. @br{}
  Foreground color as a @class{gdk-color}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-foreground-gdk atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-foreground-gdk 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{foreground-gdk} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-foreground-rgba -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "foreground-rgba"
                                               'gtk-text-tag) 't)
 "The @code{foreground-rgba} property of type @class{gdk-rgba}
  (Read / Write) @br{}
  Foreground color as a @class{gdk-rgba}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-foreground-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-foreground-rgba 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{foreground-rgba} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-foreground-set --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "foreground-set"
                                               'gtk-text-tag) 't)
 "The @code{foreground-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the foreground color. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-foreground-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-foreground-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{foreground-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-indent ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "indent" 'gtk-text-tag) 't)
 "The @code{indent} property of type @code{:int} (Read / Write) @br{}
  Amount to indent the paragraph, in pixels. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-indent atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-indent 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{indent} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-indent-set ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "indent-set" 'gtk-text-tag) 't)
 "The @code{indent-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects indentation. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-indent-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-indent-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{indent-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-invisible -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "invisible" 'gtk-text-tag) 't)
 "The @code{invisible} property of type @code{:boolean} (Read / Write) @br{}
  Whether this text is hidden.
  Note that there may still be problems with the support for invisible text,
  in particular when navigating programmatically inside a buffer containing
  invisible segments. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-invisible atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-invisible 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{invisible} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-invisible-set ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "invisible-set"
                                               'gtk-text-tag) 't)
 "The @code{invisible-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects text visibility. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-invisible-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-invisible-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{invisible-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-justification ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "justification"
                                               'gtk-text-tag) 't)
 "The @code{justification} property of type @symbol{gtk-justification}
  (Read / Write) @br{}
  Left, right, or center justification. @br{}
  Default value: @code{:left}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-justification atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-justification 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{justification} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-justification-set -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "justification-set"
                                               'gtk-text-tag) 't)
 "The @code{justification-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects paragraph justification. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-justification-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-justification-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{justification-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-language --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "language" 'gtk-text-tag) 't)
 "The @code{language} property of type @code{:string} (Read / Write) @br{}
  The language this text is in, as an ISO code. Pango can use this as a hint
  when rendering the text. If not set, an appropriate default will be used.
  Note that the initial value of this property depends on the current locale,
  see also the function @fun{gtk-get-default-language}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-language atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-language 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{language} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-language-set ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "language-set" 'gtk-text-tag) 't)
 "The @code{language-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the language the text is rendered as. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-language-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-language-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{language-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-left-margin -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "left-margin" 'gtk-text-tag) 't)
 "The @code{left-margin} property of type @code{:int} (Read / Write) @br{}
  Width of the left margin in pixels. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-left-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-left-margin 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{left-margin} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-left-margin-set -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "left-margin-set"
                                               'gtk-text-tag) 't)
 "The @code{left-margin-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the left margin. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-left-margin-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-left-margin-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{left-margin-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-letter-spacing --------------------------------------------

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "letter-spacing" 'gtk-text-tag)
      't)
 "The @code{letter-spacing} property of type @code{:int} (Read / Write) @br{}
  Extra spacing between graphemes, in Pango units. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 3.16")

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-tag-letter-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-letter-spacing 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-text-tag]{letter-spacing} of the
    @class{gtk-text-tag} class.
  @end{short}

  Since 3.16
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-letter-spacing-set ----------------------------------------

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "letter-spacing-set"
                                               'gtk-text-tag) 't)
 "The @code{letter-spacing-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects letter spacing. @br{}
  Default value: @code{nil} @br{}
  Since 3.16")

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-tag-letter-spacing-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-letter-spacing-set 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-text-tag]{letter-spacing-set} of the
    @class{gtk-text-tag} class.
  @end{short}

  Since 3.16
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-name ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'gtk-text-tag) 't)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct) @br{}
  Name used to refer to the text tag. @code{Nil} for anonymous tags. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-name 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{name} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-paragraph-background --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "paragraph-background"
                                               'gtk-text-tag) 't)
 "The @code{paragraph-background} property of type @code{:string}
  (Write) @br{}
  The paragraph background color as a string. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-paragraph-background atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-paragraph-background 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{paragraph-background} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-paragraph-background-gdk ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "paragraph-background-gdk"
                                               'gtk-text-tag) 't)
 "The @code{paragraph-background-gdk} property of type @class{gdk-color}
  (Read / Write) @br{}
  The paragraph background color as a as a @class{gdk-color}. @br{}
  @b{Warning:}
  @code{paragraph-background-gdk} has been deprecated since version 3.4
  and should not be used in newly-written code. Use
  @code{paragraph-background-rgba} instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-paragraph-background-gdk atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-paragraph-background-gdk 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{paragraph-background-gdk} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-paragraph-background-rgba ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "paragraph-background-rgba"
                                               'gtk-text-tag) 't)
 "The @code{paragraph-background-rgba} property of type @class{gdk-rgba}
  (Read / Write) @br{}
  The paragraph background color as a as a @class{gdk-rgba}.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-paragraph-background-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-paragraph-background-rgba 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{paragraph-background-rgba} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-paragraph-background-set ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "paragraph-background-set"
                                               'gtk-text-tag) 't)
 "The @code{paragraph-background-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the paragraph background color. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-paragraph-background-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-paragraph-background-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{paragraph-background-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-pixels-above-lines ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-above-lines"
                                               'gtk-text-tag) 't)
 "The @code{pixels-above-lines} property of type @code{:int}
  (Read / Write) @br{}
  Pixels of blank space above paragraphs. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-pixels-above-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-pixels-above-lines 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{pixels-above-lines} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-pixels-above-lines-set ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-above-lines-set"
                                               'gtk-text-tag) 't)
 "The @code{pixels-above-lines-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the number of pixels above lines. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-pixels-above-lines-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-pixels-above-lines-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{pixels-above-lines-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-pixels-below-lines ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-below-lines"
                                               'gtk-text-tag) 't)
 "The @code{pixels-below-lines} property of type @code{:int}
  (Read / Write) @br{}
  Pixels of blank space below paragraphs. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-pixels-below-lines atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-pixels-below-lines 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{pixels-below-lines} of the
  @class{gtk-text-tag} class.
  @see-clas{gtk-text-tag}")

;;; --- gtk-text-tag-pixels-below-lines-set ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-below-lines-set"
                                               'gtk-text-tag) 't)
 "The @code{pixels-below-lines-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the number of pixels above lines. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-pixels-below-lines-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-pixels-below-lines-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{pixels-below-lines-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-pixels-inside-wrap ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-inside-wrap"
                                               'gtk-text-tag) 't)
 "The @code{pixels-inside-wrap} property of type @code{:int}
  (Read / Write) @br{}
  Pixels of blank space between wrapped lines in a paragraph. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-pixels-inside-wrap atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-pixels-inside-wrap 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{pixels-inside-wrap} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-pixels-inside-wrap-set ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pixels-inside-wrap-set"
                                               'gtk-text-tag) 't)
 "The @code{pixels-inside-wrap-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the number of pixels between wrapped lines. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-pixels-inside-wrap-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-pixels-inside-wrap-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{pixels-inside-wrap-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-right-margin ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "right-margin" 'gtk-text-tag) 't)
 "The @code{right-margin} property of type @code{:int} (Read / Write) @br{}
  Width of the right margin in pixels. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-right-margin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-right-margin 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{right-margin} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-right-margin-set ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "right-margin-set"
                                               'gtk-text-tag) 't)
 "The @code{right-margin-set} property of type @code{:boolean}
  Read / Write) @br{}
  Whether this tag affects the right margin. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-right-margin-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-right-margin-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{right-margin-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-rise ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rise" 'gtk-text-tag) 't)
 "The @code{rise} property of type @code{:int} (Read / Write) @br{}
  Offset of text above the baseline (below the baseline if rise is negative)
  in Pango units. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-rise atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-rise 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{rise} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-rise-set --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rise-set" 'gtk-text-tag) 't)
 "The @code{rise-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the rise. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-rise-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-rise-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{rise-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-scale -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "scale" 'gtk-text-tag) 't)
 "The @code{scale} property of type @code{:double} (Read / Write) @br{}
  Font size as a scale factor relative to the default font size. This properly
  adapts to theme changes etc. so is recommended. Pango predefines some scales
  such as @code{PANGO_SCALE_X_LARGE}. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-scale atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-scale 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{scale} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-scale-set -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "scale-set" 'gtk-text-tag) 't)
 "The @code{scale-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag scales the font size by a factor. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-scale-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-scale-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-view]{scale-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-size ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size" 'gtk-text-tag) 't)
 "The @code{size} property of type @code{:int} (Read / Write) @br{}
  Font size in Pango units. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-size 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{size} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-size-points -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size-points" 'gtk-text-tag) 't)
 "The @code{size-points} property of type @code{:double} (Read / Write) @br{}
  Font size in points. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-size-points atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-size-points 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{size-points} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-size-set --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size-set" 'gtk-text-tag) 't)
 "The @code{size-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font size. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-size-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-size-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{size-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-stretch ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stretch" 'gtk-text-tag) 't)
 "The @code{stretch} property of type @code{PangoStretch}
  (Read / Write) @br{}
  Font stretch as a @code{PangoStretch}, e. g.
  @code{PANGO_STRETCH_CONDENSED}. @br{}
  Default value: @code{PANGO_STRETCH_NORMAL}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-stretch atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-stretch 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{stretch} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-stretch-set -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stretch-set" 'gtk-text-tag) 't)
 "The @code{stretch-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the font stretch. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-stretch-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-stretch-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{stretch-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-strikethrough ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "strikethrough"
                                               'gtk-text-tag) 't)
 "The @code{strikethrough} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to strike through the text. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-strikethrough atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-strikethrough 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{strikethrough} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-strikethrough-rgba ----------------------------------------

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "strikethrough-rgba"
                                               'gtk-text-tag) 't)
 "The @code{strikethrough-rgba} property of type @symbol{gdk-rgba}
  (Read / Write) @br{}
  This property modifies the color of strikeouts. If not set, strikeouts will
  use the forground color. @br{}
  Since 3.16")

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-tag-strikethrough-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-strikethrough-rgba 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-text-tag]{strikethrough-rgba} of the
    @class{gtk-text-tag} class.
  @end{short}

  Since 3.16
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-strikethrough-rgba-set ------------------------------------

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "strikethrough-rgba-set"
                                               'gtk-text-tag) 't)
 "The @code{strikethrough-rgba-set} property of type @code{:boolean}
  (Read / Write) @br{}
  If the @code{strikethrough-rgba} property has been set. @br{}
  Default value: @code{nil} @br{}
  Since 3.16")

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-tag-strikethrough-rgba-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-strikethrough-rgba-set 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-text-tag]{strikethrough-rgba-set} of the
    @class{gtk-text-tag} class.
  @end{short}

  Since 3.16
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-strikethrough-set -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "strikethrough-set"
                                       'gtk-text-tag) 't)
 "The @code{strikethrough-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects strikethrough. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-strikethrough-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-strikethrough-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{strikethrough-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-style -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "style" 'gtk-text-tag) 't)
 "The @code{style} property of type @code{PangoStyle} (Read / Write) @br{}
  Font style as a @code{PangoStyle}, e. g. @code{PANGO_STYLE_ITALIC}. @br{}
  Default value: @code{PANGO_STYLE_NORMAL}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-style 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{style} of the @class{gtk-text-tag}
  class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-style-set -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "style-set" 'gtk-text-tag) 't)
 "The @code{style-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font style. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-style-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-style-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{style-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-tabs ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tabs" 'gtk-text-tag) 't)
 "The @code{tabs} property of type @code{PangoTabArray*} (Read / Write) @br{}
  Custom tabs for this text.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-tabs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-tabs 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{tabs} of the @class{gtk-text-tag}
  class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-tabs-set --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tabs-set" 'gtk-text-tag) 't)
 "The @code{tabs-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects tabs. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-tabs-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-tabs-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{tabs-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-underline -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "underline" 'gtk-text-tag) 't)
 "The @code{underline} property of type @code{PangoUnderline}
  (Read / Write) @br{}
  Style of underline for this text. @br{}
  Default value: @code{PANGO_UNDERLINE_NONE}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-underline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-underline 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{underline} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-underline-rgba --------------------------------------------

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "underline-rgba"
                                               'gtk-text-tag) 't)
 "The @code{underline-rgba} property of type @symbol{gdk-rgba}
  (Read / Write) @br{}
  This property modifies the color of underlines. If not set, underlines will
  use the forground color. If @code{underline} is set to
  @code{PANGO_UNDERLINE_ERROR}, an alternate color may be applied instead of the
  foreground. Setting this property will always override those defaults. @br{}
  Since 3.16")

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-tag-underline-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-underline-rgba 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-text-tag]{underline-rgba} of the
    @class{gtk-text-tag} class.
  @end{short}

  Since 3.16
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-underline-rgba-set ----------------------------------------

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "underline-rgba-set"
                                               'gtk-text-tag) 't)
 "The @code{underline-rgba-set} property of type @code{:boolean}
  (Read / Write) @br{}
  If the @code{underline-rgba} property has been set. @br{}
  Default value: @code{nil} @br{}
  Since 3.16")

#+(and gtk-3-16 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-text-tag-underline-rgba-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-underline-rgba-set 'function)
 "@version{2019-4-12}
  @begin{short}
    Accessor of the slot @slot[gtk-text-tag]{underline-rgba-set} of the
    @class{gtk-text-tag} class.
  @end{short}

  Since 3.16
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-underline-set ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "underline-set"
                                               'gtk-text-tag) 't)
 "The @code{underline-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects underlining. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-underline-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-underline-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{underline-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-variant ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "variant" 'gtk-text-tag) 't)
 "The @code{variant} property of type @code{PangoVariant}
  (Read / Write) @br{}
  Font variant as a @code{PangoVariant}, e. g.
  @code{PANGO_VARIANT_SMALL_CAPS}. @br{}
  Default value: @code{PANGO_VARIANT_NORMAL}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-variant atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-variant 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-view]{variant} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-variant-set -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "variant-set" 'gtk-text-tag) 't)
 "The @code{variant-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the font variant. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-variant-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-variant-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{variant-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-weight ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "weight" 'gtk-text-tag) 't)
 "The @code{weight} property of type @code{:int} (Read / Write) @br{}
  Font weight as an integer, see predefined values in @code{PangoWeight}; for
  example, @code{PANGO_WEIGHT_BOLD}. @br{}
  Allowed values: >= 0 @br{}
  Default value: 400")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-weight atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-weight 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{weight} of the @class{gtk-text-tag}
  class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-weight-set ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "weight-set" 'gtk-text-tag) 't)
 "The @code{weight-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font weight. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-weight-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-weight-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{weight-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-wrap-mode -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-mode" 'gtk-text-tag) 't)
 "The @code{wrap-mode} property of type @symbol{gtk-wrap-mode}
  (Read / Write) @br{}
  Whether to wrap lines never, at word boundaries, or at character
  boundaries. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-wrap-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-wrap-mode 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{wrap-mode} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; --- gtk-text-tag-wrap-mode-set ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-mode-set"
                                               'gtk-text-tag) 't)
 "The @code{wrap-mode-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects line wrap mode. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-tag-wrap-mode-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-text-tag-wrap-mode-set 'function)
 "@version{2013-3-25}
  Accessor of the slot @slot[gtk-text-tag]{wrap-mode-set} of the
  @class{gtk-text-tag} class.
  @see-class{gtk-text-tag}")

;;; ----------------------------------------------------------------------------
;;; enum GtkWrapMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkWrapMode" gtk-wrap-mode
  (:export t
   :type-initializer "gtk_wrap_mode_get_type")
  (:none 0)
  (:char 1)
  (:word 2)
  (:word-char 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-wrap-mode atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-wrap-mode atdoc:*external-symbols*)
 "@version{2013-3-25}
  @short{Describes a type of line wrapping.}
  @begin{pre}
(define-g-enum \"GtkWrapMode\" gtk-wrap-mode
  (:export t
   :type-initializer \"gtk_wrap_mode_get_type\")
  (:none 0)
  (:char 1)
  (:word 2)
  (:word-char 3))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Do not wrap lines; just make the text area wider.}
    @entry[:char]{Wrap text, breaking lines anywhere the cursor can appear
      (between characters, usually - if you want to be technical, between
      graphemes, see the function @fun{pango-get-log-attrs}).}
    @entry[:word]{Wrap text, breaking lines in between words.}
    @entry[:word-char]{Wrap text, breaking lines in between words, or if that is
      not enough, also between graphemes.}
  @end{table}
  @see-function{pango-get-log-attrs}")

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
;;; #if (defined(__SIZEOF_INT__) && defined(__SIZEOF_POINTER__))
;;;                              && (__SIZEOF_INT__ == __SIZEOF_POINTER__)
;;;   /* unusable, just for ABI compat */
;;;   guint padding[2];
;;; #endif
;;; };
;;; ----------------------------------------------------------------------------

(defcstruct gtk-text-appearance
  (:bg-color (g-boxed-foreign gdk-color))
  (:fg-color (g-boxed-foreign gdk-color))
  (:rise :int)
  (:underline :uint)
  (:strikethrough :uint)
  (:draw-bg :uint)
  (:inside-selection :uint)
  (:is-text :uint)
  (:rgba (g-boxed-foreign gdk-rgba))
  (:padding :uint))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-appearance atdoc:*type-name-alias*) "CStruct"
      (documentation 'gtk-text-appearance 'type)
 "@version{2013-6-4}
  @short{}
  @begin{pre}
(defcstruct gtk-text-appearance
  (:bg-color (g-boxed-foreign gdk-color))
  (:fg-color (g-boxed-foreign gdk-color))
  (:rise :int)
  (:underline :uint)
  (:strikethrough :uint)
  (:draw-bg :uint)
  (:inside-selection :uint)
  (:is-text :uint)
  (:rgba (g-boxed-foreign gdk-rgba))
  (:padding :uint))
  @end{pre}")

(export 'gtk-text-appearance)

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
;;; ----------------------------------------------------------------------------

(defcstruct gtk-text-attributes
  (:appearance (:pointer (:struct gtk-text-appearance)))
  (:justification gtk-justification)
  (:direction gtk-text-direction)
  (:font (g-boxed-foreign pango-font-description))
  (:font-scale :double)
  (:left-margin :int)
  (:right-margin :int)
  (:indent :int)
  (:pixels-above-lines :int)
  (:pixels-below-lines :int)
  (:pixels-inside-wrap :int)
  (:tabs :pointer)             ; type is pango-tab-array
  (:wrap-mode gtk-wrap-mode)
  (:language (g-boxed-foreign pango-language))
  (:invisible :uint)
  (:bg-full-height :uint)
  (:editable :uint))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-text-attributes atdoc:*type-name-alias*) "CStruct"
      (documentation 'gtk-text-attributes 'type)
 "@version{2013-6-4}
  @begin{short}
    Using @sym{gtk-text-attributes} directly should rarely be necessary. It is
    primarily useful with the @fun{gtk-text-iter-get-attributes} function. As
    with most GTK+ structs, the fields in this struct should only be read, never
    modified directly.
  @end{short}
  @begin{pre}
(defcstruct gtk-text-attributes
  (:appearance (:pointer (:struct gtk-text-appearance)))
  (:justification gtk-justification)
  (:direction gtk-text-direction)
  (:font (g-boxed-foreign pango-font-description))
  (:font-scale :double)
  (:left-margin :int)
  (:right-margin :int)
  (:indent :int)
  (:pixels-above-lines :int)
  (:pixels-below-lines :int)
  (:pixels-inside-wrap :int)
  (:tabs :pointer) ; type is pango-tab-array
  (:wrap-mode gtk-wrap-mode)
  (:language (g-boxed-foreign pango-language))
  (:invisible :uint)
  (:bg-full-height :uint)
  (:editable :uint))
  @end{pre}")

(export 'gtk-text-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_new ()
;;;
;;; GtkTextTag * gtk_text_tag_new (const gchar *name);
;;;
;;; Creates a GtkTextTag. Configure the tag using object arguments, i.e. using
;;; g_object_set().
;;;
;;; name :
;;;     tag name, or NULL
;;;
;;; Returns :
;;;     a new GtkTextTag
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_get_priority ()
;;;
;;; gint gtk_text_tag_get_priority (GtkTextTag *tag);
;;;
;;; Get the tag priority.
;;;
;;; tag :
;;;     a GtkTextTag
;;;
;;; Returns :
;;;     The tag's priority.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_set_priority ()
;;;
;;; void gtk_text_tag_set_priority (GtkTextTag *tag, gint priority);
;;;
;;; Sets the priority of a GtkTextTag. Valid priorities are start at 0 and go to
;;; one less than gtk_text_tag_table_get_size(). Each tag in a table has a
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
;;;     a GtkTextTag
;;;
;;; priority :
;;;     the new priority
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
;;;     a GtkTextTag
;;;
;;; event_object :
;;;     object that received the event, such as a widget
;;;
;;; event :
;;;     the event
;;;
;;; iter :
;;;     location where the event was received
;;;
;;; Returns :
;;;     result of signal emission (whether the event was handled)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_changed ()
;;;
;;; void
;;; gtk_text_tag_changed (GtkTextTag *tag, gboolean size_changed);
;;;
;;; Emits the “tag-changed” signal on the GtkTextTagTable where the tag is
;;; included.
;;;
;;; The signal is already emitted when setting a GtkTextTag property. This
;;; function is useful for a GtkTextTag subclass.
;;;
;;; tag :
;;;     a GtkTextTag.
;;;
;;; size_changed :
;;;     whether the change affects the GtkTextView layout.
;;;
;;; Since 3.20
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
;;;     a new GtkTextAttributes, free with gtk_text_attributes_unref().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_copy ()
;;;
;;; GtkTextAttributes * gtk_text_attributes_copy (GtkTextAttributes *src);
;;;
;;; Copies src and returns a new GtkTextAttributes.
;;;
;;; src :
;;;     a GtkTextAttributes to be copied
;;;
;;; Returns :
;;;     a copy of src, free with gtk_text_attributes_unref()
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
;;;     a GtkTextAttributes
;;;
;;; dest :
;;;     another GtkTextAttributes
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
;;;     a GtkTextAttributes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_ref ()
;;;
;;; GtkTextAttributes * gtk_text_attributes_ref (GtkTextAttributes *values);
;;;
;;; Increments the reference count on values.
;;;
;;; values :
;;;     a GtkTextAttributes
;;;
;;; Returns :
;;;     the GtkTextAttributes that were passed in
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.text-tag.lisp ------------------------------------------
