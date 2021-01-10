;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-text.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
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
;;; --------------------------------------------------------------------------
;;;
;;; GtkCellRendererText
;;;
;;;     Renders text in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererText
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_text_new
;;;     gtk_cell_renderer_text_set_fixed_height_from_font
;;;
;;; Properties
;;;
;;;             gboolean    align-set                Read / Write
;;;       PangoAlignment    alignment                Read / Write
;;;        PangoAttrList*   attributes               Read / Write
;;;                gchar*   background               Write
;;;             GdkColor*   background-gdk           Read / Write
;;;              GdkRGBA*   background-rgba          Read / Write
;;;             gboolean    background-set           Read / Write
;;;             gboolean    editable                 Read / Write
;;;             gboolean    editable-set             Read / Write
;;;   PangoEllipsizeMode    ellipsize                Read / Write
;;;             gboolean    ellipsize-set            Read / Write
;;;                gchar*   family                   Read / Write
;;;             gboolean    family-set               Read / Write
;;;                gchar*   font                     Read / Write
;;; PangoFontDescription*   font-desc                Read / Write
;;;                gchar*   foreground               Write
;;;             GdkColor*   foreground-gdk           Read / Write
;;;              GdkRGBA*   foreground-rgba          Read / Write
;;;             gboolean    foreground-set           Read / Write
;;;                gchar*   language                 Read / Write
;;;             gboolean    language-set             Read / Write
;;;                gchar*   markup                   Write
;;;                 gint    max-width-chars          Read / Write
;;;                gchar*   placeholder-text         Read / Write
;;;             gboolean    rise-set                 Read / Write
;;;              gdouble    scale                    Read / Write
;;;             gboolean    scale-set                Read / Write
;;;             gboolean    single-paragraph-mode    Read / Write
;;;                 gint    size                     Read / Write
;;;              gdouble    size-points              Read / Write
;;;             gboolean    size-set                 Read / Write
;;;         PangoStretch    stretch                  Read / Write
;;;             gboolean    stretch-set              Read / Write
;;;             gboolean    strikethrough            Read / Write
;;;             gboolean    strikethrough-set        Read / Write
;;;           PangoStyle    style                    Read / Write
;;;             gboolean    style-set                Read / Write
;;;                gchar*   text                     Read / Write
;;;       PangoUnderline    underline                Read / Write
;;;             gboolean    underline-set            Read / Write
;;;         PangoVariant    variant                  Read / Write
;;;             gboolean    variant-set              Read / Write
;;;                 gint    weight                   Read / Write
;;;             gboolean    weight-set               Read / Write
;;;                 gint    width-chars              Read / Write
;;;        PangoWrapMode    wrap-mode                Read / Write
;;;                 gint    wrap-width               Read / Write
;;;
;;; Signals
;;;
;;;                 void    edited                   Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererText
;;;                 ├── GtkCellRendererAccel
;;;                 ├── GtkCellRendererCombo
;;;                 ╰── GtkCellRendererSpin
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererText
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererText" gtk-cell-renderer-text
  (:superclass gtk-cell-renderer
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_text_get_type")
  ((align-set
    gtk-cell-renderer-text-align-set
    "align-set" "gboolean" t t)
   (alignment
    gtk-cell-renderer-text-alignment
    "alignment" "PangoAlignment" t t)
   (attributes
    gtk-cell-renderer-text-attributes
    "attributes" "PangoAttrList" t t)
   (background
    gtk-cell-renderer-text-background
    "background" "gchararray" nil t)
   (background-gdk
    gtk-cell-renderer-text-background-gdk
    "background-gdk" "GdkColor" t t)
   (background-rgba
    gtk-cell-renderer-text-background-rgba
    "background-rgba" "GdkRGBA" t t)
   (background-set
    gtk-cell-renderer-text-background-set
    "background-set" "gboolean" t t)
   (editable
    gtk-cell-renderer-text-editable
    "editable" "gboolean" t t)
   (editable-set
    gtk-cell-renderer-text-editable-set
    "editable-set" "gboolean" t t)
   (ellipsize
    gtk-cell-renderer-text-ellipsize
    "ellipsize" "PangoEllipsizeMode" t t)
   (ellipsize-set
    gtk-cell-renderer-text-ellipsize-set
    "ellipsize-set" "gboolean" t t)
   (family
    gtk-cell-renderer-text-family
    "family" "gchararray" t t)
   (family-set
    gtk-cell-renderer-text-family-set
    "family-set" "gboolean" t t)
   (font
    gtk-cell-renderer-text-font
    "font" "gchararray" t t)
   (font-desc
    gtk-cell-renderer-text-font-desc
    "font-desc" "PangoFontDescription" t t)
   (foreground
    gtk-cell-renderer-text-foreground
    "foreground" "gchararray" nil t)
   (foreground-gdk
    gtk-cell-renderer-text-foreground-gdk
    "foreground-gdk" "GdkColor" t t)
   (foreground-rgba
    gtk-cell-renderer-text-foreground-rgba
    "foreground-rgba" "GdkRGBA" t t)
   (foreground-set
    gtk-cell-renderer-text-foreground-set
    "foreground-set" "gboolean" t t)
   (language
    gtk-cell-renderer-text-language
    "language" "gchararray" t t)
   (language-set
    gtk-cell-renderer-text-language-set
    "language-set" "gboolean" t t)
   (markup
    gtk-cell-renderer-text-markup
    "markup" "gchararray" nil t)
   (max-width-chars
    gtk-cell-renderer-text-max-width-chars
    "max-width-chars" "gint" t t)
   #+gtk-3-6
   (placeholder-text
    gtk-cell-renderer-text-placeholder-text
    "placeholder-text" "gchararray" t t)
   (rise
    gtk-cell-renderer-text-rise
    "rise" "gint" t t)
   (rise-set
    gtk-cell-renderer-text-rise-set
    "rise-set" "gboolean" t t)
   (scale
    gtk-cell-renderer-text-scale
    "scale" "gdouble" t t)
   (scale-set
    gtk-cell-renderer-text-scale-set
    "scale-set" "gboolean" t t)
   (single-paragraph-mode
    gtk-cell-renderer-text-single-paragraph-mode
    "single-paragraph-mode" "gboolean" t t)
   (size
    gtk-cell-renderer-text-size
    "size" "gint" t t)
   (size-points
    gtk-cell-renderer-text-size-points
    "size-points" "gdouble" t t)
   (size-set
    gtk-cell-renderer-text-size-set
    "size-set" "gboolean" t t)
   (stretch
    gtk-cell-renderer-text-stretch
    "stretch" "PangoStretch" t t)
   (stretch-set
    gtk-cell-renderer-text-stretch-set
    "stretch-set" "gboolean" t t)
   (strikethrough
    gtk-cell-renderer-text-strikethrough
    "strikethrough" "gboolean" t t)
   (strikethrough-set
    gtk-cell-renderer-text-strikethrough-set
    "strikethrough-set" "gboolean" t t)
   (style
    gtk-cell-renderer-text-style
    "style" "PangoStyle" t t)
   (style-set
    gtk-cell-renderer-text-style-set
    "style-set" "gboolean" t t)
   (text
    gtk-cell-renderer-text-text
    "text" "gchararray" t t)
   (underline
    gtk-cell-renderer-text-underline
    "underline" "PangoUnderline" t t)
   (underline-set
    gtk-cell-renderer-text-underline-set
    "underline-set" "gboolean" t t)
   (variant
    gtk-cell-renderer-text-variant
    "variant" "PangoVariant" t t)
   (variant-set
    gtk-cell-renderer-text-variant-set
    "variant-set" "gboolean" t t)
   (weight
    gtk-cell-renderer-text-weight
    "weight" "gint" t t)
   (weight-set
    gtk-cell-renderer-text-weight-set
    "weight-set" "gboolean" t t)
   (width-chars
    gtk-cell-renderer-text-width-chars
    "width-chars" "gint" t t)
   (wrap-mode
    gtk-cell-renderer-text-wrap-mode
    "wrap-mode" "PangoWrapMode" t t)
   (wrap-width
    gtk-cell-renderer-text-wrap-width
    "wrap-width" "gint" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-text 'type)
 "@version{2020-6-13}
  @begin{short}
    A @sym{gtk-cell-renderer-text} renders a given text in its cell, using the
    font, color and style information provided by its properties.
  @end{short}
  The text will be ellipsized if it is too long and the @code{ellipsize}
  property allows it.

  If the @code{mode} property is @code{:editable}, the
  @sym{gtk-cell-renderer-text} allows to edit its text using an entry.
  @begin[Signal Details]{dictionary}
    @subheading{The \"edited\" signal}
      @begin{pre}
 lambda (renderer path new-text)    : Run Last
      @end{pre}
      This signal is emitted after renderer has been edited. It is the
      responsibility of the application to update the model and store
      @arg{new-text} at the position indicated by path.
      @begin[code]{table}
        @entry[renderer]{The @sym{gtk-cell-renderer-text} object which received
          the signal.}
        @entry[path]{A string with the path identifying the edited cell.}
        @entry[new-text]{A string with the new text.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-cell-renderer-text-align-set}
  @see-slot{gtk-cell-renderer-text-alignment}
  @see-slot{gtk-cell-renderer-text-attributes}
  @see-slot{gtk-cell-renderer-text-background}
  @see-slot{gtk-cell-renderer-text-background-gdk}
  @see-slot{gtk-cell-renderer-text-background-rgba}
  @see-slot{gtk-cell-renderer-text-background-set}
  @see-slot{gtk-cell-renderer-text-editable}
  @see-slot{gtk-cell-renderer-text-editable-set}
  @see-slot{gtk-cell-renderer-text-ellipsize}
  @see-slot{gtk-cell-renderer-text-ellipsize-set}
  @see-slot{gtk-cell-renderer-text-family}
  @see-slot{gtk-cell-renderer-text-family-set}
  @see-slot{gtk-cell-renderer-text-font}
  @see-slot{gtk-cell-renderer-text-font-desc}
  @see-slot{gtk-cell-renderer-text-foreground}
  @see-slot{gtk-cell-renderer-text-foreground-gdk}
  @see-slot{gtk-cell-renderer-text-foreground-rgba}
  @see-slot{gtk-cell-renderer-text-foreground-set}
  @see-slot{gtk-cell-renderer-text-language}
  @see-slot{gtk-cell-renderer-text-language-set}
  @see-slot{gtk-cell-renderer-text-markup}
  @see-slot{gtk-cell-renderer-text-max-width-chars}
  @see-slot{gtk-cell-renderer-text-placeholder-text}
  @see-slot{gtk-cell-renderer-text-rise}
  @see-slot{gtk-cell-renderer-text-rise-set}
  @see-slot{gtk-cell-renderer-text-scale}
  @see-slot{gtk-cell-renderer-text-scale-set}
  @see-slot{gtk-cell-renderer-text-single-paragraph-mode}
  @see-slot{gtk-cell-renderer-text-size}
  @see-slot{gtk-cell-renderer-text-size-points}
  @see-slot{gtk-cell-renderer-text-size-set}
  @see-slot{gtk-cell-renderer-text-stretch}
  @see-slot{gtk-cell-renderer-text-stretch-set}
  @see-slot{gtk-cell-renderer-text-strikethrough}
  @see-slot{gtk-cell-renderer-text-strikethrough-set}
  @see-slot{gtk-cell-renderer-text-style}
  @see-slot{gtk-cell-renderer-text-style-set}
  @see-slot{gtk-cell-renderer-text-text}
  @see-slot{gtk-cell-renderer-text-underline}
  @see-slot{gtk-cell-renderer-text-underline-set}
  @see-slot{gtk-cell-renderer-text-variant}
  @see-slot{gtk-cell-renderer-text-variant-set}
  @see-slot{gtk-cell-renderer-text-weight}
  @see-slot{gtk-cell-renderer-text-weight-set}
  @see-slot{gtk-cell-renderer-text-width-chars}
  @see-slot{gtk-cell-renderer-text-wrap-mode}
  @see-slot{gtk-cell-renderer-text-wrap-width}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-renderer-text-align-set ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "align-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{align-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the alignment mode. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-align-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-align-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-align-set object) => align-set}
  @syntax[]{(setf (gtk-cell-renderer-text-align-set object) align-set)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[align-set]{a boolean wether this tag affects the alignment mode}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{align-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the alignment mode.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-alignment ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "alignment"
                                               'gtk-cell-renderer-text) 't)
 "The @code{alignment} property of type @symbol{pango-alignment} (Read / Write)
  @br{}
  Specifies how to align the lines of text with respect to each other. Note that
  this property describes how to align the lines of text in case there are
  several of them. The @code{xalign} property of @class{gtk-cell-renderer}, on
  the other hand, sets the horizontal alignment of the whole text. @br{}
  Default value: @code{:left}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-alignment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-alignment 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-alignment object) => alignment}
  @syntax[]{(setf (gtk-cell-renderer-text-alignment object) alignment)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[alignment]{a value of the @symbol{pango-alignment} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{alignment} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Specifies how to align the lines of text with respect to each other. Note that
  this property describes how to align the lines of text in case there are
  several of them. The @code{xalign} property of @class{gtk-cell-renderer}, on
  the other hand, sets the horizontal alignment of the whole text.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-attributes --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "attributes"
                                               'gtk-cell-renderer-text) 't)
 "The @code{attributes} property of type @class{pango-attr-list} (Read / Write)
  @br{}
  A list of style attributes to apply to the text of the renderer.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-attributes atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-attributes 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-attributes object) => attributes}
  @syntax[]{(setf (gtk-cell-renderer-text-attributes object) attributes)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[attributes]{a @class{pango-attr-list} structure}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer]{attributes} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  A list of style attributes to apply to the text of the renderer.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-background --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background"
                                               'gtk-cell-renderer-text) 't)
 "The @code{background} property of type @code{:string} (Write) @br{}
  Background color as a string. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-background atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-background 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-background object) => background}
  @syntax[]{(setf (gtk-cell-renderer-text-background object) background)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[background]{a string with the background color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{background} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Background color as a string.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-background-gdk ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-gdk"
                                               'gtk-cell-renderer-text) 't)
 "The @code{background-gdk} property of type @class{gdk-color} (Read / Write)
  @br{}
  Background color. @br{}
  @em{Warning:} The @code{background-gdk} property has been deprecated since
  version 3.4 and should not be used in newly written code. Use the
  @code{background-rgba} property instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-background-gdk
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-background-gdk 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-background-gdk object) => background}
  @syntax[]{(setf (gtk-cell-renderer-text-background-gdk object) background)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[background]{a @class{gdk-color} with the background color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{background-gdk} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Background color as a @class{gdk-color}.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-background-rgba ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-rgba"
                                               'gtk-cell-renderer-text) 't)
 "The @code{background-rgba} property of type @class{gdk-rgba} (Read / Write)
  @br{}
  Background color.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-background-rgba
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-background-rgba 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-background-rgba object) => background}
  @syntax[]{(setf (gtk-cell-renderer-text-background-rgba object) background)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[background]{a @class{gdk-rgba} color with the background color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{background-rgba} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  The @class{gdk-rgba} background color.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-background-set ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "background-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{background-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects the background color. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-background-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-background-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-background-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-background-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects the background color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{background-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}
  Whether this tag affects the background color.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-editable ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editable"
                                               'gtk-cell-renderer-text) 't)
 "The @code{editable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the text can be modified by the user. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-editable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-editable 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-editable object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-editable object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether the text can be modified by the user}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{editable} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether the text can be modified by the user.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-editable-set ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editable-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{editable-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects text editability. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-editable-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-editable-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-editable-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-editable-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects text editability}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{editable-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects text editability.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-ellipsize ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ellipsize"
                                               'gtk-cell-renderer-text) 't)
 "The @code{ellipsize} property of type @symbol{pango-ellipsize-mode}
  (Read / Write) @br{}
  Specifies the preferred place to ellipsize the string, if the cell renderer
  does not have enough room to display the entire string. Setting it to
  @code{:none} turns off ellipsizing. See the @code{wrap-width} property for
  another way of making the text fit into a given width. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-ellipsize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-ellipsize 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-ellipsize object) => mode}
  @syntax[]{(setf (gtk-cell-renderer-text-ellipsize object) mode)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[mode]{a @symbol{pango-ellipsize-mode} value}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{ellipsize} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Specifies the preferred place to ellipsize the string, if the cell renderer
  does not have enough room to display the entire string. Setting it to
  @code{:none} turns off ellipsizing. See the @code{wrap-width} property for
  another way of making the text fit into a given width.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-ellipsize-set -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ellipsize-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{ellipsize-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the ellipsize mode. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-ellipsize-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-ellipsize-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-ellipsize-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-ellipsize-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects the ellipsize mode}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{ellipsize-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the ellipsize mode.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-family ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "family"
                                               'gtk-cell-renderer-text) 't)
 "The @code{family} property of type @code{:string} (Read / Write) @br{}
  Name of the font family, e.g. Sans, Helvetica, Times, Monospace. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-family atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-family 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-family object) => font}
  @syntax[]{(setf (gtk-cell-renderer-text-family object) font)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[font]{a string with the name of the font family}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{family} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Name of the font family, e.g. Sans, Helvetica, Times, Monospace.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-family-set --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "family-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{family-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font family. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-family-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-family-set 'function)
 "@version{2021-1-7}
  @syntax[]{(gtk-cell-renderer-text-family-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-family-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects the font family}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{family-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the font family.
  @see-class{gtk-cell-renderer-text}
  @see-function{gtk-cell-renderer-text-family}")

;;; --- gtk-cell-renderer-text-font --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font"
                                               'gtk-cell-renderer-text) 't)
 "The @code{font} property of type @code{:string} (Read / Write) @br{}
  Font description as a string, e.g. \"Sans Italic 12\". @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-font atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-font 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-font object) => font}
  @syntax[]{(setf (gtk-cell-renderer-text-font object) font)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[font]{a string with the font description}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{font} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Font description as a string, e.g. \"Sans Italic 12\".
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-font-desc ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font-desc"
                                               'gtk-cell-renderer-text) 't)
 "The @code{font-desc} property of type @class{pango-font-description}
  (Read / Write) @br{}
  A Pango font description.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-font-desc atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-font-desc 'function)
 "@version{2021-1-7}
  @syntax[]{(gtk-cell-renderer-text-font-desc object) => font-desc}
  @syntax[]{(setf (gtk-cell-renderer-text-font-desc object) font-desc)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[font-desc]{a @class{pango-font-description} instance}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{font-desc} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  A Pango font description.
  @see-class{gtk-cell-renderer-text}
  @see-class{pango-font-description}")

;;; --- gtk-cell-renderer-text-foreground --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "foreground"
                                               'gtk-cell-renderer-text) 't)
 "The @code{foreground} property of type @code{:string} (Write) @br{}
  Foreground color as a string. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-foreground atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-foreground 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-foreground object) => foreground}
  @syntax[]{(setf (gtk-cell-renderer-text-foreground object) foreground)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[foreground]{a string with the foreground color}
  @begin{short}
    Accessor of the slot @slot[gtk-cell-renderer-text]{foreground} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Foreground color as a string.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-foreground-gdk ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "foreground-gdk"
                                               'gtk-cell-renderer-text) 't)
 "The @code{foreground-gdk} property of type @class{gdk-color} (Read / Write)
  @br{}
  Foreground color. @br{}
  @em{Warning:} The @code{foreground-gdk} property has been deprecated since
  version 3.4 and should not be used in newly written code. Use the
  @code{foreground-rgba} property instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-foreground-gdk
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-foreground-gdk 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-foreground-gdk object) => foreground}
  @syntax[]{(setf (gtk-cell-renderer-text-foreground-gdk object) foreground)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[foreground]{a @class{gdk-color} color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{foreground-gdk} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Foreground color as a @class{gdk-color}.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-foreground-rgba ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "foreground-rgba"
                                               'gtk-cell-renderer-text) 't)
 "The @code{foreground-rgba} property of type @class{gdk-rgba} (Read / Write)
  @br{}
  Foreground color.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-foreground-rgba
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-foreground-rgba 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-foreground-rgba object) => foreground}
  @syntax[]{(setf (gtk-cell-renderer-text-foreground-rgba object) foreground)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[foreground]{a @class{gdk-rgba} color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{foreground-rgba} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  The @class{gdk-rgba} foreground color.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-foreground-set ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "foreground-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{foreground-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects the foreground color. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-foreground-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-foreground-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-foreground-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-foreground-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects the foreground color}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{foreground-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the foreground color.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-language ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "language"
                                               'gtk-cell-renderer-text) 't)
 "The @code{language} property of type @code{:string} (Read / Write) @br{}
  The language this text is in, as an ISO code. Pango can use this as a hint
  when rendering the text. If you do not understand this parameter, you
  probably do not need it. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-language atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-language 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-language object) => language}
  @syntax[]{(setf (gtk-cell-renderer-text-language object) language)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[language]{a string with the language this text is in}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{language} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  The language this text is in, as an ISO code. Pango can use this as a hint
  when rendering the text. If you do not understand this parameter, you
  probably do not need it.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-language-set ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "language-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{language-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the language the text is rendered as. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-language-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-language-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-language-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-language-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects the language}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{language-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the language the text is rendered as.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-markup ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "markup"
                                               'gtk-cell-renderer-text) 't)
 "The @code{markup} property of type @code{:string} (Write) @br{}
  Marked up text to render. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-markup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-markup 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-markup object) => markup}
  @syntax[]{(setf (gtk-cell-renderer-text-markup object) markup)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[markup]{a string with the marked up text to render}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{markup} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Marked up text to render.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-max-width-chars ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-width-chars"
                                               'gtk-cell-renderer-text) 't)
 "The @code{max-width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired maximum width of the cell, in characters. If this property is
  set to -1, the width will be calculated automatically. For cell renderers that
  ellipsize or wrap text. This property controls the maximum reported width of
  the cell. The cell should not receive any greater allocation unless it is set
  to expand in its @class{gtk-cell-layout} and all of the cell's siblings have
  received their natural width. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-max-width-chars
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-max-width-chars 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-max-width-chars object) => width}
  @syntax[]{(setf (gtk-cell-renderer-text-max-width-chars object) width)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[width]{an integer with the maximum width of the cell}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{max-width-chars} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  The desired maximum width of the cell, in characters. If this property is
  set to -1, the width will be calculated automatically. For cell renderers that
  ellipsize or wrap text. This property controls the maximum reported width of
  the cell. The cell should not receive any greater allocation unless it is set
  to expand in its @class{gtk-cell-layout} and all of the cell's siblings have
  received their natural width.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-placeholder-text --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "placeholder-text"
                                               'gtk-cell-renderer-text) 't)
 "The @code{placeholder-text} property of type @code{:string} (Read / Write)
  @br{}
  The text that will be displayed in the @class{gtk-cell-renderer} if
  @code{editable} is @em{true} and the cell is empty. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-placeholder-text
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-placeholder-text 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-placeholder-text object) => text}
  @syntax[]{(setf (gtk-cell-renderer-text-placeholder-text object) text)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[text]{a string with the placeholder text}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{placeholder-text} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  The text that will be displayed in the @class{gtk-cell-renderer} if
  @code{editable} is @em{true} and the cell is empty.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-rise --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rise"
                                               'gtk-cell-renderer-text) 't)
 "The @code{rise} property of type @code{:int} (Read / Write) @br{}
  Offset of text above the baseline, below the baseline if @code{rise} is
  negative. @br{}
  Allowed values: >= -2147483647 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-rise atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-rise 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-rise object) => rise}
  @syntax[]{(setf (gtk-cell-renderer-text-rise object) rise)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[rise]{an integer with the offset of text above the baseline}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{rise} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Offset of text above the baseline, below the baseline if @code{rise} is
  negative.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-rise-set ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rise-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{rise-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the rise. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-rise-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-rise-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-rise-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-rise-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects the rise}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{rise-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the rise.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-scale -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "scale"
                                               'gtk-cell-renderer-text) 't)
 "The @code{scale} property of type @code{:double} (Read / Write) @br{}
  Font scaling factor. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-scale atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-scale 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-scale object) => scale}
  @syntax[]{(setf (gtk-cell-renderer-text-scale object) scale)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[scale]{a @code{:double} with the font scaling factor}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{scale} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Font scaling factor.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-scale-set ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "scale-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{scale-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag scales the font size by a factor. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-scale-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-scale-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-scale-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-scale-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag scales the font size by a factor}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{scale-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag scales the font size by a factor.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-single-paragraph-mode ---------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "single-paragraph-mode"
                                               'gtk-cell-renderer-text) 't)
 "The @code{single-paragraph-mode} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to keep all text in a single paragraph. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-single-paragraph-mode
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-single-paragraph-mode 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-single-paragraph-mode object) => mode}
  @syntax[]{(setf (gtk-cell-renderer-text-single-paragraph-mode object) mode)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[mode]{a boolean wether to keep all text in a single paragraph}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{single-paragraph-mode} slot
    of the @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether to keep all text in a single paragraph.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-size --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size"
                                               'gtk-cell-renderer-text) 't)
 "The @code{size} property of type @code{:int} (Read / Write) @br{}
  Font size. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-size 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-size object) => size}
  @syntax[]{(setf (gtk-cell-renderer-text-size object) size)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[size]{an integer with the font size}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{size} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Font size.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-size-points -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size-points"
                                               'gtk-cell-renderer-text) 't)
 "The @code{size-points} property of type @code{:double} (Read / Write) @br{}
  Font size in points. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-size-points atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-size-points 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-size-points object) => size}
  @syntax[]{(setf (gtk-cell-renderer-text-size-points object) size)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[size]{a @code{:double} with the font size in points}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{size-points} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Font size in points.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-size-set ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{size-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font size. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-size-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-size-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-size-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-size-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects the font size}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{size-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the font size.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-stretch -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stretch"
                                               'gtk-cell-renderer-text) 't)
 "The @code{stretch} property of type @symbol{pango-stretch} (Read / Write)
  @br{}
  Font stretch. @br{}
  Default value: @code{:normal}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-stretch atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-stretch 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-stretch object) => stretch}
  @syntax[]{(setf (gtk-cell-renderer-text-stretch object) stretch)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[stretch]{a @symbol{pango-stretch} value}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{stretch} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Font stretch.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-stretch-set -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stretch-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{stretch-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font stretch. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-stretch-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-stretch-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-stretch-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-stretch-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects the font stretch}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{stretch-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the font stretch.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-strikethrough -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "strikethrough"
                                               'gtk-cell-renderer-text) 't)
 "The @code{strikethrough} property @code{:boolean} (Read / Write) @br{}
  Whether to strike through the text. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-strikethrough atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-strikethrough 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-strikethrough object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-strikethrough object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether to strike through the text}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{strikethrough} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether to strike through the text.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-strikethrough-set -------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "strikethrough-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{strikethrough-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects strikethrough. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-strikethrough-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-strikethrough-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-strikethrough-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-strikethrough-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects strikethrough}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{strikethrough-set} slot of
    the @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects strikethrough.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-style -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "style"
                                               'gtk-cell-renderer-text) 't)
 "The @code{style} property of type @symbol{pango-style} (Read / Write) @br{}
  Font style. @br{}
  Default value: @code{:normal}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-style 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-style object) => style}
  @syntax[]{(setf (gtk-cell-renderer-text-style object) style)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[style]{a @symbol{pango-style} value}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{style} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Font style.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-style-set ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "style-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{style-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font style. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-style-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-style-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-style-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-style-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a @symbol{pango-style} value}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{style-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the font style.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-text --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text"
                                               'gtk-cell-renderer-text) 't)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  Text to render. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-text 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-text object) => text}
  @syntax[]{(setf (gtk-cell-renderer-text-text object) text)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[text]{a string with the text to render}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{text} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Text to render.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-underline ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "underline"
                                               'gtk-cell-renderer-text) 't)
 "The @code{underline} property of type @symbol{pango-underline} (Read / Write)
  @br{}
  Style of underline for this text. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-underline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-underline 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-underline object) => underline}
  @syntax[]{(setf (gtk-cell-renderer-text-underline object) underline)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[underline]{a @symbol{pango-underline} value}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{underline} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Style of underline for this text.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-underline-set -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "underline-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{underline-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects underlining. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-underline-set
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-underline-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-underline-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-underline-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects underlining}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{underline-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects underlining.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-variant -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "variant"
                                               'gtk-cell-renderer-text) 't)
 "The @code{variant} property of type @symbol{pango-variant} (Read / Write)
  @br{}
  Font variant. @br{}
  Default value: @code{:normal}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-variant atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-variant 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-variant object) => variant}
  @syntax[]{(setf (gtk-cell-renderer-text-variant object) variant)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[variant]{a @symbol{pango-variant} value}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{variant} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Font variant.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-variant-set -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "variant-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{variant-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font variant. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-variant-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-variant-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-variant-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-variant-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects the font variant}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{variant-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the font variant.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-weight ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "weight"
                                               'gtk-cell-renderer-text) 't)
 "The @code{weight} property of type @code{:int} (Read / Write) @br{}
  Font weight. @br{}
  Allowed values: >= 0 @br{}
  Default value: 400")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-weight atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-weight 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-weight object) => weight}
  @syntax[]{(setf (gtk-cell-renderer-text-weight object) weight)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[weight]{an integer with the font weight}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{weight} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Font weight.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-weight-set --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "weight-set"
                                               'gtk-cell-renderer-text) 't)
 "The @code{weight-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font weight. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-weight-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-weight-set 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-weight-set object) => setting}
  @syntax[]{(setf (gtk-cell-renderer-text-weight-set object) setting)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[setting]{a boolean wether this tag affects the font weight}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{weight-set} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Whether this tag affects the font weight.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-width-chars -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width-chars"
                                               'gtk-cell-renderer-text) 't)
 "The @code{width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired width of the cell, in characters. If this property is set to -1,
  the width will be calculated automatically, otherwise the cell will request
  either 3 characters or the property value, whichever is greater. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-width-chars 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-width-chars object) => width}
  @syntax[]{(setf (gtk-cell-renderer-text-width-chars object) width)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[width]{an integer with the width of the cell}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{width-chars} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  The desired width of the cell, in characters. If this property is set to -1,
  the width will be calculated automatically, otherwise the cell will request
  either 3 characters or the property value, whichever is greater.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-wrap-mode ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-mode"
                                               'gtk-cell-renderer-text) 't)
 "The @code{wrap-mode} property of type @symbol{pango-wrap-mode} (Read / Write)
  @br{}
  Specifies how to break the string into multiple lines, if the cell renderer
  does not have enough room to display the entire string. This property has no
  effect unless the @code{wrap-width} property is set. @br{}
  Default value: @code{:char}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-wrap-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-wrap-mode 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-wrap-mode object) => mode}
  @syntax[]{(setf (gtk-cell-renderer-text-wrap-mode object) mode)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[mode]{a @symbol{pango-wrap-mode} value}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-text]{wrap-mode} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Specifies how to break the string into multiple lines, if the cell renderer
  does not have enough room to display the entire string. This property has no
  effect unless the @code{wrap-width} property is set.
  @see-class{gtk-cell-renderer-text}")

;;; --- gtk-cell-renderer-text-wrap-width --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-width"
                                               'gtk-cell-renderer-text) 't)
 "The @code{wrap-width} property of type @code{:int} (Read / Write) @br{}
  Specifies the minimum width at which the text is wrapped. The
  @code{wrap-mode} property can be used to influence at what character
  positions the line breaks can be placed. Setting @code{wrap-width} to -1
  turns wrapping off. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-text-wrap-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-text-wrap-width 'function)
 "@version{2020-6-13}
  @syntax[]{(gtk-cell-renderer-text-wrap-width object) => width}
  @syntax[]{(setf (gtk-cell-renderer-text-wrap-width object) width)}
  @argument[object]{a @class{gtk-cell-renderer-text} object}
  @argument[width]{an integer with the minimum width at which text is wrapped}0
  @begin{short}
    Accessor of the slot @code{wrap-width} slot of the
    @class{gtk-cell-renderer-text} class.
  @end{short}

  Specifies the minimum width at which the text is wrapped. The
  @code{wrap-mode} property can be used to influence at what character
  positions the line breaks can be placed. Setting @code{wrap-width} to -1
  turns wrapping off.
  @see-class{gtk-cell-renderer-text}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_text_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-text-new))

(defun gtk-cell-renderer-text-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @return{The new @class{gtk-cell-renderer-text} object.}
  @begin{short}
    Creates a new cell renderer text.
  @end{short}
  Adjust how text is drawn using object properties. Object properties can be
  set globally, with the function @fun{g-object-property}. Also, with
  @class{gtk-tree-view-column}, you can bind a property to a value in a
  @class{gtk-tree-model}. For example, you can bind the
  @slot[gtk-cell-renderer-text]{text} property on the cell renderer to a string
  value in the model, thus rendering a different string in each row of the
  @class{gtk-tree-view}.
  @see-class{gtk-cell-renderer-text}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-view-column}
  @see-class{gtk-tree-model}
  @see-function{g-object-property}"
  (make-instance 'gtk-cell-renderer-text))

(export 'gtk-cell-renderer-text-new)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_text_set_fixed_height_from_font ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_renderer_text_set_fixed_height_from_font"
          gtk-cell-renderer-text-set-fixed-height-from-font) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-13}
  @argument[renderer]{a @class{gtk-cell-renderer-text} object}
  @argument[number-of-rows]{an integer with the number of rows of text each cell
    renderer is allocated, or -1}
  @begin{short}
    Sets the height of a renderer to explicitly be determined by the
    @code{font} and @code{y-pad} properties set on it.
  @end{short}
  Further changes in these properties do not affect the height, so they must be
  accompanied by a subsequent call to this function. Using this function is
  unflexible, and should really only be used if calculating the size of a cell
  is too slow, i.e., a massive number of cells displayed. If
  @arg{number-of-rows} is -1, then the fixed height is unset, and the height is
  determined by the properties again.
  @see-class{gtk-cell-renderer-text}"
  (renderer (g-object gtk-cell-renderer-text))
  (number-of-rows :int))

(export 'gtk-cell-renderer-text-set-fixed-height-from-font)

;;; --- End of file gtk.cell-renderer-text.lisp --------------------------------
