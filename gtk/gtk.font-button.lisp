;;; ----------------------------------------------------------------------------
;;; gtk.font-button.lisp
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
;;; GtkFontButton
;;;
;;;     A button to launch a font chooser dialog
;;;
;;; Types and Values
;;;
;;;     GtkFontButton
;;;
;;; Functions
;;;
;;;     gtk_font_button_new
;;;     gtk_font_button_new_with_font
;;;     gtk_font_button_set_font_name                      Accessor
;;;     gtk_font_button_get_font_name                      Accessor
;;;     gtk_font_button_set_show_style                     Accessor
;;;     gtk_font_button_get_show_style                     Accessor
;;;     gtk_font_button_set_show_size                      Accessor
;;;     gtk_font_button_get_show_size                      Accessor
;;;     gtk_font_button_set_use_font                       Accessor
;;;     gtk_font_button_get_use_font                       Accessor
;;;     gtk_font_button_set_use_size                       Accessor
;;;     gtk_font_button_get_use_size                       Accessor
;;;     gtk_font_button_set_title                          Accessor
;;;     gtk_font_button_get_title                          Accessor
;;;
;;; Properties
;;;
;;;        gchar*  font-name     Read / Write
;;;     gboolean   show-size     Read / Write
;;;     gboolean   show-style    Read / Write
;;;        gchar*  title         Read / Write
;;;     gboolean   use-font      Read / Write
;;;     gboolean   use-size      Read / Write
;;;
;;; Signals
;;;
;;;         void   font-set      Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkFontButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFontButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable, GtkActivatable and GtkFontChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontButton" gtk-font-button
  (:superclass gtk-button
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActionable"
                 "GtkActivatable"
                 "GtkFontChooser")
    :type-initializer "gtk_font_button_get_type")
  ((font-name
    gtk-font-button-font-name
    "font-name" "gchararray" t t)
   (show-size
    gtk-font-button-show-size
    "show-size" "gboolean" t t)
   (show-style
    gtk-font-button-show-style
    "show-style" "gboolean" t t)
   (title
    gtk-font-button-title
    "title" "gchararray" t t)
   (use-font
    gtk-font-button-use-font
    "use-font" "gboolean" t t)
   (use-size
    gtk-font-button-use-size
    "use-size" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-font-button 'type)
 "@version{2013-6-18}
  @begin{short}
    The @sym{gtk-font-button} is a button which displays the currently selected
    font and allows to open a font chooser dialog to change the font. It is a
    suitable widget for selecting a font in a preference dialog.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"font-set\" signal}
      @begin{pre}
 lambda (widget)    : Run First
      @end{pre}
      The \"font-set\" signal is emitted when the user selects a font. When
      handling this signal, use the @fun{gtk-font-button-font-name} slot access
      function to find out which font was just selected.

      Note that this signal is only emitted when the user changes the font. If
      you need to react to programmatic font changes as well, use the
      \"notify::font-name\" signal.
      @begin[code]{table}
        @entry[widget]{The object which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-font-button-font-name}
  @see-slot{gtk-font-button-show-size}
  @see-slot{gtk-font-button-show-style}
  @see-slot{gtk-font-button-title}
  @see-slot{gtk-font-button-use-font}
  @see-slot{gtk-font-button-use-size}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-font-button-font-name ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "font-name" 'gtk-font-button) 't)
 "The @code{font-name} property of type @code{:string} (Read / Write) @br{}
  The name of the currently selected font. @br{}
  @em{Warning:} @code{font-name} has been deprecated since version 3.22 and
  should not be used in newly-written code. Use the @code{font} property
  instead. @br{}
  Default value: \"Sans 12\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-button-font-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-button-font-name 'function)
 "@version{2019-5-14}
  @syntax[]{(gtk-font-button-font-name object) => fontname}
  @syntax[]{(setf (gtk-font-button-font-name object) fontname)}
  @argument[object]{a @class{gtk-font-button} widget}
  @argument[fontname]{name of font to display in the font chooser dialog}
  @begin{short}
    Accessor of the @slot[gtk-font-button]{font-name} slot of the
    @class{gtk-font-button} class.
  @end{short}

  The @sym{(setf gtk-font-button-font-name)} slot access function
  sets or updates the currently displayed font in the font picker dialog.

  The @sym{gtk-font-name-button-font-name} slot access function
  retrieves the name of the currently selected font.

  This name includes style and size information as well. If you want to render
  something with the font, use this string with the
  @fun{pango-font-description-from-string} function. If you are interested in
  peeking certain values, family name, style, size, weight, just query these
  properties from the @class{pango-font-description} object.
  @begin[Warning]{dictionary}
    The @sym{gtk-font-button-font-name} slot access function has been deprecated
    since version 3.22 and should not be used in newly-written code. Use the
    @fun{gtk-font-chooser-font} function instead.
  @end{dictionary}
  @see-class{gtk-font-button}")

;;; --- gtk-font-button-show-size ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-size" 'gtk-font-button) 't)
 "The @code{show-size} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the selected font size will be shown in
  the label. For a more WYSIWYG way to show the selected size, see the
  @code{use-size} property. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-button-show-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-button-show-size 'function)
 "@version{2019-5-14}
  @syntax[]{(gtk-font-button-show-size object) => show-size}
  @syntax[]{(setf (gtk-font-button-show-size object) show-size)}
  @argument[object]{a @class{gtk-font-button} widget}
  @argument[show-size]{@em{true} if font size should be displayed in dialog}
  @begin{short}
    Accessor of the @slot[gtk-font-button]{show-size} slot of the
    @class{gtk-font-button} class.
  @end{short}

  If @arg{show-size} is @em{true}, the font size will be displayed along with
  the name of the selected font.
  @see-class{gtk-font-button}")

;;; --- gtk-font-button-show-style ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-style" 'gtk-font-button) 't)
 "The @code{show-style} property of type @code{:boolean}
  (Read / Write) @br{}
  If this property is set to @em{true}, the name of the selected font style will
  be shown in the label. For a more WYSIWYG way to show the selected style, see
  the @code{use-font} property. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-button-show-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-button-show-style 'function)
 "@version{2019-5-14}
  @syntax[]{(gtk-font-button-show-style object) => show-style}
  @syntax[]{(setf (gtk-font-button-show-style object) show-style)}
  @argument[object]{a @class{gtk-font-button} widget}
  @argument[show-style]{@em{true} if font style should be displayed in label}
  @begin{short}
    Accessor of the @slot[gtk-font-button]{show-style} slot of the
    @class{gtk-font-button} class.
  @end{short}

  If @arg{show-style} is @em{true}, the font style will be displayed along with
  name of the selected font.
  @see-class{gtk-font-button}")

;;; --- gtk-font-button-title --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-font-button) 't)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the font chooser dialog. @br{}
  Default value: \"Pick a Font\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-button-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-button-title 'function)
 "@version{2019-5-14}
  @syntax[]{(gtk-font-button-title object) => title}
  @syntax[]{(setf (gtk-font-button-title object) title)}
  @argument[object]{a @class{gtk-font-button} widget}
  @argument[title]{a string containing the font chooser dialog title}
  @begin{short}
    Accessor of the @slot[gtk-font-button]{title} slot of the
    @class{gtk-font-button} class.
  @end{short}

  The @sym{gtk-font-button-title} slot access function
  retrieves the title of the font chooser dialog.

  The @sym{(setf gtk-font-button-title)} slot access function
  sets the title for the font chooser dialog.
  @see-class{gtk-font-button}")

;;; --- gtk-font-button-use-font -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-font" 'gtk-font-button) 't)
 "The @code{use-font} property of type  @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the label will be drawn in the selected
  font. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-button-use-font atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-button-use-font 'function)
 "@version{2019-5-14}
  @syntax[]{(gtk-font-button-title object) => use-font}
  @syntax[]{(setf (gtk-font-button-title object) use-font)}
  @argument[object]{a @class{gtk-font-button} widget}
  @argument[use-font]{if @em{true}, font name will be written using font chosen}
  @begin{short}
    Accessor of the @slot[gtk-font-button]{use-font} slot of the
    @class{gtk-font-button} class.
  @end{short}

  If @arg{use-font} is @em{true}, the font name will be written using the
  selected font.
  @see-class{gtk-font-button}")

;;; --- gtk-font-button-use-size -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-size" 'gtk-font-button) 't)
 "The @code{use-size} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the label will be drawn with the
  selected font size. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-button-use-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-button-use-size 'function)
 "@version{2019-5-14}
  @syntax[]{(gtk-font-button-title object) => title}
  @syntax[]{(setf (gtk-font-button-title object) title)}
  @argument[object]{a @class{gtk-font-button} widget}
  @argument[use-size]{if @em{true}, font name will be written using the selected
    size}
  @begin{short}
    Accessor of the @slot[gtk-font-button]{use-size} slot of the
    @class{gtk-font-button} class.
  @end{short}

  If @arg{use-size} is @em{true}, the font name will be written using the
  selected size.
  @see-class{gtk-font-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-new))

(defun gtk-font-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2019-5-14}
  @return{A new font picker widget.}
  @short{Creates a new font picker widget.}
  @see-class{gtk-font-button}"
  (make-instance 'gtk-font-button))

(export 'gtk-font-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new_with_font ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-new-with-font))

(defun gtk-font-button-new-with-font (fontname)
 #+cl-cffi-gtk-documentation
 "@version{2019-5-14}
  @argument[fontname]{name of font to display in font chooser dialog}
  @return{A new font picker widget.}
  @short{Creates a new font picker widget.}
  @see-class{gtk-font-button}"
  (make-instance 'gtk-font-button
                 :font-name fontname))

(export 'gtk-font-button-new-with-font)

;;; --- End of file gtk.font-button.lisp ---------------------------------------
