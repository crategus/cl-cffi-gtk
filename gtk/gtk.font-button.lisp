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
;;;     gtk_font_button_set_font_name
;;;     gtk_font_button_get_font_name
;;;     gtk_font_button_set_show_style
;;;     gtk_font_button_get_show_style
;;;     gtk_font_button_set_show_size
;;;     gtk_font_button_get_show_size
;;;     gtk_font_button_set_use_font
;;;     gtk_font_button_get_use_font
;;;     gtk_font_button_set_use_size
;;;     gtk_font_button_get_use_size
;;;     gtk_font_button_set_title
;;;     gtk_font_button_get_title
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
 lambda (widget)   : Run First
      @end{pre}
      The \"font-set\" signal is emitted when the user selects a font. When
      handling this signal, use the function @fun{gtk-font-button-get-font-name}
      to find out which font was just selected.

      Note that this signal is only emitted when the user changes the font. If
      you need to react to programmatic font changes as well, use the
      \"notify::font-name\" signal.
      @begin[code]{table}
        @entry[widget]{The object which received the signal.}
      @end{table}
      Since 2.4
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
  @b{Warning:} @code{font-name} has been deprecated since version 3.22 and
  should not be used in newly-written code. Use the @code{font} property
  instead. @br{}
  Default value: \"Sans 12\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-button-font-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-button-font-name 'function)
 "@version{2013-6-18}
  @begin{short}
    Accessor of the slot @slot[gtk-font-button]{font-name} of the
    @class{gtk-font-button} class.
  @end{short}
  @see-class{gtk-font-button}")

;;; --- gtk-font-button-show-size ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-size" 'gtk-font-button) 't)
 "The @code{show-size} property of type @code{:boolean}
  (Read / Write) @br{}
  If this property is set to @em{true}, the selected font size will be shown in
  the label. For a more WYSIWYG way to show the selected size, see the
  @code{\"use-size\"} property. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-button-show-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-button-show-size 'function)
 "@version{2013-6-18}
  @begin{short}
    Accessor of the slot @slot[gtk-font-button]{show-size} of the
    @class{gtk-font-button} class.
  @end{short}
  @see-class{gtk-font-button}")

;;; --- gtk-font-button-show-style ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-style" 'gtk-font-button) 't)
 "The @code{show-style} property of type @code{:boolean}
  (Read / Write) @br{}
  If this property is set to @em{true}, the name of the selected font style will
  be shown in the label. For a more WYSIWYG way to show the selected style, see
  the @code{\"use-font\"} property. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-button-show-style atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-button-show-style 'function)
 "@version{2013-6-18}
  @begin{short}
    Accessor of the slot @slot[gtk-font-button]{show-style} of the
    @class{gtk-font-button} class.
  @end{short}
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
 "@version{2013-6-18}
  @begin{short}
    Accessor of the slot @slot[gtk-font-button]{title} of the
    @class{gtk-font-button} class.
  @end{short}
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
 "@version{2013-6-18}
  @begin{short}
    Accessor of the slot @slot[gtk-font-button]{use-font} of the
    @class{gtk-font-button} class.
  @end{short}
  @see-class{gtk-font-button}")

;;; --- gtk-font-button-use-size -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-size" 'gtk-font-button) 't)
 "The @code{\"use-size\"} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the label will be drawn with the
  selected font size. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-font-button-use-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-font-button-use-size 'function)
 "@version{2013-6-18}
  @begin{short}
    Accessor of the slot @slot[gtk-font-button]{use-size} of the
    @class{gtk-font-button} class.
  @end{short}
  @see-class{gtk-font-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-new))

(defun gtk-font-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @return{A new font picker widget.}
  @short{Creates a new font picker widget.}

  Since 2.4"
  (make-instance 'gtk-font-button))

(export 'gtk-font-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new_with_font ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-new-with-font))

(defun gtk-font-button-new-with-font (fontname)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[fontname]{name of font to display in font chooser dialog}
  @return{A new font picker widget.}
  @short{Creates a new font picker widget.}

  Since 2.4"
  (make-instance 'gtk-font-button
                 :font-name fontname))

(export 'gtk-font-button-new-with-font)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_font_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-set-font-name))

(defun gtk-font-button-set-font-name (font-button fontname)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @argument[fontname]{name of font to display in the font chooser dialog}
  @return{The @arg{fontname}.}
  @begin{short}
    Sets or updates the currently displayed font in the font picker dialog.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-font-button-set-font-name} function has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the
    @fun{gtk-font-chooser-set-font} function instead.
  @end{dictionary}
  @see-function{gtk-font-button-get-font-name}"
  (setf (gtk-font-button-font-name font-button) fontname))

(export 'gtk-font-button-set-font-name)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_font_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-get-font-name))

(defun gtk-font-button-get-font-name (font-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-24}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @return{An internal copy of the font name.}
  @begin{short}
    Retrieves the name of the currently selected font.
  @end{short}
  This name includes style and size information as well. If you want to render
  something with the font, use this string with the function
  @fun{pango-font-description-from-string}. If you are interested in peeking
  certain values (family name, style, size, weight) just query these properties
  from the @class{pango-font-description} object.
  @begin[Warning]{dictionary}
    The @sym{gtk-font-button-get-font-name} function has been deprecated since
    version 3.22 and should not be used in newly-written code. Use the
    @fun{gtk-font-chooser-get-font} function instead.
  @end{dictionary}
  @see-function{pango-font-description-from-string}
  @see-function{gtk-font-button-set-font-name}"
  (gtk-font-button-font-name font-button))

(export 'gtk-font-button-get-font-name)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_show_style ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-set-show-style))

(defun gtk-font-button-set-show-style (font-button show-style)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @argument[show-style]{@em{true} if font style should be displayed in label}
  @begin{short}
    If @arg{show-style} is @em{true}, the font style will be displayed along
    with name of the selected font.
  @end{short}

  Since 2.4"
  (setf (gtk-font-button-show-style font-button) show-style))

(export 'gtk-font-button-set-show-style)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_show_style ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-get-show-style))

(defun gtk-font-button-get-show-style (font-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @return{Whether the font style will be shown in the label.}
  @begin{short}
    Returns whether the name of the font style will be shown in the label.
  @end{short}

  Since 2.4"
  (gtk-font-button-show-style font-button))

(export 'gtk-font-button-get-show-style)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_show_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-set-show-size))

(defun gtk-font-button-set-show-size (font-button show-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @argument[show-size]{@em{true} if font size should be displayed in dialog}
  @begin{short}
    If @arg{show-size} is @em{true}, the font size will be displayed along with
    the name of the selected font.
  @end{short}

  Since 2.4"
  (setf (gtk-font-button-show-size font-button) show-size))

(export 'gtk-font-button-set-show-size)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_show_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-get-show-size))

(defun gtk-font-button-get-show-size (font-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @return{Whether the font size will be shown in the label.}
  @begin{short}
    Returns whether the font size will be shown in the label.
  @end{short}

  Since 2.4"
  (gtk-font-button-show-size font-button))

(export 'gtk-font-button-get-show-size)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_use_font ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-set-use-font))

(defun gtk-font-button-set-use-font (font-button use-font)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @argument[use-font]{if @em{true}, font name will be written using font chosen}
  @begin{short}
    If @arg{use-font} is @em{true}, the font name will be written using the
    selected font.
  @end{short}

  Since 2.4"
  (setf (gtk-font-button-use-font font-button) use-font))

(export 'gtk-font-button-set-use-font)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_use_font ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-get-use-font))

(defun gtk-font-button-get-use-font (font-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @return{Whether the selected font is used in the label.}
  @begin{short}
    Returns whether the selected font is used in the label.
  @end{short}

  Since 2.4"
  (gtk-font-button-use-font font-button))

(export 'gtk-font-button-get-use-font)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_use_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-set-use-size))

(defun gtk-font-button-set-use-size (font-button use-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @argument[use-size]{if @em{true}, font name will be written using the selected
    size}
  @begin{short}
    If @arg{use-size} is @em{true}, the font name will be written using the
    selected size.
  @end{short}

  Since 2.4"
  (setf (gtk-font-button-use-size font-button) use-size))

(export 'gtk-font-button-set-use-size)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_use_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-get-use-size))

(defun gtk-font-button-get-use-size (font-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @return{Whether the selected size is used in the label.}
  @begin{short}
    Returns whether the selected size is used in the label.
  @end{short}

  Since 2.4"
  (gtk-font-button-use-size font-button))

(export 'gtk-font-button-get-use-size)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_set_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-set-title))

(defun gtk-font-button-set-title (font-button title)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @argument[title]{a string containing the font chooser dialog title}
  @begin{short}
    Sets the title for the font chooser dialog.
  @end{short}

  Since 2.4"
  (setf (gtk-font-button-title font-button) title))

(export 'gtk-font-button-set-title)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_get_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-font-button-get-title))

(defun gtk-font-button-get-title (font-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-18}
  @argument[font-button]{a @class{gtk-font-button} widget}
  @return{An internal copy of the title string.}
  @begin{short}
    Retrieves the title of the font chooser dialog.
  @end{short}

  Since 2.4"
  (gtk-font-button-title font-button))

(export 'gtk-font-button-get-title)

;;; --- End of file gtk.font-button.lisp ---------------------------------------
