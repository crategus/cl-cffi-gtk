;;; ----------------------------------------------------------------------------
;;; gtk.color-button.lisp
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
;;; GtkColorButton
;;;
;;;     A button to launch a color selection dialog
;;;
;;; Types and Values
;;;
;;;     GtkColorButton
;;;
;;; Functions
;;;
;;;     gtk_color_button_new
;;;     gtk_color_button_new_with_color
;;;     gtk_color_button_new_with_rgba
;;;     gtk_color_button_set_color                         Accessor
;;;     gtk_color_button_get_color                         Accessor
;;;     gtk_color_button_set_alpha                         Accessor
;;;     gtk_color_button_get_alpha                         Accessor
;;;     gtk_color_button_set_rgba                          Accessor
;;;     gtk_color_button_get_rgba                          Accessor
;;;     gtk_color_button_set_use_alpha                     Accessor
;;;     gtk_color_button_get_use_alpha                     Accessor
;;;     gtk_color_button_set_title                         Accessor
;;;     gtk_color_button_get_title                         Accessor
;;;
;;; Properties
;;;
;;;        guint   alpha          Read / Write
;;;     GdkColor*  color          Read / Write
;;;      GdkRGBA*  rgba           Read / Write
;;;     gboolean   show-editor    Read / Write
;;;        gchar*  title          Read / Write
;;;     gboolean   use-alpha      Read / Write
;;;
;;; Signals
;;;
;;;         void   color-set      Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkColorButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkColorButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable, GtkActivatable and GtkColorChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkColorButton" gtk-color-button
  (:superclass gtk-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable"
                "GtkColorChooser")
   :type-initializer "gtk_color_button_get_type")
  ((alpha
    gtk-color-button-alpha
    "alpha" "guint" t t)
   (color
    gtk-color-button-color
    "color" "GdkColor" t t)
   (rgba
    gtk-color-button-rgba
    "rgba" "GdkRGBA" t t)
   #+gtk-3-20
   (show-editor
    gtk-color-buton-show-editor
    "show-editor" "gboolean" t t)
   (title
    gtk-color-button-title
    "title" "gchararray" t t)
   (use-alpha
    gtk-color-button-use-alpha
    "use-alpha" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-color-button 'type)
 "@version{2013-2-24}
  @begin{short}
    The @sym{gtk-color-button} is a button which displays the currently selected
    color and allows to open a color selection dialog to change the color. It is
    suitable widget for selecting a color in a preference dialog.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"color-set\" signal}
      @begin{pre}
 lambda (widget)    : Run First
      @end{pre}
      The \"color-set\" signal is emitted when the user selects a color. When
      handling this signal, use the @fun{gtk-color-button-get-color} function
      and the @fun{gtk-color-button-get-alpha} (or the
      @fun{gtk-color-button-get-rgba} function) to find out which color was just
      selected.
      Note that this signal is only emitted when the user changes the color. If
      you need to react to programmatic color changes as well, use the
      \"notify::color\" signal.
      @begin[code]{table}
        @entry[widget]{The object which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-color-button-alpha}
  @see-slot{gtk-color-button-color}
  @see-slot{gtk-color-button-rgba}
  @see-slot{gtk-color-button-title}
  @see-slot{gtk-color-button-use-alpha}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-color-button-alpha -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "alpha" 'gtk-color-button) 't)
 "The @code{alpha} property of tpye @code{:uint} (Read / Write) @br{}
  The selected opacity value (0 fully transparent, 65535 fully opaque). @br{}
  Allowed values: <= 65535 @br{}
  Default value: 65535")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-button-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-alpha 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @slot[gtk-color-button]{alpha} of the
    @class{gtk-color-button} class.
  @end{short}")

;;; gtk-color-button-color -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "color" 'gtk-color-button) 't)
 "The @code{color} property of type @class{gdk-color} (Read / Write) @br{}
  The selected color. @br{}
  @b{Warning:}
  @code{color} has been deprecated since version 3.4 and should not be
  used in newly-written code. Use @code{rgba} instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-button-color atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-color 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @slot[gtk-color-button]{color} of the
    @class{gtk-color-button} class.
  @end{short}")

;;; --- gtk-color-button-rgba --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rgba" 'gtk-color-button) 't)
 "The @code{rgba} property of type @class{gdk-rgba} (Read / Write) @br{}
  The RGBA color. @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-button-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-rgba 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @slot[gtk-color-button]{rgba} of the
    @class{gtk-color-button} class.
  @end{short}")

;;; --- gtk-color-button-show-editor -------------------------------------------

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "show-editor" 'gtk-color-button)
                     't)
 "The @code{show-editor} property of type @code{:boolean} (Read / Write) @br{}
  Set this property to @em{true} to skip the palette in the dialog and go
  directly to the color editor. This property should be used in cases where the
  palette in the editor would be redundant, such as when the color button is
  already part of a palette. @br{}
  Default value: @code{nil} @br{}
  Since 3.20")

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-color-button-show-editor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-show-editor 'function)
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-color-button]{show-editor} of the
    @class{gtk-color-button} class.
  @end{short}")

;;; --- gtk-color-button-title -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-color-button) 't)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the color selection dialog. @br{}
  Default value: \"Pick a Color\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-button-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-title 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @slot[gtk-color-button]{title} of the
    @class{gtk-color-button} class.
  @end{short}")

;;; gtk-color-button-use-alpha -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-alpha"
                                               'gtk-color-button) 't)
 "The @code{use-alpha} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the color swatch on the button is
  rendered against a checkerboard background to show its opacity and the opacity
  slider is displayed in the color selection dialog. @br{}
  Default value: @code{nil} @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-button-use-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-use-alpha 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @slot[gtk-color-button]{use-alpha} of the
    @class{gtk-color-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-new))

(defun gtk-color-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @return{a new color button}
  @short{Creates a new color button.}

  This returns a widget in the form of a small button containing a swatch
  representing the current selected color. When the button is clicked, a
  color-selection dialog will open, allowing the user to select a color. The
  swatch will be updated to reflect the new color when the user finishes.

  Since 2.4"
  (make-instance 'gtk-color-button))

(export 'gtk-color-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new_with_color ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-new-with-color))

(defun gtk-color-button-new-with-color (color)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[color]{a @class{gdk-color} to set the current color with}
  @return{A new color button.}
  @short{Creates a new color button.}
  @begin[Warning]{dictionary}
    The @sym{gtk-color-button-new-with-color} function has been deprecated since
    version 3.4 and should not be used in newly-written code. Use the
    @fun{gtk-color-button-new-with-rgba} function instead.
  @end{dictionary}"
  (make-instance 'gtk-color-button
                 :color color))

(export 'gtk-color-button-new-with-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new_with_rgba ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-new-with-rgba))

(defun gtk-color-button-new-with-rgba (rgba)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[rgba]{a @class{gdk-rgba} to set the current color with}
  @return{A new color button.}
  @short{Creates a new color button.}

  Since 3.0"
  (make-instance 'gtk-color-button
                 :rgba rgba))

(export 'gtk-color-button-new-with-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_set_color ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-set-color))

(defun gtk-color-button-set-color (button color)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[button]{a @class{gtk-color-button} widget}
  @argument[color]{a @class{gdk-color} to set the current color with}
  @subheading{Warning}
    The @fun{gtk-color-button-set-color} function is deprecated and should not
    be used in newly-written code. Use the @fun{gtk-color-chooser-set-rgba}
    instead.

  @short{Sets the current color to be color.}

  Since 2.4"
  (setf (gtk-color-button-color button) color))

(export 'gtk-color-button-set-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_get_color ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-get-color))

(defun gtk-color-button-get-color (button)
 #+cl-cffi-gtk-documentation
 "@version{2019-4-22}
  @argument[button]{a @class{gtk-color-button} widget}
  @argument[color]{a @class{gdk-color} to fill in with the current color}
  @begin{short}
    Sets color to be the current color in the @class{gtk-color-button} widget.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-color-button-get-color} function has been deprecated since
    version 3.4 and should not be used in newly-written code. Use the
    @fun{gtk-color-chooser-rgba} slot access function instead.
  @end{dictionary}"
  (gtk-color-button-color button))

(export 'gtk-color-button-get-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_set_alpha ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-set-alpha))

(defun gtk-color-button-set-alpha (button alpha)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[button]{a @class{gtk-color-button} widget}
  @argument[alpha]{an integer between 0 and 65535}
  @subheading{Warning}
    The @fun{gtk-color-button-set-alpha} function has been deprecated since
    version 3.4 and should not be used in newly-written code. Use the
    @fun{gtk-color-chooser-set-rgba} function instead.

  @short{Sets the current opacity to be @arg{alpha}.}

  Since 2.4"
  (setf (gtk-color-button-alpha button) alpha))

(export 'gtk-color-button-set-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_get_alpha ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-get-alpha))

(defun gtk-color-button-get-alpha (button)
 #+cl-cffi-gtk-documentation
 "@version{2019-4-22}
  @argument[button]{a @class{gtk-color-button} widget}
  @return{An integer between 0 and 65535.}
  @short{Returns the current alpha value.}
  @begin[Warning]{dictionary}
    The @sym{gtk-color-button-get-alpha} function has been deprecated since
    version 3.4 and should not be used in newly-written code. Use the
    @fun{gtk-color-chooser-rgba} slot access function instead.
  @end{dictionary}"
  (gtk-color-button-alpha button))

(export 'gtk-color-button-get-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_set_rgba ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-set-rgba))

(defun gtk-color-button-set-rgba (button rgba)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[button]{a @class{gtk-color-button} widget}
  @argument[rgba]{a @class{gdk-rgba} to set the current color with}
  @subheading{Warning}
    The @fun{gtk-color-button-set-rgba} has been deprecated since version 3.4
    and should not be used in newly-written code. Use the
    @fun{gtk-color-chooser-set-rgba} function instead.

  @short{Sets the current color to be @arg{rgba}.}

  Since 3.0"
  (setf (gtk-color-button-rgba button) rgba))

(export 'gtk-color-button-set-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_get_rgba ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-get-rgba))

(defun gtk-color-button-get-rgba (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[button]{a @class{gtk-color-button} widget}
  @begin{short}
    Returns the current color in the @class{gtk-color-button} widget.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-color-button-get-rgba} function has been deprecated since
    version 3.4 and should not be used in newly-written code. Use the
    @fun{gtk-color-chooser-rgba} slot access function instead.
  @end{dictionary}"
  (gtk-color-button-rgba button))

(export 'gtk-color-button-get-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_set_use_alpha ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-set-use-alpha))

(defun gtk-color-button-set-use-alpha (button use-alpha)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[button]{a @class{gtk-color-button} widget}
  @argument[use-alpha]{@em{true} if color button should use alpha channel,
    @code{nil} if not}
  @subheading{Warning}
    The @fun{gtk-color-button-set-use-alpha} function has been deprecated since
    version 3.4 and should not be used in newly-written code. Use the
    @fun{gtk-color-chooser-set-use-alpha} function instead.

  @short{Sets whether or not the color button should use the alpha channel.}

  Since 2.4"
  (setf (gtk-color-button-use-alpha button) use-alpha))

(export 'gtk-color-button-set-use-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_get_use_alpha ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-get-use-alpha))

(defun gtk-color-button-get-use-alpha (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[button]{a @class{gtk-color-button} widget}
  @return{@em{True} if the color sample uses alpha channel, @code{nil} if not.}
  @subheading{Warning}
    The @fun{gtk-color-button-get-use-alpha} function has been deprecated since
    version 3.4 and should not be used in newly-written code. Use the
    @fun{gtk-color-chooser-get-use-alpha} function instead.

  @short{Does the color selection dialog use the alpha channel?}

  Since 2.4"
  (gtk-color-button-use-alpha button))

(export 'gtk-color-button-get-use-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_set_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-set-title))

(defun gtk-color-button-set-title (button title)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[button]{a @class{gtk-color-button} widget}
  @argument[title]{string containing new window title}
  @short{Sets the title for the color selection dialog.}

  Since 2.4"
  (setf (gtk-color-button-title button) title))

(export 'gtk-color-button-set-title)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_get_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-get-title))

(defun gtk-color-button-get-title (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[button]{a @class{gtk-color-button} widget}
  @return{An internal string, do not free the return value.}
  @short{Gets the title of the color selection dialog.}

  Since 2.4"
  (gtk-color-button-title button))

(export 'gtk-color-button-get-title)

;;; --- End of file gtk.color-button.lisp --------------------------------------
