;;; ----------------------------------------------------------------------------
;;; gtk.color-button.lisp
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
;;;        guint    alpha          Read / Write
;;;     GdkColor*   color          Read / Write
;;;      GdkRGBA*   rgba           Read / Write
;;;     gboolean    show-editor    Read / Write
;;;        gchar*   title          Read / Write
;;;     gboolean    use-alpha      Read / Write
;;;
;;; Signals
;;;
;;;         void    color-set      Run First
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
 "@version{2020-5-23}
  @begin{short}
    @sym{gtk-color-button} is a button which displays the currently selected
    color and allows to open a color selection dialog to change the color. It
    is a suitable widget for selecting a color in a preference dialog.
  @end{short}

  @image[color-button]{}
  @begin[Signal Details]{dictionary}
    @subheading{The \"color-set\" signal}
      @begin{pre}
 lambda (widget)    : Run First
      @end{pre}
      The \"color-set\" signal is emitted when the user selects a color. When
      handling this signal, use the functions @fun{gtk-color-button-color}
      and @fun{gtk-color-button-alpha}, or the function
      @fun{gtk-color-button-rgba} to find out which color was just selected.
      Note that this signal is only emitted when the user changes the color.
      If you need to react to programmatic color changes as well, use the
      \"notify::color\" signal.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-color-button} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-color-button-alpha}
  @see-slot{gtk-color-button-color}
  @see-slot{gtk-color-button-rgba}
  @see-slot{gtk-color-button-show-editor}
  @see-slot{gtk-color-button-title}
  @see-slot{gtk-color-button-use-alpha}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-color-button-alpha -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "alpha" 'gtk-color-button) 't)
 "The @code{alpha} property of tpye @code{:uint} (Read / Write) @br{}
  The selected opacity value, 0 fully transparent, 65535 fully opaque. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 65535")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-button-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-alpha 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-color-button-alpha object) => alpha)}
  @syntax[]{(setf (gtk-color-button-alpha object) alpha)}
  @argument[object]{a @class{gtk-color-button} widget}
  @argument[alpha]{an integer between 0 and 65535}
  @begin{short}
    Accessor of the @slot[gtk-color-button]{alpha} slot of the
    @class{gtk-color-button} class.
  @end{short}

  The slot access function @sym{gtk-color-button-alpha} returns the current
  alpha value. The slot access function @sym{(setf gtk-color-button-alpha)}
  sets the current opacity to be @arg{alpha}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-button-alpha} has been deprecated since version
    3.4 and should not be used in newly-written code. Use the function
    @fun{gtk-color-chooser-rgba} instead.
  @end{dictionary}
  @see-class{gtk-color-button}
  @see-function{gtk-color-chooser-rgba}")

;;; gtk-color-button-color -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "color" 'gtk-color-button) 't)
 "The @code{color} property of type @class{gdk-color} (Read / Write) @br{}
  The selected color. @br{}
  @em{Warning:} The @code{color} property has been deprecated since version 3.4
  and should not be used in newly-written code. Use the @code{rgba}
  property instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-button-color atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-color 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-color-button-color object) => color)}
  @syntax[]{(setf (gtk-color-button-color object) color)}
  @argument[object]{a @class{gtk-color-button} widget}
  @argument[color]{a @class{gdk-color} to set the current color with}
  @begin{short}
    Accessor of the @slot[gtk-color-button]{color} slot of the
    @class{gtk-color-button} class.
  @end{short}

  The slot access function @sym{(setf gtk-color-button-color)} sets
  @arg{color} to be the current color in the color button.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-button-color} is deprecated and should not be
    used in newly-written code. Use the function @fun{gtk-color-chooser-rgba}
    instead.
  @end{dictionary}
  @see-class{gtk-color-button}
  @see-function{gtk-color-chooser-rgba}")

;;; --- gtk-color-button-rgba --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rgba" 'gtk-color-button) 't)
 "The @code{rgba} property of type @class{gdk-rgba} (Read / Write) @br{}
  The selected RGBA color.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-button-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-rgba 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-color-button-rgba object) => rgba)}
  @syntax[]{(setf (gtk-color-button-rgba object) rgba)}
  @argument[object]{a @class{gtk-color-button} widget}
  @argument[rgba]{a @class{gdk-rgba} to set the current color with}
  @begin{short}
    Accessor of the @slot[gtk-color-button]{rgba} slot of the
    @class{gtk-color-button} class.
  @end{short}

  The slot access function @sym{gtk-color-button-rgba} returns the current color
  in the color button. The slot access function @sym{(setf gtk-color-button)}
  sets the current color to be @arg{rgba}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-button-rgba} has been deprecated since version
    3.4 and should not be used in newly-written code. Use the function
    @fun{gtk-color-chooser-rgba} instead.
  @end{dictionary}
  @see-class{gtk-color-button}
  @see-function{gtk-color-chooser-rgba}")

;;; --- gtk-color-button-show-editor -------------------------------------------

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "show-editor"
                                               'gtk-color-button) 't)
 "The @code{show-editor} property of type @code{:boolean} (Read / Write) @br{}
  Set this property to @em{true} to skip the palette in the dialog and go
  directly to the color editor. This property should be used in cases where the
  palette in the editor would be redundant, such as when the color button is
  already part of a palette. Since 3.20 @br{}
  Default value: @em{false}")

#+(and gtk-3-20 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-color-button-show-editor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-show-editor 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-color-button-show-editor object) => show-editor)}
  @syntax[]{(setf (gtk-color-button-show-editor object) show-editor)}
  @argument[object]{a @class{gtk-color-button} widget}
  @argument[show-editor]{a boolean wether to skip the palette in the dialog}
  @begin{short}
    Accessor of the @slot[gtk-color-button]{show-editor} slot of the
    @class{gtk-color-button} class.
  @end{short}

  Set this property to @em{true} to skip the palette in the dialog and go
  directly to the color editor. This property should be used in cases where the
  palette in the editor would be redundant, such as when the color button is
  already part of a palette.

  Since 3.20
  @see-class{gtk-color-button}")

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
 "@version{2020-5-23}
  @syntax[]{(gtk-color-button-title object) => title)}
  @syntax[]{(setf (gtk-color-button-title object) title)}
  @argument[object]{a @class{gtk-color-button} widget}
  @argument[title]{string containing the window title}
  @begin{short}
    Accessor of the @slot[gtk-color-button]{title} slot of the
    @class{gtk-color-button} class.
  @end{short}

  The slot access function @sym{gtk-color-button-title} gets the title of the
  color selection dialog. The slot access function
  @sym{(setf gtk-color-button-title)} sets the title for the color selection
  dialog.
  @see-class{gtk-color-button}")

;;; gtk-color-button-use-alpha -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-alpha"
                                               'gtk-color-button) 't)
 "The @code{use-alpha} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the color swatch on the button is
  rendered against a checkerboard background to show its opacity and the opacity
  slider is displayed in the color selection dialog. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-button-use-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-button-use-alpha 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-color-button-use-alpha object) => use-alpha)}
  @syntax[]{(setf (gtk-color-button-use-alpha object) use-alpha)}
  @argument[object]{a @class{gtk-color-button} widget}
  @argument[use-alpha]{@em{true} if the color button should use alpha channel,
    @em{false} if not}
  @begin{short}
    Accessor of the @slot[gtk-color-button]{use-alpha} slot of the
    @class{gtk-color-button} class.
  @end{short}

  Sets whether or not the color button should use the alpha channel.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-button-use-alpha} has been deprecated since
    version 3.4 and should not be used in newly-written code. Use the function
    @fun{gtk-color-chooser-use-alpha} function instead.
  @end{dictionary}
  @see-class{gtk-color-button}
  @see-function{gtk-color-chooser-use-alpha}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-new))

(defun gtk-color-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @return{A new @class{gtk-color-button} widget.}
  @short{Creates a new color button.}

  This returns a widget in the form of a small button containing a swatch
  representing the current selected color. When the button is clicked, a
  color-selection dialog will open, allowing the user to select a color. The
  swatch will be updated to reflect the new color when the user finishes.
  @see-class{gtk-color-button}"
  (make-instance 'gtk-color-button))

(export 'gtk-color-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new_with_color ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-new-with-color))

(defun gtk-color-button-new-with-color (color)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @argument[color]{a @class{gdk-color} to set the current color with}
  @return{A new @class{gtk-color-button} widget.}
  @short{Creates a new color button.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-button-new-with-color} has been deprecated since
    version 3.4 and should not be used in newly-written code. Use the function
    @fun{gtk-color-button-new-with-rgba} instead.
  @end{dictionary}
  @see-class{gtk-color-button}
  @see-function{gtk-color-button-new-with-rgba}"
  (make-instance 'gtk-color-button
                 :color color))

(export 'gtk-color-button-new-with-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new_with_rgba ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-button-new-with-rgba))

(defun gtk-color-button-new-with-rgba (rgba)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @argument[rgba]{a @class{gdk-rgba} to set the current color with}
  @return{A new @class{gtk-color-button} widget.}
  @short{Creates a new color button.}
  @see-class{gtk-color-button}"
  (make-instance 'gtk-color-button
                 :rgba rgba))

(export 'gtk-color-button-new-with-rgba)

;;; --- End of file gtk.color-button.lisp --------------------------------------
