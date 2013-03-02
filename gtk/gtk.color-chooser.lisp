;;; ----------------------------------------------------------------------------
;;; gtk.color-chooser.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; GtkColorChooser
;;;
;;; Interface implemented by widgets for choosing colors
;;;
;;; Synopsis
;;;
;;;     GtkColorChooser
;;;
;;;     gtk_color_chooser_get_rgba
;;;     gtk_color_chooser_set_rgba
;;;     gtk_color_chooser_get_use_alpha
;;;     gtk_color_chooser_set_use_alpha
;;;     gtk_color_chooser_add_palette
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColorChooser
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkColorChooser" gtk-color-chooser
  (:export t
   :type-initializer "gtk_color_chooser_get_type")
  (rgba
   gtk-color-chooser-rgba
   "rgba" "GdkRGBA" t t)
  (use-alpha
   gtk-color-chooser-use-alpha
   "use-alpha" "gboolean" t t))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-chooser atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-color-chooser 'type)
 "@version{2013-2-24}
  @begin{short}
    GtkColorChooser is an interface that is implemented by widgets for choosing
    colors.
  @end{short}
  Depending on the situation, colors may be allowed to have alpha
  (translucency).

  In GTK+, the main widgets that implement this interface are
  GtkColorChooserWidget, GtkColorChooserDialog and GtkColorButton.
  @begin[Signal Details]{dictionary}
    @subheading{The \"color-activated\" signal}
      Emitted when a color is activated from the color chooser. This usually
      happens when the user clicks a color swatch, or a color is selected and
      the user presses one of the keys Space, Shift+Space, Return or Enter.
      @begin{pre}
 void user_function (GtkColorChooser *chooser,
                     GdkRGBA         *color,
                     gpointer         user_data)      : Run First
      @end{pre}
      @begin[code]{table}
        @entry[chooser]{the object which received the signal}
        @entry[color]{the color}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}
      Since 3.4
  @end{dictionary}
  @see-slot{gtk-color-chooser-rgba}
  @see-slot{gtk-color-chooser-use-alpha}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rgba" 'gtk-color-chooser) 't)
 "The @code{\"rgba\"} property of type @code{GdkRGBA*} (Read / Write)@br{}
  The @code{\"rgba\"} property contains the currently selected color, as a
  GdkRGBA struct. The property can be set to change the current selection
  programmatically.@br{}
  Since 3.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-alpha" 'gtk-color-chooser) 't)
 "The @code{\"use-alpha\"} property of type @code{gboolean} (Read / Write)@br{}
  When @code{\"use-alpha\"} is TRUE, colors may have alpha (translucency)
  information. When it is FALSE, the GdkRGBA struct obtained via the
  @code{\"rgba\"} property will be forced to have alpha == 1.@br{}
  Implementations are expected to show alpha by rendering the color over a
  non-uniform background (like a checkerboard pattern).@br{}
  Default value: TRUE@br{}
  Since 3.4")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-color-chooser-rgba -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-chooser-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-chooser-rgba 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"rgba\"} of the @class{gtk-color-chooser} class.
  @end{short}")

;;; --- gtk-color-chooser-use-alpha --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-chooser-use-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-chooser-use-alpha 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"use-alpha\"} of the @class{gtk-color-chooser}#
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_get_rgba ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-get-rgba))

(defun gtk-color-chooser-get-rgba (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-24}
  @argument[chooser]{a GtkColorChooser}
  @argument[color]{return location for the color}
  @short{Gets the currently-selected color.}

  Since 3.4"
  (gtk-color-chooser-rgba chooser))

(export 'gtk-color-chooser-get-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_set_rgba ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-set-rgba))

(defun gtk-color-chooser-set-rgba (chooser color)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-24}
  @argument[chooser]{a GtkColorChooser}
  @argument[color]{the new color}
  @short{Sets the color.}

  Since 3.4"
  (setf (gtk-color-chooser-rgba chooser) color))

(export 'gtk-color-chooser-set-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_get_use_alpha ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-get-use-alpha))

(defun gtk-color-chooser-get-use-alpha (chooser)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-24}
  @argument[chooser]{a GtkColorChooser}
  @return{TRUE if the color chooser uses the alpha channel, FALSE if not}
  @begin{short}
    Returns whether the color chooser shows the alpha channel.
  @end{short}

  Since 3.4"
  (gtk-color-chooser-use-alpha chooser))

(export 'gtk-color-chooser-get-use-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_set_use_alpha ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-color-chooser-set-use-alpha))

(defun gtk-color-chooser-set-use-alpha (chooser use-alpha)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-24}
  @argument[chooser]{a GtkColorChooser}
  @argument[use-alpha]{TRUE if color chooser should use alpha channel,
    FALSE if not}
  @begin{short}
    Sets whether or not the color chooser should use the alpha channel.
  @end{short}

  Since 3.4"
  (setf (gtk-color-chooser-use-alpha chooser) use-alpha))

(export 'gtk-color-chooser-set-use-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_add_palette ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_chooser_add_palette" %gtk-color-chooser-add-palette) :void
  (chooser (g-object gtk-color-chooser))
  (orientation gtk-orientation)
  (colors-per-line :int)
  (n-colors :int)
  (colors :pointer))

(defun gtk-color-chooser-add-palette (chooser
                                      orientation
                                      colors-per-line
                                      colors)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-24}
  @argument[chooser]{a GtkColorChooser}
  @argument[orientation]{GTK_ORIENTATION_HORIZONTAL if the palette should be
    displayed in rows, GTK_ORIENTATION_VERTICAL for columns}
  @argument[colors_per_line]{the number of colors to show in each row/column}
  @argument[colors]{the colors of the palette, or NULL}
  @begin{short}
    Adds a palette to the color chooser. If orientation is horizontal, the
    colors are grouped in rows, with colors_per_line colors in each row. If
    horizontal is FALSE, the colors are grouped in columns instead.
  @end{short}

  The default color palette of GtkColorChooserWidget has 27 colors, organized
  in columns of 3 colors. The default gray palette has 9 grays in a single row.

  The layout of the color chooser widget works best when the palettes have
  9-10 columns.

  Calling this function for the first time has the side effect of removing the
  default color and gray palettes from the color chooser.

  If colors is NULL, removes all previously added palettes.

  Since 3.4"
  (with-foreign-boxed-array (n-colors colors-ptr gdk-rgba colors)
    (%gtk-color-chooser-add-palette chooser
                                    orientation
                                    colors-per-line
                                    n-colors
                                    colors-ptr)))

(export 'gtk-color-chooser-add-palette)

;;; --- End of file gtk.color-chooser.lisp -------------------------------------
