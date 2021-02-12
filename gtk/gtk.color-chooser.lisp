;;; ----------------------------------------------------------------------------
;;; gtk.color-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;;     Interface implemented by widgets for choosing colors
;;;
;;; Types and Values
;;;
;;;     GtkColorChooser
;;;
;;; Functions
;;;
;;;     gtk_color_chooser_get_rgba                         Accessor
;;;     gtk_color_chooser_set_rgba                         Accessor
;;;     gtk_color_chooser_get_use_alpha                    Accessor
;;;     gtk_color_chooser_set_use_alpha                    Accessor
;;;     gtk_color_chooser_add_palette
;;;
;;; Properties
;;;
;;;      GdkRGBA*   rgba               Read / Write
;;;     gboolean    use-alpha          Read / Write
;;;
;;; Signals
;;;
;;;         void    color-activated    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkColorChooser
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-chooser atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-color-chooser 'type)
 "@version{2021-1-23}
  @begin{short}
    The @sym{gtk-color-chooser} interface is an interface that is implemented
    by widgets for choosing colors.
  @end{short}
  Depending on the situation, colors may be allowed to have alpha
  (translucency).

  The main widgets that implement this interface are @class{gtk-color-button},
  @class{gtk-color-chooser-widget}, and @class{gtk-color-chooser-dialog}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"color-activated\" signal}
      @begin{pre}
 lambda (chooser color)    : Run First
      @end{pre}
      Emitted when a color is activated from the color chooser. This usually
      happens when the user clicks a color swatch, or a color is selected and
      the user presses one of the keys Space, Shift+Space, Return or Enter.
      @begin[code]{table}
        @entry[chooser]{The @sym{gtk-color-chooser} widget which received
          the signal.}
        @entry[color]{The @struct{gdk-rgba} color.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-color-chooser-rgba}
  @see-slot{gtk-color-chooser-use-alpha}
  @see-struct{gdk-rgba}
  @see-class{gtk-color-button}
  @see-class{gtk-color-chooser-dialog}
  @see-class{gtk-color-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-color-chooser-rgba -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "rgba" 'gtk-color-chooser) 't)
 "The @code{rgba} property of type @struct{gdk-rgba} (Read / Write) @br{}
  The @code{rgba} property contains the currently selected color, as a
  RGBA color. The property can be set to change the current selection
  programmatically.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-chooser-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-chooser-rgba 'function)
 "@version{*2021-1-24}
  @syntax[]{(gtk-color-chooser-rgba object) => color}
  @syntax[]{(setf (gtk-color-chooser-rgba object) color)}
  @argument[object]{a @class{gtk-color-chooser} widget}
  @argument[color]{a @struct{gdk-rgba} color}
  @begin{short}
    Accessor of the @slot[gtk-color-chooser]{rgba} slot of the
    @class{gtk-color-chooser} class.
  @end{short}

  The slot access function @sym{gtk-color-chooser-rgba} gets the currently
  selected color. The slot access function @sym{(setf gtk-color-chooser-rgba)}
  sets the color.

  The @code{rgba} property contains the currently selected color, as a
  @struct{gdk-rgba} color. The property can be set to change the current
  selection programmatically.
  @see-struct{gdk-rgba}
  @see-class{gtk-color-chooser}")

;;; --- gtk-color-chooser-use-alpha --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-alpha"
                                               'gtk-color-chooser) 't)
 "The @code{use-alpha} property of type @code{:boolean} (Read / Write) @br{}
  When @code{use-alpha} is @em{true}, colors may have alpha (translucency)
  information. When it is @em{false}, the RGBA color obtained via the
  @code{rgba} property will be forced to have the alpha value 1.0.
  Implementations are expected to show alpha by rendering the color over a
  non-uniform background (like a checkerboard pattern). @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-chooser-use-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-chooser-use-alpha 'function)
 "@version{2020-5-22}
  @syntax[]{(gtk-color-chooser-use-alpha object) => use-alpha}
  @syntax[]{(setf (gtk-color-chooser-use-alpha object) use-alpha)}
  @argument[object]{a @class{gtk-color-chooser} widget}
  @argument[use-alpha]{@em{true} if the color chooser should use alpha channel,
    @em{false} if not}
  @begin{short}
    Accessor of the @slot[gtk-color-chooser]{use-alpha} slot of the
    @class{gtk-color-chooser} class.
  @end{short}

  The slot access function @sym{gtk-color-chooser-use-alpha} returns whether
  the color chooser shows the alpha channel. The slot access function
  @sym{(setf gtk-color-chooser-use-alpha)} sets whether or not the color
  chooser should use the alpha channel.
  @see-class{gtk-color-chooser}")

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
 "@version{*2021-1-24}
  @argument[chooser]{a @class{gtk-color-chooser} widget}
  @argument[orientation]{a value of the @symbol{gtk-orientation} enumeration}
  @argument[colors-per-line]{an integer with the number of colors to show in
    each row/column}
  @argument[colors]{a list with the @struct{gdk-rgba} colors of the palette,
    or @code{nil}}
  @begin{short}
    Adds a palette to the color chooser.
  @end{short}
  If the orientation is @code{:horizontal}, the colors are grouped in rows,
  with @arg{colors-per-line} colors in each row. If the orientation is
  @code{:vertical}, the colors are grouped in columns instead.

  The default color palette of the color chooser widget has 27 colors,
  organized in columns of 3 colors. The default gray palette has 9 grays in a
  single row. The layout of the color chooser widget works best when the
  palettes have 9-10 columns.

  Calling this function for the first time has the side effect of removing the
  default color and gray palettes from the color chooser. If @arg{colors} is
  @code{nil}, removes all previously added palettes.
  @see-struct{gdk-rgba}
  @see-class{gtk-color-chooser}
  @see-symbol{gtk-orientation}"
  (if colors
      (with-foreign-boxed-array (n-colors colors-ptr gdk-rgba colors)
        (%gtk-color-chooser-add-palette chooser
                                        orientation
                                        colors-per-line
                                        n-colors
                                        colors-ptr))
      (%gtk-color-chooser-add-palette chooser
                                      orientation
                                      colors-per-line
                                      0
                                      (null-pointer))))

(export 'gtk-color-chooser-add-palette)

;;; --- End of file gtk.color-chooser.lisp -------------------------------------
