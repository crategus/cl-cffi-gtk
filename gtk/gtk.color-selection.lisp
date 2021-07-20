;;; ----------------------------------------------------------------------------
;;; gtk.color-selection.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; GtkColorSelection
;;;
;;;     Deprecated widget used to select a color
;;;
;;; Types and Values
;;;
;;;     GtkColorSelection
;;;
;;; Functions
;;;
;;;     gtk_color_selection_new
;;;     gtk_color_selection_set_has_opacity_control
;;;     gtk_color_selection_get_has_opacity_control
;;;     gtk_color_selection_set_has_palette
;;;     gtk_color_selection_get_has_palette
;;;     gtk_color_selection_get_current_alpha
;;;     gtk_color_selection_set_current_alpha
;;;     gtk_color_selection_get_current_color
;;;     gtk_color_selection_set_current_color
;;;     gtk_color_selection_get_previous_alpha
;;;     gtk_color_selection_set_previous_alpha
;;;     gtk_color_selection_get_previous_color
;;;     gtk_color_selection_set_previous_color
;;;     gtk_color_selection_get_current_rgba
;;;     gtk_color_selection_set_current_rgba
;;;     gtk_color_selection_get_previous_rgba
;;;     gtk_color_selection_set_previous_rgba
;;;     gtk_color_selection_is_adjusting
;;;     gtk_color_selection_palette_from_string
;;;     gtk_color_selection_palette_to_string
;;;     gtk_color_selection_set_change_palette_with_screen_hook
;;;
;;; Properties
;;;
;;;        guint    current-alpha          Read / Write
;;;     GdkColor*   current-color          Read / Write
;;;      GdkRGBA*   current-rgba           Read / Write
;;;     gboolean    has-opacity-control    Read / Write
;;;     gboolean    has-palette            Read / Write
;;;
;;; Signals
;;;
;;;         void    color-changed          Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkColorSelection
;;;
;;; Implemented Interfaces
;;;
;;;     GtkColorSelection implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorSelection
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkColorSelection" gtk-color-selection
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_color_selection_get_type")
  ((current-alpha
    gtk-color-selection-current-alpha
    "current-alpha" "guint" t t)
   (current-color
    gtk-color-selection-current-color
    "current-color" "GdkColor" t t)
   (current-rgba
    gtk-color-selection-current-rgba
    "current-rgba" "GdkRGBA" t t)
   (has-opacity-control
    gtk-color-selection-has-opacity-control
    "has-opacity-control" "gboolean" t t)
   (has-palette
    gtk-color-selection-has-palette
    "has-palette" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-color-selection 'type)
 "@version{2021-7-20}
  @begin{short}
    The @sym{gtk-color-selection} widget is used to select a color.
  @end{short}
  It consists of a color wheel and a number of sliders and entry boxes for
  color parameters such as hue, saturation, value, red, green, blue, and
  opacity. It is found on the @class{gtk-color-selection-dialog} widget.
  @begin[Warning]{dictionary}
    The @sym{gtk-color-selection} widget is deprecated since GTK 3.4 and should
    not be used in newly written code.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"color-changed\" signal}
      @begin{pre}
 lambda (selection)    :run-first
      @end{pre}
      The signal is emitted when the color changes in the color selector
      according to its update policy.
      @begin[code]{table}
        @entry[selection]{The @sym{gtk-color-selection} widget which received
        the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-color-selection-current-alpha}
  @see-slot{gtk-color-selection-current-color}
  @see-slot{gtk-color-selection-current-rgba}
  @see-slot{gtk-color-selection-has-opacity-control}
  @see-slot{gtk-color-selection-has-palette}
  @see-class{gtk-color-selection-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-color-selection-current-alpha --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-alpha"
                                               'gtk-color-selection) 't)
 "The @code{current-alpha} property of type @code{:uint} (Read / Write) @br{}
  The current opacity value, 0 fully transparent, 65535 fully opaque. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 65535")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-current-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-current-alpha 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-color-selection-current-alpha object) => alpha}
  @syntax[]{(setf (gtk-color-selection-current-alpha object) alpha)}
  @argument[object]{a @class{gtk-color-selection} widget}
  @argument[alpha]{an integer between 0 and 65535}
  @begin{short}
    Accessor of the @slot[gtk-color-selection]{current-alpha} slot of the
    @class{gtk-color-selection} class.
  @end{short}

  The slot access function @sym{gtk-color-selection-current-alpha} returns the
  current alpha value. The slot access function
  @sym{(setf gtk-color-selection-current-alpha)} sets the current opacity. The
  first time this is called, it will also set the original opacity to be
  @arg{alpha} too.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-current-alpha} is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}")

;;; --- gtk-color-selection-current-color --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-color"
                                               'gtk-color-selection) 't)
 "The @code{current-color} property of type @class{gdk-color} (Read / Write)
  @br{}
  The current color.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-current-color atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-current-color 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-color-selection-current-color object) => color}
  @syntax[]{(setf (gtk-color-selection-current-color object) color)}
  @argument[object]{a @class{gtk-color-selection} widget}
  @argument[color]{a @class{gdk-color} color}
  @begin{short}
    Accessor of the @slot[gtk-color-selection]{current-color} slot of the
    @class{gtk-color-selection} class.
  @end{short}

  The slot access function @sym{gtk-color-selection-current-alpha} gets the
  current color in the color selector. The slot access function
  @sym{(setf gtk-color-selection-current-alpha)} sets the current color. The
  first time this is called, it will also set the original color to be
  @arg{color} too.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-current-color} is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}
  @see-class{gdk-color}")

;;; --- gtk-color-selection-current-rgba ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-rgba"
                                               'gtk-color-selection) 't)
 "The @code{current-rgba} property of type @class{gdk-rgba} (Read / Write) @br{}
  The current RGBA color.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-current-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-current-rgba 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-color-selection-current-rgba object) => rgba}
  @syntax[]{(setf (gtk-color-selection-current-rgba object) rgba)}
  @argument[object]{a @class{gtk-color-selection} widget}
  @argument[rgba]{a @class{gdk-rgba} color to set the current color with}
  @begin{short}
    Accessor of the @slot[gtk-color-selection]{current-rgba} slot of the
    @class{gtk-color-selection} class.
  @end{short}

  The slot access function @sym{gtk-color-selection-current-rgba} gets the
  current color in the color selector. The slot access function
  @sym{(setf gtk-color-selection-current-rgba)} sets the current color. The
  first time this is called, it will also set the original color to be
  @arg{rgba} too.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-current-rgba} is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}
  @see-class{gdk-rgba}")

;;; --- gtk-color-selection-has-opacity-control --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-opacity-control"
                                               'gtk-color-selection) 't)
 "The @code{has-opacity-control} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the color selector should allow setting the opacity. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-has-opacity-control
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-has-opacity-control 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-color-selection-has-opacity-control object) => has-opacity}
  @syntax[]{(setf (gtk-color-selection-has-opacity-control object) has-opacity)}
  @argument[object]{a @class{gtk-color-selection} widget}
  @argument[has-opacity]{@em{true} if the color selector can set the opacity,
    @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gtk-color-selection]{has-opacity-control} slot of the
    @class{gtk-color-selection} class.
  @end{short}

  The slot access function @sym{gtk-color-selection-has-opacity-control}
  determines whether the color selector has an opacity control. The slot access
  function @sym{(setf gtk-color-selection-has-opacity-control)} sets the color
  selector to use or not use opacity.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-has-opacity-control} is deprecated
    since version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}")

;;; --- gtk-color-selection-has-palette ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-palette"
                                               'gtk-color-selection) 't)
 "The @code{has-palette} property of type @code{:boolean} (Read / Write) @br{}
  Whether a palette should be used. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-has-palette atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-has-palette 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-color-selection-has-palette object) => has-palette}
  @syntax[]{(setf (gtk-color-selection-has-palette object) has-palette)}
  @argument[object]{a @class{gtk-color-selection} widget}
  @argument[has-palette]{@em{true} if the color palette is to be visible,
    @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gtk-color-selection]{has-palette} slot of the
    @class{gtk-color-selection} class.
  @end{short}

  The slot access function @sym{gtk-color-selection-has-palette} determines
  whether the color selector has a color palette. The slot access function
  @sym{(setf gtk-color-selection-has-palette)} shows and hides the color
  palette.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-has-palette} is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @return{A new @class{gtk-color-selection} widget.}
  @short{Creates a new color selector.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-new} is deprecated since version 3.4
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}"
  (make-instance 'gtk-color-selection))

(export 'gtk-color-selection-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_alpha ()
;;; gtk_color_selection_set_previous_alpha ()
;;; -> gtk-color-selection-previous-alpha
;;; ----------------------------------------------------------------------------

(defun (setf gtk-color-selection-previous-alpha) (alpha selection)
  (foreign-funcall "gtk_color_selection_previous_alpha"
                   (g-object gtk-color-selection) selection
                   :uint16 alpha
                   :void)
  alpha)

(defcfun ("gtk_color_selection_get_previous_alpha"
           gtk-color-selection-previous-alpha) :uint16
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @syntax[]{(gtk-color-selection-previous-alpha selection) => alpha}
  @syntax[]{(setf (gtk-color-selection-previous-alpha selection) alpha)}
  @argument[selection]{a @class{gtk-color-selection} widget}
  @argument[alpha]{an integer between 0 and 65535}
  @begin{short}
    The function @sym{gtk-color-selection-previous-alpha} returns the previous
    alpha value.
  @end{short}
  The function @sym{(setf gtk-color-selection-previous-alpha)} sets the
  previous alpha value.

  This function should be called with some hesitations, as it might seem
  confusing to have that alpha change.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-previous-alpha} is deprecated
    since version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}"
  (selection (g-object gtk-color-selection)))

(export 'gtk-color-selection-previous-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_color ()
;;; gtk_color_selection_set_previous_color ()
;;; -> gtk-color-selection-previous-color
;;; ----------------------------------------------------------------------------

(defun (setf gtk-color-selection-previous-color) (color selection)
  (foreign-funcall "gtk_color_selection_set_previous_color"
                   (g-object gtk-color-selection) selection
                   (g-boxed-foreign gdk-color) color
                   :void)
  color)

(defcfun ("gtk_color_selection_get_previous_color"
          %gtk-color-selection-get-previous-color) :void
  (selection (g-object gtk-color-selection))
  (color (g-boxed-foreign gdk-color)))

(defun gtk-color-selection-previous-color (selection)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @syntax[]{(gtk-color-selection-previous-color selection) => color}
  @syntax[]{(setf (gtk-color-selection-previous-color selection) color)}
  @argument[selection]{a @class{gtk-color-selection} widget}
  @argument[color]{a @class{gdk-color} color}
  @begin{short}
    The function @sym{gtk-color-selection-previous-color} gets the previous
    color value.
  @end{short}
  The function @sym{(setf gtk-color-selection-previous-color)} sets the
  previous color.

  This function should be called with some hesitations, as it might seem
  confusing to have that color change. Calling the function
  @fun{gtk-color-selection-current-color} will also set this color the first
  time it is called.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-previous-color} is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}
  @see-class{gdk-color}
  @see-function{gtk-color-selection-current-rgba}"
  (let ((color (gdk-color-new)))
    (%gtk-color-selection-get-previous-color selection color)
    color))

(export 'gtk-color-selection-previous-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_rgba ()
;;; gtk_color_selection_set_previous_rgba ()
;;; -> gtk-color-selection-previous-rgba
;;; ----------------------------------------------------------------------------

(defun (setf gtk-color-selection-previous-rgba) (rgba selection)
  (foreign-funcall "gtk_color_selection_set_previous_rgba"
                   (g-object gkt-color-selection) selection
                   (g-boxed-foreign gdk-rgba) rgba
                   :void)
  rgba)

(defcfun ("gtk_color_selection_get_previous_rgba"
          %gtk-color-selection-get-previous-rgba) :void
  (selection (g-object gtk-color-selection))
  (rgba (g-boxed-foreign gdk-rgba)))

(defun gtk-color-selection-previous-rgba (selection)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @syntax[]{(gtk-color-selection-previous-rgba selection) => rgba}
  @syntax[]{(setf (gtk-color-selection-previous-rgba selection) rgba)}
  @argument[selection]{a @class{gtk-color-selection} widget}
  @argument[rgba]{a @class{gdk-rgba} color to set the previous color with}
  @begin{short}
    The function @sym{gtk-color-selection-previous-rgba} gets the previous
    color.
  @end{short}
  The function @sym{(setf gtk-color-selection-previous-rgba)} sets the previous
  color.

  This function should be called with some hesitations, as it might seem
  confusing to have that color change. Calling the function
  @fun{gtk-color-selection-current-rgba} will also set this color the first
  time it is called.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-previous-rgba} is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}
  @see-class{gdk-rgba}
  @see-function{gtk-color-selection-current-color}"
  (let ((rgba (make-gdk-rgba)))
    (%gtk-color-selection-get-previous-rgba selection rgba)
    rgba))

(export 'gtk-color-selection-previous-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_is_adjusting ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_is_adjusting" gtk-color-selection-is-adjusting)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[selection]{a @class{gtk-color-selection} widget}
  @begin{return}
    @em{True} if the user is currently dragging a color around, and @em{false}
    if the selection has stopped.
  @end{return}
  @begin{short}
    Gets the current state of the color selector.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-is-adjusting} is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}"
  (selection g-object))

(export 'gtk-color-selection-is-adjusting)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_palette_from_string ()             not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_palette_from_string"
          %gtk-color-selection-palette-from-string) :boolean
  (str :string)
  (colors :pointer)
  (n-colors :pointer))

(defun gtk-color-selection-palette-from-string (str)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[str]{a string encoding a color palette}
  @return{@em{True} if a color palette was successfully parsed.}
  @begin{short}
    Parses a color palette from a string.
  @end{short}
  The string is a colon-separated list of color names readable by the function
  @fun{gdk-color-parse}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-palette-from-string} is deprecated
    since version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}
  @see-function{gdk-color-parse}"
  (with-foreign-objects ((colors :pointer) (n-colors :int))
    (when (%gtk-color-selection-palette-from-string str colors n-colors)
      (iter (with colors-ar = (mem-ref colors :pointer))
            (for i from 0 below (mem-ref n-colors :int))
            (for color-ptr =
                 (inc-pointer colors-ar
                              (* i (foreign-type-size 'gdk-color-cstruct))))
            (for color = (convert-from-foreign color-ptr
                                               '(g-boxed-foreign gdk-color)))
            (collect color)
            (finally (g-free colors-ar))))))

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_palette_to_string ()               not exported
;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation, is this correct?

(defcfun ("gtk_color_selection_palette_to_string"
          gtk-color-selection-palette-to-string)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2020-1-21}
  @argument[colors]{an array of colors}
  @argument[n-colors]{length of the array}
  @return{Allocated string encoding the palette.}
  @begin{short}
    Encodes a palette as a string, useful for persistent storage.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gtk-color-selection-palette-to-string} is deprecated
    since version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-color-selection}"
  (colors :pointer)
  (n-colors :int))

;;; ----------------------------------------------------------------------------
;;; GtkColorSelectionChangePaletteFunc ()
;;;
;;; void (*GtkColorSelectionChangePaletteFunc) (const GdkColor *colors,
;;;                                             gint n_colors);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_change_palette_with_screen_hook ()
;;;
;;; GtkColorSelectionChangePaletteWithScreenFunc
;;; gtk_color_selection_set_change_palette_with_screen_hook
;;;                          (GtkColorSelectionChangePaletteWithScreenFunc func)
;;;
;;; Installs a global function to be called whenever the user tries to modify
;;; the palette in a color selection.
;;;
;;; This function should save the new palette contents, and update the
;;; "gtk-color-palette" GtkSettings property so all GtkColorSelection widgets
;;; will be modified.
;;;
;;; func :
;;;     a function to call when the custom palette needs saving
;;;
;;; Returns :
;;;     the previous change palette hook (that was replaced)
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkColorSelectionChangePaletteWithScreenFunc ()
;;;
;;; void (*GtkColorSelectionChangePaletteWithScreenFunc)
;;;                                                     (GdkScreen *screen,
;;;                                                      const GdkColor *colors,
;;;                                                      gint n_colors);
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.color-selection.lisp -----------------------------------
