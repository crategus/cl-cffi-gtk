;;; ----------------------------------------------------------------------------
;;; gtk.color-selection.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; A widget used to select a color
;;;
;;; Synopsis
;;;
;;;     GtkColorSelection
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorSelection
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkColorSelection" 'gtk-color-selection))

(define-g-object-class "GtkColorSelection" gtk-color-selection
  (:superclass gtk-vbox
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
 "@version{2013-6-3}
  @begin{short}
    The @sym{gtk-color-selection} is a widget that is used to select a color. It
    consists of a color wheel and number of sliders and entry boxes for color
    parameters such as hue, saturation, value, red, green, blue, and opacity. It
    is found on the standard color selection dialog box
    @class{gtk-color-selection-dialog}.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"color-changed\" signal}
      @begin{pre}
 lambda (colorselection)   : Run First
      @end{pre}
      This signal is emitted when the color changes in the
      @sym{gtk-color-selection} according to its update policy.
      @begin[code]{table}
        @entry[colorselection]{The object which received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-color-selection-current-alpha}
  @see-slot{gtk-color-selection-current-color}
  @see-slot{gtk-color-selection-current-rgba}
  @see-slot{gtk-color-selection-has-opacity-control}
  @see-slot{gtk-color-selection-has-palette}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-alpha"
                                               'gtk-color-selection) 't)
 "The @code{\"current-alpha\"} property of type @code{:uint}
  (Read / Write) @br{}
  The current opacity value (0 fully transparent, 65535 fully opaque). @br{}
  Allowed values: <= 65535 @br{}
  Default value: 65535")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-color"
                                               'gtk-color-selection) 't)
 "The @code{\"current-color\"} property of type @class{gdk-color}
  (Read / Write) @br{}
  The current color.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-rgba"
                                               'gtk-color-selection) 't)
 "The @code{\"current-rgba\"} property of type @class{gdk-rgba}
  (Read / Write) @br{}
  The current RGBA color. @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-opacity-control"
                                               'gtk-color-selection) 't)
 "The @code{\"has-opacity-control\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the color selector should allow setting opacity. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-palette"
                                               'gtk-color-selection) 't)
 "The @code{\"has-palette\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether a palette should be used. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-current-alpha atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-current-alpha 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"current-alpha\"} of the
    @class{gtk-color-selection} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-current-color atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-current-color 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"current-color\"} of the
    @class{gtk-color-selection} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-current-rgba atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-current-rgba 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"current-rgba\"} of the
    @class{gtk-color-selection} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-has-opacity-control atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-has-opacity-control 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"has-opacity-control\"} of the
    @class{gtk-color-selection} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-has-palette atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-has-palette 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the slot @code{\"has-palette\"} of the
    @class{gtk-color-selection} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

(define-child-property "GtkColorSelection"
                       gtk-color-selection-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkColorSelection"
                       gtk-color-selection-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkColorSelection"
                       gtk-color-selection-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkColorSelection"
                       gtk-color-selection-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkColorSelection"
                       gtk-color-selection-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-child-expand 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the child property @code{\"expand\"} of the
    @class{gtk-color-selection} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-child-fill 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the child property @code{\"fill\"} of the
    @class{gtk-color-selection} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-child-padding 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the child property @code{\"padding\"} of the
    @class{gtk-color-selection} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-child-pack-type 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the child property @code{\"pack-type\"} of the
    @class{gtk-color-selection} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-selection-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-selection-child-position 'function)
 "@version{2013-2-24}
  @begin{short}
    Accessor of the child property @code{\"position\"} of the
    @class{gtk-color-selection} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @return{A new @class{gtk-color-selection} widget}
  @short{Creates a new @class{gtk-color-selection} object.}"
  (make-instance 'gtk-color-selection))

(export 'gtk-color-selection-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_has_opacity_control ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-set-has-opacity-control (colorsel has-opacity)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @argument[has-opacity]{@em{true} if colorsel can set the opacity,
    @code{nil} otherwise}
  @short{Sets the colorsel to use or not use opacity.}"
  (setf (gtk-color-selection-has-opacity-control colorsel) has-opacity))

(export 'gtk-color-selection-set-has-opacity-control)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_has_opacity_control ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-get-has-opacity-control (colorsel)
 #+cl-cffi-gtk-documentation
 "@version{2014-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @return{@em{True} if the colorsel has an opacity control,
    @code{nil} if it does not.}
  @begin{short}
    Determines whether the colorsel has an opacity control.
  @end{short}"
  (gtk-color-selection-has-opacity-control colorsel))

(export 'gtk-color-selection-get-has-opacity-control)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_has_palette ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-set-has-palette (colorsel has-palette)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @argument[has-palette]{@em{true} if palette is to be visible,
    @code{nil} otherwise}
  @begin{short}
    Shows and hides the palette based upon the value of @arg{has-palette}.
  @end{short}"
  (setf (gtk-color-selection-has-palette colorsel) has-palette))

(export 'gtk-color-selection-set-has-palette)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_has_palette ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-get-has-palette (colorsel)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @return{@em{True} if the selector has a palette, @code{nil} if it has not.}
  @begin{short}
    Determines whether the color selector has a color palette.
  @end{short}"
  (gtk-color-selection-has-palette colorsel))

(export 'gtk-color-selection-get-has-palette)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_current_alpha ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-get-current-alpha (colorsel)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @return{An integer between 0 and 65535.}
  @short{Returns the current alpha value.}"
  (gtk-color-selection-current-alpha colorsel))

(export 'gtk-color-selection-get-current-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_current_alpha ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-set-current-alpha (colorsel alpha)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @argument[alpha]{an integer between 0 and 65535}
  @begin{short}
    Sets the current opacity to be @arg{alpha}.
  @end{short}

  The first time this is called, it will also set the original opacity to be
  alpha too."
  (setf (gtk-color-selection-current-alpha colorsel) alpha))

(export 'gtk-color-selection-set-current-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_current_color ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-get-current-color (colorsel)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @argument[color]{a @class{gdk-color} to fill in with the current color}
  @begin{short}
    Sets color to be the current color in the @class{gtk-color-selection}
    widget.
  @end{short}"
  (gtk-color-selection-current-color colorsel))

(export 'gtk-color-selection-get-current-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_current_color ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-selection-set-current-color (colorsel color)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @argument[color]{a @class{gdk-color} to set the current color with}
  @begin{short}
    Sets the current color to be @arg{color}.
  @end{short}

  The first time this is called, it will also set the original color to be
  color too."
  (setf (gtk-color-selection-current-color colorsel) color))

(export 'gtk-color-selection-set-current-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_alpha ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_get_previous_alpha"
           gtk-color-selection-get-previous-alpha) :uint16
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @return{An integer between 0 and 65535.}
  @short{Returns the previous alpha value.}"
  (colorsel (g-object gtk-color-selection)))

(export 'gtk-color-selection-get-previous-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_previous_alpha ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_set_previous_alpha"
           gtk-color-selection-set-previous-alpha) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @argument[alpha]{an integer between 0 and 65535}
  @begin{short}
    Sets the 'previous' alpha to be @arg{alpha}.
  @end{short}

  This function should be called with some hesitations, as it might seem
  confusing to have that alpha change."
  (colorsel (g-object gtk-color-selection))
  (alpha :uint16))

(export 'gtk-color-selection-set-previous-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_color ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_get_previous_color"
          %gtk-color-selection-get-previous-color) :void
  (color-selection (g-object gtk-color-selection))
  (color (g-boxed-foreign gdk-color)))

(defun gtk-color-selection-get-previous-color (color-selection)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @short{Fills color in with the original color value.}"
  (let ((color (make-gdk-color)))
    (%gtk-color-selection-get-previous-color color-selection color)
    color))

(export 'gtk-color-selection-get-previous-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_previous_color ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_set_previous_color"
          gtk-color-selection-set-previous-color) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @argument[color]{a @class{gdk-color} to set the previous color with}
  @begin{short}
    Sets the 'previous' color to be @arg{color}.
  @end{short}

  This function should be called with some hesitations, as it might seem
  confusing to have that color change. Calling the
  @fun{gtk-color-selection-set-current-color} function will also set this color
  the first time it is called."
  (color-selection (g-object gtk-color-selection))
  (color (g-boxed-foreign gdk-color)))

(export 'gtk-color-selection-set-previous-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_current_rgba ()
;;;
;;; void gtk_color_selection_get_current_rgba (GtkColorSelection *colorsel,
;;;                                            GdkRGBA *rgba);
;;;
;;; Sets rgba to be the current color in the GtkColorSelection widget.
;;;
;;; colorsel :
;;;     a GtkColorSelection
;;;
;;; rgba :
;;;     a GdkRGBA to fill in with the current color. [out]
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_current_rgba ()
;;;
;;; void gtk_color_selection_set_current_rgba (GtkColorSelection *colorsel,
;;;                                            const GdkRGBA *rgba);
;;;
;;; Sets the current color to be rgba.
;;;
;;; The first time this is called, it will also set the original color to be
;;; rgba too.
;;;
;;; colorsel :
;;;     a GtkColorSelection
;;;
;;; rgba :
;;;     A GdkRGBA to set the current color with
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_rgba ()
;;;
;;; void gtk_color_selection_get_previous_rgba (GtkColorSelection *colorsel,
;;;                                             GdkRGBA *rgba);
;;;
;;; Fills rgba in with the original color value.
;;;
;;; colorsel :
;;;     a GtkColorSelection
;;;
;;; rgba :
;;;     a GdkRGBA to fill in with the original color value
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_previous_rgba ()
;;;
;;; void gtk_color_selection_set_previous_rgba (GtkColorSelection *colorsel,
;;;                                             const GdkRGBA *rgba);
;;;
;;; Sets the 'previous' color to be rgba.
;;;
;;; This function should be called with some hesitations, as it might seem
;;; confusing to have that color change. Calling
;;; gtk_color_selection_set_current_rgba() will also set this color the first
;;; time it is called.
;;;
;;; colorsel :
;;;     a GtkColorSelection
;;;
;;; rgba :
;;;     a GdkRGBA to set the previous color with
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_is_adjusting ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_is_adjusting" gtk-color-selection-is-adjusting)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colorsel]{a @class{gtk-color-selection} widget}
  @begin{return}
    @em{True} if the user is currently dragging a color around, and @code{nil}
    if the selection has stopped.
  @end{return}
  @begin{short}
    Gets the current state of the @arg{colorsel}.
  @end{short}"
  (color-selection g-object))

(export 'gtk-color-selection-is-adjusting)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_palette_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_palette_from_string"
          %gtk-color-selection-palette-from-string) :boolean
  (str :string)
  (colors :pointer)
  (n-colors :pointer))

(defun gtk-color-selection-palette-from-string (str)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[str]{a string encoding a color palette}
  @return{@em{True} if a palette was successfully parsed.}
  @begin{short}
    Parses a color palette string; the string is a colon-separated list of color
    names readable by the @fun{gdk-color-parse} function.
  @end{short}"
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

(export 'gtk-color-selection-palette-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_palette_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_color_selection_palette_to_string"
          gtk-color-selection-palette-to-string)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-3}
  @argument[colors]{an array of colors}
  @argument[n-colors]{length of the array}
  @return{Allocated string encoding the palette.}
  @begin{short}
    Encodes a palette as a string, useful for persistent storage.
  @end{short}"
  (colors :pointer)
  (n-colors :int))

(export 'gtk-color-selection-palette-to-string)

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
