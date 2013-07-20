;;; ----------------------------------------------------------------------------
;;; gtk.scale.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkScale
;;;
;;; A slider widget for selecting a value from a range
;;;
;;; Synopsis
;;;
;;;     GtkScale
;;;
;;;     gtk_scale_new
;;;     gtk_scale_new_with_range
;;;     gtk_scale_set_digits
;;;     gtk_scale_set_draw_value
;;;     gtk_scale_set_has_origin
;;;     gtk_scale_set_value_pos
;;;     gtk_scale_get_digits
;;;     gtk_scale_get_draw_value
;;;     gtk_scale_get_has_origin
;;;     gtk_scale_get_value_pos
;;;     gtk_scale_get_layout
;;;     gtk_scale_get_layout_offsets
;;;     gtk_scale_add_mark
;;;     gtk_scale_clear_marks
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkScale
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkScale" gtk-scale
  (:superclass gtk-range
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_scale_get_type")
  ((digits
    gtk-scale-digits
    "digits" "gint" t t)
   (draw-value
    gtk-scale-draw-value
    "draw-value" "gboolean" t t)
   (has-origin
    gtk-scale-has-origin
    "has-origin" "gboolean" t t)
   (value-pos
    gtk-scale-value-pos
    "value-pos" "GtkPositionType" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-scale 'type)
 "@version{2013-4-28}
  @begin{short}
    A @sym{gtk-scale} is a slider control used to select a numeric value. To use
    it, you will probably want to investigate the methods on its base class,
    @class{gtk-range}, in addition to the methods for @sym{gtk-scale} itself. To
    set the value of a scale, you would normally use the function
    @fun{gtk-range-set-value}. To detect changes to the value, you would
    normally use the \"value-changed\" signal.
  @end{short}

  Note that using the same upper and lower bounds for the @sym{gtk-scale}
  (through the @class{gtk-range} methods) will hide the slider itself. This is
  useful for applications that want to show an undeterminate value on the scale,
  without changing the layout of the application (such as movie or music
  players).

  @subheading{GtkScale as GtkBuildable}
    @sym{gtk-scale} supports a custom @code{<marks>} element, which can contain
    multiple @code{<mark>} elements. The \"value\" and \"position\" attributes
    have the same meaning as @fun{gtk-scale-add-mark} parameters of the same
    name. If the element is not empty, its content is taken as the markup to
    show at the mark. It can be translated with the usual \"translatable\" and
    \"context\" attributes.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"slider-length\" style property}
      @code{\"slider-length\"} of type @code{:int} (Read)@br{}
      Length of scale's slider. @br{}
      Allowed values: >= 0@br{}
      Default value: 31

    @subheading{The \"value-spacing\" style property}
      @code{\"value-spacing\"} of type @code{:int} (Read)@br{}
      Space between value text and the slider/trough area. @br{}
      Allowed values: >= 0@br{}
      Default value: 2
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"format-value\" signal}
      @begin{pre}
 lambda (scale value)   : Run Last
      @end{pre}
      Signal which allows you to change how the scale value is displayed.
      Connect a signal handler which returns an allocated string representing
      value. That string will then be used to display the scale's value.
      Here's an example signal handler which displays a value 1.0 as with
      \"-->1.0<--\".
      @begin{pre}
 static gchar*
 format_value_callback (GtkScale *scale,
                        gdouble   value)
 {
   return g_strdup_printf (\"-->%0.*g<--\",
                           gtk_scale_get_digits (scale), value);
 @}
      @end{pre}
      @begin[code]{table}
        @entry[scale]{The object which received the signal.}
        @entry[value]{The value to format.}
        @entry[Returns]{Allocated string representing value.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-scale-digits}
  @see-slot{gtk-scale-draw-value}
  @see-slot{gtk-scale-has-origin}
  @see-slot{gtk-scale-value-pos}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "digits" 'gtk-scale) 't)
 "The @code{\"digits\"} property of type @code{:int} (Read / Write)@br{}
  The number of decimal places that are displayed in the value. @br{}
  Allowed values: [@code{G_MAXULONG}, 64]@br{}
  Default value: 1")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "draw-value" 'gtk-scale) 't)
 "The @code{\"draw-value\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether the current value is displayed as a string next to the slider. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-origin" 'gtk-scale) 't)
 "The @code{\"has-origin\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether the scale has an origin. @br{}
  Default value: @em{true}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value-pos" 'gtk-scale) 't)
 "The @code{\"value-pos\"} property of type @symbol{gtk-position-type}
  (Read / Write)@br{}
  The position in which the current value is displayed. @br{}
  Default value: @code{:top}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-digits atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-digits 'function)
 "@version{2013-3-21}
  Accessor of the slot @code{\"digits\"} of the @class{gtk-scale} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-draw-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-draw-value 'function)
 "@version{2013-3-21}
  Accessor of the slot @code{\"draw-value\"} of the @class{gtk-scale} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-has-origin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-has-origin 'function)
 "@version{2013-3-21}
  Accessor of the slot @code{\"has-origin\"} of the @class{gtk-scale} class.")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-value-pos atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-value-pos 'function)
 "@version{2013-3-21}
  Accessor of the slot @code{\"value-pos\"} of the @class{gtk-scale} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_scale_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-new))

(defun gtk-scale-new (orientation adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[orientation]{the scale's orientation.}
  @argument[adjustment]{the @class{gtk-adjustment} object which sets the range
    of the scale, or @code{nil} to create a new adjustment}
  @return{A new @class{gtk-scale} widget.}
  @short{Creates a new @class{gtk-scale} widget.}

  Since 3.0"
  (make-instance 'gtk-scale
                 :orientation orientation
                 :adjustment adjustment))

(export 'gtk-scale-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_new_with_range ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-new-with-range))

(defun gtk-scale-new-with-range (orientation min max step)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[orientation]{the scale's orientation}
  @argument[min]{minimum value}
  @argument[max]{maximum value}
  @argument[step]{step increment (tick size) used with keyboard shortcuts}
  @return{A new @class{gtk-scale} widget.}
  @begin{short}
    Creates a new scale widget with the given @arg{orientation} that lets the
    user input a number between @arg{min} and @arg{max} (including @arg{min}
    and @arg{max}) with the increment @arg{step}. @arg{step} must be nonzero;
    it is the distance the slider moves when using the arrow keys to adjust the
    scale value.
  @end{short}

  Note that the way in which the precision is derived works best if @arg{step}
  is a power of ten. If the resulting precision is not suitable for your needs,
  use the function @fun{gtk-scale-set-digits} to correct it.

  Since 3.0
  @see-function{gtk-scale-set-digits}"
  (make-instance 'gtk-scale
                 :orientation orientation
                 :adjustment (make-instance 'gtk-adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

(export 'gtk-scale-new-with-range)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_set_digits ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-set-digits))

(defun gtk-scale-set-digits (scale digits)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @argument[digits]{the number of decimal places to display, e. g. use 1 to
    display 1.0, 2 to display 1.00, etc}
  Sets the number of decimal places that are displayed in the value. Also
  causes the value of the adjustment to be rounded off to this number of
  @arg{digits}, so the retrieved value matches the value the user saw."
  (setf (gtk-scale-digits scale) digits))

(export 'gtk-scale-set-digits)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_set_draw_value ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-set-draw-value))

(defun gtk-scale-set-draw-value (scale draw-value)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @argument[draw-value]{@em{true} to draw the value}
  Specifies whether the current value is displayed as a string next to the
  slider."
  (setf (gtk-scale-draw-value scale) draw-value))

(export 'gtk-scale-set-draw-value)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_set_has_origin ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-set-has-origin))

(defun gtk-scale-set-has-origin (scale has-origin)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @argument[has-origin]{@em{true} if the scale has an origin}
  @begin{short}
    If @arg{has-origin} is set to @em{true} (the default), the scale will
    highlight the part of the @arg{scale} between the origin (bottom or left
    side) of the scale and the current value.
  @end{short}

  Since 3.4"
  (setf (gtk-scale-has-origin scale) has-origin))

(export 'gtk-scale-set-has-origin)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_set_value_pos ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-set-value-pos))

(defun gtk-scale-set-value-pos (scale pos)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @argument[pos]{the position in which the current value is displayed}
  Sets the position in which the current value is displayed."
  (setf (gtk-scale-value-pos scale) pos))

(export 'gtk-scale-set-value-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_digits ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-get-digits))

(defun gtk-scale-get-digits (scale)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @return{The number of decimal places that are displayed.}
  Gets the number of decimal places that are displayed in the value."
  (gtk-scale-digits scale))

(export 'gtk-scale-get-digits)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_draw_value ()
;;; ----------------------------------------------------------------------------

(declaim (inline gkt-scale-get-draw-value))

(defun gtk-scale-get-draw-value (scale)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @return{Whether the current value is displayed as a string.}
  Returns whether the current value is displayed as a string next to the
  slider."
  (gtk-scale-draw-value scale))

(export 'gtk-scale-get-draw-value)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_has_origin ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-get-has-origin))

(defun gtk-scale-get-has-origin (scale)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @return{@em{True} if the scale has an origin.}
  @short{Returns whether the scale has an origin.}

  Since 3.4"
  (gtk-scale-has-origin scale))

(export 'gtk-scale-get-has-origin)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_value_pos ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-get-value-pos))

(defun gtk-scale-get-value-pos (scale)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @return{The position in which the current value is displayed.}
  Gets the position in which the current value is displayed."
  (gtk-scale-value-pos scale))

(export 'gtk-scale-get-value-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_layout ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_get_layout" gtk-scale-get-layout) (g-object pango-layout)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @begin{return}
    The @class{pango-layout} for this scale, or @code{nil} if the
    @code{\"draw-value\"} property is @code{nil}.
  @end{return}
  @begin{short}
    Gets the @class{pango-layout} used to display the scale. The returned object
    is owned by the scale so does not need to be freed by the caller.
  @end{short}

  Since 2.4"
  (scale (g-object gtk-scale)))

(export 'gtk-scale-get-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_layout_offsets ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_get_layout_offsets" %gtk-scale-get-layout-offsets) :void
  (scale (g-object gtk-scale))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-scale-get-layout-offsets (scale)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @begin{return}
    @code{x} -- X offset of layout, or @code{nil} @br{}
    @code{y} -- Y offset of layout, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the coordinates where the scale will draw the @class{pango-layout}
    representing the text in the scale. Remember when using the
    @class{pango-layout} function you need to convert to and from pixels using
    @code{PANGO_PIXELS()} or @code{PANGO_SCALE}.
  @end{short}

  If the @code{\"draw-value\"} property is @code{nil}, the return values are
  undefined.

  Since 2.4"
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-scale-get-layout-offsets scale x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gtk-scale-get-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_add_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_add_mark" gtk-scale-add-mark) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @argument[value]{the value at which the mark is placed, must be between the
    lower and upper limits of the scales' adjustment}
  @argument[position]{where to draw the mark}
  @argument[markup]{Text to be shown at the mark, using Pango markup,
    or @code{nil}}
  @short{Adds a mark at value.}

  A mark is indicated visually by drawing a tick mark next to the scale, and
  GTK+ makes it easy for the user to position the scale exactly at the marks
  value. For a horizontal scale, @code{:top} and @code{:left} are drawn above
  the scale, anything else below. For a vertical scale, @code{:left} and
  @code{:top} are drawn to the left of the scale, anything else to the right.

  If markup is not @code{nil}, text is shown next to the tick mark.

  To remove marks from a scale, use the function @fun{gtk-scale-clear-marks}.

  Since 2.16
  @see-function{gtk-scale-clear-marks}"
  (scale (g-object gtk-scale))
  (value :double)
  (pos gtk-position-type)
  (markup :string))

(export 'gtk-scale-add-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_clear_marks ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_clear_marks" gtk-scale-clear-marks) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[scale]{a @class{gtk-scale} widget}
  @begin{short}
    Removes any marks that have been added with the function
    @fun{gtk-scale-add-mark}.
  @end{short}

  Since 2.16
  @see-function{gtk-scale-add-mark}"
  (scale (g-object gtk-scale)))

(export 'gtk-scale-clear-marks)

;;; ----------------------------------------------------------------------------
;;; GtkHScale
;;;
;;; A horizontal slider widget for selecting a value from a range
;;;
;;; Synopsis
;;;
;;;     GtkHScale
;;;
;;;     gtk_hscale_new
;;;     gtk_hscale_new_with_range
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHScale
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHScale" 'gtk-hscale)
  (setf *lisp-name-exceptions*
        (append '(("GtkHScale" GTK-HSCALE)) *lisp-name-exceptions*)))

(define-g-object-class "GtkHScale" gtk-hscale
  (:superclass gtk-scale
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_hscale_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hscale 'type)
 "@version{2013-4-28}
  @subheading{Warning}
    @sym{gtk-hscale} has been deprecated, use @class{gtk-scale} instead.

  @begin{short}
    The @sym{gtk-hscale} widget is used to allow the user to select a value
    using a horizontal slider. To create one, use the function
    @fun{gtk-hscale-new-with-range}.
  @end{short}

  The position to show the current value, and the number of decimal places
  shown can be set using the parent @class{gtk-scale} class's functions.")

;;; ----------------------------------------------------------------------------
;;; gtk_hscale_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hscale-new))

(defun gtk-hscale-new (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[adjustment]{the @class{gtk-adjustment} which sets the range of the
    scale}
  @return{A new @class{gtk-hscale} widget.}
  @subheading{Warning}
    @sym{gtk-hscale-new} has been deprecated since version 3.2 and should not be
    used in newly written code. Use the function @fun{gtk-scale-new} with
    @code{:horizontal} instead.

  @short{Creates a new @class{gtk-hscale} widget.}
  @see-function{gtk-scale-new}"
  (make-instance 'gtk-scale
                 :orientation :horizontal
                 :adjustment adjustment))

(export 'gtk-hscale-new)

;;; ----------------------------------------------------------------------------
;;; gtk_hscale_new_with_range ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hscale-new-with-range))

(defun gtk-hscale-new-with-range (min max step)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[min]{minimum value}
  @argument[max]{maximum value}
  @argument[step]{step increment (tick size) used with keyboard shortcuts}
  @return{A new @class{gthk-hscale} widget.}
  @subheading{Warning}
    @sym{gtk-hscale-new-with-range} has been deprecated since version 3.2 and
    should not be used in newly written code. Use the function
    @fun{gtk-scale-new-with-range} with @code{:horizontal} instead.

  @begin{short}
    Creates a new horizontal scale widget that lets the user input a number
    between @arg{min} and @arg{max} (including @arg{min} and @arg{max}) with the
    increment @arg{step}. @arg{step} must be nonzero; it is the distance the
    slider moves when using the arrow keys to adjust the scale value.
  @end{short}

  Note that the way in which the precision is derived works best if @arg{step}
  is a power of ten. If the resulting precision is not suitable for your needs,
  use  the function @fun{gtk-scale-set-digits} to correct it."
  (make-instance 'gtk-scale
                 :orientation :horizontal
                 :adjustment (make-instance 'gtk-adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

(export 'gtk-hscale-new-with-range)

;;; ----------------------------------------------------------------------------
;;; GtkVScale
;;;
;;; A vertical slider widget for selecting a value from a range
;;;
;;; Synopsis
;;;
;;;     GtkVScale
;;;
;;;     gtk_vscale_new
;;;     gtk_vscale_new_with_range
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVScale
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkVScale" 'gtk-hscale)
  (setf *lisp-name-exceptions*
        (append '(("GtkVScale" GTK-VSCALE)) *lisp-name-exceptions*)))

(define-g-object-class "GtkVScale" gtk-vscale
  (:superclass gtk-scale
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_vscale_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-vscale 'type)
 "@version{2013-4-28}
  @subheading{Warning}
    @sym{gtk-vscale} has been deprecated, use @class{gtk-vcale} instead.

  @begin{short}
    The @sym{gtk-vscale} widget is used to allow the user to select a value
    using a vertical slider. To create one, use the function
    @fun{gtk-hscale-new-with-range}.
  @end{short}

  The position to show the current value, and the number of decimal places
  shown can be set using the parent @class{gtk-scale} class's functions.")

;;; ----------------------------------------------------------------------------
;;; gtk_vscale_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vscale-new))

(defun gtk-vscale-new (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[adjustment]{the @class{gtk-adjustment} which sets the range of the
    scale}
  @return{A new @class{gtk-vscale} widget.}
  @subheading{Warning}
    @sym{gtk-vscale-new} has been deprecated since version 3.2 and should not be
    used in newly written code. Use the function @fun{gtk-scale-new} with
    @code{:vertival} instead.

  @short{Creates a new @class{gtk-vscale}.}"
  (make-instance 'gtk-scale
                 :orientation :vertical
                 :adjustment adjustment))

(export 'gtk-vscale-new)

;;; ----------------------------------------------------------------------------
;;; gtk_vscale_new_with_range ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vscale-new-with-range))

(defun gtk-vscale-new-with-range (min max step)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[min]{minimum value}
  @argument[max]{maximum value}
  @argument[step]{step increment (tick size) used with keyboard shortcuts}
  @return{A new @class{gtk-vscale}.}
  @subheading{Warning}
    @sym{gtk-vscale-new-with-range} has been deprecated since version 3.2 and
    should not be used in newly written code. Use the function
    @fun{gtk-scale-new-with-range} with @code{:vertical} instead.

  @begin{short}
    Creates a new vertical scale widget that lets the user input a number
    between min and max (including @arg{min} and @arg{max}) with the increment
    @arg{step}. The increment @arg{step} must be nonzero; it is the distance the
    slider moves when using the arrow keys to adjust the scale value.
  @end{short}

  Note that the way in which the precision is derived works best if @arg{step}
  is a power of ten. If the resulting precision is not suitable for your needs,
  use the function @fun{gtk-scale-set-digits} to correct it.
  @see-function{gtk-scale-set-digits}"
  (make-instance 'gtk-scale
                 :orientation :vertical
                 :adjustment (make-instance 'gtk-adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

(export 'gtk-vscale-new-with-range)

;;; --- End of file gtk.scale.lisp ---------------------------------------------
