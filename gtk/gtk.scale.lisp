;;; ----------------------------------------------------------------------------
;;; gtk.scale.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
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
;;; GtkScale
;;;
;;;     A slider widget for selecting a value from a range
;;;
;;; Types and Values
;;;
;;;     GtkScale
;;;
;;; Functions
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
;;;
;;; Properties
;;;
;;;                gint    digits           Read / Write
;;;            gboolean    draw-value       Read / Write
;;;            gboolean    has-origin       Read / Write
;;;     GtkPositionType    value-pos        Read / Write
;;;
;;; Style Properties
;;;
;;;                gint    slider-length    Read
;;;                gint    value-spacing    Read
;;;
;;; Signals
;;;
;;;               gchar*   format-value     Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkRange
;;;                 ╰── GtkScale
;;;                     ├── GtkHScale
;;;                     ╰── GtkVScale
;;;
;;; Implemented Interfaces
;;;
;;;     GtkScale implements AtkImplementorIface, GtkBuildable and GtkOrientable.
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-scale 'type)
 "@version{*2021-4-16}
  @begin{short}
    A @sym{gtk-scale} widget is a slider control used to select a numeric value.
  @end{short}
  To use it, you will probably want to investigate the methods on its base
  @class{gtk-range} class, in addition to the methods for the @sym{gtk-scale}
  class itself. To set the value of a scale, you would normally use the function
  @fun{gtk-range-value}. To detect changes to the value, you would normally use
  the \"value-changed\" signal.

  @image[scales]{}

  Note that using the same upper and lower bounds for the @sym{gtk-scale}
  widget, through the @class{gtk-range} methods, will hide the slider itself.
  This is useful for applications that want to show an undeterminate value on
  the scale, without changing the layout of the application, such as movie or
  music players.
  @begin[GtkScale as GtkBuildable]{dictionary}
    The @sym{gtk-scale} widget supports a custom @code{<marks>} element, which
    can contain multiple @code{<mark>} elements. The \"value\" and \"position\"
    attributes have the same meaning as the parameters of the function
    @fun{gtk-scale-add-mark} of the same name. If the element is not empty, its
    content is taken as the markup to show at the mark. It can be translated
    with the usual \"translatable\" and \"context\" attributes.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 scale[.fine-tune][.marks-before][.marks-after]
 ├── marks.top
 │   ├── mark
 │   ┊    ├── [label]
 │   ┊    ╰── indicator
 ┊   ┊
 │   ╰── mark
 ├── [value]
 ├── contents
 │   ╰── trough
 │       ├── slider
 │       ├── [highlight]
 │       ╰── [fill]
 ╰── marks.bottom
     ├── mark
     ┊    ├── indicator
     ┊    ╰── [label]
     ╰── mark
    @end{pre}
    The @sym{gtk-scale} widget has a main CSS node with name @code{scale} and
    a subnode for its contents, with subnodes named @code{trough} and
    @code{slider}.

    The main node gets the style class @code{.fine-tune} added when the scale
    is in 'fine-tuning' mode.

    If the scale has an origin, see the function @fun{gtk-scale-has-origin},
    there is a subnode with name @code{highlight} below the trough node that
    is used for rendering the highlighted part of the trough.

    If the scale is showing a fill level, see the function
    @fun{gtk-range-show-fill-level}, there is a subnode with name @code{fill}
    below the trough node that is used for rendering the filled in part of the
    trough.

    If marks are present, there is a marks subnode before or after the contents
    node, below which each mark gets a node with name @code{mark}. The marks
    nodes get either the @code{.top} or @code{.bottom} style class.

    The mark node has a subnode named @code{indicator}. If the mark has text,
    it also has a subnode named @code{label}. When the mark is either above or
    left of the scale, the label subnode is the first when present. Otherwise,
    the indicator subnode is the first.

    The main CSS node gets the @code{marks-before} and/or @code{marks-after}
    style classes added depending on what marks are present.

    If the scale is displaying the value, see the @code{draw-value} property,
    there is subnode with name @code{value}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[slider-length]{entry}
        The @code{slider-length} style property of type @code{:int} (Read) @br{}
        Length of scale's slider. @br{}
        @em{Warning:} The @code{slider-length} style property has been
        deprecated since version 3.20 and should not be used in newly-written
        code. Use min-height/min-width CSS properties on the slider element
        instead. The value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 31
      @end{entry}
      @begin[value-spacing]{entry}
        The @code{value-spacing} style property of type @code{:int} (Read) @br{}
        Space between value text and the slider/trough area. @br{}
        @em{Warning:} The @code{value-spacing} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use min-height/min-width CSS properties on the value element
        instead. The value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"format-value\" signal}
      @begin{pre}
 lambda (scale value)    :run-last
      @end{pre}
      Signal which allows you to change how the scale value is displayed.
      Connect a signal handler which returns an allocated string representing
      @arg{value}. That string will then be used to display the scale's value.
      If no user provided handlers are installed, the value will be displayed
      on its own, rounded according to the value of the @code{digits} property.
      Here is an example signal handler which displays a value 1.0 as with
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
        @entry[scale]{The @sym{gtk-scale} widget which received the signal.}
        @entry[value]{A double float with the value to format.}
        @entry[Returns]{String representing the value.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-scale-digits}
  @see-slot{gtk-scale-draw-value}
  @see-slot{gtk-scale-has-origin}
  @see-slot{gtk-scale-value-pos}
  @see-class{gtk-range}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-scale-digits -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "digits" 'gtk-scale) 't)
 "The @code{digits} property of type @code{:int} (Read / Write) @br{}
  The number of decimal places that are displayed in the value. @br{}
  Allowed values: [-1, 64] @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-digits atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-digits 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-scale-digits object) => digits}
  @syntax[]{(setf (gtk-scale-digits object) digits)}
  @argument[object]{a @class{gtk-scale} widget}
  @argument[digits]{an integer with the number of decimal places to display,
    e.g. use 1 to display 1.0, 2 to display 1.00, etc}
  @begin{short}
    Accessor of the @slot[gtk-scale]{digits} slot of the @class{gtk-scale}
    class.
  @end{short}

  The slot access function @sym{gtk-scale-digits} returns the number of decimal
  places that are displayed. The slot access function
  @sym{(setf gtk-scale-digits)} sets the number of decimal places that are
  displayed in the value. Also causes the value of the adjustment to be rounded
  off to this number of @arg{digits}, so the retrieved value matches the value
  the user saw.
  @see-class{gtk-scale}")

;;; --- gtk-scale-draw-value ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "draw-value" 'gtk-scale) 't)
 "The @code{draw-value} property of type @code{:boolean} (Read / Write) @br{}
  Whether the current value is displayed as a string next to the slider. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-draw-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-draw-value 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-scale-draw-value object) => draw-value}
  @syntax[]{(setf (gtk-scale-digits object) draw-value)}
  @argument[object]{a @class{gtk-scale} widget}
  @argument[draw-value]{@em{true} to draw the value}
  @begin{short}
    Accessor of the @slot[gtk-scale]{draw-value} slot of the @class{gtk-scale}
    class.
  @end{short}

  The slot access function @sym{gtk-scale-draw-value} returns whether the
  current value is displayed as a string next to the slider. The slot access
  function @sym{(setf gtk-scale-draw-value)} specifies whether the current value
  is displayed as a string next to the slider.
  @see-class{gtk-scale}")

;;; --- gtk-scale-has-origin ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-origin" 'gtk-scale) 't)
 "The @code{has-origin} property of type @code{:boolean} (Read / Write) @br{}
  Whether the scale has an origin. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-has-origin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-has-origin 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-scale-has-origin object) => has-origin}
  @syntax[]{(setf (gtk-scale-digits object) has-origin)}
  @argument[object]{a @class{gtk-scale} widget}
  @argument[has-origin]{@em{true} if the scale has an origin}
  @begin{short}
    Accessor of the @slot[gtk-scale]{has-origin} slot of the @class{gtk-scale}
    class.
  @end{short}

  The slot access function @sym{gtk-scale-has-origin} returns whether the scale
  has an origin.

  If @arg{has-origin} is set to @em{true}, the default, the scale will
  highlight the part of the scale between the origin, bottom or left
  side, of the scale and the current value.
  @see-class{gtk-scale}")

;;; --- gtk-scale-value-pos ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value-pos" 'gtk-scale) 't)
 "The @code{value-pos} property of type @symbol{gtk-position-type}
  (Read / Write) @br{}
  The position in which the current value is displayed. @br{}
  Default value: @code{:top}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scale-value-pos atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scale-value-pos 'function)
 "@version{2020-5-31}
  @syntax[]{(gtk-scale-value-pos object) => pos}
  @syntax[]{(setf (gtk-scale-digits object) pos)}
  @argument[object]{a @class{gtk-scale} widget}
  @argument[pos]{a @symbol{gtk-position-type} with the position in which the
    current value is displayed}
  @begin{short}
    Accessor of the @slot[gtk-scale]{value-pos} slot of the @class{gtk-scale}
    class.
  @end{short}

  The slot access function @sym{gtk-scale-value-pos} gets the position in which
  the current value is displayed. The slot access function
  @sym{gtk-scale-value-pos} sets the position in which the current value is
  displayed.
  @see-class{gtk-scale}")

;;; ----------------------------------------------------------------------------
;;; gtk_scale_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scale-new))

(defun gtk-scale-new (orientation adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[orientation]{a value of the @symbol{gtk-orientation} enumeration}
  @argument[adjustment]{the @class{gtk-adjustment} object which sets the range
    of the scale, or @code{nil} to create a new adjustment}
  @return{A new @class{gtk-scale} widget.}
  @short{Creates a new scale widget.}
  @see-class{gtk-scale}"
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
 "@version{2020-5-31}
  @argument[orientation]{a value of the @symbol{gtk-orientation} enumeration}
  @argument[min]{a @code{:double} with the minimum value}
  @argument[max]{a @code{:double} with the maximum value}
  @argument[step]{a @code{:double} with the step increment, tick size, used
    with keyboard shortcuts}
  @return{A new @class{gtk-scale} widget.}
  @begin{short}
    Creates a new scale widget with the given @arg{orientation} that lets the
    user input a number between @arg{min} and @arg{max}, including @arg{min}
    and @arg{max}, with the increment @arg{step}.
  @end{short}
  @arg{step} must be nonzero. It is the distance the slider moves when using
  the arrow keys to adjust the scale value.

  Note that the way in which the precision is derived works best if @arg{step}
  is a power of ten. If the resulting precision is not suitable for your needs,
  use the function @fun{gtk-scale-digits} to correct it.
  @see-class{gtk-scale}
  @see-function{gtk-scale-digits}"
  (make-instance 'gtk-scale
                 :orientation orientation
                 :adjustment (make-instance 'gtk-adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

(export 'gtk-scale-new-with-range)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_layout () -> gtk-scale-layout
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_get_layout" gtk-scale-layout)
    (g-object pango-layout :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[scale]{a @class{gtk-scale} widget}
  @begin{return}
    The @class{pango-layout} for this scale, or @code{nil} if the
    @slot[gtk-scale]{draw-value} property is @code{nil}.
  @end{return}
  @begin{short}
    Gets the @class{pango-layout} used to display the scale.
  @end{short}
  @see-class{gtk-scale}"
  (scale (g-object gtk-scale)))

(export 'gtk-scale-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_layout_offsets () -> gtk-scale-layout-offsets
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_get_layout_offsets" %gtk-scale-get-layout-offsets) :void
  (scale (g-object gtk-scale))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-scale-layout-offsets (scale)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[scale]{a @class{gtk-scale} widget}
  @begin{return}
    @code{x} -- an integer with the x offset of layout, or @code{nil} @br{}
    @code{y} -- an integer with the y offset of layout, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the coordinates where the scale will draw the @class{pango-layout}
    representing the text in the scale.
  @end{short}
  Remember when using the @class{pango-layout} functions you need to convert
  to and from pixels using the function @fun{pango-pixels} or the constant
  @var{+pango-scale+}. If the @slot[gtk-scale]{draw-value} property is
  @code{nil}, the return values are undefined.
  @see-class{gtk-scale}
  @see-function{pango-pixels}
  @see-variable{+pango-scale+}"
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-scale-get-layout-offsets scale x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'gtk-scale-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_add_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_add_mark" gtk-scale-add-mark) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-31}
  @argument[scale]{a @class{gtk-scale} widget}
  @argument[value]{a @code{:double} with the value at which the mark is placed,
    must be between the lower and upper limits of the scale's adjustment}
  @argument[position]{a value of the @symbol{gtk-position-type} enumeration}
  @argument[markup]{a string with the text to be shown at the mark, using Pango
    markup, or @code{nil}}
  @short{Adds a mark at value.}

  A mark is indicated visually by drawing a tick mark next to the scale, and
  GTK+ makes it easy for the user to position the scale exactly at the marks
  value. For a horizontal scale, @code{:top} and @code{:left} are drawn above
  the scale, anything else below. For a vertical scale, @code{:left} and
  @code{:top} are drawn to the left of the scale, anything else to the right.

  If markup is not @code{nil}, text is shown next to the tick mark.

  To remove marks from a scale, use the function @fun{gtk-scale-clear-marks}.
  @see-class{gtk-scale}
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
 "@version{2020-5-31}
  @argument[scale]{a @class{gtk-scale} widget}
  @begin{short}
    Removes any marks that have been added with the function
    @fun{gtk-scale-add-mark}.
  @end{short}
  @see-class{gtk-scale}
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
   :export nil
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_hscale_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hscale 'type)
 "@version{2013-4-28}
  @begin{short}
    The @sym{gtk-hscale} widget is used to allow the user to select a value
    using a horizontal slider. To create one, use the function
    @fun{gtk-hscale-new-with-range}.
  @end{short}

  The position to show the current value, and the number of decimal places
  shown can be set using the parent @class{gtk-scale} class's functions.
  @begin[Warning]{dictionary}
    @sym{gtk-hscale} has been deprecated, use @class{gtk-scale} instead.
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gtk_hscale_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-hscale-new (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[adjustment]{the @class{gtk-adjustment} which sets the range of the
    scale}
  @return{A new @class{gtk-hscale} widget.}
  @short{Creates a new @class{gtk-hscale} widget.}
  @begin[Warning]{dictionary}
    @sym{gtk-hscale-new} has been deprecated since version 3.2 and should not be
    used in newly written code. Use the function @fun{gtk-scale-new} with
    @code{:horizontal} instead.
  @end{dictionary}
  @see-function{gtk-scale-new}"
  (make-instance 'gtk-scale
                 :orientation :horizontal
                 :adjustment adjustment))

;;; ----------------------------------------------------------------------------
;;; gtk_hscale_new_with_range ()
;;; ----------------------------------------------------------------------------

(defun gtk-hscale-new-with-range (min max step)
 #+cl-cffi-gtk-documentation
 "@version{2014-1-22}
  @argument[min]{minimum value}
  @argument[max]{maximum value}
  @argument[step]{step increment (tick size) used with keyboard shortcuts}
  @return{A new @class{gtk-hscale} widget.}
  @begin{short}
    Creates a new horizontal scale widget that lets the user input a number
    between @arg{min} and @arg{max} (including @arg{min} and @arg{max}) with the
    increment @arg{step}. @arg{step} must be nonzero; it is the distance the
    slider moves when using the arrow keys to adjust the scale value.
  @end{short}

  Note that the way in which the precision is derived works best if @arg{step}
  is a power of ten. If the resulting precision is not suitable for your needs,
  use  the function @fun{gtk-scale-digits} to correct it.
  @begin[Warning]{dictionary}
    @sym{gtk-hscale-new-with-range} has been deprecated since version 3.2 and
    should not be used in newly written code. Use the function
    @fun{gtk-scale-new-with-range} with @code{:horizontal} instead.
  @end{dictionary}
  @see-class{gtk-hscale}
  @see-function{gtk-scale-new-with-range}
  @see-function{gtk-scale-digits}"
  (make-instance 'gtk-scale
                 :orientation :horizontal
                 :adjustment (make-instance 'gtk-adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

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
  (register-object-type "GtkVScale" 'gtk-vscale)
  (setf *lisp-name-exceptions*
        (append '(("GtkVScale" GTK-VSCALE)) *lisp-name-exceptions*)))

(define-g-object-class "GtkVScale" gtk-vscale
  (:superclass gtk-scale
   :export nil
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_vscale_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-vscale 'type)
 "@version{2013-10-2}
  @begin{short}
    The @sym{gtk-vscale} widget is used to allow the user to select a value
    using a vertical slider. To create one, use the function
    @fun{gtk-hscale-new-with-range}.
  @end{short}

  The position to show the current value, and the number of decimal places
  shown can be set using the parent @class{gtk-scale} class's functions.
  @begin[Warning]{dictionary}
    @sym{gtk-vscale} has been deprecated, use @class{gtk-scale} instead.
  @end{dictionary}
  @see-class{gtk-scale}")

;;; ----------------------------------------------------------------------------
;;; gtk_vscale_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-vscale-new (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[adjustment]{the @class{gtk-adjustment} which sets the range of the
    scale}
  @return{A new @class{gtk-vscale} widget.}
  @short{Creates a new @class{gtk-vscale}.}
  @begin[Warning]{dictionary}
    @sym{gtk-vscale-new} has been deprecated since version 3.2 and should not be
    used in newly written code. Use the function @fun{gtk-scale-new} with
    @code{:vertival} instead.
  @end{dictionary}"
  (make-instance 'gtk-scale
                 :orientation :vertical
                 :adjustment adjustment))

;;; ----------------------------------------------------------------------------
;;; gtk_vscale_new_with_range ()
;;; ----------------------------------------------------------------------------

(defun gtk-vscale-new-with-range (min max step)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-28}
  @argument[min]{minimum value}
  @argument[max]{maximum value}
  @argument[step]{step increment (tick size) used with keyboard shortcuts}
  @return{A new @class{gtk-vscale}.}
  @begin{short}
    Creates a new vertical scale widget that lets the user input a number
    between min and max (including @arg{min} and @arg{max}) with the increment
    @arg{step}. The increment @arg{step} must be nonzero; it is the distance the
    slider moves when using the arrow keys to adjust the scale value.
  @end{short}

  Note that the way in which the precision is derived works best if @arg{step}
  is a power of ten. If the resulting precision is not suitable for your needs,
  use the function @fun{gtk-scale-digits} to correct it.
  @begin[Warning]{dictionary}
    @sym{gtk-vscale-new-with-range} has been deprecated since version 3.2 and
    should not be used in newly written code. Use the function
    @fun{gtk-scale-new-with-range} with @code{:vertical} instead.
  @end{dictionary}
  @see-function{gtk-scale-digits}"
  (make-instance 'gtk-scale
                 :orientation :vertical
                 :adjustment (make-instance 'gtk-adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

;;; --- End of file gtk.scale.lisp ---------------------------------------------
