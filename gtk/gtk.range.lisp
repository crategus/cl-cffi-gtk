;;; ----------------------------------------------------------------------------
;;; gtk.range.lisp
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
;;; GtkRange
;;;
;;;     Base class for widgets which visualize an adjustment
;;;
;;; Types and Values
;;;
;;;     GtkRange
;;;     GtkSensitivityType
;;;
;;; Functions
;;;
;;;     gtk_range_get_fill_level
;;;     gtk_range_get_restrict_to_fill_level
;;;     gtk_range_get_show_fill_level
;;;     gtk_range_set_fill_level
;;;     gtk_range_set_restrict_to_fill_level
;;;     gtk_range_set_show_fill_level
;;;     gtk_range_get_adjustment
;;;     gtk_range_set_adjustment
;;;     gtk_range_get_inverted
;;;     gtk_range_set_inverted
;;;     gtk_range_get_value
;;;     gtk_range_set_value
;;;     gtk_range_set_increments
;;;     gtk_range_set_range
;;;     gtk_range_get_round_digits
;;;     gtk_range_set_round_digits
;;;     gtk_range_set_lower_stepper_sensitivity
;;;     gtk_range_get_lower_stepper_sensitivity
;;;     gtk_range_set_upper_stepper_sensitivity
;;;     gtk_range_get_upper_stepper_sensitivity
;;;     gtk_range_get_flippable
;;;     gtk_range_set_flippable
;;;     gtk_range_get_min_slider_size
;;;     gtk_range_get_range_rect
;;;     gtk_range_get_slider_range
;;;     gtk_range_get_slider_size_fixed
;;;     gtk_range_set_min_slider_size
;;;     gtk_range_set_slider_size_fixed
;;;
;;; Properties
;;;
;;;      GtkAdjustment*   adjustment                   Read / Write / Construct
;;;            gdouble    fill-level                   Read / Write
;;;           gboolean    inverted                     Read / Write
;;; GtkSensitivityType    lower-stepper-sensitivity    Read / Write
;;;           gboolean    restrict-to-fill-level       Read / Write
;;;               gint    round-digits                 Read / Write
;;;           gboolean    show-fill-level              Read / Write
;;; GtkSensitivityType    upper-stepper-sensitivity    Read / Write
;;;
;;; Style Properties
;;;
;;;               gint    arrow-displacement-x         Read
;;;               gint    arrow-displacement-y         Read
;;;             gfloat    arrow-scaling                Read
;;;               gint    slider-width                 Read
;;;               gint    stepper-size                 Read
;;;               gint    stepper-spacing              Read
;;;               gint    trough-border                Read
;;;           gboolean    trough-under-steppers        Read
;;;
;;; Signals
;;;
;;;               void    adjust-bounds                Run Last
;;;           gboolean    change-value                 Run Last
;;;               void    move-slider                  Action
;;;               void    value-changed                Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkRange
;;;                 ├── GtkScale
;;;                 ╰── GtkScrollbar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRange implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkSensitivityType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkSensitivityType" gtk-sensitivity-type
  (:export t
   :type-initializer "gtk_sensitivity_type_get_type")
  (:auto 0)
  (:on 1)
  (:off 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-sensitivity-type atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-sensitivity-type atdoc:*external-symbols*)
 "@version{2021-12-26}
  @begin{short}
    Determines how GTK handles the sensitivity of stepper arrows at the end of
    range widgets.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkSensitivityType\" gtk-sensitivity-type
  (:export t
   :type-initializer \"gtk_sensitivity_type_get_type\")
  (:auto 0)
  (:on 1)
  (:off 2))
  @end{pre}
  @begin[code]{table}
    @entry[:auto]{The arrow is made insensitive if the thumb is at the end.}
    @entry[:on]{The arrow is always sensitive.}
    @entry[:off]{The arrow is always insensitive.}
  @end{table}
  @see-class{gtk-range}")

;;; ----------------------------------------------------------------------------
;;; struct GtkRange
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRange" gtk-range
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_range_get_type")
  ((adjustment
    gtk-range-adjustment
    "adjustment" "GtkAdjustment" t t)
   (fill-level
    gtk-range-fill-level
    "fill-level" "gdouble" t t)
   (inverted
    gtk-range-inverted
    "inverted" "gboolean" t t)
   (lower-stepper-sensitivity
    gtk-range-lower-stepper-sensitivity
    "lower-stepper-sensitivity" "GtkSensitivityType" t t)
   (restrict-to-fill-level
    gtk-range-restrict-to-fill-level
    "restrict-to-fill-level" "gboolean" t t)
   (round-digits
    gtk-range-round-digits
    "round-digits" "gint" t t)
   (show-fill-level
    gtk-range-show-fill-level
    "show-fill-level" "gboolean" t t)
   (upper-stepper-sensitivity
    gtk-range-upper-stepper-sensitivity
    "upper-stepper-sensitivity" "GtkSensitivityType" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-range 'type)
 "@version{2021-12-26}
  @begin{short}
    The @sym{gtk-range} class is the common base class for widgets which
    visualize an adjustment, e.g. the @class{gtk-scale} or @class{gtk-scrollbar}
    widgets.
  @end{short}

  Apart from signals for monitoring the parameters of the adjustment, the
  @sym{gtk-range} class provides properties and methods for influencing the
  sensitivity of the \"steppers\". It also provides properties and methods for
  setting a \"fill level\" on range widgets. See the @fun{gtk-range-fill-level}
  function.
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[arrow-displacement-x]{entry}
        The @code{arrow-displacement-x} style property of type @code{:int}
        (Read) @br{}
        How far in the x direction to move the arrow when the button is
        depressed. @br{}
        @em{Warning:} The @code{arrow-displacement-x} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. The value of this style property is ignored. @br{}
        Default value: 0
      @end{entry}
      @begin[arrow-displacement-y]{entry}
        The @code{arrow-displacement-y} style property of type @code{:int}
        (Read) @br{}
        How far in the y direction to move the arrow when the button is
        depressed. @br{}
        @em{Warning:} The @code{arrow-displacement-y} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. The value of this style property is ignored. @br{}
        Default value: 0
      @end{entry}
      @begin[arrow-scaling]{entry}
        The @code{arrow-scaling} style property of type @code{:float} (Read)
        @br{}
        The arrow size proportion relative to the scroll button size. @br{}
        @em{Warning:} The @code{arrow-scaling} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. The value of this style property is ignored. @br{}
        Allowed values: [0.0,1.0] @br{}
        Default value: 0.5
      @end{entry}
      @begin[slider-width]{entry}
        The @code{slider-width} style property of type @code{:int} (Read) @br{}
        Width of scrollbar or scale thumb. @br{}
        @em{Warning:} The @code{slider-width} style property has been deprecated
        since version 3.20 and should not be used in newly written code. The
        value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 14
      @end{entry}
      @begin[stepper-size]{entry}
        The @code{stepper-size} style property of type @code{:int} (Read) @br{}
        Length of step buttons at ends. @br{}
        @em{Warning:} The @code{stepper-size} style property has been deprecated
        since version 3.20 and should not be used in newly written code. The
        value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 14
      @end{entry}
      @begin[stepper-spacing]{entry}
        The @code{stepper-spacing} style property of type @code{:int} (Read)
        @br{}
        The spacing between the stepper buttons and thumb. Note that
        stepper-spacing will not have any effect if there are no steppers. @br{}
        @em{Warming:} The @code{stepper-spacing} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. The value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[trough-border]{entry}
        The @code{trough-border} style property of type @code{:int} (Read) @br{}
        Spacing between thumb/steppers and outer trough bevel. @br{}
        @em{Warning:} The @code{trough-border} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. The value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 1
      @end{entry}
      @begin[trough-under-steppers]{entry}
        The @code{trough-under-steppers} style property of type @code{:boolean}
        (Read) @br{}
        Whether to draw the trough across the full length of the range or to
        exclude the steppers and their spacing. @br{}
        @em{Warning:} The @code{trough-under-steppers} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. The value of this style property is ignored, and the widget will
        behave as if it was set to @em{true}. @br{}
        Default value: @em{true}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"adjust-bounds\" signal}
      @begin{pre}
 lambda (range value)    :run-last
      @end{pre}
      Emitted before clamping a value, to give the application a chance to
      adjust the bounds.
      @begin[code]{table}
        @entry[range]{The @sym{gtk-range} widget that received the signal.}
        @entry[value]{A double float with the value before we clamp.}
      @end{table}
    @subheading{The \"change-value\" signal}
      @begin{pre}
 lambda (range scroll value)    :run-last
      @end{pre}
      The signal is emitted when a scroll action is performed on a range. It
      allows an application to determine the type of scroll event that occurred
      and the resultant new value. The application can handle the event itself
      and return @em{true} to prevent further processing. Or, by returning
      @em{false}, it can pass the event to other handlers until the default GTK
      handler is reached. The value parameter is unrounded. An application that
      overrides the \"change-value\" signal is responsible for clamping the
      value to the desired number of decimal digits. The default GTK handler
      clamps the value based on \"round-digits\". It is not possible to use
      delayed update policies in an overridden \"change-value\" handler.
      @begin[code]{table}
        @entry[range]{The @sym{gtk-range} widget that received the signal.}
        @entry[scroll]{The @symbol{gtk-scroll-type} value of scroll action that
          was performed.}
        @entry[value]{The double float value resulting from the scroll action.}
        @entry[Returns]{@em{True} to prevent other handlers from being invoked
          for the signal, @em{false} to propagate the signal further.}
      @end{table}
    @subheading{The \"move-slider\" signal}
      @begin{pre}
 lambda (range step)    :action
      @end{pre}
      Virtual function that moves the slider. Used for keybindings.
      @begin[code]{table}
        @entry[range]{The @sym{gtk-range} widget that received the signal.}
        @entry[step]{The @symbol{gtk-scroll-type} value how to move the slider.}
      @end{table}
    @subheading{The \"value-changed\" signal}
      @begin{pre}
 lambda (range)    :run-last
      @end{pre}
      Emitted when the range value changes.
      @begin[code]{table}
        @entry[range]{The @sym{gtk-range} widget that received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-range-adjustment}
  @see-slot{gtk-range-fill-level}
  @see-slot{gtk-range-inverted}
  @see-slot{gtk-range-lower-stepper-sensitivity}
  @see-slot{gtk-range-restrict-to-fill-level}
  @see-slot{gtk-range-round-digits}
  @see-slot{gtk-range-show-fill-level}
  @see-slot{gtk-range-upper-stepper-sensitivity}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-range-adjustment ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "adjustment" 'gtk-range) 't)
 "The @code{adjustment} property of type @class{gtk-adjustment}
  (Read / Write / Construct) @br{}
  The adjustment that contains the current value of this range object.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-range-adjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-range-adjustment 'function)
 "@version{2021-12-26}
  @syntax[]{(gtk-range-adjustment object) => adjustement}
  @syntax[]{(setf (gtk-range-adjustment object) adjustment)}
  @argument[object]{a @class{gtk-range} widget}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @begin{short}
    Accessor of the @slot[gtk-range]{adjustment} slot of the @class{gtk-range}
    class.
  @end{short}

  The @sym{gtk-range-adjustment} slot access function gets the adjustment which
  is the \"model\" object for the range. The @sym{(setf gtk-range-adjustment)}
  slot access function sets the adjustment to be used as the \"model\" object
  for this range.

  The adjustment indicates the current range value, the minimum and maximum
  range values, the step/page increments used for keybindings and scrolling,
  and the page size. The page size is normally 0 for the @class{gtk-scale}
  widget and nonzero for the @class{gtk-scrollbar} widget, and indicates the
  size of the visible area of the widget being scrolled. The page size affects
  the size of the scrollbar slider.
  @see-class{gtk-range}
  @see-class{gtk-scale}
  @see-class{gtk-scrollbar}")

;;; --- gtk-range-fill-level ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "fill-level" 'gtk-range) 't)
 "The @code{fill-level} property of type @code{:double} (Read / Write) @br{}
  The fill level, e.g. prebuffering of a network stream. See the
  @fun{gtk-range-fill-level} function. @br{}
  Default value: 1.79769e+308")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-range-fill-level atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-range-fill-level 'function)
 "@version{2021-12-26}
  @syntax[]{(gtk-range-fill-level object) => fill-level}
  @syntax[]{(setf (gtk-range-fill-level object) fill-level)}
  @argument[object]{a @class{gtk-range} widget}
  @argument[fill-level]{a double float with the new position of the fill level
    indicator}
  @begin{short}
    Accessor of the @slot[gtk-range]{fill-level} slot of the @class{gtk-range}
    class.
  @end{short}

  The @sym{gtk-range-fill-level} slot access function gets the current position
  of the fill level indicator. The @sym{(setf gtk-range-fill-level)} slot access
  function sets the position of the fill level indicator.

  The \"fill level\" is probably best described by its most prominent use case,
  which is an indicator for the amount of pre-buffering in a streaming media
  player. In that use case, the value of the range would indicate the current
  play position, and the fill level would be the position up to which the
  file/stream has been downloaded.

  This amount of prebuffering can be displayed on the trough of the range and is
  themeable separately from the trough. To enable fill level display, use the
  @fun{gtk-range-show-fill-level} function. The range defaults to not
  showing the fill level.

  Additionally, it is possible to restrict the slider position of the range to
  values which are smaller than the fill level. This is controller by the
  @fun{gtk-range-restrict-to-fill-level} function and is by default enabled.
  @see-class{gtk-range}
  @see-function{gtk-range-show-fill-level}
  @see-function{gtk-range-restrict-to-fill-level}")

;;; --- gtk-range-inverted -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inverted" 'gtk-range) 't)
 "The @code{inverted} property of type @code{:boolean} (Read / Write) @br{}
  Invert direction slider moves to increase range value. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-range-inverted atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-range-inverted 'function)
 "@version{2021-12-26}
  @syntax[]{(gtk-range-inverted object) => setting}
  @syntax[]{(setf (gtk-range-inverted object) setting)}
  @argument[object]{a @class{gtk-range} widget}
  @argument[setting]{@em{true} to invert the range}
  @begin{short}
    Accessor of the @slot[gtk-range]{inverted} slot of the @class{gtk-range}
    class.
  @end{short}

  The @sym{gtk-range-inverted} slot access function gets whether the range is
  inverted.

  Ranges normally move from lower to higher values as the slider moves from
  top to bottom or left to right. Inverted ranges have higher values at the
  top or on the right rather than on the bottom or left.
  @see-class{gtk-range}")

;;; --- gtk-rangke-lower-stepper-sensitivity -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "lower-stepper-sensitivity"
                                               'gtk-range) 't)
 "The @code{lower-stepper-sensitivity} property of type
  @symbol{gtk-sensitivity-type} (Read / Write) @br{}
  The sensitivity policy for the stepper that points to the lower side of the
  adjustment. @br{}
  Default value: @code{:auto}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-range-lower-stepper-sensitivity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-range-lower-stepper-sensitivity 'function)
 "@version{2021-12-26}
  @syntax[]{(gtk-range-lower-stepper-sensitivity object) => sensitivity}
  @syntax[]{(setf (gtk-range-lower-stepper-sensitivity object) sensitivity)}
  @argument[object]{a @class{gtk-range} widget}
  @argument[sensitivity]{a value of the @symbol{gtk-sensitivity-type}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk-range]{lower-stepper-sensitivity} slot of the
    @class{gtk-range} class.
  @end{short}

  The @sym{gtk-range-lower-stepper-sensitivity} slot access function gets the
  sensitivity policy for the stepper that points to the 'lower' end of the
  adjustment of the range. The @sym{(setf gtk-range-lower-stepper-sensitivity}
  slot access function sets the sensitivity policy.
  @see-class{gtk-range}
  @see-function{gtk-range-upper-stepper-sensitivity}")

;;; --- gtk-range-restrict-to-fill-level ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "restrict-to-fill-level"
                                               'gtk-range) 't)
 "The @code{restrict-to-fill-level} property of type @code{:boolean}
  (Read / Write) @br{}
  Controls whether slider movement is restricted to an upper boundary set by the
  fill level. See the @fun{gtk-range-restrict-to-fill-level} function. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-range-restrict-to-fill-level atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-range-restrict-to-fill-level 'function)
 "@version{2021-12-26}
  @syntax[]{(gtk-range-restrict-to-fill-level object) => setting}
  @syntax[]{(setf (gtk-range-restrict-to-fill-level object) setting)}
  @argument[object]{a @class{gtk-range} widget}
  @argument[setting]{a boolean whether the fill level restricts slider movement}
  @begin{short}
    Accessor of the @slot[gtk-range]{restrict-to-fill-level} slot of the
    @class{gtk-range} class.
  @end{short}

  The @sym{gtk-range-restrict-to-fill-level} slot access function gets whether
  the range is restricted to the fill level. The
  @sym{(setf gtk-range-restrict-to-fill-level)} slot access function sets
  whether the slider is restricted to the fill level. See the
  @fun{gtk-range-fill-level} function for a general description of the fill
  level concept.
  @see-class{gtk-range}
  @see-function{gtk-range-fill-level}")

;;; --- gtk-range-round-digits -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "round-digits" 'gtk-range) 't)
 "The @code{round-digits} property of type @code{:int} (Read / Write) @br{}
  The number of digits to round the value to when it changes, or -1. See the
  \"change-value\" signal. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-range-round-digits atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-range-round-digits 'function)
 "@version{2021-12-26}
  @syntax[]{(gtk-range-round-digits object) => round-digits}
  @syntax[]{(setf (gtk-range-round-digits object) round-digits)}
  @argument[object]{a @class{gtk-range} widget}
  @argument[round-digits]{an integer with the precision in digits, or -1}
  @begin{short}
    Accessor of the @slot[gtk-range]{round-digits} slot of the @class{gtk-range}
    class.
  @end{short}

  The @sym{gtk-range-round-digits} slot access function gets the number of
  digits to round the value to when it changes. The
  @sym{(setf gtk-range-round-digits)} slot access function sets the number of
  digits to round the value to when it changes. See the \"change-value\" signal.
  @see-class{gtk-range}")

;;; --- gtk-range-show-fill-level ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-fill-level" 'gtk-range) 't)
 "The @code{show-fill-level} property of type @code{:boolean} (Read / Write)
  @br{}
  Controls whether fill level indicator graphics are displayed on the trough.
  See the @fun{gtk-range-show-fill-level} function. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-range-show-fill-level atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-range-show-fill-level 'function)
 "@version{2021-12-26}
  @syntax[]{(gtk-range-show-fill-level object) => show-fill-level}
  @syntax[]{(setf (gtk-range-show-fill-level object) show-fill-level)}
  @argument[object]{a @class{gtk-range} widget}
  @argument[show-fill-level]{a boolean whether a fill level indicator graphics
    is shown}
  @begin{short}
    Accessor of the @slot[gtk-range]{show-fill-level} slot of the
    @class{gtk-range} class.
  @end{short}

  The @sym{gtk-range-show-fill-level} slot access function gets whether the
  range displays the fill level graphically. The
  @sym{(setf gtk-range-show-fill-level)} slot access function sets whether a
  graphical fill level is show on the trough. See the @fun{gtk-range-fill-level}
  function for a general description of the fill level concept.
  @see-class{gtk-range}
  @see-function{gtk-range-fill-level}")

;;; --- gtk-range-upper-stepper-sensitivity ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "upper-stepper-sensitivity"
                                               'gtk-range) 't)
 "The @code{upper-stepper-sensitivity} property of type
  @symbol{gtk-sensitivity-type} (Read / Write) @br{}
  The sensitivity policy for the stepper that points to the upper side of the
  adjustment. @br{}
  Default value: @code{:auto}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-range-upper-stepper-sensitivity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-range-upper-stepper-sensitivity 'function)
 "@version{2021-12-26}
  @syntax[]{(gtk-range-upper-stepper-sensitivity object) => sensitivity}
  @syntax[]{(setf (gtk-range-upper-stepper-sensitivity object) sensitivity)}
  @argument[object]{a @class{gtk-range} widget}
  @argument[sensitivity]{a value of the @symbol{gtk-sensitivity-type}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk-range]{upper-stepper-sensitivity} slot of the
    @class{gtk-range} class.
  @end{short}

  The @sym{gtk-range-upper-stepper-sensitivity} slot access function gets the
  sensitivity policy for the stepper that points to the 'upper' end of the
  adjustment of the range. The @sym{(setf gtk-range-upper-stepper-sensitivity)}
  slot access function sets the sensitivity policy for the stepper that points
  to the 'upper' end of the adjustment of the range.
  @see-class{gtk-range}")

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_value ()
;;; gtk_range_set_value () -> gtk-range-value
;;; ----------------------------------------------------------------------------

(defun (setf gtk-range-value) (value range)
  (setf (gtk-adjustment-value (gtk-range-adjustment range)) value))

(defun gtk-range-value (range)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-26}
  @syntax[]{(gtk-range-value range) => value}
  @syntax[]{(setf (gtk-range-value range) value)}
  @argument[range]{a @class{gtk-range} widget}
  @argument[value]{a double float with the value of the range}
  @begin{short}
    Accessor of the value of the range.
  @end{short}

  The @sym{gtk-range-value} function gets the current value of the range. The
  @sym{(setf gtk-range-value)} function sets the current value of the range.

  If the value is outside the minimum or maximum range values, it will be
  clamped to fit inside them. The range emits the \"value-changed\" signal if
  the value changes.
  @see-class{gtk-range}"
  (gtk-adjustment-value (gtk-range-adjustment range)))

(export 'gtk-range-value)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_increments ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-increments))

(defun gtk-range-set-increments (range step page)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-26}
  @argument[range]{a @class{gtk-range} widget}
  @argument[step]{a double float with the step size}
  @argument[page]{a double float with the page size}
  @begin{short}
    Sets the step and page sizes for the range.
  @end{short}
  The step size is used when the user clicks the @class{gtk-scrollbar} arrows
  or moves the @class{gtk-scale} widget via arrow keys. The page size is used
  for example when moving via @kbd{Page Up} or @kbd{Page Down} keys.
  @see-class{gtk-range}"
  (setf (gtk-adjustment-page-increment (gtk-range-adjustment range)) page
        (gtk-adjustment-step-increment (gtk-range-adjustment range)) step))

(export 'gtk-range-set-increments)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_range ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-range))

(defun gtk-range-set-range (range min max)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-26}
  @argument[range]{a @class{gtk-range} widget}
  @argument[min]{a double float with the minimum range value}
  @argument[max]{a double float with the maximum range value}
  @begin{short}
    Sets the allowable values in the range, and clamps the range value to be
    between @arg{min} and @arg{max}.
  @end{short}
  If the range has a non-zero page size, it is clamped between @arg{min} and
  @code{(@arg{max} - @code{page-size})}.
  @see-class{gtk-range}"
  (setf (gtk-adjustment-lower (gtk-range-adjustment range)) min
        (gtk-adjustment-upper (gtk-range-adjustment range)) max))

(export 'gtk-range-set-range)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_flippable ()
;;; gtk_range_set_flippable () -> gtk-range-flippable
;;; ----------------------------------------------------------------------------

(defun (setf gtk-range-flippable) (flippable range)
  (foreign-funcall "gtk_range_set_flippable"
                   (g-object gtk-range) range
                   :boolean flippable
                   :void)
  flippable)

(defcfun ("gtk_range_get_flippable" gtk-range-flippable) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-26}
  @syntax[]{gtk-range-flippable range) => flippable}
  @syntax[]{(setf (gtk-range-flippable range) flippable)}
  @argument[range]{a @class{gtk-range} widget}
  @argument[[flippable]{@em{true} to make the range flippable}
  @begin{short}
    Accessor of the flippable property of the range.
  @end{short}

  If a range is flippable, it will switch its direction if it is horizontal and
  its direction is @code{:rtl}. See the @fun{gtk-widget-direction} function.
  @see-class{gtk-range}
  @see-function{gtk-widget-direction}"
  (range (g-object gtk-range)))

(export 'gtk-range-flippable)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_min_slider_size ()
;;; gtk_range_get_min_slider_size () -> gtk-range-min-slider-size
;;; ----------------------------------------------------------------------------

(defun (setf gtk-range-min-slider-size) (min-size range)
  (foreign-funcall "gtk_range_set_min_slider_size"
                   (g-object gtk-range) range
                   :int min-size
                   :void)
  min-size)

(defcfun ("gtk_range_get_min_slider_size" gtk-range-min-slider-size) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-12-26}
  @syntax[]{(gtk-range-min-slider-size range) => min-size}
  @syntax[]{(setf (gtk-range-min-slider-size range) min-size)}
  @argument[range]{a @class{gtk-range} widget}
  @argument[min-size]{an integer with the minimum size of the slider}
  @begin{short}
    Accessor of the minimum size of the slider of the range.
  @end{short}

  The @sym{gtk-range-min-slider-size} function retrieves the minimum size of
  the slider of the range. The @sym{(setf gtk-range-min-slider-size)} function
  sets the minimum size of the slider of the range.

  This function is useful mainly for @class{gtk-range} subclasses.
  @begin[Warning]{dictionary}
    The @sym{gtk-range-min-slider-size} function has been deprecated since
    version 3.20 and should not be used in newly written code. Use the
    min-height/min-width CSS properties on the slider node.
  @end{dictionary}
  @see-class{gtk-range}"
  (range (g-object gtk-range)))

(export 'gtk-range-min-slider-size)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_range_rect () -> gtk-range-range-rect
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_range_get_range_rect" %gtk-range-get-range-rect) :void
  (range (g-object gtk-range))
  (range-rect (g-boxed-foreign gdk-rectangle)))

(defun gtk-range-range-rect (range)
 #+cl-cffi-gtk-documentation
 "@version{2021-26-26}
  @argument[range]{a @class{gtk-range} widget}
  @return{The @class{gdk-rectangle} instance with the range rectangle.}
  @begin{short}
    This function returns the area that contains the trough of the range and its
    steppers, in widget window coordinates.
  @end{short}

  This function is useful mainly for @class{gtk-range} subclasses.
  @see-class{gtk-range}"
  (let ((range-rect (gdk-rectangle-new)))
    (%gtk-range-get-range-rect range range-rect)
    range-rect))

(export 'gtk-range-range-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_slider_range () -> gtk-range-slider-range
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_range_get_slider_range" %gtk-range-get-slider-range) :void
  (range (g-object gtk-range))
  (slider-start (:pointer :int))
  (slider-end (:pointer :int)))

(defun gtk-range-slider-range (range)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-26}
  @argument[range]{a @class{gtk-range} widget}
  @begin{return}
    @arg{slider-start} -- an integer with the start of the slider, or @code{nil}
    @br{}
    @arg{slider-end} -- an integer with the end of the slider, or @code{nil}
  @end{return}
  @begin{short}
    This function returns sliders range along the long dimension, in
    widget window coordinates.
  @end{short}

  This function is useful mainly for @class{gtk-range} subclasses.
  @see-class{gtk-range}"
  (with-foreign-objects ((slider-start :int) (slider-end :int))
    (%gtk-range-get-slider-range range slider-start slider-end)
    (values (if (not (null-pointer-p slider-start))
                (mem-ref slider-start :int)
                nil)
            (if (not (null-pointer-p slider-end))
                (mem-ref slider-end :int)
                nil))))

(export 'gtk-range-slider-range)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_slider_size_fixed ()
;;; gtk_range_get_slider_size_fixed () -> gtk-range-slider-size-fixed
;;; ----------------------------------------------------------------------------

(defun (setf gtk-range-slider-size-fixed) (size-fixed range)
  (foreign-funcall "gtk_range_set_slider_size_fixed"
                   (g-object gtk-range) range
                   :boolean size-fixed
                   :void)
  size-fixed)

(defcfun ("gtk_range_get_slider_size_fixed" gtk-range-slider-size-fixed)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-12-26}
  @syntax[]{(gtk-range-slider-size-fixed range) => size-fixed}
  @syntax[]{(setf (gtk-range-slider-size-fixed range) size-fixed)}
  @argument[range]{a @class{gtk-range} widget}
  @argument[size-fixed]{@em{true} to make the slider size constant}
  @begin{short}
    Whether the slider of the range has a fixed size.
  @end{short}

  Sets whether the slider of the range has a fixed size, or a size that depends
  on its page size of the adjustment.

  This function is useful mainly for @class{gtk-range} subclasses.
  @see-class{gtk-range}"
  (range (g-object gtk-range)))

(export 'gtk-range-slider-size-fixed)

;;; --- End of file gtk.range.lisp ---------------------------------------------
