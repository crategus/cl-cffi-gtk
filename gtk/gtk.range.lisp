;;; ----------------------------------------------------------------------------
;;; gtk.range.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See http://www.gtk.org.
;;; 
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; Base class for widgets which visualize an adjustment
;;;     
;;; Synopsis
;;; 
;;;     GtkRange
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
;;;     
;;;     GtkSensitivityType
;;;     
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
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkRange
;;;                      +----GtkScale
;;;                      +----GtkScrollbar
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkRange implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; 
;;; Properties
;;; 
;;;   "adjustment"               GtkAdjustment*       : Read / Write / Construct
;;;   "fill-level"               gdouble              : Read / Write
;;;   "inverted"                 gboolean             : Read / Write
;;;   "lower-stepper-sensitivity" GtkSensitivityType  : Read / Write
;;;   "restrict-to-fill-level"   gboolean             : Read / Write
;;;   "round-digits"             gint                 : Read / Write
;;;   "show-fill-level"          gboolean             : Read / Write
;;;   "upper-stepper-sensitivity" GtkSensitivityType  : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "arrow-displacement-x"     gint                 : Read
;;;   "arrow-displacement-y"     gint                 : Read
;;;   "arrow-scaling"            gfloat               : Read
;;;   "slider-width"             gint                 : Read
;;;   "stepper-size"             gint                 : Read
;;;   "stepper-spacing"          gint                 : Read
;;;   "trough-border"            gint                 : Read
;;;   "trough-under-steppers"    gboolean             : Read
;;; 
;;; Signals
;;; 
;;;   "adjust-bounds"                                 : Run Last
;;;   "change-value"                                  : Run Last
;;;   "move-slider"                                   : Action
;;;   "value-changed"                                 : Run Last
;;; 
;;; Description
;;; 
;;; GtkRange is the common base class for widgets which visualize an adjustment,
;;; e.g GtkScale or GtkScrollbar.
;;; 
;;; Apart from signals for monitoring the parameters of the adjustment, GtkRange
;;; provides properties and methods for influencing the sensitivity of the
;;; "steppers". It also provides properties and methods for setting a
;;; "fill level" on range widgets. See gtk_range_set_fill_level().
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "adjustment" property
;;; 
;;;   "adjustment"               GtkAdjustment*       : Read / Write / Construct
;;; 
;;; The GtkAdjustment that contains the current value of this range object.
;;;
;;; ----------------------------------------------------------------------------
;;; The "fill-level" property
;;; 
;;;   "fill-level"               gdouble               : Read / Write
;;; 
;;; The fill level (e.g. prebuffering of a network stream).
;;; See gtk_range_set_fill_level().
;;; 
;;; Default value: 1.79769e+308
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "inverted" property
;;; 
;;;   "inverted"                 gboolean              : Read / Write
;;; 
;;; Invert direction slider moves to increase range value.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "lower-stepper-sensitivity" property
;;; 
;;;   "lower-stepper-sensitivity" GtkSensitivityType    : Read / Write
;;; 
;;; The sensitivity policy for the stepper that points to the adjustment's
;;; lower side.
;;; 
;;; Default value: GTK_SENSITIVITY_AUTO
;;;
;;; ----------------------------------------------------------------------------
;;; The "restrict-to-fill-level" property
;;; 
;;;   "restrict-to-fill-level"   gboolean              : Read / Write
;;; 
;;; The restrict-to-fill-level property controls whether slider movement is
;;; restricted to an upper boundary set by the fill level. See
;;; gtk_range_set_restrict_to_fill_level().
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "round-digits" property
;;; 
;;;   "round-digits"             gint                  : Read / Write
;;; 
;;; The number of digits to round the value to when it changes, or -1. See
;;; "change-value".
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.24
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-fill-level" property
;;; 
;;;   "show-fill-level"          gboolean              : Read / Write
;;; 
;;; The show-fill-level property controls whether fill level indicator graphics
;;; are displayed on the trough. See gtk_range_set_show_fill_level().
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "upper-stepper-sensitivity" property
;;; 
;;;   "upper-stepper-sensitivity" GtkSensitivityType    : Read / Write
;;; 
;;; The sensitivity policy for the stepper that points to the adjustment's
;;; upper side.
;;; 
;;; Default value: GTK_SENSITIVITY_AUTO
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-displacement-x" style property
;;; 
;;;   "arrow-displacement-x"     gint                  : Read
;;; 
;;; How far in the x direction to move the arrow when the button is depressed.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-displacement-y" style property
;;; 
;;;   "arrow-displacement-y"     gint                  : Read
;;; 
;;; How far in the y direction to move the arrow when the button is depressed.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-scaling" style property
;;; 
;;;   "arrow-scaling"            gfloat                : Read
;;; 
;;; The arrow size proportion relative to the scroll button size.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "slider-width" style property
;;; 
;;;   "slider-width"             gint                  : Read
;;; 
;;; Width of scrollbar or scale thumb.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 14
;;;
;;; ----------------------------------------------------------------------------
;;; The "stepper-size" style property
;;; 
;;;   "stepper-size"             gint                  : Read
;;; 
;;; Length of step buttons at ends.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 14
;;;
;;; ----------------------------------------------------------------------------
;;; The "stepper-spacing" style property
;;; 
;;;   "stepper-spacing"          gint                  : Read
;;; 
;;; The spacing between the stepper buttons and thumb. Note that stepper-spacing
;;; won't have any effect if there are no steppers.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "trough-border" style property
;;; 
;;;   "trough-border"            gint                  : Read
;;; 
;;; Spacing between thumb/steppers and outer trough bevel.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "trough-under-steppers" style property
;;; 
;;;   "trough-under-steppers"    gboolean              : Read
;;; 
;;; Whether to draw the trough across the full length of the range or to exclude
;;; the steppers and their spacing.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "adjust-bounds" signal
;;; 
;;; void user_function (GtkRange *range,
;;;                     gdouble   value,
;;;                     gpointer  user_data)      : Run Last
;;; 
;;; Emitted before clamping a value, to give the application a chance to adjust
;;; the bounds.
;;; 
;;; range :
;;;     the GtkRange that received the signal
;;; 
;;; value :
;;;     the value before we clamp
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;;
;;; ----------------------------------------------------------------------------
;;; The "change-value" signal
;;; 
;;; gboolean user_function (GtkRange     *range,
;;;                         GtkScrollType scroll,
;;;                         gdouble       value,
;;;                         gpointer      user_data)      : Run Last
;;; 
;;; The "change-value" signal is emitted when a scroll action is performed on a
;;; range. It allows an application to determine the type of scroll event that
;;; occurred and the resultant new value. The application can handle the event
;;; itself and return TRUE to prevent further processing. Or, by returning
;;; FALSE, it can pass the event to other handlers until the default GTK+
;;; handler is reached.
;;; 
;;; The value parameter is unrounded. An application that overrides the
;;; GtkRange::change-value signal is responsible for clamping the value to the
;;; desired number of decimal digits; the default GTK+ handler clamps the value
;;; based on "round-digits".
;;; 
;;; It is not possible to use delayed update policies in an overridden
;;; "change-value" handler.
;;; 
;;; range :
;;;     the GtkRange that received the signal
;;; 
;;; scroll :
;;;     the type of scroll action that was performed
;;; 
;;; value :
;;;     the new value resulting from the scroll action
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;; 
;;; Returns :
;;;     TRUE to prevent other handlers from being invoked for the signal, FALSE
;;;     to propagate the signal further
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-slider" signal
;;; 
;;; void user_function (GtkRange     *range,
;;;                     GtkScrollType step,
;;;                     gpointer      user_data)      : Action
;;; 
;;; Virtual function that moves the slider. Used for keybindings.
;;; 
;;; range :
;;;     the GtkRange that received the signal
;;; 
;;; step :
;;;     how to move the slider
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;;
;;; ----------------------------------------------------------------------------
;;; The "value-changed" signal
;;; 
;;; void user_function (GtkRange *range,
;;;                     gpointer  user_data)      : Run Last
;;; 
;;; Emitted when the range value changes.
;;; 
;;; range :
;;;     the GtkRange that received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRange
;;; 
;;; struct GtkRange;
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

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_fill_level ()
;;; 
;;; gdouble gtk_range_get_fill_level (GtkRange *range);
;;; 
;;; Gets the current position of the fill level indicator.
;;; 
;;; range :
;;;     A GtkRange
;;; 
;;; Returns :
;;;     The current fill level
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-get-fill-level))

(defun gtk-range-get-fill-level (range)
  (gtk-range-fill-level range))

(export 'gtk-range-get-fill-level)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_restrict_to_fill_level ()
;;; 
;;; gboolean gtk_range_get_restrict_to_fill_level (GtkRange *range);
;;; 
;;; Gets whether the range is restricted to the fill level.
;;; 
;;; range :
;;;     A GtkRange
;;; 
;;; Returns :
;;;     TRUE if range is restricted to the fill level.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-get-restrict-to-fill-level))

(defun gtk-range-get-restrict-to-fill-level (range)
  (gtk-range-restrict-to-fill-level range))

(export 'gtk-range-get-restrict-to-fill-level)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_show_fill_level ()
;;; 
;;; gboolean gtk_range_get_show_fill_level (GtkRange *range);
;;; 
;;; Gets whether the range displays the fill level graphically.
;;; 
;;; range :
;;;     A GtkRange
;;; 
;;; Returns :
;;;     TRUE if range shows the fill level.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-get-show-fill-level))

(defun gtk-range-get-show-fill-level (range)
  (gtk-range-show-fill-level range)) 

(export 'gtk-range-get-show-fill-level)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_fill_level ()
;;; 
;;; void gtk_range_set_fill_level (GtkRange *range, gdouble fill_level);
;;; 
;;; Set the new position of the fill level indicator.
;;; 
;;; The "fill level" is probably best described by its most prominent use case,
;;; which is an indicator for the amount of pre-buffering in a streaming media
;;; player. In that use case, the value of the range would indicate the current
;;; play position, and the fill level would be the position up to which the
;;; file/stream has been downloaded.
;;; 
;;; This amount of prebuffering can be displayed on the range's trough and is
;;; themeable separately from the trough. To enable fill level display, use
;;; gtk_range_set_show_fill_level(). The range defaults to not showing the fill
;;; level.
;;; 
;;; Additionally, it's possible to restrict the range's slider position to
;;; values which are smaller than the fill level. This is controller by
;;; gtk_range_set_restrict_to_fill_level() and is by default enabled.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; fill_level :
;;;     the new position of the fill level indicator
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-fill-level))

(defun gtk-range-set-fill-level (range fill-level)
  (setf (gtk-range-fill-level range) fill-level))

(export 'gtk-range-set-fill-level)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_restrict_to_fill_level ()
;;; 
;;; void gtk_range_set_restrict_to_fill_level (GtkRange *range,
;;;                                            gboolean restrict_to_fill_level);
;;; 
;;; Sets whether the slider is restricted to the fill level. See
;;; gtk_range_set_fill_level() for a general description of the fill level
;;; concept.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; restrict_to_fill_level :
;;;     whether the fill level restricts slider movement
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-restrict-to-fill-level))

(defun gtk-range-set-restrict-to-fill-level (range restrict-to-fill-level)
  (setf (gtk-range-restrict-to-fill-level range) restrict-to-fill-level))

(export 'gtk-range-set-restrict-to-fill-level)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_show_fill_level ()
;;; 
;;; void gtk_range_set_show_fill_level (GtkRange *range,
;;;                                     gboolean show_fill_level);
;;; 
;;; Sets whether a graphical fill level is show on the trough. See
;;; gtk_range_set_fill_level() for a general description of the fill level
;;; concept.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; show_fill_level :
;;;     whether a fill level indicator graphics is shown
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-show-fill-level))

(defun gtk-range-set-show-fill-level (range show-fill-level)
  (setf (gtk-range-show-fill-level range) show-fill-level))

(export 'gtk-range-set-show-fill-level)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_adjustment ()
;;; 
;;; GtkAdjustment * gtk_range_get_adjustment (GtkRange *range);
;;; 
;;; Get the GtkAdjustment which is the "model" object for GtkRange. See
;;; gtk_range_set_adjustment() for details. The return value does not have a
;;; reference added, so should not be unreferenced.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; Returns :
;;;     a GtkAdjustment
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-get-adjustment))

(defun gtk-range-get-adjustment (range)
  (gtk-range-adjustment range))

(export 'gtk-range-get-adjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_adjustment ()
;;; 
;;; void gtk_range_set_adjustment (GtkRange *range, GtkAdjustment *adjustment);
;;; 
;;; Sets the adjustment to be used as the "model" object for this range widget.
;;; The adjustment indicates the current range value, the minimum and maximum
;;; range values, the step/page increments used for keybindings and scrolling,
;;; and the page size. The page size is normally 0 for GtkScale and nonzero for
;;; GtkScrollbar, and indicates the size of the visible area of the widget being
;;; scrolled. The page size affects the size of the scrollbar slider.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-adjustment))

(defun gtk-range-set-adjustment (range adjustment)
  (setf (gtk-range-adjustment range) adjustment))

(export 'gtk-range-set-adjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_inverted ()
;;; 
;;; gboolean gtk_range_get_inverted (GtkRange *range);
;;; 
;;; Gets the value set by gtk_range_set_inverted().
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; Returns :
;;;     TRUE if the range is inverted
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-get-inverted))

(defun gtk-range-get-inverted (range)
  (gtk-range-inverted range))

(export 'gtk-range-get-inverted)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_inverted ()
;;; 
;;; void gtk_range_set_inverted (GtkRange *range, gboolean setting);
;;; 
;;; Ranges normally move from lower to higher values as the slider moves from
;;; top to bottom or left to right. Inverted ranges have higher values at the
;;; top or on the right rather than on the bottom or left.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; setting :
;;;     TRUE to invert the range
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-inverted))

(defun gtk-range-set-inverted (range setting)
  (setf (gtk-range-inverted range) setting))

(export 'gtk-range-set-inverted)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_value ()
;;; 
;;; gdouble gtk_range_get_value (GtkRange *range);
;;; 
;;; Gets the current value of the range.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; Returns :
;;;     current value of the range
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-get-value))

(defun gtk-range-get-value (range)
  (gtk-adjustment-value (gtk-range-adjustment range)))

(export 'gtk-range-get-value)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_value ()
;;; 
;;; void gtk_range_set_value (GtkRange *range, gdouble value);
;;; 
;;; Sets the current value of the range; if the value is outside the minimum or
;;; maximum range values, it will be clamped to fit inside them. The range emits
;;; the "value-changed" signal if the value changes.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; value :
;;;     new value of the range
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-value))

(defun gtk-range-set-value (range value)
  (setf (gtk-adjustment-value (gtk-range-adjustment range)) value))

(export 'gtk-range-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_increments ()
;;; 
;;; void gtk_range_set_increments (GtkRange *range,
;;;                                gdouble step,
;;;                                gdouble page);
;;; 
;;; Sets the step and page sizes for the range. The step size is used when the
;;; user clicks the GtkScrollbar arrows or moves GtkScale via arrow keys. The
;;; page size is used for example when moving via Page Up or Page Down keys.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; step :
;;;     step size
;;; 
;;; page :
;;;     page size
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-increments))

(defun gtk-range-set-increments (range step page)
  (setf (gtk-adjustment-page-increment (gtk-range-adjustment range)) page
        (gtk-adjustment-step-increment (gtk-range-adjustment range)) step))

(export 'gtk-range-set-increments)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_range ()
;;; 
;;; void gtk_range_set_range (GtkRange *range, gdouble min, gdouble max);
;;; 
;;; Sets the allowable values in the GtkRange, and clamps the range value to be
;;; between min and max. (If the range has a non-zero page size, it is clamped
;;; between min and max - page-size.)
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; min :
;;;     minimum range value
;;; 
;;; max :
;;;     maximum range value
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-range))

(defun gtk-range-set-range (range min max)
  (setf (gtk-adjustment-lower (gtk-range-adjustment range)) min
        (gtk-adjustment-upper (gtk-range-adjustment range)) max))

(export 'gtk-range-set-range)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_round_digits ()
;;; 
;;; gint gtk_range_get_round_digits (GtkRange *range);
;;; 
;;; Gets the number of digits to round the value to when it changes.
;;; See "change-value".
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; Returns :
;;;     the number of digits to round to
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-get-round-digits))

(defun gtk-range-get-round-digits (range)
  (gtk-range-round-digits range))

(export 'gtk-range-get-round-digits)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_round_digits ()
;;; 
;;; void gtk_range_set_round_digits (GtkRange *range, gint round_digits);
;;; 
;;; Sets the number of digits to round the value to when it changes.
;;; See "change-value".
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; round_digits :
;;;     the precision in digits, or -1
;;; 
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-round-digits))

(defun gtk-range-set-round-digits (range round-digits)
  (setf (gtk-range-round-digits range) round-digits))

(export 'gtk-range-set-round-digits)

;;; ----------------------------------------------------------------------------
;;; enum GtkSensitivityType
;;; 
;;; typedef enum {
;;;   GTK_SENSITIVITY_AUTO,
;;;   GTK_SENSITIVITY_ON,
;;;   GTK_SENSITIVITY_OFF
;;; } GtkSensitivityType;
;;; 
;;; Determines how GTK+ handles the sensitivity of stepper arrows at the end of
;;; range widgets.
;;; 
;;; GTK_SENSITIVITY_AUTO
;;;     The arrow is made insensitive if the thumb is at the end
;;; 
;;; GTK_SENSITIVITY_ON
;;;     The arrow is always sensitive
;;; 
;;; GTK_SENSITIVITY_OFF
;;;     The arrow is always insensitive
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkSensitivityType" gtk-sensitivity-type
  (:export t
   :type-initializer "gtk_sensitivity_type_get_type")
  (:auto 0)
  (:on 1)
  (:off 2))

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_lower_stepper_sensitivity ()
;;; 
;;; void gtk_range_set_lower_stepper_sensitivity
;;;                                             (GtkRange *range,
;;;                                              GtkSensitivityType sensitivity)
;;; 
;;; Sets the sensitivity policy for the stepper that points to the 'lower' end
;;; of the GtkRange's adjustment.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; sensitivity :
;;;     the lower stepper's sensitivity policy.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-lower-stepper-sensitivity))

(defun gtk-range-set-lower-stepper-sensitivity (range sensitivity)
  (setf (gtk-range-lower-stepper-sensitivity range) sensitivity))

(export 'gtk-range-set-lower-stepper-sensitivity)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_lower_stepper_sensitivity ()
;;; 
;;; GtkSensitivityType gtk_range_get_lower_stepper_sensitivity (GtkRange *range)
;;; 
;;; Gets the sensitivity policy for the stepper that points to the 'lower' end
;;; of the GtkRange's adjustment.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; Returns :
;;;     The lower stepper's sensitivity policy.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-get-lower-stepper-sensitivity))

(defun gtk-range-get-lower-stepper-sensitivity (range)
  (gtk-range-lower-stepper-sensitivity range))

(export 'gtk-range-get-lower-stepper-sensitivity)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_upper_stepper_sensitivity ()
;;; 
;;; void gtk_range_set_upper_stepper_sensitivity
;;;                                             (GtkRange *range,
;;;                                              GtkSensitivityType sensitivity)
;;; 
;;; Sets the sensitivity policy for the stepper that points to the 'upper' end
;;; of the GtkRange's adjustment.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; sensitivity :
;;;     the upper stepper's sensitivity policy.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-set-upper-stepper-sensitivity))

(defun gtk-range-set-upper-stepper-sensitivity (range sensitivity)
  (setf (gtk-range-upper-stepper-sensitivity range) sensitivity))

(export 'gtk-range-set-upper-stepper-sensitivity)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_upper_stepper_sensitivity ()
;;; 
;;; GtkSensitivityType gtk_range_get_upper_stepper_sensitivity (GtkRange *range)
;;; 
;;; Gets the sensitivity policy for the stepper that points to the 'upper' end
;;; of the GtkRange's adjustment.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; Returns :
;;;     The upper stepper's sensitivity policy.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-range-get-upper-stepper-sensitivity))

(defun gtk-range-get-upper-stepper-sensitivity (range)
  (gtk-range-upper-stepper-sensitivity range))

(export 'gtk-range-get-upper-stepper-sensitivity)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_flippable ()
;;; 
;;; gboolean gtk_range_get_flippable (GtkRange *range);
;;; 
;;; Gets the value set by gtk_range_set_flippable().
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; Returns :
;;;     TRUE if the range is flippable
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_range_get_flippable" gtk-range-get-flippable) :boolean
  (range (g-object gtk-range)))

(export 'gtk-range-get-flippable)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_flippable ()
;;; 
;;; void gtk_range_set_flippable (GtkRange *range, gboolean flippable);
;;; 
;;; If a range is flippable, it will switch its direction if it is horizontal
;;; and its direction is GTK_TEXT_DIR_RTL.
;;; 
;;; See gtk_widget_get_direction().
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; flippable :
;;;     TRUE to make the range flippable
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_range_get_flippable" gtk-range-set-flippable) :void
  (range (g-object gtk-range))
  (flippable :boolean))

(export 'gtk-range-set-flippable)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_min_slider_size ()
;;; 
;;; gint gtk_range_get_min_slider_size (GtkRange *range);
;;; 
;;; This function is useful mainly for GtkRange subclasses.
;;; 
;;; See gtk_range_set_min_slider_size().
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; Returns :
;;;     The minimum size of the range's slider.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_range_get_min_slider_size" gtk-range-get-min-slider-size) :int
  (range (g-object gtk-range)))

(export 'gtk-range-get-min-slider-size)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_range_rect ()
;;; 
;;; void gtk_range_get_range_rect (GtkRange *range, GdkRectangle *range_rect);
;;; 
;;; This function returns the area that contains the range's trough and its
;;; steppers, in widget->window coordinates.
;;; 
;;; This function is useful mainly for GtkRange subclasses.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; range_rect :
;;;     return location for the range rectangle
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_range_get_range_rect" %gtk-range-get-range-rect) :void
  (range (g-object gtk-range))
  (range-rect (g-boxed-foreign gdk-rectangle)))

(defun gtk-range-get-range-rect (range)
  (let ((range-rect (make-gdk-rectangle)))
    (%gtk-range-get-range-rect range range-rect)
    range-rect))

(export 'gtk-range-get-range-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_slider_range ()
;;; 
;;; void gtk_range_get_slider_range (GtkRange *range,
;;;                                  gint *slider_start,
;;;                                  gint *slider_end);
;;; 
;;; This function returns sliders range along the long dimension, in
;;; widget->window coordinates.
;;; 
;;; This function is useful mainly for GtkRange subclasses.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; slider_start :
;;;     return location for the slider's start, or NULL
;;; 
;;; slider_end :
;;;     return location for the slider's end, or NULL
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_range_get_slider_range" %gtk-range-get-slider-range) :void
  (range (g-object gtk-range))
  (slider-start (:pointer :int))
  (slider-end (:pointer :int)))

(defun gtk-range-get-slider-range (range)
  (with-foreign-objects ((slider-start :int) (slider-end :int))
    (%gtk-range-get-slider-range range slider-start slider-end)
    (values (if (not (null-pointer-p slider-start))
                (mem-ref slider-start :int)
                nil)
            (if (not (null-pointer-p slider-end))
                (mem-ref slider-end :int)
                nil))))

(export 'gtk-range-get-slider-range)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_slider_size_fixed ()
;;; 
;;; gboolean gtk_range_get_slider_size_fixed (GtkRange *range);
;;; 
;;; This function is useful mainly for GtkRange subclasses.
;;; 
;;; See gtk_range_set_slider_size_fixed().
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; Returns :
;;;     whether the range's slider has a fixed size.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_range_get_slider_size_fixed" gtk-range-get-slider-size-fixed)
    :boolean
  (range (g-object gtk-range)))

(export 'gtk-range-get-slider-size-fixed)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_min_slider_size ()
;;; 
;;; void gtk_range_set_min_slider_size (GtkRange *range, gint min_size);
;;; 
;;; Sets the minimum size of the range's slider.
;;; 
;;; This function is useful mainly for GtkRange subclasses.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; min_size :
;;;     The slider's minimum size
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_range_set_min_slider_size" gtk-range-set-min-slider-size) :void
  (range (g-object gtk-range))
  (min-size :int))

(export 'gtk-range-set-min-slider-size)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_slider_size_fixed ()
;;; 
;;; void gtk_range_set_slider_size_fixed (GtkRange *range, gboolean size_fixed);
;;; 
;;; Sets whether the range's slider has a fixed size, or a size that depends on
;;; its adjustment's page size.
;;; 
;;; This function is useful mainly for GtkRange subclasses.
;;; 
;;; range :
;;;     a GtkRange
;;; 
;;; size_fixed :
;;;     TRUE to make the slider size constant
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_range_set_slider_size_fixed" gtk-range-set-slider-size-fixed)
    :void
  (range (g-object gtk-range))
  (size-fixed :boolean))

(export 'gtk-range-set-slider-size-fixed)

;;; --- End of file gtk.range.lisp ---------------------------------------------
