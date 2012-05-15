;;; ----------------------------------------------------------------------------
;;; gtk.adjustment.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.2. See http://www.gtk.org.
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
;;; GtkAdjustment
;;; 
;;; A representation of an adjustable bounded value
;;;     
;;; Synopsis
;;; 
;;;     GtkAdjustment
;;;     
;;;     gtk_adjustment_new
;;;     gtk_adjustment_get_value
;;;     gtk_adjustment_set_value
;;;     gtk_adjustment_clamp_page
;;;     gtk_adjustment_changed
;;;     gtk_adjustment_value_changed
;;;     gtk_adjustment_configure
;;;     gtk_adjustment_get_lower
;;;     gtk_adjustment_get_page_increment
;;;     gtk_adjustment_get_page_size
;;;     gtk_adjustment_get_step_increment
;;;     gtk_adjustment_get_minimum_increment
;;;     gtk_adjustment_get_upper
;;;     gtk_adjustment_set_lower
;;;     gtk_adjustment_set_page_increment
;;;     gtk_adjustment_set_page_size
;;;     gtk_adjustment_set_step_increment
;;;     gtk_adjustment_set_upper
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkAdjustment
;;; 
;;; Properties
;;; 
;;;   "lower"                    gdouble               : Read / Write
;;;   "page-increment"           gdouble               : Read / Write
;;;   "page-size"                gdouble               : Read / Write
;;;   "step-increment"           gdouble               : Read / Write
;;;   "upper"                    gdouble               : Read / Write
;;;   "value"                    gdouble               : Read / Write
;;; 
;;; Signals
;;; 
;;;   "changed"                                        : No Recursion
;;;   "value-changed"                                  : No Recursion
;;; 
;;; Description
;;; 
;;; The GtkAdjustment object represents a value which has an associated lower
;;; and upper bound, together with step and page increments, and a page size. It
;;; is used within several GTK+ widgets, including GtkSpinButton, GtkViewport,
;;; and GtkRange (which is a base class for GtkHScrollbar, GtkVScrollbar,
;;; GtkHScale, and GtkVScale).
;;; 
;;; The GtkAdjustment object does not update the value itself. Instead it is
;;; left up to the owner of the GtkAdjustment to control the value.
;;; 
;;; The owner of the GtkAdjustment typically calls the
;;; gtk_adjustment_value_changed() and gtk_adjustment_changed() functions after
;;; changing the value and its bounds. This results in the emission of the
;;; "value_changed" or "changed" signal respectively.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "lower" property
;;; 
;;;   "lower"                    gdouble               : Read / Write
;;; 
;;; The minimum value of the adjustment.
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "page-increment" property
;;; 
;;;   "page-increment"           gdouble               : Read / Write
;;; 
;;; The page increment of the adjustment.
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "page-size" property
;;; 
;;;   "page-size"                gdouble               : Read / Write
;;; 
;;; The page size of the adjustment. Note that the page-size is irrelevant and
;;; should be set to zero if the adjustment is used for a simple scalar value,
;;; e.g. in a GtkSpinButton.
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "step-increment" property
;;; 
;;;   "step-increment"           gdouble               : Read / Write
;;; 
;;; The step increment of the adjustment.
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "upper" property
;;; 
;;;   "upper"                    gdouble               : Read / Write
;;; 
;;; The maximum value of the adjustment. Note that values will be restricted by
;;; upper - page-size if the page-size property is nonzero.
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "value" property
;;; 
;;;   "value"                    gdouble               : Read / Write
;;; 
;;; The value of the adjustment.
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "changed" signal
;;; 
;;; void user_function (GtkAdjustment *adjustment,
;;;                     gpointer       user_data)       : No Recursion
;;; 
;;; Emitted when one or more of the GtkAdjustment fields have been changed,
;;; other than the value field.
;;; 
;;; adjustment :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;;
;;; ----------------------------------------------------------------------------
;;; The "value-changed" signal
;;; 
;;; void user_function (GtkAdjustment *adjustment,
;;;                     gpointer       user_data)       : No Recursion
;;; 
;;; Emitted when the GtkAdjustment value field has been changed.
;;; 
;;; adjustment :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAdjustment
;;; 
;;; typedef struct _GtkAdjustment GtkAdjustment;
;;; 
;;; The GtkAdjustment struct contains only private fields and should not be
;;; directly accessed.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAdjustment" gtk-adjustment
  (:superclass g-initially-unowned
   :export t
   :interfaces nil
   :type-initializer "gtk_adjustment_get_type")
  ((lower
    gtk-adjustment-lower
    "lower" "gdouble" t t)
   (page-increment
    gtk-adjustment-page-increment
    "page-increment" "gdouble" t t)
   (page-size
    gtk-adjustment-page-size
    "page-size" "gdouble" t t)
   (step-increment
    gtk-adjustment-step-increment
    "step-increment" "gdouble" t t)
   (upper
    gtk-adjustment-upper
    "upper" "gdouble" t t)
   (value
    gtk-adjustment-value
    "value" "gdouble" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_new ()
;;; 
;;; GtkAdjustment * gtk_adjustment_new (gdouble value,
;;;                                     gdouble lower,
;;;                                     gdouble upper,
;;;                                     gdouble step_increment,
;;;                                     gdouble page_increment,
;;;                                     gdouble page_size);
;;; 
;;; Creates a new GtkAdjustment.
;;; 
;;; value :
;;;     the initial value
;;; 
;;; lower :
;;;     the minimum value
;;; 
;;; upper :
;;;     the maximum value
;;; 
;;; step_increment :
;;;     the step increment
;;; 
;;; page_increment :
;;;     the page increment
;;; 
;;; page_size :
;;;     the page size
;;; 
;;; Returns :
;;;     a new GtkAdjustment
;;; ----------------------------------------------------------------------------

(defun gtk-adjustment-new (value
                           lower
                           upper
                           step-increment
                           page-increment
                           page-size)
  (make-instance 'gtk-adjustment
                 :value value
                 :lower lower
                 :upper upper
                 :step-increment step-increment
                 :page-increment page-increment
                 :page-size page-size))

(export 'gtk-adjustment-new)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_value ()
;;; 
;;; gdouble gtk_adjustment_get_value (GtkAdjustment *adjustment);
;;; 
;;; Gets the current value of the adjustment. See gtk_adjustment_set_value().
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; Returns :
;;;     The current value of the adjustment
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-value))

(defun gtk-adjustment-get-value (adjustment)
  (gtk-adjustment-value adjustment))

(export 'gtk-adjustment-get-value)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_value ()
;;; 
;;; void gtk_adjustment_set_value (GtkAdjustment *adjustment, gdouble value);
;;; 
;;; Sets the GtkAdjustment value. The value is clamped to lie between
;;; GtkAdjustment.lower and GtkAdjustment.upper.
;;; 
;;; Note that for adjustments which are used in a GtkScrollbar, the effective
;;; range of allowed values goes from GtkAdjustment.lower to GtkAdjustment.upper
;;; - GtkAdjustment.page_size.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; value :
;;;     the new value
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-value))

(defun gtk-adjustment-set-value (adjustment value)
  (setf (gtk-adjustment-value adjustment) value))

(export 'gtk-adjustment-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_clamp_page ()
;;; 
;;; void gtk_adjustment_clamp_page (GtkAdjustment *adjustment,
;;;                                 gdouble lower,
;;;                                 gdouble upper);
;;; 
;;; Updates the GtkAdjustment GtkAdjustment.value to ensure that the range
;;; between lower and upper is in the current page (i.e. between
;;; GtkAdjustment.value and GtkAdjustment.value + GtkAdjustment.page_size). If
;;; the range is larger than the page size, then only the start of it will be in
;;; the current page. A "changed" signal will be emitted if the value is
;;; changed.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; lower :
;;;     the lower value
;;; 
;;; upper :
;;;     the upper value
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_clamp_page" gtk-adjustment-clamp-page) :void
  (adjustment (g-object gtk-adjustment))
  (lower :double)
  (upper :double))

(export 'gtk-adjustment-clamp-page)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_changed ()
;;; 
;;; void gtk_adjustment_changed (GtkAdjustment *adjustment);
;;; 
;;; Emits a "changed" signal from the GtkAdjustment. This is typically called by
;;; the owner of the GtkAdjustment after it has changed any of the GtkAdjustment
;;; fields other than the value.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_changed" gtk-adjustment-changed) :void
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-adjustment-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_value_changed ()
;;; 
;;; void gtk_adjustment_value_changed (GtkAdjustment *adjustment);
;;; 
;;; Emits a "value_changed" signal from the GtkAdjustment. This is typically
;;; called by the owner of the GtkAdjustment after it has changed the
;;; GtkAdjustment value field.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_value_changed" gtk-adjustment-value-changed) :void
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-adjustment-value-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_configure ()
;;; 
;;; void gtk_adjustment_configure (GtkAdjustment *adjustment,
;;;                                gdouble value,
;;;                                gdouble lower,
;;;                                gdouble upper,
;;;                                gdouble step_increment,
;;;                                gdouble page_increment,
;;;                                gdouble page_size);
;;; 
;;; Sets all properties of the adjustment at once.
;;; 
;;; Use this function to avoid multiple emissions of the "changed" signal. See
;;; gtk_adjustment_set_lower() for an alternative way of compressing multiple
;;; emissions of "changed" into one.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; value :
;;;     the new value
;;; 
;;; lower :
;;;     the new minimum value
;;; 
;;; upper :
;;;     the new maximum value
;;; 
;;; step_increment :
;;;     the new step increment
;;; 
;;; page_increment :
;;;     the new page increment
;;; 
;;; page_size :
;;;     the new page size
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_configure" gtk-adjustment-configure) :void
  (adjustment (g-object gtk-adjustment))
  (value :double)
  (lower :double)
  (upper :double)
  (step-increment :double)
  (page-increment :double)
  (page-size :double))

(export 'gtk-adjustment-configure)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_lower ()
;;; 
;;; gdouble gtk_adjustment_get_lower (GtkAdjustment *adjustment);
;;; 
;;; Retrieves the minimum value of the adjustment.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; Returns :
;;;     The current minimum value of the adjustment
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-lower))

(defun gtk-adjustment-get-lower (adjustment)
  (gtk-adjustment-lower adjustment))

(export 'gtk-adjustment-get-lower)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_page_increment ()
;;; 
;;; gdouble gtk_adjustment_get_page_increment (GtkAdjustment *adjustment);
;;; 
;;; Retrieves the page increment of the adjustment.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; Returns :
;;;     the current page increment of the adjustment
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-page-increment))

(defun gtk-adjustment-get-page-increment (adjustment)
  (gtk-adjustment-page-increment adjustment))

(export 'gtk-adjustment-get-page-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_page_size ()
;;; 
;;; gdouble gtk_adjustment_get_page_size (GtkAdjustment *adjustment);
;;; 
;;; Retrieves the page size of the adjustment.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; Returns :
;;;     the current page size of the adjustment
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-page-size))

(defun gtk-adjustment-get-page-size (adjustment)
  (gtk-adjustment-page-size adjustment))

(export 'gtk-adjustment-get-page-size)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_step_increment ()
;;; 
;;; gdouble gtk_adjustment_get_step_increment (GtkAdjustment *adjustment);
;;; 
;;; Retrieves the step increment of the adjustment.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; Returns :
;;;     the current step increment of the adjustment
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-step-increment))

(defun gtk-adjustment-get-step-increment (adjustment)
  (gtk-adjustment-step-increment adjustment))

(export 'gtk-adjustment-get-step-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_minimum_increment ()
;;; 
;;; gdouble gtk_adjustment_get_minimum_increment (GtkAdjustment *adjustment);
;;; 
;;; Gets the smaller of step increment and page increment.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; Returns :
;;;     the minimum increment of adjustment
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-minimum-increment))

(defun gtk-adjustment-get-minimum-increment (adjustment)
  (min (gtk-adjustment-step-increment adjustment)
       (gtk-adjustment-page-increment adjustment)))

(export 'gtk-adjustment-get-minimum-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_upper ()
;;; 
;;; gdouble gtk_adjustment_get_upper (GtkAdjustment *adjustment);
;;; 
;;; Retrieves the maximum value of the adjustment.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; Returns :
;;;     the current maximum value of the adjustment
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-upper))

(defun gtk-adjustment-get-upper (adjustment)
  (gtk-adjustment-upper adjustment))

(export 'gtk-adjustment-get-upper)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_lower ()
;;; 
;;; void gtk_adjustment_set_lower (GtkAdjustment *adjustment, gdouble lower);
;;; 
;;; Sets the minimum value of the adjustment.
;;; 
;;; When setting multiple adjustment properties via their individual setters,
;;; multiple "changed" signals will be emitted. However, since the emission of
;;; the "changed" signal is tied to the emission of the "GObject::notify"
;;; signals of the changed properties, it's possible to compress the "changed"
;;; signals into one by calling g_object_freeze_notify() and
;;; g_object_thaw_notify() around the calls to the individual setters.
;;; 
;;; Alternatively, using a single g_object_set() for all the properties to
;;; change, or using gtk_adjustment_configure() has the same effect of
;;; compressing "changed" emissions.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; lower :
;;;     the new minimum value
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-lower))

(defun gtk-adjustment-set-lower (adjustment lower)
  (setf (gtk-adjustment-lower adjustment) lower))

(export 'gtk-adjustment-set-lower)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_page_increment ()
;;; 
;;; void gtk_adjustment_set_page_increment (GtkAdjustment *adjustment,
;;;                                         gdouble page_increment);
;;; 
;;; Sets the page increment of the adjustment.
;;; 
;;; See gtk_adjustment_set_lower() about how to compress multiple emissions of
;;; the "changed" signal when setting multiple adjustment properties.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; page_increment :
;;;     the new page increment
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-page-increment))

(defun gtk-adjustment-set-page-increment (adjustment page-increment)
  (setf (gtk-adjustment-page-increment adjustment) page-increment))

(export 'gtk-adjustment-set-page-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_page_size ()
;;; 
;;; void gtk_adjustment_set_page_size (GtkAdjustment *adjustment,
;;;                                    gdouble page_size);
;;; 
;;; Sets the page size of the adjustment.
;;; 
;;; See gtk_adjustment_set_lower() about how to compress multiple emissions of
;;; the "changed" signal when setting multiple adjustment properties.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; page_size :
;;;     the new page size
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-page-size))

(defun gtk-adjustment-set-page-size (adjustment page-size)
  (setf (gtk-adjustment-page-size adjustment) page-size))

(export 'gtk-adjustment-set-page-size)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_step_increment ()
;;; 
;;; void gtk_adjustment_set_step_increment (GtkAdjustment *adjustment,
;;;                                         gdouble step_increment);
;;; 
;;; Sets the step increment of the adjustment.
;;; 
;;; See gtk_adjustment_set_lower() about how to compress multiple emissions of
;;; the "changed" signal when setting multiple adjustment properties.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; step_increment :
;;;     the new step increment
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-step-increment))

(defun gtk-adjustment-set-step-increment (adjustment step-increment)
  (setf (gtk-adjustment-step-increment adjustment) step-increment))

(export 'gtk-adjustment-set-step-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_upper ()
;;; 
;;; void gtk_adjustment_set_upper (GtkAdjustment *adjustment, gdouble upper);
;;; 
;;; Sets the maximum value of the adjustment.
;;; 
;;; Note that values will be restricted by upper - page-size if the page-size
;;; property is nonzero.
;;; 
;;; See gtk_adjustment_set_lower() about how to compress multiple emissions of
;;; the "changed" signal when setting multiple adjustment properties.
;;; 
;;; adjustment :
;;;     a GtkAdjustment
;;; 
;;; upper :
;;;     the new maximum value
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustent-set-upper))

(defun gtk-adjustment-set-upper (adjustment upper)
  (setf (gtk-adjustment-upper adjustment) upper))

(export 'gtk-adjustment-set-upper)

;;; --- End of file gtk.adjustment.lisp ----------------------------------------
