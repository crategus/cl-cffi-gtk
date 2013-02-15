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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAdjustment
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-adjustment 'type)
 "@version{2013-2-10}
  @begin{short}
    The GtkAdjustment object represents a value which has an associated lower
    and upper bound, together with step and page increments, and a page size. It
    is used within several GTK+ widgets, including GtkSpinButton, GtkViewport,
    and GtkRange (which is a base class for GtkHScrollbar, GtkVScrollbar,
    GtkHScale, and GtkVScale).
  @end{short}

  The GtkAdjustment object does not update the value itself. Instead it is
  left up to the owner of the GtkAdjustment to control the value.

  The owner of the GtkAdjustment typically calls the
  gtk_adjustment_value_changed() and gtk_adjustment_changed() functions after
  changing the value and its bounds. This results in the emission of the
  \"value_changed\" or \"changed\" signal respectively.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      Emitted when one or more of the GtkAdjustment fields have been changed,
      other than the value field.
      @begin{pre}
 void user_function (GtkAdjustment *adjustment,
                     gpointer       user_data)       : No Recursion
      @end{pre}
      @begin[code]{table}
        @entry[adjustment]{the object which received the signal}
        @entry[user_data]{user data set when the signal handler was connected}
      @end{table}
    @subheading{The \"value-changed\" signal}
      Emitted when the GtkAdjustment value field has been changed.
      @begin{pre}
 void user_function (GtkAdjustment *adjustment,
                     gpointer       user_data)       : No Recursion
      @end{pre}
      @begin[code]{table}
        @entry[adjustment]{the object which received the signal.}
        @entry[user_data]{user data set when the signal handler was connected}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-adjustment-lower}
  @see-slot{gtk-adjustment-page-increment}
  @see-slot{gtk-adjustment-page-size}
  @see-slot{gtk-adjustment-step-increment}
  @see-slot{gtk-adjustment-upper}
  @see-slot{gtk-adjustment-value}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "lower" 'gtk-adjustment) 't)
 "The @code{\"lower\"} property of type @code{gdouble} (Read / Write)@br{}
  The minimum value of the adjustment.@br{}
  Default value: @code{0}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page-increment" 'gtk-adjustment) 't)
 "The @code{\"page-increment\"} property of type @code{gdouble}
  (Read / Write)@br{}
  The page increment of the adjustment.@br{}
  Default value: @code{0}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page-size" 'gtk-adjustment) 't)
 "The @code{\"page-size\"} property of type @code{gdouble} (Read / Write)@br{}
  The page size of the adjustment. Note that the page-size is irrelevant and
  should be set to zero if the adjustment is used for a simple scalar value,
  e.g. in a GtkSpinButton.@br{}
  Default value: @code{0}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "step-increment" 'gtk-adjustment) 't)
 "The @code{\"step-increment\"} property of type @code{gdouble}
  (Read / Write)@br{}
  The step increment of the adjustment.@br{}
  Default value: @code{0}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "upper" 'gtk-adjustment) 't)
 "The @code{\"upper\"} property of type @code{gdouble} (Read / Write)@br{}
  The maximum value of the adjustment. Note that values will be restricted by
  upper - page-size if the page-size property is nonzero.@br{}
  Default value: @code{0}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value" 'gtk-adjustment) 't)
 "The @code{\"value\"} property of type @code{gdouble} (Read / Write)@br{}
  The value of the adjustment.@br{}
  Default value: @code{0}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-adjustment-lower ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-lower atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-lower 'function)
 "@version{2013-2-10}
  @begin{short}
    Accessor of the slot @code{\"lower\"} of the @class{gtk-adjustment} class.
  @end{short}")

;;; --- gtk-adjustment-page-increment ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-page-increment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-page-increment 'function)
 "@version{2013-2-10}
  @begin{short}
    Accessor of the slot @code{\"page-increment\"} of the @class{gtk-adjustment}
    class.
  @end{short}")

;;; --- gtk-adjustment-page-size -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-page-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-page-size 'function)
 "@version{2013-2-10}
  @begin{short}
    Accessor of the slot @code{\"page-size\"} of the @class{gtk-adjustment}
    class.
  @end{short}")

;;; --- gtk-adjustment-step-increment ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-step-increment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-step-increment 'function)
 "@version{2013-2-10}
  @begin{short}
    Accessor of the slot @code{\"step-increment\"} of the @class{gtk-adjustment}
    class.
  @end{short}")

;;; --- gtk-adjustment-upper ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-upper atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-upper 'function)
 "@version{2013-2-10}
  @begin{short}
    Accessor of the slot @code{\"upper\"} of the @class{gtk-adjustment} class.
  @end{short}")

;;; --- gtk-adjustment-value ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-value 'function)
 "@version{2013-2-10}
  @begin{short}
    Accessor of the slot @code{\"value\"} of the @class{gtk-adjustment} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-adjustment-new (value
                           lower
                           upper
                           step-increment
                           page-increment
                           page-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[value]{the initial value}
  @argument[lower]{the minimum value}
  @argument[upper]{the maximum value}
  @argument[step-increment]{the step increment}
  @argument[page-increment]{the page increment}
  @argument[page-size]{the page size}
  @return{a new GtkAdjustment}
  @short{Creates a new GtkAdjustment.}"
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
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-value))

(defun gtk-adjustment-get-value (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-19}
  @argument[adjustment]{a GtkAdjustment}
  @return{The current value of the adjustment.}
  @short{Gets the current value of the adjustment.}
  See gtk_adjustment_set_value()."
  (gtk-adjustment-value adjustment))

(export 'gtk-adjustment-get-value)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_value ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-value))

(defun gtk-adjustment-set-value (adjustment value)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @argument[value]{the new value}
  @begin{short}
    Sets the GtkAdjustment value. The value is clamped to lie between
    GtkAdjustment.lower and GtkAdjustment.upper.
  @end{short}

  Note that for adjustments which are used in a GtkScrollbar, the effective
  range of allowed values goes from GtkAdjustment.lower to GtkAdjustment.upper
  - GtkAdjustment.page_size."
  (setf (gtk-adjustment-value adjustment) value))

(export 'gtk-adjustment-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_clamp_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_clamp_page" gtk-adjustment-clamp-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @argument[lower]{the lower value}
  @argument[upper]{the upper value}
  @begin{short}
    Updates the GtkAdjustment GtkAdjustment.value to ensure that the range
    between lower and upper is in the current page (i.e. between
    GtkAdjustment.value and GtkAdjustment.value + GtkAdjustment.page_size).
  @end{short}
  If the range is larger than the page size, then only the start of it will be
  in the current page. A \"changed\" signal will be emitted if the value is
  changed."
  (adjustment (g-object gtk-adjustment))
  (lower :double)
  (upper :double))

(export 'gtk-adjustment-clamp-page)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_changed" gtk-adjustment-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @begin{short}
    Emits a \"changed\" signal from the GtkAdjustment. This is typically called
    by the owner of the GtkAdjustment after it has changed any of the
    GtkAdjustment fields other than the value.
  @end{short}"
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-adjustment-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_value_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_value_changed" gtk-adjustment-value-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @begin{short}
    Emits a \"value_changed\" signal from the GtkAdjustment. This is typically
    called by the owner of the GtkAdjustment after it has changed the
    GtkAdjustment value field.
  @end{short}"
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-adjustment-value-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_configure ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_configure" gtk-adjustment-configure) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10} 
  @argument[adjustment]{a GtkAdjustment}
  @argument[value]{the new value}
  @argument[lower]{the new minimum value}
  @argument[upper]{the new maximum value}
  @argument[step_increment]{the new step increment}
  @argument[page_increment]{the new page increment}
  @argument[page_size]{the new page size}
  @begin{short}
    Sets all properties of the adjustment at once.
  @end{short}
  Use this function to avoid multiple emissions of the \"changed\" signal. See
  gtk_adjustment_set_lower() for an alternative way of compressing multiple
  emissions of \"changed\" into one.

  Since 2.14"
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
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-lower))

(defun gtk-adjustment-get-lower (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @return{The current minimum value of the adjustment.}
  @short{Retrieves the minimum value of the adjustment.}@break{}
  Since 2.14"
  (gtk-adjustment-lower adjustment))

(export 'gtk-adjustment-get-lower)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_page_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-page-increment))

(defun gtk-adjustment-get-page-increment (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @return{The current page increment of the adjustment.}
  @short{Retrieves the page increment of the adjustment.}@break{}
  Since 2.14"
  (gtk-adjustment-page-increment adjustment))

(export 'gtk-adjustment-get-page-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_page_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-page-size))

(defun gtk-adjustment-get-page-size (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @return{The current page size of the adjustment.}
  @short{Retrieves the page size of the adjustment.}@break{}
  Since 2.14"
  (gtk-adjustment-page-size adjustment))

(export 'gtk-adjustment-get-page-size)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_step_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-step-increment))

(defun gtk-adjustment-get-step-increment (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @return{The current step increment of the adjustment.}
  @short{Retrieves the step increment of the adjustment.}@break{}
  Since 2.14"
  (gtk-adjustment-step-increment adjustment))

(export 'gtk-adjustment-get-step-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_minimum_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-minimum-increment))

(defun gtk-adjustment-get-minimum-increment (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @return{The minimum increment of adjustment.}
  @short{Gets the smaller of step increment and page increment.}@break{}
  Since 3.2"
  (min (gtk-adjustment-step-increment adjustment)
       (gtk-adjustment-page-increment adjustment)))

(export 'gtk-adjustment-get-minimum-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_upper ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-upper))

(defun gtk-adjustment-get-upper (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @return{The current maximum value of the adjustment.}
  @short{Retrieves the maximum value of the adjustment.}@break{}
  Since 2.14"
  (gtk-adjustment-upper adjustment))

(export 'gtk-adjustment-get-upper)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_lower ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-lower))

(defun gtk-adjustment-set-lower (adjustment lower)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @argument[lower]{the new minimum value}
  @begin{short}
    Sets the minimum value of the adjustment.
  @end{short}

  When setting multiple adjustment properties via their individual setters,
  multiple \"changed\" signals will be emitted. However, since the emission of
  the \"changed\" signal is tied to the emission of the \"GObject::notify\"
  signals of the changed properties, it's possible to compress the \"changed\"
  signals into one by calling g_object_freeze_notify() and
  g_object_thaw_notify() around the calls to the individual setters.

  Alternatively, using a single g_object_set() for all the properties to
  change, or using gtk_adjustment_configure() has the same effect of
  compressing \"changed\" emissions.

  Since 2.14"
  (setf (gtk-adjustment-lower adjustment) lower))

(export 'gtk-adjustment-set-lower)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_page_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-page-increment))

(defun gtk-adjustment-set-page-increment (adjustment page-increment)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @argument[page_increment]{the new page increment}
  @begin{short}
    Sets the page increment of the adjustment.
  @end{short}

  See gtk_adjustment_set_lower() about how to compress multiple emissions of
  the \"changed\" signal when setting multiple adjustment properties.

  Since 2.14"
  (setf (gtk-adjustment-page-increment adjustment) page-increment))

(export 'gtk-adjustment-set-page-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_page_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-page-size))

(defun gtk-adjustment-set-page-size (adjustment page-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @argument[page_size]{the new page size}
  @begin{short}
    Sets the page size of the adjustment.
  @end{short}

  See gtk_adjustment_set_lower() about how to compress multiple emissions of
  the \"changed\" signal when setting multiple adjustment properties.

  Since 2.14"
  (setf (gtk-adjustment-page-size adjustment) page-size))

(export 'gtk-adjustment-set-page-size)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_step_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-step-increment))

(defun gtk-adjustment-set-step-increment (adjustment step-increment)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @argument[step_increment]{the new step increment}
  @begin{short}
    Sets the step increment of the adjustment.
  @end{short}

  See gtk_adjustment_set_lower() about how to compress multiple emissions of
  the \"changed\" signal when setting multiple adjustment properties.

  Since 2.14"
  (setf (gtk-adjustment-step-increment adjustment) step-increment))

(export 'gtk-adjustment-set-step-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_upper ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustent-set-upper))

(defun gtk-adjustment-set-upper (adjustment upper)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-10}
  @argument[adjustment]{a GtkAdjustment}
  @argument[upper]{the new maximum value}
  @begin{short}
    Sets the maximum value of the adjustment.
  @end{short}

  Note that values will be restricted by upper - page-size if the page-size
  property is nonzero.

  See gtk_adjustment_set_lower() about how to compress multiple emissions of
  the \"changed\" signal when setting multiple adjustment properties.

  Since 2.14"
  (setf (gtk-adjustment-upper adjustment) upper))

(export 'gtk-adjustment-set-upper)

;;; --- End of file gtk.adjustment.lisp ----------------------------------------
