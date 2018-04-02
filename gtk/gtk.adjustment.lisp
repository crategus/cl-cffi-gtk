;;; ----------------------------------------------------------------------------
;;; gtk.adjustment.lisp
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
 "@version{2013-5-23}
  @begin{short}
    The @sym{gtk-adjustment} object represents a value which has an associated
    lower and upper bound, together with step and page increments, and a page
    size. It is used within several GTK+ widgets, including
    @class{gtk-spin-button}, @class{gtk-viewport}, and @class{gtk-range} (which
    is a base class for @class{gtk-hscrollbar}, @class{gtk-vscrollbar},
    @class{gtk-hscale}, and @class{gtk-vscale}).
  @end{short}

  The @sym{gtk-adjustment} object does not update the value itself. Instead it
  is left up to the owner of the @sym{gtk-adjustment} to control the value.

  The owner of the @sym{gtk-adjustment} typically calls the the functions
  @fun{gtk-adjustment-value-changed} and @sym{gtk-adjustment-changed} functions
  after changing the value and its bounds. This results in the emission of the
  \"value_changed\" or \"changed\" signal respectively.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (adjustment)   : No Recursion
      @end{pre}
      Emitted when one or more of the @sym{gtk-adjustment} fields have been
      changed, other than the value field.
      @begin[code]{table}
        @entry[adjustment]{The object which received the signal.}
      @end{table}
    @subheading{The \"value-changed\" signal}
      @begin{pre}
 lambda (adjustment)   : No Recursion
      @end{pre}
      Emitted when the @sym{gtk-adjustment} value field has been changed.
      @begin[code]{table}
        @entry[adjustment]{The object which received the signal.}
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
 "The @code{\"lower\"} property of type @code{:double} (Read / Write) @br{}
  The minimum value of the adjustment. @br{}
  Default value: 0 @br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page-increment" 'gtk-adjustment) 't)
 "The @code{\"page-increment\"} property of type @code{:double}
  (Read / Write) @br{}
  The page increment of the adjustment. @br{}
  Default value: 0 @br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page-size" 'gtk-adjustment) 't)
 "The @code{\"page-size\"} property of type @code{:double} (Read / Write) @br{}
  The page size of the adjustment. Note that the @arg{page-size} is irrelevant
  and should be set to zero if the adjustment is used for a simple scalar value,
  e. g. in a @class{gtk-spin-button}. @br{}
  Default value: 0 @br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "step-increment" 'gtk-adjustment) 't)
 "The @code{\"step-increment\"} property of type @code{:double}
  (Read / Write) @br{}
  The step increment of the adjustment. @br{}
  Default value: 0 @br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "upper" 'gtk-adjustment) 't)
 "The @code{\"upper\"} property of type @code{:double} (Read / Write) @br{}
  The maximum value of the adjustment. Note that values will be restricted by
  @arg{upper} - @arg{page-size} if the @arg{page-size} property is nonzero.@br{}
  Default value: 0 @br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value" 'gtk-adjustment) 't)
 "The @code{\"value\"} property of type @code{:double} (Read / Write) @br{}
  The value of the adjustment. @br{}
  Default value: 0 @br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
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
 "@version{2013-5-23}
  @argument[value]{the initial value}
  @argument[lower]{the minimum value}
  @argument[upper]{the maximum value}
  @argument[step-increment]{the step increment}
  @argument[page-increment]{the page increment}
  @argument[page-size]{the page size}
  @return{A new @class{gtk-adjustment} object.}
  @short{Creates a new @class{gtk-adjustment} object.}"
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
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @return{The current value of the adjustment.}
  @short{Gets the current value of the adjustment.}
  See the function @fun{gtk-adjustment-set-value}.
  @see-function{gtk-adjustment-set-value}"
  (gtk-adjustment-value adjustment))

(export 'gtk-adjustment-get-value)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_value ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-value))

(defun gtk-adjustment-set-value (adjustment value)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @argument[value]{the new value}
  @begin{short}
    Sets the @class{gtk-adjustment} value. The value is clamped to lie between
    @code{lower} and @code{upper}.
  @end{short}

  Note that for adjustments which are used in a @class{gtk-scrollbar}, the
  effective range of allowed values goes from @code{lower} to @code{upper}
  - @code{page-size}."
  (setf (gtk-adjustment-value adjustment) value))

(export 'gtk-adjustment-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_clamp_page ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_clamp_page" gtk-adjustment-clamp-page) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @argument[lower]{the lower value}
  @argument[upper]{the upper value}
  @begin{short}
    Updates the @class{gtk-adjustment} @code{value} to ensure that the range
    between @code{lower} and @code{upper} is in the current page (i. e. between
    @code{value} and @code{value} + @code{page-size}).
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
 "@version{2013-6-4}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  Emits a \"changed\" signal from the @class{gtk-adjustment}. This is
  typically called by the owner of the @class{gtk-adjustment} after it has
  changed any of the @class{gtk-adjustment} fields other than the value."
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-adjustment-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_value_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_value_changed" gtk-adjustment-value-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @begin{short}
    Emits a \"value_changed\" signal from the @class{gtk-adjustment} object.
  @end{short}
  This is typically called by the owner of the @class{gtk-adjustment} object
  after it has changed the @class{gtk-adjustment} value field."
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-adjustment-value-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_configure ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_configure" gtk-adjustment-configure) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @argument[value]{the new value}
  @argument[lower]{the new minimum value}
  @argument[upper]{the new maximum value}
  @argument[step-increment]{the new step increment}
  @argument[page-increment]{the new page increment}
  @argument[page-size]{the new page size}
  @begin{short}
    Sets all properties of the adjustment at once.
  @end{short}
  Use this function to avoid multiple emissions of the \"changed\" signal. See
  the function @fun{gtk-adjustment-set-lower} for an alternative way of
  compressing multiple emissions of \"changed\" into one.

  Since 2.14
  @see-function{gtk-adjustment-set-lower}"
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
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @return{The current minimum value of the adjustment.}
  @short{Retrieves the minimum value of the adjustment.}

  Since 2.14"
  (gtk-adjustment-lower adjustment))

(export 'gtk-adjustment-get-lower)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_page_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-page-increment))

(defun gtk-adjustment-get-page-increment (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @return{The current page increment of the adjustment.}
  @short{Retrieves the page increment of the adjustment.}

  Since 2.14"
  (gtk-adjustment-page-increment adjustment))

(export 'gtk-adjustment-get-page-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_page_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-page-size))

(defun gtk-adjustment-get-page-size (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @return{The current page size of the adjustment.}
  @short{Retrieves the page size of the adjustment.}

  Since 2.14"
  (gtk-adjustment-page-size adjustment))

(export 'gtk-adjustment-get-page-size)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_step_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-step-increment))

(defun gtk-adjustment-get-step-increment (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @return{The current step increment of the adjustment.}
  @short{Retrieves the step increment of the adjustment.}

  Since 2.14"
  (gtk-adjustment-step-increment adjustment))

(export 'gtk-adjustment-get-step-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_minimum_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-minimum-increment))

(defun gtk-adjustment-get-minimum-increment (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @return{The minimum increment of adjustment.}
  @short{Gets the smaller of step increment and page increment.}

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
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @return{The current maximum value of the adjustment.}
  @short{Retrieves the maximum value of the adjustment.}

  Since 2.14"
  (gtk-adjustment-upper adjustment))

(export 'gtk-adjustment-get-upper)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_lower ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-lower))

(defun gtk-adjustment-set-lower (adjustment lower)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @argument[lower]{the new minimum value}
  @begin{short}
    Sets the minimum value of the adjustment.
  @end{short}

  When setting multiple adjustment properties via their individual setters,
  multiple \"changed\" signals will be emitted. However, since the emission of
  the \"changed\" signal is tied to the emission of the \"notify\" signals of
  the changed properties, it is possible to compress the \"changed\"
  signals into one by calling the functions @fun{g-object-freeze-notify} and
  @fun{g-object-thaw-notify} around the calls to the individual setters.

  Alternatively, using a single @fun{g-object-set} for all the properties to
  change, or using the function @fun{gtk-adjustment-configure} has the same
  effect of compressing \"changed\" emissions.

  Since 2.14
  @see-function{g-object-freeze-notify}
  @see-function{g-object-thaw-notify}
  @see-function{g-object-set}"
  (setf (gtk-adjustment-lower adjustment) lower))

(export 'gtk-adjustment-set-lower)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_page_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-page-increment))

(defun gtk-adjustment-set-page-increment (adjustment page-increment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @argument[page-increment]{the new page increment}
  @begin{short}
    Sets the page increment of the adjustment.
  @end{short}

  See the functions @fun{gtk-adjustment-set-lower} about how to compress
  multiple emissions of the \"changed\" signal when setting multiple adjustment
  properties.

  Since 2.14
  @see-function{gtk-adjustment-set-lower}"
  (setf (gtk-adjustment-page-increment adjustment) page-increment))

(export 'gtk-adjustment-set-page-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_page_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-page-size))

(defun gtk-adjustment-set-page-size (adjustment page-size)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @argument[page-size]{the new page size}
  @begin{short}
    Sets the page size of the adjustment.
  @end{short}

  See the function @fun{gtk-adjustment-set-lower} about how to compress multiple
  emissions of the \"changed\" signal when setting multiple adjustment
  properties.

  Since 2.14
  @see-function{gtk-adjustment-set-lower}"
  (setf (gtk-adjustment-page-size adjustment) page-size))

(export 'gtk-adjustment-set-page-size)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_step_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-set-step-increment))

(defun gtk-adjustment-set-step-increment (adjustment step-increment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @argument[step-increment]{the new step increment}
  @begin{short}
    Sets the step increment of the adjustment.
  @end{short}

  See the function @fun{gtk-adjustment-set-lower} about how to compress multiple
  emissions of the \"changed\" signal when setting multiple adjustment
  properties.

  Since 2.14
  @see-function{gtk-adjustment-set-lower}"
  (setf (gtk-adjustment-step-increment adjustment) step-increment))

(export 'gtk-adjustment-set-step-increment)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_set_upper ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustent-set-upper))

(defun gtk-adjustment-set-upper (adjustment upper)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @argument[upper]{the new maximum value}
  @begin{short}
    Sets the maximum value of the adjustment.
  @end{short}

  Note that values will be restricted by @code{upper} - @code{page-size} if the
  @code{page-size} property is nonzero.

  See the function @fun{gtk-adjustment-set-lower} about how to compress multiple
  emissions of the \"changed\" signal when setting multiple adjustment
  properties.

  Since 2.14
  @see-function{gtk-adjustment-set-lower}"
  (setf (gtk-adjustment-upper adjustment) upper))

(export 'gtk-adjustment-set-upper)

;;; --- End of file gtk.adjustment.lisp ----------------------------------------
