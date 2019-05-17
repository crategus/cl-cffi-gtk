;;; ----------------------------------------------------------------------------
;;; gtk.adjustment.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     A representation of an adjustable bounded value
;;;
;;; Types and values
;;;
;;;     GtkAdjustment
;;;
;;; Functions
;;;
;;;     gtk_adjustment_new
;;;     gtk_adjustment_get_value                           Accessor
;;;     gtk_adjustment_set_value                           Accessor
;;;     gtk_adjustment_clamp_page
;;;     gtk_adjustment_changed                           * deprecated
;;;     gtk_adjustment_value_changed                     * deprecated
;;;     gtk_adjustment_configure
;;;     gtk_adjustment_get_lower                           Accessor
;;;     gtk_adjustment_get_page_increment                  Accessor
;;;     gtk_adjustment_get_page_size                       Accessor
;;;     gtk_adjustment_get_step_increment                  Accessor
;;;     gtk_adjustment_get_minimum_increment
;;;     gtk_adjustment_get_upper                           Accessor
;;;     gtk_adjustment_set_lower                           Accessor
;;;     gtk_adjustment_set_page_increment                  Accessor
;;;     gtk_adjustment_set_page_size                       Accessor
;;;     gtk_adjustment_set_step_increment                  Accessor
;;;     gtk_adjustment_set_upper                           Accessor
;;;
;;; Properties
;;;
;;;     gdouble   lower             Read / Write
;;;     gdouble   page-increment    Read / Write
;;;     gdouble   page-size         Read / Write
;;;     gdouble   step-increment    Read / Write
;;;     gdouble   upper             Read / Write
;;;     gdouble   value             Read / Write
;;;
;;; Signals
;;;
;;;        void   changed           No Recursion
;;;        void   value-changed     No Recursion
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkAdjustment
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-adjustment 'type)
 "@version{2019-3-4}
  @begin{short}
    The @sym{gtk-adjustment} object represents a value which has an associated
    lower and upper bound, together with step and page increments, and a page
    size.
  @end{short}
  It is used within several GTK+ widgets, including @class{gtk-spin-button},
  @class{gtk-viewport}, and @class{gtk-range}, which is a base class for
  @class{gtk-scrollbar}, and @class{gtk-scale}.

  The @sym{gtk-adjustment} object does not update the value itself. Instead it
  is left up to the owner of the @sym{gtk-adjustment} to control the value.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (adjustment)    : No Recursion
      @end{pre}
      Emitted when one or more of the @sym{gtk-adjustment} fields have been
      changed, other than the value field.
      @begin[code]{table}
        @entry[adjustment]{The @sym{gtk-adjustment} object which received the
          signal.}
      @end{table}
    @subheading{The \"value-changed\" signal}
      @begin{pre}
 lambda (adjustment)    : No Recursion
      @end{pre}
      Emitted when the @sym{gtk-adjustment} value field has been changed.
      @begin[code]{table}
        @entry[adjustment]{The @sym{gtk-adjustment} object which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-adjustment-lower}
  @see-slot{gtk-adjustment-page-increment}
  @see-slot{gtk-adjustment-page-size}
  @see-slot{gtk-adjustment-step-increment}
  @see-slot{gtk-adjustment-upper}
  @see-slot{gtk-adjustment-value}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-adjustment-lower ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "lower" 'gtk-adjustment) 't)
 "The @code{lower} property of type @code{:double} (Read / Write) @br{}
  The minimum value of the adjustment. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-lower atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-lower 'function)
 "@version{2019-5-17}
  @syntax[]{(gtk-adjustment-lower object) => lower}
  @syntax[]{(setf (gtk-adjustment-lower object) lower)}
  @argument[object]{a @class{gtk-adjustment} object}
  @argument[lower]{the new minimum value}
  @begin{short}
    Accessor of the @slot[gtk-adjustment]{lower} slot of the
    @class{gtk-adjustment} class.
  @end{short}

  The @sym{gtk-adjustment-lower} slot access function
  retrieves the minimum value of the adjustment.

  The @sym{(setf gtk-adjustment-lower)} slot access function
  sets the minimum value of the adjustment.

  When setting multiple adjustment properties via their individual setters,
  multiple \"changed\" signals will be emitted. However, since the emission of
  the \"changed\" signal is tied to the emission of the \"notify\" signals of
  the changed properties, it is possible to compress the \"changed\"
  signals into one by calling the @fun{g-object-freeze-notify} and
  @fun{g-object-thaw-notify} functions around the calls to the individual
  setters.

  Alternatively, using a single @fun{g-object-set} for all the properties to
  change, or using the @fun{gtk-adjustment-configure} function has the same
  effect of compressing \"changed\" emissions.
  @see-class{gtk-adjustment}")

;;; --- gtk-adjustment-page-increment ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page-increment" 'gtk-adjustment)
     't)
 "The @code{page-increment} property of type @code{:double}
  (Read / Write) @br{}
  The page increment of the adjustment. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-page-increment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-page-increment 'function)
 "@version{2019-5-17}
  @syntax[]{(gtk-adjustment-page-increment object) => page-increment}
  @syntax[]{(setf (gtk-adjustment-page-increment object) page-increment)}
  @argument[object]{a @class{gtk-adjustment} object}
  @argument[page-increment]{the new page increment}
  @begin{short}
    Accessor of the @slot[gtk-adjustment]{page-increment} slot of the
    @class{gtk-adjustment} class.
  @end{short}

  The @sym{gtk-adjustment-page-increment} slot access function
  retrieves the page increment of the adjustment.

  The @sym{(setf gtk-adjustment-page-increment)} slot access function
  sets the page increment of the adjustment.

  See the @fun{gtk-adjustment-lower} slot access function about how to compress
  multiple emissions of the \"changed\" signal when setting multiple adjustment
  properties.
  @see-class{gtk-adjustment}")

;;; --- gtk-adjustment-page-size -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "page-size" 'gtk-adjustment) 't)
 "The @code{page-size} property of type @code{:double} (Read / Write) @br{}
  The page size of the adjustment. Note that the @code{page-size} is irrelevant
  and should be set to zero if the adjustment is used for a simple scalar value,
  e. g. in a @class{gtk-spin-button}. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-page-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-page-size 'function)
 "@version{2019-5-17}
  @syntax[]{(gtk-adjustment-page-size object) => page-size}
  @syntax[]{(setf (gtk-adjustment-page-size object) page-size)}
  @argument[object]{a @class{gtk-adjustment} object}
  @argument[page-size]{the new page size}
  @begin{short}
    Accessor of the @slot[gtk-adjustment]{page-size} slot of the
    @class{gtk-adjustment} class.
  @end{short}

  The @sym{gtk-adjustment-page-size} slot access function
  retrieves the page size of the adjustment.

  The @sym{(setf gtk-adjustment-page-size)} slot access function
  sets the page size of the adjustment.

  See the @fun{gtk-adjustment-lower} slot access function about how to compress
  multiple emissions of the \"changed\" signal when setting multiple adjustment
  properties.
  @see-class{gtk-adjustment}")

;;; --- gtk-adjustment-step-increment ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "step-increment" 'gtk-adjustment)
      't)
 "The @code{step-increment} property of type @code{:double} (Read / Write) @br{}
  The step increment of the adjustment. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-step-increment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-step-increment 'function)
 "@version{2019-5-17}
  @syntax[]{(gtk-adjustment-step-increment object) => step-increment}
  @syntax[]{(setf (gtk-adjustment-step-increment object) step-increment)}
  @argument[object]{a @class{gtk-adjustment} object}
  @argument[step-increment]{the new step increment}
  @begin{short}
    Accessor of the @slot[gtk-adjustment]{step-increment} of the
    @class{gtk-adjustment} class.
  @end{short}

  The @sym{gtk-adjustment-step-increment} slot access function
  retrieves the step increment of the adjustment.

  The @sym{(setf gtk-adjustment-step-increment)} slot access function
  sets the step increment of the adjustment.

  See the @fun{gtk-adjustment-lower} slot access function about how to compress
  multiple emissions of the \"changed\" signal when setting multiple adjustment
  properties.
  @see-class{gtk-adjustment}")

;;; --- gtk-adjustment-upper ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "upper" 'gtk-adjustment) 't)
 "The @code{upper} property of type @code{:double} (Read / Write) @br{}
  The maximum value of the adjustment. Note that values will be restricted by
  @code{upper} - @code{page-size} if the @code{page-size} property is nonzero.
  @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-upper atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-upper 'function)
 "@version{2019-5-17}
  @syntax[]{(gtk-adjustment-upper object) => upper}
  @syntax[]{(setf (gtk-adjustment-upper object) upper)}
  @argument[object]{a @class{gtk-adjustment} object}
  @argument[upper]{the new maximum value}
  @begin{short}
    Accessor of the @slot[gtk-adjustment]{upper} slot of the
    @class{gtk-adjustment} class.
  @end{short}

  The @sym{gtk-adjustment-upper} slot access function
  retrieves the maximum value of the adjustment.

  The @sym{(setf gtk-adjustment-upper)} slot access function
  sets the maximum value of the adjustment.

  Note that values will be restricted by @code{upper} - @code{page-size} if the
  @code{page-size} property is nonzero.

  See the @fun{gtk-adjustment-lower} slot access function about how to compress
  multiple emissions of the \"changed\" signal when setting multiple adjustment
  properties.
  @see-class{gtk-adjustment}")

;;; --- gtk-adjustment-value ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value" 'gtk-adjustment) 't)
 "The @code{value} property of type @code{:double} (Read / Write) @br{}
  The value of the adjustment. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-adjustment-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-adjustment-value 'function)
 "@version{2019-5-17}
  @syntax[]{(gtk-adjustment-value object) => value}
  @syntax[]{(setf (gtk-adjustment-value object) value)}
  @argument[object]{a @class{gtk-adjustment} object}
  @argument[value]{the new value}
  @begin{short}
    Accessor of the @slot[gtk-adjustment]{value} slot of the
    @class{gtk-adjustment} class.
  @end{short}

  The @sym{gtk-adjustment-value} slot access function
  gets the current value of the adjustment.

  The @sym{(setf gtk-adjustment-value)} slot access function
  sets the @class{gtk-adjustment} value. The value is clamped to lie between
  @code{lower} and @code{upper}.

  Note that for adjustments which are used in a @class{gtk-scrollbar}, the
  effective range of allowed values goes from @code{lower} to @code{upper}
  - @code{page-size}.
  @see-class{gtk-adjustment}")

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
  @short{Creates a new @class{gtk-adjustment} object.}
  @see-class{gtk-adjustment}"
  (make-instance 'gtk-adjustment
                 :value value
                 :lower lower
                 :upper upper
                 :step-increment step-increment
                 :page-increment page-increment
                 :page-size page-size))

(export 'gtk-adjustment-new)

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
  changed.
  @see-class{gtk-adjustment}"
  (adjustment (g-object gtk-adjustment))
  (lower :double)
  (upper :double))

(export 'gtk-adjustment-clamp-page)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_changed" gtk-adjustment-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2019-3-4}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  Emits a \"changed\" signal from the @class{gtk-adjustment}. This is
  typically called by the owner of the @class{gtk-adjustment} after it has
  changed any of the @class{gtk-adjustment} fields other than the value.
  @begin[Warning]{dictionary}
    The @sym{gtk-adjustment-changed} function has been deprecated since version
    3.18 and should not be used in newly-written code. GTK+ emits the
    \“changed\” signal itself whenever any of the properties (other than value)
    change.
  @end{dictionary}
  @see-class{gtk-adjustment}"
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-adjustment-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_value_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_adjustment_value_changed" gtk-adjustment-value-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2019-3-4}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @begin{short}
    Emits a \"value_changed\" signal from the @class{gtk-adjustment} object.
  @end{short}
  This is typically called by the owner of the @class{gtk-adjustment} object
  after it has changed the @class{gtk-adjustment} value field.
  @begin[Warning]{dictionary}
    The @sym{gtk-adjustment-value-changed} function has been deprecated since
    version 3.18 and should not be used in newly-written code. GTK+ emits the
    \“value-changed\” signal itself whenever any of the properties (other than
    value) change.
  @end{dictionary}
  @see-class{gtk-adjustment}"
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
  the @fun{gtk-adjustment-lower} function for an alternative way of compressing
  multiple emissions of \"changed\" signals into one.
  @see-class{gtk-adjustment}
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
;;; gtk_adjustment_get_minimum_increment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-adjustment-get-minimum-increment))

(defun gtk-adjustment-get-minimum-increment (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-23}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @return{The minimum increment of adjustment.}
  @short{Gets the smaller of step increment and page increment.}
  @see-class{gtk-adjustment}"
  (min (gtk-adjustment-step-increment adjustment)
       (gtk-adjustment-page-increment adjustment)))

(export 'gtk-adjustment-get-minimum-increment)

;;; --- End of file gtk.adjustment.lisp ----------------------------------------
