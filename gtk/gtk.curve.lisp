;;; ----------------------------------------------------------------------------
;;; gtk.curve.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkCurve
;;; 
;;; Allows direct editing of a curve
;;; 
;;; Synopsis
;;; 
;;;     GtkCurve
;;;
;;;     gtk_curve_new
;;;     gtk_curve_reset
;;;     gtk_curve_set_gamma
;;;     gtk_curve_set_range
;;;     gtk_curve_get_vector
;;;     gtk_curve_set_vector
;;;     gtk_curve_set_curve_type
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkWidget
;;;                      +----GtkDrawingArea
;;;                            +----GtkCurve
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkCurve implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "curve-type"               GtkCurveType          : Read / Write
;;;   "max-x"                    gfloat                : Read / Write
;;;   "max-y"                    gfloat                : Read / Write
;;;   "min-x"                    gfloat                : Read / Write
;;;   "min-y"                    gfloat                : Read / Write
;;; 
;;; Signals
;;; 
;;;   "curve-type-changed"                             : Run First
;;; 
;;; Description
;;; 
;;; The GtkCurve widget allows the user to edit a curve covering a range of
;;; values. It is typically used to fine-tune color balances in graphics
;;; applications like the Gimp.
;;; 
;;; The GtkCurve widget has 3 modes of operation - spline, linear and free. In
;;; spline mode the user places points on the curve which are automatically
;;; connected together into a smooth curve. In linear mode the user places
;;; points on the curve which are connected by straight lines. In free mode the
;;; user can draw the points of the curve freely, and they are not connected at
;;; all.
;;; 
;;; As of GTK+ 2.20, GtkCurve has been deprecated since it is too specialized.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "curve-type" property
;;; 
;;;   "curve-type"               GtkCurveType          : Read / Write
;;; 
;;; Is this curve linear, spline interpolated, or free-form.
;;; 
;;; Default value: GTK_CURVE_TYPE_SPLINE
;;;
;;; ----------------------------------------------------------------------------
;;; The "max-x" property
;;; 
;;;   "max-x"                    gfloat                : Read / Write
;;; 
;;; Maximum possible X value.
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "max-y" property
;;; 
;;;   "max-y"                    gfloat                : Read / Write
;;; 
;;; Maximum possible value for Y.
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "min-x" property
;;; 
;;;   "min-x"                    gfloat                : Read / Write
;;; 
;;; Minimum possible value for X.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "min-y" property
;;; 
;;;   "min-y"                    gfloat                : Read / Write
;;; 
;;; Minimum possible value for Y.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "curve-type-changed" signal
;;; 
;;; void user_function (GtkCurve *curve,
;;;                     gpointer  user_data)      : Run First
;;; 
;;; Emitted when the curve type has been changed. The curve type can be changed
;;; explicitly with a call to gtk_curve_set_curve_type(). It is also changed as
;;; a side-effect of calling gtk_curve_reset() or gtk_curve_set_gamma().
;;; 
;;; curve :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkCurveType
;;; 
;;; typedef enum {
;;;   GTK_CURVE_TYPE_LINEAR,       /* linear interpolation */
;;;   GTK_CURVE_TYPE_SPLINE,       /* spline interpolation */
;;;   GTK_CURVE_TYPE_FREE          /* free form curve */
;;; } GtkCurveType;
;;; 
;;; Warning
;;; 
;;; GtkCurveType is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkCurveType" gtk-curve-type
  (:export t
   :type-initializer "gtk_curve_type_get_type")
  (:linear 0)
  (:spline 1)
  (:free 2))

;;; ----------------------------------------------------------------------------
;;; struct GtkCurve
;;; 
;;; struct GtkCurve;
;;; 
;;; Warning
;;; 
;;; GtkCurve is deprecated and should not be used in newly-written code.
;;; 
;;; The GtkCurve struct contains private data only, and should be accessed using
;;; the functions below.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCurve" gtk-curve
  (:superclass gtk-drawing-area
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_curve_get_type")
  ((curve-type
    gtk-curve-curve-type
    "curve-type" "GtkCurveType" t t)
   (max-x
    gtk-curve-max-x
    "max-x" "gfloat" t t)
   (max-y
    gtk-curve-max-y
    "max-y" "gfloat" t t)
   (min-x
    gtk-curve-min-x
    "min-x" "gfloat" t t)
   (min-y
    gt-curve-min-y
    "min-y" "gfloat" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_curve_new ()
;;; 
;;; GtkWidget * gtk_curve_new (void);
;;; 
;;; Warning
;;; 
;;; gtk_curve_new has been deprecated since version 2.20 and should not be used
;;; in newly-written code. Don't use this widget anymore.
;;; 
;;; Creates a new GtkCurve.
;;; 
;;; Returns :
;;;     a new GtkCurve.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_curve_reset ()
;;; 
;;; void gtk_curve_reset (GtkCurve *curve);
;;; 
;;; Warning
;;; 
;;; gtk_curve_reset has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Don't use this widget anymore.
;;; 
;;; Resets the curve to a straight line from the minimum x and y values to the
;;; maximum x and y values (i.e. from the bottom-left to the top-right corners).
;;; The curve type is not changed.
;;; 
;;; curve :
;;;     a GtkCurve.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_curve_set_gamma ()
;;; 
;;; void gtk_curve_set_gamma (GtkCurve *curve, gfloat gamma_);
;;; 
;;; Warning
;;; 
;;; gtk_curve_set_gamma has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Don't use this widget anymore.
;;; 
;;; Recomputes the entire curve using the given gamma value. A gamma value of 1
;;; results in a straight line. Values greater than 1 result in a curve above
;;; the straight line. Values less than 1 result in a curve below the straight
;;; line. The curve type is changed to GTK_CURVE_TYPE_FREE. FIXME: Needs a more
;;; precise definition of gamma.
;;; 
;;; curve :
;;;     a GtkCurve.
;;; 
;;; gamma_ :
;;;     the gamma value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_curve_set_range ()
;;; 
;;; void gtk_curve_set_range (GtkCurve *curve,
;;;                           gfloat min_x,
;;;                           gfloat max_x,
;;;                           gfloat min_y,
;;;                           gfloat max_y);
;;; 
;;; Warning
;;; 
;;; gtk_curve_set_range has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Don't use this widget anymore.
;;; 
;;; Sets the minimum and maximum x and y values of the curve. The curve is also
;;; reset with a call to gtk_curve_reset().
;;; 
;;; curve :
;;;     a GtkCurve.
;;; 
;;; min_x :
;;;     the minimum x value.
;;; 
;;; max_x :
;;;     the maximum x value.
;;; 
;;; min_y :
;;;     the minimum y value.
;;; 
;;; max_y :
;;;     the maximum y value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_curve_get_vector ()
;;; 
;;; void gtk_curve_get_vector (GtkCurve *curve, int veclen, gfloat vector[]);
;;; 
;;; Warning
;;; 
;;; gtk_curve_get_vector has been deprecated since version 2.20 and should not
;;; be used in newly-written code. Don't use this widget anymore.
;;; 
;;; Returns a vector of points representing the curve.
;;; 
;;; curve :
;;;     a GtkCurve.
;;; 
;;; veclen :
;;;     the number of points to calculate.
;;; 
;;; vector :
;;;     returns the points.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_curve_set_vector ()
;;; 
;;; void gtk_curve_set_vector (GtkCurve *curve, int veclen, gfloat vector[]);
;;; 
;;; Warning
;;; 
;;; gtk_curve_set_vector has been deprecated since version 2.20 and should not
;;; be used in newly-written code. Don't use this widget anymore.
;;; 
;;; Sets the vector of points on the curve. The curve type is set to
;;; GTK_CURVE_TYPE_FREE.
;;; 
;;; curve :
;;;     a GtkCurve.
;;; 
;;; veclen :
;;;     the number of points
;;; 
;;; vector :
;;;     the points on the curve
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_curve_set_curve_type ()
;;; 
;;; void gtk_curve_set_curve_type (GtkCurve *curve, GtkCurveType type);
;;; 
;;; Warning
;;; 
;;; gtk_curve_set_curve_type has been deprecated since version 2.20 and should
;;; not be used in newly-written code. Don't use this widget anymore.
;;; 
;;; Sets the type of the curve. The curve will remain unchanged except when
;;; changing from a free curve to a linear or spline curve, in which case the
;;; curve will be changed as little as possible.
;;; 
;;; curve :
;;;     a GtkCurve.
;;; 
;;; type :
;;;     the type of the curve.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; GtkGammaCurve
;;; 
;;; A subclass of GtkCurve for editing gamma curves
;;; 
;;; Synopsis
;;; 
;;;     GtkGammaCurve
;;;
;;;     gtk_gamma_curve_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkWidget
;;;                      +----GtkContainer
;;;                            +----GtkBox
;;;                                  +----GtkVBox
;;;                                        +----GtkGammaCurve
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkGammaCurve implements AtkImplementorIface, GtkBuildable and
;;; GtkOrientable.
;;;
;;; Description
;;; 
;;; The GtkGammaCurve widget is a variant of GtkCurve specifically for editing
;;; gamma curves, which are used in graphics applications such as the Gimp.
;;; 
;;; The GtkGammaCurve widget shows a curve which the user can edit with the
;;; mouse just like a GtkCurve widget. On the right of the curve it also
;;; displays 5 buttons, 3 of which change between the 3 curve modes (spline,
;;; linear and free), and the other 2 set the curve to a particular gamma value,
;;; or reset it to a straight line.
;;; 
;;; As of GTK+ 2.20, GtkGammaCurve has been deprecated since it is too
;;; specialized.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkGammaCurve
;;; 
;;; struct GtkGammaCurve;
;;; 
;;; Warning
;;; 
;;; GtkGammaCurve is deprecated and should not be used in newly-written code.
;;; 
;;; The GtkGammaCurve struct contains private data only, and should be accessed
;;; using the functions below.
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkGammaCurve" 'gtk-gamma-curve))

(define-g-object-class "GtkGammaCurve" gtk-gamma-curve
  (:superclass gtk-vbox
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_gamma_curve_get_type")
  nil)

;;; ----------------------------------------------------------------------------

(define-child-property "GtkGammaCurve"
                       gtk-gamma-curve-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkGammaCurve"
                       gtk-gamma-curve-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkGammaCurve"
                       gtk-gamma-curve-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkGammaCurve"
                       gtk-gamma-curve-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkGammaCurve"
                       gtk-gamma-curve-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_gamma_curve_new ()
;;; 
;;; GtkWidget * gtk_gamma_curve_new (void);
;;; 
;;; Warning
;;; 
;;; gtk_gamma_curve_new has been deprecated since version 2.20 and should not be
;;; used in newly-written code. Don't use this widget anymore.
;;; 
;;; Creates a new GtkGammaCurve.
;;; 
;;; Returns :
;;;     a new GtkGammaCurve.
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.curve.lisp ---------------------------------------------
