;;; ----------------------------------------------------------------------------
;;; gtk.h-scale.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GTK 2.2.2 Reference Manual
;;; See http://www.gtk.org.
;;; 
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkHScale
;;; 
;;; A horizontal slider widget for selecting a value from a range
;;; 	
;;; Synopsis
;;; 
;;;     GtkHScale
;;;     gtk_hscale_new
;;;     gtk_hscale_new_with_range
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkRange
;;;                      +----GtkScale
;;;                            +----GtkHScale
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkHScale implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; Description
;;; 
;;; The GtkHScale widget is used to allow the user to select a value using a
;;; horizontal slider. To create one, use gtk_hscale_new_with_range().
;;; 
;;; The position to show the current value, and the number of decimal places
;;; shown can be set using the parent GtkScale class's functions.
;;; 
;;; GtkHScale has been deprecated, use GtkScale instead.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkHScale
;;; 
;;; struct GtkHScale;
;;; 
;;; Warning
;;; 
;;; GtkHScale is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkHScale" gtk-h-scale
  (:superclass gtk-scale
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_hscale_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_hscale_new ()
;;; 
;;; GtkWidget * gtk_hscale_new (GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_hscale_new has been deprecated since version 3.2 and should not be used
;;; in newly-written code. Use gtk_scale_new() with GTK_ORIENTATION_HORIZONTAL
;;; instead
;;; 
;;; Creates a new GtkHScale.
;;; 
;;; adjustment :
;;; 	the GtkAdjustment which sets the range of the scale.
;;; 
;;; Returns :
;;; 	a new GtkHScale.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_hscale_new_with_range ()
;;; 
;;; GtkWidget * gtk_hscale_new_with_range (gdouble min,
;;;                                        gdouble max,
;;;                                        gdouble step);
;;; 
;;; Warning
;;; 
;;; gtk_hscale_new_with_range has been deprecated since version 3.2 and should
;;; not be used in newly-written code. Use gtk_scale_new_with_range() with
;;; GTK_ORIENTATION_HORIZONTAL instead
;;; 
;;; Creates a new horizontal scale widget that lets the user input a number
;;; between min and max (including min and max) with the increment step. step
;;; must be nonzero; it's the distance the slider moves when using the arrow
;;; keys to adjust the scale value.
;;; 
;;; Note that the way in which the precision is derived works best if step is a
;;; power of ten. If the resulting precision is not suitable for your needs,
;;; use gtk_scale_set_digits() to correct it.
;;; 
;;; min :
;;; 	minimum value
;;; 
;;; max :
;;; 	maximum value
;;; 
;;; step :
;;; 	step increment (tick size) used with keyboard shortcuts
;;; 
;;; Returns :
;;; 	a new GtkHScale
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.h-scale.lisp -------------------------------------------
