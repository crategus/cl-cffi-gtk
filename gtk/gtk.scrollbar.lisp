;;; ----------------------------------------------------------------------------
;;; gtk.scrollbar.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;; GtkScrollbar
;;; 
;;; A Scrollbar
;;;     
;;; Synopsis
;;; 
;;;     GtkScrollbar
;;;     
;;;     gtk_scrollbar_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkRange
;;;                      +----GtkScrollbar
;;;                            +----GtkHScrollbar
;;;                            +----GtkVScrollbar
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkScrollbar implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; 
;;; Style Properties
;;; 
;;;   "fixed-slider-length"            gboolean              : Read
;;;   "has-backward-stepper"           gboolean              : Read
;;;   "has-forward-stepper"            gboolean              : Read
;;;   "has-secondary-backward-stepper" gboolean              : Read
;;;   "has-secondary-forward-stepper"  gboolean              : Read
;;;   "min-slider-length"              gint                  : Read
;;; 
;;; Description
;;; 
;;; The GtkScrollbar widget is a horizontal or vertical scrollbar, depending on
;;; the value of the "orientation" property.
;;; 
;;; The position of the thumb in a scrollbar is controlled by the scroll
;;; adjustments. See GtkAdjustment for the fields in an adjustment - for
;;; GtkScrollbar, the GtkAdjustment.value field represents the position of the
;;; scrollbar, which must be between the GtkAdjustment.lower field and
;;; GtkAdjustment.upper - GtkAdjustment.page_size. The GtkAdjustment.page_size
;;; field represents the size of the visible scrollable area. The
;;; GtkAdjustment.step_increment and GtkAdjustment.page_increment fields are
;;; used when the user asks to step down (using the small stepper arrows) or
;;; page down (using for example the PageDown key).
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "fixed-slider-length" style property
;;; 
;;;   "fixed-slider-length"      gboolean              : Read
;;; 
;;; Don't change slider size, just lock it to the minimum length.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-backward-stepper" style property
;;; 
;;;   "has-backward-stepper"     gboolean              : Read
;;; 
;;; Display the standard backward arrow button.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-forward-stepper" style property
;;; 
;;;   "has-forward-stepper"      gboolean              : Read
;;; 
;;; Display the standard forward arrow button.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-secondary-backward-stepper" style property
;;; 
;;;   "has-secondary-backward-stepper" gboolean              : Read
;;; 
;;; Display a second backward arrow button on the opposite end of the scrollbar.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-secondary-forward-stepper" style property
;;; 
;;;   "has-secondary-forward-stepper" gboolean              : Read
;;; 
;;; Display a second forward arrow button on the opposite end of the scrollbar.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "min-slider-length" style property
;;; 
;;;   "min-slider-length"        gint                  : Read
;;; 
;;; Minimum length of scrollbar slider.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 21
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkScrollbar
;;; 
;;; struct GtkScrollbar;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkScrollbar" gtk-scrollbar
  (:superclass gtk-range
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_scrollbar_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollbar_new ()
;;; 
;;; GtkWidget * gtk_scrollbar_new (GtkOrientation orientation,
;;;                                GtkAdjustment *adjustment);
;;; 
;;; Creates a new scrollbar with the given orientation.
;;; 
;;; orientation :
;;;     the scrollbar's orientation.
;;; 
;;; adjustment :
;;;     the GtkAdjustment to use, or NULL to create a new adjustment
;;; 
;;; Returns :
;;;     the new GtkScrollbar
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollbar-new))

(defun gtk-scrollbar-new (orientation adjustment)
  (make-instance 'gtk-scrollbar
                 :orientation orientation
                 :adjustment adjustment))

(export 'gtk-scrollbar-new)

;;; ----------------------------------------------------------------------------
;;; GtkHScrollbar
;;; 
;;; A horizontal scrollbar
;;;     
;;; Synopsis
;;; 
;;;     GtkHScrollbar
;;;     
;;;     gtk_hscrollbar_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkRange
;;;                      +----GtkScrollbar
;;;                            +----GtkHScrollbar
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkHScrollbar implements AtkImplementorIface, GtkBuildable and
;;; GtkOrientable.
;;;
;;; Description
;;; 
;;; The GtkHScrollbar widget is a widget arranged horizontally creating a
;;; scrollbar. See GtkScrollbar for details on scrollbars. GtkAdjustment
;;; pointers may be added to handle the adjustment of the scrollbar or it may be
;;; left NULL in which case one will be created for you. See GtkScrollbar for a
;;; description of what the fields in an adjustment represent for a scrollbar.
;;; 
;;; GtkHScrollbar has been deprecated, use GtkScrollbar instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHScrollbar
;;; 
;;; struct GtkHScrollbar;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHScrollbar" 'gtk-hscrollbar)
  (setf *lisp-name-exceptions*
        (append '(("GtkHScrollbar" GTK-HSCROLLBAR)) *lisp-name-exceptions*)))

(define-g-object-class "GtkHScrollbar" gtk-hscrollbar
  (:superclass gtk-scrollbar
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_hscrollbar_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_hscrollbar_new ()
;;; 
;;; GtkWidget * gtk_hscrollbar_new (GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_hscrollbar_new has been deprecated since version 3.2 and should not be
;;; used in newly-written code. Use gtk_scrollbar_new() with
;;; GTK_ORIENTATION_HORIZONTAL instead
;;; 
;;; Creates a new horizontal scrollbar.
;;; 
;;; adjustment :
;;;     the GtkAdjustment to use, or NULL to create a new adjustment
;;; 
;;; Returns :
;;;     the new GtkHScrollbar
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hscrollbar-new))

(defun gtk-hscrollbar-new (adjustment)
  (make-instance 'gtk-scrollbar
                 :orientation :horizontal
                 :adjustment adjustment))

(export 'gtk-hscrollbar-new)

;;; ----------------------------------------------------------------------------
;;; GtkVScrollbar
;;; 
;;; A vertical scrollbar
;;;     
;;; Synopsis
;;; 
;;;     GtkVScrollbar
;;;     
;;;     gtk_vscrollbar_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkRange
;;;                      +----GtkScrollbar
;;;                            +----GtkVScrollbar
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkVScrollbar implements AtkImplementorIface, GtkBuildable and
;;; GtkOrientable.
;;; 
;;; Description
;;; 
;;; The GtkVScrollbar widget is a widget arranged vertically creating a
;;; scrollbar. See GtkScrollbar for details on scrollbars. GtkAdjustment
;;; pointers may be added to handle the adjustment of the scrollbar or it may be
;;; left NULL in which case one will be created for you. See GtkScrollbar for a
;;; description of what the fields in an adjustment represent for a scrollbar.
;;; 
;;; GtkVScrollbar has been deprecated, use GtkScrollbar instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVScrollbar
;;; 
;;; struct GtkVScrollbar;
;;;
;;; The GtkVScrollbar struct contains private data and should be accessed using
;;; the functions below.
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkVScrollbar" 'gtk-vscrollbar)
  (setf *lisp-name-exceptions*
        (append '(("GtkVScrollbar" GTK-VSCROLLBAR)) *lisp-name-exceptions*)))

(define-g-object-class "GtkVScrollbar" gtk-vscrollbar
  (:superclass gtk-scrollbar
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_vscrollbar_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_vscrollbar_new ()
;;; 
;;; GtkWidget * gtk_vscrollbar_new (GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_vscrollbar_new has been deprecated since version 3.2 and should not be
;;; used in newly-written code. Use gtk_scrollbar_new() with
;;; GTK_ORIENTATION_VERTICAL instead
;;; 
;;; Creates a new vertical scrollbar.
;;; 
;;; adjustment :
;;;     the GtkAdjustment to use, or NULL to create a new adjustment
;;; 
;;; Returns :
;;;     the new GtkVScrollbar
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vscrollbar-new))

(defun gtk-vscrollbar-new (adjustment)
  (make-instance 'gtk-scrollbar
                 :orientation :vertical
                 :adjustment adjustment))

(export 'gtk-vscrollbar-new)

;;; --- End of file gtk.scrollbar.lisp -----------------------------------------
