;;; ----------------------------------------------------------------------------
;;; gtk.scrollbar.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkScrollbar
;;; 
;;; A Scrollbar
;;;     
;;; Synopsis
;;; 
;;;     GtkScrollbar
;;;     
;;;     gtk_scrollbar_new
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkScrollbar
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-scrollbar 'type)
 "@version{2013-3-22}
  @begin{short}
    The GtkScrollbar widget is a horizontal or vertical scrollbar, depending on
    the value of the \"orientation\" property.
  @end{short}

  The position of the thumb in a scrollbar is controlled by the scroll
  adjustments. See GtkAdjustment for the fields in an adjustment - for
  GtkScrollbar, the GtkAdjustment.value field represents the position of the
  scrollbar, which must be between the GtkAdjustment.lower field and
  GtkAdjustment.upper - GtkAdjustment.page_size. The GtkAdjustment.page_size
  field represents the size of the visible scrollable area. The
  GtkAdjustment.step_increment and GtkAdjustment.page_increment fields are
  used when the user asks to step down (using the small stepper arrows) or
  page down (using for example the PageDown key).
  @begin[Style Property Details]{dictionary}
    @subheading{The \"fixed-slider-length\" style property}
      @code{\"fixed-slider-length\"} of type @code{:boolean} (Read)@br{}
      Don't change slider size, just lock it to the minimum length. @br{}
      Default value: @code{nil}

    @subheading{The \"has-backward-stepper\" style property}
      @code{\"has-backward-stepper\"} of type @code{:boolean} (Read)@br{}
      Display the standard backward arrow button. @br{}
      Default value: @em{true}

    @subheading{The \"has-forward-stepper\" style property}
      @code{\"has-forward-stepper\"} of type @code{:boolean} (Read)@br{}
      Display the standard forward arrow button. @br{}
      Default value: @em{true}

    @subheading{The \"has-secondary-backward-stepper\" style property}
      @code{\"has-secondary-backward-stepper\"} of type @code{:boolean}
      (Read)@br{}
      Display a second backward arrow button on the opposite end of the
      scrollbar. @br{}
      Default value: @code{nil}

    @subheading{The \"has-secondary-forward-stepper\" style property}
      @code{\"has-secondary-forward-stepper\"} of type @code{:boolean}
      (Read)@br{}
      Display a second forward arrow button on the opposite end of the
      scrollbar. @br{}
      Default value: @code{nil}

    @subheading{The \"min-slider-length\" style property}
      @code{\"min-slider-length\"} of type @code{:int} (Read)@br{}
      Minimum length of scrollbar slider. @br{}
      Allowed values: >= 0@br{}
      Default value: 21
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gtk_scrollbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollbar-new))

(defun gtk-scrollbar-new (orientation adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[orientation]{the scrollbar's orientation.}
  @argument[adjustment]{the GtkAdjustment to use, or NULL to create a new
    adjustment}
  @return{the new GtkScrollbar}
  @short{Creates a new scrollbar with the given orientation.}

  Since 3.0"
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
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHScrollbar
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hscrollbar 'type)
 "@version{2013-3-8}
  @begin{short}
    The @sym{gtk-hscrollbar} widget is a widget arranged horizontally creating
    a scrollbar. See @class{gtk-scrollbar} for details on scrollbars.
  @end{short}
  @class{gtk-adjustment} pointers may be added to handle the adjustment of the
  scrollbar or it may be left @code{nil} in which case one will be created for
  you. See @class{gtk-scrollbar} for a description of what the fields in an
  adjustment represent for a scrollbar.

  @sym{gtk-hscrollbar} has been deprecated, use @class{gtk-scrollbar} instead.")

;;; ----------------------------------------------------------------------------
;;; gtk_hscrollbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-hscrollbar-new))

(defun gtk-hscrollbar-new (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-8}
  @argument[adjustment]{The @class{gtk-adjustment} to use, or @code{nil} to
    create a new adjustment}
  @return{The new @class{gtk-hscrollbar} widget.}
  @subheading{Warning}
  @sym{gtk-hscrollbar-new} has been deprecated since version 3.2 and should not
  be used in newly-written code. Use @fun{gtk-scrollbar-new} with
  @code{:horizontal} instead.

  @short{Creates a new horizontal scrollbar.}"
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
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVScrollbar
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-vscrollbar 'type)
 "@version{2013-3-23}
  @begin{short}
    The GtkVScrollbar widget is a widget arranged vertically creating a
    scrollbar. See GtkScrollbar for details on scrollbars. GtkAdjustment
    pointers may be added to handle the adjustment of the scrollbar or it may be
    left NULL in which case one will be created for you. See GtkScrollbar for a
    description of what the fields in an adjustment represent for a scrollbar.
  @end{short}

  GtkVScrollbar has been deprecated, use GtkScrollbar instead.")

;;; ----------------------------------------------------------------------------
;;; gtk_vscrollbar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-vscrollbar-new))

(defun gtk-vscrollbar-new (adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-23}
  @argument[adjustment]{the GtkAdjustment to use, or NULL to create a new
    adjustment}
  @return{the new GtkVScrollbar}
  Warning

  gtk_vscrollbar_new has been deprecated since version 3.2 and should not be
  used in newly-written code. Use gtk_scrollbar_new() with
  GTK_ORIENTATION_VERTICAL instead

  Creates a new vertical scrollbar."
  (make-instance 'gtk-scrollbar
                 :orientation :vertical
                 :adjustment adjustment))

(export 'gtk-vscrollbar-new)

;;; --- End of file gtk.scrollbar.lisp -----------------------------------------
