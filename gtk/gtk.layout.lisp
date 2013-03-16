;;; ----------------------------------------------------------------------------
;;; gtk.layout.lisp
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
;;; GtkLayout
;;; 
;;; Infinite scrollable area containing child widgets and/or custom drawing
;;;     
;;; Synopsis
;;; 
;;;     GtkLayout
;;;     
;;;     gtk_layout_new
;;;     gtk_layout_put
;;;     gtk_layout_move
;;;     gtk_layout_set_size
;;;     gtk_layout_get_size
;;;     gtk_layout_get_hadjustment               * deprecated *
;;;     gtk_layout_get_vadjustment               * deprecated *
;;;     gtk_layout_set_hadjustment               * deprecated *
;;;     gtk_layout_set_vadjustment               * deprecated *
;;;     gtk_layout_get_bin_window
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkLayout
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkLayout" 'gtk-layout))

(define-g-object-class "GtkLayout" gtk-layout
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkScrollable")
   :type-initializer "gtk_layout_get_type")
  ((height
    gtk-layout-height
    "height" "guint" t t)
   (width
    gtk-layout-width
    "width" "guint" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-layout 'type)
 "@version{2013-3-10}
  @begin{short}
    GtkLayout is similar to GtkDrawingArea in that it's a \"blank slate\" and
    doesn't do anything but paint a blank background by default. It's different
    in that it supports scrolling natively (you can add it to a
    GtkScrolledWindow), and it can contain child widgets, since it's a
    GtkContainer. However if you're just going to draw, a GtkDrawingArea is a
    better choice since it has lower overhead.
  @end{short}

  When handling expose events on a GtkLayout, you must draw to GTK_LAYOUT
  (layout)->bin_window, rather than to GTK_WIDGET (layout)->window, as you
  would for a drawing area.
  @begin[Child Property Details]{dictionary}
    @subheading{The \"x\" child property}
      @code{\"x\"} of type @code{gint} (Read / Write)@br{}
      X position of child widget.@br{}
      Default value: @code{0}

    @subheading{The \"y\" child property}
      @code{\"y\"} of type @code{gint} (Read / Write)@br{}
      Y position of child widget.@br{}
      Default value: @code{0}
  @end{dictionary}
  @see-slot{gtk-layout-height}
  @see-slot{gtk-layout-width}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "height" 'gtk-layout) 't)
 "The @code{\"height\"} property of type @code{guint} (Read / Write)@br{}
  The height of the layout.@br{}
  Allowed values: @code{<= G_MAXINT}@br{}
  Default value: @code{100}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width" 'gtk-layout) 't)
 "The @code{\"width\"} property of type @code{guint} (Read / Write)@br{}
  The width of the layout.@br{}
  Allowed values: @code{<= G_MAXINT}@br{}
  Default value: @code{100}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-layout-height atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-layout-height 'function)
 "@version{2013-3-10}
  @begin{short}
    Accessor of the slot @code{\"height\"} of the @class{gtk-layout} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-layout-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-layout-width 'function)
 "@version{2013-3-10}
  @begin{short}
    Accessor of the slot @code{\"width\"} of the @class{gtk-layout} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkLayout"
                       gtk-layout-child-x
                       "x" "gint" t t t)

(define-child-property "GtkLayout"
                       gtk-layout-child-y
                       "y" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-layout-child-x atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-layout-child-x 'function)
 "@version{2013-3-10}
  @begin{short}
    Accessor of the child property @code{\"x\"} of the @class{gtk-layout} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-layout-child-y atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-layout-child-y 'function)
 "@version{2013-3-10}
  @begin{short}
    Accessor of the child property @code{\"y\"} of the @class{gtk-layout} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_layout_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-layout-new))

(defun gtk-layout-new (&optional (hadjustment nil) (vadjustment nil))
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[hadjustment]{horizontal scroll adjustment, or NULL}
  @argument[vadjustment]{vertical scroll adjustment, or NULL}
  @return{a new GtkLayout}
  @begin{short}
    Creates a new GtkLayout. Unless you have a specific adjustment you'd like
    the layout to use for scrolling, pass NULL for hadjustment and vadjustment.
  @end{short}"
  (make-instance 'gtk-layout
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'gtk-layout-new)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_put ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_put" gtk-layout-put) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[layout]{a GtkLayout}
  @argument[child_widget]{child widget}
  @argument[x]{X position of child widget}
  @argument[y]{Y position of child widget}
  @begin{short}
    Adds child_widget to layout, at position (x,y). layout becomes the new
    parent container of child_widget.
  @end{short}"
  (layout g-object)
  (widget g-object)
  (x :int)
  (y :int))

(export 'gtk-layout-put)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_move ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_move" gtk-layout-move) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[layout]{a GtkLayout}
  @argument[child_widget]{a current child of layout}
  @argument[x]{X position to move to}
  @argument[y]{Y position to move to}
  @short{Moves a current child of layout to a new position.}"
  (layout g-object)
  (widget g-object)
  (x :int)
  (y :int))

(export 'gtk-layout-move)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_set_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-layout-set-size))

(defun gtk-layout-set-size (layout width height)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[layout]{a GtkLayout}
  @argument[width]{width of entire scrollable area}
  @argument[height]{height of entire scrollable area}
  @short{Sets the size of the scrollable area of the layout.}"
  (setf (gtk-layout-width layout) width
        (gtk-layout-height layout) height))

(export 'gtk-layout-set-size)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_size ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-layout-get-size))

(defun gtk-layout-get-size (layout)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[layout]{a GtkLayout}
  @argument[width]{location to store the width set on layout, or NULL}
  @argument[height]{location to store the height set on layout, or NULL}
  @begin{short}
    Gets the size that has been set on the layout, and that determines the total
    extents of the layout's scrollbar area. See gtk_layout_set_size().
  @end{short}"
  (values (gtk-layout-width layout)
          (gtk-layout-height layout)))

(export 'gtk-layout-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_hadjustment ()
;;; 
;;; GtkAdjustment * gtk_layout_get_hadjustment (GtkLayout *layout);
;;; 
;;; Warning
;;; 
;;; gtk_layout_get_hadjustment has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gtk_scrollable_get_hadjustment()
;;; 
;;; This function should only be called after the layout has been placed in a
;;; GtkScrolledWindow or otherwise configured for scrolling. It returns the
;;; GtkAdjustment used for communication between the horizontal scrollbar and
;;; layout.
;;; 
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;; 
;;; layout :
;;;     a GtkLayout
;;; 
;;; Returns :
;;;     horizontal scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_vadjustment ()
;;; 
;;; GtkAdjustment * gtk_layout_get_vadjustment (GtkLayout *layout);
;;; 
;;; Warning
;;; 
;;; gtk_layout_get_vadjustment has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gtk_scrollable_get_vadjustment()
;;; 
;;; This function should only be called after the layout has been placed in a
;;; GtkScrolledWindow or otherwise configured for scrolling. It returns the
;;; GtkAdjustment used for communication between the vertical scrollbar and
;;; layout.
;;; 
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;; 
;;; layout :
;;;     a GtkLayout
;;; 
;;; Returns :
;;;     vertical scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_set_hadjustment ()
;;; 
;;; void gtk_layout_set_hadjustment (GtkLayout *layout,
;;;                                  GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_layout_set_hadjustment has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gtk_scrollable_set_hadjustment()
;;; 
;;; Sets the horizontal scroll adjustment for the layout.
;;; 
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;; 
;;; layout :
;;;     a GtkLayout
;;; 
;;; adjustment :
;;;     new scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_set_vadjustment ()
;;; 
;;; void gtk_layout_set_vadjustment (GtkLayout *layout,
;;;                                  GtkAdjustment *adjustment);
;;; 
;;; Warning
;;; 
;;; gtk_layout_set_vadjustment has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gtk_scrollable_set_vadjustment()
;;; 
;;; Sets the vertical scroll adjustment for the layout.
;;; 
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;; 
;;; layout :
;;;     a GtkLayout
;;; 
;;; adjustment :
;;;     new scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_bin_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_get_bin_window" gtk-layout-get-bin-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[layout]{a GtkLayout}
  @return{a GdkWindow}
  @short{Retrieve the bin window of the layout used for drawing operations.}

  Since 2.14"
  (layout (g-object gtk-layout)))

(export 'gtk-layout-get-bin-window)

;;; --- End of file gtk.layout.lisp --------------------------------------------
