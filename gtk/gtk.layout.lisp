;;; ----------------------------------------------------------------------------
;;; gtk.layout.lisp
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
;;;     gtk_layout_get_hadjustment   * deprecated *
;;;     gtk_layout_get_vadjustment   * deprecated *
;;;     gtk_layout_set_hadjustment   * deprecated *
;;;     gtk_layout_set_vadjustment   * deprecated *
;;;     gtk_layout_get_bin_window
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkLayout
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkLayout implements AtkImplementorIface, GtkBuildable and GtkScrollable.
;;;
;;; Properties
;;; 
;;;   "height"                   guint                 : Read / Write
;;;   "width"                    guint                 : Read / Write
;;; 
;;; Child Properties
;;; 
;;;   "x"                        gint                  : Read / Write
;;;   "y"                        gint                  : Read / Write
;;; 
;;; Description
;;; 
;;; GtkLayout is similar to GtkDrawingArea in that it's a "blank slate" and
;;; doesn't do anything but paint a blank background by default. It's different
;;; in that it supports scrolling natively (you can add it to a
;;; GtkScrolledWindow), and it can contain child widgets, since it's a
;;; GtkContainer. However if you're just going to draw, a GtkDrawingArea is a
;;; better choice since it has lower overhead.
;;; 
;;; When handling expose events on a GtkLayout, you must draw to
;;; GTK_LAYOUT (layout)->bin_window, rather than to
;;; GTK_WIDGET (layout)->window, as you would for a drawing area.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "height" property
;;; 
;;;   "height"                   guint                 : Read / Write
;;; 
;;; The height of the layout.
;;; 
;;; Allowed values: <= G_MAXINT
;;; 
;;; Default value: 100
;;;
;;; ----------------------------------------------------------------------------
;;; The "width" property
;;; 
;;;   "width"                    guint                 : Read / Write
;;; 
;;; The width of the layout.
;;; 
;;; Allowed values: <= G_MAXINT
;;; 
;;; Default value: 100
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Child Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "x" child property
;;; 
;;;   "x"                        gint                  : Read / Write
;;; 
;;; X position of child widget.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "y" child property
;;; 
;;;   "y"                        gint                  : Read / Write
;;; 
;;; Y position of child widget.
;;; 
;;; Default value: 0
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkLayout
;;; 
;;; struct GtkLayout;
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkLayout" 'gtk-layout))

(define-g-object-class "GtkLayout" gtk-layout
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkScrollable")
   :type-initializer "gtk_layout_get_type")
  ((height
    gtk-layout-height
    "height" "guint" t t)
   (width
    gtk-layout-width
    "width" "guint" t t)
   (:cffi bin-window
          gtk-layout-bin-window g-object
          "gtk_layout_get_bin_window" nil)))

;;; ----------------------------------------------------------------------------

(define-child-property "GtkLayout"
                       gtk-layout-child-x
                       "x" "gint" t t t)

(define-child-property "GtkLayout"
                       gtk-layout-child-y
                       "y" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_new ()
;;; 
;;; GtkWidget * gtk_layout_new (GtkAdjustment *hadjustment,
;;;                             GtkAdjustment *vadjustment);
;;; 
;;; Creates a new GtkLayout. Unless you have a specific adjustment you'd like
;;; the layout to use for scrolling, pass NULL for hadjustment and vadjustment.
;;; 
;;; hadjustment :
;;;     horizontal scroll adjustment, or NULL
;;; 
;;; vadjustment :
;;;     vertical scroll adjustment, or NULL
;;; 
;;; Returns :
;;;     a new GtkLayout
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-layout-new))

(defun gtk-layout-new (&optional (hadjustment nil) (vadjustment nil))
  (make-instance 'gtk-layout
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'gtk-layout-new)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_put ()
;;; 
;;; void gtk_layout_put (GtkLayout *layout,
;;;                      GtkWidget *child_widget,
;;;                      gint x,
;;;                      gint y);
;;; 
;;; Adds child_widget to layout, at position (x,y). layout becomes the new
;;; parent container of child_widget.
;;; 
;;; layout :
;;;     a GtkLayout
;;; 
;;; child_widget :
;;;     child widget
;;; 
;;; x :
;;;     X position of child widget
;;; 
;;; y :
;;;     Y position of child widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_put" gtk-layout-put) :void
  (layout g-object)
  (widget g-object)
  (x :int)
  (y :int))

(export 'gtk-layout-put)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_move ()
;;; 
;;; void gtk_layout_move (GtkLayout *layout,
;;;                       GtkWidget *child_widget,
;;;                       gint x,
;;;                       gint y);
;;; 
;;; Moves a current child of layout to a new position.
;;; 
;;; layout :
;;;     a GtkLayout
;;; 
;;; child_widget :
;;;     a current child of layout
;;; 
;;; x :
;;;     X position to move to
;;; 
;;; y :
;;;     Y position to move to
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_move" gtk-layout-move) :void
  (layout g-object)
  (widget g-object)
  (x :int)
  (y :int))

(export 'gtk-layout-move)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_set_size ()
;;; 
;;; void gtk_layout_set_size (GtkLayout *layout, guint width, guint height);
;;; 
;;; Sets the size of the scrollable area of the layout.
;;; 
;;; layout :
;;;     a GtkLayout
;;; 
;;; width :
;;;     width of entire scrollable area
;;; 
;;; height :
;;;     height of entire scrollable area
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-layout-set-size))

(defun gtk-layout-set-size (layout width height)
  (setf (gtk-layout-width layout) width
        (gtk-layout-height layout) height))

(export 'gtk-layout-set-size)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_size ()
;;; 
;;; void gtk_layout_get_size (GtkLayout *layout, guint *width, guint *height);
;;; 
;;; Gets the size that has been set on the layout, and that determines the
;;; total extents of the layout's scrollbar area. See gtk_layout_set_size().
;;; 
;;; layout :
;;;     a GtkLayout
;;; 
;;; width :
;;;     location to store the width set on layout, or NULL
;;; 
;;; height :
;;;     location to store the height set on layout, or NULL
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-layout-get-size))

(defun gtk-layout-get-size (layout)
  (values (gtk-layout-width layout)
          (gtk-layout-height layout)))

(export 'gtk-layout-get-size)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_bin_window ()
;;; 
;;; GdkWindow * gtk_layout_get_bin_window (GtkLayout *layout);
;;; 
;;; Retrieve the bin window of the layout used for drawing operations.
;;; 
;;; layout :
;;;     a GtkLayout
;;; 
;;; Returns :
;;;     a GdkWindow
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_get_bin_window" gtk-layout-get-bin-window)
    (g-object gdk-window)
  (layout (g-object gtk-layout)))

(export 'gtk-layout-get-bin-window)

;;; --- End of file gtk.layout.lisp --------------------------------------------
