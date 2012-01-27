;;; ----------------------------------------------------------------------------
;;; gtk.layout.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.2 Reference Manual
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
;;;     gtk_layout_get_hadjustment
;;;     gtk_layout_get_vadjustment
;;;     gtk_layout_set_hadjustment
;;;     gtk_layout_set_vadjustment
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

(define-g-object-class "GtkLayout" gtk-layout
  (:superclass gtk-container
    :export t
    :interfaces ("AtkImplementorIface" "GtkBuildable")
    :type-initializer "gtk_layout_get_type")
  ((hadjustment gtk-layout-hadjustment
    "hadjustment" "GtkAdjustment" t t)
   (height gtk-layout-height
    "height" "guint" t t)
   (vadjustment gtk-layout-vadjustment
    "vadjustment" "GtkAdjustment" t t)
   (width gtk-layout-width
    "width" "guint" t t)
   (:cffi bin-window gtk-layout-bin-window g-object
          "gtk_layout_get_bin_window" nil)))

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
;;; 	horizontal scroll adjustment, or NULL
;;; 
;;; vadjustment :
;;; 	vertical scroll adjustment, or NULL
;;; 
;;; Returns :
;;; 	a new GtkLayout
;;; ----------------------------------------------------------------------------

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
;;; 	a GtkLayout
;;; 
;;; child_widget :
;;; 	child widget
;;; 
;;; x :
;;; 	X position of child widget
;;; 
;;; y :
;;; 	Y position of child widget
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
;;; 	a GtkLayout
;;; 
;;; child_widget :
;;; 	a current child of layout
;;; 
;;; x :
;;; 	X position to move to
;;; 
;;; y :
;;; 	Y position to move to
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
;;; 	a GtkLayout
;;; 
;;; width :
;;; 	width of entire scrollable area
;;; 
;;; height :
;;; 	height of entire scrollable area
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_size ()
;;; 
;;; void gtk_layout_get_size (GtkLayout *layout, guint *width, guint *height);
;;; 
;;; Gets the size that has been set on the layout, and that determines the
;;; total extents of the layout's scrollbar area. See gtk_layout_set_size().
;;; 
;;; layout :
;;; 	a GtkLayout
;;; 
;;; width :
;;; 	location to store the width set on layout, or NULL
;;; 
;;; height :
;;; 	location to store the height set on layout, or NULL
;;; ----------------------------------------------------------------------------

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
;;; 	a GtkLayout
;;; 
;;; Returns :
;;; 	horizontal scroll adjustment
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
;;; 	a GtkLayout
;;; 
;;; Returns :
;;; 	vertical scroll adjustment.
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
;;; 	a GtkLayout
;;; 
;;; adjustment :
;;; 	new scroll adjustment
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
;;; 	a GtkLayout
;;; 
;;; adjustment :
;;; 	new scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_bin_window ()
;;; 
;;; GdkWindow * gtk_layout_get_bin_window (GtkLayout *layout);
;;; 
;;; Retrieve the bin window of the layout used for drawing operations.
;;; 
;;; layout :
;;; 	a GtkLayout
;;; 
;;; Returns :
;;; 	a GdkWindow
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.layout.lisp --------------------------------------------
