;;; ----------------------------------------------------------------------------
;;; gtk.drawing-area.lisp
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
;;;﻿
;;; GtkDrawingArea
;;;
;;; A widget for custom user interface elements
;;;
;;; Synopsis
;;;
;;;     GtkDrawingArea
;;;
;;;     gtk_drawing_area_new
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkDrawingArea
;;;
;;; Implemented Interfaces
;;;
;;; GtkDrawingArea implements AtkImplementorIface and GtkBuildable.
;;;
;;; Description
;;;
;;; The GtkDrawingArea widget is used for creating custom user interface
;;; elements. It's essentially a blank widget; you can draw on it. After
;;; creating a drawing area, the application may want to connect to:
;;;
;;;     Mouse and button press signals to respond to input from the user. (Use
;;;     gtk_widget_add_events() to enable events you wish to receive.)
;;;
;;;     The "realize" signal to take any necessary actions when the widget is
;;;     instantiated on a particular display. (Create GDK resources in response
;;;     to this signal.)
;;;
;;;     The "configure-event" signal to take any necessary actions when the
;;;     widget changes size.
;;;
;;;     The "draw" signal to handle redrawing the contents of the widget.
;;;
;;; The following code portion demonstrates using a drawing area to display a
;;; circle in the normal widget foreground color.
;;;
;;; Note that GDK automatically clears the exposed area to the background color
;;; before sending the expose event, and that drawing is implicitly clipped to
;;; the exposed area.
;;;
;;; Example 99. Simple GtkDrawingArea usage
;;;
;;;   gboolean
;;;   draw_callback (GtkWidget *widget, cairo_t *cr, gpointer data)
;;;   {
;;;     guint width, height;
;;;     GdkRGBA color;
;;;
;;;     width = gtk_widget_get_allocated_width (widget);
;;;     height = gtk_widget_get_allocated_height (widget);
;;;     cairo_arc (cr,
;;;                width / 2.0, height / 2.0,
;;;                MIN (width, height) / 2.0,
;;;                0, 2 * G_PI);
;;;
;;;     gtk_style_context_get_color (gtk_widget_get_style_context (widget),
;;;                                  0,
;;;                                  &color);
;;;     gdk_cairo_set_source_rgba (cr, &color);
;;;
;;;     cairo_fill (cr);
;;;
;;;    return FALSE;
;;;   }
;;;   [...]
;;;     GtkWidget *drawing_area = gtk_drawing_area_new ();
;;;     gtk_widget_set_size_request (drawing_area, 100, 100);
;;;     g_signal_connect (G_OBJECT (drawing_area), "draw",
;;;                       G_CALLBACK (draw_callback), NULL);
;;;
;;;
;;; Draw signals are normally delivered when a drawing area first comes
;;; onscreen, or when it's covered by another window and then uncovered. You can
;;; also force an expose event by adding to the "damage region" of the drawing
;;; area's window; gtk_widget_queue_draw_area() and gdk_window_invalidate_rect()
;;; are equally good ways to do this. You'll then get a draw signal for the
;;; invalid region.
;;;
;;; The available routines for drawing are documented on the GDK Drawing
;;; Primitives page and the cairo documentation.
;;;
;;; To receive mouse events on a drawing area, you will need to enable them with
;;; gtk_widget_add_events(). To receive keyboard events, you will need to set
;;; the "can-focus" property on the drawing area, and you should probably draw
;;; some user-visible indication that the drawing area is focused. Use
;;; gtk_widget_has_focus() in your expose event handler to decide whether to
;;; draw the focus indicator. See gtk_render_focus() for one way to draw focus.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkDrawingArea
;;;
;;; struct GtkDrawingArea;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkDrawingArea" gtk-drawing-area
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_drawing_area_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_drawing_area_new ()
;;;
;;; GtkWidget * gtk_drawing_area_new (void);
;;;
;;; Creates a new drawing area.
;;;
;;; Returns :
;;;     a new GtkDrawingArea
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-drawing-area-new))

(defun gtk-drawing-area-new ()
  (make-instance 'gtk-drawing-area))

(export 'gtk-drawing-area-new)

;;; --- End of file gtk.drawing-area.lisp --------------------------------------
