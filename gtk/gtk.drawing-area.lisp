;;; ----------------------------------------------------------------------------
;;; gtk.drawing-area.lisp
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
;;; GtkDrawingArea
;;;
;;;     A widget for custom user interface elements
;;;
;;; Values and Types
;;;
;;;     GtkDrawingArea
;;;
;;; Functions
;;;
;;;     gtk_drawing_area_new
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkDrawingArea
;;;
;;; Implemented Interfaces
;;;
;;;     GtkDrawingArea implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkDrawingArea
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkDrawingArea" gtk-drawing-area
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_drawing_area_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-drawing-area 'type)
 "@version{2013-7-1}
  @begin{short}
    The @sym{gtk-drawing-area} widget is used for creating custom user interface
    elements. It is essentially a blank widget; you can draw on it.
  @end{short}
  After creating a drawing area, the application may want to connect to:
  @begin{itemize}
    @begin{item}
      Mouse and button press signals to respond to input from the user. Use the
      function @fun{gtk-widget-add-events} to enable events you wish to receive.
    @end{item}
    @begin{item}
      The \"realize\" signal to take any necessary actions when the widget is
      instantiated on a particular display. Create GDK resources in response
      to this signal.
    @end{item}
    @begin{item}
      The \"configure-event\" signal to take any necessary actions when the
      widget changes size.
    @end{item}
    @begin{item}
      The \"draw\" signal to handle redrawing the contents of the widget.
    @end{item}
  @end{itemize}
  The following code portion demonstrates using a drawing area to display a
  circle in the normal widget foreground color.

  Note that GDK automatically clears the exposed area to the background color
  before sending the expose event, and that drawing is implicitly clipped to
  the exposed area.

  @b{Example:} Simple @sym{gtk-drawing-area} usage
  @begin{pre}
 gboolean
 draw_callback (GtkWidget *widget, cairo_t *cr, gpointer data)
   {
     guint width, height;
     GdkRGBA color;

     width = gtk_widget_get_allocated_width (widget);
     height = gtk_widget_get_allocated_height (widget);
     cairo_arc (cr,
                width / 2.0, height / 2.0,
                MIN (width, height) / 2.0,
                0, 2 * G_PI);

     gtk_style_context_get_color (gtk_widget_get_style_context (widget),
                                  0,
                                  &color);
     gdk_cairo_set_source_rgba (cr, &color);

     cairo_fill (cr);

    return FALSE;
   @}
   [...]
   GtkWidget *drawing_area = gtk_drawing_area_new ();
   gtk_widget_set_size_request (drawing_area, 100, 100);
   g_signal_connect (G_OBJECT (drawing_area), \"draw\",
                     G_CALLBACK (draw_callback), NULL);
  @end{pre}
  Draw signals are normally delivered when a drawing area first comes
  onscreen, or when it is covered by another window and then uncovered. You can
  also force an expose event by adding to the \"damage region\" of the drawing
  area's window; the functions @fun{gtk-widget-queue-draw-area} and
  @fun{gdk-window-invalidate-rect} are equally good ways to do this. You will
  then get a draw signal for the invalid region.

  The available routines for drawing are documented on the GDK Drawing
  Primitives page and the cairo documentation.

  To receive mouse events on a drawing area, you will need to enable them with
  the function @fun{gtk-widget-add-events}. To receive keyboard events, you will
  need to set the @code{\"can-focus\"} property on the drawing area, and you
  should probably draw some user-visible indication that the drawing area is
  focused. Use the function @fun{gtk-widget-has-focus} in your expose event
  handler to decide whether to draw the focus indicator. See the function
  @see-function{gtk-render-focus} for one way to draw focus.
  @see-function{gtk-widget-add-events}
  @see-function{gtk-widget-queue-draw-area}
  @see-function{gdk-window-invalidate-rect}
  @see-function{gtk-widget-has-focus}
  @see-function{gtk-render-focus}")

;;; ----------------------------------------------------------------------------
;;; gtk_drawing_area_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-drawing-area-new))

(defun gtk-drawing-area-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-1-22}
  @return{A new @class{gtk-drawing-area} widget.}
  Creates a new drawing area.
  @see-class{gtk-drawing-area}"
  (make-instance 'gtk-drawing-area))

(export 'gtk-drawing-area-new)

;;; --- End of file gtk.drawing-area.lisp --------------------------------------
