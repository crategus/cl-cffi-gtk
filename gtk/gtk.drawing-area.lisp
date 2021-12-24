;;; ----------------------------------------------------------------------------
;;; gtk.drawing-area.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
 "@version{*2021-11-30}
  @begin{short}
    The @sym{gtk-drawing-area} widget is used for creating custom user
    interface elements. It is essentially a blank widget. You can draw on it.
  @end{short}
  After creating a drawing area, the application may want to connect to:
  @begin{itemize}
    @begin{item}
      Mouse and button press signals to respond to input from the user. Use the
      @fun{gtk-widget-add-events} function to enable events you wish to receive.
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
  Draw signals are normally delivered when a drawing area first comes onscreen,
  or when it is covered by another window and then uncovered. You can also force
  an expose event by adding to the \"damage region\" of the drawing area's
  window. The @fun{gtk-widget-queue-draw-area} and
  @fun{gdk-window-invalidate-rect} functions are equally good ways to do this.
  You will then get a draw signal for the invalid region.

  To receive mouse events on a drawing area, you will need to enable them with
  the @fun{gtk-widget-add-events} function. To receive keyboard events, you
  will need to set the @slot[gtk-widget]{can-focus} property on the drawing
  area, and you should probably draw some user visible indication that the
  drawing area is focused. Use the @fun{gtk-widget-has-focus} function in your
  expose event handler to decide whether to draw the focus indicator. See the
  @see-function{gtk-render-focus} function for one way to draw focus.
  @begin[Example]{dictionary}
    The following example demonstrates using a drawing area to display a
    circle in the normal widget foreground color.

    Note that GDK automatically clears the exposed area before sending the
    expose event, and that drawing is implicitly clipped to the exposed area.
    If you want to have a theme-provided background, you need to call the
    @fun{gtk-render-background} function in your \"draw\" signal handler.
    @begin{pre}
(defun example-drawing-area ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title \"Example Drawing Area\"
                                 :default-width 400
                                 :default-height 300))
          ;; Create the drawing area
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area \"draw\"
          (lambda (widget cr)
            (let* ((cr (pointer cr))
                   (width (gtk-widget-allocated-width widget))
                   (height (gtk-widget-allocated-height widget))
                   (context (gtk-widget-style-context widget))
                   (color (gtk-style-context-color context :focused)))
                ;; Set the color from the style context of the widget
                (gdk-cairo-set-source-rgba cr color)
                ;; Draw and fill a circle on the drawing area
                (cairo-arc cr
                           (/ width 2.0)
                           (/ height 2.0)
                           (- (/ (min width height) 2.0) 12)
                           0.0
                           (* 2.0 pi))
                (cairo-fill cr)
                ;; Destroy the Cairo context
                (cairo-destroy cr))))
      ;; Signal handler for the window to handle the signal \"destroy\"
      (g-signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window
      (gtk-container-add window area)
      (gtk-widget-show-all window))))
    @end{pre}
  @end{dictionary}
  @see-function{gtk-widget-add-events}
  @see-function{gtk-widget-queue-draw-area}
  @see-function{gdk-window-invalidate-rect}
  @see-function{gtk-widget-has-focus}
  @see-function{gtk-render-focus}
  @see-function{gtk-render-background}")

;;; ----------------------------------------------------------------------------
;;; gtk_drawing_area_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-drawing-area-new))

(defun gtk-drawing-area-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-11-30}
  @return{A new @class{gtk-drawing-area} widget.}
  @begin{short}
    Creates a new drawing area.
  @end{short}
  @see-class{gtk-drawing-area}"
  (make-instance 'gtk-drawing-area))

(export 'gtk-drawing-area-new)

;;; --- End of file gtk.drawing-area.lisp --------------------------------------
