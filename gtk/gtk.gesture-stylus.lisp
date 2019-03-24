;;; ----------------------------------------------------------------------------
;;; gtk.gesture-stylus.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;; GtkGestureStylus
;;;
;;;     Gesture for stylus input
;;;
;;; Types and Values
;;;
;;;     GtkGestureStylus
;;;
;;; Functions
;;;
;;;     gtk_gesture_stylus_new
;;;     gtk_gesture_stylus_get_axis
;;;     gtk_gesture_stylus_get_axes
;;;     gtk_gesture_stylus_get_device_tool
;;;
;;; Signals
;;;
;;;     void  down       Run Last
;;;     void  motion     Run Last
;;;     void  proximity  Run Last
;;;     void  up         Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureStylus
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureStylus
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureStylus" gtk-gesture-stylus
  (:superclass gtk-gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_stylus_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-gesture-stylus 'type)
 "@version{2019-3-23}
  @begin{short}
    @sym{gtk-gesture-stylus} is a @class{gtk-gesture} implementation specific
    to stylus input.
  @end{short}
  The provided signals just provide the basic information.

  Since 3.14


  @begin[Signal Details]{dictionary}

    @subheading{The \"down\" signal}
    @begin{pre}
  lambda (gesture arg1 arg2)    : Run Last
    @end{pre}
    @begin[code]{table}
      @entry[gesture]{The @class{gtk-gesture-stylus} object on which the signal
        is emitted.}
      @entry[arg1]{A not documented @code{:double}.}
      @entry[arg2]{A not documented @code{:double}.}
    @end{table}
    Since 3.14

    @subheading{The \"motion\" signal}
    @begin{pre}
  lambda (gesture arg1 arg2)    : Run Last
    @end{pre}
    @begin[code]{table}
      @entry[gesture]{The @class{gtk-gesture-stylus} object on which the signal
        is emitted.}
      @entry[arg1]{A not documented @code{:double}.}
      @entry[arg2]{A not documented @code{:double}.}
    @end{table}
    Since 3.14

    @subheading{The \"proximity\" signal}
    @begin{pre}
  lambda (gesture arg1 arg2)    : Run Last
    @end{pre}
    @begin[code]{table}
      @entry[gesture]{The @class{gtk-gesture-stylus} object on which the signal
        is emitted.}
      @entry[arg1]{A not documented @code{:double}.}
      @entry[arg2]{A not documented @code{:double}.}
    @end{table}
    Since 3.14

    @subheading{The \"up\" signal}
    @begin{pre}
  lambda (gesture arg1 arg2)    : Run Last
    @end{pre}
    @begin[code]{table}
      @entry[gesture]{The @class{gtk-gesture-stylus} object on which the signal
        is emitted.}
      @entry[arg1]{A not documented @code{:double}.}
      @entry[arg2]{A not documented @code{:double}.}
    @end{table}
    Since 3.14
  @end{dictionary}

  @see-class{gtk-gesture}
  @see-class{gtk-gesture-single}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_new ()
;;;
;;; GtkGesture *
;;; gtk_gesture_stylus_new (GtkWidget *widget);
;;;
;;; Creates a new GtkGestureStylus.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     a newly created stylus gesture
;;;
;;; Since: 3.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_axis ()
;;;
;;; gboolean
;;; gtk_gesture_stylus_get_axis (GtkGestureStylus *gesture,
;;;                              GdkAxisUse axis,
;;;                              gdouble *value);
;;;
;;; Returns the current value for the requested axis . This function must be
;;; called from either the “down”, “motion”, “up” or “proximity” signals.
;;;
;;; gesture :
;;;     a GtkGestureStylus
;;;
;;; axis :
;;;     requested device axis
;;;
;;; value :
;;;     return location for the axis value.
;;;
;;; Returns :
;;;     TRUE if there is a current value for the axis
;;;
;;; Since: 3.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_axes ()
;;;
;;; gboolean
;;; gtk_gesture_stylus_get_axes (GtkGestureStylus *gesture,
;;;                              GdkAxisUse axes[],
;;;                              gdouble **values);
;;;
;;; Returns the current values for the requested axes . This function must be
;;; called from either the “down”, “motion”, “up” or “proximity” signals.
;;;
;;; gesture :
;;;     a GtkGestureStylus
;;;
;;; axes :
;;;     array of requested axes, terminated with GDK_AXIS_IGNORE.
;;;
;;; values :
;;;     return location for the axis values.
;;;
;;; Returns :
;;;     TRUE if there is a current value for the axes
;;;
;;; Since: 3.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_device_tool ()
;;;
;;; GdkDeviceTool *
;;; gtk_gesture_stylus_get_device_tool (GtkGestureStylus *gesture);
;;;
;;; Returns the GdkDeviceTool currently driving input through this gesture. This
;;; function must be called from either the “down”, “motion”, “up” or
;;; “proximity” signal handlers.
;;;
;;; gesture :
;;;     a GtkGestureStylus
;;;
;;; Returns :
;;;     The current stylus tool.
;;;
;;; Since: 3.24
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.gesture-stylus.lisp ------------------------------------
