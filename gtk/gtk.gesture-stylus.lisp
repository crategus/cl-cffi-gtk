;;; ----------------------------------------------------------------------------
;;; gtk.gesture-stylus.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2020 Dieter Kaiser
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
;;;     void    down         Run Last
;;;     void    motion       Run Last
;;;     void    proximity    Run Last
;;;     void    up           Run Last
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
 "@version{2020-9-11}
  @begin{short}
    @sym{gtk-gesture-stylus} is a @class{gtk-gesture} implementation specific
    to stylus input.
  @end{short}
  The provided signals just provide the basic information.
  @begin[Signal Details]{dictionary}
    @subheading{The \"down\" signal}
    @begin{pre}
 lambda (gesture arg1 arg2)    : Run Last
    @end{pre}
    @begin[code]{table}
      @entry[gesture]{The @sym{gtk-gesture-stylus} object on which the signal
        is emitted.}
      @entry[arg1]{A not documented @code{:double}.}
      @entry[arg2]{A not documented @code{:double}.}
    @end{table}
    @subheading{The \"motion\" signal}
    @begin{pre}
  lambda (gesture arg1 arg2)    : Run Last
    @end{pre}
    @begin[code]{table}
      @entry[gesture]{The @sym{gtk-gesture-stylus} object on which the signal
        is emitted.}
      @entry[arg1]{A not documented @code{:double}.}
      @entry[arg2]{A not documented @code{:double}.}
    @end{table}
    @subheading{The \"proximity\" signal}
    @begin{pre}
  lambda (gesture arg1 arg2)    : Run Last
    @end{pre}
    @begin[code]{table}
      @entry[gesture]{The @sym{gtk-gesture-stylus} object on which the signal
        is emitted.}
      @entry[arg1]{A not documented @code{:double}.}
      @entry[arg2]{A not documented @code{:double}.}
    @end{table}
    @subheading{The \"up\" signal}
    @begin{pre}
  lambda (gesture arg1 arg2)    : Run Last
    @end{pre}
    @begin[code]{table}
      @entry[gesture]{The @sym{gtk-gesture-stylus} object on which the signal
        is emitted.}
      @entry[arg1]{A not documented @code{:double}.}
      @entry[arg2]{A not documented @code{:double}.}
    @end{table}
  @end{dictionary}
  @see-class{gtk-gesture}
  @see-class{gtk-gesture-single}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-gesture-stylus-new (widget)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[widget]{a @class{gtk-gesture-stylus} object}
  @return{A newly created @class{gtk-gesture-stylus} object.}
  @begin{short}
    Creates a new stylus gesture.
  @end{short}

  Since 3.24
  @see-class{gtk-gesture-stylus}"
  (make-instance 'gtk-gesture-stylus
                 :widget widget))

(export 'gtk-gesture-stylus-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_axis () -> gtk-gesture-stylus-axis
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_stylus_get_axis" %gtk-gesture-stylus-axis) :boolean
  (gesture (g-object gtk-gesture-stylus))
  (axis gdk-axis-use)
  (value (:pointer :double)))

(defun gtk-gesture-stylus-axis (gesture axis)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[gesture]{a @class{gtk-gesture-stylus} object}
  @argument[axis]{requested device axis of type @symbol{gdk-axis-use}}
  @return{A @code{:double} with the current value for the axis.}
  @begin{short}
    Returns the current value for the requested axis.
  @end{short}
  This function must be called from either the \"down\", \"motion\", \"up\" or
  \"proximity\" signals.

  Since 3.24
  @see-class{gtk-gesture-stylus}"
  (with-foreign-object (value :double)
    (when (%gtk-gesture-stylus-axis gesture axis value)
      (mem-ref value :double))))

(export 'gtk-gesture-stylus-axis)

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
;;; Since 3.24
;;; ----------------------------------------------------------------------------

;; TODO: Implement the function

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_device_tool () -> gtk-gesture-stylus-device-tool
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_stylus_get_device_tool" gtk-gesture-stylus-device-tool)
    (g-object gdk-device-tool)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[gesture]{a @class{gtk-gesture-stylus} object}
  @return{The current @class{gdk-device-tool} object.}
  @begin{short}
    Returns the device tool currently driving input through this gesture.
  @end{short}
  This function must be called from either the \"down\", \"motion\", \"up\" or
  \"proximity\" signal handlers.

  Since 3.24
  @see-class{gtk-gesture-stylus}"
  (gesture (g-object gtk-gesture-stylus)))

(export 'gtk-gesture-stylus-device-tool)

;;; --- End of file gtk.gesture-stylus.lisp ------------------------------------
