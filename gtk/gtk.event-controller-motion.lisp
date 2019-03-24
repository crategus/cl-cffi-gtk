;;; ----------------------------------------------------------------------------
;;; gtk.event-controller-motion.lisp
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
;;; GtkEventControllerMotion
;;;
;;;     Event controller for motion events
;;;
;;; Types and Values
;;;
;;;     GtkEventControllerMotion
;;;
;;; Functions
;;;
;;;     gtk_event_controller_motion_new
;;;
;;; Signals
;;;
;;;     void  enter   Run First
;;;     void  leave   Run First
;;;     void  motion  Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkEventControllerMotion
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEventControllerMotion
;;; ----------------------------------------------------------------------------

#+gtk-3-24
(define-g-object-class "GtkEventControllerMotion" gtk-event-controller-motion
  (:superclass gtk-event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_motion_get_type")
  nil)

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (documentation 'gtk-event-controller-motion 'type)
 "@version{2019-3-17}
  @begin{short}
    @sym{gtk-event-controller-motion} is an event controller meant for
    situations where you need to track the position of the pointer.
  @end{short}

  This object was added in 3.24.

  @begin[Signal Details]{dictionary}
    @subheading{The \"enter\" signal}
      @begin{pre}
  lambda (controller x y)    : Run First
      @end{pre}
      Signals that the pointer has entered the widget.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk-event-controller-motion} object that
          received the signal.}
        @entry[x]{the x coordinate}
        @entry[y]{the y coordinate}
      @end{table}
      Since 3.24

    @subheading{The \"leave\" signal}
      @begin{pre}
  lambda (controller)    : Run First
      @end{pre}
      Signals that pointer has left the widget.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk-event-controller-motion} object that
          received the signal.}
      @end{table}
      Since 3.24

    @subheading{The \"motion\" signal}
      @begin{pre}
  lambda (controller x y)    : Run First
      @end{pre}
      Emitted when the pointer moves inside the widget.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk-event-controller-motion} object that
          received the signal.}
        @entry[x]{the x coordinate}
        @entry[y]{the y coordinate}
      @end{table}
      Since 3.24
  @end{dictionary}
  @see-class{gtk-event-controller}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_motion_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-24
(declaim (inline gtk-event-controller-motion-new))

#+gtk-3-24
(defun gtk-event-controller-motion-new (widget)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-17}
  @argument[widget]{a @class{gtk-widget} object}
  @return{The new @class{gtk-event-controller-motion} objekt.}
  @begin{short}
    Creates a new event controller that will handle motion events for the given
    @arg{widget}.
  @end{short}

  Since 3.24
  @see-class{gtk-event-controller-motion}"
  (make-instance 'gtk-event-controller-motion
                 :widget widget))

#+gtk-3-24
(export 'gtk-event-controller-motion-new)

;;; --- End of file gtk.event-controller-motion.lisp ---------------------------
