;;; ----------------------------------------------------------------------------
;;; gtk.gesture-swipe.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2021 Dieter Kaiser
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
;;; GtkGestureSwipe
;;;
;;;     Swipe gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureSwipe
;;;
;;; Functions
;;;
;;;     gtk_gesture_swipe_new ()
;;;     gtk_gesture_swipe_get_velocity ()
;;;
;;; Signals
;;;
;;;     void    swipe    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureSwipe
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureSwipe
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureSwipe" gtk-gesture-swipe
  (:superclass gtk-gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_swipe_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-gesture-swipe 'type)
 "@version{2020-9-11}
  @begin{short}
    @sym{gtk-gesture-swipe} is a @class{gtk-gesture} implementation able to
    recognize swipes, after a press/move/.../move/release sequence happens, the
    \"swipe\" signal will be emitted, providing the velocity and directionality
    of the sequence at the time it was lifted.
  @end{short}

  If the velocity is desired in intermediate points, the function
  @fun{gtk-gesture-swipe-velocity} can be called on e.g. a \"update\" handler.

  All velocities are reported in pixels/sec units.
  @begin[Signal Details]{dictionary}
    @subheading{The \"swipe\" signal}
    @begin{pre}
 lambda (gesture velocity-x velocity-y)    :run-last
    @end{pre}
    The signal is emitted when the recognized gesture is finished, velocity
    and direction are a product of previously recorded events.
    @begin[code]{table}
      @entry[gesture]{The @sym{gtk-gesture-multi-press} object which
        received the signal.}
      @entry[velocity-x]{A double float with the velocity in the x axis,
        in pixels/sec.}
      @entry[velocity-y]{A double float with the velocity in the y axis,
        in pixels/sec.}
    @end{table}
  @end{dictionary}
  @see-class{gtk-gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_swipe_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-gesture-swipe-new))

(defun gtk-gesture-swipe-new (widget orientation)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[widget]{a @class{gtk-widget} object}
  @return{A newly created @class{gtk-gesture-swipe} object.}
  @begin{short}
    Returns a newly created gesture that recognizes swipes.
  @end{short}
  @see-class{gtk-gesture-swipe}"
  (make-instance 'gtk-gesture-swipe
                 :widget widget
                 :orientation orientation))

(export 'gtk-gesture-pan-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_swipe_get_velocity () ->gtk-gesture-swipe-velocity
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_swipe_get_velocity" %gtk-gesture-swipe-velocity) :boolean
  (gesture (g-object gtk-gesture-swipe))
  (velocity-x (:pointer :double))
  (velocity-y (:pointer :double)))

(defun gtk-gesture-swipe-velocity (gesture)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[gesture]{a @class{gtk-gesture-swipe} object}
  @begin{return}
    @code{velocity-x} -- a @code{:double} value for the velocity in the x axis,
                         in pixels/sec. @br{}
    @code{velocity-y} -- a @code{:double} value for the velocity in the y axis,
                         in pixels/sec.
  @end{return}
  @begin{short}
    If the gesture is recognized, this function returns the recorded velocity,
    as per the last event(s) processed.
  @end{short}
  @see-class{gtk-gesture-swipe}"
  (with-foreign-objects ((velocity-x :double) (velocity-y :double))
    (when (%gtk-gesture-swipe-velocity gesture velocity-x velocity-y)
      (values (mem-ref velocity-x :double)
              (mem-ref velocity-y :double)))))

(export 'gtk-gesture-swipe-velocity)

;;; --- End of file gtk.gesture-swipe.lisp -------------------------------------
