;;; ----------------------------------------------------------------------------
;;; gtk.gesture-rotate.lisp
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
;;; GtkGestureRotate
;;;
;;;     Rotate gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureRotate
;;;
;;; Functions
;;;
;;;     gtk_gesture_rotate_new
;;;     gtk_gesture_rotate_get_angle_delta
;;;
;;; Signals
;;;
;;;     void    angle-changed    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureRotate
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureRotate
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureRotate" gtk-gesture-rotate
  (:superclass gtk-gesture
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_rotate_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-gesture-rotate 'type)
 "@version{2019-3-23}
  @begin{short}
    @sym{gtk-gesture-rotate} is a @class{gtk-gesture} implementation able to
    recognize 2-finger rotations, whenever the angle between both handled
    sequences changes, the \"angle-changed\" signal is emitted.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"angle-changed\" signal}
    @begin{pre}
  lambda (gesture angle angle-delta)    : Run First
    @end{pre}
    This signal is emitted when the angle between both tracked points changes.
    @begin[code]{table}
      @entry[gesture]{The @class{gtk-gesture-rotate} object which
        received the signal.}
      @entry[angle]{Current angle in radians.}
      @entry[angle-delta]{Difference with the starting angle, in radians.}
    @end{table}
  @end{dictionary}
  @see-class{gtk-gesture-zoom}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_rotate_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-gesture-rotate-new))

(defun gtk-gesture-rotate-new (widget)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-23}
  @argument[widget]{a @class{gtk-widget}}
  @return{A newly created @class{gtk-gesture-rotate}.}
  @begin{short}
    Returns a newly created @class{gtk-gesture} that recognizes 2-touch rotation
    gestures.
  @end{short}

  Since 3.14
  @see-class{gtk-gesture-rotate}"
  (make-instance 'gtk-gesture-rotate
                 :widget widget))

(export 'gtk-gesture-rotate-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_rotate_get_angle_delta ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_rotate_get_angle_delta"
          gtk-gesture-rotate-get-angle-delta) :double
 #+cl-cffi-gtk-documentation
 "@version{2020-1-24}
  @argument[gesture]{A @class{gtk-gesture-rotate} object.}
  @return{The angle delta of type @code{:double} in radians.}
  @begin{short}
    If the gesture is active, this function returns the angle difference in
    radians since the gesture was first recognized.
  @end{short}
  If gesture is not active, 0.0d0 is returned.

  Since 3.14
  @see-class{gtk-gesture-rotate}"
  (gesture (g-object gtk-gesture-rotate)))

(export 'gtk-gesture-rotate-get-angle-delta)

;;; --- End of file gtk.gesture-rotate.lisp ------------------------------------
