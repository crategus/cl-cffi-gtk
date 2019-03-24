;;; ----------------------------------------------------------------------------
;;; gtk.gesture-zoom.lisp
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
;;; GtkGestureZoom
;;;
;;;     Zoom gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureZoom
;;;
;;; Functions
;;;
;;;     gtk_gesture_zoom_new
;;;     gtk_gesture_zoom_get_scale_delta
;;;
;;; Signals
;;;
;;;     void  scale-changed  Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureZoom
;;; ----------------------------------------------------------------------------

(in-package :gtk)

(define-g-object-class "GtkGestureZoom" gtk-gesture-zoom
  (:superclass gtk-gesture
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_zoom_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-gesture-zoom 'type)
 "@version{2019-3-23}
  @begin{short}
    @sym{gtk-gesture-zoom} is a @class{gtk-gesture} implementation able to
    recognize pinch/zoom gestures, whenever the distance between both tracked
    sequences changes, the \"scale-changed\" signal is emitted to report the
    scale factor.
  @end{short}

  Since 3.14
  @begin[Signal Details]{dictionary}
    @subheading{The \"scale-changed\" signal}
    @begin{pre}
  lambda (gesture scale)    : Run First
    @end{pre}
    This signal is emitted whenever the distance between both tracked sequences
    changes.
    @begin[code]{table}
      @entry[gesture]{The @class{gtk-gesture-zoom} object on which the signal
        is emitted.}
      @entry[scale]{Scale delta, taking the initial state as 1:1.}
    @end{table}
    Since 3.14
  @end{dictionary}
  @see-class{gtk-gesture-rotate}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_zoom_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-gesture-zoom-new))

(defun gtk-gesture-zoom-new (widget)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-23}
  @argument[widget]{a @class{gtk-widget}}
  @return{A newly created @class{gtk-gesture-zoom}.}
  @begin{short}
    Returns a newly created @class{gtk-gesture} that recognizes zoom in/out
    gestures (usually known as pinch/zoom).
  @end{short}

  Since 3.14
  @see-class{gtk-gesture-rotate}"
  (make-instance 'gtk-gesture-zoom
                 :widget widget))

(export 'gtk-gesture-zoom-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_zoom_get_scale_delta ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_zoom_get_scale_delta"
           gtk-gesture-zoom-get-scale-delta) :double
 #+cl-cffi-gtk-documentation
 "@version{2019-3-23}
  @argument[widget]{A @class{gtk-gesture-zoom}.}
  @return{The scale delta.}
  @begin{short}
    If gesture is active, this function returns the zooming difference since
    the gesture was recognized (hence the starting point is considered 1:1).
  @end{short}
  If gesture is not active, 1 is returned.

  Since 3.14
  @see-class{gtk-gesture-rotate}"
  (gesture (g-object gtk-gesture-zoom)))

(export 'gtk-gesture-zoom-get-scale-delta)

;;; --- End of file gtk.gesture-zoom.lisp --------------------------------------
