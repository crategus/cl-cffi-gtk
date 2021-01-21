;;; ----------------------------------------------------------------------------
;;; gtk.gesture-drag.lisp
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
;;; GtkGestureDrag
;;;
;;;     Drag gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureDrag
;;;
;;; Functions
;;;
;;;     gtk_gesture_drag_new
;;;     gtk_gesture_drag_get_start_point
;;;     gtk_gesture_drag_get_offset
;;;
;;; Signals
;;;
;;;     void    drag-begin	   Run Last
;;;     void    drag-end	   Run Last
;;;     void    drag-update    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureDrag
;;;                     ╰── GtkGesturePan
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureDrag
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureDrag" gtk-gesture-drag
  (:superclass gtk-gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_drag_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-gesture-drag 'type)
 "@version{2020-9-11}
  @begin{short}
    @sym{gtk-gesture-drag} is a @class{gtk-gesture} implementation that
    recognizes drag operations.
  @end{short}
  The drag operation itself can be tracked throught the \"drag-begin\",
  \"drag-update\" and \"drag-end\" signals, or the relevant coordinates be
  extracted through the functions @fun{gtk-gesture-drag-offset} and
  @fun{gtk-gesture-drag-start-point}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"drag-begin\" signal}
      @begin{pre}
 lambda (gesture start-x start-y)    : Run Last
      @end{pre}
      This signal is emitted whenever dragging starts.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture-drag} object which received the
          signal.}
        @entry[start-x]{A @code{:double} with the x coordinate, relative to the
          widget allocation.}
        @entry[start-y]{A @code{:double} with the y coordinate, relative to the
          widget allocation.}
      @end{table}
    @subheading{The \"drag-end\" signal}
      @begin{pre}
 lambda (gesture offset-x offset-y)    : Run Last
      @end{pre}
      This signal is emitted whenever the dragging is finished.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture-drag} object which received the
          signal.}
        @entry[offset-x]{A @code{:double} with the x offset, relative to the
          start point.}
        @entry[offset-y]{A @code{:double} with the y offset, relative to the
          start point.}
      @end{table}
    @subheading{The \"drag-update\" signal}
      @begin{pre}
 lambda (gesture offset-x offset-y)    : Run Last
      @end{pre}
      This signal is emitted whenever the dragging point moves.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture-drag} object which received the
          signal.}
        @entry[offset-x]{A @code{:double} with the x offset, relative to the
          start point.}
        @entry[offset-y]{A @code{:double} with the y offset, relative to the
          start point.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-gesture-swipe}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_drag_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-gesture-drag-new))

(defun gtk-gesture-drag-new (widget)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[widget]{a @class{gtk-widget} object}
  @return{A newly created @class{gtk-gesture-drag}.}
  @short{Returns a newly created gesture that recognizes drags.}
  @see-class{gtk-gesture-drag}"
  (make-instance 'gtk-gesture-drag
                 :widget widget))

(export 'gtk-gesture-drag-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_drag_get_start_point () -> gtk-gesture-drag-start-point
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_drag_get_start_point" %gtk-gesture-drag-start-point)
    :boolean
  (gesture (g-object gtk-gesture-drag))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gtk-gesture-drag-start-point (gesture)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[gesture]{a @class{gtk-gesture-drag} object}
  @begin{return}
    x -- a @code{:double} with the x coordinate for the drag start point @br{}
    y -- a @code{:double} with the y coordinate for the drag start point
  @end{return}
  @begin{short}
    If the gesture is active, this function returns the drag start coordinates,
    in window-relative coordinates.
  @end{short}
  @see-class{gtk-gesture-drag}"
  (with-foreign-objects ((x :double) (y :double))
    (when (%gtk-gesture-drag-start-point gesture x y)
      (values (mem-ref x :double)
              (mem-ref y :double)))))

(export 'gtk-gesture-drag-start-point)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_drag_get_offset () -> gtk-gesture-drag-offset
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_drag_get_offset" %gtk-gesture-drag-offset) :boolean
  (gesture (g-object gtk-gesture-drag))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gtk-gesture-drag-offset (gesture)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[gesture]{a @class{gtk-gesture-drag} object}
  @begin{return}
    x -- a @code{:double} with the x offset for the current point @br{}
    y -- a @code{:double} with the y offset for the current point.
  @end{return}
  @begin{short}
    If the gesture is active, this function returns the coordinates of the
    current point, as an offset to the starting drag point.
  @end{short}
  @see-class{gtk-gesture-drag}"
  (with-foreign-objects ((x :double) (y :double))
    (when (%gtk-gesture-drag-offset gesture x y)
      (values (mem-ref x :double)
              (mem-ref y :double)))))

(export 'gtk-gesture-drag-offset)

;;; --- End of file gtk.gesture-drag.lisp --------------------------------------
