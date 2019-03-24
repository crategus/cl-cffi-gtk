;;; ----------------------------------------------------------------------------
;;; gtk.gesture-drag.lisp
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
;;;     void	drag-begin	Run Last
;;;     void	drag-end	Run Last
;;;     void	drag-update	Run Last
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
 "@version{2019-3-22}
  @begin{short}
    @sym{gtk-gesture-drag} is a @class{gtk-gesture} implementation that
    recognizes drag operations.
  @end{short}
  The drag operation itself can be tracked throught the \"drag-begin\",
  \"drag-update\" and \"drag-end\" signals, or the relevant coordinates be
  extracted through the functions @fun{gtk-gesture-drag-get-offset} and
  @fun{gtk-gesture-drag-get-start-point}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"drag-begin\" signal}
      @begin{pre}
  lambda (gesture start-x start-y)    : Run Last
      @end{pre}
      This signal is emitted whenever dragging starts.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk-gesture-drag} object which received the
          signal.}
        @entry[start-x]{X coordinate, relative to the widget allocation.}
        @entry[start-y]{Y coordinate, relative to the widget allocation.}
      @end{table}
      Since 3.14

    @subheading{The \"drag-end\" signal}
      @begin{pre}
  lambda (gesture offset-x offset-y)    : Run Last
      @end{pre}
      This signal is emitted whenever the dragging is finished.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk-gesture-drag} object which received the
          signal.}
        @entry[offset-x]{X offset, relative to the start point.}
        @entry[offset-y]{Y offset, relative to the start point.}
      @end{table}
      Since 3.14

    @subheading{The \"drag-update\" signal}
      @begin{pre}
  lambda (gesture offset-x offset-y)    : Run Last
      @end{pre}
      This signal is emitted whenever the dragging point moves.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk-gesture-drag} object which received the
          signal.}
        @entry[offset-x]{X offset, relative to the start point.}
        @entry[offset-y]{Y offset, relative to the start point.}
      @end{table}
  @end{dictionary}
  Since 3.14
  @see-class{gtk-gesture-swipe}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_drag_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-gesture-drag-new))

(defun gtk-gesture-drag-new (widget)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-22}
  @argument[widget]{a @class{gtk-widget}}
  @return{A newly created @class{gtk-gesture-drag}.}
  @short{Returns a newly created @class{gtk-gesture} that recognizes drags.}

  Since 3.14
  @see-class{gtk-gesture-drag}"
  (make-instance 'gtk-gesture-drag
                 :widget widget))

(export 'gtk-gesture-drag-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_drag_get_start_point ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_drag_get_start_point"
           gtk-gesture-drag-get-start-point) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2019-3-22}
  @argument[gesture]{a @class{gtk-gesture-drag} object}
  @argument[x]{X coordinate for the drag start point.}
  @argument[y]{Y coordinate for the drag start point.}
  @return{@em{True} if the gesture is active.}
  @begin{short}
    If the gesture is active, this function returns @em{true} and fills in x
    and y with the drag start coordinates, in window-relative coordinates.
  @end{short}

  Since 3.14
  @see-class{gtk-gesture-drag}"
  (gesture (g-object gtk-gesture-drag))
  (x :double)
  (y :double))

(export 'gtk-gesture-drag-get-start-point)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_drag_get_offset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_drag_get_offset"
           gtk-gesture-drag-get-offset) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2019-3-22}
  @argument[gesture]{a @class{gtk-gesture-drag} object}
  @argument[x]{X offset for the current point.}
  @argument[y]{Y offset for the current point.}
  @return{@em{True} if the gesture is active.}
  @begin{short}
    If the gesture is active, this function returns @em{true} and fills in x
    and y with the coordinates of the current point, as an offset to the
    starting drag point.
  @end{short}

  Since 3.14
  @see-class{gtk-gesture-drag}"
  (gesture (g-object gtk-gesture-drag))
  (x :double)
  (y :double))

(export 'gtk-gesture-drag-get-offset)

;;; --- End of file gtk.gesture-drag.lisp --------------------------------------
