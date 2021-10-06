;;; ----------------------------------------------------------------------------
;;; gtk.gesture-multi-press.lisp
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
;;; GtkGestureMultiPress
;;;
;;;     Multipress gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureMultiPress
;;;
;;; Functions
;;;
;;;     gtk_gesture_multi_press_new
;;;     gtk_gesture_multi_press_set_area
;;;     gtk_gesture_multi_press_get_area
;;;
;;; Signals
;;;
;;;     void    pressed     Run Last
;;;     void    released    Run Last
;;;     void    stopped     Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureMultiPress
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureMultiPress
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureMultiPress" gtk-gesture-multi-press
  (:superclass gtk-gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_multi_press_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-gesture-multi-press 'type)
 "@version{2020-9-11}
  @begin{short}
    @sym{gtk-gesture-multi-press} is a @class{gtk-gesture} implementation able
    to recognize multiple clicks on a nearby zone, which can be listened for
    through the \"pressed\" signal.
  @end{short}
  Whenever time or distance between clicks exceed the GTK defaults, \"stopped\"
  is emitted, and the click counter is reset.

  Callers may also restrict the area that is considered valid for a > 1
  touch/button press through the function
  @fun{gtk-gesture-multi-press-area}, so any click happening outside that area
  is considered to be a first click of its own.
  @begin[Signal Details]{dictionary}
    @subheading{The \"pressed\" signal}
      @begin{pre}
 lambda (gesture n-press x y)    :run-last
      @end{pre}
      The signal is emitted whenever a button or touch press happens.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture-multi-press} object which
        received the signal.}
        @entry[n-press]{An integer with how many touch/button presses happened
          with this one.}
        @entry[x]{A double float with the x coordinate, in widget allocation
          coordinates.}
        @entry[y]{A double float with the y coordinate, in widget allocation
          coordinates.}
      @end{table}
    @subheading{The \"released\" signal}
      @begin{pre}
 lambda (gesture n-press x y)    :run-last
      @end{pre}
      The signal is emitted when a button or touch is released. The
      @arg{n-press} argument will report the number of press that is paired to
      this event, note that the \"stopped\" signal may have been emitted between
      the press and its release, the @arg{n-press} argument will only start over
      at the next press.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture-multi-press} object which
        received the signal.}
        @entry[n-press]{An integer with the number of press that is paired with
          this release.}
        @entry[x]{A double float with the x coordinate, in widget allocation
          coordinates.}
        @entry[y]{A double float with the y coordinate, in widget allocation
          coordinates.}
      @end{table}
    @subheading{The \"stopped\" signal}
      @begin{pre}
 lambda (gesture)    :run-last
      @end{pre}
      The signal is emitted whenever any time/distance threshold has been
      exceeded.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk-gesture-multi-press} object which
        received the signal.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_multi_press_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-gesture-multi-press-new))

(defun gtk-gesture-multi-press-new (widget)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[widget]{a @class{gtk-widget} object}
  @return{A newly created @class{gtk-gesture-multi-press} object.}
  @begin{short}
    Returns a newly created gesture that recognizes single and multiple presses.
  @end{short}
  @see-function{gtk-gesture-multi-press}"
  (make-instance 'gtk-gesture-multi-press
                 :widget widget))

(export 'gtk-gesture-multi-press-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_multi_press_get_area ()
;;; gtk_gesture_multi_press_set_area () -> gtk-gesture-multi-press-area
;;; ----------------------------------------------------------------------------

(defun (setf gtk-gesture-multi-press-area) (rect gesture)
  (foreign-funcall "gtk_gesture_multi_press_set_area"
                   (g-object gtk-gesture-multi-press) gesture
                   (g-boxed-foreign gdk-rectangle) rect
                   :void)
  rect)

(defcfun ("gtk_gesture_multi_press_get_area" %gtk-gesture-multi-press-area)
    :boolean
  (gesture (g-object gtk-gesture-multi-press))
  (rect (g-boxed-foreign gdk-rectangle)))

(defun gtk-gesture-multi-press-area (gesture)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @syntax[]{(gtk-gesture-multi-press-area gesture) => rect}
  @syntax[]{(setf (gtk-gesture-multi-press-area gesture) rect)}
  @argument[gesture]{a @class{gtk-gesture-multi-press} object}
  @argument[rect]{a @class{gdk-rectangle} with the press area}
  @begin{short}
    Accessor of the press area of the gesture.
  @end{short}

  The function @sym{gtk-gesture-multi-press-area} gets the press area. The
  function @sym{(setf gtk-gesture-multi-press-area)} sets the press area.

  If @arg{rect} is non-@code{nil}, the press area will be checked to be confined
  within the rectangle, otherwise the button count will be reset so the press is
  seen as being the first one. If @arg{rect} is @code{nil}, the area will be
  reset to an unrestricted state.

  Note: The rectangle is only used to determine whether any non-first click
  falls within the expected area. This is not akin to an input shape.
  @see-class{gtk-gesture-multi-press}
  @see-class{gdk-rectangle}"
  (let ((rect (gdk-rectangle-new)))
    (when (%gtk-gesture-multi-press-area gesture rect)
      rect)))

(export 'gtk-gesture-multi-press-area)

;;; --- End of file gtk-gesture-multi-press.lisp -------------------------------
