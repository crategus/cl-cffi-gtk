;;; ----------------------------------------------------------------------------
;;; gtk.gesture-long-press.lisp
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
;;; GtkGestureLongPress
;;;
;;;     "Press and Hold" gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureLongPress
;;;
;;; Functions
;;;
;;;     gtk_gesture_long_press_new ()
;;;
;;; Properties
;;;
;;;     gdouble  delay-factor  Read / Write
;;;
;;; Signals
;;;
;;;     void  cancelled  Run Last
;;;     void   pressed   Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureLongPress
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureLongPress
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureLongPress" gtk-gesture-long-press
  (:superclass gtk-gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_long_press_get_type")
  ((delay-factor
    gtk-gesture-long-press-delay-factor
    "delay-factor" "gdouble" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-gesture-long-press 'type)
 "@version{2019-3-23}
  @begin{short}
    @sym{gtk-gesture-long-press} is a @class{gtk-gesture} implementation able to
    recognize long presses, triggering the \"pressed\" after the timeout is
    exceeded.
  @end{short}

  If the touchpoint is lifted before the timeout passes, or if it drifts too far
  of the initial press point, the \"cancelled\" signal will be emitted.
  @begin[Signal Details]{dictionary}
    @subheading{The \"cancelled\" signal}
      @begin{pre}
  lambda (gesture)    : Run Last
      @end{pre}
      This signal is emitted whenever a press moved too far, or was released
      before \"pressed\" happened.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk-gesture-long-press} object which received
          the signal.}
      @end{table}
      Since 3.14

    @subheading{The \"pressed\" signal}
      @begin{pre}
  lambda (gesture x y)    : Run Last
      @end{pre}
      This signal is emitted whenever a press goes unmoved/unreleased longer
      than what the GTK+ defaults tell.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk-gesture-long-press} object which received
          the signal.}
        @entry[x]{The X coordinate where the press happened, relative to the
          widget allocation.}
        @entry[y]{The Y coordinate where the press happened, relative to the
          widget allocation.}
      @end{table}
  @end{dictionary}
  Since 3.14
  @see-slot{gtk-gesture-long-press-delay-factor}
  @see-class{gtk-gesture}")

;;; --- gtk-gesture-long-press-delay-factor ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "delay-factor"
                      'gtk-gesture-long-press) 't)
 "The @code{delay-factor} property of type @code{:double} (Read / Write) @br{}
  Factor by which to modify the default timeout. @br{}
  Allowed values: [0.5,2] @br{}
  Default value: 1 @br{}
  Since 3.14")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-gesture-long-press-delay-factor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-gesture-long-press-delay-factor 'function)
 "@version{2019-3-23}
  @syntax[]{(gtk-gesture-long-press-delay-factor object) => delay-factor)}
  @syntax[]{(setf (gtk-gesture-long-press-delay-factor object) delay-factor)}
  @argument[object]{a @class{gtk-gesture-long-press} object}
  @argument[delay-factor]{the factor by which to modify the default timeout}
  @begin{short}
    Accessor of the slot @slot[gtk-gesture-long-press]{delay-factor} of the
    @class{gtk-gesture-long-press} class.
  @end{short}

  Since 3.14
  @see-class{gtk-gesture-long-press}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_long_press_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-gesture-long-press-new))

(defun gtk-gesture-long-press-new (widget)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-23}
  @argument[widget]{a @class{gtk-widget}}
  @return{A newly created @class{gtk-gesture-long-press}.}
  @begin{short}
    Returns a newly created @class{gtk-gesture} that recognizes long presses.
  @end{short}

  Since 3.14
  @see-function{gtk-gesture-long-press}"
  (make-instance 'gtk-gesture-long-press
                 :widget widget))

(export 'gtk-gesture-long-press-new)

;;; --- End of File gtk.gesture-long-press.lisp --------------------------------
