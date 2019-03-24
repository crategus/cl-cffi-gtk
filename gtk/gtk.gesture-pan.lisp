;;; ----------------------------------------------------------------------------
;;; gtk.gesture-pan.lisp
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
;;; GtkGesturePan
;;;
;;;     Pan gesture
;;;
;;; Types and Values
;;;
;;;     GtkGesturePan
;;;     GtkPanDirection
;;;
;;; Functions
;;;
;;;     gtk_gesture_pan_new
;;;     gtk_gesture_pan_get_orientation                    Accessor
;;;     gtk_gesture_pan_set_orientation                    Accessor
;;;
;;; Properties
;;;
;;;     GtkOrientation  orientation  Read / Write
;;;
;;; Signals
;;;
;;;     void  pan  Run Last
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
;;; enum GtkPanDirection
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPanDirection" gtk-pan-direction
  (:export t
   :type-initializer "gtk_pan_direction_get_type")
  :left
  :right
  :up
  :down)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-pan-direction atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-pan-direction atdoc:*external-symbols*)
 "@version{2019-3-23}
  @begin{short}
    Describes the panning direction of a @class{gtk-gesture-pan}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPanDirection\" gtk-pan-direction
  (:export t
   :type-initializer \"gtk_pan_direction_get_type\")
  :left
  :right
  :up
  :down)
  @end{pre}
  @begin[code]{table}
    @entry[:left]{Panned towards the left.}
    @entry[:right]{Panned towards the right.}
    @entry[:up]{Panned upwards.}
    @entry[:down]{Panned downwards.}
  @end{table}
  Since 3.14
  @see-class{gtk-gesture-pan}")

;;; ----------------------------------------------------------------------------
;;; struct GtkGesturePan
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGesturePan" gtk-gesture-pan
  (:superclass gtk-gesture-drag
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_pan_get_type")
  ((orientation
    gtk-gesture-pan-orientation
    "orientation" "GtkOrientation" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-gesture-pan 'type)
 "@version{2019-3-23}
  @begin{short}
    @sym{gtk-gesture-pan} is a @class{gtk-gesture} implementation able to
    recognize pan gestures, those are drags that are locked to happen along one
    axis.
  @end{short}
  The axis that a @sym{gtk-gesture-pan} handles is defined at construct time,
  and can be changed through the slot access function
  @fun{gtk-gesture-pan-orientation}.

  When the gesture starts to be recognized, @sym{gtk-gesture-pan} will attempt
  to determine as early as possible whether the sequence is moving in the
  expected direction, and denying the sequence if this does not happen.

  Once a panning gesture along the expected axis is recognized, the \"pan\"
  signal will be emitted as input events are received, containing the offset in
  the given axis.

  @begin[Signal Details]{dictionary}
    @subheading{The \"pan\" signal}
    @begin{pre}
  lambda (gesture n-press x y)    : Run Last
    @end{pre}
    This signal is emitted once a panning gesture along the expected axis is
    detected.
    @begin[code]{table}
      @entry[gesture]{The @class{gtk-gesture-pan} object which received the
        signal.}
      @entry[direction]{}
      @entry[offset]{}
    @end{table}
  @end{dictionary}
  Since 3.14
  @see-slot{gtk-gesture-pan-orientation}
  @see-class{gtk-gesture}")

;;; --- gtk-gesture-pan-orientation --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "orientation" 'gtk-gesture-pan)
      't)
 "The @code{orientation} property of type @symbol{gtk-orientation}
  (Read / Write) @br{}
  The expected orientation of pan gestures. @br{}
  Default value: @code{:horizontal} @br{}
  Since 3.14")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-gesture-pan-orientation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-gesture-pan-orientation 'function)
 "@version{2019-3-23}
  @syntax[]{(gtk-gesture-pan-orientation object) => orientation)}
  @syntax[]{(setf (gtk-gesture-pan-orientation object) orientation)}
  @argument[object]{a @class{gtk-gesture} object}
  @argument[orientation]{expected orientation of type @symbol{gtk-orientation}}
  @begin{short}
    Accessor of the slot @slot[gtk-gesture-pan]{orientation} of the
    @class{gtk-gesture-pan} class.
  @end{short}

  The slot access function @sym{gtk-gesture-pan-orientation}
  returns the orientation of the pan gestures that this gesture expects.

  The slot access function @sym{(setf gtk-gesture-pan-orientation)}
  sets the orientation to be expected on pan gestures.

  Since 3.14
  @see-class{gtk-gesture-pan}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_pan_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-gesture-pan-new))

(defun gtk-gesture-pan-new (widget orientation)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-23}
  @argument[widget]{a @class{gtk-widget}}
  @argument[orientation]{expected orientation of type @symbol{gtk-orientation}}
  @return{A newly created @class{gtk-gesture-pan}.}
  @begin{short}
    Returns a newly created @class{gtk-gesture} that recognizes pan gestures.
  @end{short}

  Since 3.14
  @see-class{gtk-gesture-pan}
  @see-symbol{gtk-orientation}"
  (make-instance 'gtk-gesture-pan
                 :widget widget
                 :orientation orientation))

(export 'gtk-gesture-pan-new)

;;; --- End of file gtk.gesture-pan.lisp ---------------------------------------
