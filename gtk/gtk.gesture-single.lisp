;;; ----------------------------------------------------------------------------
;;; gtk.gesture-single.lisp
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
;;; GtkGestureSingle
;;;
;;;     Base class for mouse/single-touch gestures
;;;
;;; Types and Values
;;;
;;;     GtkGestureSingle
;;;
;;; Functions
;;;
;;;     gtk_gesture_single_get_exclusive                   Accessor
;;;     gtk_gesture_single_set_exclusive                   Accessor
;;;     gtk_gesture_single_get_touch_only                  Accessor
;;;     gtk_gesture_single_set_touch_only                  Accessor
;;;     gtk_gesture_single_get_button                      Accessor
;;;     gtk_gesture_single_set_button                      Accessor
;;;     gtk_gesture_single_get_current_button
;;;     gtk_gesture_single_get_current_sequence
;;;
;;; Properties
;;;
;;;        guint    button        Read / Write
;;;     gboolean    exclusive     Read / Write
;;;     gboolean    touch-only    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ├── GtkGestureDrag
;;;                 ├── GtkGestureLongPress
;;;                 ├── GtkGestureMultiPress
;;;                 ├── GtkGestureStylus
;;;                 ╰── GtkGestureSwipe
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureSingle
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureSingle" gtk-gesture-single
  (:superclass gtk-gesture
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_single_get_type")
  ((button
    gtk-gesture-single-button
    "button" "guint" t t)
   (exclusive
    gtk-gesture-single-exclusive
    "exclusive" "gboolean" t t)
   (touch-only
    gtk-gesture-single-touch-only
    "touch-only" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-gesture-single 'type)
 "@version{2020-9-11}
  @begin{short}
    @sym{gtk-gesture-single} is a subclass of @class{gtk-gesture}, optimized
    (although not restricted) for dealing with mouse and single-touch gestures.
  @end{short}
  Under interaction, these gestures stick to the first interacting sequence,
  which is accessible through the function
  @fun{gtk-gesture-single-current-sequence} while the gesture is being
  interacted with.

  By default gestures react to both @code{:button-primary} and touch events,
  the slot access function @fun{gtk-gesture-single-touch-only} can be used to
  change the touch behavior. Callers may also specify a different mouse button
  number to interact with through the slot access function
  @fun{gtk-gesture-single-button}, or react to any mouse button by setting 0.
  While the gesture is active, the button being currently pressed can be known
  through the function @fun{gtk-gesture-single-current-button}.
  @see-slot{gtk-gesture-single-button}
  @see-slot{gtk-gesture-single-exclusive}
  @see-slot{gtk-gesture-single-touch-only}
  @see-class{gtk-gesture}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-gesture-single-button ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "button"
                                               'gtk-gesture-single) 't)
 "The @code{button} property of type @code{:uint} (Read / Write) @br{}
  Mouse button number to listen to, or 0 to listen for any button. @br{}
  Default value: 1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-gesture-single-button atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-gesture-single-button 'function)
 "@version{2020-9-11}
  @syntax[]{(gtk-gesture-single-button object) => button)}
  @syntax[]{(setf (gtk-gesture-single-button object) button)}
  @argument[object]{a @class{gtk-gesture-single} object}
  @argument[button]{button number to listen to, or 0 for any button}
  @begin{short}
    Accessor of the @slot[gtk-gesture-single]{button} slot of the
    @class{gtk-gesture-single} class.
  @end{short}

  The slot access function @sym{gtk-gesture-single-button} returns the button
  number gesture listens for, or 0 if gesture reacts to any button press. The
  slot access function @sym{(setf gtk-gesture-single-button)} sets the button
  number gesture listens to. If non-0, every button press from a different
  button number will be ignored. Touch events implicitly match with button 1.
  @see-class{gtk-gesture-single}")

;;; --- gtk-gesture-single-exclusive -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "exclusive"
                                               'gtk-gesture-single) 't)
 "The @code{exclusive} property of type @code{:boolean} (Read / Write) @br{}
  Whether the gesture is exclusive. Exclusive gestures only listen to pointer
  and pointer emulated events. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-gesture-single-exclusive atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-gesture-single-exclusive 'function)
 "@version{2020-9-11}
  @syntax[]{(gtk-gesture-single-exclusive object) => exclusive)}
  @syntax[]{(setf (gtk-gesture-single-exclusive object) exclusive)}
  @argument[object]{a @class{gtk-gesture-single} object}
  @argument[exclusive]{@em{true} to make gesture exclusive}
  @begin{short}
    Accessor of the @slot[gtk-gesture-single]{exclusive} slot of the
    @class{gtk-gesture-single} class.
  @end{short}

  The slot access function @sym{gtk-gesture-single-exclusive} gets whether a
  gesture is exclusive. The slot access function
  @sym{(setf gtk-gesture-single-exclusive)} sets whether gesture is exclusive.
  An exclusive gesture will only handle pointer and \"pointer emulated\" touch
  events, so at any given time, there is only one sequence able to interact
  with those.
  @see-class{gtk-gesture-single}")

;;; --- gtk-gesture-single-touch-only ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "touch-only"
                                               'gtk-gesture-single) 't)
 "The @code{touch-only} property of type @code{:boolean} (Read / Write) @br{}
  Whether the gesture handles only touch events. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-gesture-single-touch-only atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-gesture-single-touch-only 'function)
 "@version{2020-9-11}
  @syntax[]{(gtk-gesture-single-touch-only object) => touch-only)}
  @syntax[]{(setf (gtk-gesture-single-touch-only object) touch-only)}
  @argument[object]{a @class{gtk-gesture-single} object}
  @argument[touch-only]{a boolean whether gesture handles only touch events}
  @begin{short}
    Accessor of the @slot[gtk-gesture-single]{touch-only} slot of the
    @class{gtk-gesture-single} class.
  @end{short}

  The slot access function @sym{gtk-gesture-single-touch-only} returns
  @em{true} if the gesture is only triggered by touch events. The slot access
  function @sym{(setf gtk-gesture-single-touch-only)} sets whether the gesture
  is only triggered by touch events. If @arg{touch-only} is @em{true}, gesture
  will only handle events of type @code{:touch-begin}, @code{:touch-update} or
  @code{:touch-end}. If @em{false}, mouse events will be handled too.
  @see-class{gtk-gesture-single}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_single_get_current_button ()
;;; -> gtk-gesture-single-current-button
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_single_get_current_button"
           gtk-gesture-single-current-button) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[gesture]{a @class{gtk-gesture-single} object}
  @return{The current button number of type @code{:uint}.}
  @begin{short}
    Returns the button number currently interacting with @arg{gesture}, or 0 if
    there is none.
  @end{short}
  @see-class{gtk-gesture-single}"
  (gesture (g-object gtk-gesture-single)))

(export 'gtk-gesture-single-current-button)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_single_get_current_sequence ()
;;; -> gtk-gesture-single-current-sequence
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_single_get_current_sequence"
           gtk-gesture-single-current-sequence)
    (g-boxed-foreign gdk-event-sequence)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[gesture]{a @class{gtk-gesture-single} object}
  @return{The current sequence of type @class{gdk-event-sequence}.}
  @begin{short}
    Returns the event sequence currently interacting with @arg{gesture}.
  @end{short}
  This is only meaningful if the function @fun{gtk-gesture-is-active} returns
  @em{true}.
  @see-class{gtk-gesture-single}
  @see-function{gtk-gesture-is-active}"
  (gesture (g-object gtk-gesture-single)))

(export 'gtk-gesture-single-current-sequence)

;;; --- End of file gtk.gesture-single-lisp ------------------------------------
