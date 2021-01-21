;;; ----------------------------------------------------------------------------
;;; gtk.revealer.lisp
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
;;; GtkRevealer
;;;
;;;     Hide and show with animation
;;;
;;; Types and Values
;;;
;;;     GtkRevealer
;;;     GtkRevealerClass
;;;     GtkRevealerTransitionType
;;;
;;; Functions
;;;
;;;     gtk_revealer_new
;;;     gtk_revealer_get_reveal_child
;;;     gtk_revealer_set_reveal_child
;;;     gtk_revealer_get_child_revealed
;;;     gtk_revealer_get_transition_duration
;;;     gtk_revealer_set_transition_duration
;;;     gtk_revealer_get_transition_type
;;;     gtk_revealer_set_transition_type
;;;
;;; Properties
;;;
;;;     child-revealed
;;;     reveal-child
;;;     transition-duration
;;;     transition-type
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkRevealer
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRevealer implements AtkImplementorIface and GtkBuildable.
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkRevealerTransitionType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkRevealerTransitionType" gtk-revealer-transition-type
  (:export t
   :type-initializer "gtk_revealer_transition_type_get_type")
  (:none 0)
  (:crossfade 1)
  (:slide-right 2)
  (:slide-left 3)
  (:slide-up 4)
  (:slide-down 5))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-revealer-transition-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-revealer-transition-type atdoc:*external-symbols*)
 "@version{2019-3-9}
  @begin{short}
    These enumeration values describe the possible transitions when the child
    of a @class{gtk-revealer} widget is shown or hidden.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkRevealerTransitionType\" gtk-revealer-transition-type
  (:export t
   :type-initializer \"gtk_revealer_transition_type_get_type\")
  (:none 0)
  (:crossfade 1)
  (:slide-right 2)
  (:slide-left 3)
  (:slide-up 4)
  (:slide-down 5))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No transition}
    @entry[:crossfade]{Fade in.}
    @entry[:slide-right]{Slide in from the left.}
    @entry[:slide-left]{Slide in from the right.}
    @entry[:slide-up]{Slide in from the bottom.}
    @entry[:slide-down]{Slide in from the top.}
  @end{table}
  @see-class{gtk-revealer}")

;;; ----------------------------------------------------------------------------
;;; struct GtkRevealer
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRevealer" gtk-revealer
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_revealer_get_type")
  ((child-revealed
    gtk-revealer-child-revealed
    "child-revealed" "gboolean" t nil)
   (reveal-child
    gtk-revealer-reveal-child
    "reveal-child" "gboolean" t t)
   (transition-duration
    gtk-revealer-transition-duration
    "transition-duration" "guint" t t)
   (transition-type
    gtk-reveaker-transition-type
    "transition-type" "GtkRevealerTransitionType" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-revealer 'type)
 "@version{2020-4-23}
  @begin{short}
    The @sym{gtk-revealer} widget is a container which animates the transition
    of its child from invisible to visible.
  @end{short}

  The style of transition can be controlled with a value of the
  @fun{gtk-revealer-transition-type} enumeration. These animations respect
  the @slot[gtk-settings]{gtk-enable-animations} setting.

  The GtkRevealer widget was added in GTK+ 3.10.
  @begin[CSS nodes]{dictionary}
    @sym{gtk-revealer} has a single CSS node with name @code{revealer}.
  @end{dictionary}
  @see-slot{gtk-revealer-child-revealed}
  @see-slot{gtk-revealer-reveal-child}
  @see-slot{gtk-revealer-transition-duration}
  @see-slot{gtk-revealer-transition-type}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-revealer-child-revealed --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "child-revealed"
                                               'gtk-revealer) 't)
 "The @code{child-revealed} property of type @code{:boolean} (Read) @br{}
  Whether the child is revealed and the animation target reached. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-revealer-child-revealed atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-revealer-child-revealed 'function)
 "@version{2020-4-23}
  @syntax[]{(gtk-revealer-child-revealed object) => revealed}
  @argument[object]{a @class{gtk-revealer} object}
  @begin{short}
    Accessor of the @slot[gtk-revealer]{child-revealed} slot of the
    @class{gtk-revealer} class.
  @end{short}

  The slot access function @sym{gtk-revealer-child-revealed} returns whether
  the child is fully revealed, in other words whether the transition to the
  revealed state is completed.
  @see-class{gtk-revealer}")

;;; --- gtk-revealer-reveal-child ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "reveal-child"
                                               'gtk-revealer) 't)
 "The @code{reveal-child} property of type @code{:boolean} (Read / Write) @br{}
  Whether the container should reveal the child. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-revealer-reveal-child atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-revealer-reveal-child 'function)
 "@version{2020-4-23}
  @syntax[]{(gtk-revealer-reveal-child object) => reveal-child}
  @syntax[]{(setf (gtk-revealer-reveal-child object) reveal-child)}
  @argument[object]{a @class{gtk-revealer} object}
  @argument[reveal-child]{@em{true} to reveal the child}
  @begin{short}
    Accessor of the @slot[gtk-revealer]{reveal-child} slot of the
    @class{gtk-revealer} class.
  @end{short}

  The slot access function @sym{gtk-revealer-reveal-child} returns whether the
  child is currently revealed. This function returns @em{true} as soon as the
  transition is to the revealed state is started. To learn whether the child
  is fully revealed, i.e. the transition is completed, use the function
  @fun{gtk-revealer-child-revealed}.

  The slot access function @sym{(setf gtk-revealer-reveal-child)} tells the
  @class{gtk-revealer} to reveal or conceal its child.

  The transition will be animated with the current transition type of the
  revealer.
  @see-class{gtk-revealer}
  @see-function{gtk-revealer-child-revealed}")

;;; --- gtk-revealer-transition-duration ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "transition-duration"
                                               'gtk-revealer) 't)
 "The @code{transition-duration} property of type @code{:uint}
  (Read / Write) @br{}
  The animation duration, in milliseconds. @br{}
  Default value: 250")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-revealer-transition-duration atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-revealer-transition-duration 'function)
 "@version{202ß-4-23}
  @syntax[]{(gtk-revealer-transition-duration object) => duration}
  @syntax[]{(setf (gtk-revealer-transition-duration object) duration)}
  @argument[object]{a @class{gtk-revealer} object}
  @argument[duration]{an unsigned integer with the duration, in milliseconds}
  @begin{short}
    Accessor of the @slot[gtk-revealer]{transition-duration} slot of the
    @class{gtk-revealer} class.
  @end{short}

  The slot access function @sym{gtk-revealer-transition-duration} returns the
  amount of time in milliseconds that transitions will take. The slot access
  function @sym{(setf gtk-revealer-transition-duration)} sets the duration that
  transitions will take.
  @see-class{gtk-revealer}")

;;; --- gtk-revealer-transition-type -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "transition-type"
                                               'gtk-revealer) 't)
 "The @code{transition-type} property of type
  @symbol{gtk-revealer-transition-type} (Read / Write) @br{}
  The type of animation used to transition. @br{}
  Default value: @code{:slide-down}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-revealer-transition-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-revealer-transition-type 'function)
 "@version{2020-4-23}
  @syntax[]{(gtk-revealer-transition-type object) => transition}
  @syntax[]{(setf (gtk-revealer-transition-type object) transition)}
  @argument[object]{a @class{gtk-revealer} object}
  @argument[transition]{the new transition type}
  @begin{short}
    Accessor of the slot @slot[gtk-revealer]{transition-type} of the
    @class{gtk-revealer} class.
  @end{short}

  The slot access function @sym{gtk-revealer-transition-type} gets the type of
  animation that will be used for transitions in revealer. The slot access
  function @sym{(setf gtk-revealer-transition-duration)} sets the type of
  animation that will be used for transitions in revealer. Available types
  include various kinds of fades and slides.
  @see-class{gtk-revealer}")

;;; ----------------------------------------------------------------------------
;;; gtk_revealer_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-revealer-new))

(defun gtk-revealer-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-4-23}
  @return{The new @class{gtk-revealer} objekt.}
  @short{Creates a new revealer.}
  @see-class{gtk-revealer}"
  (make-instance 'gtk-revealer))

(export 'gtk-revealer-new)

;;; --- End of file gtk.revealer.lisp ------------------------------------------
