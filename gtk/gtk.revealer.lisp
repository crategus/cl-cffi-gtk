;;; ----------------------------------------------------------------------------
;;; gtk.revealer.lisp
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
;;; GtkRevealer
;;;
;;;     Hide and show with animation
;;;
;;; Types and Values
;;;
;;;     GtkRevealer
;;;     GtkRevealerTransitionType
;;;
;;; Functions
;;;
;;;     gtk_revealer_new
;;;     gtk_revealer_get_reveal_child                      Accessor
;;;     gtk_revealer_set_reveal_child                      Accessor
;;;     gtk_revealer_get_child_revealed                    Accessor
;;;     gtk_revealer_get_transition_duration               Accessor
;;;     gtk_revealer_set_transition_duration               Accessor
;;;     gtk_revealer_get_transition_type                   Accessor
;;;     gtk_revealer_set_transition_type                   Accessor
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
(setf (gethash 'gtk-revealer-transition-type atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-revealer-transition-type atdoc:*external-symbols*)
 "@version{*2021-11-15}
  @begin{short}
    These enumeration values describe the possible transitions when the child
    widget of a @class{gtk-revealer} widget is shown or hidden.
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
 "@version{*2021-11-15}
  @begin{short}
    The @sym{gtk-revealer} widget is a container which animates the transition
    of its child widget from invisible to visible.
  @end{short}
  The style of transition can be controlled with a value of the
  @fun{gtk-revealer-transition-type} enumeration. These animations respect
  the @slot[gtk-settings]{gtk-enable-animations} setting.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-revealer} implementation has a single CSS node with name
    @code{revealer}.
  @end{dictionary}
  @see-slot{gtk-revealer-child-revealed}
  @see-slot{gtk-revealer-reveal-child}
  @see-slot{gtk-revealer-transition-duration}
  @see-slot{gtk-revealer-transition-type}
  @see-class{gtk-expander}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-revealer-child-revealed --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "child-revealed"
                                               'gtk-revealer) 't)
 "The @code{child-revealed} property of type @code{:boolean} (Read) @br{}
  Whether the child widget is revealed and the animation target reached. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-revealer-child-revealed atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-revealer-child-revealed 'function)
 "@version{*2021-11-15}
  @syntax[]{(gtk-revealer-child-revealed object) => revealed}
  @argument[object]{a @class{gtk-revealer} widget}
  @argument[revealed]{a boolean whether the child widget is revealed}
  @begin{short}
    Accessor of the @slot[gtk-revealer]{child-revealed} slot of the
    @class{gtk-revealer} class.
  @end{short}

  The @sym{gtk-revealer-child-revealed} slot access function returns whether
  the child widget is fully revealed, in other words whether the transition to
  the revealed state is completed.
  @see-class{gtk-revealer}")

;;; --- gtk-revealer-reveal-child ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "reveal-child"
                                               'gtk-revealer) 't)
 "The @code{reveal-child} property of type @code{:boolean} (Read / Write) @br{}
  Whether the container should reveal the child widget. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-revealer-reveal-child atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-revealer-reveal-child 'function)
 "@version{*2021-11-15}
  @syntax[]{(gtk-revealer-reveal-child object) => reveal}
  @syntax[]{(setf (gtk-revealer-reveal-child object) reveal)}
  @argument[object]{a @class{gtk-revealer} widget}
  @argument[reveal]{@em{true} to reveal the child widget}
  @begin{short}
    Accessor of the @slot[gtk-revealer]{reveal-child} slot of the
    @class{gtk-revealer} class.
  @end{short}

  The @sym{gtk-revealer-reveal-child} slot access function returns whether the
  child widget is currently revealed. The @sym{(setf gtk-revealer-reveal-child)}
  slot access function tells the revealer to reveal or conceal its child widget.

  This function returns @em{true} as soon as the transition to the revealed
  state is started. To learn whether the child widget is fully revealed, i.e.
  the transition is completed, use the @fun{gtk-revealer-child-revealed}
  function. The transition will be animated with the current transition type of
  the revealer.
  @see-class{gtk-revealer}
  @see-function{gtk-revealer-child-revealed}")

;;; --- gtk-revealer-transition-duration ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "transition-duration"
                                               'gtk-revealer) 't)
 "The @code{transition-duration} property of type @code{:uint} (Read / Write)
  @br{}
  The animation duration, in milliseconds. @br{}
  Default value: 250")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-revealer-transition-duration atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-revealer-transition-duration 'function)
 "@version{2021-11-15}
  @syntax[]{(gtk-revealer-transition-duration object) => duration}
  @syntax[]{(setf (gtk-revealer-transition-duration object) duration)}
  @argument[object]{a @class{gtk-revealer} widget}
  @argument[duration]{an unsigned integer with the duration, in milliseconds}
  @begin{short}
    Accessor of the @slot[gtk-revealer]{transition-duration} slot of the
    @class{gtk-revealer} class.
  @end{short}

  The @sym{gtk-revealer-transition-duration} slot access function returns the
  amount of time in milliseconds that transitions will take. The
  @sym{(setf gtk-revealer-transition-duration)} slot access function sets the
  duration.
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
 "@version{2021-11-15}
  @syntax[]{(gtk-revealer-transition-type object) => setting}
  @syntax[]{(setf (gtk-revealer-transition-type object) setting)}
  @argument[object]{a @class{gtk-revealer} widget}
  @argument[setting]{a value of the @symbol{gtk-revealer-transition-type}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk-revealer]{transition-type} slot of the
    @class{gtk-revealer} class.
  @end{short}

  The @sym{gtk-revealer-transition-type} slot access function gets the type of
  animation that will be used for transitions in the revealer. The
  @sym{(setf gtk-revealer-transition-duration)} slot access function sets the
  type of animation. Available types include various kinds of fades and slides.
  @see-class{gtk-revealer}
  @see-symbol{gtk-revealer-transition-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_revealer_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-revealer-new))

(defun gtk-revealer-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-11-15}
  @return{The new @class{gtk-revealer} widget.}
  @short{Creates a new revealer.}
  @see-class{gtk-revealer}"
  (make-instance 'gtk-revealer))

(export 'gtk-revealer-new)

;;; --- End of file gtk.revealer.lisp ------------------------------------------
