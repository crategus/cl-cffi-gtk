;;; ----------------------------------------------------------------------------
;;; gtk.event-controller-scroll.lisp
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
;;; GtkEventControllerScroll
;;;
;;;     Event controller for scroll events
;;;
;;; Types and Values
;;;
;;;     GtkEventControllerScroll
;;;     GtkEventControllerScrollFlags
;;;
;;; Functions
;;;
;;;     gtk_event_controller_scroll_new
;;;     gtk_event_controller_scroll_set_flags              Accessor
;;;     gtk_event_controller_scroll_get_flags              Accessor
;;;
;;; Properties
;;;
;;;     GtkEventControllerScrollFlags  flags  Read / Write
;;;
;;; Signals
;;;
;;;     void  decelerate    Run First
;;;     void  scroll        Run First
;;;     void  scroll-begin  Run First
;;;     void  scroll-end     Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkEventControllerScroll
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkEventControllerScrollFlags
;;; ----------------------------------------------------------------------------

#+gtk-3-24
(define-g-flags "GtkEventControllerScrollFlags" 
                gtk-event-controller-scroll-flags
  (:export t
   :type-initializer "gtk_event_controller_scroll_flags_get_type")
  (:none 0)
  (:vertical 1)
  (:horizontal 2)
  (:discrete 4)
  (:kinetic 8)
  (:both-axes 16))

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-event-controller-scroll-flags atdoc:*symbol-name-alias*) 
      "Flags"
      (gethash 'gtk-event-controller-scroll-flags atdoc:*external-symbols*)
 "@version{2019-3-17}
  @begin{short}
    Describes the behavior of a @class{gtk-event-controller-scroll}.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkEventControllerScrollFlags\" 
                gtk-event-controller-scroll-flags
  (:export t
   :type-initializer \"gtk_event_controller_scroll_flags_get_type\")
  (:none 0)
  (:vertical 1)
  (:horizontal 2)
  (:discrete 4)
  (:kinetic 8)
  (:both-axes 16))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Don't emit scroll.}
    @entry[:vertical]{Emit scroll with vertical deltas.}
    @entry[:horizontal]{Emit scroll with horizontal deltas.}
    @entry{:discrete]{Only emit deltas that are multiples of 1.}
    @entry[:kinetic]{Emit \"decelerate\" after continuous scroll finishes.}
    @entry[:both-axes]{Emit scroll on both axes.}
  @end{table}
  Since 3.24
  @see-class{gtk-event-controller-scroll}")

;;; ----------------------------------------------------------------------------
;;; struct GtkEventControllerScroll
;;; ----------------------------------------------------------------------------

#+gtk-3-24
(define-g-object-class "GtkEventControllerScroll" gtk-event-controller-scroll
  (:superclass gtk-event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_scroll_get_type")
  ((flags
    gtk-event-controller-scroll-flags
    "flags" "GtkEventControllerScrollFlags" t t)))

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (documentation 'gtk-event-controller-scroll 'type)
 "@version{2019-3-17}
  @begin{short}
    @sym{gtk-event-controller-scroll} is an event controller meant to handle 
    scroll events from mice and touchpads.
  @end{short}
  It is capable of handling both discrete and continuous scroll events, 
  abstracting them both on the \"scroll\" signal (deltas in the discrete case 
  are multiples of 1).

  In the case of continuous scroll events, @sym{gtk-event-controller-scroll} 
  encloses all \"scroll\" events between two \"scroll-begin\" and \"scroll-end\"
  signals.

  The behavior of the event controller can be modified by the flags given at 
  creation time, or modified at a later point through the generic function
  @fun{gtk-event-controller-scroll-flags}, e. g. because the scrolling 
  conditions of the widget changed.

  The controller can be set up to emit motion for either/both vertical and 
  horizontal scroll events through @code{:vertical}, @code{:horizontal} and
  @code{:both-axes}. If any axis is disabled, the respective \"scroll\" delta 
  will be 0. Vertical scroll events will be translated to horizontal motion for 
  the devices incapable of horizontal scrolling.

  The event controller can also be forced to emit discrete events on all devices
  through @code{:discrete}. This can be used to implement discrete actions 
  triggered through scroll events, e. g. switching across combobox options.

  The @code{:kinetic} flag toggles the emission of the \"decelerate\" signal, 
  emitted at the end of scrolling with two X/Y velocity arguments that are 
  consistent with the motion that was received.

  This object was added in 3.24.

  @begin[Signal Details]{dictionary}
    @subheading{The \"decelerate\" signal}
      @begin{pre}
  lambda (controller vel-x vel-y)    : Run First
      @end{pre}
      Emitted after scroll is finished if the @code{:kinetic} flag is set. 
      @code{vel-x} and @code{vel-y} express the initial velocity that was 
      imprinted by the scroll events. @code{vel-x} and @code{vel-y} are 
      expressed in pixels/ms.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk-event-controller-scroll} object that 
          received the signal.}
        @entry[vel-x]{x velocity}
        @entry[vel-y]{y velocity}
      @end{table}
      Since 3.24

    @subheading{The \"scroll\" signal}
      @begin{pre}
  lambda (controller dx dy)    : Run First
      @end{pre}
      Signals that the widget should scroll by the amount specified by 
      @code{dx} and @code{dy}.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk-event-controller-scroll} object that 
          received the signal.}
        @entry[dx]{x delta}
        @entry[dy]{y delta}
      @end{table}
      Since 3.24

    @subheading{The \"scroll-begin\" signal}
      @begin{pre}
  lambda (controller dx dy)    : Run First
      @end{pre}
      Signals that a new scrolling operation has begun. It will only be emitted 
      on devices capable of it.
      @code{dx} and @code{dy}.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk-event-controller-scroll} object that 
          received the signal.}
      @end{table}
      Since 3.24

    @subheading{The \"scroll-end\" signal}
      @begin{pre}
  lambda (controller dx dy)    : Run First
      @end{pre}
      Signals that a new scrolling operation has finished. It will only be 
      emitted on devices capable of it.
      @code{dx} and @code{dy}.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk-event-controller-scroll} object that 
          received the signal.}
      @end{table}
      Since 3.24
  @end{dictionary}

  @see-slot{gtk-event-controller-scroll-flags}
  @see-class{gtk-event-controller}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-event-controller-scroll-flags --------------------------------------

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "Flags" 
                                               'gtk-event-controller-scroll) 't)
 "The @code{flags} property of type
  @symbol{gtk-event-controller-scroll-flags} (Read / Write) @br{}
  The flags affecting event controller behavior. @br{}
  Since 3.24")

#+(and gtk-3-24 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-event-controller-scroll-flags 
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-event-controller-scroll-flags 'function)
 "@version{2019-3-17}
  @syntax[]{(gtk-event-controller-scroll-flags object) => flags)}
  @syntax[]{(setf (gtk-event-controller-scroll-flags object) flags)}
  @argument[object]{a @class{gtk-event-controller-scroll} object}
  @argument[flags]{behavior flags}

  @begin{short}
    Accessor of the slot @slot[gtk-event-controller-scroll]{flags} of the
    @class{gtk-event-controller-scroll} class.
  @end{short}

  The generic function @sym{gtk-event-controller-scroll-flags} 
  gets the flags conditioning the scroll controller behavior. 
 
  The generic function @sym{(setf gtk-event-controller-scroll-flags)}
  sets the flags conditioning scroll controller behavior.
  
  Since 3.24  
  @see-class{gtk-event-controller-scroll}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_scroll_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-24
(declaim (inline gtk-event-controller-scroll-new))

#+gtk-3-24
(defun gtk-event-controller-scroll-new (widget flags)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-17}
  @argument[widget]{a @class{gtk-widget} object}
  @argument[flags]{@symbol{gtk-event-controller-scroll-flags} behavior flags}
  @return{The new @class{gtk-event-controller-scroll} objekt.}
  @begin{short}
    Creates a new event controller that will handle scroll events for the given 
    @arg{widget}.
  @end{short}

  Since 3.24
  @see-class{gtk-event-controller-scroll}
  @see-symbol{gtk-event-controller-scroll-flags}"
  (make-instance 'gtk-event-controller-scroll
                 :widget widget
                 :flags flags))

#+gtk-3-24
(export 'gtk-event-controller-scroll-new)

;;; --- End of file gtk.event-controller-scroll.lisp ---------------------------
