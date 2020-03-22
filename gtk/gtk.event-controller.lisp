;;; ----------------------------------------------------------------------------
;;; gtk.event-controller.lisp
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
;;; GtkEventController
;;;
;;;     Self-contained handler of series of events
;;;
;;; Functions
;;;
;;;     gtk_event_controller_get_propagation_phase         Accessor
;;;     gtk_event_controller_set_propagation_phase         Accessor
;;;     gtk_event_controller_handle_event
;;;     gtk_event_controller_get_widget                    Accessor
;;;     gtk_event_controller_reset
;;;
;;; Properties
;;;
;;;     GtkPropagationPhase  propagation-phase  Read / Write
;;;             GtkWidget *  widget             Read / Write / Construct Only
;;;
;;; Types and Values
;;;
;;;     GtkEventController
;;;     GtkPropagationPhase
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ├── GtkEventControllerKey
;;;         ├── GtkEventControllerMotion
;;;         ├── GtkEventControllerScroll
;;;         ├── GtkGesture
;;;         ╰── GtkPadController
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkPropagationPhase
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPropagationPhase" gtk-propagation-phase
  (:export t
   :type-initializer "gtk_propagation_phase_get_type")
  (:phase-none 0)
  (:phase-capture 1)
  (:phase-bubble 2)
  (:phase-target 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-propagation-phase atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-propagation-phase atdoc:*external-symbols*)
 "@version{2019-3-16}
  @begin{short}
    Describes the stage at which events are fed into a
    @class{gtk-event-controller}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPropagationPhase\" gtk-propagation-phase
  (:export t
   :type-initializer \"gtk_propagation_phase_get_type\")
  (:phase-none 0)
  (:phase-capture 1)
  (:phase-bubble 2)
  (:phase-target 3))
  @end{pre}
  @begin[code]{table}
    @entry[:phase-none]{Events are not delivered automatically. Those can be
      manually fed through gtk_event_controller_handle_event(). This should
      only be used when full control about when, or whether the controller
      handles the event is needed.}
    @entry[:phase-capture]{Events are delivered in the capture phase. The
      capture phase happens before the bubble phase, runs from the toplevel down
      to the event widget. This option should only be used on containers that
      might possibly handle events before their children do.}
    @entry[:phase-bubble]{Events are delivered in the bubble phase. The bubble
      phase happens after the capture phase, and before the default handlers are
      run. This phase runs from the event widget, up to the toplevel.}
    @entry[:phase-target]{Events are delivered in the default widget event
      handlers, note that widget implementations must chain up on button,
      motion, touch and grab broken handlers for controllers in this phase to
      be run.}
  @end{table}
  Since 3.14
  @see-class{gtk-event-controller}")

;;; ----------------------------------------------------------------------------
;;; struct GtkEventController
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEventController" gtk-event-controller
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_get_type")
  ((propagation-phase
    gtk-event-controller-propagation-phase
    "propagation-phase" "GtkPropagationPhase" t t)
   (widget
    gtk-event-controller-widget
    "widget" "GtkWidget" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-event-controller 'type)
 "@version{2019-3-17}
  @begin{short}
    @sym{gtk-event-controller} is a base, low-level implementation for event
    controllers.
  @end{short}
  Those react to a series of @class{gdk-event}, and possibly trigger actions as
  a consequence of those.

  Since 3.14
  @see-slot{gtk-event-controller-propagate-phase}
  @see-slot{gtk-event-controller-widget}
  @see-class{gdk-event}
  @see-class{gtk-gesture}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-event-controller-propagation-phase ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "propagation-phase"
                                               'gtk-event-controller) 't)
 "The @code{propagation-phase} property of type
  @symbol{gtk-propagation-phase} (Read / Write) @br{}
  The propagation phase at which this controller will handle events. @br{}
  Since 3.14 @br{}
  Default value: @code{:phase-bubble}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-event-controller-propagation-phase
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-event-controller-propagation-phase 'function)
 "@version{2019-3-17}
  @syntax[]{(gtk-event-controller-propagation-phase object) => phase)}
  @syntax[]{(setf (gtk-event-controller-propagation-phase object) phase)}
  @argument[object]{a @class{gtk-event-controller} object}
  @argument[phase]{the propagation phase}
  @begin{short}
    Accessor of the slot @slot[gtk-event-controller]{propagation-phase} of the
    @class{gtk-event-controller} class.
  @end{short}

  The generic function @sym{gtk-event-controller-proppagation-phase}
  gets the propagation phase at which controller handles events.

  The generic function @sym{(setf gtk-event-controller-propagation-phase)}
  sets the propagation phase at which a controller handles events.

  If phase is @code{:phase-none}, no automatic event handling will be performed,
  but other additional gesture maintenance will. In that phase, the events can
  be managed by calling the function @fun{gtk-event-controller-handle-event}.

  Since 3.14
  @see-class{gtk-event-controller}
  @see-function{gtk-event-controller-handle-event}")

;;; --- gtk-event-controller-widget --------------------------------------------

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "widget"
                                               'gtk-event-controller) 't)
 "The @code{widget} property of type @class{gtk-widget} (Read / Write) @br{}
  The widget receiving the @class{gdk-event} that the controller will handle.
  Since 3.14 @br{}")

#+(and gtk-3-14 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-event-controller-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-event-controller-widget 'function)
 "@version{2019-3-17}
  @syntax[]{(gtk-event-controller-widget object) => widget)}
  @argument[object]{a @class{gtk-event-controller} object}
  @begin{short}
    Accessor of the slot @slot[gtk-event-controller]{widget} of the
    @class{gtk-event-controller} class.
  @end{short}

  The generic function @sym{gtk-event-controller-widget}
  returns the @class{gtk-widget} this controller relates to.

  Since 3.14
  @see-class{gtk-event-controller}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_handle_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_event_controller_handle_event"
           gtk-event-controller-handle-event) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2019-3-17}
  @argument[controller]{a @class{gtk-event-controller} object}
  @argument[event]{a @class{gdk-event}}
  @return{@em{true} if the @arg{event} was potentially useful to trigger the
    controller action}
  @begin{short}
    Feeds an events into @arg{controller}, so it can be interpreted and the
    controller actions triggered.
  @end{short}

  Since 3.14
  @see-class{gtk-event-controller}"
  (controller (g-object gtk-event-controller))
  (event (g-object gdk-event)))

(export 'gtk-event-controller-handle-event)

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_reset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_event_controller_reset" gtk-event-controller-reset) :void
 #+cl-cffi-gtk-documentation
 "@version{2019-3-17}
  @argument[controller]{a @class{gtk-event-controller} object}
  @begin{short}
    Resets the controller to a clean state.
  @end{short}
  Every interaction the controller did through \"handle-event\" will be dropped
  at this point.

  Since 3.14
  @see-class{gtk-event-controller}"
  (controller (g-object gtk-event-controller)))

(export 'gtk-event-controller-reset)

;;; --- End of file gtk.event-controller.lisp ----------------------------------
