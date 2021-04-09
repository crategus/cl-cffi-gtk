;;; ----------------------------------------------------------------------------
;;; gdk.events.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; Events
;;;
;;;     Functions for handling events from the window system
;;;
;;; Types and Values
;;;
;;;     GdkEventType           --> gdk.event-structures.lisp
;;;     GdkEventMask           --> gdk.event-structures.lisp
;;;     GdkEventSequence       --> gdk.event-structures.lisp
;;;
;;;     GDK_CURRENT_TIME
;;;     GDK_PRIORITY_EVENTS
;;;     GDK_PRIORITY_REDRAW
;;;     GDK_EVENT_PROPAGATE
;;;     GDK_EVENT_STOP
;;;     GDK_BUTTON_PRIMARY
;;;     GDK_BUTTON_MIDDLE
;;;     GDK_BUTTON_SECONDARY
;;;
;;; Functions
;;;
;;;     gdk_events_pending
;;;     gdk_event_peek
;;;     gdk_event_get
;;;     gdk_event_put
;;;     gdk_event_new
;;;     gdk_event_copy
;;;     gdk_event_free
;;;     gdk_event_get_axis
;;;     gdk_event_get_button
;;;     gdk_event_get_click_count
;;;     gdk_event_get_coords
;;;     gdk_event_get_keycode
;;;     gdk_event_get_keyval
;;;     gdk_event_get_root_coords
;;;     gdk_event_get_scroll_direction
;;;     gdk_event_get_scroll_deltas
;;;     gdk_event_is_scroll_stop_event
;;;     gdk_event_get_state
;;;     gdk_event_get_time
;;;     gdk_event_get_window
;;;     gdk_event_get_event_type
;;;     gdk_event_get_event_sequence
;;;     gdk_event_request_motions
;;;     gdk_events_get_angle
;;;     gdk_events_get_center
;;;     gdk_events_get_distance
;;;     gdk_event_triggers_context_menu
;;;     gdk_event_get_seat
;;;     gdk_event_get_scancode
;;;     gdk_event_get_pointer_emulated
;;;     gdk_event_handler_set
;;;     gdk_get_show_events
;;;     gdk_set_show_events
;;;     gdk_event_set_screen
;;;     gdk_event_get_screen
;;;     gdk_event_get_device
;;;     gdk_event_set_device
;;;     gdk_event_get_source_device
;;;     gdk_event_set_source_device
;;;     gdk_event_get_device_tool
;;;     gdk_event_set_device_tool
;;;     gdk_setting_get
;;;
;;; Description
;;;
;;; This section describes functions dealing with events from the window system.
;;;
;;; In GTK+ applications the events are handled automatically in
;;; gtk_main_do_event() and passed on to the appropriate widgets, so these
;;; functions are rarely needed. Though some of the fields in the Event
;;; Structures are useful.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GDK_CURRENT_TIME
;;; ----------------------------------------------------------------------------

(defconstant +gdk-current-time+ 0
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @begin{short}
    Represents the current time, and can be used anywhere a time is expected.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-current-time+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-current-time+)

;;; ----------------------------------------------------------------------------
;;; GDK_PRIORITY_EVENTS
;;;
;;; #define GDK_PRIORITY_EVENTS
;;;
;;; This is the priority that events from the X server are given in the GLib
;;; Main Loop.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PRIORITY_REDRAW
;;; ----------------------------------------------------------------------------

(defconstant +gdk-priority-redraw+ (+ +g-priority-high-idle+ 20)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @begin{short}
    This is the priority that the idle handler processing window updates is
    given in the GLib Main Loop.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-priority-redraw atdoc:*variable-name-alias*) "Constant")

(export '+gdk-priority-redraw+)

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_PROPAGATE
;;; ----------------------------------------------------------------------------

(defconstant +gdk-event-propagate+ nil
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @begin{short}
    Use this value as the return value for continuing the propagation of an
    event handler.
  @end{short}
  @see-variable{+gdk-event-stop+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-event-propagate+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-event-propagate+)

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_STOP
;;; ----------------------------------------------------------------------------

(defconstant +gdk-event-stop+ t
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @begin{short}
    Use this value as the return value for stopping the propagation of an
    event handler.
  @end{short}
  @see-variable{+gdk-event-propagate+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-event-stop+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-event-stop+)

;;; ----------------------------------------------------------------------------
;;; GDK_BUTTON_PRIMARY
;;;
;;; #define GDK_BUTTON_PRIMARY (1)
;;;
;;; The primary button. This is typically the left mouse button, or the right
;;; button in a left-handed setup.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_BUTTON_MIDDLE
;;;
;;; #define GDK_BUTTON_MIDDLE (2)
;;;
;;; The middle button.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_BUTTON_SECONDARY
;;;
;;; #define GDK_BUTTON_SECONDARY (3)
;;;
;;; The secondary button. This is typically the right mouse button, or the left
;;; button in a left-handed setup.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_events_pending ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_events_pending" gdk-events-pending) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @return{@em{True} if any events are pending.}
  @begin{short}
    Checks if any events are ready to be processed for any display.
  @end{short}
  @begin[Example]{dictionary}
    The function @code{clear-event-loop} looks for pending events.
    @begin{pre}
(defun clear-event-loop ()
  (loop while (gdk-events-pending)
        do (gtk-main-iteration-do nil)))
    @end{pre}
  @end{dictionary}
  @see-class{gdk-event}
  @see-function{gdk-event-peek}")

(export 'gdk-events-pending)

;;; ----------------------------------------------------------------------------
;;; gdk_event_peek ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_peek" gdk-event-peek) (g-boxed-foreign gdk-event :return)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @begin{return}
    A copy of the first @class{gdk-event} structure on some event queue, or
    @code{nil} if no events are in any queues.
  @end{return}
  @begin{short}
    If there is an event waiting in the event queue of some open display,
    returns a copy of it.
  @end{short}
  See the function @fun{gdk-display-peek-event}.
  @see-class{gdk-event}
  @see-function{gdk-events-pending}
  @see-function{gdk-display-peek-event}")

(export 'gdk-event-peek)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get" gdk-event-get) (g-boxed-foreign gdk-event :return)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @begin{return}
    The next @class{gdk-event} structure to be processed, or @code{nil} if no
    events are pending.
  @end{return}
  @begin{short}
    Checks all open displays for an event to be processed, fetching events from
    the windowing system if necessary.
  @end{short}
  See also the function @fun{gdk-display-event}.
  @see-class{gdk-event}
  @see-function{gdk-display-event}")

(export 'gdk-event-get)

;;; ----------------------------------------------------------------------------
;;; gdk_event_put ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_put" gdk-event-put) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @begin{short}
    Appends a copy of the given event onto the front of the event queue for
    the display, or the default event queue if the event has no window.
  @end{short}
  See also the function @fun{gdk-display-put-event}.
  @see-class{gdk-event}
  @see-function{gdk-display-put-event}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-put)

;;; ----------------------------------------------------------------------------
;;; gdk_event_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_new" %gdk-event-new) (g-boxed-foreign gdk-event)
  (event-type gdk-event-type))

(defun gdk-event-new (event-type &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2020-11-28}
  @argument[event-type]{a value of the @symbol{gdk-event-type} enumeration}
  @argument[args]{pairs of property name and property value}
  @return{A new  @class{gdk-event} structure.}
  @begin{short}
    Creates a new event of the given type.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(gdk-event-new :button-press :x 10.0d0 :y 20.0d0)
=>
#S(GDK-EVENT-BUTTON
   :TYPE :BUTTON-PRESS
   :WINDOW NIL
   :SEND-EVENT NIL
   :TIME 0
   :X 10.0d0
   :Y 20.0d0
   :AXES (0.0d0 0.0d0)
   :STATE 0
   :BUTTON 0
   :DEVICE #.(SB-SYS:INT-SAP #X00000000)
   :X-ROOT 0.0d0
   :Y-ROOT 0.0d0)
    @end{pre}
  @end{dictionary}
  @see-class{gdk-event}
  @see-symbol{gdk-event-type}"
  (cond ((member event-type '(:key-press :key-release))
         (apply #'make-gdk-event-key (list* :type event-type args)))
        ((member event-type
                 '(:button-press :2button-press :double-button-press
                   :3button-press :triple-button-press :button-release))
         (apply #'make-gdk-event-button (list* :type event-type args)))
        ((member event-type '(:motion-notify))
         (apply #'make-gdk-event-motion (list* :type event-type args)))
        ((member event-type '(:enter-notify :leave-notify))
         (apply #'make-gdk-event-crossing (list* :type event-type args)))
        ((member event-type '(:focus-change))
         (apply #'make-gdk-event-focus (list* :type event-type args)))
        ((member event-type '(:configure))
         (apply #'make-gdk-event-configure (list* :type event-type args)))
        ((member event-type '(:property-notify))
         (apply #'make-gdk-event-property (list* :type event-type args)))
        ((member event-type
                 '(:selection-clear :selection-notify :selection-request))
         (apply #'make-gdk-event-selection (list* :type event-type args)))
        ((member event-type '(:proximity-in :proximity-out))
         (apply #'make-gdk-event-proximity (list* :type event-type args)))
        ((member event-type
                 '(:drag-enter :drag-leave :drag-motion :drag-status
                   :drop-start :drop-finished))
         (apply #'make-gdk-event-dnd (list* :type event-type args)))
        ((member event-type '(:visibility-notify))
         (apply #'make-gdk-event-visibility (list* :type event-type args)))
        ((member event-type '(:scroll))
         (apply #'make-gdk-event-scroll (list* :type event-type args)))
        ((member event-type '(:window-state))
         (apply #'make-gdk-event-window-state (list* :type event-type args)))
        ((member event-type '(:setting))
         (apply #'make-gdk-event-setting (list* :type event-type args)))
        ((member event-type '(:owner-change))
         (apply #'make-gdk-event-owner-change (list* :type event-type args)))
        ((member event-type '(:grab-broken))
         (apply #'make-gdk-event-grab-broken (list* :type event-type args)))
        ((member event-type
                 '(:touch-begin :touch-update :touch-end :touch-cancel))
         (apply #'make-gdk-event-touch (list* :type event-type args)))
        ((member event-type '(:touchpad-swipe))
         (apply #'make-gdk-event-touchpad-swipe (list* :type event-type args)))
        ((member event-type '(:touchpad-pinch))
         (apply #'make-gdk-event-touchpad-pinch (list* :type event-type args)))
        ((member event-type '(:pad-button-press :pad-button-release))
         (apply #'make-gdk-event-pad-button (list* :type event-type args)))
        ((member event-type '(:pad-ring :pad-strip))
         (apply #'make-gdk-event-pad-axis (list* :type event-type args)))
        ((member event-type '(:pad-group-mode))
         (apply #'make-gdk-event-pad-group-mode (list* :type event-type args)))
        (t
         (%gdk-event-new event-type))))

(export 'gdk-event-new)

;;; ----------------------------------------------------------------------------
;;; gdk_event_copy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-event-copy))

(defun gdk-event-copy (event)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{A copy of the @class{gdk-event} structure.}
  @begin{short}
    Copies an event.
  @end{short}
  @see-class{gdk-event}"
  (copy-gdk-event event))

(export 'gdk-event-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_event_free ()
;;;
;;; void gdk_event_free (GdkEvent *event);
;;;
;;; Frees a GdkEvent, freeing or decrementing any resources associated with it.
;;; Note that this function should only be called with events returned from
;;; functions such as gdk_event_peek(), gdk_event_get(), gdk_event_copy() and
;;; gdk_event_new().
;;;
;;; event :
;;;     a GdkEvent.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_axis () -> gdk-event-axis
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_axis" %gdk-event-axis) :boolean
  (event (g-boxed-foreign gdk-event))
  (axis-use gdk-axis-use)
  (value (:pointer :double)))

(defun gdk-event-axis (event axis-use)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @argument[axis-use]{a @symbol{gdk-axis-use} value with the axis use to look
    for}
  @return{@code{value} -- a @code{:double} with the value found}
  @begin{short}
    Extract the axis value for a particular axis use from an event.
  @end{short}
  @see-class{gdk-event}"
  (with-foreign-object (value :double)
    (when (%gdk-event-axis event axis-use value)
      (mem-ref value :double))))

(export 'gdk-event-axis)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_button () -> gdk-event-button
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_button" %gdk-event-button) :boolean
  (event (g-boxed-foreign gdk-event))
  (button (:pointer :uint)))

(defun gdk-event-button (event)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{Mouse button number, or @code{nil} if the event does not deliver a
    button number.}
  @short{Extract the button number from an event.}
  @see-class{gdk-event}
  @see-class{gdk-event-button}
  @see-function{gdk-event-click-count}"
  (with-foreign-object (button :uint)
    (when (%gdk-event-button event button)
      (mem-ref button :uint))))

(export 'gdk-event-button)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_click_count () -> gdk-event-click-count
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_click_count" %gdk-event-click-count) :boolean
  (event (g-boxed-foreign gdk-event))
  (click-count (:pointer :uint)))

(defun gdk-event-click-count (event)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{Click count, or @code{nil} if the event does not deliver a click
    count.}
  @begin{short}
    Extracts the click count from an event.
  @end{short}
  @see-class{gdk-event}
  @see-class{gdk-event-button}
  @see-function{gdk-event-button}"
  (with-foreign-object (click-count :uint)
    (when (%gdk-event-click-count event click-count)
      (mem-ref click-count :uint))))

(export 'gdk-event-click-count)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_coords () -> gdk-events-coords
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_coords" %gdk-event-coords) :boolean
  (event (g-boxed-foreign gdk-event))
  (x-win (:pointer :double))
  (y-win (:pointer :double)))

;; The Lisp implementation returns the values.

(defun gdk-event-coords (event)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @begin{return}
    @code{x-win} -- a @code{:double} with the event window x coordinate @br{}
    @code{y-win} -- a @code{:double} with the event window y coordinate
  @end{return}
  @begin{short}
    Extract the event window relative x/y coordinates from an event.
  @end{short}
  @see-class{gdk-event}
  @see-function{gdk-event-root-coords}"
  (with-foreign-objects ((x :double) (y :double))
    (when (%gdk-event-coords event x y)
      (values (mem-ref x :double)
              (mem-ref y :double)))))

(export 'gdk-event-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_keycode () -> gdk-event-keycode
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_keycode" %gdk-event-keycode) :boolean
  (event (g-boxed-foreign gdk-event))
  (keycode (:pointer :uint16)))

(defun gdk-event-keycode (event)
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{An unsigned integer with the keycode.}
  @begin{short}
    Extracts the hardware keycode from an event.
  @end{short}
  @see-class{gdk-event}
  @see-class{gdk-event-key}
  @see-function{gdk-event-keyval}"
  (with-foreign-object (keycode :uint16)
    (when (%gdk-event-keycode event keycode)
      (mem-ref keycode :uint16))))

(export 'gdk-event-keycode)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_keyval () -> gdk-event-keyval
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_keyval" %gdk-event-keyval) :boolean
  (event (g-boxed-foreign gdk-event))
  (keyval (:pointer :uint)))

(defun gdk-event-keyval (event)
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{An unsigned integer with the keyval.}
  @begin{short}
    Extracts the keyval from an event.
  @end{short}
  @see-class{gdk-event}
  @see-class{gdk-event-key}
  @see-function{gdk-event-keycode}"
  (with-foreign-object (keyval :uint)
    (when (%gdk-event-keyval event keyval)
      (mem-ref keyval :uint))))

(export 'gdk-event-keyval)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_root_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_root_coords" %gdk-event-root-coords) :boolean
  (event (g-boxed-foreign gdk-event))
  (x-win (:pointer :double))
  (y-win (:pointer :double)))

(defun gdk-event-root-coords (event)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @begin{return}
    @code{x-root} -- a @code{:double} with the root window x coordinate @br{}
    @code{y-root} -- a @code{:double} with the root window y coordinate
  @end{return}
  @begin{short}
    Extract the root window relative x/y coordinates from an event.
  @end{short}
  @see-class{gdk-event}
  @see-function{gdk-event-coords}"
  (with-foreign-objects ((x :double) (y :double))
    (when (%gdk-event-root-coords event x y)
      (values (mem-ref x :double)
              (mem-ref y :double)))))

(export 'gdk-event-root-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_scroll_direction ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_scroll_direction" %gdk-event-get-scroll-direction)
    :boolean
  (event (g-boxed-foreign gdk-event))
  (direction (:pointer gdk-scroll-direction)))

(defun gdk-event-get-scroll-direction (event)
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{The scroll direction of type @symbol{gdk-scroll-direction}.}
  @begin{short}
    Extracts the scroll direction from an event.
  @end{short}
  @see-class{gdk-event}
  @see-class{gdk-event-scroll}"
  (with-foreign-object (direction 'gdk-scroll-direction)
    (when (%gdk-event-get-scroll-direction event direction)
      (mem-ref direction 'gdk-scroll-direction))))

(export 'gdk-event-get-scroll-direction)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_scroll_deltas () -> gdk-event-scroll-deltas
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_scroll_deltas" %gdk-event-scroll-deltas) :boolean
  (event (g-boxed-foreign gdk-event))
  (delta-x (:pointer :double))
  (delta-y (:pointer :double)))

(defun gdk-event-scroll-deltas (event)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-24}
  @argument[event]{a @class{gdk-event} structure}
  @begin{return}
    @code{delta-x} -- a @code{:double} with x delta @br{}
    @code{delta-y} -- a @code{:double} with y delta
  @end{return}
  @begin{short}
    Retrieves the scroll deltas from an event.
  @end{short}
  See also the function @fun{gdk-event-get-scroll-direction}.
  @see-class{gdk-event-scroll}
  @see-function{gdk-event-get-scroll-direction}
  @see-function{gdk-event-scroll-delta-x}
  @see-function{gdk-event-scroll-delta-y}"
  (with-foreign-objects ((x :double) (y :double))
    (when (%gdk-event-scroll-deltas event x y)
      (values (mem-ref x :double)
              (mem-ref y :double)))))

(export 'gdk-event-scroll-deltas)

;;; ----------------------------------------------------------------------------
;;; gdk_event_is_scroll_stop_event ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun ("gdk_event_is_scroll_stop_event" gdk-event-is-scroll-stop-event)
    :boolean
 "@version{2021-4-5}
  @argument[event]{a @class{gdk-event} instance}
  @return{A boolean wether a scroll event is a stop scroll event.}
  @begin{short}
    Check whether a scroll event is a stop scroll event.
  @end{short}
  Scroll sequences with smooth scroll information may provide a stop scroll
  event once the interaction with the device finishes, e.g. by lifting a finger.
  This stop scroll event is the signal that a widget may trigger kinetic
  scrolling based on the current velocity.

  Stop scroll events always have a a delta of 0/0.

  Since 3.20
  @see-class{gdk-event}"
  (event (g-boxed-foreign gdk-event)))

#+gdk-3-20
(export 'gdk-event-is-scroll-stop-event)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_state () -> gdk-event-state
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_state" %gdk-event-state) :boolean
  (event (g-boxed-foreign gdk-event))
  (state (:pointer gdk-modifier-type)))

(defun gdk-event-state (event)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure or @code{nil}}
  @begin{return}
    @code{state} -- the state as a @symbol{gdk-modifier-type} value
  @end{return}
  @begin{short}
    If the event contains a \"state\" field, returns the value.
  @end{short}
  Otherwise returns an empty state (0). @arg{event} may be @code{nil}, in which
  case it is treated as if the event had no state field.
  @see-class{gdk-event}"
  (with-foreign-object (state 'gdk-modifier-type)
    (if (%gdk-event-state event state)
        (mem-ref state 'gdk-modifier-type)
        0)))

(export 'gdk-event-state)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_time () -> gdk-event-time
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_time" gdk-event-time) :uint32
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{An unsigned integer with the time stamp field from the event.}
  @begin{short}
    Returns the current time of the event.
  @end{short}
  If @arg{event} is @code{nil}, returns the value of the constant
  @var{+gdk-current-time+}.
  @see-class{gdk-event}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-time)

;;; ----------------------------------------------------------------------------
;;; GdkEventSequence
;;;
;;; typedef struct _GdkEventSequence GdkEventSequence;
;;; ----------------------------------------------------------------------------

;;; Implementation moved to gtk.event-structures.lisp

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_window ()
;;;
;;; GdkWindow *
;;; gdk_event_get_window (const GdkEvent *event);
;;;
;;; Extracts the GdkWindow associated with an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     The GdkWindow associated with the event.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;; Implemented as the accessor gdk-event-window

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_event_type ()
;;;
;;; GdkEventType gdk_event_get_event_type (const GdkEvent *event);
;;;
;;; Retrieves the type of the event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     a GdkEventType
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;; Implemented as the accessor gdk-event-type

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_event_sequence () -> gdk-event-event-sequence
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_event_sequence" gdk-event-event-sequence)
    (g-boxed-foreign gdk-event-sequence)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{The event sequence of type @class{gdk-event-sequence} that the event
    belongs to.}
  @begin{short}
    If the event is of type @code{:touch-begin}, @code{:touch-update},
    @code{:touch-end} or @code{:touch-cancel}, returns the
    @class{gdk-event-sequence} to which the event belongs.
  @end{short}
  Otherwise, return @code{nil}.
  @see-class{gdk-event}
  @see-class{gdk-event-sequence}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-event-sequence)

;;; ----------------------------------------------------------------------------
;;; gdk_event_request_motions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_request_motions" gdk-event-request-motions) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @begin{short}
    Request more motion notifies if the event is a motion notify hint event.
  @end{short}

  This function should be used instead of the function @fun{gdk-window-pointer}
  to request further motion notifies, because it also works for extension events
  where motion notifies are provided for devices other than the core pointer.

  Coordinate extraction, processing and requesting more motion events from a
  @code{GDK_MOTION_NOTIFY} event usually works like this:
  @begin{pre}
{
  /* motion_event handler */
  x = motion_event->x;
  y = motion_event->y;
  /* handle (x,y) motion */
  gdk_event_request_motions (motion_event); /* handles is_hint events */
@}
  @end{pre}
  @see-class{gdk-event}
  @see-function{gdk-window-pointer}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-request-motions)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_angle () -> gdk-events-angle
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_events_get_angle" %gdk-events-angle) :boolean
  (event-1 (g-boxed-foreign gdk-event))
  (event-2 (g-boxed-foreign gdk-event))
  (angle (:pointer :double)))

(defun gdk-events-angle (event-1 event-2)
 "@version{2020-8-25}
  @argument[event-1]{a @class{gdk-event} stucture}
  @argument[event-2]{a @class{gdk-event} stucture}
  @return{A @code{:double} with the relative angle between both events.}
  @begin{short}
    If both events contain X/Y information, this function will return the
    relative angle from @arg{event-1} to @arg{event-2}.
  @end{short}
  The rotation direction for positive angles is from the positive X axis
  towards the positive Y axis.
  @see-class{gdk-event}"
  (with-foreign-object (angle :double)
    (when (%gdk-events-angle event-1 event-2 angle)
      (mem-ref angle :double))))

(export 'gdk-events-angle)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_center () -> gdk-events-center
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_events_get_center" %gdk-events-center) :boolean
  (event-1 (g-boxed-foreign gdk-event))
  (event-2 (g-boxed-foreign gdk-event))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gdk-events-center (event-1 event-2)
 "@version{2020-8-25}
  @argument[event-1]{a @class{gdk-event} structure}
  @argument[event-2]{a @class{gdk-event} structure}
  @begin{return}
    @code{x} -- a @code{:double} with the x coordinate of the center @br{}
    @code{y} -- a @code{:double} with the y coordinate of the center
  @end{return}
  @begin{short}
    If both events contain X/Y information, the center of both coordinates will
    be returned in x and y.
  @end{short}
  @see-class{gdk-event}"
  (with-foreign-objects ((x :double) (y :double))
    (when (%gdk-events-center event-1 event-2 x y)
     (values (mem-ref x :double)
             (mem-ref y :double)))))

(export 'gdk-events-center)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_distance () -> gdk-events-distance
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_events_get_distance" %gdk-events-distance) :boolean
  (event-1 (g-boxed-foreign gdk-event))
  (event-2 (g-boxed-foreign gdk-event))
  (distance (:pointer :double)))

(defun gdk-events-distance (event-1 event-2)
 "@version{2020-8-25}
  @argument[event-1]{a @class{gdk-event} structure}
  @argument[event-2]{a @class{gdk-event} structure}
  @return{A @code{:double} with the distance.}
  @begin{short}
    If both events have X/Y information, the distance between both coordinates,
    will be returned, as in a straight line going from @code{event-1} to
    @code{event-2}.
  @end{short}
  @see-class{gdk-event}"
  (with-foreign-object (distance :double)
    (when (%gdk-events-distance event-1 event-2 distance)
      (mem-ref distance :double))))

(export 'gdk-events-distance)

;;; ----------------------------------------------------------------------------
;;; gdk_event_triggers_context_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_triggers_context_menu" gdk-event-triggers-context-menu)
    :boolean
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{@em{True} if the event should trigger a contect menu.}
  @begin{short}
    This function returns whether a @class{gdk-event-button} should trigger a
    context menu, according to platform conventions.
  @end{short}
  The right mouse button always triggers context menus. Additionally, if
  the function @fun{gdk-keymap-modifier-mask} returns a non-0 mask for the
  value @code{:context-menu} of the @symbol{gdk-modifier-intent} enumeration,
  then the left mouse button will also trigger a context menu if this modifier
  is pressed.

  This function should always be used instead of simply checking for
  @code{event->button == GDK_BUTTON_SECONDARY}.
  @see-class{gdk-event}
  @see-class{gdk-event-button}
  @see-symbol{gdk-modifier-intent}
  @see-function{gdk-keymap-modifier-mask}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-triggers-context-menu)

;; -----------------------------------------------------------------------------
;;; gdk_event_get_seat ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun ("gdk_event_get_seat" gdk-event-seat) (g-object gdk-seat)
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{The @class{gdk-seat} object of this event.}
  @begin{short}
    Returns the seat this event was generated for.
  @end{short}

  Since 3.20
  @see-class{gdk-event}
  @see-class{gdk-seat}"
  (event (g-boxed-foreign gdk-event)))

#+gdk-3-20
(export 'gdk-event-seat)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_scancode () -> gdk-event-scancode
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(defcfun ("gdk_event_get_scancode" gdk-event-scancode) :int
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{A @code{:int} with the associated keyboard scancode or 0.}
  @begin{short}
    Gets the keyboard low-level scancode of a key event.
  @end{short}

  This is usually the @code{hardware-keycode} slot of the event. On Windows
  this is the high word of @code{WM_KEY{DOWN,UP@}} which contains the scancode
  and some extended flags.

  Since 3.22
  @see-class{gdk-event}"
  (event (g-boxed-foreign gdk-event)))

#+gdk-3-22
(export 'gdk-event-scancode)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_pointer_emulated () -> gdk-event-pointer-emulated
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(defcfun ("gdk_event_get_pointer_emulated" gdk-event-pointer-emulated) :boolean
 "@version{2020-8-25}
  @argument[event]{a @class{gdk-event} structure}
  @return{@em{True} if this event is emulated.}
  @begin{short}
    Returns whether this event is an 'emulated' pointer event (typically from a
    touch event), as opposed to a real one.
  @end{short}

  Since 3.22
  @see-class{gdk-event}"
  (event (g-boxed-foreign gdk-event)))

#+gdk-3-22
(export 'gdk-event-pointer-emulated)

;;; ----------------------------------------------------------------------------
;;; GdkEventFunc ()
;;;
;;; void (*GdkEventFunc) (GdkEvent *event, gpointer data);
;;;
;;; Specifies the type of function passed to gdk_event_handler_set() to handle
;;; all GDK events.
;;;
;;; event :
;;;     the GdkEvent to process.
;;;
;;; data :
;;;     user data set when the event handler was installed with
;;;     gdk_event_handler_set()
;;; ----------------------------------------------------------------------------

(defcallback gdk-event-func-callback :void
    ((event (g-boxed-foreign gdk-event)) (user-data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value user-data) event)
    (return-from-callback () nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_event_handler_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_handler_set" %gdk-event-handler-set) :void
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun gdk-event-handler-set (func)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-21}
  @argument[func]{the function to call to handle events from GDK}
  @begin{short}
    Sets the function to call to handle all events from GDK.
  @end{short}

  Note that GTK+ uses this to install its own event handler, so it is usually
  not useful for GTK+ applications. Although an application can call this
  function then call the function @fun{gtk-main-do-event} to pass events to
  GTK+.
  @see-class{gdk-event}
  @see-function{gtk-main-do-event}"
  (%gdk-event-handler-set (callback gdk-event-func-callback)
                          (allocate-stable-pointer func)
                          (callback stable-pointer-destroy-notify-cb)))

(export 'gdk-event-handler-set)

;;; ----------------------------------------------------------------------------
;;; gdk_get_show_events ()
;;; gdk_set_show_events () -> gdk-show-events
;;; ----------------------------------------------------------------------------

(defun (setf gdk-show-events) (show-events)
  (foreign-funcall "gdk_set_show_events" :boolean show-events :void)
  show-events)

(defcfun ("gdk_get_show_events" gdk-show-events) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @syntax[]{(gdk-show-events) => show-events}
  @syntax[]{(setf (gdk-show-events) show-events)}
  @argument[show-events]{@em{True} to output event debugging information.}
  @begin{short}
    Wether event debugging output is enabled.
  @end{short}

  The function @sym{gdk-show-events} gets whether event debugging output is
  enabled. The function @sym{(setf gdk-show-events)} sets whether a trace of
  received events is output.

  Note that GTK+ must be compiled with debugging (that is, configured using the
  @code{--enable-debug} option) to use this option.
  @see-class{gdk-event}")

(export 'gdk-show-events)

;;; ----------------------------------------------------------------------------
;;; gdk_event_set_screen ()
;;; gdk_event_get_screen () -> gdk-event-screen
;;; ----------------------------------------------------------------------------

(defun (setf gdk-event-screen) (screen event)
  (foreign-funcall "gdk_event_set_screen"
                   (g-boxed-foreign gdk-event) event
                   (g-object gdk-screen) screen
                   :void)
  screen)

(defcfun ("gdk_event_get_screen" gdk-event-screen) (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @syntax[]{(gdk-event-screen event) => screen}
  @syntax[]{(setf (gdk-event-screen event) screen)}
  @argument[event]{a @class{gdk-event} structure}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Accessor of the screen of an event.
  @end{short}

  The function @sym{gdk-event-screen} returns the screen for the event. The
  function @sym{(setf gdk-event-screen)} sets the screen of the event.

  The event must have been allocated by GTK+, for instance, by the function
  @fun{gdk-event-copy}.

  The screen is typically the screen for @code{event->any.window}, but for
  events such as mouse events, it is the screen where the pointer was when the
  event occurs - that is, the screen which has the root window to which
  @code{event->motion.x_root} and @code{event->motion.y_root} are relative.
  @see-class{gdk-event}
  @see-class{gdk-screen}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device ()
;;; gdk_event_set_device () -> gdk-event-device
;;; ----------------------------------------------------------------------------

(defun (setf gdk-event-device) (device event)
  (foreign-funcall "gdk_event_set_device"
                   (g-boxed-foreign gdk-event) event
                   (g-object gdk-device) device
                   :void)
  device)

(defcfun ("gdk_event_get_device" gdk-event-device) (g-object gdk-device)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @syntax[]{(gdk-event-device event) => device}
  @syntax[]{(setf (gdk-event-device event) device)}
  @argument[event]{a @class{gdk-event} structure}
  @argument[device]{a @class{gdk-device} object}
  @begin{short}
    Accessor of the \"device\" field of an event.
  @end{short}

  If the event contains a \"device\" field, the function @sym{gdk-event-device}
  will return it, else it will return @code{nil}. The function
  @sym{(setf gdk-event-device)} sets the device for an event.

  The event must have been allocated by GTK+, for instance, by the function
  @fun{gdk-event-copy}.
  @see-class{gdk-event}
  @see-class{gdk-device}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-device)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_source_device ()
;;; gdk_event_set_source_device () -> gdk-event-source-device
;;; ----------------------------------------------------------------------------

(defun (setf gdk-event-source-device) (device event)
  (foreign-funcall "gdk_event_set_source_device"
                   (g-boxed-foreign gdk-event) event
                   (g-object gdk-device) device
                   :void)
  device)

(defcfun ("gdk_event_get_source_device" gdk-event-source-device)
    (g-object gdk-device)
 #+cl-cffi-gtk-documentation
 "@version{2020-8-25}
  @syntax[]{(gdk-event-source-device event) => device}
  @syntax[]{(setf (gdk-event-source-device event) device)}
  @argument[event]{a @class{gdk-event} structure}
  @argument[device]{a @class{gdk-device} object}
  @begin{short}
    Accessor of the slave device for the event.
  @end{short}

  The function @sym{gdk-event-source-device} returns the hardware (slave)
  device that has triggered the event, falling back to the virtual (master)
  device, as in the function @fun{gdk-event-device}, if the event was not
  caused by interaction with a hardware device. The function
  @sym{(setf gdk-event-source-device)} sets the slave device for the event to
  @arg{device}.

  This may happen for example in synthesized crossing events after a
  @class{gdk-window} object updates its geometry or a grab is acquired/released.

  If the event does not contain a device field, this function will return
  @code{nil}.

  The event must have been allocated by GTK+, for instance by the function
  @fun{gdk-event-copy}.
  @see-class{gdk-event}
  @see-class{gdk-device}
  @see-class{gdk-window}
  @see-function{gdk-event-device}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-source-device)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device_tool ()
;;; gdk_event_set_device_tool () -> gdk-event-device-tool
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(defun (setf gdk-event-device-tool) (tool event)
  (foreign-funcall "gdk_event_set_device_tool"
                   (g-boxed-foreign gdk-event) event
                   (g-object gdk-device-tool) tool
                   :void)
  tool)

#+gdk-3-22
(defcfun ("gdk_event_get_device_tool" gdk-event-device-tool)
    (g-object gdk-device-tool)
 "@version{2020-8-25}
  @syntax[]{(gdk-event-device-tool event) => tool}
  @syntax[]{(setf (gdk-event-device-tool event) tool)}
  @argument[event]{a @class{gdk-event} structure}
  @argument[tool]{a @class{gdk-device-tool} object}
  @begin{short}
    Accessor of the device tool representing the tool that caused the event.
  @end{short}

  If the event was generated by a device that supports different tools (e.g. a
  tablet), the function @sym{gdk-event-device-tool} will return a
  @class{gdk-device-tool} representing the tool that caused the event.
  Otherwise, @code{nil} will be returned. The function
  @sym{(setf gdk-event-device-tool)} sets the device tool for this event,
  should be rarely used.

  Note: The @class{gdk-device-tool} object will be constant during the
  application lifetime, if settings must be stored persistently across runs,
  see the function @fun{gdk-device-tool-get-serial}.

  Since 3.22
  @see-class{gdk-event}
  @see-class{gdk-device-tool}
  @see-function{gdk-device-tool-get-serial}"
  (event (g-boxed-foreign gdk-event)))

#+gdk-3-22
(export 'gdk-event-device-tool)

;;; ----------------------------------------------------------------------------
;;; gdk_setting_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_setting_get" %gdk-setting-get) :boolean
  (name :string)
  (value (:pointer (:struct g-value))))

(defun gdk-setting-get (name gtype)
 #+cl-cffi-gtk-documentation
 "@version{2019-4-7}
  @argument[name]{a @code{:string} with the name of the setting}
  @argument[gtype]{a @code{:string} with the GType of the setting}
  @begin{return}
    The value of the setting.
  @end{return}
  @begin{short}
    Obtains a desktop-wide setting, such as the double-click time, for the
    default screen.
  @end{short}
  See the function @fun{gdk-screen-setting}
  @begin[Example]{dictionary}
    @begin{pre}
  (gdk-setting-get \"gtk-double-click-time\" \"gint\")
=> 400
    @end{pre}
  @end{dictionary}
  @see-function{gdk-screen-setting}"
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value gtype)
    (when (%gdk-setting-get name value)
      (prog1
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gdk-setting-get)

;;; --- gdk.events.lisp --------------------------------------------------------
