;;; ----------------------------------------------------------------------------
;;; gdk.events.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     GdkEventType --> gdk.event-structures.lisp
;;;     GdkEventMask --> gdk.event-structures.lisp
;;;     GdkEventSequence  --> gdk.event-structures.lisp
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
 "@version{2013-3-11}
  Represents the current time, and can be used anywhere a time is expected.")

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
 "@version{2013-6-14}
  This is the priority that the idle handler processing window updates is
  given in the GLib Main Loop.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gdk-priority-redraw atdoc:*variable-name-alias*) "Constant")

(export '+gdk-priority-redraw+)

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_PROPAGATE
;;; ----------------------------------------------------------------------------

(defconstant +gdk-event-propagate+ nil
 #+cl-cffi-gtk-documentation
 "@version{2013-9-20}
  @begin{short}
    Use this value as the return value for continuing the propagation of an
    event handler.
  @end{short}

  Since 3.4
  @see-variable{+gdk-event-stop+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+gdk-event-propagate+ atdoc:*variable-name-alias*) "Constant")

(export '+gdk-event-propagate+)

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_STOP
;;; ----------------------------------------------------------------------------

(defconstant +gdk-event-stop+ t
 #+cl-cffi-gtk-documentation
 "@version{2013-9-20}
  @begin{short}
    Use this value as the return value for stopping the propagation of an
    event handler.
  @end{short}

  Since 3.4
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
 "@version{2013-9-21}
  @return{@em{True} if any events are pending.}
  Checks if any events are ready to be processed for any display.
  @see-class{gdk-event}
  @see-function{gdk-event-peek}")

(export 'gdk-events-pending)

;;; ----------------------------------------------------------------------------
;;; gdk_event_peek ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_peek" gdk-event-peek) (g-boxed-foreign gdk-event :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-21}
  @begin{return}
    A copy of the first @class{gdk-event} on some event queue, or @code{nil} if
    no events are in any queues.
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
 "@version{2013-9-21}
  @begin{return}
    The next @class{gdk-event} to be processed, or @code{nil} if no events are
    pending.
  @end{return}
  @begin{short}
    Checks all open displays for a @class{gdk-event} to process, to be processed
    on, fetching events from the windowing system if necessary.
  @end{short}
  See the function @fun{gdk-display-get-event}.
  @see-class{gdk-event}
  @see-function{gdk-display-get-event}")

(export 'gdk-event-get)

;;; ----------------------------------------------------------------------------
;;; gdk_event_put ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_put" gdk-event-put) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-21}
  @argument[event]{a @class{gdk-event} structure}
  @begin{short}
    Appends a copy of the given event onto the front of the event queue for
    @code{event->any.window}'s display, or the default event queue if
    @code{event->any.window} is @code{nil}.
  @end{short}
  See the function @fun{gdk-display-put-event}.
  @see-class{gdk-event}
  @see-function{gdk-display-put-event}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-put)

;;; ----------------------------------------------------------------------------
;;; gdk_event_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_new" gdk-event-new) (g-boxed-foreign gdk-event)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-20}
  @argument[type]{a @symbol{gdk-event-type}}
  @return{A newly-allocated @class{gdk-event}.}
  @begin{short}
    Creates a new event of the given type. All fields are set to 0.
  @end{short}
  @see-class{gdk-event}
  @see-symbol{gdk-event-type}"
  (type gdk-event-type))

(export 'gdk-event-new)

;;; ----------------------------------------------------------------------------
;;; gdk_event_copy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gdk-event-copy))

(defun gdk-event-copy (event)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-20}
  @argument[event]{a @class{gdk-event}}
  @return{A copy of event.}
  Copies a @class{gdk-event}, copying or incrementing the reference count of the
  resources associated with it, e. g. @class{gdk-window}'s and strings.
  @see-class{gdk-event}
  @see-class{gdk-window}"
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
;;; gdk_event_get_axis ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_axis" %gdk-event-get-axis) :boolean
  (event (g-boxed-foreign gdk-event))
  (axis-use gdk-axis-use)
  (value (:pointer :double)))

(defun gdk-event-get-axis (event axis-use)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-15}
  @argument[event]{a @class{gdk-event} structure}
  @argument[axis-use]{the axis use to look for}
  @return{@code{value} -- the value found}
  Extract the axis value for a particular axis use from an event structure.
  @see-class{gdk-event}"
  (with-foreign-object (value :double)
    (when (%gdk-event-get-axis event axis-use value)
      (mem-ref value :double))))

(export 'gdk-event-get-axis)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_button ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_button" %gdk-event-get-button) :boolean
  (event (g-boxed-foreign gdk-event))
  (button (:pointer :uint)))

(defun gdk-event-get-button (event)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-21}
  @argument[event]{a @class{gdk-event}}
  @return{Mouse button number, or @code{nil} if the @arg{event} does not deliver
    a button number.}
  @short{Extract the button number from an event.}

  Since 3.2
  @see-class{gdk-event}
  @see-function{gdk-event-get-click-count}"
  (with-foreign-object (button :uint)
    (when (%gdk-event-get-button event button)
      (mem-ref button :uint))))

(export 'gdk-event-get-button)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_click_count ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_click_count" %gdk-event-get-click-count) :boolean
  (event (g-boxed-foreign gdk-event))
  (click-count (:pointer :uint)))

(defun gdk-event-get-click-count (event)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-21}
  @argument[event]{a @class{gdk-event}}
  @return{click count, or @code{nil} if the event does not delivered
    a click count}
  @begin{short}
    Extracts the click count from an event.
  @end{short}

  Since 3.2
  @see-class{gdk-event}"
  (with-foreign-object (click-count :uint)
    (when (%gdk-event-get-click-count event click-count)
      (mem-ref click-count :uint))))

(export 'gdk-event-get-click-count)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_coords" %gdk-event-get-coords) :boolean
  (event (g-boxed-foreign gdk-event))
  (x-win (:pointer :double))
  (y-win (:pointer :double)))

;; The Lisp implementation returns the values.

(defun gdk-event-get-coords (event)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-15}
  @argument[event]{a @class{gdk-event} structure}
  @begin{return}
    @code{x-win} -- event window x coordinate @br{}
    @code{y-win} -- event window y coordinate @br{}
  @end{return}
  Extract the event window relative x/y coordinates from an event."
  (with-foreign-objects ((x :double) (y :double))
    (when (%gdk-event-get-coords event x y)
      (values (mem-ref x :double)
              (mem-ref y :double)))))

(export 'gdk-event-get-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_keycode ()
;;;
;;; gboolean gdk_event_get_keycode (const GdkEvent *event, guint16 *keycode);
;;;
;;; Extracts the hardware keycode from an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; keycode :
;;;     location to store the keycode
;;;
;;; Returns :
;;;     TRUE if the event delivered a hardware keycode
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_keyval ()
;;;
;;; gboolean gdk_event_get_keyval (const GdkEvent *event, guint *keyval);
;;;
;;; Extracts the keyval from an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; keyval :
;;;     location to store the keyval
;;;
;;; Returns :
;;;     TRUE if the event delivered a key symbol
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_root_coords ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_root_coords" %gdk-event-get-root-coords) :boolean
  (event (g-boxed-foreign gdk-event))
  (x-win (:pointer :double))
  (y-win (:pointer :double)))

(defun gdk-event-get-root-coords (event)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-15}
  @argument[event]{a @class{gdk-event} structure}
  @begin{return}
    @code{x-root} -- root window x coordinate @br{}
    @code{y-root} -- root window y coordinate
  @end{return}
  Extract the root window relative x/y coordinates from an event."
  (with-foreign-objects ((x :double) (y :double))
    (when (%gdk-event-get-root-coords event x y)
      (values (mem-ref x :double)
              (mem-ref y :double)))))

(export 'gdk-event-get-root-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_scroll_direction ()
;;;
;;; gboolean gdk_event_get_scroll_direction (const GdkEvent *event,
;;;                                          GdkScrollDirection *direction);
;;;
;;; Extracts the scroll direction from an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; direction :
;;;     location to store the scroll direction
;;;
;;; Returns :
;;;     TRUE if the event delivered a scroll direction
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_scroll_deltas ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_scroll_deltas" %gdk-event-get-scroll-deltas) :boolean
  (event (g-boxed-foreign gdk-event))
  (delta-x (:pointer :double))
  (delta-y (:pointer :double)))

(defun gdk-event-get-scroll-deltas (event)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-15}
  @argument[event]{a @class{gdk-event} structure}
  @begin{return}
    @code{delta-x} -- X delta @br{}
    @code{delta-y} -- Y delta
  @end{return}
  @begin{short}
    Retrieves the scroll deltas from a @class{gdk-event} structure.
  @end{short}

  Since 3.4"
  (with-foreign-objects ((x :double) (y :double))
    (when (%gdk-event-get-scroll-deltas event x y)
      (values (mem-ref x :double)
              (mem-ref y :double)))))

(export 'gdk-event-get-scroll-deltas)

;;; ----------------------------------------------------------------------------
;;; gdk_event_is_scroll_stop_event ()
;;;
;;; gboolean
;;; gdk_event_is_scroll_stop_event (const GdkEvent *event);
;;;
;;; Check whether a scroll event is a stop scroll event. Scroll sequences with
;;; smooth scroll information may provide a stop scroll event once the
;;; interaction with the device finishes, e.g. by lifting a finger. This stop
;;; scroll event is the signal that a widget may trigger kinetic scrolling based
;;; on the current velocity.
;;;
;;; Stop scroll events always have a a delta of 0/0.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     TRUE if the event is a scroll stop event
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_state" %gdk-event-get-state) :boolean
  (event (g-boxed-foreign gdk-event))
  (state (:pointer gdk-modifier-type)))

(defun gdk-event-get-state (event)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-15}
  @argument[event]{a @class{gdk-event} structure or @code{nil}}
  @begin{return}
    @code{state} -- state
  @end{return}
  @begin{short}
    If the event contains a \"state\" field, puts that field in state.
  @end{short}
  Otherwise stores an empty state (0). @arg{event} may be @code{nil}, in which
  case it is treated as if the event had no state field."
  (with-foreign-object (state 'gdk-modifier-type)
    (when (%gdk-event-get-state event state)
      (mem-ref state 'gdk-modifier-type))))

(export 'gdk-event-get-state)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_time" gdk-event-get-time) :uint32
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[event]{a @class{gdk-event} structure}
  @return{Time stamp field from event.}
  Returns the current time of @arg{event}. If @arg{event} is @code{nil}, returns
  @variable{+gdk-current-time+}."
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-get-time)

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

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_event_sequence ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_event_sequence" gdk-event-get-event-sequence)
    (g-boxed-foreign gdk-event-sequence)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-22}
  @argument[event]{a @class{gdk-event}}
  @return{The event sequence that the event belongs to.}
  @begin{short}
    If @arg{event} is of type @code{:touch-begin}, @code{:touch-update},
    @code{:touch-end} or @code{:touch-cancel}, returns the
    @class{gdk-event-sequence} to which the @arg{event} belongs.
  @end{short}
  Otherwise, return @code{nil}.

  Since 3.4
  @see-class{gdk-event-sequence}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-get-event-sequence)

;;; ----------------------------------------------------------------------------
;;; gdk_event_request_motions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_request_motions" gdk-event-request-motions) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[event]{a valid @class{gdk-event} structure}
  @begin{short}
    Request more motion notifies if event is a motion notify hint event.
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
  @see-function{gdk-window-pointer}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-request-motions)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_angle ()
;;;
;;; gboolean gdk_events_get_angle (GdkEvent *event1,
;;;                                GdkEvent *event2,
;;;                                gdouble *angle);
;;;
;;; If both events contain X/Y information, this function will return TRUE and
;;; return in angle the relative angle from event1 to event2. The rotation
;;; direction for positive angles is from the positive X axis towards the
;;; positive Y axis.
;;;
;;; event1 :
;;;     first GdkEvent
;;;
;;; event2 :
;;;     second GdkEvent
;;;
;;; angle :
;;;     return location for the relative angle between both events
;;;
;;; Returns :
;;;     TRUE if the angle could be calculated.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_center ()
;;;
;;; gboolean gdk_events_get_center (GdkEvent *event1,
;;;                                 GdkEvent *event2,
;;;                                 gdouble *x,
;;;                                 gdouble *y);
;;;
;;; If both events contain X/Y information, the center of both coordinates will
;;; be returned in x and y.
;;;
;;; event1 :
;;;     first GdkEvent
;;;
;;; event2 :
;;;     second GdkEvent
;;;
;;; x :
;;;     return location for the X coordinate of the center
;;;
;;; y :
;;;     return location for the Y coordinate of the center
;;;
;;; Returns :
;;;     TRUE if the center could be calculated.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_distance ()
;;;
;;; gboolean gdk_events_get_distance (GdkEvent *event1,
;;;                                   GdkEvent *event2,
;;;                                   gdouble *distance);
;;;
;;; If both events have X/Y information, the distance between both coordinates
;;; (as in a straight line going from event1 to event2) will be returned.
;;;
;;; event1 :
;;;     first GdkEvent
;;;
;;; event2 :
;;;     second GdkEvent
;;;
;;; distance :
;;;     return location for the distance
;;;
;;; Returns :
;;;     TRUE if the distance could be calculated.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_triggers_context_menu ()
;;;
;;; gboolean gdk_event_triggers_context_menu (const GdkEvent *event);
;;;
;;; This function returns whether a GdkEventButton should trigger a context
;;; menu, according to platform conventions. The right mouse button always
;;; triggers context menus. Additionally, if gdk_keymap_get_modifier_mask()
;;; returns a non-0 mask for GDK_MODIFIER_INTENT_CONTEXT_MENU, then the left
;;; mouse button will also trigger a context menu if this modifier is pressed.
;;;
;;; This function should always be used instead of simply checking for
;;; event->button == GDK_BUTTON_SECONDARY.
;;;
;;; event :
;;;     a GdkEvent, currently only button events are meaningful values
;;;
;;; Returns :
;;;     TRUE if the event should trigger a context menu.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

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
      (funcall (glib::get-stable-pointer-value user-data) event)
    (return-from-callback () nil)))

;; -----------------------------------------------------------------------------
;;; gdk_event_get_seat ()
;;;
;;; GdkSeat * gdk_event_get_seat (const GdkEvent *event);
;;;
;;; Returns the GdkSeat this event was generated for.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     The GdkSeat of this event.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_scancode ()
;;;
;;; int gdk_event_get_scancode (GdkEvent *event);
;;;
;;; Gets the keyboard low-level scancode of a key event.
;;;
;;; This is usually hardware_keycode. On Windows this is the high word of
;;; WM_KEY{DOWN,UP} lParam which contains the scancode and some extended flags.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     The associated keyboard scancode or 0
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_pointer_emulated ()
;;;
;;; gboolean gdk_event_get_pointer_emulated (GdkEvent *event);
;;;
;;; Returns whether this event is an 'emulated' pointer event (typically from a
;;; touch event), as opposed to a real one.
;;;
;;; Returns :
;;;     TRUE if this event is emulated
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_handler_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_handler_set" %gdk-event-handler-set) :void
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun gdk-event-handler-set (func)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[func]{the function to call to handle events from GDK}
  @begin{short}
    Sets the function to call to handle all events from GDK.
  @end{short}

  Note that GTK+ uses this to install its own event handler, so it is usually
  not useful for GTK+ applications. Although an application can call this
  function then call the function @fun{gtk-main-do-event} to pass events to
  GTK+.
  @see-function{gtk-main-do-event}"
  (%gdk-event-handler-set (callback gdk-event-func-callback)
                          (glib:allocate-stable-pointer func)
                          (callback glib:stable-pointer-destroy-notify-cb)))

(export 'gdk-event-handler-set)

;;; ----------------------------------------------------------------------------
;;; gdk_get_show_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_show_events" gdk-get-show-events) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @return{@em{True} if event debugging output is enabled.}
  Gets whether event debugging output is enabled.")

(export 'gdk-get-show-events)

;;; ----------------------------------------------------------------------------
;;; gdk_set_show_events ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_set_show_events" gdk-set-show-events) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[show-events]{@em{True} to output event debugging information.}
  @begin{short}
    Sets whether a trace of received events is output.
  @end{short}
  Note that GTK+ must be compiled with debugging (that is, configured using the
  @code{--enable-debug} option) to use this option."
  (show-events :boolean))

(export 'gdk-set-show-events)

;;; ----------------------------------------------------------------------------
;;; gdk_event_set_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_set_screen" gdk-event-set-screen) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-21}
  @argument[event]{a @class{gdk-event}}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Sets the screen for @arg{event} to @arg{screen}.
  @end{short}
  The event must have been allocated by GTK+, for instance, by the function
  @fun{gdk-event-copy}.
  @see-class{gdk-event}
  @see-class{gdk-screen}"
  (event (g-boxed-foreign gdk-event))
  (screen (g-object gdk-screen)))

(export 'gdk-event-set-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_screen" gdk-event-get-screen) (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-21}
  @argument[event]{a @class{gdk-event}}
  @return{The screen for the event.}
  @begin{short}
    Returns the screen for the event.
  @end{short}
  The screen is typically the screen for event->any.window, but for events such
  as mouse events, it is the screen where the pointer was when the event occurs
  - that is, the screen which has the root window to which event->motion.x_root
  and event->motion.y_root are relative.
  @see-class{gdk-event}
  @see-class{gdk-screen}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-get-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_device" gdk-event-get-device) (g-object gdk-device)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-7}
  @argument[event]{a @class{gdk-event} event}
  @return{A @class{gdk-device} object, or @code{nil}.}
  @begin{short}
    If the event contains a \"device\" field, this function will return it,
    else it will return @code{nil}.
  @end{short}

  Since 3.0"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-get-device)

;;; ----------------------------------------------------------------------------
;;; gdk_event_set_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_set_device" gdk-event-set-device) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-21}
  @argument[event]{a @class{gdk-event}}
  @argument[device]{a @class{gdk-device} object}
  @begin{short}
    Sets the device for @arg{event} to @arg{device}.
  @end{short}
  The event must have been allocated by GTK+, for instance, by the function
  @fun{gdk-event-copy}.

  Since 3.0
  @see-class{gdk-event}
  @see-class{gdk-device}"
  (event (g-boxed-foreign gdk-event))
  (device (g-object gdk-device)))

(export 'gdk-event-set-device)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_source_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_source_device" gdk-event-get-source-device)
    (g-object gdk-device)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-21}
  @argument[event]{a @class{gdk-event}}
  @return{A @class{gdk-device}, or @code{nil}.}
  @begin{short}
    This function returns the hardware (slave) @class{gdk-device} that has
    triggered the event, falling back to the virtual (master) device, as in
    the function @fun{gdk-event-get-device}, if the event was not caused by
    interaction with a hardware device.
  @end{short}
  This may happen for example in synthesized crossing events after a
  @class{gdk-window} updates its geometry or a grab is acquired/released.

  If the event does not contain a device field, this function will return
  @code{nil}.

  Since 3.0
  @see-class{gdk-event}
  @see-class{gdk-device}
  @see-class{gdk-window}
  @see-function{gdk-event-get-device}"
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-get-source-device)

;;; ----------------------------------------------------------------------------
;;; gdk_event_set_source_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_set_source_device" gdk-event-set-source-device) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-9-21}
  @argument[event]{a @class{gdk-event}}
  @argument[device]{a @class{gdk-device} object}
  @begin{short}
    Sets the slave device for @arg{event} to @arg{device}.
  @end{short}

  The event must have been allocated by GTK+, for instance by the function
  @fun{gdk-event-copy}.

  Since 3.0
  @see-class{gdk-event}
  @see-class{gdk-device}
  @see-function{gdk-event-copy}"
  (event (g-boxed-foreign gdk-event))
  (device (g-object gdk-device)))

(export 'gdk-event-set-source-device)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device_tool ()
;;;
;;; GdkDeviceTool * gdk_event_get_device_tool (const GdkEvent *event);
;;;
;;; If the event was generated by a device that supports different tools (eg. a
;;; tablet), this function will return a GdkDeviceTool representing the tool
;;; that caused the event. Otherwise, NULL will be returned.
;;;
;;; Note: the GdkDeviceTool<!-- -->s will be constant during the application
;;; lifetime, if settings must be stored persistently across runs, see
;;; gdk_device_tool_get_serial()
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     The current device tool, or NULL.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_set_device_tool ()
;;;
;;; void
;;; gdk_event_set_device_tool (GdkEvent *event,
;;;                            GdkDeviceTool *tool);
;;;
;;; Sets the device tool for this event, should be rarely used.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; tool :
;;;     tool to set on the event, or NULL.
;;;
;;; Since 3.22
;;; ----------------------------------------------------------------------------

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
  See the function @fun{gdk-screen-get-setting}
  @begin[Example]{dictionary}
    @begin{pre}
  (gdk-setting-get \"gtk-double-click-time\" \"gint\")
=> 400
    @end{pre}
  @end{dictionary}
  @see-function{gdk-screen-get-setting}"
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value gtype)
    (when (%gdk-setting-get name value)
      (prog1
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gdk-setting-get)

;;; --- gdk.events.lisp --------------------------------------------------------
