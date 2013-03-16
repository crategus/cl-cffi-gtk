;;; ----------------------------------------------------------------------------
;;; gdk.events.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See yhttp://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org. See <http://www.gtk.org>.
;;; The API  documentation of the Lisp binding is available at
;;; <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; Functions for handling events from the window system
;;;
;;; Synopsis
;;;
;;;     GdkEventType --> gdk.event-structures.lisp
;;;     GdkEventMask --> gdk.event-structures.lisp
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
;;;     gdk_event_get_state
;;;     gdk_event_get_time
;;;
;;;     GdkEventSequence
;;;
;;;     gdk_event_get_event_sequence
;;;     gdk_event_request_motions
;;;     gdk_events_get_angle
;;;     gdk_events_get_center
;;;     gdk_events_get_distance
;;;     gdk_event_triggers_context_menu
;;;
;;;     gdk_event_handler_set
;;;
;;;     gdk_get_show_events
;;;     gdk_set_show_events
;;;     gdk_event_set_screen
;;;     gdk_event_get_screen
;;;     gdk_event_get_device
;;;     gdk_event_set_device
;;;     gdk_event_get_source_device
;;;     gdk_event_set_source_device
;;;
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
;;; #define             GDK_PRIORITY_EVENTS
;;;
;;; This is the priority that events from the X server are given in the GLib
;;; Main Loop.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PRIORITY_REDRAW
;;;
;;; #define GDK_PRIORITY_REDRAW (G_PRIORITY_HIGH_IDLE + 20)
;;;
;;; This is the priority that the idle handler processing window updates is
;;; given in the GLib Main Loop.
;;; ----------------------------------------------------------------------------

(defconstant gdk-priority-redraw (+ g-priority-high-idle 20))

(export 'gdk-priority-redraw)

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_PROPAGATE
;;;
;;; #define GDK_EVENT_PROPAGATE (FALSE)
;;;
;;; Use this macro as the return value for continuing the propagation of an
;;; event handler.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_STOP
;;;
;;; #define GDK_EVENT_STOP (TRUE)
;;;
;;; Use this macro as the return value for stopping the propagation of an event
;;; handler.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

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
;;;
;;; gboolean gdk_events_pending (void);
;;;
;;; Checks if any events are ready to be processed for any display.
;;;
;;; Returns :
;;;     TRUE if any events are pending.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_events_pending" gdk-events-pending) :boolean)

(export 'gdk-events-pending)

;;; ----------------------------------------------------------------------------
;;; gdk_event_peek ()
;;;
;;; GdkEvent * gdk_event_peek (void);
;;;
;;; If there is an event waiting in the event queue of some open display,
;;; returns a copy of it. See gdk_display_peek_event().
;;;
;;; Returns :
;;;     a copy of the first GdkEvent on some event queue, or NULL if no events
;;;     are in any queues. The returned GdkEvent should be freed with
;;;     gdk_event_free().
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_peek" gdk-event-peek) (g-boxed-foreign gdk-event :return))

(export 'gdk-event-peek)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get ()
;;;
;;; GdkEvent * gdk_event_get (void);
;;;
;;; Checks all open displays for a GdkEvent to process,to be processed on,
;;; fetching events from the windowing system if necessary. See
;;; gdk_display_get_event().
;;;
;;; Returns :
;;;     the next GdkEvent to be processed, or NULL if no events are pending. The
;;;     returned GdkEvent should be freed with gdk_event_free().
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get" gdk-event-get) (g-boxed-foreign gdk-event :return))

(export 'gdk-event-get)

;;; ----------------------------------------------------------------------------
;;; gdk_event_put ()
;;;
;;; void gdk_event_put (const GdkEvent *event);
;;;
;;; Appends a copy of the given event onto the front of the event queue for
;;; event->any.window's display, or the default event queue if event->any.window
;;; is NULL. See gdk_display_put_event().
;;;
;;; event :
;;;     a GdkEvent.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_put" gdk-event-put) :void
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-put)

;;; ----------------------------------------------------------------------------
;;; gdk_event_new ()
;;;
;;; GdkEvent * gdk_event_new (GdkEventType type);
;;;
;;; Creates a new event of the given type. All fields are set to 0.
;;;
;;; type :
;;;     a GdkEventType
;;;
;;; Returns :
;;;     a newly-allocated GdkEvent. The returned GdkEvent should be freed with
;;;     gdk_event_free().
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_copy ()
;;;
;;; GdkEvent * gdk_event_copy (const GdkEvent *event);
;;;
;;; Copies a GdkEvent, copying or incrementing the reference count of the
;;; resources associated with it (e.g. GdkWindow's and strings).
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     a copy of event. The returned GdkEvent should be freed with
;;;     gdk_event_free().
;;; ----------------------------------------------------------------------------

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
;;;
;;; gboolean gdk_event_get_axis (const GdkEvent *event,
;;;                              GdkAxisUse axis_use,
;;;                              gdouble *value);
;;;
;;; Extract the axis value for a particular axis use from an event structure.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; axis_use :
;;;     the axis use to look for
;;;
;;; value :
;;;     location to store the value found
;;;
;;; Returns :
;;;     TRUE if the specified axis was found, otherwise FALSE
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_axis" %gdk-event-get-axis) :boolean
  (event (g-boxed-foreign gdk-event))
  (axis-use gdk-axis-use)
  (value (:pointer :double)))

(defun gdk-event-get-axis (event axis)
  (with-foreign-object (value :double)
    (when (%gdk-event-get-axis event axis value)
      (mem-ref value :double))))

(export 'gdk-event-get-axis)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_button ()
;;;
;;; gboolean gdk_event_get_button (const GdkEvent *event, guint *button);
;;;
;;; Extract the button number from an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; button :
;;;     location to store mouse button number
;;;
;;; Returns :
;;;     TRUE if the event delivered a button number
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_click_count ()
;;;
;;; gboolean gdk_event_get_click_count (const GdkEvent *event,
;;;                                     guint *click_count);
;;;
;;; Extracts the click count from an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; click_count :
;;;     location to store click count
;;;
;;; Returns :
;;;     TRUE if the event delivered a click count
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_coords ()
;;;
;;; gboolean gdk_event_get_coords (const GdkEvent *event,
;;;                                gdouble *x_win,
;;;                                gdouble *y_win);
;;;
;;; Extract the event window relative x/y coordinates from an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; x_win :
;;;     location to put event window x coordinate
;;;
;;; y_win :
;;;     location to put event window y coordinate
;;;
;;; Returns :
;;;     TRUE if the event delivered event window coordinates
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_coords" %gdk-event-get-coords) :boolean
  (event (g-boxed-foreign gdk-event))
  (x-win (:pointer :double))
  (y-win (:pointer :double)))

;; The Lisp implementation returns the values.

(defun gdk-event-get-coords (event)
  (with-foreign-objects ((x :double) (y :double))
    (when (%gdk-event-get-coords event x y)
      (values (mem-ref x :double) (mem-ref y :double)))))

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
;;;
;;; gboolean gdk_event_get_root_coords (const GdkEvent *event,
;;;                                     gdouble *x_root,
;;;                                     gdouble *y_root);
;;;
;;; Extract the root window relative x/y coordinates from an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; x_root :
;;;     location to put root window x coordinate
;;;
;;; y_root :
;;;     location to put root window y coordinate
;;;
;;; Returns :
;;;     TRUE if the event delivered root window coordinates
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_root_coords" %gdk-event-get-root-coords) :boolean
  (event (g-boxed-foreign gdk-event))
  (x-win (:pointer :double))
  (y-win (:pointer :double)))

(defun gdk-event-get-root-coords (event)
  (with-foreign-objects ((x :double) (y :double))
    (when (%gdk-event-get-root-coords event x y)
      (values (mem-ref x :double) (mem-ref y :double)))))

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
;;;
;;; gboolean gdk_event_get_scroll_deltas (const GdkEvent *event,
;;;                                       gdouble *delta_x,
;;;                                       gdouble *delta_y);
;;;
;;; Retrieves the scroll deltas from a GdkEvent
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; delta_x :
;;;     return location for X delta
;;;
;;; delta_y :
;;;     return location for Y delta
;;;
;;; Returns :
;;;     TRUE if the event contains smooth scroll information
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_scroll_deltas" %gdk-event-get-scroll-deltas) :boolean
  (event (g-boxed-foreign gdk-event))
  (delta-x (:pointer :double))
  (delta-y (:pointer :double)))

(defun gdk-event-get-scroll-deltas (event)
  (with-foreign-objects ((x :double) (y :double))
    (when (%gdk-event-get-scroll-deltas event x y)
      (values (mem-ref x :double)
              (mem-ref y :double)))))

(export 'gdk-event-get-scroll-deltas)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_state ()
;;;
;;; gboolean gdk_event_get_state (const GdkEvent *event, GdkModifierType *state)
;;;
;;; If the event contains a "state" field, puts that field in state. Otherwise
;;; stores an empty state (0). Returns TRUE if there was a state field in the
;;; event. event may be NULL, in which case it's treated as if the event had no
;;; state field.
;;;
;;; event :
;;;     a GdkEvent or NULL
;;;
;;; state :
;;;     return location for state
;;;
;;; Returns :
;;;     TRUE if there was a state field in the event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_state" %gdk_event_get_state) :boolean
  (event (g-boxed-foreign gdk-event))
  (state (:pointer gdk-modifier-type)))

(defun gdk-event-get-state (event)
  (with-foreign-object (state 'gdk-modifier-type)
    (when (%gdk_event_get_state event state)
      (mem-ref state 'gdk-modifier-type))))

(export 'gdk-event-get-state)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_time ()
;;;
;;; guint32 gdk_event_get_time (const GdkEvent *event);
;;;
;;; returns GDK_CURRENT_TIME. If event is NULL, returns GDK_CURRENT_TIME.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     time stamp field from event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_get_time" gdk-event-get-time) :uint32
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-event-get-time)

;;; ----------------------------------------------------------------------------
;;; GdkEventSequence
;;;
;;; typedef struct _GdkEventSequence GdkEventSequence;
;;; ----------------------------------------------------------------------------

;;; Implementation moved to gtk.event-structures.lisp

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_event_sequence ()
;;;
;;; GdkEventSequence * gdk_event_get_event_sequence (const GdkEvent *event);
;;;
;;; If event if of type GDK_TOUCH_BEGIN, GDK_TOUCH_UPDATE, GDK_TOUCH_END or
;;; GDK_TOUCH_CANCEL, returns the GdkEventSequence to which the event belongs.
;;; Otherwise, return NULL.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     the event sequence that the event belongs to
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_request_motions ()
;;;
;;; void gdk_event_request_motions (const GdkEventMotion *event);
;;;
;;; Request more motion notifies if event is a motion notify hint event.
;;;
;;; This function should be used instead of gdk_window_get_pointer() to request
;;; further motion notifies, because it also works for extension events where
;;; motion notifies are provided for devices other than the core pointer.
;;; Coordinate extraction, processing and requesting more motion events from a
;;; GDK_MOTION_NOTIFY event usually works like this:
;;;
;;;   {
;;;     /* motion_event handler */
;;;     x = motion_event->x;
;;;     y = motion_event->y;
;;;     /* handle (x,y) motion */
;;;     gdk_event_request_motions (motion_event); /* handles is_hint events */
;;;   }
;;;
;;; event :
;;;     a valid GdkEvent
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_event_request_motions" gdk-event-request-motions) :void
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
;;; gdk_event_handler_set ()
;;;
;;; void gdk_event_handler_set (GdkEventFunc func,
;;;                             gpointer data,
;;;                             GDestroyNotify notify);
;;;
;;; Sets the function to call to handle all events from GDK.
;;;
;;; Note that GTK+ uses this to install its own event handler, so it is usually
;;; not useful for GTK+ applications. (Although an application can call this
;;; function then call gtk_main_do_event() to pass events to GTK+.)
;;;
;;; func :
;;;     the function to call to handle events from GDK.
;;;
;;; data :
;;;     user data to pass to the function.
;;;
;;; notify :
;;;     the function to call when the handler function is removed, i.e. when
;;;     gdk_event_handler_set() is called with another event handler.
;;; ----------------------------------------------------------------------------

(defcallback gdk-event-func-callback :void
    ((event (g-boxed-foreign gdk-event)) (user-data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value user-data) event)
    (return-from-callback () nil)))

(defcfun ("gdk_event_handler_set" %gdk-event-handler-set) :void
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun gdk-event-handler-set (fn)
  (%gdk-event-handler-set (callback gdk-event-func-callback)
                          (glib::allocate-stable-pointer fn)
                          (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gdk-event-handler-set)

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

;;; ----------------------------------------------------------------------------
;;; gdk_get_show_events ()
;;;
;;; gboolean gdk_get_show_events (void);
;;;
;;; Gets whether event debugging output is enabled.
;;;
;;; Returns :
;;;     TRUE if event debugging output is enabled.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_show_events" gdk-get-show-events) :boolean)

(export 'gdk-get-show-events)

;;; ----------------------------------------------------------------------------
;;; gdk_set_show_events ()
;;;
;;; void gdk_set_show_events (gboolean show_events);
;;;
;;; Sets whether a trace of received events is output. Note that GTK+ must be
;;; compiled with debugging (that is, configured using the --enable-debug
;;; option) to use this option.
;;;
;;; show_events :
;;;     TRUE to output event debugging information.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_set_show_events" gdk-set-show-events) :void
  (show-events :boolean))

(export 'gdk-set-show-events)

;;; ----------------------------------------------------------------------------
;;; gdk_event_set_screen ()
;;;
;;; void gdk_event_set_screen (GdkEvent *event, GdkScreen *screen);
;;;
;;; Sets the screen for event to screen. The event must have been allocated by
;;; GTK+, for instance, by gdk_event_copy().
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; screen :
;;;     a GdkScreen
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_screen ()
;;;
;;; GdkScreen * gdk_event_get_screen (const GdkEvent *event);
;;;
;;; Returns the screen for the event. The screen is typically the screen for
;;; event->any.window, but for events such as mouse events, it is the screen
;;; where the pointer was when the event occurs - that is, the screen which has
;;; the root window to which event->motion.x_root and event->motion.y_root are
;;; relative.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     the screen for the event
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device ()
;;;
;;; GdkDevice * gdk_event_get_device (const GdkEvent *event);
;;;
;;; If the event contains a "device" field, this function will return it, else
;;; it will return NULL.
;;;
;;; event :
;;;     a GdkEvent.
;;;
;;; Returns :
;;;     a GdkDevice, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_set_device ()
;;;
;;; void gdk_event_set_device (GdkEvent *event, GdkDevice *device);
;;;
;;; Sets the device for event to device. The event must have been allocated by
;;; GTK+, for instance, by gdk_event_copy().
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_source_device ()
;;;
;;; GdkDevice * gdk_event_get_source_device (const GdkEvent *event);
;;;
;;; This function returns the hardware (slave) GdkDevice that has triggered the
;;; event, falling back to the virtual (master) device (as in
;;; gdk_event_get_device()) if the event wasn't caused by interaction with a
;;; hardware device. This may happen for example in synthesized crossing events
;;; after a GdkWindow updates its geometry or a grab is acquired/released.
;;;
;;; If the event does not contain a device field, this function will return
;;; NULL.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     a GdkDevice, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_set_source_device ()
;;;
;;; void gdk_event_set_source_device (GdkEvent *event, GdkDevice *device);
;;;
;;; Sets the slave device for event to device.
;;;
;;; The event must have been allocated by GTK+, for instance by
;;; gdk_event_copy().
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_setting_get ()
;;;
;;; gboolean gdk_setting_get (const gchar *name, GValue *value);
;;;
;;; Obtains a desktop-wide setting, such as the double-click time, for the
;;; default screen. See gdk_screen_get_setting().
;;;
;;; name :
;;;     the name of the setting.
;;;
;;; value :
;;;     location to store the value of the setting.
;;;
;;; Returns :
;;;     TRUE if the setting existed and a value was stored in value, FALSE
;;;     otherwise.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_setting_get" %gdk-setting-get) :boolean
  (name :string)
  (value (:pointer g-value)))

(defun gdk-get-setting (name)
  (with-foreign-object (value 'g-value)
    (g-value-zero value)
    (when (%gdk-setting-get name value)
      (prog1
        (parse-g-value value)
        (g-value-unset value)))))

(export 'gdk-get-setting)

;;; --- gdk.events.lisp --------------------------------------------------------

