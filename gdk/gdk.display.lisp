;;; ----------------------------------------------------------------------------
;;; gdk.display.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
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
;;; GdkDisplay
;;;
;;; Controls a set of GdkScreens and their associated input devices
;;;
;;; Synopsis
;;;
;;;     GdkDisplay
;;;
;;;     gdk_display_open
;;;     gdk_display_get_default
;;;     gdk_display_get_name
;;;     gdk_display_get_n_screens
;;;     gdk_display_get_screen
;;;     gdk_display_get_default_screen
;;;     gdk_display_get_device_manager
;;;     gdk_display_pointer_ungrab                    * deprecated *
;;;     gdk_display_keyboard_ungrab                   * deprecated *
;;;     gdk_display_pointer_is_grabbed                * deprecated *
;;;     gdk_display_device_is_grabbed
;;;     gdk_display_beep
;;;     gdk_display_sync
;;;     gdk_display_flush
;;;     gdk_display_close
;;;     gdk_display_is_closed
;;;     gdk_display_get_event
;;;     gdk_display_peek_event
;;;     gdk_display_put_event
;;;     gdk_display_has_pending
;;;     gdk_display_set_double_click_time
;;;     gdk_display_set_double_click_distance
;;;     gdk_display_get_pointer                       * deprecated *
;;;     gdk_display_list_devices                      * deprecated *
;;;     gdk_display_get_window_at_pointer             * deprecated *
;;;     gdk_display_warp_pointer                      * deprecated *
;;;     gdk_display_supports_cursor_color
;;;     gdk_display_supports_cursor_alpha
;;;     gdk_display_get_default_cursor_size
;;;     gdk_display_get_maximal_cursor_size
;;;     gdk_display_get_default_group
;;;     gdk_display_supports_selection_notification
;;;     gdk_display_request_selection_notification
;;;     gdk_display_supports_clipboard_persistence
;;;     gdk_display_store_clipboard
;;;     gdk_display_supports_shapes
;;;     gdk_display_supports_input_shapes
;;;     gdk_display_supports_composite
;;;     gdk_display_get_app_launch_context
;;;     gdk_display_notify_startup_complete
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GdkDisplay
;;;
;;; Signals
;;;
;;;   "closed"                                         : Run Last
;;;   "opened"                                         : Run Last
;;;
;;; Description
;;;
;;; GdkDisplay objects purpose are two fold:
;;;
;;;   * To manage and provide information about input devices (pointers and
;;;     keyboards)
;;;   * To manage and provide information about the available GdkScreens
;;;
;;; GdkDisplay objects are the GDK representation of an X Display, which can be
;;; described as a workstation consisting of a keyboard, a pointing device (such
;;; as a mouse) and one or more screens. It is used to open and keep track of
;;; various GdkScreen objects currently instantiated by the application. It is
;;; also used to access the keyboard(s) and mouse pointer(s) of the display.
;;;
;;; Most of the input device handling has been factored out into the separate
;;; GdkDeviceManager object. Every display has a device manager, which you can
;;; obtain using gdk_display_get_device_manager().
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "closed" signal
;;;
;;; void user_function (GdkDisplay *display,
;;;                     gboolean    is_error,
;;;                     gpointer    user_data)      : Run Last
;;;
;;; The ::closed signal is emitted when the connection to the windowing system
;;; for display is closed.
;;;
;;; display :
;;;     the object on which the signal is emitted
;;;
;;; is_error :
;;;     TRUE if the display was closed due to an error
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "opened" signal
;;;
;;; void user_function (GdkDisplay *display,
;;;                     gpointer    user_data)      : Run Last
;;;
;;; The ::opened signal is emitted when the connection to the windowing system
;;; for display is opened.
;;;
;;; display :
;;;     the object on which the signal is emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDisplay
;;;
;;; typedef struct _GdkDisplay GdkDisplay;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDisplay" gdk-display
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_display_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gdk_display_open ()
;;;
;;; GdkDisplay * gdk_display_open (const gchar *display_name);
;;;
;;; Opens a display.
;;;
;;; display_name :
;;;     the name of the display to open
;;;
;;; Returns :
;;;     a GdkDisplay, or NULL if the display could not be opened
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_open" gdk-display-open) (g-object gdk-display)
  (display-name :string))

(export 'gdk-display-open)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default ()
;;;
;;; GdkDisplay * gdk_display_get_default (void);
;;;
;;; Gets the default GdkDisplay. This is a convenience function for
;;; gdk_display_manager_get_default_display (gdk_display_manager_get()).
;;;
;;; Returns :
;;;     a GdkDisplay, or NULL if there is no default display
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default" gdk-display-get-default)
    (g-object gdk-display))

(export 'gdk-display-get-default)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_name ()
;;;
;;; const gchar * gdk_display_get_name (GdkDisplay *display);
;;;
;;; Gets the name of the display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     a string representing the display name. This string is owned by GDK and
;;;     should not be modified or freed.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_name" gdk-display-get-name) :string
  (display (g-object gdk-display)))

(export 'gdk-display-get-name)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_n_screens ()
;;;
;;; gint gdk_display_get_n_screens (GdkDisplay *display);
;;;
;;; Gets the number of screen managed by the display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     number of screens.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_n_screens" gdk-display-get-n-screens) :int
  (display (g-object gdk-display)))

(export 'gdk-display-get-n-screens)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_screen ()
;;;
;;; GdkScreen * gdk_display_get_screen (GdkDisplay *display, gint screen_num);
;;;
;;; Returns a screen object for one of the screens of the display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; screen_num :
;;;     the screen number
;;;
;;; Returns :
;;;     the GdkScreen object
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_screen" gdk-display-get-screen) (g-object gdk-screen)
  (display (g-object gdk-display))
  (screen-num :int))

(export 'gdk-display-get-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_screen ()
;;;
;;; GdkScreen * gdk_display_get_default_screen (GdkDisplay *display);
;;;
;;; Get the default GdkScreen for display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     the default GdkScreen object for display
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default_screen" gdk-display-get-default-screen)
    (g-object gdk-screen)
  (display (g-object gdk-display)))

(export 'gdk-display-get-default-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_device_manager ()
;;;
;;; GdkDeviceManager * gdk_display_get_device_manager (GdkDisplay *display);
;;;
;;; Returns the GdkDeviceManager associated to display.
;;;
;;; display :
;;;     a GdkDisplay.
;;;
;;; Returns :
;;;     A GdkDeviceManager, or NULL. This memory is owned by GDK and must not be
;;;     freed or unreferenced.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_device_manager" gdk-display-get-device-manager)
    (g-object gdk-device-manager)
  (display (g-object gdk-display)))

(export 'gdk-display-get-device-manager)

;;; ----------------------------------------------------------------------------
;;; gdk_display_pointer_ungrab ()
;;;
;;; void gdk_display_pointer_ungrab (GdkDisplay *display, guint32 time_);
;;;
;;; Warning
;;;
;;; gdk_display_pointer_ungrab has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gdk_device_ungrab(), together with
;;; gdk_device_grab() instead.
;;;
;;; Release any pointer grab.
;;;
;;; display :
;;;     a GdkDisplay.
;;;
;;; time_ :
;;;     a timestap (e.g. GDK_CURRENT_TIME).
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_keyboard_ungrab ()
;;;
;;; void gdk_display_keyboard_ungrab (GdkDisplay *display, guint32 time_);
;;;
;;; Warning
;;;
;;; gdk_display_keyboard_ungrab has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gdk_device_ungrab(), together with
;;; gdk_device_grab() instead.
;;;
;;; Release any keyboard grab
;;;
;;; display :
;;;     a GdkDisplay.
;;;
;;; time_ :
;;;     a timestap (e.g GDK_CURRENT_TIME).
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_pointer_is_grabbed ()
;;;
;;; gboolean gdk_display_pointer_is_grabbed (GdkDisplay *display);
;;;
;;; Warning
;;;
;;; gdk_display_pointer_is_grabbed has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gdk_display_device_is_grabbed() instead.
;;;
;;; Test if the pointer is grabbed.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     TRUE if an active X pointer grab is in effect
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_device_is_grabbed ()
;;;
;;; gboolean gdk_display_device_is_grabbed (GdkDisplay *display,
;;;                                         GdkDevice *device);
;;;
;;; Returns TRUE if there is an ongoing grab on device for display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Returns :
;;;     TRUE if there is a grab in effect for device.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_device_is_grabbed" gdk-display-device-is-grabbed)
    :boolean
  (display (g-object gdk-display))
  (device (g-object gdk-device)))

(export 'gdk-display-device-is-grabbed)

;;; ----------------------------------------------------------------------------
;;; gdk_display_beep ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_beep" gdk-display-beep) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[display]{A @class{gdk-display} object.}
  @short{Emits a short beep on @arg{display}.}

  Since 2.2"
  (display (g-object gdk-display)))

(export 'gdk-display-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_display_sync ()
;;;
;;; void gdk_display_sync (GdkDisplay *display);
;;;
;;; Flushes any requests queued for the windowing system and waits until all
;;; requests have been handled. This is often used for making sure that the
;;; display is synchronized with the current state of the program. Calling
;;; gdk_display_sync() before gdk_error_trap_pop() makes sure that any errors
;;; generated from earlier requests are handled before the error trap is
;;; removed.
;;;
;;; This is most useful for X11. On windowing systems where requests are handled
;;; synchronously, this function will do nothing.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_sync" gdk-display-sync) :void
  (display (g-object gdk-display)))

(export 'gdk-display-sync)

;;; ----------------------------------------------------------------------------
;;; gdk_display_flush ()
;;;
;;; void gdk_display_flush (GdkDisplay *display);
;;;
;;; Flushes any requests queued for the windowing system; this happens
;;; automatically when the main loop blocks waiting for new events, but if your
;;; application is drawing without returning control to the main loop, you may
;;; need to call this function explicitely. A common case where this function
;;; needs to be called is when an application is executing drawing commands from
;;; a thread other than the thread where the main loop is running.
;;;
;;; This is most useful for X11. On windowing systems where requests are handled
;;; synchronously, this function will do nothing.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_flush" gdk-display-flush) :void
  (display (g-object gdk-display)))

(export 'gdk-display-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_display_close ()
;;;
;;; void gdk_display_close (GdkDisplay *display);
;;;
;;; Closes the connection to the windowing system for the given display, and
;;; cleans up associated resources.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_close" gdk-display-close) :void
  (display (g-object gdk-display)))

(export 'gdk-display-close)

;;; ----------------------------------------------------------------------------
;;; gdk_display_is_closed ()
;;;
;;; gboolean gdk_display_is_closed (GdkDisplay *display);
;;;
;;; Finds out if the display has been closed.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     TRUE if the display is closed.
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_is_closed" gdk-display-is-closed) :boolean
  (display (g-object gdk-display)))

(export 'gdk-display-is-closed)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_event ()
;;;
;;; GdkEvent * gdk_display_get_event (GdkDisplay *display);
;;;
;;; Gets the next GdkEvent to be processed for display, fetching events from the
;;; windowing system if necessary.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     the next GdkEvent to be processed, or NULL if no events are pending. The
;;;     returned GdkEvent should be freed with gdk_event_free().
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_event" gdk-display-get-event)
    (g-boxed-foreign gdk-event :return)
  (display (g-object gdk-display)))

(export 'gdk-display-get-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_peek_event ()
;;;
;;; GdkEvent * gdk_display_peek_event (GdkDisplay *display);
;;;
;;; Gets a copy of the first GdkEvent in the display's event queue, without
;;; removing the event from the queue. (Note that this function will not get
;;; more events from the windowing system. It only checks the events that have
;;; already been moved to the GDK event queue.)
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     a copy of the first GdkEvent on the event queue, or NULL if no events
;;;     are in the queue. The returned GdkEvent should be freed with
;;;     gdk_event_free().
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_peek_event" gdk-display-peek-event)
         (g-boxed-foreign gdk-event :return)
  (display (g-object gdk-display)))

(export 'gdk-display-peek-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_put_event ()
;;;
;;; void gdk_display_put_event (GdkDisplay *display, const GdkEvent *event);
;;;
;;; Appends a copy of the given event onto the front of the event queue for
;;; display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; event :
;;;     a GdkEvent.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_put_event" gdk-display-put-event) :void
  (display (g-object gdk-display))
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-display-put-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_has_pending ()
;;;
;;; gboolean gdk_display_has_pending (GdkDisplay *display);
;;;
;;; Returns whether the display has events that are waiting to be processed.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     TRUE if there are events ready to be processed.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_has_pending" gdk-display-has-pending) :boolean
  (display (g-object gdk-display)))

(export 'gdk-display-has-pending)

;;; ----------------------------------------------------------------------------
;;; gdk_display_set_double_click_time ()
;;;
;;; void gdk_display_set_double_click_time (GdkDisplay *display, guint msec);
;;;
;;; Sets the double click time (two clicks within this time interval count as a
;;; double click and result in a GDK_2BUTTON_PRESS event). Applications should
;;; not set this, it is a global user-configured setting.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; msec :
;;;     double click time in milliseconds (thousandths of a second)
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_set_double_click_time" gdk-display-set-double-click-time)
    :void
  (display (g-object gdk-display))
  (msec :uint))

(export 'gdk-display-set-double-click-time)

;;; ----------------------------------------------------------------------------
;;; gdk_display_set_double_click_distance ()
;;;
;;; void gdk_display_set_double_click_distance (GdkDisplay *display,
;;;                                             guint distance);
;;;
;;; Sets the double click distance (two clicks within this distance count as a
;;; double click and result in a GDK_2BUTTON_PRESS event). See also
;;; gdk_display_set_double_click_time(). Applications should not set this, it is
;;; a global user-configured setting.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; distance :
;;;     distance in pixels
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_set_double_click_distance"
          gdk-display-set-double-click-distance) :void
  (display (g-object gdk-display))
  (distance :uint))

(export 'gdk-display-set-double-click-distance)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_pointer ()
;;;
;;; void gdk_display_get_pointer (GdkDisplay *display,
;;;                               GdkScreen **screen,
;;;                               gint *x,
;;;                               gint *y,
;;;                               GdkModifierType *mask);
;;;
;;; Warning
;;;
;;; gdk_display_get_pointer has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gdk_device_get_position() instead.
;;;
;;; Gets the current location of the pointer and the current modifier mask for a
;;; given display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; screen :
;;;     location to store the screen that the cursor is on, or NULL
;;;
;;; x :
;;;     location to store root window X coordinate of pointer, or NULL
;;;
;;; y :
;;;     location to store root window Y coordinate of pointer, or NULL
;;;
;;; mask :
;;;     location to store current modifier mask, or NULL
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_list_devices ()
;;;
;;; GList * gdk_display_list_devices (GdkDisplay *display);
;;;
;;; Warning
;;;
;;; gdk_display_list_devices has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gdk_device_manager_list_devices()
;;; instead.
;;;
;;; Returns the list of available input devices attached to display. The list is
;;; statically allocated and should not be freed.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     a list of GdkDevice
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_window_at_pointer ()
;;;
;;; GdkWindow * gdk_display_get_window_at_pointer (GdkDisplay *display,
;;;                                                gint *win_x,
;;;                                                gint *win_y);
;;;
;;; Warning
;;;
;;; gdk_display_get_window_at_pointer has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gdk_device_get_window_at_position() instead.
;;;
;;; Obtains the window underneath the mouse pointer, returning the location of
;;; the pointer in that window in win_x, win_y for screen. Returns NULL if the
;;; window under the mouse pointer is not known to GDK (for example, belongs to
;;; another application).
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; win_x :
;;;     return location for x coordinate of the pointer location relative to the
;;;     window origin, or NULL
;;;
;;; win_y :
;;;     return location for y coordinate of the pointer location relative & to
;;;     the window origin, or NULL
;;;
;;; Returns :
;;;     the window under the mouse pointer, or NULL
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_warp_pointer ()
;;;
;;; void gdk_display_warp_pointer (GdkDisplay *display,
;;;                                GdkScreen *screen,
;;;                                gint x,
;;;                                gint y);
;;;
;;; Warning
;;;
;;; gdk_display_warp_pointer has been deprecated since version 3.0 and should
;;; not be used in newly-written code. Use gdk_device_warp() instead.
;;;
;;; Warps the pointer of display to the point x,y on the screen screen, unless
;;; the pointer is confined to a window by a grab, in which case it will be
;;; moved as far as allowed by the grab. Warping the pointer creates events as
;;; if the user had moved the mouse instantaneously to the destination.
;;;
;;; Note that the pointer should normally be under the control of the user. This
;;; function was added to cover some rare use cases like keyboard navigation
;;; support for the color picker in the GtkColorSelectionDialog.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; screen :
;;;     the screen of display to warp the pointer to
;;;
;;; x :
;;;     the x coordinate of the destination
;;;
;;; y :
;;;     the y coordinate of the destination
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_cursor_color ()
;;;
;;; gboolean gdk_display_supports_cursor_color (GdkDisplay *display);
;;;
;;; Returns TRUE if multicolored cursors are supported on display. Otherwise,
;;; cursors have only a forground and a background color.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     whether cursors can have multiple colors.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_cursor_color" gdk-display-supports-cursor-color)
    :boolean
  (display (g-object gdk-display)))

(export 'gdk-display-supports-cursor-color)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_cursor_alpha ()
;;;
;;; gboolean gdk_display_supports_cursor_alpha (GdkDisplay *display);
;;;
;;; Returns TRUE if cursors can use an 8bit alpha channel on display. Otherwise,
;;; cursors are restricted to bilevel alpha (i.e. a mask).
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     whether cursors can have alpha channels.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_cursor_alpha" gdk-display-supports-cursor-alpha)
    :boolean
  (display (g-object gdk-display)))

(export 'gdk-display-supports-cursor-alpha)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_cursor_size ()
;;;
;;; guint gdk_display_get_default_cursor_size (GdkDisplay *display);
;;;
;;; Returns the default size to use for cursors on display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     the default cursor size.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default_cursor_size"
           gdk-display-get-default-cursor-size) :uint
  (display (g-object gdk-display)))

(export 'gdk-display-get-default-cursor-size)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_maximal_cursor_size ()
;;;
;;; void gdk_display_get_maximal_cursor_size (GdkDisplay *display,
;;;                                           guint *width,
;;;                                           guint *height);
;;;
;;; Gets the maximal size to use for cursors on display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; width :
;;;     the return location for the maximal cursor width
;;;
;;; height :
;;;     the return location for the maximal cursor height
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_maximal_cursor_size"
          %gdk-display-get-maximal-cursor-size) :void
  (display (g-object gdk-display))
  (width :pointer)
  (height :pointer))

(defun gdk-display-get-maximal-cursor-size (display)
  (with-foreign-objects ((width :uint)
                         (height :uint))
    (%gdk-display-get-maximal-cursor-size display width height)
    (values (mem-ref width :uint)
            (mem-ref height :uint))))

(export 'gdk-display-get-maximal-cursor-size)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_group ()
;;;
;;; GdkWindow * gdk_display_get_default_group (GdkDisplay *display);
;;;
;;; Returns the default group leader window for all toplevel windows on display.
;;; This window is implicitly created by GDK. See gdk_window_set_group().
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     The default group leader window for display.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default_group" gdk-display-get-default-group)
    (g-object gdk-window)
  (display (g-object gdk-display)))

(export 'gdk-display-get-default-group)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_selection_notification ()
;;;
;;; gboolean gdk_display_supports_selection_notification (GdkDisplay *display);
;;;
;;; Returns whether GdkEventOwnerChange events will be sent when the owner of a
;;; selection changes.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     whether GdkEventOwnerChange events will be sent.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_selection_notification"
           gdk-display-supports-selection-notification) :boolean
  (display (g-object gdk-display)))

(export 'gdk-display-supports-selection-notification)

;;; ----------------------------------------------------------------------------
;;; gdk_display_request_selection_notification ()
;;;
;;; gboolean gdk_display_request_selection_notification (GdkDisplay *display,
;;;                                                      GdkAtom selection);
;;;
;;; Request GdkEventOwnerChange events for ownership changes of the selection
;;; named by the given atom.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; selection :
;;;     the GdkAtom naming the selection for which ownership change notification
;;;     is requested
;;;
;;; Returns :
;;;     whether GdkEventOwnerChange events will be sent.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_request_selection_notification"
           gdk-display-request-selection-notification) :boolean
  (display (g-object gdk-display))
  (selection gdk-atom-as-string))

(export 'gdk-display-request-selection-notification)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_clipboard_persistence ()
;;;
;;; gboolean gdk_display_supports_clipboard_persistence (GdkDisplay *display);
;;;
;;; Returns whether the speicifed display supports clipboard persistance; i.e.
;;; if it's possible to store the clipboard data after an application has quit.
;;; On X11 this checks if a clipboard daemon is running.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     TRUE if the display supports clipboard persistance.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_clipboard_persistence"
           gdk-display-supports-clipboard-persistence) :boolean
  (display (g-object gdk-display)))

(export 'gdk-display-supports-clipboard-persistence)

;;; ----------------------------------------------------------------------------
;;; gdk_display_store_clipboard ()
;;;
;;; void gdk_display_store_clipboard (GdkDisplay *display,
;;;                                   GdkWindow *clipboard_window,
;;;                                   guint32 time_,
;;;                                   const GdkAtom *targets,
;;;                                   gint n_targets);
;;;
;;; Issues a request to the clipboard manager to store the clipboard data. On
;;; X11, this is a special program that works according to the freedesktop
;;; clipboard specification, available at
;;; www.freedesktop.org/Standards/clipboard-manager-spec.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; clipboard_window :
;;;     a GdkWindow belonging to the clipboard owner
;;;
;;; time_ :
;;;     a timestamp
;;;
;;; targets :
;;;     an array of targets that should be saved, or NULL if all available
;;;     targets should be saved
;;;
;;; n_targets :
;;;     length of the targets array
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_store_clipboard" %gdk-display-store-clipboard) :void
  (display (g-object gdk-display))
  (clipboard-window (g-object gdk-window))
  (time :uint32)
  (targets :pointer)
  (n-targets :int))

(defun gdk-display-store-clipboard (display clipboard-window time targets)
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ptr 'gdk-atom-as-string n-targets)
      (loop
        for str in targets
        for i from 0
        do (setf (mem-aref targets-ptr 'gdk-atom-as-string i) str))
      (%gdk-display-store-clipboard display
                                    clipboard-window
                                    time
                                    targets-ptr
                                    n-targets))))

(export 'gdk-display-store-clipboard)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_shapes ()
;;;
;;; gboolean gdk_display_supports_shapes (GdkDisplay *display);
;;;
;;; Returns TRUE if gdk_window_shape_combine_mask() can be used to create shaped
;;; windows on display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     TRUE if shaped windows are supported
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_shapes" gdk-display-supports-shapes) :boolean
  (display (g-object gdk-display)))

(export 'gdk-display-supports-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_input_shapes ()
;;;
;;; gboolean gdk_display_supports_input_shapes (GdkDisplay *display);
;;;
;;; Returns TRUE if gdk_window_input_shape_combine_mask() can be used to modify
;;; the input shape of windows on display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     TRUE if windows with modified input shape are supported
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_input_shapes" gdk-display-supports-input-shapes)
    :boolean
  (display (g-object gdk-display)))

(export 'gdk-display-supports-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_composite ()
;;;
;;; gboolean gdk_display_supports_composite (GdkDisplay *display);
;;;
;;; Returns TRUE if gdk_window_set_composited() can be used to redirect drawing
;;; on the window using compositing.
;;;
;;; Currently this only works on X11 with XComposite and XDamage extensions
;;; available.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     TRUE if windows may be composited.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_composite" gdk-display-supports-composite)
    :boolean
  (display (g-object gdk-display)))

(export 'gdk-display-supports-composite)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_app_launch_context ()
;;;
;;; GdkAppLaunchContext * gdk_display_get_app_launch_context
;;;                                                       (GdkDisplay *display);
;;;
;;; Returns a GdkAppLaunchContext suitable for launching applications on the
;;; given display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     a new GdkAppLaunchContext for display. Free with g_object_unref() when
;;;     done
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_app_launch_context"
           gdk-display-get-app-launch-context) (g-object gdk-app-launch-context)
  (display (g-object gdk-display)))

(export 'gdk-display-get-app-launch-context)

;;; ----------------------------------------------------------------------------
;;; gdk_display_notify_startup_complete ()
;;;
;;; void gdk_display_notify_startup_complete (GdkDisplay *display,
;;;                                           const gchar *startup_id);
;;;
;;; Indicates to the GUI environment that the application has finished loading,
;;; using a given identifier.
;;;
;;; GTK+ will call this function automatically for GtkWindow with custom
;;; startup-notification identifier unless
;;; gtk_window_set_auto_startup_notification() is called to disable that
;;; feature.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; startup_id :
;;;     a startup-notification identifier, for which notification process should
;;;     be completed
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_notify_startup_complete"
           gdk-display-notify-startup-complete) :void
  (display (g-object gdk-display))
  (startup-id :string))

(export 'gdk-display-notify-startup-complete)

;;; --- End of file gdk.display.lisp -------------------------------------------
