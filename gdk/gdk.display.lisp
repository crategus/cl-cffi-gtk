;;; ----------------------------------------------------------------------------
;;; gdk.display.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 2 Reference Manual
;;; See http://www.gtk.org
;;;
;;; Copyright (C) 2009, 2011 Kalyanov Dmitry
;;; Copyright (C) 2011, 2012 Dr. Dieter Kaiser
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
;;; Controls the keyboard/mouse pointer grabs and a set of GdkScreens
;;;	
;;; Synopsis
;;;
;;;     GdkDisplay;
;;;
;;;     gdk_display_open
;;;     gdk_display_get_default
;;;     gdk_display_get_name
;;;     gdk_display_get_n_screens
;;;     gdk_display_get_screen
;;;     gdk_display_get_default_screen
;;;     gdk_display_pointer_ungrab
;;;     gdk_display_keyboard_ungrab
;;;     gdk_display_pointer_is_grabbed
;;;     gdk_display_beep
;;;     gdk_display_sync
;;;     gdk_display_flush
;;;     gdk_display_close
;;;     gdk_display_is_closed 
;;;     gdk_display_list_devices
;;;     gdk_display_get_event
;;;     gdk_display_peek_event
;;;     gdk_display_put_event
;;;     gdk_display_add_client_message_filter
;;;     gdk_display_set_double_click_time
;;;     gdk_display_set_double_click_distance
;;;     gdk_display_get_pointer
;;;     gdk_display_get_window_at_pointer
;;;     GdkDisplayPointerHooks
;;;     gdk_display_set_pointer_hooks
;;;     gdk_display_warp_pointer
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
;;;
;;; Object Hierarchy
;;;
;;;  GObject
;;;   +----GdkDisplay

;;; ----------------------------------------------------------------------------
;;;
;;; Description
;;;
;;; GdkDisplay objects purpose are two fold:
;;;
;;;    * To grab/ungrab keyboard focus and mouse pointer
;;;    * To manage and provide information about the GdkScreen(s) available
;;;      for this GdkDisplay
;;;
;;; GdkDisplay objects are the GDK representation of the X Display which can be
;;; described as a workstation consisting of a keyboard a pointing device (such
;;; as a mouse) and one or more screens. It is used to open and keep track of
;;; various GdkScreen objects currently instanciated by the application. It is
;;; also used to grab and release the keyboard and the mouse pointer.
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;;
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
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDisplay
;;;
;;; typedef struct _GdkDisplay GdkDisplay;
;;;
;;; The GdkDisplay struct is the GDK representation of an X display. All its
;;; fields are private and should not be accessed directly.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDisplay" gdk-display
  (:type-initializer "gdk_display_get_type")
  ((:cffi name gdk-display-name (g-string :free-from-foreign nil)
          "gdk_display_get_name" nil)
   (:cffi n-screens gdk-display-n-screens :int
          "gdk_display_get_n_screens" nil)
   (:cffi default-screen gdk-display-default-screen (g-object gdk-screen)
          "gdk_display_get_default_screen" nil)
   (:cffi devices gdk-display-devices (g-list g-object :free-from-foreign nil)
          "gdk_display_list_devices" nil)
   (:cffi supports-cursor-color gdk-display-supports-cursor-color :boolean
          "gdk_display_supports_cursor_color" nil)
   (:cffi supports-cursor-alpha gdk-display-supports-color-alpha :boolean
          "gdk_display_supports_cursor_alpha" nil)
   (:cffi default-cursor-size gdk-display-default-cursor-size :uint
          "gdk_display_get_default_cursor_size" nil)
   (:cffi default-group gdk-display-default-group (g-object gdk-window)
          "gdk_display_get_default_group" nil)
   (:cffi supports-selection-notification
          gdk-display-supports-selection-notification :boolean
          "gdk_display_supports_selection_notification" nil)
   (:cffi supports-clipboard-persistence
          gdk-display-supports-clipboard-persistence :boolean
          "gdk_display_supports_clipboard_persistence" nil)
   (:cffi supports-shapes gdk-display-supports-shapes :boolean
          "gdk_display_supports_shapes" nil)
   (:cffi supports-input-shapes gdk-display-supports-input-shapes :boolean
          "gdk_display_supports_input_shapes" nil)
   (:cffi supports-composite gdk-display-supports-composite :boolean
          "gdk_display_supports_composite" nil)
   (:cffi core-pointer gdk-display-core-pointer g-object
     "gdk_display_get_core_pointer" nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_display_open ()
;;;
;;; GdkDisplay * gdk_display_open (const gchar *display_name)
;;;
;;; Opens a display.
;;;
;;; display_name :
;;;	the name of the display to open
;;;
;;; Returns :
;;;	a GdkDisplay, or NULL if the display could not be opened.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_open" gdk-display-open) (g-object gdk-display)
  (display-name :string))

(export 'gdk-display-open)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default ()
;;;
;;; GdkDisplay * gdk_display_get_default (void)
;;;
;;; Gets the default GdkDisplay. This is a convenience function for
;;; gdk_display_manager_get_default_display (gdk_display_manager_get()).
;;;
;;; Returns :
;;;	a GdkDisplay, or NULL if there is no default display. [transfer none]
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default" gdk-default-display) (g-object gdk-display))

(export 'gdk-default-display)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_name ()
;;;
;;; const gchar * gdk_display_get_name (GdkDisplay *display)
;;;
;;; Gets the name of the display.
;;;
;;; display :
;;;	a GdkDisplay
;;;
;;; Returns :
;;;	a string representing the display name. This string is owned by GDK and
;;;     should not be modified or freed.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_n_screens ()
;;;
;;; gint gdk_display_get_n_screens (GdkDisplay *display)
;;;
;;; Gets the number of screen managed by the display.
;;;
;;; display :
;;;	a GdkDisplay
;;;
;;; Returns :
;;;	number of screens.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_screen ()
;;;
;;; GdkScreen * gdk_display_get_screen (GdkDisplay *display, gint screen_num)
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
;;; GdkScreen * gdk_display_get_default_screen (GdkDisplay *display)
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

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_pointer_ungrab ()
;;;
;;; void gdk_display_pointer_ungrab (GdkDisplay *display, guint32 time_)
;;;
;;; Release any pointer grab.
;;;
;;; display :
;;;     a GdkDisplay.
;;;
;;; time_  :
;;;     a timestap (e.g. GDK_CURRENT_TIME).
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_pointer_ungrab" gdk-display-pointer-ungrab) :void
  (display (g-object gdk-display))
  (time :uint32))

(export 'gdk-display-pointer-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_display_keyboard_ungrab ()
;;;
;;; void gdk_display_keyboard_ungrab (GdkDisplay *display, guint32 time_)
;;;
;;; Release any keyboard grab
;;;
;;; display : a GdkDisplay.
;;; time_   : a timestap (e.g GDK_CURRENT_TIME).
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_keyboard_ungrab" gdk-display-keyboard-ungrab) :void
  (display (g-object gdk-display))
  (time :uint32))

(export 'gdk-display-keyboard-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_display_pointer_is_grabbed ()
;;;
;;; gboolean gdk_display_pointer_is_grabbed (GdkDisplay *display)
;;;
;;; Test if the pointer is grabbed.
;;;
;;; display : a GdkDisplay
;;; Returns : TRUE if an active X pointer grab is in effect
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_pointer_is_grabbed" gdk-display-pointer-is-grabbed)
    :boolean
  (display (g-object gdk-display)))

(export 'gdk-display-pointer-is-grabbed)

;;; ----------------------------------------------------------------------------
;;; gdk_display_beep ()
;;;
;;; void gdk_display_beep (GdkDisplay *display)
;;;
;;; Emits a short beep on display
;;;
;;; display : a GdkDisplay
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_beep" gdk-display-beep) :void
  (display (g-object gdk-display)))

(export 'gdk-display-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_display_sync ()
;;;
;;; void gdk_display_sync (GdkDisplay *display)
;;;
;;; Flushes any requests queued for the windowing system and waits until all
;;; requests have been handled. This is often used for making sure that the
;;; display is synchronized with the current state of the program. Calling
;;; gdk_display_sync() before gdk_error_trap_pop() makes sure that any errors
;;; generated from earlier requests are handled before the error trap is
;;; removed.
;;;
;;; This is most useful for X11. On windowing systems where requests are
;;; handled synchronously, this function will do nothing.
;;;
;;; display : a GdkDisplay
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_sync" gdk-display-sync) :void
  (display (g-object gdk-display)))

(export 'gdk-display-sync)

;;; ----------------------------------------------------------------------------
;;; gdk_display_flush ()
;;;
;;; void gdk_display_flush (GdkDisplay *display)
;;;
;;; Flushes any requests queued for the windowing system; this happens
;;; automatically when the main loop blocks waiting for new events, but if your
;;; application is drawing without returning control to the main loop, you may
;;; need to call this function explicitely. A common case where this function
;;; needs to be called is when an application is executing drawing commands
;;; from a thread other than the thread where the main loop is running.
;;;
;;; This is most useful for X11. On windowing systems where requests are
;;; handled synchronously, this function will do nothing.
;;;
;;; display : a GdkDisplay
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_flush" gdk-display-flush) :void
  (display (g-object gdk-display)))

(export 'gdk-display-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_display_close ()
;;;
;;; void gdk_display_close (GdkDisplay *display)
;;;
;;; Closes the connection to the windowing system for the given display, and
;;; cleans up associated resources.
;;;
;;; display : a GdkDisplay
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_close" gdk-display-close) :void
  (display (g-object gdk-display)))

(export 'gdk-display-close)

;;; ----------------------------------------------------------------------------
;;; gdk_display_is_closed ()
;;;
;;; gboolean gdk_display_is_closed (GdkDisplay *display)
;;;
;;; Finds out if the display has been closed.
;;;
;;; display : a GdkDisplay
;;; Returns : TRUE if the display is closed.
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_list_devices ()
;;;
;;; GList * gdk_display_list_devices (GdkDisplay *display)
;;;
;;; Returns the list of available input devices attached to display. The list
;;; is statically allocated and should not be freed.
;;;
;;; display : a GdkDisplay
;;; Returns : a list of GdkDevice
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_event ()
;;;
;;; GdkEvent * gdk_display_get_event (GdkDisplay *display)
;;;
;;; Gets the next GdkEvent to be processed for display, fetching events from
;;; the windowing system if necessary.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     the next GdkEvent to be processed, or NULL if no events are pending.
;;;     The returned GdkEvent should be freed with gdk_event_free().
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
;;; GdkEvent * gdk_display_peek_event (GdkDisplay *display)
;;;
;;; Gets a copy of the first GdkEvent in the display's event queue, without
;;; removing the event from the queue. (Note that this function will not get
;;; more events from the windowing system. It only checks the events that have
;;; already been moved to the GDK event queue.)
;;;
;;; display : a GdkDisplay
;;; Returns : a copy of the first GdkEvent on the event queue, or NULL if no
;;;           events are in the queue. The returned GdkEvent should be freed
;;;           with gdk_event_free().
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
;;; void gdk_display_put_event (GdkDisplay *display, const GdkEvent *event)
;;;
;;; Appends a copy of the given event onto the front of the event queue for
;;; display.
;;;
;;; display : a GdkDisplay
;;; event   : a GdkEvent.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_put_event" gdk-display-put-event) :void
  (display (g-object gdk-display))
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-display-put-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_add_client_message_filter ()
;;;
;;; void gdk_display_add_client_message_filter (GdkDisplay *display,
;;;                                                        GdkAtom message_type,
;;;                                                        GdkFilterFunc func,
;;;                                                        gpointer data)
;;;
;;; Adds a filter to be called when X ClientMessage events are received.
;;; See gdk_window_add_filter() if you are interested in filtering other types
;;; of events.
;;;
;;; display      : a GdkDisplay for which this message filter applies
;;; message_type : the type of ClientMessage events to receive. This will be
;;;                checked against the message_type field of the XClientMessage
;;;                event struct.
;;;
;;; func : the function to call to process the event.
;;; data : user data to pass to func.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_add_client_message_filter"
          %gdk-display-add-client-message-filter) :void
  (display (g-object gdk-display))
  (message-type gdk-atom-as-string)
  (func :pointer)
  (data :pointer))

(defun gdk-display-add-client-message-filter (display message-type fn)
  (%gdk-display-add-client-message-filter display
                                          message-type
                                          (callback gdk-client-message-filter-func)
                                          (allocate-stable-pointer fn)))

(export 'gdk-display-add-client-message-filter)

;;; ----------------------------------------------------------------------------
;;; gdk_display_set_double_click_time ()
;;; 
;;; void gdk_display_set_double_click_time (GdkDisplay *display, guint msec)
;;;
;;; Sets the double click time (two clicks within this time interval count as a
;;; double click and result in a GDK_2BUTTON_PRESS event). Applications should
;;; not set this, it is a global user-configured setting.
;;;
;;; display : a GdkDisplay
;;; msec    : double click time in milliseconds (thousandths of a second)
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_set_double_click_time" gdk-display-set-double-click-time)
    :void
  (display (g-object gdk-display))
  (msec :uint))

(export 'gdk-display-set-double-click-time)

;;; ----------------------------------------------------------------------------
;; gdk_display_set_double_click_distance ()
;;;
;;; void gdk_display_set_double_click_distance (GdkDisplay *display,
;;;                                                        guint distance)
;;;
;;; Sets the double click distance (two clicks within this distance count as a
;;; double click and result in a GDK_2BUTTON_PRESS event). See also
;;; gdk_display_set_double_click_time(). Applications should not set this, it
;;; is a global user-configured setting.
;;;
;;; display  : a GdkDisplay
;;; distance : distance in pixels
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
;;;                                          GdkScreen **screen,
;;;                                          gint *x,
;;;                                          gint *y,
;;;                                          GdkModifierType *mask)
;;;
;;; Gets the current location of the pointer and the current modifier mask for
;;; a given display.
;;;
;;; display : a GdkDisplay
;;; screen  : location to store the screen that the cursor is on, or NULL.
;;;           [out][allow-none]
;;; x       : location to store root window X coordinate of pointer, or NULL.
;;;           [out][allow-none]
;;; y       : location to store root window Y coordinate of pointer, or NULL.
;;;           [out][allow-none]
;;; mask    : location to store current modifier mask, or NULL.
;;;           [out][allow-none]
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_pointer" %gdk-display-get-pointer) :void
  (display (g-object gdk-display))
  (screen :pointer)
  (x :pointer)
  (y :pointer)
  (mask :pointer))

(defun gdk-display-get-pointer (display)
  (with-foreign-objects ((screen :pointer)
                         (x :int)
                         (y :int)
                         (mask 'modifier-type))
    (%gdk-display-get-pointer display screen x y mask)
    (values (mem-ref screen '(g-object gdk-screen))
            (mem-ref x :int)
            (mem-ref y :int)
            (mem-ref mask :int))))

(export 'gdk-display-get-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_window_at_pointer ()
;;;
;;; GdkWindow * gdk_display_get_window_at_pointer (GdkDisplay *display,
;;;                                                           gint *win_x,
;;;                                                           gint *win_y)
;;;
;;; Obtains the window underneath the mouse pointer, returning the location of
;;; the pointer in that window in win_x, win_y for screen. Returns NULL if the
;;; window under the mouse pointer is not known to GDK (for example, belongs to
;;; another application).
;;;
;;; display : a GdkDisplay
;;; win_x   : return location for x coordinate of the pointer location relative
;;;           to the window origin, or NULL. [out][allow-none]
;;; win_y   : return location for y coordinate of the pointer location relative
;;;           & to the window origin, or NULL. [out][allow-none]
;;; Returns : the window under the mouse pointer, or NULL. [transfer none]
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_window_at_pointer"
          gdk-display-get-window-at-pointer) (g-object gdk-window)
  (display (g-object gdk-display))
  (win-x :pointer)
  (win-y :pointer))

(defun display-get-window-at-pointer (display)
  (with-foreign-objects ((win-x :int) (win-y :int))
    (let ((win (gdk-display-get-window-at-pointer display win-x win-y)))
      (values win (mem-ref win-x :int) (mem-ref win-y :int)))))

(export 'gdk-display-get-window-at-pointer)

;;; ----------------------------------------------------------------------------
;;; struct GdkDisplayPointerHooks {
;;;  void (*get_pointer)              (GdkDisplay      *display,
;;;                    GdkScreen      **screen,
;;;                    gint            *x,
;;;                    gint            *y,
;;;                    GdkModifierType *mask);
;;;  GdkWindow* (*window_get_pointer) (GdkDisplay      *display,
;;;                    GdkWindow       *window,
;;;                    gint            *x,
;;;                    gint            *y,
;;;                    GdkModifierType *mask);
;;;  GdkWindow* (*window_at_pointer)  (GdkDisplay      *display,
;;;                    gint            *win_x,
;;;                    gint            *win_y);
;;; };
;;;
;;; A table of pointers to functions for getting quantities related to the
;;; current pointer position. Each GdkDisplay has a table of this type, which
;;; can be set using gdk_display_set_pointer_hooks().
;;;
;;; This is only useful for such low-level tools as an event recorder.
;;; Applications should never have any reason to use this facility
;;; 
;;; get_pointer ()
;;;	Obtains the current pointer position and modifier state. The position
;;;     is given in coordinates relative to the window containing the pointer,
;;;     which is returned in window.
;;;
;;; window_get_pointer ()
;;;	Obtains the window underneath the mouse pointer. Current pointer
;;;     position and modifier state are returned in x, y and mask. The position
;;;     is given in coordinates relative to window.
;;;
;;; window_at_pointer ()
;;;	Obtains the window underneath the mouse pointer, returning the location
;;;     of that window in win_x, win_y. Returns NULL if the window under the
;;;     mouse pointer is not known to GDK (for example, belongs to another
;;;     application).
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_set_pointer_hooks ()
;;;
;;; GdkDisplayPointerHooks * gdk_display_set_pointer_hooks 
;;;                          (GdkDisplay *display,
;;;                           const GdkDisplayPointerHooks *new_hooks)
;;;
;;; Warning
;;;
;;; gdk_display_set_pointer_hooks has been deprecated since version 2.24 and
;;; should not be used in newly-written code. This function will go away in
;;; GTK 3 for lack of use cases.
;;;
;;; This function allows for hooking into the operation of getting the current
;;; location of the pointer on a particular display. This is only useful for
;;; such low-level tools as an event recorder. Applications should never have
;;; any reason to use this facility.
;;; 
;;; display :
;;;     a GdkDisplay
;;;
;;; new_hooks :
;;;     a table of pointers to functions for getting quantities related to the
;;;     current pointer position, or NULL to restore the default table.
;;;
;;; Returns :
;;;     the previous pointer hook table
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_warp_pointer ()
;;;
;;; void gdk_display_warp_pointer (GdkDisplay *display,
;;;                                           GdkScreen *screen,
;;;                                           gint x,
;;;                                           gint y)
;;;
;;; Warps the pointer of display to the point x,y on the screen screen, unless
;;; the pointer is confined to a window by a grab, in which case it will be
;;; moved as far as allowed by the grab. Warping the pointer creates events as
;;; if the user had moved the mouse instantaneously to the destination.
;;;
;;; Note that the pointer should normally be under the control of the user.
;;; This function was added to cover some rare use cases like keyboard
;;; navigation support for the color picker in the GtkColorSelectionDialog.
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

(defcfun ("gdk_display_warp_pointer" gdk-display-warp-pointer) :void
  (display (g-object gdk-display))
  (screen (g-object gdk-screen))
  (x :int)
  (y :int))

(export 'gdk-display-warp-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_cursor_color ()
;;;
;;; gboolean gdk_display_supports_cursor_color (GdkDisplay *display)
;;;
;;; Returns TRUE if multicolored cursors are supported on display. Otherwise,
;;; cursors have only a forground and a background color.
;;;
;;; display : a GdkDisplay
;;; Returns : whether cursors can have multiple colors.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_cursor_alpha ()
;;;
;;; gboolean gdk_display_supports_cursor_alpha (GdkDisplay *display)
;;;
;;; Returns TRUE if cursors can use an 8bit alpha channel on display. Otherwise,
;;; cursors are restricted to bilevel alpha (i.e. a mask).
;;;
;;; display : a GdkDisplay
;;; Returns : whether cursors can have alpha channels.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_cursor_size ()
;;;
;;; guint gdk_display_get_default_cursor_size (GdkDisplay *display)
;;;
;;; Returns the default size to use for cursors on display.
;;;
;;; display : a GdkDisplay
;;; Returns : the default cursor size.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_maximal_cursor_size ()
;;;
;;; void gdk_display_get_maximal_cursor_size (GdkDisplay *display,
;;;                                                      guint *width,
;;;                                                      guint *height)
;;;
;;; Gets the maximal size to use for cursors on display.
;;;
;;; display : a GdkDisplay
;;; width   : the return location for the maximal cursor width
;;; height  : the return location for the maximal cursor height
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_maximal_cursor_size"
          %gdk-display-get-maximal-cursor-size) :void
  (display (g-object gdk-display))
  (width :pointer)
  (height :pointer))

(defun gdk-display-get-maximal-cursor-size (display)
  (with-foreign-objects ((width :uint) (height :uint))
    (%gdk-display-get-maximal-cursor-size display width height)
    (values (mem-ref width :uint) (mem-ref height :uint))))

(export 'gdk-display-get-maximal-cursor-size)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_group ()
;;;
;;; GdkWindow * gdk_display_get_default_group (GdkDisplay *display)
;;;
;;; Returns the default group leader window for all toplevel windows on display.
;;; This window is implicitly created by GDK. See gdk_window_set_group().
;;;
;;; display : a GdkDisplay
;;; Returns : The default group leader window for display
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_selection_notification ()
;;;
;;; gboolean gdk_display_supports_selection_notification (GdkDisplay *display)
;;;
;;; Returns whether GdkEventOwnerChange events will be sent when the owner of a
;;; selection changes.
;;;
;;; display : a GdkDisplay
;;; Returns : whether GdkEventOwnerChange events will be sent.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_request_selection_notification ()
;;;
;;; gboolean gdk_display_request_selection_notification
;;;                                     (GdkDisplay *display, GdkAtom selection)
;;;
;;; Request GdkEventOwnerChange events for ownership changes of the selection
;;; named by the given atom.
;;;
;;; display   : a GdkDisplay
;;; selection : the GdkAtom naming the selection for which ownership change
;;;             notification is requested
;;; Returns   : whether GdkEventOwnerChange events will be sent.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_request_selection_notification"
          gdk-display-request-selection-notification ) :boolean
  (display (g-object gdk-display))
  (selection gdk-atom-as-string))

(export 'gdk-display-request-selection-notification)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_clipboard_persistence ()
;;;
;;; gboolean gdk_display_supports_clipboard_persistence (GdkDisplay *display)
;;;
;;; Returns whether the speicifed display supports clipboard persistance; i.e.
;; if it's possible to store the clipboard data after an application has quit.
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

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_store_clipboard ()
;;;
;;; void gdk_display_store_clipboard (GdkDisplay *display,
;;;                                              GdkWindow *clipboard_window,
;;;                                              guint32 time_,
;;;                                              const GdkAtom *targets,
;;;                                              gint n_targets)
;;;
;;; Issues a request to the clipboard manager to store the clipboard data.
;;; On X11, this is a special program that works according to the freedesktop
;;; clipboard specification, available at
;;; http://www.freedesktop.org/Standards/clipboard-manager-spec.
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
;;;     targets should be saved.
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

(defun display-store-clipboard (display clipboard-window time targets)
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
;;; gboolean gdk_display_supports_shapes (GdkDisplay *display)
;;;
;;; Returns TRUE if gdk_window_shape_combine_mask() can be used to create
;;; shaped windows on display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     TRUE if shaped windows are supported
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_input_shapes ()
;;;
;;; gboolean gdk_display_supports_input_shapes (GdkDisplay *display)
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

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_composite ()
;;;
;;; gboolean gdk_display_supports_composite (GdkDisplay *display)
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

;;; *** NOT IMPLEMENTED ***

;;; --- End of file gdk.display.lisp -------------------------------------------
