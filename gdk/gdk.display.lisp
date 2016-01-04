;;; ----------------------------------------------------------------------------
;;; gdk.display.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.16 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2015 Dieter Kaiser
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
;;;     gdk_display_get_n_screens                     * deprecated *
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
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDisplay
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDisplay" gdk-display
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_display_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gdk-display 'type)
 "@version{2013-6-17}
  @begin{short}
    @sym{gdk-display} objects purpose are two fold:
    @begin{itemize}
      @item{To manage and provide information about input devices (pointers and
        keyboards).}
      @item{To manage and provide information about the available
        @class{gdk-screen} objects.}
    @end{itemize}
  @end{short}
  @sym{gdk-display} objects are the GDK representation of an X Display, which
  can be described as a workstation consisting of a keyboard, a pointing device
  (such as a mouse) and one or more screens. It is used to open and keep track
  of various @class{gdk-screen} objects currently instantiated by the
  application. It is also used to access the keyboard(s) and mouse pointer(s)
  of the display.

  Most of the input device handling has been factored out into the separate
  @class{gdk-device-manager} object. Every display has a device manager, which
  you can obtain using the function @fun{gdk-display-get-device-manager}.
  @begin[Signal Details]{dictionary}
    @subheading{The \"closed\" signal}
      @begin{pre}
 lambda (display is-error)   : Run Last
      @end{pre}
      The \"closed\" signal is emitted when the connection to the windowing
      system for @arg{display} is closed.
      @begin[code]{table}
        @entry[display]{The @sym{gdk-display} object on which the signal is
          emitted.}
        @entry[is-error]{@em{True} if the @arg{display} was closed due to an
          error.}
      @end{table}
      Since 2.2

    @subheading{The \"opened\" signal}
      @begin{pre}
 lambda (display)   : Run Last
      @end{pre}
      The \"opened\" signal is emitted when the connection to the windowing
      system for @arg{display} is opened.
      @begin[code]{table}
        @entry[display]{The @sym{gdk-display} object on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-class{gdk-screen}
  @see-class{gdk-device-manager}
  @see-function{gdk-display-get-device-manager}")

;;; ----------------------------------------------------------------------------
;;; gdk_display_open ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_open" gdk-display-open) (g-object gdk-display)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display-name]{the name of the display to open}
  @begin{return}
    A @class{gdk-display} object, or @code{nil} if the display could not be
    opened.
  @end{return}
  @short{Opens a display named by @arg{display-name}.}

  Since 2.2
  @see-class{gdk-display}"
  (display-name :string))

(export 'gdk-display-open)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default" gdk-display-get-default)
    (g-object gdk-display)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @begin{return}
    A @class{gdk-display} object, or @code{nil} if there is no default display.
  @end{return}
  @begin{short}
    Gets the default @class{gdk-display} object. This is a convenience function
    for the call @code{(gdk-display-manager-get-default-display
    (gdk-display-manager-get))}.
  @end{short}

  Since 2.2
  @see-class{gdk-display}
  @see-function{gdk-display-manager-get}
  @see-function{gdk-display-manager-get-default-display}")

(export 'gdk-display-get-default)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_name" gdk-display-get-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @begin{return}
    A string representing the display name. This string is owned by GDK and
    should not be modified or freed.
  @end{return}
  @short{Gets the name of the @arg{display}.}

  Since 2.2
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-get-name)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_n_screens ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_n_screens" gdk-display-get-n-screens) :int
 #+cl-cffi-gtk-documentation
 "@version{2015-12-30}
  @argument[display]{a @class{gdk-display} object}
  @return{Number of screens.}
  @subheading{Warning}
    The function @sym{gdk-display-get-n-screens} has been deprecated since
    version 3.10 and should not be used in newly-written code. The number of
    screens is always 1.

  @short{Gets the number of screens managed by the @arg{display}.}

  Since 2.2
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-get-n-screens)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_screen" gdk-display-get-screen) (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @argument[screen-num]{the screen number}
  @return{The @class{gdk-screen} object.}
  @short{Returns a screen object for one of the screens of the @arg{display}.}

  Since 2.2
  @see-class{gdk-display}"
  (display (g-object gdk-display))
  (screen-num :int))

(export 'gdk-display-get-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_screen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default_screen" gdk-display-get-default-screen)
    (g-object gdk-screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-21}
  @argument[display]{a @class{gdk-display} object}
  @return{The default @class{gdk-screen} object for @arg{display}.}
  @short{Get the default @class{gdk-screen} object for @arg{display}.}

  Since 2.2
  @see-class{gdk-display}
  @see-class{gdk-screen}"
  (display (g-object gdk-display)))

(export 'gdk-display-get-default-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_device_manager ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_device_manager" gdk-display-get-device-manager)
    (g-object gdk-device-manager)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @begin{return}
    A @class{gdk-device-manager} object, or @code{nil}. This memory is owned by
    GDK and must not be freed or unreferenced.
  @end{return}
  @begin{short}
    Returns the @class{gdk-device-manager} object associated to @arg{display}.
  @end{short}

  Since 3.0
  @see-class{gdk-display}
  @see-class{gdk-device-manager}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_device_is_grabbed" gdk-display-device-is-grabbed)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @argument[device]{a @class{gdk-device} object}
  @return{@em{True} if there is a grab in effect for @arg{device}.}
  Returns @em{true} if there is an ongoing grab on @arg{device} for
  @arg{display}.
  @see-class{gdk-display}"
  (display (g-object gdk-display))
  (device (g-object gdk-device)))

(export 'gdk-display-device-is-grabbed)

;;; ----------------------------------------------------------------------------
;;; gdk_display_beep ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_beep" gdk-display-beep) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @short{Emits a short beep on @arg{display}.}

  Since 2.2
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_display_sync ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_sync" gdk-display-sync) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @begin{short}
    Flushes any requests queued for the windowing system and waits until all
    requests have been handled. This is often used for making sure that the
    @arg{display} is synchronized with the current state of the program. Calling
    the function @sym{gdk-display-sync} before the function
    @fun{gdk-error-trap-pop} makes sure that any errors generated from earlier
    requests are handled before the error trap is removed.
  @end{short}

  This is most useful for X11. On windowing systems where requests are handled
  synchronously, this function will do nothing.

  Since 2.2
  @see-class{gdk-display}
  @see-function{gdk-error-trap-pop}"
  (display (g-object gdk-display)))

(export 'gdk-display-sync)

;;; ----------------------------------------------------------------------------
;;; gdk_display_flush ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_flush" gdk-display-flush) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @begin{short}
    Flushes any requests queued for the windowing system; this happens
    automatically when the main loop blocks waiting for new events, but if your
    application is drawing without returning control to the main loop, you may
    need to call this function explicitely. A common case where this function
    needs to be called is when an application is executing drawing commands from
    a thread other than the thread where the main loop is running.
  @end{short}

  This is most useful for X11. On windowing systems where requests are handled
  synchronously, this function will do nothing.

  Since 2.4
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_display_close ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_close" gdk-display-close) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @begin{short}
    Closes the connection to the windowing system for the given @arg{display},
    and cleans up associated resources.
  @end{short}

  Since 2.2
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-close)

;;; ----------------------------------------------------------------------------
;;; gdk_display_is_closed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_is_closed" gdk-display-is-closed) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @return{@em{True} if the @arg{display} is closed.}
  @short{Finds out if the @arg{display} has been closed.}

  Since 2.22
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-is-closed)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_event" gdk-display-get-event)
    (g-boxed-foreign gdk-event :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-21}
  @argument[display]{a @class{gdk-display} object}
  @begin{return}
    The next @class{gdk-event} event to be processed, or @code{nil} if no events
    are pending.
  @end{return}
  @begin{short}
    Gets the next @class{gdk-event} event to be processed for @arg{display},
    fetching events from the windowing system if necessary.
  @end{short}

  Since 2.2
  @see-class{gdk-display}
  @see-class{gdk-event}"
  (display (g-object gdk-display)))

(export 'gdk-display-get-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_peek_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_peek_event" gdk-display-peek-event)
         (g-boxed-foreign gdk-event :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-21}
  @argument[display]{a @class{gdk-display} object}
  @begin{return}
    A copy of the first @class{gdk-event} event on the event queue, or
    @code{nil} if no events are in the queue.
  @end{return}
  @begin{short}
    Gets a copy of the first @class{gdk-event} event in the @arg{display}'s
    event queue, without removing the event from the queue.
  @end{short}
  Note that this function will not get more events from the windowing system. It
  only checks the events that have already been moved to the GDK event queue.

  Since 2.2
  @see-class{gdk-display}
  @see-class{gdk-event}"
  (display (g-object gdk-display)))

(export 'gdk-display-peek-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_put_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_put_event" gdk-display-put-event) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @argument[event]{a @class{gdk-event} event}
  @begin{short}
    Appends a copy of the given @arg{event} onto the front of the event queue
    for @arg{display}.
  @end{short}

  Since 2.2
  @see-class{gdk-display}"
  (display (g-object gdk-display))
  (event (g-boxed-foreign gdk-event)))

(export 'gdk-display-put-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_has_pending ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_has_pending" gdk-display-has-pending) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @return{@em{True} if there are events ready to be processed.}
  @begin{short}
    Returns whether the @arg{display} has events that are waiting to be
    processed.
  @end{short}

  Since 3.0
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-has-pending)

;;; ----------------------------------------------------------------------------
;;; gdk_display_set_double_click_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_set_double_click_time" gdk-display-set-double-click-time)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @argument[msec]{double click time in milliseconds (thousandths of a second)}
  @begin{short}
    Sets the double click time (two clicks within this time interval count as a
    double click and result in a @code{:2button-press} event). Applications
    should not set this, it is a global user configured setting.
  @end{short}

  Since 2.2
  @see-class{gdk-display}"
  (display (g-object gdk-display))
  (msec :uint))

(export 'gdk-display-set-double-click-time)

;;; ----------------------------------------------------------------------------
;;; gdk_display_set_double_click_distance ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_set_double_click_distance"
          gdk-display-set-double-click-distance) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @argument[distance]{distance in pixels}
  @begin{short}
    Sets the double click distance (two clicks within this distance count as a
    double click and result in a @code{:2button-press} event). See also the
    function @fun{gdk-display-set-double-click-time}. Applications should not
    set this, it is a global user-configured setting.
  @end{short}

  Since 2.4
  @see-class{gdk-display}
  @see-function{gdk-display-set-double-click-time}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_cursor_color" gdk-display-supports-cursor-color)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @return{Whether cursors can have multiple colors.}
  @begin{short}
    Returns @em{true} if multicolored cursors are supported on @arg{display}.
    Otherwise, cursors have only a forground and a background color.
  @end{short}

  Since 2.4
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-supports-cursor-color)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_cursor_alpha ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_cursor_alpha" gdk-display-supports-cursor-alpha)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @return{Whether cursors can have alpha channels.}
  @begin{short}
    Returns @em{true} if cursors can use an 8bit alpha channel on @arg{display}.
    Otherwise, cursors are restricted to bilevel alpha (i. e. a mask).
  @end{short}

  Since 2.4
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-supports-cursor-alpha)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_cursor_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default_cursor_size"
           gdk-display-get-default-cursor-size) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @return{The default cursor size.}
  @short{Returns the default size to use for cursors on @arg{display}.}

  Since 2.4
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-get-default-cursor-size)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_maximal_cursor_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_maximal_cursor_size"
          %gdk-display-get-maximal-cursor-size) :void
  (display (g-object gdk-display))
  (width :pointer)
  (height :pointer))

(defun gdk-display-get-maximal-cursor-size (display)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @begin{return}
    @code{width} -- the maximal cursor width @br{}
    @code{height} -- the maximal cursor height
  @end{return}
  @short{Gets the maximal size to use for cursors on @arg{display}.}

  Since 2.4
  @see-class{gdk-display}"
  (with-foreign-objects ((width :uint)
                         (height :uint))
    (%gdk-display-get-maximal-cursor-size display width height)
    (values (mem-ref width :uint)
            (mem-ref height :uint))))

(export 'gdk-display-get-maximal-cursor-size)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default_group" gdk-display-get-default-group)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @return{The default group leader window for @arg{display}.}
  @begin{short}
    Returns the default group leader window for all toplevel windows on
    @arg{display}. This window is implicitly created by GDK. See the function
    @fun{gdk-window-set-group}.
  @end{short}

  Since 2.4
  @see-class{gdk-display}
  @see-function{gdk-window-set-group}"
  (display (g-object gdk-display)))

(export 'gdk-display-get-default-group)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_selection_notification ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_selection_notification"
           gdk-display-supports-selection-notification) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @return{Whether @class{gdk-event-owner-change} events will be sent.}
  @begin{short}
    Returns whether @class{gdk-event-owner-change} events will be sent when the
    owner of a selection changes.
  @end{short}

  Since 2.6
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-supports-selection-notification)

;;; ----------------------------------------------------------------------------
;;; gdk_display_request_selection_notification ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_request_selection_notification"
           gdk-display-request-selection-notification) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @argument[selection]{the @symbol{gdk-atom} naming the selection for which
    ownership change notification is requested}
  @return{Whether @class{gdk-event-owner-change} events will be sent.}
  @begin{short}
    Request @class{gdk-event-owner-change} events for ownership changes of the
    @arg{selection} named by the given atom.
  @end{short}

  Since 2.6
  @see-class{gdk-display}"
  (display (g-object gdk-display))
  (selection gdk-atom-as-string))

(export 'gdk-display-request-selection-notification)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_clipboard_persistence ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_clipboard_persistence"
           gdk-display-supports-clipboard-persistence) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @return{@em{True} if the @arg{display} supports clipboard persistance.}
  @begin{short}
    Returns whether the specified @arg{display} supports clipboard persistance;
    i. e. if it is possible to store the clipboard data after an application has
    quit. On X11 this checks if a clipboard daemon is running.
  @end{short}

  Since 2.6
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-supports-clipboard-persistence)

;;; ----------------------------------------------------------------------------
;;; gdk_display_store_clipboard ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_store_clipboard" %gdk-display-store-clipboard) :void
  (display (g-object gdk-display))
  (clipboard-window (g-object gdk-window))
  (time :uint32)
  (targets :pointer)
  (n-targets :int))

;; TODO: Is the case NIL for TARGETS handled correctly?

(defun gdk-display-store-clipboard (display clipboard-window time targets)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @argument[clipboard-window]{a @class{gdk-window} object belonging to the
    clipboard owner}
  @argument[time]{a timestamp}
  @argument[targets]{a list of targets that should be saved, or @code{nil} if
    all available targets should be saved}
  @begin{short}
    Issues a request to the clipboard manager to store the clipboard data. On
    X11, this is a special program that works according to the freedesktop
    clipboard specification, available at
    @a[http://www.freedesktop.org/wiki/ClipboardManager]{freedesktop.org}.
  @end{short}

  Since 2.6
  @see-class{gdk-display}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_shapes" gdk-display-supports-shapes) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-12}
  @argument[display]{a @class{gdk-display} object}
  @return{@em{True} if shaped windows are supported.}
  @begin{short}
    Returns @em{true} if the function @fun{gdk-window-shape-combine-region} can
    be used to create shaped windows on @arg{display}.
  @end{short}

  Since 2.10
  @see-class{gdk-display}
  @see-function{gdk-window-shape-combine-region}"
  (display (g-object gdk-display)))

(export 'gdk-display-supports-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_input_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_input_shapes" gdk-display-supports-input-shapes)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-12}
  @argument[display]{a @class{gdk-display} object}
  @return{@em{True} if windows with modified input shape are supported.}
  @begin{short}
    Returns @em{true} if the function
    @fun{gdk-window-input-shape-combine-region} can be used to modify the input
    shape of windows on @arg{display}.
  @end{short}

  Since 2.10
  @see-class{gdk-display}
  @see-function{gdk-window-input-shape-combine-region}"
  (display (g-object gdk-display)))

(export 'gdk-display-supports-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_composite ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_supports_composite" gdk-display-supports-composite)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @return{@em{True} if windows may be composited.}
  @begin{short}
    Returns @em{true} if the function @fun{gdk-window-set-composited} can be
    used to redirect drawing on the window using compositing.
  @end{short}

  Currently this only works on X11 with XComposite and XDamage extensions
  available.

  Since 2.12
  @see-class{gdk-display}
  @see-function{gdk-window-set-composited}"
  (display (g-object gdk-display)))

(export 'gdk-display-supports-composite)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_app_launch_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_app_launch_context"
           gdk-display-get-app-launch-context) (g-object gdk-app-launch-context)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @begin{return}
    A new @class{gdk-app-launch-context} object for @arg{display}. Free with
    @fun{g-object-unref} when done.
  @end{return}
  @begin{short}
    Returns a @class{gdk-app-launch-context} object suitable for launching
    applications on the given @arg{display}.
  @end{short}

  Since 3.0
  @see-class{gdk-display}"
  (display (g-object gdk-display)))

(export 'gdk-display-get-app-launch-context)

;;; ----------------------------------------------------------------------------
;;; gdk_display_notify_startup_complete ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_notify_startup_complete"
           gdk-display-notify-startup-complete) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[display]{a @class{gdk-display} object}
  @argument[startup-id]{a startup notification identifier, for which
    notification process should be completed}
  @begin{short}
    Indicates to the GUI environment that the application has finished loading,
    using a given identifier.
  @end{short}

  GTK+ will call this function automatically for @class{gtk-window} windows with
  a custom startup notification identifier unless the function the function
  @fun{gtk-window-set-auto-startup-notification} is called to disable that
  feature.

  Since 3.0
  @see-class{gdk-display}
  @see-function{gtk-window-set-auto-startup-notification}"
  (display (g-object gdk-display))
  (startup-id :string))

(export 'gdk-display-notify-startup-complete)

;;; --- End of file gdk.display.lisp -------------------------------------------
