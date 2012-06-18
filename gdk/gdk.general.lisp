;;; ----------------------------------------------------------------------------
;;; gdk.general.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; General
;;;
;;; Library initialization and miscellaneous functions
;;;
;;; Synopsis
;;;
;;;     gdk_init
;;;     gdk_init_check
;;;     gdk_parse_args
;;;     gdk_get_display_arg_name
;;;     gdk_notify_startup_complete
;;;     gdk_notify_startup_complete_with_id
;;;     
;;;     gdk_get_program_class
;;;     gdk_set_program_class
;;;     
;;;     gdk_get_display
;;;     
;;;     gdk_flush
;;;     
;;;     gdk_screen_width
;;;     gdk_screen_height
;;;     gdk_screen_width_mm
;;;     gdk_screen_height_mm
;;;     
;;;     gdk_pointer_grab
;;;
;;;     GdkGrabStatus
;;;
;;;     gdk_pointer_ungrab
;;;     gdk_pointer_is_grabbed
;;;     gdk_set_double_click_time
;;;     
;;;     gdk_keyboard_grab
;;;     gdk_keyboard_ungrab
;;;     
;;;     gdk_beep
;;;     
;;;     gdk_error_trap_push
;;;     gdk_error_trap_pop
;;;     gdk_error_trap_pop_ignored
;;;     
;;;     GDK_WINDOWING_X11
;;;     GDK_WINDOWING_WIN32
;;;     
;;;     GDK_VERSION_3_0
;;;     GDK_VERSION_3_2
;;;     GDK_VERSION_3_4
;;;     GDK_VERSION_MIN_REQUIRED
;;;     GDK_VERSION_MAX_ALLOWED
;;;
;;; Description
;;;
;;; This section describes the GDK initialization functions and miscellaneous
;;; utility functions, as well as deprecation facilities.
;;;
;;; The GDK and GTK+ headers annotate deprecated APIs in a way that produces
;;; compiler warnings if these deprecated APIs are used. The warnings can be
;;; turned off by defining the macro GDK_DISABLE_DEPRECATION_WARNINGS before
;;; including the glib.h header.
;;;
;;; GDK and GTK+ also provide support for building applications against defined
;;; subsets of deprecated or new APIs. Define the macro GDK_VERSION_MIN_REQUIRED
;;; to specify up to what version you want to receive warnings about deprecated
;;; APIs. Define the macro GDK_VERSION_MAX_ALLOWED to specify the newest version
;;; whose API you want to use.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_init ()
;;;
;;; void gdk_init (gint *argc, gchar ***argv);
;;;
;;; Initializes the GDK library and connects to the windowing system. If
;;; initialization fails, a warning message is output and the application
;;; terminates with a call to exit(1).
;;;
;;; Any arguments used by GDK are removed from the array and argc and argv are
;;; updated accordingly.
;;;
;;; GTK+ initializes GDK in gtk_init() and so this function is not usually
;;; needed by GTK+ applications.
;;;
;;; argc :
;;;     the number of command line arguments
;;;
;;; argv :
;;;     the array of command line arguments
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_init_check ()
;;;
;;; gboolean gdk_init_check (gint *argc, gchar ***argv);
;;;
;;; Initializes the GDK library and connects to the windowing system, returning
;;; TRUE on success.
;;;
;;; Any arguments used by GDK are removed from the array and argc and argv are
;;; updated accordingly.
;;;
;;; GTK+ initializes GDK in gtk_init() and so this function is not usually
;;; needed by GTK+ applications.
;;;
;;; argc :
;;;     the number of command line arguments
;;;
;;; argv :
;;;     the array of command line arguments
;;;
;;; Returns :
;;;     TRUE if initialization succeeded.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_parse_args ()
;;;
;;; void gdk_parse_args (gint *argc, gchar ***argv);
;;;
;;; Parse command line arguments, and store for future use by calls to
;;; gdk_display_open().
;;;
;;; Any arguments used by GDK are removed from the array and argc and argv are
;;; updated accordingly.
;;;
;;; You shouldn't call this function explicitely if you are using gtk_init(),
;;; gtk_init_check(), gdk_init(), or gdk_init_check().
;;;
;;; argc :
;;;     the number of command line arguments.
;;;
;;; argv :
;;;     the array of command line arguments
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_get_display_arg_name ()
;;;
;;; const gchar * gdk_get_display_arg_name (void);
;;;
;;; Gets the display name specified in the command line arguments passed to
;;; gdk_init() or gdk_parse_args(), if any.
;;;
;;; Returns :
;;;     the display name, if specified explicitely, otherwise NULL this string
;;;     is owned by GTK+ and must not be modified or freed.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_notify_startup_complete ()
;;;
;;; void gdk_notify_startup_complete (void);
;;;
;;; Indicates to the GUI environment that the application has finished loading.
;;; If the applications opens windows, this function is normally called after
;;; opening the application's initial set of windows.
;;;
;;; GTK+ will call this function automatically after opening the first GtkWindow
;;; unless gtk_window_set_auto_startup_notification() is called to disable that
;;; feature.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_notify_startup_complete" %gdk-notify-startup-complete) :void)

;;; ----------------------------------------------------------------------------
;;; gdk_notify_startup_complete_with_id ()
;;;
;;; void gdk_notify_startup_complete_with_id (const gchar *startup_id);
;;;
;;; Indicates to the GUI environment that the application has finished loading,
;;; using a given identifier.
;;;
;;; GTK+ will call this function automatically for GtkWindow with custom
;;; startup-notification identifier unless
;;; gtk_window_set_auto_startup_notification() is called to disable that
;;; feature.
;;;
;;; startup_id :
;;;     a startup-notification identifier, for which notification process should
;;;     be completed
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_notify_startup_complete_with_id"
          %gdk-notify-startup-complete-with-id) :void
  (startup-id :string))

(defun gdk-notify-startup-complete (&optional startup-id)
  (if startup-id
      (%gdk-notify-startup-complete-with-id startup-id)
      (%gdk-notify-startup-complete)))

(export 'gdk-notify-startup-complete)

;;; ----------------------------------------------------------------------------
;;; gdk_get_program_class ()
;;;
;;; const gchar * gdk_get_program_class (void);
;;;
;;; Gets the program class. Unless the program class has explicitly been set
;;; with gdk_set_program_class() or with the --class commandline option, the
;;; default value is the program name (determined with g_get_prgname()) with the
;;; first character converted to uppercase.
;;;
;;; Returns :
;;;     the program class.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_program_class" gdk-get-program-class)
    (:string :free-from-foreign nil))

(export 'gdk-get-programm-class)

;;; ----------------------------------------------------------------------------
;;; gdk_set_program_class ()
;;;
;;; void gdk_set_program_class (const gchar *program_class);
;;;
;;; Sets the program class. The X11 backend uses the program class to set the
;;; class name part of the WM_CLASS property on toplevel windows; see the ICCCM.
;;;
;;; program_class :
;;;     a string.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_set_program_class" gdk-set-program-class) :void
  (program-class (:string :free-to-foreign t)))

(export 'gdk-set-program-class)

;;; ----------------------------------------------------------------------------
;;; gdk_get_display ()
;;;
;;; gchar * gdk_get_display (void);
;;;
;;; Gets the name of the display, which usually comes from the DISPLAY
;;; environment variable or the --display command line option.
;;;
;;; Returns :
;;;     the name of the display.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_display" gdk-get-display) (:string :free-from-foreign nil))

(export 'gdk-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_flush ()
;;;
;;; void gdk_flush (void);
;;;
;;; Flushes the output buffers of all display connections and waits until all
;;; requests have been processed. This is rarely needed by applications.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_flush" gdk-flush) :void)

(export 'gdk-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_width ()
;;;
;;; gint gdk_screen_width (void);
;;;
;;; Returns the width of the default screen in pixels.
;;;
;;; Returns :
;;;     the width of the default screen in pixels.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_width" %gdk-screen-width) :int)

(export '%gdk-screen-width)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_height ()
;;;
;;; gint gdk_screen_height (void);
;;;
;;; Returns the height of the default screen in pixels.
;;;
;;; Returns :
;;;     the height of the default screen in pixels.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_height" %gdk-screen-height) :int)

(export '%gdk-screen-height)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_width_mm ()
;;;
;;; gint gdk_screen_width_mm (void);
;;;
;;; Returns the width of the default screen in millimeters. Note that on many X
;;; servers this value will not be correct.
;;;
;;; Returns :
;;;     the width of the default screen in millimeters, though it is not always
;;;     correct.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_width_mm" %gdk-screen-width-mm) :int)

(export '%gdk-screen-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_height_mm ()
;;;
;;; gint gdk_screen_height_mm (void);
;;;
;;; Returns the height of the default screen in millimeters. Note that on many X
;;; servers this value will not be correct.
;;;
;;; Returns :
;;;     the height of the default screen in millimeters, though it is not always
;;;     correct.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_height_mm" %gdk-screen-height-mm) :int)

(export '%gdk-screen-heigth-mm)

;;; ----------------------------------------------------------------------------
;;; enum GdkGrabStatus
;;;
;;; typedef enum {
;;;   GDK_GRAB_SUCCESS         = 0,
;;;   GDK_GRAB_ALREADY_GRABBED = 1,
;;;   GDK_GRAB_INVALID_TIME    = 2,
;;;   GDK_GRAB_NOT_VIEWABLE    = 3,
;;;   GDK_GRAB_FROZEN          = 4
;;; } GdkGrabStatus;
;;;
;;; Returned by gdk_pointer_grab() and gdk_keyboard_grab() to indicate success
;;; or the reason for the failure of the grab attempt.
;;;
;;; GDK_GRAB_SUCCESS
;;;     the resource was successfully grabbed.
;;;
;;; GDK_GRAB_ALREADY_GRABBED
;;;     the resource is actively grabbed by another client.
;;;
;;; GDK_GRAB_INVALID_TIME
;;;     the resource was grabbed more recently than the specified time.
;;;
;;; GDK_GRAB_NOT_VIEWABLE
;;;     the grab window or the confine_to window are not viewable.
;;;
;;; GDK_GRAB_FROZEN
;;;     the resource is frozen by an active grab of another client.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkGrabStatus" gdk-grab-status
  ()
  :success
  :already-grabbed
  :invalid-time
  :not-viewable
  :frozen)

;;; ----------------------------------------------------------------------------
;;; gdk_pointer_grab ()
;;;
;;; GdkGrabStatus gdk_pointer_grab (GdkWindow *window,
;;;                                 gboolean owner_events,
;;;                                 GdkEventMask event_mask,
;;;                                 GdkWindow *confine_to,
;;;                                 GdkCursor *cursor,
;;;                                 guint32 time_);
;;;
;;; Warning
;;;
;;; gdk_pointer_grab has been deprecated since version 3.0 and should not be
;;; used in newly-written code. Use gdk_device_grab() instead.
;;;
;;; Grabs the pointer (usually a mouse) so that all events are passed to this
;;; application until the pointer is ungrabbed with gdk_pointer_ungrab(), or the
;;; grab window becomes unviewable. This overrides any previous pointer grab by
;;; this client.
;;;
;;; Pointer grabs are used for operations which need complete control over mouse
;;; events, even if the mouse leaves the application. For example in GTK+ it is
;;; used for Drag and Drop, for dragging the handle in the GtkHPaned and
;;; GtkVPaned widgets.
;;;
;;; Note that if the event mask of an X window has selected both button press
;;; and button release events, then a button press event will cause an automatic
;;; pointer grab until the button is released. X does this automatically since
;;; most applications expect to receive button press and release events in
;;; pairs. It is equivalent to a pointer grab on the window with owner_events
;;; set to TRUE.
;;;
;;; If you set up anything at the time you take the grab that needs to be
;;; cleaned up when the grab ends, you should handle the GdkEventGrabBroken
;;; events that are emitted when the grab ends unvoluntarily.
;;;
;;; window :
;;;     the GdkWindow which will own the grab (the grab window).
;;;
;;; owner_events :
;;;     if FALSE then all pointer events are reported with respect to window and
;;;     are only reported if selected by event_mask. If TRUE then pointer events
;;;     for this application are reported as normal, but pointer events outside
;;;     this application are reported with respect to window and only if
;;;     selected by event_mask. In either mode, unreported events are discarded.
;;;
;;; event_mask :
;;;     specifies the event mask, which is used in accordance with owner_events.
;;;     Note that only pointer events (i.e. button and motion events) may be
;;;     selected.
;;;
;;; confine_to :
;;;     If non-NULL, the pointer will be confined to this window during the
;;;     grab. If the pointer is outside confine_to, it will automatically be
;;;     moved to the closest edge of confine_to and enter and leave events will
;;;     be generated as necessary.
;;;
;;; cursor :
;;;     the cursor to display while the grab is active. If this is NULL then the
;;;     normal cursors are used for window and its descendants, and the cursor
;;;     for window is used for all other windows.
;;;
;;; time_ :
;;;     the timestamp of the event which led to this pointer grab. This usually
;;;     comes from a GdkEventButton struct, though GDK_CURRENT_TIME can be used
;;;     if the time isn't known.
;;;
;;; Returns :
;;;     GDK_GRAB_SUCCESS if the grab was successful.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pointer_grab" gdk-pointer-grab) gdk-grab-status
  (window (g-object gdk-window))
  (owner-events :boolean)
  (event-mask gdk-event-mask)
  (confine-to (g-object gdk-window))
  (cursor (g-boxed-foreign gdk-cursor))
  (time :uint32))

(export 'gdk-pointer-grab)

;;; ----------------------------------------------------------------------------
;;; gdk_pointer_ungrab ()
;;;
;;; void gdk_pointer_ungrab (guint32 time_);
;;;
;;; Warning
;;;
;;; gdk_pointer_ungrab has been deprecated since version 3.0 and should not be
;;; used in newly-written code. Use gdk_device_ungrab(), together with
;;; gdk_device_grab() instead.
;;;
;;; Ungrabs the pointer on the default display, if it is grabbed by this
;;; application.
;;;
;;; time_ :
;;;     a timestamp from a GdkEvent, or GDK_CURRENT_TIME if no timestamp is
;;;     available.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pointer_ungrab" gdk-pointer-ungrab) :void
  (time :uint32))

(export 'gdk-pointer-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_pointer_is_grabbed ()
;;;
;;; gboolean gdk_pointer_is_grabbed (void);
;;;
;;; Warning
;;;
;;; gdk_pointer_is_grabbed has been deprecated since version 3.0 and should not
;;; be used in newly-written code. Use gdk_display_device_is_grabbed() instead.
;;;
;;; Returns TRUE if the pointer on the default display is currently grabbed by
;;; this application.
;;;
;;; Note that this does not take the inmplicit pointer grab on button presses
;;; into account.
;;;
;;; Returns :
;;;     TRUE if the pointer is currently grabbed by this application.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pointer_is_grabbed" gdk-pointer-is-grabbed) :boolean)

(export 'gdk-pointer-is-grabbed)

;;; ----------------------------------------------------------------------------
;;; gdk_set_double_click_time ()
;;;
;;; void gdk_set_double_click_time (guint msec);
;;;
;;; Set the double click time for the default display. See
;;; gdk_display_set_double_click_time(). See also
;;; gdk_display_set_double_click_distance(). Applications should not set this,
;;; it is a global user-configured setting.
;;;
;;; msec :
;;;     double click time in milliseconds (thousandths of a second)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_keyboard_grab ()
;;;
;;; GdkGrabStatus gdk_keyboard_grab (GdkWindow *window,
;;;                                  gboolean owner_events,
;;;                                  guint32 time_);
;;;
;;; Warning
;;;
;;; gdk_keyboard_grab has been deprecated since version 3.0 and should not be
;;; used in newly-written code. Use gdk_device_grab() instead.
;;;
;;; Grabs the keyboard so that all events are passed to this application until
;;; the keyboard is ungrabbed with gdk_keyboard_ungrab(). This overrides any
;;; previous keyboard grab by this client.
;;;
;;; If you set up anything at the time you take the grab that needs to be
;;; cleaned up when the grab ends, you should handle the GdkEventGrabBroken
;;; events that are emitted when the grab ends unvoluntarily.
;;;
;;; window :
;;;     the GdkWindow which will own the grab (the grab window).
;;;
;;; owner_events :
;;;     if FALSE then all keyboard events are reported with respect to window.
;;;     If TRUE then keyboard events for this application are reported as
;;;     normal, but keyboard events outside this application are reported with
;;;     respect to window. Both key press and key release events are always
;;;     reported, independant of the event mask set by the application.
;;;
;;; time_ :
;;;     a timestamp from a GdkEvent, or GDK_CURRENT_TIME if no timestamp is
;;;     available.
;;;
;;; Returns :
;;;     GDK_GRAB_SUCCESS if the grab was successful.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyboard_grab" gdk-keyboard-grab) gdk-grab-status
  (window (g-object gdk-window))
  (owner-events :boolean)
  (time :uint32))

(export 'gdk-keyboard-grab)

;;; ----------------------------------------------------------------------------
;;; gdk_keyboard_ungrab ()
;;;
;;; void gdk_keyboard_ungrab (guint32 time_);
;;;
;;; Warning
;;;
;;; gdk_keyboard_ungrab has been deprecated since version 3.0 and should not be
;;; used in newly-written code. Use gdk_device_ungrab(), together with
;;; gdk_device_grab() instead.
;;;
;;; Ungrabs the keyboard on the default display, if it is grabbed by this
;;; application.
;;;
;;; time_ :
;;;     a timestamp from a GdkEvent, or GDK_CURRENT_TIME if no timestamp is
;;;     available.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyboard_ungrab" gdk-keyboard-ungrab) :void
  (time :uint32))

(export 'gdk-keyboard-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_beep ()
;;;
;;; void gdk_beep (void);
;;;
;;; Emits a short beep on the default display.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_beep" gdk-beep) :void)

(export 'gdk-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_error_trap_push ()
;;;
;;; void gdk_error_trap_push (void);
;;;
;;; This function allows X errors to be trapped instead of the normal behavior
;;; of exiting the application. It should only be used if it is not possible to
;;; avoid the X error in any other way. Errors are ignored on all GdkDisplay
;;; currently known to the GdkDisplayManager. If you don't care which error
;;; happens and just want to ignore everything, pop with
;;; gdk_error_trap_pop_ignored(). If you need the error code, use
;;; gdk_error_trap_pop() which may have to block and wait for the error to
;;; arrive from the X server.
;;;
;;; This API exists on all platforms but only does anything on X.
;;;
;;; You can use gdk_x11_display_error_trap_push() to ignore errors on only a
;;; single display.
;;;
;;; Example 1. Trapping an X error
;;;
;;;   gdk_error_trap_push ();
;;;   
;;;    // ... Call the X function which may cause an error here ...
;;;   
;;;   
;;;   if (gdk_error_trap_pop ())
;;;    {
;;;      // ... Handle the error here ...
;;;    }
;;;
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_error_trap_push" gdk-error-trap-push) :void)

(export 'gdk-error-trap-push)

;;; ----------------------------------------------------------------------------
;;; gdk_error_trap_pop ()
;;;
;;; gint gdk_error_trap_pop (void);
;;;
;;; Removes an error trap pushed with gdk_error_trap_push(). May block until an
;;; error has been definitively received or not received from the X server.
;;; gdk_error_trap_pop_ignored() is preferred if you don't need to know whether
;;; an error occurred, because it never has to block. If you don't need the
;;; return value of gdk_error_trap_pop(), use gdk_error_trap_pop_ignored().
;;;
;;; Prior to GDK 3.0, this function would not automatically sync for you, so you
;;; had to gdk_flush() if your last call to Xlib was not a blocking round trip.
;;;
;;; Returns :
;;;     X error code or 0 on success
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_error_trap_pop" gdk-error-trap-pop) :int)

(export 'gdk-error-trap-pop)

;;; ----------------------------------------------------------------------------
;;; gdk_error_trap_pop_ignored ()
;;;
;;; void gdk_error_trap_pop_ignored (void);
;;;
;;; Removes an error trap pushed with gdk_error_trap_push(), but without
;;; bothering to wait and see whether an error occurred. If an error arrives
;;; later asynchronously that was triggered while the trap was pushed, that
;;; error will be ignored.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_WINDOWING_X11
;;;
;;; #define GDK_WINDOWING_X11
;;;
;;; The GDK_WINDOWING_X11 macro is defined if the X11 backend is supported.
;;;
;;; Use this macro to guard code that is specific to the X11 backend.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_WINDOWING_WIN32
;;;
;;; #define GDK_WINDOWING_WIN32
;;;
;;; The GDK_WINDOWING_WIN32 macro is defined if the Win32 backend is supported.
;;;
;;; Use this macro to guard code that is specific to the Win32 backend.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_VERSION_3_0
;;;
;;; #define GDK_VERSION_3_0 (G_ENCODE_VERSION (3, 0))
;;;
;;; A macro that evaluates to the 3.0 version of GDK, in a format that can be
;;; used by the C pre-processor.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_VERSION_3_2
;;;
;;; #define GDK_VERSION_3_2 (G_ENCODE_VERSION (3, 2))
;;;
;;; A macro that evaluates to the 3.2 version of GDK, in a format that can be
;;; used by the C pre-processor.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_VERSION_3_4
;;;
;;; #define GDK_VERSION_3_4 (G_ENCODE_VERSION (3, 4))
;;;
;;; A macro that evaluates to the 3.4 version of GDK, in a format that can be
;;; used by the C pre-processor.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_VERSION_MIN_REQUIRED
;;;
;;; #define GDK_VERSION_MIN_REQUIRED (GDK_VERSION_PREV_STABLE)
;;;
;;; A macro that should be defined by the user prior to including the gdk.h
;;; header. The definition should be one of the predefined GDK version macros:
;;; GDK_VERSION_3_0, GDK_VERSION_3_2,...
;;;
;;; This macro defines the lower bound for the GLib API to use.
;;;
;;; If a function has been deprecated in a newer version of GDK, it is possible
;;; to use this symbol to avoid the compiler warnings without disabling warning
;;; for every deprecated function.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_VERSION_MAX_ALLOWED
;;;
;;; #define GDK_VERSION_MAX_ALLOWED GDK_VERSION_MIN_REQUIRED
;;;
;;; A macro that should be defined by the user prior to including the gdk.h
;;; header. The definition should be one of the predefined GDK version macros:
;;; GDK_VERSION_3_0, GDK_VERSION_3_2,...
;;;
;;; This macro defines the upper bound for the GDK API to use.
;;;
;;; If a function has been introduced in a newer version of GDK, it is possible
;;; to use this symbol to get compiler warnings when trying to use that
;;; function.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.general.lisp -------------------------------------------
