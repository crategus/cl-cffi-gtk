;;; ----------------------------------------------------------------------------
;;; gdk.general.lisp
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
;;; General
;;;
;;; Library initialization and miscellaneous functions
;;; 
;;; SYNOPSIS
;;; 
;;;     gdk_init
;;;     gdk_init_check
;;;     gdk_parse_args
;;;     gdk_get_display_arg_name
;;;     gdk_set_locale
;;;     gdk_set_sm_client_id
;;;     gdk_exit
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
;;;     GdkGrabStatus
;;;
;;;     gdk_pointer_grab
;;;     gdk_pointer_ungrab
;;;     gdk_pointer_is_grabbed
;;;     gdk_set_double_click_time
;;;
;;;     gdk_keyboard_grab
;;;     gdk_keyboard_ungrab
;;;
;;;     gdk_beep
;;;
;;;     gdk_get_use_xshm              *deprecated*
;;;     gdk_set_use_xshm              *deprecated*
;;;
;;;     gdk-error-trap-push ()
;;;     gdk-error-trap-pop ()
;;;
;;;     GDK_WINDOWING_X11
;;;     GDK_WINDOWING_WIN32
;;;
;;; DESCRIPTION
;;;
;;; This section describes the GDK initialization functions and miscellaneous
;;; utility functions.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_init ()
;;;
;;; void gdk_init (gint *argc, gchar ***argv)
;;;
;;; Initializes the GDK library and connects to the X server. If initialization
;;; fails, a warning message is output and the application terminates with a
;;; call to exit(1).
;;;
;;; Any arguments used by GDK are removed from the array and argc and argv are
;;; updated accordingly.
;;;
;;; GTK+ initializes GDK in gtk_init() and so this function is not usually
;;; needed by GTK+ applications.
;;;
;;; argc : . [inout]
;;; argv : . [array length=argc][inout]
;;;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_init_check ()
;;;
;;; gboolean gdk_init_check (gint *argc, gchar ***argv)
;;;
;;; Initialize the library for use.
;;;
;;; Arguments: "argc" is the number of arguments. "argv" is an array of strings.
;;;
;;; Results: "argc" and "argv" are modified to reflect any arguments which were
;;; not handled. (Such arguments should either be handled by the application or
;;; dismissed). If initialization fails, returns FALSE, otherwise TRUE.
;;;
;;; Side effects: The library is initialized.
;;;
;;; Initializes the GDK library and connects to the X server, returning TRUE on
;;; success.
;;;
;;; Any arguments used by GDK are removed from the array and argc and argv are
;;; updated accordingly.
;;;
;;; GTK+ initializes GDK in gtk_init() and so this function is not usually
;;; needed by GTK+ applications.
;;;
;;; argc : . [inout]
;;; argv : . [array length=argc][inout]
;;;
;;; Returns :
;;;	TRUE if initialization succeeded.
;;;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_parse_args ()
;;;
;;; void gdk_parse_args (gint *argc, gchar ***argv)
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
;;; 	the number of command line arguments.
;;;
;;; argv :
;;;	the array of command line arguments. [inout][array length=argc]
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_get_display_arg_name ()
;;;
;;; const gchar * gdk_get_display_arg_name (void)
;;;
;;; Gets the display name specified in the command line arguments passed to
;;; gdk_init() or gdk_parse_args(), if any.
;;;
;;; Returns :
;;;	the display name, if specified explicitely, otherwise NULL this string
;;;     is owned by GTK+ and must not be modified or freed.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_set_locale ()
;;;
;;; gchar * gdk_set_locale (void)
;;;
;;; Warning
;;;
;;; gdk_set_locale has been deprecated since version 2.24 and should not be
;;; used in newly-written code. Use setlocale() directly
;;;
;;; Initializes the support for internationalization by calling the setlocale()
;;; system call. This function is called by gtk_set_locale() and so GTK+
;;; applications should use that instead.
;;;
;;; The locale to use is determined by the LANG environment variable, so to run
;;; an application in a certain locale you can do something like this:
;;;
;;;  1 export LANG="fr"
;;;  2 ... run application ...
;;;
;;; If the locale is not supported by X then it is reset to the standard "C"
;;; locale.
;;;
;;; Returns :
;;;	the resulting locale.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_set_locale" gdk-set-locale) (:string :free-from-foreign nil))

(export 'gdk-set-locale)

;;; ----------------------------------------------------------------------------
;;; gdk_set_sm_client_id ()
;;;
;;; void gdk_set_sm_client_id (const gchar *sm_client_id)
;;;
;;; WARNING
;;;
;;; gdk_set_sm_client_id has been deprecated since version 2.24 and should not
;;; be used in newly-written code. Use gdk_x11_set_sm_client_id() instead.
;;;
;;; Sets the SM_CLIENT_ID property on the application's leader window so that
;;; the window manager can save the application's state using the X11R6 ICCCM
;;; session management protocol.
;;;
;;; See the X Session Management Library documentation for more information on
;;; session management and the Inter-Client Communication Conventions Manual
;;; (ICCCM) for information on the WM_CLIENT_LEADER property. (Both documents
;;; are part of the X Window System distribution.)

;;; sm_client_id :
;;;	the client id assigned by the session manager when the connection was
;;;     opened, or NULL to remove the property.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_set_sm_client_id" gdk-set-sm-client-id) :void
  (sm-client-id :string))

(export 'gdk-set-sm-client-id)

;;; ----------------------------------------------------------------------------
;;; gdk_exit ()
;;;
;;; void gdk_exit (gint error_code)
;;;
;;; WARNING
;;;
;;; gdk_exit is deprecated and should not be used in newly-written code.
;;; 
;;; Exits the application using the exit() system call.
;;; 
;;; This routine is provided mainly for backwards compatibility, since it used
;;; to perform tasks necessary to exit the application cleanly. Those tasks are
;;; now performed in a function which is automatically called on exit (via the
;;; use of g_atexit()).
;;;
;;; error_code :
;;;	the error code to pass to the exit() call.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_notify_startup_complete ()
;;;
;;; void gdk_notify_startup_complete (void)
;;;
;;; Indicates to the GUI environment that the application has finished loading.
;;; If the applications opens windows, this function is normally called after
;;; opening the application's initial set of windows.
;;;
;;; GTK+ will call this function automatically after opening the first
;;; GtkWindow unless gtk_window_set_auto_startup_notification() is called to
;;; disable that feature.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;; This function is not exported. It is for internal use.
;; See the next function which combines the functions
;; gdk_notifiy_startup_complete and gdk_notifiy_startup_complete_with_id

(defcfun ("gdk_notifiy_startup_complete" %gdk-notify-startup-complete) :void)

;;; ----------------------------------------------------------------------------
;;; gdk_notify_startup_complete_with_id ()
;;;
;;; void gdk_notify_startup_complete_with_id (const gchar *startup_id)
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
;;;	a startup-notification identifier, for which notification process
;;;     should be completed
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
;;; const char * gdk_get_program_class (void)
;;;
;;; Gets the program class. Unless the program class has explicitly been set
;;; with gdk_set_program_class() or with the --class commandline option, the
;;; default value is the program name (determined with g_get_prgname()) with
;;; the first character converted to uppercase.
;;;
;;; Returns :
;;;	the program class.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_program_class" gdk-get-program-class)
    (:string :free-from-foreign nil))

(export 'gdk-get-programm-class)

;;; ----------------------------------------------------------------------------
;;; gdk_set_program_class ()
;;;
;;; void gdk_set_program_class (const char *program_class)
;;;
;;; Sets the program class. The X11 backend uses the program class to set the
;;; class name part of the WM_CLASS property on toplevel windows; see the ICCCM.
;;;
;;; program_class :
;;;	a string.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_set_program-class" gdk-set-program-class) :void
  (program-class (:string :free-to-foreign t)))

(export 'gdk-set-program-class)

;; The following is cut out

;(defun program-class ()
;  (gdk-get-program-class))

;(defun (setf program-class) (new-value)
;  (gdk-set-program-class new-value))

;(export 'program-class)

;;; ----------------------------------------------------------------------------
;;; gdk_get_display ()
;;;
;;; gchar * gdk_get_display (void)
;;;
;;; Gets the name of the display, which usually comes from the DISPLAY
;;; environment variable or the --display command line option.
;;;
;;; Returns :
;;;	the name of the display.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_display" gdk-get-display) (:string :free-from-foreign nil))

(export 'gdk-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_flush ()
;;;
;;; void gdk_flush (void)
;;;
;;; Flushes the X output buffer and waits until all requests have been
;;; processed by the server. This is rarely needed by applications. It's main
;;; use is for trapping X errors with gdk_error_trap_push() and
;;; gdk_error_trap_pop().
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_flush" gdk-flush) :void)

(export 'gdk-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_width ()
;;;
;;; gint gdk_screen_width (void)
;;;
;;; Returns the width of the default screen in pixels.
;;;
;;; Returns :
;;;	the width of the default screen in pixels.
;;; ----------------------------------------------------------------------------

;; Do not use the name gdk-screen-width.
;; It is an accessor for the class gdk-screen.

(defcfun ("gdk_screen_width" %gdk-screen-width) :int)

(export '%gdk-screen-width)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_height ()
;;;
;;; gint gdk_screen_height (void)
;;; 
;;; Returns the height of the default screen in pixels.
;;;
;;; Returns :
;;;	the height of the default screen in pixels.
;;; ----------------------------------------------------------------------------

;; Do not use the name gdk-screen-height.
;; It is an accessor for the class gdk-screen.

(defcfun ("gdk_screen_height" %gdk-screen-height) :int)

(export '%gdk-screen-height)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_width_mm ()
;;;
;;; gint gdk_screen_width_mm (void)
;;;
;;; Returns the width of the default screen in millimeters. Note that on many
;;; X servers this value will not be correct.
;;;
;;; Returns :
;;;	the width of the default screen in millimeters, though it is not
;;;     always correct.
;;; ----------------------------------------------------------------------------

;; Do not use the name gdk-screen-width-mm.
;; It is an accessor for the class gdk-screen.

(defcfun ("gdk_screen_width_mm" %gdk-screen-width-mm) :int)

(export '%gdk-screen-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_height_mm ()
;;;
;;; gint gdk_screen_height_mm (void)
;;;
;;; Returns the height of the default screen in millimeters. Note that on many
;;; X servers this value will not be correct.
;;;
;;; Returns :
;;;	the height of the default screen in millimeters, though it is not
;;;     always correct.
;;; ----------------------------------------------------------------------------

;; Do not use the name gdk-screen-height-mm.
;; It is an accessor for the class gdk-screen.

(defcfun ("gdk_screen_height_mm" %gdk-screen-height-mm) :int)

(export '%gdk-screen-heigth-mm)

;;; ----------------------------------------------------------------------------
;;; enum GdkGrabStatus
;;;
;;; typedef enum
;;; {
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
;;;	the resource was successfully grabbed.
;;;
;;; GDK_GRAB_ALREADY_GRABBED
;;;	the resource is actively grabbed by another client.
;;;
;;; GDK_GRAB_INVALID_TIME
;;;	the resource was grabbed more recently than the specified time.
;;;
;;; GDK_GRAB_NOT_VIEWABLE
;;;	the grab window or the confine_to window are not viewable.
;;;
;;; GDK_GRAB_FROZEN
;;;	the resource is frozen by an active grab of another client.
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
;;;                                           gboolean owner_events,
;;;                                           GdkEventMask event_mask,
;;;                                           GdkWindow *confine_to,
;;;                                           GdkCursor *cursor,
;;;                                           guint32 time_)
;;;
;;; Grabs the pointer (usually a mouse) so that all events are passed to this
;;; application until the pointer is ungrabbed with gdk_pointer_ungrab(), or
;;; the grab window becomes unviewable. This overrides any previous pointer
;;; grab by this client.
;;;
;;; Pointer grabs are used for operations which need complete control over
;;; mouse events, even if the mouse leaves the application. For example in GTK+
;;; it is used for Drag and Drop, for dragging the handle in the GtkHPaned and
;;; GtkVPaned widgets, and for resizing columns in GtkCList widgets.
;;;
;;; Note that if the event mask of an X window has selected both button press
;;; and button release events, then a button press event will cause an
;;; automatic pointer grab until the button is released. X does this
;;; automatically since most applications expect to receive button press and
;;; release events in pairs. It is equivalent to a pointer grab on the window
;;; with owner_events set to TRUE.
;;;
;;; If you set up anything at the time you take the grab that needs to be
;;; cleaned up when the grab ends, you should handle the GdkEventGrabBroken
;;; events that are emitted when the grab ends unvoluntarily.
;;;
;;; window :
;;;	the GdkWindow which will own the grab (the grab window).
;;;
;;; owner-events :
;;;	if FALSE then all pointer events are reported with respect to window
;;;     and are only reported if selected by event-mask. If TRUE then pointer
;;;     events for this application are reported as normal, but pointer events
;;;     outside this application are reported with respect to window and only
;;;     if selected by event_mask. In either mode, unreported events are
;;;     discarded.
;;;
;;; event-mask :
;;;	specifies the event mask, which is used in accordance with
;;;     owner-events. Note that only pointer events (i.e. button and motion
;;;     events) may be selected.
;;;
;;; confine-to :
;;;	If non-NULL, the pointer will be confined to this window during the
;;;     grab. If the pointer is outside confine_to, it will automatically be
;;;     moved to the closest edge of confine_to and enter and leave events will
;;;     be generated as necessary.
;;;
;;; cursor :
;;;	the cursor to display while the grab is active. If this is NULL then
;;;     the normal cursors are used for window and its descendants, and the
;;;     cursor for window is used for all other windows.
;;;
;;; time :
;;;	the timestamp of the event which led to this pointer grab. This usually
;;;     comes from a GdkEventButton struct, though GDK_CURRENT_TIME can be used
;;;     if the time isn't known.
;;;
;;; Returns :
;;;	GDK_GRAB_SUCCESS if the grab was successful.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pointer_grab" gdk-pointer-grab) gdk-grab-status
  (window (g-object gdk-window))
  (owner-events :boolean)
  (event-mask gdk-event-mask)
  (confine-to (g-object gdk-window))
  (cursor (g-boxed-foreign cursor))
  (time :uint32))

(export 'gdk-pointer-grab)

;;; ----------------------------------------------------------------------------
;;; gdk_pointer_ungrab ()
;;;
;;; void gdk_pointer_ungrab (guint32 time_)
;;;
;;; Ungrabs the pointer on the default display, if it is grabbed by this
;;; application.
;;;
;;; time :
;;;     a timestamp from a GdkEvent, or GDK_CURRENT_TIME if no timestamp is
;;;     available.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pointer_ungrab" gdk-pointer-ungrab) :void
  (time :uint32))

(export 'gdk-pointer-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_pointer_is_grabbed ()
;;;
;;; gboolean gdk_pointer_is_grabbed (void)
;;;
;;; Returns TRUE if the pointer on the default display is currently grabbed by
;;; this application.
;;;
;;; Note that this does not take the implicit pointer grab on button presses
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
;;; void gdk_set_double_click_time (guint msec)
;;;
;;; Set the double click time for the default display.
;;; See gdk_display_set_double_click_time().
;;; See also gdk_display_set_double_click_distance(). Applications should not
;;; set this, it is a global user-configured setting.
;;;
;;; msec :
;;;     double click time in milliseconds (thousandths of a second)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_keyboard_grab ()
;;;
;;; GdkGrabStatus gdk_keyboard_grab (GdkWindow *window,
;;;                                            gboolean owner_events,
;;;                                            guint32 time_)
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
;;;	the GdkWindow which will own the grab (the grab window).
;;;
;;; owner-events :
;;;	if FALSE then all keyboard events are reported with respect to window.
;;;     If TRUE then keyboard events for this application are reported as
;;;     normal, but keyboard events outside this application are reported with
;;;     respect to window. Both key press and key release events are always
;;;     reported, independant of the event mask set by the application.
;;;
;;; time :
;;;	a timestamp from a GdkEvent, or GDK_CURRENT_TIME if no timestamp is
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
;;; void gdk_keyboard_ungrab (guint32 time_)
;;;
;;; Ungrabs the keyboard on the default display, if it is grabbed by this
;;; application.
;;;
;;; time :
;;;     a timestamp from a GdkEvent, or GDK_CURRENT_TIME if no timestamp is
;;;     available.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyboard_ungrab" gdk-keyboard-ungrab) :void
  (time :uint32))

(export 'gdk-keyboard-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_beep ()
;;;
;;; void gdk_beep (void)
;;;
;;; Emits a short beep on the default display.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_beep" gdk-beep) :void)

(export 'gdk-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_error_trap_push ()
;;;
;;; void gdk_error_trap_push (void)
;;;
;;; This function allows X errors to be trapped instead of the normal behavior
;;; of exiting the application. It should only be used if it is not possible to
;;; avoid the X error in any other way.
;;;
;;; Example 1. Trapping an X error
;;;
;;; 1 gdk_error_trap_push ();
;;; 2 /* ... Call the X function which may cause an error here ... */
;;; 3 /* Flush the X queue to catch errors now. */
;;; 4 gdk_flush ();
;;; 5 if (gdk_error_trap_pop ())
;;; 6   {
;;; 7     /* ... Handle the error here ... */
;;; 8   }
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_error_trap_push" gdk-error-trap-push) :void)

(export 'gdk-error-trap-push)

;;; ----------------------------------------------------------------------------
;;; gdk_error_trap_pop ()
;;;
;;; gint gdk_error_trap_pop (void)
;;;
;;; Removes the X error trap installed with gdk_error_trap_push().
;;;
;;; Returns :
;;;     the X error code, or 0 if no error occurred.
;;; ----------------------------------------------------------------------------

;(defcfun ("gdK_error_trap_pop" gdk-error-trap-pop) :int)

;(export 'gdk-error-trap-pop)

;;; ----------------------------------------------------------------------------
;;; GDK_WINDOWING_X11
;;;
;;; #define GDK_WINDOWING_X11
;;;
;;; This macro is defined if GDK is configured to use the X11 backend.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_WINDOWING_WIN32
;;;
;;; #define GDK_WINDOWING_WIN32
;;;
;;; This macro is defined if GDK is configured to use the Win32 backend.
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.general.lisp -------------------------------------------
