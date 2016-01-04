;;; ----------------------------------------------------------------------------
;;; gdk.general.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>
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
;;; General
;;;
;;;     Library initialization and miscellaneous functions
;;;
;;; Types and Values
;;;
;;;     GdkGrabStatus                            -> gdk.device.lisp
;;;
;;; Functions
;;;
;;;     gdk_init                                 * not implemented *
;;;     gdk_init_check                           * not implemented *
;;;     gdk_parse_args                           * not implemented *
;;;     gdk_get_display_arg_name
;;;     gdk_notify_startup_complete
;;;     gdk_notify_startup_complete_with_id
;;;     gdk_set_allowed_backends                 * not implemented *
;;;     gdk_get_program_class
;;;     gdk_set_program_class
;;;
;;;     gdk_get_display                          * deprecated *
;;;
;;;     gdk_flush
;;;     gdk_screen_width
;;;     gdk_screen_height
;;;     gdk_screen_width_mm
;;;     gdk_screen_height_mm
;;;
;;;     gdk_pointer_grab                         * deprecated *
;;;     gdk_pointer_ungrab                       * deprecated *
;;;     gdk_pointer_is_grabbed                   * deprecated *
;;;
;;;     gdk_set_double_click_time
;;;
;;;     gdk_keyboard_grab                        * deprecated *
;;;     gdk_keyboard_ungrab                      * deprecated *
;;;
;;;     gdk_beep
;;;     gdk_error_trap_push
;;;     gdk_error_trap_pop
;;;     gdk_error_trap_pop_ignored
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
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_display_arg_name" gdk-get-display-arg-name)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @return{The display name, if specified explicitely, otherwise @code{nil}.}
  @begin{short}
    Gets the display name specified in the command line arguments passed to
    the functions @code{gdk_init} or @code{gdk_parse_args}, if any.
  @end{short}

  Since 2.2")

(export 'gdk-get-display-arg-name)

;;; ----------------------------------------------------------------------------
;;; gdk_notify_startup_complete ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_notify_startup_complete" gdk-notify-startup-complete) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-26}
  @begin{short}
    Indicates to the GUI environment that the application has finished loading.
  @end{short}
  If the applications opens windows, this function is normally called after
  opening the application's initial set of windows.

  GTK+ will call this function automatically after opening the first
  @class{gtk-window} unless the function
  @fun{gtk-window-set-auto-startup-notification} is called to disable that
  feature.

  Since 2.2
  @see-function{gdk-notify-startup-complete-with-id}
  @see-function{gtk-window-set-auto-startup-notification}")

(export 'gdk-notify-startup-complete)

;;; ----------------------------------------------------------------------------
;;; gdk_notify_startup_complete_with_id ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_notify_startup_complete_with_id"
           gdk-notify-startup-complete-with-id) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-26}
  @argument[startup-id]{a startup notification identifier, for which
    notification process should be completed}
  @begin{short}
    Indicates to the GUI environment that the application has finished loading,
    using a given identifier.
  @end{short}

  GTK+ will call this function automatically for @class{gtk-window} with custom
  startup notification identifier unless the function
  @fun{gtk-window-set-auto-startup-notification} is called to disable that
  feature.

  Since 2.12
  @see-function{gdk-notify-startup-complete}
  @see-function{gtk-window-set-auto-startup-notification}"
  (startup-id :string))

(export 'gdk-notify-startup-complete-with-id)

;;; ----------------------------------------------------------------------------
;;; gdk_set_allowed_backends ()
;;;
;;; void gdk_set_allowed_backends (const gchar *backends);
;;;
;;; Sets a list of backends that GDK should try to use.
;;;
;;; This can be be useful if your application does not work with certain GDK
;;; backends.
;;;
;;; By default, GDK tries all included backends.
;;;
;;; For example,
;;;
;;; gdk_set_allowed_backends ("wayland,quartz,*");
;;;
;;; instructs GDK to try the Wayland backend first, followed by the Quartz
;;; backend, and then all others.
;;;
;;; If the GDK_BACKEND environment variable is set, it determines what backends
;;; are tried in what order, while still respecting the set of allowed backends
;;; that are specified by this function.
;;;
;;; The possible backend names are x11, win32, quartz, broadway, wayland. You
;;; can also include a * in the list to try all remaining backends.
;;;
;;; This call must happen prior to gdk_display_open(), gtk_init(),
;;; gtk_init_with_args() or gtk_init_check() in order to take effect.
;;;
;;; Parameters
;;;
;;; backends
;;;     a comma-separated list of backends
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_get_program_class ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_program_class" gdk-get-program-class)
    (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @return{The program class.}
  Gets the program class. Unless the program class has explicitly been set
  with the function @fun{gdk-set-program-class} or with the @code{--class}
  commandline option, the default value is the program name determined with
  the function @fun{g-get-prgname} and with the first character converted to
  uppercase.
  @see-function{gdk-set-program-class}
  @see-function{g-get-prgname}")

(export 'gdk-get-program-class)

;;; ----------------------------------------------------------------------------
;;; gdk_set_program_class ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_set_program_class" gdk-set-program-class) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @argument[program-class]{a string}
  Sets the program class. The X11 backend uses the program class to set the
  class name part of the @code{WM_CLASS} property on toplevel windows; see the
  Inter-Client Communication Conventions Manual (ICCCM).
  @see-function{gdk-get-program-class}"
  (program-class (:string :free-to-foreign t)))

(export 'gdk-set-program-class)

;;; ----------------------------------------------------------------------------
;;; gdk_get_display ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_display" gdk-get-display) (:string :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2015-12-30}
  @return{The name of the display.}
  @begin{short}
    Gets the name of the display, which usually comes from the @code{DISPLAY}
    environment variable or the @code{--display} command line option.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-get-display} has been deprecated since version 3.8
    and should not be used in newly-written code. Call
    @code{(gdk-display-get-name (gdk-display-get-default))} instead.
  @end{dictionary}
  @see-function{gdk-display-get-name}
  @see-function{gdk-display-get-default}")

(export 'gdk-get-display)

;;; ----------------------------------------------------------------------------
;;; gdk_flush ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_flush" gdk-flush) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  Flushes the output buffers of all display connections and waits until all
  requests have been processed. This is rarely needed by applications.")

(export 'gdk-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_width" gdk-screen-width) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @return{The width of the default screen in pixels.}
  Returns the width of the default screen in pixels.
  @see-function{gdk-screen-height}
  @see-function{gdk-screen-width-mm}
  @see-function{gdk-screen-height-mm}")

(export 'gdk-screen-width)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_height" gdk-screen-height) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @return{The height of the default screen in pixels.}
  Returns the height of the default screen in pixels.
  @see-function{gdk-screen-width}
  @see-function{gdk-screen-width-mm}
  @see-function{gdk-screen-height-mm}")

(export 'gdk-screen-height)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_width_mm ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_width_mm" gdk-screen-width-mm) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @return{The width of the default screen in millimeters, though it is not
    always correct.}
  Returns the width of the default screen in millimeters. Note that on many X
  servers this value will not be correct.
  @see-function{gdk-screen-width}
  @see-function{gdk-screen-height}
  @see-function{gdk-screen-height-mm}")

(export 'gdk-screen-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_height_mm ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_screen_height_mm" gdk-screen-height-mm) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @return{The height of the default screen in millimeters, though it is not
    always correct.}
  Returns the height of the default screen in millimeters. Note that on many X
  servers this value will not be correct.
  @see-function{gdk-screen-height}
  @see-function{gdk-screen-width}
  @see-function{gdk-screen-width-mm}")

(export 'gdk-screen-height-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_pointer_grab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pointer_grab" gdk-pointer-grab) gdk-grab-status
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[window]{the @class{gdk-window} which will own the grab
    (the grab window)}
  @argument[owner-events]{if @code{nil} then all pointer events are reported
    with respect to window and are only reported if selected by
    @arg{event-mask}. If @em{true} then pointer events for this application are
    reported as normal, but pointer events outside this application are reported
    with respect to window and only if selected by @arg{event-mask}. In either
    mode, unreported events are discarded.}
  @argument[event-mask]{specifies the event mask, which is used in accordance
    with @arg{owner-events}. Note that only pointer events (i. e. button and
    motion events) may be selected.}
  @argument[confine-to]{If non-@code{NULL}, the pointer will be confined to this
    window during the grab. If the pointer is outside @arg{confine-to}, it will
    automatically be moved to the closest edge of @arg{confine-to} and enter and
    leave events will be generated as necessary.}
  @argument[cursor]{the cursor to display while the grab is active. If this is
    @code{NULL} then the normal cursors are used for @arg{window} and its
    descendants, and the cursor for @arg{window} is used for all other windows.}
  @argument[time]{the timestamp of the event which led to this pointer grab.
    This usually comes from a @class{gdk-event-button} struct, though
    @var{+gdk-current-time+} can be used if the time isn't known.}
  @return{@code{:success} if the grab was successful.}
  @begin{short}
    Grabs the pointer (usually a mouse) so that all events are passed to this
    application until the pointer is ungrabbed with @fun{gdk-pointer-ungrab}, or
    the grab window becomes unviewable. This overrides any previous pointer grab
    by this client.
  @end{short}

  Pointer grabs are used for operations which need complete control over mouse
  events, even if the mouse leaves the application. For example in GTK+ it is
  used for Drag and Drop, for dragging the handle in the @class{gtk-paned}
  widgets.

  Note that if the event mask of an X window has selected both button press
  and button release events, then a button press event will cause an automatic
  pointer grab until the button is released. X does this automatically since
  most applications expect to receive button press and release events in
  pairs. It is equivalent to a pointer grab on the window with
  @arg{owner-events} set to @em{true}.

  If you set up anything at the time you take the grab that needs to be
  cleaned up when the grab ends, you should handle the
  @class{gdk-event-grab-broken} events that are emitted when the grab ends
  unvoluntarily.
  @begin[Warning]{dictionary}
    @sym{gdk-pointer-grab} has been deprecated since version 3.0 and should not
    be used in newly-written code. Use @fun{gdk-device-grab} instead.
  @end{dictionary}
  @see-function{gdk-device-grab}
  @see-function{gdk-pointer-ungrab}"
  (window (g-object gdk-window))
  (owner-events :boolean)
  (event-mask gdk-event-mask)
  (confine-to (g-object gdk-window))
  (cursor (g-object gdk-cursor))
  (time :uint32))

(export 'gdk-pointer-grab)

;;; ----------------------------------------------------------------------------
;;; gdk_pointer_ungrab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pointer_ungrab" gdk-pointer-ungrab) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[time]{a timestamp from a @class{gdk-event}, or
    @var{+gdk-current-time+} if no timestamp is available}
  @begin{short}
    Ungrabs the pointer on the default display, if it is grabbed by this
    application.
  @end{short}
  @begin[Warning]{dictionary}
    @sym{gdk-pointer-ungrab} has been deprecated since version 3.0 and should
    not be used in newly written code. Use @fun{gdk-device-ungrab}, together
    with @fun{gdk-device-grab} instead.
  @end{dictionary}
  @see-function{gdk-device-grab}
  @see-function{gdk-device-ungrab}"
  (time :uint32))

(export 'gdk-pointer-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_pointer_is_grabbed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pointer_is_grabbed" gdk-pointer-is-grabbed) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @return{@em{True} if the pointer is currently grabbed by this application.}
  @begin{short}
    Returns @em{true} if the pointer on the default display is currently
    grabbed by this application.
  @end{short}

  Note that this does not take the inmplicit pointer grab on button presses
  into account.
  @begin[Warning]{dictionary}
    @sym{gdk-pointer-is-grabbed} has been deprecated since version 3.0 and
    should not be used in newly written code. Use
    @fun{gdk-display-device-is-grabbed} instead.
  @end{dictionary}
  @see-function{gdk-display-device-is-grabbed}")

(export 'gdk-pointer-is-grabbed)

;;; ----------------------------------------------------------------------------
;;; gdk_set_double_click_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_set_double_click_time" gdk-set-double-click-time) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @argument[msec]{double click time in milliseconds (thousandths of a second)}
  @begin{short}
    Set the double click time for the default display.
  @end{short}
  See the function @fun{gdk-display-set-double-click-time}. See also the
  function @fun{gdk-display-set-double-click-distance}. Applications should not
  set this, it is a global user-configured setting.
  @see-function{gdk-display-set-double-click-time}
  @see-function{gdk-display-set-double-click-distance}"
  (msec :uint))

(export 'gdk-set-double-click-time)

;;; ----------------------------------------------------------------------------
;;; gdk_keyboard_grab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyboard_grab" gdk-keyboard-grab) gdk-grab-status
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[window]{the @class{gdk-window} which will own the grab (the
    grab window).}
  @argument[owner-events]{if @code{nil} then all keyboard events are reported
    with respect to window. If @em{true} then keyboard events for this
    application are reported as normal, but keyboard events outside this
    application are reported with respect to window. Both key press and key
    release events are always reported, independant of the event mask set by the
    application.}
  @argument[time]{a timestamp from a @class{gdk-event}, or
    @var{+gdk-current-time+} if no timestamp is available.}
  @return{@code{:success} if the grab was successful.}
  @begin{short}
    Grabs the keyboard so that all events are passed to this application until
    the keyboard is ungrabbed with @fun{gdk-keyboard-ungrab}. This overrides any
    previous keyboard grab by this client.
  @end{short}

  If you set up anything at the time you take the grab that needs to be
  cleaned up when the grab ends, you should handle the
  @class{gdk-event-grab-broken} events that are emitted when the grab ends
  unvoluntarily.
  @begin[Warning]{dictionary}
    @sym{gdk-keyboard-grab} has been deprecated since version 3.0 and should not
    be used in newly-written code. Use @fun{gdk-device-grab} instead.
  @end{dictionary}
  @see-function{gdk-device-grab}
  @see-function{gdk-keyboard-ungrab}"
  (window (g-object gdk-window))
  (owner-events :boolean)
  (time :uint32))

(export 'gdk-keyboard-grab)

;;; ----------------------------------------------------------------------------
;;; gdk_keyboard_ungrab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_keyboard_ungrab" gdk-keyboard-ungrab) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[time]{a timestamp from a @class{gdk-event}, or
    @var{+gdk-current-time+} if no timestamp is available}
  @begin{short}
    Ungrabs the keyboard on the default display, if it is grabbed by this
    application.
  @end{short}
  @begin[Warning]{dictionary}
    @sym{gdk-keyboard-ungrab} has been deprecated since version 3.0 and should
    not be used in newly written code. Use @fun{gdk-device-ungrab}, together
    with @fun{gdk-device-grab} instead.
  @end{dictionary}
  @see-function{gdk-device-grab}
  @see-function{gdk-device-ungrab}"
  (time :uint32))

(export 'gdk-keyboard-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_beep ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_beep" gdk-beep) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  Emits a short beep on the default display.")

(export 'gdk-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_error_trap_push ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_error_trap_push" gdk-error-trap-push) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @begin{short}
    This function allows X errors to be trapped instead of the normal behavior
    of exiting the application.
  @end{short}
  It should only be used if it is not possible to avoid the X error in any other
  way. Errors are ignored on all @class{gdk-display} currently known to the
  @class{gdk-display-manager}. If you do not care which error happens and just
  want to ignore everything, pop with the function
  @fun{gdk-error-trap-pop-ignored}. If you need the error code, use the function
  @fun{gdk-error-trap-pop} which may have to block and wait for the error to
  arrive from the X server.

  This API exists on all platforms but only does anything on X.

  You can use the function @code{gdk-x11-display-error-trap-push} to ignore
  errors on only a single display.

  @b{Example:} Trapping an X error
  @begin{pre}
   gdk_error_trap_push ();

    // ... Call the X function which may cause an error here ...


   if (gdk_error_trap_pop ())
    {
      // ... Handle the error here ...
    @}
  @end{pre}
  @see-class{gdk-display}
  @see-class{gdk-display-manager}
  @see-function{gdk-error-trap-pop-ignored}
  @see-function{gdk-error-trap-pop}")

(export 'gdk-error-trap-push)

;;; ----------------------------------------------------------------------------
;;; gdk_error_trap_pop ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_error_trap_pop" gdk-error-trap-pop) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @return{X error code or 0 on success.}
  @begin{short}
    Removes an error trap pushed with the function @fun{gdk-error-trap-push}.
  @end{short}
  May block until an error has been definitively received or not received from
  the X server. The function @fun{gdk-error-trap-pop-ignored} is preferred if
  you do not need to know whether an error occurred, because it never has to
  block. If you do not need the return value of @sym{gdk-error-trap-pop}, use
  the function @fun{gdk-error-trap-pop-ignored}.

  Prior to GDK 3.0, this function would not automatically sync for you, so you
  had to call the function @fun{gdk-flush} if your last call to Xlib was not a
  blocking round trip.
  @see-function{gdk-error-trap-push}
  @see-function{gdk-error-trap-pop-ignored}
  @see-fun{gdk-flush}")

(export 'gdk-error-trap-pop)

;;; ----------------------------------------------------------------------------
;;; gdk_error_trap_pop_ignored ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_error_trap_pop_ignored" gdk-error-trap-pop-ignored) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-30}
  @begin{short}
    Removes an error trap pushed with the function @fun{gdk-error-trap-push},
    but without bothering to wait and see whether an error occurred.
  @end{short}
  If an error arrives later asynchronously that was triggered while the trap was
  pushed, that error will be ignored.

  Since 3.0
  @see-fun{gdk-error-trap-push}")

(export 'gdk-error-trap-pop-ignored)

;;; --- End of file gdk.general.lisp -------------------------------------------
