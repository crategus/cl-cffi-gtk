;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.main-loop.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
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

(in-package :gtk)

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-disable-setlocale 'function)
 "@version{2012-12-23}
  @begin{short}
    Prevents @fun{gtk-init}, @fun{gtk-init-check},
    @code{gtk_init_with_args()} and @code{gtk_parse_args()} from automatically
    calling @code{setlocale (LC_ALL, \"\")}.
  @end{short}
  You would want to use this function if you wanted to set the locale for your
  program to something other than the user's locale, or if you wanted to set
  different values for different locale categories.

  Most programs should not need to call this function.
  @begin[Lisp Implemention]{dictionary}
    In the Lisp implementationt the function @sym{gtk-init} is called
    automatically when loading the library @code{cl-cffi-gtk}. Therefore
    @sym{gtk-disable-setlocale} does not have any effect.
  @end{dictionary}
  @see-function{gtk-init}
  @see-function{gtk-init-check}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-get-default-language 'function)
 "@version{2012-12-20}
  @return{the default language as a @class{pango-language}, must not be freed}
  @begin{short}
    Returns the @class{pango-language} for the default language currently in
    effect.
  @end{short}
  (Note that this can change over the life of an application.) The default
  language is derived from the current locale. It determines, for example,
  whether GTK+ uses the right-to-left or left-to-right text direction.

  This function is equivalent to @fun{pango-language-get-default}. See that
  function for details.
  @begin[Example]{dictionary}
    @begin{pre}
 (setq lang (gtk-get-default-language))
=> #<PANGO-LANGUAGE {C7B3C51@}>
 (pango-language-to-string lang)
=> \"de-de\"
 (pango-language-get-sample-string lang)
=> \"Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich.\"
    @end{pre}
  @end{dictionary}
  @see-class{pango-language}
  @see-function{pango-language-get-default}")

;;; ----------------------------------------------------------------------------
;;; gtk_parse_args ()
;;;
;;; gboolean gtk_parse_args (int *argc, char ***argv);
;;;
;;; Parses command line arguments, and initializes global attributes of GTK+,
;;; but does not actually open a connection to a display. (See
;;; gdk_display_open(), gdk_get_display_arg_name())
;;;
;;; Any arguments used by GTK+ or GDK are removed from the array and argc and
;;; argv are updated accordingly.
;;;
;;; There is no need to call this function explicitely if you are using
;;; gtk_init(), or gtk_init_check().
;;;
;;; argc :
;;;     a pointer to the number of command line arguments
;;;
;;; argv :
;;;     a pointer to the array of command line arguments
;;;
;;; Returns :
;;;     TRUE if initialization succeeded, otherwise FALSE
;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-init 'function)
 "@version{2012-12-23}
  @argument[argc]{Address of the @arg{argc} parameter of your @code{main()}
    function (or 0 if @arg{argv} is @code{NULL}). This will be changed if any
    arguments were handled.}
  @argument[argv]{Address of the @arg{argv} parameter of @code{main()}, or
    @code{NULL}. Any options understood by GTK+ are stripped before return.}
  @begin{short}
    Call this function before using any other GTK+ functions in your GUI
    applications.
  @end{short}
  It will initialize everything needed to operate the toolkit and parses some
  standard command line options.

  Although you are expected to pass the @arg{argc}, @arg{argv} parameters from
  @code{main()} to this function, it is possible to pass @code{NULL} if
  @arg{argv} is not available or commandline handling is not required.

  @arg{argc} and @arg{argv} are adjusted accordingly so your own code will never
  see those standard arguments.

  Note that there are some alternative ways to initialize GTK+: if you are
  calling @code{gtk_parse_args()}, @fun{gtk-init-check},
  @code{gtk_init_with_args()} or @code{g_option_context_parse()} with the option
  group returned by @code{gtk_get_option_group()}, you don't have to call
  @sym{gtk-init}.
  @begin[Notes]{dictionary}
    This function will terminate your program if it was unable to initialize the
    windowing system for some reason. If you want your program to fall back to a
    textual interface you want to call @fun{gtk-init-check} instead.

    Since 2.18, GTK+ calls signal @code{(SIGPIPE, SIG_IGN)} during
    initialization, to ignore @code{SIGPIPE} signals, since these are almost
    never wanted in graphical applications. If you do need to handle
    @code{SIGPIPE} for some reason, reset the handler after @fun{gtk-init}, but
    notice that other libraries (e. g. @code{libdbus} or @code{gvfs}) might do
    similar things.
  @end{dictionary}
  @begin[Lisp Implemention]{dictionary}
    In the Lisp implementation @sym{gtk-init} calls the C function
    @code{gtk_init_check ()} which is implemented through @fun{gtk-init-check}.
    Both functions are never called directly. The function @sym{gtk-init} is
    called automatically when loading the library @code{cl-cffi-gtk}.
  @end{dictionary}
  @see-function{gtk-init-check}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-init-check 'function)
 "@version{2012-12-23}
  @argument[argc]{Address of the @code{argc} parameter of your @code{main()}
    function (or 0 if @code{argv} is @code{NULL}). This will be changed if any
    arguments were handled.}
  @argument[argv]{Address of the @code{argv} parameter of @code{main()}, or
    @code{NULL}. Any options understood by GTK+ are stripped before return.}
  @return{TRUE if the windowing system has been successfully initialized, FALSE
    otherwise}
  @begin{short}
    This function does the same work as gtk_init() with only a single change: It
    does not terminate the program if the windowing system can't be initialized.
    Instead it returns FALSE on failure.
  @end{short}

  This way the application can fall back to some other means of communication
  with the user - for example a curses or command line interface.
  @begin[Lisp Implemention]{dictionary}
    In the Lisp implementation @sym{gtk-init-check} is called from the
    function @fun{gtk-init}. Both functions are never called directly. The
    function @fun{gtk-init} is called automatically when loading the library
    @code{cl-cffi-gtk}.
  @end{dictionary}
  @see-function{gtk-init}")

;;; ----------------------------------------------------------------------------
;;; gtk_init_with_args ()
;;;
;;; gboolean gtk_init_with_args (gint *argc,
;;;                              gchar ***argv,
;;;                              const gchar *parameter_string,
;;;                              const GOptionEntry *entries,
;;;                              const gchar *translation_domain,
;;;                              GError **error);
;;;
;;; This function does the same work as gtk_init_check(). Additionally, it
;;; allows you to add your own commandline options, and it automatically
;;; generates nicely formatted --help output. Note that your program will be
;;; terminated after writing out the help output.
;;;
;;; argc :
;;;     Address of the argc parameter of your main() function (or 0 if argv is
;;;     NULL). This will be changed if any arguments were handled.
;;;
;;; argv :
;;;     Address of the argv parameter of main(), or NULL. Any options understood
;;;     by GTK+ are stripped before return.
;;;
;;; parameter_string :
;;;     a string which is displayed in the first line of --help output, after
;;;     programname [OPTION...]
;;;
;;; entries :
;;;     a NULL-terminated array of GOptionEntrys describing the options of your
;;;     program
;;;
;;; translation_domain :
;;;     a translation domain to use for translating the --help output for the
;;;     options in entries and the parameter_string with gettext(), or NULL
;;;
;;; error :
;;;     a return location for errors
;;;
;;; Returns :
;;;     TRUE if the windowing system has been successfully initialized, FALSE
;;;     otherwise
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_option_group ()
;;;
;;; GOptionGroup * gtk_get_option_group (gboolean open_default_display);
;;;
;;; Returns a GOptionGroup for the commandline arguments recognized by GTK+ and
;;; GDK.
;;;
;;; You should add this group to your GOptionContext with
;;; g_option_context_add_group(), if you are using g_option_context_parse() to
;;; parse your commandline arguments.
;;;
;;; open_default_display :
;;;     whether to open the default display when parsing the commandline
;;;     arguments
;;;
;;; Returns :
;;;     a GOptionGroup for the commandline arguments recognized by GTK+
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-events-pending 'function)
 "@version{2012-12-20}
  @return{TRUE if any events are pending, FALSE otherwise.}
  @short{Checks if any events are pending.}

  This can be used to update the UI and invoke timeouts etc. while doing some
  time intensive computation.
  @begin[Example]{dictionary}
    Example 7. Updating the UI during a long computation
    @begin{pre}
 /* computation going on... */

 while (gtk_events_pending ())
        gtk_main_iteration ();

 /* ...computation continued */
    @end{pre}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-main 'function)
 "@version{2012-12-20}
  @short{Runs the main loop until @fun{gtk-main-quit} is called.}

  You can nest calls to @sym{gtk-main}. In that case @fun{gtk-main-quit} will
  make the innermost invocation of the main loop return.
  @see-function{gtk-main-quit}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-main-level 'function)
 "@version{2012-12-20}
  @return{The nesting level of the current invocation of the main loop.}
  @short{Asks for the current nesting level of the main loop.}
  @see-function{gtk-main}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-main-quit 'function)
 "@version{2012-12-20}
  @short{Makes the innermost invocation of the main loop return when it regains
    control.}
  @see-function{gtk-main}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-main-iteration 'function)
 "@version{2012-12-20}
  @return{TRUE if @fun{gtk-main-quit} has been called for the innermost
    mainloop}
  @short{Runs a single iteration of the mainloop.}

  If no events are waiting to be processed GTK+ will block until the next
  event is noticed. If you don't want to block look at
  @fun{gtk-main-iteration-do} or check if any events are pending with
  @fun{gtk-events-pending} first.
  @see-function{gtk-main-iteration-do}
  @see-function{gtk-events-pending}
  @see-function{gtk-main-quit}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-main-iteration-do 'function)
 "@version{2012-12-20}
  @argument[blocking]{TRUE if you want GTK+ to block if no events are pending}
  @return{TRUE if @fun{gtk-main-quit} has been called for the innermost
    mainloop}
  @begin{short}
    Runs a single iteration of the mainloop. If no events are available either
    return or block depending on the value of blocking.
  @end{short}
  @see-function{gtk-main-quit}
  @see-function{gtk-main-iteration}")

;;; ----------------------------------------------------------------------------
;;; gtk_main_do_event ()
;;;
;;; void gtk_main_do_event (GdkEvent *event);
;;;
;;; Processes a single GDK event.
;;;
;;; This is public only to allow filtering of events between GDK and GTK+. You
;;; will not usually need to call this function directly.
;;;
;;; While you should not call this function directly, you might want to know how
;;; exactly events are handled. So here is what this function does with the
;;; event:
;;;
;;;     Compress enter/leave notify events. If the event passed build an
;;;     enter/leave pair together with the next event (peeked from GDK), both
;;;     events are thrown away. This is to avoid a backlog of (de-)highlighting
;;;     widgets crossed by the pointer.
;;;
;;;     Find the widget which got the event. If the widget can't be determined
;;;     the event is thrown away unless it belongs to a INCR transaction. In
;;;     that case it is passed to gtk_selection_incr_event().
;;;
;;;     Then the event is pushed onto a stack so you can query the currently
;;;     handled event with gtk_get_current_event().
;;;
;;;     The event is sent to a widget. If a grab is active all events for
;;;     widgets that are not in the contained in the grab widget are sent to the
;;;     latter with a few exceptions:
;;;
;;;         Deletion and destruction events are still sent to the event widget
;;;         for obvious reasons.
;;;
;;;         Events which directly relate to the visual representation of the
;;;         event widget.
;;;
;;;         Leave events are delivered to the event widget if there was an enter
;;;         event delivered to it before without the paired leave event.
;;;
;;;         Drag events are not redirected because it is unclear what the
;;;         semantics of that would be.
;;;
;;;     Another point of interest might be that all key events are first passed
;;;     through the key snooper functions if there are any. Read the description
;;;     of gtk_key_snooper_install() if you need this feature.
;;;
;;;     After finishing the delivery the event is popped from the event stack.
;;;
;;; event :
;;;     An event to process (normally passed by GDK)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkModuleInitFunc ()
;;;
;;; void (*GtkModuleInitFunc) (gint *argc, gchar ***argv);
;;;
;;; Each GTK+ module must have a function gtk_module_init() with this prototype.
;;; This function is called after loading the module.
;;;
;;; argc :
;;;     GTK+ always passes NULL for this argument. [allow-none]
;;;
;;; argv :
;;;     GTK+ always passes NULL for this argument
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkModuleDisplayInitFunc ()
;;;
;;; void (*GtkModuleDisplayInitFunc) (GdkDisplay *display);
;;;
;;; A multihead-aware GTK+ module may have a gtk_module_display_init() function
;;; with this prototype. GTK+ calls this function for each opened display.
;;;
;;; display :
;;;     an open GdkDisplay
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_true ()
;;;
;;; gboolean gtk_true (void);
;;;
;;; All this function does it to return TRUE.
;;;
;;; This can be useful for example if you want to inhibit the deletion of a
;;; window. Of course you should not do this as the user expects a reaction from
;;; clicking the close icon of the window...
;;;
;;; Example 8. A persistent window
;;;
;;;   #include <gtk/gtk.h><
;;;
;;;   int
;;;   main (int argc, char **argv)
;;;   {
;;;     GtkWidget *win, *but;
;;;
;;;     gtk_init (&argc, &argv);
;;;
;;;     win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;     g_signal_connect (win, "delete-event",
;;;                       G_CALLBACK (gtk_true), NULL);
;;;     g_signal_connect (win, "destroy",
;;;                       G_CALLBACK (gtk_main_quit), NULL);
;;;
;;;     but = gtk_button_new_with_label ("Close yourself. I mean it!");
;;;     g_signal_connect_swapped (but, "clicked",
;;;                               G_CALLBACK (gtk_object_destroy), win);
;;;     gtk_container_add (GTK_CONTAINER (win), but);
;;;
;;;     gtk_widget_show_all (win);
;;;
;;;     gtk_main ();
;;;
;;;     return 0;
;;;   }
;;;
;;;
;;;
;;; Returns :
;;;     TRUE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_false ()
;;;
;;; gboolean gtk_false (void);
;;;
;;; Analogical to gtk_true(), this function does nothing but always returns
;;; FALSE.
;;;
;;; Returns :
;;;     FALSE
;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-grab-add 'function)
 "@version{2012-12-20}
  @argument[widget]{The widget that grabs keyboard and pointer events.}
  @short{Makes widget the current grabbed widget.}

  This means that interaction with other widgets in the same application is
  blocked and mouse as well as keyboard events are delivered to this widget.

  If @arg{widget} is not sensitive, it is not set as the current grabbed widget
  and this function does nothing.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-grab-get-current 'function)
 "@version{2012-12-20}
  @return{The widget which currently has the grab or @code{NULL} if no grab is
    active.}
  @short{Queries the current grab of the default window group.}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-grab-remove 'function)
 "@version{2012-12-20}
  @argument[widget]{The widget which gives up the grab.}
  @short{Removes the grab from the given @arg{widget}.}

  You have to pair calls to @fun{gtk-grab-add} and @sym{gtk-grab-remove}.

  If @arg{widget} does not have the grab, this function does nothing.
  @see-function{gtk-grab-add}")

;;; ----------------------------------------------------------------------------
;;; gtk_device_grab_add ()
;;;
;;; void gtk_device_grab_add (GtkWidget *widget,
;;;                           GdkDevice *device,
;;;                           gboolean block_others);
;;;
;;; Adds a GTK+ grab on device, so all the events on device and its associated
;;; pointer or keyboard (if any) are delivered to widget. If the block_others
;;; parameter is TRUE, any other devices will be unable to interact with widget
;;; during the grab.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; device :
;;;     a GtkDevice to grab on.
;;;
;;; block_others :
;;;     TRUE to prevent other devices to interact with widget.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_device_grab_remove ()
;;;
;;; void gtk_device_grab_remove (GtkWidget *widget, GdkDevice *device);
;;;
;;; Removes a device grab from the given widget.
;;;
;;; You have to pair calls to gtk_device_grab_add() and
;;; gtk_device_grab_remove().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PRIORITY_RESIZE
;;;
;;; #define GTK_PRIORITY_RESIZE (G_PRIORITY_HIGH_IDLE + 10)
;;;
;;; Use this priority for functionality related to size allocation.
;;;
;;; It is used internally by GTK+ to compute the sizes of widgets. This priority
;;; is higher than GDK_PRIORITY_REDRAW to avoid resizing a widget which was just
;;; redrawn.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_key_snooper_install ()
;;;
;;; guint gtk_key_snooper_install (GtkKeySnoopFunc snooper, gpointer func_data);
;;;
;;; Warning
;;;
;;; gtk_key_snooper_install has been deprecated since version 3.4 and should not
;;; be used in newly-written code. Key snooping should not be done. Events
;;; should be handled by widgets.
;;;
;;; Installs a key snooper function, which will get called on all key events
;;; before delivering them normally.
;;;
;;; snooper :
;;;     a GtkKeySnoopFunc
;;;
;;; func_data :
;;;     data to pass to snooper
;;;
;;; Returns :
;;;     a unique id for this key snooper for use with gtk_key_snooper_remove().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkKeySnoopFunc ()
;;;
;;; gint (*GtkKeySnoopFunc) (GtkWidget *grab_widget,
;;;                          GdkEventKey *event,
;;;                          gpointer func_data);
;;;
;;; Key snooper functions are called before normal event delivery. They can be
;;; used to implement custom key event handling.
;;;
;;; grab_widget :
;;;     the widget to which the event will be delivered
;;;
;;; event :
;;;     the key event
;;;
;;; func_data :
;;;     data supplied to gtk_key_snooper_install()
;;;
;;; Returns :
;;;     TRUE to stop further processing of event, FALSE to continue.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_key_snooper_remove ()
;;;
;;; void gtk_key_snooper_remove (guint snooper_handler_id);
;;;
;;; Warning
;;;
;;; gtk_key_snooper_remove has been deprecated since version 3.4 and should not
;;; be used in newly-written code. Key snooping should not be done. Events
;;; should be handled by widgets.
;;;
;;; Removes the key snooper function with the given id.
;;;
;;; snooper_handler_id :
;;;     Identifies the key snooper to remove
;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-get-current-event 'function)
 "@version{2012-12-20}
  @return{a copy of the current event, or @code{NULL} if there is no current
    event. The returned event must be freed with @fun{gdk-event-free}.}
  @short{Obtains a copy of the event currently being processed by GTK+.}

  For example, if you are handling a \"clicked\" signal, the current event will
  be the @code{GdkEventButton} that triggered the ::clicked signal.")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-get-current-event-time 'function)
 "@version{2012-12-20}
  @return{The timestamp from the current event, or @code{GDK_CURRENT_TIME}.}
  @begin{short}
    If there is a current event and it has a timestamp, return that timestamp,
    otherwise return @code{GDK_CURRENT_TIME}.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_state ()
;;;
;;; gboolean gtk_get_current_event_state (GdkModifierType *state);
;;;
;;; If there is a current event and it has a state field, place that state field
;;; in state and return TRUE, otherwise return FALSE.
;;;
;;; state :
;;;     a location to store the state of the current event. [out]
;;;
;;; Returns :
;;;     TRUE if there was a current event and it had a state field
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_device ()
;;;
;;; GdkDevice * gtk_get_current_event_device (void);
;;;
;;; If there is a current event and it has a device, return that device,
;;; otherwise return NULL.
;;;
;;; Returns :
;;;     a GdkDevice, or NULL
;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-get-event-widget 'function)
 "@version{2012-12-20}
  @argument[event]{a GdkEvent}
  @return{The widget that originally received event, or @code{NULL}.}
  @begin{short}
    If @arg{event} is @code{NULL} or the @arg{event} was not associated with any
    widget, returns @code{NULL}, otherwise returns the widget that received the
    event originally.
  @end{short}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-propagate-event 'function)
 "@version{2012-12-22}
  @argument[widget]{a @class{gtk-widget}}
  @argument[event]{an event}
  @begin{short}
    Sends an event to a widget, propagating the event to parent widgets if the
    event remains unhandled.
  @end{short}

  Events received by GTK+ from GDK normally begin in @code{gtk_main_do_event()}.
  Depending on the type of event, existence of modal dialogs, grabs, etc., the
  event may be propagated; if so, this function is used.

  @sym{gtk-propagate-event} calls @fun{gtk-widget-event} on each widget it
  decides to send the event to. So @fun{gtk-widget-event} is the lowest-level
  function; it simply emits the \"event\" and possibly an event-specific signal
  on a widget. @sym{gtk-propagate-event} is a bit higher-level, and
  @code{gtk_main_do_event()} is the highest level.

  All that said, you most likely don't want to use any of these functions;
  synthesizing events is rarely needed. There are almost certainly better ways
  to achieve your goals. For example, use @fun{gdk-window-invalidate-rect} or
  @fun{gtk-widget-queue-draw} instead of making up expose events.
  @see-function{gtk-widget-event}
  @see-function{gdk-window-invalidate-rect}
  @see-function{gtk-widget-queue-draw}")

;;; --- End of file atdoc-gtk.main.loop.lisp -----------------------------------
