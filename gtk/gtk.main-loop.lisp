;;; ----------------------------------------------------------------------------
;;; gtk.main-loop.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Main loop and Events
;;;
;;; Library initialization, main event loop, and events
;;;
;;; Synopsis
;;;
;;;     gtk_disable_setlocale                    * not exported *
;;;     gtk_get_default_language
;;;     gtk_parse_args                           * not implemented *
;;;     gtk_init                                 * not exported *
;;;     gtk_init_check                           * not exported *
;;;     gtk_init_with_args                       * not implemented *
;;;     gtk_get_option_group                     * not implemented *
;;;     gtk_events_pending
;;;     gtk_main
;;;     gtk_main_level
;;;     gtk_main_quit
;;;     gtk_main_iteration
;;;     gtk_main_iteration_do
;;;     gtk_main_do_event
;;;     gtk_true                                 * not implemented *
;;;     gtk_false                                * not implemented *
;;;     gtk_grab_add
;;;     gtk_grab_get_current
;;;     gtk_grab_remove
;;;     gtk_device_grab_add
;;;     gtk_device_grab_remove
;;;
;;;     GTK_PRIORITY_RESIZE
;;;
;;;     gtk_key_snooper_install                  * deprecated *
;;;     gtk_key_snooper_remove                   * deprecated *
;;;
;;;     gtk_get_current_event
;;;     gtk_get_current_event_time
;;;     gtk_get_current_event_state
;;;     gtk_get_current_event_device
;;;     gtk_get_event_widget
;;;     gtk_propagate_event
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; gtk_disable_setlocale ()
;;; ----------------------------------------------------------------------------

;; TODO: Because GTK+ is initialized, when loading the Lisp library, this
;; function should have no effect. We do not export the implementation.

(defcfun ("gtk_disable_setlocale" %gtk-disable-setlocale) :void
 #+cl-cffi-gtk-documentation
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

;(export 'gtk-disable-setlocale)

;;; ----------------------------------------------------------------------------
;;; gtk_get_default_language ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_default_language" gtk-get-default-language)
    (g-boxed-foreign pango-language)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-9}
  @return{The default language as a @class{pango-language} object.}
  @begin{short}
    Returns the @class{pango-language} object for the default language currently
    in effect. The default language is derived from the current locale.
  @end{short}
  Note that this can change over the life of an application. It determines, for
  example, whether GTK+ uses the right-to-left or left-to-right text direction.

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

(export 'gtk-get-default-language)

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

;;; ----------------------------------------------------------------------------
;;; gtk_init ()
;;; ----------------------------------------------------------------------------

;;; TODO: The function is for internal use and not exported.
;;; Rework the handling of command line parameters in this function and
;;; the function %gtk-init-check.

(defun %gtk-init ()
 #+cl-cffi-gtk-documentation
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
    notice that other libraries (e.g. @code{libdbus} or @code{gvfs}) might do
    similar things.
  @end{dictionary}
  @begin[Lisp Implemention]{dictionary}
    In the Lisp implementation @sym{gtk-init} calls the C function
    @code{gtk_init_check()} which is implemented through @fun{gtk-init-check}.
    Both functions are never called directly. The function @sym{gtk-init} is
    called automatically when loading the library @code{cl-cffi-gtk}.
  @end{dictionary}
  @see-function{gtk-init-check}"
  (%gtk-init-check (foreign-alloc :int :initial-element 0)
                   (foreign-alloc :string :initial-contents '("/usr/bin/sbcl")))
  #+ (and sbcl (not win32))
  (sb-unix::enable-interrupt sb-unix:sigpipe #'sb-unix::sigpipe-handler)
  #+nil(with-foreign-objects ((argc :int)
                         (argv '(:pointer :string) 1))
    (setf (mem-ref argc :int) 0
          (mem-ref argv '(:pointer :string))
          (foreign-alloc :string :count 1 :initial-element "/usr/bin/sbcl"))
    (unwind-protect
         (unless (%gtk-init-check argc argv)
           (error "Cannot initialize Gtk+"))
      (foreign-free (mem-ref argv '(:pointer :string))))))

;;; ----------------------------------------------------------------------------
;;; gtk_init_check ()
;;; ----------------------------------------------------------------------------

;;; TODO: The function is for internal use and not exported.
;;; Rework the handling of command line parameters in this function and
;;; the function %gtk-init.

(defcfun ("gtk_init_check" %gtk-init-check) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2012-12-23}
  @argument[argc]{Address of the @code{argc} parameter of your @code{main()}
    function (or 0 if @code{argv} is @code{NULL}). This will be changed if any
    arguments were handled.}
  @argument[argv]{Address of the @arg{argv} parameter of @code{main()}, or
    @code{nil}. Any options understood by GTK+ are stripped before return.}
  @return{@em{true} if the windowing system has been successfully initialized,
    @code{nil} otherwise}
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
  @see-function{gtk-init}"
  (argc (:pointer :int))
  (argv (:pointer (:pointer :string))))

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

;; TODO: Implement GOptionGroup

;;; ----------------------------------------------------------------------------
;;; gtk_events_pending ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_events_pending" gtk-events-pending) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-9}
  @return{@em{True} if any events are pending, @code{nil} otherwise.}
  @short{Checks if any events are pending.}

  This can be used to update the UI and invoke timeouts etc. while doing some
  time intensive computation.
  @begin[Example]{dictionary}
    Updating the UI during a long computation.
    @begin{pre}
 ;; computation going on ...

 (loop while (gtk-events-pending)
       do (gtk-main-iteration))

 ;; ... computation continued
  @end{pre}
  @end{dictionary}")

(export 'gtk-events-pending)

;;; ----------------------------------------------------------------------------
;;; gtk_main ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_main" %gtk-main) :void)

(defun gtk-main ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-9}
  @short{Runs the main loop until @fun{gtk-main-quit} is called.}

  You can nest calls to @sym{gtk-main}. In that case @fun{gtk-main-quit} will
  make the innermost invocation of the main loop return.
  @begin[Lisp Implementation]{dictionary}
    In the Lisp binding to GTK+ @sym{gtk-main} is not called directly but
    through the @fun{within-main-loop} macro. The @fun{within-main-loop} macro
    does some additional bookkeeping, which is necessary to run a Lisp program.
  @end{dictionary}
  @see-function{within-main-loop}
  @see-function{gtk-main-quit}"
  (with-gdk-threads-lock (%gtk-main)))

(export 'gtk-main)

;;; ----------------------------------------------------------------------------
;;; gtk_main_level ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_main_level" gtk-main-level) :uint
 #+cl-cffi-gtk-documentation
 "@version{2012-12-20}
  @return{The nesting level of the current invocation of the main loop.}
  Asks for the current nesting level of the main loop.
  @see-function{gtk-main}")

(export 'gtk-main-level)

;;; ----------------------------------------------------------------------------
;;; gtk_main_quit ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_main_quit" gtk-main-quit) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-9}
  @begin{short}
    Makes the innermost invocation of the main loop return when it regains
    control.
  @end{short}
  @begin[Lisp Implementation]{dictionary}
    In the Lisp binding to GTK+ @sym{gtk-main} is not called, but the
    @fun{leave-gtk-main} function. The @fun{leave-gtk-main} function does some
    additional bookkeeping, which is necessary to stop a Lisp program safely.
  @end{dictionary}
  @see-function{leave-gtk-main}
  @see-function{gtk-main}")

(export 'gtk-main-quit)

;;; ----------------------------------------------------------------------------
;;; gtk_main_iteration ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_main_iteration" gtk-main-iteration) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2012-12-20}
  @return{@em{True} if @fun{gtk-main-quit} has been called for the innermost
    main loop.}
  @short{Runs a single iteration of the main loop.}

  If no events are waiting to be processed GTK+ will block until the next
  event is noticed. If you don't want to block look at
  @fun{gtk-main-iteration-do} or check if any events are pending with
  @fun{gtk-events-pending} first.
  @see-function{gtk-main-iteration-do}
  @see-function{gtk-events-pending}
  @see-function{gtk-main-quit}")

(export 'gtk-main-iteration)

;;; ----------------------------------------------------------------------------
;;; gtk_main_iteration_do ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_main_iteration_do" gtk-main-iteration-do) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2012-12-20}
  @argument[blocking]{@em{True} if you want GTK+ to block if no events are
    pending.}
  @return{@arg{True} if @fun{gtk-main-quit} has been called for the innermost
    main loop.}
  Runs a single iteration of the main loop. If no events are available either
  return or block depending on the value of @arg{blocking}.
  @see-function{gtk-main-iteration}
  @see-function{gtk-main-quit}"
  (blocking :boolean))

(export 'gtk-main-iteration-do)

;;; ----------------------------------------------------------------------------
;;; gtk_main_do_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_main_do_event" gtk-main-do-event) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-9}
  @argument[event]{An event to process (normally passed by GDK).}
  @short{Processes a single GDK event.}

  This is public only to allow filtering of events between GDK and GTK+. You
  will not usually need to call this function directly.

  While you should not call this function directly, you might want to know how
  exactly events are handled. So here is what this function does with the
  event:
  @begin{itemize}
    @begin{item}
      Compress enter/leave notify events. If the event passed build an
      enter/leave pair together with the next event (peeked from GDK), both
      events are thrown away. This is to avoid a backlog of (de-)highlighting
      widgets crossed by the pointer.
    @end{item}
    @begin{item}
      Find the widget which got the event. If the widget can't be determined
      the event is thrown away unless it belongs to a @code{INCR} transaction.
      In that case it is passed to @code{gtk_selection_incr_event()}.
    @end{item}
    @begin{item}
      Then the event is pushed onto a stack so you can query the currently
      handled event with @fun{gtk-get-current-event}.
    @end{item}
    @begin{item}
      The event is sent to a widget. If a grab is active all events for
      widgets that are not in the contained in the grab widget are sent to the
      latter with a few exceptions:
      @begin{itemize}
        @begin{item}
          Deletion and destruction events are still sent to the event widget
          for obvious reasons.
        @end{item}
        @begin{item}
          Events which directly relate to the visual representation of the
          event widget.
        @end{item}
        @begin{item}
          Leave events are delivered to the event widget if there was an enter
          event delivered to it before without the paired leave event.
        @end{item}
        @begin{item}
          Drag events are not redirected because it is unclear what the
          semantics of that would be.
        @end{item}
      @end{itemize}
    @end{item}
    @begin{item}
      Another point of interest might be that all key events are first passed
      through the key snooper functions if there are any. Read the description
      of @code{gtk_key_snooper_install()} if you need this feature.
    @end{item}
    @begin{item}
      After finishing the delivery the event is popped from the event stack.
    @end{item}
  @end{itemize}
  @see-function{gtk-get-current-event}"
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-main-do-event)

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

;;; ----------------------------------------------------------------------------
;;; gtk_grab_add ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grab_add" gtk-grab-add) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-11}
  @argument[widget]{the widget that grabs keyboard and pointer events}
  @short{Makes @arg{widget} the current grabbed widget.}
  This means that interaction with other widgets in the same application is
  blocked and mouse as well as keyboard events are delivered to this
  @arg{widget}.

  If @arg{widget} is not sensitive, it is not set as the current grabbed widget
  and this function does nothing."
  (widget (g-object gtk-widget)))

(export 'gtk-grab-add)

;;; ----------------------------------------------------------------------------
;;; gtk_grab_get_current ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grab_get_current" gtk-grab-get-current) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-11}
  @return{The widget which currently has the grab or @code{nil} if no grab is
    active.}
  Queries the current grab of the default window group.")

(export 'gtk-grab-get-current)

;;; ----------------------------------------------------------------------------
;;; gtk_grab_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_grab_remove" gtk-grab-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2012-3-11}
  @argument[widget]{the widget which gives up the grab}
  @short{Removes the grab from the given @arg{widget}.}
  You have to pair calls to @fun{gtk-grab-add} and @sym{gtk-grab-remove}.

  If @arg{widget} does not have the grab, this function does nothing.
  @see-function{gtk-grab-add}"
  (widget g-object))

(export 'gtk-grab-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_device_grab_add ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_device_grab_add" gtk-device-grab-add) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-5}
  @argument[widget]{a @class{gtk-widget}}
  @argument[device]{a @class{gdk-device} to grab on}
  @argument[block-others]{@em{true} to prevent other devices to interact with
    @arg{widget}}
  @begin{short}
    Adds a grab on @arg{device}, so all the events on @arg{device} and its
    associated pointer or keyboard (if any) are delivered to @arg{widget}.
  @end{short}
  If the @arg{block-others} parameter is @em{true}, any other devices will be
  unable to interact with widget during the grab.

  Since 3.0
  @see-function{gtk-device-grab-remove}"
  (widget (g-object gtk-widget))
  (device (g-object gdk-device))
  (block-others :boolean))

(export 'gtk-device-grab-add)

;;; ----------------------------------------------------------------------------
;;; gtk_device_grab_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_device_grab_remove" gtk-device-grab-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-11}
  @argument[widget]{a @class{gtk-widget}}
  @argument[device]{a @class{gdk-device}}
  @begin{short}
    Removes a device grab from the given @arg{widget}.
  @end{short}
  You have to pair calls to @fun{gtk-device-grab-add} and
  @sym{gtk-device-grab-remove}.

  Since 3.0
  @see-function{gtk-device-grab-add}"
  (widget (g-object gtk-widget))
  (device (g-object gdk-device)))

(export 'gtk-device-grab-remove)

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

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_current_event" gtk-get-current-event)
    (g-boxed-foreign gdk-event :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-11}
  @return{A copy of the current event, or @code{nil} if there is no current
    event.}
  @short{Obtains a copy of the event currently being processed by GTK+.}

  For example, if you are handling a \"clicked\" signal, the current event will
  be the @class{gdk-event-button} that triggered the \"clicked\" signal.
  @see-function{gtk-get-current-event-time}
  @see-function{gtk-get-current-event-state}
  @see-function{gtk-get-current-event-device}")

(export 'gtk-get-current-event)

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_time ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_current_event_time" gtk-get-current-event-time) :uint32
 #+cl-cffi-gtk-documentation
 "@version{2012-3-11}
  @return{The timestamp from the current event, or @var{+gdk-current-time+}.}
  If there is a current event and it has a timestamp, return that timestamp,
  otherwise return @var{+gdk-current-time+}.
  @see-function{gtk-get-current-event}
  @see-variable{+gdk-current-time+}")

(export 'gtk-get-current-event-time)

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_current_event_state" %gtk-get-current-event-state) :boolean
  (state (:pointer gdk-modifier-type)))

(defun gtk-get-current-event-state ()
 #+cl-cffi-gtk-documentation
 "@version{2013-3-11}
  @begin{return}
    The state of the current event or @code{nil} if there is no current event.
  @end{return}
  If there is a current event and it has a state field, return that state
  field, otherwise return @code{nil}.
  @see-function{gtk-get-current-event}"
  (with-foreign-object (state 'gdk-modifier-type)
    (when (%gtk-get-current-event-state state)
      (mem-ref state 'gdk-modifier-type))))

(export 'gtk-get-current-event-state)

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_current_event_device" gtk-get-current-event-device)
    (g-object gdk-device)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-11}
  @return{A @class{gdk-device}, or @code{nil}.}
  If there is a current event and it has a device, return that device,
  otherwise return @code{nil}.
  @see-function{gtk-get-current-event}")

(export 'gtk-get-current-event-device)

;;; ----------------------------------------------------------------------------
;;; gtk_get_event_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_event_widget" gtk-get-event-widget) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-3-11}
  @argument[event]{a @class{gdk-event}}
  @return{The widget that originally received @arg{event}, or @code{nil}.}
  If @arg{event} is @code{nil} or the @arg{event} was not associated with any
  widget, returns @code{nil}, otherwise returns the widget that received the
  @arg{event} originally."
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-get-event-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_propagate_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_propagate_event" gtk-propagate-event) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-11}
  @argument[widget]{a @class{gtk-widget}}
  @argument[event]{a @class{gdk-event}}
  @begin{short}
    Sends an event to a widget, propagating the event to parent widgets if the
    event remains unhandled.
  @end{short}

  Events received by GTK+ from GDK normally begin in @fun{gtk-main-do-event}.
  Depending on the type of event, existence of modal dialogs, grabs, etc.,
  the event may be propagated; if so, this function is used.

  @sym{gtk-propagate-event} calls @fun{gtk-widget-event} on each widget it
  decides to send the event to. So @fun{gtk-widget-event} is the lowest-level
  function; it simply emits the \"event\" and possibly an event-specific signal
  on a widget. @sym{gtk-propagate-event} is a bit higher-level, and
  @fun{gtk-main-do-event} is the highest level.

  All that said, you most likely don't want to use any of these functions;
  synthesizing events is rarely needed. There are almost certainly better ways
  to achieve your goals. For example, use @fun{gdk-window-invalidate-rect} or
  @fun{gtk-widget-queue-draw} instead of making up expose events.
  @see-function{gtk-widget-event}
  @see-function{gtk-main-do-event}
  @see-function{gdk-window-invalidate-rect}
  @see-function{gtk-widget-queue-draw}"
  (widget g-object)
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-propagate-event)

;;; --- End of file gtk.main.loop.lisp -----------------------------------------
