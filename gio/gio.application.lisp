;;; ----------------------------------------------------------------------------
;;; gio.application.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.36.4 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; GApplication
;;;
;;; Core application class
;;;
;;; Synopsis
;;;
;;;     GApplication
;;;     GApplicationClass
;;;     GApplicationFlags
;;;
;;;     g_application_id_is_valid
;;;     g_application_new
;;;     g_application_get_application_id
;;;     g_application_set_application_id
;;;     g_application_get_inactivity_timeout
;;;     g_application_set_inactivity_timeout
;;;     g_application_get_flags
;;;     g_application_set_flags
;;;     g_application_get_dbus_connection
;;;     g_application_get_dbus_object_path
;;;     g_application_set_action_group                     * deprecated *
;;;     g_application_get_is_registered
;;;     g_application_get_is_remote
;;;     g_application_register
;;;     g_application_hold
;;;     g_application_release
;;;     g_application_quit
;;;     g_application_activate
;;;     g_application_open
;;;     g_application_run
;;;     g_application_set_default
;;;     g_application_get_default
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GApplication
;;;
;;; Implemented Interfaces
;;;
;;; GApplication implements GActionGroup and GActionMap.
;;;
;;; Properties
;;;
;;;   "action-group"             GActionGroup*        : Write
;;;   "application-id"           gchar*               : Read / Write / Construct
;;;   "flags"                    GApplicationFlags    : Read / Write
;;;   "inactivity-timeout"       guint                : Read / Write
;;;   "is-registered"            gboolean             : Read
;;;   "is-remote"                gboolean             : Read
;;;
;;; Signals
;;;
;;;   "activate"                                      : Run Last
;;;   "command-line"                                  : Run Last
;;;   "open"                                          : Run Last
;;;   "shutdown"                                      : Run Last
;;;   "startup"                                       : Run First
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GApplication
;;; ----------------------------------------------------------------------------

(define-g-object-class "GApplication" g-application
  (:superclass g-object
   :export t
   :interfaces ("GActionGroup"
                "GActionMap")
   :type-initializer "g_application_get_type")
  ((action-group
    g-application-action-group
    "action-group" "GActionGroup" nil t)
   (application-id
    g-application-application-id
    "application-id" "gchar" t t)
   (flags
    g-application-flags
    "flags" "GApplicationFlags" t t)
   (inactivity-timeout
    g-application-inactivity-timeout
    "inactivity-timeout" "guint" t t)
   (is-registered
    g-application-is-registered
    "is-registered" "gboolean" t nil)
   (is-remote
    g-application-is-remote
    "is-remote" "gboolean" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-application 'type)
 "@version{2013-7-27}
  @begin{short}
    A @sym{g-application} is the foundation of an application, unique for a
    given application identifier. The @sym{g-application} class wraps some
    low-level platform-specific services and is intended to act as the
    foundation for higher-level application classes such as
    @class{gtk-application} or @code{MxApplication}. In general, you should not
    use this class outside of a higher level framework.
  @end{short}

  One of the core features that @sym{g-application} provides is process
  uniqueness, in the context of a \"session\". The session concept is
  platform-dependent, but corresponds roughly to a graphical desktop login. When
  your application is launched again, its arguments are passed through platform
  communication to the already running program. The already running instance of
  the program is called the primary instance. On Linux, the D-Bus session bus is
  used for communication.

  @sym{g-application} provides convenient life cycle management by maintaining a
  use count for the primary application instance. The use count can be changed
  using the functions @fun{g-application-hold} and @fun{g-application-release}.
  If it drops to zero, the application exits. Higher-level classes such as
  @class{gtk-application} employ the use count to ensure that the application
  stays alive as long as it has any opened windows.

  Before using @sym{g-application}, you must choose an
  \"application identifier\". The expected form of an application identifier is
  very close to that of of a DBus bus name. Examples include:
  \"com.example.MyApp\", \"org.example.internal-apps.Calculator\". For details
  on valid application identifiers, see the function
  @fun{g-application-id-is-valid}.

  On Linux, the application identifier is claimed as a well-known bus name on
  the user's session bus. This means that the uniqueness of your application
  is scoped to the current session. It also means that your application may
  provide additional services through registration of other object paths at
  that bus name. The registration of these object paths should be done with
  the shared GDBus session bus. Note that due to the internal architecture of
  GDBus, method calls can be dispatched at any time, even if a main loop is
  not running. For this reason, you must ensure that any object paths that
  you wish to register are registered before @sym{g-application} attempts to
  acquire the bus name of your application, which happens in the function
  @fun{g-application-register}. Unfortunately, this means that you cannot use
  the function @fun{g-application-get-is-remote} to decide if you want to
  register object paths.

  @sym{g-application} also implements the @class{g-action-group} and
  @class{g-action-map} interfaces and lets you easily export actions by adding
  them with the function @fun{g-action-map-add-action}. When invoking an action
  by calling the function @fun{g-action-group-activate-action} on the
  application, it is always invoked in the primary instance. The actions are
  also exported on the session bus, and GIO provides the @code{GDBusActionGroup}
  wrapper to conveniently access them remotely. GIO provides a
  @code{GDBusMenuModel} wrapper for remote access to exported
  @code{GMenuModel}'s.

  There is a number of different entry points into a @sym{g-application}:
  @begin{itemize}
    @item{via 'Activate' (i. e. just starting the application)}
    @item{via 'Open' (i. e. opening some files)}
    @item{by handling a command-line}
    @item{via activating an action}
  @end{itemize}
  The \"startup\" signal lets you handle the application initialization for all
  of these in a single place.

  Regardless of which of these entry points is used to start the application,
  @sym{g-application} passes some platform data from the launching instance to
  the primary instance, in the form of a @type{g-variant} dictionary mapping
  strings to variants. To use platform data, override the @code{before_emit} or
  @code{after_emit} virtual functions in your @sym{g-application} subclass. When
  dealing with @code{GApplicationCommandLine} objects, the platform data is
  directly available via @code{g_application_command_line_get_cwd()},
  @code{g_application_command_line_get_environ()} and
  @code{g_application_command_line_get_platform_data()}.

  As the name indicates, the platform data may vary depending on the operating
  system, but it always includes the current directory (key \"cwd\"), and
  optionally the environment (i. e. the set of environment variables and their
  values) of the calling process (key \"environ\"). The environment is only
  added to the platform data if the @code{:send-enviroment} flag is set.
  @sym{g-application} subclasses can add their own platform data by overriding
  the @code{add_platform_data} virtual function. For instance,
  @class{gtk-application} adds startup notification data in this way.

  To parse commandline arguments you may handle the \"command-line\" signal or
  override the @code{local_command_line()} vfunc, to parse them in either the
  primary instance or the local instance, respectively.

  @b{Example:} Opening files with a @sym{g-application}
  @begin{pre}
 #include <gio/gio.h>
 #include <stdlib.h>
 #include <string.h>

 static void
 activate (GApplication *application)
 {
   g_print (\"activated\n\");

   /* Note: when doing a longer-lasting action here that returns
    * to the mainloop, you should use g_application_hold() and
    * g_application_release() to keep the application alive until
    * the action is completed.
    */
 @}

 static void
 open (GApplication  *application,
       GFile        **files,
       gint           n_files,
       const gchar   *hint)
 {
   gint i;

   for (i = 0; i < n_files; i++)
     {
       gchar *uri = g_file_get_uri (files[i]);
       g_print (\"open %s\n\", uri);
       g_free (uri);
     @}

   /* Note: when doing a longer-lasting action here that returns
    * to the mainloop, you should use g_application_hold() and
    * g_application_release() to keep the application alive until
    * the action is completed.
    */
 @}

 int
 main (int argc, char **argv)
 {
   GApplication *app;
   int status;

   app = g_application_new (\"org.gtk.TestApplication\",
                            G_APPLICATION_HANDLES_OPEN);
   g_signal_connect (app, \"activate\", G_CALLBACK (activate), NULL);
   g_signal_connect (app, \"open\", G_CALLBACK (open), NULL);
   g_application_set_inactivity_timeout (app, 10000);

   status = g_application_run (app, argc, argv);

   g_object_unref (app);

   return status;
 @}
  @end{pre}
  @b{Example:} A @sym{g-application} with actions
  @begin{pre}
 #include <gio/gio.h>
 #include <stdlib.h>
 #include <string.h>

 static void
 activate (GApplication *application)
 {
   g_application_hold (application);
   g_print (\"activated\n\");
   g_application_release (application);
 @}

 static void
 activate_action (GAction  *action,
                  GVariant *parameter,
                  gpointer  data)
 {
   GApplication *application = G_APPLICATION (data);

   g_application_hold (application);
   g_print (\"action %s activated\n\", g_action_get_name (action));
   g_application_release (application);
 @}

 static void
 activate_toggle_action (GSimpleAction *action,
                         GVariant      *parameter,
                         gpointer       data)
 {
   GApplication *application = G_APPLICATION (data);
   GVariant *state;
   gboolean b;

   g_print (\"action %s activated\n\", g_action_get_name (G_ACTION (action)));

   g_application_hold (application);
   state = g_action_get_state (G_ACTION (action));
   b = g_variant_get_boolean (state);
   g_variant_unref (state);
   g_simple_action_set_state (action, g_variant_new_boolean (!b));
   g_print (\"state change %d -> %d\n\", b, !b);
   g_application_release (application);
 @}

 static void
 add_actions (GApplication *app)
 {
   GSimpleAction *action;

   action = g_simple_action_new (\"simple-action\", NULL);
   g_signal_connect (action, \"activate\", G_CALLBACK (activate_action), app);
   g_action_map_add_action (G_ACTION_MAP (app), G_ACTION (action));
   g_object_unref (action);

   action = g_simple_action_new_stateful (\"toggle-action\", NULL,
                                          g_variant_new_boolean (FALSE));
   g_signal_connect (action, \"activate\",
                     G_CALLBACK (activate_toggle_action), app);
   g_action_map_add_action (G_ACTION_MAP (app), G_ACTION (action));
   g_object_unref (action);
 @}

 static void
 describe_and_activate_action (GActionGroup *group,
                               const gchar  *name)
 {
   const GVariantType *param_type;
   GVariant *state;
   gboolean enabled;
   gchar *tmp;

   param_type = g_action_group_get_action_parameter_type (group, name);
   state = g_action_group_get_action_state (group, name);
   enabled = g_action_group_get_action_enabled (group, name);

   g_print (\"action name:      %s\n\", name);
   tmp = param_type ? g_variant_type_dup_string (param_type) : NULL;
   g_print (\"parameter type:   %s\n\", tmp ? tmp : \"<none>\");
   g_free (tmp);
   g_print (\"state type:       %s\n\",
            state ? g_variant_get_type_string (state) : \"<none>\");
   tmp = state ? g_variant_print (state, FALSE) : NULL;
   g_print (\"state:            %s\n\", tmp ? tmp : \"<none>\");
   g_free (tmp);
   g_print (\"enabled:          %s\n\", enabled ? \"true\" : \"false\");

   if (state != NULL)
     g_variant_unref (state);

   g_action_group_activate_action (group, name, NULL);
 @}

 int
 main (int argc, char **argv)
 {
   GApplication *app;
   int status;

   app = g_application_new (\"org.gtk.TestApplication\", 0);
   g_signal_connect (app, \"activate\", G_CALLBACK (activate), NULL);
   g_application_set_inactivity_timeout (app, 10000);

   add_actions (app);

   if (argc > 1 && strcmp (argv[1], \"--simple-action\") == 0)
     {
       g_application_register (app, NULL, NULL);
       describe_and_activate_action (G_ACTION_GROUP (app), \"simple-action\");
       exit (0);
     @}
   else if (argc > 1 && strcmp (argv[1], \"--toggle-action\") == 0)
     {
       g_application_register (app, NULL, NULL);
       describe_and_activate_action (G_ACTION_GROUP (app), \"toggle-action\");
       exit (0);
     @}

   status = g_application_run (app, argc, argv);

   g_object_unref (app);

   return status;
 @}
  @end{pre}
  @b{Example:} A @sym{g-application} with menus
  @begin{pre}
    FIXME: MISSING XINCLUDE CONTENT
  @end{pre}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (application)   : Run Last
      @end{pre}
      The \"activate\" signal is emitted on the primary instance when an
      activation occurs. See the function @fun{g-application-activate}.
      @begin[code]{table}
        @entry[application]{The application.}
      @end{table}
    @subheading{The \"command-line\" signal}
      @begin{pre}
 lambda (application command-line)   : Run Last
      @end{pre}
      The \"command-line\" signal is emitted on the primary instance when a
      commandline is not handled locally. See the function
      @fun{g-application-run} and the @code{GApplicationCommandLine}
      documentation for more information.
      @begin[code]{table}
        @entry[application]{The application.}
        @entry[command-line]{A @code{GApplicationCommandLine} representing the
          passed commandline.}
        @entry[Returns]{An integer that is set as the exit status for the
          calling process. See the function
          @fun{g-application-command-line-set-exit-status}.}
      @end{table}
    @subheading{The \"open\" signal}
      @begin{pre}
 lambda (application files n-files hint)   : Run Last
      @end{pre}
      The \"open\" signal is emitted on the primary instance when there are
      files to open. See the function @fun{g-application-open} for more
      information.
      @begin[code]{table}
        @entry[application]{The application.}
        @entry[files]{An array of @code{GFiles}.}
        @entry[n-files]{The length of files.}
        @entry[hint]{A hint provided by the calling instance.}
      @end{table}
    @subheading{The \"shutdown\" signal}
      @begin{pre}
 lambda (application)   : Run Last
      @end{pre}
      The \"shutdown\" signal is emitted only on the registered primary instance
      immediately after the main loop terminates.
      @begin[code]{table}
        @entry[application]{The application.}
      @end{table}
    @subheading{The \"startup\" signal}
      @begin{pre}
 lambda (application)   : Run First
      @end{pre}
      The \"startup\" signal is emitted on the primary instance immediately
      after registration. See the function @fun{g-application-register}.
      @begin[code]{table}
        @entry[application]{The application.}
      @end{table}
  @end{dictionary}
  Since 2.28
  @see-slot{g-application-action-group}
  @see-slot{g-application-application-id}
  @see-slot{g-application-flags}
  @see-slot{g-application-inactivity-timeout}
  @see-slot{g-application-is-registered}
  @see-slot{g-application-is-remote}
  @see-class{gtk-application}
  @see-class{g-action-group}
  @see-function{g-action-group-activate-action}
  @see-class{g-action-map}
  @see-function{g-action-map-add-action}
  @see-symbol{g-application-flags}
  @see-function{g-application-run}
  @see-function{g-application-open}
  @see-function{g-application-activate}
  @see-function{g-application-register}
  @see-function{g-application-hold}
  @see-function{g-application-release}
  @see-function{g-application-id-is-valid}
  @see-function{g-application-get-is-remote}
  @see-function{g-application-command-line-set-exit-status}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "action-group"
                                               'g-application) 't)
 "The @code{\"action-group\"} property of type @class{g-action-group}
  (Write) @br{}
  The group of actions that the application exports.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "application-id"
                                               'g-application) 't)
 "The @code{\"application-id\"} property of type @code{:string}
  (Read / Write / Construct) @br{}
  The unique identifier for the application. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "flags" 'g-application) 't)
 "The @code{\"flags\"} property of type @symbol{g-application-flags}
  (Read / Write)@br{}
  Flags specifying the behaviour of the application.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inactivity-timeout"
                                               'g-application) 't)
 "The @code{\"inactivity-timeout\"} property of type @code{:uint}
  (Read / Write)@br{}
  Time (ms) to stay alive after becoming idle. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-registered"
                                               'g-application) 't)
 "The @code{\"is-registered\"} property of type @code{:boolean} (Read) @br{}
  If the function @fun{g-application-register} has been called. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-remote" 'g-application) 't)
 "The @code{\"is-remote\"} property of type @code{:boolean} (Read) @br{}
  If this application instance is remote. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-action-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-action-group 'function)
 "@version{2013-5-1}
  Accessor of the slot @code{\"action-group\"} of the @class{g-application}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-application-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-application-id 'function)
 "@version{2013-5-1}
  Accessor of the slot @code{\"application-id\"} of the @class{g-application}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-flags atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-flags 'function)
 "@version{2013-5-1}
  Accessor of the slot @code{\"flags\"} of the @class{g-application}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-inactivity-timeout atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-inactivity-timeout 'function)
 "@version{2013-5-1}
  Accessor of the slot @code{\"inactivity-timeout\"} of the
  @class{g-application} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-is-registered atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-is-registered 'function)
 "@version{2013-5-1}
  Accessor of the slot @code{\"is-registered\"} of the @class{g-application}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-is-remote atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-is-remote 'function)
 "@version{2013-5-1}
  Accessor of the slot @code{\"is-remote\"} of the @class{g-application}
  class.")

;;; ----------------------------------------------------------------------------
;;; struct GApplicationClass
;;;
;;; struct GApplicationClass {
;;;   /* signals */
;;;   void     (* startup)             (GApplication    *application);
;;;
;;;   void     (* activate)            (GApplication    *application);
;;;
;;;   void     (* open)                (GApplication    *application,
;;;                                     GFile          **files,
;;;                                     gint             n_files,
;;;                                     const gchar     *hint);
;;;
;;;   int     (* command_line)         (GApplication    *application,
;;;                                     GApplicationCommandLine *command_line)
;;;
;;;   /* vfuncs */
;;;   gboolean (* local_command_line)  (GApplication    *application,
;;;                                     gchar         ***arguments,
;;;                                     int             *exit_status);
;;;
;;;   void     (* before_emit)         (GApplication    *application,
;;;                                     GVariant        *platform_data);
;;;   void     (* after_emit)          (GApplication    *application,
;;;                                     GVariant        *platform_data);
;;;   void     (* add_platform_data)   (GApplication    *application,
;;;                                     GVariantBuilder *builder);
;;;   void     (* quit_mainloop)       (GApplication    *application);
;;;   void     (* run_mainloop)        (GApplication    *application);
;;;   void     (* shutdown)            (GApplication    *application);
;;; };
;;;
;;; Virtual function table for GApplication.
;;;
;;; startup ()
;;;     invoked on the primary instance immediately after registration
;;;
;;; activate ()
;;;     invoked on the primary instance when an activation occurs
;;;
;;; open ()
;;;     invoked on the primary instance when there are files to open
;;;
;;; command_line ()
;;;     invoked on the primary instance when a command-line is not handled
;;;     locally
;;;
;;; local_command_line ()
;;;     invoked (locally) when the process has been invoked via commandline
;;;     execution (as opposed to, say, D-Bus activation - which is not currently
;;;     supported by GApplication). The virtual function has the chance to
;;;     inspect (and possibly replace) the list of command line arguments.
;;;     See g_application_run() for more information.
;;;
;;; before_emit ()
;;;     invoked on the primary instance before 'activate', 'open',
;;;     'command-line' or any action invocation, gets the 'platform data' from
;;;     the calling instance
;;;
;;; after_emit ()
;;;     invoked on the primary instance after 'activate', 'open', 'command-line'
;;;     or any action invocation, gets the 'platform data' from the calling
;;;     instance
;;;
;;; add_platform_data ()
;;;     invoked (locally) to add 'platform data' to be sent to the primary
;;;     instance when activating, opening or invoking actions
;;;
;;; quit_mainloop ()
;;;     Used to be invoked on the primary instance when the use count of the
;;;     application drops to zero (and after any inactivity timeout, if
;;;     requested). Not used anymore since 2.32
;;;
;;; run_mainloop ()
;;;     Used to be invoked on the primary instance from g_application_run() if
;;;     the use-count is non-zero. Since 2.32, GApplication is iterating the
;;;     main context directly and is not using run_mainloop anymore
;;;
;;; shutdown ()
;;;     invoked only on the registered primary instance immediately after the
;;;     main loop terminates
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GApplicationFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GApplicationFlags" g-application-flags
  (:export t
   :type-initializer "g_application_flags_get_type")
  (:none 0)
  (:is-service 1)
  (:is-launcher 2)
  (:handles-open 4)
  (:handles-command-line 8)
  (:send-enviroment 16)
  (:non-unique 32))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-flags atdoc:*symbol-name-alias*) "Enum"
      (gethash 'g-application-flags atdoc:*external-symbols*)
 "@version{2013-5-1}
  @begin{short}
    Flags used to define the behaviour of a @class{g-application} object.
  @end{short}
  @begin{pre}
(define-g-flags \"GApplicationFlags\" g-application-flags
  (:export t
   :type-initializer \"g_application_flags_get_type\")
  (:none 0)
  (:is-service 1)
  (:is-launcher 2)
  (:handles-open 4)
  (:handles-command-line 8)
  (:send-enviroment 16)
  (:non-unique 32))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Default}
    @entry[:is-service]{Run as a service. In this mode, registration fails if
      the service is already running, and the application will stay around for
      a while when the use count falls to zero.}
    @entry[:is-launcher]{Do not try to become the primary instance.}
    @entry[:handles-open]{This application handles opening files (in the primary
      instance). Note that this flag only affects the default implementation of
      @code{local_command_line()}, and has no effect if
      @code{:handles-command-line} is given. See the function
      @fun{g-application-run} for details.}
    @entry[:handles-command-line]{This application handles command line
      arguments (in the primary instance). Note that this flag only affect the
      default implementation of @code{local_command_line()}. See the function
      @fun{g-application-run} for details.}
    @entry[:send-enviroment]{Send the environment of the launching process to
      the primary instance. Set this flag if your application is expected to
      behave differently depending on certain environment variables. For
      instance, an editor might be expected to use the @code{GIT_COMMITTER_NAME}
      environment variable when editing a git commit message. The environment is
      available to the \"command-line\" signal handler, via the function
      @fun{g-application-command-line-getenv}.}
    @entry[:non-unique]{Make no attempts to do any of the typical
      single-instance application negotiation. The application neither attempts
      to become the owner of the application ID nor does it check if an existing
      owner already exists. Everything occurs in the local process.
      Since: 2.30.}
  @end{table}
  Since 2.28")

;;; ----------------------------------------------------------------------------
;;; g_application_id_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_id_is_valid" g-application-id-is-valid) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application-id]{a potential application identifier}
  @return{@em{True} if @arg{application-id} is valid.}
  @begin{short}
    Checks if @arg{application-id} is a valid application identifier.
  @end{short}

  A valid ID is required for calls to the functions @fun{g-application-new} and
  @fun{g-application-set-application-id}.

  For convenience, the restrictions on application identifiers are reproduced
  here:
  @begin{itemize}
    @item{Application identifiers must contain only the ASCII characters
      \"[A-Z][a-z][0-9]_-.\" and must not begin with a digit.}
    @item{Application identifiers must contain at least one '.' (period)
      character (and thus at least three elements).}
    @item{Application identifiers must not begin or end with a '.' (period)
      character.}
    @item{Application identifiers must not contain consecutive '.' (period)
      characters.}
    @item{Application identifiers must not exceed 255 characters.}
  @end{itemize}"
  (application-id :string))

(export 'g-application-id-is-valid)

;;; ----------------------------------------------------------------------------
;;; g_application_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-new))

(defun g-application-new (application-id flags)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-9}
  @argument[application-id]{the application ID}
  @argument[flags]{the application flags}
  @return{A new @class{g-application} object.}
  @begin{short}
    Creates a new @class{g-application} object.
  @end{short}

  The application ID must be valid. See the @fun{g-application-id-is-valid}
  function.
  @see-function{g-application-id-is-valid}"
  (make-instance 'g-application
                 :application-id application-id
                 :flags flags))

(export 'g-application-new)

;;; ----------------------------------------------------------------------------
;;; g_application_get_application_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-get-application-id))

(defun g-application-get-application-id (application)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @return{The identifier for application, owned by application.}
  @begin{short}
    Gets the unique identifier for application.
  @end{short}

  Since 2.28"
  (g-application-application-id application))

(export 'g-application-get-application-id)

;;; ----------------------------------------------------------------------------
;;; g_application_set_application_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-set-application-id))

(defun g-application-set-application-id (application application-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @argument[application-id]{the identifier for application}
  @begin{short}
    Sets the unique identifier for application.
  @end{short}

  The application ID can only be modified if application has not yet been
  registered.

  The application ID must be valid. See the function
  @fun{g-application-id-is-valid}.

  Since 2.28
  @see-function{g-application-id-is-valid}"
  (setf (g-application-application-id application) application-id))

(export 'g-application-set-application-id)

;;; ----------------------------------------------------------------------------
;;; g_application_get_inactivity_timeout ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-get-inactivity-timeout))

(defun g-application-get-inactivity-timeout (application)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @return{The timeout, in milliseconds.}
  @begin{short}
    Gets the current inactivity timeout for the application.
  @end{short}

  This is the amount of time (in milliseconds) after the last call to the
  function @fun{g-application-release} before the application stops running.

  Since 2.28"
  (g-application-inactivity-timeout application))

(export 'g-application-get-inactivity-timeout)

;;; ----------------------------------------------------------------------------
;;; g_application_set_inactivity_timeout ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-set-inactivity-timeout))

(defun g-application-set-inactivity-timeout (application inactivity-timeout)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @argument[inactivity-timeout]{the timeout, in milliseconds}
  @begin{short}
    Sets the current inactivity timeout for the application.
  @end{short}

  This is the amount of time (in milliseconds) after the last call to the
  function @fun{g-application-release} before the application stops running.

  This call has no side effects of its own. The value set here is only used
  for next time the function @fun{g-application-release} drops the use count to
  zero. Any timeouts currently in progress are not impacted.

  Since 2.28
  @see-function{g-application-release}"
  (setf (g-application-inactivity-timeout application) inactivity-timeout))

(export 'g-application-set-inactivity-timeout)

;;; ----------------------------------------------------------------------------
;;; g_application_get_flags ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-get-flags))

(defun g-application-get-flags (application)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @return{The flags for application.}
  @begin{short}
    Gets the flags for @arg{application}.
  @end{short}

  See the @symbol{g-application-flags} enumeration.

  Since 2.28"
  (g-application-flags application))

(export 'g-application-get-flags)

;;; ----------------------------------------------------------------------------
;;; g_application_set_flags ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-set-flags))

(defun g-application-set-flags (application flags)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @argument[flags]{the flags for @arg{application}}
  @begin{short}
    Sets the flags for application.
  @end{short}

  The flags can only be modified if application has not yet been registered.

  See the @symbol{g-application-flags} enumeration.

  Since 2.28"
  (setf (g-application-flags application) flags))

(export 'g-application-set-flags)

;;; ----------------------------------------------------------------------------
;;; g_application_get_dbus_connection ()
;;;
;;; GDBusConnection * g_application_get_dbus_connection
;;;                                                 (GApplication *application);
;;;
;;; Gets the GDBusConnection being used by the application, or NULL.
;;;
;;; If GApplication is using its D-Bus backend then this function will return
;;; the GDBusConnection being used for uniqueness and communication with the
;;; desktop environment and other instances of the application.
;;;
;;; If GApplication is not using D-Bus then this function will return NULL. This
;;;; includes the situation where the D-Bus backend would normally be in use but
;;; we were unable to connect to the bus.
;;;
;;; This function must not be called before the application has been registered.
;;; See g_application_get_is_registered().
;;;
;;; application :
;;;     a GApplication
;;;
;;; Returns :
;;;     a GDBusConnection, or NULL. [transfer none]
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_get_dbus_object_path ()
;;;
;;; const gchar * g_application_get_dbus_object_path
;;;                                                 (GApplication *application);
;;;
;;; Gets the D-Bus object path being used by the application, or NULL.
;;;
;;; If GApplication is using its D-Bus backend then this function will return
;;; the D-Bus object path that GApplication is using. If the application is the
;;; primary instance then there is an object published at this path. If the
;;; application is not the primary instance then the result of this function is
;;; undefined.
;;;
;;; If GApplication is not using D-Bus then this function will return NULL. This
;;; includes the situation where the D-Bus backend would normally be in use but
;;; we were unable to connect to the bus.
;;;
;;; This function must not be called before the application has been registered.
;;; See g_application_get_is_registered().
;;;
;;; application :
;;;     a GApplication
;;;
;;; Returns :
;;;     the object path, or NULL
;;;
;;; Since 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_set_action_group ()
;;;
;;; void g_application_set_action_group (GApplication *application,
;;;                                      GActionGroup *action_group);
;;;
;;; Warning
;;;
;;; g_application_set_action_group has been deprecated since version 2.32 and
;;; should not be used in newly-written code. Use the GActionMap interface
;;; instead. Never ever mix use of this API with use of GActionMap on the same
;;; application or things will go very badly wrong. This function is known to
;;; introduce buggy behaviour (ie: signals not emitted on changes to the action
;;; group), so you should really use GActionMap instead.
;;;
;;; This used to be how actions were associated with a GApplication. Now there
;;; is GActionMap for that.
;;;
;;; application :
;;;     a GApplication
;;;
;;; action_group :
;;;     a GActionGroup, or NULL
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_get_is_registered ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-get-is-registered))

(defun g-application-get-is-registered (application)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @return{@em{True} if @arg{application} is registered.}
  @begin{short}
    Checks if @arg{application} is registered.
  @end{short}

  An application is registered if the function @fun{g-application-register}
  has been successfully called.

  Since 2.28
  @see-function{g-application-register}"
  (g-application-is-registered application))

(export 'g-application-get-is-registered)

;;; ----------------------------------------------------------------------------
;;; g_application_get_is_remote ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-get-is-remote))

(defun g-application-get-is-remote (application)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @return{@em{True} if @arg{application} is remote.}
  @begin{short}
    Checks if @arg{application} is remote.
  @end{short}

  If @arg{application} is remote then it means that another instance of
  @arg{application} already exists (the 'primary' instance). Calls to perform
  actions on @arg{application} will result in the actions being performed by
  the primary instance.

  The value of this property cannot be accessed before the function
  @fun{g-application-register} has been called. See the function
  @fun{g-application-get-is-registered}.

  Since 2.28
  @see-function{g-application-register}"
  (g-application-is-remote application))

(export 'g-application-get-is-remote)

;;; ----------------------------------------------------------------------------
;;; g_application_register ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_register" %g-application-register) :boolean
  (application (g-object g-application))
  (cancellable (g-object g-cancellable))
  (err :pointer))

(defun g-application-register (application cancellable)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @argument[cancellable]{a @code{GCancellable}, or @code{nil}}
  @return{@em{True} if registration succeeded.}
  @begin{short}
    Attempts registration of the @arg{application}.
  @end{short}

  This is the point at which the application discovers if it is the primary
  instance or merely acting as a remote for an already-existing primary
  instance. This is implemented by attempting to acquire the application
  identifier as a unique bus name on the session bus using GDBus.

  Due to the internal architecture of GDBus, method calls can be dispatched at
  any time (even if a main loop is not running). For this reason, you must
  ensure that any object paths that you wish to register are registered before
  calling this function.

  If the application has already been registered then @em{true} is returned
  with no work performed.

  The \"startup\" signal is emitted if registration succeeds and application is
  the primary instance.

  In the event of an error (such as cancellable being cancelled, or a failure
  to connect to the session bus), @code{nil} is returned and error is set
  appropriately.

  Note: the return value of this function is not an indicator that this
  instance is or is not the primary instance of the application.
  See the function @fun{g-application-get-is-remote} for that.

  Since 2.28
  @see-function{g-application-get-is-remote}"
  (with-g-error (err)
    (%g-application-register application cancellable err)))

(export 'g-application-register)

;;; ----------------------------------------------------------------------------
;;; g_application_hold ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_hold" g-application-hold) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @begin{short}
    Increases the use count of @arg{application}.
  @end{short}

  Use this function to indicate that the application has a reason to continue
  to run. For example, the function @sym{g-application-hold} is called by GTK+
  when a toplevel window is on the screen.

  To cancel the hold, call the function @fun{g-application-release}.
  @see-function{g-application-release}"
  (application (g-object g-application)))

(export 'g-application-hold)

;;; ----------------------------------------------------------------------------
;;; g_application_release ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_release" g-application-release) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @begin{short}
    Decrease the use count of application.
  @end{short}

  When the use count reaches zero, the @arg{application} will stop running.

  Never call this function except to cancel the effect of a previous call
  to the function @fun{g-application-hold}.
  @see-function{g-application-hold}"
  (application (g-object g-application)))

(export 'g-application-release)

;;; ----------------------------------------------------------------------------
;;; g_application_quit ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_quit" g-application-quit) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @begin{short}
    Immediately quits the @arg{application}.
  @end{short}

  Upon return to the mainloop, the function @fun{g-application-run} will return,
  calling only the 'shutdown' function before doing so.

  The hold count is ignored.

  The result of calling the function @fun{g-application-run} again after it
  returns is unspecified.

  Since 2.32
  @see-function{g-application-run}"
  (application (g-object g-application)))

(export 'g-application-quit)

;;; ----------------------------------------------------------------------------
;;; g_application_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_activate" g-application-activate) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @begin{short}
    Activates the @arg{application}.
  @end{short}

  In essence, this results in the \"activate\" signal being emitted in the
  primary instance.

  The @arg{application} must be registered before calling this function.

  Since 2.28"
  (application (g-object g-application)))

(export 'g-application-activate)

;;; ----------------------------------------------------------------------------
;;; g_application_open ()
;;;
;;; void g_application_open (GApplication *application,
;;;                          GFile **files,
;;;                          gint n_files,
;;;                          const gchar *hint);
;;;
;;; Opens the given files.
;;;
;;; In essence, this results in the "open" signal being emitted in the primary
;;; instance.
;;;
;;; n_files must be greater than zero.
;;;
;;; hint is simply passed through to the ::open signal. It is intended to be
;;; used by applications that have multiple modes for opening files (eg: "view"
;;; vs "edit", etc). Unless you have a need for this functionality, you should
;;; use "".
;;;
;;; The application must be registered before calling this function and it must
;;; have the G_APPLICATION_HANDLES_OPEN flag set.
;;;
;;; application :
;;;     a GApplication
;;;
;;; files :
;;;     an array of GFiles to open
;;;
;;; n_files :
;;;     the length of the files array
;;;
;;; hint :
;;;     a hint (or ""), but never NULL
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_run ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_run" g-application-run) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @argument[argc]{the @code{argc} from @code{main()} (or 0 if @arg{argv} is
    @code{nil})}
  @argument[argv]{the @arg{argv} from @code{main()}, or @code{nil}}
  @return{The exit status.}
  @begin{short}
    Runs the application.
  @end{short}

  This function is intended to be run from @code{main()} and its return value is
  intended to be returned by @code{main()}. Although you are expected to pass
  the @arg{argc}, @arg{argv} parameters from @code{main()} to this function, it
  is possible to pass @code{nil} if @arg{argv} is not available or commandline
  handling is not required.

  First, the @code{local_command_line()} virtual function is invoked. This
  function always runs on the local instance. It gets passed a pointer to a
  @code{null}-terminated copy of @arg{argv} and is expected to remove the
  arguments that it handled (shifting up remaining arguments). See Example 18,
  \"Split commandline handling\" for an example of parsing argv manually.
  Alternatively, you may use the @code{GOptionContext} API, after setting
  @code{argc = g_strv_length (argv);}.

  The last argument to @code{local_command_line()} is a pointer to the status
  variable which can used to set the exit status that is returned from the
  function @sym{g-application-run}.

  If @code{local_command_line()} returns @em{true}, the command line is expected
  to be completely handled, including possibly registering as the primary
  instance, calling the functions @fun{g-application-activate} or
  @fun{g-application-open}, etc.

  If @code{local_command_line()} returns @code{nil} then the application is
  registered and the \"command-line\" signal is emitted in the primary instance
  (which may or may not be this instance). The signal handler gets passed a
  @code{GApplicationCommandLine} object that (among other things) contains the
  remaining @code{commandline} arguments that have not been handled by
  @code{local_command_line()}.

  If the application has the @code{:command-line} flag set then the default
  implementation of @code{local_command_line()} always returns @code{nil}
  immediately, resulting in the @code{commandline} always being handled in the
  primary instance.

  Otherwise, the default implementation of @code{local_command_line()} tries to
  do a couple of things that are probably reasonable for most applications.
  First, the function @fun{g-application-register} is called to attempt to
  register the application. If that works, then the command line arguments are
  inspected. If no @code{commandline} arguments are given, then the function
  @fun{g-application-activate} is called. If @code{commandline} arguments are
  given and the @code{:open} flag is set then they are assumed to be filenames
  and the function @fun{g-application-open} is called.

  If you need to handle commandline arguments that are not filenames, and you
  do not mind @code{commandline} handling to happen in the primary instance, you
  should set @code{:command-line} and process the @code{commandline} arguments
  in your \"command-line\" signal handler, either manually or using the
  @code{GOptionContext} API.

  If you are interested in doing more complicated local handling of the
  @code{commandline} then you should implement your own @class{g-application}
  subclass and override @code{local_command_line()}. In this case, you most
  likely want to return @em{true} from your @code{local_command_line()}
  implementation to suppress the default handling. See Example 18,
  \"Split commandline handling\" for an example.

  If, after the above is done, the use count of the application is zero then
  the exit status is returned immediately. If the use count is non-zero then
  the default main context is iterated until the use count falls to zero, at
  which point 0 is returned.

  If the @code{:is-service} flag is set, then the exiting at use count
  of zero is delayed for a while (i. e.: the instance stays around to provide
  its service to others).

  Since 2.28
  @see-function{g-application-activate}
  @see-function{g-application-open}
  @see-function{g-application-register}"
  (application (g-object g-application))
  (argc :int)
  (argv (:pointer (:pointer :char))))

(export 'g-application-run)

;;; ----------------------------------------------------------------------------
;;; g_application_set_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_set_default" g-application-set-default) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{the application to set as default, or @code{nil}}
  @begin{short}
    Sets or unsets the default application for the process, as returned by
    the function @fun{g-application-get-default}.
  @end{short}

  This function does not take its own reference on application. If application
  is destroyed then the default application will revert back to @code{nil}.

  Since 2.32
  @see-function{g-application-get-default}"
  (application (g-object g-application)))

(export 'g-application-set-default)

;;; ----------------------------------------------------------------------------
;;; g_application_get_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_get_default" g-application-get-default) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @return{The default application for this process, or @code{nil}.}
  @begin{short}
    Returns the default @class{g-application} instance for this process.
  @end{short}

  Normally there is only one @class{g-application} per process and it becomes
  the default when it is created. You can exercise more control over this by
  using the function @fun{g-application-set-default}.

  If there is no default application then @code{nil} is returned.

  Since 2.32
  @see-function{g-application-set-default}")

(export 'g-application-get-default)

;;; --- End of file gio.application.lisp ---------------------------------------
