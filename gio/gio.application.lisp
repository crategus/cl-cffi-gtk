;;; ----------------------------------------------------------------------------
;;; gio.application.lisp
;;;
;;; The documentation has been copied from the GIO Reference Manual
;;; for GIO 2.32.1 The latest version of this documentation can be found on-line
;;; at http://library.gnome.org/devel/gio/unstable/.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;;     
;;;     g_application_set_action_group
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
;;; 
;;; Description
;;; 
;;; A GApplication is the foundation of an application, unique for a given
;;; application identifier. The GApplication class wraps some low-level
;;; platform-specific services and is intended to act as the foundation for
;;; higher-level application classes such as GtkApplication or MxApplication.
;;; In general, you should not use this class outside of a higher level
;;; framework.
;;; 
;;; One of the core features that GApplication provides is process uniqueness,
;;; in the context of a "session". The session concept is platform-dependent,
;;; but corresponds roughly to a graphical desktop login. When your application
;;; is launched again, its arguments are passed through platform communication
;;; to the already running program. The already running instance of the program
;;; is called the primary instance. On Linux, the D-Bus session bus is used for
;;; communication.
;;; 
;;; GApplication provides convenient life cycle management by maintaining a use
;;; count for the primary application instance. The use count can be changed
;;; using g_application_hold() and g_application_release(). If it drops to zero,
;;; the application exits. Higher-level classes such as GtkApplication employ
;;; the use count to ensure that the application stays alive as long as it has
;;; any opened windows.
;;; 
;;; Before using GApplication, you must choose an "application identifier". The
;;; expected form of an application identifier is very close to that of of a
;;; DBus bus name. Examples include: "com.example.MyApp",
;;; "org.example.internal-apps.Calculator". For details on valid application
;;; identifiers, see g_application_id_is_valid().
;;; 
;;; On Linux, the application identifier is claimed as a well-known bus name on
;;; the user's session bus. This means that the uniqueness of your application
;;; is scoped to the current session. It also means that your application may
;;; provide additional services (through registration of other object paths) at
;;; that bus name. The registration of these object paths should be done with
;;; the shared GDBus session bus. Note that due to the internal architecture of
;;; GDBus, method calls can be dispatched at any time (even if a main loop is
;;; not running). For this reason, you must ensure that any object paths that
;;; you wish to register are registered before GApplication attempts to acquire
;;; the bus name of your application (which happens in
;;; g_application_register()). Unfortunately, this means that you cannot use
;;; g_application_get_is_remote() to decide if you want to register object
;;; paths.
;;; 
;;; GApplication also implements the GActionGroup and GActionMap interfaces and
;;; lets you easily export actions by adding them with
;;; g_action_map_add_action(). When invoking an action by calling
;;; g_action_group_activate_action() on the application, it is always invoked
;;; in the primary instance. The actions are also exported on the session bus,
;;; and GIO provides the GDBusActionGroup wrapper to conveniently access them
;;; remotely. GIO provides a GDBusMenuModel wrapper for remote access to
;;; exported GMenuModels.
;;; 
;;; There is a number of different entry points into a GApplication:
;;; 
;;;     via 'Activate' (i.e. just starting the application)
;;;     via 'Open' (i.e. opening some files)
;;;     by handling a command-line
;;;     via activating an action
;;; 
;;; The "startup" signal lets you handle the application initialization for all
;;; of these in a single place.
;;; 
;;; Regardless of which of these entry points is used to start the application,
;;; GApplication passes some platform data from the launching instance to the
;;; primary instance, in the form of a GVariant dictionary mapping strings to
;;; variants. To use platform data, override the before_emit or after_emit
;;; virtual functions in your GApplication subclass. When dealing with
;;; GApplicationCommandLine objects, the platform data is directly available
;;; via g_application_command_line_get_cwd(),
;;; g_application_command_line_get_environ() and
;;; g_application_command_line_get_platform_data().
;;; 
;;; As the name indicates, the platform data may vary depending on the operating
;;; system, but it always includes the current directory (key "cwd"), and
;;; optionally the environment (ie the set of environment variables and their
;;; values) of the calling process (key "environ"). The environment is only
;;; added to the platform data if the G_APPLICATION_SEND_ENVIRONMENT flag is
;;; set. GApplication subclasses can add their own platform data by overriding
;;; the add_platform_data virtual function. For instance, GtkApplication adds
;;; startup notification data in this way.
;;; 
;;; To parse commandline arguments you may handle the "command-line" signal or
;;; override the local_command_line() vfunc, to parse them in either the primary
;;; instance or the local instance, respectively.
;;; 
;;; Example 14. Opening files with a GApplication
;;; 
;;; #include <gio/gio.h>
;;; #include <stdlib.h>
;;; #include <string.h>
;;; 
;;; static void
;;; activate (GApplication *application)
;;; {
;;;   g_print ("activated\n");
;;; 
;;;   /* Note: when doing a longer-lasting action here that returns
;;;    * to the mainloop, you should use g_application_hold() and
;;;    * g_application_release() to keep the application alive until
;;;    * the action is completed.
;;;    */
;;; }
;;; 
;;; static void
;;; open (GApplication  *application,
;;;       GFile        **files,
;;;       gint           n_files,
;;;       const gchar   *hint)
;;; {
;;;   gint i;
;;; 
;;;   for (i = 0; i < n_files; i++)
;;;     {
;;;       gchar *uri = g_file_get_uri (files[i]);
;;;       g_print ("open %s\n", uri);
;;;       g_free (uri);
;;;     }
;;; 
;;;   /* Note: when doing a longer-lasting action here that returns
;;;    * to the mainloop, you should use g_application_hold() and
;;;    * g_application_release() to keep the application alive until
;;;    * the action is completed.
;;;    */
;;; }
;;; 
;;; int
;;; main (int argc, char **argv)
;;; {
;;;   GApplication *app;
;;;   int status;
;;; 
;;;   app = g_application_new ("org.gtk.TestApplication",
;;;                            G_APPLICATION_HANDLES_OPEN);
;;;   g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);
;;;   g_signal_connect (app, "open", G_CALLBACK (open), NULL);
;;;   g_application_set_inactivity_timeout (app, 10000);
;;; 
;;;   status = g_application_run (app, argc, argv);
;;; 
;;;   g_object_unref (app);
;;; 
;;;   return status;
;;; }
;;; 
;;; Example 15. A GApplication with actions    
;;; 
;;; #include <gio/gio.h>
;;; #include <stdlib.h>
;;; #include <string.h>
;;; 
;;; static void
;;; activate (GApplication *application)
;;; {
;;;   g_application_hold (application);
;;;   g_print ("activated\n");
;;;   g_application_release (application);
;;; }
;;; 
;;; static void
;;; activate_action (GAction  *action,
;;;                  GVariant *parameter,
;;;                  gpointer  data)
;;; {
;;;   GApplication *application = G_APPLICATION (data);
;;; 
;;;   g_application_hold (application);
;;;   g_print ("action %s activated\n", g_action_get_name (action));
;;;   g_application_release (application);
;;; }
;;; 
;;; static void
;;; activate_toggle_action (GSimpleAction *action,
;;;                         GVariant      *parameter,
;;;                         gpointer       data)
;;; {
;;;   GApplication *application = G_APPLICATION (data);
;;;   GVariant *state;
;;;   gboolean b;
;;; 
;;;   g_print ("action %s activated\n", g_action_get_name (G_ACTION (action)));
;;; 
;;;   g_application_hold (application);
;;;   state = g_action_get_state (G_ACTION (action));
;;;   b = g_variant_get_boolean (state);
;;;   g_variant_unref (state);
;;;   g_simple_action_set_state (action, g_variant_new_boolean (!b));
;;;   g_print ("state change %d -> %d\n", b, !b);
;;;   g_application_release (application);
;;; }
;;; 
;;; static void
;;; add_actions (GApplication *app)
;;; {
;;;   GSimpleAction *action;
;;; 
;;;   action = g_simple_action_new ("simple-action", NULL);
;;;   g_signal_connect (action, "activate", G_CALLBACK (activate_action), app);
;;;   g_action_map_add_action (G_ACTION_MAP (app), G_ACTION (action));
;;;   g_object_unref (action);
;;; 
;;;   action = g_simple_action_new_stateful ("toggle-action", NULL,
;;;                                          g_variant_new_boolean (FALSE));
;;;   g_signal_connect (action, "activate",
;;;                     G_CALLBACK (activate_toggle_action), app);
;;;   g_action_map_add_action (G_ACTION_MAP (app), G_ACTION (action));
;;;   g_object_unref (action);
;;; }
;;; 
;;; static void
;;; describe_and_activate_action (GActionGroup *group,
;;;                               const gchar  *name)
;;; {
;;;   const GVariantType *param_type;
;;;   GVariant *state;
;;;   gboolean enabled;
;;;   gchar *tmp;
;;; 
;;;   param_type = g_action_group_get_action_parameter_type (group, name);
;;;   state = g_action_group_get_action_state (group, name);
;;;   enabled = g_action_group_get_action_enabled (group, name);
;;; 
;;;   g_print ("action name:      %s\n", name);
;;;   tmp = param_type ? g_variant_type_dup_string (param_type) : NULL;
;;;   g_print ("parameter type:   %s\n", tmp ? tmp : "<none>");
;;;   g_free (tmp);
;;;   g_print ("state type:       %s\n",
;;;            state ? g_variant_get_type_string (state) : "<none>");
;;;   tmp = state ? g_variant_print (state, FALSE) : NULL;
;;;   g_print ("state:            %s\n", tmp ? tmp : "<none>");
;;;   g_free (tmp);
;;;   g_print ("enabled:          %s\n", enabled ? "true" : "false");
;;; 
;;;   if (state != NULL)
;;;     g_variant_unref (state);
;;; 
;;;   g_action_group_activate_action (group, name, NULL);
;;; }
;;; 
;;; int
;;; main (int argc, char **argv)
;;; {
;;;   GApplication *app;
;;;   int status;
;;; 
;;;   app = g_application_new ("org.gtk.TestApplication", 0);
;;;   g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);
;;;   g_application_set_inactivity_timeout (app, 10000);
;;; 
;;;   add_actions (app);
;;; 
;;;   if (argc > 1 && strcmp (argv[1], "--simple-action") == 0)
;;;     {
;;;       g_application_register (app, NULL, NULL);
;;;       describe_and_activate_action (G_ACTION_GROUP (app), "simple-action");
;;;       exit (0);
;;;     }
;;;   else if (argc > 1 && strcmp (argv[1], "--toggle-action") == 0)
;;;     {
;;;       g_application_register (app, NULL, NULL);
;;;       describe_and_activate_action (G_ACTION_GROUP (app), "toggle-action");
;;;       exit (0);
;;;     }
;;; 
;;;   status = g_application_run (app, argc, argv);
;;; 
;;;   g_object_unref (app);
;;; 
;;;   return status;
;;; }
;;; 
;;; Example 16. A GApplication with menus 
;;; 
;;; FIXME: MISSING XINCLUDE CONTENT
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "action-group" property
;;; 
;;;   "action-group"             GActionGroup*         : Write
;;; 
;;; The group of actions that the application exports.
;;;
;;; ----------------------------------------------------------------------------
;;; The "application-id" property
;;; 
;;;   "application-id"           gchar*               : Read / Write / Construct
;;; 
;;; The unique identifier for the application.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "flags" property
;;; 
;;;   "flags"                    GApplicationFlags     : Read / Write
;;; 
;;; Flags specifying the behaviour of the application.
;;;
;;; ----------------------------------------------------------------------------
;;; The "inactivity-timeout" property
;;; 
;;;   "inactivity-timeout"       guint                 : Read / Write
;;; 
;;; Time (ms) to stay alive after becoming idle.
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "is-registered" property
;;; 
;;;   "is-registered"            gboolean              : Read
;;; 
;;; If g_application_register() has been called.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "is-remote" property
;;; 
;;;   "is-remote"                gboolean              : Read
;;; 
;;; If this application instance is remote.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate" signal
;;; 
;;; void user_function (GApplication *application,
;;;                     gpointer      user_data)        : Run Last
;;; 
;;; The ::activate signal is emitted on the primary instance when an activation
;;; occurs. See g_application_activate().
;;; 
;;; application :
;;;     the application
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;;
;;; ----------------------------------------------------------------------------
;;; The "command-line" signal
;;; 
;;; gint user_function (GApplication            *application,
;;;                     GApplicationCommandLine *command_line,
;;;                     gpointer                 user_data)         : Run Last
;;; 
;;; The ::command-line signal is emitted on the primary instance when a
;;; commandline is not handled locally. See g_application_run() and the
;;; GApplicationCommandLine documentation for more information.
;;; 
;;; application :
;;;     the application
;;; 
;;; command_line :
;;;     a GApplicationCommandLine representing the passed commandline
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Returns :
;;;     An integer that is set as the exit status for the calling process.
;;;     See g_application_command_line_set_exit_status().
;;;
;;; ----------------------------------------------------------------------------
;;; The "open" signal
;;; 
;;; void user_function (GApplication *application,
;;;                     gpointer      files,
;;;                     gint          n_files,
;;;                     gchar        *hint,
;;;                     gpointer      user_data)        : Run Last
;;; 
;;; The ::open signal is emitted on the primary instance when there are files
;;; to open. See g_application_open() for more information.
;;; 
;;; application :
;;;     the application
;;; 
;;; files :
;;;     an array of GFiles
;;; 
;;; n_files :
;;;     the length of files
;;; 
;;; hint :
;;;     a hint provided by the calling instance
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;;
;;; ----------------------------------------------------------------------------
;;; The "shutdown" signal
;;; 
;;; void user_function (GApplication *application,
;;;                     gpointer      user_data)        : Run Last
;;; 
;;; The ::shutdown signal is emitted only on the registered primary instance
;;; immediately after the main loop terminates.
;;; 
;;; application :
;;;     the application
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;;
;;; ----------------------------------------------------------------------------
;;; The "startup" signal
;;; 
;;; void user_function (GApplication *application,
;;;                     gpointer      user_data)        : Run First
;;; 
;;; The ::startup signal is emitted on the primary instance immediately after
;;; registration. See g_application_register().
;;; 
;;; application :
;;;     the application
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GApplication
;;; 
;;; typedef struct _GApplication GApplication;
;;; 
;;; Since 2.28
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
;;; 
;;; typedef enum {
;;;   G_APPLICATION_FLAGS_NONE,
;;;   G_APPLICATION_IS_SERVICE           =  (1 << 0),
;;;   G_APPLICATION_IS_LAUNCHER          =  (1 << 1),
;;;   G_APPLICATION_HANDLES_OPEN         =  (1 << 2),
;;;   G_APPLICATION_HANDLES_COMMAND_LINE =  (1 << 3),
;;;   G_APPLICATION_SEND_ENVIRONMENT     =  (1 << 4),
;;;   G_APPLICATION_NON_UNIQUE           =  (1 << 5)
;;; } GApplicationFlags;
;;; 
;;; Flags used to define the behaviour of a GApplication.
;;; 
;;; G_APPLICATION_FLAGS_NONE
;;;     Default
;;; 
;;; G_APPLICATION_IS_SERVICE
;;;     Run as a service. In this mode, registration fails if the service is
;;;     already running, and the application will stay around for a while when
;;;     the use count falls to zero.
;;; 
;;; G_APPLICATION_IS_LAUNCHER
;;;     Don't try to become the primary instance.
;;; 
;;; G_APPLICATION_HANDLES_OPEN
;;;     This application handles opening files (in the primary instance). Note
;;;     that this flag only affects the default implementation of
;;;     local_command_line(), and has no effect if
;;;     G_APPLICATION_HANDLES_COMMAND_LINE is given. See g_application_run()
;;;     for details.
;;; 
;;; G_APPLICATION_HANDLES_COMMAND_LINE
;;;     This application handles command line arguments (in the primary
;;;     instance). Note that this flag only affect the default implementation
;;;     of local_command_line(). See g_application_run() for details.
;;; 
;;; G_APPLICATION_SEND_ENVIRONMENT
;;;     Send the environment of the launching process to the primary instance.
;;;     Set this flag if your application is expected to behave differently
;;;     depending on certain environment variables. For instance, an editor
;;;     might be expected to use the GIT_COMMITTER_NAME environment variable
;;;     when editing a git commit message. The environment is available to the
;;;     "command-line" signal handler, via g_application_command_line_getenv().
;;; 
;;; G_APPLICATION_NON_UNIQUE
;;;     Make no attempts to do any of the typical single-instance application
;;;     negotiation. The application neither attempts to become the owner of
;;;     the application ID nor does it check if an existing owner already
;;;     exists. Everything occurs in the local process. Since: 2.30.
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(define-g-flags "GApplicationFlags" g-application-flags
  (:export t
   :type-initializer "g_application_flags_get_type")
  (:flags-none 0)
  (:is-service 1)
  (:is-launcher 2)
  (:handles-open 4)
  (:handles-command-line 8)
  (:send-enviroment 16)
  (:non-unique 32))

;;; ----------------------------------------------------------------------------
;;; g_application_id_is_valid ()
;;; 
;;; gboolean g_application_id_is_valid (const gchar *application_id);
;;; 
;;; Checks if application_id is a valid application identifier.
;;; 
;;; A valid ID is required for calls to g_application_new() and
;;; g_application_set_application_id().
;;; 
;;; For convenience, the restrictions on application identifiers are reproduced
;;; here:
;;; 
;;;     Application identifiers must contain only the ASCII characters
;;;     "[A-Z][a-z][0-9]_-." and must not begin with a digit.
;;;
;;;     Application identifiers must contain at least one '.' (period) character
;;;     (and thus at least three elements).
;;;
;;;     Application identifiers must not begin or end with a '.' (period)
;;;     character.
;;;
;;;     Application identifiers must not contain consecutive '.' (period)
;;;     characters.
;;;
;;;     Application identifiers must not exceed 255 characters.
;;; 
;;; application_id :
;;;     a potential application identifier
;;; 
;;; Returns :
;;;     TRUE if application_id is valid
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_id_is_valid" g-application-id-is-valid) :boolean
  (application-id :string))

(export 'g-application-id-is-valid)

;;; ----------------------------------------------------------------------------
;;; g_application_new ()
;;; 
;;; GApplication * g_application_new (const gchar *application_id,
;;;                                   GApplicationFlags flags);
;;; 
;;; Creates a new GApplication instance.
;;; 
;;; This function calls g_type_init() for you.
;;; 
;;; The application id must be valid. See g_application_id_is_valid().
;;; 
;;; application_id :
;;;     the application id
;;; 
;;; flags :
;;;     the application flags
;;; 
;;; Returns :
;;;     a new GApplication instance
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-new))

(defun g-application-new (application-id flags)
  (make-instance 'g-application
                 :application-id application-id
                 :flags flags))

(export 'g-application-new)

;;; ----------------------------------------------------------------------------
;;; g_application_get_application_id ()
;;; 
;;; const gchar * g_application_get_application_id (GApplication *application);
;;; 
;;; Gets the unique identifier for application.
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; Returns :
;;;     the identifier for application, owned by application
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-get-application-id))

(defun g-application-get-application-id (application)
  (g-application-application-id application))

(export 'g-application-get-application-id)

;;; ----------------------------------------------------------------------------
;;; g_application_set_application_id ()
;;; 
;;; void g_application_set_application_id (GApplication *application,
;;;                                        const gchar *application_id);
;;; 
;;; Sets the unique identifier for application.
;;; 
;;; The application id can only be modified if application has not yet been
;;; registered.
;;; 
;;; The application id must be valid. See g_application_id_is_valid().
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; application_id :
;;;     the identifier for application
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-set-application-id))

(defun g-application-set-application-id (application application-id)
  (setf (g-application-application-id application) application-id))

(export 'g-application-set-application-id)

;;; ----------------------------------------------------------------------------
;;; g_application_get_inactivity_timeout ()
;;; 
;;; guint g_application_get_inactivity_timeout (GApplication *application);
;;; 
;;; Gets the current inactivity timeout for the application.
;;; 
;;; This is the amount of time (in milliseconds) after the last call to
;;; g_application_release() before the application stops running.
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; Returns :
;;;     the timeout, in milliseconds
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-get-inactivity-timeout))

(defun g-application-get-inactivity-timeout (application)
  (g-application-inactivity-timeout application))

(export 'g-application-get-inactivity-timeout)

;;; ----------------------------------------------------------------------------
;;; g_application_set_inactivity_timeout ()
;;; 
;;; void g_application_set_inactivity_timeout (GApplication *application,
;;;                                            guint inactivity_timeout);
;;; 
;;; Sets the current inactivity timeout for the application.
;;; 
;;; This is the amount of time (in milliseconds) after the last call to
;;; g_application_release() before the application stops running.
;;; 
;;; This call has no side effects of its own. The value set here is only used
;;; for next time g_application_release() drops the use count to zero. Any
;;; timeouts currently in progress are not impacted.
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; inactivity_timeout :
;;;     the timeout, in milliseconds
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-set-inactivity-timeout))

(defun g-application-set-inactivity-timeout (application inactivity-timeout)
  (setf (g-application-inactivity-timeout application) inactivity-timeout))

(export 'g-application-set-inactivity-timeout)

;;; ----------------------------------------------------------------------------
;;; g_application_get_flags ()
;;; 
;;; GApplicationFlags g_application_get_flags (GApplication *application);
;;; 
;;; Gets the flags for application.
;;; 
;;; See GApplicationFlags.
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; Returns :
;;;     the flags for application
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-get-flags))

(defun g-application-get-flags (application)
  (g-application-flags application))

(export 'g-application-get-flags)

;;; ----------------------------------------------------------------------------
;;; g_application_set_flags ()
;;; 
;;; void g_application_set_flags (GApplication *application,
;;;                               GApplicationFlags flags);
;;; 
;;; Sets the flags for application.
;;; 
;;; The flags can only be modified if application has not yet been registered.
;;; 
;;; See GApplicationFlags.
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; flags :
;;;     the flags for application
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-application-set-flags))

(defun g-application-set-flags (application flags)
  (setf (g-application-flags application) flags))

(export 'g-application-set-flags)

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
;;; 
;;; gboolean g_application_get_is_registered (GApplication *application);
;;; 
;;; Checks if application is registered.
;;; 
;;; An application is registered if g_application_register() has been
;;; successfully called.
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; Returns :
;;;     TRUE if application is registered
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_get_is_remote ()
;;; 
;;; gboolean g_application_get_is_remote (GApplication *application);
;;; 
;;; Checks if application is remote.
;;; 
;;; If application is remote then it means that another instance of application
;;; already exists (the 'primary' instance). Calls to perform actions on
;;; application will result in the actions being performed by the primary
;;; instance.
;;; 
;;; The value of this property cannot be accessed before
;;; g_application_register() has been called.
;;; See g_application_get_is_registered().
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; Returns :
;;;     TRUE if application is remote
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_register ()
;;; 
;;; gboolean g_application_register (GApplication *application,
;;;                                  GCancellable *cancellable,
;;;                                  GError **error);
;;; 
;;; Attempts registration of the application.
;;; 
;;; This is the point at which the application discovers if it is the primary
;;; instance or merely acting as a remote for an already-existing primary
;;; instance. This is implemented by attempting to acquire the application
;;; identifier as a unique bus name on the session bus using GDBus.
;;; 
;;; Due to the internal architecture of GDBus, method calls can be dispatched at
;;; any time (even if a main loop is not running). For this reason, you must
;;; ensure that any object paths that you wish to register are registered before
;;; calling this function.
;;; 
;;; If the application has already been registered then TRUE is returned with
;;; no work performed.
;;; 
;;; The "startup" signal is emitted if registration succeeds and application is
;;; the primary instance.
;;; 
;;; In the event of an error (such as cancellable being cancelled, or a failure
;;; to connect to the session bus), FALSE is returned and error is set
;;; appropriately.
;;; 
;;; Note: the return value of this function is not an indicator that this
;;; instance is or is not the primary instance of the application.
;;; See g_application_get_is_remote() for that.
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; cancellable :
;;;     a GCancellable, or NULL
;;; 
;;; error :
;;;     a pointer to a NULL GError, or NULL
;;; 
;;; Returns :
;;;     TRUE if registration succeeded
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_hold ()
;;; 
;;; void g_application_hold (GApplication *application);
;;; 
;;; Increases the use count of application.
;;; 
;;; Use this function to indicate that the application has a reason to continue
;;; to run. For example, g_application_hold() is called by GTK+ when a toplevel
;;; window is on the screen.
;;; 
;;; To cancel the hold, call g_application_release().
;;; 
;;; application :
;;;     a GApplication
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_release ()
;;; 
;;; void g_application_release (GApplication *application);
;;; 
;;; Decrease the use count of application.
;;; 
;;; When the use count reaches zero, the application will stop running.
;;; 
;;; Never call this function except to cancel the effect of a previous call
;;; to g_application_hold().
;;; 
;;; application :
;;;     a GApplication
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_quit ()
;;; 
;;; void g_application_quit (GApplication *application);
;;; 
;;; Immediately quits the application.
;;; 
;;; Upon return to the mainloop, g_application_run() will return, calling only
;;; the 'shutdown' function before doing so.
;;; 
;;; The hold count is ignored.
;;; 
;;; The result of calling g_application_run() again after it returns is
;;; unspecified.
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_activate ()
;;; 
;;; void g_application_activate (GApplication *application);
;;; 
;;; Activates the application.
;;; 
;;; In essence, this results in the "activate" signal being emitted in the
;;; primary instance.
;;; 
;;; The application must be registered before calling this function.
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

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
;;; 
;;; int g_application_run (GApplication *application, int argc, char **argv);
;;; 
;;; Runs the application.
;;; 
;;; This function is intended to be run from main() and its return value is
;;; intended to be returned by main(). Although you are expected to pass the
;;; argc, argv parameters from main() to this function, it is possible to pass
;;; NULL if argv is not available or commandline handling is not required.
;;; 
;;; First, the local_command_line() virtual function is invoked. This function
;;; always runs on the local instance. It gets passed a pointer to a
;;; NULL-terminated copy of argv and is expected to remove the arguments that
;;; it handled (shifting up remaining arguments). See Example 18, “Split
;;; commandline handling” for an example of parsing argv manually.
;;; Alternatively, you may use the GOptionContext API, after setting
;;; argc = g_strv_length (argv);.
;;; 
;;; The last argument to local_command_line() is a pointer to the status
;;; variable which can used to set the exit status that is returned from
;;; g_application_run().
;;; 
;;; If local_command_line() returns TRUE, the command line is expected to be
;;; completely handled, including possibly registering as the primary instance,
;;; calling g_application_activate() or g_application_open(), etc.
;;; 
;;; If local_command_line() returns FALSE then the application is registered and
;;; the "command-line" signal is emitted in the primary instance (which may or
;;; may not be this instance). The signal handler gets passed a
;;; GApplicationCommandLine object that (among other things) contains the
;;; remaining commandline arguments that have not been handled by
;;; local_command_line().
;;; 
;;; If the application has the G_APPLICATION_HANDLES_COMMAND_LINE flag set then
;;; the default implementation of local_command_line() always returns FALSE
;;; immediately, resulting in the commandline always being handled in the
;;; primary instance.
;;; 
;;; Otherwise, the default implementation of local_command_line() tries to do
;;; a couple of things that are probably reasonable for most applications.
;;; First, g_application_register() is called to attempt to register the
;;; application. If that works, then the command line arguments are inspected.
;;; If no commandline arguments are given, then g_application_activate() is
;;; called. If commandline arguments are given and the
;;; G_APPLICATION_HANDLES_OPEN flag is set then they are assumed to be filenames
;;; and g_application_open() is called.
;;; 
;;; If you need to handle commandline arguments that are not filenames, and you
;;; don't mind commandline handling to happen in the primary instance, you
;;; should set G_APPLICATION_HANDLES_COMMAND_LINE and process the commandline
;;; arguments in your "command-line" signal handler, either manually or using
;;; the GOptionContext API.
;;; 
;;; If you are interested in doing more complicated local handling of the
;;; commandline then you should implement your own GApplication subclass and
;;; override local_command_line(). In this case, you most likely want to return
;;; TRUE from your local_command_line() implementation to suppress the default
;;; handling. See Example 18, “Split commandline handling” for an example.
;;; 
;;; If, after the above is done, the use count of the application is zero then
;;; the exit status is returned immediately. If the use count is non-zero then
;;; the default main context is iterated until the use count falls to zero, at
;;; which point 0 is returned.
;;; 
;;; If the G_APPLICATION_IS_SERVICE flag is set, then the exiting at use count
;;; of zero is delayed for a while (ie: the instance stays around to provide
;;; its service to others).
;;; 
;;; application :
;;;     a GApplication
;;; 
;;; argc :
;;;     the argc from main() (or 0 if argv is NULL)
;;; 
;;; argv :
;;;     the argv from main(), or NULL
;;; 
;;; Returns :
;;;     the exit status
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_run" g-application-run) :int
  (application (g-object g-application))
  (argc :int)
  (argv (:pointer (:pointer :char))))

(export 'g-application-run)

;;; ----------------------------------------------------------------------------
;;; g_application_set_default ()
;;; 
;;; void g_application_set_default (GApplication *application);
;;; 
;;; Sets or unsets the default application for the process, as returned by
;;; g_application_get_default().
;;; 
;;; This function does not take its own reference on application. If application
;;; is destroyed then the default application will revert back to NULL.
;;; 
;;; application :
;;;     the application to set as default, or NULL
;;; 
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_get_default ()
;;; 
;;; GApplication * g_application_get_default (void);
;;; 
;;; Returns the default GApplication instance for this process.
;;; 
;;; Normally there is only one GApplication per process and it becomes the
;;; default when it is created. You can exercise more control over this by
;;; using g_application_set_default().
;;; 
;;; If there is no default application then NULL is returned.
;;; 
;;; Returns :
;;;     the default application for this process, or NULL
;;; 
;;; Since 2.32
;;; ----------------------------------------------------------------------------


;;; --- End of file gio.application.lisp ---------------------------------------
