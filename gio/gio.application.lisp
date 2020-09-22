;;; ----------------------------------------------------------------------------
;;; gio.application.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.62 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2020 Dieter Kaiser
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
;;;     Core application class
;;;
;;; Types and Values
;;;
;;;     GApplication
;;;     GApplicationFlags
;;;
;;; Functions
;;;
;;;     g_application_id_is_valid
;;;     g_application_new
;;;     g_application_get_application_id                   Accessor
;;;     g_application_set_application_id                   Accessor
;;;     g_application_get_inactivity_timeout               Accessor
;;;     g_application_set_inactivity_timeout               Accessor
;;;     g_application_get_flags                            Accessor
;;;     g_application_set_flags                            Accessor
;;;
;;;     g_application_get_resource_base_path               Accessor
;;;     g_application_set_resource_base_path               Accessor
;;;
;;;     g_application_get_dbus_connection
;;;     g_application_get_dbus_object_path
;;;     g_application_set_action_group                     deprecated
;;;     g_application_get_is_registered                    Accessor
;;;     g_application_get_is_remote                        Accessor
;;;     g_application_register
;;;     g_application_hold
;;;     g_application_release
;;;     g_application_quit
;;;     g_application_activate
;;;     g_application_open
;;;
;;;     g_application_send_notification
;;;     g_application_withdraw_notification
;;;
;;;     g_application_run
;;;
;;;     g_application_add_main_option_entries
;;;     g_application_add_main_option
;;;     g_application_add_option_group
;;;     g_application_set_option_context_parameter_string
;;;     g_application_set_option_context_summary
;;;     g_application_set_option_context_description
;;;
;;;     g_application_set_default
;;;     g_application_get_default
;;;
;;;     g_application_mark_busy
;;;     g_application_unmark_busy
;;;     g_application_get_is_busy                          Accessor
;;;     g_application_bind_busy_property
;;;     g_application_unbind_busy_property
;;;
;;; Properties
;;;
;;;      GActionGroup*   action-group            Write
;;;             gchar*   application-id          Read / Write / Construct
;;; GApplicationFlags    flags                   Read / Write
;;;             guint    inactivity-timeout      Read / Write
;;;          gboolean    is-busy                 Read
;;;          gboolean	 is-registered           Read
;;;          gboolean	 is-remote               Read
;;;             gchar*   resource-base-path      Read / Write
;;;
;;; Signals
;;;
;;;              void    activate                Run Last
;;;              gint    command-line            Run Last
;;;              gint    handle-local-options    Run Last
;;;          gboolean    name-lost               Run Last
;;;              void    open                    Run Last
;;;              void    shutdown                Run Last
;;;              void    startup                 Run First
;;;
;;; Object Hierarchy
;;;
;;;     GFlags
;;;     ╰── GApplicationFlags
;;;
;;;     GObject
;;;     ╰── GApplication
;;;
;;; Implemented Interfaces
;;;
;;;     GApplication implements GActionGroup and GActionMap.
;;; ----------------------------------------------------------------------------

(in-package :gio)

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
  (:non-unique 32)
  (:can-override-app-id 64)
  (:allow-replacement 128)
  (:replace 256))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'g-application-flags atdoc:*external-symbols*)
 "@version{#2020-2-1}
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
  (:non-unique 32)
  (:can-override-app-id 64)
  (:allow-replacement 128)
  (:replace 256))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Default}
    @entry[:is-service]{Run as a service. In this mode, registration fails if
      the service is already running, and the application will initially wait
      up to 10 seconds for an initial activation message to arrive.}
    @entry[:is-launcher]{Do not try to become the primary instance.}
    @entry[:handles-open]{This application handles opening files in the primary
      instance. Note that this flag only affects the default implementation of
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
      owner already exists. Everything occurs in the local process.}
    @entry[:can-override-app-id]{Allow users to override the application ID from
      the command line with @code{--gapplication-app-id}. Since 2.48}
    @entry[:allow-replacement]{Allow another instance to take over the bus name.
      Since 2.60}
    @entry[:replace]{Take over from another instance. This flag is usually set
      by passing @code{--gapplication-replace} on the commandline. Since 2.60}
  @end{table}
  @see-class{g-application}
  @see-function{g-application-run}")

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
    "application-id" "gchararray" t t)
   (flags
    g-application-flags
    "flags" "GApplicationFlags" t t)
   (inactivity-timeout
    g-application-inactivity-timeout
    "inactivity-timeout" "guint" t t)
   #+glib-2-44
   (is-busy
    g-application-is-busy
    "is-busy" "gboolean" t nil)
   (is-registered
    g-application-is-registered
    "is-registered" "gboolean" t nil)
   (is-remote
    g-application-is-remote
    "is-remote" "gboolean" t nil)
   #+glib-2-42
   (resource-base-path
    g-application-resource-base-bath
    "resource-base-path" "gchararray" t t)
   ))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-application 'type)
 "@version{#2020-2-1}
  @begin{short}
    A @sym{g-application} is the foundation of an application.
  @end{short}
  The @sym{g-application} class wraps some low-level platform-specific services
  and is intended to act as the foundation for higher-level application classes
  such as @class{gtk-application} or @code{MxApplication}. In general, you
  should not use this class outside of a higher level framework.

  @sym{g-application} provides convenient life cycle management by maintaining
  a \"use count\" for the primary application instance. The use count can be
  changed using the functions @fun{g-application-hold} and
  @fun{g-application-release}. If it drops to zero, the application exits.
  Higher-level classes such as @class{gtk-application} employ the use count to
  ensure that the application stays alive as long as it has any opened windows.

  Another feature that @sym{g-application}, optionally, provides is process
  uniqueness. Applications can make use of this functionality by providing a
  unique application ID. If given, only one application with this ID can be
  running at a time per session. The session concept is platform-dependent, but
  corresponds roughly to a graphical desktop login. When your application is
  launched again, its arguments are passed through platform communication to the
  already running program. The already running instance of the program is called
  the \"primary instance\"; for non-unique applications this is the always the
  current instance. On Linux, the D-Bus session bus is used for communication.

  The use of @sym{g-application} differs from some other commonly-used
  uniqueness libraries, such as libunique, in important ways. The application
  is not expected to manually register itself and check if it is the primary
  instance. Instead, the @code{main()} function of a @sym{g-application} should
  do very little more than instantiating the application instance, possibly
  connecting signal handlers, then calling the function @fun{g-application-run}.
  All checks for uniqueness are done internally. If the application is the
  primary instance then the startup signal is emitted and the mainloop runs.
  If the application is not the primary instance then a signal is sent to the
  primary instance and the function @fun{g-application-run} promptly returns.
  See the code examples below.

  If used, the expected form of an application identifier is the same as that of
  of a D-Bus well-known bus name. Examples include: @code{com.example.MyApp},
  @code{org.example.internal_apps.Calculator}, @code{org._7_zip.Archiver}. For
  details on valid application identifiers, see the function
  @fun{g-application-id-is-valid}.

  On Linux, the application identifier is claimed as a well-known bus name on
  the user's session bus. This means that the uniqueness of your application is
  scoped to the current session. It also means that your application may provide
  additional services, through registration of other object paths, at that bus
  name. The registration of these object paths should be done with the shared
  GDBus session bus. Note that due to the internal architecture of GDBus, method
  calls can be dispatched at any time, even if a main loop is not running. For
  this reason, you must ensure that any object paths that you wish to register
  are registered before @sym{g-application} attempts to acquire the bus name of
  your application, which happens in the function @fun{g-application-register}.
  Unfortunately, this means that you cannot use the function
  @fun{g-application-is-remote} to decide if you want to register object paths.

  @sym{g-application} also implements the @class{g-action-group} and
  @class{g-action-map} interfaces and lets you easily export actions by adding
  them with the function @fun{g-action-map-add-action}. When invoking an action
  by calling the function @fun{g-action-group-activate-action} on the
  application, it is always invoked in the primary instance. The actions are
  also exported on the session bus, and GIO provides the @code{GDBusActionGroup}
  wrapper to conveniently access them remotely. GIO provides a
  @code{GDBusMenuModel} wrapper for remote access to exported
  @class{g-menu-model}s.

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
  directly available via the functions
  @code{g_application_command_line_get_cwd()},
  @code{g_application_command_line_get_environ()} and
  @code{g_application_command_line_get_platform_data()}.

  As the name indicates, the platform data may vary depending on the operating
  system, but it always includes the current directory (key \"cwd\"), and
  optionally the environment, i. e. the set of environment variables and their
  values, of the calling process (key \"environ\"). The environment is only
  added to the platform data if the @code{:send-enviroment} flag is set.
  @sym{g-application} subclasses can add their own platform data by overriding
  the @code{add_platform_data} virtual function. For instance,
  @sym{gtk-application} adds startup notification data in this way.

  To parse commandline arguments you may handle the \"command-line\" signal or
  override the @code{local_command_line()} vfunc, to parse them in either the
  primary instance or the local instance, respectively.
  @begin[Example]{dictionary}
    A simple example to show the use of the signals of an application.
    @begin{pre}
(defun example-application-open (&optional (argv nil))
  (within-main-loop
    (let ((app (make-instance 'g-application
                              :application-id \"com.crategus.application-open\"
                              :inactivity-timeout 10000
                              :flags :handles-open)))

      ;; Signal handler \"startup\"
      (g-signal-connect app \"startup\"
                        (lambda (application)
                          (declare (ignore application))
                          (format t \"The application is in startup.~%\")))

      ;; Signal handler \"activate\"
      (g-signal-connect app \"activate\"
                        (lambda (application)
                          (declare (ignore application))
                          (format t \"The application is in activate.~%\")
                          ;; Note: when doing a longer-lasting action here that
                          ;; returns to the mainloop, you should use
                          ;; g-application-hold and g-application-release to
                          ;; keep the application alive until the action is
                          ;; completed.
                        ))

      ;; Signal handler \"open\"
      (g-signal-connect app \"open\"
                        (lambda (application files n-files hint)
                          (declare (ignore application))
                          (format t \"The application is in open.~%\")
                          (format t \"    files : ~A~%\" files)
                          (format t \"  n-files : ~A~%\" n-files)
                          (format t \"     hint : ~A~%\" hint)
                          ;; TODO: The argument 'files' is a C pointer to an
                          ;; array of files. The conversion to a list of strings
                          ;; with the call (convert-from-foreign files 'g-strv)
                          ;; does not work. Search a better implementation to
                          ;; get a list of files.
                        ))

      ;; Signal handler \"shutdown\"
      (g-signal-connect app \"shutdown\"
                        (lambda (application)
                          (declare (ignore application))
                          (format t \"The application is in shutdown.~%\")
                          ;; Stop the main loop
                          (leave-gtk-main)))

      ;; Start the application
      (g-application-run app argv))))
    @end{pre}
    An example to show the implementation of actions for an application.
    @begin{pre}
(defun activate-action (app name)
  (let ((param-type (g-action-group-get-action-parameter-type app name))
        (state (g-action-group-get-action-state app name))
        (enabled (g-action-group-get-action-enabled app name)))
    ;; Print information about the action
    (format t \"     action name : ~A~%\" name)
    (format t \"  parameter type : ~A~%\" param-type)
    (unless (null-pointer-p state)
      (format t \"      state type : ~A~%\" (g-variant-type-string state)))
    (format t \"           state : ~A~%\" state)
    (format t \"         enabled : ~A~%~%\" enabled)
    ;; Activate the action
    (g-action-group-activate-action app name state)))

(defun example-application-action (&optional (argv nil))
  (within-main-loop
    (let ((app (make-instance 'g-application
                              :application-id \"com.crategus.application-action\"
                              :inactivity-timeout 10000
                              :flags :none)))

      ;; Create the action \"simple-action\"
      (let ((action (g-simple-action-new \"simple-action\" nil)))
        ;; Connect a handler to the signal activate
        (g-signal-connect action \"activate\"
            (lambda (action parameter)
              (declare (ignore parameter))
              (format t \"Action ~A is activated.~%~%\" (g-action-name action))))
        ;; Add the action to the action map of the application
        (g-action-map-add-action app action))

      ;; Create the action \"toggle-action\"
      (let ((action (g-simple-action-new-stateful \"toggle-action\"
                                                  (g-variant-type-new \"b\")
                                                  (g-variant-new-boolean nil))))
        ;; Connect a handler to the signal activate
        (g-signal-connect action \"activate\"
            (lambda (action parameter)
              (declare (ignore parameter))
              (format t \"Action ~A is activated.~%\" (g-action-name action))
              (let ((state (g-variant-boolean (g-action-state action))))
                (if state
                    (setf (g-simple-action-state action)
                          (g-variant-new-boolean nil))
                    (setf (g-simple-action-state action)
                          (g-variant-new-boolean t)))
                (format t \"The state changed from ~A to ~A.~%~%\"
                          state
                          (not state)))))
        ;; Add the action to the action map of the application
        (g-action-map-add-action app action))

      ;; Signal handler \"activate\"
      (g-signal-connect app \"activate\"
                        (lambda (application)
                          (format t \"The application is in activate.~%~%\")
                          ;; Activate the actions and print information
                          (activate-action application \"simple-action\")
                          (activate-action application \"toggle-action\")))

      ;; Signal handler \"shutdown\"
      (g-signal-connect app \"shutdown\"
                        (lambda (application)
                          (declare (ignore application))
                          (format t \"The application is in shutdown.~%\")
                          ;; Stop the main loop
                          (leave-gtk-main)))

      ;; Start the application
      (g-application-run app argv))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (application)    : Run Last
      @end{pre}
      The \"activate\" signal is emitted on the primary instance when an
      activation occurs. See the function @fun{g-application-activate}.
      @begin[code]{table}
        @entry[application]{The application.}
      @end{table}
    @subheading{The \"command-line\" signal}
      @begin{pre}
 lambda (application command-line)    : Run Last
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
    @subheading{The \"handle-local-options\" signal}
      @begin{pre}
 lambda (application options)    : Run Last
      @end{pre}
      The \"handle-local-options\" signal is emitted on the local instance after
      the parsing of the commandline options has occurred.

      You can add options to be recognised during commandline option parsing
      using the functions @fun{g-application-add-main-option-entries} and
      @fun{g-application-add-option-group}.

      Signal handlers can inspect options, along with values pointed to from the
      @code{arg_data} of an installed @code{GOptionEntrys}, in order to decide
      to perform certain actions, including direct local handling, which may be
      useful for options like --version.

      In the event that the application is marked @code{:handles-command-line}
      the \"normal processing\" will send the options dictionary to the primary
      instance where it can be read with the function
      @fun{g-application-command-line-get-options-dict}. The signal handler can
      modify the dictionary before returning, and the modified dictionary will
      be sent.

      In the event that @code{:handles-command-line} is not set,
      \"normal processing\" will treat the remaining uncollected command line
      arguments as filenames or URIs. If there are no arguments, the application
      is activated by the function @fun{g-application-activate}. One or more
      arguments results in a call to the function @fun{g-application-open}.

      If you want to handle the local commandline arguments for yourself by
      converting them to calls to the functions @fun{g-application-open} or
      @fun{g-action-group-activate-action} then you must be sure to register
      the application first. You should probably not call the function
      @fun{g-application-activate} for yourself, however: just return -1 and
      allow the default handler to do it for you. This will ensure that the
      @code{--gapplication-service} switch works properly, i. e. no activation
      in that case.

      Note that this signal is emitted from the default implementation of
      @code{local_command_line()}. If you override that function and don't
      chain up then this signal will never be emitted.

      You can override @code{local_command_line()} if you need more powerful
      capabilities than what is provided here, but this should not normally be
      required. Since 2.40.
      @begin[code]{table}
        @entry[application]{The @sym{g-application} object.}
        @entry[options]{The options dictionary of type @symbol{g-variant-dict}.}
        @entry[Returns]{An exit code. If you have handled your options and want
          to exit the process, return a non-negative option, 0 for success, and
          a positive value for failure. To continue, return -1 to let the
          default option processing continue.}
      @end{table}
    @subheading{The \"name-lost\" signal}
      @begin{pre}
 lambda (application)    : Run Last
      @end{pre}
      The \"name-lost\" signal is emitted only on the registered primary
      instance when a new instance has taken over. This can only happen if the
      application is using the @code{:allow-replacement} flag of
      @symbol{g-application-flags}.

      The default handler for this signal calls the function
      @fun{g-application-quit}. Since 2.60.
      @begin[code]{table}
        @entry[application]{The @sym{g-application} object.}
        @entry[options]{The options dictionary of type @symbol{g-variant-dict}.}
        @entry[Returns]{@arg{True} if the signal has been handled.}
      @end{table}
    @subheading{The \"open\" signal}
      @begin{pre}
 lambda (application files n-files hint)    : Run Last
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
 lambda (application)    : Run Last
      @end{pre}
      The \"shutdown\" signal is emitted only on the registered primary instance
      immediately after the main loop terminates.
      @begin[code]{table}
        @entry[application]{The application.}
      @end{table}
    @subheading{The \"startup\" signal}
      @begin{pre}
 lambda (application)    : Run First
      @end{pre}
      The \"startup\" signal is emitted on the primary instance immediately
      after registration. See the function @fun{g-application-register}.
      @begin[code]{table}
        @entry[application]{The application.}
      @end{table}
  @end{dictionary}
  @see-slot{g-application-action-group}
  @see-slot{g-application-application-id}
  @see-slot{g-application-flags}
  @see-slot{g-application-inactivity-timeout}
  @see-slot{g-application-is-busy}
  @see-slot{g-application-is-registered}
  @see-slot{g-application-is-remote}
  @see-slot{g-application-resource-base-path}
  @see-class{gtk-application}
  @see-class{g-action-group}
  @see-class{g-action-map}
  @see-symbol{g-application-flags}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g-application-action-group ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "action-group"
                                               'g-application) 't)
 "The @code{action-group} property of type @class{g-action-group} (Write) @br{}
  The group of actions that the application exports. @br{}
  @u{Warning:} The @code{action-group} property is deprecated since version
  2.32. Use the @class{g-action-map} interface instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-action-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-action-group 'function)
 "@version{2020-2-1}
  @syntax[]{(setf (g-application-action-group object) action-group)}
  @argument[object]{a @class{g-application} object}
  @argument[action-group]{a @class{gtk-action-group}, or @code{nil}}
  @begin{short}
    Accessor of the @slot[g-application]{action-group} slot of the
    @class{g-application} class.
  @end{short}

  This used to be how actions were associated with a @class{g-application}. Now
  there is @class{g-action-map} for that.
  @begin[Warning]{dictionary}
    The function @sym{g-application-action-group} has been deprecated since
    version 2.32 and should not be used in newly-written code. Use the
    @class{g-action-map} interface instead. Never ever mix use of this API with
    use of @class{g-action-map} on the same application or things will go very
    badly wrong. This function is known to introduce buggy behaviour, i.e.
    signals not emitted on changes to the action group.
  @end{dictionary}
  @see-class{g-application}
  @see-class{g-action-map}")

;;; --- g-application-application-id -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "application-id"
                                               'g-application) 't)
 "The @code{application-id} property of type @code{:string}
  (Read / Write / Construct) @br{}
  The unique identifier for the application. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-application-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-application-id 'function)
 "@version{2020-2-1}
  @syntax[]{(g-application-application-id object) => applicaton-id}
  @syntax[]{(setf (g-application-application-id object) application-id)}
  @argument[object]{a @class{g-application} object}
  @argument[application-id]{a @code{:string} with the identifier of the
    application}
  @begin{short}
    Accessor of the @slot[g-application]{application-id} slot of the
    @class{g-application} class.
  @end{short}

  The slot access function @sym{g-application-application-id}
  gets the unique identifier for the application.

  The slot access function @sym{(setf g-application-application-id)}
  sets the unique identifier for application.

  The application ID can only be modified if the application has not yet been
  registered. The application ID must be valid. See the function
  @fun{g-application-id-is-valid}.
  @see-class{g-application}
  @see-fun{g-application-id-is-valid}")

;;; --- g-application-flags ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "flags" 'g-application) 't)
 "The @code{flags} property of type @symbol{g-application-flags}
  (Read / Write) @br{}
  Flags specifying the behaviour of the application.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-flags atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-flags 'function)
 "@version{2020-2-2}
  @syntax[]{(g-application-flags object) => flags}
  @syntax[]{(setf (g-application-flags object) flags)}
  @argument[object]{a @class{g-application} object}
  @argument[flags]{the flags of type @symbol{g-application-flags} for the
    application}
  @begin{short}
    Accessor of the @slot[g-application]{flags} slot of the
    @class{g-application} class.
  @end{short}

  The slot access function @sym{g-application-flags} gets the flags for the
  application. The slot access function @sym{(setf g-application-flags)}
  sets the flags for the application.

  The flags can only be modified if application has not yet been registered.
  See the @symbol{g-application-flags} flags.
  @see-class{g-application}
  @see-symbol{g-application-flags}")

;;; --- g-application-inactivity-timeout ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inactivity-timeout"
                                               'g-application) 't)
 "The @code{inactivity-timeout} property of type @code{:uint}
  (Read / Write) @br{}
  Time in milliseconds to stay alive after becoming idle. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-inactivity-timeout atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-inactivity-timeout 'function)
 "@version{2020-2-2}
  @syntax[]{(g-application-inactivity-timeout object) => timeout}
  @syntax[]{(setf (g-application-inactivity-timeout object) timeout)}
  @argument[object]{a @class{g-application} object}
  @argument[timeout]{an @code{:uint} with the timeout in milliseconds}
  @begin{short}
    Accessor of the @slot[g-application]{inactivity-timeout} slot of the
    @class{g-application} class.
  @end{short}

  The slot access function @sym{g-application-inactivity-timeout} gets the
  current inactivity timeout for the application. The slot access function
  @sym{(setf g-application-inactivity-timeout)} sets the current inactivity
  timeout for the application.

  This is the amount of time in milliseconds after the last call to the
  function @fun{g-application-release} before the application stops running.

  This call has no side effects of its own. The value set here is only used
  for next time the function @fun{g-application-release} drops the use count to
  zero. Any timeouts currently in progress are not impacted.
  @see-class{g-application}
  @see-function{g-application-release}")

;;; --- g-application-is-busy --------------------------------------------------

#+(and glib-2-44 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "is-busy" 'g-application) 't)
 "The @code{is-busy} property of type @code{:boolean} (Read) @br{}
  Whether the application is currently marked as busy through the functions
  @fun{g-application-mark-busy} or @fun{g-application-bind-busy-property}.
  Since 2.44 @br{}
  Default value: @arg{false}")

#+(and glib-2-44 cl-cffi-gtk-documentation)
(setf (gethash 'g-application-is-busy atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-is-busy 'function)
 "@version{2020-2-1}
  @syntax[]{(g-application-is-busy object) => is-busy}
  @argument[object]{a @class{g-application} object}
  @return{@arg{True} if the application is currenty marked as busy.}
  @begin{short}
    Accessor of the @slot[g-application]{is-busy} slot of the
    @class{g-application} class.
  @end{short}

  Gets the application's current busy state, as set through the functions
  @fun{g-application-mark-busy} or @fun{g-application-bind-busy-property}.

  Since 2.44
  @see-class{g-application}
  @see-function{g-application-mark-busy}
  @see-function{g-application-bind-busy-property}")

;;; --- g-application-is-registered --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-registered"
                                               'g-application) 't)
 "The @code{is-registered} property of type @code{:boolean} (Read) @br{}
  If the function @fun{g-application-register} has been called. @br{}
  Default value: @arg{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-is-registered atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-is-registered 'function)
 "@version{2020-2-2}
  @syntax[]{(g-application-is-registered object) => is-registered}
  @argument[object]{a @class{g-application} object}
  @return{@arg{True} if the application is registered.}
  @begin{short}
    Accessor of the @slot[g-application]{is-registered} slot of the
    @class{g-application} class.
  @end{short}

  Checks if the application is registered. An application is registered if the
  function @fun{g-application-register} has been successfully called.
  @see-class{g-application}
  @see-function{g-application-is-registered}")

;;; --- g-application-is-remote ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-remote" 'g-application) 't)
 "The @code{is-remote} property of type @code{:boolean} (Read) @br{}
  If this application instance is remote. @br{}
  Default value: @arg{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-is-remote atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-is-remote 'function)
 "@version{2020-2-2}
  @syntax[]{(g-application-is-remote object) => is-remote}
  @argument[object]{a @class{g-application} object}
  @return{@arg{True} if the application is remote.}
  @begin{short}
    Accessor of the @slot[g-application]{is-remote} slot of the
    @class{g-application} class.
  @end{short}

  Checks if the application is remote. If the application is remote then it
  means that another instance of the application already exists, the 'primary'
  instance. Calls to perform actions on the application will result in the
  actions being performed by the primary instance.

  The value of this property cannot be accessed before the function
  @fun{g-application-register} has been called. See the function
  @fun{g-application-is-registered}.
  @see-class{g-application}
  @see-function{g-applicatoin-is-registered}")

;;; --- g-application-resource-base-path ---------------------------------------

#+(and glib-2-42 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "resource-base-path"
                                               'g-application) 't)
 "The @code{resource-base-path} property of type @code{:string} (Read / Write)
  @br{}
  The base resource path for the application. Since 2.42 @br{}
  Default value: @code{nil}")

#+(and glib-2-42 cl-cffi-gtk-documentation)
(setf (gethash 'g-application-resource-base-path atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-resource-base-path 'function)
 "@version{#2020-2-2}
  @syntax[]{(g-application-is-remote object) => resource-path}
  @syntax[]{(setf (g-application-is-remote object) resource-path)}
  @argument[object]{a @class{g-application} object}
  @argument[resource-path]{a @code{:string} with the resource path to use}
  @begin{short}
    Accessor of the @slot[g-application]{resource-base-path} slot of the
    @class{g-application} class.
  @end{short}

  The slot access function @sym{g-application-resource-base-path} gets the
  resource base path of the application. The slot access function
  @sym{(setf g-application-resource-base-path)} sets or unsets the base resource
  path of the application.

  The path is used to automatically load various application resources such as
  menu layouts and action descriptions. The various types of resources will be
  found at fixed names relative to the given base path.

  By default, the resource base path is determined from the application ID by
  prefixing '/' and replacing each '.' with '/'. This is done at the time that
  the @class{g-application} object is constructed. Changes to the application
  ID after that point will not have an impact on the resource base path.

  As an example, if the application has an ID of \"org.example.app\" then the
  default resource base path will be \"/org/example/app\". If this is a
  @class{gtk-application}, and you have not manually changed the path, then GTK
  will then search for the menus of the application at
  \"/org/example/app/gtk/menus.ui\".

  See @code{GResource} for more information about adding resources to your
  application.

  You can disable automatic resource loading functionality by setting the path
  to @code{nil}.

  Changing the resource base path once the application is running is not
  recommended. The point at which the resource path is consulted for forming
  paths for various purposes is unspecified. When writing a sub-class of
  @class{g-application} you should either set the @code{resource-base-path}
  property at construction time, or call this function during the instance
  initialization. Alternatively, you can call this function in the
  @code{GApplicationClass.startup} virtual function, before chaining up to the
  parent implementation.

  Since 2.42
  @see-class{g-application}")

;;; ----------------------------------------------------------------------------
;;; g_application_id_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_id_is_valid" g-application-id-is-valid) :boolean
 #+cl-cffi-gtk-documentation
 "@version{#2020-2-2}
  @argument[application-id]{a potential application identifier}
  @return{@em{True} if @arg{application-id} is valid.}
  @begin{short}
    Checks if @arg{application-id} is a valid application identifier.
  @end{short}
  A valid ID is required for calls to the functions @fun{g-application-new} and
  @fun{g-application-application-id}.

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
  @end{itemize}
  @begin[Example]{dictionary}
    @begin{pre}
  (g-application-id-is-valid \"com.crategus.application\")
=> T
  (g-application-id-is-valid \"application\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{g-application}
  @see-function{g-application-new}
  @see-function{g-application-application-id}"
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
  @see-class{g-application}
  @see-function{g-application-id-is-valid}"
  (make-instance 'g-application
                 :application-id application-id
                 :flags flags))

(export 'g-application-new)

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
;;; g_application_register ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_register" %g-application-register) :boolean
  (application (g-object g-application))
  (cancellable (g-object g-cancellable))
  (err :pointer))

(defun g-application-register (application cancellable)
 #+cl-cffi-gtk-documentation
 "@version{2020-2-2}
  @argument[application]{a @class{g-application} object}
  @argument[cancellable]{a @code{GCancellable}, or @code{nil}}
  @return{@em{True} if registration succeeded.}
  @begin{short}
    Attempts registration of the application.
  @end{short}
  This is the point at which the application discovers if it is the primary
  instance or merely acting as a remote for an already-existing primary
  instance. This is implemented by attempting to acquire the application
  identifier as a unique bus name on the session bus using GDBus.

  Due to the internal architecture of GDBus, method calls can be dispatched at
  any time, even if a main loop is not running. For this reason, you must
  ensure that any object paths that you wish to register are registered before
  calling this function.

  If the application has already been registered then @arg{true} is returned
  with no work performed.

  The \"startup\" signal is emitted if registration succeeds and application is
  the primary instance.

  In the event of an error, such as @arg{cancellable} being cancelled, or a
  failure to connect to the session bus, @arg{false} is returned and error is
  set appropriately.

  Note: The return value of this function is not an indicator that this
  instance is or is not the primary instance of the application.
  See the function @fun{g-application-is-remote} for that.
  @see-class{g-application}
  @see-function{g-application-is-remote}"
  (with-g-error (err)
    (%g-application-register application cancellable err)))

(export 'g-application-register)

;;; ----------------------------------------------------------------------------
;;; g_application_hold ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_hold" g-application-hold) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-2}
  @argument[application]{a @class{g-application} object}
  @begin{short}
    Increases the use count of the application.
  @end{short}
  Use this function to indicate that the application has a reason to continue
  to run. For example, the function @sym{g-application-hold} is called by GTK+
  when a toplevel window is on the screen.

  To cancel the hold, call the function @fun{g-application-release}.
  @see-class{g-application}
  @see-function{g-application-release}"
  (application (g-object g-application)))

(export 'g-application-hold)

;;; ----------------------------------------------------------------------------
;;; g_application_release ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_release" g-application-release) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-2}
  @argument[application]{a @class{g-application} object}
  @begin{short}
    Decrease the use count of the application.
  @end{short}
  When the use count reaches zero, the application will stop running.

  Never call this function except to cancel the effect of a previous call
  to the function @fun{g-application-hold}.
  @see-class{g-application}
  @see-function{g-application-hold}"
  (application (g-object g-application)))

(export 'g-application-release)

;;; ----------------------------------------------------------------------------
;;; g_application_quit ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_quit" g-application-quit) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-2}
  @argument[application]{a @class{g-application} object}
  @begin{short}
    Immediately quits the application.
  @end{short}
  Upon return to the mainloop, the function @fun{g-application-run} will return,
  calling only the 'shutdown' function before doing so.

  The hold count is ignored.

  The result of calling the function @fun{g-application-run} again after it
  returns is unspecified.
  @see-class{g-application}
  @see-function{g-application-run}"
  (application (g-object g-application)))

(export 'g-application-quit)

;;; ----------------------------------------------------------------------------
;;; g_application_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_activate" g-application-activate) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-2}
  @argument[application]{a @class{g-application} object}
  @begin{short}
    Activates the application.
  @end{short}
  In essence, this results in the \"activate\" signal being emitted in the
  primary instance.

  The application must be registered before calling this function.
  @see-class{g-application}"
  (application (g-object g-application)))

(export 'g-application-activate)

;;; ----------------------------------------------------------------------------
;;; g_application_open ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_open" %g-application-open) :void
  (application (g-object g-application))
  (files g-strv)
  (n-files :int)
  (hint :string))

(defun g-application-open (application files hint)
 #+cl-cffi-gtk-documentation
 "@version{2020-2-2}
  @argument[application]{a @class{g-application} object}
  @argument[files]{a list of strings with the file names}
  @argument[hint]{a string with a hint or an empty string \"\"}
  @begin{short}
    Opens the given files.
  @end{short}
  In essence, this results in the \"open\" signal being emitted in the primary
  instance.

  @arg{hint} is simply passed through to the \"open\" signal. It is intended to
  be used by applications that have multiple modes for opening files, e. g.
  \"view\" vs \"edit\". Unless you have a need for this functionality, you
  should use an empty string \"\".

  The application must be registered before calling this function and it must
  have the @code{:handles-open} flag set.
  @see-class{g-application}"
  (%g-application-open application files (length files) hint))

(export 'g-application-open)

;;; ----------------------------------------------------------------------------
;;; g_application_send_notification ()
;;;
;;; void
;;; g_application_send_notification (GApplication *application,
;;;                                  const gchar *id,
;;;                                  GNotification *notification);
;;;
;;; Sends a notification on behalf of application to the desktop shell. There is
;;; no guarantee that the notification is displayed immediately, or even at all.
;;;
;;; Notifications may persist after the application exits. It will be
;;; D-Bus-activated when the notification or one of its actions is activated.
;;;
;;; Modifying notification after this call has no effect. However, the object
;;; can be reused for a later call to this function.
;;;
;;; id may be any string that uniquely identifies the event for the application.
;;; It does not need to be in any special format. For example, "new-message"
;;; might be appropriate for a notification about new messages.
;;;
;;; If a previous notification was sent with the same id , it will be replaced
;;; with notification and shown again as if it was a new notification. This
;;; works even for notifications sent from a previous execution of the
;;; application, as long as id is the same string.
;;;
;;; id may be NULL, but it is impossible to replace or withdraw notifications
;;; without an id.
;;;
;;; If notification is no longer relevant, it can be withdrawn with
;;; g_application_withdraw_notification().
;;;
;;; application :
;;;     a GApplication
;;;
;;; id :
;;;     id of the notification, or NULL.
;;;
;;; notification :
;;;     the GNotification to send
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_withdraw_notification ()
;;;
;;; void
;;; g_application_withdraw_notification (GApplication *application,
;;;                                      const gchar *id);
;;;
;;; Withdraws a notification that was sent with
;;; g_application_send_notification().
;;;
;;; This call does nothing if a notification with id doesn't exist or the
;;; notification was never sent.
;;;
;;; This function works even for notifications sent in previous executions of
;;; this application, as long id is the same as it was for the sent
;;; notification.
;;;
;;; Note that notifications are dismissed when the user clicks on one of the
;;; buttons in a notification or triggers its default action, so there is no
;;; need to explicitly withdraw the notification in that case.
;;;
;;; application :
;;;     a GApplication
;;;
;;; id :
;;;     id of a previously sent notification
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_run ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_run" %g-application-run) :int
  (application (g-object g-application))
  (argc :int)
  (argv g-strv))

(defun g-application-run (application argv)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-1}
  @argument[application]{a @class{g-application} object}
  @argument[argv]{a list of strings with commandline parameters, or @code{nil}}
  @return{An integer with the exit status.}
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
  arguments that it handled, shifting up remaining arguments. See Example 18,
  \"Split commandline handling\" for an example of parsing @arg{argv} manually.
  Alternatively, you may use the @code{GOptionContext} API.

  The last argument to @code{local_command_line()} is a pointer to the status
  variable which can be used to set the exit status that is returned from the
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
  @see-class{g-application}
  @see-function{g-application-activate}
  @see-function{g-application-open}
  @see-function{g-application-register}"
  (%g-application-run application (length argv) argv))

(export 'g-application-run)

;;; ----------------------------------------------------------------------------
;;; g_application_add_main_option_entries ()
;;;
;;; void
;;; g_application_add_main_option_entries (GApplication *application,
;;;                                       const GOptionEntry *entries);
;;;
;;; Adds main option entries to be handled by application .
;;;
;;; This function is comparable to g_option_context_add_main_entries().
;;;
;;; After the commandline arguments are parsed, the “handle-local-options”
;;; signal will be emitted. At this point, the application can inspect the
;;; values pointed to by arg_data in the given GOptionEntrys.
;;;
;;; Unlike GOptionContext, GApplication supports giving a NULL arg_data for a
;;; non-callback GOptionEntry. This results in the argument in question being
;;; packed into a GVariantDict which is also passed to “handle-local-options”,
;;; where it can be inspected and modified. If
;;; G_APPLICATION_HANDLES_COMMAND_LINE is set, then the resulting dictionary is
;;; sent to the primary instance, where
;;; g_application_command_line_get_options_dict() will return it. This "packing"
;;; is done according to the type of the argument -- booleans for normal flags,
;;; strings for strings, bytestrings for filenames, etc. The packing only occurs
;;; if the flag is given (ie: we do not pack a "false" GVariant in the case that
;;; a flag is missing).
;;;
;;; In general, it is recommended that all commandline arguments are parsed
;;; locally. The options dictionary should then be used to transmit the result
;;; of the parsing to the primary instance, where g_variant_dict_lookup() can be
;;; used. For local options, it is possible to either use arg_data in the usual
;;; way, or to consult (and potentially remove) the option from the options
;;; dictionary.
;;;
;;; This function is new in GLib 2.40. Before then, the only real choice was to
;;; send all of the commandline arguments (options and all) to the primary
;;; instance for handling. GApplication ignored them completely on the local
;;; side. Calling this function "opts in" to the new behaviour, and in
;;; particular, means that unrecognised options will be treated as errors.
;;; Unrecognised options have never been ignored when
;;; G_APPLICATION_HANDLES_COMMAND_LINE is unset.
;;;
;;; If “handle-local-options” needs to see the list of filenames, then the use
;;; of G_OPTION_REMAINING is recommended. If arg_data is NULL then
;;; G_OPTION_REMAINING can be used as a key into the options dictionary. If you
;;; do use G_OPTION_REMAINING then you need to handle these arguments for
;;; yourself because once they are consumed, they will no longer be visible to
;;; the default handling (which treats them as filenames to be opened).
;;;
;;; It is important to use the proper GVariant format when retrieving the
;;; options with g_variant_dict_lookup():
;;;
;;; for G_OPTION_ARG_NONE, use b
;;; for G_OPTION_ARG_STRING, use &s
;;; for G_OPTION_ARG_INT, use i
;;; for G_OPTION_ARG_INT64, use x
;;; for G_OPTION_ARG_DOUBLE, use d
;;; for G_OPTION_ARG_FILENAME, use ^ay
;;; for G_OPTION_ARG_STRING_ARRAY, use &as
;;; for G_OPTION_ARG_FILENAME_ARRAY, use ^aay
;;;
;;; application :
;;;     a GApplication
;;;
;;; entries :
;;;     a NULL-terminated list of GOptionEntrys
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_add_main_option ()
;;;
;;; void
;;; g_application_add_main_option (GApplication *application,
;;;                                const char *long_name,
;;;                                char short_name,
;;;                                GOptionFlags flags,
;;;                                GOptionArg arg,
;;;                                const char *description,
;;;                                const char *arg_description);
;;;
;;; Add an option to be handled by application .
;;;
;;; Calling this function is the equivalent of calling
;;; g_application_add_main_option_entries() with a single GOptionEntry that has
;;; its arg_data member set to NULL.
;;;
;;; The parsed arguments will be packed into a GVariantDict which is passed to
;;; “handle-local-options”. If G_APPLICATION_HANDLES_COMMAND_LINE is set, then
;;; it will also be sent to the primary instance. See
;;; g_application_add_main_option_entries() for more details.
;;;
;;; See GOptionEntry for more documentation of the arguments.
;;;
;;; application :
;;;     the GApplication
;;;
;;; long_name :
;;;     the long name of an option used to specify it in a commandline
;;;
;;; short_name :
;;;     the short name of an option
;;;
;;; flags :
;;;     flags from GOptionFlags
;;;
;;; arg :
;;;     the type of the option, as a GOptionArg
;;;
;;; description :
;;;     the description for the option in --help output
;;;
;;; arg_description :
;;;     the placeholder to use for the extra argument parsed by the option in
;;;     --help output.
;;;
;;; Since 2.42
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_add_option_group ()
;;;
;;; void
;;; g_application_add_option_group (GApplication *application,
;;;                                 GOptionGroup *group);
;;;
;;; Adds a GOptionGroup to the commandline handling of application .
;;;
;;; This function is comparable to g_option_context_add_group().
;;;
;;; Unlike g_application_add_main_option_entries(), this function does not deal
;;; with NULL arg_data and never transmits options to the primary instance.
;;;
;;; The reason for that is because, by the time the options arrive at the
;;; primary instance, it is typically too late to do anything with them. Taking
;;; the GTK option group as an example: GTK will already have been initialised
;;; by the time the “command-line” handler runs. In the case that this is not
;;; the first-running instance of the application, the existing instance may
;;; already have been running for a very long time.
;;;
;;; This means that the options from GOptionGroup are only really usable in the
;;; case that the instance of the application being run is the first instance.
;;; Passing options like --display= or --gdk-debug= on future runs will have no
;;; effect on the existing primary instance.
;;;
;;; Calling this function will cause the options in the supplied option group
;;; to be parsed, but it does not cause you to be "opted in" to the new
;;; functionality whereby unrecognised options are rejected even if
;;; G_APPLICATION_HANDLES_COMMAND_LINE was given.
;;;
;;; application :
;;;     the GApplication
;;;
;;; group :
;;;     a GOptionGroup.
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_set_option_context_parameter_string ()
;;;
;;; void
;;; g_application_set_option_context_parameter_string
;;;                                (GApplication *application,
;;;                                 const gchar *parameter_string);
;;;
;;; Sets the parameter string to be used by the commandline handling of
;;; application .
;;;
;;; This function registers the argument to be passed to g_option_context_new()
;;; when the internal GOptionContext of application is created.
;;;
;;; See g_option_context_new() for more information about parameter_string .
;;;
;;; application :
;;;     the GApplication
;;;
;;; parameter_string :
;;;     a string which is displayed in the first line of --help output, after
;;;     the usage summary programname [OPTION...].
;;;
;;; Since 2.56
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_set_option_context_summary ()
;;;
;;; void
;;; g_application_set_option_context_summary
;;;                                (GApplication *application,
;;;                                 const gchar *summary);
;;;
;;; Adds a summary to the application option context.
;;;
;;; See g_option_context_set_summary() for more information.
;;;
;;; application :
;;;     the GApplication
;;;
;;; summary :
;;;     a string to be shown in --help output before the list of options, or
;;;     NULL.
;;;
;;; Since 2.56
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_set_option_context_description ()
;;;
;;; void
;;; g_application_set_option_context_description
;;;                                (GApplication *application,
;;;                                 const gchar *description);
;;;
;;; Adds a description to the application option context.
;;;
;;; See g_option_context_set_description() for more information.
;;;
;;; application :
;;;     the GApplication
;;;
;;; description :
;;;     a string to be shown in --help output after the list of options, or
;;;     NULL.
;;;
;;; Since 2.56
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_set_default ()
;;; g_application_get_default () -> g-application-default
;;; ----------------------------------------------------------------------------

(defun (setf g-application-default) (application)
  (foreign-funcall "g_application_set_default"
                   (g-object g-application) application
                   :void)
  application)

(defcfun ("g_application_get_default" g-application-default)
    (g-object g-application)
 #+cl-cffi-gtk-documentation
 "@version{*2020-5-28}
  @syntax[]{(g-application-default) => application}
  @syntax[]{(setf (g-application-default) application)}
  @argument[application]{the @class{g-application} object to set as default,
    or @code{nil}}
  @begin{short}
    Accessor of the default application for the process.
  @end{short}

  The function @sym{g-application-default} returns the default application
  instance for this process. The function @sym{(setf g-application-default)}
  sets or unsets the default application for the process.

  This function does not take its own reference on application. If application
  is destroyed then the default application will revert back to @code{nil}.

  Normally there is only one application per process and it becomes the default
  when it is created. You can exercise more control over this by using this
  function.

  If there is no default application then @code{nil} is returned.
  @see-class{g-application}")

(export 'g-application-default)

;;; ----------------------------------------------------------------------------
;;; g_application_mark_busy ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_mark_busy" g-application-mark-busy) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-28}
  @argument[application]{a @class{g-application} object}
  @begin{short}
    Increases the busy count of the application.
  @end{short}

  Use this function to indicate that the application is busy, for instance
  while a long running operation is pending.

  The busy state will be exposed to other processes, so a session shell will
  use that information to indicate the state to the user, e. g. with a
  spinner.

  To cancel the busy indication, use the function
  @fun{g-application-unmark-busy}.
  @see-class{g-application}
  @see-function{g-application-unmark-busy}"
  (application (g-object g-application)))

(export 'g-application-mark-busy)

;;; ----------------------------------------------------------------------------
;;; g_application_unmark_busy ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_unmark_busy" g-application-unmark-busy) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-28}
  @argument[application]{a @class{g-application} object}
  @begin{short}
    Decreases the busy count of the application.
  @end{short}

  When the busy count reaches zero, the new state will be propagated to other
  processes.

  This function must only be called to cancel the effect of a previous call
  to the function @fun{g-application-mark-busy}.
  @see-class{g-application}
  @see-function{g-application-mark-busy}"
  (application (g-object g-application)))

(export 'g-application-unmark-busy)

;;; ----------------------------------------------------------------------------
;;; g_application_bind_busy_property ()
;;;
;;; void
;;; g_application_bind_busy_property (GApplication *application,
;;;                                   gpointer object,
;;;                                   const gchar *property);
;;;
;;; Marks application as busy (see g_application_mark_busy()) while property on
;;; object is TRUE.
;;;
;;; The binding holds a reference to application while it is active, but not to
;;; object . Instead, the binding is destroyed when object is finalized.
;;;
;;; application :
;;;     a GApplication
;;;
;;; object :
;;;     a GObject.
;;;
;;; property :
;;;     the name of a boolean property of object
;;;
;;; Since 2.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_unbind_busy_property ()
;;;
;;; void
;;; g_application_unbind_busy_property (GApplication *application,
;;;                                     gpointer object,
;;;                                     const gchar *property);
;;;
;;; Destroys a binding between property and the busy state of application that
;;; was previously created with g_application_bind_busy_property().
;;;
;;; application :
;;;     a GApplication
;;;
;;; object :
;;;     a GObject.
;;;
;;; property :
;;;     the name of a boolean property of object
;;;
;;; Since 2.44
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.application.lisp ---------------------------------------
