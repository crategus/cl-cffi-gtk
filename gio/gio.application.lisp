;;; ----------------------------------------------------------------------------
;;; gio.application.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
  #+glib-2-60
  (:allow-replacement 128)
  #+glib-2-60
  (:replace 256)
)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-flags atdoc:*symbol-name-alias*)
      "GFlags"
      (gethash 'g-application-flags atdoc:*external-symbols*)
 "@version{2021-7-27}
  @begin{short}
    Flags used to define the behaviour of a @class{g-application} instance.
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
    @entry[:none]{Default.}
    @entry[:is-service]{Run as a service. In this mode, registration fails if
      the service is already running, and the application will initially wait
      up to 10 seconds for an initial activation message to arrive.}
    @entry[:is-launcher]{Do not try to become the primary instance.}
    @entry[:handles-open]{This application handles opening files in the primary
      instance. Note that this flag only affects the default implementation of
      the virtual function @code{local_command_line()}, and has no effect if
      the @code{:handles-command-line} flag is given. See the function
      @fun{g-application-run} for details.}
    @entry[:handles-command-line]{This application handles command line
      arguments in the primary instance. Note that this flag only affect the
      default implementation of the virtual function
      @code{local_command_line()}. See the function @fun{g-application-run} for
      details.}
    @entry[:send-enviroment]{Send the environment of the launching process to
      the primary instance. Set this flag if your application is expected to
      behave differently depending on certain environment variables. For
      instance, an editor might be expected to use the @code{GIT_COMMITTER_NAME}
      environment variable when editing a GIT commit message. The environment
      is available to the \"command-line\" signal handler, via the function
      @fun{g-application-command-line-getenv}.}
    @entry[:non-unique]{Make no attempts to do any of the typical
      single-instance application negotiation. The application neither attempts
      to become the owner of the application ID nor does it check if an
      existing owner already exists. Everything occurs in the local process.}
    @entry[:can-override-app-id]{Allow users to override the application ID
      from the command line with the option @code{--gapplication-app-id}.}
    @entry[:allow-replacement]{Allow another instance to take over the bus
      name. Since 2.60}
    @entry[:replace]{Take over from another instance. This flag is usually set
      by passing the option @code{--gapplication-replace} on the command line.
      Since 2.60}
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
   (is-busy
    g-application-is-busy
    "is-busy" "gboolean" t nil)
   (is-registered
    g-application-is-registered
    "is-registered" "gboolean" t nil)
   (is-remote
    g-application-is-remote
    "is-remote" "gboolean" t nil)
   (resource-base-path
    g-application-resource-base-bath
    "resource-base-path" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-application 'type)
 "@version{2021-7-27}
  @begin{short}
    The @sym{g-application} class is the foundation of an application.
  @end{short}
  The @sym{g-application} class wraps some low-level platform-specific services
  and is intended to act as the foundation for higher-level application classes
  such as the @class{gtk-application} class. In general, you should not use this
  class outside of a higher level framework.

  The @sym{g-application} class provides convenient life cycle management by
  maintaining a \"use count\" for the primary application instance. The use
  count can be changed using the functions @fun{g-application-hold} and
  @fun{g-application-release}. If it drops to zero, the application exits.
  Higher-level classes such as the @class{gtk-application} class employ the use
  count to ensure that the application stays alive as long as it has any opened
  windows.

  Another feature that the @sym{g-application} class, optionally, provides is
  process uniqueness. Applications can make use of this functionality by
  providing a unique application ID. If given, only one application with this
  ID can be running at a time per session. The session concept is platform
  dependent, but corresponds roughly to a graphical desktop login. When your
  application is launched again, its arguments are passed through platform
  communication to the already running program. The already running instance of
  the program is called the \"primary instance\". For non-unique applications
  this is always the current instance. On Linux, the D-Bus session bus is used
  for communication.

  The use of the @sym{g-application} class differs from some other commonly
  used uniqueness libraries, such as @code{libunique}, in important ways. The
  application is not expected to manually register itself and check if it is the
  primary instance. Instead, the @code{main()} function of a @sym{g-application}
  instance should do very little more than instantiating the application
  instance, possibly connecting signal handlers, then calling the function
  @fun{g-application-run}. All checks for uniqueness are done internally. If the
  application is the primary instance then the startup signal is emitted and the
  mainloop runs. If the application is not the primary instance then a signal is
  sent to the primary instance and the function @fun{g-application-run} promptly
  returns. See the code examples below.

  If used, the expected form of an application identifier is the same as that of
  of a D-Bus well-known bus name. Examples include: @code{com.example.MyApp},
  @code{org.example.apps.Calculator}, @code{org._7_zip.Archiver}. For details
  on valid application identifiers, see the function
  @fun{g-application-id-is-valid}.

  On Linux, the application identifier is claimed as a well-known bus name on
  the session bus of the user. This means that the uniqueness of the application
  is scoped to the current session. It also means that the application may
  provide additional services, through registration of other object paths, at
  that bus name. The registration of these object paths should be done with the
  shared GDBus session bus. Note that due to the internal architecture of GDBus,
  method calls can be dispatched at any time, even if a main loop is not
  running. For this reason, you must ensure that any object paths that you wish
  to register are registered before a @sym{g-application} instance attempts to
  acquire the bus name of your application, which happens in the function
  @fun{g-application-register}. Unfortunately, this means that you cannot use
  the function @fun{g-application-is-remote} to decide if you want to register
  object paths.

  The @sym{g-application} class also implements the @class{g-action-group} and
  @class{g-action-map} interfaces and lets you easily export actions by adding
  them with the function @fun{g-action-map-add-action}. When invoking an action
  by calling the function @fun{g-action-group-activate-action} on the
  application, it is always invoked in the primary instance. The actions are
  also exported on the session bus, and GIO provides the @code{GDBusActionGroup}
  wrapper to conveniently access them remotely. GIO provides a
  @code{GDBusMenuModel} wrapper for remote access to exported
  @class{g-menu-model} objects.

  There is a number of different entry points into a @sym{g-application}
  instance:
  @begin{itemize}
    @item{via 'Activate' (i.e. just starting the application)}
    @item{via 'Open' (i.e. opening some files)}
    @item{by handling a command-line}
    @item{via activating an action}
  @end{itemize}
  The \"startup\" signal lets you handle the application initialization for all
  of these in a single place.

  Regardless of which of these entry points is used to start the application,
  the @sym{g-application} instance passes some platform data from the launching
  instance to the primary instance, in the form of a @type{g-variant}
  dictionary mapping strings to variants. To use platform data, override the
  virtual functions @code{before_emit} or @code{after_emit} in your
  @sym{g-application} subclass. When dealing with
  @class{g-application-command-line} objects, the platform data is directly
  available via the functions @fun{g-application-command-line-cwd},
  @fun{g-application-command-line-environ} and
  @fun{g-application-command-line-get-platform-data}.

  As the name indicates, the platform data may vary depending on the operating
  system, but it always includes the current directory, key \"cwd\", and
  optionally the environment, i.e. the set of environment variables and their
  values, of the calling process, key \"environ\". The environment is only
  added to the platform data if the @code{:send-enviroment} flag is set. A
  @sym{g-application} subclass can add own platform data by overriding
  the virtual function @code{add_platform_data}. For instance, the
  @class{gtk-application} class adds startup notification data in this way.

  To parse command line arguments you may handle the \"command-line\" signal or
  override the virtual function @code{local_command_line()}, to parse them in
  either the primary instance or the local instance, respectively.
  @begin[Examples]{dictionary}
    An example to show the use of the signals of an application.
    @begin{pre}
(defun application-open (&optional (argv nil))
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
                          (format t \" n-files : ~A~%\" n-files)
                          (format t \"    hint : ~A~%\" hint)
                          ;; The argument FILES is a C pointer to an array of
                          ;; GFile objects. We list the pathnames of the files.
                          (dotimes (i n-files)
                            (let ((file (mem-aref files '(g-object g-file) i)))
                              (format t \" ~a~%\" (g-file-path file))))))
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
  (let ((param-type (g-action-group-action-parameter-type app name))
        (state (g-action-group-action-state app name))
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

(defun application-action (&optional (argv nil))
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
 lambda (application)    :run-last
      @end{pre}
      The signal is emitted on the primary instance when an activation occurs.
      See the function @fun{g-application-activate}.
      @begin[code]{table}
        @entry[application]{The @sym{g-application} instance which received the
          signal.}
      @end{table}
    @subheading{The \"command-line\" signal}
      @begin{pre}
 lambda (application cmdline)    :run-last
      @end{pre}
      The signal is emitted on the primary instance when a command line is not
      handled locally. See the function @fun{g-application-run} and the
      @class{g-application-command-line} documentation for more information.
      @begin[code]{table}
        @entry[application]{The @sym{g-application} instance which received the
          signal.}
        @entry[cmdline]{A @class{g-application-command-line} object representing
          the passed command line.}
        @entry[Returns]{An integer that is set as the exit status for the
          calling process.}
      @end{table}
    @subheading{The \"handle-local-options\" signal}
      @begin{pre}
 lambda (application options)    :run-last
      @end{pre}
      The signal is emitted on the local instance after the parsing of the
      command line options has occurred. You can add options to be recognised
      during command line option parsing using the functions
      @fun{g-application-add-main-option-entries} and
      @fun{g-application-add-option-group}.

      Signal handlers can inspect options, along with values pointed to from
      the @code{arg-data} of an installed option entry, in order to decide to
      perform certain actions, including direct local handling, which may be
      useful for options like @code{--version}.

      In the event that the application is marked with the
      @code{:handles-command-line} flag the \"normal processing\" will send the
      options dictionary to the primary instance where it can be read with the
      function @fun{g-application-command-line-options-dict}. The signal
      handler can modify the dictionary before returning, and the modified
      dictionary will be sent.

      In the event that the @code{:handles-command-line} flag is not set,
      \"normal processing\" will treat the remaining uncollected command line
      arguments as filenames or URIs. If there are no arguments, the application
      is activated by the function @fun{g-application-activate}. One or more
      arguments results in a call to the function @fun{g-application-open}.

      If you want to handle the local command line arguments for yourself by
      converting them to calls to the functions @fun{g-application-open} or
      @fun{g-action-group-activate-action} then you must be sure to register
      the application first. You should probably not call the function
      @fun{g-application-activate} for yourself, however, just return -1 and
      allow the default handler to do it for you. This will ensure that the
      @code{--gapplication-service} switch works properly, i.e. no activation
      in that case.

      Note that this signal is emitted from the default implementation of the
      virtual function @code{local_command_line()}. If you override that
      function and do not chain up then this signal will never be emitted.

      You can override the virtual function @code{local_command_line()} if you
      need more powerful capabilities than what is provided here, but this
      should not normally be required.
      @begin[code]{table}
        @entry[application]{The @sym{g-application} instance which received the
          signal.}
        @entry[options]{The options dictionary of type @class{g-variant-dict}.}
        @entry[Returns]{An exit code. If you have handled your options and want
          to exit the process, return a non-negative option, 0 for success, and
          a positive value for failure. Return -1 to let the default option
          processing continue.}
      @end{table}
    @subheading{The \"name-lost\" signal}
      @begin{pre}
 lambda (application)    :run-last
      @end{pre}
      The signal is emitted only on the registered primary instance when a new
      instance has taken over. This can only happen if the application is using
      the @code{:allow-replacement} flag. The default handler for this signal
      calls the function @fun{g-application-quit}. Since 2.60.
      @begin[code]{table}
        @entry[application]{The @sym{g-application} instance which received the
          signal.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{table}
    @subheading{The \"open\" signal}
      @begin{pre}
 lambda (application files n-files hint)    :run-last
      @end{pre}
      The signal is emitted on the primary instance when there are files to
      open. See the function @fun{g-application-open} for more information.
      @begin[code]{table}
        @entry[application]{The @sym{g-application} instance which received the
          signal.}
        @entry[files]{A C array of @class{g-file} objects.}
        @entry[n-files]{An integer with the length of @arg{files}.}
        @entry[hint]{A string with a hint provided by the calling instance.}
      @end{table}
    @subheading{The \"shutdown\" signal}
      @begin{pre}
 lambda (application)    :run-last
      @end{pre}
      The signal is emitted only on the registered primary instance immediately
      after the main loop terminates.
      @begin[code]{table}
        @entry[application]{The @sym{g-application} instance which received the
          signal.}
      @end{table}
    @subheading{The \"startup\" signal}
      @begin{pre}
 lambda (application)    :run-first
      @end{pre}
      The signal is emitted on the primary instance immediately after
      registration. See the function @fun{g-application-register}.
      @begin[code]{table}
        @entry[application]{The @sym{g-application} instance which received the
          signal.}
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
  @em{Warning:} The @code{action-group} property is deprecated since version
  2.32. Use the @class{g-action-map} interface instead.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-action-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-action-group 'function)
 "@version{2021-7-27}
  @syntax[]{(setf (g-application-action-group object) group)}
  @argument[object]{a @class{g-application} instance}
  @argument[group]{a @class{g-action-group} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[g-application]{action-group} slot of the
    @class{g-application} class.
  @end{short}

  This used to be how actions were associated with a @class{g-application}
  instance. Now there is the @class{g-action-map} interface for that.
  @begin[Warning]{dictionary}
    The function @sym{g-application-action-group} has been deprecated since
    version 2.32 and should not be used in newly written code. Use the
    @class{g-action-map} interface instead. Never ever mix use of this API with
    use of the @class{g-action-map} interface on the same application or things
    will go very badly wrong. This function is known to introduce buggy
    behaviour, i.e. signals not emitted on changes to the action group.
  @end{dictionary}
  @see-class{g-application}
  @see-class{g-action-group}
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
 "@version{2027-7-27}
  @syntax[]{(g-application-application-id object) => applicaton-id}
  @syntax[]{(setf (g-application-application-id object) application-id)}
  @argument[object]{a @class{g-application} instance}
  @argument[application-id]{a string with the identifier of the application}
  @begin{short}
    Accessor of the @slot[g-application]{application-id} slot of the
    @class{g-application} class.
  @end{short}

  The slot access function @sym{g-application-application-id} gets the unique
  identifier for the application. The slot access function
  @sym{(setf g-application-application-id)} sets the unique identifier.

  The application ID can only be modified if the application has not yet been
  registered. The application ID must be valid. See the function
  @fun{g-application-id-is-valid}.
  @see-class{g-application}
  @see-function{g-application-id-is-valid}")

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
 "@version{2021-7-27}
  @syntax[]{(g-application-flags object) => flags}
  @syntax[]{(setf (g-application-flags object) flags)}
  @argument[object]{a @class{g-application} instance}
  @argument[flags]{the @symbol{g-application-flags} flags for the application}
  @begin{short}
    Accessor of the @slot[g-application]{flags} slot of the
    @class{g-application} class.
  @end{short}

  The slot access function @sym{g-application-flags} gets the flags for the
  application. The slot access function @sym{(setf g-application-flags)}
  sets the flags.

  The flags can only be modified if the application has not yet been registered.
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
 "@version{2021-7-27}
  @syntax[]{(g-application-inactivity-timeout object) => timeout}
  @syntax[]{(setf (g-application-inactivity-timeout object) timeout)}
  @argument[object]{a @class{g-application} instance}
  @argument[timeout]{an unsigned integer with the timeout in milliseconds}
  @begin{short}
    Accessor of the @slot[g-application]{inactivity-timeout} slot of the
    @class{g-application} class.
  @end{short}

  The slot access function @sym{g-application-inactivity-timeout} gets the
  current inactivity timeout for the application. The slot access function
  @sym{(setf g-application-inactivity-timeout)} sets the inactivity timeout.

  This is the amount of time in milliseconds after the last call to the
  function @fun{g-application-release} before the application stops running.

  This call has no side effects of its own. The value set here is only used
  for next time the function @fun{g-application-release} drops the use count to
  zero. Any timeouts currently in progress are not impacted.
  @see-class{g-application}
  @see-function{g-application-release}")

;;; --- g-application-is-busy --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-busy" 'g-application) 't)
 "The @code{is-busy} property of type @code{:boolean} (Read) @br{}
  Whether the application is currently marked as busy. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-is-busy atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-is-busy 'function)
 "@version{2021-7-27}
  @syntax[]{(g-application-is-busy object) => is-busy}
  @argument[object]{a @class{g-application} instance}
  @argument[is-busy]{@em{true} if the application is currenty marked as busy}
  @begin{short}
    Accessor of the @slot[g-application]{is-busy} slot of the
    @class{g-application} class.
  @end{short}

  Gets the current busy state of the application, as set through the functions
  @fun{g-application-mark-busy} or @fun{g-application-bind-busy-property}.
  @see-class{g-application}
  @see-function{g-application-mark-busy}
  @see-function{g-application-bind-busy-property}")

;;; --- g-application-is-registered --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-registered"
                                               'g-application) 't)
 "The @code{is-registered} property of type @code{:boolean} (Read) @br{}
  Whether the application is registered. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-is-registered atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-is-registered 'function)
 "@version{2021-7-27}
  @syntax[]{(g-application-is-registered object) => is-registered}
  @argument[object]{a @class{g-application} instance}
  @argument[is-registered]{@em{true} if the application is registered}
  @begin{short}
    Accessor of the @slot[g-application]{is-registered} slot of the
    @class{g-application} class.
  @end{short}

  Checks if the application is registered. An application is registered if the
  function @fun{g-application-register} has been successfully called.
  @see-class{g-application}
  @see-function{g-application-register}")

;;; --- g-application-is-remote ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-remote" 'g-application) 't)
 "The @code{is-remote} property of type @code{:boolean} (Read) @br{}
  Whether the application is remote. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-is-remote atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-is-remote 'function)
 "@version{2021-7-27}
  @syntax[]{(g-application-is-remote object) => is-remote}
  @argument[object]{a @class{g-application} instance}
  @argument[is-remote]{@em{true} if the application is remote}
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
  @see-function{g-application-register}
  @see-function{g-applicatoin-is-registered}")

;;; --- g-application-resource-base-path ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resource-base-path"
                                               'g-application) 't)
 "The @code{resource-base-path} property of type @code{:string} (Read / Write)
  @br{}
  The base resource path for the application. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-resource-base-path atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-resource-base-path 'function)
 "@version{2021-7-27}
  @syntax[]{(g-application-is-remote object) => path}
  @syntax[]{(setf (g-application-is-remote object) path)}
  @argument[object]{a @class{g-application} instance}
  @argument[path]{a string with the resource base path to use}
  @begin{short}
    Accessor of the @slot[g-application]{resource-base-path} slot of the
    @class{g-application} class.
  @end{short}

  The slot access function @sym{g-application-resource-base-path} gets the
  resource base path of the application. The slot access function
  @sym{(setf g-application-resource-base-path)} sets or unsets the resource base
  path.

  The resource base path is used to automatically load various application
  resources such as menu layouts and action descriptions. The various types of
  resources will be found at fixed names relative to the given resource base
  path.

  By default, the resource base path is determined from the application ID by
  prefixing '/' and replacing each '.' with '/'. This is done at the time that
  the @class{g-application} instance is constructed. Changes to the application
  ID after that point will not have an impact on the resource base path.

  As an example, if the application has an ID of \"org.example.app\" then the
  default resource base path will be \"/org/example/app\". If this is a
  @class{gtk-application} instance, and you have not manually changed the
  resource base path, then GTK will then search for the menus of the application
  at \"/org/example/app/gtk/menus.ui\".

  See the @class{g-resource} documentation for more information about adding
  resources to your application.

  You can disable automatic resource loading functionality by setting the
  resource base path to @code{nil}.

  Changing the resource base path once the application is running is not
  recommended. The point at which the resource base path is consulted for
  forming paths for various purposes is unspecified. When writing a subclass
  of the @class{g-application} class you should either set the
  @slot[g-application]{resource-base-path} property at construction time, or
  call this function during the instance initialization.
  @see-class{g-application}
  @see-class{gtk-application}")

;;; ----------------------------------------------------------------------------
;;; g_application_id_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_id_is_valid" g-application-id-is-valid) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-27}
  @argument[application-id]{a string with a potential application identifier}
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
    @item{Application identifiers must contain at least one '.' period
      character and thus at least three elements.}
    @item{Application identifiers must not begin or end with a '.' period
      character.}
    @item{Application identifiers must not contain consecutive '.' period
      characters.}
    @item{Application identifiers must not exceed 255 characters.}
  @end{itemize}
  @begin[Example]{dictionary}
    @begin{pre}
(g-application-id-is-valid \"com.crategus.application\") => T
(g-application-id-is-valid \"application\") => NIL
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
 "@version{2021-7-27}
  @argument[application-id]{a string with the application ID}
  @argument[flags]{the @symbol{g-application-flags} application flags}
  @return{A new @class{g-application} instance.}
  @begin{short}
    Creates a new @class{g-application} instance.
  @end{short}
  The application ID must be valid. See the function
  @fun{g-application-id-is-valid}.
  @see-class{g-application}
  @see-symbol{g-application-flags}
  @see-function{g-application-id-is-valid}"
  (make-instance 'g-application
                 :application-id application-id
                 :flags flags))

(export 'g-application-new)

;;; ----------------------------------------------------------------------------
;;; g_application_get_dbus_connection ()
;;;
;;; GDBusConnection *
;;; g_application_get_dbus_connection (GApplication *application);
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
;;; const gchar *
;;; g_application_get_dbus_object_path (GApplication *application);
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
  (cancellable :pointer)
  (err :pointer))

(defun g-application-register (application)
 #+cl-cffi-gtk-documentation
 "@version{2027-7-27}
  @argument[application]{a @class{g-application} instance}
  @return{@em{True} if registration succeeded.}
  @begin{short}
    Attempts registration of the application.
  @end{short}
  This is the point at which the application discovers if it is the primary
  instance or merely acting as a remote for an already existing primary
  instance. This is implemented by attempting to acquire the application
  identifier as a unique bus name on the session bus using GDBus.

  Due to the internal architecture of GDBus, method calls can be dispatched at
  any time, even if a main loop is not running. For this reason, you must
  ensure that any object paths that you wish to register are registered before
  calling this function.

  If the application has already been registered then @em{true} is returned
  with no work performed. The \"startup\" signal is emitted if registration
  succeeds and the application is the primary instance. In the event of an
  error @em{false} is returned.
  @begin[Note]{dictionary}
    The return value of this function is not an indicator that this instance is
    or is not the primary instance of the application. See the function
    @fun{g-application-is-remote} for that.
  @end{dictionary}
  @see-class{g-application}
  @see-function{g-application-is-remote}"
  (with-g-error (err)
    (%g-application-register application (null-pointer) err)))

(export 'g-application-register)

;;; ----------------------------------------------------------------------------
;;; g_application_hold ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_hold" g-application-hold) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-27}
  @argument[application]{a @class{g-application} instance}
  @begin{short}
    Increases the use count of the application.
  @end{short}
  Use this function to indicate that the application has a reason to continue
  to run. For example, the function @sym{g-application-hold} is called by GTK
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
 "@version{2021-7-27}
  @argument[application]{a @class{g-application} instance}
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
 "@version{2021-7-27}
  @argument[application]{a @class{g-application} instance}
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
 "@version{2021-7-27}
  @argument[application]{a @class{g-application} instance}
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

;; TODO: The argument FILES is wrong. It must be of type GFile**.

(defcfun ("g_application_open" %g-application-open) :void
  (application (g-object g-application))
  (files g-strv)
  (n-files :int)
  (hint :string))

(defun g-application-open (application files hint)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-27}
  @argument[application]{a @class{g-application} instance}
  @argument[files]{a list of strings with the file names}
  @argument[hint]{a string with a hint or an empty string \"\"}
  @begin{short}
    This results in the \"open\" signal being emitted in the primary instance.
  @end{short}

  The argument @arg{hint} is simply passed through to the \"open\" signal. It
  is intended to be used by applications that have multiple modes for opening
  files, e.g. \"view\" vs \"edit\". Unless you have a need for this
  functionality, you should use an empty string \"\".

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
;;; This call does nothing if a notification with id does not exist or the
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
 "@version{2021-7-27}
  @argument[application]{a @class{g-application} instance}
  @argument[argv]{a list of strings with command line parameters, or @code{nil}}
  @return{An integer with the exit status.}
  @begin{short}
    Runs the application.
  @end{short}
  This function is intended to be run from @code{main()} and its return value
  is intended to be returned by @code{main()}. Although you are expected to
  pass the @code{argc}, @code{argv} parameters from @code{main()} to this
  function, it is possible to pass @code{nil} if @code{argv} is not available
  or command line handling is not required. Note that on Windows, @code{argc}
  and @code{argv} are ignored, and the function
  @code{g_win32_get_command_line()} is called internally, for proper support of
  Unicode command line arguments.

  The @class{g-application} instance will attempt to parse the command line
  arguments. You can add command line flags to the list of recognised options by
  way of the function @fun{g-application-add-main-option-entries}. After this,
  the \"handle-local-options\" signal is emitted, from which the application
  can inspect the values of its option entries.

  The \"handle-local-options\" signal handler is a good place to handle options
  such as @code{--version}, where an immediate reply from the local process is
  desired, instead of communicating with an already-running instance. A
  \"handle-local-options\" signal handler can stop further processing by
  returning a non-negative value, which then becomes the exit status of the
  process.

  What happens next depends on the @symbol{g-application-flags} flags: if the
  @code{:handles-command-line} flag was specified then the remaining command
  line arguments are sent to the primary instance, where a \"command-line\"
  signal is emitted. Otherwise, the remaining command line arguments are assumed
  to be a list of files. If there are no files listed, the application is
  activated via the \"activate\" signal. If there are one or more files, and the
  @code{:handles-open} flag was specified then the files are opened via the
  \"open\" signal.

  If you are interested in doing more complicated local handling of the
  command line then you should implement your own @class{g-application} subclass
  and override the virtual function @code{local_command_line()}. In this case,
  you most likely want to return @em{true} from your @code{local_command_line()}
  implementation to suppress the default handling.

  If, after the above is done, the use count of the application is zero then
  the exit status is returned immediately. If the use count is non-zero then
  the default main context is iterated until the use count falls to zero, at
  which point 0 is returned.

  If the @code{:is-service} flag is set, then the service will run for as much
  as 10 seconds with a use count of zero while waiting for the message that
  caused the activation to arrive. After that, if the use count falls to zero
  the application will exit immediately, except in the case that the function
  @fun{g-application-inactivity-timeout} is in use.

  This function sets the program name with the function @fun{g-prgname}, if not
  already set, to the basename of @code{argv[0]}.

  Much like the function @fun{g-main-loop-run}, this function will acquire the
  main context for the duration that the application is running.

  Applications that are not explicitly flagged as services or launchers, i.e.
  neither the @code{:is-service} or the @code{:is-launcher} are given as flags,
  will check, from the default handler for the virtual function
  @code{local_command_line}, if the @code{--gapplication-service} option was
  given in the command line. If this flag is present then normal command line
  processing is interrupted and the @code{:is-service} flag is set. This
  provides a \"compromise\" solution whereby running an application directly
  from the command line will invoke it in the normal way, which can be useful
  for debugging. while still allowing applications to be D-Bus activated in
  service mode. The D-Bus service file should invoke the executable with the
  @code{--gapplication-service} option as the sole command line argument. This
  approach is suitable for use by most graphical applications but should not be
  used from applications like editors that need precise control over when
  processes invoked via the command line will exit and what their exit status
  will be.
  @see-class{g-application}
  @see-symbol{g-application-flags}
  @see-function{g-prgname}
  @see-function{g-main-loop}
  @see-function{g-application-inactivity-timeout}
  @see-function{g-application-add-main-option-entries}"
  (%g-application-run application (length argv) argv))

(export 'g-application-run)

;;; ----------------------------------------------------------------------------
;;; g_application_add_main_option_entries ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_add_main_option_entries"
          %g-application-add-main-option-entries) :void
  (application (g-object g-application))
  (entries :pointer (:pointer (:struct glib::g-option-entry))))

;; TODO: This code duplicates the implementation of g-option-group-add-entries.

(defun g-application-add-main-option-entries (application entries)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[application]{a @class{g-application} instance}
  @argument[entries]{a list of option entries}
  @begin{short}
    Adds main option entries to be handled by the application.
  @end{short}

  This function is comparable to the function
  @fun{g-option-context-add-main-entries}.

  After the command line arguments are parsed, the \"handle-local-options\"
  signal will be emitted. At this point, the application can inspect the values
  pointed to by @code{arg-data} in the given option entries.

  Unlike the @type{g-option-context} implementation, the @class{g-application}
  class supports giving a @code{NULL} for @code{arg-data} for a non-callback
  option entry. This results in the argument in question being packed into a
  @class{g-variant-dict} which is also passed to the \"handle-local-options\"
  signal handler, where it can be inspected and modified. If the
  @code{:handles-command-line} flag is set, then the resulting dictionary is
  sent to the primary instance, where the function
  @fun{g-application-command-line-options-dict} will return it. This \"packing\"
  is done according to the type of the argument - booleans for normal flags,
  strings for strings, bytestrings for filenames, etc. The packing only occurs
  if the flag is given, i.e. we do not pack a \"false\" @type{g-variant} in the
  case that a flag is missing.

  In general, it is recommended that all command line arguments are parsed
  locally. The options dictionary should then be used to transmit the result
  of the parsing to the primary instance, where the function
  @fun{g-variant-dict-lookup} can be used. For local options, it is possible to
  either use @code{arg-data} in the usual way, or to consult, and potentially
  remove, the option from the options dictionary.

  This function is new in GLib 2.40. Before then, the only real choice was to
  send all of the command line arguments, options and all, to the primary
  instance for handling. The @class{g-application} instance ignored them
  completely on the local side. Calling this function \"opts in\" to the new
  behaviour, and in particular, means that unrecognised options will be treated
  as errors. Unrecognised options have never been ignored when the
  @code{:handles-command-line} flag is unset.

  If the \"handle-local-options\" signal needs to see the list of filenames,
  then the use of @code{G_OPTION_REMAINING} is recommended. If @code{arg-data}
  is @code{NULL} then @code{G_OPTION_REMAINING} can be used as a key into the
  options dictionary. If you do use @code{G_OPTION_REMAINING} then you need to
  handle these arguments for yourself because once they are consumed, they will
  no longer be visible to the default handling, which treats them as filenames
  to be opened.

  It is important to use the proper @type{g-variant} format when retrieving the
  options with the function @fun{g-variant-dict-lookup}:
  @begin{itemize}
    @item{for @code{:none}, use \"b\"}
    @item{for @code{:string}, use \"&s\"}
    @item{for @code{:int}, use \"i\"}
    @item{for @code{:int64}, use \"x\"}
    @item{for @code{:double}, use \"d\"}
    @item{for @code{:filename}, use \"^ay\"}
    @item{for @code{:string-array}, use \"&as\"}
    @item{for @code{:filename-array}, use \"^aay\"}
  @end{itemize}
  @see-class{g-application}"
  (let ((n-entries (length entries)))
    (with-foreign-object (entries-ptr '(:struct glib::g-option-entry)
                                      (1+ n-entries))
      (loop
        for entry in entries
        for i from 0
        for entry-ptr = (mem-aptr entries-ptr '(:struct glib::g-option-entry) i)
        do (with-foreign-slots ((glib::long-name
                                 glib::short-name
                                 glib::flags
                                 glib::arg
                                 glib::arg-data
                                 glib::description
                                 glib::arg-description)
                                entry-ptr (:struct glib::g-option-entry))
             (setf glib::long-name (first entry)
                   glib::short-name (if (second entry)
                                        (char-code (second entry))
                                        0)
                   glib::flags (third entry)
                   glib::arg (fourth entry)
                   ;; TODO: Check this. It is not correct?
                   glib::arg-data (if (and (fifth entry)
                                           (member (fourth entry)
                                                   '(:none :int :double :string
                                                     :filename :string-array
                                                     :filename-array :int64)))
                                (symbol-value (fifth entry))
                                (null-pointer))
                   glib::description (sixth entry)
                   glib::arg-description (if (seventh entry)
                                             (seventh entry)
                                             (null-pointer)))))
      ;; Set the fields of the last entry to NULL pointer or 0
      (let ((entry-ptr (mem-aptr entries-ptr
                                 '(:struct glib::g-option-entry)
                                 n-entries)))
        (with-foreign-slots ((glib::long-name
                              glib::short-name
                              glib::flags
                              glib::arg
                              glib::arg-data
                              glib::description
                              glib::arg-description)
                             entry-ptr (:struct glib::g-option-entry))
          (setf glib::long-name (null-pointer))
          (setf glib::short-name 0)
          (setf glib::flags 0)
          (setf glib::arg 0)
          (setf glib::arg-data (null-pointer))
          (setf glib::description (null-pointer))
          (setf glib::arg-description (null-pointer)))
        (%g-application-add-main-option-entries application entries-ptr)))))

(export 'g-application-add-main-option-entries)

;;; ----------------------------------------------------------------------------
;;; g_application_add_main_option ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_add_main_option" g-application-add-main-option) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[application]{a @class{g-application} instance}
  @argument[long-name]{a string with the long name of an option used to specify
    it in a command line}
  @argument[short-name]{a printable ASCII character with the short name of an
    option}
  @argument[flags]{the @symbol{g-option-flags} flags}
  @argument[arg]{a @symbol{g-option-arg} value for the type of the option}
  @argument[description]{a string with the description for the option in
    @code{--help} output}
  @argument[arg-description]{a string with the placeholder to use for the extra
    argument parsed by the option in @code{--help} output}
  @begin{short}
    Add an option to be handled by the application.
  @end{short}

  Calling this function is the equivalent of calling the function
  @fun{g-application-add-main-option-entries} with a single option entry that
  has its @code{arg-data} member set to @code{NULL}.

  The parsed arguments will be packed into a @class{g-variant-dict} which is
  passed to the \"handle-local-options\" signal handler. If the
  @code{:handles-command-line} flag is set, then it will also be sent to the
  primary instance. See the function @fun{g-application-add-main-option-entries}
  for more details.

  See the function @fun{g-option-group-add-entries} for more documentation of
  the arguments.
  @see-class{g-application}
  @see-function{g-option-group-add-entries}"
  (application (g-object g-application))
  (long-name :string)
  (short-name :char)
  (flags g-option-flags)
  (arg g-option-arg)
  (description :string)
  (arg-description :string))

(export 'g-application-add-main-option)

;;; ----------------------------------------------------------------------------
;;; g_application_add_option_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_add_option_group" g-application-add-option-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-11}
  @argument[application]{a @class{g-application} instance}
  @argument[group]{a @type{g-option-group} instance}
  @begin{short}
    Adds a @type{g-option-group} instance to the command line handling of the
    application.
  @end{short}

  This function is comparable to the function @fun{g-option-context-add-group}.

  Unlike the function @fun{g-application-add-main-option-entries}, this function
  does not deal with @code{NULL} for @code{arg-data} and never transmits options
  to the primary instance.

  The reason for that is because, by the time the options arrive at the primary
  instance, it is typically too late to do anything with them. Taking the GTK
  option group as an example: GTK will already have been initialised by the time
  the \"command-line\" signal handler runs. In the case that this is not the
  first-running instance of the application, the existing instance may already
  have been running for a very long time.

  This means that the options from the @type{g-option-group} instance are only
  really usable in the case that the instance of the application being run is
  the first instance. Passing options like @code{--display=} or
  @code{--gdk-debug=} on future runs will have no effect on the existing primary
  instance.

  Calling this function will cause the options in the supplied option group to
  be parsed, but it does not cause you to be \"opted in\" to the new
  functionality whereby unrecognised options are rejected even if the
  @code{:handles-command-line} flag was given.
  @see-class{g-application}"
  (application (g-object g-application))
  (group (:pointer (:struct g-option-group))))

(export 'g-application-add-option-group)

;;; ----------------------------------------------------------------------------
;;; g_application_set_option_context_parameter_string ()
;;; ----------------------------------------------------------------------------

#+glib-2-56
(defcfun ("g_application_set_option_context_parameter_string"
           g-application-set-option-context-parameter-string) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-12}
  @argument[application]{a @class{g-application} instance}
  @argument[parameter]{a string which is displayed in the first line of
    @code{--help} output}
  @begin{short}
    Sets the parameter string to be used by the command line handling of the
    application .
  @end{short}

  This function registers the argument to be passed to the function
  @fun{g-option-context-new} when the internal @type{g-option-context} instance
  of the application is created.

  See the function @fun{g-option-context-new} for more information about the
  argument @arg{parameter}.

  Since 2.56
  @see-class{g-application}
  @see-type{g-option-context}
  @see-function{g-option-context-new}"
  (application (g-object g-application))
  (parameter :string))

#+glib-2-56
(export 'g-application-set-option-context-parameter-string)

;;; ----------------------------------------------------------------------------
;;; g_application_set_option_context_summary ()
;;; ----------------------------------------------------------------------------

#+glib-2-56
(defcfun ("g_application_set_option_context_summary"
           g-application-set-option-context-summary) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-12}
  @argument[application]{a @class{g-application} instance}
  @argument[summary]{a string to be shown in @code{--help} output before the
    list of options, or @code{nil}}
  @begin{short}
    Adds a summary to the application option context.
  @end{short}
  See the function @fun{g-option-context-summary} for more information.

  Since 2.56
  @see-class{g-application}
  @see-function{g-option-context-summary}"
  (application (g-object g-application))
  (summary :string))

#+glib-2-56
(export 'g-application-set-option-context-summary)

;;; ----------------------------------------------------------------------------
;;; g_application_set_option_context_description ()
;;; ----------------------------------------------------------------------------

#+glib-2-56
(defcfun ("g_application_set_option_context_description"
           g-application-set-option-context-description) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-12}
  @argument[application]{a @class{g-application} instance}
  @argument[description]{a string to be shown in @code{--help} output after the
    list of options, or @code{nil}}
  @begin{short}
    Adds a description to the application option context.
  @end{short}
  See the function @fun{g-option-context-description} for more information.

  Since 2.56
  @see-class{g-application}
  @see-function{g-option-context-descripton}"
  (application (g-object g-application))
  (description :string))

#+glib-2-56
(export 'g-application-set-option-context-description)

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
 "@version{2021-7-27}
  @syntax[]{(g-application-default) => application}
  @syntax[]{(setf (g-application-default) application)}
  @argument[application]{the @class{g-application} instance to set as default,
    or @code{nil}}
  @begin{short}
    Accessor of the default application for the process.
  @end{short}

  The function @sym{g-application-default} returns the default application
  instance for this process. The function @sym{(setf g-application-default)}
  sets or unsets the default application.

  This function does not take its own reference on application. If the
  application is destroyed then the default application will revert back to
  @code{nil}.

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
 "@version{2021-7-27}
  @argument[application]{a @class{g-application} instance}
  @begin{short}
    Increases the busy count of the application.
  @end{short}
  Use this function to indicate that the application is busy, for instance
  while a long running operation is pending.

  The busy state will be exposed to other processes, so a session shell will
  use that information to indicate the state to the user, e.g. with a
  spinner. To cancel the busy indication, use the function
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
 "@version{2021-7-27}
  @argument[application]{a @class{g-application} instance}
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
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_bind_busy_property" g-application-bind-busy-property)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-12}
  @argument[application]{a @class{g-application} instance}
  @argument[object]{a @class{g-object} instance}
  @argument[property]{a string with the name of a boolean property of
    @arg{object}}
  @begin{short}
    Marks the application as busy while the property on the object is @em{true}.
  @end{short}
  See the function @fun{g-application-mark-busy}.

  The binding holds a reference to the application while it is active, but not
  to the object. Instead, the binding is destroyed when the object is finalized.
  @see-class{g-application}
  @see-function{g-application-mark-busy}"
  (application (g-object g-application))
  (object g-object)
  (property :string))

(export 'g-application-bind-busy-property)

;;; ----------------------------------------------------------------------------
;;; g_application_unbind_busy_property ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_unbind_busy_property"
           g-application-unbind-busy-property) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-12}
  @argument[application]{a @class{g-application} instance}
  @argument[object]{a @class{g-object} instance}
  @argument[property]{a string with the name of a boolean property of
    @arg{object}}
  @begin{short}
    Destroys a binding between the property and the busy state of the
    application that was previously created with the function
    @fun{g-application-bind-busy-property}.
  @end{short}
  @see-class{g-application}
  @see-class{g-object}
  @see-function{g-application-bind-busy-property}"
  (application (g-object g-application))
  (object g-object)
  (property :string))

(export 'g-application-unbind-busy-property)

;;; --- End of file gio.application.lisp ---------------------------------------
