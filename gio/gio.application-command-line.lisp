;;; ----------------------------------------------------------------------------
;;; gio.application-command-line.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.62 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 Dieter Kaiser
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
;;; GApplicationCommandLine
;;;
;;;     A command-line invocation of an application
;;;
;;; Types and Values
;;;
;;;     GApplicationCommandLine
;;;     GApplicationCommandLineClass
;;;
;;; Functions
;;;
;;;     g_application_command_line_get_arguments
;;;     g_application_command_line_get_cwd
;;;     g_application_command_line_get_environ
;;;     g_application_command_line_get_options_dict
;;;     g_application_command_line_get_stdin
;;;     g_application_command_line_create_file_for_arg
;;;     g_application_command_line_getenv
;;;     g_application_command_line_get_is_remote
;;;     g_application_command_line_get_platform_data
;;;     g_application_command_line_set_exit_status
;;;     g_application_command_line_get_exit_status
;;;     g_application_command_line_print
;;;     g_application_command_line_printerr
;;;
;;; Properties
;;;
;;;     GVariant*   arguments        Write / Construct Only
;;;     gboolean    is-remote        Read
;;;     GVariant*   options          Write / Construct Only
;;;     GVariant*   platform-data    Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GApplicationCommandLine
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GApplicationCommandLine
;;; ----------------------------------------------------------------------------

(define-g-object-class "GApplicationCommandLine" g-application-command-line
  (:superclass g-object
   :export t
   :interfaces ()
   :type-initializer "g_application_command_line_get_type")
  ((arguments
    g-application-command-line-arguments
    "arguments" "GVariant" nil nil)
   (is-remote
    g-application-command-line-is-remote
    "is-remote" "gboolean" t nil)
   (options
    g-application-command-line-options
    "options" "GVariant" nil nil)
   (platform-data
    g-application-command-line-platform-data
    "platform-data" "GVariant" nil nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-application-command-line 'type)
 "@version{2020-2-18}
  @begin{short}
    @sym{g-application-command-line} represents a command-line invocation of an
    application.
  @end{short}
  It is created by @class{g-application} and emitted in the \"command-line\"
  signal and virtual function.

  The class contains the list of arguments that the program was invoked with.
  It is also possible to query if the commandline invocation was local (i. e.
  the current process is running in direct response to the invocation) or
  remote (i. e.: some other process forwarded the commandline to this process).

  The @sym{g-application-command-line} object can provide the @code{argc} and
  @code{argv} parameters for use with the @code{GOptionContext} command-line
  parsing API, with the @fun{g-application-command-line-get-arguments} function.
  See gapplication-example-cmdline3.c for an example.

  The exit status of the originally-invoked process may be set and messages can
  be printed to stdout or stderr of that process. The lifecycle of the
  originally-invoked process is tied to the lifecycle of this object (i. e.:
  the process exits when the last reference is dropped).

  The main use for @sym{g-application-command-line} (and the \"command-line\"
  signal) is 'Emacs server' like use cases: You can set the EDITOR environment
  variable to have e. g. git use your favourite editor to edit commit messages,
  and if you already have an instance of the editor running, the editing will
  happen in the running instance, instead of opening a new one. An important
  aspect of this use case is that the process that gets started by git does not
  return until the editing is done.

  Normally, the commandline is completely handled in the \"command-line\"
  handler. The launching instance exits once the signal handler in the primary
  instance has returned, and the return value of the signal handler becomes the
  exit status of the launching instance.
  @begin{pre}
static int
command_line (GApplication            *application,
              GApplicationCommandLine *cmdline)
{
  gchar **argv;
  gint argc;
  gint i;

  argv = g_application_command_line_get_arguments (cmdline, &argc);

  g_application_command_line_print (cmdline,
                                    \"This text is written back\n\"
                                    \"to stdout of the caller\n\");

  for (i = 0; i < argc; i++)
    g_print (\"argument %d: %s\n\", i, argv[i]);

  g_strfreev (argv);

  return 0;
@}
  @end{pre}
  The complete example can be found here: gapplication-example-cmdline.c

  In more complicated cases, the handling of the comandline can be split
  between the launcher and the primary instance.
  @begin{pre}
static gboolean
 test_local_cmdline (GApplication   *application,
                     gchar        ***arguments,
                     gint           *exit_status)
{
  gint i, j;
  gchar **argv;

  argv = *arguments;

  i = 1;
  while (argv[i])
    {
      if (g_str_has_prefix (argv[i], \"--local-\"))
        {
          g_print (\"handling argument %s locally\n\", argv[i]);
          g_free (argv[i]);
          for (j = i; argv[j]; j++)
            argv[j] = argv[j + 1];
        @}
      else
        {
          g_print (\"not handling argument %s locally\n\", argv[i]);
          i++;
        @}
    @}

  *exit_status = 0;

  return FALSE;
@}

static void
test_application_class_init (TestApplicationClass *class)
{
  G_APPLICATION_CLASS (class)->local_command_line = test_local_cmdline;

  ...
@}
  @end{pre}
  In this example of split commandline handling, options that start with
  --local- are handled locally, all other options are passed to the
  \"command-line\" handler which runs in the primary instance.

  The complete example can be found here: gapplication-example-cmdline2.c

  If handling the commandline requires a lot of work, it may be better to defer
  it.
  @begin{pre}
static gboolean
my_cmdline_handler (gpointer data)
{
  GApplicationCommandLine *cmdline = data;

  // do the heavy lifting in an idle

  g_application_command_line_set_exit_status (cmdline, 0);
  g_object_unref (cmdline); // this releases the application

  return G_SOURCE_REMOVE;
@}

static int
command_line (GApplication            *application,
              GApplicationCommandLine *cmdline)
{
  // keep the application running until we are done with this commandline
  g_application_hold (application);

  g_object_set_data_full (G_OBJECT (cmdline),
                          \"application\", application,
                          (GDestroyNotify)g_application_release);

  g_object_ref (cmdline);
  g_idle_add (my_cmdline_handler, cmdline);

  return 0;
@}
  @end{pre}
  In this example the commandline is not completely handled before the
  \"command-line\" handler returns. Instead, we keep a reference to the
  @sym{g-application-command-line} object and handle it later (in this example,
  in an idle). Note that it is necessary to hold the application until you are
  done with the commandline.

  The complete example can be found here: gapplication-example-cmdline3.c
  @see-slot{g-application-command-line-arguments}
  @see-slot{g-application-command-line-is-remote}
  @see-slot{g-application-command-line-options}
  @see-slot{g-application-command-line-platform-data}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g-application-command-line-arguments -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "arguments"
                                               'g-application-command-line) 't)
 "The @code{arguments} property of type @symbol{g-variant}
  (Write / Construct only) @br{}
  The commandline that caused this \"command-line\" signal emission. @br{}
  Allowed values: @code{GVariant<aay>} @br{}
  Default value: @code{nil}")

;;; --- g-application-command-line-is-remote -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-remote"
                                               'g-application-command-line) 't)
 "The @code{is-remote} property of type @code{:boolean} (Read) @br{}
  @em{True} if this is a remote commandline. @br{}
  Default value: @em{false}")

;;; --- g-application-command-line-options -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "options"
                                               'g-application-command-line) 't)
 "The @code{options} property of type @symbol{g-variant}
  (Write / Construct Only) @br{}
  The options sent along with the commandline. @br{}
  Allowed values: @code{GVariant<a{sv@}>} @br{}
  Default value: @code{nil}")

;;; --- g-application-command-line-platform-data -------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "platform-data"
                                               'g-application-command-line) 't)
 "The @code{platform-data} property of type @symbol{g-variant}
  (Write / Construct Only) @br{}
  Platform-specific data for the commandline. @br{}
  Allowed values: @code{GVariant<a{sv@}>} @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_arguments ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_arguments"
           %g-application-command-line-get-arguments) g-strv
  (cmdline (g-object g-application-command-line))
  (argc (:pointer :int)))

(defun g-application-command-line-get-arguments (cmdline)
 #+cl-cffi-gtk-documentation
 "@version{2020-2-20}
  @argument[cmdline]{a @class{g-application-command-line} object}
  @return{The list of strings containing the command line arguments.}
  @begin{short}
    Gets the list of arguments that was passed on the command line.
  @end{short}

  The strings in the list may contain non-UTF-8 data on UNIX (such as
  filenames or arguments given in the system locale) but are always in UTF-8
  on Windows.

  If you wish to use the return value with @code{GOptionContext}, you must use
  the function @code{g_option_context_parse_strv()}.
  @see-class{g-application-command-line}"
  (with-foreign-object (argc :int)
    (%g-application-command-line-get-arguments cmdline argc)))

(export 'g-application-command-line-get-arguments)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_cwd ()
;;;
;;; const gchar *
;;; g_application_command_line_get_cwd (GApplicationCommandLine *cmdline);
;;;
;;; Gets the working directory of the command line invocation. The string may
;;; contain non-utf8 data.
;;;
;;; It is possible that the remote application did not send a working directory,
;;; so this may be NULL.
;;;
;;; The return value should not be modified or freed and is valid for as long
;;; as cmdline exists.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; Returns :
;;;     the current directory, or NULL.
;;;
;;; Since: 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_cwd"
           g-application-command-line-get-cwd) g-string
  (cmdline (g-object g-application-command-line)))

(export 'g-application-command-line-get-cwd)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_environ ()
;;;
;;; const gchar * const *
;;; g_application_command_line_get_environ
;;;                                (GApplicationCommandLine *cmdline);
;;;
;;; Gets the contents of the 'environ' variable of the command line invocation,
;;; as would be returned by g_get_environ(), ie as a NULL-terminated list of
;;; strings in the form 'NAME=VALUE'. The strings may contain non-utf8 data.
;;;
;;; The remote application usually does not send an environment. Use
;;; G_APPLICATION_SEND_ENVIRONMENT to affect that. Even with this flag set it
;;; is possible that the environment is still not available (due to invocation
;;; messages from other applications).
;;;
;;; The return value should not be modified or freed and is valid for as long
;;; as cmdline exists.
;;;
;;; See g_application_command_line_getenv() if you are only interested in the
;;; value of a single environment variable.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; Returns :
;;;     the environment strings, or NULL if they were not sent.
;;;
;;; Since: 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_environ"
           g-application-command-line-get-environ) g-strv
  (cmdline (g-object g-application-command-line)))

(export 'g-application-command-line-get-environ)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_options_dict ()
;;;
;;; GVariantDict *
;;; g_application_command_line_get_options_dict
;;;                                (GApplicationCommandLine *cmdline);
;;;
;;; Gets the options there were passed to g_application_command_line().
;;;
;;; If you did not override local_command_line() then these are the same options
;;; that were parsed according to the GOptionEntrys added to the application
;;; with g_application_add_main_option_entries() and possibly modified from your
;;; GApplication::handle-local-options handler.
;;;
;;; If no options were sent then an empty dictionary is returned so that you
;;; don't need to check for NULL.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; Returns :
;;;     a GVariantDict with the options.
;;;
;;; Since: 2.40
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_stdin ()
;;;
;;; GInputStream *
;;; g_application_command_line_get_stdin (GApplicationCommandLine *cmdline);
;;;
;;; Gets the stdin of the invoking process.
;;;
;;; The GInputStream can be used to read data passed to the standard input of
;;; the invoking process. This doesn't work on all platforms. Presently, it is
;;; only available on UNIX when using a DBus daemon capable of passing file
;;; descriptors. If stdin is not available then NULL will be returned. In the
;;; future, support may be expanded to other platforms.
;;;
;;; You must only call this function once per commandline invocation.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; Returns :
;;;     a GInputStream for stdin.
;;;
;;; Since: 2.34
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_create_file_for_arg ()
;;;
;;; GFile *
;;; g_application_command_line_create_file_for_arg
;;;                                (GApplicationCommandLine *cmdline,
;;;                                 const gchar *arg);
;;;
;;; Creates a GFile corresponding to a filename that was given as part of the
;;; invocation of cmdline .
;;;
;;; This differs from g_file_new_for_commandline_arg() in that it resolves
;;; relative pathnames using the current working directory of the invoking
;;; process rather than the local process.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; arg :
;;;     an argument from cmdline .
;;;
;;; Returns :
;;;     a new GFile.
;;;
;;; Since: 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_getenv ()
;;;
;;; const gchar *
;;; g_application_command_line_getenv (GApplicationCommandLine *cmdline,
;;;                                    const gchar *name);
;;;
;;; Gets the value of a particular environment variable of the command line
;;; invocation, as would be returned by g_getenv(). The strings may contain
;;; non-utf8 data.
;;;
;;; The remote application usually does not send an environment. Use
;;; G_APPLICATION_SEND_ENVIRONMENT to affect that. Even with this flag set it is
;;; possible that the environment is still not available (due to invocation
;;; messages from other applications).
;;;
;;; The return value should not be modified or freed and is valid for as long
;;; as cmdline exists.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; name :
;;;     the environment variable to get.
;;;
;;; Returns :
;;;     the value of the variable, or NULL if unset or unsent
;;;
;;; Since: 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_is_remote ()
;;;
;;; gboolean
;;; g_application_command_line_get_is_remote
;;;                                (GApplicationCommandLine *cmdline);
;;;
;;; Determines if cmdline represents a remote invocation.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; Returns :
;;;     TRUE if the invocation was remote
;;;
;;;Since: 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_platform_data ()
;;;
;;; GVariant *
;;; g_application_command_line_get_platform_data
;;;                                (GApplicationCommandLine *cmdline);
;;;
;;; Gets the platform data associated with the invocation of cmdline .
;;;
;;; This is a GVariant dictionary containing information about the context in
;;; which the invocation occurred. It typically contains information like the
;;; current working directory and the startup notification ID.
;;;
;;; For local invocation, it will be NULL.
;;;
;;; cmdline :
;;;     GApplicationCommandLine
;;;
;;; Returns :
;;;     the platform data, or NULL.
;;;
;;; Since: 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_set_exit_status ()
;;;
;;; void
;;; g_application_command_line_set_exit_status
;;;                                (GApplicationCommandLine *cmdline,
;;;                                 int exit_status);
;;;
;;; Sets the exit status that will be used when the invoking process exits.
;;;
;;; The return value of the “command-line” signal is passed to this function
;;; when the handler returns. This is the usual way of setting the exit status.
;;;
;;; In the event that you want the remote invocation to continue running and
;;; want to decide on the exit status in the future, you can use this call. For
;;; the case of a remote invocation, the remote process will typically exit when
;;; the last reference is dropped on cmdline . The exit status of the remote
;;; process will be equal to the last value that was set with this function.
;;;
;;; In the case that the commandline invocation is local, the situation is
;;; slightly more complicated. If the commandline invocation results in the
;;; mainloop running (ie: because the use-count of the application increased to
;;; a non-zero value) then the application is considered to have been
;;; 'successful' in a certain sense, and the exit status is always zero. If the
;;; application use count is zero, though, the exit status of the local
;;; GApplicationCommandLine is used.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; exit_status :
;;;     the exit status
;;;
;;; Since: 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_exit_status ()
;;;
;;; int
;;; g_application_command_line_get_exit_status
;;;                                (GApplicationCommandLine *cmdline);
;;;
;;; Gets the exit status of cmdline . See
;;; g_application_command_line_set_exit_status() for more information.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; Returns :
;;;     the exit status
;;;
;;; Since: 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_print ()
;;;
;;; void
;;; g_application_command_line_print (GApplicationCommandLine *cmdline,
;;;                                   const gchar *format,
;;;                                   ...);
;;;
;;; Formats a message and prints it using the stdout print handler in the
;;; invoking process.
;;;
;;; If cmdline is a local invocation then this is exactly equivalent to
;;; g_print(). If cmdline is remote then this is equivalent to calling g_print()
;;; in the invoking process.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; format :
;;;     a printf-style format string
;;;
;;; ... :
;;;     arguments, as per format
;;;
;;; Since: 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_printerr ()
;;;
;;; void
;;; g_application_command_line_printerr (GApplicationCommandLine *cmdline,
;;;                                      const gchar *format,
;;;                                      ...);
;;;
;;; Formats a message and prints it using the stderr print handler in the
;;; invoking process.
;;;
;;; If cmdline is a local invocation then this is exactly equivalent to
;;; g_printerr(). If cmdline is remote then this is equivalent to calling
;;; g_printerr() in the invoking process.
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; format :
;;;     a printf-style format string
;;;
;;; ... :
;;;     arguments, as per format
;;;
;;; Since: 2.28
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.application-command-line.lisp --------------------------
