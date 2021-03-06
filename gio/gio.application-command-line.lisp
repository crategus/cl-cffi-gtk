;;; ----------------------------------------------------------------------------
;;; gio.application-command-line.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 - 2021 Dieter Kaiser
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
;;;     g_application_command_line_get_is_remote           Accessor
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
 "@version{2021-5-7}
  @begin{short}
    The @sym{g-application-command-line} class represents a command-line
    invocation of an application.
  @end{short}
  It is created by the @class{g-application} object and emitted in the
  \"command-line\" signal and virtual function.

  The class contains the list of arguments that the program was invoked with.
  It is also possible to query if the commandline invocation was local (i.e.
  the current process is running in direct response to the invocation) or
  remote (i.e. some other process forwarded the commandline to this process).

  The @sym{g-application-command-line} object can provide the @code{argc} and
  @code{argv} parameters for use with the @code{GOptionContext} command-line
  parsing API, with the function @fun{g-application-command-line-get-arguments}.

  The exit status of the originally-invoked process may be set and messages can
  be printed to stdout or stderr of that process. The lifecycle of the
  originally-invoked process is tied to the lifecycle of this object (i.e. the
  process exits when the last reference is dropped).

  The main use for the @sym{g-application-command-line} object (and the
  \"command-line\" signal) is 'Emacs server' like use cases: You can set the
  EDITOR environment variable to have e.g. GIT use your favourite editor to edit
  commit messages, and if you already have an instance of the editor running,
  the editing will happen in the running instance, instead of opening a new one.
  An important aspect of this use case is that the process that gets started by
  GIT does not return until the editing is done.
  @begin[Example]{dictionary}
    Normally, the command line is completely handled in the \"command-line\"
    handler. The launching instance exits once the signal handler in the
    primary instance has returned, and the return value of the signal handler
    becomes the exit status of the launching instance.
    @begin{pre}
(defun application-commandline (&optional (argv nil))
  (within-main-loop
    (let ((app (make-instance 'g-application
                              :application-id
                              \"com.crategus.application-commandline\"
                              :inactivity-timeout 10000
                              :flags :handles-command-line)))

      ;; Signal handler \"startup\"
      (g-signal-connect app \"startup\"
                        (lambda (application)
                          (declare (ignore application))
                          (format t \"Signal handler 'startup'~%\")))

      ;; Signal handler \"command-line\"
      (g-signal-connect app \"command-line\"
          (lambda (application cmdline)
            (declare (ignore application))
            (let ((args (g-application-command-line-get-arguments cmdline)))
              (format t \"Signal handler 'command-line'~%\")
              (format t \"  arguments : ~a~%\" args)
              ;; Return the exit status
              0)))

      ;; Signal handler \"shutdown\"
      (g-signal-connect app \"shutdown\"
                        (lambda (application)
                          (declare (ignore application))
                          (format t \"Signal handler 'shutdown'~%\")
                          ;; Stop the main loop
                          (leave-gtk-main)))

      ;; Start the application
      (g-application-run app argv))))
    @end{pre}
  @end{dictionary}
  @see-slot{g-application-command-line-arguments}
  @see-slot{g-application-command-line-is-remote}
  @see-slot{g-application-command-line-options}
  @see-slot{g-application-command-line-platform-data}
  @see-class{g-application}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g-application-command-line-arguments -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "arguments"
                                               'g-application-command-line) 't)
 "The @code{arguments} property of type @type{g-variant}
  (Write / Construct only) @br{}
  The command line that caused this \"command-line\" signal emission. @br{}
  Allowed values: @code{GVariant<aay>} @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-command-line-arguments
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-command-line-arguments 'function)
 "@version{2021-5-7}
  @argument[object]{a @class{g-application-command-line} instance}
  @begin{short}
    The command line that caused this \"command-line\" signal emission.
  @end{short}
  The @slot[g-application-command-line]{arguments} property is not readable and
  set when constructing the instance.

  The function @fun{g-application-command-line-get-arguments} gets the list of
  arguments that was passed on the command line.
  @see-class{g-application-command-line}
  @see-function{g-application-command-line-get-arguments}")

;;; --- g-application-command-line-is-remote -----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "is-remote"
                                               'g-application-command-line) 't)
 "The @code{is-remote} property of type @code{:boolean} (Read) @br{}
  @em{True} if this is a remote commandline. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-command-line-is-remote
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-command-line-is-remote 'function)
 "@version{2021-5-7}
  @argument[object]{a @class{g-application-command-line} instance}
  @return{@em{True} if the invocation was remote.}
  @begin{short}
    Determines if the command line represents a remote invocation.
  @end{short}
  @see-class{g-application-command-line}")

;;; --- g-application-command-line-options -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "options"
                                               'g-application-command-line) 't)
 "The @code{options} property of type @type{g-variant}
  (Write / Construct Only) @br{}
  The options sent along with the commandline. @br{}
  Allowed values: @code{GVariant<a{sv@}>} @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-command-line-options
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-command-line-options 'function)
 "@version{2021-5-7}
  @argument[object]{a @class{g-application-command-line} instance}
  @begin{short}
    The options sent along with the commandline.
  @end{short}
  The @slot[g-application-command-line]{options} property is not readable and
  set when constructing the instance.
  @see-class{g-application-command-line}")

;;; --- g-application-command-line-platform-data -------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "platform-data"
                                               'g-application-command-line) 't)
 "The @code{platform-data} property of type @type{g-variant}
  (Write / Construct Only) @br{}
  Platform-specific data for the commandline. @br{}
  Allowed values: @code{GVariant<a{sv@}>} @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-command-line-platform-data
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-command-line-platform-data 'function)
 "@version{2021-5-7}
  @argument[object]{a @class{g-application-command-line} instance}
  @begin{short}
    Platform-specific data for the commandline.
  @end{short}
  The @slot[g-application-command-line]{platform-data} property is not readable
  and set when constructing the instance.
  @see-class{g-application-command-line}")

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_arguments ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_arguments"
           %g-application-command-line-get-arguments) g-strv
  (cmdline (g-object g-application-command-line))
  (argc (:pointer :int)))

(defun g-application-command-line-get-arguments (cmdline)
 #+cl-cffi-gtk-documentation
 "@version{*2021-5-7}
  @argument[cmdline]{a @class{g-application-command-line} instance}
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
;;; g_application_command_line_get_cwd ()                  not exported
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
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_cwd" g-application-command-line-cwd)
    :string
  (cmdline (g-object g-application-command-line)))

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_environ ()              not exported
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
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_environ"
           g-application-command-line-environ) g-strv
  (cmdline (g-object g-application-command-line)))

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_options_dict ()         not exported
;;;
;;; GVariantDict *
;;; g_application_command_line_get_options_dict
;;;                                (GApplicationCommandLine *cmdline);
;;;
;;;
;;; cmdline :
;;;     a GApplicationCommandLine
;;;
;;; Returns :
;;;     a GVariantDict with the options.
;;;
;;; Since 2.40
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_options_dict"
           g-application-command-line-options-dict)
    (:pointer (:struct g-variant-dict))
 #+cl-cffi-gtk-documentation
 "@version{2020-12-10}
  @argument[cmdline]{a @class{g-application-command-line} instance}
  @return{A @type{g-variant-dict} instance with the options.}
  @begin{short}
    Gets the options there were passed to the function
    @fun{g-application-command-line}.
  @end{short}

  If you did not override the virtual function @code{local_command_line} then
  these are the same options that were parsed according to the
  @code{GOptionEntry}s added to the application with the function
  @fun{g-application-add-main-option-entries} and possibly modified from your
  \"handle-local-options\" signal handler.

  If no options were sent then an empty dictionary is returned so that you
  do not need to check for @code{nil}.
  @see-class{g-application-command-line}
  @see-function{g-application-add-main-option-entries}"
  (cmdline (g-object g-application-command-line)))

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_stdin ()
;;;
;;; GInputStream *
;;; g_application_command_line_get_stdin (GApplicationCommandLine *cmdline);
;;;
;;; Gets the stdin of the invoking process.
;;;
;;; The GInputStream can be used to read data passed to the standard input of
;;; the invoking process. This does not work on all platforms. Presently, it is
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
;;; Since 2.34
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
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_getenv ()                   not exported
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
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_getenv" g-application-command-line-getenv)
    :string
  (cmdline (g-object g-application-command-line-getenv))
  (name :string))

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
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_exit_status ()          not exported
;;; g_application_command_line_set_exit_status ()
;;; -> g-application-command-line-exit-status
;;; ----------------------------------------------------------------------------

(defun (setf g-application-command-line-exit-status) (exit-status cmdline)
  (foreign-funcall "g_application_command_line_set_exit_status"
                   (g-object g-application-command-line) cmdline
                   :int exit-status
                   :void)
  exit-status)

(defcfun ("g_application_command_line_get_exit_status"
           g-application-command-line-exit-status) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-12-10}
  @syntax[]{(g-application-command-line-exit-status cmdline) => exit-status}
  @syntax[]{(setf (g-application-command-line-exit-status cmdline) exit-status)}
  @argument[cmdline]{a @class{g-application-command-line} instance}
  @argument[exit-status]{an integer with the exit status}
  @begin{short}
    Accessor of the exit status of a @class{g-application-command-line}
    instance.
  @end{short}

  The function @sym{g-application-command-line-exit-status} gets the exit
  status of @arg{cmdline}. The function
  @sym{(setf g-application-command-line-exit-status)} sets the exit status that
  will be used when the invoking process exits.

  The return value of the \"command-line\" signal is passed to this function
  when the handler returns. This is the usual way of setting the exit status.

  In the event that you want the remote invocation to continue running and
  want to decide on the exit status in the future, you can use this call. For
  the case of a remote invocation, the remote process will typically exit when
  the last reference is dropped on @arg{cmdline}. The exit status of the remote
  process will be equal to the last value that was set with this function.

  In the case that the commandline invocation is local, the situation is
  slightly more complicated. If the commandline invocation results in the
  mainloop running (ie: because the use-count of the application increased to
  a non-zero value) then the application is considered to have been
  'successful' in a certain sense, and the exit status is always zero. If the
  application use count is zero, though, the exit status of the local
  @class{g-application-command-line} instance is used.
  @see-class{g-application-command-line}"
  (cmdline (g-object g-application-command-line)))

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
;;; Since 2.28
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
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.application-command-line.lisp --------------------------
