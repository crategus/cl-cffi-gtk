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
 "@version{2021-9-18}
  @begin{short}
    The @sym{g-application-command-line} class represents a command line
    invocation of an application.
  @end{short}
  It is created by the @class{g-application} object and emitted in the
  \"command-line\" signal and virtual function.

  The class contains the list of arguments that the program was invoked with.
  It is also possible to query if the command line invocation was local, i.e.
  the current process is running in direct response to the invocation, or
  remote, i.e. some other process forwarded the command line to this process.

  The @sym{g-application-command-line} object can provide the command line
  arguments for use with the @class{g-option-context} command line parsing API,
  with the @fun{g-application-command-line-get-arguments} function.

  The exit status of the originally invoked process may be set and messages can
  be printed to stdout or stderr of that process. The life cycle of the
  originally invoked process is tied to the life cycle of this object, i.e. the
  process exits when the last reference is dropped.

  The main use for the @sym{g-application-command-line} object, and the
  \"command-line\" signal, is 'Emacs server' like use cases: You can set the
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
(defun application-cmdline (&rest argv)
  (within-main-loop
    (let ((app (make-instance 'g-application
                              :application-id
                              \"com.crategus.application-cmdline\"
                              :inactivity-timeout 10000
                              :flags :handles-command-line))
          (argv (if argv argv #+sbcl sb-ext:*posix-argv*)))
      ;; Signal handler \"startup\"
      (g-signal-connect app \"startup\"
                        (lambda (application)
                          (declare (ignore application))
                          (format t \"Signal handler 'startup'~%\")))
      ;; Signal handler \"command-line\"
      (g-signal-connect app \"command-line\"
          (lambda (application cmdline)
            (declare (ignore application))
            (let ((args (g-application-command-line-get-arguments cmdline))
                  (data (g-application-command-line-get-platform-data cmdline)))
              (format t \"Signal handler 'command-line'~%\")
              (format t \"     arguments : ~a~%\" args)
              (format t \" platform-data : ~a~%\" data)
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
      (g-application-run app argv)))
  (join-gtk-main))
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
  (Write / Construct Only) @br{}
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
  @em{True} if this is a remote command line. @br{}
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
  The options sent along with the command line. @br{}
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
    The options sent along with the command line.
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
  Platform-specific data for the command line. @br{}
  Allowed values: @code{GVariant<a{sv@}>} @br{}
  Default value: @code{NULL}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-application-command-line-platform-data
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-application-command-line-platform-data 'function)
 "@version{2021-5-7}
  @argument[object]{a @class{g-application-command-line} instance}
  @begin{short}
    Platform-specific data for the command line.
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
;;; g_application_command_line_get_cwd ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_cwd" g-application-command-line-cwd)
    (:string :free-from-foreign nil)
 "@version{2021-8-3}
  @argument[cmdline]{a @class{g-application-command-line} object}
  @return{A string with the current directory, or @code{nil}.}
  @begin{short}
    Gets the working directory of the command line invocation.
  @end{short}
  The string may contain non-UTF8 data.

  It is possible that the remote application did not send a working directory,
  so this may be @code{nil}.
  @begin[Example]{dictionary}
    @begin{pre}
(defvar cmd (make-instance 'g-application-command-line)) => CMD
(g-application-command-line-cwd cmd) => \"/home/dieter/Lisp/lisp-projects\"
    @end{pre}
  @end{dictionary}
  @see-class{g-application-command-line}"
  (cmdline (g-object g-application-command-line)))

(export 'g-application-command-line-cwd)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_environ ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_environ"
           g-application-command-line-environ) (g-strv :free-from-foreign nil)
 "@version{2021-8-3}
  @argument[cmdline]{a @class{g-application-command-line} object}
  @return{A list of strings with the environment strings, or @code{nil} if they
    were not sent.}
  @begin{short}
    Gets the contents of the 'environ' variable of the command line invocation,
    as would be returned by the function @fun{g-environ}.
  @end{short}
  Each item in the list of the form @code{NAME} = @code{VALUE}. The strings may
  contain non-UTF8 data.

  The remote application usually does not send an environment. Use the
  @code{:send-enviroment} flag to affect that. Even with this flag set it is
  possible that the environment is still not available, due to invocation
  messages from other applications.

  See the function @fun{g-application-command-line-getenv} if you are only
  interested in the value of a single environment variable.
  @see-class{g-application-command-line}
  @see-function{g-application-command-line-getenv}
  @see-function{g-environ}"
  (cmdline (g-object g-application-command-line)))

(export 'g-application-command-line-environ)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_options_dict ()
;;; -> g-application-command-line-optons-dict
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_options_dict"
           g-application-command-line-options-dict)
    (g-boxed-foreign g-variant-dict)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-12}
  @argument[cmdline]{a @class{g-application-command-line} instance}
  @return{A @class{g-variant-dict} instance with the options.}
  @begin{short}
    Gets the options that were passed to the function
    @fun{g-application-command-line}.
  @end{short}

  If you did not override the virtual function @code{local_command_line} then
  these are the same options that were parsed according to the
  options entries added to the application with the function
  @fun{g-application-add-main-option-entries} and possibly modified from your
  \"handle-local-options\" signal handler.

  If no options were sent then an empty dictionary is returned so that you
  do not need to check for @code{nil}.
  @see-class{g-application-command-line}
  @see-class{g-variant-dict}
  @see-function{g-application-add-main-option-entries}"
  (cmdline (g-object g-application-command-line)))

(export 'g-application-command-line-options-dict)

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
;;; You must only call this function once per command line invocation.
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
;;; g_application_command_line_getenv ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_getenv" g-application-command-line-getenv)
    (:string :free-from-foreign nil)
 "@version{2021-8-3}
  @argument[cmdline]{a @class{g-application-command-line} object}
  @argument[name]{a string with the environment variable to get}
  @return{A string with the value of the variable, or @code{nil} if unset or
    unsent.}
  @begin{short}
    Gets the value of a particular environment variable of the command line
    invocation, as would be returned by the function @fun{g-getenv}.
  @end{short}
  The strings may contain non-UTF8 data.

  The remote application usually does not send an environment. Use the
  @code{:send-enviroment} flag to affect that. Even with this flag set it is
  possible that the environment is still not available, due to invocation
  messages from other applications.
  @begin[Example]{dictionary}
    @begin{pre}
(defvar cmd (make-instance 'g-application-command-line)) => CMD
(g-application-command-line-getenv cmd \"HOME\") => \"/home/dieter\"
(g-application-command-line-getenv cmd \"unkown\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g-application-command-line}
  @see-function{g-getenv}"
  (cmdline (g-object g-application-command-line))
  (name :string))

(export 'g-application-command-line-getenv)

;;; ----------------------------------------------------------------------------
;;; g_application_command_line_get_platform_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_application_command_line_get_platform_data"
           g-application-command-line-get-platform-data)
    (:pointer (:struct g-variant))
 "@version{2021-8-3}
  @argument[cmdline]{a @class{g-application-command-line} object}
  @return{A @type{g-variant} dictionary with the platform data, or a @code{NULL}
    pointer.}
  @begin{short}
    Gets the platform data associated with the invocation of @arg{cmdline}.
  @end{short}

  This is a @type{g-variant} dictionary containing information about the context
  in which the invocation occurred. It typically contains information like the
  current working directory and the startup notification ID.

  For local invocation, it will be a @code{NULL} pointer.
  @see-class{g-application-command-line}
  @see-type{g-variant}"
  (cmdline (g-object g-application-command-line)))

(export 'g-application-command-line-get-platform-data)

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

  In the case that the command line invocation is local, the situation is
  slightly more complicated. If the command line invocation results in the
  mainloop running, i.e. because the use-count of the application increased to
  a non-zero value, then the application is considered to have been
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
