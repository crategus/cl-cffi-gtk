;;; ----------------------------------------------------------------------------
;;; glib.package.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
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

(defpackage :glib
  (:use :cl :cffi :iter)
  (:export ;; Symbols from glib.init.lisp
;           #:at-init
;           #:at-finalize

           ;; Symbols from glib.stable-pointer.lisp
;           #:allocate-stable-pointer
;           #:free-stable-pointer
;           #:get-stable-pointer-value
           #:with-stable-pointer
;           #:stable-pointer-destroy-notify-cb

           ;; Symbols from glib.version.lisp
;           #:push-library-version-features
;           #:require-library-version

           ;; Symbols from glib.error.lisp
           #:with-catching-to-g-error
           #:with-g-error))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (find-package :glib) t)
 "GLib is a general-purpose utility library, which provides many useful data
  types, macros, type conversions, string utilities, file utilities, a mainloop
  abstraction, and so on. It works on many UNIX-like platforms, as well as
  Windows and OS X. GLib is released under the GNU Library General Public
  License (GNU LGPL).

  This is the API documentation of a Lisp binding to the library GLib. Only a
  small part of GLib is implemented in Lisp which is necessary to implement
  GTK+ in Lisp.
  @begin[Version Information]{section}
    Variables and functions to check the GLib version.

    GLib provides version information, primarily useful in configure checks for
    builds that have a configure script. Applications will not typically use the
    features described here.

    @about-symbol{glib-major-version}
    @about-symbol{glib-minor-version}
    @about-symbol{glib-micro-version}
    @about-symbol{glib-binary-age}
    @about-symbol{glib-interface-age}
    @about-function{glib-check-version}
    @about-variable{+glib-major-version+}
    @about-variable{+glib-minor-version+}
    @about-variable{+glib-micro-version+}
    @about-symbol{GLIB_CHECK_VERSION}
    @about-symbol{GLIB_VERSION_2_26}
    @about-symbol{GLIB_VERSION_2_28}
    @about-symbol{GLIB_VERSION_2_30}
    @about-symbol{GLIB_VERSION_2_32}
    @about-symbol{GLIB_VERSION_MIN_REQUIRED}
    @about-symbol{GLIB_VERSION_MAX_ALLOWED}
  @end{section}
  @begin[Miscellaneous]{section}
    Documentation of several type definitions and functions, which are
    needed for the implemenation of the GTK+ library. Only a small part of the
    GLib library is implemented.

    @heading{Basic Types}
      Standard GLib types, defined for ease-of-use and portability.
      Only the following types are implemented:

      @about-type{g-size}
      @about-type{g-ssize}
      @about-type{g-offset}

    @heading{Memory Allocation}
      The following functions for the general memory-handling are implemented:

      @about-function{g-malloc}
      @about-function{g-free}

    @heading{Date and Time Functions}
      Calendrical calculations and miscellaneous time stuff.
      Only the following struct is implemented:

      @about-type{g-time-val}

    @heading{String Utility Functions - Various string-related functions}
      Implemented is:

      @about-type{g-string}
      @about-type{g-strv}

    @heading{Doubly-Linked Lists}
      Linked lists containing integer values or pointers to data, with the ability
      to iterate over the list in both directions

      Implemented is:

      @about-type{g-list}

    @heading{Singly-Linked Lists}
      Linked lists containing integer values or pointers to data, limited to
      iterating over the list in one direction

      Implemented is:

      @about-type{g-slist}
  @end{section}
  @begin[Threads]{section}
    Portable support for threads, mutexes, locks, conditions and thread private
    data.

    Threads act almost like processes, but unlike processes all threads of one
    process share the same memory. This is good, as it provides easy
    communication between the involved threads via this shared memory, and it is
    bad, because strange things (so called \"Heisenbugs\") might happen if the
    program is not carefully designed. In particular, due to the concurrent
    nature of threads, no assumptions on the order of execution of code running
    in different threads can be made, unless order is explicitly forced by the
    programmer through synchronization primitives.

    The aim of the thread-related functions in GLib is to provide a portable
    means for writing multi-threaded software. There are primitives for mutexes
    to protect the access to portions of memory (@code{GMutex}, @code{GRecMutex}
    and @code{GRWLock}). There is a facility to use individual bits for locks
    (@code{g_bit_lock()}). There are primitives for condition variables to allow
    synchronization of threads (@code{GCond}). There are primitives for
    thread-private data - data that every thread has a private instance of
    (@code{GPrivate}). There are facilities for one-time initialization
    (@code{GOnce}, @code{g_once_init_enter()}). Finally, there are primitives to
    create and manage threads (@type{g-thread}).

    The GLib threading system used to be initialized with
    @code{g_thread_init()}. This is no longer necessary. Since version 2.32, the
    GLib threading system is automatically initialized at the start of your
    program, and all thread-creation functions and synchronization primitives
    are available right away.

    Note that it is not safe to assume that your program has no threads even if
    you don't call @fun{g-thread-new} yourself. GLib and GIO can and will create
    threads for their own purposes in some cases, such as when using
    @code{g_unix_signal_source_new()} or when using GDBus.

    Originally, UNIX did not have threads, and therefore some traditional UNIX
    APIs are problematic in threaded programs. Some notable examples are
    @begin{itemize}
      @item{C library functions that return data in statically allocated
        buffers, such as @code{strtok()} or @code{strerror()}. For many of
        these, there are thread-safe variants with a @code{_r} suffix, or you
        can look at corresponding GLib APIs (like @code{g_strsplit()} or
        @code{g_strerror()}).}
      @item{@code{setenv()} and @code{unsetenv()} manipulate the process
        environment in a not thread-safe way, and may interfere with
        @code{getenv()} calls in other threads. Note that @code{getenv()} calls
        may be hidden behind other APIs. For example, GNU @code{gettext()} calls
        @code{getenv()} under the covers. In general, it is best to treat the
        environment as readonly. If you absolutely have to modify the
        environment, do it early in @code{main()}, when no other threads are
        around yet.}
      @item{@code{setlocale()} changes the locale for the entire process,
        affecting all threads. Temporary changes to the locale are often made to
        change the behavior of string scanning or formatting functions like
        @code{scanf()} or @code{printf()}. GLib offers a number of string APIs
        (like @code{g_ascii_formatd()} or @code{g_ascii_strtod()}) that can
        often be used as an alternative. Or you can use the @code{uselocale()}
        function to change the locale only for the current thread.}
    @item{@code{fork()} only takes the calling thread into the child's copy of
        the process image. If other threads were executing in critical sections
        they could have left mutexes locked which could easily cause deadlocks
        in the new child. For this reason, you should call @code{exit()} or
        @code{exec()} as soon as possible in the child and only make signal-safe
        library calls before that.}
    @item{@code{daemon()} uses @code{fork()} in a way contrary to what is
      described above. It should not be used with GLib programs.}
    @end{itemize}
    GLib itself is internally completely thread-safe (all global data is
    automatically locked), but individual data structure instances are not
    automatically locked for performance reasons. For example, you must
    coordinate accesses to the same @code{GHashTable} from multiple threads. The
    two notable exceptions from this rule are @type{g-main-loop} and
    @code{GAsyncQueue}, which are thread-safe and need no further
    application-level locking to be accessed from multiple threads. Most
    refcounting functions such as g_object_ref() are also thread-safe.

    @about-function{G_THREAD_ERROR}
    @about-type{g-thread-error}
    @about-type{g-thread}
    @about-function{g-thread-new}
    @about-function{g-thread-try-new}
    @about-function{g-thread-ref}
    @about-function{g-thread-unref}
    @about-function{g-thread-join}
    @about-function{g-thread-yield}
    @about-function{g-thread-exit}
    @about-function{g-thread-self}
    @about-function{g-mutex-init}
    @about-function{g-mutex-clear}
    @about-function{g-mutex-lock}
    @about-function{g-mutex-trylock}
    @about-function{g-mutex-unlock}
    @about-function{g-rec-mutex-init}
    @about-function{g-rec-mutex-clear}
    @about-function{g-rec-mutex-lock}
    @about-function{g-rec-mutex-trylock}
    @about-function{g-rec-mutex-unlock}
    @about-type{g-mutex}
    @about-function{G_LOCK_DEFINE}
    @about-function{G_LOCK_DEFINE_STATIC}
    @about-function{G_LOCK_EXTERN}
    @about-function{G_LOCK}
    @about-function{G_TRYLOCK}
    @about-function{G_UNLOCK}
    @about-function{GRecMutex}
    @about-function{GRWLock}
    @about-function{g-rw-lock-init}
    @about-function{g-rw-lock-clear}
    @about-function{g-rw-lock-writer-lock}
    @about-function{g-rw-lock-writer-trylock}
    @about-function{g-rw-lock-writer-unlock}
    @about-function{g-rw-lock-reader-lock}
    @about-function{g-rw-lock-reader-trylock}
    @about-function{g-rw-lock-reader-unlock}
    @about-type{g-cond}
    @about-function{g-cond-init}
    @about-function{g-cond-clear}
    @about-function{g-cond-wait}
    @about-function{g-cond-timed-wait}
    @about-function{g-cond-wait-until}
    @about-function{g-cond-signal}
    @about-function{g-cond-broadcast}
    @about-function{GPrivate}
    @about-function{G_PRIVATE_INIT}
    @about-function{g-private-get}
    @about-function{g-private-set}
    @about-function{g-private-replace}
    @about-function{GOnce}
    @about-function{GOnceStatus}
    @about-function{G_ONCE_INIT}
    @about-function{g-once}
    @about-function{g-once-init-enter}
    @about-function{g-once-init-leave}
    @about-function{g-bit-lock}
    @about-function{g-bit-trylock}
    @about-function{g-bit-unlock}
    @about-function{g-pointer-bit-lock}
    @about-function{g-pointer-bit-trylock}
    @about-function{g-pointer-bit-unlock}
  @end{section}
  @begin[The Main Event Loop]{section}
    The main event loop manages all the available sources of events for GLib and
    GTK+ applications. These events can come from any number of different types
    of sources such as file descriptors (plain files, pipes or sockets) and
    timeouts. New types of event sources can also be added using
    @fun{g-source-attach}.

    To allow multiple independent sets of sources to be handled in different
    threads, each source is associated with a @type{g-main-context}. A
    @type{g-main-context} can only be running in a single thread, but sources
    can be added to it and removed from it from other threads.

    Each event source is assigned a priority. The default priority,
    @var{g-priority-default}, is @code{0}. Values less than @code{0} denote
    higher priorities. Values greater than @code{0} denote lower priorities.
    Events from high priority sources are always processed before events from
    lower priority sources.

    Idle functions can also be added, and assigned a priority. These will be run
    whenever no events with a higher priority are ready to be processed.

    The @type{g-main-loop} data type represents a main event loop. A
    @type{g-main-loop} is created with @fun{g-main-loop-new}. After adding the
    initial event sources, @fun{g-main-loop-run} is called. This continuously
    checks for new events from each of the event sources and dispatches them.
    Finally, the processing of an event from one of the sources leads to a call
    to @fun{g-main-loop-quit} to exit the main loop, and @fun{g-main-loop-run}
    returns.

    It is possible to create new instances of @type{g-main-loop} recursively.
    This is often used in GTK+ applications when showing modal dialog boxes.
    Note that event sources are associated with a particular
    @type{g-main-context}, and will be checked and dispatched for all main loops
    associated with that @type{g-main-context}.

    GTK+ contains wrappers of some of these functions, e. g. @fun{gtk-main},
    @fun{gtk-main-quit} and @fun{gtk-events-pending}.

    @subheading{Creating new source types}
    One of the unusual features of the @type{g-main-loop} functionality is that
    new types of event source can be created and used in addition to the builtin
    type of event source. A new event source type is used for handling GDK
    events. A new source type is created by deriving from the  @type{g-source}
    structure. The derived type of source is represented by a structure that has
    the @type{g-source} structure as a first element, and other elements
    specific to the new source type. To create an instance of the new source
    type, call @fun{g-source-new} passing in the size of the derived structure
    and a table of functions. These @type{g-source-funcs} determine the behavior
    of the new source type.

    New source types basically interact with the main context in two ways. Their
    prepare function in @type{g-source-funcs} can set a timeout to determine the
    maximum amount of time that the main loop will sleep before checking the
    source again. In addition, or as well, the source can add file descriptors
    to the set that the main context checks using @fun{g-source-add-poll}.

    @subheading{Customizing the main loop iteration}
    Single iterations of a @type{g-main-context} can be run with
    @fun{g-main-context-iteration}. In some cases, more detailed control of
    exactly how the details of the main loop work is desired, for instance, when
    integrating the @type{g-main-loop} with an external main loop. In such
    cases, you can call the component functions of
    @fun{g-main-context-iteration} directly. These functions are
    @fun{g-main-context-prepare}, @fun{g-main-context-query},
    @fun{g-main-context-check} and @fun{g-main-context-dispatch}.

    On Unix, the GLib mainloop is incompatible with @code{fork()}. Any program
    using the mainloop must either @code{exec()} or @code{exit()} from the child
    without returning to the mainloop.

    @about-type{g-main-loop}
    @about-function{g-main-loop-new}
    @about-function{g-main-loop-ref}
    @about-function{g-main-loop-unref}
    @about-function{g-main-loop-run}
    @about-function{g-main-loop-quit}
    @about-function{g-main-loop-is-running}
    @about-function{g-main-loop-get-context}
    @about-function{g-main-new}
    @about-function{g-main-destroy}
    @about-function{g-main-run}
    @about-function{g-main-quit}
    @about-function{g-main-is-running}
    @about-function{G_PRIORITY_HIGH}
    @about-function{G_PRIORITY_DEFAULT}
    @about-function{G_PRIORITY_HIGH_IDLE}
    @about-function{G_PRIORITY_DEFAULT_IDLE}
    @about-function{G_PRIORITY_LOW}
    @about-function{G_SOURCE_CONTINUE}
    @about-function{G_SOURCE_REMOVE}
    @about-type{g-main-context}
    @about-function{g-main-context-new}
    @about-function{g-main-context-ref}
    @about-function{g-main-context-unref}
    @about-function{g-main-context-default}
    @about-function{g-main-context-iteration}
    @about-function{g-main-iteration}
    @about-function{g-main-context-pending}
    @about-function{g-main-pending}
    @about-function{g-main-context-find-source-by-id}
    @about-function{g-main-context-find-source-by-user-data}
    @about-function{g-main-context-find-source-by-funcs-user-data}
    @about-function{g-main-context-wakeup}
    @about-function{g-main-context-acquire}
    @about-function{g-main-context-release}
    @about-function{g-main-context-is-owner}
    @about-function{g-main-context-wait}
    @about-function{g-main-context-prepare}
    @about-function{g-main-context-query}
    @about-function{g-main-context-check}
    @about-function{g-main-context-dispatch}
    @about-function{g-main-context-set-poll-func}
    @about-function{g-main-context-get-poll-func}
    @about-function{g-main-context-add-poll}
    @about-function{g-main-context-remove-poll}
    @about-function{g-main-depth}
    @about-function{g-main-current-source}
    @about-function{g-main-set-poll-func}
    @about-function{g-main-context-invoke}
    @about-function{g-main-context-invoke-full}
    @about-function{g-main-context-get-thread-default}
    @about-function{g-main-context-ref-thread-default}
    @about-function{g-main-context-push-thread-default}
    @about-function{g-main-context-pop-thread-default}
    @about-function{g-timeout-source-new}
    @about-function{g-timeout-source-new-seconds}
    @about-function{g-timeout-add}
    @about-function{g-timeout-add-full}
    @about-function{g-timeout-add-seconds}
    @about-function{g-timeout-add-seconds-full}
    @about-function{g-idle-source-new}
    @about-function{g-idle-add}
    @about-function{g-idle-add-full}
    @about-function{g-idle-remove-by-data}
    @about-function{GPid}
    @about-function{g-child-watch-source-new}
    @about-function{g-child-watch-add}
    @about-function{g-child-watch-add-full}
    @about-type{g-poll-fd}
    @about-function{g-poll}
    @about-function{G_POLLFD_FORMAT}
    @about-type{g-source}
    @about-type{g-source-funcs}
    @about-type{g-source-callback-funcs}
    @about-function{g-source-new}
    @about-function{g-source-ref}
    @about-function{g-source-unref}
    @about-function{g-source-set-funcs}
    @about-function{g-source-attach}
    @about-function{g-source-destroy}
    @about-function{g-source-is-destroyed}
    @about-function{g-source-set-priority}
    @about-function{g-source-get-priority}
    @about-function{g-source-set-can-recurse}
    @about-function{g-source-get-can-recurse}
    @about-function{g-source-get-id}
    @about-function{g-source-get-name}
    @about-function{g-source-set-name}
    @about-function{g-source-set-name-by-id}
    @about-function{g-source-get-context}
    @about-function{g-source-set-callback}
    @about-function{g-source-set-callback-indirect}
    @about-function{g-source-add-poll}
    @about-function{g-source-remove-poll}
    @about-function{g-source-add-child-source}
    @about-function{g-source-remove-child-source}
    @about-function{g-source-get-time}
    @about-function{g-source-get-current-time}
    @about-function{g-source-remove}
    @about-function{g-source-remove-by-funcs-user-data}
    @about-function{g-source-remove-by-user-data}
  @end{section}
  @begin[Quarks]{section}
    A 2-way association between a string and a unique integer identifier

    @about-type{g-quark}
    @about-function{g-quark-from-string}
    @about-function{g-quark-from-static-string}
    @about-function{g-quark-to-string}
    @about-function{g-quark-try-string}
    @about-function{g-intern-string}
    @about-function{g-intern-static-string}
  @end{section}
  @begin[Error Reporting]{section}
    A system for reporting errors.

    GLib provides a standard method of reporting errors from a called function
    to the calling code. (This is the same problem solved by exceptions in other
    languages.) It is important to understand that this method is both a data
    type (the GError object) and a set of rules. If you use GError incorrectly,
    then your code will not properly interoperate with other code that uses
    GError, and users of your API will probably get confused.

    First and foremost: GError should only be used to report recoverable runtime
    errors, never to report programming errors. If the programmer has screwed
    up, then you should use @code{g_warning()}, @code{g_return_if_fail()},
    @code{g_assert()}, @code{g_error()}, or some similar facility.
    (Incidentally, remember that the @code{g_error()} function should only be
    used for programming errors, it should not be used to print any error
    reportable via GError.)

    Examples of recoverable runtime errors are \"file not found\" or \"failed to
    parse input.\" Examples of programming errors are \"NULL passed to
    strcmp()\" or \"attempted to free the same pointer twice.\" These two kinds
    of errors are fundamentally different: runtime errors should be handled or
    reported to the user, programming errors should be eliminated by fixing the
    bug in the program. This is why most functions in GLib and GTK+ do not use
    the GError facility.

    Functions that can fail take a return location for a GError as their last
    argument. For example:
    @begin{pre}
  gboolean g_file_get_contents (const gchar  *filename,
                                gchar       **contents,
                                gsize        *length,
                                GError      **error);
    @end{pre}
    If you pass a non-@code{NULL} value for the error argument, it should point
    to a location where an error can be placed. For example:
    @begin{pre}
  gchar *contents;
  GError *err = NULL;
  g_file_get_contents (\"foo.txt\", &contents, NULL, &err);
  g_assert ((contents == NULL && err != NULL) ||
            (contents != NULL && err == NULL));
  if (err != NULL)
    {
      /* Report error to user, and free error */
      g_assert (contents == NULL);
      fprintf (stderr, \"Unable to read file: %s\n\", err->message);
      g_error_free (err);
    @}
  else
    {
      /* Use file contents */
      g_assert (contents != NULL);
    @}
    @end{pre}
    Note that @code{err != NULL} in this example is a reliable indicator of
    whether @code{g_file_get_contents()} failed. Additionally,
    @code{g_file_get_contents()} returns a boolean which indicates whether it
    was successful.

    Because @code{g_file_get_contents()} returns @code{FALSE} on failure, if you
    are only interested in whether it failed and do not need to display an error
    message, you can pass @code{NULL} for the error argument:
    @begin{pre}
  if (g_file_get_contents (\"foo.txt\", &contents, NULL, NULL))
     /* no error occurred */ ;
  else
    /* error */ ;
    @end{pre}
    The GError object contains three fields: @arg{domain} indicates the module
    the error-reporting function is located in, @arg{code} indicates the
    specific error that occurred, and @arg{message} is a user-readable error
    message with as many details as possible. Several functions are provided to
    deal with an error received from a called function: @code{g_error_matches()}
    returns @code{TRUE} if the error matches a given domain and code,
    @code{g_propagate_error()} copies an error into an error location (so the
    calling function will receive it), and @code{g_clear_error()} clears an
    error location by freeing the error and resetting the location to
    @code{NULL}. To display an error to the user, simply display
    @code{error->message}, perhaps along with additional context known only to
    the calling function (the file being opened, or whatever - though in the
    @code{g_file_get_contents()} case, @code{error->message} already contains a
    filename).

    When implementing a function that can report errors, the basic tool is
    @code{g_set_error()}. Typically, if a fatal error occurs you want to
    @code{g_set_error()}, then return immediately. @code{g_set_error()} does
    nothing if the error location passed to it is @code{NULL}. Here is an
    example:
    @begin{pre}
  gint
  foo_open_file (GError **error)
  {
    gint fd;

    fd = open (\"file.txt\", O_RDONLY);

    if (fd < 0)
      {
        g_set_error (error,
                     FOO_ERROR,                 /* error domain */
                     FOO_ERROR_BLAH,            /* error code */
                     \"Failed to open file: %s\", /* error message */
                     g_strerror (errno));
        return -1;
      @}
    else
      return fd;
  @}
    @end{pre}
    Things are somewhat more complicated if you yourself call another function
    that can report a GError. If the sub-function indicates fatal errors in some
    way other than reporting a GError, such as by returning @code{TRUE} on
    success, you can simply do the following:
    @begin{pre}
  gboolean
  my_function_that_can_fail (GError **err)
  {
    g_return_val_if_fail (err == NULL || *err == NULL, FALSE);

    if (!sub_function_that_can_fail (err))
      {
        /* assert that error was set by the sub-function */
        g_assert (err == NULL || *err != NULL);
        return FALSE;
      @}

    /* otherwise continue, no error occurred */
    g_assert (err == NULL || *err == NULL);
  @}
    @end{pre}
    If the sub-function does not indicate errors other than by reporting a
    GError, you need to create a temporary GError since the passed-in one may be
    @code{NULL}. @code{g_propagate_error()} is intended for use in this case.
    @begin{pre}
  gboolean
  my_function_that_can_fail (GError **err)
  {
    GError *tmp_error;

    g_return_val_if_fail (err == NULL || *err == NULL, FALSE);

    tmp_error = NULL;
    sub_function_that_can_fail (&tmp_error);

    if (tmp_error != NULL)
      {
        /* store tmp_error in err, if err != NULL,
         * otherwise call g_error_free() on tmp_error
         */
        g_propagate_error (err, tmp_error);
        return FALSE;
      @}

    /* otherwise continue, no error occurred */
  @}
    @end{pre}
    Error pileups are always a bug. For example, this code is incorrect:
    @begin{pre}
  gboolean
  my_function_that_can_fail (GError **err)
  {
    GError *tmp_error;

    g_return_val_if_fail (err == NULL || *err == NULL, FALSE);

    tmp_error = NULL;
    sub_function_that_can_fail (&tmp_error);
    other_function_that_can_fail (&tmp_error);

    if (tmp_error != NULL)
      {
        g_propagate_error (err, tmp_error);
        return FALSE;
      @}
  @}
    @end{pre}
    @code{tmp_error} should be checked immediately after
    @code{sub_function_that_can_fail()}, and either cleared or propagated
    upward. The rule is: after each error, you must either handle the error, or
    return it to the calling function. Note that passing @code{NULL} for the
    error location is the equivalent of handling an error by always doing
    nothing about it. So the following code is fine, assuming errors in
    @code{sub_function_that_can_fail()} are not fatal to
    @code{my_function_that_can_fail()}:
    @begin{pre}
  gboolean
  my_function_that_can_fail (GError **err)
  {
    GError *tmp_error;

    g_return_val_if_fail (err == NULL || *err == NULL, FALSE);

    sub_function_that_can_fail (NULL); /* ignore errors */

    tmp_error = NULL;
    other_function_that_can_fail (&tmp_error);

    if (tmp_error != NULL)
      {
        g_propagate_error (err, tmp_error);
        return FALSE;
      @}
  @}
    @end{pre}
    Note that passing @code{NULL} for the error location ignores errors; it is
    equivalent to try @code{{ sub_function_that_can_fail(); @} catch (...) {@}}
    in C++. It does not mean to leave errors unhandled; it means to handle them
    by doing nothing.

    @b{Error domains and codes are conventionally named as follows:}
    @begin{itemize}
      @item{The error domain is called @code{<NAMESPACE>_<MODULE>_ERROR}, for
        example @code{G_SPAWN_ERROR} or @code{G_THREAD_ERROR}:
        @begin{pre}
  #define G_SPAWN_ERROR g_spawn_error_quark ()

  GQuark
  g_spawn_error_quark (void)
    {
      return g_quark_from_static_string (\"g-spawn-error-quark\");
    @}
        @end{pre}}
      @item{The quark function for the error domain is called
        @code{<namespace>_<module>_error_quark}, for example
        @code{g_spawn_error_quark()} or @code{g_thread_error_quark()}.}
      @item{The error codes are in an enumeration called
        @code{<Namespace><Module>Error}; for example, @code{GThreadError} or
        @code{GSpawnError}.}
      @item{Members of the error code enumeration are called
        @code{<NAMESPACE>_<MODULE>_ERROR_<CODE>}, for example
        @code{G_SPAWN_ERROR_FORK} or @code{G_THREAD_ERROR_AGAIN}.}
      @item{If there is a \"generic\" or \"unknown\" error code for
        unrecoverable errors it does not make sense to distinguish with specific
        codes, it should be called @code{<NAMESPACE>_<MODULE>_ERROR_FAILED}, for
        example @code{G_SPAWN_ERROR_FAILED}.}
    @end{itemize}
    @b{Summary of rules for use of GError:}
    @begin{itemize}
      @item{Do not report programming errors via GError.}
      @item{The last argument of a function that returns an error should be a
        location where a GError can be placed (i. e. \"@code{GError** error}\").
        If GError is used with varargs, the @code{GError**} should be the last
        argument before the \"...\".}
      @item{The caller may pass @code{NULL} for the @code{GError**} if they are
        not interested in details of the exact error that occurred.}
      @item{If @code{NULL} is passed for the @code{GError**} argument, then
        errors should not be returned to the caller, but your function should
        still abort and return if an error occurs. That is, control flow should
        not be affected by whether the caller wants to get a GError.}
      @item{If a GError is reported, then your function by definition had a
        fatal failure and did not complete whatever it was supposed to do. If
        the failure was not fatal, then you handled it and you should not report
        it. If it was fatal, then you must report it and discontinue whatever
        you were doing immediately.}
      @item{If a GError is reported, out parameters are not guaranteed to be set
        to any defined value.}
      @item{A @code{GError*} must be initialized to @code{NULL} before passing
        its address to a function that can report errors.}
      @item{\"Piling up\" errors is always a bug. That is, if you assign a new
        GError to a @code{GError*} that is non-@code{NULL}, thus overwriting the
        previous error, it indicates that you should have aborted the operation
        instead of continuing. If you were able to continue, you should have
        cleared the previous error with @code{g_clear_error()}.
        @code{g_set_error()} will complain if you pile up errors.}
      @item{By convention, if you return a boolean value indicating success then
        @code{TRUE} means success and @code{FALSE} means failure. If
        @code{FALSE} is returned, the error must be set to a non-@code{NULL}
        value.}
      @item{A @code{NULL} return value is also frequently used to mean that an
        error occurred. You should make clear in your documentation whether
        @code{NULL} is a valid return value in non-error cases; if @code{NULL}
        is a valid value, then users must check whether an error was returned to
        see if the function succeeded.}
      @item{When implementing a function that can report errors, you may want to
        add a check at the top of your function that the error return location
        is either @code{NULL} or contains a @code{NULL} error (e. g.
        @code{g_return_if_fail (error == NULL || *error == NULL);}).}
    @end{itemize}
    @about-type{g-error}
    @about-function{g-error-new}
    @about-function{g-error-new-literal}
    @about-function{g-error-new-valist}
    @about-function{g-error-free}
    @about-function{g-error-copy}
    @about-function{g-error-matches}
    @about-function{g-set-error}
    @about-function{g-set-error-literal}
    @about-function{g-propagate-error}
    @about-function{g-clear-error}
    @about-function{g-prefix-error}
    @about-function{g-propagate-prefixed-error}
  @end{section}
  @begin[Miscellaneous Utility Functions]{section}
    A selection of portable utility functions.

    @about-function{g-get-application-name}
    @about-function{g-set-application-name}
    @about-function{g-get-prgname}
    @about-function{g-set-prgname}
    @about-function{g-get-environ}
    @about-function{g-environ-getenv}
    @about-function{g-environ-setenv}
    @about-function{g-environ-unsetenv}
    @about-function{g-getenv}
    @about-function{g-setenv}
    @about-function{g-unsetenv}
    @about-function{g-listenv}
    @about-function{g-get-user-name}
    @about-function{g-get-real-name}
    @about-function{g-get-user-cache-dir}
    @about-function{g-get-user-data-dir}
    @about-function{g-get-user-config-dir}
    @about-function{g-get-user-runtime-dir}
    @about-type{g-user-directory}
    @about-function{g-get-user-special-dir}
    @about-function{g-get-system-data-dirs}
    @about-function{g-get-system-config-dirs}
    @about-function{g-reload-user-special-dirs-cache}
    @about-function{g-get-host-name}
    @about-function{g-get-home-dir}
    @about-function{g-get-tmp-dir}
    @about-function{g-get-current-dir}
    @about-function{g-basename}
    @about-function{g-dirname}
    @about-function{g-path-is-absolute}
    @about-function{g-path-skip-root}
    @about-function{g-path-get-basename}
    @about-function{g-path-get-dirname}
    @about-function{g-build-filename}
    @about-function{g-build-filenamev}
    @about-function{g-build-path}
    @about-function{g-build-pathv}
    @about-function{g-format-size}
    @about-function{GFormatSizeFlags}
    @about-function{g-format-size-full}
    @about-function{g-format-size-for-display}
    @about-function{g-find-program-in-path}
    @about-function{g-bit-nth-lsf}
    @about-function{g-bit-nth-msf}
    @about-function{g-bit-storage}
    @about-function{g-spaced-primes-closest}
    @about-function{g-atexit}
    @about-function{g-parse-debug-string}
    @about-function{GDebugKey}
    @about-function{g-qsort-with-data}
    @about-function{g-nullify-pointer}
  @end{section}
  @begin[GVariantType]{section}
    Introduction to the GVariant type system.

    @about-symbol{g-variant-type}
    @about-variable{+g-variant-type-boolean+}
    @about-variable{+g-variant-type-byte+}
    @about-variable{+g-variant-type-int16+}
    @about-variable{+g-variant-type-uint16+}
    @about-variable{+g-variant-type-int32}
    @about-variable{+g-variant-type-uint32+}
    @about-variable{+g-variant-type-int64+}
    @about-variable{+g-variant-type-uint64+}
    @about-variable{+g-variant-type-handle+}
    @about-variable{+g-variant-type-double+}
    @about-variable{+g-variant-type-string+}
    @about-variable{+g-variant-type-object-path+}
    @about-variable{+g-variant-type-signature+}
    @about-variable{+g-variant-type-variant+}
    @about-variable{+g-variant-type-any+}
    @about-variable{+g-variant-type-basic+}
    @about-variable{+g-variant-type-maxbe+}
    @about-variable{+g-variant-type-array+}
    @about-variable{+g-variant-type-tulpe+}
    @about-variable{+g-variant-type-unit+}
    @about-variable{+g-variant-type-dict-entry+}
    @about-variable{+g-variant-type-dictionary+}
    @about-variable{+g-variant-type-string-array+}
    @about-variable{+g-variant-type-object-path-array+}
    @about-variable{+g-variant-type-bytestring+}
    @about-variable{+g-variant-type-bytestring-array+}
    @about-variable{+g-variant-type-vardict+}
    @about-function{g-variant-type}
    @about-function{g-variant-type-free}
    @about-function{g-variant-type-copy}
    @about-function{g-variant-type-new}
    @about-function{g-variant-type-string-is-valid}
    @about-function{g-variant-type-string-scan}
    @about-function{g-variant-type-get-string-length}
    @about-function{g-variant-type-peek-string}
    @about-function{g-variant-type-dup-string}
    @about-function{g-variant-type-is-definite}
    @about-function{g-variant-type-is-container}
    @about-function{g-variant-type-is-basic}
    @about-function{g-variant-type-is-maybe}
    @about-function{g-variant-type-is-array}
    @about-function{g-variant-type-is-tuple}
    @about-function{g-variant-type-is-dict-entry}
    @about-function{g-variant-type-is-variant}
    @about-function{g-variant-type-hash}
    @about-function{g-variant-type-equal}
    @about-function{g-variant-type-is-subtype-of}
    @about-function{g-variant-type-new-maybe}
    @about-function{g-variant-type-new-array}
    @about-function{g-variant-type-new-tuple}
    @about-function{g-variant-type-new-dict-entry}
    @about-function{g-variant-type-element}
    @about-function{g-variant-type-n-items}
    @about-function{g-variant-type-first}
    @about-function{g-variant-type-next}
    @about-function{g-variant-type-key}
    @about-function{g-variant-type-value}
  @end{section}
  @begin[GVariant]{section}
    Strongly typed value datatype.

    @about-symbol{g-variant}
    @about-function{g-variant-unref}
    @about-function{g-variant-ref}
    @about-function{g-variant-ref-sink}
    @about-function{g-variant-is-floating}
    @about-function{g-variant-take-ref}
    @about-function{g-variant-get-type}
    @about-function{g-variant-get-type-string}
    @about-function{g-variant-is-of-type}
    @about-function{g-variant-is-container}
    @about-function{g-variant-compare}
    @about-function{g-variant-classify}
    @about-symbol{g-variant-class}
    @about-function{g-variant-get}
    @about-function{g-variant-get-va}
    @about-function{g-variant-new}
    @about-function{g-variant-new-va}
    @about-function{g-variant-new-boolean}
    @about-function{g-variant-new-byte}
    @about-function{g-variant-new-int16}
    @about-function{g-variant-new-uint16}
    @about-function{g-variant-new-int32}
    @about-function{g-variant-new-uint32}
    @about-function{g-variant-new-int64}
    @about-function{g-variant-new-uint64}
    @about-function{g-variant-new-handle}
    @about-function{g-variant-new-double}
    @about-function{g-variant-new-string}
    @about-function{g-variant-new_object-path}
    @about-function{g-variant-is-object-path}
    @about-function{g-variant-new-signature}
    @about-function{g-variant-is-signature}
    @about-function{g-variant-new-variant}
    @about-function{g-variant-new-strv}
    @about-function{g-variant-new-objv}
    @about-function{g-variant-new-bytestring}
    @about-function{g-variant-new-bytestring-array}
    @about-function{g-variant-get-boolean}
    @about-function{g-variant-get-byte}
    @about-function{g-variant-get-int16}
    @about-function{g-variant-get-uint16}
    @about-function{g-variant-get-int32}
    @about-function{g-variant-get-uint32}
    @about-function{g-variant-get-int64}
    @about-function{g-variant-get-int64}
    @about-function{g-variant-get-handle}
    @about-function{g-variant-get-double}
    @about-function{g-variant-get-string}
    @about-function{g-variant-dup-string}
    @about-function{g-variant-get-variant}
    @about-function{g-variant-get-strv}
    @about-function{g-variant-dup-strv}
    @about-function{g-variant-get-objv}
    @about-function{g-variant-dup-objv}
    @about-function{g-variant-get-bytestring}
    @about-function{g-variant-dup-bytestring}
    @about-function{g-variant-get-bytestring_array}
    @about-function{g-variant-dup-bytestring-array}
    @about-function{g-variant-new-maybe}
    @about-function{g-variant-new-array}
    @about-function{g-variant-new-tuple}
    @about-function{g-variant-new-dict-entry}
    @about-function{g-variant-new-new-fixed-array}
    @about-function{g-variant-get-maybe}
    @about-function{g-variant-n-children}
    @about-function{g-variant-get-child-value}
    @about-function{g-variant-get-child}
    @about-function{g-variant-lookup-value}
    @about-function{g-variant-lookup}
    @about-function{g-variant-get-fixed-array}
    @about-function{g-variant-get-size}
    @about-function{g-variant-get-data}
    @about-function{g-variant-store}
    @about-function{g-variant-new-from-data}
    @about-function{g-variant-byteswap}
    @about-function{g-variant-get-normal-form}
    @about-function{g-variant-g-variant-is-normal-form}
    @about-function{g-variant-hash}
    @about-function{g-variant-equal}
    @about-function{g-variant-print}
    @about-function{g-variant-string}
    @about-symbol{g-variant-iter}
    @about-function{g-variant-iter-copy}
    @about-function{g-variant-iter-free}
    @about-function{g-variant-iter-init}
    @about-function{g-variant-iter-n-children}
    @about-function{g-variant-iter-new}
    @about-function{g-variant-iter-next-value}
    @about-function{g-variant-iter-next}
    @about-function{g-variant-iter-loop}
    @about-function{g-variant-builder}
    @about-function{g-variant-builder-unref}
    @about-function{g-variant-builder-ref}
    @about-function{g-variant-builder-new}
    @about-function{g-variant-builder-init}
    @about-function{g-variant-builder-clear}
    @about-function{g-variant-builder-add-value}
    @about-function{g-variant-builder-add}
    @about-function{g-variant-builder-add-parsed}
    @about-function{g-variant-builder-end}
    @about-function{g-variant-builder-open}
    @about-function{g-variant-builder-close}
    @about-symbol{g-variant-parse-error}
    @about-symbol{G_VARIANT_PARSE_ERROR}
    @about-function{g-variant-parse}
    @about-function{g-variant-new-parsed-va}
    @about-function{g-variant-new-parsed}
  @end{section}")

;;; --- End of file glib.package.lisp ------------------------------------------
