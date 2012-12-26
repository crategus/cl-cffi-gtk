;;; ----------------------------------------------------------------------------
;;; atdoc-glib.package.lisp
;;;
;;; Documentation strings for the library GLib.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
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

(in-package :glib)

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

    The GLib headers annotate deprecated APIs in a way that produces compiler
    warnings if these deprecated APIs are used. The warnings can be turned off
    by defining the macro @code{GLIB_DISABLE_DEPRECATION_WARNINGS} before
    including the @code{glib.h} header.

    GLib also provides support for building applications against defined subsets
    of deprecated or new GLib APIs. Define the macro
    @code{GLIB_VERSION_MIN_REQUIRED} to specify up to what version of GLib you
    want to receive warnings about deprecated APIs. Define the macro
    @code{GLIB_VERSION_MAX_ALLOWED} to specify the newest version of GLib whose
    API you want to use.

    @about-symbol{*glib-major-version*}
    @about-symbol{*glib-minor-version*}
    @about-symbol{*glib-micro-version*}
    @about-symbol{*glib-binary-age*}
    @about-symbol{*glib-interface-age*}
    @about-function{glib-check-version}
  @end{section}
  @begin[Miscellaneous]{section}
    Documentation of several type definitions and functions, which are
    needed for the implemenation of the GTK+ library, but are not fully
    implemented.

    @b{Basic Types}

    Standard GLib types, defined for ease-of-use and portability.
    Only the following types are implemented:

    @about-type{g-size}
    @about-type{g-ssize}
    @about-type{g-offset}
    @b{Memory Allocation}

    The following functions for the general memory-handling are implemented:

    @about-function{g-malloc}
    @about-function{g-free}
    @b{Date and Time Functions}

    Calendrical calculations and miscellaneous time stuff.
    Only the following struct is implemented:

    @about-type{g-time-val}
    @b{String Utility Functions - Various string-related functions}

    Implemented is:

    @about-type{g-string}
    @about-type{g-strv}
    @b{Doubly-Linked Lists}

    Linked lists containing integer values or pointers to data, with the ability
    to iterate over the list in both directions

    Implemented is:

    @about-type{g-list}
    @b{Singly-Linked Lists}

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
    The Main Event Loop manages all available sources of events.

    The main event loop manages all the available sources of events for GLib and
    GTK+ applications. These events can come from any number of different types
    of sources such as file descriptors (plain files, pipes or sockets) and
    timeouts. New types of event sources can also be added using
    @fun{g-source-attach}.

    To allow multiple independent sets of sources to be handled in different
    threads, each source is associated with a @code{GMainContext}. A
    @code{GMainContext} can only be running in a single thread, but sources can
    be added to it and removed from it from other threads.

    Each event source is assigned a priority. The default priority,
    @code{G_PRIORITY_DEFAULT}, is 0. Values less than 0 denote higher
    priorities. Values greater than 0 denote lower priorities. Events from high
    priority sources are always processed before events from lower priority
    sources.

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

    @b{Creating new source types}

    One of the unusual features of the @type{g-main-loop} functionality is that
    new types of event source can be created and used in addition to the builtin
    type of event source. A new event source type is used for handling GDK
    events. A new source type is created by deriving from the GSource structure.
    The derived type of source is represented by a structure that has the
    GSource structure as a first element, and other elements specific to the new
    source type. To create an instance of the new source type, call
    @fun{g-source-new} passing in the size of the derived structure and a table
    of functions. These GSourceFuncs determine the behavior of the new source
    type.

    New source types basically interact with the main context in two ways. Their
    prepare function in GSourceFuncs can set a timeout to determine the maximum
    amount of time that the main loop will sleep before checking the source
    again. In addition, or as well, the source can add file descriptors to the
    set that the main context checks using @fun{g-source-add-poll}.

    @b{Customizing the main loop iteration}

    Single iterations of a GMainContext can be run with
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
    @about-function{GSource}
    @about-function{GSourceFuncs}
    @about-function{GSourceCallbackFuncs}
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

    Quarks are associations between strings and integer identifiers. Given
    either the string or the GQuark identifier it is possible to retrieve the
    other.

    Quarks are used for both Datasets and Keyed Data Lists.

    To create a new quark from a string, use @fun{g-quark-from-string} or
    @code{g_quark_from_static_string()}.

    To find the string corresponding to a given GQuark, use
    @fun{g-quark-to-string}.

    To find the GQuark corresponding to a given string, use
    g_quark_try_string().

    Another use for the string pool maintained for the quark functions is string
    interning, using g_intern_string() or g_intern_static_string(). An interned
    string is a canonical representation for a string. One important advantage
    of interned strings is that they can be compared for equality by a simple
    pointer comparison, rather than using strcmp().

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
    languages.) It's important to understand that this method is both a data
    type (the GError object) and a set of rules. If you use GError incorrectly,
    then your code will not properly interoperate with other code that uses
    GError, and users of your API will probably get confused.

    First and foremost: GError should only be used to report recoverable runtime
    errors, never to report programming errors. If the programmer has screwed
    up, then you should use g_warning(), g_return_if_fail(), g_assert(),
    g_error(), or some similar facility. (Incidentally, remember that the
    g_error() function should only be used for programming errors, it should not
    be used to print any error reportable via GError.)

    Examples of recoverable runtime errors are \"file not found\" or \"failed to
    parse input.\" Examples of programming errors are \"NULL passed to strcmp()\"
    or \"attempted to free the same pointer twice.\" These two kinds of errors are
    fundamentally different: runtime errors should be handled or reported to the
    user, programming errors should be eliminated by fixing the bug in the
    program. This is why most functions in GLib and GTK+ do not use the GError
    facility.

    Functions that can fail take a return location for a GError as their last
    argument. For example:
    @begin{pre}
  gboolean g_file_get_contents (const gchar  *filename,
                                gchar       **contents,
                                gsize        *length,
                                GError      **error);
    @end{pre}
    If you pass a non-NULL value for the error argument, it should point to a
    location where an error can be placed. For example:
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
    Note that err != NULL in this example is a reliable indicator of whether
    g_file_get_contents() failed. Additionally, g_file_get_contents() returns a
    boolean which indicates whether it was successful.

    Because g_file_get_contents() returns FALSE on failure, if you are only
    interested in whether it failed and don't need to display an error message,
    you can pass NULL for the error argument:
    @begin{pre}
  if (g_file_get_contents (\"foo.txt\", &contents, NULL, NULL))
     /* no error occurred */ ;
  else
    /* error */ ;
    @end{pre}
    The GError object contains three fields: domain indicates the module the
    error-reporting function is located in, code indicates the specific error
    that occurred, and message is a user-readable error message with as many
    details as possible. Several functions are provided to deal with an error
    received from a called function: g_error_matches() returns TRUE if the error
    matches a given domain and code, g_propagate_error() copies an error into an
    error location (so the calling function will receive it), and
    g_clear_error() clears an error location by freeing the error and resetting
    the location to NULL. To display an error to the user, simply display
    error->message, perhaps along with additional context known only to the
    calling function (the file being opened, or whatever -- though in the
    g_file_get_contents() case, error->message already contains a filename).

    When implementing a function that can report errors, the basic tool is
    g_set_error(). Typically, if a fatal error occurs you want to g_set_error(),
    then return immediately. g_set_error() does nothing if the error location
    passed to it is NULL. Here's an example:
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
    way other than reporting a GError, such as by returning TRUE on success, you
    can simply do the following:
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
    NULL. g_propagate_error() is intended for use in this case.
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
    tmp_error should be checked immediately after sub_function_that_can_fail(),
    and either cleared or propagated upward. The rule is: after each error, you
    must either handle the error, or return it to the calling function. Note
    that passing NULL for the error location is the equivalent of handling an
    error by always doing nothing about it. So the following code is fine,
    assuming errors in sub_function_that_can_fail() are not fatal to
    my_function_that_can_fail():
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
    Note that passing NULL for the error location ignores errors; it's
    equivalent to try { sub_function_that_can_fail(); @} catch (...) {@} in C++.
    It does not mean to leave errors unhandled; it means to handle them by doing
    nothing.
 
    @b{Error domains and codes are conventionally named as follows:}
    @begin{itemize}
      @item{The error domain is called <NAMESPACE>_<MODULE>_ERROR, for example
        G_SPAWN_ERROR or G_THREAD_ERROR:
        @begin{pre}
  #define G_SPAWN_ERROR g_spawn_error_quark ()

  GQuark
  g_spawn_error_quark (void)
    {
      return g_quark_from_static_string (\"g-spawn-error-quark\");
    @}
        @end{pre}}
      @item{The quark function for the error domain is called
        <namespace>_<module>_error_quark, for example g_spawn_error_quark() or
        g_thread_error_quark().}
      @item{The error codes are in an enumeration called <Namespace><Module>Error;
        for example,GThreadError or GSpawnError.}
      @item{Members of the error code enumeration are called
        <NAMESPACE>_<MODULE>_ERROR_<CODE>, for example G_SPAWN_ERROR_FORK or
        G_THREAD_ERROR_AGAIN.}
      @item{If there's a \"generic\" or \"unknown\" error code for unrecoverable errors
        it doesn't make sense to distinguish with specific codes, it should be
        called <NAMESPACE>_<MODULE>_ERROR_FAILED, for example
        G_SPAWN_ERROR_FAILED.}
    @end{itemize}
    @b{Summary of rules for use of GError:}
    @begin{itemize}
      @item{Do not report programming errors via GError.}
      @item{The last argument of a function that returns an error should be a
        location where a GError can be placed (i.e. \"GError** error\"). If GError
        is used with varargs, the GError** should be the last argument before
        the \"...\".}
      @item{The caller may pass NULL for the GError** if they are not interested in
        details of the exact error that occurred.}
      @item{If NULL is passed for the GError** argument, then errors should not be
        returned to the caller, but your function should still abort and return
        if an error occurs. That is, control flow should not be affected by
        whether the caller wants to get a GError.}
      @item{If a GError is reported, then your function by definition had a fatal
        failure and did not complete whatever it was supposed to do. If the
        failure was not fatal, then you handled it and you should not report it.
        If it was fatal, then you must report it and discontinue whatever you
        were doing immediately.}
      @item{If a GError is reported, out parameters are not guaranteed to be set to
        any defined value.}
      @item{A GError* must be initialized to NULL before passing its address to a
        function that can report errors.}
      @item{\"Piling up\" errors is always a bug. That is, if you assign a new GError
        to a GError* that is non-NULL, thus overwriting the previous error, it
        indicates that you should have aborted the operation instead of
        continuing. If you were able to continue, you should have cleared the
        previous error with g_clear_error(). g_set_error() will complain if you
        pile up errors.}
      @item{By convention, if you return a boolean value indicating success then
        TRUE means success and FALSE means failure. If FALSE is returned, the
        error must be set to a non-NULL value.}
      @item{A NULL return value is also frequently used to mean that an error
        occurred. You should make clear in your documentation whether NULL is a
        valid return value in non-error cases; if NULL is a valid value, then
        users must check whether an error was returned to see if the function
        succeeded.}
      @item{When implementing a function that can report errors, you may want to add
        a check at the top of your function that the error return location is
        either NULL or contains a NULL error (e.g. g_return_if_fail
        (error == NULL || *error == NULL);).}
    @end{itemize}
    @b{Synopsis}

    @about-function{GError}
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
    @about-function{GUserDirectory}
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
  @end{section}")

;;; ----------------------------------------------------------------------------
;;;
;;; glib.main-loop.lisp
;;;
;;; ----------------------------------------------------------------------------

;;; GMainContext

(setf (documentation 'g-main-context-pending 'function)
 "@argument[context]{a GMainContext (if NULL, the default context will be used)}
  @return{TRUE if events are pending.}
  Checks if any sources have pending events for the given context.")

(setf (documentation 'g-main-context-find-source-by-id 'function)
 "@argument[context]{a GMainContext (if NULL, the default context will be used)}
  @argument[source_id]{the source ID, as returned by g_source_get_id().}
  @return{the GSource if found, otherwise, NULL}
  Finds a GSource given a pair of context and ID.")

(setf (documentation 'g-main-context-find-source-by-user-data 'function)
 "@argument[context]{a GMainContext}
  @argument[user-data]{the user_data for the callback.}
  @return{the source, if one was found, otherwise NULL}
  Finds a source with the given user data for the callback. If multiple
  sources exist with the same user data, the first one found will be returned.")

(setf (documentation 'g-main-context-find-source-by-funcs-user-data 'function)
 "@argument[context]{a GMainContext (if NULL, the default context will be used)}
  @argument[funcs]{the source_funcs passed to g_source_new().}
  @argument[user-data]{the user data from the callback.}
  @return{the source, if one was found, otherwise NULL}
  Finds a source with the given source functions and user data. If multiple
  sources exist with the same source function and user data, the first one
  found will be returned.")

(setf (documentation 'g-main-context-wakeup 'function)
 "@argument[context]{a GMainContext}
  If context is currently waiting in a poll(), interrupt the poll(), and
  continue the iteration process.")

(setf (documentation 'g-main-context-acquire 'function)
 "@argument[context]{a GMainContext}
  @return{TRUE if the operation succeeded, and this thread is now the owner of
    context.}
  Tries to become the owner of the specified context. If some other thread is
  the owner of the context, returns FALSE immediately. Ownership is properly
  recursive: the owner can require ownership again and will release ownership
  when g_main_context_release() is called as many times as
  g_main_context_acquire().

  You must be the owner of a context before you can call
  g_main_context_prepare(), g_main_context_query(), g_main_context_check(),
  g_main_context_dispatch().")

(setf (documentation 'g-main-context-release 'function)
 "@argument[context]{a GMainContex}
  Releases ownership of a context previously acquired by this thread with
  g_main_context_acquire(). If the context was acquired multiple times, the
  ownership will be released only when g_main_context_release() is called as
  many times as it was acquired.")

(setf (documentation 'g-main-context-is-owner 'function)
 "@argument[context]{a GMainContext}
  @return{TRUE if current thread is owner of context.}
  Determines whether this thread holds the (recursive) ownership of this
  GMainContext. This is useful to know before waiting on another thread that
  may be blocking to get ownership of context.

 Since 2.10")

(setf (documentation 'g-main-context-wait 'function)
 "@arg{context]{a GMainContext}
  @argument[cond]{a condition variable}
  @argument[mutex]{a mutex, currently held}
  @return{TRUE if the operation succeeded, and this thread is now the owner of
    context.}
  Tries to become the owner of the specified context, as with
  g_main_context_acquire(). But if another thread is the owner, atomically
  drop mutex and wait on cond until that owner releases ownership or until
  cond is signaled, then try again (once) to become the owner.")

(setf (documentation 'g-main-context-prepare 'function)
 "@argument[context]{a GMainContext}
  @argument[priority]{location to store priority of highest priority source already
    ready.}
  @return{TRUE if some source is ready to be dispatched prior to polling.}
  Prepares to poll sources within a main loop. The resulting information for
  polling is determined by calling g_main_context_query().")

(setf (documentation 'g-main-context-query 'function)
 "@argument[context]{a GMainContext}
  @argument[max_priority]{maximum priority source to check}
  @argument[timeout_]{location to store timeout to be used in polling}
  @argument[fds]{location to store GPollFD records that need to be polled}
  @argument[n_fds]{length of fds.}
  @return{the number of records actually stored in fds, or, if more than n_fds
    records need to be stored, the number of records that need to be stored.}
  Determines information necessary to poll this main loop.")

(setf (documentation 'g-main-context-check 'function)
 "@argument[context]{a GMainContext}
  @argument[max_priority]{the maximum numerical priority of sources to check}
  @argument[fds]{array of GPollFD's that was passed to the last call to
    g_main_context_query()}
  @argument[n_fds]{return value of g_main_context_query()}
  @return{TRUE if some sources are ready to be dispatched.}
  Passes the results of polling back to the main loop.")

(setf (documentation 'g-main-context-dispatch 'function)
 "@argument[context]{a GMainContext}
  Dispatches all pending sources.")

(setf (documentation 'g-main-context-set-poll-func 'function)
 "@argument[context]{a GMainContext}
  @argument[func]{the function to call to poll all file descriptors}
  Sets the function to use to handle polling of file descriptors. It will be
  used instead of the poll() system call (or GLib's replacement function,
  which is used where poll() isn't available).

  This function could possibly be used to integrate the GLib event loop with
  an external event loop.")

(setf (documentation 'g-main-context-get-poll-func 'function)
 "@argument[context]{a GMainContext}
  @return{the poll function}
  Gets the poll function set by g_main_context_set_poll_func().")

(setf (documentation 'g-main-context-add-poll 'function)
 "@argument[context]{a GMainContext (or NULL for the default context)}
  @argument[fd]{a GPollFD structure holding information about a file descriptor to
    watch.}
  @argument[priority]{the priority for this file descriptor which should be the same
    as the priority used for g_source_attach() to ensure that the file
    descriptor is polled whenever the results may be needed.}
  Adds a file descriptor to the set of file descriptors polled for this
  context. This will very seldom be used directly. Instead a typical event
  source will use g_source_add_poll() instead.")

(setf (documentation 'g-main-context-remove-poll 'function)
 "@argument[context]{a GMainContext}
  @argument[fd]{a GPollFD descriptor previously added with g_main_context_add_poll()}
  Removes file descriptor from the set of file descriptors to be polled for a
  particular context.")

(setf (documentation 'g-main-depth 'function)
 "@return{The main loop recursion level in the current thread}
  Returns the depth of the stack of calls to g_main_context_dispatch() on any
  GMainContext in the current thread. That is, when called from the toplevel,
  it gives 0. When called from within a callback from
  g_main_context_iteration() (or g_main_loop_run(), etc.) it returns 1. When
  called from within a callback to a recursive call to
  g_main_context_iteration(), it returns 2. And so forth.

  This function is useful in a situation like the following: Imagine an
  extremely simple \"garbage collected\" system.
  @begin{pre}
  static GList *free_list;

  gpointer
  allocate_memory (gsize size)
  {
    gpointer result = g_malloc (size);
    free_list = g_list_prepend (free_list, result);
    return result;
  @}

  void
  free_allocated_memory (void)
  {
    GList *l;
    for (l = free_list; l; l = l->next);
      g_free (l->data);
    g_list_free (free_list);
    free_list = NULL;
   @}

  [...]

  while (TRUE);
   {
     g_main_context_iteration (NULL, TRUE);
     free_allocated_memory();
    @}
  @end{pre}
  This works from an application, however, if you want to do the same thing
  from a library, it gets more difficult, since you no longer control the main
  loop. You might think you can simply use an idle function to make the call
  to free_allocated_memory(), but that doesn't work, since the idle function
  could be called from a recursive callback. This can be fixed by using
  g_main_depth()
  @begin{pre}
  gpointer
  allocate_memory (gsize size)
  {
    FreeListBlock *block = g_new (FreeListBlock, 1);
    block->mem = g_malloc (size);
    block->depth = g_main_depth ();
    free_list = g_list_prepend (free_list, block);
    return block->mem;
  @}

  void
  free_allocated_memory (void)
  {
    GList *l;

    int depth = g_main_depth ();
    for (l = free_list; l; );
      {
        GList *next = l->next;
        FreeListBlock *block = l->data;
        if (block->depth > depth)
          {
            g_free (block->mem);
            g_free (block);
            free_list = g_list_delete_link (free_list, l);
          @}

        l = next;
      @}
    @}
  @end{pre}
  There is a temptation to use g_main_depth() to solve problems with
  reentrancy. For instance, while waiting for data to be received from the
  network in response to a menu item, the menu item might be selected again.
  It might seem that one could make the menu item's callback return
  immediately and do nothing if g_main_depth() returns a value greater than 1.
  However, this should be avoided since the user then sees selecting the menu
  item do nothing. Furthermore, you'll find yourself adding these checks all
  over your code, since there are doubtless many, many things that the user
  could do. Instead, you can use the following techniques:
  @begin{itemize}
    @item{Use gtk_widget_set_sensitive() or modal dialogs to prevent the user
      from interacting with elements while the main loop is recursing.}
    @item{Avoid main loop recursion in situations where you can't handle
      arbitrary callbacks. Instead, structure your code so that you simply
      return to the main loop and then get called again when there is more work
      to do.}
  @end{itemize}")

(setf (documentation 'g-main-current-source 'function)
 "@return{The currently firing source or NULL.}
  Returns the currently firing source for this thread.

  Since 2.12")

(setf (documentation 'g-timeout-source-new 'function)
 "@argument[interval]{the timeout interval in milliseconds.}
  @return{the newly-created timeout source}
  @short{Creates a new timeout source.}

  The source will not initially be associated with any GMainContext and must
  be added to one with g_source_attach() before it will be executed.

  The interval given is in terms of monotonic time, not wall clock time. See
  g_get_monotonic_time().")

(setf (documentation 'g-timeout-source-new-seconds 'function)
 "@argument[interval]{the timeout interval in seconds}
  @return{the newly-created timeout source}
  @short{Creates a new timeout source.}

  The source will not initially be associated with any GMainContext and must
  be added to one with g_source_attach() before it will be executed.

  The scheduling granularity/accuracy of this timeout source will be in
  seconds.

  The interval given in terms of monotonic time, not wall clock time. See
  g_get_monotonic_time().

  Since 2.14")

(setf (documentation 'g-timeout-add 'function)
 "@argument[interval]{the time between calls to the function, in milliseconds
    (1/1000ths of asecond)}
  @argument[function]{function to call}
  @argument[data]{data to pass to function}
  @return{the ID (greater than 0) of the event source.}
  Sets a function to be called at regular intervals, with the default
  priority, G_PRIORITY_DEFAULT. The function is called repeatedly until it
  returns FALSE, at which point the timeout is automatically destroyed and the
  function will not be called again. The first call to the function will be at
  the end of the first interval.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval (it does not
  try to 'catch up' time lost in delays).

  If you want to have a timer in the \"seconds\" range and do not care about the
  exact time of the first call of the timer, use the g_timeout_add_seconds()
  function; this function allows for more optimizations and more efficient
  system power usage.

  This internally creates a main loop source using g_timeout_source_new() and
  attaches it to the main loop context using g_source_attach(). You can do
  these steps manually if you need greater control.

  The interval given is in terms of monotonic time, not wall clock time. See
  g_get_monotonic_time().")

(setf (documentation 'g-timeout-add-full 'function)
 "@argument[priority]{the priority of the timeout source. Typically this will be in
    the range between G_PRIORITY_DEFAULT and G_PRIORITY_HIGH.}
  @argument[interval]{the time between calls to the function, in milliseconds
    (1/1000ths of a second)}
  @argument[function]{function to call}
  @argument[data]{data to pass to function}
  @argument[notify]{function to call when the timeout is removed, or NULL}
  @return{the ID (greater than 0) of the event source. Rename to: g_timeout_add}
  Sets a function to be called at regular intervals, with the given priority.
  The function is called repeatedly until it returns FALSE, at which point the
  timeout is automatically destroyed and the function will not be called
  again. The notify function is called when the timeout is destroyed. The
  first call to the function will be at the end of the first interval.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval (it does not
  try to 'catch up' time lost in delays).

  This internally creates a main loop source using g_timeout_source_new() and
  attaches it to the main loop context using g_source_attach(). You can do
  these steps manually if you need greater control.

  The interval given in terms of monotonic time, not wall clock time. See
  g_get_monotonic_time().")

(setf (documentation 'g-timeout-add-seconds 'function)
 "@argument[interval]{the time between calls to the function, in seconds}
  @argument[function]{function to call}
  @argument[data]{data to pass to function}
  @return{the ID (greater than 0) of the event source.}
  Sets a function to be called at regular intervals with the default priority,
  G_PRIORITY_DEFAULT. The function is called repeatedly until it returns
  FALSE, at which point the timeout is automatically destroyed and the
  function will not be called again.

  This internally creates a main loop source using
  g_timeout_source_new_seconds() and attaches it to the main loop context
  using g_source_attach(). You can do these steps manually if you need greater
  control. Also see g_timeout_add_seconds_full().

  Note that the first call of the timer may not be precise for timeouts of one
  second. If you need finer precision and have such a timeout, you may want to
  use g_timeout_add() instead.

  The interval given is in terms of monotonic time, not wall clock time. See
  g_get_monotonic_time().

  Since 2.14")

(setf (documentation 'g-timeout-add-seconds-full 'function)
 "@argument[priority]{the priority of the timeout source. Typically this will be in
    the range between G_PRIORITY_DEFAULT and G_PRIORITY_HIGH.}
  @argument[interval]{the time between calls to the function, in seconds}
  @argument[function]{function to call}
  @argument[data]{data to pass to function}
  @argument[notify]{function to call when the timeout is removed, or NULL}
  @return{the ID (greater than 0) of the event source. Rename to:
    g_timeout_add_seconds}
  Sets a function to be called at regular intervals, with priority. The
  function is called repeatedly until it returns FALSE, at which point the
  timeout is automatically destroyed and the function will not be called
  again.

  Unlike g_timeout_add(), this function operates at whole second granularity.
  The initial starting point of the timer is determined by the implementation
  and the implementation is expected to group multiple timers together so that
  they fire all at the same time. To allow this grouping, the interval to the
  first timer is rounded and can deviate up to one second from the specified
  interval. Subsequent timer iterations will generally run at the specified
  interval.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval

  If you want timing more precise than whole seconds, use g_timeout_add()
  instead.

  The grouping of timers to fire at the same time results in a more power and
  CPU efficient behavior so if your timer is in multiples of seconds and you
  don't require the first timer exactly one second from now, the use of
  g_timeout_add_seconds() is preferred over g_timeout_add().

  This internally creates a main loop source using
  g_timeout_source_new_seconds() and attaches it to the main loop context
  using g_source_attach(). You can do these steps manually if you need greater
  control.

  The interval given is in terms of monotonic time, not wall clock time. See
  g_get_monotonic_time().

 Since 2.14")

(setf (documentation 'g-idle-source-new 'function)
 "@return{the newly-created idle source}
  @short{Creates a new idle source.}

  The source will not initially be associated with any GMainContext and must
  be added to one with g_source_attach() before it will be executed. Note that
  the default priority for idle sources is G_PRIORITY_DEFAULT_IDLE, as
  compared to other sources which have a default priority of
  G_PRIORITY_DEFAULT.")

(setf (documentation 'g-idle-add 'function)
 "@argument[function]{function to call}
  @argument[data]{data to pass to function.}
  @return{the ID (greater than 0) of the event source.}
  Adds a function to be called whenever there are no higher priority events
  pending to the default main loop. The function is given the default idle
  priority, G_PRIORITY_DEFAULT_IDLE. If the function returns FALSE it is
  automatically removed from the list of event sources and will not be called
  again.

  This internally creates a main loop source using g_idle_source_new() and
  attaches it to the main loop context using g_source_attach(). You can do
  these steps manually if you need greater control.")

(setf (documentation 'g-idle-add-full 'function)
 "@argument[priority]{the priority of the idle source. Typically this will be in the
    range between G_PRIORITY_DEFAULT_IDLE and G_PRIORITY_HIGH_IDLE.}
  @argument[function]{function to call}
  @argument[data]{data to pass to function}
  @argument[notify]{function to call when the idle is removed, or NULL}
  @return{the ID (greater than 0) of the event source. Rename to: g_idle_add}
  Adds a function to be called whenever there are no higher priority events
  pending. If the function returns FALSE it is automatically removed from the
  list of event sources and will not be called again.

  This internally creates a main loop source using g_idle_source_new() and
  attaches it to the main loop context using g_source_attach(). You can do
  these steps manually if you need greater control.")

(setf (documentation 'g-idle-remove-by-data 'function)
 "@argument[data]{the data for the idle source's callback.}
  @return{TRUE if an idle source was found and removed.}
  Removes the idle function with the given data.")

(setf (documentation 'g-source-callback-funcs 'type)
 "The GSourceCallbackFuncs struct contains functions for managing callback
  objects.
  @begin{pre}
  struct GSourceCallbackFuncs

  struct GSourceCallbackFuncs {
    void (*ref)   (gpointer     cb_data);
    void (*unref) (gpointer     cb_data);
    void (*get)   (gpointer     cb_data,
                   GSource     *source,
                   GSourceFunc *func,
                   gpointer    *data);
  @};

  ref ()
      Called when a reference is added to the callback object
 
  unref ()
      Called when a reference to the callback object is dropped
 
  get ()
      Called to extract the callback function and data from the callback
      object.
  @end{pre}")

(setf (documentation 'g-source-new 'function)
 "@argument[source-funcs]{structure containing functions that implement the sources
    behavior.}
  @argument[struct-size]{size of the GSource structure to create.}
  @return{the newly-created GSource.}
  Creates a new GSource structure. The size is specified to allow creating
  structures derived from GSource that contain additional data. The size
  passed in must be at least sizeof (GSource).

  The source will not initially be associated with any GMainContext and must
  be added to one with g_source_attach() before it will be executed.")

(setf (documentation 'g-source-ref 'function)
 "@argument[source]{a GSource}
  @return{source}
  Increases the reference count on a source by one.")

(setf (documentation 'g-source-unref 'function)
 "@argument[source]{a GSource}
  Decreases the reference count of a source by one. If the resulting reference
  count is zero the source and associated memory will be destroyed.")

(setf (documentation 'g-source-set-funcs 'function)
 "@argument[source]{a GSource}
  @argument[funcs]{the new GSourceFuncs}
  Sets the source functions (can be used to override default implementations)
  of an unattached source.

  Since 2.12")

(setf (documentation 'g-source-attach 'function)
 "@argument[source]{a GSource}
  @argument[context]{a GMainContext (if NULL, the default context will be used)}
  @return{the ID (greater than 0) for the source within the GMainContext.}
  Adds a GSource to a context so that it will be executed within that context.
  Remove it by calling @fun{g-source-destroy}.")

(setf (documentation 'g-source-destroy 'function)
 "@argument[source]{a GSource}
  Removes a source from its GMainContext, if any, and mark it as destroyed.
  The source cannot be subsequently added to another context.")

(setf (documentation 'g-source-is-destroyed 'function)
 "@argument[source]{a GSource}
  @return{TRUE if the source has been destroyed}
  This is important when you operate upon your objects from within idle
  handlers, but may have freed the object before the dispatch of your idle
  handler.
  @begin{pre}
  static gboolean
  idle_callback (gpointer data)
  {
    SomeWidget *self = data;

    GDK_THREADS_ENTER ();
    /* do stuff with self */
    self->idle_id = 0;
    GDK_THREADS_LEAVE ();

    return G_SOURCE_REMOVE;
  @}

  static void
  some_widget_do_stuff_later (SomeWidget *self)
  {
    self->idle_id = g_idle_add (idle_callback, self);
  @}

  static void
  some_widget_finalize (GObject *object)
  {
    SomeWidget *self = SOME_WIDGET (object);

    if (self->idle_id)
      g_source_remove (self->idle_id);

    G_OBJECT_CLASS (parent_class)->finalize (object);
  @}
  @end{pre}
  This will fail in a multi-threaded application if the widget is destroyed
  before the idle handler fires due to the use after free in the callback. A
  solution, to this particular problem, is to check to if the source has
  already been destroy within the callback.
  @begin{pre}
  static gboolean
  idle_callback (gpointer data)
  {
    SomeWidget *self = data;

    GDK_THREADS_ENTER ();
    if (!g_source_is_destroyed (g_main_current_source ()))
      {
        /* do stuff with self */
      @}
    GDK_THREADS_LEAVE ();

    return FALSE;
  @}
  @end{pre}
  Since 2.12")

(setf (documentation 'g-source-set-priority 'function)
 "@argument[source]{a GSource}
  @argument[priority]{the new priority.}
  Sets the priority of a source. While the main loop is being run, a source
  will be dispatched if it is ready to be dispatched and no sources at a
  higher (numerically smaller) priority are ready to be dispatched.")

(setf (documentation 'g-source-get-priority 'function)
 "@argument[source]{a GSource}
  @return{the priority of the source}
  Gets the priority of a source.")

(setf (documentation 'g-source-set-can-recurse 'function)
 "@argument[source]{a GSource}
  @argument[can_recurse]{whether recursion is allowed for this source}
  Sets whether a source can be called recursively. If can_recurse is TRUE,
  then while the source is being dispatched then this source will be processed
  normally. Otherwise, all processing of this source is blocked until the
  dispatch function returns.")

(setf (documentation 'g-source-get-can-recurse 'function)
 "@argument[source]{a GSource}
  @return{whether recursion is allowed.}
  Checks whether a source is allowed to be called recursively. See
  g_source_set_can_recurse().")

(setf (documentation 'g-source-get-id 'function)
 "@argument[source]{a GSource}
  @return{the ID (greater than 0) for the source}
  Returns the numeric ID for a particular source. The ID of a source is a
  positive integer which is unique within a particular main loop context. The
  reverse mapping from ID to source is done by
  g_main_context_find_source_by_id().")

(setf (documentation 'g-source-get-name 'function)
 "@argument[source]{a GSource}
  @return{the name of the source}
  Gets a name for the source, used in debugging and profiling. The name may be
  NULL if it has never been set with g_source_set_name().

  Since 2.26")

(setf (documentation 'g-source-set-name 'function)
 "@argument[source]{a GSource}
  @argument[name]{debug name for the source}
  Sets a name for the source, used in debugging and profiling. The name
  defaults to NULL.

  The source name should describe in a human-readable way what the source
  does. For example, \"X11 event queue\" or \"GTK+ repaint idle handler\" or
  whatever it is.

  It is permitted to call this function multiple times, but is not recommended
  due to the potential performance impact. For example, one could change the
  name in the \"check\" function of a GSourceFuncs to include details like the
  event type in the source name.

  Since 2.26")

(setf (documentation 'g-source-get-context 'function)
 "@argument[source]{a GSource}
  @return{the GMainContext with which the source is associated, or NULL if the
    context has not yet been added to a source}
  Gets the GMainContext with which the source is associated. Calling this
  function on a destroyed source is an error.")

(setf (documentation 'g-source-set-callback 'function)
 "@argument[source]{the source}
  @argument[func]{a callback function}
  @argument[data]{the data to pass to callback function}
  @argument[notify]{a function to call when data is no longer in use, or NULL}
  Sets the callback function for a source. The callback for a source is called
  from the source's dispatch function.

  The exact type of func depends on the type of source; ie. you should not
  count on func being called with data as its first parameter.

  Typically, you won't use this function. Instead use functions specific to
  the type of source you are using.")

(setf (documentation 'g-source-add-poll 'function)
 "@argument[source]{a GSource}
  @argument[fd]{a GPollFD structure holding information about a file descriptor to
    watch.}
  Adds a file descriptor to the set of file descriptors polled for this
  source. This is usually combined with g_source_new() to add an event source.
  The event source's check function will typically test the revents field in
  the GPollFD struct and return TRUE if events need to be processed.")

(setf (documentation 'g-source-remove-poll 'function)
 "@argument[source]{a GSource}
  @argument[fd]{a GPollFD structure previously passed to g_source_add_poll().}
  Removes a file descriptor from the set of file descriptors polled for this
  source.")

(setf (documentation 'g-source-get-current-time 'function)
 "@argument[source]{a GSource}
  @argument[timeval]{GTimeVal structure in which to store current time.}
  Warning

  g_source_get_current_time has been deprecated since version 2.28 and should
  not be used in newly-written code. use g_source_get_time() instead
  
  This function ignores source and is otherwise the same as
  g_get_current_time().")

(setf (documentation 'g-source-remove 'function)
 "@argument[tag]{the ID of the source to remove.}
  @return{TRUE if the source was found and removed.}
  Removes the source with the given id from the default main context. The id
  of a GSource is given by g_source_get_id(), or will be returned by the
  functions g_source_attach(), g_idle_add(), g_idle_add_full(),
  g_timeout_add(), g_timeout_add_full(), g_child_watch_add(),
  g_child_watch_add_full(), g_io_add_watch(), and g_io_add_watch_full().

  See also g_source_destroy(). You must use g_source_destroy() for sources
  added to a non-default main context.")

(setf (documentation 'g-source-remove-by-funcs-user-data 'function)
 "@argument[funcs]{The source_funcs passed to g_source_new()}
  @argument[user_data]{the user data for the callback}
  @return{TRUE if a source was found and removed.}
  Removes a source from the default main loop context given the source
  functions and user data. If multiple sources exist with the same source
  functions and user data, only one will be destroyed.")

(setf (documentation 'g-source-remove-by-user-data 'function)
 "@argument[user_data]{the user_data for the callback.}
  @return{TRUE if a source was found and removed.}
  Removes a source from the default main loop context given the user data for
  the callback. If multiple sources exist with the same user data, only one
  will be destroyed.")

;;; ----------------------------------------------------------------------------
;;;
;;; glib.quark.lisp
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation 'g-quark 'type)
 "A @code{GQuark} is a non-zero integer which uniquely identifies a particular
  string. A @code{GQuark} value of zero is associated to NULL.")

(setf (documentation 'g-quark-from-string 'function)
 "@argument[string]{a string}
  @return{the GQuark identifying the string, or 0 if string is NULL.}
  Gets the GQuark identifying the given string. If the string does not
  currently have an associated GQuark, a new GQuark is created, using a copy
  of the string.")

(setf (documentation 'g-quark-to-string 'function)
 "@argument[quark]{a @code{GQuark}.}
  @return{the string associated with the @code{GQuark}.}
  Gets the string associated with the given @code{GQuark}.")


;;; ----------------------------------------------------------------------------
;;;
;;; glib.error.lisp
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation 'g-error 'type)
 "The GError structure contains information about an error that has occurred.
  @begin{pre}
  struct GError
 
  struct GError {
    GQuark       domain;
    gint         code;
    gchar       *message;
  @};

  GQuark domain;
      error domain, e.g. G_FILE_ERROR
  
  gint code;
      error code, e.g. G_FILE_ERROR_NOENT
  
  gchar *message;
      human-readable informative error message
  @end{pre}")

#|
(defmacro with-g-error ((err) &body body)
  `(with-foreign-object (,err :pointer)
     (setf (mem-ref ,err :pointer) (null-pointer))
     (unwind-protect
          (progn ,@body)
       (maybe-raise-g-error-condition (mem-ref ,err :pointer))
       (g-clear-error ,err))))

(defmacro with-catching-to-g-error ((err) &body body)
  `(handler-case
       (progn ,@body)
     (g-error-condition (e)
       (g-set-error-literal ,err
                            (g-error-condition-domain e)
                            (g-error-condition-code e)
                            (g-error-condition-message e)))))
|#

(setf (documentation 'g-error-new-literal 'function)
 "@argument[domain]{error domain}
  @argument[code]{error code}
  @argument[message]{error message}
  @return{a new GError}
  Creates a new GError; unlike g_error_new(), message is not a printf()-style
  format string. Use this function if message contains text you don't have
  control over, that could include printf() escape sequences.")

(setf (documentation 'g-error-free 'function)
 "@argument[error]{a GError}
  Frees a GError and associated resources.")

(setf (documentation 'g-error-copy 'function)
 "@argument[error]{a GError}
  @return{a new GError}
  Makes a copy of error.")

(setf (documentation 'g-error-matches 'function)
 "@argument[error]{a GError or NULL}
  @argument[domain]{an error domain}
  @argument[code]{an error code}
  @return{whether error has domain and code}
  Returns TRUE if error matches domain and code, FALSE otherwise. In
  particular, when error is NULL, FALSE will be returned.")

(setf (documentation 'g-set-error-literal 'function)
 "@argument[err]{a return location for a GError, or NULL}
  @argument[domain]{error domain}
  @argument[code]{error code}
  @argument[message]{error message}
  Does nothing if err is NULL; if err is non-NULL, then *err must be NULL. A
  new GError is created and assigned to *err. Unlike g_set_error(), message is
  not a printf()-style format string. Use this function if message contains
  text you don't have control over, that could include printf() escape
  sequences.
 
  Since 2.18")

(setf (documentation 'g-propagate-error 'function)
 "@argument[dest]{error return location}
  @argument[src]{error to move into the return location}
  If dest is NULL, free src; otherwise, moves src into *dest. The error
  variable dest points to must be NULL.")

(setf (documentation 'g-clear-error 'function)
 "@argument[err]{a GError return location}
  If err is NULL, does nothing. If err is non-NULL, calls g_error_free() on
  *err and sets *err to NULL.")

;;; ----------------------------------------------------------------------------
;;;
;;; glib.utils.lisp
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation 'g-get-application-name 'function)
 "@version{2012-12-12}
  @return{human-readable application name. May return @code{nil}.}
  @short{Gets a human-readable name for the application, as set by
    @fun{g-set-application-name}.}

  This name should be localized if possible, and is intended for display to the
  user. Contrast with @fun{g-get-prgname}, which gets a non-localized name. If
  @fun{g-set-application-name} has not been called, returns the result of
  @fun{g-get-prgname} (which may be @code{nil} if @fun{g-set-prgname} has also
  not been called).

  Since 2.2
  @see-function{g-set-application-name}
  @see-function{g-get-prgname}
  @see-function{g-set-prgname}")

(setf (documentation 'g-set-application-name 'function)
 "@version{2012-12-12}
  @argument[application-name]{localized name of the application}
  @short{Sets a human-readable name for the application.}

  This name should be localized if possible, and is intended for display to the
  user. Contrast with @fun{g-set-prgname}, which sets a non-localized name.
  @fun{g-set-prgname} will be called automatically by @code{gtk-init}, but
  @sym{g-set-application-name} will not.

  Note that for thread safety reasons, this function can only be called once.

  The application name will be used in contexts such as error messages, or
  when displaying an application's name in the task list.

  Since 2.2
  @see-function{g-set-prgname}")

(setf (documentation 'g-get-prgname 'function)
 "@version{2012-12-12}
  @return{the name of the program. The returned string belongs to GLib and must
    not be modified or freed.}
  Gets the name of the program. This name should not be localized, contrast
  with @fun{g-get-application-name}. (If you are using GDK or GTK+ the program
  name is set in @code{gdk-init}, which is called by @code{gtk-init}. The
  program name is found by taking the last component of @code{argv[0]}.)
  @see-function{g-get-application-name}")

(setf (documentation 'g-set-prgname 'function)
 "@version{2012-12-12}
  @argument[prgname]{the name of the program.}
  Sets the name of the program. This name should not be localized, contrast
  with @fun{g-set-application-name}. Note that for thread-safety reasons this
  function can only be called once.
  @see-function{g-set-application-name}")

(setf (documentation 'g-getenv 'function)
 "@version{2012-12-12}
  @argument[variable]{the environment variable to get, in the GLib file name
    encoding}
  @return{the value of the environment variable, or @code{nil} if the
    environment variable is not found. The returned string may be overwritten by
    the next call to @sym{g-getenv}, @fun{g-setenv} or @code{g_unsetenv()}.}
  @short{Returns the value of an environment variable.}

  The name and value are in the GLib file name encoding. On UNIX, this means
  the actual bytes which might or might not be in some consistent character
  set and encoding. On Windows, it is in UTF-8. On Windows, in case the
  environment variable's value contains references to other environment
  variables, they are expanded.
  @begin[Example]{dictionary}
    @begin{pre}
 (g-getenv \"HOME\") => \"/home/dieter\"
    @end{pre}
  @end{dictionary}
  @see-function{g-setenv}")

(setf (documentation 'g-setenv 'function)
 "@version{2012-12-12}
  @argument[variable]{the environment variable to set, must not contain '='.}
  @argument[value]{the value for to set the variable to.}
  @argument[overwrite]{whether to change the variable if it already exists.}
  @return{FALSE if the environment variable couldn't be set.}
  @short{Sets an environment variable.}

  Both the variable's name and value should be in the GLib file name encoding.
  On UNIX, this means that they can be arbitrary byte strings. On Windows, they
  should be in UTF-8.

  Note that on some systems, when variables are overwritten, the memory used
  for the previous variables and its value isn't reclaimed.

  @begin[Warning]{dictionary}
    Environment variable handling in UNIX is not thread-safe, and your program
    may crash if one thread calls @sym{g-setenv} while another thread is calling
    @code{getenv()}. (And note that many functions, such as @code{gettext()},
    call @code{getenv()} internally.) This function is only safe to use at the
    very start of your program, before creating any other threads (or creating
    objects that create worker threads of their own).

    If you need to set up the environment for a child process, you can use
    @code{g_get_environ()} to get an environment array, modify that with
    @code{g_environ_setenv()} and @code{g_environ_unsetenv()}, and then pass
    that array directly to @code{execvpe()}, @code{g_spawn_async()}, or the
    like.
  @end{dictionary}
  Since 2.4")

(setf (documentation 'g-listenv 'function)
 "@version{2012-12-12}
  @return{a list of strings}
  @short{Gets the names of all variables set in the environment.}

  Programs that want to be portable to Windows should typically use this
  function and @fun{g-getenv} instead of using the environ array from the C
  library directly. On Windows, the strings in the environ array are in system
  codepage encoding, while in most of the typical use cases for environment
  variables in GLib-using programs you want the UTF-8 encoding that this
  function and @fun{g-getenv} provide.

  Since 2.8
  @see-function{g-getenv}")

(setf (documentation 'g-get-user-name 'function)
 "@version{2012-12-12}
  @return{the user name of the current user.}
  Gets the user name of the current user. The encoding of the returned string
  is system-defined. On UNIX, it might be the preferred file name encoding, or
  something else, and there is no guarantee that it is even consistent on a
  machine. On Windows, it is always UTF-8.
  @begin[Example]{dictionary}
    @begin{pre}
 (g-get-user-name) => \"dieter\"
    @end{pre}
  @end{dictionary}")

(setf (documentation 'g-get-real-name 'function)
 "@version{2012-12-12}
  @return{the user's real name.}
  Gets the real name of the user. This usually comes from the user's entry in
  the passwd file. The encoding of the returned string is system-defined. (On
  Windows, it is, however, always UTF-8.) If the real user name cannot be
  determined, the string \"Unknown\" is returned.
  @begin[Example]{dictionary}
    @begin{pre}
 (g-get-real-name) => \"Dieter Kaiser\"
    @end{pre}
  @end{dictionary}")

(setf (documentation 'g-get-user-cache-dir 'function)
 "@return{a string owned by GLib that must not be modified or freed.}
  @short{Returns a base directory in which to store non-essential, cached data
    specific to particular user.}

  On UNIX platforms this is determined using the mechanisms described in the
  XDG Base Directory Specification. In this case the directory retrieved will
  be XDG_CACHE_HOME.

  On Windows is the directory that serves as a common repository for temporary
  Internet files. A typical path is C:\Documents and Settings\username\Local
  Settings\Temporary Internet Files. See documentation for
  CSIDL_INTERNET_CACHE.

  Since 2.6")

(setf (documentation 'g-get-user-data-dir 'function)
 "@return{a string owned by GLib that must not be modified or freed.}
  @short{Returns a base directory in which to access application data such as
    icons that is customized for a particular user.}

  On UNIX platforms this is determined using the mechanisms described in the
  XDG Base Directory Specification. In this case the directory retrieved will
  be XDG_DATA_HOME.

  On Windows this is the folder to use for local (as opposed to roaming)
  application data. See documentation for CSIDL_LOCAL_APPDATA. Note that on
  Windows it thus is the same as what g_get_user_config_dir() returns.

  Since 2.6")

(setf (documentation 'g-get-user-config-dir 'function)
 "@return{a string owned by GLib that must not be modified or freed.}
  @short{Returns a base directory in which to store user-specific application
    configuration information such as user preferences and settings.}

  On UNIX platforms this is determined using the mechanisms described in the
  XDG Base Directory Specification. In this case the directory retrieved will
  be XDG_CONFIG_HOME.

  On Windows this is the folder to use for local (as opposed to roaming)
  application data. See documentation for CSIDL_LOCAL_APPDATA. Note that on
  Windows it thus is the same as what g_get_user_data_dir() returns.

  Since 2.6")

(setf (documentation 'g-user-directory 'type)
 "These are logical ids for special directories which are defined depending on
  the platform used. You should use g_get_user_special_dir() to retrieve the
  full path associated to the logical id.

  The GUserDirectory enumeration can be extended at later date. Not every
  platform has a directory for every logical id in this enumeration.

  Since 2.14
  @begin{pre}
  typedef enum {
    G_USER_DIRECTORY_DESKTOP,
    G_USER_DIRECTORY_DOCUMENTS,
    G_USER_DIRECTORY_DOWNLOAD,
    G_USER_DIRECTORY_MUSIC,
    G_USER_DIRECTORY_PICTURES,
    G_USER_DIRECTORY_PUBLIC_SHARE,
    G_USER_DIRECTORY_TEMPLATES,
    G_USER_DIRECTORY_VIDEOS,

    G_USER_N_DIRECTORIES
  @} GUserDirectory;

  G_USER_DIRECTORY_DESKTOP
      the user's Desktop directory

  G_USER_DIRECTORY_DOCUMENTS
      the user's Documents directory

  G_USER_DIRECTORY_DOWNLOAD
      the user's Downloads directory

  G_USER_DIRECTORY_MUSIC
      the user's Music directory

  G_USER_DIRECTORY_PICTURES
      the user's Pictures directory

  G_USER_DIRECTORY_PUBLIC_SHARE
      the user's shared directory

  G_USER_DIRECTORY_TEMPLATES
      the user's Templates directory

  G_USER_DIRECTORY_VIDEOS
      the user's Movies directory

  G_USER_N_DIRECTORIES
      the number of enum values
  @end{pre}")

(setf (documentation 'g-get-user-special-dir 'function)
 "@argument[directory]{the logical id of special directory}
  @return{the path to the specified special directory, or NULL if the logical id
    was not found. The returned string is owned by GLib and should not be
    modified or freed.}
  @short{Returns the full path of a special directory using its logical id.}

  On Unix this is done using the XDG special user directories. For
  compatibility with existing practise, G_USER_DIRECTORY_DESKTOP falls back to
  $HOME/Desktop when XDG special user directories have not been set up.

  Depending on the platform, the user might be able to change the path of the
  special directory without requiring the session to restart; GLib will not
  reflect any change once the special directories are loaded.

  Since 2.14")

(setf (documentation 'g-get-system-data-dirs 'function)
 "@return{a NULL-terminated array of strings owned by GLib that must not be
    modified or freed}
  @short{Returns an ordered list of base directories in which to access
    system-wide application data.}

  On UNIX platforms this is determined using the mechanisms described in the
  XDG Base Directory Specification In this case the list of directories
  retrieved will be XDG_DATA_DIRS.

  On Windows the first elements in the list are the Application Data and
  Documents folders for All Users. (These can be determined only on Windows
  2000 or later and are not present in the list on other Windows versions.)
  See documentation for CSIDL_COMMON_APPDATA and CSIDL_COMMON_DOCUMENTS.

  Then follows the \"share\" subfolder in the installation folder for the
  package containing the DLL that calls this function, if it can be
  determined.

  Finally the list contains the \"share\" subfolder in the installation folder
  for GLib, and in the installation folder for the package the application's
  .exe file belongs to.

  The installation folders above are determined by looking up the folder where
  the module (DLL or EXE) in question is located. If the folder's name is
  \"bin\", its parent is used, otherwise the folder itself.

  Note that on Windows the returned list can vary depending on where this
  function is called.

  Since 2.6")

(setf (documentation 'g-get-system-config-dirs 'function)
 "@return{a NULL-terminated array of strings owned by GLib that must not be
    modified or freed}
  @short{Returns an ordered list of base directories in which to access
    system-wide configuration information.}

  On UNIX platforms this is determined using the mechanisms described in the
  XDG Base Directory Specification. In this case the list of directories
  retrieved will be XDG_CONFIG_DIRS.

  On Windows is the directory that contains application data for all users. A
  typical path is C:\Documents and Settings\All Users\Application Data. This
  folder is used for application data that is not user specific. For example,
  an application can store a spell-check dictionary, a database of clip art,
  or a log file in the CSIDL_COMMON_APPDATA folder. This information will not
  roam and is available to anyone using the computer.
 
  Since 2.6")

(setf (documentation 'g-get-host-name 'function)
 "@return{the host name of the machine.}
  @short{Return a name for the machine.}

  The returned name is not necessarily a fully-qualified domain name, or even
  present in DNS or some other name service at all. It need not even be unique
  on your local network or site, but usually it is. Callers should not rely on
  the return value having any specific properties like uniqueness for security
  purposes. Even if the name of the machine is changed while an application is
  running, the return value from this function does not change. The returned
  string is owned by GLib and should not be modified or freed. If no name can
  be determined, a default fixed string \"localhost\" is returned.")

(setf (documentation 'g-get-home-dir 'function)
 "@return{the current user's home directory}
  @short{Gets the current user's home directory as defined in the password
    database.}

  Note that in contrast to traditional UNIX tools, this function prefers
  passwd entries over the HOME environment variable.
 
  One of the reasons for this decision is that applications in many cases need
  special handling to deal with the case where HOME is

  Not owned by the user
  Not writeable
  Not even readable
 
  Since applications are in general not written to deal with these situations
  it was considered better to make g_get_home_dir() not pay attention to HOME
  and to return the real home directory for the user. If applications want to
  pay attention to HOME, they can do:
  @begin{pre}
  const char *homedir = g_getenv (\"HOME\");
  if (!homedir)
     homedir = g_get_home_dir ();
  @end{pre}")

(setf (documentation 'g-get-tmp-dir 'function)
 "@return{the directory to use for temporary files.}
  Gets the directory to use for temporary files. This is found from inspecting
  the environment variables TMPDIR, TMP, and TEMP in that order. If none of
  those are defined \"/tmp\" is returned on UNIX and \"C:\\\" on Windows. The
  encoding of the returned string is system-defined. On Windows, it is always
  UTF-8. The return value is never NULL or the empty string.")

(setf (documentation 'g-get-current-dir 'function)
 "@return{the current directory}
  @short{Gets the current directory.}

  The returned string should be freed when no longer needed. The encoding of
  the returned string is system defined. On Windows, it is always UTF-8.")

(setf (documentation 'g-path-is-absolute 'function)
 "@argument[file_name]{a file name}
  @return{TRUE if file_name is absolute}
  Returns TRUE if the given file_name is an absolute file name. Note that this
  is a somewhat vague concept on Windows.
 
  On POSIX systems, an absolute file name is well-defined. It always starts
  from the single root directory. For example \"/usr/local\".

  On Windows, the concepts of current drive and drive-specific current
  directory introduce vagueness. This function interprets as an absolute file
  name one that either begins with a directory separator such as \"\Users\tml\"
  or begins with the root on a drive, for example \"C:\Windows\". The first case
  also includes UNC paths such as \"\\myserver\docs\foo\". In all cases, either
  slashes or backslashes are accepted.

  Note that a file name relative to the current drive root does not truly
  specify a file uniquely over time and across processes, as the current drive
  is a per-process value and can be changed.
 
  File names relative the current directory on some specific drive, such as
  \"D:foo/bar\", are not interpreted as absolute by this function, but they
  obviously are not relative to the normal current directory as returned by
  getcwd() or g_get_current_dir() either. Such paths should be avoided, or
  need to be handled using Windows-specific code.")

(setf (documentation 'g-build-filename 'function)
 "@argument[first_element]{the first element in the path}
  @argument[...]{remaining elements in path, terminated by NULL}
  @return{a newly-allocated string that must be freed with g_free().}
  Creates a filename from a series of elements using the correct separator for
  filenames.

  On Unix, this function behaves identically to g_build_path
  (G_DIR_SEPARATOR_S, first_element, ....).

  On Windows, it takes into account that either the backslash (\ or slash (/)
  can be used as separator in filenames, but otherwise behaves as on Unix.
  When file pathname separators need to be inserted, the one that last
  previously occurred in the parameters (reading from left to right) is used.

  No attempt is made to force the resulting filename to be an absolute path.
  If the first element is a relative path, the result will be a relative path.")

(setf (documentation 'g-build-filenamev 'function)
 "@version{2012-12-23}
  @argument[args]{array of strings containing the path elements}
  @return{a newly-allocated string that must be freed with @fun{g-free}.}
  @begin{short}
    Behaves exactly like @fun{g-build-filename}, but takes the path elements as
    a string array, instead of varargs.
  @end{short}
  This function is mainly meant for language bindings.
 
  Since 2.8")

;;; --- End of file atdoc-glib.lisp --------------------------------------------
