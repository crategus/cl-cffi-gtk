;;; ----------------------------------------------------------------------------
;;; glib.main-loop.lisp
;;;
;;; The documentation of this file is taken from the GLib 2.68 Reference
;;; Manual and modified to document the Lisp binding to the GLib library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; The Main Event Loop
;;;
;;;     Manages all available sources of events
;;;
;;; Types and Values
;;;
;;;     GMainLoop
;;;
;;;     G_PRIORITY_HIGH
;;;     G_PRIORITY_DEFAULT
;;;     G_PRIORITY_HIGH_IDLE
;;;     G_PRIORITY_DEFAULT_IDLE
;;;     G_PRIORITY_LOW
;;;     G_SOURCE_CONTINUE
;;;     G_SOURCE_REMOVE
;;;
;;;     GMainContext
;;;     GMainContextPusher
;;;     GPid
;;;     G_PID_FORMAT
;;;     GPollFD
;;;     G_POLLFD_FORMAT
;;;
;;;     GSource
;;;     GSourceFuncs
;;;     GSourceCallbackFuncs
;;;
;;; Functions
;;;
;;;     g_main_loop_new
;;;     g_main_loop_ref
;;;     g_main_loop_unref
;;;     g_main_loop_run
;;;     g_main_loop_quit
;;;     g_main_loop_is_running
;;;     g_main_loop_get_context
;;;
;;;     g_main_new                                         deprecated
;;;     g_main_destroy                                     deprecated
;;;     g_main_run                                         deprecated
;;;     g_main_quit                                        deprecated
;;;     g_main_is_running                                  deprecated
;;;
;;;     g_main_context_new
;;;     g_main_context_ref
;;;     g_main_context_unref
;;;     g_main_context_default
;;;     g_main_context_iteration
;;;     g_main_iteration                                   deprecated
;;;     g_main_context_pending
;;;     g_main_pending                                     deprecated
;;;     g_main_context_find_source_by_id
;;;     g_main_context_find_source_by_user_data
;;;     g_main_context_find_source_by_funcs_user_data
;;;     g_main_context_wakeup
;;;     g_main_context_acquire
;;;     g_main_context_release
;;;     g_main_context_is_owner
;;;     g_main_context_wait                                deprecated
;;;     g_main_context_prepare
;;;     g_main_context_query
;;;     g_main_context_check
;;;     g_main_context_dispatch
;;;     g_main_context_set_poll_func
;;;     g_main_context_get_poll_func
;;;
;;;     GPollFunc
;;;
;;;     g_main_context_add_poll
;;;     g_main_context_remove_poll
;;;     g_main_depth
;;;     g_main_current_source
;;;     g_main_set_poll_func                               deprecated
;;;     g_main_context_invoke
;;;     g_main_context_invoke_full
;;;
;;;     g_main_context_pusher_new
;;;     g_main_context_pusher_free
;;;
;;;     g_main_context_get_thread_default
;;;     g_main_context_ref_thread_default
;;;     g_main_context_push_thread_default
;;;     g_main_context_pop_thread_default
;;;
;;;     g_timeout_source_new
;;;     g_timeout_source_new_seconds
;;;     g_timeout_add
;;;     g_timeout_add_full
;;;     g_timeout_add_seconds
;;;     g_timeout_add_seconds_full
;;;
;;;     g_idle_source_new
;;;     g_idle_add
;;;     g_idle_add_full
;;;     g_idle_remove_by_data
;;;
;;;     GChildWatchFunc
;;;
;;;     g_child_watch_source_new
;;;     g_child_watch_add
;;;     g_child_watch_add_full
;;;
;;;     g_poll
;;;
;;;     GSourceDummyMarshal
;;;     GSourceDisposeFunc
;;;
;;;     g_source_new
;;;     g_source_ref
;;;     g_source_unref
;;;     g_source_set_funcs
;;;     g_source_set_dispose_function
;;;     g_source_attach
;;;     g_source_destroy
;;;     g_source_is_destroyed
;;;     g_source_set_priority
;;;     g_source_get_priority
;;;     g_source_set_can_recurse
;;;     g_source_get_can_recurse
;;;     g_source_get_id
;;;     g_source_get_name
;;;     g_source_set_name
;;;     g_source_set_name_by_id
;;;     g_source_get_context
;;;     g_source_set_callback
;;;
;;;     GSourceFunc
;;;     G_SOURCE_FUNC
;;;     g_source_set_callback_indirect
;;;
;;;     g_source_set_ready_time
;;;     g_source_get_ready_time
;;;     g_source_add_unix_fd
;;;     g_source_remove_unix_fd
;;;     g_source_modify_unix_fd
;;;     g_source_query_unix_fd
;;;
;;;     g_source_add_poll
;;;     g_source_remove_poll
;;;     g_source_add_child_source
;;;     g_source_remove_child_source
;;;     g_source_get_time
;;;     g_source_get_current_time                          deprecated
;;;     g_source_remove
;;;     g_source_remove_by_funcs_user_data
;;;     g_source_remove_by_user_data
;;;
;;;     GClearHandleFunc
;;;     g_clear_handle_id
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_HIGH
;;; ----------------------------------------------------------------------------

(defconstant +g-priority-high+ -100
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @variable-value{-100}
  @short{Use this for high priority event sources.}
  It is not used within GLib or GTK+.
  @see-variable{+g-priority-default+}
  @see-variable{+g-priority-low+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-priority-high+ atdoc:*variable-name-alias*) "Constant")

(export '+g-priority-high+)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_DEFAULT
;;; ----------------------------------------------------------------------------

(defconstant +g-priority-default+ 0
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @variable-value{0}
  @short{Use this for default priority event sources.}
  In GLib this priority is used when adding timeout functions with the function
  @fun{g-timeout-add}. In GDK this priority is used for events from the X
  server.
  @see-variable{+g-priority-high+}
  @see-variable{+g-priority-low+}
  @see-function{g-timeout-add}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-priority-default+ atdoc:*variable-name-alias*) "Constant")

(export '+g-priority-default+)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_HIGH_IDLE
;;; ----------------------------------------------------------------------------

(defconstant +g-priority-high-idle+ 100
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @variable-value{100}
  @short{Use this for high priority idle functions.}
  GTK+ uses @sym{+g-priority-high-idle+} + 10 for resizing operations, and
  @sym{+g-priority-high-idle+} + 20 for redrawing operations. This is done to
  ensure that any pending resizes are processed before any pending redraws, so
  that widgets are not redrawn twice unnecessarily.
  @see-variable{+g-priority-default-idle+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-priority-high-idle+ atdoc:*variable-name-alias*) "Constant")

(export '+g-priority-high-idle+)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_DEFAULT_IDLE
;;; ----------------------------------------------------------------------------

(defconstant +g-priority-default-idle+ 200
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @variable-value{200}
  @short{Use this for default priority idle functions.}
  In GLib this priority is used when adding idle functions with the function
  @fun{g-idle-add}.
  @see-variable{+g-priority-high-idle+}
  @see-function{g-idle-add}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-priority-default-idle+ atdoc:*variable-name-alias*)
      "Constant")

(export '+g-priority-default-idle+)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_LOW
;;; ----------------------------------------------------------------------------

(defconstant +g-priority-low+ 300
 #+cl-cffi-gtk-documentation
 "@version{2021-4-2}
  @variable-value{300}
  @short{Use this for very low priority background tasks.}
  It is not used within GLib or GTK+.
  @see-variable{+g-priority-default+}
  @see-variable{+g-priority-high+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-priority-low+ atdoc:*variable-name-alias*) "Constant")

(export '+g-priority-low+)

;;; ----------------------------------------------------------------------------
;;; G_SOURCE_CONTINUE
;;; ----------------------------------------------------------------------------

(defconstant +g-source-continue+ t
 #+cl-cffi-gtk-documentation
 "@version{*2021-4-9}
  @variable-value{t}
  @begin{short}
    Use this constant as the return value of a @symbol{g-source-func} callback
    function to leave the @type{g-source} instance in the main loop.
  @end{short}
  @see-type{g-source}
  @see-symbol{g-source-func}
  @see-variable{+g-source-remove+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-source-continue+ atdoc:*variable-name-alias*) "Constant")

(export '+g-source-continue+)

;;; ----------------------------------------------------------------------------
;;; G_SOURCE_REMOVE
;;; ----------------------------------------------------------------------------

(defconstant +g-source-remove+ nil
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @variable-value{nil}
  @begin{short}
    Use this constant as the return value of a @symbol{g-source-func} callback
    function to remove the @type{g-source} instance from the main loop.
  @end{short}
  @see-type{g-source}
  @see-symbol{g-source-func}
  @see-variable{+g-source-continue+}")

#+cl-cffi-gtk-documentation
(setf (gethash '+g-source-remove+ atdoc:*variable-name-alias*) "Constant")

(export '+g-source-remove+)

;;; ----------------------------------------------------------------------------
;;; GMainLoop
;;; ----------------------------------------------------------------------------

(defcstruct g-main-loop)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-main-loop atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'g-main-loop 'type)
 "@version{2021-4-9}
  @begin{short}
    The main event loop manages all the available sources of events for GLib
    and GTK+ applications. These events can come from any number of different
    types of sources such as file descriptors (plain files, pipes or sockets)
    and timeouts.
  @end{short}
  New types of event sources can also be added using the function
  @fun{g-source-attach}.

  To allow multiple independent sets of sources to be handled in different
  threads, each source is associated with a @type{g-main-context} instance. A
  @type{g-main-context} instance can only be running in a single thread, but
  sources can be added to it and removed from it from other threads. All
  functions which operate on a @type{g-main-context} instance or a built-in
  @type{g-source} instance are thread-safe.

  Each event source is assigned a priority. The default priority,
  @var{+g-priority-default+}, is 0. Values less than 0 denote higher priorities.
  Values greater than 0 denote lower priorities. Events from high priority
  sources are always processed before events from lower priority sources.

  Idle functions can also be added, and assigned a priority. These will be run
  whenever no events with a higher priority are ready to be processed.

  The @sym{g-main-loop} data type represents a main event loop. A
  @sym{g-main-loop} instance is created with the function @fun{g-main-loop-new}.
  After adding the initial event sources, the function @fun{g-main-loop-run} is
  called. This continuously checks for new events from each of the event sources
  and dispatches them. Finally, the processing of an event from one of the
  sources leads to a call to the funcion @fun{g-main-loop-quit} to exit the
  main loop, and the function @fun{g-main-loop-run} returns.

  It is possible to create new instances of @sym{g-main-loop} instances
  recursively. This is often used in GTK+ applications when showing modal
  dialog boxes. Note that event sources are associated with a particular
  @type{g-main-context} instance, and will be checked and dispatched for all
  main loops associated with that @type{g-main-context} instance.

  GTK+ contains wrappers of some of these functions, e.g. the functions
  @fun{gtk-main}, @fun{gtk-main-quit} and @fun{gtk-events-pending}.
  @see-type{g-main-context}
  @see-type{g-source}")

(export 'g-main-loop)

;;; ----------------------------------------------------------------------------
;;; GMainContext
;;; ----------------------------------------------------------------------------

(defcstruct g-main-context)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-main-context atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'g-main-context 'type)
 "@version{2021-4-2}
  @begin{short}
    The @sym{g-main-context} structure is an opaque data type representing a
    set of sources to be handled in a main loop.
  @end{short}
  @see-type{g-main-loop}")

(export 'g-main-context)

;;; ----------------------------------------------------------------------------
;;; GMainContextPusher
;;;
;;; typedef void GMainContextPusher GLIB_AVAILABLE_TYPE_IN_2_64;
;;;
;;; Opaque type. See g_main_context_pusher_new() for details.
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GPid
;;;
;;; typedef int GPid;
;;;
;;; A type which is used to hold a process identification.
;;;
;;; On UNIX, processes are identified by a process id (an integer), while
;;; Windows uses process handles (which are pointers).
;;;
;;; GPid is used in GLib only for descendant processes spawned with the g_spawn
;;; functions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PID_FORMAT
;;;
;;; #define G_PID_FORMAT "i"
;;;
;;; A format specifier that can be used in printf()-style format strings when
;;; printing a GPid.
;;;
;;; Since 2.50
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GPollFD                                         not exported
;;; ----------------------------------------------------------------------------

(defcstruct g-poll-fd
  (fd :int) ; TODO: #if defined (G_OS_WIN32) && GLIB_SIZEOF_VOID_P == 8
  (events :ushort)
  (revent :ushort))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-poll-fd atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'g-poll-fd 'type)
 "@version{2021-4-2}
  @begin{short}
    Represents a file descriptor, which events to poll for, and which events
    occurred.
  @end{short}
  @begin{pre}
(defcstruct g-poll-fd
  (fd :int)
  (events :ushort)
  (revent :ushort))
  @end{pre}
  @begin[code]{table}
    @entry[fd]{The file descriptor to poll (or a @code{HANDLE} on Win32).}
    @entry[events]{A bitwise combination from @code{GIOCondition},
      specifying which events should be polled for. Typically for reading from
      a file descriptor you would use @code{G_IO_IN | G_IO_HUP | G_IO_ERR}, and
      for writing you would use @code{G_IO_OUT | G_IO_ERR}.}
    @entry[revents]{A bitwise combination of flags from @code{GIOCondition},
      returned from the @code{poll()} function to indicate which events
      occurred.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; G_POLLFD_FORMAT
;;;
;;; #define G_POLLFD_FORMAT "%d"
;;;
;;; A format specifier that can be used in printf()-style format strings when
;;; printing the fd member of a GPollFD.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GSource
;;; ----------------------------------------------------------------------------

(defcstruct g-source
  ;; Private fields of the C structure
  (callback-data :pointer)
  (callback-funcs :pointer)
  (source-funcs :pointer)
  (ref-count :uint)
  (context :pointer)
  (priority :int)
  (flags :uint)
  (source-id :uint)
  (poll-fds :pointer)
  (prev :pointer)
  (next :pointer)
  (name :string)
  (priv :pointer))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-source atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'g-source 'type)
 "@version{2021-4-2}
  @begin{short}
    The @sym{g-source} structure is an opaque data type representing an event
    source.
  @end{short}
  @see-function{g-source-new}")

(export 'g-source)

;;; ----------------------------------------------------------------------------
;;; struct GSourceFuncs                                    not exported
;;; ----------------------------------------------------------------------------

(defcstruct g-source-funcs
  (prepare :pointer)           ; lambda (source timeout)
  (check :pointer)             ; lambda (source)
  (dispatch :pointer)          ; lambda (source callback user-data)
  (finalize :pointer)          ; lambda (source) Can be NULL
  ;; Private data of the C structure
  (closure-callback :pointer)  ; no documentation
  (closure-marshal :pointer))  ; no documentation

#+cl-cffi-gtk-documentation
(setf (gethash 'g-source-funcs atdoc:*type-name-alias*)
      "CStruct"
      (documentation 'g-source-funcs 'type)
 "@version{2021-4-2}
  @begin{short}
    The @sym{g-source-funcs} structure contains a table of functions used to
    handle event sources in a generic manner.
  @end{short}

  For idle sources, the @code{prepare} and @code{check} functions always return
  @em{true} to indicate that the source is always ready to be processed. The
  @code{prepare} function also returns a timeout value of 0 to ensure that the
  @code{poll()} call does not block since that would be time wasted which could
  have been spent running the idle function.

  For timeout sources, the @code{prepare} and @code{check} functions both return
  @em{true} if the timeout interval has expired. The @code{prepare} function
  also returns a timeout value to ensure that the @code{poll()} call does not
  block too long and miss the next timeout.

  For file descriptor sources, the @code{prepare} function typically returns
  @code{nil}, since it must wait until @code{poll()} has been called before it
  knows whether any events need to be processed. It sets the returned timeout
  to -1 to indicate that it does not mind how long the @code{poll()} call
  blocks. In the @code{check} function, it tests the results of the
  @code{poll()} call to see if the required condition has been met, and returns
  @em{true} if so.
  @begin{pre}
(defcstruct g-source-funcs
  (prepare :pointer)
  (check :pointer)
  (dispatch :pointer)
  (finalize :pointer)
  (closure-callback :pointer)
  (closure-marshal :pointer))
  @end{pre}
  @begin[code]{table}
    @begin[prepare (source timeout)]{entry}
      Called before all the file descriptors are polled. If the source can
      determine that it is ready here, without waiting for the results of the
      @code{poll()} call, it should return @em{true}. It can also return a
      @code{timeout} value which should be the maximum timeout (in milliseconds)
      which should be passed to the @code{poll()} call. The actual timeout used
      will be -1 if all sources returned -1, or it will be the minimum of all
      the @code{timeout} values returned which were >= 0.
    @end{entry}
    @begin[check (source)]{entry}
      Called after all the file descriptors are polled. The source should
      return @em{true} if it is ready to be dispatched. Note that some time may
      have passed since the previous @code{prepare} function was called, so the
      source should be checked again here.
    @end{entry}
    @begin[dispatch (source callback user-data]{entry}
      Called to dispatch the event source, after it has returned @em{true} in
      either its @code{prepare} or its @code{check} function. The
      @code{dispatch} function is passed in a callback function and data. The
      callback function may be @code{NULL} if the source was never connected to
      a callback using @fun{g-source-set-callback}. The @code{dispatch} function
      should call the callback function with @arg{user-data} and whatever
      additional parameters are needed for this type of event source.
    @end{entry}
    @begin[finalize (source)]{entry}
      Called when the source is finalized.
    @end{entry}
  @end{table}
  @see-type{g-source}
  @see-function{g-source-set-callback}")

;;; ----------------------------------------------------------------------------
;;; struct GSourceCallbackFuncs                            not exported
;;; ----------------------------------------------------------------------------

(defcstruct g-source-callback-funcs
  (ref :pointer)
  (unref :pointer)
  (get :pointer))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-source-callback-funcs 'type)
 "@version{2021-4-2}
  @begin{short}
    The @sym{g-source-callback-funcs} structure contains functions for managing
    callback objects.
  @end{short}
  @begin{pre}
(defcstruct g-source-callback-funcs
  (ref :pointer)
  (unref :pointer)
  (get :pointer))
  @end{pre}
  @begin[code]{table}
    @entry[ref]{Called when a reference is added to the callback object.}
    @entry[unref]{Called when a reference to the callback object is dropped.}
    @entry[get]{Called to extract the callback function and data from the
        callback object.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; g_main_loop_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_new" g-main-loop-new) (:pointer (:struct g-main-loop))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance, if a
    @code{null}-pointer, the default context will be used}
  @argument[is-running]{set to @em{true} to indicate that the main loop is
    running}
  @return{A new @type{g-main-loop} instance.}
  @short{Creates a new main loop.}
  @begin[Example]{dictionary}
    Create a running main loop with a default context and quit the main loop.
    @begin{pre}
(setq main-loop (g-main-loop-new (null-pointer) t))
=> #.(SB-SYS:INT-SAP #X0808DF88)
(g-main-loop-is-running main-loop) => T
(g-main-loop-quit main-loop)
(g-main-loop-is-running main-loop) => NIL
    @end{pre}
  @end{dictionary}
  @see-type{g-main-loop}
  @see-type{g-main-context}
  @see-function{g-main-loop-run}"
  (context (:pointer (:struct g-main-context)))
  (is-running :boolean))

(export 'g-main-loop-new)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_ref" g-main-loop-ref) (:pointer (:struct g-main-loop))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[loop]{a @type{g-main-loop} instance}
  @return{The argument @arg{loop}.}
  @begin{short}
    Increases the reference count on a main loop by one.
  @end{short}
  @see-type{g-main-loop}
  @see-function{g-main-loop-unref}"
  (loop (:pointer (:struct g-main-loop))))

(export 'g-main-loop-ref)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_unref" g-main-loop-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[loop]{a @type{g-main-loop} instance}
  @begin{short}
    Decreases the reference count on a main loop by one.
  @end{short}
  If the result is zero, free @arg{loop} and free all associated memory.
  @see-type{g-main-loop}
  @see-function{g-main-loop-ref}"
  (loop (:pointer (:struct g-main-loop))))

(export 'g-main-loop-unref)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_run ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_run" g-main-loop-run) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[loop]{a @type{g-main-loop} instance}
  @begin{short}
    Runs a main loop until the function @fun{g-main-loop-quit} is called on the
    main loop.
  @end{short}
  If this is called for the thread of the main loop's context, it will process
  events from the main loop, otherwise it will simply wait.
  @see-type{g-main-loop}
  @see-type{g-main-context}
  @see-function{g-main-loop-quit}"
  (loop (:pointer (:struct g-main-loop))))

(export 'g-main-loop-run)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_quit ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_quit" g-main-loop-quit) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[loop]{a @type{g-main-loop} instance}
  @short{Stops a main loop from running.}
  Any calls to the function @fun{g-main-loop-run} for the main loop will return.

  Note that sources that have already been dispatched when the function
  @sym{g-main-loop-quit} is called will still be executed.
  @see-type{g-main-loop}
  @see-function{g-main-loop-run}"
  (loop (:pointer (:struct g-main-loop))))

(export 'g-main-loop-quit)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_is_running ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_is_running" g-main-loop-is-running) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[loop]{a @type{g-main-loop} instance}
  @return{@em{True} if the main loop is currently being run.}
  @begin{short}
    Checks to see if the main loop is currently being run via the function
    @fun{g-main-loop-run}.
  @end{short}
  @see-type{g-main-loop}
  @see-function{g-main-loop-run}
  @see-function{g-main-loop-quit}"
  (loop (:pointer (:struct g-main-loop))))

(export 'g-main-loop-is-running)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_get_context () -> g-main-loop-context
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_get_context" g-main-loop-context)
    (:pointer (:struct g-main-context))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[loop]{a @type{g-main-loop} instance}
  @return{The @type{g-main-context} instance of @arg{loop}.}
  @short{Returns the context of the main loop.}
  @see-type{g-main-loop}
  @see-type{g-main-context}"
  (loop (:pointer (:struct g-main-loop))))

(export 'g-main-loop-context)

;;; ----------------------------------------------------------------------------
;;; g_main_new()
;;;
;;; #define g_main_new(is_running)
;;;
;;; Warning
;;;
;;; g_main_new has been deprecated since version 2.2 and should not be used in
;;; newly-written code. Use g_main_loop_new() instead
;;;
;;; Creates a new GMainLoop for th default main context.
;;;
;;; is_running :
;;;     set to TRUE to indicate that the loop is running. This is not very
;;;     important since calling g_main_run() will set this to TRUE anyway.
;;;
;;; Returns :
;;;     a new GMainLoop
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_destroy()
;;;
;;; #define g_main_destroy(loop)
;;;
;;; Warning
;;;
;;; g_main_destroy has been deprecated since version 2.2 and should not be used
;;; in newly-written code. Use g_main_loop_unref() instead
;;;
;;; Frees the memory allocated for the GMainLoop.
;;;
;;; loop :
;;;     a GMainLoop
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_run()
;;;
;;; #define g_main_run(loop)
;;;
;;; Warning
;;;
;;; g_main_run has been deprecated since version 2.2 and should not be used in
;;; newly-written code. Use g_main_loop_run() instead
;;;
;;; Runs a main loop until it stops running.
;;;
;;; loop :
;;;     a GMainLoop
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_quit()
;;;
;;; #define g_main_quit(loop)
;;;
;;; Warning
;;;
;;; g_main_quit has been deprecated since version 2.2 and should not be used in
;;; newly-written code. Use g_main_loop_quit() instead
;;;
;;; Stops the GMainLoop. If g_main_run() was called to run the GMainLoop, it
;;; will now return.
;;;
;;; loop :
;;;     a GMainLoop
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_is_running()
;;;
;;; #define g_main_is_running(loop)
;;;
;;; Warning
;;;
;;; g_main_is_running has been deprecated since version 2.2 and should not be
;;; used in newly-written code. Use g_main_loop_is_running() instead
;;;
;;; Checks if the main loop is running.
;;;
;;; loop :
;;;     a GMainLoop
;;;
;;; Returns :
;;;     TRUE if the main loop is running
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_new" g-main-context-new)
    (:pointer (:struct g-main-context))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @return{The new @type{g-main-context} instance.}
  @short{Creates a new context.}
  @see-type{g-main-context}")

(export 'g-main-context-new)

;;; ----------------------------------------------------------------------------
;;; g_main_context_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_ref" g-main-context-ref)
    (:pointer (:struct g-main-context))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance}
  @return{The argument @arg{context}.}
  @begin{short}
    Increases the reference count on a context by one.
  @end{short}
  @see-type{g-main-context}
  @see-function{g-main-context-unref}"
  (context (:pointer (:struct g-main-context))))

(export 'g-main-context-ref)

;;; ----------------------------------------------------------------------------
;;; g_main_context_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_unref" g-main-context-unref) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance}
  @begin{short}
    Decreases the reference count on a context by one.
  @end{short}
  If the result is zero, free @arg{context} and free all associated memory.
  @see-type{g-main-context}
  @see-function{g-main-context-ref}"
  (context (:pointer (:struct g-main-context))))

(export 'g-main-context-unref)

;;; ----------------------------------------------------------------------------
;;; g_main_context_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_default" g-main-context-default)
    (:pointer (:struct g-main-context))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @return{A @type{g-main-context} instance with the global default context.}
  @short{Returns the global default context.}
  This is the context used for main loop functions when a main loop is not
  explicitly specified, and corresponds to the \"main\" main loop.
  @see-type{g-main-context}")

(export 'g-main-context-default)

;;; ----------------------------------------------------------------------------
;;; g_main_context_iteration ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_iteration" g-main-context-iteration) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance, if @code{null}-pointer,
    the default context will be used}
  @argument[may-block]{a boolean whether the call may block}
  @return{@em{True} if events were dispatched.}
  @short{Runs a single iteration for the given main loop.}
  This involves checking to see if any event sources are ready to be processed,
  then if no events sources are ready and @arg{may-block} is @em{true}, waiting
  for a source to become ready, then dispatching the highest priority events
  sources that are ready. Otherwise, if @arg{may-block} is @em{false} sources
  are not waited to become ready, only those highest priority events sources
  will be dispatched (if any), that are ready at this given moment without
  further waiting.

  Note that even when @arg{may-block} is @em{true}, it is still possible for
  the function @sym{g-main-context-iteration} to return @em{false}, since the
  wait may be interrupted for other reasons than an event source becoming ready.
  @see-type{g-main-context}"
  (context (:pointer (:struct g-main-context)))
  (may-block :boolean))

(export 'g-main-context-iteration)

;;; ----------------------------------------------------------------------------
;;; g_main_iteration()
;;;
;;; #define g_main_iteration(may_block)
;;;
;;; Warning
;;;
;;; g_main_iteration has been deprecated since version 2.2 and should not be
;;; used in newly-written code. Use g_main_context_iteration() instead.
;;;
;;; Runs a single iteration for the default GMainContext.
;;;
;;; may_block :
;;;     set to TRUE if it should block (i.e. wait) until an event source becomes
;;;     ready. It will return after an event source has been processed. If set
;;;     to FALSE it will return immediately if no event source is ready to be
;;;     processed.
;;;
;;; Returns :
;;;     TRUE if more events are pending.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_pending ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_pending" g-main-context-pending) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance, if @code{null}-pointer,
    the default context will be used}
  @return{@em{True} if events are pending.}
  @begin{short}
    Checks if any sources have pending events for the given main context.
  @end{short}
  @see-type{g-main-context}"
  (context (:pointer (:struct g-main-context))))

(export 'g-main-context-pending)

;;; ----------------------------------------------------------------------------
;;; g_main_pending
;;;
;;; #define g_main_pending()
;;;
;;; Warning
;;;
;;; g_main_pending has been deprecated since version 2.2 and should not be used
;;; in newly-written code. Use g_main_context_pending() instead.
;;;
;;; Checks if any events are pending for the default GMainContext (i.e. ready
;;; to be processed).
;;;
;;; Returns :
;;;     TRUE if any events are pending.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_find_source_by_id ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_find_source_by_id" g-main-context-find-source-by-id)
    (:pointer (:struct g-source))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance, if @code{null}-pointer,
    the default context will be used}
  @argument[source-id]{an unsigned integer source ID, as returned by the
    function @fun{g-source-id}}
  @return{The @type{g-source} instance if found, otherwise, a
    @code{null}-pointer.}
  @begin{short}
    Finds a source given a pair of context and ID.
  @end{short}
  It is a programmer error to attempt to look up a non-existent source.

  More specifically: source IDs can be reissued after a source has been
  destroyed and therefore it is never valid to use this function with a source
  ID which may have already been removed. An example is when scheduling an idle
  to run in another thread with the function @fun{g-idle-add}: the idle may
  already have run and been removed by the time this function is called on its
  (now invalid) source ID. This source ID may have been reissued, leading to
  the operation being performed against the wrong source.
  @see-type{g-main-context}
  @see-type{g-source}
  @see-function{g-source-id}
  @see-function{g-idle-add}"
  (context (:pointer (:struct g-main-context)))
  (source-id :uint))

(export 'g-main-context-find-source-by-id)

;;; ----------------------------------------------------------------------------
;;; g_main_context_find_source_by_user_data ()             not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_find_source_by_user_data"
          g-main-context-find-source-by-user-data)
    (:pointer (:struct g-source))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[context]{a @type{g-main-context} object}
  @argument[user-data]{the user data for the callback}
  @return{The source, if one was found, otherwise @code{null}-pointer.}
  @begin{short}
    Finds a source with the given user data for the callback.
  @end{short}
  If multiple sources exist with the same user data, the first one found will
  be returned."
  (context (:pointer (:struct g-main-context)))
  (user-data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_main_context_find_source_by_funcs_user_data ()       not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_find_source_by_funcs_user_data"
          g-main-context-find-source-by-funcs-user-data)
    (:pointer (:struct g-source))
 #+cl-cffi-gtk-documentation
 "@version{2013-4-4}
  @argument[context]{a @type{g-main-context} object, if @code{null}-pointer,
    the default context will be used}
  @argument[funcs]{the source funcs passed to @fun{g-source-new}}
  @argument[user-data]{the user data from the callback}
  @return{The source, if one was found, otherwise @code{null}-pointer.}
  @begin{short}
    Finds a source with the given source functions and user data.
  @end{short}
  If multiple sources exist with the same source function and user data, the
  first one found will be returned.
  @see-function{g-source-new}"
  (context (:pointer (:struct g-main-context)))
  (funcs (:pointer (:struct g-source-funcs)))
  (user-data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_main_context_wakeup ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_wakeup" g-main-context-wakeup) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance}
  @begin{short}
    If @arg{context} is currently blocking in a call of the function
    @fun{g-main-context-iteration} waiting for a source to become ready,
    cause it to stop blocking and return.
  @end{short}
  Otherwise, cause the next invocation of the function
  @fun{g-main-context-iteration} to return without blocking.

  This API is useful for low-level control over a main context. For example,
  integrating it with main loop implementations such as a @type{g-main-loop}
  implementation.

  Another related use for this function is when implementing a main loop with a
  termination condition, computed from multiple threads:
  @begin{pre}
#define NUM_TASKS 10
static volatile gint tasks_remaining = NUM_TASKS;
...

while (g_atomic_int_get (&tasks_remaining) != 0)
  g_main_context_iteration (NULL, TRUE);
Then in a thread:

perform_work();

if (g_atomic_int_dec_and_test (&tasks_remaining))
  g_main_context_wakeup (NULL);
  @end{pre}
  @see-type{g-main-context}
  @see-function{g-main-context-iteration}"
  (context (:pointer (:struct g-main-context))))

(export 'g-main-context-wakeup)

;;; ----------------------------------------------------------------------------
;;; g_main_context_acquire ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_acquire" g-main-context-acquire) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance}
  @return{@em{True} if the operation succeeded, and this thread is now the
    owner of @arg{context}.}
  @short{Tries to become the owner of the specified context.}
  If some other thread is the owner of the context, returns @em{false}
  immediately. Ownership is properly recursive: the owner can require ownership
  again and will release ownership when the function
  @fun{g-main-context-release} is called as many times as the function
  @sym{g-main-context-acquire}.
  @see-type{g-main-context}
  @see-function{g-main-context-release}"
  (context (:pointer (:struct g-main-context))))

(export 'g-main-context-acquire)

;;; ----------------------------------------------------------------------------
;;; g_main_context_release ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_release" g-main-context-release) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance}
  @begin{short}
    Releases ownership of a context previously acquired by this thread with
    the function @fun{g-main-context-acquire}.
  @end{short}
  If the context was acquired multiple times, the ownership will be released
  only when the function @sym{g-main-context-release} is called as many times
  as it was acquired.
  @see-type{g-main-context}
  @see-function{g-main-context-acquire}"
  (context (:pointer (:struct g-main-context))))

(export 'g-main-context-release)

;;; ----------------------------------------------------------------------------
;;; g_main_context_is_owner ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_is_owner" g-main-context-is-owner) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance}
  @return{@em{True} if the current thread is owner of @arg{context}.}
  @begin{short}
    Determines whether this thread holds the (recursive) ownership of this
    context.
  @end{short}
  This is useful to know before waiting on another thread that may be blocking
  to get ownership of context.
  @see-type{g-main-context}"
  (context (:pointer (:struct g-main-context))))

(export 'g-main-context-is-owner)

;;; ----------------------------------------------------------------------------
;;; g_main_context_wait ()
;;;
;;; gboolean
;;; g_main_context_wait (GMainContext *context,
;;;                      GCond *cond,
;;;                      GMutex *mutex);
;;;
;;; g_main_context_wait has been deprecated since version 2.58 and should not
;;; be used in newly-written code.
;;;
;;; Use g_main_context_is_owner() and separate locking instead.
;;;
;;; Tries to become the owner of the specified context, as with
;;; g_main_context_acquire(). But if another thread is the owner, atomically
;;; drop mutex and wait on cond until that owner releases ownership or until
;;; cond is signaled, then try again (once) to become the owner.
;;;
;;; context :
;;;     a GMainContext
;;;
;;; cond :
;;;     a condition variable
;;;
;;; mutex :
;;;     a mutex, currently held
;;;
;;; Returns :
;;;     TRUE if the operation succeeded, and this thread is now the owner of
;;;     context .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_prepare ()                              not exported
;;; ----------------------------------------------------------------------------

;; TODO: priority is a foreign integer, return this value

(defcfun ("g_main_context_prepare" g-main-context-prepare) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @argument[context]{a @type{g-main-context} instance}
  @argument[priority]{location to store a foreign integer with the priority of
    highest priority source already ready}
  @return{@em{True} if some source is ready to be dispatched prior to polling.}
  @begin{short}
    Prepares to poll sources within a main loop.
  @end{short}
  The resulting information for polling is determined by calling
  the function @fun{g-main-context-query}.
  @see-type{g-main-context}
  @see-function{g-main-context-query}
  @see-function{g-main-context-check}
  @see-function{g-main-context-dispatch}"
  (context (:pointer (:struct g-main-context)))
  (priority (:pointer :int)))

;;; ----------------------------------------------------------------------------
;;; g_main_context_query ()                                not exported
;;; ----------------------------------------------------------------------------

;; TODO: Return the foreign values timeout, and fds

(defcfun ("g_main_context_query" g-main-context-query) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @argument[context]{a @type{g-main-context} instance}
  @argument[max-priority]{an integer with the maximum priority source to check}
  @argument[timeout]{location to store a foreign integer with the timeout to be
    used in polling}
  @argument[fds]{location to store an pointer to @type{g-poll-fd} records that
    need to be polled}
  @argument[n-fds]{length of @arg{fds}}
  @return{The number of records actually stored in @arg{fds}, or, if more than
  @arg{n-fds} records need to be stored, the number of records that need to be
    stored.}
  Determines information necessary to poll this main loop.
  @see-type{g-main-context}
  @see-function{g-main-context-check}
  @see-function{g-main-context-prepare}
  @see-function{g-main-context-dispatch}"
  (context (:pointer (:struct g-main-context)))
  (max-priority :int)
  (timeout (:pointer :int))
  (fds (:pointer (:struct g-poll-fd)))
  (n-fds :int))

;;; ----------------------------------------------------------------------------
;;; g_main_context_check ()                                not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_check" g-main-context-check) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @argument[context]{a @type{g-main-context} instance}
  @argument[max-priority]{an integer with the maximum numerical priority of
    sources to check}
  @argument[fds]{a pointer with the location of an array of @type{g-poll-fd}'s
    that was passed to the last call to @fun{g-main-context-query}}
  @argument[n-fds]{return value of the function @fun{g-main-context-query}}
  @return{@em{True} if some sources are ready to be dispatched.}
  @begin{short}
    Passes the results of polling back to the main loop.
  @end{short}
  @see-type{g-main-context}
  @see-type{g-poll-fd}
  @see-function{g-main-context-query}
  @see-function{g-main-context-prepare}
  @see-function{g-main-context-dispatch}"
  (context (:pointer (:struct g-main-context)))
  (max-priority :int)
  (fds (:pointer (:struct g-poll-fd)))
  (n-fds :int))

;;; ----------------------------------------------------------------------------
;;; g_main_context_dispatch ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_dispatch" g-main-context-dispatch) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[context]{a @type{g-main-context} instance}
  @begin{short}
    Dispatches all pending sources.
  @end{short}
  You must have successfully acquired the context with the function
  @fun{g-main-context-acquire} before you may call this function.
  @see-type{g-main-context}
  @see-function{g-main-context-aquire}"
  (context (:pointer (:struct g-main-context))))

(export 'g-main-context-dispatch)

;;; ----------------------------------------------------------------------------
;;; g_main_context_set_poll_func ()                        not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_set_poll_func" g-main-context-set-poll-func) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @argument[context]{a @type{g-main-context} instance}
  @argument[func]{the function to call to poll all file descriptors}
  @begin{short}
    Sets the function to use to handle polling of file descriptors.
  @end{short}
  It will be used instead of the @code{poll()} system call (or GLib's
  replacement function, which is used where @code{poll()} is not available).

  This function could possibly be used to integrate the GLib event loop with
  an external event loop.
  @see-type{g-main-context}
  @see-function{g-main-context-get-poll-func}"
  (context (:pointer (:struct g-main-context)))
  (func :pointer))

;;; ----------------------------------------------------------------------------
;;; g_main_context_get_poll_func ()                        not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_get_poll_func" g-main-context-get-poll-func) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @argument[context]{a @type{g-main-context} instance}
  @return{The poll function.}
  @begin{short}
    Gets the poll function set by the function
    @fun{g-main-context-set-poll-func}.
  @end{short}
  @see-type{g-main-context}
  @see-function{g-main-context-set-poll-func}"
  (context (:pointer (:struct g-main-context))))

;;; ----------------------------------------------------------------------------
;;; GPollFunc ()
;;;
;;; gint (*GPollFunc) (GPollFD *ufds, guint nfsd, gint timeout_);
;;;
;;; Specifies the type of function passed to g_main_context_set_poll_func().
;;; The semantics of the function should match those of the poll() system call.
;;;
;;; ufds :
;;;     an array of GPollFD elements
;;;
;;; nfsd :
;;;     the number of elements in ufds
;;;
;;; timeout_ :
;;;     the maximum time to wait for an event of the file descriptors. A
;;;     negative value indicates an infinite timeout.
;;;
;;; Returns :
;;;     the number of GPollFD elements which have events or errors reported, or
;;;     -1 if an error occurred.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_add_poll ()                             not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_add_poll" g-main-context-add-poll) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @argument[context]{a @type{g-main-context} instance or @code{null}-pointer
    for the default context}
  @argument[fd]{a @type{g-poll-fd} instance holding information about a file
    descriptor to watch}
  @argument[priority]{an integer with the priority for this file descriptor
    which should be the same as the priority used for the functin
    @fun{g-source-attach} to ensure that the file descriptor is polled whenever
    the results may be needed}
  @begin{short}
    Adds a file descriptor to the set of file descriptors polled for this
    context.
  @end{short}
  This will very seldom be used directly. Instead a typical event source will
  use the function @fun{g-source-add-poll} instead.
  @see-type{g-main-context}
  @see-type{g-poll-fd}
  @see-function{g-source-add-poll}
  @see-function{g-source-attach}"
  (context (:pointer (:struct g-main-context)))
  (fd (:pointer (:struct g-poll-fd)))
  (priority :int))

;;; ----------------------------------------------------------------------------
;;; g_main_context_remove_poll ()                          not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_remove_poll" g-main-context-remove-poll) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @argument[context]{a @type{g-main-context} instance}
  @argument[fd]{a @type{g-poll-fd} descriptor previously added with the
    function @fun{g-main-context-add-poll}}
  @begin{short}
    Removes file descriptor from the set of file descriptors to be polled for a
    particular context.
  @end{short}
  @see-type{g-main-context}
  @see-type{g-poll-fd}
  @see-function{g-main-context-add-poll}"
  (context (:pointer (:struct g-main-context)))
  (fd (:pointer (:struct g-poll-fd))))

;;; ----------------------------------------------------------------------------
;;; g_main_depth ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_depth" g-main-depth) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @return{An integer with the main loop recursion level in the current thread.}
  @begin{short}
    Returns the depth of the stack of calls to the function
    @fun{g-main-context-dispatch} on any context in the current thread.
  @end{short}
  That is, when called from the toplevel, it gives 0. When called from within
  a callback from the function @fun{g-main-context-iteration} (or the function
  @fun{g-main-loop-run}, etc.) it returns 1. When called from within a
  callback to a recursive call to the function @fun{g-main-context-iteration},
  it returns 2. And so forth.

  There is a temptation to use the function @sym{g-main-depth} to solve problems
  with reentrancy. For instance, while waiting for data to be received from the
  network in response to a menu item, the menu item might be selected again.
  It might seem that one could make the menu item's callback return
  immediately and do nothing if the function @sym{g-main-depth} returns a value
  greater than 1. However, this should be avoided since the user then sees
  selecting the menu item do nothing. Furthermore, you will find yourself adding
  these checks all over your code, since there are doubtless many things
  that the user could do. Instead, you can use the following techniques:
  @begin{itemize}
    @item{Use the function @fun{gtk-widget-sensitive} or modal dialogs to
      prevent the user from interacting with elements while the main loop is
      recursing.}
    @item{Avoid main loop recursion in situations where you cannot handle
      arbitrary callbacks. Instead, structure your code so that you simply
      return to the main loop and then get called again when there is more work
      to do.}
  @end{itemize}
  @see-type{g-main-context}
  @see-function{g-main-context-dispatch}
  @see-function{g-main-context-iteration}
  @see-function{g-main-loop-run}")

(export 'g-main-depth)

;;; ----------------------------------------------------------------------------
;;; g_main_current_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_current_source" g-main-current-source)
    (:pointer (:struct g-source))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @return{The currently firing @type{g-source} instance or @code{null}-pointer.}
  @short{Returns the currently firing source for this thread.}
  @see-type{g-source}")

(export 'g-main-current-source)

;;; ----------------------------------------------------------------------------
;;; g_main_set_poll_func()
;;;
;;; #define g_main_set_poll_func(func)
;;;
;;; Warning
;;;
;;; g_main_set_poll_func has been deprecated since version 2.2 and should not be
;;; used in newly-written code. Use g_main_context_set_poll_func() again
;;;
;;; Sets the function to use for the handle polling of file descriptors for the
;;; default main context.
;;;
;;; func :
;;;     the function to call to poll all file descriptors
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_invoke ()
;;;
;;; void g_main_context_invoke (GMainContext *context,
;;;                             GSourceFunc function,
;;;                             gpointer data);
;;;
;;; Invokes a function in such a way that context is owned during the invocation
;;; of function.
;;;
;;; If context is NULL then the global default main context - as returned by
;;; g_main_context_default() - is used.
;;;
;;; If context is owned by the current thread, function is called directly.
;;; Otherwise, if context is the thread-default main context of the current
;;; thread and g_main_context_acquire() succeeds, then function is called and
;;; g_main_context_release() is called afterwards.
;;;
;;; In any other case, an idle source is created to call function and that
;;; source is attached to context (presumably to be run in another thread). The
;;; idle source is attached with G_PRIORITY_DEFAULT priority. If you want a
;;; different priority, use g_main_context_invoke_full().
;;;
;;; Note that, as with normal idle functions, function should probably return
;;; FALSE. If it returns TRUE, it will be continuously run in a loop (and may
;;; prevent this call from returning).
;;;
;;; context :
;;;     a GMainContext, or NULL
;;;
;;; function :
;;;     function to call
;;;
;;; data :
;;;     data to pass to function
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_invoke_full ()
;;;
;;; void g_main_context_invoke_full (GMainContext *context,
;;;                                  gint priority,
;;;                                  GSourceFunc function,
;;;                                  gpointer data,
;;;                                  GDestroyNotify notify);
;;;
;;; Invokes a function in such a way that context is owned during the invocation
;;; of function.
;;;
;;; This function is the same as g_main_context_invoke() except that it lets you
;;; specify the priority incase function ends up being scheduled as an idle and
;;; also lets you give a GDestroyNotify for data.
;;;
;;; notify should not assume that it is called from any particular thread or
;;; with any particular context acquired.
;;;
;;; context :
;;;     a GMainContext, or NULL
;;;
;;; priority :
;;;     the priority at which to run function
;;;
;;; function :
;;;     function to call
;;;
;;; data :
;;;     data to pass to function
;;;
;;; notify :
;;;     a function to call when data is no longer in use, or NULL
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_pusher_new ()
;;;
;;; GMainContextPusher *
;;; g_main_context_pusher_new (GMainContext *main_context);
;;;
;;; Push main_context as the new thread-default main context for the current
;;; thread, using g_main_context_push_thread_default(), and return a new
;;; GMainContextPusher. Pop with g_main_context_pusher_free(). Using
;;; g_main_context_pop_thread_default() on main_context while a
;;; GMainContextPusher exists for it can lead to undefined behaviour.
;;;
;;; Using two GMainContextPushers in the same scope is not allowed, as it leads
;;; to an undefined pop order.
;;;
;;; This is intended to be used with g_autoptr(). Note that g_autoptr() is only
;;; available when using GCC or clang, so the following example will only work
;;; with those compilers:
;;;
;;; typedef struct
;;; {
;;;   ...
;;;   GMainContext *context;
;;;   ...
;;; } MyObject;
;;;
;;; static void
;;; my_object_do_stuff (MyObject *self)
;;; {
;;;   g_autoptr(GMainContextPusher) pusher =
;;;       g_main_context_pusher_new (self->context);
;;;
;;;   // Code with main context as the thread default here
;;;
;;;   if (cond)
;;;     // No need to pop
;;;     return;
;;;
;;;   // Optionally early pop
;;;   g_clear_pointer (&pusher, g_main_context_pusher_free);
;;;
;;;   // Code with main context no longer the thread default here
;;; }
;;;
;;; main_context :
;;;     a main context to push.
;;;
;;; Returns :
;;;     a GMainContextPusher.
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_pusher_free ()
;;;
;;; void
;;; g_main_context_pusher_free (GMainContextPusher *pusher);
;;;
;;; Pop pusher ’s main context as the thread default main context. See
;;; g_main_context_pusher_new() for details.
;;;
;;; This will pop the GMainContext as the current thread-default main context,
;;; but will not call g_main_context_unref() on it.
;;;
;;; pusher :
;;;     a GMainContextPusher.
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_get_thread_default ()                   not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_get_thread_default" g-main-context-thread-default)
    (:pointer (:struct g-main-context))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @begin{return}
    The thread default @type{g-main-context}, or @code{NULL} if the thread
    default context is the global default context.
  @end{return}
  @begin{short}
    Gets the thread default @type{g-main-context} for this thread.
  @end{short}
  Asynchronous operations that want to be able to be run in contexts other than
  the default one should call this method or the function
  @fun{g-main-context-ref-thread-default} to get a @type{g-main-context} to add
  their @type{g-source}s to. Note that even in single-threaded
  programs applications may sometimes want to temporarily push a non-default
  context, so it is not safe to assume that this will always return @code{NULL}
  if you are running in the default thread.

  If you need to hold a reference on the context, use the function
  @fun{g-main-context-ref-thread-default} instead.
  @see-type{g-main-context}
  @see-function{g-main-context-ref-thread-default}")

;;; ----------------------------------------------------------------------------
;;; g_main_context_ref_thread_default ()                   not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_ref_thread_default" g-main-context-ref-thread-default)
    (:pointer (:struct g-main-context))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @begin{return}
    The thread-default @symbol{g-main-context}. Unref with the function
    @fun{g-main-context-unref} when you are done with it
  @end{return}
  @begin{short}
    Gets the thread-default @symbol{g-main-context} for this thread, as with
    the function @fun{g-main-context-get-thread-default}, but also adds a
    reference to it with the function @fun{g-main-context-ref}.
  @end{short}
  In addition, unlike the function @fun{g-main-context-get-thread-default}, if
  the thread-default context is the global default context, this will return
  that @symbol{g-main-context}, with a ref added to it, rather than returning
  @code{NULL}.
  @see-symbol{g-main-context}
  @see-function{g-main-context-ref}
  @see-function{g-main-context-unref}
  @see-function{g-main-context-thread-default}")

;;; ----------------------------------------------------------------------------
;;; g_main_context_push_thread_default ()
;;;
;;; void g_main_context_push_thread_default (GMainContext *context);
;;;
;;; Acquires context and sets it as the thread-default context for the current
;;; thread. This will cause certain asynchronous operations (such as most
;;; gio-based I/O) which are started in this thread to run under context and
;;; deliver their results to its main loop, rather than running under the global
;;; default context in the main thread. Note that calling this function changes
;;; the context returned by g_main_context_get_thread_default(), not the one
;;; returned by g_main_context_default(), so it does not affect the context used
;;; by functions like g_idle_add().
;;;
;;; Normally you would call this function shortly after creating a new thread,
;;; passing it a GMainContext which will be run by a GMainLoop in that thread,
;;; to set a new default context for all async operations in that thread. (In
;;; this case, you don't need to ever call g_main_context_pop_thread_default().)
;;; In some cases however, you may want to schedule a single operation in a
;;; non-default context, or temporarily use a non-default context in the main
;;; thread. In that case, you can wrap the call to the asynchronous operation
;;; inside a g_main_context_push_thread_default() /
;;; g_main_context_pop_thread_default() pair, but it is up to you to ensure that
;;; no other asynchronous operations accidentally get started while the
;;; non-default context is active.
;;;
;;; Beware that libraries that predate this function may not correctly handle
;;; being used from a thread with a thread-default context. Eg, see
;;; g_file_supports_thread_contexts().
;;;
;;; context :
;;;     a GMainContext, or NULL for the global default context
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_pop_thread_default ()
;;;
;;; void g_main_context_pop_thread_default (GMainContext *context);
;;;
;;; Pops context off the thread-default context stack (verifying that it was on
;;; the top of the stack).
;;;
;;; context :
;;;     a GMainContext object, or NULL
;;;
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_timeout_source_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_timeout_source_new" g-timeout-source-new)
    (:pointer (:struct g-source))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[interval]{an integer with the timeout interval in milliseconds}
  @return{The newly created @type{g-source} timeout source.}
  @short{Creates a new timeout source.}
  The source will not initially be associated with any context and must be added
  to one with the function @fun{g-source-attach} before it will be executed.
  @see-type{g-source}
  @see-function{g-source-attach}
  @see-function{g-timeout-source-new-seconds}"
  (interval :int))

(export 'g-timeout-source-new)

;;; ----------------------------------------------------------------------------
;;; g_timeout_source_new_seconds ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_timeout_source_new_seconds" g-timeout-source-new-seconds)
    (:pointer (:struct g-source))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[interval]{an integer with the timeout interval in seconds}
  @return{The newly created @type{g-source} timeout source.}
  @short{Creates a new timeout source.}
  The source will not initially be associated with any context and must be added
  to one with the function @fun{g-source-attach} before it will be executed.

  The scheduling granularity/accuracy of this timeout source will be in
  seconds.
  @see-type{g-source}
  @see-function{g-source-attach}
  @see-function{g-timeout-source-new}"
  (interval :int))

(export 'g-timeout-source-new-seconds)

;;; ----------------------------------------------------------------------------
;;; g_timeout_add ()
;;; ----------------------------------------------------------------------------

(defun g-timeout-add (interval func &key (priority +g-priority-default+))
 #+cl-cffi-gtk-documentation
 "@version{*2021-4-10}
  @argument[interval]{an integer with the time between calls to the function,
    in milliseconds}
  @argument[function]{a @symbol{g-source-func} callback function to call}
  @argument[priority]{an integer with the priority of the timeout source.
    Typically this will be in the range between @var{+g-priority-default+} and
    @var{+g-priority-high+}.}
  @return{The unsigned integer ID greater than 0 of the event source.}
  @begin{short}
    Sets a function to be called at regular intervals, with @arg{priority}.
  @end{short}
  The default value is @var{+g-priority-default+}. The function is called
  repeatedly until it returns @em{false}, at which point the timeout is
  automatically destroyed and the function will not be called again. The first
  call to the function will be at the end of the first interval.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval (it does not
  try to 'catch up' time lost in delays).

  If you want to have a timer in the \"seconds\" range and do not care about
  the exact time of the first call of the timer, use the function
  @fun{g-timeout-add-seconds}; this function allows for more optimizations and
  more efficient system power usage.

  This internally creates a main loop source using the function
  @fun{g-timeout-source-new} and attaches it to the main loop context using
  the function @fun{g-source-attach}. You can do these steps manually if you
  need greater control.
  @see-symbol{g-source-func}
  @see-function{g-timeout-add-seconds}
  @see-function{g-timeout-source-new}
  @see-function{g-source-attach}"
  (%g-timeout-add-full priority
                       interval
                       (callback g-source-func)
                       (allocate-stable-pointer func)
                       (callback stable-pointer-destroy-notify-cb)))

(export 'g-timeout-add)

;;; ----------------------------------------------------------------------------
;;; g_timeout_add_full ()                                  not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_timeout_add_full" %g-timeout-add-full) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-7-20}
  @argument[priority]{the priority of the timeout source. Typically this will
    be in the range between @var{+g-priority-default+} and
    @var{+g-priority-high+}.}
  @argument[interval]{the time between calls to the function, in milliseconds
    (1/1000ths of a second)}
  @argument[function]{function to call}
  @argument[data]{data to pass to @arg{function}}
  @argument[notify]{function to call when the timeout is removed, or
    @code{null}-pointer}
  @return{the ID (greater than @code{0}) of the event source.}
  @begin{short}
    Sets a function to be called at regular intervals, with the given priority.
  @end{short}
  The function is called repeatedly until it returns @code{nil}, at which point
  the timeout is automatically destroyed and the function will not be called
  again. The @arg{notify} function is called when the timeout is destroyed. The
  first call to the function will be at the end of the first @arg{interval}.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval (it does not
  try to 'catch up' time lost in delays).

  This internally creates a main loop source using @fun{g-timeout-source-new}
  and attaches it to the main loop context using @fun{g-source-attach}. You can
  do these steps manually if you need greater control.

  The interval given in terms of monotonic time, not wall clock time. See
  @code{g_get_monotonic_time()}."
  (priority :int)
  (interval-milliseconds :uint)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

;;; ----------------------------------------------------------------------------
;;; g_timeout_add_seconds ()
;;; ----------------------------------------------------------------------------

(defun g-timeout-add-seconds (interval func
                              &key (priority +g-priority-default+))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[interval]{an unsigned integer with the time between calls to the
    function, in seconds}
  @argument[function]{a @symbol{g-source-func} callback function to call}
  @argument[priority]{an integer with the priority of the timeout source.
    Typically this will be in the range between @var{+g-priority-default+} and
    @var{+g-priority-high+}.}
  @return{The unsigned integer ID greater than 0 of the event source.}
  @begin{short}
    Sets a function to be called at regular intervals with @arg{priority}.
  @end{short}
  The default value is @var{+g-priority-default+}. The function is called
  repeatedly until it returns @em{false}, at which point the timeout is
  automatically destroyed and the function will not be called again.

  This internally creates a main loop source using the function
  @fun{g-timeout-source-new-seconds} and attaches it to the main loop context
  using the function @fun{g-source-attach}. You can do these steps manually if
  you need greater control.

  Note that the first call of the timer may not be precise for timeouts of one
  second. If you need finer precision and have such a timeout, you may want to
  use the function @fun{g-timeout-add} instead.
  @see-symbol{g-source-func}
  @see-function{g-timeout-source-new-seconds}
  @see-function{g-source-attach}
  @see-function{g-timeout-add}"
  (%g-timeout-add-seconds-full priority
                               interval
                               (callback g-source-func)
                               (allocate-stable-pointer func)
                               (callback stable-pointer-destroy-notify-cb)))

(export 'g-timeout-add-seconds)

;;; ----------------------------------------------------------------------------
;;; g_timeout_add_seconds_full ()                          not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_timeout_add_seconds_full" %g-timeout-add-seconds-full) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-01-17}
  @argument[priority]{the priority of the timeout source. Typically this will
    be in the range between @var{+g-priority-default+} and
    @var{+g-priority-high+}.}
  @argument[interval]{the time between calls to the function, in seconds}
  @argument[function]{function to call}
  @argument[data]{data to pass to function}
  @argument[notify]{function to call when the timeout is removed, or
    @code{null}-pointer}
  @return{the ID (greater than @code{0}) of the event source.}
  @begin{short}
    Sets a @arg{function} to be called at regular intervals, with priority.
  @end{short}
  The @arg{function} is called repeatedly until it returns @code{nil}, at which
  point the timeout is automatically destroyed and the function will not be
  called again.

  Unlike @fun{g-timeout-add}, this function operates at whole second
  granularity. The initial starting point of the timer is determined by the
  implementation and the implementation is expected to group multiple timers
  together so that they fire all at the same time. To allow this grouping, the
  interval to the first timer is rounded and can deviate up to one second from
  the specified @arg{interval}. Subsequent timer iterations will generally run
  at the specified interval.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval

  If you want timing more precise than whole seconds, use @fun{g-timeout-add}
  instead.

  The grouping of timers to fire at the same time results in a more power and
  CPU efficient behavior so if your timer is in multiples of seconds and you
  don't require the first timer exactly one second from now, the use of
  @fun{g-timeout-add-seconds} is preferred over @fun{g-timeout-add}.

  This internally creates a main loop source using
  @fun{g-timeout-source-new-seconds} and attaches it to the main loop context
  using @fun{g-source-attach}. You can do these steps manually if you need
  greater control.

  The interval given is in terms of monotonic time, not wall clock time. See
  @code{g_get_monotonic_time()}.

  Since 2.14"
  (priority :int)
  (interval-seconds :uint)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

;;; ----------------------------------------------------------------------------
;;; g_idle_source_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_idle_source_new" g-idle-source-new) (:pointer (:struct g-source))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @return{The newly created @type{g-source} idle source.}
  @begin{short}
    Creates a new idle source.
  @end{short}

  The source will not initially be associated with any context and must be
  added to one with the function @fun{g-source-attach} before it will be
  executed. Note that the default priority for idle sources is
  @var{+g-priority-default-idle+}, as compared to other sources which have
  a default priority of @var{+g-priority-default+}.
  @see-type{g-source}
  @see-function{g-source-attach}")

(export 'g-idle-source-new)

;;; ----------------------------------------------------------------------------
;;; g_idle_add ()
;;; ----------------------------------------------------------------------------

(defun g-idle-add (func &key (priority +g-priority-default-idle+))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[func]{a @symbol{g-source-func} callback function to call}
  @argument[priority]{an integer with the priority of the idle source, typically
    this will be in the range between @var{+g-priority-default-idle+} and
    @var{+g-priority-high-idle+}}
  @return{The unsigned integer ID greater than 0 of the event source.}
  @begin{short}
    Adds a function to be called whenever there are no higher priority events
    pending to the default main loop.
  @end{short}
  The keyword argument @arg{priority} has the default value
  @var{+g-priority-default-idle+}. If the function returns @em{false} it is
  automatically removed from the list of event sources and will not be called
  again.

  This internally creates a main loop source using the function
  @fun{g-idle-source-new} and attaches it to the main loop context using
  the function @fun{g-source-attach}. You can do these steps manually if you
  need greater control.
  @see-symbol{g-source-func}
  @see-function{g-idle-source-new}
  @see-function{g-source-attach}"
  (%g-idle-add-full priority
                    (callback g-source-func)
                    (allocate-stable-pointer func)
                    (callback stable-pointer-destroy-notify-cb)))

(export 'g-idle-add)

;;; ----------------------------------------------------------------------------
;;; g_idle_add_full ()                                     not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_idle_add_full" %g-idle-add-full) :uint
 #+cl-cffi-gtk-documentation
 "@version{2013-1-1}
  @argument[priority]{the priority of the idle source. Typically this will be
    in the range between G_PRIORITY_DEFAULT_IDLE and G_PRIORITY_HIGH_IDLE.}
  @argument[function]{function to call}
  @argument[data]{data to pass to function}
  @argument[notify]{function to call when the idle is removed, or NULL}
  @return{the ID (greater than 0) of the event source. Rename to: g_idle_add}
  @begin{short}
    Adds a function to be called whenever there are no higher priority events
    pending.
  @end{short}
  If the function returns FALSE it is automatically removed from the list of
  event sources and will not be called again.

  This internally creates a main loop source using g_idle_source_new() and
  attaches it to the main loop context using g_source_attach(). You can do
  these steps manually if you need greater control."
  (priority :uint)
  (function :pointer)
  (data :pointer)
  (notify :pointer))

;;; ----------------------------------------------------------------------------
;;; g_idle_remove_by_data ()                               not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_idle_remove_by_data" g-idle-remove-by-data) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-1-1}
  @argument[data]{the data for the idle source's callback.}
  @return{TRUE if an idle source was found and removed.}
  @short{Removes the idle function with the given data.}"
  (data :pointer))

;;; ----------------------------------------------------------------------------
;;; GChildWatchFunc ()
;;;
;;; void (*GChildWatchFunc) (GPid pid, gint status, gpointer user_data);
;;;
;;; The type of functions to be called when a child exists.
;;;
;;; pid :
;;;     the process id of the child process
;;;
;;; status :
;;;     Status information about the child process, see waitpid(2) for more
;;;     information about this field
;;;
;;; user_data :
;;;     user data passed to g_child_watch_add()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_child_watch_source_new ()
;;;
;;; GSource * g_child_watch_source_new (GPid pid);
;;;
;;; Creates a new child_watch source.
;;;
;;; The source will not initially be associated with any GMainContext and must
;;; be added to one with g_source_attach() before it will be executed.
;;;
;;; Note that child watch sources can only be used in conjunction with
;;; g_spawn... when the G_SPAWN_DO_NOT_REAP_CHILD flag is used.
;;;
;;; Note that on platforms where GPid must be explicitly closed (see
;;; g_spawn_close_pid()) pid must not be closed while the source is still
;;; active. Typically, you will want to call g_spawn_close_pid() in the callback
;;; function for the source.
;;;
;;; Note further that using g_child_watch_source_new() is not compatible with
;;; calling waitpid(-1) in the application. Calling waitpid() for individual
;;; pids will still work fine.
;;;
;;; pid :
;;;     process to watch. On POSIX the pid of a child process. On Windows a
;;;     handle for a process (which does not have to be a child).
;;;
;;; Returns :
;;;     the newly-created child watch source
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_child_watch_add ()
;;;
;;; guint g_child_watch_add (GPid pid,
;;;                          GChildWatchFunc function,
;;;                          gpointer data);
;;;
;;; Sets a function to be called when the child indicated by pid exits, at a
;;; default priority, G_PRIORITY_DEFAULT.
;;;
;;; If you obtain pid from g_spawn_async() or g_spawn_async_with_pipes() you
;;; will need to pass G_SPAWN_DO_NOT_REAP_CHILD as flag to the spawn function
;;; for the child watching to work.
;;;
;;; Note that on platforms where GPid must be explicitly closed (see
;;; g_spawn_close_pid()) pid must not be closed while the source is still
;;; active. Typically, you will want to call g_spawn_close_pid() in the callback
;;; function for the source.
;;;
;;; GLib supports only a single callback per process id.
;;;
;;; This internally creates a main loop source using g_child_watch_source_new()
;;; and attaches it to the main loop context using g_source_attach(). You can do
;;; these steps manually if you need greater control.
;;;
;;; pid :
;;;     process id to watch. On POSIX the pid of a child process. On Windows a
;;;     handle for a process (which does not have to be a child).
;;;
;;; function :
;;;     function to call
;;;
;;; data :
;;;     data to pass to function
;;;
;;; Returns :
;;;     the ID (greater than 0) of the event source.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_child_watch_add_full ()
;;;
;;; guint g_child_watch_add_full (gint priority,
;;;                               GPid pid,
;;;                               GChildWatchFunc function,
;;;                               gpointer data,
;;;                               GDestroyNotify notify);
;;;
;;; Sets a function to be called when the child indicated by pid exits, at the
;;; priority priority.
;;;
;;; If you obtain pid from g_spawn_async() or g_spawn_async_with_pipes() you
;;; will need to pass G_SPAWN_DO_NOT_REAP_CHILD as flag to the spawn function
;;; for the child watching to work.
;;;
;;; Note that on platforms where GPid must be explicitly closed (see
;;; g_spawn_close_pid()) pid must not be closed while the source is still
;;; active. Typically, you will want to call g_spawn_close_pid() in the callback
;;; function for the source.
;;;
;;; GLib supports only a single callback per process id.
;;;
;;; This internally creates a main loop source using g_child_watch_source_new()
;;; and attaches it to the main loop context using g_source_attach(). You can
;;; do these steps manually if you need greater control.
;;;
;;; priority :
;;;     the priority of the idle source. Typically this will be in the range
;;;     between G_PRIORITY_DEFAULT_IDLE and G_PRIORITY_HIGH_IDLE.
;;;
;;; pid :
;;;     process to watch. On POSIX the pid of a child process. On Windows a
;;;     handle for a process (which does not have to be a child).
;;;
;;; function :
;;;     function to call
;;;
;;; data :
;;;     data to pass to function
;;;
;;; notify :
;;;     function to call when the idle is removed, or NULL
;;;
;;; Returns :
;;;     the ID (greater than 0) of the event source. Rename to:
;;;     g_child_watch_add
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_poll ()
;;;
;;; gint g_poll (GPollFD *fds, guint nfds, gint timeout);
;;;
;;; Polls fds, as with the poll() system call, but portably. (On systems that
;;; don't have poll(), it is emulated using select().) This is used internally
;;; by GMainContext, but it can be called directly if you need to block until
;;; a file descriptor is ready, but don't want to run the full main loop.
;;;
;;; Each element of fds is a GPollFD describing a single file descriptor to
;;; poll. The fd field indicates the file descriptor, and the events field
;;; indicates the events to poll for. On return, the revents fields will be
;;; filled with the events that actually occurred.
;;;
;;; On POSIX systems, the file descriptors in fds can be any sort of file
;;; descriptor, but the situation is much more complicated on Windows. If you
;;; need to use g_poll() in code that has to run on Windows, the easiest
;;; solution is to construct all of your GPollFDs with
;;; g_io_channel_win32_make_pollfd().
;;;
;;; fds :
;;;     file descriptors to poll
;;;
;;; nfds :
;;;     the number of file descriptors in fds
;;;
;;; timeout :
;;;     amount of time to wait, in milliseconds, or -1 to wait forever
;;;
;;; Returns :
;;;     the number of entries in fds whose revents fields were filled in, or 0
;;;     if the operation timed out, or -1 on error or if the call was
;;;     interrupted.
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GSourceDummyMarshal ()
;;;
;;; void (*GSourceDummyMarshal) (void);
;;;
;;; This is just a placeholder for GClosureMarshal, which cannot be used here
;;; for dependency reasons.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GSourceDisposeFunc ()
;;;
;;; void
;;; (*GSourceDisposeFunc) (GSource *source);
;;;
;;; Dispose function for source . See g_source_set_dispose_function() for
;;; details.
;;;
;;; source :
;;;     GSource that is currently being disposed
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_new ()                                        not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_new" g-source-new) (:pointer (:struct g-source))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[source-funcs]{a @type{g-source-funcs} instance containing functions
    that implement the sources behavior}
  @argument[struct-size]{size of the @type{g-source} structure to create}
  @return{The newly-created @type{g-source} instance.}
  @begin{short}
    Creates a new source.
  @end{short}
  The size is specified to allow creating structures derived from a
  @type{g-source} structure that contain additional data. The size passed in
  must be at least @code{(cffi:foreign-type-size '(:struct g-source))}.

  The source will not initially be associated with any context and must be added
  to one with the function @fun{g-source-attach} before it will be executed.
  @see-type{g-source}
  @see-type{g-source-funcs}
  @see-function{g-source-attach}"
  (source-funcs (:pointer (:struct g-source-funcs)))
  (struct-size :uint))

;;; ----------------------------------------------------------------------------
;;; g_source_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_ref" g-source-ref) (:pointer (:struct g-source))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[source]{a @type{g-source} instance}
  @return{The argument @arg{source}.}
  @short{Increases the reference count on a source by one.}
  @see-type{g-source}
  @see-function{g-source-unref}"
  (source (:pointer (:struct g-source))))

(export 'g-source-ref)

;;; ----------------------------------------------------------------------------
;;; g_source_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_unref" g-source-unref) :void
 "@version{2021-4-10}
  @argument[source]{a @type{g-source} instance}
  @begin{short}
    Decreases the reference count of a @rg{source} by one.
  @end{short}
  If the resulting reference count is zero the source and associated memory
  will be destroyed.
  @see-type{g-source}
  @see-function{g-source-ref}"
  (source (:pointer (:struct g-source))))

(export 'g-source-unref)

;;; ----------------------------------------------------------------------------
;;; g_source_set_funcs ()                                  not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_set_funcs" g-source-set-funcs) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-21}
  @argument[source]{a @type{g-source} instance}
  @argument[funcs]{the new @type{g-source-funcs}}
  @begin{short}
    Sets the source functions of an unattached source.
  @end{short}
  This function can be used to override default implementations.

  Since 2.12"
  (source (:pointer (:struct g-source)))
  (funcs (:pointer (:struct g-source-funcs))))

;;; ----------------------------------------------------------------------------
;;; g_source_set_dispose_function ()
;;;
;;; void
;;; g_source_set_dispose_function (GSource *source,
;;;                                GSourceDisposeFunc dispose);
;;;
;;; Set dispose as dispose function on source . dispose will be called once the
;;; reference count of source reaches 0 but before any of the state of the
;;; source is freed, especially before the finalize function is called.
;;;
;;; This means that at this point source is still a valid GSource and it is
;;; allow for the reference count to increase again until dispose returns.
;;;
;;; The dispose function can be used to clear any "weak" references to the
;;; source in other data structures in a thread-safe way where it is possible
;;; for another thread to increase the reference count of source again while it
;;; is being freed.
;;;
;;; The finalize function can not be used for this purpose as at that point
;;; source is already partially freed and not valid anymore.
;;;
;;; This should only ever be called from GSource implementations.
;;;
;;; source :
;;;     A GSource to set the dispose function on
;;;
;;; dispose :
;;;     GSourceDisposeFunc to set on the source
;;;
;;; Since 2.64
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_attach ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_attach" g-source-attach) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[source]{a @type{g-source} instance}
  @argument[context]{a @type{g-main-context} instance, if @code{NULL}-pointer,
    the default context will be used}
  @begin{return}
    The unsigned integer ID greater than 0 for the source within @arg{context}.
  @end{return}
  @begin{short}
    Adds a source to a context so that it will be executed within that context.
  @end{short}
  Remove it by calling the function @fun{g-source-destroy}.
  @see-type{g-source}
  @see-type{g-main-context}
  @see-function{g-source-destroy}"
  (source (:pointer (:struct g-source)))
  (context (:pointer (:struct g-main-context))))

(export 'g-source-attach)

;;; ----------------------------------------------------------------------------
;;; g_source_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_destroy" g-source-destroy) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[source]{a @type{g-source} instance}
  @begin{short}
    Removes a source from its context, if any, and mark it as destroyed.
  @end{short}
  The source cannot be subsequently added to another context.
  @see-type{g-source}
  @see-function{g-source-attach}
  @see-function{g-source-is-destroyed}"
  (source (:pointer (:struct g-source))))

(export 'g-source-destroy)

;;; ----------------------------------------------------------------------------
;;; g_source_is_destroyed ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_is_destroyed" g-source-is-destroyed) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-10}
  @argument[source]{a @type{g-source} instance}
  @return{@em{True} if @arg{source} has been destroyed.}
  @begin{short}
    Returns whether the source has been destroyed.
  @end{short}

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
  @see-type{g-source}"
  (source (:pointer (:struct g-source))))

(export 'g-source-is-destroyed)

;;; ----------------------------------------------------------------------------
;;; g_source_get_priority ()
;;; g_source_set_priority () -> g-source-priority
;;; ----------------------------------------------------------------------------

(defun (setf g-source-priority) (priority source)
  (foreign-funcall "g_source_set_priority"
                   (:pointer (:struct g-source)) source
                   :int priority
                   :void)
  priority)

(defcfun ("g_source_get_priority" g-source-priority) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-4-12}
  @syntax[]{(g-source-priority source) => priority}
  @syntax[]{(setf (g-source-priority source) => priority)}
  @argument[source]{a @type{g-source} instance}
  @argument[priority]{an integer with the priority}
  @begin{short}
    The function @sym{g-source-priority} gets the priority of the source.
  @end{short}
  The function @sym{(setf g-source-priority)} sets the priority.

  While the main loop is being run, a source will be dispatched if it is ready
  to be dispatched and no sources at a higher (numerically smaller) priority
  are ready to be dispatched.
  @see-type{g-source}"
  (source (:pointer (:struct g-source))))

(export 'g-source-priority)

;;; ----------------------------------------------------------------------------
;;; g_source_get_can_recurse ()
;;; g_source_set_can_recurse () -> g-source-can-recursive
;;; ----------------------------------------------------------------------------

(defun (setf g-source-can-recurse) (can-recurse source)
  (foreign-funcall "g_source_set_can_recurse"
                   (:pointer (:struct g-source)) source
                   :boolean can-recurse
                   :void)
  can-recurse)

(defcfun ("g_source_get_can_recurse" g-source-can-recurse) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @syntax[]{(g-source-can-recurse source) => can-recurse}
  @syntax[]{(setf g-source-can-recurse source) can-recurse)}
  @argument[source]{a @type{g-source} instance}
  @argument[can-recurse]{a boolean whether recursion is allowed for this source}
  @begin{short}
    The function @sym{g-source-can-recurse} checks whether a source is allowed
    to be called recursively.
  @end{short}
  Sets whether @arg{source} can be called recursively.

  If @arg{can-recurse} is @em{true}, then while the source is being dispatched
  then this source will be processed normally. Otherwise, all processing of this
  source is blocked until the dispatch function returns.
  @see-type{g-source}"
  (source (:pointer (:struct g-source))))

(export 'g-source-can-recurse)

;;; ----------------------------------------------------------------------------
;;; g_source_get_id () -> g-source-id
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_get_id" g-source-id) :uint
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[source]{a @type{g-source} instance}
  @return{An unsigned integer with the ID greater than 0 for @arg{source}.}
  @begin{short}
    Returns the numeric ID for a particular source.
  @end{short}
  The ID of a source is a positive integer which is unique within a particular
  main loop context. The reverse mapping from ID to source is done by the
  function @fun{g-main-context-find-source-by-id}.
  @see-type{g-source}
  @see-function{g-main-context-find-source-by-id}"
  (source (:pointer (:struct g-source))))

(export 'g-source-id)

;;; ----------------------------------------------------------------------------
;;; g_source_get_name ()
;;; g_source_set_name () -> g-source-name
;;; ----------------------------------------------------------------------------

(defun (setf g-source-name) (name source)
  (foreign-funcall "g_source_set_name"
                   (:pointer (:struct g-source)) source
                   :string name
                   :void)
  name)

(defcfun ("g_source_get_name" g-source-name) :string
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @syntax[]{(g-source-name source) => name}
  @syntax[]{(setf (g-source-name source) name)}
  @argument[source]{a @type{g-source}}
  @argument[name]{a string with the debug name for the source}
  @begin{short}
    The function @sym{g-source-name} gets a name for the source, used in
    ebugging and profiling.
  @end{short}
  The function @sym{(setf g-source-name)} sets a name for the source. The name
  may be @code{NULL}.

  The source name should describe in a human-readable way what the source
  does. For example, \"X11 event queue\" or \"GTK+ repaint idle handler\" or
  whatever it is.

  It is permitted to call this function multiple times, but is not recommended
  due to the potential performance impact. For example, one could change the
  name in the \"check\" function of a @code{GSourceFuncs} to include details
  like the event type in the source name.
  @see-type{g-source}"
  (source (:pointer (:struct g-source))))

(export 'g-source-name)

;;; ----------------------------------------------------------------------------
;;; g_source_set_name_by_id ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_set_name_by_id" g-source-set-name-by-id) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[source-id]{an integer with the source ID}
  @argument[name]{a string with the debug name for the source}
  @begin{short}
    Sets the name of a source using its ID.
  @end{short}
  This is a convenience utility to set source names from the return value of
  the functions @fun{g-idle-add}, @fun{g-timeout-add}, etc.
  @see-type{g-source}
  @see-function{g-idle-add}
  @see-function{g-timeout-add}
  @see-function{g-source-name}"
  (id :uint)
  (name :string))

(export 'g-source-set-name-by-id)

;;; ----------------------------------------------------------------------------
;;; g_source_get_context () -> g-source-context
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_get_context" g-source-context)
    (:pointer (:struct g-main-context))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[source]{a @type{g-source} instance}
  @return{The @type{g-main-context} instance with which the source is
    associated, or @code{NULL} if the context has not yet been added to a
    source.}
  @begin{short}
    Gets the context with which the source is associated.
  @end{short}
  Calling this function on a destroyed source is an error.
  @see-type{g-source}
  @see-type{g-main-context}"
  (source (:pointer (:struct g-source))))

(export 'g-source-context)

;;; ----------------------------------------------------------------------------
;;; g_source_set_callback ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_set_callback" %g-source-set-callback) :void
  (source (:pointer (:struct g-source)))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun g-source-set-callback (source func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[source]{a @type{g-source} instance}
  @argument[func]{a @symbol{g-source-func} callback function}
  @begin{short}
    Sets the callback function for a source.
  @end{short}
  The callback function for a source is called from the source's dispatch
  function.

  Typically, you will not use this function. Instead use functions specific to
  the type of source you are using.
  @see-type{g-source}
  @see-symbol{g-source-func}"
  (%g-source-set-callback source
                          (callback g-source-func)
                          (allocate-stable-pointer func)
                          (callback stable-pointer-destroy-notify-cb)))

(export 'g-source-set-callback)

;;; ----------------------------------------------------------------------------
;;; GSourceFunc ()
;;; ----------------------------------------------------------------------------

(defcallback g-source-func :boolean
    ((data :pointer))
  (funcall (get-stable-pointer-value data)))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-source-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'g-source-func atdoc:*external-symbols*)
 "@version{2021-4-13}
  @begin{short}
    Specifies the type of callback function passed to the functions
    @fun{g-timeout-add}, and @fun{g-idle-add}.
  @end{short}
  @begin{pre}
 lambda ()
  @end{pre}
  @begin[code]{table}
    @entry[Returns]{@em{False} if the source should be removed. The constants
    @var{+g-source-continue+} and @var{+g-source-remove+} are memorable names
    for the return value.}
  @end{table}
  @see-function{g-timeout-add}
  @see-function{g-idle-add}")

(export 'g-source-func)

;;; ----------------------------------------------------------------------------
;;; g_source_set_callback_indirect ()
;;;
;;; void
;;; g_source_set_callback_indirect (GSource *source,
;;;                                 gpointer callback_data,
;;;                                 GSourceCallbackFuncs *callback_funcs);
;;;
;;; Sets the callback function storing the data as a refcounted callback
;;; "object". This is used internally. Note that calling
;;; g_source_set_callback_indirect() assumes an initial reference count on
;;; callback_data, and thus callback_funcs->unref will eventually be called
;;; once more than callback_funcs->ref.
;;;
;;; source :
;;;     the source
;;;
;;; callback_data :
;;;     pointer to callback data "object"
;;;
;;; callback_funcs :
;;;     functions for reference counting callback_data and getting the callback
;;;     and data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_set_ready_time ()
;;;
;;; void g_source_set_ready_time (GSource *source, gint64 ready_time);
;;;
;;; Sets a GSource to be dispatched when the given monotonic time is reached
;;; (or passed). If the monotonic time is in the past (as it always will be if
;;; ready_time is 0) then the source will be dispatched immediately.
;;;
;;; If ready_time is -1 then the source is never woken up on the basis of the
;;; passage of time.
;;;
;;; Dispatching the source does not reset the ready time. You should do so
;;; yourself, from the source dispatch function.
;;;
;;; Note that if you have a pair of sources where the ready time of one suggests
;;; that it will be delivered first but the priority for the other suggests that
;;; it would be delivered first, and the ready time for both sources is reached
;;; during the same main context iteration then the order of dispatch is
;;; undefined.
;;;
;;; source :
;;;     a GSource
;;;
;;; ready_time :
;;;     the monotonic time at which the source will be ready, 0 for
;;;     "immediately", -1 for "never"
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_get_ready_time ()
;;;
;;; gint64 g_source_get_ready_time (GSource *source);
;;;
;;; Gets the "ready time" of source, as set by g_source_set_ready_time().
;;;
;;; Any time before the current monotonic time (including 0) is an indication
;;; that the source will fire immediately.
;;;
;;; source :
;;;     a GSource
;;;
;;; Returns :
;;;     the monotonic ready time, -1 for "never"
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_add_unix_fd ()
;;;
;;; gpointer
;;; g_source_add_unix_fd (GSource *source,
;;;                       gint fd,
;;;                       GIOCondition events);
;;;
;;; Monitors fd for the IO events in events.
;;;
;;; The tag returned by this function can be used to remove or modify the
;;; monitoring of the fd using g_source_remove_unix_fd() or
;;; g_source_modify_unix_fd().
;;;
;;; It is not necessary to remove the fd before destroying the source; it will
;;; be cleaned up automatically.
;;;
;;; As the name suggests, this function is not available on Windows.
;;;
;;; source :
;;;     a GSource
;;;
;;; fd :
;;;     the fd to monitor
;;;
;;; events :
;;;     an event mask
;;;
;;; Returns :
;;;     an opaque tag
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_remove_unix_fd ()
;;;
;;; void g_source_remove_unix_fd (GSource *source, gpointer tag);
;;;
;;; Reverses the effect of a previous call to g_source_add_unix_fd().
;;;
;;; You only need to call this if you want to remove an fd from being watched
;;; while keeping the same source around. In the normal case you will just want
;;; to destroy the source.
;;;
;;; As the name suggests, this function is not available on Windows.
;;;
;;; source :
;;;     a GSource
;;;
;;; tag :
;;;     the tag from g_source_add_unix_fd()
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_modify_unix_fd ()
;;;
;;; void
;;; g_source_modify_unix_fd (GSource *source,
;;;                          gpointer tag,
;;;                          GIOCondition new_events);
;;;
;;; Updates the event mask to watch for the fd identified by tag.
;;;
;;; tag is the tag returned from g_source_add_unix_fd().
;;;
;;; If you want to remove a fd, don't set its event mask to zero. Instead, call
;;; g_source_remove_unix_fd().
;;;
;;; As the name suggests, this function is not available on Windows.
;;;
;;; source :
;;;     a GSource
;;;
;;; tag :
;;;     the tag from g_source_add_unix_fd()
;;;
;;; new_events :
;;;     the new event mask to watch
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_query_unix_fd ()
;;;
;;; GIOCondition g_source_query_unix_fd (GSource *source, gpointer tag);
;;;
;;; Queries the events reported for the fd corresponding to tag on source
;;; during the last poll.
;;;
;;; The return value of this function is only defined when the function is
;;; called from the check or dispatch functions for source.
;;;
;;; As the name suggests, this function is not available on Windows.
;;;
;;; source :
;;;     a GSource
;;;
;;; tag :
;;;     the tag from g_source_add_unix_fd()
;;;
;;; Returns :
;;;     the conditions reported on the fd
;;;
;;; Since 2.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_add_poll ()                                   not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_add_poll" g-source-add-poll) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-9}
  @argument[source]{a @type{g-source} instance}
  @argument[fd]{a @type{g-poll-fd} instance holding information about a file
    descriptor to watch}
  @begin{short}
    Adds a file descriptor to the set of file descriptors polled for this
    source.
  @end{short}
  This is usually combined with the function @fun{g-source-new} to add an event
  source. The event source's check function will typically test the revents
  field in the @type{g-poll-fd} instance and return @em{true} if events need
  to be processed.
  @see-type{g-source}
  @see-type{g-poll-fd}
  @see-function{g-source-new}"
  (source (:pointer (:struct g-source)))
  (fd (:pointer (:struct g-poll-fd))))

;;; ----------------------------------------------------------------------------
;;; g_source_remove_poll ()                                not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_remove_poll" g-source-remove-poll) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-13}
  @argument[source]{a @type{g-source} structure}
  @argument[fd]{a @type{g-poll-fd} structure previously passed to the function
    @fun{g-source-add-poll}}
  Removes a file descriptor from the set of file descriptors polled for this
  source."
  (source (:pointer (:struct g-source)))
  (fd (:pointer (:struct g-poll-fd))))

;;; ----------------------------------------------------------------------------
;;; g_source_add_child_source ()
;;;
;;; void g_source_add_child_source (GSource *source, GSource *child_source);
;;;
;;; Adds child_source to source as a "polled" source; when source is added to a
;;; GMainContext, child_source will be automatically added with the same
;;; priority, when child_source is triggered, it will cause source to dispatch
;;; (in addition to calling its own callback), and when source is destroyed, it
;;; will destroy child_source as well. (source will also still be dispatched if
;;; its own prepare/check functions indicate that it is ready.)
;;;
;;; If you don't need child_source to do anything on its own when it triggers,
;;; you can call g_source_set_dummy_callback() on it to set a callback that does
;;; nothing (except return TRUE if appropriate).
;;;
;;; source will hold a reference on child_source while child_source is attached
;;; to it.
;;;
;;; source :
;;;     a GSource
;;;
;;; child_source :
;;;     a second GSource that source should "poll"
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_remove_child_source ()
;;;
;;; void g_source_remove_child_source (GSource *source, GSource *child_source);
;;;
;;; Detaches child_source from source and destroys it.
;;;
;;; source :
;;;     a GSource
;;;
;;; child_source :
;;;     a GSource previously passed to g_source_add_child_source().
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_get_time () -> g-source-time
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_get_time" g-source-time) :uint64
 #+cl-cffi-gtk-documentation
 "@version{2021-4-13}
  @argument[source]{a @type{g-source} instance}
  @return{An unsigned integer with the monotonic time in microseconds.}
  @begin{short}
    Gets the time to be used when checking this source.
  @end{short}
  The time here is the system monotonic time, if available, or some other
  reasonable alternative otherwise.
  @see-type{g-source}"
  (source (:pointer (:struct g-source))))

(export 'g-source-time)

;;; ----------------------------------------------------------------------------
;;; g_source_get_current_time ()
;;;
;;; void
;;; g_source_get_current_time (GSource *source, GTimeVal *timeval);
;;;
;;; g_source_get_current_time has been deprecated since version 2.28 and should
;;; not be used in newly-written code.
;;;
;;; use g_source_get_time() instead
;;;
;;; This function ignores source and is otherwise the same as
;;; g_get_current_time().
;;;
;;; source :
;;;     a GSource
;;;
;;; timeval :
;;;     GTimeVal structure in which to store current time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_remove" g-source-remove) :boolean
 #+cl-cffi-gtk-documentation
 "@version{*2021-4-13}
  @argument[source-id]{the unsigned integer ID of the source to remove}
  @return{@em{True} if the source was found and removed.}
  @begin{short}
    Removes the source with the given ID from the default main context.
  @end{short}
  The ID of a @type{g-source} instance is given by the function
  @fun{g-source-id}, or will be returned by the functions @fun{g-source-attach},
  @fun{g-idle-add}, @fun{g-timeout-add}.

  You must use the function @fun{g-source-destroy} for sources added to a
  non-default main context.
  @see-type{g-source}
  @see-function{g-source-id}
  @see-function{g-source-destroy}"
  (source-id :uint))

(export 'g-source-remove)

;;; ----------------------------------------------------------------------------
;;; g_source_remove_by_funcs_user_data ()                  not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_remove_by_funcs_user_data"
          g-source-remove-by-funcs-user-data)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[funcs]{The @arg{source-funcs} passed to @fun{g-source-new}}
  @argument[user-data]{the user data for the callback}
  @return{@em{True} if a source was found and removed.}
  @begin{short}
    Removes a source from the default main loop context given the source
    functions and user data.
  @end{short}
  If multiple sources exist with the same source functions and user data, only
  one will be destroyed.
  @see-function{g-source-new}"
  (funcs (:pointer (:struct g-source-funcs)))
  (user-data :pointer))

;;; ----------------------------------------------------------------------------
;;; g_source_remove_by_user_data ()                        not exported
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_remove_by_user_data" g-source-remove-by-user-data) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-4-9}
  @argument[user-data]{the @arg{user-data} for the callback}
  @return{@em{True} if a source was found and removed.}
  @begin{short}
    Removes a source from the default main loop context given the user data for
    the callback.
  @end{short}
  If multiple sources exist with the same user data, only one will be
  destroyed."
  (user-data :pointer))

;;; ----------------------------------------------------------------------------
;;; GClearHandleFunc ()
;;;
;;; void
;;; (*GClearHandleFunc) (guint handle_id);
;;;
;;; Specifies the type of function passed to g_clear_handle_id(). The
;;; implementation is expected to free the resource identified by handle_id ;
;;; for instance, if handle_id is a GSource ID, g_source_remove() can be used.
;;;
;;; handle_id :
;;;     the handle ID to clear
;;;
;;; Since 2.56
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_clear_handle_id ()
;;;
;;; void
;;; g_clear_handle_id (guint *tag_ptr, GClearHandleFunc clear_func);
;;;
;;; Clears a numeric handler, such as a GSource ID.
;;;
;;; tag_ptr must be a valid pointer to the variable holding the handler.
;;;
;;; If the ID is zero then this function does nothing. Otherwise, clear_func()
;;; is called with the ID as a parameter, and the tag is set to zero.
;;;
;;; A macro is also included that allows this function to be used without
;;; pointer casts.
;;;
;;; tag_ptr :
;;;     a pointer to the handler ID.
;;;
;;; clear_func :
;;;     the function to call to clear the handler.
;;;
;;; Since 2.56
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.main-loop.lisp  ---------------------------------------
