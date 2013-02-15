;;; ----------------------------------------------------------------------------
;;; glib.main-loop.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; The Main Event Loop - manages all available sources of events
;;;     
;;; Synopsis
;;;
;;;     GMainLoop
;;;     GMainContext
;;;     GPollFD
;;;     GSource
;;;     GSourceFuncs
;;;     
;;;     g_main_loop_new
;;;     g_main_loop_ref
;;;     g_main_loop_unref
;;;     g_main_loop_run
;;;     g_main_loop_quit
;;;     g_main_loop_is_running
;;;     g_main_loop_get_context
;;;     g_main_new
;;;     g_main_destroy
;;;     g_main_run
;;;     g_main_quit
;;;     g_main_is_running
;;;     
;;;     G_PRIORITY_HIGH
;;;     G_PRIORITY_DEFAULT
;;;     G_PRIORITY_HIGH_IDLE
;;;     G_PRIORITY_DEFAULT_IDLE
;;;     G_PRIORITY_LOW
;;;     
;;;     G_SOURCE_CONTINUE
;;;     G_SOURCE_REMOVE
;;;     
;;;     g_main_context_new
;;;     g_main_context_ref
;;;     g_main_context_unref
;;;     g_main_context_default
;;;     g_main_context_iteration
;;;     g_main_iteration
;;;     g_main_context_pending
;;;     g_main_pending
;;;     g_main_context_find_source_by_id
;;;     g_main_context_find_source_by_user_data
;;;     g_main_context_find_source_by_funcs_user_data
;;;     g_main_context_wakeup
;;;     g_main_context_acquire
;;;     g_main_context_release
;;;     g_main_context_is_owner
;;;     g_main_context_wait
;;;     g_main_context_prepare
;;;     g_main_context_query
;;;     g_main_context_check
;;;     g_main_context_dispatch
;;;     g_main_context_set_poll_func
;;;     g_main_context_get_poll_func
;;;     g_main_context_add_poll
;;;     g_main_context_remove_poll
;;;     g_main_depth
;;;     g_main_current_source
;;;     g_main_set_poll_func
;;;     g_main_context_invoke
;;;     g_main_context_invoke_full
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
;;;     GPid
;;;     
;;;     g_child_watch_source_new
;;;     g_child_watch_add
;;;     g_child_watch_add_full
;;;     
;;;     g_poll
;;;     G_POLLFD_FORMAT
;;;     
;;;     GSourceCallbackFuncs
;;;     
;;;     g_source_new
;;;     g_source_ref
;;;     g_source_unref
;;;     g_source_set_funcs
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
;;;     g_source_set_callback_indirect
;;;     g_source_add_poll
;;;     g_source_remove_poll
;;;     g_source_add_child_source
;;;     g_source_remove_child_source
;;;     g_source_get_time
;;;     g_source_get_current_time
;;;     g_source_remove
;;;     g_source_remove_by_funcs_user_data
;;;     g_source_remove_by_user_data
;;; 
;;; Description
;;; 
;;; The main event loop manages all the available sources of events for GLib and
;;; GTK+ applications. These events can come from any number of different types
;;; of sources such as file descriptors (plain files, pipes or sockets) and
;;; timeouts. New types of event sources can also be added using
;;; g_source_attach().
;;; 
;;; To allow multiple independent sets of sources to be handled in different
;;; threads, each source is associated with a GMainContext. A GMainContext can
;;; only be running in a single thread, but sources can be added to it and
;;; removed from it from other threads.
;;; 
;;; Each event source is assigned a priority. The default priority,
;;; G_PRIORITY_DEFAULT, is 0. Values less than 0 denote higher priorities.
;;; Values greater than 0 denote lower priorities. Events from high priority
;;; sources are always processed before events from lower priority sources.
;;; 
;;; Idle functions can also be added, and assigned a priority. These will be run
;;; whenever no events with a higher priority are ready to be processed.
;;; 
;;; The GMainLoop data type represents a main event loop. A GMainLoop is created
;;; with g_main_loop_new(). After adding the initial event sources,
;;; g_main_loop_run() is called. This continuously checks for new events from
;;; each of the event sources and dispatches them. Finally, the processing of an
;;; event from one of the sources leads to a call to g_main_loop_quit() to exit
;;; the main loop, and g_main_loop_run() returns.
;;; 
;;; It is possible to create new instances of GMainLoop recursively. This is
;;; often used in GTK+ applications when showing modal dialog boxes. Note that
;;; event sources are associated with a particular GMainContext, and will be
;;; checked and dispatched for all main loops associated with that GMainContext.
;;; 
;;; GTK+ contains wrappers of some of these functions, e.g. gtk_main(),
;;; gtk_main_quit() and gtk_events_pending().
;;; 
;;; Creating new source types
;;; 
;;; One of the unusual features of the GMainLoop functionality is that new types
;;; of event source can be created and used in addition to the builtin type of
;;; event source. A new event source type is used for handling GDK events. A new
;;; source type is created by deriving from the GSource structure. The derived
;;; type of source is represented by a structure that has the GSource structure
;;; as a first element, and other elements specific to the new source type. To
;;; create an instance of the new source type, call g_source_new() passing in
;;; the size of the derived structure and a table of functions. These
;;; GSourceFuncs determine the behavior of the new source type.
;;; 
;;; New source types basically interact with the main context in two ways. Their
;;; prepare function in GSourceFuncs can set a timeout to determine the maximum
;;; amount of time that the main loop will sleep before checking the source
;;; again. In addition, or as well, the source can add file descriptors to the
;;; set that the main context checks using g_source_add_poll().
;;; 
;;; Customizing the main loop iteration
;;; 
;;; Single iterations of a GMainContext can be run with
;;; g_main_context_iteration(). In some cases, more detailed control of exactly
;;; how the details of the main loop work is desired, for instance, when
;;; integrating the GMainLoop with an external main loop. In such cases, you can
;;; call the component functions of g_main_context_iteration() directly. These
;;; functions are g_main_context_prepare(), g_main_context_query(),
;;; g_main_context_check() and g_main_context_dispatch().
;;; 
;;; On Unix, the GLib mainloop is incompatible with fork(). Any program using
;;; the mainloop must either exec() or exit() from the child without returning
;;; to the mainloop.
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; GMainLoop
;;; 
;;; typedef struct _GMainLoop GMainLoop;
;;; 
;;; The GMainLoop struct is an opaque data type representing the main event loop
;;; of a GLib or GTK+ application.
;;; ----------------------------------------------------------------------------

(defcstruct g-main-loop)

(export 'g-main-loop)

;;; ----------------------------------------------------------------------------
;;; GMainContext
;;; 
;;; typedef struct _GMainContext GMainContext;
;;; 
;;; The GMainContext struct is an opaque data type representing a set of sources
;;; to be handled in a main loop.
;;; ----------------------------------------------------------------------------

(defcstruct g-main-context)

(export 'g-main-context)

;;; ----------------------------------------------------------------------------
;;; struct GPollFD
;;; 
;;; struct GPollFD {
;;; #if defined (G_OS_WIN32) && GLIB_SIZEOF_VOID_P == 8
;;;   gint64 fd;
;;; #else
;;;   gint        fd;
;;; #endif
;;;   gushort     events;
;;;   gushort     revents;
;;; };
;;; 
;;; Represents a file descriptor, which events to poll for, and which events
;;; occurred.
;;; 
;;; gint64 fd;
;;;     the file descriptor to poll (or a HANDLE on Win32)
;;; 
;;; gint fd;
;;;     
;;; 
;;; gushort events;
;;;     a bitwise combination from GIOCondition, specifying which events should
;;;     be polled for. Typically for reading from a file descriptor you would
;;;     use G_IO_IN | G_IO_HUP | G_IO_ERR, and for writing you would use
;;;     G_IO_OUT | G_IO_ERR.
;;; 
;;; gushort revents;
;;;     a bitwise combination of flags from GIOCondition, returned from the
;;;     poll() function to indicate which events occurred.
;;; ----------------------------------------------------------------------------

(defcstruct g-poll-fd
  (fd :int) ; TODO: #if defined (G_OS_WIN32) && GLIB_SIZEOF_VOID_P == 8
  (events :ushort)
  (revent :ushort))

(export 'g-poll-fd)

;;; ----------------------------------------------------------------------------
;;; struct GSource
;;; 
;;; struct GSource {
;;; };
;;; 
;;; The GSource struct is an opaque data type representing an event source.
;;; ----------------------------------------------------------------------------

(defcstruct g-source)

(export 'g-source)

;;; ----------------------------------------------------------------------------
;;; struct GSourceFuncs
;;; 
;;; struct GSourceFuncs {
;;;   gboolean (*prepare)  (GSource    *source,
;;;                         gint       *timeout_);
;;;   gboolean (*check)    (GSource    *source);
;;;   gboolean (*dispatch) (GSource    *source,
;;;                         GSourceFunc callback,
;;;                         gpointer    user_data);
;;;   void     (*finalize) (GSource    *source); /* Can be NULL */
;;; };
;;; 
;;; The GSourceFuncs struct contains a table of functions used to handle event
;;; sources in a generic manner.
;;; 
;;; For idle sources, the prepare and check functions always return TRUE to
;;; indicate that the source is always ready to be processed. The prepare
;;; function also returns a timeout value of 0 to ensure that the poll() call
;;; doesn't block (since that would be time wasted which could have been spent
;;; running the idle function).
;;; 
;;; For timeout sources, the prepare and check functions both return TRUE if the
;;; timeout interval has expired. The prepare function also returns a timeout
;;; value to ensure that the poll() call doesn't block too long and miss the
;;; next timeout.
;;; 
;;; For file descriptor sources, the prepare function typically returns FALSE,
;;; since it must wait until poll() has been called before it knows whether any
;;; events need to be processed. It sets the returned timeout to -1 to indicate
;;; that it doesn't mind how long the poll() call blocks. In the check function,
;;; it tests the results of the poll() call to see if the required condition has
;;; been met, and returns TRUE if so.
;;; 
;;; prepare ()
;;;     Called before all the file descriptors are polled. If the source can
;;;     determine that it is ready here (without waiting for the results of the
;;;     poll() call) it should return TRUE. It can also return a timeout_ value
;;;     which should be the maximum timeout (in milliseconds) which should be
;;;     passed to the poll() call. The actual timeout used will be -1 if all
;;;     sources returned -1, or it will be the minimum of all the timeout_
;;;     values returned which were >= 0.
;;; 
;;; check ()
;;;     Called after all the file descriptors are polled. The source should
;;;     return TRUE if it is ready to be dispatched. Note that some time may
;;;     have passed since the previous prepare function was called, so the
;;;     source should be checked again here.
;;; 
;;; dispatch ()
;;;     Called to dispatch the event source, after it has returned TRUE in
;;;     either its prepare or its check function. The dispatch function is
;;;     passed in a callback function and data. The callback function may be
;;;     NULL if the source was never connected to a callback using
;;;     g_source_set_callback(). The dispatch function should call the callback
;;;     function with user_data and whatever additional parameters are needed
;;;     for this type of event source.
;;; 
;;; finalize ()
;;;     Called when the source is finalized.
;;; ----------------------------------------------------------------------------

(defcstruct g-source-funcs
  (prepare :pointer)
  (check :pointer)
  (dispatch :pointer)
  (finalize :pointer)
  (closure-callback :pointer)
  (closure-marshal :pointer))

(export 'g-source-funcs)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_new" g-main-loop-new) (:pointer g-main-loop)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-25}
  @argument[context]{a @type{g-main-context} (if a @code{null}-pointer, the
    default context will be used)}
  @argument[is-running]{set to @code{t} to indicate that the loop is running.
    This is not very important since calling @fun{g-main-loop-run} will set this
    to @code{t} anyway.}
  @return{A new @type{g-main-loop}.}
  @short{Creates a new @type{g-main-loop} structure.}
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
  @see-function{g-main-loop-run}"
  (context (:pointer g-main-context))
  (is-running :boolean))

(export 'g-main-loop-new)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_ref" g-main-loop-ref) (:pointer g-main-loop)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-25}
  @argument[loop]{a @type{g-main-loop}}
  @return{The argument @arg{loop}.}
  @short{Increases the reference count on a @type{g-main-loop} object by one.}
  @see-type{g-main-loop}"
  (loop (:pointer g-main-loop)))

(export 'g-main-loop-ref)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_unref" g-main-loop-unref) (:pointer g-main-loop)
 #+cl-cffi-gtk-documentation
 "@version{2012-12-25}
  @argument[loop]{a @type{g-main-loop}}
  @short{Decreases the reference count on a @type{g-main-loop} object by one.}
  If the result is zero, free the @arg{loop} and free all associated memory.
  @see-type{g-main-loop}"
  (loop (:pointer g-main-loop)))

(export 'g-main-loop-unref)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_run ()
;;; 
;;; void g_main_loop_run (GMainLoop *loop);
;;; 
;;; Runs a main loop until g_main_loop_quit() is called on the loop. If this is
;;; called for the thread of the loop's GMainContext, it will process events
;;; from the loop, otherwise it will simply wait.
;;; 
;;; loop :
;;;     a GMainLoop
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_run" g-main-loop-run) :void
 #+cl-cffi-gtk-documentation
 "@version{2012-12-25}
  @argument[loop]{a @type{g-main-loop}}
  @begin{short}
    Runs a main @arg{loop} until @fun{g-main-loop-quit} is called on the
    @arg{loop}.
  @end{short}
  If this is called for the thread of the @arg{loop}'s @type{g-main-context}, it
  will process events from the @arg{loop}, otherwise it will simply wait."
  (loop (:pointer g-main-loop)))

(export 'g-main-loop-run)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_quit ()
;;; 
;;; void g_main_loop_quit (GMainLoop *loop);
;;; 
;;; Stops a GMainLoop from running. Any calls to g_main_loop_run() for the loop
;;; will return.
;;; 
;;; Note that sources that have already been dispatched when g_main_loop_quit()
;;; is called will still be executed.
;;; 
;;; loop :
;;;     a GMainLoop
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_quit" g-main-loop-quit) :void
  (loop (:pointer g-main-loop)))

(export 'g-main-loop-quit)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_is_running ()
;;; 
;;; gboolean g_main_loop_is_running (GMainLoop *loop);
;;; 
;;; Checks to see if the main loop is currently being run via g_main_loop_run().
;;; 
;;; loop :
;;;     a GMainLoop.
;;; 
;;; Returns :
;;;     TRUE if the mainloop is currently being run.
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_is_running" g-main-loop-is-running) :boolean
  (loop (:pointer g-main-loop)))

(export 'g-main-loop-is-running)

;;; ----------------------------------------------------------------------------
;;; g_main_loop_get_context ()
;;; 
;;; GMainContext * g_main_loop_get_context (GMainLoop *loop);
;;; 
;;; Returns the GMainContext of loop.
;;; 
;;; loop :
;;;     a GMainLoop.
;;; 
;;; Returns :
;;;     the GMainContext of loop.
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_loop_get_context" g-main-loop-get-context)
    (:pointer g-main-context)
  (loop (:pointer g-main-loop)))

(export 'g-main-loop-get-context)

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
;;; G_PRIORITY_HIGH
;;; 
;;; #define G_PRIORITY_HIGH -100
;;; 
;;; Use this for high priority event sources.
;;; 
;;; It is not used within GLib or GTK+.
;;; ----------------------------------------------------------------------------

(defconstant g-priority-high -100)

(export 'g-priority-high)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_DEFAULT
;;; 
;;; #define G_PRIORITY_DEFAULT 0
;;; 
;;; Use this for default priority event sources.
;;; 
;;; In GLib this priority is used when adding timeout functions with
;;; g_timeout_add(). In GDK this priority is used for events from the X server.
;;; ----------------------------------------------------------------------------

(defconstant g-priority-default 0)

(export 'g-priority-default)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_HIGH_IDLE
;;; 
;;; #define G_PRIORITY_HIGH_IDLE 100
;;; 
;;; Use this for high priority idle functions.
;;; 
;;; GTK+ uses G_PRIORITY_HIGH_IDLE + 10 for resizing operations, and
;;; G_PRIORITY_HIGH_IDLE + 20 for redrawing operations. (This is done to ensure
;;; that any pending resizes are processed before any pending redraws, so that
;;; widgets are not redrawn twice unnecessarily.)
;;; ----------------------------------------------------------------------------

(defconstant g-priority-high-idle 100)

(export 'g-priority-high-idle)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_DEFAULT_IDLE
;;; 
;;; #define G_PRIORITY_DEFAULT_IDLE 200
;;; 
;;; Use this for default priority idle functions.
;;; 
;;; In GLib this priority is used when adding idle functions with g_idle_add().
;;; ----------------------------------------------------------------------------

(defconstant g-priority-default-idle 200)

(export 'g-priority-default-idle)

;;; ----------------------------------------------------------------------------
;;; G_PRIORITY_LOW
;;; 
;;; #define G_PRIORITY_LOW 300
;;; 
;;; Use this for very low priority background tasks.
;;; 
;;; It is not used within GLib or GTK+.
;;; ----------------------------------------------------------------------------

(defconstant g-priority-low 300)

(export 'g-priority-low)

;;; ----------------------------------------------------------------------------
;;; G_SOURCE_CONTINUE
;;; 
;;; #define G_SOURCE_CONTINUE TRUE
;;; 
;;; Use this macro as the return value of a GSourceFunc to leave the GSource in
;;; the main loop.
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_SOURCE_REMOVE
;;; 
;;; #define G_SOURCE_REMOVE FALSE
;;; 
;;; Use this macro as the return value of a GSourceFunc to remove the GSource
;;; from the main loop.
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_new ()
;;; 
;;; GMainContext * g_main_context_new (void);
;;; 
;;; Creates a new GMainContext structure.
;;; 
;;; Returns :
;;;     the new GMainContext
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_new" g-main-context-new) (:pointer g-main-context))

(export 'g-main-context-new)

;;; ----------------------------------------------------------------------------
;;; g_main_context_ref ()
;;; 
;;; GMainContext * g_main_context_ref (GMainContext *context);
;;; 
;;; Increases the reference count on a GMainContext object by one.
;;; 
;;; context :
;;;     a GMainContext
;;; 
;;; Returns :
;;;     the context that was passed in (since 2.6)
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_ref" g-main-context-ref) (:pointer g-main-context)
  (context (:pointer g-main-context)))

(export 'g-main-context-ref)

;;; ----------------------------------------------------------------------------
;;; g_main_context_unref ()
;;; 
;;; void g_main_context_unref (GMainContext *context);
;;; 
;;; Decreases the reference count on a GMainContext object by one. If the result
;;; is zero, free the context and free all associated memory.
;;; 
;;; context :
;;;     a GMainContext
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_unref" g-main-context-unref) (:pointer g-main-context)
  (context (:pointer g-main-context)))

(export 'g-main-context-unref)

;;; ----------------------------------------------------------------------------
;;; g_main_context_default ()
;;; 
;;; GMainContext * g_main_context_default (void);
;;; 
;;; Returns the global default main context. This is the main context used for
;;; main loop functions when a main loop is not explicitly specified, and
;;; corresponds to the "main" main loop. See also
;;; g_main_context_get_thread_default().
;;; 
;;; Returns :
;;;     the global default main context
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_default" g-main-context-default)
    (:pointer g-main-context))

(export 'g-main-context-default)

;;; ----------------------------------------------------------------------------
;;; g_main_context_iteration ()
;;; 
;;; gboolean g_main_context_iteration (GMainContext *context,
;;;                                    gboolean may_block);
;;; 
;;; Runs a single iteration for the given main loop. This involves checking to
;;; see if any event sources are ready to be processed, then if no events
;;; sources are ready and may_block is TRUE, waiting for a source to become
;;; ready, then dispatching the highest priority events sources that are ready.
;;; Otherwise, if may_block is FALSE sources are not waited to become ready,
;;; only those highest priority events sources will be dispatched (if any), that
;;; are ready at this given moment without further waiting.
;;; 
;;; Note that even when may_block is TRUE, it is still possible for
;;; g_main_context_iteration() to return FALSE, since the the wait may be
;;; interrupted for other reasons than an event source becoming ready.
;;; 
;;; context :
;;;     a GMainContext (if NULL, the default context will be used)
;;; 
;;; may_block :
;;;     whether the call may block.
;;; 
;;; Returns :
;;;     TRUE if events were dispatched.
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_iteration" g-main-context-iteration) :boolean
  (context (:pointer g-main-context))
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
;;; 
;;; gboolean g_main_context_pending (GMainContext *context);
;;; 
;;; Checks if any sources have pending events for the given context.
;;; 
;;; context :
;;;     a GMainContext (if NULL, the default context will be used)
;;; 
;;; Returns :
;;;     TRUE if events are pending.
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_pending" g-main-context-pending) :boolean
  (context (:pointer g-main-context)))

(export 'g-main-context-pending)

;;; ----------------------------------------------------------------------------
;;; g_main_pending
;;; 
;;; #define g_main_pending()
;;; 
;;; Checks if any events are pending for the default GMainContext (i.e. ready to
;;; be processed).
;;; 
;;; Returns :
;;;     TRUE if any events are pending.
;;;
;;; Deprected: Since 2.2: Use g_main_context_pending() instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_find_source_by_id ()
;;; 
;;; GSource * g_main_context_find_source_by_id (GMainContext *context,
;;;                                             guint source_id);
;;; 
;;; Finds a GSource given a pair of context and ID.
;;; 
;;; context :
;;;     a GMainContext (if NULL, the default context will be used)
;;; 
;;; source_id :
;;;     the source ID, as returned by g_source_get_id().
;;; 
;;; Returns :
;;;     the GSource if found, otherwise, NULL
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_find_source_by_id" g-main-context-find-source-by-id)
    (:pointer g-source)
  (context (:pointer g-main-context))
  (source-id :uint))

(export 'g-main-context-find-source-by-id)

;;; ----------------------------------------------------------------------------
;;; g_main_context_find_source_by_user_data ()
;;; 
;;; GSource * g_main_context_find_source_by_user_data (GMainContext *context,
;;;                                                    gpointer user_data);
;;; 
;;; Finds a source with the given user data for the callback. If multiple
;;; sources exist with the same user data, the first one found will be returned.
;;; 
;;; context :
;;;     a GMainContext
;;; 
;;; user_data :
;;;     the user_data for the callback.
;;; 
;;; Returns :
;;;     the source, if one was found, otherwise NULL
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_find_source_by_user_data"
          g-main-context-find-source-by-user-data)
    (:pointer g-source)
  (context (:pointer g-main-context))
  (user-data :pointer))

(export 'g-main-context-find-source-by-user-data)

;;; ----------------------------------------------------------------------------
;;; g_main_context_find_source_by_funcs_user_data ()
;;; 
;;; GSource * g_main_context_find_source_by_funcs_user_data
;;;                                                      (GMainContext *context,
;;;                                                       GSourceFuncs *funcs,
;;;                                                       gpointer user_data);
;;; 
;;; Finds a source with the given source functions and user data. If multiple
;;; sources exist with the same source function and user data, the first one
;;; found will be returned.
;;; 
;;; context :
;;;     a GMainContext (if NULL, the default context will be used)
;;; 
;;; funcs :
;;;     the source_funcs passed to g_source_new().
;;; 
;;; user_data :
;;;     the user data from the callback.
;;; 
;;; Returns :
;;;     the source, if one was found, otherwise NULL
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_find_source_by_funcs_user_data"
          g-main-context-find-source-by-funcs-user-data)
    (:pointer g-source)
  (context (:pointer g-main-context))
  (funcs (:pointer g-source-funcs))
  (user-data :pointer))

(export 'g-main-context-find-source-by-funcs-user-data)

;;; ----------------------------------------------------------------------------
;;; g_main_context_wakeup ()
;;; 
;;; void g_main_context_wakeup (GMainContext *context);
;;; 
;;; If context is currently waiting in a poll(), interrupt the poll(), and
;;; continue the iteration process.
;;; 
;;; context :
;;;     a GMainContext
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_wakeup" g-main-context-wakeup) :void
  (context (:pointer g-main-context)))

(export 'g-main-context-wakeup)

;;; ----------------------------------------------------------------------------
;;; g_main_context_acquire ()
;;; 
;;; gboolean g_main_context_acquire (GMainContext *context);
;;; 
;;; Tries to become the owner of the specified context. If some other thread is
;;; the owner of the context, returns FALSE immediately. Ownership is properly
;;; recursive: the owner can require ownership again and will release ownership
;;; when g_main_context_release() is called as many times as
;;; g_main_context_acquire().
;;; 
;;; You must be the owner of a context before you can call
;;; g_main_context_prepare(), g_main_context_query(), g_main_context_check(),
;;; g_main_context_dispatch().
;;; 
;;; context :
;;;     a GMainContext
;;; 
;;; Returns :
;;;     TRUE if the operation succeeded, and this thread is now the owner of
;;;     context.
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_acquire" g-main-context-acquire) :boolean
  (context (:pointer g-main-context)))

(export 'g-main-context-acquire)

;;; ----------------------------------------------------------------------------
;;; g_main_context_release ()
;;; 
;;; void g_main_context_release (GMainContext *context);
;;; 
;;; Releases ownership of a context previously acquired by this thread with
;;; g_main_context_acquire(). If the context was acquired multiple times, the
;;; ownership will be released only when g_main_context_release() is called as
;;; many times as it was acquired.
;;; 
;;; context :
;;;     a GMainContext
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_release" g-main-context-release) :void
  (context (:pointer g-main-context)))

(export 'g-main-context-release)

;;; ----------------------------------------------------------------------------
;;; g_main_context_is_owner ()
;;; 
;;; gboolean g_main_context_is_owner (GMainContext *context);
;;; 
;;; Determines whether this thread holds the (recursive) ownership of this
;;; GMainContext. This is useful to know before waiting on another thread that
;;; may be blocking to get ownership of context.
;;; 
;;; context :
;;;     a GMainContext
;;; 
;;; Returns :
;;;     TRUE if current thread is owner of context.
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_is_owner" g-main-context-is-owner) :boolean
  (context (:pointer g-main-context)))

(export 'g-main-context-is-owner)

;;; ----------------------------------------------------------------------------
;;; g_main_context_wait ()
;;; 
;;; gboolean g_main_context_wait (GMainContext *context,
;;;                               GCond *cond,
;;;                               GMutex *mutex);
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
;;;     context.
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_wait" g-main-context-wait) :boolean
  (context (:pointer g-main-context))
  (cond (:pointer g-cond))
  (mutex (:pointer g-mutex)))

(export 'g-main-context-wait)

;;; ----------------------------------------------------------------------------
;;; g_main_context_prepare ()
;;; 
;;; gboolean g_main_context_prepare (GMainContext *context, gint *priority);
;;; 
;;; Prepares to poll sources within a main loop. The resulting information for
;;; polling is determined by calling g_main_context_query().
;;; 
;;; context :
;;;     a GMainContext
;;; 
;;; priority :
;;;     location to store priority of highest priority source already ready.
;;; 
;;; Returns :
;;;     TRUE if some source is ready to be dispatched prior to polling.
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_prepare" g-main-context-prepare) :boolean
  (context (:pointer g-main-context))
  (priority-ret (:pointer :int)))

(export 'g-main-context-prepare)

;;; ----------------------------------------------------------------------------
;;; g_main_context_query ()
;;; 
;;; gint g_main_context_query (GMainContext *context,
;;;                            gint max_priority,
;;;                            gint *timeout_,
;;;                            GPollFD *fds,
;;;                            gint n_fds);
;;; 
;;; Determines information necessary to poll this main loop.
;;; 
;;; context :
;;;     a GMainContext
;;; 
;;; max_priority :
;;;     maximum priority source to check
;;; 
;;; timeout_ :
;;;     location to store timeout to be used in polling
;;; 
;;; fds :
;;;     location to store GPollFD records that need to be polled
;;; 
;;; n_fds :
;;;     length of fds.
;;; 
;;; Returns :
;;;     the number of records actually stored in fds, or, if more than n_fds
;;;     records need to be stored, the number of records that need to be stored.
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_query" g-main-context-query) :int
  (context (:pointer g-main-context))
  (max-priority :int)
  (timeout (:pointer :int))
  (fds (:pointer g-poll-fd))
  (n-dfs :int))

(export 'g-main-context-query)

;;; ----------------------------------------------------------------------------
;;; g_main_context_check ()
;;; 
;;; gint g_main_context_check (GMainContext *context,
;;;                            gint max_priority,
;;;                            GPollFD *fds,
;;;                            gint n_fds);
;;; 
;;; Passes the results of polling back to the main loop.
;;; 
;;; context :
;;;     a GMainContext
;;; 
;;; max_priority :
;;;     the maximum numerical priority of sources to check
;;; 
;;; fds :
;;;     array of GPollFD's that was passed to the last call to
;;;     g_main_context_query()
;;; 
;;; n_fds :
;;;     return value of g_main_context_query()
;;; 
;;; Returns :
;;;     TRUE if some sources are ready to be dispatched.
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_check" g-main-context-check) :int
  (context (:pointer g-main-context))
  (max-priority :int)
  (fds (:pointer g-poll-fd))
  (n-fds :int))

(export 'g-main-context-check)

;;; ----------------------------------------------------------------------------
;;; g_main_context_dispatch ()
;;; 
;;; void g_main_context_dispatch (GMainContext *context);
;;; 
;;; Dispatches all pending sources.
;;; 
;;; context :
;;;     a GMainContext
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_dispatch" g-main-context-dispatch) :void
  (context (:pointer g-main-context)))

(export 'g-main-context-dispatch)

;;; ----------------------------------------------------------------------------
;;; g_main_context_set_poll_func ()
;;; 
;;; void g_main_context_set_poll_func (GMainContext *context, GPollFunc func);
;;; 
;;; Sets the function to use to handle polling of file descriptors. It will be
;;; used instead of the poll() system call (or GLib's replacement function,
;;; which is used where poll() isn't available).
;;; 
;;; This function could possibly be used to integrate the GLib event loop with
;;; an external event loop.
;;; 
;;; context :
;;;     a GMainContext
;;; 
;;; func :
;;;     the function to call to poll all file descriptors
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_set_poll_func" g-main-context-set-poll-func) :void
  (context (:pointer g-main-context))
  (func :pointer))

(export 'g-main-context-set-poll-func)

;;; ----------------------------------------------------------------------------
;;; g_main_context_get_poll_func ()
;;; 
;;; GPollFunc g_main_context_get_poll_func (GMainContext *context);
;;; 
;;; Gets the poll function set by g_main_context_set_poll_func().
;;; 
;;; context :
;;;     a GMainContext
;;; 
;;; Returns :
;;;     the poll function
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_get_poll_func" g-main-context-get-poll-func) :pointer
  (context (:pointer g-main-context)))

(export 'g-main-context-get-poll-func)

;;; ----------------------------------------------------------------------------
;;; GPollFunc ()
;;; 
;;; gint (*GPollFunc) (GPollFD *ufds, guint nfsd, gint timeout_);
;;; 
;;; Specifies the type of function passed to g_main_context_set_poll_func(). The
;;; semantics of the function should match those of the poll() system call.
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
;;; g_main_context_add_poll ()
;;; 
;;; void g_main_context_add_poll (GMainContext *context,
;;;                               GPollFD *fd,
;;;                               gint priority);
;;; 
;;; Adds a file descriptor to the set of file descriptors polled for this
;;; context. This will very seldom be used directly. Instead a typical event
;;; source will use g_source_add_poll() instead.
;;; 
;;; context :
;;;     a GMainContext (or NULL for the default context)
;;; 
;;; fd :
;;;     a GPollFD structure holding information about a file descriptor to
;;;     watch.
;;; 
;;; priority :
;;;     the priority for this file descriptor which should be the same as the
;;;     priority used for g_source_attach() to ensure that the file descriptor
;;;     is polled whenever the results may be needed.
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_add_poll" g-main-context-add-poll) :void
  (context (:pointer g-main-context))
  (fd (:pointer g-poll-fd))
  (priority :int))

(export 'g-main-context-add-poll)

;;; ----------------------------------------------------------------------------
;;; g_main_context_remove_poll ()
;;; 
;;; void g_main_context_remove_poll (GMainContext *context, GPollFD *fd);
;;; 
;;; Removes file descriptor from the set of file descriptors to be polled for a
;;; particular context.
;;; 
;;; context :
;;;     a GMainContext
;;; 
;;; fd :
;;;     a GPollFD descriptor previously added with g_main_context_add_poll()
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_context_remove_poll" g-main-context-remove-poll) :void
  (context (:pointer g-main-context))
  (fd (:pointer g-poll-fd)))

(export 'g-main-context-remove-poll)

;;; ----------------------------------------------------------------------------
;;; g_main_depth ()
;;; 
;;; gint g_main_depth (void);
;;; 
;;; Returns the depth of the stack of calls to g_main_context_dispatch() on any
;;; GMainContext in the current thread. That is, when called from the toplevel,
;;; it gives 0. When called from within a callback from
;;; g_main_context_iteration() (or g_main_loop_run(), etc.) it returns 1. When
;;; called from within a callback to a recursive call to
;;; g_main_context_iteration(), it returns 2. And so forth.
;;; 
;;; This function is useful in a situation like the following: Imagine an
;;; extremely simple "garbage collected" system.
;;; 
;;;   static GList *free_list;
;;;   
;;;   gpointer
;;;   allocate_memory (gsize size)
;;;   { 
;;;     gpointer result = g_malloc (size);
;;;     free_list = g_list_prepend (free_list, result);
;;;     return result;
;;;   }
;;;   
;;;   void
;;;   free_allocated_memory (void)
;;;   {
;;;     GList *l;
;;;     for (l = free_list; l; l = l->next);
;;;       g_free (l->data);
;;;     g_list_free (free_list);
;;;     free_list = NULL;
;;;    }
;;;   
;;;   [...]
;;;   
;;;   while (TRUE); 
;;;    {
;;;      g_main_context_iteration (NULL, TRUE);
;;;      free_allocated_memory();
;;;     }
;;; 
;;; This works from an application, however, if you want to do the same thing
;;; from a library, it gets more difficult, since you no longer control the main
;;; loop. You might think you can simply use an idle function to make the call
;;; to free_allocated_memory(), but that doesn't work, since the idle function
;;; could be called from a recursive callback. This can be fixed by using
;;; g_main_depth()
;;; 
;;;   gpointer
;;;   allocate_memory (gsize size)
;;;   { 
;;;     FreeListBlock *block = g_new (FreeListBlock, 1);
;;;     block->mem = g_malloc (size);
;;;     block->depth = g_main_depth ();   
;;;     free_list = g_list_prepend (free_list, block);
;;;     return block->mem;
;;;   }
;;;   
;;;   void
;;;   free_allocated_memory (void)
;;;   {
;;;     GList *l;
;;;     
;;;     int depth = g_main_depth ();
;;;     for (l = free_list; l; );
;;;       {
;;;         GList *next = l->next;
;;;         FreeListBlock *block = l->data;
;;;         if (block->depth > depth)
;;;           {
;;;             g_free (block->mem);
;;;             g_free (block);
;;;             free_list = g_list_delete_link (free_list, l);
;;;           }
;;;                 
;;;         l = next;
;;;       }
;;;     }
;;; 
;;; There is a temptation to use g_main_depth() to solve problems with
;;; reentrancy. For instance, while waiting for data to be received from the
;;; network in response to a menu item, the menu item might be selected again.
;;; It might seem that one could make the menu item's callback return
;;; immediately and do nothing if g_main_depth() returns a value greater than 1.
;;; However, this should be avoided since the user then sees selecting the menu
;;; item do nothing. Furthermore, you'll find yourself adding these checks all
;;; over your code, since there are doubtless many, many things that the user
;;; could do. Instead, you can use the following techniques:
;;; 
;;;     Use gtk_widget_set_sensitive() or modal dialogs to prevent the user from
;;;     interacting with elements while the main loop is recursing.
;;; 
;;;     Avoid main loop recursion in situations where you can't handle arbitrary
;;;     callbacks. Instead, structure your code so that you simply return to the
;;;     main loop and then get called again when there is more work to do.
;;; 
;;; Returns :
;;;     The main loop recursion level in the current thread
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_depth" g-main-depth) :int)

(export 'g-main-depth)

;;; ----------------------------------------------------------------------------
;;; g_main_current_source ()
;;; 
;;; GSource * g_main_current_source (void);
;;; 
;;; Returns the currently firing source for this thread.
;;; 
;;; Returns :
;;;     The currently firing source or NULL.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("g_main_current_source" g-main-current-source) (:pointer g-source))

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
;;; g_main_context_get_thread_default ()
;;; 
;;; GMainContext * g_main_context_get_thread_default (void);
;;; 
;;; Gets the thread-default GMainContext for this thread. Asynchronous
;;; operations that want to be able to be run in contexts other than the default
;;; one should call this method or g_main_context_ref_thread_default() to get a
;;; GMainContext to add their GSources to. (Note that even in single-threaded
;;; programs applications may sometimes want to temporarily push a non-default
;;; context, so it is not safe to assume that this will always return NULL if
;;; you are running in the default thread.)
;;; 
;;; If you need to hold a reference on the context, use
;;; g_main_context_ref_thread_default() instead.
;;; 
;;; Returns :
;;;     the thread-default GMainContext, or NULL if the thread-default context
;;;     is the global default context
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_main_context_ref_thread_default ()
;;; 
;;; GMainContext * g_main_context_ref_thread_default (void);
;;; 
;;; Gets the thread-default GMainContext for this thread, as with
;;; g_main_context_get_thread_default(), but also adds a reference to it with
;;; g_main_context_ref(). In addition, unlike
;;; g_main_context_get_thread_default(), if the thread-default context is the
;;; global default context, this will return that GMainContext (with a ref added
;;; to it) rather than returning NULL.
;;; 
;;; Returns :
;;;     the thread-default GMainContext. Unref with g_main_context_unref() when
;;;     you are done with it
;;; 
;;; Since 2.32
;;; ----------------------------------------------------------------------------

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
;;; 
;;; GSource * g_timeout_source_new (guint interval);
;;; 
;;; Creates a new timeout source.
;;; 
;;; The source will not initially be associated with any GMainContext and must
;;; be added to one with g_source_attach() before it will be executed.
;;; 
;;; The interval given is in terms of monotonic time, not wall clock time. See
;;; g_get_monotonic_time().
;;; 
;;; interval :
;;;     the timeout interval in milliseconds.
;;; 
;;; Returns :
;;;     the newly-created timeout source
;;; ----------------------------------------------------------------------------

(defcfun ("g_timeout_source_new" g-timeout-source-new) (:pointer g-source)
  (interval-milliseconds :int))

(export 'g-timeout-source-new)

;;; ----------------------------------------------------------------------------
;;; g_timeout_source_new_seconds ()
;;; 
;;; GSource * g_timeout_source_new_seconds (guint interval);
;;; 
;;; Creates a new timeout source.
;;; 
;;; The source will not initially be associated with any GMainContext and must
;;; be added to one with g_source_attach() before it will be executed.
;;; 
;;; The scheduling granularity/accuracy of this timeout source will be in
;;; seconds.
;;; 
;;; The interval given in terms of monotonic time, not wall clock time. See
;;; g_get_monotonic_time().
;;; 
;;; interval :
;;;     the timeout interval in seconds
;;; 
;;; Returns :
;;;     the newly-created timeout source
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("g_timeout_source_new_seconds" g-timeout-source-new-seconds)
    (:pointer g-source)
  (interval-seconds :int))

(export 'g-timeout-source-new-seconds)

;;; ----------------------------------------------------------------------------
;;; g_timeout_add ()
;;; 
;;; guint g_timeout_add (guint interval,
;;;                      GSourceFunc function,
;;;                      gpointer data);
;;; 
;;; Sets a function to be called at regular intervals, with the default
;;; priority, G_PRIORITY_DEFAULT. The function is called repeatedly until it
;;; returns FALSE, at which point the timeout is automatically destroyed and the
;;; function will not be called again. The first call to the function will be at
;;; the end of the first interval.
;;; 
;;; Note that timeout functions may be delayed, due to the processing of other
;;; event sources. Thus they should not be relied on for precise timing. After
;;; each call to the timeout function, the time of the next timeout is
;;; recalculated based on the current time and the given interval (it does not
;;; try to 'catch up' time lost in delays).
;;; 
;;; If you want to have a timer in the "seconds" range and do not care about the
;;; exact time of the first call of the timer, use the g_timeout_add_seconds()
;;; function; this function allows for more optimizations and more efficient
;;; system power usage.
;;; 
;;; This internally creates a main loop source using g_timeout_source_new() and
;;; attaches it to the main loop context using g_source_attach(). You can do
;;; these steps manually if you need greater control.
;;; 
;;; The interval given is in terms of monotonic time, not wall clock time. See
;;; g_get_monotonic_time().
;;; 
;;; interval :
;;;     the time between calls to the function, in milliseconds (1/1000ths of a
;;;     second)
;;; 
;;; function :
;;;     function to call
;;; 
;;; data :
;;;     data to pass to function
;;; 
;;; Returns :
;;;     the ID (greater than 0) of the event source.
;;; ----------------------------------------------------------------------------

(defcallback call-timeout-from-main-loop-cb :boolean ((data :pointer))
  (restart-case
      (progn (funcall (glib::get-stable-pointer-value data)))
    (return-from-callback () nil)))

(defun g-timeout-add (interval func &key (priority g-priority-default))
  (g-timeout-add-full priority
                      interval
                      (callback call-timeout-from-main-loop-cb)
                      (glib::allocate-stable-pointer func)
                      (callback glib::stable-pointer-destroy-notify-cb)))

(export 'g-timeout-add)

;;; ----------------------------------------------------------------------------
;;; g_timeout_add_full ()
;;; 
;;; guint g_timeout_add_full (gint priority,
;;;                           guint interval,
;;;                           GSourceFunc function,
;;;                           gpointer data,
;;;                           GDestroyNotify notify);
;;; 
;;; Sets a function to be called at regular intervals, with the given priority.
;;; The function is called repeatedly until it returns FALSE, at which point the
;;; timeout is automatically destroyed and the function will not be called
;;; again. The notify function is called when the timeout is destroyed. The
;;; first call to the function will be at the end of the first interval.
;;; 
;;; Note that timeout functions may be delayed, due to the processing of other
;;; event sources. Thus they should not be relied on for precise timing. After
;;; each call to the timeout function, the time of the next timeout is
;;; recalculated based on the current time and the given interval (it does not
;;; try to 'catch up' time lost in delays).
;;; 
;;; This internally creates a main loop source using g_timeout_source_new() and
;;; attaches it to the main loop context using g_source_attach(). You can do
;;; these steps manually if you need greater control.
;;; 
;;; The interval given in terms of monotonic time, not wall clock time. See
;;; g_get_monotonic_time().
;;; 
;;; priority :
;;;     the priority of the timeout source. Typically this will be in the range
;;;     between G_PRIORITY_DEFAULT and G_PRIORITY_HIGH.
;;; 
;;; interval :
;;;     the time between calls to the function, in milliseconds (1/1000ths of a
;;;     second)
;;; 
;;; function :
;;;     function to call
;;; 
;;; data :
;;;     data to pass to function
;;; 
;;; notify :
;;;     function to call when the timeout is removed, or NULL
;;; 
;;; Returns :
;;;     the ID (greater than 0) of the event source. Rename to: g_timeout_add
;;; ----------------------------------------------------------------------------

(defcfun ("g_timeout_add_full" g-timeout-add-full) :uint
  (priority :int)
  (interval-milliseconds :uint)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(export 'g-timeout-add-full)

;;; ----------------------------------------------------------------------------
;;; g_timeout_add_seconds ()
;;; 
;;; guint g_timeout_add_seconds (guint interval,
;;;                              GSourceFunc function,
;;;                              gpointer data);
;;; 
;;; Sets a function to be called at regular intervals with the default priority,
;;; G_PRIORITY_DEFAULT. The function is called repeatedly until it returns
;;; FALSE, at which point the timeout is automatically destroyed and the
;;; function will not be called again.
;;; 
;;; This internally creates a main loop source using
;;; g_timeout_source_new_seconds() and attaches it to the main loop context
;;; using g_source_attach(). You can do these steps manually if you need greater
;;; control. Also see g_timeout_add_seconds_full().
;;; 
;;; Note that the first call of the timer may not be precise for timeouts of one
;;; second. If you need finer precision and have such a timeout, you may want to
;;; use g_timeout_add() instead.
;;; 
;;; The interval given is in terms of monotonic time, not wall clock time. See
;;; g_get_monotonic_time().
;;; 
;;; interval :
;;;     the time between calls to the function, in seconds
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
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defun g-timeout-add-seconds (interval func
                                       &key (priority g-priority-default))
  (g-timeout-add-seconds-full priority
                              interval
                              (callback call-timeout-from-main-loop-cb)
                              (glib::allocate-stable-pointer func)
                              (callback glib::stable-pointer-destroy-notify-cb)))

(export 'g-timeout-add-seconds)

;;; ----------------------------------------------------------------------------
;;; g_timeout_add_seconds_full ()
;;; 
;;; guint g_timeout_add_seconds_full (gint priority,
;;;                                   guint interval,
;;;                                   GSourceFunc function,
;;;                                   gpointer data,
;;;                                   GDestroyNotify notify);
;;; 
;;; Sets a function to be called at regular intervals, with priority. The
;;; function is called repeatedly until it returns FALSE, at which point the
;;; timeout is automatically destroyed and the function will not be called
;;; again.
;;; 
;;; Unlike g_timeout_add(), this function operates at whole second granularity.
;;; The initial starting point of the timer is determined by the implementation
;;; and the implementation is expected to group multiple timers together so that
;;; they fire all at the same time. To allow this grouping, the interval to the
;;; first timer is rounded and can deviate up to one second from the specified
;;; interval. Subsequent timer iterations will generally run at the specified
;;; interval.
;;; 
;;; Note that timeout functions may be delayed, due to the processing of other
;;; event sources. Thus they should not be relied on for precise timing. After
;;; each call to the timeout function, the time of the next timeout is
;;; recalculated based on the current time and the given interval
;;; 
;;; If you want timing more precise than whole seconds, use g_timeout_add()
;;; instead.
;;; 
;;; The grouping of timers to fire at the same time results in a more power and
;;; CPU efficient behavior so if your timer is in multiples of seconds and you
;;; don't require the first timer exactly one second from now, the use of
;;; g_timeout_add_seconds() is preferred over g_timeout_add().
;;; 
;;; This internally creates a main loop source using
;;; g_timeout_source_new_seconds() and attaches it to the main loop context
;;; using g_source_attach(). You can do these steps manually if you need greater
;;; control.
;;; 
;;; The interval given is in terms of monotonic time, not wall clock time. See
;;; g_get_monotonic_time().
;;; 
;;; priority :
;;;     the priority of the timeout source. Typically this will be in the range
;;;     between G_PRIORITY_DEFAULT and G_PRIORITY_HIGH.
;;; 
;;; interval :
;;;     the time between calls to the function, in seconds
;;; 
;;; function :
;;;     function to call
;;; 
;;; data :
;;;     data to pass to function
;;; 
;;; notify :
;;;     function to call when the timeout is removed, or NULL
;;; 
;;; Returns :
;;;     the ID (greater than 0) of the event source. Rename to:
;;;     g_timeout_add_seconds
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcfun ("g_timeout_add_seconds_full" g-timeout-add-seconds-full) :uint
  (priority :int)
  (interval-seconds :uint)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(export 'g-timeout-add-seconds-full)

;;; ----------------------------------------------------------------------------
;;; g_idle_source_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_idle_source_new" g-idle-source-new) (:pointer g-source)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-7}
  @return{The newly created idle source.}
  @begin{short}
    Creates a new idle source.
  @end{short}@break{}
  The source will not initially be associated with any @type{g-main-context} and
  must be added to one with @fun{g-source-attach} before it will be executed.
  Note that the default priority for idle sources is
  @variable{g-priority-default-idle}, as compared to other sources which have a
  default priority of @variable{g-priority-default}.")

(export 'g-idle-source-new)

;;; ----------------------------------------------------------------------------
;;; g_idle_add ()
;;; 
;;; guint g_idle_add (GSourceFunc function, gpointer data);
;;; 
;;; Adds a function to be called whenever there are no higher priority events
;;; pending to the default main loop. The function is given the default idle
;;; priority, G_PRIORITY_DEFAULT_IDLE. If the function returns FALSE it is
;;; automatically removed from the list of event sources and will not be called
;;; again.
;;; 
;;; This internally creates a main loop source using g_idle_source_new() and
;;; attaches it to the main loop context using g_source_attach(). You can do
;;; these steps manually if you need greater control.
;;; 
;;; function :
;;;     function to call
;;; 
;;; data :
;;;     data to pass to function.
;;; 
;;; Returns :
;;;     the ID (greater than 0) of the event source.
;;; ----------------------------------------------------------------------------

(defcfun ("g_idle_add" %g-idle-add) :uint
  (function :pointer)
  (data :pointer))

(defun g-idle-add (func &key (priority g-priority-default))
  (g-idle-add-full priority
                   (callback call-timeout-from-main-loop-cb)
                   (glib::allocate-stable-pointer func)
                   (callback glib::stable-pointer-destroy-notify-cb)))

(export 'g-idle-add)

;;; ----------------------------------------------------------------------------
;;; g_idle_add_full ()
;;; 
;;; guint g_idle_add_full (gint priority,
;;;                        GSourceFunc function,
;;;                        gpointer data,
;;;                        GDestroyNotify notify);
;;; 
;;; Adds a function to be called whenever there are no higher priority events
;;; pending. If the function returns FALSE it is automatically removed from the
;;; list of event sources and will not be called again.
;;; 
;;; This internally creates a main loop source using g_idle_source_new() and
;;; attaches it to the main loop context using g_source_attach(). You can do
;;; these steps manually if you need greater control.
;;; 
;;; priority :
;;;     the priority of the idle source. Typically this will be in the range
;;;     between G_PRIORITY_DEFAULT_IDLE and G_PRIORITY_HIGH_IDLE.
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
;;;     the ID (greater than 0) of the event source. Rename to: g_idle_add
;;; ----------------------------------------------------------------------------

(defcfun ("g_idle_add_full" g-idle-add-full) :uint
  (priority :uint)
  (function :pointer)
  (data :pointer)
  (notify :pointer))

(export 'g-idle-add-full)

;;; ----------------------------------------------------------------------------
;;; g_idle_remove_by_data ()
;;; 
;;; gboolean g_idle_remove_by_data (gpointer data);
;;; 
;;; Removes the idle function with the given data.
;;; 
;;; data :
;;;     the data for the idle source's callback.
;;; 
;;; Returns :
;;;     TRUE if an idle source was found and removed.
;;; ----------------------------------------------------------------------------

(defcfun ("g_idle_remove_by_data" g-idle-remove-by-data) :boolean
  (data :pointer))

(export 'g-idle-remove-by-data)

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
;;;     handle for a process (which doesn't have to be a child).
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
;;;     handle for a process (which doesn't have to be a child).
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
;;; and attaches it to the main loop context using g_source_attach(). You can do
;;; these steps manually if you need greater control.
;;; 
;;; priority :
;;;     the priority of the idle source. Typically this will be in the range
;;;     between G_PRIORITY_DEFAULT_IDLE and G_PRIORITY_HIGH_IDLE.
;;; 
;;; pid :
;;;     process to watch. On POSIX the pid of a child process. On Windows a
;;;     handle for a process (which doesn't have to be a child).
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
;;; by GMainContext, but it can be called directly if you need to block until a
;;; file descriptor is ready, but don't want to run the full main loop.
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
;;; G_POLLFD_FORMAT
;;; 
;;; #define G_POLLFD_FORMAT "%#I64x"
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
;;; struct GSourceCallbackFuncs
;;; 
;;; struct GSourceCallbackFuncs {
;;;   void (*ref)   (gpointer     cb_data);
;;;   void (*unref) (gpointer     cb_data);
;;;   void (*get)   (gpointer     cb_data,
;;;                  GSource     *source, 
;;;                  GSourceFunc *func,
;;;                  gpointer    *data);
;;; };
;;; 
;;; The GSourceCallbackFuncs struct contains functions for managing callback
;;; objects.
;;; 
;;; ref ()
;;;     Called when a reference is added to the callback object
;;; 
;;; unref ()
;;;     Called when a reference to the callback object is dropped
;;; 
;;; get ()
;;;     Called to extract the callback function and data from the callback
;;;     object.
;;; ----------------------------------------------------------------------------

(defcstruct g-source-callback-funcs
  (ref :pointer)
  (unref :pointer)
  (get :pointer))

(export 'g-source-callback-funcs)

;;; ----------------------------------------------------------------------------
;;; g_source_new ()
;;; 
;;; GSource * g_source_new (GSourceFuncs *source_funcs, guint struct_size);
;;; 
;;; Creates a new GSource structure. The size is specified to allow creating
;;; structures derived from GSource that contain additional data. The size
;;; passed in must be at least sizeof (GSource).
;;; 
;;; The source will not initially be associated with any GMainContext and must
;;; be added to one with g_source_attach() before it will be executed.
;;; 
;;; source_funcs :
;;;     structure containing functions that implement the sources behavior.
;;; 
;;; struct_size :
;;;     size of the GSource structure to create.
;;; 
;;; Returns :
;;;     the newly-created GSource.
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_new" g-source-new) (:pointer g-source)
  (source-funcs (:pointer g-source-funcs))
  (struct-size :uint))

(export 'g-source-new)

;;; ----------------------------------------------------------------------------
;;; g_source_ref ()
;;; 
;;; GSource * g_source_ref (GSource *source);
;;; 
;;; Increases the reference count on a source by one.
;;; 
;;; source :
;;;     a GSource
;;; 
;;; Returns :
;;;     source
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_ref" g-source-ref) (:pointer g-source)
  (source (:pointer g-source)))

(export 'g-source-ref)

;;; ----------------------------------------------------------------------------
;;; g_source_unref ()
;;; 
;;; void g_source_unref (GSource *source);
;;; 
;;; Decreases the reference count of a source by one. If the resulting reference
;;; count is zero the source and associated memory will be destroyed.
;;; 
;;; source :
;;;     a GSource
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_unref" g-source-unref) :void
  (source (:pointer g-source)))

(export 'g-source-unref)

;;; ----------------------------------------------------------------------------
;;; g_source_set_funcs ()
;;; 
;;; void g_source_set_funcs (GSource *source, GSourceFuncs *funcs);
;;; 
;;; Sets the source functions (can be used to override default implementations)
;;; of an unattached source.
;;; 
;;; source :
;;;     a GSource
;;; 
;;; funcs :
;;;     the new GSourceFuncs
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_set_funcs" g-source-set-funcs) :void
  (source (:pointer g-source))
  (funcs (:pointer g-source-funcs)))

(export 'g-source-set-funcs)

;;; ----------------------------------------------------------------------------
;;; g_source_attach ()
;;; 
;;; guint g_source_attach (GSource *source, GMainContext *context);
;;; 
;;; Adds a GSource to a context so that it will be executed within that context.
;;; Remove it by calling g_source_destroy().
;;; 
;;; source :
;;;     a GSource
;;; 
;;; context :
;;;     a GMainContext (if NULL, the default context will be used)
;;; 
;;; Returns :
;;;     the ID (greater than 0) for the source within the GMainContext.
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_attach" g-source-attach) :uint
  (source (:pointer g-source))
  (context (:pointer g-main-context)))

(export 'g-source-attach)

;;; ----------------------------------------------------------------------------
;;; g_source_destroy ()
;;; 
;;; void g_source_destroy (GSource *source);
;;; 
;;; Removes a source from its GMainContext, if any, and mark it as destroyed.
;;; The source cannot be subsequently added to another context.
;;; 
;;; source :
;;;     a GSource
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_destroy" g-source-destroy) :void
  (source (:pointer g-source)))

(export 'g-source-destroy)

;;; ----------------------------------------------------------------------------
;;; g_source_is_destroyed ()
;;; 
;;; gboolean g_source_is_destroyed (GSource *source);
;;; 
;;; Returns whether source has been destroyed.
;;; 
;;; This is important when you operate upon your objects from within idle
;;; handlers, but may have freed the object before the dispatch of your idle
;;; handler.
;;; 
;;;   static gboolean 
;;;   idle_callback (gpointer data)
;;;   {
;;;     SomeWidget *self = data;
;;;      
;;;     GDK_THREADS_ENTER ();
;;;     /* do stuff with self */
;;;     self->idle_id = 0;
;;;     GDK_THREADS_LEAVE ();
;;;      
;;;     return G_SOURCE_REMOVE;
;;;   }
;;;    
;;;   static void 
;;;   some_widget_do_stuff_later (SomeWidget *self)
;;;   {
;;;     self->idle_id = g_idle_add (idle_callback, self);
;;;   }
;;;    
;;;   static void 
;;;   some_widget_finalize (GObject *object)
;;;   {
;;;     SomeWidget *self = SOME_WIDGET (object);
;;;      
;;;     if (self->idle_id)
;;;       g_source_remove (self->idle_id);
;;;      
;;;     G_OBJECT_CLASS (parent_class)->finalize (object);
;;;   }
;;; 
;;; This will fail in a multi-threaded application if the widget is destroyed
;;; before the idle handler fires due to the use after free in the callback. A
;;; solution, to this particular problem, is to check to if the source has
;;; already been destroy within the callback.
;;; 
;;;   static gboolean 
;;;   idle_callback (gpointer data)
;;;   {
;;;     SomeWidget *self = data;
;;;     
;;;     GDK_THREADS_ENTER ();
;;;     if (!g_source_is_destroyed (g_main_current_source ()))
;;;       {
;;;         /* do stuff with self */
;;;       }
;;;     GDK_THREADS_LEAVE ();
;;;     
;;;     return FALSE;
;;;   }
;;; 
;;; source :
;;;     a GSource
;;; 
;;; Returns :
;;;     TRUE if the source has been destroyed
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_is_destroyed" g-source-is-destroyed) :boolean
  (source (:pointer g-source)))

(export 'g-source-is-destroyed)

;;; ----------------------------------------------------------------------------
;;; g_source_set_priority ()
;;; 
;;; void g_source_set_priority (GSource *source, gint priority);
;;; 
;;; Sets the priority of a source. While the main loop is being run, a source
;;; will be dispatched if it is ready to be dispatched and no sources at a
;;; higher (numerically smaller) priority are ready to be dispatched.
;;; 
;;; source :
;;;     a GSource
;;; 
;;; priority :
;;;     the new priority.
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_set_priority" g-source-set-priority) :void
  (source (:pointer g-source))
  (priority :int))

(export 'g-source-set-priority)

;;; ----------------------------------------------------------------------------
;;; g_source_get_priority ()
;;; 
;;; gint g_source_get_priority (GSource *source);
;;; 
;;; Gets the priority of a source.
;;; 
;;; source :
;;;     a GSource
;;; 
;;; Returns :
;;;     the priority of the source
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_get_priority" g-source-get-priority) :int
  (source (:pointer g-source)))

(export 'g-source-get-priority)

;;; ----------------------------------------------------------------------------
;;; g_source_set_can_recurse ()
;;; 
;;; void g_source_set_can_recurse (GSource *source, gboolean can_recurse);
;;; 
;;; Sets whether a source can be called recursively. If can_recurse is TRUE,
;;; then while the source is being dispatched then this source will be processed
;;; normally. Otherwise, all processing of this source is blocked until the
;;; dispatch function returns.
;;; 
;;; source :
;;;     a GSource
;;; 
;;; can_recurse :
;;;     whether recursion is allowed for this source
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_set_can_recurse" g-source-set-can-recurse) :void
  (source (:pointer g-source))
  (can-recurse :boolean))

(export 'g-source-set-can-recurse)

;;; ----------------------------------------------------------------------------
;;; g_source_get_can_recurse ()
;;; 
;;; gboolean g_source_get_can_recurse (GSource *source);
;;; 
;;; Checks whether a source is allowed to be called recursively. See
;;; g_source_set_can_recurse().
;;; 
;;; source :
;;;     a GSource
;;; 
;;; Returns :
;;;     whether recursion is allowed.
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_get_can_recurse" g-source-get-can-recurse) :boolean
  (source (:pointer g-source)))

(export 'g-source-get-can-recurse)

;;; ----------------------------------------------------------------------------
;;; g_source_get_id ()
;;; 
;;; guint g_source_get_id (GSource *source);
;;; 
;;; Returns the numeric ID for a particular source. The ID of a source is a
;;; positive integer which is unique within a particular main loop context. The
;;; reverse mapping from ID to source is done by
;;; g_main_context_find_source_by_id().
;;; 
;;; source :
;;;     a GSource
;;; 
;;; Returns :
;;;     the ID (greater than 0) for the source
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_get_id" g-source-get-id) :uint
  (source (:pointer g-source)))

(export 'g-source-get-id)

;;; ----------------------------------------------------------------------------
;;; g_source_get_name ()
;;; 
;;; const char * g_source_get_name (GSource *source);
;;; 
;;; Gets a name for the source, used in debugging and profiling. The name may be
;;; NULL if it has never been set with g_source_set_name().
;;; 
;;; source :
;;;     a GSource
;;; 
;;; Returns :
;;;     the name of the source
;;; 
;;; Since 2.26
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_get_name" g-source-get-name) :string
  (source (:pointer g-source)))

(export 'g-source-get-name)

;;; ----------------------------------------------------------------------------
;;; g_source_set_name ()
;;; 
;;; void g_source_set_name (GSource *source, const char *name);
;;; 
;;; Sets a name for the source, used in debugging and profiling. The name
;;; defaults to NULL.
;;; 
;;; The source name should describe in a human-readable way what the source
;;; does. For example, "X11 event queue" or "GTK+ repaint idle handler" or
;;; whatever it is.
;;; 
;;; It is permitted to call this function multiple times, but is not recommended
;;; due to the potential performance impact. For example, one could change the
;;; name in the "check" function of a GSourceFuncs to include details like the
;;; event type in the source name.
;;; 
;;; source :
;;;     a GSource
;;; 
;;; name :
;;;     debug name for the source
;;; 
;;; Since 2.26
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_set_name" g-source-set-name) :void
  (source (:pointer g-source))
  (name :string))

(export 'g-source-set-name)

;;; ----------------------------------------------------------------------------
;;; g_source_set_name_by_id ()
;;; 
;;; void g_source_set_name_by_id (guint tag, const char *name);
;;; 
;;; Sets the name of a source using its ID.
;;; 
;;; This is a convenience utility to set source names from the return value of
;;; g_idle_add(), g_timeout_add(), etc.
;;; 
;;; tag :
;;;     a GSource ID
;;; 
;;; name :
;;;     debug name for the source
;;; 
;;; Since 2.26
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_get_context ()
;;; 
;;; GMainContext * g_source_get_context (GSource *source);
;;; 
;;; Gets the GMainContext with which the source is associated. Calling this
;;; function on a destroyed source is an error.
;;; 
;;; source :
;;;     a GSource
;;; 
;;; Returns :
;;;     the GMainContext with which the source is associated, or NULL if the
;;;     context has not yet been added to a source
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_get_context" g-source-get-context) (:pointer g-main-context)
  (source (:pointer g-source)))

(export 'g-source-get-context)

;;; ----------------------------------------------------------------------------
;;; g_source_set_callback ()
;;; 
;;; void g_source_set_callback (GSource *source,
;;;                             GSourceFunc func,
;;;                             gpointer data,
;;;                             GDestroyNotify notify);
;;; 
;;; Sets the callback function for a source. The callback for a source is called
;;; from the source's dispatch function.
;;; 
;;; The exact type of func depends on the type of source; ie. you should not
;;; count on func being called with data as its first parameter.
;;; 
;;; Typically, you won't use this function. Instead use functions specific to
;;; the type of source you are using.
;;; 
;;; source :
;;;     the source
;;; 
;;; func :
;;;     a callback function
;;; 
;;; data :
;;;     the data to pass to callback function
;;; 
;;; notify :
;;;     a function to call when data is no longer in use, or NULL
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_set_callback" g-source-set-callback) :void
  (source (:pointer g-source))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(export 'g-source-set-callback)

;;; ----------------------------------------------------------------------------
;;; GSourceFunc ()
;;; 
;;; gboolean (*GSourceFunc) (gpointer user_data);
;;; 
;;; Specifies the type of function passed to g_timeout_add(),
;;; g_timeout_add_full(), g_idle_add(), and g_idle_add_full().
;;; 
;;; user_data :
;;;     data passed to the function, set when the source was created with one of
;;;     the above functions
;;; 
;;; Returns :
;;;     FALSE if the source should be removed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_source_set_callback_indirect ()
;;; 
;;; void g_source_set_callback_indirect (GSource *source,
;;;                                      gpointer callback_data,
;;;                                      GSourceCallbackFuncs *callback_funcs);
;;; 
;;; Sets the callback function storing the data as a refcounted callback
;;; "object". This is used internally. Note that calling
;;; g_source_set_callback_indirect() assumes an initial reference count on
;;; callback_data, and thus callback_funcs->unref will eventually be called once
;;; more than callback_funcs->ref.
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
;;; g_source_add_poll ()
;;; 
;;; void g_source_add_poll (GSource *source, GPollFD *fd);
;;; 
;;; Adds a file descriptor to the set of file descriptors polled for this
;;; source. This is usually combined with g_source_new() to add an event source.
;;; The event source's check function will typically test the revents field in
;;; the GPollFD struct and return TRUE if events need to be processed.
;;; 
;;; source :
;;;     a GSource
;;; 
;;; fd :
;;;     a GPollFD structure holding information about a file descriptor to
;;;     watch.
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_add_poll" g-source-add-poll) :void
  (source (:pointer g-source))
  (fd (:pointer g-poll-fd)))

(export 'g-source-add-poll)

;;; ----------------------------------------------------------------------------
;;; g_source_remove_poll ()
;;; 
;;; void g_source_remove_poll (GSource *source, GPollFD *fd);
;;; 
;;; Removes a file descriptor from the set of file descriptors polled for this
;;; source.
;;; 
;;; source :
;;;     a GSource
;;; 
;;; fd :
;;;     a GPollFD structure previously passed to g_source_add_poll().
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_remove_poll" g-source-remove-poll) :void
  (source (:pointer g-source))
  (fd (:pointer g-poll-fd)))

(export 'g-source-remove-poll)

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
;;; g_source_get_time ()
;;; 
;;; gint64 g_source_get_time (GSource *source);
;;; 
;;; Gets the time to be used when checking this source. The advantage of calling
;;; this function over calling g_get_monotonic_time() directly is that when
;;; checking multiple sources, GLib can cache a single value instead of having
;;; to repeatedly get the system monotonic time.
;;; 
;;; The time here is the system monotonic time, if available, or some other
;;; reasonable alternative otherwise. See g_get_monotonic_time().
;;; 
;;; source :
;;;     a GSource
;;; 
;;; Returns :
;;;     the monotonic time in microseconds
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_get_time" g-source-get-time) :void
  (source (:pointer g-source)))

(export 'g-source-get-time)

;;; ----------------------------------------------------------------------------
;;; g_source_get_current_time ()
;;; 
;;; void g_source_get_current_time (GSource *source, GTimeVal *timeval);
;;; 
;;; Warning
;;; 
;;; g_source_get_current_time has been deprecated since version 2.28 and should
;;; not be used in newly-written code. use g_source_get_time() instead
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

(defcfun ("g_source_get_current_time" g-source-get-current-time) :void
  (source (:pointer g-source))
  (timeval-ret (:pointer g-time-val)))

(export 'g-source-get-current-time)

;;; ----------------------------------------------------------------------------
;;; g_source_remove ()
;;; 
;;; gboolean g_source_remove (guint tag);
;;; 
;;; Removes the source with the given id from the default main context. The id
;;; of a GSource is given by g_source_get_id(), or will be returned by the
;;; functions g_source_attach(), g_idle_add(), g_idle_add_full(),
;;; g_timeout_add(), g_timeout_add_full(), g_child_watch_add(),
;;; g_child_watch_add_full(), g_io_add_watch(), and g_io_add_watch_full().
;;; 
;;; See also g_source_destroy(). You must use g_source_destroy() for sources
;;; added to a non-default main context.
;;; 
;;; tag :
;;;     the ID of the source to remove.
;;; 
;;; Returns :
;;;     TRUE if the source was found and removed.
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_remove" g-source-remove) :boolean
  (id :uint))

(export 'g-source-remove)

;;; ----------------------------------------------------------------------------
;;; g_source_remove_by_funcs_user_data ()
;;; 
;;; gboolean g_source_remove_by_funcs_user_data (GSourceFuncs *funcs,
;;;                                              gpointer user_data);
;;; 
;;; Removes a source from the default main loop context given the source
;;; functions and user data. If multiple sources exist with the same source
;;; functions and user data, only one will be destroyed.
;;; 
;;; funcs :
;;;     The source_funcs passed to g_source_new()
;;; 
;;; user_data :
;;;     the user data for the callback
;;; 
;;; Returns :
;;;     TRUE if a source was found and removed.
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_remove_by_funcs_user_data"
          g-source-remove-by-funcs-user-data)
    :boolean
  (funcs (:pointer g-source-funcs))
  (data :pointer))

(export 'g-source-remove-by-funcs-user-data)

;;; ----------------------------------------------------------------------------
;;; g_source_remove_by_user_data ()
;;; 
;;; gboolean g_source_remove_by_user_data (gpointer user_data);
;;; 
;;; Removes a source from the default main loop context given the user data for
;;; the callback. If multiple sources exist with the same user data, only one
;;; will be destroyed.
;;; 
;;; user_data :
;;;     the user_data for the callback.
;;; 
;;; Returns :
;;;     TRUE if a source was found and removed.
;;; ----------------------------------------------------------------------------

(defcfun ("g_source_remove_by_user_data" g-source-remove-by-user-data) :boolean
  (data :pointer))

(export 'g-source-remove-by-user-data)

;;; --- End of file glib.main-loop.lisp  ---------------------------------------
