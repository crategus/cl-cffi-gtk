;;; ----------------------------------------------------------------------------
;;; atdoc-glib.main-loop.lisp
;;;
;;; Documentation strings for the library GLib.
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.32.3 Reference Manual. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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

;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-priority-high atdoc:*variable-name-alias*) "Constant")
(setf (documentation 'g-priority-high 'variable)
 "@version{2012-12-25}
  @variable-value{-100}
  @short{Use this for high priority event sources.}
  It is not used within GLib or GTK+.")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-priority-default atdoc:*variable-name-alias*) "Constant")
(setf (documentation 'g-priority-default 'variable)
 "@version{2012-12-25}
  @variable-value{0}
  @short{Use this for default priority event sources.}

  In GLib this priority is used when adding timeout functions with
  @fun{g-timeout-add}. In GDK this priority is used for events from the X
  server.
  @see-function{g-timeout-add}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-priority-high-idle atdoc:*variable-name-alias*) "Constant")
(setf (documentation 'g-priority-high-idle 'variable)
 "@version{2012-12-25}
  @variable-value{100}
  @short{Use this for high priority idle functions.}

  GTK+ uses @code{@sym{+g-priority-high-idle+} + 10} for resizing operations,
  and @code{@sym{+g-priority-high-idle+} + 20} for redrawing operations. (This
  is done to ensure that any pending resizes are processed before any pending
  redraws, so that widgets are not redrawn twice unnecessarily.)")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-priority-default-idle atdoc:*variable-name-alias*) "Constant")
(setf (documentation 'g-priority-default-idle 'variable)
 "@version{2012-12-25}
  @variable-value{200}
  @short{Use this for default priority idle functions.}

  In GLib this priority is used when adding idle functions with
  @fun{g-idle-add}.
  @see-function{g-idle-add}")

;;; ----------------------------------------------------------------------------

(setf (gethash 'g-priority-low atdoc:*variable-name-alias*) "Constant")
(setf (documentation 'g-priority-low 'variable)
 "@version{2012-12-25}
  @variable-value{300}
  @short{Use this for very low priority background tasks.}

  It is not used within GLib or GTK+.")

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

;;; --- g-main-context ---------------------------------------------------------

(setf (gethash 'g-main-context atdoc:*type-name-alias*) "CStruct")
(setf (documentation 'g-main-context 'type)
 "@version{2012-12-25}
  @begin{short}
    The @sym{g-main-context} struct is an opaque data type representing a set of
    sources to be handled in a main loop.
  @end{short}")

;;; --- g-main-context-new -----------------------------------------------------

(setf (documentation 'g-main-context-new 'function)
 "@version{2012-12-25}
  @return{The new @type{g-main-context}.}
  @short{Creates a new @sym{g-main-context} structure.}
  @see-type{g-main-context}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-main-context-ref 'function)
 "@version{2012-12-25}
  @argument[context]{a @type{g-main-context}}
  @return{The @arg{context} that was passed in (since 2.6).}
  @short{Increases the reference count on a @type{g-main-context} object by
    one.}
  @see-type{g-main-context}
  @see-function{g-main-context-unref}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-main-context-unref 'function)
 "@version{2012-12-26}
  @argument[context]{a @type{g-main-context}}
  @begin{short}
    Decreases the reference count on a @type{g-main-context} object by one. If
    the result is zero, free the context and free all associated memory.
  @end{short}
  @see-type{g-main-context}
  @see-function{g-main-context-ref}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-main-context-default 'function)
 "@version{2013-2-12}
  @return{The global default main context.}
  @short{Returns the global default main context.}
  This is the main context used for main loop functions when a main loop is not
  explicitly specified, and corresponds to the \"main\" main loop. See also
  @code{g_main_context_get_thread_default()}.
  @see-type{g-main-context}")

;;; ----------------------------------------------------------------------------

(setf (documentation 'g-main-context-iteration 'function)
 "@version{2012-12-26}
  @argument[context]{a @type{g-main-context} (if @code{null}-pointer, the
    default context will be used)}
  @argument[may-block]{whether the call may block.}
  @return{@arg{true} if events were dispatched.}
  @short{Runs a single iteration for the given main loop.}
  This involves checking to see if any event sources are ready to be processed,
  then if no events sources are ready and @arg{may-block} is @arg{true}, waiting
  for a source to become ready, then dispatching the highest priority events
  sources that are ready. Otherwise, if @arg{may-block} is @code{nil} sources
  are not waited to become ready, only those highest priority events sources
  will be dispatched (if any), that are ready at this given moment without
  further waiting.

  Note that even when @arg{may-block} is @arg{true}, it is still possible for
  @sym{g-main-context-iteration} to return @code{nil}, since the the wait may be
  interrupted for other reasons than an event source becoming ready.
  @see-type{g-main-context}")

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

;;; --- g-main-context-pending -------------------------------------------------

(setf (documentation 'g-main-context-pending 'function)
 "@version{2013-01-01}
  @argument[context]{a @type{g-main-context} (if @code{null}-pointer, the
    default context will be used)}
  @return{@arg{true} if events are pending.}
  @short{Checks if any sources have pending events for the given context.}")

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

;;; --- g-main-context-find-source-by-id ---------------------------------------

(setf (documentation 'g-main-context-find-source-by-id 'function)
 "@version{2013-01-01}
  @argument[context]{a @type{g-main-context} (if @code{null}-pointer, the
    default context will be used)}
  @argument[source-id]{the source ID, as returned by @fun{g-source-get-id}.}
  @return{The @type{g-source} if found, otherwise, @code{null}-pointer}
  @short{Finds a @type{g-source} given a pair of context and ID.}")

;;; --- g-main-context-find-source-by-user-data --------------------------------

(setf (documentation 'g-main-context-find-source-by-user-data 'function)
 "@version{2013-01-01}
  @argument[context]{a @type{g-main-context}}
  @argument[user-data]{the user data for the callback.}
  @return{The source, if one was found, otherwise @code{null}-pointer.}
  @begin{short}
    Finds a source with the given user data for the callback.
  @end{short}
  If multiple sources exist with the same user data, the first one found will be
  returned.")

;;; --- g-main-context-find-source-by-funcs-user-data --------------------------

(setf (documentation 'g-main-context-find-source-by-funcs-user-data 'function)
 "@version{2013-01-01}
  @argument[context]{a @type{g-main-context} (if @code{null}-pointer, the
    default context will be used)}
  @argument[funcs]{the source funcs passed to @fun{g-source-new}.}
  @argument[user-data]{the user data from the callback.}
  @return{The source, if one was found, otherwise @code{null}-pointer.}
  @begin{short}
    Finds a source with the given source functions and user data.
  @end{short}
  If multiple sources exist with the same source function and user data, the
  first one found will be returned.")

;;; --- g-main-context-wakeup --------------------------------------------------

(setf (documentation 'g-main-context-wakeup 'function)
 "@version{2013-01-01}
  @argument[context]{a @type{g-main-context}}
  @begin{short}
    If context is currently waiting in a @code{poll()}, interrupt the
    @code{poll()}, and continue the iteration process.
  @end{short}")

;;; --- g-main-context-acquire -------------------------------------------------

(setf (documentation 'g-main-context-acquire 'function)
 "@version{2013-01-01}
  @argument[context]{a @type{g-main-context}}
  @return{@arg{true} if the operation succeeded, and this thread is now the
    owner of context.}
  @short{Tries to become the owner of the specified context.}
  If some other thread is the owner of the context, returns @code{nil}
  immediately. Ownership is properly recursive: the owner can require ownership
  again and will release ownership when @fun{g-main-context-release} is called
  as many times as @fun{g-main-context-acquire}.

  You must be the owner of a context before you can call
  @fun{g-main-context-prepare}, @fun{g-main-context-query},
  @fun{g-main-context-check}, @fun{g-main-context-dispatch}.")

;;; --- g-main-context-release -------------------------------------------------

(setf (documentation 'g-main-context-release 'function)
 "@version{2013-01-01}
  @argument[context]{a @type{g-main-context}}
  @begin{short}
    Releases ownership of a context previously acquired by this thread with
    @fun{g-main-context-acquire}.
  @end{short}
  If the context was acquired multiple times, the ownership will be released
  only when @fun{g-main-context-release} is called as many times as it was
  acquired.")

;;; --- g-main-context-is-owner ------------------------------------------------

(setf (documentation 'g-main-context-is-owner 'function)
 "@version{2013-01-01}
  @argument[context]{a @type{g-main-context}}
  @return{@arg{true} if current thread is owner of context.}
  @begin{short}
    Determines whether this thread holds the (recursive) ownership of this
    @type{g-main-context}.
  @end{short}
  This is useful to know before waiting on another thread that may be blocking
  to get ownership of context.

  Since 2.10")

;;; --- g-main-context-wait ----------------------------------------------------

(setf (documentation 'g-main-context-wait 'function)
 "@version{2013-01-16}
  @argument[context]{a @type{g-main-context}}
  @argument[cond]{a condition variable}
  @argument[mutex]{a mutex, currently held}
  @return{@arg{true} if the operation succeeded, and this thread is now the
    owner of context.}
  @begin{short}
    Tries to become the owner of the specified context, as with
    @fun{g-main-context-acquire}.
  @end{short}
  But if another thread is the owner, atomically drop mutex and wait on cond
  until that owner releases ownership or until cond is signaled, then try again
  (once) to become the owner.")

;;; --- g-main-context-prepare -------------------------------------------------

(setf (documentation 'g-main-context-prepare 'function)
 "@version{2013-01-16}
  @argument[context]{a @type{g-main-context}}
  @argument[priority]{location to store priority of highest priority source
    already ready.}
  @return{@arg{true} if some source is ready to be dispatched prior to polling.}
  @begin{short}
    Prepares to poll sources within a main loop.
  @end{short}
  The resulting information for polling is determined by calling
  @fun{g-main-context-query}.")

;;; --- g-main-context-query ---------------------------------------------------

(setf (documentation 'g-main-context-query 'function)
 "@version{2013-01-16}
  @argument[context]{a @type{g-main-context}}
  @argument[max-priority]{maximum priority source to check}
  @argument[timeout]{location to store timeout to be used in polling}
  @argument[fds]{location to store @type{g-poll-fd} records that need to be
    polled}
  @argument[n-fds]{length of @arg{fds}.}
  @return{the number of records actually stored in @arg{fds}, or, if more than
  @arg{n-fds} records need to be stored, the number of records that need to be
    stored.}
  @short{Determines information necessary to poll this main loop.}")

;;; --- g-main-context-check ---------------------------------------------------

(setf (documentation 'g-main-context-check 'function)
 "@version{2013-01-16}
  @argument[context]{a @type{g-main-context}}
  @argument[max-priority]{the maximum numerical priority of sources to check}
  @argument[fds]{array of @type{g-poll-fd}'s that was passed to the last call to
    @fun{g-main-context-query}}
  @argument[n-fds]{return value of @code{g-main-context-query}}
  @return{@arg{true} if some sources are ready to be dispatched.}
  @short{Passes the results of polling back to the main loop.}")

;;; --- g-main-context-dispatch ------------------------------------------------

(setf (documentation 'g-main-context-dispatch 'function)
 "@version{2013-01-16}
  @argument[context]{a @type{g-main-context}}
  @short{Dispatches all pending sources.}")

;;; --- g-main-context-set-poll-func -------------------------------------------

(setf (documentation 'g-main-context-set-poll-func 'function)
 "@version{2013-01-16}
  @argument[context]{a @type{g-main-context}}
  @argument[func]{the function to call to poll all file descriptors}
  @begin{short}
    Sets the function to use to handle polling of file descriptors.
  @end{short}
  It will be used instead of the @code{poll()} system call (or GLib's
  replacement function, which is used where @code{poll()} isn't available).

  This function could possibly be used to integrate the GLib event loop with
  an external event loop.")

;;; --- g-main-context-get-poll-func -------------------------------------------

(setf (documentation 'g-main-context-get-poll-func 'function)
 "@version{2013-01-16}
  @argument[context]{a @type{g-main-context}}
  @return{the poll function}
  @short{Gets the poll function set by @fun{g-main-context-set-poll-func}.}")

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

;;; --- g-main-context-add-poll ------------------------------------------------

(setf (documentation 'g-main-context-add-poll 'function)
 "@version{2013-01-16}
  @argument[context]{a @type{g-main-context} (or @code{null}-pointer for the
    default context)}
  @argument[fd]{a @type{g-poll-fd} structure holding information about a file
    descriptor to watch.}
  @argument[priority]{the priority for this file descriptor which should be the
    same as the priority used for @fun{g-source-attach} to ensure that the file
    descriptor is polled whenever the results may be needed.}
  @begin{short}
    Adds a file descriptor to the set of file descriptors polled for this
    context.
  @end{short}
  This will very seldom be used directly. Instead a typical event source will
  use @fun{g-source-add-poll} instead.")

;;; --- g-main-context-remove-poll ---------------------------------------------

(setf (documentation 'g-main-context-remove-poll 'function)
 "@version{2013-01-16}
  @argument[context]{a @type{g-main-context}}
  @argument[fd]{a @type{g-poll-fd} descriptor previously added with
    @fun{g-main-context-add-poll}}
  @begin{short}
    Removes file descriptor from the set of file descriptors to be polled for a
    particular context.
  @end{short}")

;;; --- g-main-depth -----------------------------------------------------------

(setf (documentation 'g-main-depth 'function)
 "@version{2013-01-16}
  @return{The main loop recursion level in the current thread.}
  @begin{short}
    Returns the depth of the stack of calls to @fun{g-main-context-dispatch} on
    any @type{g-main-context} in the current thread.
  @end{short}
  That is, when called from the toplevel, it gives @code{0}. When called from
  within a callback from @fun{g-main-context-iteration} (or
  @fun{g-main-loop-run}, etc.) it returns @code{1}. When called from within a
  callback to a recursive call to @fun{g-main-context-iteration}, it returns
  @code{2}. And so forth.

  There is a temptation to use @sym{g-main-depth} to solve problems with
  reentrancy. For instance, while waiting for data to be received from the
  network in response to a menu item, the menu item might be selected again.
  It might seem that one could make the menu item's callback return
  immediately and do nothing if @sym{g-main-depth} returns a value greater than
  @code{1}. However, this should be avoided since the user then sees selecting
  the menu item do nothing. Furthermore, you'll find yourself adding these
  checks all over your code, since there are doubtless many, many things that
  the user could do. Instead, you can use the following techniques:
  @begin{itemize}
    @item{Use @fun{gtk-widget-set-sensitive} or modal dialogs to prevent the
      user from interacting with elements while the main loop is recursing.}
    @item{Avoid main loop recursion in situations where you can't handle
      arbitrary callbacks. Instead, structure your code so that you simply
      return to the main loop and then get called again when there is more work
      to do.}
  @end{itemize}")

;;; --- g-main-current-source --------------------------------------------------

(setf (documentation 'g-main-current-source 'function)
 "@version{2013-01-16}
  @return{The currently firing source or @code{null}-pointer.}
  @short{Returns the currently firing source for this thread.}

  Since 2.12")

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

;;; --- g-timeout-source-new ---------------------------------------------------

(setf (documentation 'g-timeout-source-new 'function)
 "@version{2013-01-16}
  @argument[interval]{the timeout interval in milliseconds.}
  @return{the newly-created timeout source}
  @short{Creates a new timeout source.}

  The source will not initially be associated with any @type{g-main-context} and
  must be added to one with @fun{g-source-attach} before it will be executed.

  The interval given is in terms of monotonic time, not wall clock time. See
  @code{g_get_monotonic_time()}.")

;;; --- g-timeout-source-new-seconds -------------------------------------------

(setf (documentation 'g-timeout-source-new-seconds 'function)
 "@version{2013-01-16}
  @argument[interval]{the timeout interval in seconds}
  @return{the newly-created timeout source}
  @short{Creates a new timeout source.}

  The source will not initially be associated with any @type{g-main-context} and
  must be added to one with @fun{g-source-attach} before it will be executed.

  The scheduling granularity/accuracy of this timeout source will be in
  seconds.

  The interval given in terms of monotonic time, not wall clock time. See
  @code{g_get_monotonic_time()}.

  Since 2.14")

;;; --- g-timeout-add ----------------------------------------------------------

(setf (documentation 'g-timeout-add 'function)
 "@version{2013-01-16}
  @argument[interval]{the time between calls to the function, in milliseconds
    (1/1000ths of asecond)}
  @argument[function]{function to call}
  @argument[data]{data to pass to function}
  @return{The ID (greater than @code{0}) of the event source.}
  @begin{short}
    Sets a function to be called at regular intervals, with the default
    priority, @var{g-priority-default}.
  @end{short}
  The function is called repeatedly until it returns @code{nil}, at which point
  the timeout is automatically destroyed and the function will not be called
  again. The first call to the function will be at the end of the first
  interval.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval (it does not
  try to 'catch up' time lost in delays).

  If you want to have a timer in the \"seconds\" range and do not care about the
  exact time of the first call of the timer, use the @fun{g-timeout-add-seconds}
  function; this function allows for more optimizations and more efficient
  system power usage.

  This internally creates a main loop source using @fun{g-timeout-source-new}
  and attaches it to the main loop context using @fun{g-source-attach}. You can
  do these steps manually if you need greater control.

  The interval given is in terms of monotonic time, not wall clock time. See
  @code{g_get_monotonic_time()}.")

;;; --- g-timeout-add-full -----------------------------------------------------

(setf (documentation 'g-timeout-add-full 'function)
 "@version{2013-01-17}
  @argument[priority]{the priority of the timeout source. Typically this will be
    in the range between @var{g-priority-default} and @var{g-priority-high}.}
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
  @code{g_get_monotonic_time()}.")

;;; --- g-timeout-add-seconds --------------------------------------------------

(setf (documentation 'g-timeout-add-seconds 'function)
 "@version{2013-01-17}
  @argument[interval]{the time between calls to the function, in seconds}
  @argument[function]{function to call}
  @argument[data]{data to pass to @arg{function}}
  @return{the ID (greater than @code{0}) of the event source.}
  @begin{short}
    Sets a function to be called at regular intervals with the default priority,
    @var{g-priority-default}.
  @end{short}
  The function is called repeatedly until it returns @code{nil}, at which point
  the timeout is automatically destroyed and the function will not be called
  again.

  This internally creates a main loop source using
  @fun{g-timeout-source-new-seconds} and attaches it to the main loop context
  using @fun{g-source-attach}. You can do these steps manually if you need
  greater control. Also see @fun{g-timeout-add-seconds-full}.

  Note that the first call of the timer may not be precise for timeouts of one
  second. If you need finer precision and have such a timeout, you may want to
  use @fun{g-timeout-add} instead.

  The interval given is in terms of monotonic time, not wall clock time. See
  @code{g_get_monotonic_time()}.

  Since 2.14")

;;; --- g-timeout-add-seconds-full ---------------------------------------------

(setf (documentation 'g-timeout-add-seconds-full 'function)
 "@version{2013-01-17}
  @argument[priority]{the priority of the timeout source. Typically this will be
    in the range between @var{g-priority-default} and @var{g-priority-high}.}
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

 Since 2.14")

;;; --- g-idle-source-new ------------------------------------------------------

(setf (documentation 'g-idle-source-new 'function)
 "@version{2013-01-01}
  @return{the newly-created idle source}
  @short{Creates a new idle source.}

  The source will not initially be associated with any GMainContext and must
  be added to one with g_source_attach() before it will be executed. Note that
  the default priority for idle sources is G_PRIORITY_DEFAULT_IDLE, as
  compared to other sources which have a default priority of
  G_PRIORITY_DEFAULT.")

;;; --- g-idle-add -------------------------------------------------------------

(setf (documentation 'g-idle-add 'function)
 "@version{2013-01-01}
  @argument[function]{function to call}
  @argument[data]{data to pass to function.}
  @return{the ID (greater than 0) of the event source.}
  @begin{short}
    Adds a function to be called whenever there are no higher priority events
    pending to the default main loop.
  @end{short}
  The function is given the default idle priority, G_PRIORITY_DEFAULT_IDLE. If
  the function returns FALSE it is automatically removed from the list of event
  sources and will not be called again.

  This internally creates a main loop source using g_idle_source_new() and
  attaches it to the main loop context using g_source_attach(). You can do
  these steps manually if you need greater control.")

;;; --- g-idle-add-full --------------------------------------------------------

(setf (documentation 'g-idle-add-full 'function)
 "@version{2013-01-01}
  @argument[priority]{the priority of the idle source. Typically this will be in
    the range between G_PRIORITY_DEFAULT_IDLE and G_PRIORITY_HIGH_IDLE.}
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
  these steps manually if you need greater control.")

;;; --- g-idle-remove-by-data --------------------------------------------------

(setf (documentation 'g-idle-remove-by-data 'function)
 "@version{2013-01-01}
  @argument[data]{the data for the idle source's callback.}
  @return{TRUE if an idle source was found and removed.}
  @short{Removes the idle function with the given data.}")

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

;;; --- g-poll-fd --------------------------------------------------------------

(setf (gethash 'g-poll-fd atdoc:*type-name-alias*) "CStruct")
(setf (documentation 'g-poll-fd 'type)
 "@version{2013-01-01}
  @begin{short}
    Represents a file descriptor, which events to poll for, and which events
    occurred.
  @end{short}
  @begin{pre}
(defcstruct g-poll-fd
  (fd :int) ; TODO: #if defined (G_OS_WIN32) && GLIB_SIZEOF_VOID_P == 8
  (events :ushort)
  (revent :ushort))
  @end{pre}
  @begin{table}
    @entry[fd]{}
    @entry[gint64 fd]{the file descriptor to poll (or a HANDLE on Win32)}
    @entry[gushort events]{a bitwise combination from @code{GIOCondition},
      specifying which events should be polled for. Typically for reading from a
      file descriptor you would use @code{G_IO_IN | G_IO_HUP | G_IO_ERR}, and
      for writing you would use @code{G_IO_OUT | G_IO_ERR}.}
    @entry[gushort revents]{a bitwise combination of flags from
      @code{GIOCondition}, returned from the @code{poll()} function to indicate
      which events occurred.}
  @end{table}")

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

;;; --- g-source ---------------------------------------------------------------

;;; --- g-source-funcs ---------------------------------------------------------

;;; --- g-source-callback-funcs ------------------------------------------------

(setf (documentation 'g-source-callback-funcs 'type)
 "@version{2013-01-01}
  @begin{short}
    The GSourceCallbackFuncs struct contains functions for managing callback
    objects.
  @end{short}
  @begin[Lisp Implementation]{dictionary}
    @begin{pre}
(defcstruct g-source-callback-funcs
  (ref :pointer)
  (unref :pointer)
  (get :pointer))
    @end{pre}
    @begin{table}
      @entry[ref]{Called when a reference is added to the callback object}
      @entry[unref]{Called when a reference to the callback object is dropped}
      @entry[get]{Called to extract the callback function and data from the
        callback object.}
    @end{table}
  @end{dictionary}")

;;; --- g-source-new -----------------------------------------------------------

;;; --- g-source-ref -----------------------------------------------------------

(setf (documentation 'g-source-ref 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @return{source}
  @short{Increases the reference count on a source by one.}")

;;; --- g-source-unref ---------------------------------------------------------

(setf (documentation 'g-source-unref 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @begin{short}
    Decreases the reference count of a source by one.
  @end{short}
  If the resulting reference count is zero the source and associated memory will
  be destroyed.")

;;; --- g-source-set-funcs -----------------------------------------------------

(setf (documentation 'g-source-set-funcs 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @argument[funcs]{the new GSourceFuncs}
  @begin{short}
    Sets the source functions (can be used to override default implementations)
    of an unattached source.
  @end{short}

  Since 2.12")

;;; --- g-source-attach --------------------------------------------------------

(setf (documentation 'g-source-attach 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @argument[context]{a GMainContext (if NULL, the default context will be used)}
  @return{the ID (greater than 0) for the source within the GMainContext.}
  @begin{short}
    Adds a GSource to a context so that it will be executed within that context.
  @end{short}
  Remove it by calling @fun{g-source-destroy}.")

;;; --- g-source-destroy -------------------------------------------------------

(setf (documentation 'g-source-destroy 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @begin{short}
    Removes a source from its GMainContext, if any, and mark it as destroyed.
  @end{short}
  The source cannot be subsequently added to another context.")

;;; --- g-source-is-destroyed --------------------------------------------------

(setf (documentation 'g-source-is-destroyed 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @return{TRUE if the source has been destroyed}
  @begin{short}
    This is important when you operate upon your objects from within idle
    handlers, but may have freed the object before the dispatch of your idle
    handler.
  @end{short}
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

;;; --- g-source-set-priority --------------------------------------------------

(setf (documentation 'g-source-set-priority 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @argument[priority]{the new priority.}
  @begin{short}
    Sets the priority of a source.
  @end{short}
  While the main loop is being run, a source will be dispatched if it is ready
  to be dispatched and no sources at a higher (numerically smaller) priority are
  ready to be dispatched.")

;;; --- g-source-get-priority --------------------------------------------------

(setf (documentation 'g-source-get-priority 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @return{the priority of the source}
  @short{Gets the priority of a source.}")

;;; --- g-source-set-can-recurse -----------------------------------------------

(setf (documentation 'g-source-set-can-recurse 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @argument[can_recurse]{whether recursion is allowed for this source}
  @begin{short}
    Sets whether a source can be called recursively.
  @end{short}
  If can_recurse is TRUE, then while the source is being dispatched then this
  source will be processed normally. Otherwise, all processing of this source is
  blocked until the dispatch function returns.")

;;; --- g-source-get-can-recurse -----------------------------------------------

(setf (documentation 'g-source-get-can-recurse 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @return{whether recursion is allowed.}
  @begin{short}
    Checks whether a source is allowed to be called recursively.
  @end{short}
  See g_source_set_can_recurse().")

;;; --- g-source-get-id --------------------------------------------------------

(setf (documentation 'g-source-get-id 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @return{the ID (greater than 0) for the source}
  @begin{short}
    Returns the numeric ID for a particular source.
  @end{short}
  The ID of a source is a positive integer which is unique within a particular
  main loop context. The reverse mapping from ID to source is done by
  g_main_context_find_source_by_id().")

;;; --- g-source-get-name ------------------------------------------------------

(setf (documentation 'g-source-get-name 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @return{the name of the source}
  @begin{short}
    Gets a name for the source, used in debugging and profiling.
  @end{short}
  The name may be NULL if it has never been set with g_source_set_name().

  Since 2.26")

;;; --- g-source-set-name ------------------------------------------------------

(setf (documentation 'g-source-set-name 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @argument[name]{debug name for the source}
  @begin{short}
    Sets a name for the source, used in debugging and profiling.
  @end{short}
  The name defaults to NULL.

  The source name should describe in a human-readable way what the source
  does. For example, \"X11 event queue\" or \"GTK+ repaint idle handler\" or
  whatever it is.

  It is permitted to call this function multiple times, but is not recommended
  due to the potential performance impact. For example, one could change the
  name in the \"check\" function of a GSourceFuncs to include details like the
  event type in the source name.

  Since 2.26")

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

;;; --- g-source-get-context ---------------------------------------------------

(setf (documentation 'g-source-get-context 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @return{the GMainContext with which the source is associated, or NULL if the
    context has not yet been added to a source}
  @begin{short}
    Gets the GMainContext with which the source is associated.
  @end{short}
  Calling this function on a destroyed source is an error.")

;;; --- g-source-set-callback --------------------------------------------------

(setf (documentation 'g-source-set-callback 'function)
 "@version{2013-01-01}
  @argument[source]{the source}
  @argument[func]{a callback function}
  @argument[data]{the data to pass to callback function}
  @argument[notify]{a function to call when data is no longer in use, or NULL}
  @begin{short}
    Sets the callback function for a source.
  @end{short}
  The callback for a source is called from the source's dispatch function.

  The exact type of func depends on the type of source; ie. you should not
  count on func being called with data as its first parameter.

  Typically, you won't use this function. Instead use functions specific to
  the type of source you are using.")

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

;;; --- g-source-add-poll ------------------------------------------------------

(setf (documentation 'g-source-add-poll 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @argument[fd]{a GPollFD structure holding information about a file descriptor
   to watch.}
  @begin{short}
    Adds a file descriptor to the set of file descriptors polled for this
    source.
  @end{short}
  This is usually combined with g_source_new() to add an event source.
  The event source's check function will typically test the revents field in
  the GPollFD struct and return TRUE if events need to be processed.")

;;; --- g-source-remove-poll ---------------------------------------------------

(setf (documentation 'g-source-remove-poll 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @argument[fd]{a GPollFD structure previously passed to g_source_add_poll().}
  @begin{short}
    Removes a file descriptor from the set of file descriptors polled for this
    source.
  @end{short}")

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

;;; --- g-source-get-time ------------------------------------------------------

(setf (documentation 'g-source-get-time 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @return{the monotonic time in microseconds}
  @begin{short}
    Gets the time to be used when checking this source.
  @end{short}
  The advantage of calling this function over calling g_get_monotonic_time()
  directly is that when checking multiple sources, GLib can cache a single value
  instead of having to repeatedly get the system monotonic time.

  The time here is the system monotonic time, if available, or some other
  reasonable alternative otherwise. See g_get_monotonic_time().

  Since 2.28")

;;; --- g-source-get-current-time ----------------------------------------------

(setf (documentation 'g-source-get-current-time 'function)
 "@version{2013-01-01}
  @argument[source]{a GSource}
  @argument[timeval]{GTimeVal structure in which to store current time.}
  @begin[Warning]{dictionary}
    @sym{g-source-get-current-time} has been deprecated since version 2.28 and
    should not be used in newly-written code. Use @fun{g-source-get-time}
    instead.

    This function ignores source and is otherwise the same as
    @fun{g-get-current-time}.
  @end{dictionary}
  @see-function{g-source-get-time}")

;;; --- g-source-remove --------------------------------------------------------

(setf (documentation 'g-source-remove 'function)
 "@version{2013-01-01}
  @argument[tag]{the ID of the source to remove.}
  @return{TRUE if the source was found and removed.}
  @begin{short}
    Removes the source with the given id from the default main context.
  @end{short}
  The id of a GSource is given by g_source_get_id(), or will be returned by the
  functions g_source_attach(), g_idle_add(), g_idle_add_full(),
  g_timeout_add(), g_timeout_add_full(), g_child_watch_add(),
  g_child_watch_add_full(), g_io_add_watch(), and g_io_add_watch_full().

  See also g_source_destroy(). You must use g_source_destroy() for sources
  added to a non-default main context.")

;;; --- g-source-remove-by-funcs-user-data -------------------------------------

(setf (documentation 'g-source-remove-by-funcs-user-data 'function)
 "@version{2013-01-01}
  @argument[funcs]{The source_funcs passed to g_source_new()}
  @argument[user_data]{the user data for the callback}
  @return{TRUE if a source was found and removed.}
  @begin{short}
    Removes a source from the default main loop context given the source
    functions and user data.
  @end{short}
  If multiple sources exist with the same source functions and user data, only
  one will be destroyed.")

;;; --- g-source-remove-by-user-data -------------------------------------------

(setf (documentation 'g-source-remove-by-user-data 'function)
 "@version{2013-01-01}
  @argument[user_data]{the user_data for the callback.}
  @return{TRUE if a source was found and removed.}
  @begin{short}
    Removes a source from the default main loop context given the user data for
    the callback.
  @end{short}
  If multiple sources exist with the same user data, only one will be
  destroyed.")

;;; --- End of file glib.main-loop.lisp  ---------------------------------------
