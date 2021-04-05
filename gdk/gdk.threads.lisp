;;; ----------------------------------------------------------------------------
;;; gdk.threads.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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
;;; Threads
;;;
;;;     Functions for using GDK in multi-threaded programs
;;;
;;; Functions
;;;
;;;     GDK_THREADS_ENTER
;;;     GDK_THREADS_LEAVE
;;;
;;;     gdk_threads_init
;;;     gdk_threads_enter
;;;     gdk_threads_leave
;;;     gdk_threads_set_lock_functions
;;;     gdk_threads_add_idle
;;;     gdk_threads_add_idle_full
;;;     gdk_threads_add_timeout
;;;     gdk_threads_add_timeout_full
;;;     gdk_threads_add_timeout_seconds
;;;     gdk_threads_add_timeout_seconds_full
;;;
;;; Description
;;;
;;; For thread safety, GDK relies on the thread primitives in GLib, and on the
;;; thread-safe GLib main loop.
;;;
;;; GLib is completely thread safe (all global data is automatically locked),
;;; but individual data structure instances are not automatically locked for
;;; performance reasons. So e.g. you must coordinate accesses to the same
;;; GHashTable from multiple threads.
;;;
;;; GLib is completely thread safe (all global data is automatically locked),
;;; but individual data structure instances are not automatically locked for
;;; performance reasons. So e.g. you must coordinate accesses to the same
;;; GHashTable from multiple threads.
;;;
;;; GTK+, however, is not thread safe. You should only use GTK+ and GDK from
;;; the thread gtk_init() and gtk_main() were called on. This is usually
;;; referred to as the “main thread”.
;;;
;;; Signals on GTK+ and GDK types, as well as non-signal callbacks, are emitted
;;; in the main thread.
;;;
;;; You can schedule work in the main thread safely from other threads by using
;;; gdk_threads_add_idle() and gdk_threads_add_timeout():
;;;
;;; static void
;;; worker_thread (void)
;;; {
;;;   ExpensiveData *expensive_data = do_expensive_computation ();
;;;
;;;   gdk_threads_add_idle (got_value, expensive_data);
;;; }
;;;
;;; static gboolean
;;; got_value (gpointer user_data)
;;; {
;;;   ExpensiveData *expensive_data = user_data;
;;;
;;;   my_app->expensive_data = expensive_data;
;;;   gtk_button_set_sensitive (my_app->button, TRUE);
;;;   gtk_button_set_label (my_app->button, expensive_data->result_label);
;;;
;;;   return G_SOURCE_REMOVE;
;;; }
;;;
;;; You should use gdk_threads_add_idle() and gdk_threads_add_timeout() instead
;;; of g_idle_add() and g_timeout_add() since libraries not under your control
;;; might be using the deprecated GDK locking mechanism. If you are sure that
;;; none of the code in your application and libraries use the deprecated
;;; gdk_threads_enter() or gdk_threads_leave() methods, then you can safely use
;;; g_idle_add() and g_timeout_add().
;;;
;;; For more information on this "worker thread" pattern, you should also look
;;; at GTask, which gives you high-level tools to perform expensive tasks from
;;; worker threads, and will handle thread management for you.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------

;;; A marco to execute code between gdk-threads-enter and gdk-threads-leave

(defmacro with-gdk-threads-lock (&body body)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  A Lisp macro to execute @arg{body} between the functions
  @fun{gdk-threads-enter} and @fun{gdk-threads-leave}."
  `(progn
     (gdk-threads-enter)
     (unwind-protect
       (progn ,@body)
       (gdk-threads-leave))))

(export 'with-gdk-threads-lock)

;;; ----------------------------------------------------------------------------

;; Callback function for:
;;
;;     gdk-threads-add-idle-full
;;     gdk-threads-add-timeout-full
;;     gdk-threads-add-timeout-seconds-full

;(defcallback source-func-cb :boolean
;    ((data :pointer))
;  (funcall (glib::get-stable-pointer-value data)))

;;; ----------------------------------------------------------------------------
;;; GDK_THREADS_ENTER
;;;
;;; #define GDK_THREADS_ENTER() gdk_threads_enter()
;;;
;;; GDK_THREADS_ENTER has been deprecated since version 3.6 and should not be
;;; used in newly-written code. Use g_main_context_invoke(), g_idle_add() and
;;; related functions if you need to schedule GTK+ calls from other threads.
;;;
;;; This macro marks the beginning of a critical section in which GDK and GTK+
;;; functions can be called safely and without causing race conditions. Only one
;;; thread at a time can be in such a critial section. The macro expands to a
;;; no-op if G_THREADS_ENABLED has not been defined. Typically
;;; gdk_threads_enter() should be used instead of this macro.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_THREADS_LEAVE
;;;
;;; #define GDK_THREADS_LEAVE() gdk_threads_leave()
;;;
;;; GDK_THREADS_LEAVE has been deprecated since version 3.6 and should not be
;;; used in newly-written code. Deprecated in 3.6.
;;;
;;; This macro marks the end of a critical section begun with GDK_THREADS_ENTER.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_threads_init ()
;;; ----------------------------------------------------------------------------

;; not exported

(defcfun ("gdk_threads_init" gdk-threads-init) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @begin{short}
    Initializes GDK so that it can be used from multiple threads in conjunction
    with the functions @fun{gdk-threads-enter} and @fun{gdk-threads-leave}.
  @end{short}

  This call must be made before any use of the main loop from GTK+; to be
  safe, call it before the function @code{gtk_init()}.
  @begin[Warning]{dictionary}
    The function @sym{gdk-threads-init} has been deprecated since version 3.6
    and should not be used in newly-written code. All GDK and GTK+ calls should
    be made from the main thread.
  @end{dictionary}
  @see-function{gdk-threads-enter}
  @see-function{gdk-threads-leave}")

;;; ----------------------------------------------------------------------------
;;; gdk_threads_enter ()
;;; ----------------------------------------------------------------------------

;; not exported

(defcfun ("gdk_threads_enter" gdk-threads-enter) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @begin{short}
    This function marks the beginning of a critical section in which GDK and
    GTK+ functions can be called safely and without causing race conditions.
    Only one thread at a time can be in such a critial section.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-threads-enter} has been deprecated since version 3.6
    and should not be used in newly-written code. All GDK and GTK+ calls should
    be made from the main thread.
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gdk_threads_leave ()
;;; ----------------------------------------------------------------------------

;; not exported

(defcfun ("gdk_threads_leave" gdk-threads-leave) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @begin{short}
    Leaves a critical region begun with the function @fun{gdk-threads-enter}.
  @end{short}
  @begin[Warning]{dictionary}
    The function @sym{gdk-threads-leave} has been deprecated since version 3.6
    and should not be used in newly-written code. All GDK and GTK+ calls should
    be made from the main thread.
  @end{dictionary}
  @see-function{gdk-threads-enter}")

;;; ----------------------------------------------------------------------------
;;; gdk_threads_set_lock_functions ()
;;;
;;; void gdk_threads_set_lock_functions (GCallback enter_fn, GCallback leave_fn)
;;;
;;; Warning
;;;
;;; gdk_threads_set_lock_functions has been deprecated since version 3.6 and
;;; should not be used in newly-written code. All GDK and GTK+ calls should be
;;; made from the main thread
;;;
;;; Allows the application to replace the standard method that GDK uses to
;;; protect its data structures. Normally, GDK creates a single GMutex that is
;;; locked by gdk_threads_enter(), and released by gdk_threads_leave(); using
;;; this function an application provides, instead, a function enter_fn that is
;;; called by gdk_threads_enter() and a function leave_fn that is called by
;;; gdk_threads_leave().
;;;
;;; The functions must provide at least same locking functionality as the
;;; default implementation, but can also do extra application specific
;;; processing.
;;;
;;; As an example, consider an application that has its own recursive lock that
;;; when held, holds the GTK+ lock as well. When GTK+ unlocks the GTK+ lock when
;;; entering a recursive main loop, the application must temporarily release its
;;; lock as well.
;;;
;;; Most threaded GTK+ apps won't need to use this method.
;;;
;;; This method must be called before gdk_threads_init(), and cannot be called
;;; multiple times.
;;;
;;; enter_fn :
;;;     function called to guard GDK
;;;
;;; leave_fn :
;;;     function called to release the guard
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_idle ()
;;; ----------------------------------------------------------------------------

(defun gdk-threads-add-idle (func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-3}
  @argument[func]{a @symbol{g-source-func} callback function to call}
  @return{An unsigned integer ID, greater than 0, of the event source.}
  @begin{short}
    A wrapper for the common usage of the function
    @fun{gdk-threads-add-idle-full} assigning the default priority,
    @var{+g-priority-default-idle+}.
  @end{short}
  @see-symbol{g-source-func}
  @see-function{gdk-threads-add-idle-full}"
  (%gdk-threads-add-idle-full +g-priority-default-idle+
                              (callback g-source-func)
                              (allocate-stable-pointer func)
                              (callback stable-pointer-destroy-notify-cb)))

(export 'gdk-threads-add-idle)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_idle_full ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_threads_add_idle_full" %gdk-threads-add-idle-full) :uint
  (priority :int)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

;; The Lisp implementation does not support the arguments data and notify.
;; The argument data is used in %gdk-threads-idle-full to pass the Lisp
;; function func and the argument notify is used to pass the function
;; stable-pointer-destroy-notify-cb which frees the allocated pointer
;; to the Lisp function func.

(defun gdk-threads-add-idle-full (priority func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-3}
  @argument[priority]{an integer with the priority of the idle source,
    typically this will be in the range between @var{+g-priority-default-idle+}
    and @var{+g-priority-high-idle+}}
  @argument[func]{a @symbol{g-source-func} callback function to call}
  @return{An unsigned integer with the ID, greater than 0, of the event source.}
  @begin{short}
    Adds a function to be called whenever there are no higher priority events
    pending.
  @end{short}
  If the function returns @em{false} it is automatically removed from the list
  of event sources and will not be called again.

  This variant of the function @sym{g-idle-add-full} calls the function with
  the GDK lock held. It can be thought of a MT-safe version for GTK+ widgets
  for the following use case, where you have to worry about
  @code{idle_callback()} running in thread A and accessing self after it has
  been finalized in thread B:
  @begin{pre}
static gboolean
idle_callback (gpointer data)
{
   /* gdk_threads_enter(); would be needed for g_idle_add() */

   SomeWidget *self = data;
   /* do stuff with self */

   self->idle_id = 0;

   /* gdk_threads_leave(); would be needed for g_idle_add() */
   return FALSE;
@}

static void
some_widget_do_stuff_later (SomeWidget *self)
{
   self->idle_id = gdk_threads_add_idle (idle_callback, self)
   /* using g_idle_add() here would require thread protection in the
      callback */
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
  @see-symbol{g-source-func}
  @see-function{gdk-threads-add-idle}"
  (%gdk-threads-add-idle-full priority
                              (callback g-source-func)
                              (allocate-stable-pointer func)
                              (callback stable-pointer-destroy-notify-cb)))

(export 'gdk-threads-add-idle-full)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_timeout ()
;;; ----------------------------------------------------------------------------

;; The Lisp implementation does not support the argument data.

(defun gdk-threads-add-timeout (interval func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-3}
  @argument[interval]{an unsigned integer with the time between calls to the
    function, in milliseconds (1/1000ths of a second)}
  @argument[func]{a @symbol{g-source-func} callback function to call}
  @return{An unsigned integer ID, greater than 0, of the event source.}
  @begin{short}
    A wrapper for the common usage of the function
    @fun{gdk-threads-add-timeout-full} assigning the default priority,
    @var{+g-priority-default+}.
  @end{short}
  @see-symbol{g-source-func}
  @see-function{gdk-threads-add-timeout-full}"
  (%gdk-threads-add-timeout-full +g-priority-default+
                                 interval
                                 (callback g-source-func)
                                 (allocate-stable-pointer func)
                                 (callback stable-pointer-destroy-notify-cb)))

(export 'gdk-threads-add-timeout)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_timeout_full ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_threads_add_timeout_full" %gdk-threads-add-timeout-full) :uint
  (priority :int)
  (interval :uint)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

;; The Lisp implementation does not support the arguments data and notify.

(defun gdk-threads-add-timeout-full (priority interval func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-3}
  @argument[priority]{an integer with the priority of the timeout source,
    typically this will be in the range between @var{+g-priority-default-idle+}
    and @var{+g-priority-high-idle+}}
  @argument[interval]{an unsigned integer with the time between calls to the
    function, in milliseconds (1/1000ths of a second)}
  @argument[func]{a @symbol{g-source-func} callback function to call}
  @return{An unsigned integer ID, greater than 0, of the event source.}
  @begin{short}
    Sets a function to be called at regular intervals holding the GDK lock,
    with the given priority.
  @end{short}
  The function is called repeatedly until it returns @em{false}, at which point
  the timeout is automatically destroyed and the function will not be called
  again. The notify function is called when the timeout is destroyed. The first
  call to the function will be at the end of the first interval.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval It does not
  try to 'catch up' time lost in delays.

  This variant of the function @code{g_timeout_add_full()} can be thought of
  a MT-safe version for GTK+ widgets for the following use case:
  @begin{pre}
static gboolean timeout_callback (gpointer data)
{
   SomeWidget *self = data;

   /* do stuff with self */

   self->timeout_id = 0;

   return G_SOURCE_REMOVE;
@}

static void some_widget_do_stuff_later (SomeWidget *self)
{
   self->timeout_id = g_timeout_add (timeout_callback, self)
@}

static void some_widget_finalize (GObject *object)
{
   SomeWidget *self = SOME_WIDGET (object);

   if (self->timeout_id)
     g_source_remove (self->timeout_id);

   G_OBJECT_CLASS (parent_class)->finalize (object);
@}
  @end{pre}
  @see-symbol{g-source-func}
  @see-function{gdk-threads-add-timeout}"
  (%gdk-threads-add-timeout-full priority
                                 interval
                                 (callback g-source-func)
                                 (allocate-stable-pointer func)
                                 (callback stable-pointer-destroy-notify-cb)))

(export 'gdk-threads-add-timeout-full)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_timeout_seconds ()
;;; ----------------------------------------------------------------------------

;; The Lisp implementation does not support the argument data.

(defun gdk-threads-add-timeout-seconds (interval func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-3}
  @argument[interval]{an unsigned integer with the time between calls to the
    function, in seconds}
  @argument[func]{a @symbol{g-source-func} callback function to call}
  @return{An unsigned integer ID, greater than 0, of the event source.}
  @begin{short}
    A wrapper for the common usage of the function
    @fun{gdk-threads-add-timeout-seconds-full} assigning the default priority,
    @var{+g-priority-default+}.
  @end{short}
  @see-symbol{g-source-func}
  @see-function{gdk-threads-add-timeout-full}"
  (%gdk-threads-add-timeout-seconds-full
                                 +g-priority-default+
                                 interval
                                 (callback g-source-func)
                                 (allocate-stable-pointer func)
                                 (callback stable-pointer-destroy-notify-cb)))

(export 'gdk-threads-add-timeout-seconds)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_timeout_seconds_full ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_threads_add_timeout_seconds_full"
          %gdk-threads-add-timeout-seconds-full) :uint
  (priority :int)
  (interval :uint)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

;; The Lisp implementation does not support the arguments data and notify.

(defun gdk-threads-add-timeout-seconds-full (priority interval func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-3}
  @argument[priority]{an integer with the priority of the timeout source,
    typically this will be in the range between @var{+g-priority-default-idle+}
    and @var{+g-priority-high-idle+}}
  @argument[interval]{an unsigned integer with the time between calls to the
    function, in seconds}
  @argument[func]{a @symbol{g-source-func} callback function to call}
  @return{An unsigned integer ID, greater than 0, of the event source.}
  @begin{short}
    A variant of the function @fun{gdk-threads-add-timeout-full} with
    second-granularity.
  @end{short}
  See the function @fun{g-timeout-add-seconds} for a discussion of why it
  is a good idea to use this function if you do not need finer granularity.
  @see-symbol{g-source-func}
  @see-function{gdk-threads-add-timeout-full}"
  (%gdk-threads-add-timeout-seconds-full
                                 priority
                                 interval
                                 (callback g-source-func)
                                 (allocate-stable-pointer func)
                                 (callback stable-pointer-destroy-notify-cb)))

(export 'gdk-threads-add-timeout-seconds-full)

;;; --- End of file gdk.threads.lisp -------------------------------------------
