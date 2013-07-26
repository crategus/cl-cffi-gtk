;;; ----------------------------------------------------------------------------
;;; gdk.threads.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GDK 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;;
;;; Threads
;;;
;;; Functions for using GDK in multi-threaded programs
;;;
;;; Synopsis
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
;;; GTK+ is "thread aware" but not thread safe — it provides a global lock
;;; controlled by gdk_threads_enter()/gdk_threads_leave() which protects all use
;;; of GTK+. That is, only one thread can use GTK+ at any given time.
;;;
;;; Unfortunately the above holds with the X11 backend only. With the Win32
;;; backend, GDK calls should not be attempted from multiple threads at all.
;;;
;;; You must call gdk_threads_init() before executing any other GTK+ or GDK
;;; functions in a threaded GTK+ program.
;;;
;;; Idles, timeouts, and input functions from GLib, such as g_idle_add(), are
;;; executed outside of the main GTK+ lock. So, if you need to call GTK+ inside
;;; of such a callback, you must surround the callback with a
;;; gdk_threads_enter()/gdk_threads_leave() pair or use
;;; gdk_threads_add_idle_full() which does this for you. However, event
;;; dispatching from the mainloop is still executed within the main GTK+ lock,
;;; so callback functions connected to event signals like "button-press-event",
;;; do not need thread protection.
;;;
;;; In particular, this means, if you are writing widgets that might be used in
;;; threaded programs, you must surround timeouts and idle functions in this
;;; matter.
;;;
;;; As always, you must also surround any calls to GTK+ not made within a signal
;;; handler with a gdk_threads_enter()/gdk_threads_leave() pair.
;;;
;;; Before calling gdk_threads_leave() from a thread other than your main
;;; thread, you probably want to call gdk_flush() to send all pending commands
;;; to the windowing system. (The reason you don't need to do this from the main
;;; thread is that GDK always automatically flushes pending commands when it
;;; runs out of incoming events to process and has to sleep while waiting for
;;; more events.)
;;;
;;; A minimal main program for a threaded GTK+ application looks like:
;;;
;;;   int
;;;   main (int argc, char *argv[])
;;;   {
;;;     GtkWidget *window;
;;;
;;;     gdk_threads_init ();
;;;     gdk_threads_enter ();
;;;
;;;     gtk_init (&argc, &argv);
;;;
;;;     window = create_window ();
;;;     gtk_widget_show (window);
;;;
;;;     gtk_main ();
;;;     gdk_threads_leave ();
;;;
;;;     return 0;
;;;   }
;;;
;;; Callbacks require a bit of attention. Callbacks from GTK+ signals are made
;;; within the GTK+ lock. However callbacks from GLib (timeouts, IO callbacks,
;;; and idle functions) are made outside of the GTK+ lock. So, within a signal
;;; handler you do not need to call gdk_threads_enter(), but within the other
;;; types of callbacks, you do.
;;;
;;; Erik Mouw contributed the following code example to illustrate how to use
;;; threads within GTK+ programs.
;;;
;;;   /*------------------------------------------------------------------------
;;;    * Filename:      gtk-thread.c
;;;    * Version:       0.99.1
;;;    * Copyright:     Copyright (C) 1999, Erik Mouw
;;;    * Author:        Erik Mouw <J.A.K.Mouw@its.tudelft.nl>
;;;    * Description:   GTK threads example.
;;;    * Created at:    Sun Oct 17 21:27:09 1999
;;;    * Modified by:   Erik Mouw <J.A.K.Mouw@its.tudelft.nl>
;;;    * Modified at:   Sun Oct 24 17:21:41 1999
;;;    *----------------------------------------------------------------------*/
;;;   /*
;;;    * Compile with:
;;;    *
;;;    * cc -o gtk-thread gtk-thread.c `gtk-config --cflags --libs gthread`
;;;    *
;;;    * Thanks to Sebastian Wilhelmi and Owen Taylor for pointing out some
;;;    * bugs.
;;;    *
;;;    */
;;;
;;;   #include <stdio.h>
;;;   #include <stdlib.h>
;;;   #include <unistd.h>
;;;   #include <time.h>
;;;   #include <gtk/gtk.h>
;;;   #include <glib.h>
;;;   #include <pthread.h>
;;;
;;;   #define YES_IT_IS    (1)
;;;   #define NO_IT_IS_NOT (0)
;;;
;;;   typedef struct
;;;   {
;;;     GtkWidget *label;
;;;     int what;
;;;   } yes_or_no_args;
;;;
;;;   G_LOCK_DEFINE_STATIC (yes_or_no);
;;;   static volatile int yes_or_no = YES_IT_IS;
;;;
;;;   void destroy (GtkWidget *widget, gpointer data)
;;;   {
;;;     gtk_main_quit ();
;;;   }
;;;
;;;   void *argument_thread (void *args)
;;;   {
;;;     yes_or_no_args *data = (yes_or_no_args *)args;
;;;     gboolean say_something;
;;;
;;;     for (;;)
;;;       {
;;;         /* sleep a while */
;;;         sleep(rand() / (RAND_MAX / 3) + 1);
;;;
;;;         /* lock the yes_or_no_variable */
;;;         G_LOCK(yes_or_no);
;;;
;;;         /* do we have to say something? */
;;;         say_something = (yes_or_no != data->what);
;;;
;;;         if(say_something)
;;;        {
;;;          /* set the variable */
;;;          yes_or_no = data->what;
;;;        }
;;;
;;;         /* Unlock the yes_or_no variable */
;;;         G_UNLOCK (yes_or_no);
;;;
;;;         if (say_something)
;;;        {
;;;          /* get GTK thread lock */
;;;          gdk_threads_enter ();
;;;
;;;          /* set label text */
;;;          if(data->what == YES_IT_IS)
;;;            gtk_label_set_text (GTK_LABEL (data->label), "O yes, it is!");
;;;          else
;;;            gtk_label_set_text (GTK_LABEL (data->label), "O no, it isn't!");
;;;
;;;          /* release GTK thread lock */
;;;          gdk_threads_leave ();
;;;        }
;;;       }
;;;
;;;     return NULL;
;;;   }
;;;
;;;   int main (int argc, char *argv[])
;;;   {
;;;     GtkWidget *window;
;;;     GtkWidget *label;
;;;     yes_or_no_args yes_args, no_args;
;;;     pthread_t no_tid, yes_tid;
;;;
;;;     /* init threads */
;;;     gdk_threads_init ();
;;;     gdk_threads_enter ();
;;;
;;;     /* init gtk */
;;;     gtk_init(&argc, &argv);
;;;
;;;     /* init random number generator */
;;;     srand ((unsigned int) time (NULL));
;;;
;;;     /* create a window */
;;;     window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;
;;;     g_signal_connect (window, "destroy", G_CALLBACK (destroy), NULL);
;;;
;;;     gtk_container_set_border_width (GTK_CONTAINER (window), 10);
;;;
;;;     /* create a label */
;;;     label = gtk_label_new("And now for something completely different ...");
;;;     gtk_container_add (GTK_CONTAINER (window), label);
;;;
;;;     /* show everything */
;;;     gtk_widget_show (label);
;;;     gtk_widget_show (window);
;;;
;;;     /* create the threads */
;;;     yes_args.label = label;
;;;     yes_args.what = YES_IT_IS;
;;;     pthread_create (&yes_tid, NULL, argument_thread, &yes_args);
;;;
;;;     no_args.label = label;
;;;     no_args.what = NO_IT_IS_NOT;
;;;     pthread_create (&no_tid, NULL, argument_thread, &no_args);
;;;
;;;     /* enter the GTK main loop */
;;;     gtk_main ();
;;;     gdk_threads_leave ();
;;;
;;;     return 0;
;;;   }
;;;
;;; Unfortunately, all of the above documentation holds with the X11 backend
;;; only. With the Win32 backend, GDK and GTK+ calls should not be attempted
;;; from multiple threads at all. Combining the GDK lock with other locks such
;;; as the Python global interpreter lock can be complicated.
;;;
;;; For these reason, the threading support has been deprecated in GTK+ 3.6.
;;; Instead of calling GTK+ directly from multiple threads, it is recommended to
;;; use g_idle_add(), g_main_context_invoke() and similar functions to make
;;; these calls from the main thread instead. The main thread is the thread
;;; which has called gtk_init() and is running the GTK+ mainloop. GTK+ itself
;;; will continue to use the GDK lock internally as long as the deprecated
;;; functionality is still available, and other libraries should probably do
;;; the same.
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

(defcallback source-func-cb :boolean
    ((data :pointer))
  (funcall (glib::get-stable-pointer-value data)))

;;; ----------------------------------------------------------------------------
;;; GDK_THREADS_ENTER
;;;
;;; #define GDK_THREADS_ENTER() gdk_threads_enter()
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
;;; This macro marks the end of a critical section begun with GDK_THREADS_ENTER.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_threads_init ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_threads_init" gdk-threads-init) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @subheading{Warning}
    The function @sym{gdk-threads-init} has been deprecated since version 3.6
    and should not be used in newly-written code. All GDK and GTK+ calls should
    be made from the main thread.

  @begin{short}
    Initializes GDK so that it can be used from multiple threads in conjunction
    with the functions @fun{gdk-threads-enter} and @fun{gdk-threads-leave}.
  @end{short}

  This call must be made before any use of the main loop from GTK+; to be
  safe, call it before the function @code{gtk_init()}.
  @see-function{gdk-threads-enter}
  @see-function{gdk-threads-leave}")

(glib::at-init () (gdk-threads-init))

(export 'gdk-threads-init)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_enter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_threads_enter" gdk-threads-enter) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @subheading{Warning}
    The function @sym{gdk-threads-enter} has been deprecated since version 3.6
    and should not be used in newly-written code. All GDK and GTK+ calls should
    be made from the main thread.

  @begin{short}
    This function marks the beginning of a critical section in which GDK and
    GTK+ functions can be called safely and without causing race conditions.
    Only one thread at a time can be in such a critial section.
  @end{short}")

(export 'gdk-threads-enter)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_leave ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_threads_leave" gdk-threads-leave) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @subheading{Warning}
    The function @sym{gdk-threads-leave} has been deprecated since version 3.6
    and should not be used in newly-written code. All GDK and GTK+ calls should
    be made from the main thread.

  @begin{short}
    Leaves a critical region begun with the function @fun{gdk-threads-enter}.
  @end{short}
  @see-function{gdk-threads-enter}")

(export 'gdk-threads-leave)

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
 "@version{2013-6-17}
  @argument[func]{function to call}
  @return{The ID (greater than 0) of the event source.}
  @begin{short}
    A wrapper for the common usage of the function
    @fun{gdk-threads-add-idle-full} assigning the default priority,
    @var{g-priority-default-idle}.
  @end{short}

  See the function @fun{gdk-threads-add-idle-full}.

  Since 2.12
  @see-function{gdk-threads-add-idle-full}"
  (%gdk-threads-add-idle-full g-priority-default-idle
                             (callback source-func-cb)
                             (glib::allocate-stable-pointer func)
                             (callback glib::stable-pointer-destroy-notify-cb)))

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
 "@version{2013-6-17}
  @argument[priority]{the priority of the idle source. Typically this will be in
    the range between @var{g-priority-default-idle} and
    @var{g-priority-high-idle}.}
  @argument[func]{function to call}
  @return{The ID (greater than 0) of the event source.}
  @begin{short}
    Adds a function to be called whenever there are no higher priority events
    pending. If the function returns @code{nil} it is automatically removed from
    the list of event sources and will not be called again.
  @end{short}

  This variant of the function @fun{g-idle-add-full} calls function with the
  GDK lock held. It can be thought of a MT-safe version for GTK+ widgets for the
  following use case, where you have to worry about @code{idle_callback()}
  running in thread A and accessing self after it has been finalized in
  thread B:
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
  Since 2.12"
  (%gdk-threads-add-idle-full priority
                              (callback source-func-cb)
                              (glib::allocate-stable-pointer func)
                              (callback stable-pointer-destroy-notify-cb)))

(export 'gdk-threads-add-idle-full)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_timeout ()
;;; ----------------------------------------------------------------------------

;; The Lisp implementation does not support the argument data.

(defun gdk-threads-add-timeout (interval func)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[interval]{the time between calls to the function, in milliseconds
    (1/1000ths of a second)}
  @argument[func]{function to call}
  @return{The ID (greater than 0) of the event source.}
  @begin{short}
    A wrapper for the common usage of the function
    @fun{gdk-threads-add-timeout-full} assigning the default priority,
    @var{g-priority-default}.
  @end{short}

  See the function @fun{gdk-threads-add-timeout-full}.

  Since 2.12
  @see-function{gdk-threads-add-timeout-full}"
  (%gdk-threads-add-timeout-full
                             g-priority-default
                             interval
                             (callback source-func-cb)
                             (glib::allocate-stable-pointer func)
                             (callback glib::stable-pointer-destroy-notify-cb)))

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
 "@version{2013-6-17}
  @argument[priority]{the priority of the timeout source. Typically this will
    be in the range between @var{g-priority-default-idle} and
    @var{g-priority-high-idle}.}
  @argument[interval]{the time between calls to the function, in milliseconds
    (1/1000ths of a second)}
  @argument[func]{function to call}
  @return{The ID (greater than 0) of the event source.}
  @begin{short}
    Sets a function to be called at regular intervals holding the GDK lock, with
    the given priority. The function is called repeatedly until it returns
    @code{nil}, at which point the timeout is automatically destroyed and the
    function will not be called again. The notify function is called when the
    timeout is destroyed. The first call to the function will be at the end of
    the first interval.
  @end{short}

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval (it does not
  try to 'catch up' time lost in delays).

  This variant of the function @fun{g-timeout-add-full} can be thought of a
  MT-safe version for GTK+ widgets for the following use case:
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

  Since 2.12"
  (%gdk-threads-add-timeout-full priority
                                 interval
                                 (callback source-func-cb)
                                 (glib::allocate-stable-pointer func)
                                 (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gdk-threads-add-timeout-full)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_timeout_seconds ()
;;; ----------------------------------------------------------------------------

;; The Lisp implementation does not support the argument data.

(defun gdk-threads-add-timeout-seconds (interval func)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-17}
  @argument[interval]{the time between calls to the function, in seconds}
  @argument[func]{function to call}
  @return{The ID (greater than 0) of the event source.}
  @begin{short}
    A wrapper for the common usage of the function
    @fun{gdk-threads-add-timeout-seconds-full} assigning the default priority,
    @var{g-priority-default}.
  @end{short}

  For details, see the function @fun{gdk-threads-add-timeout-full}.

  Since 2.14
  @see-function{gdk-threads-add-timeout-full}"
  (%gdk-threads-add-timeout-seconds-full
                             g-priority-default
                             interval
                             (callback source-func-cb)
                             (glib::allocate-stable-pointer func)
                             (callback glib::stable-pointer-destroy-notify-cb)))

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
 "@version{2013-6-17}
  @argument[priority]{the priority of the timeout source. Typically this will be
    in the range between @var{g-priority-default-idle} and
    @var{g-priority-high-idle}.}
  @argument[interval]{the time between calls to the function, in seconds}
  @argument[func]{function to call}
  @return{The ID (greater than 0) of the event source.}
  @begin{short}
    A variant of the function @fun{gdk-threads-add-timeout-full} with
    second-granularity.
  @end{short}
  See the function @fun{g-timeout-add-seconds-full} for a discussion of why it
  is a good idea to use this function if you do not need finer granularity.

  Since 2.14"
  (%gdk-threads-add-timeout-seconds-full
                             priority
                             interval
                             (callback source-func-cb)
                             (glib::allocate-stable-pointer func)
                             (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gdk-threads-add-timeout-seconds-full)

;;; --- End of file gdk.threads.lisp -------------------------------------------
