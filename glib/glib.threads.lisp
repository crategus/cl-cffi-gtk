;;; ----------------------------------------------------------------------------
;;; glib.threads.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation of this file has been copied from the
;;; GLib 2.30.2 Reference Manual.  See http://www.gtk.org.
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
;;; Threads
;;; 
;;; Thread abstraction; including threads, different mutexes, conditions and
;;; thread private data
;;; 
;;; Synopsis
;;;
;;;    g-cond
;;;    g-thread-error
;;;    g-thread-prior
;;;    g-thread
;;;
;;;    g-thread-init
;;;         
;;;    g-thread-get-initialized
;;;    g-thread-self
;;;    g-thread-join
;;;    g-thread-set-priority
;;;         
;;; The following symbols related to threads are not implemented:
;;;
;;;    GThreadFunctions
;;;    g_thread_supported
;;;    g_thread_create
;;;    g_thread_create_full
;;;    g_thread_yield
;;;    g_thread_exit
;;;    g_thread_foreach
;;; 
;;;    G_THREADS_ENABLED
;;;    G_THREADS_IMPL_POSIX
;;;    G_THREADS_IMPL
;;;    G_THREAD_ERROR
;;; 
;;; Only the following symbol is implemented for mutex:
;;;
;;;    g-mutex
;;; 
;;; Description
;;; 
;;; Threads act almost like processes, but unlike processes all threads of one
;;; process share the same memory. This is good, as it provides easy
;;; communication between the involved threads via this shared memory, and it
;;; is bad, because strange things (so called "Heisenbugs") might happen if the
;;; program is not carefully designed. In particular, due to the concurrent
;;; nature of threads, no assumptions on the order of execution of code running
;;; in different threads can be made, unless order is explicitly forced by the
;;; programmer through synchronization primitives.
;;; 
;;; The aim of the thread related functions in GLib is to provide a portable
;;; means for writing multi-threaded software. There are primitives for mutexes
;;; to protect the access to portions of memory (GMutex, GStaticMutex,
;;; G_LOCK_DEFINE, GStaticRecMutex and GStaticRWLock). There is a facility to
;;; use individual bits for locks (g_bit_lock()). There are primitives for
;;; condition variables to allow synchronization of threads (GCond). There are
;;; primitives for thread-private data - data that every thread has a private
;;; instance of (GPrivate, GStaticPrivate). There are facilities for one-time
;;; initialization (GOnce, g_once_init_enter()). Last but definitely not least
;;; there are primitives to portably create and manage threads (GThread).
;;; 
;;; The threading system is initialized with g_thread_init(), which takes an
;;; optional custom thread implementation or NULL for the default
;;; implementation. If you want to call g_thread_init() with a non-NULL
;;; argument this must be done before executing any other GLib functions
;;; (except g_mem_set_vtable()). This is a requirement even if no threads are
;;; in fact ever created by the process.
;;; 
;;; Calling g_thread_init() with a NULL argument is somewhat more relaxed. You
;;; may call any other glib functions in the main thread before g_thread_init()
;;; as long as g_thread_init() is not called from a glib callback, or with any
;;; locks held. However, many libraries above glib does not support late
;;; initialization of threads, so doing this should be avoided if possible.
;;; 
;;; Please note that since version 2.24 the GObject initialization function
;;; g_type_init() initializes threads (with a NULL argument), so most
;;; applications, including those using Gtk+ will run with threads enabled. If
;;; you want a special thread implementation, make sure you call g_thread_init()
;;; before g_type_init() is called.
;;; 
;;; After calling g_thread_init(), GLib is completely thread safe (all global
;;; data is automatically locked), but individual data structure instances are
;;; not automatically locked for performance reasons. So, for example you must
;;; coordinate accesses to the same GHashTable from multiple threads. The two
;;; notable exceptions from this rule are GMainLoop and GAsyncQueue, which are
;;; threadsafe and need no further application-level locking to be accessed from
;;; multiple threads.
;;; 
;;; To help debugging problems in multithreaded applications, GLib supports
;;; error-checking mutexes that will give you helpful error messages on common
;;; problems. To use error-checking mutexes, define the symbol
;;; G_ERRORCHECK_MUTEXES when compiling the application.
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; g-cond
;;; 
;;; typedef struct _GCond GCond;
;;; 
;;; The GCond struct is an opaque data structure that represents a condition.
;;; Threads can block on a GCond if they find a certain condition to be false.
;;; If other threads change the state of this condition they signal the GCond,
;;; and that causes the waiting threads to be woken up.
;;; 
;;; Example 8. Using GCond to block a thread until a condition is satisfied
;;; 
;;;  1 GCond* data_cond = NULL; /* Must be initialized somewhere */
;;;  2 GMutex* data_mutex = NULL; /* Must be initialized somewhere */
;;;  3 gpointer current_data = NULL;
;;;  4
;;;  5 void
;;;  6 push_data (gpointer data)
;;;  7 {
;;;  8   g_mutex_lock (data_mutex);
;;;  9   current_data = data;
;;; 10   g_cond_signal (data_cond);
;;; 11   g_mutex_unlock (data_mutex);
;;; 12 }
;;; 13
;;; 14 gpointer
;;; 15 pop_data (void)
;;; 16 {
;;; 17   gpointer data;
;;; 18
;;; 19   g_mutex_lock (data_mutex);
;;; 20   while (!current_data)
;;; 21     g_cond_wait (data_cond, data_mutex);
;;; 22   data = current_data;
;;; 23   current_data = NULL;
;;; 24   g_mutex_unlock (data_mutex);
;;; 25 
;;; 26   return data;
;;; 27 }
;;; 
;;; Whenever a thread calls pop_data() now, it will wait until current_data is
;;; non-NULL, i.e. until some other thread has called push_data().
;;; 
;;; Note
;;; 
;;; It is important to use the g_cond_wait() and g_cond_timed_wait() functions
;;; only inside a loop which checks for the condition to be true. It is not
;;; guaranteed that the waiting thread will find the condition fulfilled after
;;; it wakes up, even if the signaling thread left the condition in that state:
;;; another thread may have altered the condition before the waiting thread got
;;; the chance to be woken up, even if the condition itself is protected by a
;;; GMutex, like above.
;;; 
;;; A GCond should only be accessed via the following functions.
;;; 
;;; Note
;;; 
;;; All of the g_cond_* functions are actually macros. Apart from taking their
;;; addresses, you can however use them as if they were functions.
;;; ----------------------------------------------------------------------------

(defcstruct g-cond)

(export 'g-cond)

;;; ----------------------------------------------------------------------------
;;; enum GThreadError
;;; 
;;; typedef enum {
;;;   G_THREAD_ERROR_AGAIN /* Resource temporarily unavailable */
;;; } GThreadError;
;;; 
;;; Possible errors of thread related functions.
;;; 
;;; G_THREAD_ERROR_AGAIN
;;;     a thread couldn't be created due to resource shortage. Try again later.
;;; ----------------------------------------------------------------------------

(defcenum g-thread-error
  :g-thread-error-again)

(export 'g-thread-error)

;;; ----------------------------------------------------------------------------
;;; enum GThreadPriority
;;; 
;;; typedef enum {
;;;   G_THREAD_PRIORITY_LOW,
;;;   G_THREAD_PRIORITY_NORMAL,
;;;   G_THREAD_PRIORITY_HIGH,
;;;   G_THREAD_PRIORITY_URGENT
;;; } GThreadPriority;
;;; 
;;; Specifies the priority of a thread.
;;; 
;;; Note
;;; 
;;; It is not guaranteed that threads with different priorities really behave
;;; accordingly. On some systems (e.g. Linux) there are no thread priorities.
;;; On other systems (e.g. Solaris) there doesn't seem to be different
;;; scheduling for different priorities. All in all try to avoid being dependent
;;; on priorities.
;;; 
;;; G_THREAD_PRIORITY_LOW
;;;     a priority lower than normal
;;; 
;;; G_THREAD_PRIORITY_NORMAL
;;;     the default priority
;;; 
;;; G_THREAD_PRIORITY_HIGH
;;;     a priority higher than normal
;;; 
;;; G_THREAD_PRIORITY_URGENT
;;;     the highest priority
;;; ----------------------------------------------------------------------------

(defcenum g-thread-priority
  :g-thread-priority-low
  :g-thread-priority-normal
  :g-thread-priority-hight
  :g-thread-priority-urgent)

(export 'g-thread-priority)

;;; ----------------------------------------------------------------------------
;;; struct GThread
;;; 
;;; struct GThread {
;;; };
;;; 
;;; The GThread struct represents a running thread. It has three public
;;; read-only members, but the underlying struct is bigger, so you must not
;;; copy this struct.
;;; 
;;; Note
;;; 
;;; Resources for a joinable thread are not fully released until
;;; g_thread_join() is called for that thread.
;;; ----------------------------------------------------------------------------

(defcstruct g-thread)

(export 'g-thread)

;;; ----------------------------------------------------------------------------
;;; g_thread_init ()
;;; 
;;; void g_thread_init (GThreadFunctions *vtable)
;;; 
;;; If you use GLib from more than one thread, you must initialize the thread
;;; system by calling g_thread_init(). Most of the time you will only have to
;;; call g_thread_init (NULL).
;;; 
;;; Note
;;; 
;;; Do not call g_thread_init() with a non-NULL parameter unless you really
;;; know what you are doing.
;;; 
;;; Note
;;; 
;;; g_thread_init() must not be called directly or indirectly as a callback
;;; from GLib. Also no mutexes may be currently locked while calling
;;; g_thread_init().
;;; 
;;; Note
;;; 
;;; g_thread_init() changes the way in which GTimer measures elapsed time. As a
;;; consequence, timers that are running while g_thread_init() is called may
;;; report unreliable times.
;;; 
;;; Calling g_thread_init() multiple times is allowed (since version 2.24), but
;;; nothing happens except for the first call. If the argument is non-NULL on
;;; such a call a warning will be printed, but otherwise the argument is
;;; ignored.
;;; 
;;; If no thread system is available and vtable is NULL or if not all elements
;;; of vtable are non-NULL, then g_thread_init() will abort.
;;; 
;;; Note
;;; 
;;; To use g_thread_init() in your program, you have to link with the libraries
;;; that the command pkg-config --libs gthread-2.0 outputs. This is not the
;;; case for all the other thread related functions of GLib. Those can be used
;;; without having to link with the thread libraries.
;;; 
;;; vtable :
;;;     a function table of type GThreadFunctions, that provides the entry
;;;     points to the thread system to be used.
;;; ----------------------------------------------------------------------------

(defcfun ("g_thread_init" g-thread-init) :void
  (vtable :pointer))

(export 'g-thread-init)

;;; ----------------------------------------------------------------------------
;;; g_thread_get_initialized ()
;;; 
;;; gboolean g_thread_get_initialized (void)
;;; 
;;; Indicates if g_thread_init() has been called.
;;; 
;;; Returns :
;;;     TRUE if threads have been initialized.
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("g_thread_get_initialized" g-thread-get-initialized) :boolean)

(export 'g-thread-get-initialized)

;;; ----------------------------------------------------------------------------
;;; g_thread_self ()
;;; 
;;; GThread * g_thread_self (void)
;;; 
;;; This functions returns the GThread corresponding to the calling thread.
;;; 
;;; Returns :
;;;     the current thread.
;;; ----------------------------------------------------------------------------

(defcfun ("g_thread_self" g-thread-self) (:pointer g-thread))

(export 'g-thread-self)

;;; ----------------------------------------------------------------------------
;;; g_thread_join ()
;;; 
;;; gpointer g_thread_join (GThread *thread)
;;; 
;;; Waits until thread finishes, i.e. the function func, as given to
;;; g_thread_create(), returns or g_thread_exit() is called by thread. All
;;; resources of thread including the GThread struct are released. thread must
;;; have been created with joinable=TRUE in g_thread_create(). The value
;;; returned by func or given to g_thread_exit() by thread is returned by this
;;; function.
;;; 
;;; thread :
;;;     a GThread to be waited for.
;;; 
;;; Returns :
;;;     the return value of the thread.
;;; ----------------------------------------------------------------------------

(defcfun ("g_thread_join" g-thread-join) :pointer
  (thread (:pointer g-thread)))

(export 'g-thread-join)

;;; ----------------------------------------------------------------------------
;;; g_thread_set_priority ()
;;; 
;;; void g_thread_set_priority (GThread *thread, GThreadPriority priority)
;;; 
;;; Changes the priority of thread to priority.
;;; 
;;; Note
;;; 
;;; It is not guaranteed that threads with different priorities really behave
;;; accordingly. On some systems (e.g. Linux) there are no thread priorities. On
;;; other systems (e.g. Solaris) there doesn't seem to be different scheduling
;;; for different priorities. All in all try to avoid being dependent on
;;; priorities.
;;; 
;;; thread :
;;;     a GThread.
;;; 
;;; priority :
;;;     a new priority for thread.
;;; ----------------------------------------------------------------------------

(defcfun ("g_thread_set_priority" g-thread-set-priority) :void
  (thread (:pointer g-thread))
  (priority g-thread-priority))

(export 'g-thread-priority)

;;; ----------------------------------------------------------------------------
;;; struct GThreadFunctions
;;; 
;;; struct GThreadFunctions {
;;;   GMutex*  (*mutex_new)           (void);
;;;   void     (*mutex_lock)          (GMutex               *mutex);
;;;   gboolean (*mutex_trylock)       (GMutex               *mutex);
;;;   void     (*mutex_unlock)        (GMutex               *mutex);
;;;   void     (*mutex_free)          (GMutex               *mutex);
;;;   GCond*   (*cond_new)            (void);
;;;   void     (*cond_signal)         (GCond                *cond);
;;;   void     (*cond_broadcast)      (GCond                *cond);
;;;   void     (*cond_wait)           (GCond                *cond,
;;;                                    GMutex               *mutex);
;;;   gboolean (*cond_timed_wait)     (GCond                *cond,
;;;                                    GMutex               *mutex,
;;;                                    GTimeVal             *end_time);
;;;   void      (*cond_free)          (GCond                *cond);
;;;   GPrivate* (*private_new)        (GDestroyNotify        destructor);
;;;   gpointer  (*private_get)        (GPrivate             *private_key);
;;;   void      (*private_set)        (GPrivate             *private_key,
;;;                                    gpointer              data);
;;;   void      (*thread_create)      (GThreadFunc           func,
;;;                                    gpointer              data,
;;;                                    gulong                stack_size,
;;;                                    gboolean              joinable,
;;;                                    gboolean              bound,
;;;                                    GThreadPriority       priority,
;;;                                    gpointer              thread,
;;;                                    GError              **error);
;;;   void      (*thread_yield)       (void);
;;;   void      (*thread_join)        (gpointer              thread);
;;;   void      (*thread_exit)        (void);
;;;   void      (*thread_set_priority)(gpointer              thread,
;;;                                    GThreadPriority       priority);
;;;   void      (*thread_self)        (gpointer              thread);
;;;   gboolean  (*thread_equal)       (gpointer              thread1,
;;;                                    gpointer              thread2);
;;; };
;;; 
;;; This function table is used by g_thread_init() to initialize the thread
;;; system. The functions in the table are directly used by their g_* prepended
;;; counterparts (described in this document). For example, if you call
;;; g_mutex_new() then mutex_new() from the table provided to g_thread_init()
;;; will be called.
;;; 
;;; Note
;;; 
;;; Do not use this struct unless you know what you are doing.
;;; 
;;; mutex_new ()
;;;     virtual function pointer for g_mutex_new()
;;; 
;;; mutex_lock ()
;;;     virtual function pointer for g_mutex_lock()
;;; 
;;; mutex_trylock ()
;;;     virtual function pointer for g_mutex_trylock()
;;; 
;;; mutex_unlock ()
;;;     virtual function pointer for g_mutex_unlock()
;;; 
;;; mutex_free ()
;;;     virtual function pointer for g_mutex_free()
;;; 
;;; cond_new ()
;;;     virtual function pointer for g_cond_new()
;;; 
;;; cond_signal ()
;;;     virtual function pointer for g_cond_signal()
;;; 
;;; cond_broadcast ()
;;;     virtual function pointer for g_cond_broadcast()
;;; 
;;; cond_wait ()
;;;     virtual function pointer for g_cond_wait()
;;; 
;;; cond_timed_wait ()
;;;     virtual function pointer for g_cond_timed_wait()
;;; 
;;; cond_free ()
;;;     virtual function pointer for g_cond_free()
;;; 
;;; private_new ()
;;;     virtual function pointer for g_private_new()
;;; 
;;; private_get ()
;;;     virtual function pointer for g_private_get()
;;; 
;;; private_set ()
;;;     virtual function pointer for g_private_set()
;;; 
;;; thread_create ()
;;;     virtual function pointer for g_thread_create()
;;; 
;;; thread_yield ()
;;;     virtual function pointer for g_thread_yield()
;;; 
;;; thread_join ()
;;;     virtual function pointer for g_thread_join()
;;; 
;;; thread_exit ()
;;;     virtual function pointer for g_thread_exit()
;;; 
;;; thread_set_priority ()
;;;     virtual function pointer for g_thread_set_priority()
;;; 
;;; thread_self ()
;;;     virtual function pointer for g_thread_self()
;;; 
;;; thread_equal ()
;;;     used internally by recursive mutex locks and by some assertion checks
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_thread_supported ()
;;; 
;;; gboolean g_thread_supported ()
;;; 
;;; This function returns TRUE if the thread system is initialized, and FALSE
;;; if it is not.
;;; 
;;; Note
;;; 
;;; This function is actually a macro. Apart from taking the address of it you
;;; can however use it as if it was a function.
;;; 
;;; Returns :
;;;     TRUE, if the thread system is initialized.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; GThreadFunc ()
;;; 
;;; gpointer (*GThreadFunc) (gpointer data);
;;; 
;;; Specifies the type of the func functions passed to g_thread_create() or 
;;; g_thread_create_full().
;;; 
;;; data :
;;;     data passed to the thread.
;;; 
;;; Returns :
;;;     the return value of the thread, which will be returned by
;;;     g_thread_join().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_thread_create ()
;;; 
;;; GThread * g_thread_create (GThreadFunc func,
;;;                            gpointer data,
;;;                            gboolean joinable,
;;;                            GError **error)
;;; 
;;; This function creates a new thread with the default priority.
;;; 
;;; If joinable is TRUE, you can wait for this threads termination calling
;;; g_thread_join(). Otherwise the thread will just disappear when it
;;; terminates.
;;; 
;;; The new thread executes the function func with the argument data. If the
;;; thread was created successfully, it is returned.
;;; 
;;; error can be NULL to ignore errors, or non-NULL to report errors. The error
;;; is set, if and only if the function returns NULL.
;;; 
;;; func :
;;;     a function to execute in the new thread.
;;; 
;;; data :
;;;     an argument to supply to the new thread.
;;; 
;;; joinable :
;;;     should this thread be joinable?
;;; 
;;; error :
;;;     return location for error.
;;; 
;;; Returns :
;;;     the new GThread on success.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_thread_create_full ()
;;; 
;;; GThread * g_thread_create_full (GThreadFunc func,
;;;                                 gpointer data,
;;;                                 gulong stack_size,
;;;                                 gboolean joinable,
;;;                                 gboolean bound,
;;;                                 GThreadPriority priority,
;;;                                 GError **error)
;;; 
;;; This function creates a new thread with the priority priority. If the
;;; underlying thread implementation supports it, the thread gets a stack size
;;; of stack_size or the default value for the current platform, if
;;; stack_size is 0.
;;; 
;;; If joinable is TRUE, you can wait for this threads termination calling
;;; g_thread_join(). Otherwise the thread will just disappear when it
;;; terminates. If bound is TRUE, this thread will be scheduled in the system
;;; scope, otherwise the implementation is free to do scheduling in the process
;;; scope. The first variant is more expensive resource-wise, but generally
;;; faster. On some systems (e.g. Linux) all threads are bound.
;;; 
;;; The new thread executes the function func with the argument data. If the
;;; thread was created successfully, it is returned.
;;; 
;;; error can be NULL to ignore errors, or non-NULL to report errors. The error
;;; is set, if and only if the function returns NULL.
;;; 
;;; Note
;;; 
;;; It is not guaranteed that threads with different priorities really behave
;;; accordingly. On some systems (e.g. Linux) there are no thread priorities. On
;;; other systems (e.g. Solaris) there doesn't seem to be different scheduling
;;; for different priorities. All in all try to avoid being dependent on
;;; priorities. Use G_THREAD_PRIORITY_NORMAL here as a default.
;;; 
;;; Note
;;; 
;;; Only use g_thread_create_full() if you really can't use g_thread_create()
;;; instead. g_thread_create() does not take stack_size, bound, and priority as
;;; arguments, as they should only be used in cases in which it is unavoidable.
;;; 
;;; func :
;;;     a function to execute in the new thread.
;;; 
;;; data :
;;;     an argument to supply to the new thread.
;;; 
;;; stack_size :
;;;     a stack size for the new thread.
;;; 
;;; joinable :
;;;     should this thread be joinable?
;;; 
;;; bound :
;;;     should this thread be bound to a system thread?
;;; 
;;; priority :
;;;     a priority for the thread.
;;; 
;;; error :
;;;     return location for error.
;;; 
;;; Returns :
;;;     the new GThread on success.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_thread_yield ()
;;; 
;;; void g_thread_yield ()
;;; 
;;; Gives way to other threads waiting to be scheduled.
;;; 
;;; This function is often used as a method to make busy wait less evil. But in
;;; most cases you will encounter, there are better methods to do that. So in
;;; general you shouldn't use this function.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_thread_exit ()
;;; 
;;; void g_thread_exit (gpointer retval)
;;; 
;;; Exits the current thread. If another thread is waiting for that thread using
;;; g_thread_join() and the current thread is joinable, the waiting thread will
;;; be woken up and get retval as the return value of g_thread_join(). If the 
;;; current thread is not joinable, retval is ignored. Calling
;;; 
;;; 1 g_thread_exit (retval);
;;; 
;;; is equivalent to returning retval from the function func, as given to 
;;; g_thread_create().
;;; 
;;; Note
;;; 
;;; Never call g_thread_exit() from within a thread of a GThreadPool, as that 
;;; will mess up the bookkeeping and lead to funny and unwanted results.
;;; 
;;; retval :
;;;     the return value of this thread.
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; g_thread_foreach ()
;;; 
;;; void g_thread_foreach (GFunc thread_func, gpointer user_data)
;;; 
;;; Call thread_func on all existing GThread structures. Note that threads may
;;; decide to exit while thread_func is running, so without intimate knowledge 
;;; about the lifetime of foreign threads, thread_func shouldn't access the 
;;; GThread* pointer passed in as first argument. However, thread_func will not 
;;; be called for threads which are known to have exited already.
;;; 
;;; Due to thread lifetime checks, this function has an execution complexity 
;;; which is quadratic in the number of existing threads.
;;; 
;;; thread_func :
;;;     function to call for all GThread structures
;;; 
;;; user_data :
;;;     second argument to thread_func
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; *** NOT IMPLEMENTED ***

;;; ----------------------------------------------------------------------------
;;; G_THREADS_ENABLED
;;; 
;;; #define G_THREADS_ENABLED
;;; 
;;; This macro is defined if GLib was compiled with thread support. This does
;;; not necessarily mean that there is a thread implementation available, but it
;;; does mean that the infrastructure is in place and that once you provide a
;;; thread implementation to g_thread_init(), GLib will be multi-thread safe. If
;;; G_THREADS_ENABLED is not defined, then Glib is not, and cannot be,
;;; multi-thread safe.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_THREADS_IMPL_POSIX
;;; 
;;; #define G_THREADS_IMPL_POSIX
;;; 
;;; This macro is defined if POSIX style threads are used.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_THREADS_IMPL_NONE
;;; 
;;; #define G_THREADS_IMPL_NONE
;;; 
;;; This macro is defined if no thread implementation is used. You can, however,
;;; provide one to g_thread_init() to make GLib multi-thread safe.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_THREAD_ERROR
;;; 
;;; #define G_THREAD_ERROR g_thread_error_quark ()
;;; 
;;; The error domain of the GLib thread subsystem.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GMutex
;;; 
;;; typedef struct _GMutex GMutex;
;;; 
;;; The GMutex struct is an opaque data structure to represent a mutex (mutual
;;; exclusion). It can be used to protect data against shared access. Take for
;;; example the following function:
;;; 
;;; Example 2. A function which will not work in a threaded environment
;;; 
;;;  1 int
;;;  2 give_me_next_number (void)
;;;  3 {
;;;  4   static int current_number = 0;
;;;  5 
;;;  6   /* now do a very complicated calculation to calculate the new
;;;  7    * number, this might for example be a random number generator
;;;  8    */
;;;  9   current_number = calc_next_number (current_number);
;;; 10 
;;; 11   return current_number;
;;; 12 }
;;; 
;;; 
;;; It is easy to see that this won't work in a multi-threaded application.
;;; There current_number must be protected against shared access. A first naive
;;; implementation would be:
;;; 
;;; Example 3. The wrong way to write a thread-safe function
;;; 
;;;  1 int
;;;  2 give_me_next_number (void)
;;;  3 {
;;;  4   static int current_number = 0;
;;;  5   int ret_val;
;;;  6   static GMutex * mutex = NULL;
;;;  7 
;;;  8   if (!mutex) mutex = g_mutex_new ();
;;;  9
;;; 10   g_mutex_lock (mutex);
;;; 11   ret_val = current_number = calc_next_number (current_number);
;;; 12   g_mutex_unlock (mutex);
;;; 13 
;;; 14   return ret_val;
;;; 15 }
;;; 
;;; This looks like it would work, but there is a race condition while
;;; constructing the mutex and this code cannot work reliable. Please do not
;;; use such constructs in your own programs! One working solution is:
;;; 
;;; Example 4. A correct thread-safe function
;;;  
;;;  1 static GMutex *give_me_next_number_mutex = NULL;
;;;  2 
;;;  3 /* this function must be called before any call to
;;;  4  * give_me_next_number()
;;;  5  *
;;;  6  * it must be called exactly once.
;;;  7  */
;;;  8 void
;;;  9 init_give_me_next_number (void)
;;; 10 {
;;; 11   g_assert (give_me_next_number_mutex == NULL);
;;; 12   give_me_next_number_mutex = g_mutex_new ();
;;; 13 }
;;; 14
;;; 15 int
;;; 16 give_me_next_number (void)
;;; 17 {
;;; 18   static int current_number = 0;
;;; 19   int ret_val;
;;; 20 
;;; 21   g_mutex_lock (give_me_next_number_mutex);
;;; 22   ret_val = current_number = calc_next_number (current_number);
;;; 23   g_mutex_unlock (give_me_next_number_mutex);
;;; 24
;;; 25   return ret_val;
;;; 26 }
;;; 
;;; GStaticMutex provides a simpler and safer way of doing this.
;;; 
;;; If you want to use a mutex, and your code should also work without calling
;;; g_thread_init() first, then you cannot use a GMutex, as g_mutex_new()
;;; requires that the thread system be initialized. Use a GStaticMutex instead.
;;; 
;;; A GMutex should only be accessed via the following functions.
;;; 
;;; Note
;;; 
;;; All of the g_mutex_* functions are actually macros. Apart from taking their
;;; addresses, you can however use them as if they were functions.
;;; ----------------------------------------------------------------------------

(defcstruct g-mutex)

(export 'g-mutex)

;;; --- End of file glib.threads.lisp ------------------------------------------
