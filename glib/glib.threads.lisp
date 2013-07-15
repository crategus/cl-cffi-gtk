;;; ----------------------------------------------------------------------------
;;; glib.threads.lisp
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
;;; Threads
;;;
;;; Portable support for threads, mutexes, locks, conditions and thread private
;;; data
;;;
;;; Synopsis
;;;
;;;     G_THREAD_ERROR
;;;
;;;     GThreadError
;;;     GThread
;;;
;;;     g_thread_new
;;;     g_thread_try_new
;;;     g_thread_ref
;;;     g_thread_unref
;;;     g_thread_join
;;;     g_thread_yield
;;;     g_thread_exit
;;;     g_thread_self
;;;
;;;     GMutex
;;;
;;;     g_mutex_init
;;;     g_mutex_clear
;;;     g_mutex_lock
;;;     g_mutex_trylock
;;;     g_mutex_unlock
;;;
;;;     G_LOCK_DEFINE
;;;     G_LOCK_DEFINE_STATIC
;;;     G_LOCK_EXTERN
;;;     G_LOCK
;;;     G_TRYLOCK
;;;     G_UNLOCK
;;;
;;;     GRecMutex
;;;
;;;     g_rec_mutex_init
;;;     g_rec_mutex_clear
;;;     g_rec_mutex_lock
;;;     g_rec_mutex_trylock
;;;     g_rec_mutex_unlock
;;;
;;;     GRWLock
;;;
;;;     g_rw_lock_init
;;;     g_rw_lock_clear
;;;     g_rw_lock_writer_lock
;;;     g_rw_lock_writer_trylock
;;;     g_rw_lock_writer_unlock
;;;     g_rw_lock_reader_lock
;;;     g_rw_lock_reader_trylock
;;;     g_rw_lock_reader_unlock
;;;
;;;     GCond
;;;
;;;     g_cond_init
;;;     g_cond_clear
;;;     g_cond_wait
;;;     g_cond_timed_wait
;;;     g_cond_wait_until
;;;     g_cond_signal
;;;     g_cond_broadcast
;;;
;;;     GPrivate
;;;
;;;     G_PRIVATE_INIT
;;;     g_private_get
;;;     g_private_set
;;;     g_private_replace
;;;
;;;     GOnce
;;;     GOnceStatus
;;;
;;;     G_ONCE_INIT
;;;
;;;     g_once
;;;     g_once_init_enter
;;;     g_once_init_leave
;;;
;;;     g_bit_lock
;;;     g_bit_trylock
;;;     g_bit_unlock
;;;     g_pointer_bit_lock
;;;     g_pointer_bit_trylock
;;;     g_pointer_bit_unlock
;;;
;;; Description
;;;
;;; Threads act almost like processes, but unlike processes all threads of one
;;; process share the same memory. This is good, as it provides easy
;;; communication between the involved threads via this shared memory, and it is
;;; bad, because strange things (so called "Heisenbugs") might happen if the
;;; program is not carefully designed. In particular, due to the concurrent
;;; nature of threads, no assumptions on the order of execution of code running
;;; in different threads can be made, unless order is explicitly forced by the
;;; programmer through synchronization primitives.
;;;
;;; The aim of the thread-related functions in GLib is to provide a portable
;;; means for writing multi-threaded software. There are primitives for mutexes
;;; to protect the access to portions of memory (GMutex, GRecMutex and GRWLock).
;;; There is a facility to use individual bits for locks (g_bit_lock()). There
;;; are primitives for condition variables to allow synchronization of threads
;;; (GCond). There are primitives for thread-private data - data that every
;;; thread has a private instance of (GPrivate). There are facilities for
;;; one-time initialization (GOnce, g_once_init_enter()). Finally, there are
;;; primitives to create and manage threads (GThread).
;;;
;;; The GLib threading system used to be initialized with g_thread_init(). This
;;; is no longer necessary. Since version 2.32, the GLib threading system is
;;; automatically initialized at the start of your program, and all
;;; thread-creation functions and synchronization primitives are available right
;;; away.
;;;
;;; Note that it is not safe to assume that your program has no threads even if
;;; you don't call g_thread_new() yourself. GLib and GIO can and will create
;;; threads for their own purposes in some cases, such as when using
;;; g_unix_signal_source_new() or when using GDBus.
;;;
;;; Originally, UNIX did not have threads, and therefore some traditional UNIX
;;; APIs are problematic in threaded programs. Some notable examples are
;;;
;;;     C library functions that return data in statically allocated buffers,
;;;     such as strtok() or strerror(). For many of these, there are thread-safe
;;;     variants with a _r suffix, or you can look at corresponding GLib APIs
;;;     (like g_strsplit() or g_strerror()).
;;;
;;;     setenv() and unsetenv() manipulate the process environment in a not
;;;     thread-safe way, and may interfere with getenv() calls in other threads.
;;;     Note that getenv() calls may be “hidden” behind other APIs. For example,
;;;     GNU gettext() calls getenv() under the covers. In general, it is best to
;;;     treat the environment as readonly. If you absolutely have to modify the
;;;     environment, do it early in main(), when no other threads are around
;;;     yet.
;;;
;;;     setlocale() changes the locale for the entire process, affecting all
;;;     threads. Temporary changes to the locale are often made to change the
;;;     behavior of string scanning or formatting functions like scanf() or
;;;     printf(). GLib offers a number of string APIs (like g_ascii_formatd() or
;;;     g_ascii_strtod()) that can often be used as an alternative. Or you can
;;;     use the uselocale() function to change the locale only for the current
;;;     thread.
;;;
;;;     fork() only takes the calling thread into the child's copy of the
;;;     process image. If other threads were executing in critical sections they
;;;     could have left mutexes locked which could easily cause deadlocks in the
;;;     new child. For this reason, you should call exit() or exec() as soon as
;;;     possible in the child and only make signal-safe library calls before
;;;     that.
;;;
;;;     daemon() uses fork() in a way contrary to what is described above. It
;;;     should not be used with GLib programs.
;;;
;;; GLib itself is internally completely thread-safe (all global data is
;;; automatically locked), but individual data structure instances are not
;;; automatically locked for performance reasons. For example, you must
;;; coordinate accesses to the same GHashTable from multiple threads. The two
;;; notable exceptions from this rule are GMainLoop and GAsyncQueue, which are
;;; thread-safe and need no further application-level locking to be accessed
;;; from multiple threads. Most refcounting functions such as g_object_ref() are
;;; also thread-safe.
;;; ----------------------------------------------------------------------------

(in-package :glib)

;;; ----------------------------------------------------------------------------
;;; G_THREAD_ERROR
;;;
;;; #define G_THREAD_ERROR g_thread_error_quark ()
;;;
;;; The error domain of the GLib thread subsystem.
;;; ----------------------------------------------------------------------------

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
;;; GThread
;;;
;;; typedef struct {
;;; } GThread;
;;;
;;; The GThread struct represents a running thread. This struct is returned by
;;; g_thread_new() or g_thread_try_new(). You can obtain the GThread struct
;;; representing the current thead by calling g_thread_self().
;;;
;;; GThread is refcounted, see g_thread_ref() and g_thread_unref(). The thread
;;; represented by it holds a reference while it is running, and g_thread_join()
;;; consumes the reference that it is given, so it is normally not necessary to
;;; manage GThread references explicitly.
;;;
;;; The structure is opaque -- none of its fields may be directly accessed.
;;; ----------------------------------------------------------------------------

(defcstruct g-thread)

(export 'g-thread)

;;; ----------------------------------------------------------------------------
;;; GThreadFunc ()
;;;
;;; gpointer (*GThreadFunc) (gpointer data);
;;;
;;; Specifies the type of the func functions passed to g_thread_new() or
;;; g_thread_try_new().
;;;
;;; data :
;;;     data passed to the thread
;;;
;;; Returns :
;;;     the return value of the thread
;;; ----------------------------------------------------------------------------

;; TODO: The callback is defined to return a :boolean and not a :pointer.
;; SBCL crashes when we return a value, but the type is a pointer.
;; Try to generalize the implementation.

(defcallback g-thread-func-cb :boolean ((data :pointer))
  (funcall (glib::get-stable-pointer-value data)))

;;; ----------------------------------------------------------------------------
;;; g_thread_new ()
;;;
;;; GThread * g_thread_new (const gchar *name, GThreadFunc func, gpointer data);
;;;
;;; This function creates a new thread. The new thread starts by invoking func
;;; with the argument data. The thread will run until func returns or until
;;; g_thread_exit() is called from the new thread. The return value of func
;;; becomes the return value of the thread, which can be obtained with
;;; g_thread_join().
;;;
;;; The name can be useful for discriminating threads in a debugger. Some
;;; systems restrict the length of name to 16 bytes.
;;;
;;; If the thread can not be created the program aborts. See g_thread_try_new()
;;; if you want to attempt to deal with failures.
;;;
;;; To free the struct returned by this function, use g_thread_unref(). Note
;;; that g_thread_join() implicitly unrefs the GThread as well.
;;;
;;; name :
;;;     a name for the new thread
;;;
;;; func :
;;;     a function to execute in the new thread
;;;
;;; data :
;;;     an argument to supply to the new thread
;;;
;;; Returns :
;;;     the new GThread
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

(defcfun ("g_thread_new" %g-thread-new) (:pointer (:struct g-thread))
  (name :string)
  (func :pointer)
  (data :pointer))

;; TODO: The allocated stable pointer is not freed automatically.
;; The pointer must be valid as long as the thread is running.

(defun g-thread-new (name func)
  (let ((ptr (glib::allocate-stable-pointer func)))
    (%g-thread-new name (callback g-thread-func-cb) ptr)))

(export 'g-thread-new)

;;; ----------------------------------------------------------------------------
;;; g_thread_try_new ()
;;;
;;; GThread * g_thread_try_new (const gchar *name,
;;;                             GThreadFunc func,
;;;                             gpointer data,
;;;                             GError **error);
;;;
;;; This function is the same as g_thread_new() except that it allows for the
;;; possibility of failure.
;;;
;;; If a thread can not be created (due to resource limits), error is set and
;;; NULL is returned.
;;;
;;; name :
;;;     a name for the new thread
;;;
;;; func :
;;;     a function to execute in the new thread
;;;
;;; data :
;;;     an argument to supply to the new thread
;;;
;;; error :
;;;     return location for error, or NULL
;;;
;;; Returns :
;;;     the new GThread, or NULL if an error occurred
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_thread_ref ()
;;;
;;; GThread * g_thread_ref (GThread *thread);
;;;
;;; Increase the reference count on thread.
;;;
;;; thread :
;;;     a GThread
;;;
;;; Returns :
;;;     a new reference to thread
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_thread_unref ()
;;;
;;; void g_thread_unref (GThread *thread);
;;;
;;; Decrease the reference count on thread, possibly freeing all resources
;;; associated with it.
;;;
;;; Note that each thread holds a reference to its GThread while it is running,
;;; so it is safe to drop your own reference to it if you don't need it anymore.
;;;
;;; thread :
;;;     a GThread
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_thread_join ()
;;;
;;; gpointer g_thread_join (GThread *thread);
;;;
;;; Waits until thread finishes, i.e. the function func, as given to
;;; g_thread_new(), returns or g_thread_exit() is called. If thread has already
;;; terminated, then g_thread_join() returns immediately.
;;;
;;; Any thread can wait for any other thread by calling g_thread_join(), not
;;; just its 'creator'. Calling g_thread_join() from multiple threads for the
;;; same thread leads to undefined behaviour.
;;;
;;; The value returned by func or given to g_thread_exit() is returned by this
;;; function.
;;;
;;; g_thread_join() consumes the reference to the passed-in thread. This will
;;; usually cause the GThread struct and associated resources to be freed. Use
;;; g_thread_ref() to obtain an extra reference if you want to keep the GThread
;;; alive beyond the g_thread_join() call.
;;;
;;; thread :
;;;     a GThread
;;;
;;; Returns :
;;;     the return value of the thread
;;; ----------------------------------------------------------------------------

(defcfun ("g_thread_join" g-thread-join) :pointer
  (thread (:pointer (:struct g-thread))))

(export 'g-thread-join)

;;; ----------------------------------------------------------------------------
;;; g_thread_yield ()
;;;
;;; void g_thread_yield ();
;;;
;;; Causes the calling thread to voluntarily relinquish the CPU, so that other
;;; threads can run.
;;;
;;; This function is often used as a method to make busy wait less evil.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_thread_exit ()
;;;
;;; void g_thread_exit (gpointer retval);
;;;
;;; Terminates the current thread.
;;;
;;; If another thread is waiting for us using g_thread_join() then the waiting
;;; thread will be woken up and get retval as the return value of
;;; g_thread_join().
;;;
;;; Calling g_thread_exit (retval) is equivalent to returning retval from the
;;; function func, as given to g_thread_new().
;;;
;;; Note
;;;
;;; You must only call g_thread_exit() from a thread that you created yourself
;;; with g_thread_new() or related APIs. You must not call this function from a
;;; thread created with another threading library or or from within a
;;; GThreadPool.
;;;
;;; retval :
;;;     the return value of this thread
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_thread_self ()
;;;
;;; GThread * g_thread_self (void);
;;;
;;; This functions returns the GThread corresponding to the current thread. Note
;;; that this function does not increase the reference count of the returned
;;; struct.
;;;
;;; This function will return a GThread even for threads that were not created
;;; by GLib (i.e. those created by other threading APIs). This may be useful for
;;; thread identification purposes (i.e. comparisons) but you must not use GLib
;;; functions (such as g_thread_join()) on these threads.
;;;
;;; Returns :
;;;     the GThread representing the current thread
;;; ----------------------------------------------------------------------------

(defcfun ("g_thread_self" g-thread-self) (:pointer (:struct g-thread)))

(export 'g-thread-self)

;;; ----------------------------------------------------------------------------
;;; union GMutex
;;;
;;; union _GMutex
;;; {
;;;   /*< private >*/
;;;   gpointer p;
;;;   guint i[2];
;;; };
;;;
;;; The GMutex struct is an opaque data structure to represent a mutex (mutual
;;; exclusion). It can be used to protect data against shared access. Take for
;;; example the following function:
;;;
;;; Example 2. A function which will not work in a threaded environment
;;;
;;;   int
;;;   give_me_next_number (void)
;;;   {
;;;     static int current_number = 0;
;;;
;;;     /* now do a very complicated calculation to calculate the new
;;;      * number, this might for example be a random number generator
;;;      */
;;;     current_number = calc_next_number (current_number);
;;;
;;;     return current_number;
;;;   }
;;;
;;; It is easy to see that this won't work in a multi-threaded application.
;;; There current_number must be protected against shared access. A GMutex can
;;; be used as a solution to this problem:
;;;
;;; Example 3. Using GMutex to protected a shared variable
;;;
;;;   int
;;;   give_me_next_number (void)
;;;   {
;;;     static GMutex mutex;
;;;     static int current_number = 0;
;;;     int ret_val;
;;;
;;;     g_mutex_lock (&mutex);
;;;     ret_val = current_number = calc_next_number (current_number);
;;;     g_mutex_unlock (&mutex);
;;;
;;;     return ret_val;
;;;   }
;;;
;;; Notice that the GMutex is not initialised to any particular value. Its
;;; placement in static storage ensures that it will be initialised to
;;; all-zeros, which is appropriate.
;;;
;;; If a GMutex is placed in other contexts (eg: embedded in a struct) then it
;;; must be explicitly initialised using g_mutex_init().
;;;
;;; A GMutex should only be accessed via g_mutex_ functions.
;;; ----------------------------------------------------------------------------

(defcstruct g-mutex)

(export 'g-mutex)

;;; ----------------------------------------------------------------------------
;;; g_mutex_init ()
;;;
;;; void g_mutex_init (GMutex *mutex);
;;;
;;; Initializes a GMutex so that it can be used.
;;;
;;; This function is useful to initialize a mutex that has been allocated on the
;;; stack, or as part of a larger structure. It is not necessary to initialize a
;;; mutex that has been created that has been statically allocated.
;;;
;;; typedef struct {
;;;   GMutex m;
;;;   ...
;;; } Blob;
;;;
;;; Blob *b;
;;;
;;; b = g_new (Blob, 1);
;;; g_mutex_init (&b->m);
;;;
;;; To undo the effect of g_mutex_init() when a mutex is no longer needed, use
;;; g_mutex_clear().
;;;
;;; Calling g_mutex_init() on an already initialized GMutex leads to undefined
;;; behaviour.
;;;
;;; mutex :
;;;     an uninitialized GMutex
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_mutex_clear ()
;;;
;;; void g_mutex_clear (GMutex *mutex);
;;;
;;; Frees the resources allocated to a mutex with g_mutex_init().
;;;
;;; This function should not be used with a GMutex that has been statically
;;; allocated.
;;;
;;; Calling g_mutex_clear() on a locked mutex leads to undefined behaviour.
;;;
;;; mutex :
;;;     an initialized GMutex
;;;
;;; Sine: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_mutex_lock ()
;;;
;;; void g_mutex_lock (GMutex *mutex);
;;;
;;; Locks mutex. If mutex is already locked by another thread, the current
;;; thread will block until mutex is unlocked by the other thread.
;;;
;;; Note
;;;
;;; GMutex is neither guaranteed to be recursive nor to be non-recursive. As
;;; such, calling g_mutex_lock() on a GMutex that has already been locked by the
;;; same thread results in undefined behaviour (including but not limited to
;;; deadlocks).
;;;
;;; mutex :
;;;     a GMutex
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_mutex_trylock ()
;;;
;;; gboolean g_mutex_trylock (GMutex *mutex);
;;;
;;; Tries to lock mutex. If mutex is already locked by another thread, it
;;; immediately returns FALSE. Otherwise it locks mutex and returns TRUE.
;;;
;;; Note
;;;
;;; GMutex is neither guaranteed to be recursive nor to be non-recursive. As
;;; such, calling g_mutex_lock() on a GMutex that has already been locked by the
;;; same thread results in undefined behaviour (including but not limited to
;;; deadlocks or arbitrary return values).
;;;
;;; mutex :
;;;     a GMutex
;;;
;;; Returns :
;;;     TRUE if mutex could be locked
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_mutex_unlock ()
;;;
;;; void g_mutex_unlock (GMutex *mutex);
;;;
;;; Unlocks mutex. If another thread is blocked in a g_mutex_lock() call for
;;; mutex, it will become unblocked and can lock mutex itself.
;;;
;;; Calling g_mutex_unlock() on a mutex that is not locked by the current thread
;;; leads to undefined behaviour.
;;;
;;; mutex :
;;;     a GMutex
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_LOCK_DEFINE()
;;;
;;; #define G_LOCK_DEFINE(name)
;;;
;;; The G_LOCK_* macros provide a convenient interface to GMutex. G_LOCK_DEFINE
;;; defines a lock. It can appear in any place where variable definitions may
;;; appear in programs, i.e. in the first block of a function or outside of
;;; functions. The name parameter will be mangled to get the name of the GMutex.
;;; This means that you can use names of existing variables as the parameter -
;;; e.g. the name of the variable you intend to protect with the lock. Look at
;;; our give_me_next_number() example using the G_LOCK_* macros:
;;;
;;; Example 4. Using the G_LOCK_* convenience macros
;;;
;;;   G_LOCK_DEFINE (current_number);
;;;
;;;   int
;;;   give_me_next_number (void)
;;;   {
;;;     static int current_number = 0;
;;;     int ret_val;
;;;
;;;     G_LOCK (current_number);
;;;     ret_val = current_number = calc_next_number (current_number);
;;;     G_UNLOCK (current_number);
;;;
;;;     return ret_val;
;;;   }
;;;
;;; name :
;;;     the name of the lock
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_LOCK_DEFINE_STATIC()
;;;
;;; #define G_LOCK_DEFINE_STATIC(name)
;;;
;;; This works like G_LOCK_DEFINE, but it creates a static object.
;;;
;;; name :
;;;     the name of the lock
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_LOCK_EXTERN()
;;;
;;; #define G_LOCK_EXTERN(name)
;;;
;;; This declares a lock, that is defined with G_LOCK_DEFINE in another module.
;;;
;;; name :
;;;     the name of the lock
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_LOCK()
;;;
;;; #define G_LOCK(name)
;;;
;;; Works like g_mutex_lock(), but for a lock defined with G_LOCK_DEFINE.
;;;
;;; name :
;;;     the name of the lock
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_TRYLOCK()
;;;
;;; #define G_TRYLOCK(name)
;;;
;;; Works like g_mutex_trylock(), but for a lock defined with G_LOCK_DEFINE.
;;;
;;; name :
;;;     the name of the lock
;;;
;;; Returns :
;;;     TRUE, if the lock could be locked.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_UNLOCK()
;;;
;;; #define G_UNLOCK(name)
;;;
;;; Works like g_mutex_unlock(), but for a lock defined with G_LOCK_DEFINE.
;;;
;;; name :
;;;     the name of the lock
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GRecMutex
;;;
;;; struct GRecMutex {
;;; };
;;;
;;; The GRecMutex struct is an opaque data structure to represent a recursive
;;; mutex. It is similar to a GMutex with the difference that it is possible to
;;; lock a GRecMutex multiple times in the same thread without deadlock. When
;;; doing so, care has to be taken to unlock the recursive mutex as often as it
;;; has been locked.
;;;
;;; If a GRecMutex is allocated in static storage then it can be used without
;;; initialisation. Otherwise, you should call g_rec_mutex_init() on it and
;;; g_rec_mutex_clear() when done.
;;;
;;; A GRecMutex should only be accessed with the g_rec_mutex_ functions.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rec_mutex_init ()
;;;
;;; void g_rec_mutex_init (GRecMutex *rec_mutex);
;;;
;;; Initializes a GRecMutex so that it can be used.
;;;
;;; This function is useful to initialize a recursive mutex that has been
;;; allocated on the stack, or as part of a larger structure.
;;;
;;; It is not necessary to initialise a recursive mutex that has been statically
;;; allocated.
;;;
;;; typedef struct {
;;;   GRecMutex m;
;;;   ...
;;; } Blob;
;;;
;;; Blob *b;
;;;
;;; b = g_new (Blob, 1);
;;; g_rec_mutex_init (&b->m);
;;;
;;; Calling g_rec_mutex_init() on an already initialized GRecMutex leads to
;;; undefined behaviour.
;;;
;;; To undo the effect of g_rec_mutex_init() when a recursive mutex is no longer
;;; needed, use g_rec_mutex_clear().
;;;
;;; rec_mutex :
;;;     an uninitialized GRecMutex
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rec_mutex_clear ()
;;;
;;; void g_rec_mutex_clear (GRecMutex *rec_mutex);
;;;
;;; Frees the resources allocated to a recursive mutex with g_rec_mutex_init().
;;;
;;; This function should not be used with a GRecMutex that has been statically
;;; allocated.
;;;
;;; Calling g_rec_mutex_clear() on a locked recursive mutex leads to undefined
;;; behaviour.
;;;
;;; rec_mutex :
;;;     an initialized GRecMutex
;;;
;;; Sine: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rec_mutex_lock ()
;;;
;;; void g_rec_mutex_lock (GRecMutex *rec_mutex);
;;;
;;; Locks rec_mutex. If rec_mutex is already locked by another thread, the
;;; current thread will block until rec_mutex is unlocked by the other thread.
;;; If rec_mutex is already locked by the current thread, the 'lock count' of
;;; rec_mutex is increased. The mutex will only become available again when it
;;; is unlocked as many times as it has been locked.
;;;
;;; rec_mutex :
;;;     a GRecMutex
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rec_mutex_trylock ()
;;;
;;; gboolean g_rec_mutex_trylock (GRecMutex *rec_mutex);
;;;
;;; Tries to lock rec_mutex. If rec_mutex is already locked by another thread,
;;; it immediately returns FALSE. Otherwise it locks rec_mutex and returns TRUE.
;;;
;;; rec_mutex :
;;;     a GRecMutex
;;;
;;; Returns :
;;;     TRUE if rec_mutex could be locked
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rec_mutex_unlock ()
;;;
;;; void g_rec_mutex_unlock (GRecMutex *rec_mutex);
;;;
;;; Unlocks rec_mutex. If another thread is blocked in a g_rec_mutex_lock() call
;;; for rec_mutex, it will become unblocked and can lock rec_mutex itself.
;;;
;;; Calling g_rec_mutex_unlock() on a recursive mutex that is not locked by the
;;; current thread leads to undefined behaviour.
;;;
;;; rec_mutex :
;;;     a GRecMutex
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GRWLock
;;;
;;; struct GRWLock {
;;; };
;;;
;;; The GRWLock struct is an opaque data structure to represent a reader-writer
;;; lock. It is similar to a GMutex in that it allows multiple threads to
;;; coordinate access to a shared resource.
;;;
;;; The difference to a mutex is that a reader-writer lock discriminates between
;;; read-only ('reader') and full ('writer') access. While only one thread at a
;;; time is allowed write access (by holding the 'writer' lock via
;;; g_rw_lock_writer_lock()), multiple threads can gain simultaneous read-only
;;; access (by holding the 'reader' lock via g_rw_lock_reader_lock()).
;;;
;;; Example 5. An array with access functions
;;;
;;;   GRWLock lock;
;;;   GPtrArray *array;
;;;
;;;   gpointer
;;;   my_array_get (guint index)
;;;   {
;;;     gpointer retval = NULL;
;;;
;;;     if (!array)
;;;       return NULL;
;;;
;;;     g_rw_lock_reader_lock (&lock);
;;;     if (index < array->len)
;;;       retval = g_ptr_array_index (array, index);
;;;     g_rw_lock_reader_unlock (&lock);
;;;
;;;     return retval;
;;;   }
;;;
;;;   void
;;;   my_array_set (guint index, gpointer data)
;;;   {
;;;     g_rw_lock_writer_lock (&lock);
;;;
;;;     if (!array)
;;;       array = g_ptr_array_new ();
;;;
;;;     if (index >= array->len)
;;;       g_ptr_array_set_size (array, index+1);
;;;     g_ptr_array_index (array, index) = data;
;;;
;;;     g_rw_lock_writer_unlock (&lock);
;;;   }
;;;
;;; This example shows an array which can be accessed by many readers (the
;;; my_array_get() function) simultaneously, whereas the writers (the
;;; my_array_set() function) will only be allowed once at a time and only if no
;;; readers currently access the array. This is because of the potentially
;;; dangerous resizing of the array. Using these functions is fully multi-thread
;;; safe now.
;;;
;;; If a GRWLock is allocated in static storage then it can be used without
;;; initialisation. Otherwise, you should call g_rw_lock_init() on it and
;;; g_rw_lock_clear() when done.
;;;
;;; A GRWLock should only be accessed with the g_rw_lock_ functions.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rw_lock_init ()
;;;
;;; void g_rw_lock_init (GRWLock *rw_lock);
;;;
;;; Initializes a GRWLock so that it can be used.
;;;
;;; This function is useful to initialize a lock that has been allocated on the
;;; stack, or as part of a larger structure. It is not necessary to initialise a
;;; reader-writer lock that has been statically allocated.
;;;
;;;   typedef struct {
;;;     GRWLock l;
;;;     ...
;;;   } Blob;
;;;
;;;   Blob *b;
;;;
;;;   b = g_new (Blob, 1);
;;;   g_rw_lock_init (&b->l);
;;;
;;; To undo the effect of g_rw_lock_init() when a lock is no longer needed, use
;;; g_rw_lock_clear().
;;;
;;; Calling g_rw_lock_init() on an already initialized GRWLock leads to
;;; undefined behaviour.
;;;
;;; rw_lock :
;;;     an uninitialized GRWLock
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rw_lock_clear ()
;;;
;;; void g_rw_lock_clear (GRWLock *rw_lock);
;;;
;;; Frees the resources allocated to a lock with g_rw_lock_init().
;;;
;;; This function should not be used with a GRWLock that has been statically
;;; allocated.
;;;
;;; Calling g_rw_lock_clear() when any thread holds the lock leads to undefined
;;; behaviour.
;;;
;;; rw_lock :
;;;     an initialized GRWLock
;;;
;;; Sine: 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rw_lock_writer_lock ()
;;;
;;; void g_rw_lock_writer_lock (GRWLock *rw_lock);
;;;
;;; Obtain a write lock on rw_lock. If any thread already holds a read or write
;;; lock on rw_lock, the current thread will block until all other threads have
;;; dropped their locks on rw_lock.
;;;
;;; rw_lock :
;;;     a GRWLock
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rw_lock_writer_trylock ()
;;;
;;; gboolean g_rw_lock_writer_trylock (GRWLock *rw_lock);
;;;
;;; Tries to obtain a write lock on rw_lock. If any other thread holds a read or
;;; write lock on rw_lock, it immediately returns FALSE. Otherwise it locks
;;; rw_lock and returns TRUE.
;;;
;;; rw_lock :
;;;     a GRWLock
;;;
;;; Returns :
;;;     TRUE if rw_lock could be locked
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rw_lock_writer_unlock ()
;;;
;;; void g_rw_lock_writer_unlock (GRWLock *rw_lock);
;;;
;;; Release a write lock on rw_lock.
;;;
;;; Calling g_rw_lock_writer_unlock() on a lock that is not held by the current
;;; thread leads to undefined behaviour.
;;;
;;; rw_lock :
;;;     a GRWLock
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rw_lock_reader_lock ()
;;;
;;; void g_rw_lock_reader_lock (GRWLock *rw_lock);
;;;
;;; Obtain a read lock on rw_lock. If another thread currently holds the write
;;; lock on rw_lock or blocks waiting for it, the current thread will block.
;;; Read locks can be taken recursively.
;;;
;;; It is implementation-defined how many threads are allowed to hold read locks
;;; on the same lock simultaneously.
;;;
;;; rw_lock :
;;;     a GRWLock
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rw_lock_reader_trylock ()
;;;
;;; gboolean g_rw_lock_reader_trylock (GRWLock *rw_lock);
;;;
;;; Tries to obtain a read lock on rw_lock and returns TRUE if the read lock was
;;; successfully obtained. Otherwise it returns FALSE.
;;;
;;; rw_lock :
;;;     a GRWLock
;;;
;;; Returns :
;;;     TRUE if rw_lock could be locked
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_rw_lock_reader_unlock ()
;;;
;;; void g_rw_lock_reader_unlock (GRWLock *rw_lock);
;;;
;;; Release a read lock on rw_lock.
;;;
;;; Calling g_rw_lock_reader_unlock() on a lock that is not held by the current
;;; thread leads to undefined behaviour.
;;;
;;; rw_lock :
;;;     a GRWLock
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GCond
;;;
;;; struct GCond {
;;; };
;;;
;;; The GCond struct is an opaque data structure that represents a condition.
;;; Threads can block on a GCond if they find a certain condition to be false.
;;; If other threads change the state of this condition they signal the GCond,
;;; and that causes the waiting threads to be woken up.
;;;
;;; Consider the following example of a shared variable. One or more threads can
;;; wait for data to be published to the variable and when another thread
;;; publishes the data, it can signal one of the waiting threads to wake up to
;;; collect the data.
;;;
;;; Example 6.  Using GCond to block a thread until a condition is satisfied
;;;
;;;   gpointer current_data = NULL;
;;;   GMutex data_mutex;
;;;   GCond data_cond;
;;;
;;;   void
;;;   push_data (gpointer data)
;;;   {
;;;     g_mutex_lock (&data_mutex);
;;;     current_data = data;
;;;     g_cond_signal (&data_cond);
;;;     g_mutex_unlock (&data_mutex);
;;;   }
;;;
;;;   gpointer
;;;   pop_data (void)
;;;   {
;;;     gpointer data;
;;;
;;;     g_mutex_lock (&data_mutex);
;;;     while (!current_data)
;;;       g_cond_wait (&data_cond, &data_mutex);
;;;     data = current_data;
;;;     current_data = NULL;
;;;     g_mutex_unlock (&data_mutex);
;;;
;;;     return data;
;;;   }
;;;
;;; Whenever a thread calls pop_data() now, it will wait until current_data is
;;; non-NULL, i.e. until some other thread has called push_data().
;;;
;;; The example shows that use of a condition variable must always be paired
;;; with a mutex. Without the use of a mutex, there would be a race between the
;;; check of current_data by the while loop in pop_data and waiting.
;;; Specifically, another thread could set pop_data after the check, and signal
;;; the cond (with nobody waiting on it) before the first thread goes to sleep.
;;; GCond is specifically useful for its ability to release the mutex and go to
;;; sleep atomically.
;;;
;;; It is also important to use the g_cond_wait() and g_cond_wait_until()
;;; functions only inside a loop which checks for the condition to be true. See
;;; g_cond_wait() for an explanation of why the condition may not be true even
;;; after it returns.
;;;
;;; If a GCond is allocated in static storage then it can be used without
;;; initialisation. Otherwise, you should call g_cond_init() on it and
;;; g_cond_clear() when done.
;;;
;;; A GCond should only be accessed via the g_cond_ functions.
;;; ----------------------------------------------------------------------------

(defcstruct g-cond)

(export 'g-cond)

;;; ----------------------------------------------------------------------------
;;; g_cond_init ()
;;;
;;; void g_cond_init (GCond *cond);
;;;
;;; Initialises a GCond so that it can be used.
;;;
;;; This function is useful to initialise a GCond that has been allocated as
;;; part of a larger structure. It is not necessary to initialise a GCond that
;;; has been statically allocated.
;;;
;;; To undo the effect of g_cond_init() when a GCond is no longer needed, use
;;; g_cond_clear().
;;;
;;; Calling g_cond_init() on an already-initialised GCond leads to undefined
;;; behaviour.
;;;
;;; cond :
;;;     an uninitialized GCond
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cond_clear ()
;;;
;;; void g_cond_clear (GCond *cond);
;;;
;;; Frees the resources allocated to a GCond with g_cond_init().
;;;
;;; This function should not be used with a GCond that has been statically
;;; allocated.
;;;
;;; Calling g_cond_clear() for a GCond on which threads are blocking leads to
;;; undefined behaviour.
;;;
;;; cond :
;;;     an initialised GCond
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cond_wait ()
;;;
;;; void g_cond_wait (GCond *cond, GMutex *mutex);
;;;
;;; Atomically releases mutex and waits until cond is signalled.
;;;
;;; When using condition variables, it is possible that a spurious wakeup may
;;; occur (ie: g_cond_wait() returns even though g_cond_signal() was not
;;; called). It's also possible that a stolen wakeup may occur. This is when
;;; g_cond_signal() is called, but another thread acquires mutex before this
;;; thread and modifies the state of the program in such a way that when
;;; g_cond_wait() is able to return, the expected condition is no longer met.
;;;
;;; For this reason, g_cond_wait() must always be used in a loop. See the
;;; documentation for GCond for a complete example.
;;;
;;; cond :
;;;     a GCond
;;;
;;; mutex :
;;;     a GMutex that is currently locked
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cond_timed_wait ()
;;;
;;; gboolean g_cond_timed_wait (GCond *cond, GMutex *mutex, GTimeVal *abs_time);
;;;
;;; Warning
;;;
;;; g_cond_timed_wait has been deprecated since version 2.32 and should not be
;;; used in newly-written code. Use g_cond_wait_until() instead.
;;;
;;; Waits until this thread is woken up on cond, but not longer than until the
;;; time specified by abs_time. The mutex is unlocked before falling asleep and
;;; locked again before resuming.
;;;
;;; If abs_time is NULL, g_cond_timed_wait() acts like g_cond_wait().
;;;
;;; This function can be used even if g_thread_init() has not yet been called,
;;; and, in that case, will immediately return TRUE.
;;;
;;; To easily calculate abs_time a combination of g_get_current_time() and
;;; g_time_val_add() can be used.
;;;
;;; cond :
;;;     a GCond
;;;
;;; mutex :
;;;     a GMutex that is currently locked
;;;
;;; abs_time :
;;;     a GTimeVal, determining the final time
;;;
;;; Returns :
;;;     TRUE if cond was signalled, or FALSE on timeout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cond_wait_until ()
;;;
;;; gboolean g_cond_wait_until (GCond *cond, GMutex *mutex, gint64 end_time);
;;;
;;; Waits until either cond is signalled or end_time has passed.
;;;
;;; As with g_cond_wait() it is possible that a spurious or stolen wakeup could
;;; occur. For that reason, waiting on a condition variable should always be in
;;; a loop, based on an explicitly-checked predicate.
;;;
;;; TRUE is returned if the condition variable was signalled (or in the case of
;;; a spurious wakeup). FALSE is returned if end_time has passed.
;;;
;;; The following code shows how to correctly perform a timed wait on a
;;; condition variable (extended the example presented in the documentation for
;;; GCond):
;;;
;;;   gpointer
;;;   pop_data_timed (void)
;;;   {
;;;     gint64 end_time;
;;;     gpointer data;
;;;
;;;     g_mutex_lock (&data_mutex);
;;;
;;;     end_time = g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
;;;     while (!current_data)
;;;       if (!g_cond_wait_until (&data_cond, &data_mutex, end_time))
;;;         {
;;;           // timeout has passed.
;;;           g_mutex_unlock (&data_mutex);
;;;           return NULL;
;;;         }
;;;
;;;     // there is data for us
;;;     data = current_data;
;;;     current_data = NULL;
;;;
;;;     g_mutex_unlock (&data_mutex);
;;;
;;;     return data;
;;;   }
;;;
;;; Notice that the end time is calculated once, before entering the loop and
;;; reused. This is the motivation behind the use of absolute time on this API
;;; -- if a relative time of 5 seconds were passed directly to the call and a
;;; spurious wakeup occurred, the program would have to start over waiting again
;;; (which would lead to a total wait time of more than 5 seconds).
;;;
;;; cond :
;;;     a GCond
;;;
;;; mutex :
;;;     a GMutex that is currently locked
;;;
;;; end_time :
;;;     the monotonic time to wait until
;;;
;;; Returns :
;;;     TRUE on a signal, FALSE on a timeout
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cond_signal ()
;;;
;;; void g_cond_signal (GCond *cond);
;;;
;;; If threads are waiting for cond, at least one of them is unblocked. If no
;;; threads are waiting for cond, this function has no effect. It is good
;;; practice to hold the same lock as the waiting thread while calling this
;;; function, though not required.
;;;
;;; cond :
;;;     a GCond
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cond_broadcast ()
;;;
;;; void g_cond_broadcast (GCond *cond);
;;;
;;; If threads are waiting for cond, all of them are unblocked. If no threads
;;; are waiting for cond, this function has no effect. It is good practice to
;;; lock the same mutex as the waiting threads while calling this function,
;;; though not required.
;;;
;;; cond :
;;;     a GCond
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GPrivate
;;;
;;; struct GPrivate {
;;; };
;;;
;;; The GPrivate struct is an opaque data structure to represent a thread-local
;;; data key. It is approximately equivalent to the
;;; pthread_setspecific()/pthread_getspecific() APIs on POSIX and to
;;; TlsSetValue()/TlsGetValue() on Windows.
;;;
;;; If you don't already know why you might want this functionality, then you
;;; probably don't need it.
;;;
;;; GPrivate is a very limited resource (as far as 128 per program, shared
;;; between all libraries). It is also not possible to destroy a GPrivate after
;;; it has been used. As such, it is only ever acceptable to use GPrivate in
;;; static scope, and even then sparingly so.
;;;
;;; See G_PRIVATE_INIT() for a couple of examples.
;;;
;;; The GPrivate structure should be considered opaque. It should only be
;;; accessed via the g_private_ functions.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_PRIVATE_INIT()
;;;
;;; #define G_PRIVATE_INIT(notify)
;;;
;;; A macro to assist with the static initialisation of a GPrivate.
;;;
;;; This macro is useful for the case that a GDestroyNotify function should be
;;; associated the key. This is needed when the key will be used to point at
;;; memory that should be deallocated when the thread exits.
;;;
;;; Additionally, the GDestroyNotify will also be called on the previous value
;;; stored in the key when g_private_replace() is used.
;;;
;;; If no GDestroyNotify is needed, then use of this macro is not required -- if
;;; the GPrivate is declared in static scope then it will be properly
;;; initialised by default (ie: to all zeros). See the examples below.
;;;
;;;   static GPrivate name_key = G_PRIVATE_INIT (g_free);
;;;
;;;   // return value should not be freed
;;;   const gchar *
;;;   get_local_name (void)
;;;   {
;;;     return g_private_get (&name_key);
;;;   }
;;;
;;;   void
;;;   set_local_name (const gchar *name)
;;;   {
;;;     g_private_replace (&name_key, g_strdup (name));
;;;   }
;;;
;;;   static GPrivate count_key;   // no free function
;;;
;;;   gint
;;;   get_local_count (void)
;;;   {
;;;     return GPOINTER_TO_INT (g_private_get (&count_key));
;;;   }
;;;
;;;   void
;;;   set_local_count (gint count)
;;;   {
;;;     g_private_set (&count_key, GINT_TO_POINTER (count));
;;;   }
;;;
;;; notify :
;;;     a GDestroyNotify
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_private_get ()
;;;
;;; gpointer g_private_get (GPrivate *key);
;;;
;;; Returns the current value of the thread local variable key.
;;;
;;; If the value has not yet been set in this thread, NULL is returned. Values
;;; are never copied between threads (when a new thread is created, for
;;; example).
;;;
;;; key :
;;;     a GPrivate
;;;
;;; Returns :
;;;     the thread-local value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_private_set ()
;;;
;;; void g_private_set (GPrivate *key, gpointer value);
;;;
;;; Sets the thread local variable key to have the value value in the current
;;; thread.
;;;
;;; This function differs from g_private_replace() in the following way: the
;;; GDestroyNotify for key is not called on the old value.
;;;
;;; key :
;;;     a GPrivate
;;;
;;; value :
;;;     the new value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_private_replace ()
;;;
;;; void g_private_replace (GPrivate *key, gpointer value);
;;;
;;; Sets the thread local variable key to have the value value in the current
;;; thread.
;;;
;;; This function differs from g_private_set() in the following way: if the
;;; previous value was non-NULL then the GDestroyNotify handler for key is run
;;; on it.
;;;
;;; key :
;;;     a GPrivate
;;;
;;; value :
;;;     the new value
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GOnce
;;;
;;; struct GOnce {
;;;   volatile GOnceStatus status;
;;;   volatile gpointer retval;
;;; };
;;;
;;; A GOnce struct controls a one-time initialization function. Any one-time
;;; initialization function must have its own unique GOnce struct.
;;;
;;; volatile GOnceStatus status;
;;;     the status of the GOnce
;;;
;;; volatile gpointer retval;
;;;     the value returned by the call to the function, if status is
;;;     G_ONCE_STATUS_READY
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GOnceStatus
;;;
;;; typedef enum {
;;;   G_ONCE_STATUS_NOTCALLED,
;;;   G_ONCE_STATUS_PROGRESS,
;;;   G_ONCE_STATUS_READY
;;; } GOnceStatus;
;;;
;;; The possible statuses of a one-time initialization function controlled by a
;;; GOnce struct.
;;;
;;; G_ONCE_STATUS_NOTCALLED
;;;     the function has not been called yet.
;;;
;;; G_ONCE_STATUS_PROGRESS
;;;     the function call is currently in progress.
;;;
;;; G_ONCE_STATUS_READY
;;;     the function has been called.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; G_ONCE_INIT
;;;
;;; #define G_ONCE_INIT { G_ONCE_STATUS_NOTCALLED, NULL }
;;;
;;; A GOnce must be initialized with this macro before it can be used.
;;;
;;;   GOnce my_once = G_ONCE_INIT;
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_once()
;;;
;;; #define g_once(once, func, arg)
;;;
;;; The first call to this routine by a process with a given GOnce struct calls
;;; func with the given argument. Thereafter, subsequent calls to g_once() with
;;; the same GOnce struct do not call func again, but return the stored result
;;; of the first call. On return from g_once(), the status of once will be
;;; G_ONCE_STATUS_READY.
;;;
;;; For example, a mutex or a thread-specific data key must be created exactly
;;; once. In a threaded environment, calling g_once() ensures that the
;;; initialization is serialized across multiple threads.
;;;
;;; Calling g_once() recursively on the same GOnce struct in func will lead to a
;;; deadlock.
;;;
;;;   gpointer
;;;   get_debug_flags (void)
;;;   {
;;;     static GOnce my_once = G_ONCE_INIT;
;;;
;;;     g_once (&my_once, parse_debug_flags, NULL);
;;;
;;;     return my_once.retval;
;;;   }
;;;
;;; once :
;;;     a GOnce structure
;;;
;;; func :
;;;     the GThreadFunc function associated to once. This function is called
;;;     only once, regardless of the number of times it and its associated GOnce
;;;     struct are passed to g_once().
;;;
;;; arg :
;;;     data to be passed to func
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_once_init_enter ()
;;;
;;; gboolean g_once_init_enter (volatile void *location);
;;;
;;; Function to be called when starting a critical initialization section. The
;;; argument location must point to a static 0-initialized variable that will be
;;; set to a value other than 0 at the end of the initialization section. In
;;; combination with g_once_init_leave() and the unique address value_location,
;;; it can be ensured that an initialization section will be executed only once
;;; during a program's life time, and that concurrent threads are blocked until
;;; initialization completed. To be used in constructs like this:
;;;
;;;   static gsize initialization_value = 0;
;;;
;;;   if (g_once_init_enter (&initialization_value))
;;;     {
;;;       gsize setup_value = 42; /** initialization code here **/
;;;
;;;       g_once_init_leave (&initialization_value, setup_value);
;;;     }
;;;
;;;   /** use initialization_value here **/
;;;
;;; location :
;;;     location of a static initializable variable containing 0
;;;
;;; Returns :
;;;     TRUE if the initialization section should be entered, FALSE and blocks
;;;     otherwise
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_once_init_leave ()
;;;
;;; void g_once_init_leave (volatile void *location, gsize result);
;;;
;;; Counterpart to g_once_init_enter(). Expects a location of a static
;;; 0-initialized initialization variable, and an initialization value other
;;; than 0. Sets the variable to the initialization value, and releases
;;; concurrent threads blocking in g_once_init_enter() on this initialization
;;; variable.
;;;
;;; location :
;;;     location of a static initializable variable containing 0
;;;
;;; result :
;;;     new non-0 value for *value_location
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bit_lock ()
;;;
;;; void g_bit_lock (volatile gint *address, gint lock_bit);
;;;
;;; Sets the indicated lock_bit in address. If the bit is already set, this call
;;; will block until g_bit_unlock() unsets the corresponding bit.
;;;
;;; Attempting to lock on two different bits within the same integer is not
;;; supported and will very probably cause deadlocks.
;;;
;;; The value of the bit that is set is (1u << bit). If bit is not between 0 and
;;; 31 then the result is undefined.
;;;
;;; This function accesses address atomically. All other accesses to address
;;; must be atomic in order for this function to work reliably.
;;;
;;; address :
;;;     a pointer to an integer
;;;
;;; lock_bit :
;;;     a bit value between 0 and 31
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bit_trylock ()
;;;
;;; gboolean g_bit_trylock (volatile gint *address, gint lock_bit);
;;;
;;; Sets the indicated lock_bit in address, returning TRUE if successful. If the
;;; bit is already set, returns FALSE immediately.
;;;
;;; Attempting to lock on two different bits within the same integer is not
;;; supported.
;;;
;;; The value of the bit that is set is (1u << bit). If bit is not between 0 and
;;; 31 then the result is undefined.
;;;
;;; This function accesses address atomically. All other accesses to address
;;; must be atomic in order for this function to work reliably.
;;;
;;; address :
;;;     a pointer to an integer
;;;
;;; lock_bit :
;;;     a bit value between 0 and 31
;;;
;;; Returns :
;;;     TRUE if the lock was acquired
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_bit_unlock ()
;;;
;;; void g_bit_unlock (volatile gint *address, gint lock_bit);
;;;
;;; Clears the indicated lock_bit in address. If another thread is currently
;;; blocked in g_bit_lock() on this same bit then it will be woken up.
;;;
;;; This function accesses address atomically. All other accesses to address
;;; must be atomic in order for this function to work reliably.
;;;
;;; address :
;;;     a pointer to an integer
;;;
;;; lock_bit :
;;;     a bit value between 0 and 31
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_pointer_bit_lock ()
;;;
;;; void g_pointer_bit_lock (volatile void *address, gint lock_bit);
;;;
;;; This is equivalent to g_bit_lock, but working on pointers (or other
;;; pointer-sized values).
;;;
;;; For portability reasons, you may only lock on the bottom 32 bits of the
;;; pointer.
;;;
;;; address :
;;;     a pointer to a gpointer-sized value
;;;
;;; lock_bit :
;;;     a bit value between 0 and 31
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_pointer_bit_trylock ()
;;;
;;; gboolean g_pointer_bit_trylock (volatile void *address, gint lock_bit);
;;;
;;; This is equivalent to g_bit_trylock, but working on pointers (or other
;;; pointer-sized values).
;;;
;;; For portability reasons, you may only lock on the bottom 32 bits of the
;;; pointer.
;;;
;;; address :
;;;     a pointer to a gpointer-sized value
;;;
;;; lock_bit :
;;;     a bit value between 0 and 31
;;;
;;; Returns :
;;;     TRUE if the lock was acquired
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_pointer_bit_unlock ()
;;;
;;; void g_pointer_bit_unlock (volatile void *address, gint lock_bit);
;;;
;;; This is equivalent to g_bit_unlock, but working on pointers (or other
;;; pointer-sized values).
;;;
;;; For portability reasons, you may only lock on the bottom 32 bits of the
;;; pointer.
;;;
;;; address :
;;;     a pointer to a gpointer-sized value
;;;
;;; lock_bit :
;;;     a bit value between 0 and 31
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; --- End of file glib.threads.lisp ------------------------------------------
