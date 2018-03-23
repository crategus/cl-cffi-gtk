;;; ----------------------------------------------------------------------------
;;; gio.cancellable.lisp
;;;
;;; The documentation has been copied from the GIO Reference Manual
;;; for GIO 2.32.3. The latest version of this documentation can be found
;;; on-line at http://library.gnome.org/devel/gio/unstable/.
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
;;;
;;; GCancellable
;;; 
;;; Thread-safe Operation Cancellation Stack
;;;     
;;; Synopsis
;;; 
;;;     GCancellable
;;;
;;;     g_cancellable_new
;;;     g_cancellable_is_cancelled
;;;     g_cancellable_set_error_if_cancelled
;;;     g_cancellable_get_fd
;;;     g_cancellable_make_pollfd
;;;     g_cancellable_release_fd
;;;     g_cancellable_source_new
;;;     g_cancellable_get_current
;;;     g_cancellable_pop_current
;;;     g_cancellable_push_current
;;;     g_cancellable_reset
;;;     g_cancellable_connect
;;;     g_cancellable_disconnect
;;;     g_cancellable_cancel
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GCancellable
;;; 
;;; Signals
;;; 
;;;   "cancelled"                                      : Run Last
;;; 
;;; Description
;;; 
;;; GCancellable is a thread-safe operation cancellation stack used throughout
;;; GIO to allow for cancellation of synchronous and asynchronous operations.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "cancelled" signal
;;; 
;;; void user_function (GCancellable *cancellable,
;;;                     gpointer      user_data)        : Run Last
;;; 
;;; Emitted when the operation has been cancelled.
;;; 
;;; Can be used by implementations of cancellable operations. If the operation
;;; is cancelled from another thread, the signal will be emitted in the thread
;;; that cancelled the operation, not the thread that is running the operation.
;;; 
;;; Note that disconnecting from this signal (or any signal) in a multi-threaded
;;; program is prone to race conditions. For instance it is possible that a
;;; signal handler may be invoked even after a call to
;;; g_signal_handler_disconnect() for that handler has already returned.
;;; 
;;; There is also a problem when cancellation happen right before connecting to
;;; the signal. If this happens the signal will unexpectedly not be emitted, and
;;; checking before connecting to the signal leaves a race condition where this
;;; is still happening.
;;; 
;;; In order to make it safe and easy to connect handlers there are two helper
;;; functions: g_cancellable_connect() and g_cancellable_disconnect() which
;;; protect against problems like this.
;;; 
;;; An example of how to us this:
;;; 
;;;   /* Make sure we don't do any unnecessary work if already cancelled */
;;;   if (g_cancellable_set_error_if_cancelled (cancellable))
;;;     return;
;;;   /* Set up all the data needed to be able to
;;;    * handle cancellation of the operation */
;;;   my_data = my_data_new (...);
;;;   
;;;   id = 0;
;;;   if (cancellable)
;;;     id = g_cancellable_connect (cancellable,
;;;                     G_CALLBACK (cancelled_handler)
;;;                     data, NULL);
;;;   
;;;   /* cancellable operation here... */
;;;   
;;;   g_cancellable_disconnect (cancellable, id);
;;;   
;;;   /* cancelled_handler is never called after this, it
;;;    * is now safe to free the data */
;;;   my_data_free (my_data);
;;; 
;;; Note that the cancelled signal is emitted in the thread that the user
;;; cancelled from, which may be the main thread. So, the cancellable signal
;;; should not do something that can block.
;;; 
;;; cancellable :
;;;     a GCancellable.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GCancellable
;;; 
;;; typedef struct _GCancellable GCancellable;
;;; 
;;; Allows actions to be cancelled.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GCancellable" g-cancellable
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "g_cancellable_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_new ()
;;; 
;;; GCancellable * g_cancellable_new (void);
;;; 
;;; Creates a new GCancellable object.
;;; 
;;; Applications that want to start one or more operations that should be
;;; cancellable should create a GCancellable and pass it to the operations.
;;; 
;;; One GCancellable can be used in multiple consecutive operations or in
;;; multiple concurrent operations.
;;; 
;;; Returns :
;;;     a GCancellable.
;;; ----------------------------------------------------------------------------

(declaim (inline g-cancellable-new))

(defun g-cancellable-new ()
  (make-instance 'g-cancellable))

(export 'g-cancellable-new)

;;; ----------------------------------------------------------------------------
;;; g_cancellable_is_cancelled ()
;;; 
;;; gboolean g_cancellable_is_cancelled (GCancellable *cancellable);
;;; 
;;; Checks if a cancellable job has been cancelled.
;;; 
;;; cancellable :
;;;     a GCancellable or NULL
;;; 
;;; Returns :
;;;     TRUE if cancellable is cancelled, FALSE if called with NULL or if item
;;;     is not cancelled.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_set_error_if_cancelled ()
;;; 
;;; gboolean g_cancellable_set_error_if_cancelled (GCancellable *cancellable,
;;;                                                GError **error);
;;; 
;;; If the cancellable is cancelled, sets the error to notify that the operation
;;; was cancelled.
;;; 
;;; cancellable :
;;;     a GCancellable or NULL
;;; 
;;; error :
;;;     GError to append error state to
;;; 
;;; Returns :
;;;     TRUE if cancellable was cancelled, FALSE if it was not
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_get_fd ()
;;; 
;;; int g_cancellable_get_fd (GCancellable *cancellable);
;;; 
;;; Gets the file descriptor for a cancellable job. This can be used to
;;; implement cancellable operations on Unix systems. The returned fd will turn
;;; readable when cancellable is cancelled.
;;; 
;;; You are not supposed to read from the fd yourself, just check for readable
;;; status. Reading to unset the readable status is done with
;;; g_cancellable_reset().
;;; 
;;; After a successful return from this function, you should use
;;; g_cancellable_release_fd() to free up resources allocated for the returned
;;; file descriptor.
;;; 
;;; See also g_cancellable_make_pollfd().
;;; 
;;; cancellable :
;;;     a GCancellable.
;;; 
;;; Returns :
;;;     A valid file descriptor. -1 if the file descriptor is not supported, or
;;;     on errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_make_pollfd ()
;;; 
;;; gboolean g_cancellable_make_pollfd (GCancellable *cancellable,
;;;                                     GPollFD *pollfd);
;;; 
;;; Creates a GPollFD corresponding to cancellable; this can be passed to
;;; g_poll() and used to poll for cancellation. This is useful both for unix
;;; systems without a native poll and for portability to windows.
;;; 
;;; When this function returns TRUE, you should use g_cancellable_release_fd()
;;; to free up resources allocated for the pollfd. After a FALSE return, do not
;;; call g_cancellable_release_fd().
;;; 
;;; If this function returns FALSE, either no cancellable was given or resource
;;; limits prevent this function from allocating the necessary structures for
;;; polling. (On Linux, you will likely have reached the maximum number of file
;;; descriptors.) The suggested way to handle these cases is to ignore the
;;; cancellable.
;;; 
;;; You are not supposed to read from the fd yourself, just check for readable
;;; status. Reading to unset the readable status is done with
;;; g_cancellable_reset().
;;; 
;;; cancellable :
;;;     a GCancellable or NULL
;;; 
;;; pollfd :
;;;     a pointer to a GPollFD
;;; 
;;; Returns :
;;;     TRUE if pollfd was successfully initialized, FALSE on failure to prepare
;;;     the cancellable.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_release_fd ()
;;; 
;;; void g_cancellable_release_fd (GCancellable *cancellable);
;;; 
;;; Releases a resources previously allocated by g_cancellable_get_fd() or
;;; g_cancellable_make_pollfd().
;;; 
;;; For compatibility reasons with older releases, calling this function is not
;;; strictly required, the resources will be automatically freed when the
;;; cancellable is finalized. However, the cancellable will block scarce file
;;; descriptors until it is finalized if this function is not called. This can
;;; cause the application to run out of file descriptors when many GCancellables
;;; are used at the same time.
;;; 
;;; cancellable :
;;;     a GCancellable
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_source_new ()
;;; 
;;; GSource * g_cancellable_source_new (GCancellable *cancellable);
;;; 
;;; Creates a source that triggers if cancellable is cancelled and calls its
;;; callback of type GCancellableSourceFunc. This is primarily useful for
;;; attaching to another (non-cancellable) source with
;;; g_source_add_child_source() to add cancellability to it.
;;; 
;;; For convenience, you can call this with a NULL GCancellable, in which case
;;; the source will never trigger.
;;; 
;;; cancellable :
;;;     a GCancellable, or NULL
;;; 
;;; Returns :
;;;     the new GSource
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GCancellableSourceFunc ()
;;; 
;;; gboolean (*GCancellableSourceFunc) (GCancellable *cancellable,
;;;                                     gpointer user_data);
;;; 
;;; This is the function type of the callback used for the GSource returned by
;;; g_cancellable_source_new().
;;; 
;;; cancellable :
;;;     the GCancellable
;;; 
;;; user_data :
;;;     data passed in by the user.
;;; 
;;; Returns :
;;;     it should return FALSE if the source should be removed.
;;; 
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_get_current ()
;;; 
;;; GCancellable * g_cancellable_get_current (void);
;;; 
;;; Gets the top cancellable from the stack.
;;; 
;;; Returns :
;;;     a GCancellable from the top of the stack, or NULL if the stack is empty
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_pop_current ()
;;; 
;;; void g_cancellable_pop_current (GCancellable *cancellable);
;;; 
;;; Pops cancellable off the cancellable stack (verifying that cancellable is on
;;; the top of the stack).
;;; 
;;; cancellable :
;;;     a GCancellable object
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_push_current ()
;;; 
;;; void g_cancellable_push_current (GCancellable *cancellable);
;;; 
;;; Pushes cancellable onto the cancellable stack. The current cancellable can
;;; then be received using g_cancellable_get_current().
;;; 
;;; This is useful when implementing cancellable operations in code that does
;;; not allow you to pass down the cancellable object.
;;; 
;;; This is typically called automatically by e.g. GFile operations, so you
;;; rarely have to call this yourself.
;;; 
;;; cancellable :
;;;     a GCancellable object
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_reset ()
;;; 
;;; void g_cancellable_reset (GCancellable *cancellable);
;;; 
;;; Resets cancellable to its uncancelled state.
;;; 
;;; If cancellable is currently in use by any cancellable operation then the
;;; behavior of this function is undefined.
;;; 
;;; cancellable :
;;;     a GCancellable object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_connect ()
;;; 
;;; gulong g_cancellable_connect (GCancellable *cancellable,
;;;                               GCallback callback,
;;;                               gpointer data,
;;;                               GDestroyNotify data_destroy_func);
;;; 
;;; Convenience function to connect to the "cancelled" signal. Also handles the
;;; race condition that may happen if the cancellable is cancelled right before
;;; connecting.
;;; 
;;; callback is called at most once, either directly at the time of the connect
;;; if cancellable is already cancelled, or when cancellable is cancelled in
;;; some thread.
;;; 
;;; data_destroy_func will be called when the handler is disconnected, or
;;; immediately if the cancellable is already cancelled.
;;; 
;;; See "cancelled" for details on how to use this.
;;; 
;;; cancellable :
;;;     A GCancellable.
;;; 
;;; callback :
;;;     The GCallback to connect.
;;; 
;;; data :
;;;     Data to pass to callback.
;;; 
;;; data_destroy_func :
;;;     Free function for data or NULL
;;; 
;;; Returns :
;;;     The id of the signal handler or 0 if cancellable has already been
;;;     cancelled.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_disconnect ()
;;; 
;;; void g_cancellable_disconnect (GCancellable *cancellable, gulong handler_id)
;;; 
;;; Disconnects a handler from a cancellable instance similar to
;;; g_signal_handler_disconnect(). Additionally, in the event that a signal
;;; handler is currently running, this call will block until the handler has
;;; finished. Calling this function from a "cancelled" signal handler will
;;; therefore result in a deadlock.
;;; 
;;; This avoids a race condition where a thread cancels at the same time as the
;;; cancellable operation is finished and the signal handler is removed. See
;;; "cancelled" for details on how to use this.
;;; 
;;; If cancellable is NULL or handler_id is 0 this function does nothing.
;;; 
;;; cancellable :
;;;     A GCancellable or NULL.
;;; 
;;; handler_id :
;;;     Handler id of the handler to be disconnected, or 0.
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_cancellable_cancel ()
;;; 
;;; void g_cancellable_cancel (GCancellable *cancellable);
;;; 
;;; Will set cancellable to cancelled, and will emit the "cancelled" signal.
;;; (However, see the warning about race conditions in the documentation for
;;; that signal if you are planning to connect to it.)
;;; 
;;; This function is thread-safe. In other words, you can safely call it from a
;;; thread other than the one running the operation that was passed the
;;; cancellable.
;;; 
;;; The convention within gio is that cancelling an asynchronous operation
;;; causes it to complete asynchronously. That is, if you cancel the operation
;;; from the same thread in which it is running, then the operation's
;;; GAsyncReadyCallback will not be invoked until the application returns to the
;;; main loop.
;;; 
;;; cancellable :
;;;     a GCancellable object.
;;; ----------------------------------------------------------------------------


;;; --- End of file gio.cancellable.lisp ---------------------------------------