;;; GLib Event Loop
;;;
;;; from Nokia Symbian Developer's Library v1.1
;;;
;;; Copyright (C) 2010 Nokia Corporation and/or its subsidiary(-ies).
;;; All rights reserved. Unless otherwise stated, these materials are provided
;;; under the terms of the Eclipse Public License v1.0.
;;;
;;; The GLib event loop manages all the sources of an event available for GLib.
;;; These events can come from different kinds of sources like file descriptors
;;; (plain file descriptors, sockets, or pipes), time-outs, or any kind of
;;; source that can be added. To allow multiple independent sets of events to be
;;; handled in different threads, each source is associated with a GMainContext.
;;;
;;; Each event source is given a priority. The default priority is
;;; G_PRIORITY_DEFAULT and its value is 0. Values less than 0 denote higher
;;; priority and values greater than zero denote lower priority. The events from
;;; higher priority sources are processed earlier than events from lower
;;; priority sources.
;;;
;;; The GMainLoop data type represents an event loop. GMainContext is a
;;; parameter to GMainLoop. If GMainContext is passed as NULL, then a main loop
;;; with the default context is created. After the sources are added to
;;; GMainContext and a GMainLoop variable is created, g_main_loop_run() is
;;; called. This checks continuously for events from its sources and dispatches
;;; them. Finally when all the events have been processed, g_main_loop_quit()
;;; must be called to return from g_main_loop_run().
;;;
;;; Note:
;;; Sources are associated with GMainContext and not with GMainLoop. Events from
;;; sources will be checked and dispatched from all the GMainLoop the
;;; GMainContext is associated with.
;;;
;;; Creating an event loop
;;;
;;; GLib provides ready-made functions for adding the following sources either
;;; to the default context or to a custom context:
;;;
;;;     time-outs
;;;     I/O
;;;     child watch
;;;     idle source (events from idle sources are ready if none of the other
;;;     sources with priority > G_PRIORITY_DEFAULT are ready)
;;;
;;; The following example code demonstrates the way to add a time-out source to
;;; the default context. The program makes 10 calls to timeout_callback(), which
;;; at the 10th call, calls g_main_loop_quit to make the main loop return.

(defpackage :demo-glib
  (:use :glib :cffi :common-lisp))

(in-package :demo-glib)

(let ((counter 0))
  (defun timeout-callback (loop)
    (incf counter)
    (format t "timout-callback called ~d times~%" counter)
    (if (>= counter 10)
        (progn
          ;; Reset the counter
          (setf counter 0)
          ;; Stop the main loop from running
          (g-main-loop-quit loop)
          ;; Stop the source
          +g-source-remove+
        )
        ;; Continue the source
        +g-source-continue+)))

(defun main-1 ()
  (let ((loop (g-main-loop-new (null-pointer) nil)))
    ;; Add a timeout source to the main loop.
    (g-timeout-add 100
                   ;; Our callback function.
                   (lambda ()
                     (timeout-callback loop)))
    ;; Start the main loop. We return if g-main-loop-quit is called.
    (g-main-loop-run loop)
    ;; Free the resources for the main loop.
    (g-main-loop-unref loop)))

(export 'main-1)

;;; The following code demonstrates the method to add the time-out source to a
;;; different context than the default context:

(defun main-2 ()
  (let* ((source (g-timeout-source-new 500))
         (context (g-main-context-new))
         (id (g-source-attach source context))
         (loop (g-main-loop-new context nil)))
    (declare (ignore id))
    (g-source-set-callback source (lambda () (timeout-callback loop)))
    (g-main-loop-run loop)
    (g-main-loop-unref loop)))

(export 'main-2)

;;; The same process can be used for other default sources like child watch,
;;; I/O, and idle source since they are GLib APIs that allow the creation of a
;;; source directly.
;;;
;;; Creating a new source
;;;
;;; The previous section applies only to four types of sources (time-outs, I/O,
;;; child watch, and idle source ), for which GLib provides ready-made
;;; functions. The program below demonstrates the creation of a new source.


(defcallback prepare :boolean ((source :pointer)
                               (timeout :int))
  (declare (ignore source))
  (setf timeout -1)
  t)

(defcallback check :boolean ((source :pointer))
  (declare (ignore source))
  t)

(defcallback dispatch :boolean ((source :pointer)
                                (callback :pointer)
                                (user-data :pointer))
  (declare (ignore source callback))
  (if (funcall (glib::get-stable-pointer-value user-data))
      t
      nil))

(defun main-3 ()
  (with-foreign-object (sourcefuncs '(:struct g-source-funcs))
    (setf (foreign-slot-value sourcefuncs '(:struct g-source-funcs) 'glib::prepare)
          (callback prepare)
          (foreign-slot-value sourcefuncs '(:struct g-source-funcs) 'glib::check)
          (callback check)
          (foreign-slot-value sourcefuncs '(:struct g-source-funcs) 'glib::dispatch)
          (callback dispatch)
          (foreign-slot-value sourcefuncs '(:struct g-source-funcs) 'glib::finalize)
          (cffi:null-pointer))
    (let* ((source (g-source-new sourcefuncs (foreign-type-size '(:struct g-source))))
           (context (g-main-context-new))
           (id (g-source-attach source context))
           (loop (g-main-loop-new context nil)))
      (g-source-set-callback source (lambda () (timeout-callback loop)))
      (g-main-loop-run loop)
      (g-main-loop-unref loop)
    )))

(export 'main-3)

;;; The creation of a new source requires us to define at least 3 functions:
;;;
;;; prepare():
;;;     Called before all the file descriptors are polled. If the source can
;;;     determine that it is ready here (without waiting for the results of the
;;;     poll() call), it should return TRUE. It can also return a time-out value
;;;     which should be the maximum time-out (in milliseconds) which should be
;;;     passed to the poll() call. The actual time-out used will be -1 if all
;;;     sources returned -1, or it will be the minimum of all the timeout_values
;;;     returned which were >= 0
;;;
;;; check():
;;;     Called after all the file descriptors are polled. The source should
;;;     return TRUE if it is ready to be dispatched.
;;;     Note: Time may have passed since the previous prepare function was
;;;     called, so the source should be checked again.
;;;
;;; dispatch():
;;;     Called to dispatch the event source after it has returned TRUE in either
;;;     its prepare or check function. The dispatch function is passed in a
;;;     callback function and data. The callback function may be NULL if the
;;;     source was never connected to a callback using g_source_set_callback().
;;;     The dispatch function should call the callback function with user_data
;;;     and the additional parameters that are needed for this type of event
;;;     source.
;;;
;;; Customizing the main loop iteration
;;;
;;; Single iteration of GMainContext can be run in g_main_context_iteration ().
;;; When a more detailed control of how the main loop runs is desired call the
;;; component function of g_main_context iteration() directly.
;;; The component functions of g_main_context iteration() are listed below:
;;; g_main_context_prepare()
;;; g_main_context_query()
;;; g_main_context_check()

