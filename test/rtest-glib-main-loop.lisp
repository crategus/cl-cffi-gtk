(def-suite glib-main-loop :in glib-suite)
(in-suite glib-main-loop)

(defvar *verbose-glib-main-loop* nil)

;;; A callback function for use as a source function

(let ((counter 0))
  (defun timeout-callback (loop)
    (incf counter)
    (when *verbose-glib-main-loop*
      (format t "~&timout-callback called ~d times~%" counter))
    (if (>= counter 10)
        (progn
          ;; Reset the counter
          (setf counter 0)
          ;; Stop the main loop from running
          (g-main-loop-quit loop)
          ;; Stop the source
          +g-source-remove+)
        ;; Continue the source
        +g-source-continue+)))

;;;   GMainLoop

(defvar *main-loop* nil)
(defvar *main-thread* nil)
(defvar *main-thread-level* nil)
(defvar *main-thread-lock* (bt:make-lock "main-thread lock"))

(test g-main-loop
  (bt:with-lock-held (*main-thread-lock*)
    (when (and *main-thread* (not (bt:thread-alive-p *main-thread*)))
      (setf *main-thread* nil))
    (unless *main-thread*
      (setf *main-thread*
            (bt:make-thread (lambda ()
                              (setf *main-loop*
                                    (g-main-loop-new (null-pointer) nil))
                              (g-main-loop-run *main-loop*))
                            :name "rtest-glib-thread")
            *main-thread-level* 0))
    (incf *main-thread-level*))

    (sleep 1)
    (is-true (bt:thread-alive-p *main-thread*))
    (is (= 1 *main-thread-level*))
    (is-true (pointerp *main-loop*))
    (is-true (g-main-loop-is-running *main-loop*))
    (is (= 0 (g-main-depth)))

    (is-true (pointer-eq *main-loop*
                         (g-main-loop-ref *main-loop*)))
    (g-main-loop-unref *main-loop*)

    (is-true (pointer-eq (g-main-context-default)
                         (g-main-loop-context *main-loop*)))
    (is-false (g-main-context-is-owner (g-main-context-default)))

    (bt:with-lock-held (*main-thread-lock*)
      (decf *main-thread-level*)
      (when (zerop *main-thread-level*)
        ;; instead of gtk-main-quit
        (g-main-loop-quit *main-loop*)))

    (sleep 1)
    (is-false (bt:thread-alive-p *main-thread*))
    (is (= 0 *main-thread-level*))
    (is-true (pointerp *main-loop*))
    (is-false (g-main-loop-is-running *main-loop*)))

;;;   GMainContext

(test g-main-context
  (let ((context (g-main-context-new)))
    (is-true (pointerp context))
    (is-true (pointer-eq context (g-main-context-ref context)))
    (g-main-context-unref context)
    (g-main-context-unref context)))

;;;     GPollFD
;;;     GSource
;;;     GSourceFuncs

;;;   g_main_loop_new
;;;   g_main_loop_ref
;;;   g_main_loop_unref
;;;   g_main_loop_quit
;;;   g_main_loop_is_running
;;;   g_main_loop_get_context

(test g-main-loop-new
  (let ((loop (g-main-loop-new (null-pointer) t)))
    (is-true (pointerp loop))
    (is-true (not (null-pointer-p loop)))
    (is-true (g-main-loop-is-running loop))
    (is-true (pointer-eq loop (g-main-loop-ref loop)))
    (g-main-loop-unref loop)
    (is-true (pointer-eq (g-main-context-default)
                         (g-main-loop-context loop)))
    (g-main-loop-quit loop)
    (is-false (g-main-loop-is-running loop))
    (g-main-loop-unref loop)))

;;;     g_main_loop_run

;;;     g_main_new
;;;     g_main_destroy
;;;     g_main_run
;;;     g_main_quit
;;;     g_main_is_running

;;;     G_PRIORITY_HIGH
;;;     G_PRIORITY_DEFAULT
;;;     G_PRIORITY_HIGH_IDLE
;;;     G_PRIORITY_DEFAULT_IDLE
;;;     G_PRIORITY_LOW

(test g-priority-constants
  (is (= -100 +g-priority-high+))
  (is (=    0 +g-priority-default+))
  (is (=  100 +g-priority-high-idle+))
  (is (=  200 +g-priority-default-idle+))
  (is (=  300 +g-priority-low+)))

;;;     G_SOURCE_CONST
;;;     G_SOURCE_REMOVE

(test g-source-constants
  (is-true +g-source-continue+)
  (is-false +g-source-remove+))

;;;   g_main_context_new
;;;   g_main_context_ref
;;;   g_main_context_unref
;;;   g_main_context_default

(test g-main-context-new
  (let ((context (g-main-context-new)))
    (is-true (pointerp context))
    (is-true (pointer-eq context (g-main-context-ref context)))
    (g-main-context-unref context)
    (g-main-context-unref context)
    (is-true (not (pointer-eq context (g-main-context-default))))))

;;;     g_main_context_iteration

;; TODO: FIND EXAMPLE CODE TO TEST

(test g-main-context-iteration
  (let* ((source (g-timeout-source-new 500))
         (context (g-main-context-new))
         (id (g-source-attach source context))
         (loop (g-main-loop-new context t)))
    (g-source-set-callback source (lambda () (timeout-callback loop)))
    (is-true (g-main-loop-is-running loop))
    (is-true (g-main-context-find-source-by-id context id))
    (is-true (pointer-eq source
                         (g-main-context-find-source-by-id context id)))
    (is-false (g-main-context-pending context))
    (is-true (g-main-context-iteration context t)) ; TODO: Check this
))

;;;     g_main_iteration
;;;     g_main_context_pending
;;;     g_main_pending

;;;   g_main_context_find_source_by_id

(test g-main-context-find-source-by-id
  (let* ((source (g-timeout-source-new 500))
         (context (g-main-context-new))
         (id (g-source-attach source context)))
    (is-true (g-main-context-find-source-by-id context id))
    (is-true (pointer-eq source
                         (g-main-context-find-source-by-id context id)))))

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

;; This functionality is no longer exported

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

#+nil
(test g-source-new
  (with-foreign-object (sourcefuncs '(:struct g-source-funcs))
    (setf (foreign-slot-value sourcefuncs
                              '(:struct g-source-funcs) 'glib::prepare)
          (callback prepare)
          (foreign-slot-value sourcefuncs
                              '(:struct g-source-funcs) 'glib::check)
          (callback check)
          (foreign-slot-value sourcefuncs
                              '(:struct g-source-funcs) 'glib::dispatch)
          (callback dispatch)
          (foreign-slot-value sourcefuncs
                              '(:struct g-source-funcs) 'glib::finalize)
          (cffi:null-pointer))
    (let* ((source (g-source-new sourcefuncs
                       (foreign-type-size '(:struct g-source))))
           (context (g-main-context-new))
           (id (g-source-attach source context))
           (loop (g-main-loop-new context nil)))
      (g-source-set-callback source (lambda () (timeout-callback loop)))

      (is-false (g-source-is-destroyed source))
      (is (= +g-priority-default+ (g-source-get-priority source)))
      (is-false (g-source-get-can-recurse source))

      (is (= id (g-source-id source)))
      (g-source-set-name source "timeout-callback")
      (is (equal "timeout-callback" (g-source-get-name source)))
      (g-source-set-name-by-id id "name set by id")
      (is (equal "timeout-callback" (g-source-get-name source)))
      (is-true (pointer-eq context (g-source-get-context source)))

      (g-main-loop-run loop)
      (g-main-loop-unref loop)
    )))

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
;;;     g_source_get_current_time
;;;     g_source_remove
;;;     g_source_remove_by_funcs_user_data
;;;     g_source_remove_by_user_data

;;; 2021-10-18
