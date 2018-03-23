(in-package :gtk-testsuite)

(def-suite gtk-main-loop :in gtk-suite)
(in-suite gtk-main-loop)

(defvar *verbose-gtk-main-loop* nil)

;;;   gtk_disable_setlocale                    * not exported *

;;;   gtk_get_default_language

(test gtk-get-default-language
  (is (eq 'pango-language (type-of (gtk-get-default-language)))))

;;;   gtk_parse_args                           * not implemented *
;;;   gtk_init                                 * not exported *
;;;   gtk_init_check                           * not exported *
;;;   gtk_init_with_args                       * not implemented *

;;;   gtk_get_option_group

(test gtk-get-option-group
  (is-true (pointerp (gtk-get-option-group nil))))

;;;   gtk_events_pending

;;;   gtk_main
;;;   gtk_main_quit
;;;   gtk_main_level

(defun main-idle-cb ()
;  (format t "~&Execute main-idle-cb in level ~A.~%"
;            (gtk-main-level))
  ;; Quit the main loop.
  (gtk-main-quit)
  ;; Remove the idle source.
  +g-source-remove+)

(test gtk-main
  ;; Add a idle source to the main loop.
  (g-idle-add #'main-idle-cb)
  ;; Start the main loop.
  ;; We return if gtk-main-quit is called in the idle callback.
  (gtk-main))

(defun main ()
  ;; Add a idle source to the main loop.
  (g-idle-add #'main-idle-cb)
  ;; Start the main loop.
  ;; We return if gtk-main-quit is called in the idle callback.
  (gtk-main))

(let ((counter 0))
  (defun main-timeout-callback ()
    (incf counter)
    (is (= 1 (gtk-main-level)))
;    (when *verbose-gtk-main-loop*
;      (format t "~&main-timeout-callback called ~d times~%" counter))
    (is (= 1 (gtk-main-level)))
    (if (>= counter 3)
        (progn
          ;; Reset the counter
          (setf counter 0)
          ;; Stop the main loop from running
          (gtk-main-quit)
          ;; Stop the source
          +g-source-remove+)
        ;; Continue the source
        +g-source-continue+)))

(test gtk-main.1
  (let ()
    ;; Add a timeout source to the main loop.
    (g-timeout-add 100 #'main-timeout-callback)
    ;; Start the main loop. We return if gtk-main-quit is called in the timeout.
    (gtk-main)))



;;;     gtk_main_iteration
;;;     gtk_main_iteration_do
;;;     gtk_main_do_event
;;;     gtk_true                                 * not implemented *
;;;     gtk_false                                * not implemented *
;;;     gtk_grab_add
;;;     gtk_grab_get_current
;;;     gtk_grab_remove
;;;     gtk_device_grab_add
;;;     gtk_device_grab_remove
;;;
;;;     GTK_PRIORITY_RESIZE
;;;
;;;     gtk_key_snooper_install                  * deprecated *
;;;     gtk_key_snooper_remove                   * deprecated *
;;;
;;;     gtk_get_current_event
;;;     gtk_get_current_event_time
;;;     gtk_get_current_event_state
;;;     gtk_get_current_event_device
;;;     gtk_get_event_widget
;;;     gtk_propagate_event

