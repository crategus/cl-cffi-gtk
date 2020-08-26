(def-suite gtk-main-loop :in gtk-suite)
(in-suite gtk-main-loop)

(defvar *verbose-gtk-main-loop* t)

;;;   gtk_disable_setlocale                    * not exported *

;;;   gtk_get_default_language

(test gtk-default-language
  (is (eq 'pango-language (type-of (gtk-default-language)))))

;;;   gtk_get_locale_direction

(test gtk-locale-direction
  (is (eq :ltr (gtk-locale-direction))))

;;;   gtk_parse_args                           * not implemented *
;;;   gtk_init                                 * not exported *
;;;   gtk_init_check                           * not exported *
;;;   gtk_init_with_args                       * not implemented *

;;;   gtk_get_option_group

(test gtk-option-group
  (is-true (pointerp (gtk-option-group nil)))
  (is-true (pointerp (gtk-option-group t))))

;;;   gtk_events_pending

#+nil
(defun my-event-handler (event)
  (when *verbose-gtk-main-loop*
    (format t "~&in MY-EVENT-HANDLER with event:~%")
    (format t "~a~%" event))
  ;; Pass the event to GTK event handler
  (gtk-main-do-event event))

#+nil
(defun gtk-events-pending-callback ()
  (is (= 1 (gtk-main-level)))
  (when *verbose-gtk-main-loop*
    (format t "~&in GTK-EVENTS-PENDING-CALLBACK~%"))
  (gdk-event-put (make-gdk-event-button :type :button-press
                                        :x 0.0d0
                                        :y 0.0d0
                                        :x-root 0.0d0
                                        :y-root 0.0d0
                                        :axes '(0.0d0 0.0d0)
                                        :state :button1-mask
                                        :device
                                        (gdk-seat-pointer (gdk-display-default-seat (gdk-display-default)))
                                        :button 1
                                        :time +gdk-current-time+))
  (gdk-event-put (gdk-event-new :key-press))
  (loop while (not (gtk-events-pending))
        do (when *verbose-gtk-main-loop*
             (format t "No Events are pending.~%")))
  (gtk-main-quit)
  +g-source-remove+)

;; TODO:  Find examples to show the functionality of the functions for events.

#+nil
(test gtk-events-pending
  (is (= 0 (gtk-main-level)))
  (gdk-event-handler-set #'my-event-handler)
  (g-timeout-add 100 #'gtk-events-pending-callback)
  (gtk-main)
  (gdk-event-handler-set #'gtk-main-do-event)
  (is (= 0 (gtk-main-level))))

;;;   gtk_main
;;;   gtk_main_quit
;;;   gtk_main_level

(defun main-idle-cb ()
  (is (= 1 (gtk-main-level)))
  ;; Quit the main loop.
  (gtk-main-quit)
  ;; Remove the idle source.
  +g-source-remove+)

(test gtk-main.1
  ;; Add a idle source to the main loop.
  (g-idle-add #'main-idle-cb)
  ;; Start the main loop.
  ;; We return if gtk-main-quit is called in the idle callback.
  (gtk-main)
  (is (= 0 (gtk-main-level))))

(let ((counter 0))
  (defun main-timeout-callback ()
    (incf counter)
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

(test gtk-main.2
  (let ()
    ;; Add a timeout source to the main loop.
    (g-timeout-add 100 #'main-timeout-callback)
    ;; Start the main loop. We return if gtk-main-quit is called in the timeout.
    (gtk-main)
    (is (= 0 (gtk-main-level)))))

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

;;;     gtk_key_snooper_install                  * deprecated *
;;;     gtk_key_snooper_remove                   * deprecated *

;;;     gtk_get_current_event
;;;     gtk_get_current_event_time
;;;     gtk_get_current_event_state
;;;     gtk_get_current_event_device

;;;     gtk_get_event_widget
;;;     gtk_propagate_event

