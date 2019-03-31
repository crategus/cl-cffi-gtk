
;; We need GTK+ to have a button widget.
(asdf:load-system :cl-cffi-gtk)

(def-suite gobject-signals :in gobject-suite)
(in-suite gobject-signals)

(test signal-info-id
  (let ((pressed-id (g-signal-lookup "pressed" "GtkButton"))
        (released-id (g-signal-lookup "released" "GtkButton"))
        (clicked-id (g-signal-lookup "clicked" "GtkButton"))
        (enter-id (g-signal-lookup "enter" "GtkButton"))
        (leave-id (g-signal-lookup "leave" "GtkButton"))
        (activate-id (g-signal-lookup "activate" "GtkButton")))
  (is (equal (list activate-id clicked-id pressed-id released-id enter-id leave-id)
             (mapcar #'signal-info-id (list-signals "GtkButton"))))))

(test signal-info-name
  (is (equal '("activate" "clicked" "pressed" "released" "enter" "leave")
             (mapcar #'signal-info-name (list-signals "GtkButton")))))

#+nil             
(test signal-info-flags
  (is (equal '((:RUN-FIRST :ACTION)
               (:RUN-FIRST)
               (:RUN-FIRST)
               (:RUN-FIRST :ACTION)
               (:RUN-FIRST)
               (:RUN-FIRST))
             (mapcar #'signal-info-flags (list-signals "GtkButton")))))

(test signal-info-param-types
  (is (equal '(nil nil nil nil nil nil)
             (mapcar #'signal-info-param-types (list-signals "GtkButton")))))

(test signal-info-detail
  (is (equal '(nil nil nil nil nil nil)
             (mapcar #'signal-info-detail (list-signals "GtkButton")))))
             

;;;     GSignalInvocationHint
;;;     GSignalCMarshaller
;;;     GSignalFlags
;;;     GSignalMatchType
;;;     GSignalQuery
;;;
;;;     G_SIGNAL_TYPE_STATIC_SCOPE
;;;     G_SIGNAL_MATCH_MASK
;;;     G_SIGNAL_FLAGS_MASK
;;;
;;;     g_signal_new
;;;     g_signal_newv
;;;     g_signal_new_valist

;;;   g_signal_query

(test g-signal-query
  (let* ((signal-id (g-signal-lookup "clicked" "GtkButton"))
         (query (g-signal-query signal-id)))
      (is (= signal-id (signal-info-id query)))
      (is (equal "clicked" (signal-info-name query)))
      (is (equal "GtkButton" (g-type-name (signal-info-owner-type query))))
      (is (equal '(:action :run-first)
                  (stable-sort (signal-info-flags query)
                               #'string-lessp)))
      (is (equal "void" (g-type-name (signal-info-return-type query))))
      (is-false (signal-info-param-types query))
      (is-false (signal-info-detail query))))

;;;   g_signal_lookup
;;;   g_signal_name

(test g-signal-name
  (let ((pressed-id (g-signal-lookup "pressed" "GtkButton"))
        (released-id (g-signal-lookup "released" "GtkButton"))
        (clicked-id (g-signal-lookup "clicked" "GtkButton"))
        (enter-id (g-signal-lookup "enter" "GtkButton"))
        (leave-id (g-signal-lookup "leave" "GtkButton"))
        (activate-id (g-signal-lookup "activate" "GtkButton")))
    (is (equal "pressed" (g-signal-name pressed-id)))
    (is (equal "released" (g-signal-name released-id)))
    (is (equal "clicked" (g-signal-name clicked-id)))
    (is (equal "enter" (g-signal-name enter-id)))
    (is (equal "leave" (g-signal-name leave-id)))
    (is (equal "activate" (g-signal-name activate-id)))))

;;;   g_signal_list_ids

(test g-signal-list-ids
  (let ((pressed-id (g-signal-lookup "pressed" "GtkButton"))
        (released-id (g-signal-lookup "released" "GtkButton"))
        (clicked-id (g-signal-lookup "clicked" "GtkButton"))
        (enter-id (g-signal-lookup "enter" "GtkButton"))
        (leave-id (g-signal-lookup "leave" "GtkButton"))
        (activate-id (g-signal-lookup "activate" "GtkButton")))
    (is-false (g-signal-list-ids "gboolean"))
    (is (equal '(1) (g-signal-list-ids "GObject")))
    (is (equal (list activate-id clicked-id pressed-id released-id enter-id leave-id)
               (g-signal-list-ids "GtkButton")))))

;;;   g_signal_emit

(test g-signal-emit
  (let* ((message nil)
         (button (make-instance 'gtk-button))
;         (signal-id (g-signal-lookup "clicked" "GtkButton"))
         ;; Connect a signal handler
         (handler-id (g-signal-connect button "clicked"
                       (lambda (widget)
                         (declare (ignore widget))
                         (setf message "Signal 'clicked' for button")
                         t))))
    ;; The signal handler writes a message in the variable MESSAGE.
    ;; We emit the signal and check the value of MESSAGE.
    (is-true (integerp handler-id))
    (is-false (setf message nil))
    (g-signal-emit button "clicked")
    (is (equal "Signal 'clicked' for button" message))))

;;;     g_signal_emit_by_name
;;;     g_signal_emitv
;;;     g_signal_emit_valist
;;;     g_signal_connect
;;;     g_signal_connect_after
;;;     g_signal_connect_swapped
;;;     g_signal_connect_object
;;;
;;;     GConnectFlags
;;;
;;;     g_signal_connect_data
;;;     g_signal_connect_closure
;;;     g_signal_connect_closure_by_id

;;;   g_signal_handler_block
;;;   g_signal_handler_unblock

(test g-signal-handler-block
  (let* ((button (make-instance 'gtk-button))
         (signal-id (g-signal-lookup "clicked" "GtkButton"))
         (handler-id (g-signal-connect button "clicked"
                       (lambda (widget)
                         (declare (ignore widget))
                         t))))
    ;; Block the handler
    (g-signal-handler-block button handler-id)
    (is-false (g-signal-has-handler-pending button signal-id (null-pointer) nil))
    (is-true (g-signal-has-handler-pending button signal-id (null-pointer) t))    
    ;; Unblock the handler
    (g-signal-handler-unblock button handler-id)
    (is-true (g-signal-has-handler-pending button signal-id (null-pointer) nil))
    (is-true (g-signal-has-handler-pending button signal-id (null-pointer) t))))

;;;     g_signal_handler_disconnect

;;;   g_signal_handler_find

(test g-signal-handler-find
  (let* ((button (make-instance 'gtk-button))
         (signal-id (g-signal-lookup "clicked" "GtkButton"))
         (handler-id (g-signal-connect button "clicked"
                       (lambda (widget)
                         (declare (ignore widget))
                         t))))
    (is (= handler-id (g-signal-handler-find button signal-id)))))

;;;     g_signal_handlers_block_matched
;;;     g_signal_handlers_unblock_matched
;;;     g_signal_handlers_disconnect_matched

;;;   g_signal_handler_is_connected

(test g-signal-handler-is-connected
  (let* ((button (make-instance 'gtk-button))
;         (signal-id (g-signal-lookup "clicked" "GtkButton"))
         ;; Connect a signal handler
         (handler-id (g-signal-connect button "clicked"
                       (lambda (widget)
                         (declare (ignore widget))
                         t))))
    (is-true (g-signal-handler-is-connected button handler-id))))

;;;     g_signal_handlers_block_by_func
;;;     g_signal_handlers_unblock_by_func
;;;     g_signal_handlers_disconnect_by_func
;;;     g_signal_handlers_disconnect_by_data

;;;   g_signal_has_handler_pending

(test g-signal-has-handler-pending
  (let* ((button (make-instance 'gtk-button))
         (signal-id (g-signal-lookup "clicked" "GtkButton"))
         (handler-id (g-signal-connect button "clicked"
                       (lambda (widget)
                         (declare (ignore widget))
                         t))))
    (is-true (integerp handler-id))
    ;; We have a signal handler for the signal "clicked"
    (is-true (g-signal-has-handler-pending button signal-id (null-pointer) t))
    (is-true (g-signal-has-handler-pending button signal-id (null-pointer) nil))
    ;; We have no signal handler for the signal "pressed"
    (is-false (g-signal-has-handler-pending button (g-signal-lookup "pressed" "GtkButton") (null-pointer) t))
    (is-false (g-signal-has-handler-pending button (g-signal-lookup "pressed" "GtkButton") (null-pointer) nil))))

;;;     g_signal_stop_emission
;;;     g_signal_stop_emission_by_name
;;;     g_signal_override_class_closure
;;;     g_signal_chain_from_overridden
;;;     g_signal_new_class_handler
;;;     g_signal_override_class_handler
;;;     g_signal_chain_from_overridden_handler
;;;     g_signal_add_emission_hook
;;;     g_signal_remove_emission_hook
;;;     g_signal_parse_name
;;;     g_signal_get_invocation_hint
;;;     g_signal_type_cclosure_new
;;;     g_signal_accumulator_first_wins
;;;     g_signal_accumulator_true_handled             

