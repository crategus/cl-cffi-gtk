
(def-suite gtk-main-loop :in gtk-suite)
(in-suite gtk-main-loop)

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

;;;   gtk_main_level

(test gtk-main-level
  (is (= 0 (gtk-main-level)))
  (ensure-gtk-main)
  (sleep 1)
  (is (= 1 (gtk-main-level)))
  (leave-gtk-main)
  (sleep 1)
  (is (= 0 (gtk-main-level))))

;;;     gtk_main_quit
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

