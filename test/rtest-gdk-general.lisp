(def-suite gdk-general :in gdk-suite)
(in-suite gdk-general)

;;;     gdk_init                                           not implemented
;;;     gdk_init_check                                     not implemented
;;;     gdk_parse_args                                     not implemented

;;;     gdk_get_display_arg_name

(test gdk-get-display-arg-name
  (is-false (gdk-get-display-arg-name)))

;;;     gdk_notify_startup_complete
;;;     gdk_notify_startup_complete_with_id
;;;     gdk_set_allowed_backends

;;;     gdk_get_program_class
;;;     gdk_set_program_class

(defvar *first-run-program-class* t)

(test gdk-program-class.1
  (if *first-run-program-class*
      #+(and sbcl (not windows))
      (is (string= "Sbcl" (gdk-program-class)))
      #+(and ccl (not windows))
      (is (string= "Lx86cl" (gdk-program-class)))
      #+windows
      (is (string= "Sbcl" (gdk-program-class)))
      (is (string= "Program class" (gdk-program-class)))))

(test gdk-program-class.2
  (when *first-run-program-class*
    (setf (gdk-program-class) "Program class")
    (setf *first-run-program-class* nil))
  (is (string= "Program class" (gdk-program-class))))

;;;     gdk_get_display                                    not exported
;;;     gdk_flush                                          not exported
;;;     gdk_screen_width                                   not exported
;;;     gdk_screen_height                                  not exported
;;;     gdk_screen_width_mm                                not exported
;;;     gdk_screen_height_mm                               not exported
;;;     gdk_pointer_grab                                   not exported
;;;     gdk_pointer_ungrab                                 not exported
;;;     gdk_pointer_is_grabbed                             not exported
;;;     gdk_set_double_click_time                          not exported
;;;     gdk_keyboard_grab                                  not exported
;;;     gdk_keyboard_ungrab                                not exported
;;;     gdk_beep                                           not exported
;;;     gdk_error_trap_push                                not exported
;;;     gdk_error_trap_pop                                 not exported
;;;     gdk_error_trap_pop_ignored                         not exported

;;; 2021-10-14
