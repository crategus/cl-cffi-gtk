(def-suite gdk-general :in gdk-suite)
(in-suite gdk-general)

;;;   gdk_init                                   * not implemented *
;;;   gdk_init_check                             * not implemented *
;;;   gdk_parse_args                             * not implemented *

;;;   gdk_get_display_arg_name

(test gdk-get-display-arg-name
  (is-false (gdk-get-display-arg-name)))

;;;   gdk_notify_startup_complete
;;;   gdk_notify_startup_complete_with_id

;;;   gdk_get_program_class

(defvar *first-run-program-class* t)

(test gdk-get-program-class
  (if *first-run-program-class*
      #+(and sbcl (not windows))
      (is (equal "Sbcl" (gdk-get-program-class)))
      #+(and ccl (not windows))
      (is (equal "Lx86cl" (gdk-get-program-class)))
      #+windows
      (is (equal "Sbcl.exe" (gdk-get-program-class)))
      (is (equal "Program class" (gdk-get-program-class)))))

;;;   gdk_set_program_class

(test gdk-set-program-class
  (when *first-run-program-class*
    (gdk-set-program-class "Program class")
    (setf *first-run-program-class* nil))
  (is (equal "Program class" (gdk-get-program-class))))

;;;   gdk_get_display                            * deprecated *

#-windows
(test gdk-get-display
  (is (equal (g-getenv "DISPLAY") (gdk-get-display))))

#+windows
(test gdk-get-display
  (is (equal "1\\WinSta0\\Default" (gdk-get-display))))

;;;   gdk_flush

;;;   gdk_screen_width                                deprecated
;;;   gdk_screen_height                               deprecated
;;;   gdk_screen_width_mm                             deprecated
;;;   gdk_screen_height_mm                            deprecated

;;;   gdk_pointer_grab                                deprecated
;;;   gdk_pointer_ungrab                              deprecated
;;;   gdk_pointer_is_grabbed                          deprecated

;;;   gdk_set_double_click_time

;;;   gdk_keyboard_grab                               deprecated
;;;   gdk_keyboard_ungrab                             deprecated

;;;   gdk_beep

;;;   gdk_error_trap_push
;;;   gdk_error_trap_pop
;;;   gdk_error_trap_pop_ignored

