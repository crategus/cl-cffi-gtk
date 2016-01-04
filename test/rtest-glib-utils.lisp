
(def-suite glib-utils :in glib-suite)
(in-suite glib-utils)

;;;   g_get_application_name
;;;   g_set_application_name

(defvar *first-run-application* t)

(test g-application-name
  (when *first-run-application*
    #+(and sbcl (not windows))
    (is (equal "sbcl" (g-get-application-name)))
    #+(and sbcl windows)
    (is (equal "sbcl.exe" (g-get-application-name)))
    #+(and ccl (not windows))
    (is (equal "Program" (g-get-application-name)))
    (g-set-application-name "Application"))
  (is (equal "Application" (g-get-application-name)))
  (setf *first-run-application* nil))

;;;   g_get_prgname
;;;   g_set_prgname

(defvar *first-run-program* t)

(test g-prgname
  (when *first-run-program*
    #+(and sbcl (not windows))
    (is (equal "sbcl" (g-get-prgname)))
    #+(and sbcl windows)
    (is (equal "sbcl.exe" (g-get-prgname)))
    #+(and ccl (not windows))
    (is (equal "lx86cl" (g-get-prgname)))
    (g-set-prgname "Program"))
  (is (equal "Program" (g-get-prgname)))
  (setf *first-run-program* nil))

;;;     g_get_environ
;;;     g_environ_getenv
;;;     g_environ_setenv
;;;     g_environ_unsetenv

;;;   g_getenv

#-windows
(test g-getenv
  (is (equal "/home/dieter" (g-getenv "HOME"))))
  
#+windows
(test g-getenv
  (is-false (g-getenv "HOME")))

#-windows
(test g-getenv
  (is (equal ":0" (g-getenv "DISPLAY"))))

#+windows
(test g-getenv
  (is-false (g-getenv "DISPLAY")))
  
;;;     g_setenv
;;;     g_unsetenv
;;;     g_listenv
;;;     g_get_user_name
;;;     g_get_real_name
;;;     g_get_user_cache_dir
;;;     g_get_user_data_dir
;;;     g_get_user_config_dir
;;;     g_get_user_runtime_dir
;;;
;;;     GUserDirectory
;;;
;;;     g_get_user_special_dir
;;;     g_get_system_data_dirs
;;;     g_get_system_config_dirs
;;;     g_reload_user_special_dirs_cache
;;;
;;;     g_get_host_name
;;;     g_get_home_dir
;;;     g_get_tmp_dir
;;;     g_get_current_dir
;;;     g_basename
;;;     g_dirname
;;;     g_path_is_absolute
;;;     g_path_skip_root
;;;     g_path_get_basename
;;;     g_path_get_dirname

;;;     g_build_filename

#-windows
(test g-build-filename
  (is (equal "home/dieter/Lisp"
             (g-build-filename "home" "dieter" "Lisp"))))
             
#+windows
(test g-build-filename
  (is (equal "home\\dieter\\Lisp"
             (g-build-filename "home" "dieter" "Lisp"))))
;;;     g_build_filenamev

;;;   g_build_path

(test g-build-path
  (is (equal "home/dieter/Lisp"
             (g-build-path "/" "home" "dieter" "Lisp"))))

;;;     g_build_pathv
;;;     g_format_size
;;;
;;;     GFormatSizeFlags
;;;
;;;     g_format_size_full
;;;     g_format_size_for_display
;;;
;;;     g_find_program_in_path
;;;
;;;     g_bit_nth_lsf
;;;     g_bit_nth_msf
;;;     g_bit_storage
;;;
;;;     g_spaced_primes_closest
;;;
;;;     g_atexit
;;;
;;;     g_parse_debug_string
;;;
;;;     GDebugKey
;;;
;;;     g_qsort_with_data
;;;
;;;     g_nullify_pointer

