(def-suite glib-utils :in glib-suite)
(in-suite glib-utils)

;;; --- Types and Values -------------------------------------------------------

;;;     GUserDirectory

;;;     G_OS_INFO_KEY_NAME
;;;     G_OS_INFO_KEY_PRETTY_NAME
;;;     G_OS_INFO_KEY_VERSION
;;;     G_OS_INFO_KEY_VERSION_CODENAME
;;;     G_OS_INFO_KEY_VERSION_ID
;;;     G_OS_INFO_KEY_ID
;;;     G_OS_INFO_KEY_HOME_URL
;;;     G_OS_INFO_KEY_DOCUMENTATION_URL
;;;     G_OS_INFO_KEY_SUPPORT_URL
;;;     G_OS_INFO_KEY_BUG_REPORT_URL
;;;     G_OS_INFO_KEY_PRIVACY_POLICY_URL

;;;     GFormatSizeFlags
;;;     GDebugKey

;;; --- Functions --------------------------------------------------------------

;;;     g-application-name

(defvar *first-run-application* t)

(test g-application-name
  (when *first-run-application*
    #+(and sbcl (not windows))
    (is (string= "sbcl" (g-application-name)))
    #+(and sbcl windows)
    (is (string= "sbcl.exe" (g-application-name)))
    #+(and ccl (not windows))
    (is (string= "Program" (g-application-name)))
    (is (string= "Application" (setf (g-application-name) "Application"))))
  (is (string= "Application" (g-application-name)))
  (setf *first-run-application* nil))

;;;     g-prgname

(defvar *first-run-program* t)

(test g-prgname
  (when *first-run-program*
    #+(and sbcl (not windows))
    (is (string= "sbcl" (g-prgname)))
    #+(and sbcl windows)
    (is (string= "sbcl.exe" (g-prgname)))
    #+(and ccl (not windows))
    (is (string= "lx86cl" (g-prgname)))
    (is (string= "Program" (setf (g-prgname) "Program"))))
  (is (string= "Program" (g-prgname)))
  (setf *first-run-program* nil))

;;;     g-environ

(test g-environ
  (is (every #'stringp (g-environ))))

;;;     g_environ_getenv
;;;     g_environ_setenv
;;;     g_environ_unsetenv

;;;     g-getenv
;;;     g-setenv

#-windows
(test g-getenv
  (is (string= "/home/dieter" (g-getenv "HOME"))))

#+windows
(test g-getenv
  (is-false (g-getenv "HOME")))

#-windows
(test g-getenv
  (is (string= ":0" (g-getenv "DISPLAY"))))

#+windows
(test g-getenv
  (is-false (g-getenv "DISPLAY")))

;;;     g_unsetenv

;;;     g-listenv

(test g-listenv
  (is (every #'stringp (g-listenv)))
  (is (every #'stringp (mapcar #'g-getenv (g-listenv)))))

;;;     g-user-name

(test g-user-name
  (is (string= "dieter" (g-user-name))))

;;;     g-real-name

(test g-real-name
  (is (string= "dieter" (g-real-name))))

;;;     g-user-cache-dir

(test g-user-cache-dir
  (is (string= "/home/dieter/.cache" (g-user-cache-dir))))

;;;     g-user-data-dir

(test g-user-data-dir
  (is (string= "/home/dieter/.local/share" (g-user-data-dir))))

;;;     g-user-config-dir

(test g-user-config-dir
  (is (string= "/home/dieter/.config" (g-user-config-dir))))

;;;     g-user-runtime-dir

(test g-user-runtime-dir
  (is (string= "/run/user/1000" (g-user-runtime-dir))))

;;;     g-user-special-dir

(test g-get-user-special-dir
 (is (string= "/home/dieter/Schreibtisch" (g-user-special-dir :desktop)))
 (is (string= "/home/dieter/Dokumente" (g-user-special-dir :documents)))
 (is (string= "/home/dieter/Downloads" (g-user-special-dir :download)))
 (is (string= "/home/dieter/Musik" (g-user-special-dir :music)))
 (is (string= "/home/dieter/Bilder" (g-user-special-dir :pictures)))
 (is (string= "/home/dieter/Öffentlich" (g-user-special-dir :public-share)))
 (is (string= "/home/dieter/Vorlagen" (g-user-special-dir :templates)))
 (is (string= "/home/dieter/Videos" (g-user-special-dir :videos))))

;;;     g-system-data-dirs

(test g-system-data-dirs
  (is (every #'stringp (g-system-data-dirs))))

;;;     g-system-config-dirs

(test g-system-config-dirs
  (is (every #'stringp (g-system-config-dirs))))

;;;     g_reload_user_special_dirs_cache
;;;     g_get_os_info

;;;     g-host-name

(test g-host-name
  (is (string= "dieter-notebook" (g-host-name))))

;;;     g-home-dir

(test g-home-dir
  (is (string= "/home/dieter" (g-home-dir))))

;;;     g-tmp-dir

(test g-tmp-dir
  (is (string= "/tmp" (g-tmp-dir))))

;;;     g_get_current_dir

(test g-current-dir
  (is (string= "/home/dieter/Lisp/lisp-projects/cl-gtk/test" (g-current-dir))))

;;;     g_basename
;;;     g_dirname
;;;     g_canonicalize_filename

;;;     g-path-is-absolute

(test g-path-is-absolute
  (is-false (g-path-is-absolute "../dieter"))
  (is-true  (g-path-is-absolute "/home"))
  (is-true  (g-path-is-absolute "/home/dieter"))
  (is-true  (g-path-is-absolute "/home/dieter/Lisp")))

;;;     g_path_skip_root
;;;     g_path_get_basename
;;;     g_path_get_dirname

;;;     g-build-filename

#-windows
(test g-build-filename
  (is (string= "home/dieter/Lisp"
               (g-build-filename "home" "dieter" "Lisp"))))

#+windows
(test g-build-filename
  (is (string= "home\\dieter\\Lisp"
               (g-build-filename "home" "dieter" "Lisp"))))

;;;     g_build_filenamev
;;;     g_build_filename_valist

;;;     g-build-path

(test g-build-path
  (is (string= "home/dieter/Lisp"
               (g-build-path "/" "home" "dieter" "Lisp"))))

;;;     g_build_pathv
;;;     g_format_size
;;;     g_format_size_full
;;;     g_format_size_for_display
;;;     g_find_program_in_path
;;;     g_bit_nth_lsf
;;;     g_bit_nth_msf
;;;     g_bit_storage
;;;     g_spaced_primes_closest
;;;     g_atexit
;;;     g_abort
;;;     g_parse_debug_string
;;;
;;;     (*GVoidFunc) ()
;;;     (*GFreeFunc) ()
;;;
;;;     g_qsort_with_data
;;;     g_nullify_pointer

;;; 2020-10-24
