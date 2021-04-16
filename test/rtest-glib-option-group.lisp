(def-suite glib-option-group :in glib-suite)
(in-suite glib-option-group)

;;; Types and Values

;;;     GOptionError
;;;     G_OPTION_ERROR
;;;     GOptionArg
;;;     GOptionFlags
;;;     G_OPTION_REMAINING
;;;     GOptionContext

;;;     GOptionEntry

(test g-option-entry.1
  (with-foreign-object (entry '(:struct g-option-entry))
    (setf (glib::g-option-entry-long-name entry) "long-name")
    (setf (glib::g-option-entry-short-name entry) (char-code #\l))
    (is (string= "long-name" (glib::g-option-entry-long-name entry)))
    (is (= 108 (glib::g-option-entry-short-name entry)))))

(test g-option-entry.2
  (let ((entry-list '("long-name" #\l)))
    (with-foreign-object (entry '(:struct g-option-entry))
      (setf (glib::g-option-entry-long-name entry) (pop entry-list))
      (setf (glib::g-option-entry-short-name entry) (char-code (pop entry-list)))
      (setf (glib::g-option-entry-flags entry) '(:hidden))
      (setf (glib::g-option-entry-arg entry) :int)
      (setf (glib::g-option-entry-arg-data entry) (null-pointer))
      (setf (glib::g-option-entry-description entry) "")
      (setf (glib::g-option-entry-arg-description entry) "")
      (is (string= "long-name" (glib::g-option-entry-long-name entry)))
      (is (= 108 (glib::g-option-entry-short-name entry)))
      (is (equal '(:hidden) (glib::g-option-entry-flags entry)))
      (is (eq :int (glib::g-option-entry-arg entry)))
      (is (null-pointer-p (glib::g-option-entry-arg-data entry)))
      (is (string= "" (glib::g-option-entry-description entry)))
      (is (string= "" (glib::g-option-entry-arg-description entry))))))

#+nil
(test g-option-entry.3
  (let* ((context (g-option-context-new "parameter"))
         (entries '(("long-name" #\l) ("long-name" #\l)))
         (n-entries (length entries)))
    (with-foreign-object (entries-ptr '(:struct g-option-entry) (1+ n-entries))
      (loop
        for entry in entries
        for i from 0
        for entry-ptr = (mem-aptr entries-ptr '(:struct g-option-entry) i)
        do (setf (glib::g-option-entry-long-name entry-ptr) (pop entry)
                 (glib::g-option-entry-short-name entry-ptr)
                 (char-code (pop entry))
                 (glib::g-option-entry-flags entry-ptr) '(:in-main)
                 (glib::g-option-entry-arg entry-ptr) :int
                 (glib::g-option-entry-arg-data entry-ptr) (null-pointer)
                 (glib::g-option-entry-description entry-ptr) ""
                 (glib::g-option-entry-arg-description entry-ptr) ""))
      (let ((entry-ptr (mem-aptr entries-ptr '(:struct g-option-entry) n-entries)))
        (setf (foreign-slot-value entry-ptr '(:struct g-option-entry) 'glib::long-name) (null-pointer))
        (glib::%g-option-context-add-main-entries context entries-ptr "")))))

#| The tests g-option-entry.4 and g.option-entry.5 causes a warning.
   Work on this topic.

(defvar arg-boolean (cffi:foreign-alloc :boolean :initial-element nil))
(defvar arg-string (cffi:foreign-alloc :string :initial-element ""))
(defvar arg-int (cffi:foreign-alloc :int :initial-element 0))

(defvar arg-filename (cffi:foreign-alloc :string :initial-element ""))
(defvar arg-string-array (cffi:foreign-alloc 'g-strv :initial-element '()))
(defvar arg-filename-array (cffi:foreign-alloc 'g-strv :initial-element '()))

(defvar arg-double (cffi:foreign-alloc :double :initial-element 0.0d0))
(defvar arg-int64 (cffi:foreign-alloc :int64 :initial-element 0))

(test g-option-entry.4
  (let* ((context (g-option-context-new "parameter"))
         (entries '(("long-name-1" #\a (:in-main) :none arg-boolean "Description 1" "ARG 1")
                    ("long-name-2" #\b (:in-main) :string arg-string "Description 2" "ARG 2")
                    ("long-name-3" #\c (:in-main) :int arg-int "Description 3" "ARG 3")
                    ("long-name-4" #\d (:in-main :filename) :filename arg-filename "Description 4" "ARG 4")
                    ("long-name-5" #\e (:in-main) :string-array arg-string-array "Description 5" "ARG 5")
                    ("long-name-6" #\f (:in-main) :filename-array arg-filename-array "Description 6" "ARG 6")
                    ("long-name-7" #\g (:in-main) :double arg-double "Description 7" "ARG 7")
                    ("long-name-8" #\h (:in-main) :int64 arg-int64 "Description 8" "ARG 8")
                   ))
         (n-entries (length entries)))
    (with-foreign-object (entries-ptr '(:struct g-option-entry) (1+ n-entries))
      (loop
        for entry in entries
        for i from 0
        for entry-ptr = (mem-aptr entries-ptr '(:struct g-option-entry) i)
        do (setf (foreign-slot-value entry-ptr '(:struct g-option-entry) 'glib::long-name)
                 (first entry)
                 (foreign-slot-value entry-ptr '(:struct g-option-entry) 'glib::short-name)
                 (char-code (second entry))
                 (foreign-slot-value entry-ptr '(:struct g-option-entry) 'glib::flags)
                 (third entry)
                 (foreign-slot-value entry-ptr '(:struct g-option-entry) 'glib::arg)
                 (fourth entry)
                 (foreign-slot-value entry-ptr '(:struct g-option-entry) 'glib::arg-data)
                 (progn
                   (cond ((member (fourth entry) '(:none :int :double :string :filename :string-array :filename-array :int64))
;                          (format t "~& in COND '(:int :double)~%")
;                          (format t "~& ~A and ~A~%" (fifth entry) (symbol-value (fifth entry)))
                          (symbol-value (fifth entry)))
                         (t (error "Case not handled for g-option-entry")))
                 )
                 (foreign-slot-value entry-ptr '(:struct g-option-entry) 'glib::description)
                 (sixth entry)
                 (foreign-slot-value entry-ptr '(:struct g-option-entry) 'glib::arg-description)
                 (seventh entry)
           ))

      (let ((entry-ptr (mem-aptr entries-ptr '(:struct g-option-entry) n-entries)))
        (setf (foreign-slot-value entry-ptr '(:struct g-option-entry) 'glib::long-name) (null-pointer))
        (glib::%g-option-context-add-main-entries context entries-ptr "")
;        (format t "~&~A~%" (g-option-context-get-help context nil))
        )

      (g-option-context-set-help-enabled context t)
      (let ((argv (list "prgname" "--long-name-1"
                                  "--long-name-2" "a string"
                                  "--long-name-3" "999"
                                  "--long-name-4" "filename"
                                  "--long-name-5" "a" "--long-name-5" "b"
                                  "--long-name-6" "file1" "--long-name-6" "file2"
                                  "--long-name-7" "99.9"
                                  "--long-name-8" "1234567890")))
        (is-true (g-option-context-parse context argv))
        (is-true (mem-ref arg-boolean :boolean))
        (is (equal "a string" (mem-ref arg-string :string)))
        (is (= 999 (mem-ref arg-int :int)))

        (is (equal "filename" (mem-ref arg-filename :string)))
        (is (equal '("a" "b") (mem-ref arg-string-array 'g-strv)))
        (is (equal '("file1" "file2") (mem-ref arg-filename-array 'g-strv)))
        (is (= 99.9d0 (mem-ref arg-double :double)))
        (is (= 1234567890 (mem-ref arg-int64 :int64)))
      ))))

(test g-option-entry.5
  (let ((context (g-option-context-new "parameter"))
        (entries '(("long-name-1" #\a (:in-main) :none arg-boolean "Description 1" "ARG 1")
                   ("long-name-2" #\b (:in-main) :string arg-string "Description 2" "ARG 2")
                   ("long-name-3" #\c (:in-main) :int arg-int "Description 3" "ARG 3")
                   ("long-name-4" #\d (:in-main :filename) :filename arg-filename "Description 4" "ARG 4")
                   ("long-name-5" #\e (:in-main) :string-array arg-string-array "Description 5" "ARG 5")
                   ("long-name-6" #\f (:in-main) :filename-array arg-filename-array "Description 6" "ARG 6")
                   ("long-name-7" #\g (:in-main) :double arg-double "Description 7" "ARG 7")
                   ("long-name-8" #\h (:in-main) :int64 arg-int64 "Description 8" "ARG 8")
                  )))

    (g-option-context-add-main-entries context entries "")
;    (format t "~&~A~%" (g-option-context-get-help context nil))

    (let ((argv (list "prgname" "--long-name-1"
                                "--long-name-2" "a string"
                                "--long-name-3" "999"
                                "--long-name-4" "filename"
                                "--long-name-5" "a" "--long-name-5" "b"
                                "--long-name-6" "file1" "--long-name-6" "file2"
                                "--long-name-7" "99.9"
                                "--long-name-8" "1234567890")))
      (is-true (g-option-context-parse context argv))
      (is-true (mem-ref arg-boolean :boolean))
      (is (equal "a string" (mem-ref arg-string :string)))
      (is (= 999 (mem-ref arg-int :int)))

      (is (equal "filename" (mem-ref arg-filename :string)))
      (is (equal '("a" "b") (mem-ref arg-string-array 'g-strv)))
      (is (equal '("file1" "file2") (mem-ref arg-filename-array 'g-strv)))
      (is (= 99.9d0 (mem-ref arg-double :double)))
      (is (= 1234567890 (mem-ref arg-int64 :int64)))
    )))
|#

;;;     GOptionGroup

;;; Functions

;;;     GOptionArgFunc
;;;
;;;     g_option_context_new
;;;     g_option_context_set_summary
;;;     g_option_context_get_summary
;;;     g_option_context_set_description
;;;     g_option_context_get_description
;;;
;;;     GTranslateFunc
;;;     g_option_context_set_translate_func
;;;     g_option_context_set_translation_domain
;;;     g_option_context_free
;;;     g_option_context_parse
;;;     g_option_context_parse_strv
;;;     g_option_context_set_help_enabled
;;;     g_option_context_get_help_enabled
;;;     g_option_context_set_ignore_unknown_options
;;;     g_option_context_get_ignore_unknown_options
;;;     g_option_context_get_help
;;;     g_option_context_get_strict_posix
;;;     g_option_context_set_strict_posix
;;;     g_option_context_add_main_entries
;;;     g_option_context_add_group
;;;     g_option_context_set_main_group
;;;     g_option_context_get_main_group
;;;
;;;     g_option_group_new
;;;     g_option_group_ref
;;;     g_option_group_unref
;;;     g_option_group_free
;;;     g_option_group_add_entries
;;;
;;;     GOptionParseFunc
;;;     g_option_group_set_parse_hooks
;;;     GOptionErrorFunc
;;;     g_option_group_set_error_hook
;;;     g_option_group_set_translate_func
;;;     g_option_group_set_translation_domain

;;; 2021-4-13
