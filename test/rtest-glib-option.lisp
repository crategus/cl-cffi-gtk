(def-suite g-option :in glib-suite)
(in-suite g-option)

;;; Types and Values

;;;     GOptionError
;;;     G_OPTION_ERROR
;;;     GOptionArg
;;;     GOptionFlags
;;;     G_OPTION_REMAINING
;;;     GOptionContext

;;;     GOptionEntry

(test g-option-entry.1
  (with-foreign-object (entry '(:struct glib::g-option-entry))
    (with-foreign-slots ((glib::long-name glib::short-name)
                          entry
                          (:struct glib::g-option-entry))
      (setf glib::long-name "long-name")
      (setf glib::short-name (char-code #\l))
      (is (string= "long-name" glib::long-name))
      (is (= 108 glib::short-name)))))

(test g-option-entry.2
  (let ((entries '("long-name" #\l (:hidden))))
    (with-foreign-object (entry '(:struct glib::g-option-entry))
      (with-foreign-slots ((glib::long-name
                            glib::short-name
                            glib::flags
                            glib::arg
                            glib::arg-data
                            glib::description
                            glib::arg-description)
                           entry
                           (:struct glib::g-option-entry))

        ;; Set the fields of the GOptionEntry structure
        (setf glib::long-name (pop entries))
        (setf glib::short-name (char-code (pop entries)))
        (setf glib::flags (pop entries))
        (setf glib::arg :int)
        (setf glib::arg-data (null-pointer))
        (setf glib::description "")
        (setf glib::arg-description "")

        ;; Read the fields of the GOptionEntry structure
        (is (string= "long-name" glib::long-name))
        (is (= 108 glib::short-name))
        (is (equal '(:hidden) glib::flags))
        (is (eq :int glib:: arg))
        (is (null-pointer-p glib::arg-data))
        (is (string= "" glib::description))
        (is (string= "" glib::arg-description))))))

(defvar arg-boolean (cffi:foreign-alloc :boolean :initial-element nil))
(defvar arg-string (cffi:foreign-alloc :string :initial-element ""))
(defvar arg-int (cffi:foreign-alloc :int :initial-element 0))

(defvar arg-filename (cffi:foreign-alloc :string :initial-element ""))
(defvar arg-string-array (cffi:foreign-alloc 'g-strv :initial-element '()))
(defvar arg-filename-array (cffi:foreign-alloc 'g-strv :initial-element '()))

(defvar arg-double (cffi:foreign-alloc :double :initial-element 0.0d0))
(defvar arg-int64 (cffi:foreign-alloc :int64 :initial-element 0))

#+nil
(test g-option-entry.4
  (let* ((context (g-option-context-new "parameter"))
         (entries '(("long-name-1"     ; long-name
                     #\a               ; short-name
                     (:in-main)        ; flags
                     :none             ; arg
                     nil               ; arg-data
                     "Description1"    ; description
                     "")               ; arg-description
;                    ("long-name-2"
;                     #\b
;                     (:in-main)
;                     :string
;                     arg-string
;                     "Description2"
;                     "ARG2")
;                    ("long-name-3"
;                     #\c (:in-main) :int arg-int "Description 3" "ARG 3")
;                    ("long-name-4"
;                     #\d
;                     (:in-main :filename)
;                     :filename
;                     arg-filename
;                     "Description 4"
;                     "ARG4")
;                    ("long-name-5"
;                     #\e
;                     (:in-main)
;                     :string-array arg-string-array "Description 5" "ARG 5")
;                    ("long-name-6"
;                     #\f
;                     (:in-main)
;                     :filename-array arg-filename-array "Description 6" "ARG 6")
;                    ("long-name-7"
;                     #\g (:in-main)
;                     :double arg-double "Description 7" "ARG 7")
;                    ("long-name-8"
;                     #\h (:in-main) :int64 arg-int64 "Description 8" "ARG 8")
                   ))
         (n-entries (length entries)))
    (with-foreign-object (entries-ptr '(:struct glib::g-option-entry) (1+ n-entries))
      (loop
        for entry in entries
        for i from 0
        for entry-ptr = (mem-aptr entries-ptr '(:struct glib::g-option-entry) i)
        do (setf (foreign-slot-value entry-ptr
                                     '(:struct glib::g-option-entry) 'glib::long-name)
                 (first entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct glib::g-option-entry) 'glib::short-name)
                 (char-code (second entry))
                 (foreign-slot-value entry-ptr
                                     '(:struct glib::g-option-entry) 'glib::flags)
                 (third entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct glib::g-option-entry) 'glib::arg)
                 (fourth entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct glib::g-option-entry) 'glib::arg-data)
                 (progn
                   (cond ((member (fourth entry)
                                  '(:int :double :string :filename
                                    :string-array :filename-array :int64))
                          (format t "~& ~A and ~A~%"
                                    (fifth entry) (symbol-value (fifth entry)))
                          (symbol-value (fifth entry)))
                         (t (null-pointer)))
                 )
                 (foreign-slot-value entry-ptr
                                     '(:struct glib::g-option-entry) 'glib::description)
                 (sixth entry)
                 (foreign-slot-value entry-ptr
                                     '(:struct glib::g-option-entry)
                                     'glib::arg-description)
                 (seventh entry)
           ))

      (let ((entry-ptr (mem-aptr entries-ptr '(:struct glib::g-option-entry) n-entries)))
        (setf (foreign-slot-value entry-ptr
                                  '(:struct glib::g-option-entry) 'glib::long-name)
              (null-pointer))
        (glib::%g-option-context-add-main-entries context entries-ptr "")
        (format t "~&~A~%" (g-option-context-help context nil))
        )

      (setf (g-option-context-help-enabled context) t)
      (let ((argv '("prgname"
                    "--long-name-1")))

;                                  "--long-name-2" "a string"
;                                  "--long-name-3" "999"
;                                  "--long-name-4" "filename"
;                                  "--long-name-5" "a" "--long-name-5" "b"
;                                  "--long-name-6" "file1" "--long-name-6" "file2"
;                                  "--long-name-7" "99.9"
;                                  "--long-name-8" "1234567890")))

        (format t " vorher argv : ~a~%" argv)
        (is-true (g-option-context-parse-strv context argv))
        (format t "nachher argv : ~a~%" argv)

;        (is-true (g-option-context-parse context '("file1" "file2")))
;        (is-true (g-option-context-parse context argv))
;        (is-true (mem-ref arg-boolean :boolean))
;        (is (equal "a string" (mem-ref arg-string :string)))
;        (is (= 999 (mem-ref arg-int :int)))

;        (is (equal "filename" (mem-ref arg-filename :string)))
;        (is (equal '("a" "b") (mem-ref arg-string-array 'g-strv)))
;        (is (equal '("file1" "file2") (mem-ref arg-filename-array 'g-strv)))
;        (is (= 99.9d0 (mem-ref arg-double :double)))
;        (is (= 1234567890 (mem-ref arg-int64 :int64)))
      ))))

#|
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

;;;     g_option_context_new

(test g-option-contex-new
  (is (pointerp (g-option-context-new)))
  (is (pointerp (g-option-context-new nil)))
  (is (pointerp (g-option-context-new "Descripton"))))

;;;     g_option_context_set_summary
;;;     g_option_context_get_summary

(test g-option-context-summary
  (let ((context (g-option-context-new)))
    (is-false (g-option-context-summary context))
    (is (string= "summary" (setf (g-option-context-summary context) "summary")))
    (is (string= "summary" (g-option-context-summary context)))))

;;;     g_option_context_set_description
;;;     g_option_context_get_description

(test g-option-context-description
  (let ((context (g-option-context-new)))
    (is-false (g-option-context-description context))
    (is (string= "description"
                 (setf (g-option-context-description context) "description")))
    (is (string= "description" (g-option-context-description context)))))

;;;     GTranslateFunc

(defun translate-func (str)
  (string-upcase str))

;;;     g_option_context_set_translate_func

(test g-option-context-set-translate-func
  (let ((context (g-option-context-new)))
    (is-false (g-option-context-set-translate-func context #'translate-func))
    (format t "~%~a~%" (g-option-context-help context t))
))

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

(test g-option-context-add-main-entries
  (let* ((context (g-option-context-new "Description"))
         (entries '(("long-name-1"     ; long-name
                     #\a               ; short-name
                     (:in-main)        ; flags
                     :none             ; arg
                     nil               ; arg-data
                     "Description1"    ; description
                     nil)              ; arg-description
                    ("long-name-2"
                     #\b
                     (:in-main)
                     :string
                     arg-string
                     "Description2"
                     "a string")
                    ("long-name-3"
                     #\c
                     (:in-main)
                     :int arg-int
                     "Description3"
                     "an integer")
                    ("long-name-4"
                     #\d
                     (:in-main)
                     :filename
                     arg-filename
                     "Description4"
                     "a filename")
                    ("long-name-5"
                     #\e
                     (:in-main)
                     :string-array
                     arg-string-array
                     "Description5"
                     "a string array")
                    ("long-name-6"
                     #\f
                     (:in-main)
                     :filename-array
                     arg-filename-array
                     "Description6"
                     "a filename array")
                    ("long-name-7"
                     #\g
                     (:in-main)
                     :double
                     arg-double
                     "Description7"
                     "a double float")
                    ("long-name-8"
                     #\h
                     (:in-main)
                     :int64
                     arg-int64
                     "Description8"
                     "a long integer")
                   )))

    (g-option-context-add-main-entries context entries nil)
    (format t "~&~A~%" (g-option-context-help context t))
))

;;;     g_option_context_add_group

;;;     g_option_context_set_main_group
;;;     g_option_context_get_main_group

(test g-option-context-main-group
  (let ((context (g-option-context-new)))
    (is (null-pointer-p (g-option-context-main-group context)))
    (is (pointerp (setf (g-option-context-main-group context)
                        (g-option-group-new nil nil nil))))
    (is (pointerp (g-option-context-main-group context)))))

;;;     g_option_group_new

(test g-option-group-new
  (is (pointerp (g-option-group-new nil nil nil)))
  (is (pointerp (g-option-group-new "name" nil nil)))
  (is (pointerp (g-option-group-new "name" "description" nil)))
  (is (pointerp (g-option-group-new "name" "description" "help-description"))))

;;;     g_option_group_ref
;;;     g_option_group_unref
;;;     g_option_group_free

;;;     g_option_group_add_entries

(test g-option-group-add-entries
  (let* ((context (g-option-context-new "Description"))
         (group (g-option-group-new "myGroup" "A Group"  "Help Description"))
         (entries '(("long-name-1"     ; long-name
                     #\a               ; short-name
                     (:in-main)        ; flags
                     :none             ; arg
                     nil               ; arg-data
                     "Description1"    ; description
                     nil)              ; arg-description
                    ("long-name-2"
                     #\b
                     (:in-main)
                     :string
                     arg-string
                     "Description2"
                     "a string")
                    ("long-name-3"
                     #\c
                     (:in-main)
                     :int arg-int
                     "Description3"
                     "an integer")
                    ("long-name-4"
                     #\d
                     (:in-main)
                     :filename
                     arg-filename
                     "Description4"
                     "a filename")
                    ("long-name-5"
                     #\e
                     (:in-main)
                     :string-array
                     arg-string-array
                     "Description5"
                     "a string array")
                    ("long-name-6"
                     #\f
                     (:in-main)
                     :filename-array
                     arg-filename-array
                     "Description6"
                     "a filename array")
                    ("long-name-7"
                     #\g
                     (:in-main)
                     :double
                     arg-double
                     "Description7"
                     "a double float")
                    ("long-name-8"
                     #\h
                     (:in-main)
                     :int64
                     arg-int64
                     "Description8"
                     "a long integer")
                   )))

    (g-option-group-add-entries group entries)
    (g-option-context-add-group context group)
    (format t "~&~A~%" (g-option-context-help context t group))
))

;;;     GOptionParseFunc
;;;     g_option_group_set_parse_hooks
;;;     GOptionErrorFunc
;;;     g_option_group_set_error_hook

;;;     g_option_group_set_translate_func

(test g-option-group-set-translate-func
  (let ((group (g-option-group-new nil nil nil)))
    (is-false (g-option-group-set-translate-func group #'translate-func))
))

;;;     g_option_group_set_translation_domain

(test g-option-group-set-translation-domain
  (let ((group (g-option-group-new nil nil nil)))
    (is-false (g-option-group-set-translation-domain group "myApplication"))
))

;;; Example from the GOptionContext documentation

(defvar repeats (cffi:foreign-alloc :int :initial-element 2))
(defvar max-size (cffi:foreign-alloc :int :initial-element 0))
(defvar verbose (cffi:foreign-alloc :boolean :initial-element nil))
(defvar beep (cffi:foreign-alloc :boolean :initial-element nil))
(defvar randomize (cffi:foreign-alloc :boolean :initial-element nil))

(test g-option-context-example
  (let* ((entries '(("repeats"
                     #\r
                     0
                     :int
                     repeats
                     "Average over N repetitions"
                     "N")
                    ("max-size"
                     #\m
                     0
                     :int
                     max-size
                     "Test up to 2^M items"
                     "M")
                    ("verbose"
                     #\v
                     0
                     :none
                     verbose
                     "Be verbose"
                     nil)
                    ("beep"
                     #\b
                     0
                     :none
                     beep
                     "Beep when done"
                     nil)
                    ("rand"
                     #\Nul
                     0
                     :none
                     randomize
                     "Randomize the data"
                     nil)))
         (argv '("testtreemodel" "--rand" "-vb" "-r" "1"
                                 "--max-size" "20" "--" "file1" "file2"))
         (context (g-option-context-new "- test tree model performance")))

    (g-option-context-add-main-entries context entries nil)
    (g-option-context-add-group context (gtk-option-group t))

    (unless (g-option-context-parse-strv context argv)
      (format t "Option parsing failed.~%"))

    (format t "~&Parsed arguments~%")
    (format t "   repeats : ~a~%" (mem-ref repeats :int))
    (format t "  max-size : ~a~%" (mem-ref max-size :int))
    (format t "   verbose : ~a~%" (mem-ref verbose :boolean))
    (format t "      beep : ~a~%" (mem-ref beep :boolean))
    (format t " randomize : ~a~%" (mem-ref randomize :boolean))

    (format t "~&~%~a~%" (g-option-context-help context t))
))

;;; 2021-8-9
