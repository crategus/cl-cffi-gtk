(def-suite g-option :in glib-suite)
(in-suite g-option)

(defvar *verbose-g-option* nil)

;;; --- Types and Values -------------------------------------------------------

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

(defvar arg-string (cffi:foreign-alloc :string :initial-element ""))
(defvar arg-int (cffi:foreign-alloc :int :initial-element 0))

(defvar arg-filename (cffi:foreign-alloc :string :initial-element ""))
(defvar arg-string-array (cffi:foreign-alloc 'g-strv :initial-element '()))
(defvar arg-filename-array (cffi:foreign-alloc 'g-strv :initial-element '()))

(defvar arg-double (cffi:foreign-alloc :double :initial-element 0.0d0))
(defvar arg-int64 (cffi:foreign-alloc :int64 :initial-element 0))

;;;     GOptionGroup

;;; --- Functions --------------------------------------------------------------

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

#-windows
(test g-option-context-set-translate-func
  (let ((context (g-option-context-new)))
    (is-false (g-option-context-set-translate-func context #'translate-func))
    (when *verbose-g-option*
      (format t "~%~a~%" (g-option-context-help context t)))
    (is (string= "Aufruf:
  Program [OPTION …]

Hilfeoptionen:
  -h, --help       Hilfeoptionen anzeigen

"
                 (g-option-context-help context t)))))

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

#-windows
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
    (when *verbose-g-option*
      (format t "~&~A~%" (g-option-context-help context t)))
    (is (string= "Aufruf:
  Program [OPTION …] Description

Hilfeoptionen:
  -?, --help                             Hilfeoptionen anzeigen

Anwendungsoptionen:
  -a, --long-name-1                      Description1
  -b, --long-name-2=a string             Description2
  -c, --long-name-3=an integer           Description3
  -d, --long-name-4=a filename           Description4
  -e, --long-name-5=a string array       Description5
  -f, --long-name-6=a filename array     Description6
  -g, --long-name-7=a double float       Description7
  -h, --long-name-8=a long integer       Description8

"
                 (g-option-context-help context t)))))

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

#-windows
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
    (when *verbose-g-option*
      (format t "~&~A~%" (g-option-context-help context t group)))
    (is (string= "Aufruf:
  Program [OPTION …] Description

Anwendungsoptionen:
  -a, --long-name-1                      Description1
  -b, --long-name-2=a string             Description2
  -c, --long-name-3=an integer           Description3
  -d, --long-name-4=a filename           Description4
  -e, --long-name-5=a string array       Description5
  -f, --long-name-6=a filename array     Description6
  -g, --long-name-7=a double float       Description7
  -h, --long-name-8=a long integer       Description8

"
                 (g-option-context-help context t group)))))

;;;     GOptionParseFunc
;;;     g_option_group_set_parse_hooks
;;;     GOptionErrorFunc
;;;     g_option_group_set_error_hook

;;;     g_option_group_set_translate_func

(test g-option-group-set-translate-func
  (let ((group (g-option-group-new nil nil nil)))
    (is-false (g-option-group-set-translate-func group #'translate-func))))

;;;     g_option_group_set_translation_domain

(test g-option-group-set-translation-domain
  (let ((group (g-option-group-new nil nil nil)))
    (is-false (g-option-group-set-translation-domain group "myApplication"))))

;;; Example from the GOptionContext documentation

(defvar repeats (cffi:foreign-alloc :int :initial-element 2))
(defvar max-size (cffi:foreign-alloc :int :initial-element 0))
(defvar verbose (cffi:foreign-alloc :boolean :initial-element nil))
(defvar beep (cffi:foreign-alloc :boolean :initial-element nil))
(defvar randomize (cffi:foreign-alloc :boolean :initial-element nil))

#-windows
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
    (when *verbose-g-option*
      (format t "~&Parsed arguments~%")
      (format t "   repeats : ~a~%" (mem-ref repeats :int))
      (format t "  max-size : ~a~%" (mem-ref max-size :int))
      (format t "   verbose : ~a~%" (mem-ref verbose :boolean))
      (format t "      beep : ~a~%" (mem-ref beep :boolean))
      (format t " randomize : ~a~%" (mem-ref randomize :boolean))

      (format t "~&~%~a~%" (g-option-context-help context t)))

    (is (= 1 (mem-ref repeats :int)))
    (is (= 20 (mem-ref max-size :int)))
    (is-true (mem-ref verbose :boolean))
    (is-true (mem-ref beep :boolean))
    (is-true (mem-ref randomize :boolean))

    (is (string= "Aufruf:
  Program [OPTION …] - test tree model performance

Hilfeoptionen:
  -h, --help                  Hilfeoptionen anzeigen
  --help-all                  Alle Hilfeoptionen anzeigen
  --help-gtk                  GTK+-Optionen anzeigen

Anwendungsoptionen:
  -r, --repeats=N             Average over N repetitions
  -m, --max-size=M            Test up to 2^M items
  -v, --verbose               Be verbose
  -b, --beep                  Beep when done
  --rand                      Randomize the data
  --display=ANZEIGE           X-Anzeige, die verwendet werden soll

"
                 (g-option-context-help context t)))))

;;; 2021-10-17
