(def-suite g-key-file :in glib-suite)
(in-suite g-key-file)

(defvar *key-values*
"# this is just an example
 # there can be comments before the first group

 [First Group]

  Name=Key File Example this value shows escaping

 # localized strings are stored in multiple key-value pairs
 Welcome=Hello
 Welcome[de]=Hallo
 Welcome[fr_FR]=Bonjour
 Welcome[it]=Ciao
 Welcome[be@@latin]=Hello

 [Another Group]

 Numbers=2;20;-200;0

 Booleans=true;false;true;true")

;;; --- Types and Values -------------------------------------------------------

;;;     GKeyFile
;;;     G_KEY_FILE_ERROR
;;;     GKeyFileError
;;;     GKeyFileFlags

;;; --- Functions --------------------------------------------------------------

;;;     g_key_file_new

(test g-key-file-new
  (is (pointerp (g-key-file-new))))

;;;     g_key_file_free
;;;     g_key_file_ref
;;;     g_key_file_unref

;;;     g_key_file_set_list_separator

(test g-key-file-set-list-separator
  (let ((keyfile (g-key-file-new)))
    (is-false (g-key-file-set-list-separator keyfile #\$))
    (is (equal '("string1" "string2" "string3")
               (setf (g-key-file-string-list keyfile "Group" "strings")
                     '("string1" "string2" "string3"))))
    (is (string= "string1$string2$string3$"
                 (g-key-file-string keyfile "Group" "strings")))))

;;;     g_key_file_load_from_file

(test g-key-file-load-from-file
  (let ((keyfile (g-key-file-new)))
    (is-true (g-key-file-load-from-file keyfile "rtest-glib-key-file.ini" :none))
    (is (pointerp keyfile))
    (is (stringp (g-key-file-to-data keyfile)))))

;;;     g_key_file_load_from_data

(test g-key-file-load-from-data
  (let ((keyfile (g-key-file-new)))
    (is-false (g-key-file-load-from-data keyfile "test" :none))
    (is-true (g-key-file-load-from-data keyfile *key-values* :none))))

;;;     g_key_file_load_from_bytes
;;;     g_key_file_load_from_data_dirs
;;;     g_key_file_load_from_dirs

;;;     g_key_file_to_data

(test g-key-file-to-data
  (let ((keyfile (g-key-file-new)))
    (is (string= "" (g-key-file-to-data keyfile)))))

;;;     g_key_file_save_to_file

(test g-key-file-to-file
  (let ((keyfile (g-key-file-new)))
    (is-true (g-key-file-save-to-file keyfile "rtest-glib-key-file-example.ini")))
  (let ((keyfile (g-key-file-new)))
    (is-true (g-key-file-load-from-file keyfile
                                        "rtest-glib-key-file-example.ini"
                                        :none))))

;;;     g_key_file_get_start_group

(test g-key-file-start-group
  (let ((keyfile (g-key-file-new)))
    (is-true (g-key-file-load-from-data keyfile *key-values* :none))
    (is (string= "First Group" (g-key-file-start-group keyfile)))))

;;;     g_key_file_get_groups

(test g-key-file-groups
  (let ((keyfile (g-key-file-new)))
    (is-true (g-key-file-load-from-data keyfile *key-values* :none))
    (is (equal '("First Group" "Another Group")
               (g-key-file-groups keyfile)))))

;;;     g_key_file_get_keys

(test g-key-file-keys
  (let ((keyfile (g-key-file-new)))
    (is-true (g-key-file-load-from-data keyfile *key-values* :none))
    (is (equal '("Numbers" "Booleans")
               (g-key-file-keys keyfile "Another Group")))))

;;;     g_key_file_has_group

(test g-key-file-has-group
  (let ((keyfile (g-key-file-new)))
    (is-true (g-key-file-load-from-data keyfile *key-values* :none))
    (is-false (g-key-file-has-group keyfile "unknown"))
    (is-true (g-key-file-has-group keyfile "First Group"))
    (is-true (g-key-file-has-group keyfile "Another Group"))))

;;;     g_key_file_has_key

(test g-key-file-has-key
  (let ((keyfile (g-key-file-new)))
    (is-true (g-key-file-load-from-data keyfile *key-values* :none))
    (is-false (g-key-file-has-key keyfile "Another Group" "unknown"))
    (is-true (g-key-file-has-key keyfile "Another Group" "Numbers"))
    (is-true (g-key-file-has-key keyfile "Another Group" "Booleans"))))

;;;     g_key_file_get_value
;;;     g_key_file_get_string
;;;     g_key_file_get_locale_string
;;;     g_key_file_get_locale_for_key
;;;     g_key_file_get_boolean
;;;     g_key_file_get_integer
;;;     g_key_file_get_int64
;;;     g_key_file_get_uint64
;;;     g_key_file_get_double

;;;     g_key_file_get_string_list
;;;     g_key_file_set_string_list

(test g-key-file-string-list.1
  (let ((keyfile (g-key-file-new)))
    (is-true (g-key-file-load-from-file keyfile "rtest-glib-key-file.ini" :none))
    (is (equal '("2" "20" "-200" "0")
               (g-key-file-string-list keyfile "Another Group" "Numbers")))))

(test g-key-file-string-list.2
  (let ((keyfile (g-key-file-new)))
    (is (equal '("string1" "string2" "string3")
               (setf (g-key-file-string-list keyfile "New Group" "strings")
                     '("string1" "string2" "string3"))))
    (is (equal '("string1" "string2" "string3")
               (g-key-file-string-list keyfile "New Group" "strings")))))

;;;     g_key_file_get_locale_string_list
;;;     g_key_file_get_boolean_list
;;;     g_key_file_get_integer_list
;;;     g_key_file_get_double_list
;;;     g_key_file_get_comment
;;;
;;;     g_key_file_set_value
;;;     g_key_file_set_string
;;;     g_key_file_set_locale_string
;;;     g_key_file_set_boolean
;;;     g_key_file_set_integer
;;;     g_key_file_set_int64
;;;     g_key_file_set_uint64
;;;     g_key_file_set_double
;;;     g_key_file_set_locale_string_list
;;;     g_key_file_set_boolean_list
;;;     g_key_file_set_integer_list
;;;     g_key_file_set_double_list
;;;     g_key_file_set_comment
;;;     g_key_file_remove_group
;;;     g_key_file_remove_key
;;;     g_key_file_remove_comment

;;; Examples from the GKeyFile documentation

(test g-key-file-example.1
  (let ((keyfile (g-key-file-new)))
    ;; Load a key file
    (unless (g-key-file-load-from-file keyfile "rtest-glib-key-file.ini" :none)
      (error "Error loading the key file: RTEST-GLIB-KEY-FILE.INI"))
    ;; Read a string from the key file
    (let ((value (g-key-file-string keyfile "First Group" "Welcome")))
      (unless value
        (setf value "default-value"))
      (is (string= "Hello" value)))))

(test g-key-file-example.2
  (let ((keyfile (g-key-file-new)))

    ;; Load existing key file
    (g-key-file-load-from-file keyfile "rtest-glib-key-file.ini" :none)

    ;; Add a string to the First Group
    (setf (g-key-file-string keyfile "First Group" "SomeKey") "New Value")

    ;; Save to a file
    (unless (g-key-file-save-to-file keyfile "rtest-glib-key-file-example.ini")
      (error "Error saving key file."))

    ;; Or save to data for use elsewhere
    (let ((data (g-key-file-to-data keyfile)))
      (unless data
        (error "Error saving key file."))
      (is (stringp data)))))

;;; 2021-8-13
