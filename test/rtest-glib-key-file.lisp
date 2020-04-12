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
;;;     g_key_file_free
;;;     g_key_file_ref
;;;     g_key_file_unref
;;;     g_key_file_set_list_separator

;;;     g_key_file_load_from_file

(test g-key-file-load-from-file
  (let ((key-file (g-key-file-load-from-file "rtest-glib-key-value.ini" :none)))
    (is (pointerp key-file))
    (is (stringp  (g-key-file-to-data key-file)))))

;;;     g_key_file_load_from_data

(test g-key-file-load-from-data
  (is-false (g-key-file-load-from-data "test" :none))
  (is (pointerp (g-key-file-load-from-data *key-values* :none))))

;;;     g_key_file_load_from_bytes
;;;     g_key_file_load_from_data_dirs
;;;     g_key_file_load_from_dirs
;;;     g_key_file_to_data

;;;     g_key_file_save_to_file

;;;     g_key_file_get_start_group
;;;     g_key_file_get_groups
;;;     g_key_file_get_keys
;;;     g_key_file_has_group
;;;     g_key_file_has_key
;;;
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
;;;     g_key_file_set_string_list
;;;     g_key_file_set_locale_string_list
;;;     g_key_file_set_boolean_list
;;;     g_key_file_set_integer_list
;;;     g_key_file_set_double_list
;;;     g_key_file_set_comment
;;;     g_key_file_remove_group
;;;     g_key_file_remove_key
;;;     g_key_file_remove_comment

