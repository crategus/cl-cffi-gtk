(def-suite gobject-enumeration :in gobject-suite)
(in-suite gobject-enumeration)

;;; --- Types and Values -------------------------------------------------------

;;;   g-enum-class

(test g-enum-class
  (is (= 32 (foreign-type-size '(:struct g-enum-class))))
  (is (equal '(:maximum :minimum :n-values :type-class :values)
             (stable-sort (foreign-slot-names '(:struct g-enum-class))
             #'string-lessp))))

;;;   g-enum-value

(test g-enum-value
  (is (= 24 (foreign-type-size '(:struct g-enum-value))))
  (is (equal '(:name :nick :value)
             (stable-sort (foreign-slot-names '(:struct g-enum-value))
                          #'string-lessp))))

;;;   g-flags-class

(test g-flags-class
  (is (= 24 (foreign-type-size '(:struct g-flags-class))))
  (is (equal '(:mask :n-values :type-class :values)
             (stable-sort (foreign-slot-names '(:struct g-flags-class))
                          #'string-lessp))))

;;;   g-flags-value

(test g-flags-value
  (is (= 24 (foreign-type-size '(:struct g-flags-value))))
  (is (equal '(:name :nick :value)
             (stable-sort (foreign-slot-names '(:struct g-flags-value))
                          #'string-lessp))))

;;; --- Functions --------------------------------------------------------------

;;;     G_ENUM_CLASS_TYPE
;;;     G_ENUM_CLASS_TYPE_NAME

;;;     g-type-is-enum

(test g-type-is-enum
  (is-false (g-type-is-enum "GtkDialogFlags"))
  (is-true  (g-type-is-enum "GtkWindowType"))
  (is-false (g-type-is-enum "GdkWindow")))

;;;     G_ENUM_CLASS

;;;     G_IS_ENUM_CLASS

(test g-is-enum-class
  (is-false (g-is-enum-class (g-type-class-ref "GtkDialogFlags")))
  (is-true  (g-is-enum-class (g-type-class-ref "GtkWindowType")))
  (is-false (g-is-enum-class (g-type-class-ref "GtkWindow"))))

;;;     G_TYPE_IS_FLAGS

(test g-type-is-enum
  (is-true  (g-type-is-flags "GtkDialogFlags"))
  (is-false (g-type-is-flags "GtkWindowType"))
  (is-false (g-type-is-flags "GdkWindow")))

;;;     G_FLAGS_CLASS

;;;     G_IS_FLAGS_CLASS

(test g-is-flags-class
  (is-true  (g-is-flags-class (g-type-class-ref "GtkDialogFlags")))
  (is-false (g-is-flags-class (g-type-class-ref "GtkWindowType")))
  (is-false (g-is-flags-class (g-type-class-ref "GtkWindow"))))

;;;     G_FLAGS_CLASS_TYPE

;;;     g_enum_get_value
;;;     g_enum_get_value_by_name
;;;     g_enum_get_value_by_nick
;;;     g_enum_to_string
;;;     g_flags_get_first_value
;;;     g_flags_get_value_by_name
;;;     g_flags_get_value_by_nick
;;;     g_flags_to_string
;;;     g_enum_register_static
;;;     g_flags_register_static
;;;     g_enum_complete_type_info
;;;     g_flags_complete_type_info

;;; 2020-10-16
