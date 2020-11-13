(def-suite g-value :in gobject-suite)
(in-suite g-value)

;;;     GValue

(test g-value-class
   #-windows
  (is (= 24 (foreign-type-size '(:struct g-value))))
   #+windows
  (is (= 24 (foreign-type-size '(:struct g-value))))
  (is (equal '(:data :type)
              (stable-sort (foreign-slot-names '(:struct g-value))
                           #'string-lessp))))

;;;     G_VALUE_INIT                             * not implemented *

;;;     G_VALUE_HOLDS

(test g-value-holds
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value "gint")
    (is-false (g-value-holds value "gboolean"))
    (is-true  (g-value-holds value "gint"))))

;;;     G_VALUE_TYPE

(test g-value-type
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value "gint")
    (is (eq (gtype "gint") (g-value-type value)))))

;;;     G_VALUE_TYPE_NAME

(test g-value-type-name
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value "gint")
    (is (string= "gint" (g-value-type-name value)))))

;;;     G_TYPE_IS_VALUE

(test g-type-is-value
  (is-true (g-type-is-value "gint"))
  (is-true (g-type-is-value "GtkWidget"))
  (is-false (g-type-is-value "GBoxed")))

;;;     G_TYPE_IS_VALUE_ABSTRACT

(test g-type-is-value-abstract
  (is-true (g-type-is-value "gint"))
  (is-true (g-type-is-value "GtkWidget"))
  (is-false (g-type-is-value "GBoxed")))

;;;     G_IS_VALUE                               * not implemented *

;;;     G_TYPE_VALUE

(test g-type-value
  (is (eq (gtype "GValue") (g-type-value))))

;;;     G_TYPE_VALUE_ARRAY                       * not implemented *

;;;     g_value_init

(test g-value-init
  (with-foreign-object (value '(:struct g-value))
    (is-true (pointerp (g-value-init value)))
    (is-false (foreign-slot-value value '(:struct g-value) :type))
    (is-true  (foreign-slot-value value '(:struct g-value) :data))

    (is-true (pointerp (g-value-init value "gint")))
    (is (eq (gtype "gint") (foreign-slot-value value '(:struct g-value) :type)))
    (is-true  (foreign-slot-value value '(:struct g-value) :data))

    (is (= 0 (parse-g-value value)))
    (set-g-value value 10 "gint")
    (is (= 10 (parse-g-value value)))))

;;;     g_value_copy

(test g-value-copy
  (with-foreign-objects ((value1 '(:struct g-value))
                         (value2 '(:struct g-value)))
    (is-true (pointerp (g-value-init value1 "gint")))
    (is-true (pointerp (g-value-init value2 "gint")))
    (is (= 0 (parse-g-value value1)))
;    (is (= 0 (parse-g-value value2)))
;    (set-g-value value1 10 "gint")
;    (g-value-copy value1 value2)
;    (is (= 10 (parse-g-value value1)))
;    (is (= 10 (parse-g-value value2)))
    ))

;;;     g_value_reset

(test g-value-reset
  (with-foreign-object (value '(:struct g-value))
    (is-true (pointerp (g-value-init value "gint")))
    (is (= 0 (parse-g-value value)))
    (set-g-value value 10 "gint")
    (is (= 10 (parse-g-value value)))
    (g-value-reset value)
    (is (= 0 (parse-g-value value)))))

;;;     g_value_unset

(test g-value-unset
  (with-foreign-object (value '(:struct g-value))
    (is-true (pointerp (g-value-init value "gint")))
    (is (= 0 (parse-g-value value)))
    (set-g-value value 10 "gint")
    (is (= 10 (parse-g-value value)))
    (g-value-unset value)
    (is-false (foreign-slot-value value '(:struct g-value) :type))))

;;;     g_value_set_instance
;;;     g_value_fits_pointer                     * not implemented *
;;;     g_value_peek_pointer                     * not implemented *
;;;     g_value_type_compatible
;;;     g_value_type_transformable
;;;     g_value_transform
;;;     g_value_register_transform_func
;;;     g_strdup_value_contents

;;; 2020-10-7
