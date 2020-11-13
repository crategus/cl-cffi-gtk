(def-suite gobject-param-spec :in gobject-suite)
(in-suite gobject-param-spec)

;;; --- Types and Values -------------------------------------------------------

;;;     GParamSpec
;;;     GParamSpecClass
;;;     GParamFlags

;;;     G_PARAM_STATIC_STRINGS
;;;     G_PARAM_MASK
;;;     G_PARAM_USER_SHIFT

;;;     GParamSpecTypeInfo
;;;     GParamSpecPool

;;; --- Functions --------------------------------------------------------------

;;;     g-type-is-param

(test g-type-is-param
  (is-true (g-type-is-param (g-type-from-instance (g-param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is-true (g-type-is-param (g-type-from-instance (g-param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     G_PARAM_SPEC

;;;     g-is-param-spec

(test g-is-param-spec
  (is-true (g-is-param-spec (g-param-spec-boolean "Boolean" "Bool" "Doku" t '())))
  (is-true (g-is-param-spec (g-param-spec-char "Char" "Char" "Doku" 10 50 25 '()))))

;;;     G_PARAM_SPEC_CLASS
;;;     G_IS_PARAM_SPEC_CLASS
;;;     G_PARAM_SPEC_GET_CLASS

;;;     g-param-spec-type

(test g-param-spec-type
  (is (eq (gtype "GParamBoolean")
          (g-param-spec-type (g-param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is (eq (gtype "GParamChar")
          (g-param-spec-type (g-param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     g-param-spec-type-name

(test g-param-spec-type-name
  (is (string= "GParamBoolean"
               (g-param-spec-type-name (g-param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is (string= "GParamChar"
               (g-param-spec-type-name (g-param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     g-param-spec-value-type

(test g-param-spec-value-type
  (is (eq (gtype "gboolean")
          (g-param-spec-value-type (g-param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is (eq (gtype "gchar")
          (g-param-spec-value-type (g-param-spec-char "Char" "Char" "Doku" 10 50 25 '())))))

;;;     g_param_spec_ref
;;;     g_param_spec_unref
;;;     g_param_spec_sink
;;;     g_param_spec_ref_sink

;;;     g-param-spec-default-value

(test g-param-spec-default-value
  (is-true (parse-g-value (g-param-spec-default-value (g-param-spec-boolean "Boolean" "Bool" "Doku" t '()))))
  (is-false (parse-g-value (g-param-spec-default-value (g-param-spec-boolean "Boolean" "Bool" "Doku" nil '()))))
  (is (= 25 (parse-g-value (g-param-spec-default-value (g-param-spec-int "Integer" "int" "Doku" 10 50 25 '()))))))

;;;     g-param-value-set-default

(test g-param-value-set-default
  (let ((param (g-param-spec-int "Integer" "int" "Doku" 10 50 25 '())))
    (with-foreign-object (value '(:struct g-value))
      (g-value-init value +g-type-int+)
      (is-false (g-param-value-set-default param value))
      (is (= 25 (parse-g-value value))))))

;;;     g-param-value-defaults

(test g-param-value-defaults
  (let ((param (g-param-spec-int "Integer" "int" "Doku" 10 50 25 '())))
    (with-foreign-object (value '(:struct g-value))
      (g-value-init value +g-type-int+)
      (is-false (g-param-value-defaults param value))
      (is-false (g-param-value-set-default param value))
      (is-true  (g-param-value-defaults param value)))))

;;;     g-param-value-validate

(test g-param-value-validate
  (let ((param (g-param-spec-int "Integer" "int" "Doku" 10 50 25 '())))
    (with-foreign-object (value '(:struct g-value))
      (g-value-init value +g-type-int+)
      (is-true (g-param-value-validate param value))
      (is (= 10 (parse-g-value value)))
      (set-g-value value 100 +g-type-int+)
      (is-true (g-param-value-validate param value))
      (is (= 50 (parse-g-value value)))
      (set-g-value value 25 +g-type-int+)
      (is-false (g-param-value-validate param value))
      (is (= 25 (parse-g-value value))))))

;;;     g_param_value_convert
;;;     g_param_values_cmp
;;;     g_param_spec_is_valid_name

;;;     g-param-spec-name

(test g-param-spec-name
  (let ((pspec (g-param-spec-internal "GParamBoolean" "Boolean" "Bool" "Doku" '(:readable :writable))))
    (is (string= "Boolean" (g-param-spec-name pspec)))))

;;;     g_param_spec_get_name_quark

;;;     g-param-spec-nick

(test g-param-spec-nick
  (let ((pspec (g-param-spec-internal "GParamBoolean" "Boolean" "Bool" "Doku" '(:readable :writable))))
    (is (string= "Bool" (g-param-spec-nick pspec)))))

;;;     g-param-spec-blurb

(test g-param-spec-blurb
  (let ((pspec (g-param-spec-internal "GParamBoolean" "Boolean" "Bool" "Doku" '(:readable :writable))))
    (is (string= "Doku" (g-param-spec-blurb pspec)))))

;;;     g_param_spec_get_qdata
;;;     g_param_spec_set_qdata
;;;     g_param_spec_set_qdata_full
;;;     g_param_spec_steal_qdata
;;;     g_param_spec_get_redirect_target

;;;     g-param-spec-internal

(test g-param-spec-internal
  (let ((pspec (g-param-spec-internal "GParamBoolean" "Boolean" "Bool" "Doku" '(:readable :writable))))
    (is-true (g-is-param-spec pspec))
    (is (eq (gtype "GParamBoolean") (g-param-spec-type pspec)))
    (is (string= "GParamBoolean" (g-param-spec-type-name pspec)))
    (is (eq (gtype "gboolean") (g-param-spec-value-type pspec)))))

;;;     g_param_type_register_static

;;;     g_param_spec_pool_new
;;;     g_param_spec_pool_insert
;;;     g_param_spec_pool_remove
;;;     g_param_spec_pool_lookup
;;;     g_param_spec_pool_list
;;;     g_param_spec_pool_list_owned

;;; 2020-10-10
