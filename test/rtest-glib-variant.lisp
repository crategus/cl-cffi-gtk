(in-package :gtk-testsuite)

(def-suite glib-variant :in glib-suite)
(in-suite glib-variant)

;;;     GVariant

;;;   g_variant_unref

(test g-variant-unref
  (let ((bool (g-variant-new-boolean t)))
    (is-true (g-variant-get-boolean bool))
    (g-variant-unref bool)))

;;;   g_variant_ref

(test g-variant-ref
  (let ((bool (g-variant-new-boolean t)))
    (is-true (g-variant-get-boolean bool))
    (setf bool (g-variant-ref bool))
    (is-true (g-variant-get-boolean bool))
    (g-variant-unref bool)
    (is-true (g-variant-get-boolean bool))
    (g-variant-unref bool)))

;;;   g_variant_ref_sink

(test g-variant-ref-sink
  (let ((bool (g-variant-new-boolean t)))
    (is-true (g-variant-is-floating bool))
    (setf bool (g-variant-ref-sink bool))
    (is-false (g-variant-is-floating bool))))

;;;   g_variant_is_floating

(test g-variant-is-floating
  (let ((bool (g-variant-new-boolean t)))
    (is-true (g-variant-is-floating bool))))

;;;   g_variant_take_ref

;;;   g_variant_get_type

(test g-variant-get-type
  (let ((bool (g-variant-new-boolean t)))
    (is (equal "b" (g-variant-type-dup-string (g-variant-get-type bool))))
))

;;;   g_variant_get_type_string

(test g-variant-get-type-string
  (let ((bool (g-variant-new-boolean t)))
    (is (equal "b" (g-variant-get-type-string bool)))))

;;;     g_variant_is_of_type

(test g-variant-is-of-type
  (let ((bool (g-variant-new-boolean t)))
    (is-true (g-variant-is-of-type bool (g-variant-type-new "b")))))

;;;   g_variant_is_container

(test g-variant-is-container
  (let* ((bool (g-variant-new-boolean t))
         (container (g-variant-new-variant bool)))
    (is-false (g-variant-is-container bool))
    (is-true (g-variant-is-container container))))

;;;   g_variant_compare

(test g-variant-compare
  (let ((int1 (g-variant-new-int16 2))
        (int2 (g-variant-new-int16 4)))
    (is (=  0 (g-variant-compare int1 int1)))
    (is (= -2 (g-variant-compare int1 int2)))
    (is (=  2 (g-variant-compare int2 int1)))))

;;;   GVariantClass
;;;   g_variant_classify

(test g-variant-classify
  (let ((bool (g-variant-new-boolean t)))
    (is (eq :boolean (g-variant-classify bool)))))

;;;     g_variant_check_format_string
;;;     g_variant_get
;;;     g_variant_get_va
;;;     g_variant_new
;;;     g_variant_new_va
;;;
;;;     g_variant_new_boolean
;;;     g_variant_new_byte
;;;     g_variant_new_int16
;;;     g_variant_new_uint16
;;;     g_variant_new_int32
;;;     g_variant_new_uint32
;;;     g_variant_new_int64
;;;     g_variant_new_uint64
;;;     g_variant_new_handle
;;;     g_variant_new_double
;;;     g_variant_new_string
;;;     g_variant_new_object_path
;;;     g_variant_is_object_path
;;;     g_variant_new_signature
;;;     g_variant_is_signature
;;;     g_variant_new_variant
;;;     g_variant_new_strv
;;;     g_variant_new_objv
;;;     g_variant_new_bytestring
;;;     g_variant_new_bytestring_array
;;;
;;;     g_variant_get_boolean
;;;     g_variant_get_byte
;;;     g_variant_get_int16
;;;     g_variant_get_uint16
;;;     g_variant_get_int32
;;;     g_variant_get_uint32
;;;     g_variant_get_int64
;;;     g_variant_get_uint64
;;;     g_variant_get_handle
;;;     g_variant_get_double
;;;     g_variant_get_string
;;;     g_variant_dup_string
;;;     g_variant_get_variant
;;;     g_variant_get_strv
;;;     g_variant_dup_strv
;;;     g_variant_get_objv
;;;     g_variant_dup_objv
;;;     g_variant_get_bytestring
;;;     g_variant_dup_bytestring
;;;     g_variant_get_bytestring_array
;;;     g_variant_dup_bytestring_array
;;;
;;;     g_variant_new_maybe
;;;     g_variant_new_array
;;;     g_variant_new_tuple
;;;     g_variant_new_dict_entry
;;;     g_variant_new_fixed_array
;;;
;;;     g_variant_get_maybe
;;;     g_variant_n_children
;;;     g_variant_get_child_value
;;;     g_variant_get_child
;;;     g_variant_lookup_value
;;;     g_variant_lookup
;;;     g_variant_get_fixed_array
;;;
;;;     g_variant_get_size
;;;     g_variant_get_data
;;;     g_variant_get_data_as_bytes
;;;     g_variant_store
;;;     g_variant_new_from_data
;;;     g_variant_new_from_bytes
;;;     g_variant_byteswap
;;;     g_variant_get_normal_form
;;;     g_variant_is_normal_form
;;;
;;;     g_variant_hash
;;;     g_variant_equal
;;;
;;;     g_variant_print
;;;     g_variant_print_string
;;;
;;;     GVariantIter
;;;
;;;     g_variant_iter_copy
;;;     g_variant_iter_free
;;;     g_variant_iter_init
;;;     g_variant_iter_n_children
;;;     g_variant_iter_new
;;;     g_variant_iter_next_value
;;;     g_variant_iter_next
;;;     g_variant_iter_loop
;;;
;;;     GVariantBuilder
;;;
;;;     g_variant_builder_unref
;;;     g_variant_builder_ref
;;;     g_variant_builder_new
;;;     g_variant_builder_init
;;;     g_variant_builder_clear
;;;     g_variant_builder_add_value
;;;     g_variant_builder_add
;;;     g_variant_builder_add_parsed
;;;     g_variant_builder_end
;;;     g_variant_builder_open
;;;     g_variant_builder_close
;;;
;;;     GVariantParseError
;;;
;;;     G_VARIANT_PARSE_ERROR
;;;
;;;     g_variant_parse
;;;     g_variant_new_parsed_va
;;;     g_variant_new_parsed

