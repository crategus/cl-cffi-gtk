(in-package :gtk-testsuite)

(def-suite glib-variant-type :in glib-suite)
(in-suite glib-variant-type)

;;;   GVariantType

;;;   G_VARIANT_TYPE_BOOLEAN

(test g-variant-type-boolean
  (is (equal "b" +g-variant-type-boolean+)))

;;;   G_VARIANT_TYPE_BYTE

(test g-variant-type-byte
  (is (equal "y" +g-variant-type-byte+)))

;;;     G_VARIANT_TYPE_INT16

(test g-variant-type-int16
  (is (equal "n" +g-variant-type-int16+)))

;;;     G_VARIANT_TYPE_UINT16

(test g-variant-type-uint16
  (is (equal "q" +g-variant-type-uint16+)))

;;;     G_VARIANT_TYPE_INT32

(test g-variant-type-int32
  (is (equal "i" +g-variant-type-int32+)))

;;;     G_VARIANT_TYPE_UINT32

(test g-variant-type-uint32
  (is (equal "u" +g-variant-type-uint32+)))

;;;     G_VARIANT_TYPE_INT64

(test g-variant-type-int64
  (is (equal "x" +g-variant-type-int64+)))

;;;     G_VARIANT_TYPE_UINT64

(test g-variant-type-uint64
  (is (equal "t" +g-variant-type-uint64+)))

;;;     G_VARIANT_TYPE_HANDLE

(test g-variant-type-handle
  (is (equal "h" +g-variant-type-handle+)))

;;;     G_VARIANT_TYPE_DOUBLE

(test g-variant-type-double
  (is (equal "d" +g-variant-type-double+)))

;;;     G_VARIANT_TYPE_STRING

(test g-variant-type-string
  (is (equal "s" +g-variant-type-string+)))

;;;     G_VARIANT_TYPE_OBJECT_PATH

(test g-variant-type-object-path
  (is (equal "o" +g-variant-type-object-path+)))

;;;     G_VARIANT_TYPE_SIGNATURE

(test g-variant-type-signature
  (is (equal "g" +g-variant-type-signature+)))

;;;     G_VARIANT_TYPE_VARIANT

(test g-variant-type-variant
  (is (equal "v" +g-variant-type-variant+)))

;;;     G_VARIANT_TYPE_ANY

(test g-variant-type-any
  (is (equal "*" +g-variant-type-any+)))

;;;     G_VARIANT_TYPE_BASIC

(test g-variant-type-basic
  (is (equal "?" +g-variant-type-basic+)))

;;;     G_VARIANT_TYPE_MAYBE

(test g-variant-type-maybe
  (is (equal "m*" +g-variant-type-maybe+)))

;;;     G_VARIANT_TYPE_ARRAY

(test g-variant-type-array
  (is (equal "a*" +g-variant-type-array+)))

;;;     G_VARIANT_TYPE_TUPLE

(test g-variant-type-tuple
  (is (equal "r" +g-variant-type-tuple+)))

;;;     G_VARIANT_TYPE_UNIT

(test g-variant-type-unit
  (is (equal "()" +g-variant-type-unit+)))

;;;     G_VARIANT_TYPE_DICT_ENTRY

(test g-variant-type-dict-entry
  (is (equal "{?*}" +g-variant-type-dict-entry+)))

;;;     G_VARIANT_TYPE_DICTIONARY

(test g-variant-type-dictionary
  (is (equal "a{?*}" +g-variant-type-dictionary+)))

;;;     G_VARIANT_TYPE_STRING_ARRAY

(test g-variant-type-string-array
  (is (equal "as" +g-variant-type-string-array+)))

;;;     G_VARIANT_TYPE_OBJECT_PATH_ARRAY

(test g-variant-type-object-path-array
  (is (equal "ao" +g-variant-type-object-path-array+)))

;;;     G_VARIANT_TYPE_BYTESTRING

(test g-variant-type-bytestring
  (is (equal "ay" +g-variant-type-bytestring+)))

;;;     G_VARIANT_TYPE_BYTESTRING_ARRAY

(test g-variant-type-bytestring-array
  (is (equal "aay" +g-variant-type-bytestring-array+)))

;;;     G_VARIANT_TYPE_VARDICT

(test g-variant-type-vardict
  (is (equal "a(sv)" +g-variant-type-vardict+)))

;;;     G_VARIANT_TYPE

;;;     g_variant_type_free
;;;     g_variant_type_copy

;;;   g_variant_type_new

(test g-variant-type-new
  (is (eq 'g-variant-type (type-of (g-variant-type-new "b"))))
  (is (eq 'g-variant-type (type-of (g-variant-type-new +g-variant-type-boolean+))))
)

;;;     g_variant_type_string_is_valid

(test g-variant-type-string-is-valid.1
  (is-true (g-variant-type-string-is-valid "b")))

(test g-variant-type-string-is-valid.2
  (is-true (g-variant-type-string-is-valid "aaaaai")))

(test g-variant-type-string-is-valid.3
  (is-true (g-variant-type-string-is-valid "(ui(nq((y)))s)")))

(test g-variant-type-string-is-valed.4
  (is-true (g-variant-type-string-is-valid "a(aa(ui)(qna{ya(yd)}))")))

;;;   g_variant_type_string_scan

(test g-variant-type-string-scan
  (is-true (g-variant-type-string-scan "b")))

;;;   g_variant_type_get_string_length

(test g-variant-type-get-string-length
  (is (= 1 (g-variant-type-get-string-length (g-variant-type-new "b")))))

;;;   g_variant_type_peek_string

(test g-variant-type-peek-string
  (is (equal "b" (g-variant-type-peek-string (g-variant-type-new "b")))))

;;;   g_variant_type_dup_string

(test g-variant-type-dup-string
  (is (equal "b" (g-variant-type-dup-string (g-variant-type-new "b")))))

;;;   g_variant_type_is_definite

(test g-variant-type-is-definite
  (is-true (g-variant-type-is-definite (g-variant-type-new "b"))))

;;;   g_variant_type_is_container

(test g-variant-type-is-container
  (is-false (g-variant-type-is-container (g-variant-type-new "b"))))

;;;   g_variant_type_is_basic

(test g-variant-type-is-basic
  (is-true (g-variant-type-is-basic (g-variant-type-new "b"))))

;;;   g_variant_type_is_maybe

(test g-variant-type-is-maybe
  (is-false (g-variant-type-is-maybe (g-variant-type-new "b"))))

;;;   g_variant_type_is_array

(test g-variant-type-is-array
  (is-false (g-variant-type-is-array (g-variant-type-new "b"))))

;;;   g_variant_type_is_tuple

(test g-variant-type-is-tuple
  (is-false (g-variant-type-is-tuple (g-variant-type-new "b"))))

;;;   g_variant_type_is_dict_entry

(test g-variant-type-is-dict-entry
  (is-false (g-variant-type-is-dict-entry (g-variant-type-new "b"))))

;;;   g_variant_type_is_variant

(test g-variant-type-is-variant
  (is-false (g-variant-type-is-variant (g-variant-type-new "b"))))

;;;   g_variant_type_hash

(test g-variant-type-hash
  (is-true (integerp (g-variant-type-hash (g-variant-type-new "b")))))

;;;   g_variant_type_equal

(test g-variant-type-equal
  (let ((bool1 (g-variant-type-new "b"))
        (bool2 (g-variant-type-new "b"))
        (int16 (g-variant-type-new "n")))
    (is-true (g-variant-type-equal bool1 bool2))
    (is-false (g-variant-type-equal bool1 int16))))

;;;   g_variant_type_is_subtype_of

(test g-variant-type-is-subtype-of
  (let ((bool (g-variant-type-new "b"))
        (any  (g-variant-type-new "*")))
    (is-true (g-variant-type-is-subtype-of bool bool))
    (is-true (g-variant-type-is-subtype-of bool any))
    (is-false (g-variant-type-is-subtype-of any bool))))

;;;   g_variant_type_new_maybe

(test g-variant-type-new-maybe
  (let ((bool (g-variant-type-new "b")))
    (is-true (g-variant-type-new-maybe bool))
    (is-true (g-variant-type-is-maybe (g-variant-type-new-maybe bool)))
    (is (equal "mb" (g-variant-type-dup-string (g-variant-type-new-maybe bool))))))

;;;   g_variant_type_new_array

(test g-variant-type-new-arry
  (let ((bool (g-variant-type-new "b")))
    (is-true (g-variant-type-new-array bool))
    (is-true (g-variant-type-is-array (g-variant-type-new-array bool)))
    (is (equal "ab" (g-variant-type-dup-string (g-variant-type-new-array bool))))))

;;;   g_variant_type_new_tuple

(test g-variant-type-new-tuple
  (let ((bool (g-variant-type-new "b")))
    (is-true (g-variant-type-new-tuple bool bool bool))
    (is-true (g-variant-type-is-tuple (g-variant-type-new-tuple bool bool bool)))
    (is (equal "(bbb)" (g-variant-type-dup-string (g-variant-type-new-tuple bool bool bool))))))

;;;     g_variant_type_new_dict_entry

(test g-variant-type-new-dict-entry
  (let ((bool (g-variant-type-new "b"))
        (int16 (g-variant-type-new "n")))
    (is-true (g-variant-type-new-dict-entry int16 bool))
    (is-true (g-variant-type-is-dict-entry (g-variant-type-new-dict-entry int16 bool)))
    (is (equal "{nb}" (g-variant-type-dup-string (g-variant-type-new-dict-entry int16 bool))))))

;;;     g_variant_type_element

(test g-variant-type-element
  (let ((bool (g-variant-type-new "b")))
    (is-true (g-variant-type-element (g-variant-type-new-array bool)))
    (is (equal "b" (g-variant-type-dup-string (g-variant-type-element (g-variant-type-new-array bool)))))))

;;;     g_variant_type_n_items

(test g-variant-type-n-items
  (let ((bool (g-variant-type-new "b")))
    (is (= 3 (g-variant-type-n-items (g-variant-type-new-tuple bool bool bool))))))

;;;   g_variant_type_first

(test g-variant-type-first
  (let* ((bool (g-variant-type-new "b"))
         (int16 (g-variant-type-new "n"))
         (tuple (g-variant-type-new-tuple bool int16 bool bool int16)))
    (is (equal "b" (g-variant-type-dup-string (g-variant-type-first tuple))))))

;;;   g_variant_type_next

(test g-variant-type-next
  (let* ((bool (g-variant-type-new "b"))
         (int16 (g-variant-type-new "n"))
         (tuple (g-variant-type-new-tuple bool int16 bool bool int16))
         (iter (g-variant-type-first tuple)))
    (is (equal "b" (g-variant-type-dup-string iter)))
    (setf iter (g-variant-type-next iter))
    (is (equal "n" (g-variant-type-dup-string iter)))
    (setf iter (g-variant-type-next iter))
    (is (equal "b" (g-variant-type-dup-string iter)))
    (setf iter (g-variant-type-next iter))
    (is (equal "b" (g-variant-type-dup-string iter)))
    (setf iter (g-variant-type-next iter))
    (is (equal "n" (g-variant-type-dup-string iter)))
    (setf iter (g-variant-type-next iter))
    (is (null iter))))

;;;   g_variant_type_key
;;;   g_variant_type_value

(test g-variant-type-key
  (let* ((key-type (g-variant-type-new "n"))
         (value-type (g-variant-type-new "b"))
         (dict (g-variant-type-new-dict-entry key-type value-type)))
    (is (equal "n" (g-variant-type-dup-string (g-variant-type-key dict))))
    (is (equal "b" (g-variant-type-dup-string (g-variant-type-value dict))))))

