(def-suite g-bytes :in glib-suite)
(in-suite g-bytes)

;;; --- Types and Values -------------------------------------------------------
;;;
;;;     GByteArray

;;;     GBytes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (foreign-funcall "g_bytes_get_type" g-size))

(test g-bytes-structure
  ;; Type check
  (is (g-type-is-a (gtype "GBytes") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (gtype "GBytes")
          (gtype (foreign-funcall "g_bytes_get_type" g-size)))))

;;; --- Functions --------------------------------------------------------------
;;;
;;;     g_byte_array_new
;;;     g_byte_array_steal
;;;     g_byte_array_new_take
;;;     g_byte_array_sized_new
;;;     g_byte_array_ref
;;;     g_byte_array_unref
;;;     g_byte_array_append
;;;     g_byte_array_prepend
;;;     g_byte_array_remove_index
;;;     g_byte_array_remove_index_fast
;;;     g_byte_array_remove_range
;;;     g_byte_array_sort
;;;     g_byte_array_sort_with_data
;;;     g_byte_array_set_size
;;;     g_byte_array_free
;;;     g_byte_array_free_to_bytes
;;;
;;;     g_bytes_new

(test g-bytes-new.1
  (is (typep (g-bytes-new (null-pointer) 0) 'g-bytes)))

(test g-bytes-new.2
  (multiple-value-bind (data len)
      (foreign-string-alloc "a test string")
    (is (typep (g-bytes-new data len) 'g-bytes))
    (foreign-string-free data)))

;;;     g_bytes_new_take
;;;     g_bytes_new_static
;;;     g_bytes_new_with_free_func
;;;     g_bytes_new_from_bytes

;;;     g_bytes_get_data
;;;     g_bytes_get_size

(test g-bytes-data.1
  (multiple-value-bind (data len)
      (foreign-string-alloc "a test string")
    (let ((bytes (g-bytes-new data len)))
      (is (typep bytes 'g-bytes))
      (is (pointerp (g-bytes-data bytes)))
      (is (= 14 (g-bytes-size bytes)))
      (foreign-string-free data))))

(test g-bytes-data.2
  (multiple-value-bind (data len)
      (foreign-string-alloc "a o u")
    (let ((bytes (g-bytes-new data len)))
      (is (typep bytes 'g-bytes))
      (is (pointerp (g-bytes-data bytes)))
      (is (= 6 (g-bytes-size bytes)))
      (foreign-string-free data))))

(test g-bytes-data.3
  (multiple-value-bind (data len)
      (foreign-string-alloc "ä ö ü")
    (let ((bytes (g-bytes-new data len)))
      (is (typep bytes 'g-bytes))
      (is (pointerp (g-bytes-data bytes)))
      #-windows
      (is (= 9 (g-bytes-size bytes)))
      #+windows
      (is (= 15 (g-bytes-size bytes)))
      (foreign-string-free data))))

;;;     g_bytes_hash
;;;     g_bytes_equal
;;;     g_bytes_compare
;;;     g_bytes_ref
;;;     g_bytes_unref
;;;     g_bytes_unref_to_data
;;;     g_bytes_unref_to_array

;;; 2021-10-17
