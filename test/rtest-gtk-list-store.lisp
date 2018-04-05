(in-package :gtk-testsuite)

(def-suite gtk-list-store :in gtk-suite)
(in-suite gtk-list-store)

;;;   GtkListStore

;;;   gtk_list_store_new

(test gtk-list-store-new.1
  (let ((store (make-instance 'gtk-list-store
                              :column-types '("gint" "GString" "GdkPixbuf"))))
    (is (eq 'gtk-list-store (type-of store)))
    (is (= 3 (gtk-tree-model-get-n-columns store)))
    (is (equal "gint" (g-type-name (gtk-tree-model-get-column-type store 0))))
    (is (equal "GString" (g-type-name (gtk-tree-model-get-column-type store 1))))
    (is (equal "GdkPixbuf" (g-type-name (gtk-tree-model-get-column-type store 2))))))

(test gtk-list-store-new.2
  (let ((store (gtk-list-store-new "gint" "GString" "GdkPixbuf")))
    (is (eq 'gtk-list-store (type-of store)))
    (is (= 3 (gtk-tree-model-get-n-columns store)))
    (is (equal "gint" (g-type-name (gtk-tree-model-get-column-type store 0))))
    (is (equal "GString" (g-type-name (gtk-tree-model-get-column-type store 1))))
    (is (equal "GdkPixbuf" (g-type-name (gtk-tree-model-get-column-type store 2))))))

;;;   gtk_list_store_newv                                  Not implemented

;;;   gtk_list_store_set_column_types

(test gtk-list-store-set-column-types
  (let ((store (make-instance 'gtk-list-store)))
    (gtk-list-store-set-column-types store "gint" "GString" "GdkPixbuf")
    (is (= 3 (gtk-tree-model-get-n-columns store)))
    (is (equal "gint" (g-type-name (gtk-tree-model-get-column-type store 0))))
    (is (equal "GString" (g-type-name (gtk-tree-model-get-column-type store 1))))
    (is (equal "GdkPixbuf" (g-type-name (gtk-tree-model-get-column-type store 2))))))

;;;   gtk_list_store_set

(test gtk-list-store-set
  (let* ((store (gtk-list-store-new "gint" "gchararray"))
         (iter (gtk-list-store-set store (gtk-list-store-append store) 99 "string")))
    (is (= 99 (gtk-tree-model-get-value store iter 0)))
    (is (equal "string" (gtk-tree-model-get-value store iter 1)))
    (gtk-list-store-set store iter 199)
    (is (= 199 (gtk-tree-model-get-value store iter 0)))
    (is (equal "string" (gtk-tree-model-get-value store iter 1)))
))

;;;   gtk_list_store_set_valist                            Not implemented

;;;   gtk_list_store_set_value

(test gtk-list-store-set
  (let* ((store (gtk-list-store-new "gint" "gchararray"))
         (iter (gtk-list-store-append store)))
    (gtk-list-store-set-value store iter 0 99)
    (is (= 99 (gtk-tree-model-get-value store iter 0)))
    (gtk-list-store-set-value store iter 1 "string")
    (is (equal "string" (gtk-tree-model-get-value store iter 1)))))

;;;   gtk_list_store_set_valuesv                         Not exported

;;;   gtk_list_store_remove

(test gtk-list-store-remove
  (let* ((store (gtk-list-store-new "gint" "gchararray"))
         (iter (gtk-list-store-append store)))
    (is-true iter)
    (setf iter (gtk-list-store-remove store iter))
    (is-false iter)))

;;;   gtk_list_store_insert



;;;     gtk_list_store_insert_before
;;;     gtk_list_store_insert_after
;;;     gtk_list_store_insert_with_values
;;;     gtk_list_store_insert_with_valuesv
;;;     gtk_list_store_prepend
;;;     gtk_list_store_append
;;;     gtk_list_store_clear
;;;     gtk_list_store_iter_is_valid
;;;     gtk_list_store_reorder
;;;     gtk_list_store_swap


;;;     gtk_list_store_move_before

(test gtk-list-store-move-after
  (let* ((store (gtk-list-store-new "gint" "gchararray"))
         (iter (gtk-list-store-append store)))
    (setf iter (gtk-list-store-append store))
    (gtk-list-store-move-before store iter nil)))

;;;     gtk_list_store_move_after

(test gtk-list-store-move-after
  (let* ((store (gtk-list-store-new "gint" "gchararray"))
         (iter (gtk-list-store-append store)))
    (setf iter (gtk-list-store-prepend store))
    (gtk-list-store-move-after store iter nil)))

