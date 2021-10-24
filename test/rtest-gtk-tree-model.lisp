(def-suite gtk-tree-model :in gtk-suite)
(in-suite gtk-tree-model)

(defun create-and-fill-list-store ()
  (let ((list-data '("Name1" "Name2" "Name3" "Name4" "Name5"))
        ;; Create a new list store with three columns
        (list-store (make-instance 'gtk-list-store
                                   :column-types
                                   '("gint" "gchararray" "gboolean"))))
    ;; Fill in some data
    (loop for data in list-data
          for i from 0 do
          ;; Add a new row to the model
          (gtk-list-store-set list-store
                              (gtk-list-store-append list-store)
                              i
                              data
                              nil))
    ;; Modify a particular row
    (let ((path (gtk-tree-path-new-from-string "2")))
      (gtk-list-store-set-value list-store
                                (gtk-tree-model-iter list-store path)
                                2
                                t))
    ;; Return the new list store
    list-store))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeModel
;;;     GtkTreeIter
;;;     GtkTreePath
;;;     GtkTreeRowReference
;;;     GtkTreeModelIface
;;;     GtkTreeModelFlags

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeModelForeachFunc

;;;     gtk_tree_path_new

(test gtk-tree-path-new
  (is (eq 'gtk-tree-path (type-of (gtk-tree-path-new)))))

;;;     gtk_tree_path_new_from_string

(test gtk-tree-path-new-from-string
  (is (eq 'gtk-tree-path (type-of (gtk-tree-path-new-from-string "10:4:0")))))

;;;     gtk_tree_path_new_from_indices

(test gtk-tree-path-new-from-indices
  (is (eq 'gtk-tree-path (type-of (gtk-tree-path-new-from-indices 10 4 0)))))

;;;     gtk_tree_path_new_from_indicesv

;;;     gtk_tree_path_to_string

(test gtk-tree-path-to-string
  (let ((path1 (gtk-tree-path-new-from-string "10:4:0"))
        (path2 (gtk-tree-path-new-from-indices 10 4 0)))
    (is (string= "10:4:0" (gtk-tree-path-to-string path1)))
    (is (string= "10:4:0" (gtk-tree-path-to-string path2)))))

;;;     gtk_tree_path_new_first

(test gtk-tree-path-new-first
  (is (eq 'gtk-tree-path (type-of (gtk-tree-path-new-first))))
  (is (string= "0" (gtk-tree-path-to-string (gtk-tree-path-new-first)))))

;;;     gtk_tree_path_append_index

(test gtk-tree-path-append-index
  (let ((path (gtk-tree-path-new-from-string "10")))
    (is (eq 'gtk-tree-path (type-of (gtk-tree-path-append-index path 4))))
    (is (eq 'gtk-tree-path (type-of (setf path (gtk-tree-path-append-index path 4)))))
    (is (string= "10:4" (gtk-tree-path-to-string path)))
    (is (eq 'gtk-tree-path (type-of (gtk-tree-path-append-index path 3))))
    (is (eq 'gtk-tree-path (type-of (setf path (gtk-tree-path-append-index path 3)))))
    (is (string= "10:4:3" (gtk-tree-path-to-string path)))))

;;;     gtk_tree_path_prepend_index

(test gtk-tree-path-prepend-index
  (let ((path (gtk-tree-path-new-from-string "10")))
    (is (eq 'gtk-tree-path (type-of (gtk-tree-path-prepend-index path 4))))
    (is (eq 'gtk-tree-path (type-of (setf path (gtk-tree-path-prepend-index path 4)))))
    (is (string= "4:10" (gtk-tree-path-to-string path)))
    (is (eq 'gtk-tree-path (type-of (gtk-tree-path-prepend-index path 3))))
    (is (eq 'gtk-tree-path (type-of (setf path (gtk-tree-path-prepend-index path 3)))))
    (is (string= "3:4:10" (gtk-tree-path-to-string path)))))

;;;     gtk_tree_path_get_depth

(test gtk-tree-path-depth
  (let ((path (gtk-tree-path-new-from-string "10:4:0")))
    (is (= 3 (gtk-tree-path-depth path)))))

;;;     gtk_tree_path_get_indices

(test gtk-tree-path-indices
  (let ((path (gtk-tree-path-new-from-string "10:4:0")))
    (is (equal '(10 4 0) (gtk-tree-path-indices path)))))

;;;     gtk_tree_path_get_indices_with_depth
;;;     gtk_tree_path_free

;;;     gtk_tree_path_copy

(test gtk-tree-path-copy
  (let ((path (gtk-tree-path-new-from-string "10:4:0")))
    (is (string= "10:4:0"
                 (gtk-tree-path-to-string (gtk-tree-path-copy path))))))

;;;     gtk_tree_path_compare

(test gtk-tree-path-compare
  (let ((path-1 (gtk-tree-path-new-from-string "10:4:0"))
       (path-2 (gtk-tree-path-new-from-string "10:4:1")))
  (is (= 0 (gtk-tree-path-compare path-1 path-1)))
  (is (= 0 (gtk-tree-path-compare path-2 path-2)))
  (is (= -1 (gtk-tree-path-compare path-1 path-2)))
  (is (=  1 (gtk-tree-path-compare path-2 path-1)))))

;;;     gtk_tree_path_next

(test gtk-tree-path-next
  (let ((path (gtk-tree-path-new-from-string "10:4:0")))
    (is (string= "10:4:1" (gtk-tree-path-to-string (gtk-tree-path-next path))))
    (is (string= "10:4:1" (gtk-tree-path-to-string (gtk-tree-path-next path))))
    (is (string= "10:4:1"
                 (gtk-tree-path-to-string (setf path
                                                (gtk-tree-path-next path)))))
    (is (string= "10:4:2"
                 (gtk-tree-path-to-string (setf path
                                                (gtk-tree-path-next path)))))))

;;;     gtk_tree_path_prev

(test gtk-tree-path-prev
  (let ((path (gtk-tree-path-new-from-string "10:4:2")))
    (is (string= "10:4:1" (gtk-tree-path-to-string (gtk-tree-path-prev path))))
    (is (string= "10:4:1" (gtk-tree-path-to-string (gtk-tree-path-prev path))))
    (is (string= "10:4:1"
                 (gtk-tree-path-to-string (setf path
                                                (gtk-tree-path-prev path)))))
    (is (string= "10:4:0"
                 (gtk-tree-path-to-string (setf path
                                                (gtk-tree-path-prev path)))))
    (is-false (gtk-tree-path-prev path))))

;;;     gtk_tree_path_up

(test gtk-tree-path-down
  (let ((path (gtk-tree-path-new-from-string "10:4:2")))
    (is (string= "10:4"
                 (gtk-tree-path-to-string (setf path
                                                (gtk-tree-path-up path)))))
    (is (string= "10"
                 (gtk-tree-path-to-string (setf path
                                                (gtk-tree-path-up path)))))

    (is-false (gtk-tree-path-to-string (gtk-tree-path-up path)))))

;;;     gtk_tree_path_down

(test gtk-tree-path-down
  (let ((path (gtk-tree-path-new-from-string "10:4:2")))
    (is (string= "10:4:2:0"
                 (gtk-tree-path-to-string (setf path
                                                (gtk-tree-path-down path)))))
    (is (string= "10:4:2:0:0"
                 (gtk-tree-path-to-string (setf path
                                                (gtk-tree-path-down path)))))))

;;;     gtk_tree_path_is_ancestor

(test gtk-tree-path-is-ancestor
  (let ((path-1 (gtk-tree-path-new-from-string "10:4:3"))
        (path-2 (gtk-tree-path-new-from-string "10:4:3:2")))
    (is-true (gtk-tree-path-is-ancestor path-1 path-2))
    (is-false (gtk-tree-path-is-ancestor path-2 path-1))))

;;;     gtk_tree_path_is_descendant

(test gtk-tree-path-is-descendant
  (let ((path-1 (gtk-tree-path-new-from-string "10:4:3"))
        (path-2 (gtk-tree-path-new-from-string "10:4:3:2")))
    (is-false (gtk-tree-path-is-descendant path-1 path-2))
    (is-true (gtk-tree-path-is-descendant path-2 path-1))))

;;;     gtk_tree_row_reference_new
;;;     gtk_tree_row_reference_new_proxy
;;;     gtk_tree_row_reference_get_model
;;;     gtk_tree_row_reference_get_path
;;;     gtk_tree_row_reference_valid
;;;     gtk_tree_row_reference_free
;;;     gtk_tree_row_reference_copy
;;;     gtk_tree_row_reference_inserted
;;;     gtk_tree_row_reference_deleted
;;;     gtk_tree_row_reference_reordered

;;;     gtk_tree_iter_copy
;;;     gtk_tree_iter_free

;;;     gtk_tree_model_get_flags

(test gtk-tree-model-flags
  (let ((model (make-instance 'gtk-list-store)))
    (is (equal '(:ITERS-PERSIST :LIST-ONLY) (gtk-tree-model-flags model)))))

;;;     gtk_tree_model_get_n_columns

(test gtk-tree-model-n-columns
  (let ((model (make-instance 'gtk-list-store
                              :column-types
                              '("gint" "gchararray" "gboolean"))))
    (is (= 3 (gtk-tree-model-n-columns model)))))

;;;     gtk_tree_model_get_column_type

(test gtk-tree-model-column-type
  (let ((model (make-instance 'gtk-list-store
                              :column-types
                              '("gint" "gchararray" "gboolean"))))
    (is (string= "gint" (g-type-name (gtk-tree-model-column-type model 0))))
    (is (string= "gchararray" (g-type-name (gtk-tree-model-column-type model 1))))
    (is (string= "gboolean" (g-type-name (gtk-tree-model-column-type model 2))))))

;;;     gtk_tree_model_get_iter

(test gtk-tree-model-iter
  (let ((model (create-and-fill-list-store))
        (path (gtk-tree-path-new-from-string "2")))

    (is (eq 'gtk-tree-iter (type-of (gtk-tree-model-iter model path))))

))

;;;     gtk_tree_model_get_iter_from_string

(test gtk-tree-model-iter-from-string
  (let ((model (create-and-fill-list-store)))

    (is (eq 'gtk-tree-iter (type-of (gtk-tree-model-iter-from-string model "2"))))

))

;;;     gtk_tree_model_get_iter_first

(test gtk-tree-model-iter-first
  (let ((model (create-and-fill-list-store)))

    (is (eq 'gtk-tree-iter (type-of (gtk-tree-model-iter-first model))))

))

;;;     gtk_tree_model_get_path

;(test gtk-tree-model-path
;  (let* ((model (create-and-fill-list-store))
;         (iter (gtk-tree-model-iter-from-string model "2")))

;    (is (string= "2"
;                 (gtk-tree-path-to-string (gtk-tree-model-path model iter))))

;))

;;;     gtk_tree_model_get_value
;;;     gtk_tree_model_iter_next
;;;     gtk_tree_model_iter_previous
;;;     gtk_tree_model_iter_children
;;;     gtk_tree_model_iter_has_child
;;;     gtk_tree_model_iter_n_children
;;;     gtk_tree_model_iter_nth_child
;;;     gtk_tree_model_iter_parent
;;;     gtk_tree_model_get_string_from_iter
;;;     gtk_tree_model_ref_node
;;;     gtk_tree_model_unref_node
;;;     gtk_tree_model_get
;;;     gtk_tree_model_get_valist
;;;     gtk_tree_model_foreach
;;;     gtk_tree_model_row_changed
;;;     gtk_tree_model_row_inserted
;;;     gtk_tree_model_row_has_child_toggled
;;;     gtk_tree_model_row_deleted
;;;     gtk_tree_model_rows_reordered
;;;     gtk_tree_model_rows_reordered_with_length

;;; 2021-10-19
