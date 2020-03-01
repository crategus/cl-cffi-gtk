(def-suite gtk-widget-path :in gtk-suite)
(in-suite gtk-widget-path)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWidgetPath

;;; --- Functions --------------------------------------------------------------

;;;     gtk_widget_path_append_type

(test gtk-widget-path-append-type
  (let ((path (gtk-widget-path-new)))
    (is (= 0 (gtk-widget-path-append-type path "GtkWindow")))
    (is (= 1 (gtk-widget-path-append-type path "GtkButton")))
    (is (string= "GtkWindow GtkButton" (gtk-widget-path-to-string path)))))

;;;     gtk_widget_path_append_with_siblings
;;;     gtk_widget_path_append_for_widget
;;;     gtk_widget_path_copy
;;;     gtk_widget_path_ref
;;;     gtk_widget_path_unref
;;;     gtk_widget_path_free
;;;     gtk_widget_path_get_object_type
;;;     gtk_widget_path_has_parent
;;;     gtk_widget_path_is_type
;;;     gtk_widget_path_iter_add_class

;;;     gtk_widget_path_iter_add_region

(test gtk-widget-path-iter-add-region
  (let ((path (gtk-widget-path-new)))
    (is-false (gtk-widget-path-iter-add-region
                  path
                  (gtk-widget-path-append-type path "GtkNotebook")
                  "tab"
                  '(:even :first)))
    (is (string= "GtkNotebook tab:even:first" (gtk-widget-path-to-string path)))))

;;;     gtk_widget_path_iter_clear_classes
;;;     gtk_widget_path_iter_clear_regions
;;;     gtk_widget_path_iter_get_name
;;;     gtk_widget_path_iter_get_object_name
;;;     gtk_widget_path_iter_get_object_type
;;;     gtk_widget_path_iter_get_siblings
;;;     gtk_widget_path_iter_get_sibling_index
;;;     gtk_widget_path_iter_get_state
;;;     gtk_widget_path_iter_has_class
;;;     gtk_widget_path_iter_has_name
;;;     gtk_widget_path_iter_has_qclass
;;;     gtk_widget_path_iter_has_qname
;;;     gtk_widget_path_iter_has_qregion

;;;     gtk_widget_path_iter_has_region

(test gtk-widget-path-iter-has-region
  (let ((path (gtk-widget-path-new)))
    (is-false (gtk-widget-path-iter-add-region
                  path
                  (gtk-widget-path-append-type path "GtkNotebook")
                  "tab"
                  '(:even :first)))
    (is (string= "GtkNotebook tab:even:first" (gtk-widget-path-to-string path)))
    (is (equal '(:even :first) (gtk-widget-path-iter-has-region path -1 "tab")))))

;;;     gtk_widget_path_iter_list_classes
;;;     gtk_widget_path_iter_list_regions
;;;     gtk_widget_path_iter_remove_class
;;;     gtk_widget_path_iter_remove_region

;;;     gtk_widget_path_iter_set_name

(test gtk-widget-path-iter-set-name
  (let ((path (gtk-widget-path-new)))
    (is-false (gtk-widget-path-iter-set-name
                  path
                  (gtk-widget-path-append-type path "GtkLabel")
                  "first tab label"))
    (is (string= "GtkLabel(first tab label)" (gtk-widget-path-to-string path)))))

;;;     gtk_widget_path_iter_set_object_name
;;;     gtk_widget_path_iter_set_object_type

;;;     gtk_widget_path_iter_set_state

(test gtk-widget-path-iter-set-state
  (let ((path (gtk-widget-path-new)))
    (is (= 0 (gtk-widget-path-append-type path "GtkButton")))
    (is (string= "GtkButton" (gtk-widget-path-to-string path)))
    (is-false (gtk-widget-path-iter-set-state path 0 '(:active :dir-ltr)))
    (is (equal '(:active :dir-ltr) (gtk-widget-path-iter-get-state path 0)))
    (is-false (gtk-widget-path-iter-set-state path 0 :active))
    (is (equal '(:active) (gtk-widget-path-iter-get-state path 0)))
    ;; Set flags in addition
    (let ((flags (gtk-widget-path-iter-get-state path 0)))
      (is-false (gtk-widget-path-iter-set-state path
                                                0
                                                (union flags '(:dir-ltr :selected))))
      (is (equal '(:active :selected :dir-ltr) (gtk-widget-path-iter-get-state path 0))))
    ;; Unset a flag
    (let ((flags (gtk-widget-path-iter-get-state path 0)))
      (is-false (gtk-widget-path-iter-set-state path
                                                0
                                                (set-difference flags '(:active))))
      (is (equal '(:selected :dir-ltr) (gtk-widget-path-iter-get-state path 0))))))

;;;     gtk_widget_path_length
;;;     gtk_widget_path_new
;;;     gtk_widget_path_prepend_type

;;;     gtk_widget_path_to_string

(test gtk-widget-path-append-type
  (let ((path (gtk-widget-path-new)))
    (is (= 0 (gtk-widget-path-append-type path "GtkWindow")))
    (is (= 1 (gtk-widget-path-append-type path "GtkButton")))
    (is (string= "GtkWindow GtkButton" (gtk-widget-path-to-string path)))))
