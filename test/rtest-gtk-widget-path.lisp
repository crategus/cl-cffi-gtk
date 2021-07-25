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

(test gtk-widget-path-append-for-widget
  (let ((path (gtk-widget-path-new))
        (window (make-instance 'gtk-window)))
    (is (= 0 (gtk-widget-path-append-for-widget path window)))
    (is (string= "window:dir-ltr.background"
                 (gtk-widget-path-to-string path)))))

;;;     gtk_widget_path_copy
;;;     gtk_widget_path_ref
;;;     gtk_widget_path_unref
;;;     gtk_widget_path_free

;;;     gtk_widget_path_get_object_type

(test gtk-widget-path-object-type
  (let ((path (gtk-widget-path-new)))
    (is (= 0 (gtk-widget-path-append-type path "GtkWindow")))
    (is (eq (gtype "GtkWindow")
            (gtk-widget-path-object-type path)))))

;;;     gtk-widget-path-has-parent

;; TODO: Find a working example

(test gtk-widget-path-has-parent
  (let ((path (gtk-widget-path-new)))
    (is (= 0 (gtk-widget-path-append-type path "GtkButton")))
    (is (string= "GtkButton" (gtk-widget-path-to-string path)))
    (is-false (gtk-widget-path-has-parent path "GtkWidget"))))

;;;     gtk_widget_path_is_type

(test gtk-widget-path-is-type
  (let ((path (gtk-widget-path-new)))

    (is (= 0 (gtk-widget-path-append-type path "GtkWindow")))

    (is-true (gtk-widget-path-is-type path "GtkWidget"))
    (is-true (gtk-widget-path-is-type path "GtkWindow"))
    (is-false (gtk-widget-path-is-type path "GtkButton"))
))

;;;     gtk_widget_path_iter_add_class

;;;     gtk_widget_path_iter_add_region

(test gtk-widget-path-iter-add-region
  (let ((path (gtk-widget-path-new)))
    (is-false (gtk-widget-path-iter-add-region
                  path
                  (gtk-widget-path-append-type path "GtkNotebook")
                  "tab"
                  '(:even :first)))
    (is (string= "GtkNotebook tab:even:first"
                 (gtk-widget-path-to-string path)))))

;;;     gtk_widget_path_iter_clear_classes
;;;     gtk_widget_path_iter_clear_regions

;;;     gtk_widget_path_iter_get_name
;;;     gtk_widget_path_iter_set_name

(test gtk-widget-path-iter-name.1
  (let* ((widget (make-instance 'gtk-button))
         (path (gtk-widget-path widget)))
    ;; TODO: Why is this false.
    (is-false (gtk-widget-path-iter-name path -1))))

(test gtk-widget-path-iter-name.2
  (let ((path (gtk-widget-path-new)))
    (is-true (setf (gtk-widget-path-iter-name
                        path
                        (gtk-widget-path-append-type path "GtkLabel"))
                   "first tab label"))
    (is (string= "GtkLabel(first tab label)"
                 (gtk-widget-path-to-string path)))))

;;;     gtk_widget_path_iter_get_object_name

(test gtk-widget-path-iter-get-object-name
  (let* ((widget (make-instance 'gtk-button))
         (path (gtk-widget-path widget)))
    (is (string= "button" (gtk-widget-path-iter-object-name path -1)))))

;;;     gtk_widget_path_iter_get_object_type

(test gtk-widget-path-iter-object-type
  (let* ((widget (make-instance 'gtk-button))
         (path (gtk-widget-path widget)))
    (is (string= "GtkButton"
                 (g-type-name (gtk-widget-path-iter-object-type path -1))))))

;;;     gtk_widget_path_iter_get_siblings
;;;     gtk_widget_path_iter_get_sibling_index

;;;     gtk_widget_path_iter_get_state
;;;     gtk_widget_path_iter_set_state

(test gtk-widget-path-iter-state
  (let ((path (gtk-widget-path-new)))
    (is (= 0 (gtk-widget-path-append-type path "GtkButton")))
    (is (string= "GtkButton" (gtk-widget-path-to-string path)))
    (is (equal '(:active :dir-ltr)
               (setf (gtk-widget-path-iter-state path 0)
                     '(:active :dir-ltr))))
    (is (equal '(:active :dir-ltr) (gtk-widget-path-iter-state path 0)))
    (is (eq  :active (setf (gtk-widget-path-iter-state path 0) :active)))
    (is (equal '(:active) (gtk-widget-path-iter-state path 0)))
    ;; Set more flags
    (let ((flags (gtk-widget-path-iter-state path 0)))
      (is-true (setf (gtk-widget-path-iter-state path 0)
                     (union flags '(:dir-ltr :selected))))
      (is (equal '(:active :selected :dir-ltr)
                 (gtk-widget-path-iter-state path 0))))
    ;; Unset a flag
    (let ((flags (gtk-widget-path-iter-state path 0)))
      (is-true (setf (gtk-widget-path-iter-state path 0)
                     (set-difference flags '(:active))))
      (is (equal '(:selected :dir-ltr)
                 (gtk-widget-path-iter-state path 0))))))

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
    (is (equal '(:even :first)
               (gtk-widget-path-iter-has-region path -1 "tab")))))

;;;     gtk_widget_path_iter_list_classes
;;;     gtk_widget_path_iter_list_regions
;;;     gtk_widget_path_iter_remove_class
;;;     gtk_widget_path_iter_remove_region

;;;     gtk_widget_path_iter_set_object_name
;;;     gtk_widget_path_iter_set_object_type

;;;     gtk_widget_path_length
;;;     gtk_widget_path_new
;;;     gtk_widget_path_prepend_type

;;;     gtk_widget_path_to_string

(test gtk-widget-path-append-type
  (let ((path (gtk-widget-path-new)))
    (is (= 0 (gtk-widget-path-append-type path "GtkWindow")))
    (is (= 1 (gtk-widget-path-append-type path "GtkButton")))
    (is (string= "GtkWindow GtkButton" (gtk-widget-path-to-string path)))))

;;; 2021-7-25
