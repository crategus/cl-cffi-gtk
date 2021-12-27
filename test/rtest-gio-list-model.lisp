(def-suite g-list-model :in gio-suite)
(in-suite g-list-model)

;;; --- Types and Values -------------------------------------------------------

;;;     GListModel

(test g-list-model-interface
  ;; Type check
  (is (g-type-is-interface "GListModel"))
  ;; Check the registered name
  (is (eq 'g-list-model
          (registered-object-type-by-name "GListModel")))
  ;; Check the type initializer
  (is (eq (gtype "GListModel")
          (gtype (foreign-funcall "g_list_model_get_type" g-size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (mapcar #'g-param-spec-name
                     (g-object-interface-list-properties "GListModel"))))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GListModel"
                                  G-LIST-MODEL
                                  (:EXPORT T))
             (get-g-type-definition "GListModel"))))

;;; --- Signals ----------------------------------------------------------------

;;;     void    items-changed

;;; --- Functions --------------------------------------------------------------

;;;     g_list_model_get_item_type
;;;     g_list_model_get_n_items
;;;     g_list_model_get_item
;;;     g_list_model_get_object

(test g-list-model-get.1
  (let ((store (g-list-store-new "GObject")))
    ;; Append some objects
    (is-false (g-list-store-append store (make-instance 'gtk-button)))
    (is-false (g-list-store-append store (make-instance 'gtk-label)))
    ;; Use the interace functions
    (is (eq (gtype "GObject") (g-list-model-item-type store)))
    (is (= 2 (g-list-model-n-items store)))
    (is (pointerp (g-list-model-item store 0)))
    (is (typep (g-list-model-object store 0) 'gtk-button))
    (is (typep (g-list-model-object store 1) 'gtk-label))))

(test g-list-model-get.2
  (let ((store (g-list-store-new "GtkButton")))
    ;; Append some objects
    (is-false (g-list-store-append store (make-instance 'gtk-button)))
    (is-false (g-list-store-append store (make-instance 'gtk-toggle-button)))
    ;; Use the interace functions
    (is (eq (gtype "GtkButton") (g-list-model-item-type store)))
    (is (= 2 (g-list-model-n-items store)))
    (is (pointerp (g-list-model-item store 0)))
    (is (typep (g-list-model-object store 0) 'gtk-button))
    (is (typep (g-list-model-object store 1) 'gtk-toggle-button))))

;;;     g_list_model_items_changed

;;; 2021-12-10
