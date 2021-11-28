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
;;;     g_list_model_items_changed

;;; 2021-11-16
