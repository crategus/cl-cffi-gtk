(def-suite g-list-store :in gio-suite)
(in-suite g-list-store)

;;; --- Types and Values -------------------------------------------------------

;;;     GListStore

(test g-list-store-class
  ;; Type check
  (is (g-type-is-object "GListStore"))
  ;; Check the registered name
  (is (eq 'g-list-store
          (registered-object-type-by-name "GListStore")))
  ;; Check the type initializer
  (is (eq (gtype "GListStore")
          (gtype (foreign-funcall "g_list_store_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject")
          (g-type-parent "GListStore")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GListStore"))))
  ;; Check the interfaces
  (is (equal '("GListModel")
             (mapcar #'g-type-name (g-type-interfaces "GListStore"))))
  ;; Check the class properties
  (is (equal '("item-type")
             (list-class-property-names "GListStore")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GListStore" G-LIST-STORE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GListModel"))
                       ((ITEM-TYPE G-LIST-STORE-ITEM-TYPE "item-type" "GType" T
                         NIL)))
             (get-g-type-definition "GListStore"))))

;;; --- Properties -------------------------------------------------------------

;;;    GType*   item-type

;;; --- Functions --------------------------------------------------------------

;;;     g_list_store_new
;;;     g_list_store_insert
;;;     g_list_store_insert_sorted
;;;     g_list_store_append
;;;     g_list_store_remove
;;;     g_list_store_remove_all
;;;     g_list_store_splice
;;;     g_list_store_sort
;;;     g_list_store_find
;;;     g_list_store_find_with_equal_func

;;; 2021-11-16
