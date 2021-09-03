(def-suite g-menu :in gio-suite)
(in-suite g-menu)

;;; --- Types and Values -------------------------------------------------------

;;;     GMenu

(test g-menu-class
  ;; Type check
  (is (g-type-is-object "GMenu"))
  ;; Check the registered name
  (is (eq 'g-menu
          (registered-object-type-by-name "GMenu")))
  ;; Check the type initializer
  (is (eq (gtype "GMenu")
          (gtype (foreign-funcall "g_menu_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GMenuModel") (g-type-parent "GMenu")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GMenu"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GMenu"))))
  ;; Check the class properties
  (is (equal '()
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GMenu"))
                   #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GMenu" G-MENU
                       (:SUPERCLASS G-MENU-MODEL
                        :EXPORT T :INTERFACES NIL) NIL)
             (get-g-type-definition "GMenu"))))

;;;     GMenuItem

(test g-menu-item-class
  ;; Type check
  (is (g-type-is-object "GMenuItem"))
  ;; Check the registered name
  (is (eq 'g-menu-item
          (registered-object-type-by-name "GMenuItem")))
  ;; Check the type initializer
  (is (eq (gtype "GMenuItem")
          (gtype (foreign-funcall "g_menu_item_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GMenuItem")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GMenuItem"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GMenuItem"))))
  ;; Check the class properties
  (is (equal '()
             (sort (mapcar #'g-param-spec-name
                           (g-object-class-list-properties "GMenuItem"))
                   #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GMenuItem" G-MENU-ITEM
                       (:SUPERCLASS G-OBJECT
                        :EXPORT T :INTERFACES NIL) NIL)
             (get-g-type-definition "GMenuItem"))))

;;; --- Functions --------------------------------------------------------------

;;;     g_menu_new

(test g-menu-new
  (is (typep (g-menu-new) 'g-menu)))

;;;     g_menu_freeze

(test g-menu-freeze
  (let ((menu (g-menu-new)))
    (is-true (g-menu-model-is-mutable menu))
    (is-false (g-menu-freeze menu))
    ;; A frozen menu is not mutable
    (is-false (g-menu-model-is-mutable menu))))

;;;     g_menu_insert
;;;     g_menu_prepend
;;;     g_menu_append

(test g-menu-insert
  (let ((menu (g-menu-new)))
    (is-false (g-menu-insert menu 0 "insert" "action"))
    (is (= 1 (g-menu-model-n-items menu)))
    (is-false (g-menu-prepend menu "prepend" "action"))
    (is (= 2 (g-menu-model-n-items menu)))
    (is-false (g-menu-append menu "append" "action"))
    (is (= 3 (g-menu-model-n-items menu)))))

;;;     g_menu_insert_item
;;;     g_menu_append_item
;;;     g_menu_prepend_item

(test g-menu-insert-item
  (let ((menu (g-menu-new)))
    (is-false (g-menu-insert-item menu 0 (g-menu-item-new "insert" nil)))
    (is (= 1 (g-menu-model-n-items menu)))
    (is-false (g-menu-prepend-item menu (g-menu-item-new "prepend" nil)))
    (is (= 2 (g-menu-model-n-items menu)))
    (is-false (g-menu-append-item menu (g-menu-item-new "append" nil)))
    (is (= 3 (g-menu-model-n-items menu)))))

;;;     g_menu_insert_section
;;;     g_menu_prepend_section
;;;     g_menu_append_section

(test g-menu-insert-section
  (let ((menu (g-menu-new))
        (section (g-menu-new)))
    ;; Add threee items to the section
    (is-false (g-menu-append section "item1" nil))
    (is-false (g-menu-append section "item2" nil))
    (is-false (g-menu-append section "item3" nil))
    (is (= 3 (g-menu-model-n-items section)))
    ;; Insert section into the menu
    (is-false (g-menu-insert-section menu 0 "section1" section))
    (is (= 1 (g-menu-model-n-items menu)))
    ;; Prepend section to the menu
    (is-false (g-menu-prepend-section menu "section2" section))
    (is (= 2 (g-menu-model-n-items menu)))
    ;; Append section to the menu
    (is-false (g-menu-append-section menu "section3" section))
    (is (= 3 (g-menu-model-n-items menu)))))

;;;     g_menu_append_submenu
;;;     g_menu_insert_submenu
;;;     g_menu_prepend_submenu

(test g-menu-insert-submenu
  (let ((menu (g-menu-new))
        (submenu (g-menu-new)))
    ;; Add threee items to the submenu
    (is-false (g-menu-append submenu "item1" nil))
    (is-false (g-menu-append submenu "item2" nil))
    (is-false (g-menu-append submenu "item3" nil))
    (is (= 3 (g-menu-model-n-items submenu)))
    ;; Insert submenu into the menu
    (is-false (g-menu-insert-submenu menu 0 "submenu1" submenu))
    (is (= 1 (g-menu-model-n-items menu)))
    ;; Prepend submenu to the menu
    (is-false (g-menu-prepend-submenu menu "submenu2" submenu))
    (is (= 2 (g-menu-model-n-items menu)))
    ;; Append submenu to the menu
    (is-false (g-menu-append-submenu menu "submenu3" submenu))
    (is (= 3 (g-menu-model-n-items menu)))))

;;;     g_menu_remove
;;;     g_menu_remove_all

(test g-menu-remove
  (let ((menu (g-menu-new)))
    (is-false (g-menu-append menu "append1" nil))
    (is-false (g-menu-append menu "append2" nil))
    (is-false (g-menu-append menu "append3" nil))
    (is (= 3 (g-menu-model-n-items menu)))
    (is-false (g-menu-remove menu 1))
    (is (= 2 (g-menu-model-n-items menu)))
    (is-false (g-menu-remove-all menu))
    (is (= 0 (g-menu-model-n-items menu)))))

;;;     g_menu_item_new

(test g-menu-item-new
  (is (typep (g-menu-item-new nil nil) 'g-menu-item)))

;;;     g_menu_item_new_section
;;;     g_menu_item_new_submenu
;;;     g_menu_item_new_from_model

;;;     g_menu_item_set_label
;;;     g_menu_item_set_icon
;;;     g_menu_item_set_action_and_target_value
;;;     g_menu_item_set_action_and_target
;;;     g_menu_item_set_detailed_action
;;;     g_menu_item_set_section
;;;     g_menu_item_set_submenu

;;;     g_menu_item_get_attribute_value
;;;     g_menu_item_set_attribute_value

(test g-menu-item-attribute-value.1
  (let ((item (g-menu-item-new "Label" "Action")))
    (is (string= "Label"
                 (g-variant-string
                     (g-menu-item-attribute-value item
                                                  "label"
                                                  (g-variant-type-new "s")))))
    (is (string= "Action"
                 (g-variant-string
                     (g-menu-item-attribute-value item
                                                  "action"
                                                  (g-variant-type-new "s")))))))

(test g-menu-item-attribute-value.2
  (let ((item (g-menu-item-new "Label" "Action")))
    (is (string= "Label"
                 (g-variant-string
                     (g-menu-item-attribute-value item
                                                  "label"
                                                  (g-variant-type-new "s")))))

    (is (string= "newlabel"
                 (g-variant-string
                     (setf (g-menu-item-attribute-value item "label")
                           (g-variant-new-string "newlabel")))))
    (is (string= "newlabel"
                 (g-variant-string
                     (g-menu-item-attribute-value item
                                                  "label"
                                                  (g-variant-type-new "s")))))

    (is (string= "Action"
                 (g-variant-string
                     (g-menu-item-attribute-value item
                                                  "action"
                                                  (g-variant-type-new "s")))))
    (is (string= "newaction"
                 (g-variant-string
                     (setf (g-menu-item-attribute-value item "action")
                           (g-variant-new-string "newaction")))))
    (is (string= "newaction"
                 (g-variant-string
                     (g-menu-item-attribute-value item
                                                  "action"
                                                  (g-variant-type-new "s")))))))

(test g-menu-item-attribute-value.3
  (let ((item (g-menu-item-new "Label" "Action")))
    (is (string= "Label"
                 (g-variant-string
                     (g-menu-item-attribute-value item "label"))))
    (is (string= "newlabel"
                 (g-variant-string
                     (setf (g-menu-item-attribute-value item "label")
                           (g-variant-new-string "newlabel")))))
    (is (string= "newlabel"
                 (g-variant-string
                     (g-menu-item-attribute-value item "label"))))

    (is (string= "Action"
                 (g-variant-string
                     (g-menu-item-attribute-value item "action"))))
    (is (string= "newaction"
                 (g-variant-string
                     (setf (g-menu-item-attribute-value item "action")
                           (g-variant-new-string "newaction")))))
    (is (string= "newaction"
                 (g-variant-string
                     (g-menu-item-attribute-value item "action"))))))

(test g-menu-item-attribute-value.4
  (let ((item (g-menu-item-new "Label" "Action")))
    (is (string= "Label"
                 (g-variant-string
                     (g-menu-item-attribute-value item "label" "s"))))
    (is (string= "newlabel"
                 (g-variant-string
                     (setf (g-menu-item-attribute-value item "label")
                           (g-variant-new-string "newlabel")))))
    (is (string= "newlabel"
                 (g-variant-string
                     (g-menu-item-attribute-value item "label" "s"))))

    (is (string= "Action"
                 (g-variant-string
                     (g-menu-item-attribute-value item "action" "s"))))
    (is (string= "newaction"
                 (g-variant-string
                     (setf (g-menu-item-attribute-value item "action")
                           (g-variant-new-string "newaction")))))
    (is (string= "newaction"
                 (g-variant-string
                     (g-menu-item-attribute-value item "action" "s"))))))

;;;     g_menu_item_get_attribute
;;;     g_menu_item_set_attribute

;;;     g_menu_item_get_link
;;;     g_menu_item_set_link

(test g-menu-item-link
  (let ((item (g-menu-item-new "Label" "Action"))
        (submenu (g-menu-new)))
    (is (typep (setf (g-menu-item-link item "submenu") submenu) 'g-menu-model))
    (is (typep (g-menu-item-link item "submenu") 'g-menu-model))))

;;; 2021-8-14
