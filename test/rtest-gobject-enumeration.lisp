(def-suite gobject-enumeration :in gobject-suite)
(in-suite gobject-enumeration)

;;; --- Types and Values -------------------------------------------------------

(test define-g-enum-macro
  (is (equal '(PROGN
                (DEFCENUM (GTK-WINDOW-TYPE :INT)
                          (:TOPLEVEL 0)
                          (:POPUP 1))
                (GOBJECT::REGISTER-ENUM-TYPE "GtkWindowType" 'GTK-WINDOW-TYPE)
                (EXPORT 'GTK-WINDOW-TYPE (FIND-PACKAGE "GTK"))
                (GLIB-INIT::AT-INIT NIL
                  (IF (FOREIGN-SYMBOL-POINTER "gtk_window_type_get_type")
                      (FOREIGN-FUNCALL-POINTER
                        (FOREIGN-SYMBOL-POINTER "gtk_window_type_get_type")
                        NIL
                        G-SIZE)
                      (WARN "Type initializer '~A' is not available"
                            "gtk_window_type_get_type"))))
              (macroexpand '(define-g-enum "GtkWindowType" gtk-window-type
                             (:export t
                              :type-initializer "gtk_window_type_get_type")
                             (:toplevel 0)
                             (:popup 1))))))

(test define-g-flags-macro
  (is (equal '(PROGN
                (DEFBITFIELD GDK-DRAG-ACTION
                  (:DEFAULT 1)
                  (:COPY 2)
                  (:MOVE 4)
                  (:LINK 8)
                  (:PRIVATE 16)
                  (:ASK 32))
                (GOBJECT::REGISTER-FLAGS-TYPE "GdkDragAction" 'GDK-DRAG-ACTION)
                (EXPORT 'GDK-DRAG-ACTION (FIND-PACKAGE "GDK"))
                (GLIB-INIT::AT-INIT NIL
                  (IF (FOREIGN-SYMBOL-POINTER "gdk_drag_action_get_type")
                      (FOREIGN-FUNCALL-POINTER
                        (FOREIGN-SYMBOL-POINTER "gdk_drag_action_get_type") NIL
                        G-SIZE)
                      (WARN "Type initializer '~A' is not available"
                            "gdk_drag_action_get_type"))))
             (macroexpand '(define-g-flags "GdkDragAction" gdk-drag-action
                            (:export t
                             :type-initializer "gdk_drag_action_get_type")
                            (:default 1)
                            (:copy 2)
                            (:move 4)
                            (:link 8)
                            (:private 16)
                            (:ask 32))))))

;;;   g-enum-class

(test g-enum-class
  (is (= 32 (foreign-type-size '(:struct g-enum-class))))
  (is (equal '(:maximum :minimum :n-values :type-class :values)
             (stable-sort (foreign-slot-names '(:struct g-enum-class))
             #'string-lessp))))

;;;   g-enum-value

(test g-enum-value
  (is (= 24 (foreign-type-size '(:struct g-enum-value))))
  (is (equal '(:name :nick :value)
             (stable-sort (foreign-slot-names '(:struct g-enum-value))
                          #'string-lessp))))

;;;   g-flags-class

(test g-flags-class
  (is (= 24 (foreign-type-size '(:struct g-flags-class))))
  (is (equal '(:mask :n-values :type-class :values)
             (stable-sort (foreign-slot-names '(:struct g-flags-class))
                          #'string-lessp))))

;;;   g-flags-value

(test g-flags-value
  (is (= 24 (foreign-type-size '(:struct g-flags-value))))
  (is (equal '(:name :nick :value)
             (stable-sort (foreign-slot-names '(:struct g-flags-value))
                          #'string-lessp))))

;;; --- Functions --------------------------------------------------------------

;;;     G_ENUM_CLASS_TYPE
;;;     G_ENUM_CLASS_TYPE_NAME

;;;     g-type-is-enum

(test g-type-is-enum
  (is-false (g-type-is-enum "GtkDialogFlags"))
  (is-true  (g-type-is-enum "GtkWindowType"))
  (is-false (g-type-is-enum "GdkWindow")))

;;;     G_ENUM_CLASS
;;;     G_IS_ENUM_CLASS

;;;     G_TYPE_IS_FLAGS

(test g-type-is-enum
  (is-true  (g-type-is-flags "GtkDialogFlags"))
  (is-false (g-type-is-flags "GtkWindowType"))
  (is-false (g-type-is-flags "GdkWindow")))

;;;     G_FLAGS_CLASS
;;;     G_IS_FLAGS_CLASS
;;;     G_FLAGS_CLASS_TYPE

;;;     g_enum_get_value
;;;     g_enum_get_value_by_name
;;;     g_enum_get_value_by_nick
;;;     g_enum_to_string
;;;     g_flags_get_first_value
;;;     g_flags_get_value_by_name
;;;     g_flags_get_value_by_nick
;;;     g_flags_to_string
;;;     g_enum_register_static
;;;     g_flags_register_static
;;;     g_enum_complete_type_info
;;;     g_flags_complete_type_info

;;; 2021-4-7
