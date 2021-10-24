(def-suite gtk-window-group :in gtk-suite)
(in-suite gtk-window-group)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindowGroup

(test g-window-group-class
  ;; Type check
  (is (g-type-is-object "GtkWindowGroup"))
  ;; Check the registered name
  (is (eq 'gtk-window-group
          (registered-object-type-by-name "GtkWindowGroup")))
  ;; Check the type initializer
  (is (eq (gtype "GtkWindowGroup")
          (gtype (foreign-funcall "gtk_window_group_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject")
          (g-type-parent "GtkWindowGroup")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkWindowGroup"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkWindowGroup"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkWindowGroup")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkWindowGroup" GTK-WINDOW-GROUP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_window_group_get_type")
                       NIL)
             (get-g-type-definition "GtkWindowGroup"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_group_new
;;;     gtk_window_group_add_window
;;;     gtk_window_group_remove_window
;;;     gtk_window_group_list_windows
;;;     gtk_window_group_get_current_grab
;;;     gtk_window_group_get_current_device_grab

;;; 20201-10-18
