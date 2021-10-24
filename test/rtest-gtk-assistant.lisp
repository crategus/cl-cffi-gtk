(def-suite gtk-assistant :in gtk-suite)
(in-suite gtk-assistant)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAssistantPageType

;;;     GtkAssistant

(test gtk-assistant-class
  ;; Type check
  (is (g-type-is-object "GtkAssistant"))
  ;; Check the registered name
  (is (eq 'gtk-assistant
          (registered-object-type-by-name "GtkAssistant")))
  ;; Check the type initializer
  (is (eq (gtype "GtkAssistant")
          (gtype (foreign-funcall "gtk_assistant_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkWindow")
          (g-type-parent "GtkAssistant")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkAssistant"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkAssistant"))))
  ;; Check the class properties
  (is (equal '("use-header-bar")
             (list-class-property-names "GtkAssistant")))
  ;; Get the names of the style properties
  (is (equal '("content-padding" "header-padding")
             (list-class-style-property-names "GtkAssistant")))
  ;; Get the names of the child properties
  (is (equal '("complete" "has-padding" "header-image" "page-type"
               "sidebar-image" "title")
             (list-class-child-property-names "GtkAssistant")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAssistant" GTK-ASSISTANT
                       (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_assistant_get_type")
                       ((USE-HEADER-BAR GTK-ASSISTANT-USE-HEADER-BAR
                         "use-header-bar" "gint" T NIL)))
             (get-g-type-definition "GtkAssistant"))))

;;; --- Properties -------------------------------------------------------------

;;;                 gint    use-header-bar    Read / Write / Construct Only

;;; --- Child Properties -------------------------------------------------------

;;;             gboolean    complete          Read / Write
;;;             gboolean    has-padding       Read / Write
;;;            GdkPixbuf*   header-image      Read / Write
;;; GtkAssistantPageType    page-type         Read / Write
;;;            GdkPixbuf*   sidebar-image     Read / Write
;;;                gchar*   title             Read / Write

;;; --- Style Properties -------------------------------------------------------

;;;                 gint    content-padding   Read
;;;                 gint    header-padding    Read

;;; --- Signals ----------------------------------------------------------------

;;;                 void    apply             Run Last
;;;                 void    cancel            Run Last
;;;                 void    close             Run Last
;;;                 void    escape            Action
;;;                 void    prepare           Run Last

;;; --- Functions --------------------------------------------------------------

;;;     gtk_assistant_new
;;;     gtk_assistant_get_current_page
;;;     gtk_assistant_set_current_page
;;;     gtk_assistant_get_n_pages
;;;     gtk_assistant_get_nth_page
;;;     gtk_assistant_prepend_page
;;;     gtk_assistant_append_page
;;;     gtk_assistant_insert_page
;;;     gtk_assistant_remove_page
;;;
;;;     GtkAssistantPageFunc
;;;     gtk_assistant_set_forward_page_func
;;;
;;;     gtk_assistant_set_page_type
;;;     gtk_assistant_get_page_type
;;;     gtk_assistant_set_page_title
;;;     gtk_assistant_get_page_title
;;;     gtk_assistant_set_page_header_image                * deprecated
;;;     gtk_assistant_get_page_header_image                * deprecated
;;;     gtk_assistant_set_page_side_image                  * deprecated
;;;     gtk_assistant_get_page_side_image                  * deprecated
;;;     gtk_assistant_set_page_complete
;;;     gtk_assistant_get_page_complete
;;;     gtk_assistant_set_page_has_padding
;;;     gtk_assistant_get_page_has_padding
;;;     gtk_assistant_add_action_widget
;;;     gtk_assistant_remove_action_widget
;;;     gtk_assistant_update_buttons_state
;;;     gtk_assistant_commit
;;;     gtk_assistant_next_page
;;;     gtk_assistant_previous_page

;;; 2021-10-18
