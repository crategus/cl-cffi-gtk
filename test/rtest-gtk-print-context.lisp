(def-suite gtk-print-context :in gtk-suite)
(in-suite gtk-print-context)

;;;     GtkPrintContext

(test gtk-print-context-class
  ;; Type check
  (is (g-type-is-object "GtkPrintContext"))
  ;; Check the registered name
  (is (eq 'gtk-print-context
          (registered-object-type-by-name "GtkPrintContext")))
  ;; Check the type initializer
  (is (eq (gtype "GtkPrintContext")
          (gtype (foreign-funcall "gtk_print_context_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject") (g-type-parent "GtkPrintContext")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkPrintContext"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkPrintContext"))))
  ;; Check the class properties
  (is (equal '()
             (stable-sort (mapcar #'g-param-spec-name
                                  (g-object-class-list-properties "GtkPrintContext"))
                          #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPrintContext" GTK-PRINT-CONTEXT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_print_context_get_type")
                       NIL)
             (get-g-type-definition "GtkPrintContext"))))

;;;     Functions

;;;     gtk_print_context_get_cairo_context
;;;     gtk_print_context_set_cairo_context
;;;     gtk_print_context_get_page_setup
;;;     gtk_print_context_get_width
;;;     gtk_print_context_get_height
;;;     gtk_print_context_get_dpi_x
;;;     gtk_print_context_get_dpi_y
;;;     gtk_print_context_get_pango_fontmap
;;;     gtk_print_context_create_pango_context
;;;     gtk_print_context_create_pango_layout
;;;     gtk_print_context_get_hard_margins

