(def-suite gtk-cell-view :in gtk-suite)
(in-suite gtk-cell-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellView

(test gtk-cell-view-class
  ;; Type check
  (is (g-type-is-object "GtkCellView"))
  ;; Check the registered name
  (is (eq 'gtk-cell-view
          (registered-object-type-by-name "GtkCellView")))
  ;; Check the type initializer
  (is (eq (gtype "GtkCellView")
          (gtype (foreign-funcall "gtk_cell_view_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkWidget") (g-type-parent "GtkCellView")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkCellView"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkCellLayout"
               "GtkOrientable")
             (mapcar #'g-type-name (g-type-interfaces "GtkCellView"))))
  ;; Check the class properties
  (is (equal '("background" "background-gdk" "background-rgba" "background-set"
               "cell-area" "cell-area-context" "draw-sensitive" "fit-model"
               "model" "orientation")
             (list-class-property-names "GtkCellView")))
  ;; Check the style properties
  (is (equal '()
             (list-class-style-property-names "GtkCellView")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCellView" GTK-CELL-VIEW
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkCellLayout"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_cell_view_get_type")
                       ((BACKGROUND GTK-CELL-VIEW-BACKGROUND "background"
                         "gchararray" NIL T)
                        (BACKGROUND-GDK GTK-CELL-VIEW-BACKGROUND-GDK
                         "background-gdk" "GdkColor" T T)
                        (BACKGROUND-RGBA GTK-CELL-VIEW-BACKGROUND-RGBA
                         "background-rgba" "GdkRGBA" T T)
                        (BACKGROUND-SET GTK-CELL-VIEW-BACKGROUND-SET
                         "background-set" "gboolean" T T)
                        (CELL-AREA GTK-CELL-VIEW-CELL-AREA "cell-area"
                         "GtkCellArea" T NIL)
                        (CELL-AREA-CONTEXT GTK-CELL-VIEW-CELL-AREA-CONTEXT
                         "cell-area-context" "GtkCellAreaContext" T NIL)
                        (DRAW-SENSITIVE GTK-CELL-VIEW-DRAW-SENSITIVE
                         "draw-sensitive" "gboolean" T T)
                        (FIT-MODEL GTK-CELL-VIEW-FIT-MODEL "fit-model"
                         "gboolean" T T)
                        (MODEL GTK-CELL-VIEW-MODEL "model" "GtkTreeModel" T T)))
             (get-g-type-definition "GtkCellView"))))

;;; --- Properties -------------------------------------------------------------

;;;              gchar*   background           Write
;;;           GdkColor*   background-gdk       Read / Write
;;;            GdkRGBA*   background-rgba      Read / Write
;;;           gboolean    background-set       Read / Write
;;;        GtkCellArea*   cell-area            Read / Write / Construct Only
;;; GtkCellAreaContext*   cell-area-context    Read / Write / Construct Only
;;;           gboolean    draw-sensitive       Read / Write
;;;           gboolean    fit-model            Read / Write
;;;       GtkTreeModel*   model                Read / Write

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_view_new
;;;     gtk_cell_view_new_with_context
;;;     gtk_cell_view_new_with_text
;;;     gtk_cell_view_new_with_markup
;;;     gtk_cell_view_new_with_pixbuf
;;;     gtk_cell_view_set_model                            Accessor
;;;     gtk_cell_view_get_model                            Accessor
;;;     gtk_cell_view_set_displayed_row
;;;     gtk_cell_view_get_displayed_row
;;;     gtk_cell_view_get_size_of_row
;;;     gtk_cell_view_set_background_color                 deprecated
;;;     gtk_cell_view_set_background_rgba                  Accessor
;;;     gtk_cell_view_set_draw_sensitive                   Accessor
;;;     gtk_cell_view_get_draw_sensitive                   Accessor
;;;     gtk_cell_view_set_fit_model                        Accessor
;;;     gtk_cell_view_get_fit_model                        Accessor

;;; 2021-10-19
