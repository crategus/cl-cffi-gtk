(def-suite gtk-tree-view-column :in gtk-suite)
(in-suite gtk-tree-view-column)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeViewColumnSizing

;;;     GtkTreeViewColumn

(test gtk-tree-view-column-class
  ;; Type check
  (is (g-type-is-object "GtkTreeViewColumn"))
  ;; Check the registered name
  (is (eq 'gtk-tree-view-column
          (registered-object-type-by-name "GtkTreeViewColumn")))
  ;; Check the type initializer
  (is (eq (gtype "GtkTreeViewColumn")
          (gtype (foreign-funcall "gtk_tree_view_column_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GInitiallyUnowned")
          (g-type-parent "GtkTreeViewColumn")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkTreeViewColumn"))))
  ;; Check the interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable")
             (mapcar #'g-type-name (g-type-interfaces "GtkTreeViewColumn"))))
  ;; Check the class properties
  (is (equal '("alignment" "cell-area" "clickable" "expand" "fixed-width"
               "max-width" "min-width" "reorderable" "resizable" "sizing"
               "sort-column-id" "sort-indicator" "sort-order" "spacing" "title"
               "visible" "widget" "width" "x-offset")
             (list-class-property-names "GtkTreeViewColumn")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkTreeViewColumn" GTK-TREE-VIEW-COLUMN
                       (:SUPERCLASS G-INITIALLY-UNOWNED :EXPORT T :INTERFACES
                        ("GtkBuildable" "GtkCellLayout") :TYPE-INITIALIZER
                        "gtk_tree_view_column_get_type")
                       ((ALIGNMENT GTK-TREE-VIEW-COLUMN-ALIGNMENT "alignment"
                         "gfloat" T T)
                        (CELL-AREA GTK-TREE-VIEW-COLUMN-CELL-AREA "cell-area"
                         "GtkCellArea" T NIL)
                        (CLICKABLE GTK-TREE-VIEW-COLUMN-CLICKABLE "clickable"
                         "gboolean" T T)
                        (EXPAND GTK-TREE-VIEW-COLUMN-EXPAND "expand" "gboolean"
                         T T)
                        (FIXED-WIDTH GTK-TREE-VIEW-COLUMN-FIXED-WIDTH
                         "fixed-width" "gint" T T)
                        (MAX-WIDTH GTK-TREE-VIEW-COLUMN-MAX-WIDTH "max-width"
                         "gint" T T)
                        (MIN-WIDTH GTK-TREE-VIEW-COLUMN-MIN-WIDTH "min-width"
                         "gint" T T)
                        (REORDERABLE GTK-TREE-VIEW-COLUMN-REORDERABLE
                         "reorderable" "gboolean" T T)
                        (RESIZABLE GTK-TREE-VIEW-COLUMN-RESIZABLE "resizable"
                         "gboolean" T T)
                        (SIZING GTK-TREE-VIEW-COLUMN-SIZING "sizing"
                         "GtkTreeViewColumnSizing" T T)
                        (SORT-COLUMN-ID GTK-TREE-VIEW-COLUMN-SORT-COLUMN-ID
                         "sort-column-id" "gint" T T)
                        (SORT-INDICATOR GTK-TREE-VIEW-COLUMN-SORT-INDICATOR
                         "sort-indicator" "gboolean" T T)
                        (SORT-ORDER GTK-TREE-VIEW-COLUMN-SORT-ORDER
                         "sort-order" "GtkSortType" T T)
                        (SPACING GTK-TREE-VIEW-COLUMN-SPACING "spacing" "gint"
                         T T)
                        (TITLE GTK-TREE-VIEW-COLUMN-TITLE "title" "gchararray"
                         T T)
                        (VISIBLE GTK-TREE-VIEW-COLUMN-VISIBLE "visible"
                         "gboolean" T T)
                        (WIDGET GTK-TREE-VIEW-COLUMN-WIDGET "widget"
                         "GtkWidget" T T)
                        (WIDTH GTK-TREE-VIEW-COLUMN-WIDTH "width" "gint" T NIL)
                        (X-OFFSET GTK-TREE-VIEW-COLUMN-X-OFFSET "x-offset"
                         "gint" T NIL)))
             (get-g-type-definition "GtkTreeViewColumn"))))

;;; --- Properties -------------------------------------------------------------

;;;                  gfloat    alignment         Read / Write
;;;             GtkCellArea*   cell-area         Read / Write / Construct
;;;                gboolean    clickable         Read / Write
;;;                gboolean    expand            Read / Write
;;;                    gint    fixed-width       Read / Write
;;;                    gint    max-width         Read / Write
;;;                    gint    min-width         Read / Write
;;;                gboolean    reorderable       Read / Write
;;;                gboolean    resizable         Read / Write
;;; GtkTreeViewColumnSizing    sizing            Read / Write
;;;                    gint    sort-column-id    Read / Write
;;;                gboolean    sort-indicator    Read / Write
;;;             GtkSortType    sort-order        Read / Write
;;;                    gint    spacing           Read / Write
;;;                   gchar*   title             Read / Write
;;;                gboolean    visible           Read / Write
;;;               GtkWidget*   widget            Read / Write
;;;                    gint    width             Read
;;;                    gint    x-offset          Read

;;; --- Signals ----------------------------------------------------------------

;;;                    void    clicked           Run Last

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeCellDataFunc

;;;     gtk_tree_view_column_new
;;;     gtk_tree_view_column_new_with_area
;;;     gtk_tree_view_column_new_with_attributes
;;;     gtk_tree_view_column_pack_start
;;;     gtk_tree_view_column_pack_end
;;;     gtk_tree_view_column_clear
;;;     gtk_tree_view_column_add_attribute
;;;     gtk_tree_view_column_set_attributes
;;;     gtk_tree_view_column_set_cell_data_func
;;;     gtk_tree_view_column_clear_attributes
;;;     gtk_tree_view_column_set_spacing                   Accessor
;;;     gtk_tree_view_column_get_spacing                   Accessor
;;;     gtk_tree_view_column_set_visible                   Accessor
;;;     gtk_tree_view_column_get_visible                   Accessor
;;;     gtk_tree_view_column_set_resizable                 Accessor
;;;     gtk_tree_view_column_get_resizable                 Accessor
;;;     gtk_tree_view_column_set_sizing                    Accessor
;;;     gtk_tree_view_column_get_sizing                    Accessor
;;;     gtk_tree_view_column_get_width                     Accessor
;;;     gtk_tree_view_column_get_fixed_width               Accessor
;;;     gtk_tree_view_column_set_fixed_width               Accessor
;;;     gtk_tree_view_column_set_min_width                 Accessor
;;;     gtk_tree_view_column_get_min_width                 Accessor
;;;     gtk_tree_view_column_set_max_width                 Accessor
;;;     gtk_tree_view_column_get_max_width                 Accessor
;;;     gtk_tree_view_column_clicked
;;;     gtk_tree_view_column_set_title                     Accessor
;;;     gtk_tree_view_column_get_title                     Accessor
;;;     gtk_tree_view_column_set_expand                    Accessor
;;;     gtk_tree_view_column_get_expand                    Accessor
;;;     gtk_tree_view_column_set_clickable                 Accessor
;;;     gtk_tree_view_column_get_clickable                 Accessor
;;;     gtk_tree_view_column_set_widget                    Accessor
;;;     gtk_tree_view_column_get_widget                    Accessor
;;;     gtk_tree_view_column_get_button
;;;     gtk_tree_view_column_set_alignment                 Accessor
;;;     gtk_tree_view_column_get_alignment                 Accessor
;;;     gtk_tree_view_column_set_reorderable               Accessor
;;;     gtk_tree_view_column_get_reorderable               Accessor
;;;     gtk_tree_view_column_set_sort_column_id            Accessor
;;;     gtk_tree_view_column_get_sort_column_id            Accessor
;;;     gtk_tree_view_column_set_sort_indicator            Accessor
;;;     gtk_tree_view_column_get_sort_indicator            Accessor
;;;     gtk_tree_view_column_set_sort_order                Accessor
;;;     gtk_tree_view_column_get_sort_order                Accessor
;;;     gtk_tree_view_column_cell_set_cell_data
;;;     gtk_tree_view_column_cell_get_size
;;;     gtk_tree_view_column_cell_get_position
;;;     gtk_tree_view_column_cell_is_visible
;;;     gtk_tree_view_column_focus_cell
;;;     gtk_tree_view_column_queue_resize
;;;     gtk_tree_view_column_get_tree_view
;;;     gtk_tree_view_column_get_x_offset                  Accessor

;;; 2021-10-19
