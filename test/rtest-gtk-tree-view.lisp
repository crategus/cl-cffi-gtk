(def-suite gtk-tree-view :in gtk-suite)
(in-suite gtk-tree-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeViewDropPosition

(test gtk-tree-view-drop-position
  ;; Check the type
  (is (g-type-is-enum "GtkTreeViewDropPosition"))
  ;; Check the type initializer
  (is (eq (gtype "GtkTreeViewDropPosition")
          (gtype (foreign-funcall "gtk_tree_view_drop_position_get_type"
                 g-size))))
  ;; Check the registered name
  (is (eq 'gtk-tree-view-drop-position
          (registered-enum-type "GtkTreeViewDropPosition")))
  ;; Check the names
  (is (equal '("GTK_TREE_VIEW_DROP_BEFORE" "GTK_TREE_VIEW_DROP_AFTER"
               "GTK_TREE_VIEW_DROP_INTO_OR_BEFORE"
               "GTK_TREE_VIEW_DROP_INTO_OR_AFTER")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkTreeViewDropPosition"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkTreeViewDropPosition"))))
  ;; Check the nick names
  (is (equal '("before" "after" "into-or-before" "into-or-after")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkTreeViewDropPosition"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkTreeViewDropPosition"
                             GTK-TREE-VIEW-DROP-POSITION
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_tree_view_drop_position_get_type")
                             (:BEFORE 0)
                             (:AFTER 1)
                             (:INTO-OR-BEFORE 2)
                             (:INTO-OR-AFTER 3))
             (get-g-type-definition "GtkTreeViewDropPosition"))))

;;;     GtkTreeViewPrivate

;;;     GtkTreeViewGridLines

(test gtk-tree-view-grid-lines
  ;; Check the type
  (is (g-type-is-enum "GtkTreeViewGridLines"))
  ;; Check the type initializer
  (is (eq (gtype "GtkTreeViewGridLines")
          (gtype (foreign-funcall "gtk_tree_view_grid_lines_get_type" g-size))))
  ;; Check the registered name
  (is (eq 'gtk-tree-view-grid-lines
          (registered-enum-type "GtkTreeViewGridLines")))
  ;; Check the names
  (is (equal '("GTK_TREE_VIEW_GRID_LINES_NONE"
               "GTK_TREE_VIEW_GRID_LINES_HORIZONTAL"
               "GTK_TREE_VIEW_GRID_LINES_VERTICAL"
               "GTK_TREE_VIEW_GRID_LINES_BOTH")
             (mapcar #'enum-item-name
                     (get-enum-items "GtkTreeViewGridLines"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'enum-item-value
                     (get-enum-items "GtkTreeViewGridLines"))))
  ;; Check the nick names
  (is (equal '("none" "horizontal" "vertical" "both")
             (mapcar #'enum-item-nick
                     (get-enum-items "GtkTreeViewGridLines"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkTreeViewGridLines"
                             GTK-TREE-VIEW-GRID-LINES
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_tree_view_grid_lines_get_type")
                             (:NONE 0)
                             (:HORIZONTAL 1)
                             (:VERTICAL 2)
                             (:BOTH 3))
             (get-g-type-definition "GtkTreeViewGridLines"))))

;;;     GtkTreeView

(test gtk-tree-view-class
  ;; Type check
  (is (g-type-is-object "GtkTreeView"))
  ;; Check the registered name
  (is (eq 'gtk-tree-view
          (registered-object-type-by-name "GtkTreeView")))
  ;; Check the type initializer
  (is (eq (gtype "GtkTreeView")
          (gtype (foreign-funcall "gtk_tree_view_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkContainer") (g-type-parent "GtkTreeView")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkTreeView"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkScrollable")
             (mapcar #'g-type-name (g-type-interfaces "GtkTreeView"))))
  ;; Check the class properties
  (is (equal #-linux
             '("activate-on-single-click" "enable-grid-lines" "enable-search"
               "enable-tree-lines" "expander-column" "fixed-height-mode"
               "hadjustment" "headers-clickable" "headers-visible"
               "hover-expand" "hover-selection" "hscroll-policy"
               "level-indentation" "model" "reorderable" "rubber-banding"
               "rules-hint" "search-column" "show-expanders" "tooltip-column"
               "vadjustment" "vscroll-policy")
             #+linux
             '("activate-on-single-click" "enable-grid-lines" "enable-search"
               "enable-tree-lines" "expander-column" "fixed-height-mode"
               "hadjustment" "headers-clickable" "headers-visible"
               "hover-expand" "hover-selection" "hscroll-policy"
               "level-indentation" "model" "reorderable" "rubber-banding"
               "rules-hint" "search-column" "show-expanders" "tooltip-column"
               "ubuntu-almost-fixed-height-mode" "vadjustment" "vscroll-policy")
             (list-class-property-names "GtkTreeView")))
  ;; Get the names of the style properties.
  (is (equal '("allow-rules" "even-row-color" "expander-size"
               "grid-line-pattern" "grid-line-width" "horizontal-separator"
               "indent-expanders" "odd-row-color" "tree-line-pattern"
               "tree-line-width" "vertical-separator")
             (list-class-style-property-names "GtkTreeView")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-class-child-property-names "GtkTreeView")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkTreeView" GTK-TREE-VIEW
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkScrollable")
                        :TYPE-INITIALIZER "gtk_tree_view_get_type")
                       ((ACTIVATE-ON-SINGLE-CLICK
                         GTK-TREE-VIEW-ACTIVATE-ON-SINGLE-CLICK
                         "activate-on-single-click" "gboolean" T T)
                        (ENABLE-GRID-LINES GTK-TREE-VIEW-ENABLE-GRID-LINES
                         "enable-grid-lines" "GtkTreeViewGridLines" T T)
                        (ENABLE-SEARCH GTK-TREE-VIEW-ENABLE-SEARCH
                         "enable-search" "gboolean" T T)
                        (ENABLE-TREE-LINES GTK-TREE-VIEW-ENABLE-TREE-LINES
                         "enable-tree-lines" "gboolean" T T)
                        (EXPANDER-COLUMN GTK-TREE-VIEW-EXPANDER-COLUMN
                         "expander-column" "GtkTreeViewColumn" T T)
                        (FIXED-HEIGHT-MODE GTK-TREE-VIEW-FIXED-HEIGHT-MODE
                         "fixed-height-mode" "gboolean" T T)
                        (HEADERS-CLICKABLE GTK-TREE-VIEW-HEADERS-CLICKABLE
                         "headers-clickable" "gboolean" T T)
                        (HEADERS-VISIBLE GTK-TREE-VIEW-HEADERS-VISIBLE
                         "headers-visible" "gboolean" T T)
                        (HOVER-EXPAND GTK-TREE-VIEW-HOVER-EXPAND "hover-expand"
                         "gboolean" T T)
                        (HOVER-SELECTION GTK-TREE-VIEW-HOVER-SELECTION
                         "hover-selection" "gboolean" T T)
                        (LEVEL-INDENTATION GTK-TREE-VIEW-LEVEL-INDENTATION
                         "level-indentation" "gint" T T)
                        (MODEL GTK-TREE-VIEW-MODEL "model" "GtkTreeModel" T T)
                        (REORDERABLE GTK-TREE-VIEW-REORDERABLE "reorderable"
                         "gboolean" T T)
                        (RUBBER-BANDING GTK-TREE-VIEW-RUBBER-BANDING
                         "rubber-banding" "gboolean" T T)
                        (RULES-HINT GTK-TREE-VIEW-RULES-HINT "rules-hint"
                         "gboolean" T T)
                        (SEARCH-COLUMN GTK-TREE-VIEW-SEARCH-COLUMN
                         "search-column" "gint" T T)
                        (SHOW-EXPANDERS GTK-TREE-VIEW-SHOW-EXPANDERS
                         "show-expanders" "gboolean" T T)
                        (TOOLTIP-COLUMN GTK-TREE-VIEW-TOOLTIP-COLUMN
                         "tooltip-column" "gint" T T)
                        #+linux
                        (UBUNTU-ALMOST-FIXED-HEIGHT-MODE
                         GTK-TREE-VIEW-UBUNTU-ALMOST-FIXED-HEIGHT-MODE
                         "ubuntu-almost-fixed-height-mode" "gboolean" NIL T)))
             (get-g-type-definition "GtkTreeView"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-tree-view-properties
  (let ((view (make-instance 'gtk-tree-view)))
    (is-false (gtk-tree-view-activate-on-single-click view))
    (is (eq :none (gtk-tree-view-enable-grid-lines view)))
    (is-true (gtk-tree-view-enable-search view))
    (is-false (gtk-tree-view-enable-tree-lines view))
    (is-false (gtk-tree-view-expander-column view))
    (is-false (gtk-tree-view-fixed-height-mode view))
    (is-true (gtk-tree-view-headers-clickable view))
    (is-true (gtk-tree-view-headers-visible view))
    (is-false (gtk-tree-view-hover-expand view))
    (is-false (gtk-tree-view-hover-selection view))
    (is (= 0 (gtk-tree-view-level-indentation view)))
    (is-false (gtk-tree-view-model view))
    (is-false (gtk-tree-view-reorderable view))
    (is-false (gtk-tree-view-rubber-banding view))
    (is-false (gtk-tree-view-rules-hint view))
    (is (= -1 (gtk-tree-view-search-column view)))
    (is-true (gtk-tree-view-show-expanders view))
    (is (= -1 (gtk-tree-view-tooltip-column view)))))

;;; --- Style Properties -------------------------------------------------------

;;;             gboolean    allow-rules                 Read
;;;             GdkColor*   even-row-color              Read
;;;                 gint    expander-size               Read
;;;                gchar*   grid-line-pattern           Read
;;;                 gint    grid-line-width             Read
;;;                 gint    horizontal-separator        Read
;;;             gboolean    indent-expanders            Read
;;;             GdkColor*   odd-row-color               Read
;;;                gchar*   tree-line-pattern           Read
;;;                 gint    tree-line-width             Read
;;;                 gint    vertical-separator          Read

;;; --- Signals ----------------------------------------------------------------

;;;                 void    columns-changed             Run Last
;;;                 void    cursor-changed              Run Last
;;;             gboolean    expand-collapse-cursor-row  Action
;;;             gboolean    move-cursor                 Action
;;;                 void    row-activated               Action
;;;                 void    row-collapsed               Run Last
;;;                 void    row-expanded                Run Last
;;;             gboolean    select-all                  Action
;;;             gboolean    select-cursor-parent        Action
;;;             gboolean    select-cursor-row           Action
;;;             gboolean    start-interactive-search    Action
;;;             gboolean    test-collapse-row           Run Last
;;;             gboolean    test-expand-row             Run Last
;;;             gboolean    toggle-cursor-row           Action
;;;             gboolean    unselect-all                Action


;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeViewColumnDropFunc
;;;     GtkTreeViewMappingFunc
;;;     GtkTreeViewSearchEqualFunc

;;;     gtk_tree_view_new

(test gtk-tree-view-new
  (is (typep (gtk-tree-view-new) 'gtk-tree-view)))

;;;     gtk_tree_view_new_with_model

(test gtk-tree-view-new-with-model
  (is (typep (gtk-tree-view-new-with-model nil) 'gtk-tree-view))
  (is (typep (gtk-tree-view-new-with-model (create-and-fill-list-store))
             'gtk-tree-view)))

;;;     gtk_tree_view_get_selection

(test gtk-tree-view-selection
  (is (typep (gtk-tree-view-selection (gtk-tree-view-new)) 'gtk-tree-selection))
  (is (typep (gtk-tree-view-selection
                 (gtk-tree-view-new-with-model
                     (create-and-fill-list-store)))
             'gtk-tree-selection)))

;;;     gtk_tree_view_columns_autosize

;;;     gtk_tree_view_append_column

(test gtk-tree-view-append-column
  (let ((view (gtk-tree-view-new)))
    (is (= 1
           (gtk-tree-view-append-column view
                                        (make-instance 'gtk-tree-view-column))))
    (is (= 2
           (gtk-tree-view-append-column view
                                        (make-instance 'gtk-tree-view-column))))))

;;;     gtk_tree_view_remove_column

(test gtk-tree-view-remove-column
  (let ((view (gtk-tree-view-new))
        (column1 (make-instance 'gtk-tree-view-column))
        (column2 (make-instance 'gtk-tree-view-column))
        (column3 (make-instance 'gtk-tree-view-column)))
    ;; Add columns to the tree view
    (is (= 1 (gtk-tree-view-append-column view column1)))
    (is (= 2 (gtk-tree-view-append-column view column2)))
    (is (= 3 (gtk-tree-view-append-column view column3)))
    ;; Remove columns from the tree view
    (is (= 2 (gtk-tree-view-remove-column view column1)))
    (is (= 1 (gtk-tree-view-remove-column view column3)))
    (is (= 0 (gtk-tree-view-remove-column view column2)))))

;;;     gtk_tree_view_insert_column

(test gtk-tree-view-insert-column
  (let ((view (gtk-tree-view-new)))
    (is (= 1
           (gtk-tree-view-insert-column view
                                        (make-instance 'gtk-tree-view-column)
                                        -1)))
    (is (= 2
          (gtk-tree-view-insert-column view
                                       (make-instance 'gtk-tree-view-column)
                                       0)))
    (is (= 3
           (gtk-tree-view-insert-column view
                                        (make-instance 'gtk-tree-view-column)
                                        0)))
    (is (= 4
           (gtk-tree-view-insert-column view
                                        (make-instance 'gtk-tree-view-column)
                                        1)))))

;;;     gtk_tree_view_insert_column_with_attributes
;;;     gtk_tree_view_insert_column_with_data_func

;;;     gtk_tree_view_get_n_columns

(test gtk-tree-view-n-columns
  (let ((view (gtk-tree-view-new)))
    (is (= 1
           (gtk-tree-view-append-column view
                                        (make-instance 'gtk-tree-view-column))))
    (is (= 1 (gtk-tree-view-n-columns view)))
    (is (= 2
           (gtk-tree-view-append-column view
                                        (make-instance 'gtk-tree-view-column))))
    (is (= 2 (gtk-tree-view-n-columns view)))))

;;;     gtk_tree_view_get_column

(test gtk-tree-view--column
  (let ((view (gtk-tree-view-new))
        (column1 (make-instance 'gtk-tree-view-column))
        (column2 (make-instance 'gtk-tree-view-column))
        (column3 (make-instance 'gtk-tree-view-column)))
    ;; Add columns to the tree view
    (is (= 1 (gtk-tree-view-append-column view column1)))
    (is (= 2 (gtk-tree-view-append-column view column2)))
    (is (= 3 (gtk-tree-view-append-column view column3)))
    ;; Get tree view columns
    (is (equal column1 (gtk-tree-view-column view 0)))
    (is (equal column2 (gtk-tree-view-column view 1)))
    (is (equal column3 (gtk-tree-view-column view 2)))
    (is-false (gtk-tree-view-column view 3))))

;;;     gtk_tree_view_get_columns

(test gtk-tree-view--column
  (let ((view (gtk-tree-view-new))
        (column1 (make-instance 'gtk-tree-view-column))
        (column2 (make-instance 'gtk-tree-view-column))
        (column3 (make-instance 'gtk-tree-view-column)))
    ;; Add columns to the tree view
    (is (= 1 (gtk-tree-view-append-column view column1)))
    (is (= 2 (gtk-tree-view-append-column view column2)))
    (is (= 3 (gtk-tree-view-append-column view column3)))
    ;; Get tree view columns
    (is (typep (first (gtk-tree-view-columns view)) 'gtk-tree-view-column))))

;;;     gtk_tree_view_move_column_after

(test gtk-tree-view-move-column-after
  (let ((view (gtk-tree-view-new))
        (column1 (make-instance 'gtk-tree-view-column))
        (column2 (make-instance 'gtk-tree-view-column))
        (column3 (make-instance 'gtk-tree-view-column)))
    ;; Add columns to the tree view
    (is (= 1 (gtk-tree-view-append-column view column1)))
    (is (= 2 (gtk-tree-view-append-column view column2)))
    (is (= 3 (gtk-tree-view-append-column view column3)))
    ;; Get tree view columns
    (is-false (gtk-tree-view-move-column-after view column1 column3))))

;;;     gtk_tree_view_set_column_drag_function
;;;     gtk_tree_view_scroll_to_point
;;;     gtk_tree_view_scroll_to_cell
;;;     gtk_tree_view_set_cursor
;;;     gtk_tree_view_set_cursor_on_cell
;;;     gtk_tree_view_get_cursor
;;;     gtk_tree_view_row_activated
;;;     gtk_tree_view_expand_all
;;;     gtk_tree_view_collapse_all
;;;     gtk_tree_view_expand_to_path
;;;     gtk_tree_view_expand_row
;;;     gtk_tree_view_collapse_row
;;;     gtk_tree_view_map_expanded_rows
;;;     gtk_tree_view_row_expanded
;;;     gtk_tree_view_get_path_at_pos
;;;     gtk_tree_view_is_blank_at_pos
;;;     gtk_tree_view_get_cell_area
;;;     gtk_tree_view_get_background_area
;;;     gtk_tree_view_get_visible_rect
;;;     gtk_tree_view_get_visible_range
;;;     gtk_tree_view_get_bin_window
;;;     gtk_tree_view_convert_bin_window_to_tree_coords
;;;     gtk_tree_view_convert_bin_window_to_widget_coords
;;;     gtk_tree_view_convert_tree_to_bin_window_coords
;;;     gtk_tree_view_convert_tree_to_widget_coords
;;;     gtk_tree_view_convert_widget_to_bin_window_coords
;;;     gtk_tree_view_convert_widget_to_tree_coords

;;;     gtk_tree_view_enable_model_drag_dest

(test gtk-tree-view-enable-model-drag-dest
  (let ((targets '(("text/html" :none 0)
                   ("STRING" :none 1)
                   ("number" :none 2)
                   ("image/jpeg" :none 3)
                   ("text/uri-list" :none 4)))
        (view (make-instance 'gtk-tree-view)))
    (is-false (gtk-tree-view-enable-model-drag-dest view targets :copy))))

;;;     gtk_tree_view_enable_model_drag_source

(test gtk-tree-view-enable-model-drag-source
  (let ((targets '(("text/html" :none 0)
                   ("STRING" :none 1)
                   ("number" :none 2)
                   ("image/jpeg" :none 3)
                   ("text/uri-list" :none 4)))
        (view (make-instance 'gtk-tree-view)))
    (is-false (gtk-tree-view-enable-model-drag-source view
                                                      :button1-mask
                                                      targets
                                                      :copy))))

;;;     gtk_tree_view_unset_rows_drag_source
;;;     gtk_tree_view_unset_rows_drag_dest
;;;     gtk_tree_view_set_drag_dest_row
;;;     gtk_tree_view_get_drag_dest_row
;;;     gtk_tree_view_get_dest_row_at_pos
;;;     gtk_tree_view_create_row_drag_icon
;;;
;;;     GtkTreeViewSearchPositionFunc
;;;
;;;     gtk_tree_view_get_search_equal_func
;;;     gtk_tree_view_set_search_equal_func
;;;     gtk_tree_view_get_search_entry
;;;     gtk_tree_view_set_search_entry
;;;     gtk_tree_view_get_search_position_func
;;;     gtk_tree_view_set_search_position_func
;;;
;;;     GtkTreeDestroyCountFunc
;;;
;;;     gtk_tree_view_set_destroy_count_func
;;;
;;;     GtkTreeViewRowSeparatorFunc
;;;
;;;     gtk_tree_view_get_row_separator_func
;;;     gtk_tree_view_set_row_separator_func
;;;     gtk_tree_view_is_rubber_banding_active
;;;
;;;     gtk_tree_view_get_grid_lines
;;;     gtk_tree_view_set_grid_lines
;;;     gtk_tree_view_set_tooltip_row
;;;     gtk_tree_view_set_tooltip_cell
;;;     gtk_tree_view_get_tooltip_context

;;; 2021-10-20
