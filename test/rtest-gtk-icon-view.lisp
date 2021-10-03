(def-suite gtk-icon-view :in gtk-suite)
(in-suite gtk-icon-view)

;;; Types and Values

;;;     GtkIconView
;;;     GtkIconViewDropPosition

;;; Properties

;;;         gboolean   activate-on-single-click    Read / Write
;;;      GtkCellArea*  cell-area                   Read / Write / Construct Only
;;;             gint   column-spacing              Read / Write
;;;             gint   columns                     Read / Write
;;;   GtkOrientation   item-orientation            Read / Write
;;;             gint   item-padding                Read / Write
;;;             gint   item-width                  Read / Write
;;;             gint   margin                      Read / Write
;;;             gint   markup-column               Read / Write
;;;     GtkTreeModel*  model                       Read / Write
;;;             gint   pixbuf-column               Read / Write
;;;         gboolean   reorderable                 Read / Write
;;;             gint   row-spacing                 Read / Write
;;; GtkSelectionMode   selection-mode              Read / Write
;;;             gint   spacing                     Read / Write
;;;             gint   text-column                 Read / Write
;;;             gint   tooltip-column              Read / Write

;;; Style Properties

;;;           guchar   selection-box-alpha         Read
;;;         GdkColor*  selection-box-color         Read

;;; Signals

;;;         gboolean   activate-cursor-item        Action
;;;             void   item-activated              Run Last
;;;         gboolean   move-cursor                 Action
;;;             void   select-all                  Action
;;;             void   select-cursor-item          Action
;;;             void   selection-changed           Run First
;;;             void   toggle-cursor-item          Action
;;;             void   unselect-all                Action


;;; Functions

;;;     GtkIconViewForeachFunc

;;;     gtk_icon_view_new
;;;     gtk_icon_view_new_with_area
;;;     gtk_icon_view_new_with_model
;;;     gtk_icon_view_set_model                            Accessor
;;;     gtk_icon_view_get_model                            Accessor
;;;     gtk_icon_view_set_text_column                      Accessor
;;;     gtk_icon_view_get_text_column                      Accessor
;;;     gtk_icon_view_set_markup_column                    Accessor
;;;     gtk_icon_view_get_markup_column                    Accessor
;;;     gtk_icon_view_set_pixbuf_column                    Accessor
;;;     gtk_icon_view_get_pixbuf_column                    Accessor
;;;     gtk_icon_view_get_path_at_pos
;;;     gtk_icon_view_get_item_at_pos
;;;     gtk_icon_view_convert_widget_to_bin_window_coords
;;;     gtk_icon_view_set_cursor
;;;     gtk_icon_view_get_cursor
;;;     gtk_icon_view_selected_foreach
;;;     gtk_icon_view_set_selection_mode                   Accessor
;;;     gtk_icon_view_get_selection_mode                   Accessor
;;;     gtk_icon_view_set_item_orientation                 Accessor
;;;     gtk_icon_view_get_item_orientation                 Accessor
;;;     gtk_icon_view_set_columns                          Accessor
;;;     gtk_icon_view_get_columns                          Accessor
;;;     gtk_icon_view_set_item_width                       Accessor
;;;     gtk_icon_view_get_item_width                       Accessor
;;;     gtk_icon_view_set_spacing                          Accessor
;;;     gtk_icon_view_get_spacing                          Accessor
;;;     gtk_icon_view_set_row_spacing                      Accessor
;;;     gtk_icon_view_get_row_spacing                      Accessor
;;;     gtk_icon_view_set_column_spacing                   Accessor
;;;     gtk_icon_view_get_column_spacing                   Accessor
;;;     gtk_icon_view_set_margin                           Accessor
;;;     gtk_icon_view_get_margin                           Accessor
;;;     gtk_icon_view_set_item_padding                     Accessor
;;;     gtk_icon_view_get_item_padding                     Accessor
;;;     gtk_icon_view_set_activate_on_single_click         Accessor
;;;     gtk_icon_view_get_activate_on_single_click         Accessor
;;;     gtk_icon_view_get_cell_rect
;;;     gtk_icon_view_select_path
;;;     gtk_icon_view_unselect_path
;;;     gtk_icon_view_path_is_selected
;;;     gtk_icon_view_get_selected_items
;;;     gtk_icon_view_select_all
;;;     gtk_icon_view_unselect_all
;;;     gtk_icon_view_item_activated
;;;     gtk_icon_view_scroll_to_path
;;;     gtk_icon_view_get_visible_range
;;;     gtk_icon_view_set_tooltip_item
;;;     gtk_icon_view_set_tooltip_cell
;;;     gtk_icon_view_get_tooltip_context
;;;     gtk_icon_view_set_tooltip_column                   Accessor
;;;     gtk_icon_view_get_tooltip_column                   Accessor
;;;     gtk_icon_view_get_item_row
;;;     gtk_icon_view_get_item_column

;;;     gtk_icon_view_enable_model_drag_source

(test gtk-icon-view-enable-model-drag-source
  (let ((targets '(("text/html" :none 0)
                   ("STRING" :none 1)
                   ("number" :none 2)
                   ("image/jpeg" :none 3)
                   ("text/uri-list" :none 4)))
        (view (make-instance 'gtk-icon-view)))
    (is-false (gtk-icon-view-enable-model-drag-source view
                                                      :button1-mask
                                                      targets
                                                      :copy))))

;;;     gtk_icon_view_enable_model_drag_dest

(test gtk-icon-view-enable-model-drag-dest
  (let ((targets '(("text/html" :none 0)
                   ("STRING" :none 1)
                   ("number" :none 2)
                   ("image/jpeg" :none 3)
                   ("text/uri-list" :none 4)))
        (view (make-instance 'gtk-icon-view)))
    (is-false (gtk-icon-view-enable-model-drag-dest view targets :copy))))

;;;     gtk_icon_view_unset_model_drag_source
;;;     gtk_icon_view_unset_model_drag_dest
;;;     gtk_icon_view_set_reorderable                      Accessor
;;;     gtk_icon_view_get_reorderable                      Accessor
;;;     gtk_icon_view_set_drag_dest_item
;;;     gtk_icon_view_get_drag_dest_item
;;;     gtk_icon_view_get_dest_item_at_pos
;;;     gtk_icon_view_create_drag_icon

;;; 2021-10-2
