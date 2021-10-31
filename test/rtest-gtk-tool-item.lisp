(def-suite gtk-tool-item :in gtk-suite)
(in-suite gtk-tool-item)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToolItem

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tool_item_new
;;;     gtk_tool_item_set_homogeneous
;;;     gtk_tool_item_get_homogeneous
;;;     gtk_tool_item_set_expand
;;;     gtk_tool_item_get_expand
;;;     gtk_tool_item_set_tooltip_text
;;;     gtk_tool_item_set_tooltip_markup
;;;     gtk_tool_item_set_use_drag_window
;;;     gtk_tool_item_get_use_drag_window
;;;     gtk_tool_item_set_visible_horizontal
;;;     gtk_tool_item_get_visible_horizontal
;;;     gtk_tool_item_set_visible_vertical
;;;     gtk_tool_item_get_visible_vertical
;;;     gtk_tool_item_set_is_important
;;;     gtk_tool_item_get_is_important
;;;     gtk_tool_item_get_ellipsize_mode
;;;     gtk_tool_item_get_icon_size
;;;     gtk_tool_item_get_orientation
;;;     gtk_tool_item_get_toolbar_style
;;;     gtk_tool_item_get_relief_style
;;;     gtk_tool_item_get_text_alignment
;;;     gtk_tool_item_get_text_orientation
;;;     gtk_tool_item_retrieve_proxy_menu_item

;;;     gtk_tool_item_get_proxy_menu_item
;;;     gtk_tool_item_set_proxy_menu_item

(test gtk-tool-item-proxy-menu-item
  (let ((toolitem (make-instance 'gtk-tool-item))
        (menuitem (make-instance 'gtk-menu-item
                                 :label "label")))
    ;; Set a menu item
    (is (eq menuitem
            (setf (gtk-tool-item-proxy-menu-item toolitem "label") menuitem)))
    (is (eq menuitem
            (gtk-tool-item-proxy-menu-item toolitem "label")))
    ;; Set a menu item with value NIL
    (is-false (setf (gtk-tool-item-proxy-menu-item toolitem "label") nil))
    (is-false (gtk-tool-item-proxy-menu-item toolitem "label"))))

;;;     gtk_tool_item_rebuild_menu
;;;     gtk_tool_item_toolbar_reconfigured
;;;     gtk_tool_item_get_text_size_group

;;; 2021-10-31
