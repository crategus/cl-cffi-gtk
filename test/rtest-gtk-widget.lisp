
(def-suite gtk-widget :in gtk-suite)
(in-suite gtk-widget)

;;;     GtkRequisition
;;;     GtkAllocation
;;;     GtkWidget
;;;     GtkWidgetClass
;;;     GtkSelectionData
;;;     GtkWidgetAuxInfo
;;;     GtkWidgetHelpType
;;;
;;;     gtk_widget_new
;;;     gtk_widget_destroy
;;;     gtk_widget_in_destruction
;;;     gtk_widget_destroyed
;;;     gtk_widget_unparent
;;;     gtk_widget_show
;;;     gtk_widget_show_now
;;;     gtk_widget_hide
;;;     gtk_widget_show_all
;;;     gtk_widget_map
;;;     gtk_widget_unmap
;;;     gtk_widget_realize
;;;     gtk_widget_unrealize
;;;     gtk_widget_draw
;;;     gtk_widget_queue_draw
;;;     gtk_widget_queue_resize
;;;     gtk_widget_queue_resize_no_redraw
;;;     gtk_widget_size_request
;;;     gtk_widget_get_child_requisition
;;;     gtk_widget_size_allocate
;;;     gtk_widget_add_accelerator
;;;     gtk_widget_remove_accelerator
;;;     gtk_widget_set_accel_path
;;;     gtk_widget_list_accel_closures
;;;     gtk_widget_can_activate_accel
;;;     gtk_widget_event
;;;     gtk_widget_activate
;;;     gtk_widget_reparent
;;;     gtk_widget_intersect
;;;     gtk_widget_is_focus
;;;     gtk_widget_grab_focus
;;;     gtk_widget_grab_default
;;;     gtk_widget_set_name
;;;     gtk_widget_get_name
;;;     gtk_widget_set_state
;;;     gtk_widget_set_sensitive
;;;     gtk_widget_set_parent
;;;     gtk_widget_set_parent_window
;;;     gtk_widget_get_parent_window

;;;   gtk_widget_set_events
;;;   gtk_widget_get_events
;;;   gtk_widget_add_events

(test gtk-widget-events
  (let ((eventbox (make-instance 'gtk-event-box)))
    (is (equal '() (gtk-widget-get-events eventbox)))
    (gtk-widget-set-events eventbox '(:button-press-mask))
    (is (equal '(:button-press-mask) (gtk-widget-get-events eventbox)))
    (gtk-widget-add-events eventbox
                           '(:pointer-motion-mask :button-release-mask))
    (is (equal '(:BUTTON-PRESS-MASK :BUTTON-RELEASE-MASK :POINTER-MOTION-MASK)
               (stable-sort (gtk-widget-get-events eventbox)
                            #'string< :key #'symbol-name)))))

;;;     gtk_widget_set_device_events
;;;     gtk_widget_get_device_events
;;;     gtk_widget_add_device_events
;;;     gtk_widget_set_device_enabled
;;;     gtk_widget_get_device_enabled
;;;     gtk_widget_get_toplevel
;;;     gtk_widget_get_ancestor
;;;     gtk_widget_get_visual
;;;     gtk_widget_set_visual
;;;     gtk_widget_get_pointer
;;;     gtk_widget_is_ancestor
;;;     gtk_widget_translate_coordinates
;;;     gtk_widget_hide_on_delete
;;;     gtk_widget_set_style
;;;     gtk_widget_ensure_style
;;;     gtk_widget_get_style
;;;     gtk_widget_reset_rc_styles
;;;     gtk_widget_get_default_style
;;;     gtk_widget_set_direction
;;;
;;;     GtkTextDirection
;;;
;;;     gtk_widget_get_direction
;;;     gtk_widget_set_default_direction
;;;     gtk_widget_get_default_direction
;;;     gtk_widget_shape_combine_region
;;;     gtk_widget_input_shape_combine_region
;;;     gtk_widget_path
;;;     gtk_widget_class_path
;;;     gtk_widget_get_composite_name
;;;     gtk_widget_override_background_color
;;;     gtk_widget_override_color
;;;     gtk_widget_override_font
;;;     gtk_widget_override_symbolic_color
;;;     gtk_widget_override_cursor
;;;     gtk_widget_modify_style
;;;     gtk_widget_get_modifier_style
;;;     gtk_widget_modify_fg
;;;     gtk_widget_modify_bg
;;;     gtk_widget_modify_text
;;;     gtk_widget_modify_base
;;;     gtk_widget_modify_font
;;;     gtk_widget_modify_cursor
;;;     gtk_widget_create_pango_context
;;;     gtk_widget_get_pango_context
;;;     gtk_widget_create_pango_layout
;;;     gtk_widget_render_icon
;;;     gtk_widget_render_icon_pixbuf
;;;     gtk_widget_pop_composite_child
;;;     gtk_widget_push_composite_child
;;;     gtk_widget_queue_draw_area
;;;     gtk_widget_queue_draw_region
;;;     gtk_widget_set_app_paintable
;;;     gtk_widget_set_double_buffered
;;;     gtk_widget_set_redraw_on_allocate
;;;     gtk_widget_set_composite_name
;;;     gtk_widget_mnemonic_activate

;;;     gtk_widget_class_install_style_property
;;;     gtk_widget_class_install_style_property_parser

;;;   gtk_widget_class_find_style_property

(test gtk-widget-class-find-style-property.1
  (is (equal "cursor-aspect-ratio"
             (param-spec-name
               (gtk-widget-class-find-style-property "GtkFrame"
                                                     "cursor-aspect-ratio")))))

(test gtk-widget-class-find-style-property.2
  (is (equal "cursor-color"
             (param-spec-name
               (gtk-widget-class-find-style-property "GtkFrame"
                                                     "cursor-color")))))

(test gtk-widget-class-find-style-property.3
  (is (equal "focus-line-pattern"
             (param-spec-name
               (gtk-widget-class-find-style-property "GtkFrame"
                                                     "focus-line-pattern")))))

;;;   gtk_widget_class_list_style_properties

(test gtk-widget-class-list-style-properties
  ;; Get the names of the style properties of GtkFrame.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength"
               "secondary-cursor-color" "separator-height" "separator-width"
               "text-handle-height" "text-handle-width" "visited-link-color"
               "wide-separators" "window-dragging")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkFrame")))))

;;;     gtk_widget_region_intersect
;;;     gtk_widget_send_expose
;;;     gtk_widget_send_focus_change
;;;     gtk_widget_style_get

;;;   gtk_widget_style_get_property

(test gtk-widget-style-get-property
  (let ((widget (make-instance 'gtk-frame)))
    (is (= 0.04 (gtk-widget-style-get-property widget "cursor-aspect-ratio")))
    (is-false (gtk-widget-style-get-property widget "cursor-color"))
    (is (equal ""
               (gtk-widget-style-get-property widget "focus-line-pattern")))
    (is (= 1 (gtk-widget-style-get-property widget "focus-line-width")))
    (is (= 0 (gtk-widget-style-get-property widget "focus-padding")))
    (is-true (gtk-widget-style-get-property widget "interior-focus"))
    (is (eq 'gdk-color
            (type-of (gtk-widget-style-get-property widget "link-color"))))
    (is (= 16 (gtk-widget-style-get-property widget "scroll-arrow-hlength")))
    (is (= 16 (gtk-widget-style-get-property widget "scroll-arrow-vlength")))
    (is-false (gtk-widget-style-get-property widget "secondary-cursor-color"))
    (is (=  2 (gtk-widget-style-get-property widget "separator-height")))
    (is (=  2 (gtk-widget-style-get-property widget "separator-width")))
    (is (= 20 (gtk-widget-style-get-property widget "text-handle-height")))
    (is (= 16 (gtk-widget-style-get-property widget "text-handle-width")))
    (is (eq 'gdk-color
            (type-of (gtk-widget-style-get-property widget
                                                    "visited-link-color"))))
    (is-false  (gtk-widget-style-get-property widget "wide-separators"))
    (is-false (gtk-widget-style-get-property widget "window-dragging"))))

;;;     gtk_widget_style_get_valist
;;;     gtk_widget_style_attach
;;;     gtk_widget_class_set_accessible_type
;;;     gtk_widget_class_set_accessible_role
;;;     gtk_widget_get_accessible
;;;     gtk_widget_child_focus
;;;     gtk_widget_child_notify
;;;     gtk_widget_freeze_child_notify
;;;     gtk_widget_get_child_visible
;;;     gtk_widget_get_parent
;;;     gtk_widget_get_settings
;;;     gtk_widget_get_clipboard
;;;     gtk_widget_get_display
;;;     gtk_widget_get_root_window
;;;     gtk_widget_get_screen
;;;     gtk_widget_has_screen
;;;     gtk_widget_get_size_request
;;;     gtk_widget_set_child_visible
;;;     gtk_widget_set_size_request
;;;     gtk_widget_thaw_child_notify
;;;     gtk_widget_set_no_show_all
;;;     gtk_widget_get_no_show_all
;;;     gtk_widget_list_mnemonic_labels
;;;     gtk_widget_add_mnemonic_label
;;;     gtk_widget_remove_mnemonic_label
;;;     gtk_widget_is_composited
;;;     gtk_widget_error_bell
;;;     gtk_widget_keynav_failed
;;;     gtk_widget_get_tooltip_markup
;;;     gtk_widget_set_tooltip_markup
;;;     gtk_widget_get_tooltip_text
;;;     gtk_widget_set_tooltip_text
;;;     gtk_widget_get_tooltip_window
;;;     gtk_widget_set_tooltip_window
;;;     gtk_widget_get_has_tooltip
;;;     gtk_widget_set_has_tooltip
;;;     gtk_widget_trigger_tooltip_query
;;;     gtk_widget_get_window
;;;     gtk_cairo_should_draw_window
;;;     gtk_cairo_transform_to_window
;;;     gtk_widget_get_allocated_width
;;;     gtk_widget_get_allocated_height
;;;     gtk_widget_get_allocation
;;;     gtk_widget_set_allocation
;;;     gtk_widget_get_app_paintable
;;;     gtk_widget_get_can_default
;;;     gtk_widget_set_can_default
;;;     gtk_widget_get_can_focus
;;;     gtk_widget_set_can_focus
;;;     gtk_widget_get_double_buffered
;;;     gtk_widget_get_has_window
;;;     gtk_widget_set_has_window
;;;     gtk_widget_get_sensitive
;;;     gtk_widget_is_sensitive
;;;     gtk_widget_get_state
;;;     gtk_widget_get_visible
;;;     gtk_widget_set_visible
;;;     gtk_widget_set_state_flags
;;;     gtk_widget_unset_state_flags
;;;     gtk_widget_get_state_flags
;;;     gtk_widget_has_default
;;;     gtk_widget_has_focus
;;;     gtk_widget_has_visible_focus
;;;     gtk_widget_has_grab
;;;     gtk_widget_has_rc_style
;;;     gtk_widget_is_drawable
;;;     gtk_widget_is_toplevel
;;;     gtk_widget_set_window
;;;     gtk_widget_set_receives_default
;;;     gtk_widget_get_receives_default
;;;     gtk_widget_set_support_multidevice
;;;     gtk_widget_get_support_multidevice
;;;     gtk_widget_set_realized
;;;     gtk_widget_get_realized
;;;     gtk_widget_set_mapped
;;;     gtk_widget_get_mapped
;;;     gtk_widget_get_requisition
;;;     gtk_widget_device_is_shadowed
;;;     gtk_widget_get_modifier_mask
;;;
;;;     gtk_widget_get_path
;;;     gtk_widget_get_style_context
;;;     gtk_widget_reset_style
;;;
;;;     gtk_requisition_new
;;;     gtk_requisition_copy
;;;     gtk_requisition_free
;;;
;;;     GtkSizeRequestMode
;;;     GtkRequestedSize
;;;
;;;     gtk_widget_get_preferred_height
;;;     gtk_widget_get_preferred_width
;;;     gtk_widget_get_preferred_height_for_width
;;;     gtk_widget_get_preferred_width_for_height
;;;     gtk_widget_get_request_mode
;;;     gtk_widget_get_preferred_size
;;;     gtk_distribute_natural_allocation
;;;
;;;     GtkAlign
;;;
;;;     gtk_widget_get_halign
;;;     gtk_widget_set_halign
;;;     gtk_widget_get_valign
;;;     gtk_widget_set_valign
;;;     gtk_widget_get_margin_left
;;;     gtk_widget_set_margin_left
;;;     gtk_widget_get_margin_right
;;;     gtk_widget_set_margin_right
;;;     gtk_widget_get_margin_top
;;;     gtk_widget_set_margin_top
;;;     gtk_widget_get_margin_bottom
;;;     gtk_widget_set_margin_bottom
;;;
;;;     gtk_widget_get_hexpand
;;;     gtk_widget_set_hexpand
;;;     gtk_widget_get_hexpand_set
;;;     gtk_widget_set_hexpand_set
;;;     gtk_widget_get_vexpand
;;;     gtk_widget_set_vexpand
;;;     gtk_widget_get_vexpand_set
;;;     gtk_widget_set_vexpand_set
;;;     gtk_widget_queue_compute_expand
;;;     gtk_widget_compute_expand
