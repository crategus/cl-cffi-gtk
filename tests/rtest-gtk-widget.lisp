;;; ----------------------------------------------------------------------------
;;; rtest-widget.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(in-package :gtk-tests)

(define-test gtk-widget
  (assert-true  (g-type-is-abstract "GtkWidget"))  
  (assert-true  (g-type-is-derived "GtkWidget"))  
  (assert-false (g-type-is-fundamental "GtkWidget"))  
  (assert-true  (g-type-is-value-type "GtkWidget"))
  (assert-true  (g-type-has-value-table "GtkWidget"))
  (assert-true  (g-type-is-classed "GtkWidget"))  
  (assert-true  (g-type-is-instantiatable "GtkWidget"))
  (assert-true  (g-type-is-derivable "GtkWidget"))
  (assert-true  (g-type-is-deep-derivable "GtkWidget"))
  (assert-false (g-type-is-interface "GtkWidget"))
  
  (let ((class (g-type-class-ref (gtype "GtkWidget"))))
    (assert-equal (gtype "GtkWidget")  (g-type-from-class class))
    (assert-equal (gtype "GtkWidget")
                  (g-type-from-class (g-type-class-peek "GtkWidget")))
    (assert-equal (gtype "GtkWidget")
                  (g-type-from-class (g-type-class-peek-static "GtkWidget")))
    (g-type-class-unref class))
  
  (let ((class (find-class 'gtk-widget)))
    ;; Check the class name and type of the class
    (assert-eq 'gtk-widget (class-name class))
    (assert-eq 'gobject-class (type-of class))
    (assert-eq (find-class 'gobject-class) (class-of class))
    ;; Properties of the metaclass gobject-class
    (assert-equal "GtkWidget" (gobject-class-g-type-name class))
    (assert-equal "GtkWidget" (gobject-class-direct-g-type-name class))
    (assert-equal "gtk_widget_get_type"
                  (gobject-class-g-type-initializer class))
    (assert-false (gobject-class-interface-p class)))
  
  (assert-equal (gtype "GInitiallyUnowned") (g-type-parent "GtkWidget"))
  (assert-eql 3 (g-type-depth "GtkWidget"))
  (assert-eql   (gtype "GInitiallyUnowned")
                (g-type-next-base "GtkWidget" "GObject"))
  (assert-true  (g-type-is-a "GtkWidget" "GInitiallyUnowned"))
  (assert-false (g-type-is-a "GtkWidget" "gboolean"))
  (assert-false (g-type-is-a "GtkWidget" "GtkWindow"))
  (assert-equal '("GtkMisc" "GtkContainer" "GtkRange" "GtkSeparator"
                  "GtkInvisible" "GtkProgressBar" "GtkCellView" "GtkEntry"
                  "GtkHSV" "GtkDrawingArea" "GtkCalendar")
                (mapcar #'gtype-name (g-type-children "GtkWidget")))
  (assert-equal '("AtkImplementorIface" "GtkBuildable")
                (mapcar #'gtype-name (g-type-interfaces "GtkWidget")))
  
  ;; Query infos about the class "GtkTable"
  (with-foreign-object (query 'g-type-query)
    (g-type-query "GtkWidget" query)
    (assert-equal (gtype "GtkWidget")
                  (foreign-slot-value query 'g-type-query :type))
    (assert-equal "GtkWidget"
                  (foreign-slot-value query 'g-type-query :type-name))
    (assert-eql 412 (foreign-slot-value query 'g-type-query :class-size))
    (assert-eql  16 (foreign-slot-value query 'g-type-query :instance-size)))
    
  ;; Get the names of the class properties.
  (assert-equal
      '("name" "parent" "width-request" "height-request" "visible" "sensitive"
         "app-paintable" "can-focus" "has-focus" "is-focus" "can-default"
         "has-default" "receives-default" "composite-child" "style" "events"
         "no-show-all" "has-tooltip" "tooltip-markup" "tooltip-text" "window"
         "double-buffered" "halign" "valign" "margin-left" "margin-right"
         "margin-top" "margin-bottom" "margin" "hexpand" "vexpand"
         "hexpand-set" "vexpand-set" "expand")
     (mapcar #'param-spec-name
             (g-object-class-list-properties (gtype "GtkWidget"))))
    
  ;; Get the names of the style properties.
  (assert-equal
      '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
         "focus-line-width" "focus-padding" "interior-focus" "link-color"
         "scroll-arrow-hlength" "scroll-arrow-vlength" "secondary-cursor-color"
         "separator-height" "separator-width" "visited-link-color"
         "wide-separators" "window-dragging")
      (mapcar #'param-spec-name
                (gtk-widget-class-list-style-properties (gtype "GtkWidget"))))
  
  ;; Because GtkWidget is abstract, we create a GtkSeparator
  (let* ((widget (gtk-separator-new :horizontal))
         (ptr (pointer widget)))
    ;; Access the properties
    (assert-false    (gtk-widget-app-paintable widget))
    (assert-false    (gtk-widget-can-default widget))
    (assert-false    (gtk-widget-can-focus widget))
;    (assert-false    (gtk-widget-composite-child widget)) ; This makes problems
    (assert-true     (gtk-widget-double-buffered widget))
    (assert-false    (gtk-widget-events widget))
    (assert-false    (gtk-widget-expand widget))
    (assert-eq       :gtk-align-fill
                     (gtk-widget-halign widget))
    (assert-false    (gtk-widget-has-default widget))
    (assert-false    (gtk-widget-has-focus widget))
    (assert-false    (gtk-widget-has-tooltip widget))
    (assert-eql -1   (gtk-widget-height-request widget))
    (assert-false    (gtk-widget-hexpand widget))
    (assert-false    (gtk-widget-hexpand-set widget))
    (assert-false    (gtk-widget-is-focus widget))
    (assert-eq 0     (gtk-widget-margin widget))
    (assert-eq 0     (gtk-widget-margin-bottom widget))
    (assert-eq 0     (gtk-widget-margin-left widget))
    (assert-eq 0     (gtk-widget-margin-right widget))
    (assert-eq 0     (gtk-widget-margin-top widget))
    (assert-equal "" (gtk-widget-name widget))
    (assert-false    (gtk-widget-no-show-all widget))
    (assert-false    (gtk-widget-parent widget))
    (assert-false    (gtk-widget-receives-default widget))
    (assert-true     (gtk-widget-sensitive widget))
    (assert-equal    (gtype "GtkStyle")
                     (g-type-from-instance (pointer (gtk-widget-style widget))))
    (assert-false    (gtk-widget-tooltip-markup widget))
    (assert-false    (gtk-widget-tooltip-text widget))
    (assert-eq       :gtk-align-fill
                     (gtk-widget-get-valign widget))
    (assert-false    (gtk-widget-vexpand widget))
    (assert-false    (gtk-widget-vexpand-set widget))
    (assert-false    (gtk-widget-visible widget))
    (assert-eql -1   (gtk-widget-width-request widget))
    (assert-false    (gtk-widget-window widget))
    ;; Access cffi properties
    (assert-false    (gtk-widget-parent-window widget))
    (assert-equal    (gtype "GtkSeparator")
                     (g-type-from-instance (pointer (gtk-widget-toplevel widget))))
    (assert-equal    (gtype "GdkX11Visual")
                     (g-type-from-instance (pointer (gtk-widget-visual widget))))
    (assert-equal    (gtype "GtkRcStyle")
                     (g-type-from-instance (pointer (gtk-widget-modifier-style widget))))
    (assert-equal    (gtype "PangoContext")
                     (g-type-from-instance (pointer (gtk-widget-pango-context widget))))
    (assert-true     (gtk-widget-child-visible widget))
    (assert-eq :ltr  (gtk-widget-direction widget))
    (assert-false    (gtk-widget-composite-name widget))
    (assert-error    'error
                     (gtk-widget-redraw-on-allocate widget)) ; only writeable
    (assert-eql      (gtype "GtkWidgetAccessible")
                     (g-type-from-instance (pointer (gtk-widget-accessible widget))))
    (assert-false    (gtk-widget-tooltip-window widget))
    
    ;; Get the values of style properties
    (assert-eql 0.04 (gtk-widget-style-get-property ptr "cursor-aspect-ratio"))
    (assert-false (gtk-widget-style-get-property ptr "cursor-color"))
    (assert-equal "" (gtk-widget-style-get-property ptr "focus-line-pattern"))
    (assert-eql 1 (gtk-widget-style-get-property ptr "focus-line-width"))
    (assert-eql 0 (gtk-widget-style-get-property ptr "focus-padding"))
    (assert-true (gtk-widget-style-get-property ptr "interior-focus"))
    (assert-eq 'gdk-color (type-of (gtk-widget-style-get-property ptr "link-color")))
    (assert-eql 16 (gtk-widget-style-get-property ptr "scroll-arrow-hlength"))
    (assert-eql 16 (gtk-widget-style-get-property ptr "scroll-arrow-vlength"))
    (assert-false (gtk-widget-style-get-property ptr "secondary-cursor-color"))
    (assert-eql 2 (gtk-widget-style-get-property ptr "separator-height"))
    (assert-eql 2 (gtk-widget-style-get-property ptr "separator-width"))
    (assert-eq 'gdk-color (type-of (gtk-widget-style-get-property ptr "visited-link-color")))
    (assert-true  (gtk-widget-style-get-property ptr "wide-separators"))
    (assert-false (gtk-widget-style-get-property ptr "window-dragging"))
    
    ;; Call functions
    ;; gtk_widget_new                            not implemented
    (assert-false (gtk-widget-destroy (gtk-label-new "Text")))
    (assert-false (gtk-widget-in-destruction widget))
    ;; gtk_widget_destroyed                      not implemented
    (assert-false (gtk-widget-unparent widget))
    (assert-false (gtk-widget-show widget))
    (assert-false (gtk-widget-show-now widget))
    (assert-false (gtk-widget-hide widget))
    (assert-false (gtk-widget-show-all widget))
    ;; gtk-widget-map
    ;; gtk-widget-unmap
    ;; gtk-widget-realize
    ;; gtk-widget-unrealize
    ;; gtk_widget_draw                           not implemented
    (assert-false (gtk-widget-queue-draw widget))
    (assert-false (gtk-widget-queue-resize widget))
    (assert-false (gtk-widget-queue-resize-no-redraw widget))
    (assert-equal 'gtk-requisition
                  (type-of (gtk-widget-size-request widget)))
    ;; gtk_widget_get_child_requisition          not implemented and deprecated
    ;; gtk_widget_size_allocate                  not implemented
    ;; gtk-widget-add-accelerator
    ;; gtk_widget_remove_accelerator
    ;; gtk_widget_set_accel_path
    ;; gtk_widget_list_accel_closures            not implemented
    ;; gtk_widget_can_activate_accel
    ;; gtk_widget_event
    (assert-false (gtk-widget-activate widget))
    ;; gtk_widget_reparent
    (assert-false (gtk-widget-intersect widget (make-gdk-rectangle)))
    (assert-false (gtk-widget-is-focus widget))
    ;; gtk_widget_grab_focus
    ;; gtk_widget_grab_default
    (assert-equal "widget" (gtk-widget-set-name widget "widget"))
    (assert-equal "widget" (gtk-widget-get-name widget))
    ;; gtk_widget_set_state                      deprecated
    (assert-true (gtk-widget-set-sensitive widget t))
    ;; gtk_widget_set_parent
    (assert-false (gtk-widget-get-parent-window widget))
    (assert-eq :button-press-mask
               (gtk-widget-set-events widget :button-press-mask))
    (assert-false (gtk-widget-get-events widget)) ; Why not :button-press-mask
    ;; gtk_widget_add_events                     not implemented
    ;; gtk_widget_set_device_events              not implemented
    ;; gtk_widget_get_device_events              not implemented
    ;; gtk_widget_add_device_events              not implemented
    ;; gtk_widget_set_device_enabled             not implemented
    ;; gtk_widget_get_device_enabled             not implemented
    (assert-eq 'gtk-separator (type-of (gtk-widget-get-toplevel widget)))
    (assert-eq 'gtk-separator
               (type-of (gtk-widget-get-ancestor widget "GObject")))
    (assert-eq 'gdk-visual (type-of (gtk-widget-get-visual widget)))
    (assert-eq 'gdk-visual
               (type-of (gtk-widget-set-visual widget
                                               (gtk-widget-get-visual widget))))
    (assert-equal (values -1 -1) (gtk-widget-get-pointer widget))
    (assert-false (gtk-widget-is-ancestor widget widget))
    ;; gtk_widget_translate_coordinates
    ;; gtk_widget_hide_on_delete                 not implemented
    ;; gtk_widget_set_style                      deprecated
    ;; gtk_widget_ensure_style                   deprecated
    ;; gtk_widget_get_style                      deprecated
    ;; gtk_widget_reset_rc_styles                deprecated
    ;; gtk_widget_get_default_style              deprecated
    ;; gtk_widget_set_direction                  not implemented
    ;; gtk_widget_get_direction                  not implemented
    (assert-false (gtk-widget-set-default-direction :ltr))
    (assert-eq :ltr (gtk-widget-get-default-direction))
    ;; gtk_widget_shape_combine_region           not implemented
    ;; gtk_widget_input_shape_combine_region     not implemented
    ;; gtk_widget_path                           deprecated
    ;; gtk_widget_class_path                     deprecated
    (assert-false (gtk-widget-get-composite-name widget))
    ;; gtk_widget_override_background_color      not implemented
    ;; gtk_widget_override_color                 not implemented
    ;; gtk_widget_override_font                  not implemented
    ;; gtk_widget_override_symbolic_color        not implemented
    ;; gtk_widget_override_cursor                not implemented
    ;; gtk_widget_modify_style                   not implemented and deprecated
    ;; gtk_widget_get_modifier_style             deprecated
    ;; gtk_widget_modify_fg                      deprecated
    ;; gtk_widget_modify_bg                      deprecated
    ;; gtk_widget_modify_text                    deprecated
    ;; gtk_widget_modify_base                    deprecated
    ;; gtk_widget_modify_font                    deprecated
    ;; gtk_widget_modify_cursor                  deprecated
    (assert-eq 'pango-context
               (type-of (gtk-widget-create-pango-context widget)))
    ;; gtk_widget_get_pango_context              not implemented
    (assert-eq 'pango-layout
               (type-of (gtk-widget-create-pango-layout widget "Text")))
    ;; gtk_widget_render_icon                    deprecated
    ;; gtk_widget_render_icon_pixbuf             not implemented
    (assert-false (gtk-widget-pop-composite-child))
    (assert-false (gtk-widget-push-composite-child))
    (assert-false (gtk-widget-queue-draw-area widget 0 0 1 1))
    ;; gtk_widget_queue_draw_region              not implemented
    (assert-true (gtk-widget-set-app-paintable widget t))
    (assert-true (gtk-widget-set-double-buffered widget t))
    ;; gtk_widget_set_redraw_on_allocate         not implemented
    ;; gtk_widget_set_composite_name
    (assert-true (gtk-widget-mnemonic-activate (make-instance 'gtk-button) t))
    ;; gtk_widget_class_install_style_property   not implemented
    ;; gtk_widget_class_install_style_property_parser  not implemented
    (let ((class (g-type-class-ref (gtype "GtkWidget"))))
      (assert-true (gtk-widget-class-find-style-property class "cursor-aspect-ratio"))
      (g-type-class-unref class))
    (assert-true (gtk-widget-class-list-style-properties (gtype "GtkWidget")))
    (assert-eq 'cairo-region-t 
               (type-of (gtk-widget-region-intersect widget (cairo-region-create))))
    ;; gtk_widget_send_expose                    not implemented
    ;; gtk_widget_send_focus_change              not implemented
    ;; gtk_widget_style_get                      not implemented    
    (assert-eql 0.04 (gtk-widget-style-get-property (pointer widget) "cursor-aspect-ratio"))
    ;; gtk_widget_style_get_valist               not implemented
    ;; gtk_widget_style_attach                   not implemented
    ;; gtk_widget_class_set_accessible_type      not implemented
    ;; gtk_widget_class_set_accessible_role      not implemented
    ;; gtk_widget_get_accessible                 not implemented
    ;; gtk_widget_child_focus
    ;; gtk_widget_child_notify
    ;; gtk_widget_freeze_child_notify
    ;; gtk_widget_get_child_visible              not implemented
    (assert-false (gtk-widget-get-parent widget))
    (assert-eq 'gtk-settings (type-of (gtk-widget-get-settings widget)))
    ;; gtk-widget-get-clipboard
    (assert-eq 'gdk-display (type-of (gtk-widget-get-display widget)))
    (assert-eq 'gdk-window (type-of (gtk-widget-get-root-window widget)))
    (assert-eq 'gdk-screen (type-of (gtk-widget-get-screen widget)))
    (assert-false (gtk-widget-has-screen widget))
    (assert-equal (values -1 -1) (gtk-widget-get-size-request widget))
    ;; gtk_widget_set_child_visible              not implemented
    (assert-eql 100 (gtk-widget-set-size-request widget 200 100))
    ;; gtk_widget_thaw_child_notify
    (assert-false (gtk-widget-set-no-show-all widget nil))
    (assert-false (gtk-widget-get-no-show-all widget))
    (assert-false (gtk-widget-list-mnemonic-labels widget))
    ;; gtk_widget_add_mnemonic_label
    ;; gtk_widget_remove_mnemonic_label
    ;; gtk_widget_is_composited
    ;; gtk_widget_error_bell
    ;; gtk_widget_keynav_failed                  not implemented
    (assert-false (gtk-widget-get-tooltip-markup widget))
    (assert-equal "markup" (gtk-widget-set-tooltip-markup widget "markup"))
    (assert-equal "markup" (gtk-widget-get-tooltip-text widget))
    (assert-equal "text" (gtk-widget-set-tooltip-text widget "text"))
    (assert-false (gtk-widget-get-tooltip-window widget))
    (assert-false (gtk-widget-set-tooltip-window widget nil))
    (assert-true (gtk-widget-get-has-tooltip widget))
    (assert-true (gtk-widget-set-has-tooltip widget t))
    ;; gtk_widget_trigger_tooltip_query
    (assert-false (gtk-widget-get-window widget))
    ;; gtk_cairo_should_draw_window              not implemented
    ;; gtk_cairo_transform_to_window             not implemented
    (assert-eql 1 (gtk-widget-get-allocated-width widget))
    (assert-eql 1 (gtk-widget-get-allocated-height widget))
    (assert-eq 'gdk-rectangle (type-of (gtk-widget-get-allocation widget)))
    (assert-false (gtk-widget-set-allocation widget (make-gdk-rectangle)))
    (assert-true  (gtk-widget-get-app-paintable widget))
    (assert-false (gtk-widget-get-can-default widget))
    (assert-false  (gtk-widget-set-can-default widget nil))
    (assert-false (gtk-widget-get-can-focus widget))
    (assert-false (gtk-widget-set-can-focus widget nil))
    (assert-true (gtk-widget-get-double-buffered widget))
    (assert-false (gtk-widget-get-has-window widget))
    (assert-false (gtk-widget-set-has-window widget nil))
    (assert-true (gtk-widget-get-sensitive widget))
    (assert-true (gtk-widget-is-sensitive widget))
    ;; gtk_widget_get_state                      deprecated
    (assert-true (gtk-widget-get-visible widget))
    (assert-true (gtk-widget-set-visible widget t))
    ;; gtk_widget_set_state_flags                not implemented
    ;; gtk_widget_unset_state_flags              not implemented
    ;; gtk_widget_get_state_flags                not implemented
    (assert-false (gtk-widget-has-default widget))
    (assert-false (gtk-widget-has-focus widget))
    ;; gtk_widget_has_visible_focus              not implemented
    ;; gtk_widget_has_grab                       not implemented
    ;; gtk_widget_has_rc_style                   deprecated
    (assert-false (gtk-widget-is-drawable widget))
    (assert-false (gtk-widget-is-toplevel widget))
    ;; gtk_widget_set_window
    (assert-error 'error
                  (gtk-widget-set-window widget (gtk-widget-get-window widget)))
    ;; gtk_widget_set_receives_default
    (assert-false (gtk-widget-set-receives-default widget nil))
    ;; gtk_widget_get_receives_default
    (assert-false (gtk-widget-get-receives-default widget))
    ;; gtk_widget_set_support_multidevice        not implemented
    ;; gtk_widget_get_support_multidevice        not implemented
    ;; gtk_widget_set_realized                   not implemented
    ;; gtk_widget_get_realized                   not implemented
    ;; gtk_widget_set_mapped                     not implemented
    ;; gtk_widget_get_mapped                     not implemented
    ;; gtk_widget_get_requisition                not implemented
    ;; gtk_widget_device_is_shadowed             not implemented
    ;; gtk_widget_get_path                       not implemented
    ;; gtk_widget_get_style_context              not implemented
    ;; gtk_widget_reset_style                    not implemented
    ;; 
    ;; gtk_requisition_new                       not implemented
    ;; gtk_requisition_copy                      not implemented
    ;; gtk_requisition_free                      not implemented
    ;; 
    ;; gtk_widget_get_preferred_height           not implemented
    ;; gtk_widget_get_preferred_width            not implemented
    ;; gtk_widget_get_preferred_height_for_width not implemented
    ;; gtk_widget_get_preferred_width_for_height not implemented
    ;; gtk_widget_get_request_mode               not implemented
    ;; gtk_widget_get_preferred_size             not implemented
    ;; gtk_distribute_natural_allocation         not implemented
    ;; 
    (assert-eq :gtk-align-fill (gtk-widget-get-halign widget))
    (assert-eq :gtk-align-fill (gtk-widget-set-halign widget (gtk-widget-get-halign widget)))
    (assert-eq :gtk-align-fill (gtk-widget-get-valign widget))
    (assert-eq :gtk-align-fill (gtk-widget-set-valign widget (gtk-widget-get-valign widget)))
    (assert-eql 0 (gtk-widget-get-margin-left widget))
    (assert-eql 0 (gtk-widget-set-margin-left widget (gtk-widget-get-margin-left widget)))
    (assert-eql 0 (gtk-widget-get-margin-right widget))
    (assert-eql 0 (gtk-widget-set-margin-right widget (gtk-widget-get-margin-right widget)))
    (assert-eql 0 (gtk-widget-get-margin-top widget))
    (assert-eql 0 (gtk-widget-set-margin-top widget (gtk-widget-get-margin-top widget))) 
    (assert-eql 0 (gtk-widget-get-margin-bottom widget))
    (assert-eql 0 (gtk-widget-set-margin-bottom widget (gtk-widget-get-margin-bottom widget)))
    ;; 
    (assert-false (gtk-widget-get-hexpand widget))
    (assert-false (gtk-widget-set-hexpand widget nil))
    (assert-true  (gtk-widget-get-hexpand-set widget))
    (assert-false (gtk-widget-set-hexpand-set widget nil))
    (assert-false (gtk-widget-get-vexpand widget))
    (assert-false (gtk-widget-set-vexpand widget nil))
    (assert-true  (gtk-widget-get-vexpand-set widget))
    (assert-false (gtk-widget-set-vexpand-set widget nil))
    ;; gtk_widget_queue_compute_expand           not implemented
    ;; gtk_widget_compute_expand                 not implemented

    )
)

;;; --- End of file rtest-gtk-widget.lisp --------------------------------------
