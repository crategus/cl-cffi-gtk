;;; ----------------------------------------------------------------------------
;;; gtk.generated-classes.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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

(in-package :gtk)

(define-g-enum "GtkUnit"
    unit
    (:export t :type-initializer "gtk_unit_get_type")
  (:pixel 0)
  (:points 1)
  (:inch 2)
  (:mm 3))

(define-g-enum "GtkTreeViewGridLines"
    tree-view-grid-lines
    (:export t :type-initializer "gtk_tree_view_grid_lines_get_type")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))

(define-g-enum "GtkPackDirection"
    pack-direction
    (:export t :type-initializer "gtk_pack_direction_get_type")
  (:ltr 0)
  (:rtl 1)
  (:ttb 2)
  (:btt 3))

(define-g-enum "GtkSensitivityType"
    sensitivity-type
    (:export t :type-initializer "gtk_sensitivity_type_get_type")
  (:auto 0)
  (:on 1)
  (:off 2))

(define-g-enum "GtkNotebookTab"
    notebook-tab
    (:export t :type-initializer "gtk_notebook_tab_get_type")
  (:first 0)
  (:last 1))

(define-g-enum "GtkAnchorType"
    anchor-type
    (:export t :type-initializer "gtk_anchor_type_get_type")
  (:center 0)
  (:north 1)
  (:north-west 2)
  (:north-east 3)
  (:south 4)
  (:south-west 5)
  (:south-east 6)
  (:west 7)
  (:east 8)
  (:n 1)
  (:nw 2)
  (:ne 3)
  (:s 4)
  (:sw 5)
  (:se 6)
  (:w 7)
  (:e 8))

(define-g-enum "GtkCellType"
    cell-type
    (:export t :type-initializer "gtk_cell_type_get_type")
  (:empty 0)
  (:text 1)
  (:pixmap 2)
  (:pixtext 3)
  (:widget 4))

(define-g-enum "GtkCListDragPos"
    c-list-drag-pos
    (:export t :type-initializer "gtk_clist_drag_pos_get_type")
  (:none 0)
  (:before 1)
  (:into 2)
  (:after 3))

(define-g-enum "GtkCTreeExpanderStyle"
    c-tree-expander-style
    (:export t :type-initializer "gtk_ctree_expander_style_get_type")
  (:none 0)
  (:square 1)
  (:triangle 2)
  (:circular 3))

(define-g-enum "GtkCTreeExpansionType"
    c-tree-expansion-type
    (:export t :type-initializer "gtk_ctree_expansion_type_get_type")
  (:expand 0)
  (:expand-recursive 1)
  (:collapse 2)
  (:collapse-recursive 3)
  (:toggle 4)
  (:toggle-recursive 5))

(define-g-enum "GtkCTreeLineStyle"
    c-tree-line-style
    (:export t :type-initializer "gtk_ctree_line_style_get_type")
  (:none 0)
  (:solid 1)
  (:dotted 2)
  (:tabbed 3))

(define-g-enum "GtkCTreePos"
    c-tree-pos
    (:export t :type-initializer "gtk_ctree_pos_get_type")
  (:before 0)
  (:as-child 1)
  (:after 2))

(define-g-enum "GtkIconThemeError"
    icon-theme-error
    (:export t :type-initializer "gtk_icon_theme_error_get_type")
  (:not-found 0)
  (:failed 1))

(define-g-enum "GtkMatchType"
    match-type
    (:export t :type-initializer "gtk_match_type_get_type")
  (:all 0)
  (:all-tail 1)
  (:head 2)
  (:tail 3)
  (:exact 4)
  (:last 5))

(define-g-enum "GtkPreviewType"
    preview-type
    (:export t :type-initializer "gtk_preview_type_get_type")
  (:color 0)
  (:grayscale 1))

(define-g-enum "GtkRcTokenType"
    rc-token-type
    (:export t :type-initializer "gtk_rc_token_type_get_type")
  (:invalid 270)
  (:include 271)
  (:normal 272)
  (:active 273)
  (:prelight 274)
  (:selected 275)
  (:insensitive 276)
  (:fg 277)
  (:bg 278)
  (:text 279)
  (:base 280)
  (:xthickness 281)
  (:ythickness 282)
  (:font 283)
  (:fontset 284)
  (:font-name 285)
  (:bg-pixmap 286)
  (:pixmap-path 287)
  (:style 288)
  (:binding 289)
  (:bind 290)
  (:widget 291)
  (:widget-class 292)
  (:class 293)
  (:lowest 294)
  (:gtk 295)
  (:application 296)
  (:theme 297)
  (:rc 298)
  (:highest 299)
  (:engine 300)
  (:module-path 301)
  (:im-module-path 302)
  (:im-module-file 303)
  (:stock 304)
  (:ltr 305)
  (:rtl 306)
  (:color 307)
  (:unbind 308)
  (:last 309))

(define-g-enum "GtkSideType"
    side-type
    (:export t :type-initializer "gtk_side_type_get_type")
  (:top 0)
  (:bottom 1)
  (:left 2)
  (:right 3))

(define-g-enum "GtkSpinType"
    spin-type
    (:export t :type-initializer "gtk_spin_type_get_type")
  (:step-forward 0)
  (:step-backward 1)
  (:page-forward 2)
  (:page-backward 3)
  (:home 4)
  (:end 5)
  (:user-defined 6))

(define-g-enum "GtkSubmenuDirection"
    submenu-direction
    (:export t :type-initializer "gtk_submenu_direction_get_type")
  (:left 0)
  (:right 1))

(define-g-enum "GtkSubmenuPlacement"
    submenu-placement
    (:export t :type-initializer "gtk_submenu_placement_get_type")
  (:top-bottom 0)
  (:left-right 1))

(define-g-enum "GtkToolbarChildType"
    toolbar-child-type
    (:export t :type-initializer "gtk_toolbar_child_type_get_type")
  (:space 0)
  (:button 1)
  (:togglebutton 2)
  (:radiobutton 3)
  (:widget 4))

(define-g-enum "GtkTreeViewMode"
    tree-view-mode
    (:export t :type-initializer "gtk_tree_view_mode_get_type")
  (:line 0)
  (:item 1))

(define-g-enum "GtkVisibility"
    visibility
    (:export t :type-initializer "gtk_visibility_get_type")
  (:none 0)
  (:partial 1)
  (:full 2))

(define-g-flags "GtkButtonAction"
    button-action
    (:export t :type-initializer "gtk_button_action_get_type")
  (:ignored 0)
  (:selects 1)
  (:drags 2)
  (:expands 4))

(define-g-flags "GtkDebugFlag"
    debug-flag
    (:export t :type-initializer "gtk_debug_flag_get_type")
  (:misc 1)
  (:plugsocket 2)
  (:text 4)
  (:tree 8)
  (:updates 16)
  (:keybindings 32)
  (:multihead 64)
  (:modules 128)
  (:geometry 256)
  (:icontheme 512)
  (:printing 1024)
  (:builder 2048))

(define-g-flags "GtkIconLookupFlags"
    icon-lookup-flags
    (:export t :type-initializer "gtk_icon_lookup_flags_get_type")
  (:no-svg 1)
  (:force-svg 2)
  (:use-builtin 4)
  (:generic-fallback 8)
  (:force-size 16))

(define-g-flags "GtkPrivateFlags"
    private-flags
    (:export t :type-initializer "gtk_private_flags_get_type")
  (:user-style 1)
  (:resize-pending 4)
  (:has-pointer 8)
  (:shadowed 16)
  (:has-shape-mask 32)
  (:in-reparent 64)
  (:direction-set 128)
  (:direction-ltr 256)
  (:anchored 512)
  (:child-visible 1024)
  (:redraw-on-alloc 2048)
  (:alloc-needed 4096)
  (:request-needed 8192))

(define-g-flags "GtkRcFlags"
    rc-flags
    (:export t :type-initializer "gtk_rc_flags_get_type")
  (:fg 1)
  (:bg 2)
  (:text 4)
  (:base 8))

(define-g-flags "GtkSignalRunType"
    signal-run-type
    (:export t :type-initializer "gtk_signal_run_type_get_type")
  (:first 1)
  (:last 2)
  (:both 3)
  (:no-recurse 8)
  (:action 32)
  (:no-hooks 64))

;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontButton" gtk-font-button
  (:superclass gtk-button
    :export t
    :interfaces ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
    :type-initializer "gtk_font_button_get_type")
  ((font-name
    gtk-font-button-font-name
    "font-name" "gchararray" t t)
   (show-size
    gtk-font-button-show-size
    "show-size" "gboolean" t t)
   (show-style
    gtk-font-button-show-style
    "show-style" "gboolean" t t)
   (title
    gtk-font-button-title
    "title" "gchararray" t t)
   (use-font
    gtk-font-button-use-font
    "use-font" "gboolean" t t)
   (use-size
    gtk-font-button-use-size
    "use-size" "gboolean" t t)))

(define-g-object-class "GtkComboBoxEntry" gtk-combo-box-entry
                       (:superclass gtk-combo-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                         "GtkCellLayout")
                        :type-initializer "gtk_combo_box_entry_get_type")
                       ((text-column combo-box-entry-text-column "text-column"
                         "gint" t t)))

(define-g-object-class "GtkHandleBox" gtk-handle-box
                       (:superclass gtk-bin :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_handle_box_get_type")
                       ((child-detached handle-box-child-detached
                         "child-detached" "gboolean" t nil)
                        (handle-position handle-box-handle-position
                         "handle-position" "GtkPositionType" t t)
                        (shadow handle-box-shadow "shadow" "GtkShadowType" t t)
                        (shadow-type handle-box-shadow-type "shadow-type"
                         "GtkShadowType" t t)
                        (snap-edge handle-box-snap-edge "snap-edge"
                         "GtkPositionType" t t)
                        (snap-edge-set handle-box-snap-edge-set "snap-edge-set"
                         "gboolean" t t)))

(define-g-object-class "GtkCheckMenuItem" gtk-check-menu-item
                       (:superclass gtk-menu-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_check_menu_item_get_type")
                       ((active check-menu-item-active "active" "gboolean" t t)
                        (draw-as-radio check-menu-item-draw-as-radio
                         "draw-as-radio" "gboolean" t t)
                        (inconsistent check-menu-item-inconsistent
                         "inconsistent" "gboolean" t t)))

(define-g-object-class "GtkRadioMenuItem" gtk-radio-menu-item
                       (:superclass gtk-check-menu-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_radio_menu_item_get_type")
                       ((group radio-menu-item-group "group" "GtkRadioMenuItem"
                         nil t)))

(define-g-object-class "GtkImageMenuItem" gtk-image-menu-item
                       (:superclass gtk-menu-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_image_menu_item_get_type")
                       ((accel-group image-menu-item-accel-group "accel-group"
                         "GtkAccelGroup" nil t)
                        (always-show-image image-menu-item-always-show-image
                         "always-show-image" "gboolean" t t)
                        (image image-menu-item-image "image" "GtkWidget" t t)
                        (use-stock image-menu-item-use-stock "use-stock"
                         "gboolean" t t)))

(define-g-object-class "GtkSeparatorMenuItem" gtk-separator-menu-item
                       (:superclass gtk-menu-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_separator_menu_item_get_type")
                       nil)

(define-g-object-class "GtkTearoffMenuItem" gtk-tearoff-menu-item
                       (:superclass gtk-menu-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_tearoff_menu_item_get_type")
                       nil)

(define-g-object-class "GtkSeparatorToolItem" gtk-separator-tool-item
                       (:superclass gtk-tool-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_separator_tool_item_get_type")
                       ((draw separator-tool-item-draw "draw" "gboolean" t t)))

(define-g-object-class "GtkMenuToolButton" gtk-menu-tool-button
                       (:superclass gtk-tool-button :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_menu_tool_button_get_type")
                       ((menu menu-tool-button-menu "menu" "GtkMenu" t t)
                        (:cffi arrow-tooltip-text
                         menu-tool-button-arrow-tooltip-text :string nil
                         "gtk_menu_tool_button_set_arrow_tooltip_text")
                        (:cffi arrow-tooltip-markup
                         menu-tool-button-arrow-tooltip-markup :string nil
                         "gtk_menu_tool_button_set_arrow_tooltip_markup")))

(define-g-object-class "GtkColorSelectionDialog" gtk-color-selection-dialog
                       (:superclass gtk-dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer
                        "gtk_color_selection_dialog_get_type")
                       ((cancel-button color-selection-dialog-cancel-button
                         "cancel-button" "GtkWidget" t nil)
                        (color-selection color-selection-dialog-color-selection
                         "color-selection" "GtkWidget" t nil)
                        (help-button color-selection-dialog-help-button
                         "help-button" "GtkWidget" t nil)
                        (ok-button color-selection-dialog-ok-button "ok-button"
                         "GtkWidget" t nil)))

(define-g-object-class "GtkFontSelectionDialog" gtk-font-selection-dialog
                       (:superclass gtk-dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_font_selection_dialog_get_type")
                       ((:cffi font-name font-selection-dialog-font-name
                         (g-string :free-from-foreign t :free-to-foreign t)
                         "gtk_font_selection_dialog_get_font_name"
                         "gtk_font_selection_dialog_set_font_name")
                        (:cffi preview-text font-selection-dialog-preview-text
                         :string "gtk_font_selection_dialog_get_preview_text"
                         "gtk_font_selection_dialog_set_preview_text")
                        (:cffi apply-button font-selection-dialog-apply-button
                         g-object "gtk_font_selection_dialog_get_apply_button"
                         nil)
                        (:cffi cancel-button
                         font-selection-dialog-cancel-button g-object
                         "gtk_font_selection_dialog_get_cancel_button" nil)
                        (:cffi ok-button font-selection-dialog-ok-button
                         g-object "gtk_font_selection_dialog_get_ok_button"
                         nil)))

(define-g-object-class "GtkInputDialog" gtk-input-dialog
                       (:superclass gtk-dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_input_dialog_get_type")
                       nil)

(define-g-object-class "GtkPageSetupUnixDialog" gtk-page-setup-unix-dialog
                       (:superclass gtk-dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer
                        "gtk_page_setup_unix_dialog_get_type")
                       ((:cffi page-setup page-setup-unix-dialog-page-setup
                         (g-object page-setup)
                         "gtk_page_setup_unix_dialog_get_page_setup"
                         "gtk_page_setup_unix_dialog_set_page_setup")
                        (:cffi print-settings
                         page-setup-unix-dialog-print-settings
                         (g-object print-settings)
                         "gtk_page_setup_unix_dialog_get_print_settings"
                         "gtk_page_setup_unix_dialog_set_print_settings")))

(define-g-object-class "GtkPrintUnixDialog" gtk-print-unix-dialog
                       (:superclass gtk-dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_print_unix_dialog_get_type")
                       ((current-page print-unix-dialog-current-page
                         "current-page" "gint" t t)
                        (page-setup print-unix-dialog-page-setup "page-setup"
                         "GtkPageSetup" t t)
                        (print-settings print-unix-dialog-print-settings
                         "print-settings" "GtkPrintSettings" t t)
                        (selected-printer print-unix-dialog-selected-printer
                         "selected-printer" "GtkPrinter" t nil)))

(define-g-object-class "GtkPlug" gtk-plug
                       (:superclass gtk-window :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_plug_get_type")
                       ((embedded plug-embedded "embedded" "gboolean" t nil)
                        (socket-window plug-socket-window "socket-window"
                         "GdkWindow" t nil)))

(define-g-object-class "GtkMenuBar" gtk-menu-bar
                       (:superclass gtk-menu-shell :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_menu_bar_get_type")
                       ((child-pack-direction menu-bar-child-pack-direction
                         "child-pack-direction" "GtkPackDirection" t t)
                        (pack-direction menu-bar-pack-direction
                         "pack-direction" "GtkPackDirection" t t)))

(define-g-object-class "GtkSocket" gtk-socket
                       (:superclass gtk-container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_socket_get_type")
                       nil)

(define-g-object-class "GtkRcStyle" gtk-rc-style
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_rc_style_get_type")
                       nil)

(define-g-object-class "GtkStyle" gtk-style
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_style_get_type")
                       nil)

(define-g-object-class "GtkIconTheme" gtk-icon-theme
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_icon_theme_get_type")
                       nil)
