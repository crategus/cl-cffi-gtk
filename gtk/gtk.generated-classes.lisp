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

(define-g-enum "GtkPrintStatus"
    print-status
    (:export t :type-initializer "gtk_print_status_get_type")
  (:initial 0)
  (:preparing 1)
  (:generating-data 2)
  (:sending-data 3)
  (:pending 4)
  (:pending-issue 5)
  (:printing 6)
  (:finished 7)
  (:finished-aborted 8))

(define-g-enum "GtkFileChooserAction"
    file-chooser-action
    (:export t :type-initializer "gtk_file_chooser_action_get_type")
  (:open 0)
  (:save 1)
  (:select-folder 2)
  (:create-folder 3))

(define-g-enum "GtkCellRendererAccelMode"
    cell-renderer-accel-mode
    (:export t :type-initializer "gtk_cell_renderer_accel_mode_get_type")
  (:gtk 0)
  (:other 1))

(define-g-enum "GtkMetricType"
    metric-type
    (:export t :type-initializer "gtk_metric_type_get_type")
  (:pixels 0)
  (:inches 1)
  (:centimeters 2))

(define-g-enum "GtkCurveType"
    curve-type
    (:export t :type-initializer "gtk_curve_type_get_type")
  (:linear 0)
  (:spline 1)
  (:free 2))

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

(define-g-enum "GtkFileChooserConfirmation"
    file-chooser-confirmation
    (:export t :type-initializer "gtk_file_chooser_confirmation_get_type")
  (:confirm 0)
  (:accept-filename 1)
  (:select-again 2))

(define-g-enum "GtkFileChooserError"
    file-chooser-error
    (:export t :type-initializer "gtk_file_chooser_error_get_type")
  (:nonexistent 0)
  (:bad-filename 1)
  (:already-exists 2)
  (:incomplete-hostname 3))

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

(define-g-enum "GtkNumberUpLayout"
    number-up-layout
    (:export t :type-initializer "gtk_number_up_layout_get_type")
  (:lrtb 0)
  (:lrbt 1)
  (:rltb 2)
  (:rlbt 3)
  (:tblr 4)
  (:tbrl 5)
  (:btlr 6)
  (:btrl 7))

(define-g-enum "GtkPageOrientation"
    page-orientation
    (:export t :type-initializer "gtk_page_orientation_get_type")
  (:portrait 0)
  (:landscape 1)
  (:reverse-portrait 2)
  (:reverse-landscape 3))

(define-g-enum "GtkPageSet"
    page-set
    (:export t :type-initializer "gtk_page_set_get_type")
  (:all 0)
  (:even 1)
  (:odd 2))

(define-g-enum "GtkPreviewType"
    preview-type
    (:export t :type-initializer "gtk_preview_type_get_type")
  (:color 0)
  (:grayscale 1))

(define-g-enum "GtkPrintDuplex"
    print-duplex
    (:export t :type-initializer "gtk_print_duplex_get_type")
  (:simplex 0)
  (:horizontal 1)
  (:vertical 2))

(define-g-enum "GtkPrintError"
    print-error
    (:export t :type-initializer "gtk_print_error_get_type")
  (:general 0)
  (:internal-error 1)
  (:nomem 2)
  (:invalid-file 3))

(define-g-enum "GtkPrintOperationAction"
    print-operation-action
    (:export t :type-initializer "gtk_print_operation_action_get_type")
  (:print-dialog 0)
  (:print 1)
  (:preview 2)
  (:export 3))

(define-g-enum "GtkPrintOperationResult"
    print-operation-result
    (:export t :type-initializer "gtk_print_operation_result_get_type")
  (:error 0)
  (:apply 1)
  (:cancel 2)
  (:in-progress 3))

(define-g-enum "GtkPrintPages"
    print-pages
    (:export t :type-initializer "gtk_print_pages_get_type")
  (:all 0)
  (:current 1)
  (:ranges 2))

(define-g-enum "GtkPrintQuality"
    print-quality
    (:export t :type-initializer "gtk_print_quality_get_type")
  (:low 0)
  (:normal 1)
  (:high 2)
  (:draft 3))

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

(define-g-enum "GtkRecentManagerError"
    recent-manager-error
    (:export t :type-initializer "gtk_recent_manager_error_get_type")
  (:not-found 0)
  (:invalid-uri 1)
  (:invalid-encoding 2)
  (:not-registered 3)
  (:read 4)
  (:write 5)
  (:unknown 6))

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

(define-g-enum "GtkEntryIconPosition"
    entry-icon-position
    (:export t :type-initializer "gtk_entry_icon_position_get_type")
  (:primary 0)
  (:secondary 1))

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

(define-g-interface "GtkFileChooser" gtk-file-chooser
  (:export t
   :type-initializer "gtk_file_chooser_get_type")
  (action gtk-file-chooser-action
   "action" "GtkFileChooserAction" t t)
  (do-overwrite-confirmation gtk-file-chooser-do-overwrite-confirmation
   "do-overwrite-confirmation" "gboolean" t t)
  (extra-widget gtk-file-chooser-extra-widget
   "extra-widget" "GtkWidget" t t)
  (file-system-backend gtk-file-chooser-file-system-backend
   "file-system-backend" "gchararray" nil nil)
  (filter gtk-file-chooser-filter
   "filter" "GtkFileFilter" t t)
  (local-only gtk-file-chooser-local-only
   "local-only" "gboolean" t t)
  (preview-widget gtk-file-chooser-preview-widget
   "preview-widget" "GtkWidget" t t)
  (preview-widget-active gtk-file-chooser-preview-widget-active
   "preview-widget-active" "gboolean" t t)
  (select-multiple gtk-file-chooser-select-multiple
   "select-multiple" "gboolean" t t)
  (show-hidden gtk-file-chooser-show-hidden
   "show-hidden" "gboolean" t t)
  (use-preview-label gtk-file-chooser-use-preview-label
   "use-preview-label" "gboolean" t t)
  (:cffi current-name gtk-file-chooser-current-name
         (:string :free-to-foreign t :encoding :utf-8) nil
         "gtk_file_chooser_set_current_name")
  #+win32
  (:cffi filename gtk-file-chooser-filename
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_filename_utf8"
         "gtk_file_chooser_set_filename_utf8")
  #-win32
  (:cffi filename gtk-file-chooser-filename
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_filename"
         "gtk_file_chooser_set_filename")
  #+win32
  (:cffi current-folder gtk-file-chooser-current-folder
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_current_folder_utf8"
         "gtk_file_chooser_set_current_folder_utf8")
  #-win32
  (:cffi current-folder gtk-file-chooser-current-folder
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_current_folder"
         "gtk_file_chooser_set_current_folder")
  (:cffi uri gtk-file-chooser-uri
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_uri" "gtk_file_chooser_set_uri")
  (:cffi current-folder-uri gtk-file-chooser-current-folder-uri
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_current_folder_uri"
         "gtk_file_chooser_set_current_folder_uri")
  #+win32
  (:cffi preview-filename gtk-file-chooser-preview-filename
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_preview_filename_utf8" nil)
  #-win32
  (:cffi preview-filename gtk-file-chooser-preview-filename
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_preview_filename" nil)
  (:cffi preview-uri gtk-file-chooser-preview-uri
         (g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_preview_uri" nil))

(define-g-interface "GtkFileChooserEmbed"
    file-chooser-embed
    (:export t))

(define-g-interface "GtkPrintOperationPreview"
    print-operation-preview
    (:export t :type-initializer "gtk_print_operation_preview_get_type"))

;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererPixbuf" gtk-cell-renderer-pixbuf
  (:superclass gtk-cell-renderer
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_pixbuf_get_type")
  ((follow-state gtk-cell-renderer-pixbuf-follow-state
    "follow-state" "gboolean" t t)
   (gicon gtk-cell-renderer-pixbuf-gicon
    "gicon" "GIcon" t t)
   (icon-name gtk-cell-renderer-pixbuf-icon-name
    "icon-name" "gchararray" t t)
   (pixbuf gtk-cell-renderer-pixbuf-pixbuf
    "pixbuf" "GdkPixbuf" t t)
   (pixbuf-expander-closed gtk-cell-renderer-pixbuf-pixbuf-expander-closed
    "pixbuf-expander-closed" "GdkPixbuf" t t)
   (pixbuf-expander-open gtk-cell-renderer-pixbuf-pixbuf-expander-open
    "pixbuf-expander-open" "GdkPixbuf" t t)
   (stock-detail gtk-cell-renderer-pixbuf-stock-detail
    "stock-detail" "gchararray" t t)
   (stock-id gtk-cell-renderer-pixbuf-stock-id
    "stock-id" "gchararray" t t)
   (stock-size gtk-cell-renderer-pixbuf-stock-size
    "stock-size" "guint" t t)))

(define-g-object-class "GtkCellRendererProgress" gtk-cell-renderer-progress
  (:superclass gtk-cell-renderer
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_progress_get_type")
  ((orientation gtk-cell-renderer-progress-orientation
    "orientation" "GtkProgressBarOrientation" t t)
   (pulse gtk-cell-renderer-progress-pulse
    "pulse" "gint" t t)
   (text gtk-cell-renderer-progress-text
    "text" "gchararray" t t)
   (text-xalign gtk-cell-renderer-progress-text-xalign
    "text-xalign" "gfloat" t t)
   (text-yalign gtk-cell-renderer-progress-text-yalign
    "text-yalign" "gfloat" t t)
   (value gtk-cell-renderer-progress-value
    "value" "gint" t t)))

(define-g-object-class "GtkCellRendererAccel" gtk-cell-renderer-accel
  (:superclass gtk-cell-renderer-text
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_accel_get_type")
  ((accel-key gtk-cell-renderer-accel-accel-key
    "accel-key" "guint" t t)
   (accel-mode gtk-cell-renderer-accel-accel-mode
    "accel-mode" "GtkCellRendererAccelMode" t t)
   (accel-mods gtk-cell-renderer-accel-accel-mods
    "accel-mods" "GdkModifierType" t t)
   (keycode gtk-cell-renderer-accel-keycode
    "keycode" "guint" t t)))

(define-g-object-class "GtkCellRendererCombo" gtk-cell-renderer-combo
                       (:superclass gtk-cell-renderer-text :export t :interfaces
                        nil :type-initializer
                        "gtk_cell_renderer_combo_get_type")
                       ((has-entry cell-renderer-combo-has-entry "has-entry"
                         "gboolean" t t)
                        (model cell-renderer-combo-model "model" "GtkTreeModel"
                         t t)
                        (text-column cell-renderer-combo-text-column
                         "text-column" "gint" t t)))

(define-g-object-class "GtkCellRendererSpin" gtk-cell-renderer-spin
                       (:superclass gtk-cell-renderer-text :export t :interfaces
                        nil :type-initializer
                        "gtk_cell_renderer_spin_get_type")
                       ((adjustment cell-renderer-spin-adjustment "adjustment"
                         "GtkAdjustment" t t)
                        (climb-rate cell-renderer-spin-climb-rate "climb-rate"
                         "gdouble" t t)
                        (digits cell-renderer-spin-digits "digits" "guint" t
                         t)))

(define-g-object-class "GtkCellRendererToggle" gtk-cell-renderer-toggle
                       (:superclass gtk-cell-renderer :export t :interfaces nil
                        :type-initializer "gtk_cell_renderer_toggle_get_type")
                       ((activatable cell-renderer-toggle-activatable
                         "activatable" "gboolean" t t)
                        (active cell-renderer-toggle-active "active" "gboolean"
                         t t)
                        (inconsistent cell-renderer-toggle-inconsistent
                         "inconsistent" "gboolean" t t)
                        (indicator-size cell-renderer-toggle-indicator-size
                         "indicator-size" "gint" t t)
                        (radio cell-renderer-toggle-radio "radio" "gboolean" t
                         t)))

(define-g-object-class "GtkFontButton" gtk-font-button
                       (:superclass gtk-button :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_font_button_get_type")
                       ((font-name font-button-font-name "font-name"
                         "gchararray" t t)
                        (show-size font-button-show-size "show-size" "gboolean"
                         t t)
                        (show-style font-button-show-style "show-style"
                         "gboolean" t t)
                        (title font-button-title "title" "gchararray" t t)
                        (use-font font-button-use-font "use-font" "gboolean" t
                         t)
                        (use-size font-button-use-size "use-size" "gboolean" t
                         t)))

(define-g-object-class "GtkComboBoxEntry" gtk-combo-box-entry
                       (:superclass gtk-combo-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                         "GtkCellLayout")
                        :type-initializer "gtk_combo_box_entry_get_type")
                       ((text-column combo-box-entry-text-column "text-column"
                         "gint" t t)))

(define-g-object-class "GtkAspectFrame" gtk-aspect-frame
                       (:superclass gtk-frame :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_aspect_frame_get_type")
                       ((obey-child aspect-frame-obey-child "obey-child"
                         "gboolean" t t)
                        (ratio aspect-frame-ratio "ratio" "gfloat" t t)
                        (xalign aspect-frame-xalign "xalign" "gfloat" t t)
                        (yalign aspect-frame-yalign "yalign" "gfloat" t t)))

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

(define-g-object-class "GtkViewport" gtk-viewport
                       (:superclass gtk-bin :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_viewport_get_type")
                       ((hadjustment viewport-hadjustment "hadjustment"
                         "GtkAdjustment" t t)
                        (shadow-type viewport-shadow-type "shadow-type"
                         "GtkShadowType" t t)
                        (vadjustment viewport-vadjustment "vadjustment"
                         "GtkAdjustment" t t)))

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

(define-g-object-class "GtkFileChooserDialog" gtk-file-chooser-dialog
                       (:superclass gtk-dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser")
                        :type-initializer "gtk_file_chooser_dialog_get_type")
                       nil)

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

(define-g-object-class "GtkRecentChooserDialog" gtk-recent-chooser-dialog
                       (:superclass gtk-dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable"
                         "GtkRecentChooser")
                        :type-initializer "gtk_recent_chooser_dialog_get_type")
                       nil)

(define-g-object-class "GtkPlug" gtk-plug
                       (:superclass gtk-window :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_plug_get_type")
                       ((embedded plug-embedded "embedded" "gboolean" t nil)
                        (socket-window plug-socket-window "socket-window"
                         "GdkWindow" t nil)))

(define-g-object-class "GtkButtonBox" gtk-button-box
                       (:superclass gtk-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_button_box_get_type")
                       ((layout-style button-box-layout-style "layout-style"
                         "GtkButtonBoxStyle" t t)))

(define-g-object-class "GtkHButtonBox" gtk-h-button-box
                       (:superclass gtk-button-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_hbutton_box_get_type")
                       nil)

(define-g-object-class "GtkVButtonBox" gtk-v-button-box
                       (:superclass gtk-button-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_vbutton_box_get_type")
                       nil)

(define-g-object-class "GtkFileChooserButton" gtk-file-chooser-button
                       (:superclass gtk-h-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser"
                         "GtkOrientable")
                        :type-initializer "gtk_file_chooser_button_get_type")
                       ((dialog file-chooser-button-dialog "dialog"
                         "GtkFileChooser" nil nil)
                        (focus-on-click file-chooser-button-focus-on-click
                         "focus-on-click" "gboolean" t t)
                        (title file-chooser-button-title "title" "gchararray" t
                         t)
                        (width-chars file-chooser-button-width-chars
                         "width-chars" "gint" t t)))

(define-g-object-class "GtkFileChooserWidget" gtk-file-chooser-widget
                       (:superclass gtk-v-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser"
                         "GtkFileChooserEmbed" "GtkOrientable")
                        :type-initializer "gtk_file_chooser_widget_get_type")
                       nil)

(define-g-object-class "GtkFontSelection" gtk-font-selection
                       (:superclass gtk-v-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_font_selection_get_type")
                       ((font font-selection-font "font" "GdkFont" t nil)
                        (font-name font-selection-font-name "font-name"
                         "gchararray" t t)
                        (preview-text font-selection-preview-text
                         "preview-text" "gchararray" t t)))

(define-g-object-class "GtkGammaCurve" gtk-gamma-curve
                       (:superclass gtk-v-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_gamma_curve_get_type")
                       nil)

(define-g-object-class "GtkRecentChooserWidget" gtk-recent-chooser-widget
                       (:superclass gtk-v-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
                         "GtkRecentChooser")
                        :type-initializer "gtk_recent_chooser_widget_get_type")
                       nil)

(define-g-object-class "GtkRecentChooserMenu" gtk-recent-chooser-menu
                       (:superclass gtk-menu :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable"
                         "GtkRecentChooser")
                        :type-initializer "gtk_recent_chooser_menu_get_type")
                       ((show-numbers recent-chooser-menu-show-numbers
                         "show-numbers" "gboolean" t t)))

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

(define-g-object-class "GtkCurve" gtk-curve
                       (:superclass gtk-drawing-area :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_curve_get_type")
                       ((curve-type curve-curve-type "curve-type"
                         "GtkCurveType" t t)
                        (max-x curve-max-x "max-x" "gfloat" t t)
                        (max-y curve-max-y "max-y" "gfloat" t t)
                        (min-x curve-min-x "min-x" "gfloat" t t)
                        (min-y curve-min-y "min-y" "gfloat" t t)))

(define-g-object-class "GtkRuler" gtk-ruler
                       (:superclass gtk-widget :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_ruler_get_type")
                       ((lower ruler-lower "lower" "gdouble" t t)
                        (max-size ruler-max-size "max-size" "gdouble" t t)
                        (metric ruler-metric "metric" "GtkMetricType" t t)
                        (position ruler-position "position" "gdouble" t t)
                        (upper ruler-upper "upper" "gdouble" t t)))

(define-g-object-class "GtkHRuler" gtk-h-ruler
                       (:superclass gtk-ruler :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_hruler_get_type")
                       nil)

(define-g-object-class "GtkVRuler" gtk-v-ruler
                       (:superclass gtk-ruler :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_vruler_get_type")
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

(define-g-object-class "GtkPageSetup" gtk-page-setup
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_page_setup_get_type")
                       nil)

(define-g-object-class "GtkPrintContext" gtk-print-context
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_print_context_get_type")
                       nil)

(define-g-object-class "GtkPrintOperation" gtk-print-operation
                       (:superclass g-object :export t :interfaces
                        ("GtkPrintOperationPreview") :type-initializer
                        "gtk_print_operation_get_type")
                       ((allow-async print-operation-allow-async "allow-async"
                         "gboolean" t t)
                        (current-page print-operation-current-page
                         "current-page" "gint" t t)
                        (custom-tab-label print-operation-custom-tab-label
                         "custom-tab-label" "gchararray" t t)
                        (default-page-setup print-operation-default-page-setup
                         "default-page-setup" "GtkPageSetup" t t)
                        (export-filename print-operation-export-filename
                         "export-filename" "gchararray" t t)
                        (job-name print-operation-job-name "job-name"
                         "gchararray" t t)
                        (n-pages print-operation-n-pages "n-pages" "gint" t t)
                        (print-settings print-operation-print-settings
                         "print-settings" "GtkPrintSettings" t t)
                        (show-progress print-operation-show-progress
                         "show-progress" "gboolean" t t)
                        (status print-operation-status "status"
                         "GtkPrintStatus" t nil)
                        (status-string print-operation-status-string
                         "status-string" "gchararray" t nil)
                        (track-print-status print-operation-track-print-status
                         "track-print-status" "gboolean" t t)
                        (unit print-operation-unit "unit" "GtkUnit" t t)
                        (use-full-page print-operation-use-full-page
                         "use-full-page" "gboolean" t t)))

(define-g-object-class "GtkPrintSettings" gtk-print-settings
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_print_settings_get_type")
  nil)

(define-g-object-class "GtkRecentManager" gtk-recent-manager
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gtk_recent_manager_get_type")
  ((filename gtk-recent-manager-filename
    "filename" "gchararray" t nil)
   (limit gtk-recent-manager-limit
    "limit" "gint" t t)
   (size gtk-recent-manager-size
    "size" "gint" t nil)))

(define-g-object-class "GtkItemFactory" gtk-item-factory
  (:superclass gtk-object
   :export t
   :interfaces nil
   :type-initializer "gtk_item_factory_get_type")
  nil)
