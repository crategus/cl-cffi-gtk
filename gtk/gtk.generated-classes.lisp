(in-package :gtk)

(define-g-enum "GtkTextDirection"
    text-direction
    (:export t :type-initializer "gtk_text_direction_get_type")
  (:none 0)
  (:ltr 1)
  (:rtl 2))

(define-g-enum "GtkSizeGroupMode"
    size-group-mode
    (:export t :type-initializer "gtk_size_group_mode_get_type")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))

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

(define-g-enum "GtkRecentSortType"
    recent-sort-type
    (:export t :type-initializer "gtk_recent_sort_type_get_type")
  (:none 0)
  (:mru 1)
  (:lru 2)
  (:custom 3))

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

(define-g-enum "GtkCellRendererMode"
    cell-renderer-mode
    (:export t :type-initializer "gtk_cell_renderer_mode_get_type")
  (:inert 0)
  (:activatable 1)
  (:editable 2))

(define-g-enum "GtkTreeViewColumnSizing"
    tree-view-column-sizing
    (:export t :type-initializer "gtk_tree_view_column_sizing_get_type")
  (:grow-only 0)
  (:autosize 1)
  (:fixed 2))

(define-g-enum "GtkProgressBarOrientation"
    progress-bar-orientation
    (:export t :type-initializer "gtk_progress_bar_orientation_get_type")
  (:left-to-right 0)
  (:right-to-left 1)
  (:bottom-to-top 2)
  (:top-to-bottom 3))

(define-g-enum "GtkProgressBarStyle"
    progress-bar-style
    (:export t :type-initializer "gtk_progress_bar_style_get_type")
  (:continuous 0)
  (:discrete 1))

(define-g-enum "GtkUpdateType"
    update-type
    (:export t :type-initializer "gtk_update_type_get_type")
  (:continuous 0)
  (:discontinuous 1)
  (:delayed 2))

(define-g-enum "GtkMetricType"
    metric-type
    (:export t :type-initializer "gtk_metric_type_get_type")
  (:pixels 0)
  (:inches 1)
  (:centimeters 2))

(define-g-enum "GtkSpinButtonUpdatePolicy"
    spin-button-update-policy
    (:export t :type-initializer "gtk_spin_button_update_policy_get_type")
  (:always 0)
  (:if-valid 1))

(define-g-enum "GtkCurveType"
    curve-type
    (:export t :type-initializer "gtk_curve_type_get_type")
  (:linear 0)
  (:spline 1)
  (:free 2))









(define-g-enum "GtkWrapMode"
    wrap-mode
    (:export t :type-initializer "gtk_wrap_mode_get_type")
  (:none 0)
  (:char 1)
  (:word 2)
  (:word-char 3))







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



(define-g-enum "GtkIconSize"
    icon-size
    (:export t :type-initializer "gtk_icon_size_get_type")
  (:invalid 0)
  (:menu 1)
  (:small-toolbar 2)
  (:large-toolbar 3)
  (:button 4)
  (:dnd 5)
  (:dialog 6))







(define-g-enum "GtkMessageType"
    message-type
    (:export t :type-initializer "gtk_message_type_get_type")
  (:info 0)
  (:warning 1)
  (:question 2)
  (:error 3)
  (:other 4))

(define-g-enum "GtkButtonsType"
    buttons-type
    (:export t :type-initializer "gtk_buttons_type_get_type")
  (:none 0)
  (:ok 1)
  (:close 2)
  (:cancel 3)
  (:yes-no 4)
  (:ok-cancel 5))







(define-g-enum "GtkTextBufferTargetInfo"
    text-buffer-target-info
    (:export t :type-initializer "gtk_text_buffer_target_info_get_type")
  (:buffer-contents -1)
  (:rich-text -2)
  (:text -3))











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



(define-g-enum "GtkBuilderError"
    builder-error
    (:export t :type-initializer "gtk_builder_error_get_type")
  (:invalid-type-function 0)
  (:unhandled-tag 1)
  (:missing-attribute 2)
  (:invalid-attribute 3)
  (:invalid-tag 4)
  (:missing-property-value 5)
  (:invalid-value 6)
  (:version-mismatch 7)
  (:duplicate-id 8))

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

(define-g-enum "GtkIconViewDropPosition"
    icon-view-drop-position
    (:export t :type-initializer "gtk_icon_view_drop_position_get_type")
  (:no-drop 0)
  (:drop-into 1)
  (:drop-left 2)
  (:drop-right 3)
  (:drop-above 4)
  (:drop-below 5))





(define-g-enum "GtkMatchType"
    match-type
    (:export t :type-initializer "gtk_match_type_get_type")
  (:all 0)
  (:all-tail 1)
  (:head 2)
  (:tail 3)
  (:exact 4)
  (:last 5))

(define-g-enum "GtkMenuDirectionType"
    menu-direction-type
    (:export t :type-initializer "gtk_menu_direction_type_get_type")
  (:parent 0)
  (:child 1)
  (:next 2)
  (:prev 3))

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

(define-g-enum "GtkRecentChooserError"
    recent-chooser-error
    (:export t :type-initializer "gtk_recent_chooser_error_get_type")
  (:not-found 0)
  (:invalid-uri 1))

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

;(define-g-enum "GtkResponseType"
;    response-type
;    (:export t :type-initializer "gtk_response_type_get_type")
;  (:none -1)
;  (:reject -2)
;  (:accept -3)
;  (:delete-event -4)
;  (:ok -5)
;  (:cancel -6)
;  (:close -7)
;  (:yes -8)
;  (:no -9)
;  (:apply -10)
;  (:help -11))





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

(define-g-enum "GtkTextWindowType"
    text-window-type
    (:export t :type-initializer "gtk_text_window_type_get_type")
  (:private 0)
  (:widget 1)
  (:text 2)
  (:left 3)
  (:right 4)
  (:top 5)
  (:bottom 6))

(define-g-enum "GtkToolbarChildType"
    toolbar-child-type
    (:export t :type-initializer "gtk_toolbar_child_type_get_type")
  (:space 0)
  (:button 1)
  (:togglebutton 2)
  (:radiobutton 3)
  (:widget 4))

(define-g-enum "GtkToolbarSpaceStyle"
    toolbar-space-style
    (:export t :type-initializer "gtk_toolbar_space_style_get_type")
  (:empty 0)
  (:line 1))

(define-g-enum "GtkTreeViewDropPosition"
    tree-view-drop-position
    (:export t :type-initializer "gtk_tree_view_drop_position_get_type")
  (:before 0)
  (:after 1)
  (:into-or-before 2)
  (:into-or-after 3))

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

(define-g-flags "GtkTextSearchFlags"
    text-search-flags
    (:export t :type-initializer "gtk_text_search_flags_get_type")
  (:visible-only 1)
  (:text-only 2))



(define-g-flags "GtkArgFlags"
    arg-flags
    (:export t :type-initializer "gtk_arg_flags_get_type")
  (:readable 1)
  (:writable 2)
  (:construct 4)
  (:construct-only 8)
  (:child-arg 16))



(define-g-flags "GtkButtonAction"
    button-action
    (:export t :type-initializer "gtk_button_action_get_type")
  (:ignored 0)
  (:selects 1)
  (:drags 2)
  (:expands 4))

(define-g-flags "GtkCalendarDisplayOptions"
    calendar-display-options
    (:export t :type-initializer "gtk_calendar_display_options_get_type")
  (:show-heading 1)
  (:show-day-names 2)
  (:no-month-change 4)
  (:show-week-numbers 8)
  (:week-start-monday 16)
  (:show-details 32))

(define-g-flags "GtkCellRendererState"
    cell-renderer-state
    (:export t :type-initializer "gtk_cell_renderer_state_get_type")
  (:selected 1)
  (:prelit 2)
  (:insensitive 4)
  (:sorted 8)
  (:focused 16))

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

(define-g-flags "GtkDestDefaults"
    dest-defaults
    (:export t :type-initializer "gtk_dest_defaults_get_type")
  (:motion 1)
  (:highlight 2)
  (:drop 4)
  (:all 7))

;(define-g-flags "GtkDialogFlags"
;    dialog-flags
;    (:export t :type-initializer "gtk_dialog_flags_get_type")
;  (:modal 1)
;  (:destroy-with-parent 2)
;  (:no-separator 4))

(define-g-flags "GtkFileFilterFlags"
    file-filter-flags
    (:export t :type-initializer "gtk_file_filter_flags_get_type")
  (:filename 1)
  (:uri 2)
  (:display-name 4)
  (:mime-type 8))

(define-g-flags "GtkIconLookupFlags"
    icon-lookup-flags
    (:export t :type-initializer "gtk_icon_lookup_flags_get_type")
  (:no-svg 1)
  (:force-svg 2)
  (:use-builtin 4)
  (:generic-fallback 8)
  (:force-size 16))

(define-g-flags "GtkObjectFlags"
    object-flags
    (:export t :type-initializer "gtk_object_flags_get_type")
  (:in-destruction 1)
  (:floating 2)
  (:reserved-1 4)
  (:reserved-2 8))

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

(define-g-flags "GtkRecentFilterFlags"
    recent-filter-flags
    (:export t :type-initializer "gtk_recent_filter_flags_get_type")
  (:uri 1)
  (:display-name 2)
  (:mime-type 4)
  (:application 8)
  (:group 16)
  (:age 32))

(define-g-flags "GtkSignalRunType"
    signal-run-type
    (:export t :type-initializer "gtk_signal_run_type_get_type")
  (:first 1)
  (:last 2)
  (:both 3)
  (:no-recurse 8)
  (:action 32)
  (:no-hooks 64))

(define-g-flags "GtkTargetFlags"
    target-flags
    (:export t :type-initializer "gtk_target_flags_get_type")
  (:same-app 1)
  (:same-widget 2)
  (:other-app 4)
  (:other-widget 8))

(define-g-flags "GtkTreeModelFlags"
    tree-model-flags
    (:export t :type-initializer "gtk_tree_model_flags_get_type")
  (:iters-persist 1)
  (:list-only 2))

(define-g-flags "GtkUIManagerItemType"
    ui-manager-item-type
    (:export t)
  (:auto 0)
  (:menubar 1)
  (:menu 2)
  (:toolbar 4)
  (:placeholder 8)
  (:popup 16)
  (:menuitem 32)
  (:toolitem 64)
  (:separator 128)
  (:accelerator 256)
  (:popup-with-accels 512))

(define-g-flags "GtkWidgetFlags"
    widget-flags
    (:export t :type-initializer "gtk_widget_flags_get_type")
  (:toplevel 16)
  (:no-window 32)
  (:realized 64)
  (:mapped 128)
  (:visible 256)
  (:sensitive 512)
  (:parent-sensitive 1024)
  (:can-focus 2048)
  (:has-focus 4096)
  (:can-default 8192)
  (:has-default 16384)
  (:has-grab 32768)
  (:rc-style 65536)
  (:composite-child 131072)
  (:no-reparent 262144)
  (:app-paintable 524288)
  (:receives-default 1048576)
  (:double-buffered 2097152)
  (:no-show-all 4194304))

(define-g-interface "GtkBuildable"
    buildable
    (:export t :type-initializer "gtk_buildable_get_type"))

(define-g-interface "GtkCellEditable"
    cell-editable
    (:export t :type-initializer "gtk_cell_editable_get_type"))

(define-g-interface "GtkCellLayout"
    cell-layout
    (:export t :type-initializer "gtk_cell_layout_get_type"))

(define-g-interface "GtkEditable"
    editable
    (:export t :type-initializer "gtk_editable_get_type")
  (:cffi position editable-position :int "gtk_editable_get_position"
   "gtk_editable_set_position")
  (:cffi editable editable-editable :boolean "gtk_editable_get_editable"
   "gtk_editable_set_editable"))

(define-g-interface "GtkFileChooser"
    file-chooser
    (:export t :type-initializer "gtk_file_chooser_get_type")
  (action file-chooser-action "action" "GtkFileChooserAction" t t)
  (do-overwrite-confirmation file-chooser-do-overwrite-confirmation
   "do-overwrite-confirmation" "gboolean" t t)
  (extra-widget file-chooser-extra-widget "extra-widget" "GtkWidget" t t)
  (file-system-backend file-chooser-file-system-backend "file-system-backend"
   "gchararray" nil nil)
  (filter file-chooser-filter "filter" "GtkFileFilter" t t)
  (local-only file-chooser-local-only "local-only" "gboolean" t t)
  (preview-widget file-chooser-preview-widget "preview-widget" "GtkWidget" t t)
  (preview-widget-active file-chooser-preview-widget-active
   "preview-widget-active" "gboolean" t t)
  (select-multiple file-chooser-select-multiple "select-multiple" "gboolean" t
   t)
  (show-hidden file-chooser-show-hidden "show-hidden" "gboolean" t t)
  (use-preview-label file-chooser-use-preview-label "use-preview-label"
   "gboolean" t t)
  (:cffi current-name file-chooser-current-name
   (:string :free-to-foreign t :encoding :utf-8) nil
   "gtk_file_chooser_set_current_name")
  #+win32
  (:cffi filename file-chooser-filename
   (g-string :free-from-foreign t :free-to-foreign t)
   "gtk_file_chooser_get_filename_utf8" "gtk_file_chooser_set_filename_utf8")
  #-win32
  (:cffi filename file-chooser-filename
   (g-string :free-from-foreign t :free-to-foreign t)
   "gtk_file_chooser_get_filename" "gtk_file_chooser_set_filename")
  #+win32
  (:cffi current-folder file-chooser-current-folder
   (g-string :free-from-foreign t :free-to-foreign t)
   "gtk_file_chooser_get_current_folder_utf8"
   "gtk_file_chooser_set_current_folder_utf8")
  #-win32
  (:cffi current-folder file-chooser-current-folder
   (g-string :free-from-foreign t :free-to-foreign t)
   "gtk_file_chooser_get_current_folder" "gtk_file_chooser_set_current_folder")
  (:cffi uri file-chooser-uri
   (g-string :free-from-foreign t :free-to-foreign t)
   "gtk_file_chooser_get_uri" "gtk_file_chooser_set_uri")
  (:cffi current-folder-uri file-chooser-current-folder-uri
   (g-string :free-from-foreign t :free-to-foreign t)
   "gtk_file_chooser_get_current_folder_uri"
   "gtk_file_chooser_set_current_folder_uri")
  #+win32
  (:cffi preview-filename file-chooser-preview-filename
   (g-string :free-from-foreign t :free-to-foreign t)
   "gtk_file_chooser_get_preview_filename_utf8" nil)
  #-win32
  (:cffi preview-filename file-chooser-preview-filename
   (g-string :free-from-foreign t :free-to-foreign t)
   "gtk_file_chooser_get_preview_filename" nil)
  (:cffi preview-uri file-chooser-preview-uri
   (g-string :free-from-foreign t :free-to-foreign t)
   "gtk_file_chooser_get_preview_uri" nil))

(define-g-interface "GtkFileChooserEmbed"
    file-chooser-embed
    (:export t))

(define-g-interface "GtkTreeModel"
    tree-model
    (:export t :type-initializer "gtk_tree_model_get_type"))

(define-g-interface "GtkTreeDragSource"
    tree-drag-source
    (:export t :type-initializer "gtk_tree_drag_source_get_type"))

(define-g-interface "GtkTreeDragDest"
    tree-drag-dest
    (:export t :type-initializer "gtk_tree_drag_dest_get_type"))

(define-g-interface "GtkTreeSortable"
    tree-sortable
    (:export t :type-initializer "gtk_tree_sortable_get_type"))

(define-g-interface "GtkPrintOperationPreview"
    print-operation-preview
    (:export t :type-initializer "gtk_print_operation_preview_get_type"))

(define-g-interface "GtkRecentChooser"
    recent-chooser
    (:export t :type-initializer "gtk_recent_chooser_get_type")
  (filter recent-chooser-filter "filter" "GtkRecentFilter" t t)
  (limit recent-chooser-limit "limit" "gint" t t)
  (local-only recent-chooser-local-only "local-only" "gboolean" t t)
  (recent-manager recent-chooser-recent-manager "recent-manager"
   "GtkRecentManager" nil nil)
  (select-multiple recent-chooser-select-multiple "select-multiple" "gboolean"
   t t)
  (show-icons recent-chooser-show-icons "show-icons" "gboolean" t t)
  (show-not-found recent-chooser-show-not-found "show-not-found" "gboolean" t t)
  (show-private recent-chooser-show-private "show-private" "gboolean" t t)
  (show-tips recent-chooser-show-tips "show-tips" "gboolean" t t)
  (sort-type recent-chooser-sort-type "sort-type" "GtkRecentSortType" t t))

(define-g-interface "GtkToolShell"
    tool-shell
    (:export t :type-initializer "gtk_tool_shell_get_type")
  (:cffi icon-size tool-shell-icon-size icon-size
   "gtk_tool_shell_get_icon_size" nil)
  (:cffi orientation tool-shell-orientation gtk-orientation
   "gtk_tool_shell_get_orientation" nil)
  (:cffi relief-style tool-shell-relief-style gtk-relief-style
   "gtk_tool_shell_get_relief_style" nil)
  (:cffi style tool-shell-style gtk-toolbar-style "gtk_tool_shell_get_style" nil))

(define-g-interface "GtkOrientable"
    orientable
    (:export t :type-initializer "gtk_orientable_get_type")
  (orientation orientable-orientation "orientation" "GtkOrientation" t t))

(define-g-interface "GtkActivatable"
    activatable
    (:export t :type-initializer "gtk_activatable_get_type")
  (related-action activatable-related-action "related-action" "GtkAction" t t)
  (use-action-appearance activatable-use-action-appearance
   "use-action-appearance" "gboolean" t t))

(define-g-interface "AtkImplementorIface"
    atk-implementor-iface
    (:export t))

(define-g-object-class "GtkObject" gtk-object
                       (:superclass g-initially-unowned :export t :interfaces
                        nil :type-initializer "gtk_object_get_type")
                       ((user-data gtk-object-user-data "user-data" "gpointer"
                         t t)))



(define-g-object-class "GtkCellRenderer" cell-renderer
                       (:superclass gtk-object :export t :interfaces nil
                        :type-initializer "gtk_cell_renderer_get_type")
                       ((cell-background cell-renderer-cell-background
                         "cell-background" "gchararray" nil t)
                        (cell-background-gdk cell-renderer-cell-background-gdk
                         "cell-background-gdk" "GdkColor" t t)
                        (cell-background-set cell-renderer-cell-background-set
                         "cell-background-set" "gboolean" t t)
                        (editing cell-renderer-editing "editing" "gboolean" t
                         nil)
                        (height cell-renderer-height "height" "gint" t t)
                        (is-expanded cell-renderer-is-expanded "is-expanded"
                         "gboolean" t t)
                        (is-expander cell-renderer-is-expander "is-expander"
                         "gboolean" t t)
                        (mode cell-renderer-mode "mode" "GtkCellRendererMode" t
                         t)
                        (sensitive cell-renderer-sensitive "sensitive"
                         "gboolean" t t)
                        (visible cell-renderer-visible "visible" "gboolean" t
                         t)
                        (width cell-renderer-width "width" "gint" t t)
                        (xalign cell-renderer-xalign "xalign" "gfloat" t t)
                        (xpad cell-renderer-xpad "xpad" "guint" t t)
                        (yalign cell-renderer-yalign "yalign" "gfloat" t t)
                        (ypad cell-renderer-ypad "ypad" "guint" t t)))

(define-g-object-class "GtkCellRendererPixbuf" cell-renderer-pixbuf
                       (:superclass cell-renderer :export t :interfaces nil
                        :type-initializer "gtk_cell_renderer_pixbuf_get_type")
                       ((follow-state cell-renderer-pixbuf-follow-state
                         "follow-state" "gboolean" t t)
                        (gicon cell-renderer-pixbuf-gicon "gicon" "GIcon" t t)
                        (icon-name cell-renderer-pixbuf-icon-name "icon-name"
                         "gchararray" t t)
                        (pixbuf cell-renderer-pixbuf-pixbuf "pixbuf"
                         "GdkPixbuf" t t)
                        (pixbuf-expander-closed
                         cell-renderer-pixbuf-pixbuf-expander-closed
                         "pixbuf-expander-closed" "GdkPixbuf" t t)
                        (pixbuf-expander-open
                         cell-renderer-pixbuf-pixbuf-expander-open
                         "pixbuf-expander-open" "GdkPixbuf" t t)
                        (stock-detail cell-renderer-pixbuf-stock-detail
                         "stock-detail" "gchararray" t t)
                        (stock-id cell-renderer-pixbuf-stock-id "stock-id"
                         "gchararray" t t)
                        (stock-size cell-renderer-pixbuf-stock-size
                         "stock-size" "guint" t t)))

(define-g-object-class "GtkCellRendererProgress" cell-renderer-progress
                       (:superclass cell-renderer :export t :interfaces nil
                        :type-initializer
                        "gtk_cell_renderer_progress_get_type")
                       ((orientation cell-renderer-progress-orientation
                         "orientation" "GtkProgressBarOrientation" t t)
                        (pulse cell-renderer-progress-pulse "pulse" "gint" t t)
                        (text cell-renderer-progress-text "text" "gchararray" t
                         t)
                        (text-xalign cell-renderer-progress-text-xalign
                         "text-xalign" "gfloat" t t)
                        (text-yalign cell-renderer-progress-text-yalign
                         "text-yalign" "gfloat" t t)
                        (value cell-renderer-progress-value "value" "gint" t
                         t)))

(define-g-object-class "GtkCellRendererText" cell-renderer-text
                       (:superclass cell-renderer :export t :interfaces nil
                        :type-initializer "gtk_cell_renderer_text_get_type")
                       ((align-set cell-renderer-text-align-set "align-set"
                         "gboolean" t t)
                        (alignment cell-renderer-text-alignment "alignment"
                         "PangoAlignment" t t)
                        (attributes cell-renderer-text-attributes "attributes"
                         "PangoAttrList" t t)
                        (background cell-renderer-text-background "background"
                         "gchararray" nil t)
                        (background-gdk cell-renderer-text-background-gdk
                         "background-gdk" "GdkColor" t t)
                        (background-set cell-renderer-text-background-set
                         "background-set" "gboolean" t t)
                        (editable cell-renderer-text-editable "editable"
                         "gboolean" t t)
                        (editable-set cell-renderer-text-editable-set
                         "editable-set" "gboolean" t t)
                        (ellipsize cell-renderer-text-ellipsize "ellipsize"
                         "PangoEllipsizeMode" t t)
                        (ellipsize-set cell-renderer-text-ellipsize-set
                         "ellipsize-set" "gboolean" t t)
                        (family cell-renderer-text-family "family" "gchararray"
                         t t)
                        (family-set cell-renderer-text-family-set "family-set"
                         "gboolean" t t)
                        (font cell-renderer-text-font "font" "gchararray" t t)
                        (font-desc cell-renderer-text-font-desc "font-desc"
                         "PangoFontDescription" t t)
                        (foreground cell-renderer-text-foreground "foreground"
                         "gchararray" nil t)
                        (foreground-gdk cell-renderer-text-foreground-gdk
                         "foreground-gdk" "GdkColor" t t)
                        (foreground-set cell-renderer-text-foreground-set
                         "foreground-set" "gboolean" t t)
                        (language cell-renderer-text-language "language"
                         "gchararray" t t)
                        (language-set cell-renderer-text-language-set
                         "language-set" "gboolean" t t)
                        (markup cell-renderer-text-markup "markup" "gchararray"
                         nil t)
                        (rise cell-renderer-text-rise "rise" "gint" t t)
                        (rise-set cell-renderer-text-rise-set "rise-set"
                         "gboolean" t t)
                        (scale cell-renderer-text-scale "scale" "gdouble" t t)
                        (scale-set cell-renderer-text-scale-set "scale-set"
                         "gboolean" t t)
                        (single-paragraph-mode
                         cell-renderer-text-single-paragraph-mode
                         "single-paragraph-mode" "gboolean" t t)
                        (size cell-renderer-text-size "size" "gint" t t)
                        (size-points cell-renderer-text-size-points
                         "size-points" "gdouble" t t)
                        (size-set cell-renderer-text-size-set "size-set"
                         "gboolean" t t)
                        (stretch cell-renderer-text-stretch "stretch"
                         "PangoStretch" t t)
                        (stretch-set cell-renderer-text-stretch-set
                         "stretch-set" "gboolean" t t)
                        (strikethrough cell-renderer-text-strikethrough
                         "strikethrough" "gboolean" t t)
                        (strikethrough-set cell-renderer-text-strikethrough-set
                         "strikethrough-set" "gboolean" t t)
                        (style cell-renderer-text-style "style" "PangoStyle" t
                         t)
                        (style-set cell-renderer-text-style-set "style-set"
                         "gboolean" t t)
                        (text cell-renderer-text-text "text" "gchararray" t t)
                        (underline cell-renderer-text-underline "underline"
                         "PangoUnderline" t t)
                        (underline-set cell-renderer-text-underline-set
                         "underline-set" "gboolean" t t)
                        (variant cell-renderer-text-variant "variant"
                         "PangoVariant" t t)
                        (variant-set cell-renderer-text-variant-set
                         "variant-set" "gboolean" t t)
                        (weight cell-renderer-text-weight "weight" "gint" t t)
                        (weight-set cell-renderer-text-weight-set "weight-set"
                         "gboolean" t t)
                        (width-chars cell-renderer-text-width-chars
                         "width-chars" "gint" t t)
                        (wrap-mode cell-renderer-text-wrap-mode "wrap-mode"
                         "PangoWrapMode" t t)
                        (wrap-width cell-renderer-text-wrap-width "wrap-width"
                         "gint" t t)))

(define-g-object-class "GtkCellRendererAccel" cell-renderer-accel
                       (:superclass cell-renderer-text :export t :interfaces
                        nil :type-initializer
                        "gtk_cell_renderer_accel_get_type")
                       ((accel-key cell-renderer-accel-accel-key "accel-key"
                         "guint" t t)
                        (accel-mode cell-renderer-accel-accel-mode "accel-mode"
                         "GtkCellRendererAccelMode" t t)
                        (accel-mods cell-renderer-accel-accel-mods "accel-mods"
                         "GdkModifierType" t t)
                        (keycode cell-renderer-accel-keycode "keycode" "guint"
                         t t)))

(define-g-object-class "GtkCellRendererCombo" cell-renderer-combo
                       (:superclass cell-renderer-text :export t :interfaces
                        nil :type-initializer
                        "gtk_cell_renderer_combo_get_type")
                       ((has-entry cell-renderer-combo-has-entry "has-entry"
                         "gboolean" t t)
                        (model cell-renderer-combo-model "model" "GtkTreeModel"
                         t t)
                        (text-column cell-renderer-combo-text-column
                         "text-column" "gint" t t)))

(define-g-object-class "GtkCellRendererSpin" cell-renderer-spin
                       (:superclass cell-renderer-text :export t :interfaces
                        nil :type-initializer
                        "gtk_cell_renderer_spin_get_type")
                       ((adjustment cell-renderer-spin-adjustment "adjustment"
                         "GtkAdjustment" t t)
                        (climb-rate cell-renderer-spin-climb-rate "climb-rate"
                         "gdouble" t t)
                        (digits cell-renderer-spin-digits "digits" "guint" t
                         t)))

(define-g-object-class "GtkCellRendererToggle" cell-renderer-toggle
                       (:superclass cell-renderer :export t :interfaces nil
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

(define-g-object-class "GtkFileFilter" file-filter
                       (:superclass gtk-object :export t :interfaces nil
                        :type-initializer "gtk_file_filter_get_type")
                       ((:cffi name file-filter-name :string
                         "gtk_file_filter_get_name"
                         "gtk_file_filter_set_name")))

(define-g-object-class "GtkRecentFilter" recent-filter
                       (:superclass gtk-object :export t :interfaces nil
                        :type-initializer "gtk_recent_filter_get_type")
                       nil)

(define-g-object-class "GtkTreeViewColumn" tree-view-column
                       (:superclass gtk-object :export t :interfaces
                        ("GtkBuildable" "GtkCellLayout") :type-initializer
                        "gtk_tree_view_column_get_type")
                       ((alignment tree-view-column-alignment "alignment"
                         "gfloat" t t)
                        (clickable tree-view-column-clickable "clickable"
                         "gboolean" t t)
                        (expand tree-view-column-expand "expand" "gboolean" t
                         t)
                        (fixed-width tree-view-column-fixed-width "fixed-width"
                         "gint" t t)
                        (max-width tree-view-column-max-width "max-width"
                         "gint" t t)
                        (min-width tree-view-column-min-width "min-width"
                         "gint" t t)
                        (reorderable tree-view-column-reorderable "reorderable"
                         "gboolean" t t)
                        (resizable tree-view-column-resizable "resizable"
                         "gboolean" t t)
                        (sizing tree-view-column-sizing "sizing"
                         "GtkTreeViewColumnSizing" t t)
                        (sort-indicator tree-view-column-sort-indicator
                         "sort-indicator" "gboolean" t t)
                        (sort-order tree-view-column-sort-order "sort-order"
                         "GtkSortType" t t)
                        (spacing tree-view-column-spacing "spacing" "gint" t t)
                        (title tree-view-column-title "title" "gchararray" t t)
                        (visible tree-view-column-visible "visible" "gboolean"
                         t t)
                        (widget tree-view-column-widget "widget" "GtkWidget" t
                         t)
                        (width tree-view-column-width "width" "gint" t nil)
                        (:cffi tree-view tree-view-column-tree-view g-object
                         "gtk_tree_view_column_get_tree_view" nil)
                        (:cffi sort-column-id tree-view-column-sort-column-id
                         :int "gtk_tree_view_column_get_sort_column_id"
                         "gtk_tree_view_column_set_sort_column_id")
                        (:cffi cell-renderers tree-view-column-cell-renderers
                         (g-list g-object :free-from-foreign t)
                         "gtk_tree_view_column_get_cell_renderers" nil)))

(define-g-object-class "GtkWidget" widget
                       (:superclass gtk-object :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_widget_get_type")
                       ((app-paintable widget-app-paintable "app-paintable"
                         "gboolean" t t)
                        (can-default widget-can-default "can-default"
                         "gboolean" t t)
                        (can-focus widget-can-focus "can-focus" "gboolean" t t)
                        (composite-child widget-composite-child
                         "composite-child" "gboolean" t nil)
                        (events widget-events "events" "GdkEventMask" t t)
                        (extension-events widget-extension-events
                         "extension-events" "GdkExtensionMode" t t)
                        (has-default widget-has-default "has-default"
                         "gboolean" t t)
                        (has-focus widget-has-focus "has-focus" "gboolean" t t)
                        (has-tooltip widget-has-tooltip "has-tooltip"
                         "gboolean" t t)
                        (height-request widget-height-request "height-request"
                         "gint" t t)
                        (is-focus widget-is-focus "is-focus" "gboolean" t t)
                        (name widget-name "name" "gchararray" t t)
                        (no-show-all widget-no-show-all "no-show-all"
                         "gboolean" t t)
                        (parent widget-parent "parent" "GtkContainer" t t)
                        (receives-default widget-receives-default
                         "receives-default" "gboolean" t t)
                        (sensitive widget-sensitive "sensitive" "gboolean" t t)
                        (style widget-style "style" "GtkStyle" t t)
                        (tooltip-markup widget-tooltip-markup "tooltip-markup"
                         "gchararray" t t)
                        (tooltip-text widget-tooltip-text "tooltip-text"
                         "gchararray" t t)
                        (visible widget-visible "visible" "gboolean" t t)
                        (width-request widget-width-request "width-request"
                         "gint" t t)
                        (window widget-window "window" "GdkWindow" t nil)
                        (:cffi parent-window widget-parent-window
                         (g-object gdk-window) "gtk_widget_get_parent_window"
                         "gtk_widget_set_parent_window")
                        (:cffi toplevel widget-toplevel (g-object widget)
                         "gtk_widget_get_toplevel" nil)
                        (:cffi colormap widget-colormap (g-object gdk-colormap)
                         "gtk_widget_get_colormap" "gtk_widget_set_colormap")
                        (:cffi visual widget-visual (g-object gdk-visual)
                         "gtk_widget_get_visual" nil)
                        (:cffi modifier-style widget-modifier-style
                         (g-object rc-style) "gtk_widget_get_modifier_style"
                         "gtk_widget_modify_style")
                        (:cffi pango-context widget-pango-context g-object
                         "gtk_widget_get_pango_context" nil)
                        (:cffi child-visible widget-child-visible :boolean
                         "gtk_widget_get_child_visible"
                         "gtk_widget_set_child_visible")
                        (:cffi direction widget-direction text-direction
                         "gtk_widget_get_direction" "gtk_widget_set_direction")
                        (:cffi composite-name widget-composite-name
                         (g-string :free-from-foreign t :free-to-foreign t)
                         "gtk_widget_get_composite_name"
                         "gtk_widget_set_composite_name")
                        (:cffi redraw-on-allocate widget-redraw-on-allocate
                         :boolean nil "gtk_widget_set_redraw_on_allocate")
                        (:cffi accessible widget-accessible g-object
                         "gtk_widget_get_accessible" nil)
                        (:cffi tooltip-window widget-tooltip-window g-object
                         "gtk_widget_get_tooltip_window"
                         "gtk_widget_set_tooltip_window")))

(define-g-object-class "GtkCalendar" calendar
                       (:superclass widget :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_calendar_get_type")
                       ((day calendar-day "day" "gint" t t)
                        (detail-height-rows calendar-detail-height-rows
                         "detail-height-rows" "gint" t t)
                        (detail-width-chars calendar-detail-width-chars
                         "detail-width-chars" "gint" t t)
                        (month calendar-month "month" "gint" t t)
                        (no-month-change calendar-no-month-change
                         "no-month-change" "gboolean" t t)
                        (show-day-names calendar-show-day-names
                         "show-day-names" "gboolean" t t)
                        (show-details calendar-show-details "show-details"
                         "gboolean" t t)
                        (show-heading calendar-show-heading "show-heading"
                         "gboolean" t t)
                        (show-week-numbers calendar-show-week-numbers
                         "show-week-numbers" "gboolean" t t)
                        (year calendar-year "year" "gint" t t)
                        (:cffi detail-function calendar-detail-function nil nil
                         calendar-set-detail-function)
                        (:cffi display-options calendar-display-options
                         calendar-display-options
                         "gtk_calendar_get_display_options"
                         "gtk_calendar_set_display_options")))

(define-g-object-class "GtkCellView" cell-view
                       (:superclass widget :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkCellLayout")
                        :type-initializer "gtk_cell_view_get_type")
                       ((background cell-view-background "background"
                         "gchararray" nil t)
                        (background-gdk cell-view-background-gdk
                         "background-gdk" "GdkColor" t t)
                        (background-set cell-view-background-set
                         "background-set" "gboolean" t t)
                        (model cell-view-model "model" "GtkTreeModel" t t)
                        (:cffi displayed-row cell-view-displayed-row
                         (g-boxed-foreign tree-path)
                         "gtk_cell_view_get_displayed_row"
                         "gtk_cell_view_set_displayed_row")
                        (:cffi cell-renderers cell-view-cell-renderers
                         (g-list (g-object cell-renderer) :free-from-foreign t)
                         "gtk_cell_view_get_cell_renderers" nil)))

(define-g-object-class "GtkContainer" container
                       (:superclass widget :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_container_get_type")
                       ((border-width container-border-width "border-width"
                         "guint" t t)
                        (child container-child "child" "GtkWidget" nil t)
                        (resize-mode container-resize-mode "resize-mode"
                         "GtkResizeMode" t t)
                        (:cffi focus-child container-focus-child g-object
                         "gtk_container_get_focus_child"
                         "gtk_container_set_focus_child")
                        (:cffi focus-vadjustment container-focus-vadjustment
                         g-object "gtk_container_get_focus_vadjustment"
                         "gtk_container_set_focus_vadjustment")
                        (:cffi focus-hadjustment container-focus-hadjustment
                         g-object "gtk_container_get_focus_hadjustment"
                         "gtk_container_set_focus_hadjustment")
                        (:cffi reallocate-redraws container-reallocate-redraws
                         :boolean nil "gtk_container_set_reallocate_redraws")))

(define-g-object-class "GtkBin" bin
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_bin_get_type")
                       ((:cffi child bin-child (g-object widget)
                         "gtk_bin_get_child" nil)))

(define-g-object-class "GtkAlignment" alignment
                       (:superclass bin :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_alignment_get_type")
                       ((bottom-padding alignment-bottom-padding
                         "bottom-padding" "guint" t t)
                        (left-padding alignment-left-padding "left-padding"
                         "guint" t t)
                        (right-padding alignment-right-padding "right-padding"
                         "guint" t t)
                        (top-padding alignment-top-padding "top-padding"
                         "guint" t t)
                        (xalign alignment-xalign "xalign" "gfloat" t t)
                        (xscale alignment-xscale "xscale" "gfloat" t t)
                        (yalign alignment-yalign "yalign" "gfloat" t t)
                        (yscale alignment-yscale "yscale" "gfloat" t t)))


(define-g-object-class "GtkColorButton" color-button
                       (:superclass gtk-button :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_color_button_get_type")
                       ((alpha color-button-alpha "alpha" "guint" t t)
                        (color color-button-color "color" "GdkColor" t t)
                        (title color-button-title "title" "gchararray" t t)
                        (use-alpha color-button-use-alpha "use-alpha"
                         "gboolean" t t)))

(define-g-object-class "GtkFontButton" font-button
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

(define-g-object-class "GtkLinkButton" link-button
                       (:superclass gtk-button :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_link_button_get_type")
                       ((uri link-button-uri "uri" "gchararray" t t)
                        (visited link-button-visited "visited" "gboolean" t t)))

(define-g-object-class "GtkScaleButton" scale-button
                       (:superclass gtk-button :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable"
                         "GtkOrientable")
                        :type-initializer "gtk_scale_button_get_type")
                       ((adjustment scale-button-adjustment "adjustment"
                         "GtkAdjustment" t t)
                        (icons scale-button-icons "icons" "GStrv" t t)
                        (size scale-button-size "size" "GtkIconSize" t t)
                        (value scale-button-value "value" "gdouble" t t)))

(define-g-object-class "GtkVolumeButton" volume-button
                       (:superclass scale-button :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable"
                         "GtkOrientable")
                        :type-initializer "gtk_volume_button_get_type")
                       nil)



(define-g-object-class "GtkCheckButton" check-button
  (:superclass gtk-toggle-button
   :export t
   :interfaces ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
   :type-initializer "gtk_check_button_get_type")
  nil)



(define-g-object-class "GtkComboBox" combo-box
                       (:superclass bin :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                         "GtkCellLayout")
                        :type-initializer "gtk_combo_box_get_type")
                       ((active combo-box-active "active" "gint" t t)
                        (add-tearoffs combo-box-add-tearoffs "add-tearoffs"
                         "gboolean" t t)
                        (button-sensitivity combo-box-button-sensitivity
                         "button-sensitivity" "GtkSensitivityType" t t)
                        (column-span-column combo-box-column-span-column
                         "column-span-column" "gint" t t)
                        (focus-on-click combo-box-focus-on-click
                         "focus-on-click" "gboolean" t t)
                        (has-frame combo-box-has-frame "has-frame" "gboolean" t
                         t)
                        (model combo-box-model "model" "GtkTreeModel" t t)
                        (popup-shown combo-box-popup-shown "popup-shown"
                         "gboolean" t nil)
                        (row-span-column combo-box-row-span-column
                         "row-span-column" "gint" t t)
                        (tearoff-title combo-box-tearoff-title "tearoff-title"
                         "gchararray" t t)
                        (wrap-width combo-box-wrap-width "wrap-width" "gint" t
                         t)
                        (:cffi active-iter combo-box-active-iter
                         (g-boxed-foreign tree-iter) combo-box-get-active-iter
                         "gtk_combo_box_set_active_iter")
                        (:cffi row-separator-func combo-box-separator-func nil
                         nil combo-box-set-separator-func)
                        (:cffi title combo-box-title
                         (:string :free-from-foreign nil :free-to-foreign t)
                         "gtk_combo_box_get_title" "gtk_combo_box_set_title")))

(define-g-object-class "GtkComboBoxEntry" combo-box-entry
                       (:superclass combo-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                         "GtkCellLayout")
                        :type-initializer "gtk_combo_box_entry_get_type")
                       ((text-column combo-box-entry-text-column "text-column"
                         "gint" t t)))

(define-g-object-class "GtkEventBox" event-box
                       (:superclass bin :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_event_box_get_type")
                       ((above-child event-box-above-child "above-child"
                         "gboolean" t t)
                        (visible-window event-box-visible-window
                         "visible-window" "gboolean" t t)))

(define-g-object-class "GtkExpander" expander
                       (:superclass bin :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_expander_get_type")
                       ((expanded expander-expanded "expanded" "gboolean" t t)
                        (label expander-label "label" "gchararray" t t)
                        (label-widget expander-label-widget "label-widget"
                         "GtkWidget" t t)
                        (spacing expander-spacing "spacing" "gint" t t)
                        (use-markup expander-use-markup "use-markup" "gboolean"
                         t t)
                        (use-underline expander-use-underline "use-underline"
                         "gboolean" t t)))

(define-g-object-class "GtkFrame" frame
                       (:superclass bin :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_frame_get_type")
                       ((label frame-label "label" "gchararray" t t)
                        (label-widget frame-label-widget "label-widget"
                         "GtkWidget" t t)
                        (label-xalign frame-label-xalign "label-xalign"
                         "gfloat" t t)
                        (label-yalign frame-label-yalign "label-yalign"
                         "gfloat" t t)
                        (shadow frame-shadow "shadow" "GtkShadowType" t t)
                        (shadow-type frame-shadow-type "shadow-type"
                         "GtkShadowType" t t)))

(define-g-object-class "GtkAspectFrame" aspect-frame
                       (:superclass frame :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_aspect_frame_get_type")
                       ((obey-child aspect-frame-obey-child "obey-child"
                         "gboolean" t t)
                        (ratio aspect-frame-ratio "ratio" "gfloat" t t)
                        (xalign aspect-frame-xalign "xalign" "gfloat" t t)
                        (yalign aspect-frame-yalign "yalign" "gfloat" t t)))

(define-g-object-class "GtkHandleBox" handle-box
                       (:superclass bin :export t :interfaces
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

(define-g-object-class "GtkItem" item
                       (:superclass bin :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_item_get_type")
                       nil)

(define-g-object-class "GtkMenuItem" menu-item
                       (:superclass item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_menu_item_get_type")
                       ((accel-path menu-item-accel-path "accel-path"
                         "gchararray" t t)
                        (label menu-item-label "label" "gchararray" t t)
                        (right-justified menu-item-right-justified
                         "right-justified" "gboolean" t t)
                        (submenu menu-item-submenu "submenu" "GtkMenu" t t)
                        (use-underline menu-item-use-underline "use-underline"
                         "gboolean" t t)))

(define-g-object-class "GtkCheckMenuItem" check-menu-item
                       (:superclass menu-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_check_menu_item_get_type")
                       ((active check-menu-item-active "active" "gboolean" t t)
                        (draw-as-radio check-menu-item-draw-as-radio
                         "draw-as-radio" "gboolean" t t)
                        (inconsistent check-menu-item-inconsistent
                         "inconsistent" "gboolean" t t)))

(define-g-object-class "GtkRadioMenuItem" radio-menu-item
                       (:superclass check-menu-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_radio_menu_item_get_type")
                       ((group radio-menu-item-group "group" "GtkRadioMenuItem"
                         nil t)))

(define-g-object-class "GtkImageMenuItem" image-menu-item
                       (:superclass menu-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_image_menu_item_get_type")
                       ((accel-group image-menu-item-accel-group "accel-group"
                         "GtkAccelGroup" nil t)
                        (always-show-image image-menu-item-always-show-image
                         "always-show-image" "gboolean" t t)
                        (image image-menu-item-image "image" "GtkWidget" t t)
                        (use-stock image-menu-item-use-stock "use-stock"
                         "gboolean" t t)))

(define-g-object-class "GtkSeparatorMenuItem" separator-menu-item
                       (:superclass menu-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_separator_menu_item_get_type")
                       nil)

(define-g-object-class "GtkTearoffMenuItem" tearoff-menu-item
                       (:superclass menu-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_tearoff_menu_item_get_type")
                       nil)

(define-g-object-class "GtkScrolledWindow" scrolled-window
                       (:superclass bin :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_scrolled_window_get_type")
                       ((hadjustment scrolled-window-hadjustment "hadjustment"
                         "GtkAdjustment" t t)
                        (hscrollbar-policy scrolled-window-hscrollbar-policy
                         "hscrollbar-policy" "GtkPolicyType" t t)
                        (shadow-type scrolled-window-shadow-type "shadow-type"
                         "GtkShadowType" t t)
                        (vadjustment scrolled-window-vadjustment "vadjustment"
                         "GtkAdjustment" t t)
                        (vscrollbar-policy scrolled-window-vscrollbar-policy
                         "vscrollbar-policy" "GtkPolicyType" t t)
                        (window-placement scrolled-window-window-placement
                         "window-placement" "GtkCornerType" t t)
                        (window-placement-set
                         scrolled-window-window-placement-set
                         "window-placement-set" "gboolean" t t)
                        (:cffi hscrollbar scrolled-window-hscrollbar
                         (g-object widget) "gtk_scrolled_window_get_hscrollbar"
                         nil)
                        (:cffi vscrollbar scrolled-window-vscrollbar
                         (g-object widget) "gtk_scrolled_window_get_vscrollbar"
                         nil)))

(define-g-object-class "GtkToolItem" tool-item
                       (:superclass bin :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_tool_item_get_type")
                       ((is-important tool-item-is-important "is-important"
                         "gboolean" t t)
                        (visible-horizontal tool-item-visible-horizontal
                         "visible-horizontal" "gboolean" t t)
                        (visible-vertical tool-item-visible-vertical
                         "visible-vertical" "gboolean" t t)
                        (:cffi expand tool-item-expand :boolean
                         "gtk_tool_item_get_expand" "gtk_tool_item_set_expand")
                        (:cffi use-drag-window tool-item-use-drag-window
                         :boolean "gtk_tool_item_get_use_drag_window"
                         "gtk_tool_item_set_use_drag_window")
                        (:cffi icon-size tool-item-icon-size icon-size
                         "gtk_tool_item_get_icon_size" nil)
                        (:cffi orientation tool-item-orientation gtk-orientation
                         "gtk_tool_item_get_orientation" nil)
                        (:cffi toolbar-style tool-item-toolbar-style
                         gtk-toolbar-style "gtk_tool_item_get_toolbar_style" nil)
                        (:cffi relief-style tool-item-relief-style gtk-relief-style
                         "gtk_tool_item_get_relief_style" nil)))

(define-g-object-class "GtkSeparatorToolItem" separator-tool-item
                       (:superclass tool-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_separator_tool_item_get_type")
                       ((draw separator-tool-item-draw "draw" "gboolean" t t)))

(define-g-object-class "GtkToolButton" tool-button
                       (:superclass tool-item :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_tool_button_get_type")
                       ((icon-name tool-button-icon-name "icon-name"
                         "gchararray" t t)
                        (icon-widget tool-button-icon-widget "icon-widget"
                         "GtkWidget" t t)
                        (label tool-button-label "label" "gchararray" t t)
                        (label-widget tool-button-label-widget "label-widget"
                         "GtkWidget" t t)
                        (stock-id tool-button-stock-id "stock-id" "gchararray"
                         t t)
                        (use-underline tool-button-use-underline
                         "use-underline" "gboolean" t t)))

(define-g-object-class "GtkMenuToolButton" menu-tool-button
                       (:superclass tool-button :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_menu_tool_button_get_type")
                       ((menu menu-tool-button-menu "menu" "GtkMenu" t t)
                        (:cffi arrow-tooltip-text
                         menu-tool-button-arrow-tooltip-text :string nil
                         "gtk_menu_tool_button_set_arrow_tooltip_text")
                        (:cffi arrow-tooltip-markup
                         menu-tool-button-arrow-tooltip-markup :string nil
                         "gtk_menu_tool_button_set_arrow_tooltip_markup")))

(define-g-object-class "GtkToggleToolButton" toggle-tool-button
                       (:superclass tool-button :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_toggle_tool_button_get_type")
                       ((active toggle-tool-button-active "active" "gboolean" t
                         t)))

(define-g-object-class "GtkRadioToolButton" radio-tool-button
                       (:superclass toggle-tool-button :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
                        :type-initializer "gtk_radio_tool_button_get_type")
                       ((group radio-tool-button-group "group"
                         "GtkRadioToolButton" nil t)))

(define-g-object-class "GtkViewport" viewport
                       (:superclass bin :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_viewport_get_type")
                       ((hadjustment viewport-hadjustment "hadjustment"
                         "GtkAdjustment" t t)
                        (shadow-type viewport-shadow-type "shadow-type"
                         "GtkShadowType" t t)
                        (vadjustment viewport-vadjustment "vadjustment"
                         "GtkAdjustment" t t)))




(define-g-object-class "GtkColorSelectionDialog" color-selection-dialog
                       (:superclass dialog :export t :interfaces
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

(define-g-object-class "GtkFileChooserDialog" file-chooser-dialog
                       (:superclass dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser")
                        :type-initializer "gtk_file_chooser_dialog_get_type")
                       nil)

(define-g-object-class "GtkFontSelectionDialog" font-selection-dialog
                       (:superclass dialog :export t :interfaces
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

(define-g-object-class "GtkInputDialog" input-dialog
                       (:superclass dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_input_dialog_get_type")
                       nil)


(define-g-object-class "GtkPageSetupUnixDialog" page-setup-unix-dialog
                       (:superclass dialog :export t :interfaces
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

(define-g-object-class "GtkPrintUnixDialog" print-unix-dialog
                       (:superclass dialog :export t :interfaces
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

(define-g-object-class "GtkRecentChooserDialog" recent-chooser-dialog
                       (:superclass dialog :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable"
                         "GtkRecentChooser")
                        :type-initializer "gtk_recent_chooser_dialog_get_type")
                       nil)

(define-g-object-class "GtkPlug" plug
                       (:superclass gtk-window :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_plug_get_type")
                       ((embedded plug-embedded "embedded" "gboolean" t nil)
                        (socket-window plug-socket-window "socket-window"
                         "GdkWindow" t nil)))



(define-g-object-class "GtkButtonBox" button-box
                       (:superclass gtk-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_button_box_get_type")
                       ((layout-style button-box-layout-style "layout-style"
                         "GtkButtonBoxStyle" t t)))

(define-g-object-class "GtkHButtonBox" h-button-box
                       (:superclass button-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_hbutton_box_get_type")
                       nil)

(define-g-object-class "GtkVButtonBox" v-button-box
                       (:superclass button-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_vbutton_box_get_type")
                       nil)



(define-g-object-class "GtkFileChooserButton" file-chooser-button
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

(define-g-object-class "GtkStatusbar" statusbar
                       (:superclass gtk-h-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_statusbar_get_type")
                       ((has-resize-grip statusbar-has-resize-grip
                         "has-resize-grip" "gboolean" t t)))



(define-g-object-class "GtkColorSelection" color-selection
                       (:superclass gtk-v-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_color_selection_get_type")
                       ((current-alpha color-selection-current-alpha
                         "current-alpha" "guint" t t)
                        (current-color color-selection-current-color
                         "current-color" "GdkColor" t t)
                        (has-opacity-control
                         color-selection-has-opacity-control
                         "has-opacity-control" "gboolean" t t)
                        (has-palette color-selection-has-palette "has-palette"
                         "gboolean" t t)
                        (:cffi previous-alpha color-selection-previous-alpha
                         :uint16 "gtk_color_selection_get_previous_alpha"
                         "gtk_color_selection_set_previous_alpha")
                        (:cffi previous-color color-selection-previous-color
                         (g-boxed-foreign gdk-color)
                         gtk-color-selection-get-previous-color
                         gtk-color-selection-set-previous-color)))

(define-g-object-class "GtkFileChooserWidget" file-chooser-widget
                       (:superclass gtk-v-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser"
                         "GtkFileChooserEmbed" "GtkOrientable")
                        :type-initializer "gtk_file_chooser_widget_get_type")
                       nil)

(define-g-object-class "GtkFontSelection" font-selection
                       (:superclass gtk-v-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_font_selection_get_type")
                       ((font font-selection-font "font" "GdkFont" t nil)
                        (font-name font-selection-font-name "font-name"
                         "gchararray" t t)
                        (preview-text font-selection-preview-text
                         "preview-text" "gchararray" t t)))

(define-g-object-class "GtkGammaCurve" gamma-curve
                       (:superclass gtk-v-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_gamma_curve_get_type")
                       nil)

(define-g-object-class "GtkRecentChooserWidget" recent-chooser-widget
                       (:superclass gtk-v-box :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
                         "GtkRecentChooser")
                        :type-initializer "gtk_recent_chooser_widget_get_type")
                       nil)

(define-g-object-class "GtkFixed" fixed
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_fixed_get_type")
                       ((:cffi has-window fixed-has-window :boolean
                         "gtk_fixed_get_has_window"
                         "gtk_fixed_set_has_window")))

(define-g-object-class "GtkIconView" icon-view
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkCellLayout")
                        :type-initializer "gtk_icon_view_get_type")
                       ((column-spacing icon-view-column-spacing
                         "column-spacing" "gint" t t)
                        (columns icon-view-columns "columns" "gint" t t)
                        (item-width icon-view-item-width "item-width" "gint" t
                         t)
                        (margin icon-view-margin "margin" "gint" t t)
                        (markup-column icon-view-markup-column "markup-column"
                         "gint" t t)
                        (model icon-view-model "model" "GtkTreeModel" t t)
                        (orientation icon-view-orientation "orientation"
                         "GtkOrientation" t t)
                        (pixbuf-column icon-view-pixbuf-column "pixbuf-column"
                         "gint" t t)
                        (reorderable icon-view-reorderable "reorderable"
                         "gboolean" t t)
                        (row-spacing icon-view-row-spacing "row-spacing" "gint"
                         t t)
                        (selection-mode icon-view-selection-mode
                         "selection-mode" "GtkSelectionMode" t t)
                        (spacing icon-view-spacing "spacing" "gint" t t)
                        (text-column icon-view-text-column "text-column" "gint"
                         t t)
                        (tooltip-column icon-view-tooltip-column
                         "tooltip-column" "gint" t t)))

(define-g-object-class "GtkLayout" layout
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_layout_get_type")
                       ((hadjustment layout-hadjustment "hadjustment"
                         "GtkAdjustment" t t)
                        (height layout-height "height" "guint" t t)
                        (vadjustment layout-vadjustment "vadjustment"
                         "GtkAdjustment" t t)
                        (width layout-width "width" "guint" t t)
                        (:cffi bin-window layout-bin-window g-object
                         "gtk_layout_get_bin_window" nil)))

(define-g-object-class "GtkMenuShell" menu-shell
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_menu_shell_get_type")
                       ((take-focus menu-shell-take-focus "take-focus"
                         "gboolean" t t)))

(define-g-object-class "GtkMenu" menu
                       (:superclass menu-shell :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_menu_get_type")
                       ((accel-group menu-accel-group "accel-group"
                         "GtkAccelGroup" t t)
                        (accel-path menu-accel-path "accel-path" "gchararray" t
                         t)
                        (active menu-active "active" "gint" t t)
                        (attach-widget menu-attach-widget "attach-widget"
                         "GtkWidget" t t)
                        (monitor menu-monitor "monitor" "gint" t t)
                        (tearoff-state menu-tearoff-state "tearoff-state"
                         "gboolean" t t)
                        (tearoff-title menu-tearoff-title "tearoff-title"
                         "gchararray" t t)
                        (:cffi screen menu-screen g-object nil
                         "gtk_menu_set_screen")
                        (:cffi title menu-title
                         (:string :free-from-foreign nil :free-to-foreign t)
                         "gtk_menu_get_title" "gtk_menu_set_title")))

(define-g-object-class "GtkRecentChooserMenu" recent-chooser-menu
                       (:superclass menu :export t :interfaces
                        ("AtkImplementorIface" "GtkActivatable" "GtkBuildable"
                         "GtkRecentChooser")
                        :type-initializer "gtk_recent_chooser_menu_get_type")
                       ((show-numbers recent-chooser-menu-show-numbers
                         "show-numbers" "gboolean" t t)))

(define-g-object-class "GtkMenuBar" menu-bar
                       (:superclass menu-shell :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_menu_bar_get_type")
                       ((child-pack-direction menu-bar-child-pack-direction
                         "child-pack-direction" "GtkPackDirection" t t)
                        (pack-direction menu-bar-pack-direction
                         "pack-direction" "GtkPackDirection" t t)))

(define-g-object-class "GtkNotebook" notebook
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_notebook_get_type")
                       ((enable-popup notebook-enable-popup "enable-popup"
                         "gboolean" t t)
                        (group notebook-group "group" "gpointer" t t)
                        (group-id notebook-group-id "group-id" "gint" t t)
                        (homogeneous notebook-homogeneous "homogeneous"
                         "gboolean" t t)
                        (page notebook-page "page" "gint" t t)
                        (scrollable notebook-scrollable "scrollable" "gboolean"
                         t t)
                        (show-border notebook-show-border "show-border"
                         "gboolean" t t)
                        (show-tabs notebook-show-tabs "show-tabs" "gboolean" t
                         t)
                        (tab-border notebook-tab-border "tab-border" "guint"
                         nil t)
                        (tab-hborder notebook-tab-hborder "tab-hborder" "guint"
                         t t)
                        (tab-pos notebook-tab-pos "tab-pos" "GtkPositionType" t
                         t)
                        (tab-vborder notebook-tab-vborder "tab-vborder" "guint"
                         t t)))

(define-g-object-class "GtkPaned" paned
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_paned_get_type")
                       ((max-position paned-max-position "max-position" "gint"
                         t nil)
                        (min-position paned-min-position "min-position" "gint"
                         t nil)
                        (position paned-position "position" "gint" t t)
                        (position-set paned-position-set "position-set"
                         "gboolean" t t)))

(define-g-object-class "GtkHPaned" h-paned
                       (:superclass paned :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_hpaned_get_type")
                       nil)

(define-g-object-class "GtkVPaned" v-paned
                       (:superclass paned :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_vpaned_get_type")
                       nil)

(define-g-object-class "GtkSocket" socket
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_socket_get_type")
                       nil)



(define-g-object-class "GtkTextView" text-view
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_text_view_get_type")
                       ((accepts-tab text-view-accepts-tab "accepts-tab"
                         "gboolean" t t)
                        (buffer text-view-buffer "buffer" "GtkTextBuffer" t t)
                        (cursor-visible text-view-cursor-visible
                         "cursor-visible" "gboolean" t t)
                        (editable text-view-editable "editable" "gboolean" t t)
                        (im-module text-view-im-module "im-module" "gchararray"
                         t t)
                        (indent text-view-indent "indent" "gint" t t)
                        (justification text-view-justification "justification"
                         "GtkJustification" t t)
                        (left-margin text-view-left-margin "left-margin" "gint"
                         t t)
                        (overwrite text-view-overwrite "overwrite" "gboolean" t
                         t)
                        (pixels-above-lines text-view-pixels-above-lines
                         "pixels-above-lines" "gint" t t)
                        (pixels-below-lines text-view-pixels-below-lines
                         "pixels-below-lines" "gint" t t)
                        (pixels-inside-wrap text-view-pixels-inside-wrap
                         "pixels-inside-wrap" "gint" t t)
                        (right-margin text-view-right-margin "right-margin"
                         "gint" t t)
                        (tabs text-view-tabs "tabs" "PangoTabArray" t t)
                        (wrap-mode text-view-wrap-mode "wrap-mode"
                         "GtkWrapMode" t t)))

(define-g-object-class "GtkToolbar" toolbar
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
                         "GtkToolShell")
                        :type-initializer "gtk_toolbar_get_type")
                       ((icon-size toolbar-icon-size "icon-size" "gint" t t)
                        (icon-size-set toolbar-icon-size-set "icon-size-set"
                         "gboolean" t t)
                        (show-arrow toolbar-show-arrow "show-arrow" "gboolean"
                         t t)
                        (toolbar-style toolbar-toolbar-style "toolbar-style"
                         "GtkToolbarStyle" t t)
                        (tooltips toolbar-tooltips "tooltips" "gboolean" t t)))

(define-g-object-class "GtkTreeView" tree-view
                       (:superclass container :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_tree_view_get_type")
                       ((enable-grid-lines tree-view-enable-grid-lines
                         "enable-grid-lines" "GtkTreeViewGridLines" t t)
                        (enable-search tree-view-enable-search "enable-search"
                         "gboolean" t t)
                        (enable-tree-lines tree-view-enable-tree-lines
                         "enable-tree-lines" "gboolean" t t)
                        (expander-column tree-view-expander-column
                         "expander-column" "GtkTreeViewColumn" t t)
                        (fixed-height-mode tree-view-fixed-height-mode
                         "fixed-height-mode" "gboolean" t t)
                        (hadjustment tree-view-hadjustment "hadjustment"
                         "GtkAdjustment" t t)
                        (headers-clickable tree-view-headers-clickable
                         "headers-clickable" "gboolean" t t)
                        (headers-visible tree-view-headers-visible
                         "headers-visible" "gboolean" t t)
                        (hover-expand tree-view-hover-expand "hover-expand"
                         "gboolean" t t)
                        (hover-selection tree-view-hover-selection
                         "hover-selection" "gboolean" t t)
                        (level-indentation tree-view-level-indentation
                         "level-indentation" "gint" t t)
                        (model tree-view-model "model" "GtkTreeModel" t t)
                        (reorderable tree-view-reorderable "reorderable"
                         "gboolean" t t)
                        (rubber-banding tree-view-rubber-banding
                         "rubber-banding" "gboolean" t t)
                        (rules-hint tree-view-rules-hint "rules-hint"
                         "gboolean" t t)
                        (search-column tree-view-search-column "search-column"
                         "gint" t t)
                        (show-expanders tree-view-show-expanders
                         "show-expanders" "gboolean" t t)
                        (tooltip-column tree-view-tooltip-column
                         "tooltip-column" "gint" t t)
                        (vadjustment tree-view-vadjustment "vadjustment"
                         "GtkAdjustment" t t)
                        (:cffi selection tree-view-selection g-object
                         "gtk_tree_view_get_selection" nil)
                        (:cffi column-drag-function
                         tree-view-column-drag-function nil nil
                         tree-view-set-column-drag-function)
                        (:cffi bin-window tree-view-bin-window g-object
                         "gtk_tree_view_get_bin_window" nil)
                        (:cffi search-equal-func tree-view-search-equal-func
                         nil nil tree-view-set-search-equal-func)
                        (:cffi search-entry tree-view-search-entry g-object
                         "gtk_tree_view_get_search_entry"
                         "gtk_tree_view_set_search_entry")
                        (:cffi search-position-func
                         tree-view-search-position-func nil nil
                         tree-view-set-search-position-func)
                        (:cffi row-separator-func tree-view-row-separator-func
                         nil nil tree-view-set-row-separartor-func)))

(define-g-object-class "GtkDrawingArea" drawing-area
                       (:superclass widget :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_drawing_area_get_type")
                       nil)

(define-g-object-class "GtkCurve" curve
                       (:superclass drawing-area :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_curve_get_type")
                       ((curve-type curve-curve-type "curve-type"
                         "GtkCurveType" t t)
                        (max-x curve-max-x "max-x" "gfloat" t t)
                        (max-y curve-max-y "max-y" "gfloat" t t)
                        (min-x curve-min-x "min-x" "gfloat" t t)
                        (min-y curve-min-y "min-y" "gfloat" t t)))



(define-g-object-class "GtkSpinButton" spin-button
                       (:superclass entry :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                         "GtkEditable")
                        :type-initializer "gtk_spin_button_get_type")
                       ((adjustment spin-button-adjustment "adjustment"
                         "GtkAdjustment" t t)
                        (climb-rate spin-button-climb-rate "climb-rate"
                         "gdouble" t t)
                        (digits spin-button-digits "digits" "guint" t t)
                        (numeric spin-button-numeric "numeric" "gboolean" t t)
                        (snap-to-ticks spin-button-snap-to-ticks
                         "snap-to-ticks" "gboolean" t t)
                        (update-policy spin-button-update-policy
                         "update-policy" "GtkSpinButtonUpdatePolicy" t t)
                        (value spin-button-value "value" "gdouble" t t)
                        (wrap spin-button-wrap "wrap" "gboolean" t t)))


(define-g-object-class "GtkArrow" arrow
                       (:superclass misc :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_arrow_get_type")
                       ((arrow-type arrow-arrow-type "arrow-type"
                         "GtkArrowType" t t)
                        (shadow-type arrow-shadow-type "shadow-type"
                         "GtkShadowType" t t)))







(define-g-object-class "GtkProgress" progress
                       (:superclass widget :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_progress_get_type")
                       ((activity-mode progress-activity-mode "activity-mode"
                         "gboolean" t t)
                        (show-text progress-show-text "show-text" "gboolean" t
                         t)
                        (text-xalign progress-text-xalign "text-xalign"
                         "gfloat" t t)
                        (text-yalign progress-text-yalign "text-yalign"
                         "gfloat" t t)))

(define-g-object-class "GtkProgressBar" progress-bar
                       (:superclass progress :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable")
                        :type-initializer "gtk_progress_bar_get_type")
                       ((activity-blocks progress-bar-activity-blocks
                         "activity-blocks" "guint" t t)
                        (activity-step progress-bar-activity-step
                         "activity-step" "guint" t t)
                        (adjustment progress-bar-adjustment "adjustment"
                         "GtkAdjustment" t t)
                        (bar-style progress-bar-bar-style "bar-style"
                         "GtkProgressBarStyle" t t)
                        (discrete-blocks progress-bar-discrete-blocks
                         "discrete-blocks" "guint" t t)
                        (ellipsize progress-bar-ellipsize "ellipsize"
                         "PangoEllipsizeMode" t t)
                        (fraction progress-bar-fraction "fraction" "gdouble" t
                         t)
                        (orientation progress-bar-orientation "orientation"
                         "GtkProgressBarOrientation" t t)
                        (pulse-step progress-bar-pulse-step "pulse-step"
                         "gdouble" t t)
                        (text progress-bar-text "text" "gchararray" t t)))

(define-g-object-class "GtkRange" range
                       (:superclass widget :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_range_get_type")
                       ((adjustment range-adjustment "adjustment"
                         "GtkAdjustment" t t)
                        (fill-level range-fill-level "fill-level" "gdouble" t
                         t)
                        (inverted range-inverted "inverted" "gboolean" t t)
                        (lower-stepper-sensitivity
                         range-lower-stepper-sensitivity
                         "lower-stepper-sensitivity" "GtkSensitivityType" t t)
                        (restrict-to-fill-level range-restrict-to-fill-level
                         "restrict-to-fill-level" "gboolean" t t)
                        (show-fill-level range-show-fill-level
                         "show-fill-level" "gboolean" t t)
                        (update-policy range-update-policy "update-policy"
                         "GtkUpdateType" t t)
                        (upper-stepper-sensitivity
                         range-upper-stepper-sensitivity
                         "upper-stepper-sensitivity" "GtkSensitivityType" t t)))







(define-g-object-class "GtkScrollbar" scrollbar
                       (:superclass range :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_scrollbar_get_type")
                       nil)

(define-g-object-class "GtkHScrollbar" h-scrollbar
                       (:superclass scrollbar :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_hscrollbar_get_type")
                       nil)

(define-g-object-class "GtkVScrollbar" v-scrollbar
                       (:superclass scrollbar :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_vscrollbar_get_type")
                       nil)

(define-g-object-class "GtkRuler" ruler
                       (:superclass widget :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_ruler_get_type")
                       ((lower ruler-lower "lower" "gdouble" t t)
                        (max-size ruler-max-size "max-size" "gdouble" t t)
                        (metric ruler-metric "metric" "GtkMetricType" t t)
                        (position ruler-position "position" "gdouble" t t)
                        (upper ruler-upper "upper" "gdouble" t t)))

(define-g-object-class "GtkHRuler" h-ruler
                       (:superclass ruler :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_hruler_get_type")
                       nil)

(define-g-object-class "GtkVRuler" v-ruler
                       (:superclass ruler :export t :interfaces
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :type-initializer "gtk_vruler_get_type")
                       nil)



(define-g-object-class "GtkSettings" settings
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_settings_get_type")
                       ((color-hash settings-color-hash "color-hash"
                         "GHashTable" t nil)
                        (gtk-alternative-button-order
                         settings-gtk-alternative-button-order
                         "gtk-alternative-button-order" "gboolean" t t)
                        (gtk-alternative-sort-arrows
                         settings-gtk-alternative-sort-arrows
                         "gtk-alternative-sort-arrows" "gboolean" t t)
                        (gtk-button-images settings-gtk-button-images
                         "gtk-button-images" "gboolean" t t)
                        (gtk-can-change-accels settings-gtk-can-change-accels
                         "gtk-can-change-accels" "gboolean" t t)
                        (gtk-color-palette settings-gtk-color-palette
                         "gtk-color-palette" "gchararray" t t)
                        (gtk-color-scheme settings-gtk-color-scheme
                         "gtk-color-scheme" "gchararray" t t)
                        (gtk-cursor-blink settings-gtk-cursor-blink
                         "gtk-cursor-blink" "gboolean" t t)
                        (gtk-cursor-blink-time settings-gtk-cursor-blink-time
                         "gtk-cursor-blink-time" "gint" t t)
                        (gtk-cursor-blink-timeout
                         settings-gtk-cursor-blink-timeout
                         "gtk-cursor-blink-timeout" "gint" t t)
                        (gtk-cursor-theme-name settings-gtk-cursor-theme-name
                         "gtk-cursor-theme-name" "gchararray" t t)
                        (gtk-cursor-theme-size settings-gtk-cursor-theme-size
                         "gtk-cursor-theme-size" "gint" t t)
                        (gtk-dnd-drag-threshold settings-gtk-dnd-drag-threshold
                         "gtk-dnd-drag-threshold" "gint" t t)
                        (gtk-double-click-distance
                         settings-gtk-double-click-distance
                         "gtk-double-click-distance" "gint" t t)
                        (gtk-double-click-time settings-gtk-double-click-time
                         "gtk-double-click-time" "gint" t t)
                        (gtk-enable-accels settings-gtk-enable-accels
                         "gtk-enable-accels" "gboolean" t t)
                        (gtk-enable-animations settings-gtk-enable-animations
                         "gtk-enable-animations" "gboolean" t t)
                        (gtk-enable-event-sounds
                         settings-gtk-enable-event-sounds
                         "gtk-enable-event-sounds" "gboolean" t t)
                        (gtk-enable-input-feedback-sounds
                         settings-gtk-enable-input-feedback-sounds
                         "gtk-enable-input-feedback-sounds" "gboolean" t t)
                        (gtk-enable-mnemonics settings-gtk-enable-mnemonics
                         "gtk-enable-mnemonics" "gboolean" t t)
                        (gtk-enable-tooltips settings-gtk-enable-tooltips
                         "gtk-enable-tooltips" "gboolean" t t)
                        (gtk-entry-password-hint-timeout
                         settings-gtk-entry-password-hint-timeout
                         "gtk-entry-password-hint-timeout" "guint" t t)
                        (gtk-entry-select-on-focus
                         settings-gtk-entry-select-on-focus
                         "gtk-entry-select-on-focus" "gboolean" t t)
                        (gtk-error-bell settings-gtk-error-bell
                         "gtk-error-bell" "gboolean" t t)
                        (gtk-fallback-icon-theme
                         settings-gtk-fallback-icon-theme
                         "gtk-fallback-icon-theme" "gchararray" t t)
                        (gtk-file-chooser-backend
                         settings-gtk-file-chooser-backend
                         "gtk-file-chooser-backend" "gchararray" t t)
                        (gtk-font-name settings-gtk-font-name "gtk-font-name"
                         "gchararray" t t)
                        (gtk-fontconfig-timestamp
                         settings-gtk-fontconfig-timestamp
                         "gtk-fontconfig-timestamp" "guint" t t)
                        (gtk-icon-sizes settings-gtk-icon-sizes
                         "gtk-icon-sizes" "gchararray" t t)
                        (gtk-icon-theme-name settings-gtk-icon-theme-name
                         "gtk-icon-theme-name" "gchararray" t t)
                        (gtk-im-module settings-gtk-im-module "gtk-im-module"
                         "gchararray" t t)
                        (gtk-key-theme-name settings-gtk-key-theme-name
                         "gtk-key-theme-name" "gchararray" t t)
                        (gtk-keynav-cursor-only settings-gtk-keynav-cursor-only
                         "gtk-keynav-cursor-only" "gboolean" t t)
                        (gtk-keynav-wrap-around settings-gtk-keynav-wrap-around
                         "gtk-keynav-wrap-around" "gboolean" t t)
                        (gtk-label-select-on-focus
                         settings-gtk-label-select-on-focus
                         "gtk-label-select-on-focus" "gboolean" t t)
                        (gtk-menu-bar-accel settings-gtk-menu-bar-accel
                         "gtk-menu-bar-accel" "gchararray" t t)
                        (gtk-menu-bar-popup-delay
                         settings-gtk-menu-bar-popup-delay
                         "gtk-menu-bar-popup-delay" "gint" t t)
                        (gtk-menu-images settings-gtk-menu-images
                         "gtk-menu-images" "gboolean" t t)
                        (gtk-menu-popdown-delay settings-gtk-menu-popdown-delay
                         "gtk-menu-popdown-delay" "gint" t t)
                        (gtk-menu-popup-delay settings-gtk-menu-popup-delay
                         "gtk-menu-popup-delay" "gint" t t)
                        (gtk-modules settings-gtk-modules "gtk-modules"
                         "gchararray" t t)
                        (gtk-print-backends settings-gtk-print-backends
                         "gtk-print-backends" "gchararray" t t)
                        (gtk-print-preview-command
                         settings-gtk-print-preview-command
                         "gtk-print-preview-command" "gchararray" t t)
                        (gtk-recent-files-limit settings-gtk-recent-files-limit
                         "gtk-recent-files-limit" "gint" t t)
                        (gtk-recent-files-max-age
                         settings-gtk-recent-files-max-age
                         "gtk-recent-files-max-age" "gint" t t)
                        (gtk-scrolled-window-placement
                         settings-gtk-scrolled-window-placement
                         "gtk-scrolled-window-placement" "GtkCornerType" t t)
                        (gtk-show-input-method-menu
                         settings-gtk-show-input-method-menu
                         "gtk-show-input-method-menu" "gboolean" t t)
                        (gtk-show-unicode-menu settings-gtk-show-unicode-menu
                         "gtk-show-unicode-menu" "gboolean" t t)
                        (gtk-sound-theme-name settings-gtk-sound-theme-name
                         "gtk-sound-theme-name" "gchararray" t t)
                        (gtk-split-cursor settings-gtk-split-cursor
                         "gtk-split-cursor" "gboolean" t t)
                        (gtk-theme-name settings-gtk-theme-name
                         "gtk-theme-name" "gchararray" t t)
                        (gtk-timeout-expand settings-gtk-timeout-expand
                         "gtk-timeout-expand" "gint" t t)
                        (gtk-timeout-initial settings-gtk-timeout-initial
                         "gtk-timeout-initial" "gint" t t)
                        (gtk-timeout-repeat settings-gtk-timeout-repeat
                         "gtk-timeout-repeat" "gint" t t)
                        (gtk-toolbar-icon-size settings-gtk-toolbar-icon-size
                         "gtk-toolbar-icon-size" "GtkIconSize" t t)
                        (gtk-toolbar-style settings-gtk-toolbar-style
                         "gtk-toolbar-style" "GtkToolbarStyle" t t)
                        (gtk-tooltip-browse-mode-timeout
                         settings-gtk-tooltip-browse-mode-timeout
                         "gtk-tooltip-browse-mode-timeout" "gint" t t)
                        (gtk-tooltip-browse-timeout
                         settings-gtk-tooltip-browse-timeout
                         "gtk-tooltip-browse-timeout" "gint" t t)
                        (gtk-tooltip-timeout settings-gtk-tooltip-timeout
                         "gtk-tooltip-timeout" "gint" t t)
                        (gtk-touchscreen-mode settings-gtk-touchscreen-mode
                         "gtk-touchscreen-mode" "gboolean" t t)
                        (gtk-xft-antialias settings-gtk-xft-antialias
                         "gtk-xft-antialias" "gint" t t)
                        (gtk-xft-dpi settings-gtk-xft-dpi "gtk-xft-dpi" "gint"
                         t t)
                        (gtk-xft-hinting settings-gtk-xft-hinting
                         "gtk-xft-hinting" "gint" t t)
                        (gtk-xft-hintstyle settings-gtk-xft-hintstyle
                         "gtk-xft-hintstyle" "gchararray" t t)
                        (gtk-xft-rgba settings-gtk-xft-rgba "gtk-xft-rgba"
                         "gchararray" t t)))

(define-g-object-class "GtkRcStyle" rc-style
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_rc_style_get_type")
                       nil)

(define-g-object-class "GtkStyle" style
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_style_get_type")
                       nil)

(define-g-object-class "GtkTooltip" tooltip
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_tooltip_get_type")
                       nil)


(define-g-object-class "GtkAccelMap" accel-map
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_accel_map_get_type")
                       nil)

(define-g-object-class "GtkAction" action
                       (:superclass g-object :export t :interfaces
                        ("GtkBuildable") :type-initializer
                        "gtk_action_get_type")
                       ((action-group action-action-group "action-group"
                         "GtkActionGroup" t t)
                        (gicon action-gicon "gicon" "GIcon" t t)
                        (hide-if-empty action-hide-if-empty "hide-if-empty"
                         "gboolean" t t)
                        (icon-name action-icon-name "icon-name" "gchararray" t
                         t)
                        (is-important action-is-important "is-important"
                         "gboolean" t t)
                        (label action-label "label" "gchararray" t t)
                        (name action-name "name" "gchararray" t nil)
                        (sensitive action-sensitive "sensitive" "gboolean" t t)
                        (short-label action-short-label "short-label"
                         "gchararray" t t)
                        (stock-id action-stock-id "stock-id" "gchararray" t t)
                        (tooltip action-tooltip "tooltip" "gchararray" t t)
                        (visible action-visible "visible" "gboolean" t t)
                        (visible-horizontal action-visible-horizontal
                         "visible-horizontal" "gboolean" t t)
                        (visible-overflown action-visible-overflown
                         "visible-overflown" "gboolean" t t)
                        (visible-vertical action-visible-vertical
                         "visible-vertical" "gboolean" t t)
                        (:cffi accel-path action-accel-path
                         (:string :free-from-foreign nil :free-to-foreign t)
                         "gtk_action_get_accel_path"
                         "gtk_action_set_accel_path")
                        (:cffi accel-group action-accel-group g-object nil
                         "gtk_action_set_accel_group")))

(define-g-object-class "GtkActionGroup" action-group
                       (:superclass g-object :export t :interfaces
                        ("GtkBuildable") :type-initializer
                        "gtk_action_group_get_type")
                       ((name action-group-name "name" "gchararray" t nil)
                        (sensitive action-group-sensitive "sensitive"
                         "gboolean" t t)
                        (visible action-group-visible "visible" "gboolean" t t)
                        (:cffi translate-function
                         action-group-translate-function nil nil
                         action-group-set-translate-func)
                        (:cffi translation-domain
                         action-group-translation-domain nil nil
                         gtk-action-group-set-translation-domain)))

(define-g-object-class "GtkBuilder" builder
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_builder_get_type")
                       ((translation-domain builder-translation-domain
                         "translation-domain" "gchararray" t t)))

(define-g-object-class "GtkClipboard" clipboard
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_clipboard_get_type")
                       nil)

(define-g-object-class "GtkEntryCompletion" entry-completion
                       (:superclass g-object :export t :interfaces
                        ("GtkBuildable" "GtkCellLayout") :type-initializer
                        "gtk_entry_completion_get_type")
                       ((inline-completion entry-completion-inline-completion
                         "inline-completion" "gboolean" t t)
                        (inline-selection entry-completion-inline-selection
                         "inline-selection" "gboolean" t t)
                        (minimum-key-length entry-completion-minimum-key-length
                         "minimum-key-length" "gint" t t)
                        (model entry-completion-model "model" "GtkTreeModel" t
                         t)
                        (popup-completion entry-completion-popup-completion
                         "popup-completion" "gboolean" t t)
                        (popup-set-width entry-completion-popup-set-width
                         "popup-set-width" "gboolean" t t)
                        (popup-single-match entry-completion-popup-single-match
                         "popup-single-match" "gboolean" t t)
                        (text-column entry-completion-text-column "text-column"
                         "gint" t t)
                        (:cffi entry entry-completion-entry (g-object entry)
                         "gtk_entry_completion_get_entry" nil)
                        (:cffi match-function entry-completion-match-function
                         nil nil gtk-entry-completion-set-match-function)))

(define-g-object-class "GtkIconFactory" icon-factory
                       (:superclass g-object :export t :interfaces
                        ("GtkBuildable") :type-initializer
                        "gtk_icon_factory_get_type")
                       nil)

(define-g-object-class "GtkIconTheme" icon-theme
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_icon_theme_get_type")
                       nil)

(define-g-object-class "GtkIMContext" i-m-context
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_im_context_get_type") nil)

(define-g-object-class "GtkListStore" list-store
                       (:superclass g-object :export t :interfaces
                        ("GtkBuildable" "GtkTreeDragDest" "GtkTreeDragSource"
                         "GtkTreeModel" "GtkTreeSortable")
                        :type-initializer "gtk_list_store_get_type")
                       nil)

(define-g-object-class "GtkPageSetup" page-setup
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_page_setup_get_type")
                       nil)

(define-g-object-class "GtkPrintContext" print-context
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_print_context_get_type")
                       nil)

(define-g-object-class "GtkPrintOperation" print-operation
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

(define-g-object-class "GtkPrintSettings" print-settings
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_print_settings_get_type")
                       nil)

(define-g-object-class "GtkRecentManager" recent-manager
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_recent_manager_get_type")
                       ((filename recent-manager-filename "filename"
                         "gchararray" t nil)
                        (limit recent-manager-limit "limit" "gint" t t)
                        (size recent-manager-size "size" "gint" t nil)))

(define-g-object-class "GtkSizeGroup" size-group
                       (:superclass g-object :export t :interfaces
                        ("GtkBuildable") :type-initializer
                        "gtk_size_group_get_type")
                       ((ignore-hidden size-group-ignore-hidden "ignore-hidden"
                         "gboolean" t t)
                        (mode size-group-mode "mode" "GtkSizeGroupMode" t t)))

(define-g-object-class "GtkStatusIcon" status-icon
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_status_icon_get_type")
                       ((blinking status-icon-blinking "blinking" "gboolean" t
                         t)
                        (embedded status-icon-embedded "embedded" "gboolean" t
                         nil)
                        (file status-icon-file "file" "gchararray" nil t)
                        (gicon status-icon-gicon "gicon" "GIcon" t t)
                        (has-tooltip status-icon-has-tooltip "has-tooltip"
                         "gboolean" t t)
                        (icon-name status-icon-icon-name "icon-name"
                         "gchararray" t t)
                        (orientation status-icon-orientation "orientation"
                         "GtkOrientation" t nil)
                        (pixbuf status-icon-pixbuf "pixbuf" "GdkPixbuf" t t)
                        (screen status-icon-screen "screen" "GdkScreen" t t)
                        (size status-icon-size "size" "gint" t nil)
                        (stock status-icon-stock "stock" "gchararray" t t)
                        (storage-type status-icon-storage-type "storage-type"
                         "GtkImageType" t nil)
                        (tooltip-markup status-icon-tooltip-markup
                         "tooltip-markup" "gchararray" t t)
                        (tooltip-text status-icon-tooltip-text "tooltip-text"
                         "gchararray" t t)
                        (visible status-icon-visible "visible" "gboolean" t t)))

(define-g-object-class "GtkTextBuffer" text-buffer
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_text_buffer_get_type")
                       ((copy-target-list text-buffer-copy-target-list
                         "copy-target-list" "GtkTargetList" t nil)
                        (cursor-position text-buffer-cursor-position
                         "cursor-position" "gint" t nil)
                        (has-selection text-buffer-has-selection
                         "has-selection" "gboolean" t nil)
                        (paste-target-list text-buffer-paste-target-list
                         "paste-target-list" "GtkTargetList" t nil)
                        (tag-table text-buffer-tag-table "tag-table"
                         "GtkTextTagTable" t nil)
                        (text text-buffer-text "text" "gchararray" t t)
                        (:cffi modified text-buffer-modified :boolean
                         "gtk_text_buffer_get_modified"
                         "gtk_text_buffer_set_modified")))

(define-g-object-class "GtkTextChildAnchor" text-child-anchor
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_text_child_anchor_get_type")
                       ((:cffi deleted-p text-child-anchor-deleted-p :boolean
                         "gtk_text_child_anchor_get_deleted" nil)))

(define-g-object-class "GtkTextMark" text-mark
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_text_mark_get_type")
                       ((left-gravity text-mark-left-gravity "left-gravity"
                         "gboolean" t nil)
                        (name text-mark-name "name" "gchararray" t nil)
                        (:cffi visible text-mark-visible :boolean
                         "gtk_text_mark_get_visible"
                         "gtk_text_mark_set_visible")
                        (:cffi deleted text-mark-deleted :boolean
                         "gtk_text_mark_get_deleted" nil)
                        (:cffi buffer text-mark-buffer (g-object text-buffer)
                         "gtk_text_mark_get_buffer" nil)))

(define-g-object-class "GtkTextTag" text-tag
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_text_tag_get_type")
                       ((accumulative-margin text-tag-accumulative-margin
                         "accumulative-margin" "gboolean" t t)
                        (background text-tag-background "background"
                         "gchararray" nil t)
                        (background-full-height text-tag-background-full-height
                         "background-full-height" "gboolean" t t)
                        (background-full-height-set
                         text-tag-background-full-height-set
                         "background-full-height-set" "gboolean" t t)
                        (background-gdk text-tag-background-gdk
                         "background-gdk" "GdkColor" t t)
                        (background-set text-tag-background-set
                         "background-set" "gboolean" t t)
                        (background-stipple text-tag-background-stipple
                         "background-stipple" "GdkPixmap" t t)
                        (background-stipple-set text-tag-background-stipple-set
                         "background-stipple-set" "gboolean" t t)
                        (direction text-tag-direction "direction"
                         "GtkTextDirection" t t)
                        (editable text-tag-editable "editable" "gboolean" t t)
                        (editable-set text-tag-editable-set "editable-set"
                         "gboolean" t t)
                        (family text-tag-family "family" "gchararray" t t)
                        (family-set text-tag-family-set "family-set" "gboolean"
                         t t)
                        (font text-tag-font "font" "gchararray" t t)
                        (font-desc text-tag-font-desc "font-desc"
                         "PangoFontDescription" t t)
                        (foreground text-tag-foreground "foreground"
                         "gchararray" nil t)
                        (foreground-gdk text-tag-foreground-gdk
                         "foreground-gdk" "GdkColor" t t)
                        (foreground-set text-tag-foreground-set
                         "foreground-set" "gboolean" t t)
                        (foreground-stipple text-tag-foreground-stipple
                         "foreground-stipple" "GdkPixmap" t t)
                        (foreground-stipple-set text-tag-foreground-stipple-set
                         "foreground-stipple-set" "gboolean" t t)
                        (indent text-tag-indent "indent" "gint" t t)
                        (indent-set text-tag-indent-set "indent-set" "gboolean"
                         t t)
                        (invisible text-tag-invisible "invisible" "gboolean" t
                         t)
                        (invisible-set text-tag-invisible-set "invisible-set"
                         "gboolean" t t)
                        (justification text-tag-justification "justification"
                         "GtkJustification" t t)
                        (justification-set text-tag-justification-set
                         "justification-set" "gboolean" t t)
                        (language text-tag-language "language" "gchararray" t
                         t)
                        (language-set text-tag-language-set "language-set"
                         "gboolean" t t)
                        (left-margin text-tag-left-margin "left-margin" "gint"
                         t t)
                        (left-margin-set text-tag-left-margin-set
                         "left-margin-set" "gboolean" t t)
                        (name text-tag-name "name" "gchararray" t nil)
                        (paragraph-background text-tag-paragraph-background
                         "paragraph-background" "gchararray" nil t)
                        (paragraph-background-gdk
                         text-tag-paragraph-background-gdk
                         "paragraph-background-gdk" "GdkColor" t t)
                        (paragraph-background-set
                         text-tag-paragraph-background-set
                         "paragraph-background-set" "gboolean" t t)
                        (pixels-above-lines text-tag-pixels-above-lines
                         "pixels-above-lines" "gint" t t)
                        (pixels-above-lines-set text-tag-pixels-above-lines-set
                         "pixels-above-lines-set" "gboolean" t t)
                        (pixels-below-lines text-tag-pixels-below-lines
                         "pixels-below-lines" "gint" t t)
                        (pixels-below-lines-set text-tag-pixels-below-lines-set
                         "pixels-below-lines-set" "gboolean" t t)
                        (pixels-inside-wrap text-tag-pixels-inside-wrap
                         "pixels-inside-wrap" "gint" t t)
                        (pixels-inside-wrap-set text-tag-pixels-inside-wrap-set
                         "pixels-inside-wrap-set" "gboolean" t t)
                        (right-margin text-tag-right-margin "right-margin"
                         "gint" t t)
                        (right-margin-set text-tag-right-margin-set
                         "right-margin-set" "gboolean" t t)
                        (rise text-tag-rise "rise" "gint" t t)
                        (rise-set text-tag-rise-set "rise-set" "gboolean" t t)
                        (scale text-tag-scale "scale" "gdouble" t t)
                        (scale-set text-tag-scale-set "scale-set" "gboolean" t
                         t)
                        (size text-tag-size "size" "gint" t t)
                        (size-points text-tag-size-points "size-points"
                         "gdouble" t t)
                        (size-set text-tag-size-set "size-set" "gboolean" t t)
                        (stretch text-tag-stretch "stretch" "PangoStretch" t t)
                        (stretch-set text-tag-stretch-set "stretch-set"
                         "gboolean" t t)
                        (strikethrough text-tag-strikethrough "strikethrough"
                         "gboolean" t t)
                        (strikethrough-set text-tag-strikethrough-set
                         "strikethrough-set" "gboolean" t t)
                        (style text-tag-style "style" "PangoStyle" t t)
                        (style-set text-tag-style-set "style-set" "gboolean" t
                         t)
                        (tabs text-tag-tabs "tabs" "PangoTabArray" t t)
                        (tabs-set text-tag-tabs-set "tabs-set" "gboolean" t t)
                        (underline text-tag-underline "underline"
                         "PangoUnderline" t t)
                        (underline-set text-tag-underline-set "underline-set"
                         "gboolean" t t)
                        (variant text-tag-variant "variant" "PangoVariant" t t)
                        (variant-set text-tag-variant-set "variant-set"
                         "gboolean" t t)
                        (weight text-tag-weight "weight" "gint" t t)
                        (weight-set text-tag-weight-set "weight-set" "gboolean"
                         t t)
                        (wrap-mode text-tag-wrap-mode "wrap-mode" "GtkWrapMode"
                         t t)
                        (wrap-mode-set text-tag-wrap-mode-set "wrap-mode-set"
                         "gboolean" t t)
                        (:cffi priority text-tag-priority :int
                         "gtk_text_tag_get_priority"
                         "gtk_text_tag_set_priority")))

(define-g-object-class "GtkTextTagTable" text-tag-table
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_text_tag_table_get_type")
                       nil)

(define-g-object-class "GtkTreeModelFilter" tree-model-filter
                       (:superclass g-object :export t :interfaces
                        ("GtkTreeDragSource" "GtkTreeModel") :type-initializer
                        "gtk_tree_model_filter_get_type")
                       ((child-model tree-model-filter-child-model
                         "child-model" "GtkTreeModel" t nil)
                        (virtual-root tree-model-filter-virtual-root
                         "virtual-root" "GtkTreePath" t nil)))

(define-g-object-class "GtkTreeModelSort" tree-model-sort
                       (:superclass g-object :export t :interfaces
                        ("GtkTreeDragSource" "GtkTreeModel" "GtkTreeSortable")
                        :type-initializer "gtk_tree_model_sort_get_type")
                       ((model tree-model-sort-model "model" "GtkTreeModel" t
                         nil)))

(define-g-object-class "GtkTreeSelection" tree-selection
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_tree_selection_get_type")
                       ((:cffi mode tree-selection-mode gtk-selection-mode
                         "gtk_tree_selection_get_mode"
                         "gtk_tree_selection_set_mode")
                        (:cffi select-function tree-selection-select-function
                         nil tree-selection-get-selection-function
                         tree-selection-set-select-function)
                        (:cffi tree-view tree-selection-tree-view
                         (g-object tree-view)
                         "gtk_tree_selection_get_tree_view" nil)))

(define-g-object-class "GtkTreeStore" tree-store
                       (:superclass g-object :export t :interfaces
                        ("GtkBuildable" "GtkTreeDragDest" "GtkTreeDragSource"
                         "GtkTreeModel" "GtkTreeSortable")
                        :type-initializer "gtk_tree_store_get_type")
                       nil)

(define-g-object-class "GtkUIManager" ui-manager
                       (:superclass g-object :export t :interfaces
                        ("GtkBuildable"))
                       ((add-tearoffs ui-manager-add-tearoffs "add-tearoffs"
                         "gboolean" t t)
                        (ui ui-manager-ui "ui" "gchararray" t nil)
                        (:cffi accel-group ui-manager-accel-group g-object
                         "gtk_ui_manager_get_accel_group" nil)))

(define-g-object-class "GtkWindowGroup" window-group
                       (:superclass g-object :export t :interfaces nil
                        :type-initializer "gtk_window_group_get_type")
                       ((:cffi windows window-group-windows
                         (g-list (g-object gtk-window))
                         "gtk_window_group_list_windows" nil)))

(define-g-object-class "GtkToggleAction" toggle-action
                       (:superclass action :export t :interfaces
                        ("GtkBuildable") :type-initializer
                        "gtk_toggle_action_get_type")
                       ((active toggle-action-active "active" "gboolean" t t)
                        (draw-as-radio toggle-action-draw-as-radio
                         "draw-as-radio" "gboolean" t t)))

(define-g-object-class "GtkRecentAction" recent-action
                       (:superclass action :export t :interfaces
                        ("GtkBuildable" "GtkRecentChooser") :type-initializer
                        "gtk_recent_action_get_type")
                       ((show-numbers recent-action-show-numbers "show-numbers"
                         "gboolean" t t)))

(define-g-object-class "GtkRadioAction" radio-action
                       (:superclass toggle-action :export t :interfaces
                        ("GtkBuildable") :type-initializer
                        "gtk_radio_action_get_type")
                       ((current-value radio-action-current-value
                         "current-value" "gint" t t)
                        (group radio-action-group "group" "GtkRadioAction" nil
                         t)
                        (value radio-action-value "value" "gint" t t)))

(define-g-object-class "GtkItemFactory" item-factory
                       (:superclass gtk-object :export t :interfaces nil
                        :type-initializer "gtk_item_factory_get_type")
                       nil)

