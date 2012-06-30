;;; ----------------------------------------------------------------------------
;;; gtk.tree-view.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
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
;;;
;;; GtkTreeView
;;;
;;; A widget for displaying both trees and lists
;;;
;;; Synopsis
;;;
;;;     GtkTreeView
;;;     GtkTreeViewDropPosition
;;;     GtkTreeViewPrivate
;;;
;;;     gtk_tree_view_new
;;;     gtk_tree_view_get_level_indentation
;;;     gtk_tree_view_get_show_expanders
;;;     gtk_tree_view_set_level_indentation
;;;     gtk_tree_view_set_show_expanders
;;;     gtk_tree_view_new_with_model
;;;     gtk_tree_view_get_model
;;;     gtk_tree_view_set_model
;;;     gtk_tree_view_get_selection
;;;     gtk_tree_view_get_hadjustment
;;;     gtk_tree_view_set_hadjustment
;;;     gtk_tree_view_get_vadjustment
;;;     gtk_tree_view_set_vadjustment
;;;     gtk_tree_view_get_headers_visible
;;;     gtk_tree_view_set_headers_visible
;;;     gtk_tree_view_columns_autosize
;;;     gtk_tree_view_get_headers_clickable
;;;     gtk_tree_view_set_headers_clickable
;;;     gtk_tree_view_set_rules_hint
;;;     gtk_tree_view_get_rules_hint
;;;     gtk_tree_view_append_column
;;;     gtk_tree_view_remove_column
;;;     gtk_tree_view_insert_column
;;;     gtk_tree_view_insert_column_with_attributes
;;;     gtk_tree_view_insert_column_with_data_func
;;;     gtk_tree_view_get_n_columns
;;;     gtk_tree_view_get_column
;;;     gtk_tree_view_get_columns
;;;     gtk_tree_view_move_column_after
;;;     gtk_tree_view_set_expander_column
;;;     gtk_tree_view_get_expander_column
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
;;;     gtk_tree_view_set_reorderable
;;;     gtk_tree_view_get_reorderable
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
;;;     gtk_tree_view_enable_model_drag_source
;;;     gtk_tree_view_unset_rows_drag_source
;;;     gtk_tree_view_unset_rows_drag_dest
;;;     gtk_tree_view_set_drag_dest_row
;;;     gtk_tree_view_get_drag_dest_row
;;;     gtk_tree_view_get_dest_row_at_pos
;;;     gtk_tree_view_create_row_drag_icon
;;;     gtk_tree_view_set_enable_search
;;;     gtk_tree_view_get_enable_search
;;;     gtk_tree_view_get_search_column
;;;     gtk_tree_view_set_search_column
;;;     gtk_tree_view_get_search_equal_func
;;;     gtk_tree_view_set_search_equal_func
;;;     gtk_tree_view_get_search_entry
;;;     gtk_tree_view_set_search_entry
;;;     gtk_tree_view_get_search_position_func
;;;     gtk_tree_view_set_search_position_func
;;;     gtk_tree_view_get_fixed_height_mode
;;;     gtk_tree_view_set_fixed_height_mode
;;;     gtk_tree_view_get_hover_selection
;;;     gtk_tree_view_set_hover_selection
;;;     gtk_tree_view_get_hover_expand
;;;     gtk_tree_view_set_hover_expand
;;;     gtk_tree_view_set_destroy_count_func
;;;     gtk_tree_view_get_row_separator_func
;;;     gtk_tree_view_set_row_separator_func
;;;     gtk_tree_view_get_rubber_banding
;;;     gtk_tree_view_set_rubber_banding
;;;     gtk_tree_view_is_rubber_banding_active
;;;     gtk_tree_view_get_enable_tree_lines
;;;     gtk_tree_view_set_enable_tree_lines
;;;
;;;     GtkTreeViewGridLines
;;;
;;;     gtk_tree_view_get_grid_lines
;;;     gtk_tree_view_set_grid_lines
;;;     gtk_tree_view_set_tooltip_row
;;;     gtk_tree_view_set_tooltip_cell
;;;     gtk_tree_view_get_tooltip_context
;;;     gtk_tree_view_get_tooltip_column
;;;     gtk_tree_view_set_tooltip_column
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkTreeView
;;;
;;; Implemented Interfaces
;;;
;;; GtkTreeView implements AtkImplementorIface, GtkBuildable and GtkScrollable.
;;;
;;; Properties
;;;
;;;   "enable-grid-lines"        GtkTreeViewGridLines  : Read / Write
;;;   "enable-search"            gboolean              : Read / Write
;;;   "enable-tree-lines"        gboolean              : Read / Write
;;;   "expander-column"          GtkTreeViewColumn*    : Read / Write
;;;   "fixed-height-mode"        gboolean              : Read / Write
;;;   "headers-clickable"        gboolean              : Read / Write
;;;   "headers-visible"          gboolean              : Read / Write
;;;   "hover-expand"             gboolean              : Read / Write
;;;   "hover-selection"          gboolean              : Read / Write
;;;   "level-indentation"        gint                  : Read / Write
;;;   "model"                    GtkTreeModel*         : Read / Write
;;;   "reorderable"              gboolean              : Read / Write
;;;   "rubber-banding"           gboolean              : Read / Write
;;;   "rules-hint"               gboolean              : Read / Write
;;;   "search-column"            gint                  : Read / Write
;;;   "show-expanders"           gboolean              : Read / Write
;;;   "tooltip-column"           gint                  : Read / Write
;;;
;;; Style Properties
;;;
;;;   "allow-rules"              gboolean              : Read
;;;   "even-row-color"           GdkColor*             : Read
;;;   "expander-size"            gint                  : Read
;;;   "grid-line-pattern"        gchar*                : Read
;;;   "grid-line-width"          gint                  : Read
;;;   "horizontal-separator"     gint                  : Read
;;;   "indent-expanders"         gboolean              : Read
;;;   "odd-row-color"            GdkColor*             : Read
;;;   "tree-line-pattern"        gchar*                : Read
;;;   "tree-line-width"          gint                  : Read
;;;   "vertical-separator"       gint                  : Read
;;;
;;; Signals
;;;
;;;   "columns-changed"                                : Run Last
;;;   "cursor-changed"                                 : Run Last
;;;   "expand-collapse-cursor-row"                     : Action
;;;   "move-cursor"                                    : Action
;;;   "row-activated"                                  : Action
;;;   "row-collapsed"                                  : Run Last
;;;   "row-expanded"                                   : Run Last
;;;   "select-all"                                     : Action
;;;   "select-cursor-parent"                           : Action
;;;   "select-cursor-row"                              : Action
;;;   "start-interactive-search"                       : Action
;;;   "test-collapse-row"                              : Run Last
;;;   "test-expand-row"                                : Run Last
;;;   "toggle-cursor-row"                              : Action
;;;   "unselect-all"                                   : Action
;;;
;;; Description
;;;
;;; Widget that displays any object that implements the GtkTreeModel interface.
;;;
;;; Please refer to the tree widget conceptual overview for an overview of all
;;; the objects and data types related to the tree widget and how they work
;;; together.
;;;
;;; Several different coordinate systems are exposed in the GtkTreeView API.
;;; These are:
;;;
;;; Coordinate systems in GtkTreeView API
;;;
;;; Widget coordinates
;;;   Coordinates relative to the widget (usually widget->window).
;;;
;;; Bin window coordinates
;;;   Coordinates relative to the window that GtkTreeView renders to.
;;;
;;; Tree coordinates
;;;   Coordinates relative to the entire scrollable area of GtkTreeView. These
;;;   coordinates start at (0, 0) for row 0 of the tree.
;;;
;;; Several functions are available for converting between the different
;;; coordinate systems. The most common translations are between widget and bin
;;; window coordinates and between bin window and tree coordinates. For the
;;; former you can use gtk_tree_view_convert_widget_to_bin_window_coords() (and
;;; vice versa), for the latter
;;; gtk_tree_view_convert_bin_window_to_tree_coords() (and vice versa).
;;;
;;; GtkTreeView as GtkBuildable
;;;
;;; The GtkTreeView implementation of the GtkBuildable interface accepts
;;; GtkTreeViewColumn objects as <child> elements and exposes the internal
;;; GtkTreeSelection in UI definitions.
;;;
;;; Example 61. A UI definition fragment with GtkTreeView
;;;
;;; <object class="GtkTreeView" id="treeview">
;;;   <property name="model">liststore1</property>
;;;   <child>
;;;     <object class="GtkTreeViewColumn" id="test-column">
;;;       <property name="title">Test</property>
;;;       <child>
;;;         <object class="GtkCellRendererText" id="test-renderer"/>
;;;         <attributes>
;;;           <attribute name="text">1</attribute>
;;;         </attributes>
;;;       </child>
;;;     </object>
;;;   </child>
;;;   <child internal-child="selection">
;;;     <object class="GtkTreeSelection" id="selection">
;;;       <signal name="changed" handler="on_treeview_selection_changed"/>
;;;     </object>
;;;   </child>
;;; </object>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "enable-grid-lines" property
;;;
;;;   "enable-grid-lines"        GtkTreeViewGridLines  : Read / Write
;;;
;;; Whether grid lines should be drawn in the tree view.
;;;
;;; Default value: GTK_TREE_VIEW_GRID_LINES_NONE
;;;
;;; ----------------------------------------------------------------------------
;;; The "enable-search" property
;;;
;;;   "enable-search"            gboolean              : Read / Write
;;;
;;; View allows user to search through columns interactively.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "enable-tree-lines" property
;;;
;;;   "enable-tree-lines"        gboolean              : Read / Write
;;;
;;; Whether tree lines should be drawn in the tree view.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "expander-column" property
;;;
;;;   "expander-column"          GtkTreeViewColumn*    : Read / Write
;;;
;;; Set the column for the expander column.
;;;
;;; ----------------------------------------------------------------------------
;;; The "fixed-height-mode" property
;;;
;;;   "fixed-height-mode"        gboolean              : Read / Write
;;;
;;; Setting the ::fixed-height-mode property to TRUE speeds up GtkTreeView by
;;; assuming that all rows have the same height. Only enable this option if all
;;; rows are the same height. Please see gtk_tree_view_set_fixed_height_mode()
;;; for more information on this option.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "headers-clickable" property
;;;
;;;   "headers-clickable"        gboolean              : Read / Write
;;;
;;; Column headers respond to click events.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "headers-visible" property
;;;
;;;   "headers-visible"          gboolean              : Read / Write
;;;
;;; Show the column header buttons.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "hover-expand" property
;;;
;;;   "hover-expand"             gboolean              : Read / Write
;;;
;;; Enables or disables the hover expansion mode of tree_view. Hover expansion
;;; makes rows expand or collapse if the pointer moves over them.
;;;
;;; This mode is primarily intended for treeviews in popups, e.g. in GtkComboBox
;;; or GtkEntryCompletion.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "hover-selection" property
;;;
;;;   "hover-selection"          gboolean              : Read / Write
;;;
;;; Enables or disables the hover selection mode of tree_view. Hover selection
;;; makes the selected row follow the pointer. Currently, this works only for
;;; the selection modes GTK_SELECTION_SINGLE and GTK_SELECTION_BROWSE.
;;;
;;; This mode is primarily intended for treeviews in popups, e.g. in GtkComboBox
;;; or GtkEntryCompletion.
;;;
;;; Default value: FALSE
;;;
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "level-indentation" property
;;;
;;;   "level-indentation"        gint                  : Read / Write
;;;
;;; Extra indentation for each level.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 0
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "model" property
;;;
;;;   "model"                    GtkTreeModel*         : Read / Write
;;;
;;; The model for the tree view.
;;; The "reorderable" property
;;;
;;;   "reorderable"              gboolean              : Read / Write
;;;
;;; View is reorderable.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "rubber-banding" property
;;;
;;;   "rubber-banding"           gboolean              : Read / Write
;;;
;;; Whether to enable selection of multiple items by dragging the mouse pointer.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "rules-hint" property
;;;
;;;   "rules-hint"               gboolean              : Read / Write
;;;
;;; Set a hint to the theme engine to draw rows in alternating colors.
;;;
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "search-column" property
;;;
;;;   "search-column"            gint                  : Read / Write
;;;
;;; Model column to search through during interactive search.
;;;
;;; Allowed values: >= G_MAXULONG
;;;
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-expanders" property
;;;
;;;   "show-expanders"           gboolean              : Read / Write
;;;
;;; TRUE if the view has expanders.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "tooltip-column" property
;;;
;;;   "tooltip-column"           gint                  : Read / Write
;;;
;;; The column in the model containing the tooltip texts for the rows.
;;;
;;; Allowed values: >= G_MAXULONG
;;;
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "allow-rules" style property
;;;
;;;   "allow-rules"              gboolean              : Read
;;;
;;; Allow drawing of alternating color rows.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "even-row-color" style property
;;;
;;;   "even-row-color"           GdkColor*             : Read
;;;
;;; Color to use for even rows.
;;;
;;; ----------------------------------------------------------------------------
;;; The "expander-size" style property
;;;
;;;   "expander-size"            gint                  : Read
;;;
;;; Size of the expander arrow.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 14
;;;
;;; ----------------------------------------------------------------------------
;;; The "grid-line-pattern" style property
;;;
;;;   "grid-line-pattern"        gchar*                : Read
;;;
;;; Dash pattern used to draw the tree view grid lines.
;;;
;;; Default value: "\001\001"
;;;
;;; ----------------------------------------------------------------------------
;;; The "grid-line-width" style property
;;;
;;;   "grid-line-width"          gint                  : Read
;;;
;;; Width, in pixels, of the tree view grid lines.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "horizontal-separator" style property
;;;
;;;   "horizontal-separator"     gint                  : Read
;;;
;;; Horizontal space between cells. Must be an even number.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 2
;;;
;;; ----------------------------------------------------------------------------
;;; The "indent-expanders" style property
;;;
;;;   "indent-expanders"         gboolean              : Read
;;;
;;; Make the expanders indented.
;;;
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "odd-row-color" style property
;;;
;;;   "odd-row-color"            GdkColor*             : Read
;;;
;;; Color to use for odd rows.
;;;
;;; ----------------------------------------------------------------------------
;;; The "tree-line-pattern" style property
;;;
;;;   "tree-line-pattern"        gchar*                : Read
;;;
;;; Dash pattern used to draw the tree view lines.
;;;
;;; Default value: "\001\001"
;;;
;;; ----------------------------------------------------------------------------
;;; The "tree-line-width" style property
;;;
;;;   "tree-line-width"          gint                  : Read
;;;
;;; Width, in pixels, of the tree view lines.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "vertical-separator" style property
;;;
;;;   "vertical-separator"       gint                  : Read
;;;
;;; Vertical space between cells. Must be an even number.
;;;
;;; Allowed values: >= 0
;;;
;;; Default value: 2
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "columns-changed" signal
;;;
;;; void user_function (GtkTreeView *tree_view,
;;;                     gpointer     user_data)      : Run Last
;;;
;;; The number of columns of the treeview has changed.
;;;
;;; tree_view :
;;;     the object on which the signal is emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "cursor-changed" signal
;;;
;;; void user_function (GtkTreeView *tree_view,
;;;                     gpointer     user_data)      : Run Last
;;;
;;; The position of the cursor (focused cell) has changed.
;;;
;;; tree_view :
;;;     the object on which the signal is emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "expand-collapse-cursor-row" signal
;;;
;;; gboolean user_function (GtkTreeView *treeview,
;;;                         gboolean     arg1,
;;;                         gboolean     arg2,
;;;                         gboolean     arg3,
;;;                         gpointer     user_data)      : Action
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-cursor" signal
;;;
;;; gboolean user_function (GtkTreeView    *treeview,
;;;                         GtkMovementStep arg1,
;;;                         gint            arg2,
;;;                         gpointer        user_data)      : Action
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-activated" signal
;;;
;;; void user_function (GtkTreeView       *tree_view,
;;;                     GtkTreePath       *path,
;;;                     GtkTreeViewColumn *column,
;;;                     gpointer           user_data)      : Action
;;;
;;; The "row-activated" signal is emitted when the method
;;; gtk_tree_view_row_activated() is called or the user double clicks a treeview
;;; row. It is also emitted when a non-editable row is selected and one of the
;;; keys: Space, Shift+Space, Return or Enter is pressed.
;;;
;;; For selection handling refer to the tree widget conceptual overview as well
;;; as GtkTreeSelection.
;;;
;;; tree_view :
;;;     the object on which the signal is emitted
;;;
;;; path :
;;;     the GtkTreePath for the activated row
;;;
;;; column :
;;;     the GtkTreeViewColumn in which the activation occurred
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-collapsed" signal
;;;
;;; void user_function (GtkTreeView *tree_view,
;;;                     GtkTreeIter *iter,
;;;                     GtkTreePath *path,
;;;                     gpointer     user_data)      : Run Last
;;;
;;; The given row has been collapsed (child nodes are hidden).
;;;
;;; tree_view :
;;;     the object on which the signal is emitted
;;;
;;; iter :
;;;     the tree iter of the collapsed row
;;;
;;; path :
;;;     a tree path that points to the row
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-expanded" signal
;;;
;;; void user_function (GtkTreeView *tree_view,
;;;                     GtkTreeIter *iter,
;;;                     GtkTreePath *path,
;;;                     gpointer     user_data)      : Run Last
;;;
;;; The given row has been expanded (child nodes are shown).
;;;
;;; tree_view :
;;;     the object on which the signal is emitted
;;;
;;; iter :
;;;     the tree iter of the expanded row
;;;
;;; path :
;;;     a tree path that points to the row
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "select-all" signal
;;;
;;; gboolean user_function (GtkTreeView *treeview,
;;;                         gpointer     user_data)      : Action
;;;
;;; ----------------------------------------------------------------------------
;;; The "select-cursor-parent" signal
;;;
;;; gboolean user_function (GtkTreeView *treeview,
;;;                         gpointer     user_data)      : Action
;;;
;;; ----------------------------------------------------------------------------
;;; The "select-cursor-row" signal
;;;
;;; gboolean user_function (GtkTreeView *treeview,
;;;                         gboolean     arg1,
;;;                         gpointer     user_data)      : Action
;;;
;;; ----------------------------------------------------------------------------
;;; The "start-interactive-search" signal
;;;
;;; gboolean user_function (GtkTreeView *treeview,
;;;                         gpointer     user_data)      : Action
;;;
;;; ----------------------------------------------------------------------------
;;; The "test-collapse-row" signal
;;;
;;; gboolean user_function (GtkTreeView *tree_view,
;;;                         GtkTreeIter *iter,
;;;                         GtkTreePath *path,
;;;                         gpointer     user_data)      : Run Last
;;;
;;; The given row is about to be collapsed (hide its children nodes). Use this
;;; signal if you need to control the collapsibility of individual rows.
;;;
;;; tree_view :
;;;     the object on which the signal is emitted
;;;
;;; iter :
;;;     the tree iter of the row to collapse
;;;
;;; path :
;;;     a tree path that points to the row
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     FALSE to allow collapsing, TRUE to reject
;;;
;;; ----------------------------------------------------------------------------
;;; The "test-expand-row" signal
;;;
;;; gboolean user_function (GtkTreeView *tree_view,
;;;                         GtkTreeIter *iter,
;;;                         GtkTreePath *path,
;;;                         gpointer     user_data)      : Run Last
;;;
;;; The given row is about to be expanded (show its children nodes). Use this
;;; signal if you need to control the expandability of individual rows.
;;;
;;; tree_view :
;;;     the object on which the signal is emitted
;;;
;;; iter :
;;;     the tree iter of the row to expand
;;;
;;; path :
;;;     a tree path that points to the row
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Returns :
;;;     FALSE to allow expansion, TRUE to reject
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggle-cursor-row" signal
;;;
;;; gboolean user_function (GtkTreeView *treeview,
;;;                         gpointer     user_data)      : Action
;;;
;;; ----------------------------------------------------------------------------
;;; The "unselect-all" signal
;;;
;;; gboolean user_function (GtkTreeView *treeview,
;;;                         gpointer     user_data)      : Action
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeView
;;;
;;; struct GtkTreeView;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTreeView" gtk-tree-view
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkScrollable")
   :type-initializer "gtk_tree_view_get_type")
  ((enable-grid-lines
    gtk-tree-view-enable-grid-lines
    "enable-grid-lines" "GtkTreeViewGridLines" t t)
   (enable-search
    gtk-tree-view-enable-search
    "enable-search" "gboolean" t t)
   (enable-tree-lines
    gtk-tree-view-enable-tree-lines
    "enable-tree-lines" "gboolean" t t)
   (expander-column
    gtk-tree-view-expander-column
    "expander-column" "GtkTreeViewColumn" t t)
   (fixed-height-mode
    gtk-tree-view-fixed-height-mode
    "fixed-height-mode" "gboolean" t t)
   (headers-clickable
    gtk-tree-view-headers-clickable
    "headers-clickable" "gboolean" t t)
   (headers-visible
    gtk-tree-view-headers-visible
    "headers-visible" "gboolean" t t)
   (hover-expand
    gtk-tree-view-hover-expand
    "hover-expand" "gboolean" t t)
   (hover-selection
    gtk-tree-view-hover-selection
    "hover-selection" "gboolean" t t)
   (level-indentation
    gtk-tree-view-level-indentation
    "level-indentation" "gint" t t)
   (model
    gtk-tree-view-model
    "model" "GtkTreeModel" t t)
   (reorderable
    gtk-tree-view-reorderable
    "reorderable" "gboolean" t t)
   (rubber-banding
    gtk-tree-view-rubber-banding
    "rubber-banding" "gboolean" t t)
   (rules-hint
    gtk-tree-view-rules-hint
    "rules-hint" "gboolean" t t)
   (search-column
    gtk-tree-view-search-column
    "search-column" "gint" t t)
   (show-expanders
    gtk-tree-view-show-expanders
    "show-expanders" "gboolean" t t)
   (tooltip-column
    gtk-tree-view-tooltip-column
    "tooltip-column" "gint" t t)
   (:cffi selection
          gtk-tree-view-selection g-object
          "gtk_tree_view_get_selection" nil)
   (:cffi column-drag-function
          gtk-tree-view-column-drag-function nil
          nil gtk-tree-view-set-column-drag-function)
   (:cffi bin-window
          gtk-tree-view-bin-window g-object
          "gtk_tree_view_get_bin_window" nil)
   (:cffi search-equal-func
          gtk-tree-view-search-equal-func nil
          nil gtk-tree-view-set-search-equal-func)
   (:cffi search-entry
          gtk-tree-view-search-entry g-object
          "gtk_tree_view_get_search_entry" "gtk_tree_view_set_search_entry")
   (:cffi search-position-func
          gtk-tree-view-search-position-func nil
          nil gtk-tree-view-set-search-position-func)
   (:cffi row-separator-func
          gtk-tree-view-row-separator-func nil
          nil gtk-tree-view-set-row-separartor-func)))

;;; ----------------------------------------------------------------------------
;;; enum GtkTreeViewDropPosition
;;;
;;; typedef enum {
;;;   /* drop before/after this row */
;;;   GTK_TREE_VIEW_DROP_BEFORE,
;;;   GTK_TREE_VIEW_DROP_AFTER,
;;;   /* drop as a child of this row (with fallback to before or after
;;;    * if into is not possible)
;;;    */
;;;   GTK_TREE_VIEW_DROP_INTO_OR_BEFORE,
;;;   GTK_TREE_VIEW_DROP_INTO_OR_AFTER
;;; } GtkTreeViewDropPosition;
;;;
;;; An enum for determining where a dropped row goes.
;;;
;;; GTK_TREE_VIEW_DROP_BEFORE
;;;     dropped row is inserted before
;;;
;;; GTK_TREE_VIEW_DROP_AFTER
;;;     dropped row is inserted after
;;;
;;; GTK_TREE_VIEW_DROP_INTO_OR_BEFORE
;;;     dropped row becomes a child or is inserted before
;;;
;;; GTK_TREE_VIEW_DROP_INTO_OR_AFTER
;;;     dropped row becomes a child or is inserted after
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTreeViewDropPosition" gtk-tree-view-drop-position
  (:export t
   :type-initializer "gtk_tree_view_drop_position_get_type")
  (:before 0)
  (:after 1)
  (:into-or-before 2)
  (:into-or-after 3))

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewPrivate
;;;
;;; typedef struct _GtkTreeViewPrivate GtkTreeViewPrivate;
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewSearchEqualFunc ()
;;;
;;; gboolean (*GtkTreeViewSearchEqualFunc) (GtkTreeModel *model,
;;;                                         gint column,
;;;                                         const gchar *key,
;;;                                         GtkTreeIter *iter,
;;;                                         gpointer search_data);
;;;
;;; A function used for checking whether a row in model matches a search key
;;; string entered by the user. Note the return value is reversed from what you
;;; would normally expect, though it has some similarity to strcmp() returning 0
;;; for equal strings.
;;;
;;; model :
;;;     the GtkTreeModel being searched
;;;
;;; column :
;;;     the search column set by gtk_tree_view_set_search_column()
;;;
;;; key :
;;;     the key string to compare with
;;;
;;; iter :
;;;     a GtkTreeIter pointing the row of model that should be compared with
;;;     key.
;;;
;;; search_data :
;;;     user data from gtk_tree_view_set_search_equal_func()
;;;
;;; Returns :
;;;     FALSE if the row matches, TRUE otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_new ()
;;;
;;; GtkWidget * gtk_tree_view_new (void);
;;;
;;; Creates a new GtkTreeView widget.
;;;
;;; Returns :
;;;     A newly created GtkTreeView widget.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-new))

(defun gtk-tree-view-new ()
  (make-instance 'gtk-tree-view))

(export 'gtk-tree-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_level_indentation ()
;;;
;;; gint gtk_tree_view_get_level_indentation (GtkTreeView *tree_view);
;;;
;;; Returns the amount, in pixels, of extra indentation for child levels in
;;; tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView.
;;;
;;; Returns :
;;;     the amount of extra indentation for child levels in tree_view. A return
;;;     value of 0 means that this feature is disabled.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-level-indentation))

(defun gtk-tree-view-get-level-indentation (tree-view)
  (gtk-tree-view-level-indentation tree-view))

(export 'gtk-tree-view-get-level-indentation)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_show_expanders ()
;;;
;;; gboolean gtk_tree_view_get_show_expanders (GtkTreeView *tree_view);
;;;
;;; Returns whether or not expanders are drawn in tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView.
;;;
;;; Returns :
;;;     TRUE if expanders are drawn in tree_view, FALSE otherwise.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-show-expanders))

(defun gtk-tree-view-get-show-expanders (tree-view)
  (gtk-tree-view-show-expanders tree-view))

(export 'gtk-tree-view-get-show-expanders)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_level_indentation ()
;;;
;;; void gtk_tree_view_set_level_indentation (GtkTreeView *tree_view,
;;;                                           gint indentation);
;;;
;;; Sets the amount of extra indentation for child levels to use in tree_view in
;;; addition to the default indentation. The value should be specified in
;;; pixels, a value of 0 disables this feature and in this case only the default
;;; indentation will be used. This does not have any visible effects for lists.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; indentation :
;;;     the amount, in pixels, of extra indentation in tree_view.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-level-indentation indentation))

(defun gtk-tree-view-set-level-indentation (tree-view indentation)
  (setf (gtk-tree-view-level-indentation tree-view) indentation))

(export 'gtk-tree-view-set-level-indentation)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_show_expanders ()
;;;
;;; void gtk_tree_view_set_show_expanders (GtkTreeView *tree_view,
;;;                                        gboolean enabled);
;;;
;;; Sets whether to draw and enable expanders and indent child rows in
;;; tree_view. When disabled there will be no expanders visible in trees and
;;; there will be no way to expand and collapse rows by default. Also note that
;;; hiding the expanders will disable the default indentation. You can set a
;;; custom indentation in this case using gtk_tree_view_set_level_indentation().
;;; This does not have any visible effects for lists.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; enabled :
;;;     TRUE to enable expander drawing, FALSE otherwise.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-show-expanders))

(defun gtk-tree-view-set-show-expanders (tree-view enabled)
  (setf (gtk-tree-view-show-expanders tree-view) enabled))

(export 'gtk-tree-view-set-show-expanders)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_new_with_model ()
;;;
;;; GtkWidget * gtk_tree_view_new_with_model (GtkTreeModel *model);
;;;
;;; Creates a new GtkTreeView widget with the model initialized to model.
;;;
;;; model :
;;;     the model.
;;;
;;; Returns :
;;;     A newly created GtkTreeView widget.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-new-with-model))

(defun gtk-tree-view-new-with-model (model)
  (make-instance 'gtk-tree-view
                 :model model))

(export 'gtk-tree-view-new-with-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_model ()
;;;
;;; GtkTreeModel * gtk_tree_view_get_model (GtkTreeView *tree_view);
;;;
;;; Returns the model the GtkTreeView is based on. Returns NULL if the model is
;;; unset.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     A GtkTreeModel, or NULL if none is currently being used
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-model))

(defun gtk-tree-view-get-model (tree-view)
  (gtk-tree-view-model tree-view))

(export 'gtk-tree-view-get-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_model ()
;;;
;;; void gtk_tree_view_set_model (GtkTreeView *tree_view, GtkTreeModel *model);
;;;
;;; Sets the model for a GtkTreeView. If the tree_view already has a model set,
;;; it will remove it before setting the new model. If model is NULL, then it
;;; will unset the old model.
;;;
;;; tree_view :
;;;     A GtkTreeNode.
;;;
;;; model :
;;;     The model
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-model))

(defun gtk-tree-view-set-model (tree-view model)
  (setf (gtk-tree-view-model tree-view) model))

(export 'gtk-tree-view-set-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_selection ()
;;;
;;; GtkTreeSelection * gtk_tree_view_get_selection (GtkTreeView *tree_view);
;;;
;;; Gets the GtkTreeSelection associated with tree_view.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; Returns :
;;;     A GtkTreeSelection object
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_selection" gtk-tree-view-get-selection)
    (g-object gtk-tree-selection)
  (tree-view (g-object gtk-tree-view)))

(export 'gtk-tree-view-get-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_hadjustment ()
;;;
;;; GtkAdjustment * gtk_tree_view_get_hadjustment (GtkTreeView *tree_view);
;;;
;;; Warning
;;;
;;; gtk_tree_view_get_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_get_hadjustment()
;;;
;;; Gets the GtkAdjustment currently being used for the horizontal aspect.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     A GtkAdjustment object, or NULL if none is currently being used
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_hadjustment ()
;;;
;;; void gtk_tree_view_set_hadjustment (GtkTreeView *tree_view,
;;;                                     GtkAdjustment *adjustment);
;;;
;;; Warning
;;;
;;; gtk_tree_view_set_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_set_hadjustment()
;;;
;;; Sets the GtkAdjustment for the current horizontal aspect.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; adjustment :
;;;     The GtkAdjustment to set, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_vadjustment ()
;;;
;;; GtkAdjustment * gtk_tree_view_get_vadjustment (GtkTreeView *tree_view);
;;;
;;; Warning
;;;
;;; gtk_tree_view_get_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_get_vadjustment()
;;;
;;; Gets the GtkAdjustment currently being used for the vertical aspect.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     A GtkAdjustment object, or NULL if none is currently being used
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_vadjustment ()
;;;
;;; void gtk_tree_view_set_vadjustment (GtkTreeView *tree_view,
;;;                                     GtkAdjustment *adjustment);
;;;
;;; Warning
;;;
;;; gtk_tree_view_set_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly-written code. Use
;;; gtk_scrollable_set_vadjustment()
;;;
;;; Sets the GtkAdjustment for the current vertical aspect.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; adjustment :
;;;     The GtkAdjustment to set, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_headers_visible ()
;;;
;;; gboolean gtk_tree_view_get_headers_visible (GtkTreeView *tree_view);
;;;
;;; Returns TRUE if the headers on the tree_view are visible.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; Returns :
;;;     Whether the headers are visible or not.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-headers-visible))

(defun gtk-tree-view-get-headers-visible (tree-view)
  (gtk-tree-view-headers-visible tree-view))

(export 'gtk-tree-view-get-headers-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_headers_visible ()
;;;
;;; void gtk_tree_view_set_headers_visible (GtkTreeView *tree_view,
;;;                                         gboolean headers_visible);
;;;
;;; Sets the visibility state of the headers.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; headers_visible :
;;;     TRUE if the headers are visible
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-headers-visible))

(defun gtk-tree-view-set-headers-visible (tree-view headers-visible)
  (setf (gtk-tree-view-headers-visible tree-view) headers-visible))

(export 'gtk-tree-view-set-headers-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_columns_autosize ()
;;;
;;; void gtk_tree_view_columns_autosize (GtkTreeView *tree_view);
;;;
;;; Resizes all columns to their optimal width. Only works after the treeview
;;; has been realized.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_columns_autosize" gtk-tree-view-columns-autosize) :void
  (tree-view g-object))

(export 'gtk-tree-view-columns-autosize)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_headers_clickable ()
;;;
;;; gboolean gtk_tree_view_get_headers_clickable (GtkTreeView *tree_view);
;;;
;;; Returns whether all header columns are clickable.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; Returns :
;;;     TRUE if all header columns are clickable, otherwise FALSE
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-headers-clickable))

(defun gtk-tree-view-get-headers-clickable (tree-view)
  (gtk-tree-view-headers-clickable tree-view))

(export 'gtk-tree-view-get-headers-clickable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_headers_clickable ()
;;;
;;; void gtk_tree_view_set_headers_clickable (GtkTreeView *tree_view,
;;;                                           gboolean setting);
;;;
;;; Allow the column title buttons to be clicked.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; setting :
;;;     TRUE if the columns are clickable.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-headers-clickable))

(defun gtk-tree-view-set-headers-clickable (tree-view headers-clickable)
  (setf (gtk-tree-view-headers-clickable tree-view) headers-clickable))

(export 'gtk-tree-view-set-headers-clickable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_rules_hint ()
;;;
;;; void gtk_tree_view_set_rules_hint (GtkTreeView *tree_view,
;;;                                    gboolean setting);
;;;
;;; This function tells GTK+ that the user interface for your application
;;; requires users to read across tree rows and associate cells with one
;;; another. By default, GTK+ will then render the tree with alternating row
;;; colors. Do not use it just because you prefer the appearance of the ruled
;;; tree; that's a question for the theme. Some themes will draw tree rows in
;;; alternating colors even when rules are turned off, and users who prefer that
;;; appearance all the time can choose those themes. You should call this
;;; function only as a semantic hint to the theme engine that your tree makes
;;; alternating colors useful from a functional standpoint (since it has lots of
;;; columns, generally).
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; setting :
;;;     TRUE if the tree requires reading across rows
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-rules-hint))

(defun gtk-tree-view-set-rules-hint (tree-view setting)
  (setf (gtk-tree-view-rules-hint tree-view) setting))

(export 'gtk-tree-view-set-rules-hint)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_rules_hint ()
;;;
;;; gboolean gtk_tree_view_get_rules_hint (GtkTreeView *tree_view);
;;;
;;; Gets the setting set by gtk_tree_view_set_rules_hint().
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     TRUE if rules are useful for the user of this tree
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-rules-hint))

(defun gtk-tree-view-get-rules-hint (tree-view)
  (gtk-tree-view-rules-hint tree-view))

(export 'gtk-tree-view-get-rules-hint)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_append_column ()
;;;
;;; gint gtk_tree_view_append_column (GtkTreeView *tree_view,
;;;                                   GtkTreeViewColumn *column);
;;;
;;; Appends column to the list of columns. If tree_view has "fixed_height" mode
;;; enabled, then column must have its "sizing" property set to be
;;; GTK_TREE_VIEW_COLUMN_FIXED.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; column :
;;;     The GtkTreeViewColumn to add.
;;;
;;; Returns :
;;;     The number of columns in tree_view after appending.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_append_column" gtk-tree-view-append-column) :int
  (tree-view g-object)
  (column g-object))

(export 'gtk-tree-view-append-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_remove_column ()
;;;
;;; gint gtk_tree_view_remove_column (GtkTreeView *tree_view,
;;;                                   GtkTreeViewColumn *column);
;;;
;;; Removes column from tree_view.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; column :
;;;     The GtkTreeViewColumn to remove.
;;;
;;; Returns :
;;;     The number of columns in tree_view after removing.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_remove_column" gtk-tree-view-remove-column) :int
  (tree-view (g-object gtk-tree-view))
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-remove-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column ()
;;;
;;; gint gtk_tree_view_insert_column (GtkTreeView *tree_view,
;;;                                   GtkTreeViewColumn *column,
;;;                                   gint pos);
;;;
;;; This inserts the column into the tree_view at position. If position is -1,
;;; then the column is inserted at the end. If tree_view has "fixed_height" mode
;;; enabled, then column must have its "sizing" property set to be
;;; GTK_TREE_VIEW_COLUMN_FIXED.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; column :
;;;     The GtkTreeViewColumn to be inserted.
;;;
;;; pos :
;;;     The position to insert column in.
;;;
;;; Returns :
;;;     The number of columns in tree_view after insertion.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_insert_column" gtk-tree-view-insert-column) :int
  (tree-view g-object)
  (column g-object)
  (pos :int))

(export 'gtk-tree-view-insert-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column_with_attributes ()
;;;
;;; gint gtk_tree_view_insert_column_with_attributes (GtkTreeView *tree_view,
;;;                                                   gint position,
;;;                                                   const gchar *title,
;;;                                                   GtkCellRenderer *cell,
;;;                                                   ...);
;;;
;;; Creates a new GtkTreeViewColumn and inserts it into the tree_view at
;;; position. If position is -1, then the newly created column is inserted at
;;; the end. The column is initialized with the attributes given. If tree_view
;;; has "fixed_height" mode enabled, then the new column will have its sizing
;;; property set to be GTK_TREE_VIEW_COLUMN_FIXED.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; position :
;;;     The position to insert the new column in
;;;
;;; title :
;;;     The title to set the header to
;;;
;;; cell :
;;;     The GtkCellRenderer
;;;
;;; ... :
;;;     A NULL-terminated list of attributes
;;;
;;; Returns :
;;;     The number of columns in tree_view after insertion.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_insert_column_with_data_func ()
;;;
;;; gint gtk_tree_view_insert_column_with_data_func (GtkTreeView *tree_view,
;;;                                                  gint position,
;;;                                                  const gchar *title,
;;;                                                  GtkCellRenderer *cell,
;;;                                                  GtkTreeCellDataFunc func,
;;;                                                  gpointer data,
;;;                                                  GDestroyNotify dnotify);
;;;
;;; Convenience function that inserts a new column into the GtkTreeView with the
;;; given cell renderer and a GtkCellDataFunc to set cell renderer attributes
;;; (normally using data from the model). See also
;;; gtk_tree_view_column_set_cell_data_func(),
;;; gtk_tree_view_column_pack_start(). If tree_view has "fixed_height" mode
;;; enabled, then the new column will have its "sizing" property set to be
;;; GTK_TREE_VIEW_COLUMN_FIXED.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; position :
;;;     Position to insert, -1 for append
;;;
;;; title :
;;;     column title
;;;
;;; cell :
;;;     cell renderer for column
;;;
;;; func :
;;;     function to set attributes of cell renderer
;;;
;;; data :
;;;     data for func
;;;
;;; dnotify :
;;;     destroy notifier for data
;;;
;;; Returns :
;;;     number of columns in the tree view post-insert
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_n_columns ()
;;;
;;; guint gtk_tree_view_get_n_columns (GtkTreeView *tree_view);
;;;
;;; Queries the number of columns in the given tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     The number of columns in the tree_view
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_n_columns" gtk-tree-view-get-n-columns) :int
  (tree-view (g-object gtk-tree-view)))

(export 'gtk-tree-view-get-n-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_column ()
;;;
;;; GtkTreeViewColumn * gtk_tree_view_get_column (GtkTreeView *tree_view,
;;;                                               gint n);
;;;
;;; Gets the GtkTreeViewColumn at the given position in the tree_view.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; n :
;;;     The position of the column, counting from 0.
;;;
;;; Returns :
;;;     The GtkTreeViewColumn, or NULL if the position is outside the range of
;;;     columns.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_column" gtk-tree-view-get-column) g-object
  (tree-view g-object)
  (n :int))

(export 'gtk-tree-view-get-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_columns ()
;;;
;;; GList * gtk_tree_view_get_columns (GtkTreeView *tree_view);
;;;
;;; Returns a GList of all the GtkTreeViewColumn s currently in tree_view. The
;;; returned list must be freed with g_list_free().
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     A list of GtkTreeViewColumn s.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_columns" gtk-tree-view-columns) (g-list g-object)
  (tree-view g-object))

(export 'gtk-tree-view-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_move_column_after ()
;;;
;;; void gtk_tree_view_move_column_after (GtkTreeView *tree_view,
;;;                                       GtkTreeViewColumn *column,
;;;                                       GtkTreeViewColumn *base_column);
;;;
;;; Moves column to be after to base_column. If base_column is NULL, then column
;;; is placed in the first position.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; column :
;;;     The GtkTreeViewColumn to be moved.
;;;
;;; base_column :
;;;     The GtkTreeViewColumn to be moved relative to, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_move_column_after" gtk-tree-view-move-column-after)
    :void
  (tree-view g-object)
  (column g-object)
  (base-column g-object))

(export 'gtk-tree-view-move-column-after)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_expander_column ()
;;;
;;; void gtk_tree_view_set_expander_column (GtkTreeView *tree_view,
;;;                                         GtkTreeViewColumn *column);
;;;
;;; Sets the column to draw the expander arrow at. It must be in tree_view. If
;;; column is NULL, then the expander arrow is always at the first visible
;;; column.
;;;
;;; If you do not want expander arrow to appear in your tree, set the expander
;;; column to a hidden column.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; column :
;;;     NULL, or the column to draw the expander arrow at.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-expander-column))

(defun gtk-tree-view-set-expander-column (tree-view column)
  (setf (gtk-tree-view-expander-column tree-view) column))

(export 'gtk-tree-view-set-expander-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_expander_column ()
;;;
;;; GtkTreeViewColumn * gtk_tree_view_get_expander_column
;;;                                                    (GtkTreeView *tree_view);
;;;
;;; Returns the column that is the current expander column. This column has the
;;; expander arrow drawn next to it.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     The expander column.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-expander-column))

(defun gtk-tree-view-get-expander-column (tree-view)
  (gtk-tree-view-expander-column tree-view))

(export 'gtk-tree-view-get-expander-column)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewColumnDropFunc ()
;;;
;;; gboolean (*GtkTreeViewColumnDropFunc) (GtkTreeView *tree_view,
;;;                                        GtkTreeViewColumn *column,
;;;                                        GtkTreeViewColumn *prev_column,
;;;                                        GtkTreeViewColumn *next_column,
;;;                                        gpointer data);
;;;
;;; Function type for determining whether column can be dropped in a particular
;;; spot (as determined by prev_column and next_column). In left to right
;;; locales, prev_column is on the left of the potential drop spot, and
;;; next_column is on the right. In right to left mode, this is reversed. This
;;; function should return TRUE if the spot is a valid drop spot. Please note
;;; that returning TRUE does not actually indicate that the column drop was
;;; made, but is meant only to indicate a possible drop spot to the user.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; column :
;;;     The GtkTreeViewColumn being dragged
;;;
;;; prev_column :
;;;     A GtkTreeViewColumn on one side of column
;;;
;;; next_column :
;;;     A GtkTreeViewColumn on the other side of column
;;;
;;; data :
;;;     user data
;;;
;;; Returns :
;;;     TRUE, if column can be dropped in this spot
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-column-drop-func-cb :boolean
    ((tree-view g-object)
     (column g-object)
     (prev-column g-object)
     (next-column g-object)
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (funcall fn tree-view column prev-column next-column)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_column_drag_function ()
;;;
;;; void gtk_tree_view_set_column_drag_function (GtkTreeView *tree_view,
;;;                                              GtkTreeViewColumnDropFunc func,
;;;                                              gpointer user_data,
;;;                                              GDestroyNotify destroy);
;;;
;;; Sets a user function for determining where a column may be dropped when
;;; dragged. This function is called on every column pair in turn at the
;;; beginning of a column drag to determine where a drop can take place. The
;;; arguments passed to func are: the tree_view, the GtkTreeViewColumn being
;;; dragged, the two GtkTreeViewColumn s determining the drop spot, and
;;; user_data. If either of the GtkTreeViewColumn arguments for the drop spot
;;; are NULL, then they indicate an edge. If func is set to be NULL, then
;;; tree_view reverts to the default behavior of allowing all columns to be
;;; dropped everywhere.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; func :
;;;     A function to determine which columns are reorderable, or NULL
;;;
;;; user_data :
;;;     User data to be passed to func, or NULL.
;;;
;;; destroy :
;;;     Destroy notifier for user_data, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_column_drag_function"
          %gtk-tree-view-set-column-drag-function) :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-column-drag-fuction (tree-view func)
  (%gtk-tree-view-set-column-drag-function
                                   tree-view
                                   (callback gtk-tree-view-column-drop-func-cb)
                                   (allocate-stable-pointer func)
                                   (callback stable-pointer-destroy-notify-cb)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_scroll_to_point ()
;;;
;;; void gtk_tree_view_scroll_to_point (GtkTreeView *tree_view,
;;;                                     gint tree_x,
;;;                                     gint tree_y);
;;;
;;; Scrolls the tree view such that the top-left corner of the visible area is
;;; tree_x, tree_y, where tree_x and tree_y are specified in tree coordinates.
;;; The tree_view must be realized before this function is called. If it isn't,
;;; you probably want to be using gtk_tree_view_scroll_to_cell().
;;;
;;; If either tree_x or tree_y are -1, then that direction isn't scrolled.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; tree_x :
;;;     X coordinate of new top-left pixel of visible area, or -1
;;;
;;; tree_y :
;;;     Y coordinate of new top-left pixel of visible area, or -1
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_scroll_to_point" gtk-tree-view-scroll-to-point) :void
  (tree-view g-object)
  (tree-x :int)
  (tree-y :int))

(export 'gtk-tree-view-scroll-to-point)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_scroll_to_cell ()
;;;
;;; void gtk_tree_view_scroll_to_cell (GtkTreeView *tree_view,
;;;                                    GtkTreePath *path,
;;;                                    GtkTreeViewColumn *column,
;;;                                    gboolean use_align,
;;;                                    gfloat row_align,
;;;                                    gfloat col_align);
;;;
;;; Moves the alignments of tree_view to the position specified by column and
;;; path. If column is NULL, then no horizontal scrolling occurs. Likewise, if
;;; path is NULL no vertical scrolling occurs. At a minimum, one of column or
;;; path need to be non-NULL. row_align determines where the row is placed, and
;;; col_align determines where column is placed. Both are expected to be between
;;; 0.0 and 1.0. 0.0 means left/top alignment, 1.0 means right/bottom alignment,
;;; 0.5 means center.
;;;
;;; If use_align is FALSE, then the alignment arguments are ignored, and the
;;; tree does the minimum amount of work to scroll the cell onto the screen.
;;; This means that the cell will be scrolled to the edge closest to its current
;;; position. If the cell is currently visible on the screen, nothing is done.
;;;
;;; This function only works if the model is set, and path is a valid row on the
;;; model. If the model changes before the tree_view is realized, the centered
;;; path will be modified to reflect this change.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; path :
;;;     The path of the row to move to, or NULL.
;;;
;;; column :
;;;     The GtkTreeViewColumn to move horizontally to, or NULL.
;;;
;;; use_align :
;;;     whether to use alignment arguments, or FALSE.
;;;
;;; row_align :
;;;     The vertical alignment of the row specified by path.
;;;
;;; col_align :
;;;     The horizontal alignment of the column specified by column.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_scroll_to_cell" %gtk-tree-view-scroll-to-cell) :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (column g-object)
  (use-align :boolean)
  (row-align :float)
  (col-align :float))

(defun gtk-tree-view-scroll-to-cell (tree-view
                                     path column
                                     &optional
                                     (row-align 0.5 row-align-supplied-p)
                                     (col-align 0.5 col-align-supplied-p))
  (%gtk-tree-view-scroll-to-cell tree-view
                                 path
                                 column
                                 (or row-align-supplied-p col-align-supplied-p)
                                 row-align
                                 col-align))

(export 'gtk-tree-view-scroll-to-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_cursor ()
;;;
;;; void gtk_tree_view_set_cursor (GtkTreeView *tree_view,
;;;                                GtkTreePath *path,
;;;                                GtkTreeViewColumn *focus_column,
;;;                                gboolean start_editing);
;;;
;;; Sets the current keyboard focus to be at path, and selects it. This is
;;; useful when you want to focus the user's attention on a particular row. If
;;; focus_column is not NULL, then focus is given to the column specified by it.
;;; Additionally, if focus_column is specified, and start_editing is TRUE, then
;;; editing should be started in the specified cell. This function is often
;;; followed by gtk_widget_grab_focus (tree_view) in order to give keyboard
;;; focus to the widget. Please note that editing can only happen when the
;;; widget is realized.
;;;
;;; If path is invalid for model, the current cursor (if any) will be unset and
;;; the function will return without failing.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; path :
;;;     A GtkTreePath
;;;
;;; focus_column :
;;;     A GtkTreeViewColumn, or NULL.
;;;
;;; start_editing :
;;;     TRUE if the specified cell should start being edited.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_cursor" %gtk-tree-view-set-cursor) :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (focus-column g-object)
  (start-editing :boolean))

(defun gtk-tree-view-set-cursor (tree-view path &key focus-column start-editing)
  (%gtk-tree-view-set-cursor tree-view path focus-column start-editing))

(export 'gtk-tree-view-set-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_cursor_on_cell ()
;;;
;;; void gtk_tree_view_set_cursor_on_cell (GtkTreeView *tree_view,
;;;                                        GtkTreePath *path,
;;;                                        GtkTreeViewColumn *focus_column,
;;;                                        GtkCellRenderer *focus_cell,
;;;                                        gboolean start_editing);
;;;
;;; Sets the current keyboard focus to be at path, and selects it. This is
;;; useful when you want to focus the user's attention on a particular row. If
;;; focus_column is not NULL, then focus is given to the column specified by it.
;;; If focus_column and focus_cell are not NULL, and focus_column contains 2 or
;;; more editable or activatable cells, then focus is given to the cell
;;; specified by focus_cell. Additionally, if focus_column is specified, and
;;; start_editing is TRUE, then editing should be started in the specified cell.
;;; This function is often followed by gtk_widget_grab_focus (tree_view) in
;;; order to give keyboard focus to the widget. Please note that editing can
;;; only happen when the widget is realized.
;;;
;;; If path is invalid for model, the current cursor (if any) will be unset and
;;; the function will return without failing.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; path :
;;;     A GtkTreePath
;;;
;;; focus_column :
;;;     A GtkTreeViewColumn, or NULL.
;;;
;;; focus_cell :
;;;     A GtkCellRenderer, or NULL.
;;;
;;; start_editing :
;;;     TRUE if the specified cell should start being edited.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_cursor_on_cell" %gtk-tree-view-set-cursor-on-cell)
    :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (focus-column g-object)
  (focus-cell g-object)
  (start-editing :boolean))

(defun gtk-tree-view-set-cursor-on-cell (tree-view
                                         path
                                         &key focus-column
                                              focus-cell
                                              start-editing)
  (%gtk-tree-view-set-cursor-on-cell tree-view
                                     path
                                     focus-column
                                     focus-cell
                                     start-editing))

(export 'gtk-tree-view-set-cursor-on-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_cursor ()
;;;
;;; void gtk_tree_view_get_cursor (GtkTreeView *tree_view,
;;;                                GtkTreePath **path,
;;;                                GtkTreeViewColumn **focus_column);
;;;
;;; Fills in path and focus_column with the current path and focus column. If
;;; the cursor isn't currently set, then *path will be NULL. If no column
;;; currently has focus, then *focus_column will be NULL.
;;;
;;; The returned GtkTreePath must be freed with gtk_tree_path_free() when you
;;; are done with it.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; path :
;;;     A pointer to be filled with the current cursor path, or NULL.
;;;
;;; focus_column :
;;;     A pointer to be filled with the current focus column, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_cursor" %gtk-tree-view-get-cursor) :void
  (tree-view g-object)
  (path :pointer)
  (focus-column :pointer))

(defun gtk-tree-view-get-cursor (tree-view)
  (with-foreign-objects ((path :pointer) (focus-column :pointer))
    (%gtk-tree-view-get-cursor tree-view path focus-column)
    (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
            (mem-ref focus-column 'g-object))))

(export 'gtk-tree-view-get-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_row_activated ()
;;;
;;; void gtk_tree_view_row_activated (GtkTreeView *tree_view,
;;;                                   GtkTreePath *path,
;;;                                   GtkTreeViewColumn *column);
;;;
;;; Activates the cell determined by path and column.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; path :
;;;     The GtkTreePath to be activated.
;;;
;;; column :
;;;     The GtkTreeViewColumn to be activated.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_row_activated" gtk-tree-view-row-activated) :void
  (tree-view (g-object gtk-tree-view))
  (path (g-boxed-foreign gtk-tree-path))
  (column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-row-activated)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_all ()
;;;
;;; void gtk_tree_view_expand_all (GtkTreeView *tree_view);
;;;
;;; Recursively expands all nodes in the tree_view.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_expand_all" gtk-tree-view-expand-all) :void
  (tree-view g-object))

(export 'gtk-tree-view-expand-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_collapse_all ()
;;;
;;; void gtk_tree_view_collapse_all (GtkTreeView *tree_view);
;;;
;;; Recursively collapses all visible, expanded nodes in tree_view.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_collapse_all" gtk-tree-view-collapse-all) :void
  (tree-view g-object))

(export 'gtk-tree-view-collapse-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_to_path ()
;;;
;;; void gtk_tree_view_expand_to_path (GtkTreeView *tree_view,
;;;                                    GtkTreePath *path);
;;;
;;; Expands the row at path. This will also expand all parent rows of path as
;;; necessary.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; path :
;;;     path to a row.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_expand_to_path" gtk-tree-view-expand-to-path) :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-expand-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_expand_row ()
;;;
;;; gboolean gtk_tree_view_expand_row (GtkTreeView *tree_view,
;;;                                    GtkTreePath *path,
;;;                                    gboolean open_all);
;;;
;;; Opens the row so its children are visible.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; path :
;;;     path to a row
;;;
;;; open_all :
;;;     whether to recursively expand, or just expand immediate children
;;;
;;; Returns :
;;;     TRUE if the row existed and had children
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_expand_row" gtk-tree-view-expand-row) :boolean
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (open-all :boolean))

(export 'gtk-tree-view-expand-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_collapse_row ()
;;;
;;; gboolean gtk_tree_view_collapse_row (GtkTreeView *tree_view,
;;;                                      GtkTreePath *path);
;;;
;;; Collapses a row (hides its child rows, if they exist).
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; path :
;;;     path to a row in the tree_view
;;;
;;; Returns :
;;;     TRUE if the row was collapsed.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_collapse_row" gtk-tree-view-collapse-row) :boolean
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-collapse-row)

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewMappingFunc ()
;;;
;;; void (*GtkTreeViewMappingFunc) (GtkTreeView *tree_view,
;;;                                 GtkTreePath *path,
;;;                                 gpointer user_data);
;;;
;;; Function used for gtk_tree_view_map_expanded_rows().
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; path :
;;;     The path that's expanded
;;;
;;; user_data :
;;;     user data
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-mapping-func-cb :void
    ((tree-view g-object)
     (path (g-boxed-foreign gtk-tree-path))
     (data :pointer))
  (funcall (get-stable-pointer-value data) tree-view path))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_map_expanded_rows ()
;;;
;;; void gtk_tree_view_map_expanded_rows (GtkTreeView *tree_view,
;;;                                       GtkTreeViewMappingFunc func,
;;;                                       gpointer data);
;;;
;;; Calls func on all expanded rows.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; func :
;;;     A function to be called.
;;;
;;; data :
;;;     User data to be passed to the function.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_map_expanded_rows" %gtk-tree-view-map-expanded-rows)
    :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer))

(defun gtk-tree-view-map-expanded-rows (tree-view func)
  (with-stable-pointer (ptr func)
    (%gtk-tree-view-map-expanded-rows tree-view
                                      (callback gtk-tree-view-mapping-func-cb)
                                      ptr)))

(export 'gtk-tree-view-map-expanded-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_row_expanded ()
;;;
;;; gboolean gtk_tree_view_row_expanded (GtkTreeView *tree_view,
;;;                                      GtkTreePath *path);
;;;
;;; Returns TRUE if the node pointed to by path is expanded in tree_view.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; path :
;;;     A GtkTreePath to test expansion state.
;;;
;;; Returns :
;;;     TRUE if path is expanded.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_row_expanded" gtk-tree-view-row-expanded) :boolean
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-row-expanded)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_reorderable ()
;;;
;;; void gtk_tree_view_set_reorderable (GtkTreeView *tree_view,
;;;                                     gboolean reorderable);
;;;
;;; This function is a convenience function to allow you to reorder models that
;;; support the GtkTreeDragSourceIface and the GtkTreeDragDestIface. Both
;;; GtkTreeStore and GtkListStore support these. If reorderable is TRUE, then
;;; the user can reorder the model by dragging and dropping rows. The developer
;;; can listen to these changes by connecting to the model's row_inserted and
;;; row_deleted signals. The reordering is implemented by setting up the tree
;;; view as a drag source and destination. Therefore, drag and drop can not be
;;; used in a reorderable view for any other purpose.
;;;
;;; This function does not give you any degree of control over the order -- any
;;; reordering is allowed. If more control is needed, you should probably handle
;;; drag and drop manually.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; reorderable :
;;;     TRUE, if the tree can be reordered.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-set-reorderable))

(defun gtk-tree-view-set-reorderable (tree-view reorderable)
  (setf (gtk-tree-view-reorderable tree-view) reorderable))

(export 'gtk-tree-view-set-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_reorderable ()
;;;
;;; gboolean gtk_tree_view_get_reorderable (GtkTreeView *tree_view);
;;;
;;; Retrieves whether the user can reorder the tree via drag-and-drop. See
;;; gtk_tree_view_set_reorderable().
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     TRUE if the tree can be reordered.
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-view-get-reorderable))

(defun gtk-tree-view-get-reorderable (tree-view)
  (gtk-tree-view-reorderable tree-view))

(export 'gtk-tree-view-get-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_path_at_pos ()
;;;
;;; gboolean gtk_tree_view_get_path_at_pos (GtkTreeView *tree_view,
;;;                                         gint x,
;;;                                         gint y,
;;;                                         GtkTreePath **path,
;;;                                         GtkTreeViewColumn **column,
;;;                                         gint *cell_x,
;;;                                         gint *cell_y);
;;;
;;; Finds the path at the point (x, y), relative to bin_window coordinates
;;; (please see gtk_tree_view_get_bin_window()). That is, x and y are relative
;;; to an events coordinates. x and y must come from an event on the tree_view
;;; only where event->window == gtk_tree_view_get_bin_window (). It is primarily
;;; for things like popup menus. If path is non-NULL, then it will be filled
;;; with the GtkTreePath at that point. This path should be freed with
;;; gtk_tree_path_free(). If column is non-NULL, then it will be filled with the
;;; column at that point. cell_x and cell_y return the coordinates relative to
;;; the cell background (i.e. the background_area passed to
;;; gtk_cell_renderer_render()). This function is only meaningful if tree_view
;;; is realized. Therefore this function will always return FALSE if tree_view
;;; is not realized or does not have a model.
;;;
;;; For converting widget coordinates (eg. the ones you get from
;;; GtkWidget::query-tooltip), please see
;;; gtk_tree_view_convert_widget_to_bin_window_coords().
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; x :
;;;     The x position to be identified (relative to bin_window).
;;;
;;; y :
;;;     The y position to be identified (relative to bin_window).
;;;
;;; path :
;;;     A pointer to a GtkTreePath pointer to be filled in, or NULL.
;;;
;;; column :
;;;     A pointer to a GtkTreeViewColumn pointer to be filled in, or NULL.
;;;
;;; cell_x :
;;;     A pointer where the X coordinate relative to the cell can be placed, or
;;;     NULL.
;;;
;;; cell_y :
;;;     A pointer where the Y coordinate relative to the cell can be placed, or
;;;     NULL.
;;;
;;; Returns :
;;;     TRUE if a row exists at that coordinate.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_path_at_pos" %gtk-tree-view-get-path-at-pos)
    :boolean
  (tree-view g-object)
  (x :int)
  (y :int)
  (path :pointer)
  (column :pointer)
  (cell-x :pointer)
  (cell-y :pointer))

(defun gtk-tree-view-get-path-at-pos (tree-view x y)
  (with-foreign-objects ((path :pointer)
                         (column :pointer)
                         (cell-x :int)
                         (cell-y :int))
    (when (%gtk-tree-view-get-path-at-pos tree-view
                                          x y
                                          path
                                          column
                                          cell-x cell-y)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref column 'g-object)
              (mem-ref cell-x :int)
              (mem-ref cell-y :int)))))

(export 'gtk-tree-view-get-path-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_is_blank_at_pos ()
;;;
;;; gboolean gtk_tree_view_is_blank_at_pos (GtkTreeView *tree_view,
;;;                                         gint x,
;;;                                         gint y,
;;;                                         GtkTreePath **path,
;;;                                         GtkTreeViewColumn **column,
;;;                                         gint *cell_x,
;;;                                         gint *cell_y);
;;;
;;; Determine whether the point (x, y) in tree_view is blank, that is no cell
;;; content nor an expander arrow is drawn at the location. If so, the location
;;; can be considered as the background. You might wish to take special action
;;; on clicks on the background, such as clearing a current selection, having a
;;; custom context menu or starting rubber banding.
;;;
;;; The x and y coordinate that are provided must be relative to bin_window
;;; coordinates. That is, x and y must come from an event on tree_view where
;;; event->window == gtk_tree_view_get_bin_window ().
;;;
;;; For converting widget coordinates (eg. the ones you get from
;;; GtkWidget::query-tooltip), please see
;;; gtk_tree_view_convert_widget_to_bin_window_coords().
;;;
;;; The path, column, cell_x and cell_y arguments will be filled in likewise as
;;; for gtk_tree_view_get_path_at_pos(). Please see
;;; gtk_tree_view_get_path_at_pos() for more information.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; x :
;;;     The x position to be identified (relative to bin_window)
;;;
;;; y :
;;;     The y position to be identified (relative to bin_window)
;;;
;;; path :
;;;     A pointer to a GtkTreePath pointer to be filled in, or NULL.
;;;
;;; column :
;;;     A pointer to a GtkTreeViewColumn pointer to be filled in, or NULL.
;;;
;;; cell_x :
;;;     A pointer where the X coordinate relative to the cell can be placed, or
;;;     NULL.
;;;
;;; cell_y :
;;;     A pointer where the Y coordinate relative to the cell can be placed, or
;;;     NULL.
;;;
;;; Returns :
;;;     TRUE if the area at the given coordinates is blank, FALSE otherwise.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_is_blank_at_pos" %gtk-tree-view-is-blank-at-pos)
    :boolean
  (tree-view g-object)
  (x :int)
  (y :int)
  (path :pointer)
  (column :pointer)
  (cell-x :pointer)
  (cell-y :pointer))

(defun gtk-tree-view-is-blank-at-pos (tree-view x y)
  (with-foreign-objects ((path :pointer)
                         (column :pointer)
                         (cell-x :int)
                         (cell-y :int))
    (when (%gtk-tree-view-is-blank-at-pos tree-view
                                          x y
                                          path
                                          column
                                          cell-x cell-y)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref column 'g-object)
              (mem-ref cell-x :int)
              (mem-ref cell-y :int)))))

(export 'gtk-tree-view-is-blank-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_cell_area ()
;;;
;;; void gtk_tree_view_get_cell_area (GtkTreeView *tree_view,
;;;                                   GtkTreePath *path,
;;;                                   GtkTreeViewColumn *column,
;;;                                   GdkRectangle *rect);
;;;
;;; Fills the bounding rectangle in bin_window coordinates for the cell at the
;;; row specified by path and the column specified by column. If path is NULL,
;;; or points to a path not currently displayed, the y and height fields of the
;;; rectangle will be filled with 0. If column is NULL, the x and width fields
;;; will be filled with 0. The sum of all cell rects does not cover the entire
;;; tree; there are extra pixels in between rows, for example. The returned
;;; rectangle is equivalent to the cell_area passed to
;;; gtk_cell_renderer_render(). This function is only valid if tree_view is
;;; realized.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; path :
;;;     a GtkTreePath for the row, or NULL to get only horizontal coordinates
;;;
;;; column :
;;;     a GtkTreeViewColumn for the column, or NULL to get only vertical
;;;     coordinates
;;;
;;; rect :
;;;     rectangle to fill with cell rect
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_cell_area" %gtk-tree-view-get-cell-area) :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (column g-object)
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gtk-tree-view-get-cell-area (tree-view path column)
  (let ((rect (make-gdk-rectangle :x 0 :y 0 :width 0 :height 0)))
    (%gtk-tree-view-get-cell-area tree-view path column rect)
    rect))

(export 'gtk-tree-view-get-cell-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_background_area ()
;;;
;;; void gtk_tree_view_get_background_area (GtkTreeView *tree_view,
;;;                                         GtkTreePath *path,
;;;                                         GtkTreeViewColumn *column,
;;;                                         GdkRectangle *rect);
;;;
;;; Fills the bounding rectangle in bin_window coordinates for the cell at the
;;; row specified by path and the column specified by column. If path is NULL,
;;; or points to a node not found in the tree, the y and height fields of the
;;; rectangle will be filled with 0. If column is NULL, the x and width fields
;;; will be filled with 0. The returned rectangle is equivalent to the
;;; background_area passed to gtk_cell_renderer_render(). These background areas
;;; tile to cover the entire bin window. Contrast with the cell_area, returned
;;; by gtk_tree_view_get_cell_area(), which returns only the cell itself,
;;; excluding surrounding borders and the tree expander area.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; path :
;;;     a GtkTreePath for the row, or NULL to get only horizontal coordinates
;;;
;;; column :
;;;     a GtkTreeViewColumn for the column, or NULL to get only vertical
;;;     coordiantes
;;;
;;; rect :
;;;     rectangle to fill with cell background rect
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_background_area"
          %gtk-tree-view-get-background-area) :void
  (tree-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (column g-object)
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gtk-tree-view-get-background-area (tree-view path column)
  (let ((rect (make-gdk-rectangle :x 0 :y 0 :width 0 :height 0)))
    (%gtk-tree-view-get-background-area tree-view path column rect)
    rect))

(export 'gtk-tree-view-get-background-area)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_visible_rect ()
;;;
;;; void gtk_tree_view_get_visible_rect (GtkTreeView *tree_view,
;;;                                      GdkRectangle *visible_rect);
;;;
;;; Fills visible_rect with the currently-visible region of the buffer, in tree
;;; coordinates. Convert to bin_window coordinates with
;;; gtk_tree_view_convert_tree_to_bin_window_coords(). Tree coordinates start at
;;; 0,0 for row 0 of the tree, and cover the entire scrollable area of the tree.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; visible_rect :
;;;     rectangle to fill
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_visible_rect" %gtk-tree-view-get-visible-rect)
    :void
  (tree-view g-object)
  (rectangle (g-boxed-foreign gdk-rectangle)))

(defun gtk-tree-view-get-visible-rect (tree-view)
  (let ((rect (make-gdk-rectangle :x 0 :y 0 :width 0 :height 0)))
    (%gtk-tree-view-get-visible-rect tree-view rect)
    rect))

(export 'gtk-tree-view-get-visible-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_visible_range ()
;;;
;;; gboolean gtk_tree_view_get_visible_range (GtkTreeView *tree_view,
;;;                                           GtkTreePath **start_path,
;;;                                           GtkTreePath **end_path);
;;;
;;; Sets start_path and end_path to be the first and last visible path. Note
;;; that there may be invisible paths in between.
;;;
;;; The paths should be freed with gtk_tree_path_free() after use.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; start_path :
;;;     Return location for start of region, or NULL.
;;;
;;; end_path :
;;;     Return location for end of region, or NULL.
;;;
;;; Returns :
;;;     TRUE, if valid paths were placed in start_path and end_path.
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_visible_range" %gtk-tree-view-get-visible-range)
    :boolean
  (tree-view g-object)
  (start-path :pointer)
  (end-path :pointer))

(defun gtk-tree-view-get-visible-range (tree-view)
  (with-foreign-objects ((start-path :pointer) (end-path :pointer))
    (when (%gtk-tree-view-get-visible-range tree-view start-path end-path)
      (values (mem-ref start-path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref end-path '(g-boxed-foreign gtk-tree-path :return))))))

(export 'gtk-tree-view-get-visible-range)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_bin_window ()
;;;
;;; GdkWindow * gtk_tree_view_get_bin_window (GtkTreeView *tree_view);
;;;
;;; Returns the window that tree_view renders to. This is used primarily to
;;; compare to event->window to confirm that the event on tree_view is on the
;;; right window.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     A GdkWindow, or NULL when tree_view hasn't been realized yet.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_bin_window_to_tree_coords ()
;;;
;;; void gtk_tree_view_convert_bin_window_to_tree_coords
;;;                                                     (GtkTreeView *tree_view,
;;;                                                      gint bx,
;;;                                                      gint by,
;;;                                                      gint *tx,
;;;                                                      gint *ty);
;;;
;;; Converts bin_window coordinates to coordinates for the tree (the full
;;; scrollable area of the tree).
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; bx :
;;;     X coordinate relative to bin_window
;;;
;;; by :
;;;     Y coordinate relative to bin_window
;;;
;;; tx :
;;;     return location for tree X coordinate
;;;
;;; ty :
;;;     return location for tree Y coordinate
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_bin_window_to_tree_coords"
          %gtk-tree-view-convert-bin-window-to-tree-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-bin-window-to-tree-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-bin-window-to-tree-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-bin-window-to-tree-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_bin_window_to_widget_coords ()
;;;
;;; void gtk_tree_view_convert_bin_window_to_widget_coords
;;;                                                     (GtkTreeView *tree_view,
;;;                                                      gint bx,
;;;                                                      gint by,
;;;                                                      gint *wx,
;;;                                                      gint *wy);
;;;
;;; Converts bin_window coordinates (see gtk_tree_view_get_bin_window()) to
;;; widget relative coordinates.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; bx :
;;;     bin_window X coordinate
;;;
;;; by :
;;;     bin_window Y coordinate
;;;
;;; wx :
;;;     return location for widget X coordinate
;;;
;;; wy :
;;;     return location for widget Y coordinate
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_bin_window_to_widget_coords"
          %gtk-tree-view-convert-bin-window-to-widget-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-bin-window-to-widget-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-bin-window-to-widget-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-bin-window-to-widget-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_tree_to_bin_window_coords ()
;;;
;;; void gtk_tree_view_convert_tree_to_bin_window_coords
;;;                                                     (GtkTreeView *tree_view,
;;;                                                      gint tx,
;;;                                                      gint ty,
;;;                                                      gint *bx,
;;;                                                      gint *by);
;;;
;;; Converts tree coordinates (coordinates in full scrollable area of the tree)
;;; to bin_window coordinates.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; tx :
;;;     tree X coordinate
;;;
;;; ty :
;;;     tree Y coordinate
;;;
;;; bx :
;;;     return location for X coordinate relative to bin_window
;;;
;;; by :
;;;     return location for Y coordinate relative to bin_window
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_tree_to_bin_window_coords"
          %gtk-tree-view-convert-tree-to-bin-window-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-tree-to-bin-window-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-tree-to-bin-window-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-tree-to-bin-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_tree_to_widget_coords ()
;;;
;;; void gtk_tree_view_convert_tree_to_widget_coords (GtkTreeView *tree_view,
;;;                                                   gint tx,
;;;                                                   gint ty,
;;;                                                   gint *wx,
;;;                                                   gint *wy);
;;;
;;; Converts tree coordinates (coordinates in full scrollable area of the tree)
;;; to widget coordinates.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; tx :
;;;     X coordinate relative to the tree
;;;
;;; ty :
;;;     Y coordinate relative to the tree
;;;
;;; wx :
;;;     return location for widget X coordinate
;;;
;;; wy :
;;;     return location for widget Y coordinate
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_tree_to_widget_coords"
          %gtk-tree-view-convert-tree-to-widget-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-tree-to-widget-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-tree-to-widget-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-tree-to-widget-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_widget_to_bin_window_coords ()
;;;
;;; void gtk_tree_view_convert_widget_to_bin_window_coords
;;;                                                     (GtkTreeView *tree_view,
;;;                                                      gint wx,
;;;                                                      gint wy,
;;;                                                      gint *bx,
;;;                                                      gint *by);
;;;
;;; Converts widget coordinates to coordinates for the bin_window (see
;;; gtk_tree_view_get_bin_window()).
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; wx :
;;;     X coordinate relative to the widget
;;;
;;; wy :
;;;     Y coordinate relative to the widget
;;;
;;; bx :
;;;     return location for bin_window X coordinate
;;;
;;; by :
;;;     return location for bin_window Y coordinate
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_widget_to_bin_window_coords"
          %gtk-tree-view-convert-widget-to-bin-window-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-widget-to-bin-window-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-widget-to-bin-window-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-widget-to-bin-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_convert_widget_to_tree_coords ()
;;;
;;; void gtk_tree_view_convert_widget_to_tree_coords (GtkTreeView *tree_view,
;;;                                                   gint wx,
;;;                                                   gint wy,
;;;                                                   gint *tx,
;;;                                                   gint *ty);
;;;
;;; Converts widget coordinates to coordinates for the tree (the full scrollable
;;; area of the tree).
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; wx :
;;;     X coordinate relative to the widget
;;;
;;; wy :
;;;     Y coordinate relative to the widget
;;;
;;; tx :
;;;     return location for tree X coordinate
;;;
;;; ty :
;;;     return location for tree Y coordinate
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_convert_widget_to_tree_coords"
          %gtk-tree-view-convert-widget-to-tree-coords) :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-tree-view-convert-widget-to-tree-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-tree-view-convert-widget-to-tree-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-tree-view-convert-widget-to-tree-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_enable_model_drag_dest ()
;;;
;;; void gtk_tree_view_enable_model_drag_dest (GtkTreeView *tree_view,
;;;                                            const GtkTargetEntry *targets,
;;;                                            gint n_targets,
;;;                                            GdkDragAction actions);
;;;
;;; Turns tree_view into a drop destination for automatic DND. Calling this
;;; method sets "reorderable" to FALSE.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; targets :
;;;     the table of targets that the drag will support
;;;
;;; n_targets :
;;;     the number of items in targets
;;;
;;; actions :
;;;     the bitmask of possible actions for a drag from this widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_enable_model_drag_dest"
          %gtk-tree-view-enable-model-drag-dest) :void
  (tree-view (g-object gtk-tree-view))
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-tree-view-enable-model-drag-dest (tree-view targets actions)
  (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
    (%gtk-tree-view-enable-model-drag-dest tree-view
                                           targets-ptr
                                           n-targets
                                           actions)))

(export 'gtk-tree-view-enable-model-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_enable_model_drag_source ()
;;;
;;; void gtk_tree_view_enable_model_drag_source
;;;                                          (GtkTreeView *tree_view,
;;;                                           GdkModifierType start_button_mask,
;;;                                           const GtkTargetEntry *targets,
;;;                                           gint n_targets,
;;;                                           GdkDragAction actions);
;;;
;;; Turns tree_view into a drag source for automatic DND. Calling this method
;;; sets "reorderable" to FALSE.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; start_button_mask :
;;;     Mask of allowed buttons to start drag
;;;
;;; targets :
;;;     the table of targets that the drag will support
;;;
;;; n_targets :
;;;     the number of items in targets
;;;
;;; actions :
;;;     the bitmask of possible actions for a drag from this widget
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_enable_model_drag_source"
          %gtk-tree-view-enable-model-drag-source) :void
  (tree-view (g-object gtk-tree-view))
  (start-button-mask gdk-modifier-type)
  (targets :pointer)
  (n-targets :int)
  (actions gdk-drag-action))

(defun gtk-tree-view-enable-model-drag-source (tree-view start-button-mask
                                                         targets actions)
  (with-foreign-boxed-array (n-targets targets-ptr gtk-target-entry targets)
    (%gtk-tree-view-enable-model-drag-source tree-view
                                             start-button-mask
                                             targets-ptr
                                             n-targets
                                             actions)))

(export 'gtk-tree-view-enable-model-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_unset_rows_drag_source ()
;;;
;;; void gtk_tree_view_unset_rows_drag_source (GtkTreeView *tree_view);
;;;
;;; Undoes the effect of gtk_tree_view_enable_model_drag_source(). Calling this
;;; method sets "reorderable" to FALSE.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_unset_rows_drag_dest ()
;;;
;;; void gtk_tree_view_unset_rows_drag_dest (GtkTreeView *tree_view);
;;;
;;; Undoes the effect of gtk_tree_view_enable_model_drag_dest(). Calling this
;;; method sets "reorderable" to FALSE.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_drag_dest_row ()
;;;
;;; void gtk_tree_view_set_drag_dest_row (GtkTreeView *tree_view,
;;;                                       GtkTreePath *path,
;;;                                       GtkTreeViewDropPosition pos);
;;;
;;; Sets the row that is highlighted for feedback. If path is NULL, an existing
;;; highlight is removed.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; path :
;;;     The path of the row to highlight, or NULL.
;;;
;;; pos :
;;;     Specifies whether to drop before, after or into the row
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_drag_dest_row ()
;;;
;;; void gtk_tree_view_get_drag_dest_row (GtkTreeView *tree_view,
;;;                                       GtkTreePath **path,
;;;                                       GtkTreeViewDropPosition *pos);
;;;
;;; Gets information about the row that is highlighted for feedback.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; path :
;;;     Return location for the path of the highlighted row, or NULL.
;;;
;;; pos :
;;;     Return location for the drop position, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_dest_row_at_pos ()
;;;
;;; gboolean gtk_tree_view_get_dest_row_at_pos (GtkTreeView *tree_view,
;;;                                             gint drag_x,
;;;                                             gint drag_y,
;;;                                             GtkTreePath **path,
;;;                                             GtkTreeViewDropPosition *pos);
;;;
;;; Determines the destination row for a given position. drag_x and drag_y are
;;; expected to be in widget coordinates. This function is only meaningful if
;;; tree_view is realized. Therefore this function will always return FALSE if
;;; tree_view is not realized or does not have a model.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; drag_x :
;;;     the position to determine the destination row for
;;;
;;; drag_y :
;;;     the position to determine the destination row for
;;;
;;; path :
;;;     Return location for the path of the highlighted row, or NULL.
;;;
;;; pos :
;;;     Return location for the drop position, or NULL.
;;;
;;; Returns :
;;;     whether there is a row at the given position, TRUE if this is indeed the
;;;     case.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_dest_row_at_pos"
          %gtk-tree-view-get-dest-row-at-pos) :boolean
  (tree_view (g-object tree-view))
  (drag-x :int)
  (drag-y :int)
  (path :pointer)
  (pos :pointer))

(defun gtk-tree-view-get-dest-row-at-pos (tree-view x y)
  (with-foreign-objects ((path :pointer) (pos :int))
    (when (%gtk-tree-view-get-dest-row-at-pos tree-view x y path pos)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref pos 'gtk-tree-view-drop-position)))))

(export 'gtk-tree-view-get-dest-row-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_create_row_drag_icon ()
;;;
;;; cairo_surface_t * gtk_tree_view_create_row_drag_icon
;;;                                                     (GtkTreeView *tree_view,
;;;                                                      GtkTreePath *path);
;;;
;;; Creates a cairo_surface_t representation of the row at path. This image is
;;; used for a drag icon.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; path :
;;;     a GtkTreePath in tree_view
;;;
;;; Returns :
;;;     a newly-allocated surface of the drag icon
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_enable_search ()
;;;
;;; void gtk_tree_view_set_enable_search (GtkTreeView *tree_view,
;;;                                       gboolean enable_search);
;;;
;;; If enable_search is set, then the user can type in text to search through
;;; the tree interactively (this is sometimes called "typeahead find").
;;;
;;; Note that even if this is FALSE, the user can still initiate a search using
;;; the "start-interactive-search" key binding.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; enable_search :
;;;     TRUE, if the user can search interactively
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_enable_search ()
;;;
;;; gboolean gtk_tree_view_get_enable_search (GtkTreeView *tree_view);
;;;
;;; Returns whether or not the tree allows to start interactive searching by
;;; typing in text.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     whether or not to let the user search interactively
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_column ()
;;;
;;; gint gtk_tree_view_get_search_column (GtkTreeView *tree_view);
;;;
;;; Gets the column searched on by the interactive search code.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     the column the interactive search code searches in.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_search_column ()
;;;
;;; void gtk_tree_view_set_search_column (GtkTreeView *tree_view, gint column);
;;;
;;; Sets column as the column where the interactive search code should search in
;;; for the current model.
;;;
;;; If the search column is set, users can use the "start-interactive-search"
;;; key binding to bring up search popup. The enable-search property controls
;;; whether simply typing text will also start an interactive search.
;;;
;;; Note that column refers to a column of the current model. The search column
;;; is reset to -1 when the model is changed.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; column :
;;;     the column of the model to search in, or -1 to disable searching
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_equal_func ()
;;;
;;; GtkTreeViewSearchEqualFunc gtk_tree_view_get_search_equal_func
;;;                                                    (GtkTreeView *tree_view);
;;;
;;; Returns the compare function currently in use.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     the currently used compare function for the search code.
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-search-equal-func-cb :boolean
  ((model g-object)
   (column :int)
   (key (:string :free-from-foreign nil))
   (iter (g-boxed-foreign gtk-tree-iter)) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               model column key iter)
    (return-true () t)
   (return-false () t)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_search_equal_func ()
;;;
;;; void gtk_tree_view_set_search_equal_func
;;;                               (GtkTreeView *tree_view,
;;;                                GtkTreeViewSearchEqualFunc search_equal_func,
;;;                                gpointer search_user_data,
;;;                                GDestroyNotify search_destroy);
;;;
;;; Sets the compare function for the interactive search capabilities; note that
;;; somewhat like strcmp() returning 0 for equality GtkTreeViewSearchEqualFunc
;;; returns FALSE on matches.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; search_equal_func :
;;;     the compare function to use during the search
;;;
;;; search_user_data :
;;;     user data to pass to search_equal_func, or NULL
;;;
;;; search_destroy :
;;;     Destroy notifier for search_user_data, or NULL
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_search_equal_func"
          %gtk-tree-view-set-search-equal-func) :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-search-equal-func (tree-view func)
  (%gtk-tree-view-set-search-equal-func
                                   tree-view
                                   (callback gtk-tree-view-search-equal-func-cb)
                                   (allocate-stable-pointer func)
                                   (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-tree-view-set-search-equal-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_entry ()
;;;
;;; GtkEntry * gtk_tree_view_get_search_entry (GtkTreeView *tree_view);
;;;
;;; Returns the GtkEntry which is currently in use as interactive search entry
;;; for tree_view. In case the built-in entry is being used, NULL will be
;;; returned.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     the entry currently in use as search entry
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_search_entry ()
;;;
;;; void gtk_tree_view_set_search_entry (GtkTreeView *tree_view,
;;;                                      GtkEntry *entry);
;;;
;;; Sets the entry which the interactive search code will use for this
;;; tree_view. This is useful when you want to provide a search entry in our
;;; interface at all time at a fixed position. Passing NULL for entry will make
;;; the interactive search code use the built-in popup entry again.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; entry :
;;;     the entry the interactive search code of tree_view should use or NULL
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewSearchPositionFunc ()
;;;
;;; void (*GtkTreeViewSearchPositionFunc) (GtkTreeView *tree_view,
;;;                                        GtkWidget *search_dialog,
;;;                                        gpointer user_data);
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-search-position-func :void
    ((tree-view g-object) (search-dialog g-object) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               tree-view
               search-dialog)
   (return () nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_search_position_func ()
;;;
;;; GtkTreeViewSearchPositionFunc gtk_tree_view_get_search_position_func
;;;                                                    (GtkTreeView *tree_view);
;;;
;;; Returns the positioning function currently in use.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; Returns :
;;;     the currently used function for positioning the search dialog.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_search_position_func ()
;;;
;;; void gtk_tree_view_set_search_position_func
;;;                                         (GtkTreeView *tree_view,
;;;                                          GtkTreeViewSearchPositionFunc func,
;;;                                          gpointer data,
;;;                                          GDestroyNotify destroy);
;;;
;;; Sets the function to use when positioning the search dialog.
;;;
;;; tree_view :
;;;     A GtkTreeView
;;;
;;; func :
;;;     the function to use to position the search dialog, or NULL to use the
;;;     default search position function
;;;
;;; data :
;;;     user data to pass to func, or NULL
;;;
;;; destroy :
;;;     Destroy notifier for data, or NULL
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_search_position_func"
          %gtk-tree-view-set-search-position-func) :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-set-search-position-func (tree-view func)
  (if func
      (%gtk-tree-view-set-search-position-func
                               tree-view
                               (callback gtk-tree-view-set-search-position-func)
                               (allocate-stable-pointer func)
                               (callback stable-pointer-destroy-notify-cb))
      (%gtk-tree-view-set-search-position-func tree-view
                                               (null-pointer)
                                               (null-pointer)
                                               (null-pointer))))

(export 'gtk-tree-view-set-search-position-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_fixed_height_mode ()
;;;
;;; gboolean gtk_tree_view_get_fixed_height_mode (GtkTreeView *tree_view);
;;;
;;; Returns whether fixed height mode is turned on for tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     TRUE if tree_view is in fixed height mode
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_fixed_height_mode ()
;;;
;;; void gtk_tree_view_set_fixed_height_mode (GtkTreeView *tree_view,
;;;                                           gboolean enable);
;;;
;;; Enables or disables the fixed height mode of tree_view. Fixed height mode
;;; speeds up GtkTreeView by assuming that all rows have the same height. Only
;;; enable this option if all rows are the same height and all columns are of
;;; type GTK_TREE_VIEW_COLUMN_FIXED.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; enable :
;;;     TRUE to enable fixed height mode
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_hover_selection ()
;;;
;;; gboolean gtk_tree_view_get_hover_selection (GtkTreeView *tree_view);
;;;
;;; Returns whether hover selection mode is turned on for tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     TRUE if tree_view is in hover selection mode
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_hover_selection ()
;;;
;;; void gtk_tree_view_set_hover_selection (GtkTreeView *tree_view,
;;;                                         gboolean hover);
;;;
;;; Enables or disables the hover selection mode of tree_view. Hover selection
;;; makes the selected row follow the pointer. Currently, this works only for
;;; the selection modes GTK_SELECTION_SINGLE and GTK_SELECTION_BROWSE.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; hover :
;;;     TRUE to enable hover selection mode
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_hover_expand ()
;;;
;;; gboolean gtk_tree_view_get_hover_expand (GtkTreeView *tree_view);
;;;
;;; Returns whether hover expansion mode is turned on for tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     TRUE if tree_view is in hover expansion mode
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_hover_expand ()
;;;
;;; void gtk_tree_view_set_hover_expand (GtkTreeView *tree_view,
;;;                                      gboolean expand);
;;;
;;; Enables or disables the hover expansion mode of tree_view. Hover expansion
;;; makes rows expand or collapse if the pointer moves over them.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; expand :
;;;     TRUE to enable hover selection mode
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeDestroyCountFunc ()
;;;
;;; void (*GtkTreeDestroyCountFunc) (GtkTreeView *tree_view,
;;;                                  GtkTreePath *path,
;;;                                  gint children,
;;;                                  gpointer user_data);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_destroy_count_func ()
;;;
;;; void gtk_tree_view_set_destroy_count_func (GtkTreeView *tree_view,
;;;                                            GtkTreeDestroyCountFunc func,
;;;                                            gpointer data,
;;;                                            GDestroyNotify destroy);
;;;
;;; Warning
;;;
;;; gtk_tree_view_set_destroy_count_func has been deprecated since version 3.4
;;; and should not be used in newly-written code. Accessibility does not need
;;; the function anymore.
;;;
;;; This function should almost never be used. It is meant for private use by
;;; ATK for determining the number of visible children that are removed when the
;;; user collapses a row, or a row is deleted.
;;;
;;; tree_view :
;;;     A GtkTreeView.
;;;
;;; func :
;;;     Function to be called when a view row is destroyed, or NULL.
;;;
;;; data :
;;;     User data to be passed to func, or NULL.
;;;
;;; destroy :
;;;     Destroy notifier for data, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeViewRowSeparatorFunc ()
;;;
;;; gboolean (*GtkTreeViewRowSeparatorFunc) (GtkTreeModel *model,
;;;                                          GtkTreeIter *iter,
;;;                                          gpointer data);
;;;
;;; Function type for determining whether the row pointed to by iter should be
;;; rendered as a separator. A common way to implement this is to have a boolean
;;; column in the model, whose values the GtkTreeViewRowSeparatorFunc returns.
;;;
;;; model :
;;;     the GtkTreeModel
;;;
;;; iter :
;;;     a GtkTreeIter pointing at a row in model
;;;
;;; data :
;;;     user data
;;;
;;; Returns :
;;;     TRUE if the row is a separator
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-view-row-separator-func-callback :boolean
    ((tree-model g-object)
     (iter (g-boxed-foreign gtk-tree-iter))
     (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               tree-model
               iter)
    (return-true () t)
   (return-false () nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_row_separator_func ()
;;;
;;; GtkTreeViewRowSeparatorFunc gtk_tree_view_get_row_separator_func
;;;                                                    (GtkTreeView *tree_view);
;;;
;;; Returns the current row separator function.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     the current row separator function.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_row_separator_func ()
;;;
;;; void gtk_tree_view_set_row_separator_func (GtkTreeView *tree_view,
;;;                                            GtkTreeViewRowSeparatorFunc func,
;;;                                            gpointer data,
;;;                                            GDestroyNotify destroy);
;;;
;;; Sets the row separator function, which is used to determine whether a row
;;; should be drawn as a separator. If the row separator function is NULL, no
;;; separators are drawn. This is the default value.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; func :
;;;     a GtkTreeViewRowSeparatorFunc
;;;
;;; data :
;;;     user data to pass to func, or NULL
;;;
;;; destroy :
;;;     destroy notifier for data, or NULL
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_row_separator_func"
          %gtk-tree-view-set-row-separator-func) :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-view-set-row-separator-func (tree-view func)
  (if func
      (%gtk-tree-view-set-row-separator-func tree-view
                            (callback gtk-tree-view-row-separator-func-callback)
                            (allocate-stable-pointer func)
                            (callback stable-pointer-destroy-notify-cb))
      (%gtk-tree-view-set-row-separator-func tree-view
                                             (null-pointer)
                                             (null-pointer)
                                             (null-pointer))))

(export 'gtk-tree-view-set-row-separator-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_rubber_banding ()
;;;
;;; gboolean gtk_tree_view_get_rubber_banding (GtkTreeView *tree_view);
;;;
;;; Returns whether rubber banding is turned on for tree_view. If the selection
;;; mode is GTK_SELECTION_MULTIPLE, rubber banding will allow the user to select
;;; multiple rows by dragging the mouse.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     TRUE if rubber banding in tree_view is enabled.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_rubber_banding ()
;;;
;;; void gtk_tree_view_set_rubber_banding (GtkTreeView *tree_view,
;;;                                        gboolean enable);
;;;
;;; Enables or disables rubber banding in tree_view. If the selection mode is
;;; GTK_SELECTION_MULTIPLE, rubber banding will allow the user to select
;;; multiple rows by dragging the mouse.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; enable :
;;;     TRUE to enable rubber banding
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_is_rubber_banding_active ()
;;;
;;; gboolean gtk_tree_view_is_rubber_banding_active (GtkTreeView *tree_view);
;;;
;;; Returns whether a rubber banding operation is currently being done in
;;; tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     TRUE if a rubber banding operation is currently being done in tree_view.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_is_rubber_banding_active"
           gtk-tree-view-is-rubber-banding-active) :boolean
  (tree-view g-object))

(export 'gtk-tree-view-is-rubber-banding-active)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_enable_tree_lines ()
;;;
;;; gboolean gtk_tree_view_get_enable_tree_lines (GtkTreeView *tree_view);
;;;
;;; Returns whether or not tree lines are drawn in tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView.
;;;
;;; Returns :
;;;     TRUE if tree lines are drawn in tree_view, FALSE otherwise.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_enable_tree_lines ()
;;;
;;; void gtk_tree_view_set_enable_tree_lines (GtkTreeView *tree_view,
;;;                                           gboolean enabled);
;;;
;;; Sets whether to draw lines interconnecting the expanders in tree_view. This
;;; does not have any visible effects for lists.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; enabled :
;;;     TRUE to enable tree line drawing, FALSE otherwise.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkTreeViewGridLines
;;;
;;; typedef enum {
;;;   GTK_TREE_VIEW_GRID_LINES_NONE,
;;;   GTK_TREE_VIEW_GRID_LINES_HORIZONTAL,
;;;   GTK_TREE_VIEW_GRID_LINES_VERTICAL,
;;;   GTK_TREE_VIEW_GRID_LINES_BOTH
;;; } GtkTreeViewGridLines;
;;;
;;; Used to indicate which grid lines to draw in a tree view.
;;;
;;; GTK_TREE_VIEW_GRID_LINES_NONE
;;;     No grid lines.
;;;
;;; GTK_TREE_VIEW_GRID_LINES_HORIZONTAL
;;;     Horizontal grid lines.
;;;
;;; GTK_TREE_VIEW_GRID_LINES_VERTICAL
;;;     Vertical grid lines.
;;;
;;; GTK_TREE_VIEW_GRID_LINES_BOTH
;;;     Horizontal and vertical grid lines.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTreeViewGridLines" gtk-tree-view-grid-lines
  (:export t
   :type-initializer "gtk_tree_view_grid_lines_get_type")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_grid_lines ()
;;;
;;; GtkTreeViewGridLines gtk_tree_view_get_grid_lines (GtkTreeView *tree_view);
;;;
;;; Returns which grid lines are enabled in tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     a GtkTreeViewGridLines value indicating which grid lines are enabled.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_grid_lines ()
;;;
;;; void gtk_tree_view_set_grid_lines (GtkTreeView *tree_view,
;;;                                    GtkTreeViewGridLines grid_lines);
;;;
;;; Sets which grid lines to draw in tree_view.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; grid_lines :
;;;     a GtkTreeViewGridLines value indicating which grid lines to enable.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_tooltip_row ()
;;;
;;; void gtk_tree_view_set_tooltip_row (GtkTreeView *tree_view,
;;;                                     GtkTooltip *tooltip,
;;;                                     GtkTreePath *path);
;;;
;;; Sets the tip area of tooltip to be the area covered by the row at path. See
;;; also gtk_tree_view_set_tooltip_column() for a simpler alternative. See also
;;; gtk_tooltip_set_tip_area().
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; tooltip :
;;;     a GtkTooltip
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_tooltip_row" gtk-tree-view-set-tooltip-row) :void
  (tree-view g-object)
  (tooltip g-object)
  (tree-path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-view-set-tooltip-row)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_tooltip_cell ()
;;;
;;; void gtk_tree_view_set_tooltip_cell (GtkTreeView *tree_view,
;;;                                      GtkTooltip *tooltip,
;;;                                      GtkTreePath *path,
;;;                                      GtkTreeViewColumn *column,
;;;                                      GtkCellRenderer *cell);
;;;
;;; Sets the tip area of tooltip to the area path, column and cell have in
;;; common. For example if path is NULL and column is set, the tip area will be
;;; set to the full area covered by column. See also gtk_tooltip_set_tip_area().
;;;
;;; Note that if path is not specified and cell is set and part of a column
;;; containing the expander, the tooltip might not show and hide at the correct
;;; position. In such cases path must be set to the current node under the mouse
;;; cursor for this function to operate correctly.
;;;
;;; See also gtk_tree_view_set_tooltip_column() for a simpler alternative.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; tooltip :
;;;     a GtkTooltip
;;;
;;; path :
;;;     a GtkTreePath or NULL
;;;
;;; column :
;;;     a GtkTreeViewColumn or NULL
;;;
;;; cell :
;;;     a GtkCellRenderer or NULL
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_set_tooltip_cell" gtk-tree-view-set-tooltip-cell) :void
  (tree-view g-object)
  (tooltip g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (column g-object)
  (cell g-object))

(export 'gtk-tree-view-set-tooltip-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_tooltip_context ()
;;;
;;; gboolean gtk_tree_view_get_tooltip_context (GtkTreeView *tree_view,
;;;                                             gint *x,
;;;                                             gint *y,
;;;                                             gboolean keyboard_tip,
;;;                                             GtkTreeModel **model,
;;;                                             GtkTreePath **path,
;;;                                             GtkTreeIter *iter);
;;;
;;; This function is supposed to be used in a "query-tooltip" signal handler for
;;; GtkTreeView. The x, y and keyboard_tip values which are received in the
;;; signal handler, should be passed to this function without modification.
;;;
;;; The return value indicates whether there is a tree view row at the given
;;; coordinates (TRUE) or not (FALSE) for mouse tooltips. For keyboard tooltips
;;; the row returned will be the cursor row. When TRUE, then any of model, path
;;; and iter which have been provided will be set to point to that row and the
;;; corresponding model. x and y will always be converted to be relative to
;;; tree_view's bin_window if keyboard_tooltip is FALSE.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; x :
;;;     the x coordinate (relative to widget coordinates)
;;;
;;; y :
;;;     the y coordinate (relative to widget coordinates)
;;;
;;; keyboard_tip :
;;;     whether this is a keyboard tooltip or not
;;;
;;; model :
;;;     a pointer to receive a GtkTreeModel or NULL
;;;
;;; path :
;;;     a pointer to receive a GtkTreePath or NULL
;;;
;;; iter :
;;;     a pointer to receive a GtkTreeIter or NULL
;;;
;;; Returns :
;;;     whether or not the given tooltip context points to a row.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_get_tooltip_context"
          %gtk-tree-view-get-tooltip-context) :boolean
  (tree-view g-object)
  (x :int)
  (y :int)
  (keyboard-tip :boolean)
  (model :pointer)
  (path :pointer)
  (iter :pointer))

(defun gtk-tree-view-get-tooltip-context (tree-view)
  (with-foreign-objects ((x :int)
                         (y :int)
                         (keyboard-tip :boolean)
                         (model :pointer)
                         (path :pointer)
                         (iter :pointer))
    (when (%gtk-tree-view-get-tooltip-context tree-view
                                              x
                                              y
                                              keyboard-tip
                                              model
                                              path
                                              iter)
      (values (mem-ref x :int)
              (mem-ref y :int)
              (mem-ref keyboard-tip :boolean)
              (mem-ref model 'g-object)
              (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref iter '(g-boxed-foreign gtk-tree-iter :return))))))

(export 'gtk-tree-view-get-tooltip-context)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_get_tooltip_column ()
;;;
;;; gint gtk_tree_view_get_tooltip_column (GtkTreeView *tree_view);
;;;
;;; Returns the column of tree_view's model which is being used for displaying
;;; tooltips on tree_view's rows.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; Returns :
;;;     the index of the tooltip column that is currently being used, or -1 if
;;;     this is disabled.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_set_tooltip_column ()
;;;
;;; void gtk_tree_view_set_tooltip_column (GtkTreeView *tree_view, gint column);
;;;
;;; If you only plan to have simple (text-only) tooltips on full rows, you can
;;; use this function to have GtkTreeView handle these automatically for you.
;;; column should be set to the column in tree_view's model containing the
;;; tooltip texts, or -1 to disable this feature.
;;;
;;; When enabled, "has-tooltip" will be set to TRUE and tree_view will connect a
;;; "query-tooltip" signal handler.
;;;
;;; Note that the signal handler sets the text with gtk_tooltip_set_markup(), so
;;; &, <, etc have to be escaped in the text.
;;;
;;; tree_view :
;;;     a GtkTreeView
;;;
;;; column :
;;;     an integer, which is a valid column number for tree_view's model
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.tree-view.lisp -----------------------------------------
