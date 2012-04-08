;;; ----------------------------------------------------------------------------
;;; gtk.icon-view.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkIconView
;;; 
;;; A widget which displays a list of icons in a grid
;;; 
;;; Synopsis
;;; 
;;;     GtkIconView
;;;
;;;     gtk_icon_view_new
;;;     gtk_icon_view_new_with_area
;;;     gtk_icon_view_new_with_model
;;;     gtk_icon_view_set_model
;;;     gtk_icon_view_get_model
;;;     gtk_icon_view_set_text_column
;;;     gtk_icon_view_get_text_column
;;;     gtk_icon_view_set_markup_column
;;;     gtk_icon_view_get_markup_column
;;;     gtk_icon_view_set_pixbuf_column
;;;     gtk_icon_view_get_pixbuf_column
;;;     gtk_icon_view_get_path_at_pos
;;;     gtk_icon_view_get_item_at_pos
;;;     gtk_icon_view_convert_widget_to_bin_window_coords
;;;     gtk_icon_view_set_cursor
;;;     gtk_icon_view_get_cursor
;;;     gtk_icon_view_selected_foreach
;;;     gtk_icon_view_set_selection_mode
;;;     gtk_icon_view_get_selection_mode
;;;     gtk_icon_view_set_item_orientation
;;;     gtk_icon_view_get_item_orientation
;;;     gtk_icon_view_set_columns
;;;     gtk_icon_view_get_columns
;;;     gtk_icon_view_set_item_width
;;;     gtk_icon_view_get_item_width
;;;     gtk_icon_view_set_spacing
;;;     gtk_icon_view_get_spacing
;;;     gtk_icon_view_set_row_spacing
;;;     gtk_icon_view_get_row_spacing
;;;     gtk_icon_view_set_column_spacing
;;;     gtk_icon_view_get_column_spacing
;;;     gtk_icon_view_set_margin
;;;     gtk_icon_view_get_margin
;;;     gtk_icon_view_set_item_padding
;;;     gtk_icon_view_get_item_padding
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
;;;     gtk_icon_view_set_tooltip_column
;;;     gtk_icon_view_get_tooltip_column
;;;     gtk_icon_view_get_item_row
;;;     gtk_icon_view_get_item_column
;;;     
;;;     GtkIconViewDropPosition
;;;
;;;     gtk_icon_view_enable_model_drag_source
;;;     gtk_icon_view_enable_model_drag_dest
;;;     gtk_icon_view_unset_model_drag_source
;;;     gtk_icon_view_unset_model_drag_dest
;;;     gtk_icon_view_set_reorderable
;;;     gtk_icon_view_get_reorderable
;;;     gtk_icon_view_set_drag_dest_item
;;;     gtk_icon_view_get_drag_dest_item
;;;     gtk_icon_view_get_dest_item_at_pos
;;;     gtk_icon_view_create_drag_icon
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkIconView
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkIconView implements AtkImplementorIface, GtkBuildable, GtkCellLayout
;;; and GtkScrollable.
;;;
;;; Properties
;;; 
;;;   "cell-area"                GtkCellArea*         : Read / Write / Construct
;;;   "column-spacing"           gint                 : Read / Write
;;;   "columns"                  gint                 : Read / Write
;;;   "item-orientation"         GtkOrientation       : Read / Write
;;;   "item-padding"             gint                 : Read / Write
;;;   "item-width"               gint                 : Read / Write
;;;   "margin"                   gint                 : Read / Write
;;;   "markup-column"            gint                 : Read / Write
;;;   "model"                    GtkTreeModel*        : Read / Write
;;;   "pixbuf-column"            gint                 : Read / Write
;;;   "reorderable"              gboolean             : Read / Write
;;;   "row-spacing"              gint                 : Read / Write
;;;   "selection-mode"           GtkSelectionMode     : Read / Write
;;;   "spacing"                  gint                 : Read / Write
;;;   "text-column"              gint                 : Read / Write
;;;   "tooltip-column"           gint                 : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "selection-box-alpha"      guchar               : Read
;;;   "selection-box-color"      GdkColor*            : Read
;;; 
;;; Signals
;;; 
;;;   "activate-cursor-item"                          : Action
;;;   "item-activated"                                : Run Last
;;;   "move-cursor"                                   : Action
;;;   "select-all"                                    : Action
;;;   "select-cursor-item"                            : Action
;;;   "selection-changed"                             : Run First
;;;   "toggle-cursor-item"                            : Action
;;;   "unselect-all"                                  : Action
;;; 
;;; Description
;;; 
;;; GtkIconView provides an alternative view on a GtkTreeModel. It displays the 
;;; model as a grid of icons with labels. Like GtkTreeView, it allows to select
;;; one or multiple items (depending on the selection mode, see 
;;; gtk_icon_view_set_selection_mode()). In addition to selection with the arrow
;;; keys, GtkIconView supports rubberband selection, which is controlled by 
;;; dragging the pointer.
;;; 
;;; Note that if the tree model is backed by an actual tree store (as opposed 
;;; to a flat list where the mapping to icons is obvious), GtkIconView will
;;; only display the first level of the tree and ignore the tree's branches.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "cell-area" property
;;; 
;;;   "cell-area"                GtkCellArea*         : Read / Write / Construct
;;; 
;;; The GtkCellArea used to layout cell renderers for this view.
;;; 
;;; If no area is specified when creating the icon view with
;;; gtk_icon_view_new_with_area() a GtkCellAreaBox will be used.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "column-spacing" property
;;; 
;;;   "column-spacing"           gint                  : Read / Write
;;; 
;;; The column-spacing property specifies the space which is inserted between
;;; the columns of the icon view.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 6
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "columns" property
;;; 
;;;   "columns"                  gint                  : Read / Write
;;; 
;;; The columns property contains the number of the columns in which the items
;;; should be displayed. If it is -1, the number of columns will be chosen
;;; automatically to fill the available area.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "item-orientation" property
;;; 
;;;   "item-orientation"         GtkOrientation        : Read / Write
;;; 
;;; The item-orientation property specifies how the cells (i.e. the icon and
;;; the text) of the item are positioned relative to each other.
;;; 
;;; Default value: GTK_ORIENTATION_VERTICAL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "item-padding" property
;;; 
;;;   "item-padding"             gint                  : Read / Write
;;; 
;;; The item-padding property specifies the padding around each of the icon
;;; view's item.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 6
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "item-width" property
;;; 
;;;   "item-width"               gint                  : Read / Write
;;; 
;;; The item-width property specifies the width to use for each item. If it is
;;; set to -1, the icon view will automatically determine a suitable item size.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "margin" property
;;; 
;;;   "margin"                   gint                  : Read / Write
;;; 
;;; The margin property specifies the space which is inserted at the edges of
;;; the icon view.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 6
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "markup-column" property
;;; 
;;;   "markup-column"            gint                  : Read / Write
;;; 
;;; The ::markup-column property contains the number of the model column
;;; containing markup information to be displayed. The markup column must be of
;;; type G_TYPE_STRING. If this property and the :text-column property are both
;;; set to column numbers, it overrides the text column. If both are set to -1,
;;; no texts are displayed.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "model" property
;;; 
;;;   "model"                    GtkTreeModel*         : Read / Write
;;; 
;;; The model for the icon view.
;;;
;;; ----------------------------------------------------------------------------
;;; The "pixbuf-column" property
;;; 
;;;   "pixbuf-column"            gint                  : Read / Write
;;; 
;;; The ::pixbuf-column property contains the number of the model column
;;; containing the pixbufs which are displayed. The pixbuf column must be of
;;; type GDK_TYPE_PIXBUF. Setting this property to -1 turns off the display of
;;; pixbufs.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "reorderable" property
;;; 
;;;   "reorderable"              gboolean              : Read / Write
;;; 
;;; The reorderable property specifies if the items can be reordered by DND.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.8
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-spacing" property
;;; 
;;;   "row-spacing"              gint                  : Read / Write
;;; 
;;; The row-spacing property specifies the space which is inserted between the
;;; rows of the icon view.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 6
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-mode" property
;;; 
;;;   "selection-mode"           GtkSelectionMode      : Read / Write
;;; 
;;; The ::selection-mode property specifies the selection mode of icon view.
;;; If the mode is GTK_SELECTION_MULTIPLE, rubberband selection is enabled, for
;;; the other modes, only keyboard selection is possible.
;;; 
;;; Default value: GTK_SELECTION_SINGLE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "spacing" property
;;; 
;;;   "spacing"                  gint                  : Read / Write
;;; 
;;; The spacing property specifies the space which is inserted between the cells
;;; (i.e. the icon and the text) of an item.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "text-column" property
;;; 
;;;   "text-column"              gint                  : Read / Write
;;; 
;;; The ::text-column property contains the number of the model column
;;; containing the texts which are displayed. The text column must be of type
;;; G_TYPE_STRING. If this property and the :markup-column property are both
;;; set to -1, no texts are displayed.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "tooltip-column" property
;;; 
;;;   "tooltip-column"           gint                  : Read / Write
;;; 
;;; The column in the model containing the tooltip texts for the items.
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
;;; The "selection-box-alpha" style property
;;; 
;;;   "selection-box-alpha"      guchar                : Read
;;; 
;;; Opacity of the selection box.
;;; 
;;; Default value: 64
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-box-color" style property
;;; 
;;;   "selection-box-color"      GdkColor*             : Read
;;; 
;;; Color of the selection box.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate-cursor-item" signal
;;; 
;;; gboolean user_function (GtkIconView *iconview,
;;;                         gpointer     user_data)      : Action
;;; 
;;; A keybinding signal which gets emitted when the user activates the currently
;;; focused item.
;;; 
;;; Applications should not connect to it, but may emit it with
;;; g_signal_emit_by_name() if they need to control activation programmatically.
;;; 
;;; The default bindings for this signal are Space, Return and Enter.
;;; 
;;; iconview :
;;; 	the object on which the signal is emitted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "item-activated" signal
;;; 
;;; void user_function (GtkIconView *iconview,
;;;                     GtkTreePath *path,
;;;                     gpointer     user_data)      : Run Last
;;; 
;;; The ::item-activated signal is emitted when the method
;;; gtk_icon_view_item_activated() is called or the user double clicks an item.
;;; It is also emitted when a non-editable item is selected and one of the
;;; keys: Space, Return or Enter is pressed.
;;; 
;;; iconview :
;;; 	the object on which the signal is emitted
;;; 
;;; path :
;;; 	the GtkTreePath for the activated item
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-cursor" signal
;;; 
;;; gboolean user_function (GtkIconView    *iconview,
;;;                         GtkMovementStep step,
;;;                         gint            count,
;;;                         gpointer        user_data)      : Action
;;; 
;;; The ::move-cursor signal is a keybinding signal which gets emitted when the
;;; user initiates a cursor movement.
;;; 
;;; Applications should not connect to it, but may emit it with
;;; g_signal_emit_by_name() if they need to control the cursor programmatically.
;;; 
;;; The default bindings for this signal include
;;; 
;;;     Arrow keys which move by individual steps
;;;     Home/End keys which move to the first/last item
;;;     PageUp/PageDown which move by "pages"
;;; 
;;; All of these will extend the selection when combined with the Shift
;;; modifier.
;;; 
;;; iconview :
;;; 	the object which received the signal
;;; 
;;; step :
;;; 	the granularity of the move, as a GtkMovementStep
;;; 
;;; count :
;;; 	the number of step units to move
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "select-all" signal
;;; 
;;; void user_function (GtkIconView *iconview,
;;;                     gpointer     user_data)      : Action
;;; 
;;; A keybinding signal which gets emitted when the user selects all items.
;;; 
;;; Applications should not connect to it, but may emit it with
;;; g_signal_emit_by_name() if they need to control selection programmatically.
;;; 
;;; The default binding for this signal is Ctrl-a.
;;; 
;;; iconview :
;;; 	the object on which the signal is emitted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "select-cursor-item" signal
;;; 
;;; void user_function (GtkIconView *iconview,
;;;                     gpointer     user_data)      : Action
;;; 
;;; A keybinding signal which gets emitted when the user selects the item that
;;; is currently focused.
;;; 
;;; Applications should not connect to it, but may emit it with
;;; g_signal_emit_by_name() if they need to control selection programmatically.
;;; 
;;; There is no default binding for this signal.
;;; 
;;; iconview :
;;; 	the object on which the signal is emitted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "selection-changed" signal
;;; 
;;; void user_function (GtkIconView *iconview,
;;;                     gpointer     user_data)      : Run First
;;; 
;;; The ::selection-changed signal is emitted when the selection (i.e. the set
;;; of selected items) changes.
;;; 
;;; iconview :
;;; 	the object on which the signal is emitted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggle-cursor-item" signal
;;; 
;;; void user_function (GtkIconView *iconview,
;;;                     gpointer     user_data)      : Action
;;; 
;;; A keybinding signal which gets emitted when the user toggles whether the 
;;; currently focused item is selected or not. The exact effect of this depend
;;; on the selection mode.
;;; 
;;; Applications should not connect to it, but may emit it with 
;;; g_signal_emit_by_name() if they need to control selection programmatically.
;;; 
;;; There is no default binding for this signal is Ctrl-Space.
;;; 
;;; iconview :
;;; 	the object on which the signal is emitted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "unselect-all" signal
;;; 
;;; void user_function (GtkIconView *iconview,
;;;                     gpointer     user_data)      : Action
;;; 
;;; A keybinding signal which gets emitted when the user unselects all items.
;;; 
;;; Applications should not connect to it, but may emit it with 
;;; g_signal_emit_by_name() if they need to control selection programmatically.
;;; 
;;; The default binding for this signal is Ctrl-Shift-a.
;;; 
;;; iconview :
;;; 	the object on which the signal is emitted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkIconView
;;; 
;;; struct GtkIconView;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkIconView" gtk-icon-view
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkCellLayout")
   :type-initializer "gtk_icon_view_get_type")
  ((column-spacing gtk-icon-view-column-spacing
    "column-spacing" "gint" t t)
   (columns gtk-icon-view-columns
    "columns" "gint" t t)
   (item-width gtk-icon-view-item-width
    "item-width" "gint" t t)
   (margin gtk-icon-view-margin
    "margin" "gint" t t)
   (markup-column gtk-icon-view-markup-column
    "markup-column" "gint" t t)
   (model gtk-icon-view-model
    "model" "GtkTreeModel" t t)
   (orientation gtk-icon-view-orientation
    "orientation" "GtkOrientation" t t)
   (pixbuf-column gtk-icon-view-pixbuf-column
    "pixbuf-column" "gint" t t)
   (reorderable gtk-icon-view-reorderable
    "reorderable" "gboolean" t t)
   (row-spacing gtk-icon-view-row-spacing
    "row-spacing" "gint" t t)
   (selection-mode gtk-icon-view-selection-mode
    "selection-mode" "GtkSelectionMode" t t)
   (spacing gtk-icon-view-spacing
    "spacing" "gint" t t)
   (text-column gtk-icon-view-text-column
    "text-column" "gint" t t)
   (tooltip-column gtk-icon-view-tooltip-column
    "tooltip-column" "gint" t t)))

;;; ----------------------------------------------------------------------------
;;; GtkIconViewForeachFunc ()
;;; 
;;; void (*GtkIconViewForeachFunc) (GtkIconView *icon_view,
;;;                                 GtkTreePath *path,
;;;                                 gpointer data);
;;; 
;;; A function used by gtk_icon_view_selected_foreach() to map all selected 
;;; rows. It will be called on every selected row in the view.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; path :
;;; 	The GtkTreePath of a selected row
;;; 
;;; data :
;;; 	user data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_new ()
;;; 
;;; GtkWidget * gtk_icon_view_new (void);
;;; 
;;; Creates a new GtkIconView widget
;;; 
;;; Returns :
;;; 	A newly created GtkIconView widget
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_new_with_area ()
;;; 
;;; GtkWidget * gtk_icon_view_new_with_area (GtkCellArea *area);
;;; 
;;; Creates a new GtkIconView widget using the specified area to layout cells 
;;; inside the icons.
;;; 
;;; area :
;;; 	the GtkCellArea to use to layout cells
;;; 
;;; Returns :
;;; 	A newly created GtkIconView widget
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_new_with_model ()
;;; 
;;; GtkWidget * gtk_icon_view_new_with_model (GtkTreeModel *model);
;;; 
;;; Creates a new GtkIconView widget with the model model.
;;; 
;;; model :
;;; 	The model.
;;; 
;;; Returns :
;;; 	A newly created GtkIconView widget.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_model ()
;;; 
;;; void gtk_icon_view_set_model (GtkIconView *icon_view, GtkTreeModel *model)
;;; 
;;; Sets the model for a GtkIconView. If the icon_view already has a model set,
;;; it will remove it before setting the new model. If model is NULL, then it
;;; will unset the old model.
;;; 
;;; icon_view :
;;; 	A GtkIconView
;;; 
;;; model :
;;; 	The model
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_model ()
;;; 
;;; GtkTreeModel * gtk_icon_view_get_model (GtkIconView *icon_view);
;;; 
;;; Returns the model the GtkIconView is based on. Returns NULL if the model is 
;;; unset.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	A GtkTreeModel, or NULL if none is currently being used.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_text_column ()
;;; 
;;; void gtk_icon_view_set_text_column (GtkIconView *icon_view, gint column);
;;; 
;;; Sets the column with text for icon_view to be column. The text column must 
;;; be of type G_TYPE_STRING.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; column :
;;; 	A column in the currently used model, or -1 to display no text
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_text_column ()
;;; 
;;; gint gtk_icon_view_get_text_column (GtkIconView *icon_view);
;;; 
;;; Returns the column with text for icon_view.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; Returns :
;;; 	the text column, or -1 if it's unset.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_markup_column ()
;;; 
;;; void gtk_icon_view_set_markup_column (GtkIconView *icon_view, gint column)
;;; 
;;; Sets the column with markup information for icon_view to be column. The 
;;; markup column must be of type G_TYPE_STRING. If the markup column is set to
;;; something, it overrides the text column set by
;;; gtk_icon_view_set_text_column().
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; column :
;;; 	A column in the currently used model, or -1 to display no text
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_markup_column ()
;;; 
;;; gint gtk_icon_view_get_markup_column (GtkIconView *icon_view);
;;; 
;;; Returns the column with markup text for icon_view.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; Returns :
;;; 	the markup column, or -1 if it's unset.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_pixbuf_column ()
;;; 
;;; void gtk_icon_view_set_pixbuf_column (GtkIconView *icon_view, gint column)
;;; 
;;; Sets the column with pixbufs for icon_view to be column. The pixbuf column
;;; must be of type GDK_TYPE_PIXBUF
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; column :
;;; 	A column in the currently used model, or -1 to disable
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_pixbuf_column ()
;;; 
;;; gint gtk_icon_view_get_pixbuf_column (GtkIconView *icon_view);
;;; 
;;; Returns the column with pixbufs for icon_view.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; Returns :
;;; 	the pixbuf column, or -1 if it's unset.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_path_at_pos ()
;;; 
;;; GtkTreePath * gtk_icon_view_get_path_at_pos (GtkIconView *icon_view,
;;;                                              gint x,
;;;                                              gint y);
;;; 
;;; Finds the path at the point (x, y), relative to bin_window coordinates.
;;; See gtk_icon_view_get_item_at_pos(), if you are also interested in the cell
;;; at the specified position.
;;; See gtk_icon_view_convert_widget_to_bin_window_coords() for converting
;;; widget coordinates to bin_window coordinates.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; x :
;;; 	The x position to be identified
;;; 
;;; y :
;;; 	The y position to be identified
;;; 
;;; Returns :
;;; 	The GtkTreePath corresponding to the icon or NULL if no icon exists at
;;;     that position.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_path_at_pos" gtk-icon-view-get-path-at-pos)
    (g-boxed-foreign gtk-tree-path :return)
  (icon-view g-object)
  (x :int)
  (y :int))

(export 'gtk-icon-view-get-path-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_at_pos ()
;;; 
;;; gboolean gtk_icon_view_get_item_at_pos (GtkIconView *icon_view,
;;;                                         gint x,
;;;                                         gint y,
;;;                                         GtkTreePath **path,
;;;                                         GtkCellRenderer **cell);
;;; 
;;; Finds the path at the point (x, y), relative to bin_window coordinates. In
;;; contrast to gtk_icon_view_get_path_at_pos(), this function also obtains the
;;; cell at the specified position. The returned path should be freed with
;;; gtk_tree_path_free().
;;; See gtk_icon_view_convert_widget_to_bin_window_coords() for converting
;;; widget coordinates to bin_window coordinates.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; x :
;;; 	The x position to be identified
;;; 
;;; y :
;;; 	The y position to be identified
;;; 
;;; path :
;;; 	Return location for the path, or NULL
;;; 
;;; cell :
;;; 	Return location for the renderer responsible for the cell at (x, y),
;;;     or NULL
;;; 
;;; Returns :
;;; 	TRUE if an item exists at the specified position
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_item_at_pos" %gtk-icon-view-get-item-at-pos)
    :boolean
  (icon-view g-object)
  (x :int)
  (y :int)
  (path :pointer)
  (cell :pointer))

(defun gtk-icon-view-get-item-at-pos (icon-view x y)
  (with-foreign-objects ((path :pointer) (cell :pointer))
    (when (%gtk-icon-view-get-item-at-pos icon-view x y path cell)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref cell 'g-object)))))

(export 'gtk-icon-view-get-item-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_convert_widget_to_bin_window_coords ()
;;; 
;;; void gtk_icon_view_convert_widget_to_bin_window_coords
;;;                                                     (GtkIconView *icon_view,
;;;                                                      gint wx,
;;;                                                      gint wy,
;;;                                                      gint *bx,
;;;                                                      gint *by);
;;; 
;;; Converts widget coordinates to coordinates for the bin_window, as expected
;;; by e.g. gtk_icon_view_get_path_at_pos().
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; wx :
;;; 	X coordinate relative to the widget
;;; 
;;; wy :
;;; 	Y coordinate relative to the widget
;;; 
;;; bx :
;;; 	return location for bin_window X coordinate. [out]
;;; 
;;; by :
;;; 	return location for bin_window Y coordinate. [out]
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_convert_widget_to_bin_window_coords"
          %gtk-icon-view-convert-widget-to-bin-window-coords) :void
  (icon-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun gtk-icon-view-convert-widget-to-bin-window-coords (icon-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (%gtk-icon-view-convert-widget-to-bin-window-coords icon-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'gtk-icon-view-conver-widget-to-bin-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_cursor ()
;;; 
;;; void gtk_icon_view_set_cursor (GtkIconView *icon_view,
;;;                                GtkTreePath *path,
;;;                                GtkCellRenderer *cell,
;;;                                gboolean start_editing);
;;; 
;;; Sets the current keyboard focus to be at path, and selects it. This is
;;; useful when you want to focus the user's attention on a particular item. If
;;; cell is not NULL, then focus is given to the cell specified by it.
;;; Additionally, if start_editing is TRUE, then editing should be started in
;;; the specified cell.
;;; 
;;; This function is often followed by gtk_widget_grab_focus (icon_view) in
;;; order to give keyboard focus to the widget. Please note that editing can
;;; only happen when the widget is realized.
;;; 
;;; icon_view :
;;; 	A GtkIconView
;;; 
;;; path :
;;; 	A GtkTreePath
;;; 
;;; cell :
;;; 	One of the cell renderers of icon_view, or NULL
;;; 
;;; start_editing :
;;; 	TRUE if the specified cell should start being edited
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_set_cursor" gtk-icon-view-set-cursor) :void
  (icon-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (cell g-object)
  (start-editing :boolean))

(export 'gtk-icon-view-set-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_cursor ()
;;; 
;;; gboolean gtk_icon_view_get_cursor (GtkIconView *icon_view,
;;;                                    GtkTreePath **path,
;;;                                    GtkCellRenderer **cell);
;;; 
;;; Fills in path and cell with the current cursor path and cell. If the cursor
;;; isn't currently set, then *path will be NULL. If no cell currently has
;;; focus, then *cell will be NULL.
;;; 
;;; The returned GtkTreePath must be freed with gtk_tree_path_free().
;;; 
;;; icon_view :
;;; 	A GtkIconView
;;; 
;;; path :
;;; 	Return location for the current cursor path, or NULL.
;;; 
;;; cell :
;;; 	Return location the current focus cell, or NULL.
;;; 
;;; Returns :
;;; 	TRUE if the cursor is set.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_cursor" %gtk-icon-view-get-cursor) :boolean
  (icon-view g-object)
  (path :pointer)
  (cell :pointer))

(defun gtk-icon-view-get-cursor (icon-view)
  (with-foreign-objects ((path :pointer) (cell :pointer))
    (when (%gtk-icon-view-get-cursor icon-view path cell)
      (values (mem-ref path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref cell 'g-object)))))

(export 'gtk-icon-view-get-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_selected_foreach ()
;;; 
;;; void gtk_icon_view_selected_foreach (GtkIconView *icon_view,
;;;                                      GtkIconViewForeachFunc func,
;;;                                      gpointer data);
;;; 
;;; Calls a function for each selected icon. Note that the model or selection
;;; cannot be modified from within this function.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; func :
;;; 	The function to call for each selected icon. [scope call]
;;; 
;;; data :
;;; 	User data to pass to the function.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcallback gtk-icon-view-foreach-func-callback :void
    ((icon-view g-object)
     (path (g-boxed-foreign gtk-tree-path))
     (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               icon-view
               path)
    (return () nil)))

(defcfun ("gtk_icon_view_selected_foreach" %gtk-icon-view-selected-foreach)
    :void
  (icon-view g-object)
  (func :pointer)
  (data :pointer))

(defun map-icon-view-selected (icon-view func)
  (with-stable-pointer (ptr func)
    (%gtk-icon-view-selected-foreach
                                  icon-view
                                  (callback gtk-icon-view-foreach-func-callback)
                                  ptr)))

(export 'map-icon-view-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_selection_mode ()
;;; 
;;; void gtk_icon_view_set_selection_mode (GtkIconView *icon_view,
;;;                                        GtkSelectionMode mode);
;;; 
;;; Sets the selection mode of the icon_view.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; mode :
;;; 	The selection mode
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_selection_mode ()
;;; 
;;; GtkSelectionMode gtk_icon_view_get_selection_mode (GtkIconView *icon_view)
;;; 
;;; Gets the selection mode of the icon_view.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; Returns :
;;; 	the current selection mode
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_item_orientation ()
;;; 
;;; void gtk_icon_view_set_item_orientation (GtkIconView *icon_view,
;;;                                          GtkOrientation orientation);
;;; 
;;; Sets the ::item-orientation property which determines whether the labels
;;; are drawn beside the icons instead of below.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; orientation :
;;; 	the relative position of texts and icons
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_orientation ()
;;; 
;;; GtkOrientation gtk_icon_view_get_item_orientation (GtkIconView *icon_view)
;;; 
;;; Returns the value of the ::item-orientation property which determines
;;; whether the labels are drawn beside the icons instead of below.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	the relative position of texts and icons
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_columns ()
;;; 
;;; void gtk_icon_view_set_columns (GtkIconView *icon_view, gint columns);
;;; 
;;; Sets the ::columns property which determines in how many columns the icons
;;; are arranged. If columns is -1, the number of columns will be chosen
;;; automatically to fill the available area.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; columns :
;;; 	the number of columns
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_columns ()
;;; 
;;; gint gtk_icon_view_get_columns (GtkIconView *icon_view);
;;; 
;;; Returns the value of the ::columns property.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	the number of columns, or -1
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_item_width ()
;;; 
;;; void gtk_icon_view_set_item_width (GtkIconView *icon_view, gint item_width)
;;; 
;;; Sets the ::item-width property which specifies the width to use for each
;;; item. If it is set to -1, the icon view will automatically determine a
;;; suitable item size.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; item_width :
;;; 	the width for each item
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_width ()
;;; 
;;; gint gtk_icon_view_get_item_width (GtkIconView *icon_view);
;;; 
;;; Returns the value of the ::item-width property.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	the width of a single item, or -1
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_spacing ()
;;; 
;;; void gtk_icon_view_set_spacing (GtkIconView *icon_view, gint spacing);
;;; 
;;; Sets the ::spacing property which specifies the space which is inserted
;;; between the cells (i.e. the icon and the text) of an item.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; spacing :
;;; 	the spacing
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_spacing ()
;;; 
;;; gint gtk_icon_view_get_spacing (GtkIconView *icon_view);
;;; 
;;; Returns the value of the ::spacing property.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	the space between cells
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_row_spacing ()
;;; 
;;; void gtk_icon_view_set_row_spacing (GtkIconView *icon_view,
;;;                                     gint row_spacing);
;;; 
;;; Sets the ::row-spacing property which specifies the space which is inserted
;;; between the rows of the icon view.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; row_spacing :
;;; 	the row spacing
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_row_spacing ()
;;; 
;;; gint gtk_icon_view_get_row_spacing (GtkIconView *icon_view);
;;; 
;;; Returns the value of the ::row-spacing property.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	the space between rows
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_column_spacing ()
;;; 
;;; void gtk_icon_view_set_column_spacing (GtkIconView *icon_view,
;;;                                        gint column_spacing);
;;; 
;;; Sets the ::column-spacing property which specifies the space which is
;;; inserted between the columns of the icon view.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; column_spacing :
;;; 	the column spacing
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_column_spacing ()
;;; 
;;; gint gtk_icon_view_get_column_spacing (GtkIconView *icon_view);
;;; 
;;; Returns the value of the ::column-spacing property.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	the space between columns
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_margin ()
;;; 
;;; void gtk_icon_view_set_margin (GtkIconView *icon_view, gint margin);
;;; 
;;; Sets the ::margin property which specifies the space which is inserted at
;;; the top, bottom, left and right of the icon view.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; margin :
;;; 	the margin
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_margin ()
;;; 
;;; gint gtk_icon_view_get_margin (GtkIconView *icon_view);
;;; 
;;; Returns the value of the ::margin property.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	the space at the borders
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_item_padding ()
;;; 
;;; void gtk_icon_view_set_item_padding (GtkIconView *icon_view,
;;;                                      gint item_padding);
;;; 
;;; Sets the "item-padding" property which specifies the padding around each
;;; of the icon view's items.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; item_padding :
;;; 	the item padding
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_padding ()
;;; 
;;; gint gtk_icon_view_get_item_padding (GtkIconView *icon_view);
;;; 
;;; Returns the value of the ::item-padding property.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	the padding around items
;;; 
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_select_path ()
;;; 
;;; void gtk_icon_view_select_path (GtkIconView *icon_view, GtkTreePath *path);
;;; 
;;; Selects the row at path.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; path :
;;; 	The GtkTreePath to be selected.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_select_path" gtk-icon-view-select-path) :void
  (icon-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-select-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unselect_path ()
;;; 
;;; void gtk_icon_view_unselect_path (GtkIconView *icon_view,
;;;                                   GtkTreePath *path);
;;; 
;;; Unselects the row at path.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; path :
;;; 	The GtkTreePath to be unselected.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_unselect_path" gtk-icon-view-unselect-path) :void
  (icon-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-unselect-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_path_is_selected ()
;;; 
;;; gboolean gtk_icon_view_path_is_selected (GtkIconView *icon_view,
;;;                                          GtkTreePath *path);
;;; 
;;; Returns TRUE if the icon pointed to by path is currently selected. If path
;;; does not point to a valid location, FALSE is returned.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; path :
;;; 	A GtkTreePath to check selection on.
;;; 
;;; Returns :
;;; 	TRUE if path is selected.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_path_is_selected" gtk-icon-view-path-is-selected)
    :boolean
  (icon-view g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-path-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_selected_items ()
;;; 
;;; GList * gtk_icon_view_get_selected_items (GtkIconView *icon_view);
;;; 
;;; Creates a list of paths of all selected items. Additionally, if you are
;;; planning on modifying the model after calling this function, you may want
;;; to convert the returned list into a list of GtkTreeRowReferences. To do
;;; this, you can use gtk_tree_row_reference_new().
;;; 
;;; To free the return value, use:
;;; 
;;; g_list_foreach (list, (GFunc)gtk_tree_path_free, NULL);
;;; g_list_free (list);
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; Returns :
;;; 	A GList containing a GtkTreePath for each selected row.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_selected_items" gtk-icon-view-selected-items)
    (g-list (g-boxed-foreign gtk-tree-path) :free-from-foreign t)
  (icon-view g-object))

(export 'gtk-icon-view-selected-items)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_select_all ()
;;; 
;;; void gtk_icon_view_select_all (GtkIconView *icon_view);
;;; 
;;; Selects all the icons. icon_view must has its selection mode set to
;;; GTK_SELECTION_MULTIPLE.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_select_all" gtk-icon-view-select-all) :void
  (icon-view g-object))

(export 'gtk-icon-view-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unselect_all ()
;;; 
;;; void gtk_icon_view_unselect_all (GtkIconView *icon_view);
;;; 
;;; Unselects all the icons.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_unselect_all" gtk-icon-view-unselect-all) :void
  (icon-view g-object))

(export 'gtk-icon-view-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_item_activated ()
;;; 
;;; void gtk_icon_view_item_activated (GtkIconView *icon_view,
;;;                                    GtkTreePath *path);
;;; 
;;; Activates the item determined by path.
;;; 
;;; icon_view :
;;; 	A GtkIconView
;;; 
;;; path :
;;; 	The GtkTreePath to be activated
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_scroll_to_path ()
;;; 
;;; void gtk_icon_view_scroll_to_path (GtkIconView *icon_view,
;;;                                    GtkTreePath *path,
;;;                                    gboolean use_align,
;;;                                    gfloat row_align,
;;;                                    gfloat col_align);
;;; 
;;; Moves the alignments of icon_view to the position specified by path.
;;; row_align determines where the row is placed, and col_align determines where
;;; column is placed. Both are expected to be between 0.0 and 1.0. 0.0 means
;;; left/top alignment, 1.0 means right/bottom alignment, 0.5 means center.
;;; 
;;; If use_align is FALSE, then the alignment arguments are ignored, and the
;;; tree does the minimum amount of work to scroll the item onto the screen.
;;; This means that the item will be scrolled to the edge closest to its current
;;; position. If the item is currently visible on the screen, nothing is done.
;;; 
;;; This function only works if the model is set, and path is a valid row on the
;;; model. If the model changes before the icon_view is realized, the centered
;;; path will be modified to reflect this change.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; path :
;;; 	The path of the item to move to.
;;; 
;;; use_align :
;;; 	whether to use alignment arguments, or FALSE.
;;; 
;;; row_align :
;;; 	The vertical alignment of the item specified by path.
;;; 
;;; col_align :
;;; 	The horizontal alignment of the item specified by path.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_scroll_to_path" %gtk-icon-view-scroll-to-path) :void
  (icon-view g-object)
  (path (g-boxed-foreign gtk-tree-path))
  (use-align :boolean)
  (row-align :float)
  (col-align :float))

(defun gtk-icon-view-scroll-to-path (icon-view path &key
                                     (row-align 0.5 row-align-supplied-p)
                                     (col-align 0.5 col-align-supplied-p))
  (%gtk-icon-view-scroll-to-path icon-view
                                 path
                                 (or row-align-supplied-p col-align-supplied-p)
                                 row-align col-align))

(export 'gtk-icon-view-scroll-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_visible_range ()
;;; 
;;; gboolean gtk_icon_view_get_visible_range (GtkIconView *icon_view,
;;;                                           GtkTreePath **start_path,
;;;                                           GtkTreePath **end_path);
;;; 
;;; Sets start_path and end_path to be the first and last visible path. Note
;;; that there may be invisible paths in between.
;;; 
;;; Both paths should be freed with gtk_tree_path_free() after use.
;;; 
;;; icon_view :
;;; 	A GtkIconView
;;; 
;;; start_path :
;;; 	Return location for start of region, or NULL.
;;; 
;;; end_path :
;;; 	Return location for end of region, or NULL.
;;; 
;;; Returns :
;;; 	TRUE, if valid paths were placed in start_path and end_path
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_visible_range" %gtk-icon-view-get-visible-range)
    :boolean
  (icon-view g-object)
  (start-path :pointer)
  (end-path :pointer))

(defun gtk-icon-view-get-visible-range (icon-view)
  (with-foreign-objects ((start-path :pointer) (end-path :pointer))
    (when (%gtk-icon-view-get-visible-range icon-view start-path end-path)
      (values (mem-ref start-path '(g-boxed-foreign gtk-tree-path :return))
              (mem-ref end-path '(g-boxed-foreign gtk-tree-path :return))))))

(export 'gtk-icon-view-get-visible-range)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_tooltip_item ()
;;; 
;;; void gtk_icon_view_set_tooltip_item (GtkIconView *icon_view,
;;;                                      GtkTooltip *tooltip,
;;;                                      GtkTreePath *path);
;;; 
;;; Sets the tip area of tooltip to be the area covered by the item at path.
;;; See also gtk_icon_view_set_tooltip_column() for a simpler alternative.
;;; See also gtk_tooltip_set_tip_area().
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; tooltip :
;;; 	a GtkTooltip
;;; 
;;; path :
;;; 	a GtkTreePath
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_set_tooltip_item" gtk-icon-view-set-tooltip-item) :void
  (icon-view (g-object gtk-icon-view))
  (tooltip (g-object gtk-tooltip))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-icon-view-set-tooltip-item)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_tooltip_cell ()
;;; 
;;; void gtk_icon_view_set_tooltip_cell (GtkIconView *icon_view,
;;;                                      GtkTooltip *tooltip,
;;;                                      GtkTreePath *path,
;;;                                      GtkCellRenderer *cell);
;;; 
;;; Sets the tip area of tooltip to the area which cell occupies in the item
;;; pointed to by path. See also gtk_tooltip_set_tip_area().
;;; 
;;; See also gtk_icon_view_set_tooltip_column() for a simpler alternative.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; tooltip :
;;; 	a GtkTooltip
;;; 
;;; path :
;;; 	a GtkTreePath
;;; 
;;; cell :
;;; 	a GtkCellRenderer or NULL.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_set_tooltip_cell" gtk-icon-view-set-tooltip-cell) :void
  (icon-view (g-object gtk-icon-view))
  (tooltip (g-object gtk-tooltip))
  (path (g-boxed-foreign gtk-tree-path))
  (cell-renderer (g-object gtk-cell-renderer)))

(export 'gtk-icon-view-set-tooltip-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_tooltip_context ()
;;; 
;;; gboolean gtk_icon_view_get_tooltip_context (GtkIconView *icon_view,
;;;                                             gint *x,
;;;                                             gint *y,
;;;                                             gboolean keyboard_tip,
;;;                                             GtkTreeModel **model,
;;;                                             GtkTreePath **path,
;;;                                             GtkTreeIter *iter);
;;; 
;;; This function is supposed to be used in a "query-tooltip" signal handler
;;; for GtkIconView. The x, y and keyboard_tip values which are received in the
;;; signal handler, should be passed to this function without modification.
;;; 
;;; The return value indicates whether there is an icon view item at the given
;;; coordinates (TRUE) or not (FALSE) for mouse tooltips. For keyboard tooltips
;;; the item returned will be the cursor item. When TRUE, then any of model,
;;; path and iter which have been provided will be set to point to that row and
;;; the corresponding model. x and y will always be converted to be relative to
;;; icon_view's bin_window if keyboard_tooltip is FALSE.
;;; 
;;; icon_view :
;;; 	an GtkIconView
;;; 
;;; x :
;;; 	the x coordinate (relative to widget coordinates).
;;; 
;;; y :
;;; 	the y coordinate (relative to widget coordinates).
;;; 
;;; keyboard_tip :
;;; 	whether this is a keyboard tooltip or not
;;; 
;;; model :
;;; 	a pointer to receive a GtkTreeModel or NULL.
;;; 
;;; path :
;;; 	a pointer to receive a GtkTreePath or NULL.
;;; 
;;; iter :
;;; 	a pointer to receive a GtkTreeIter or NULL.
;;; 
;;; Returns :
;;; 	whether or not the given tooltip context points to a item
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_icon_view_get_tooltip_context"
          %gtk-icon-view-get-tooltip-context) :boolean
  (icon-view (g-object gtk-icon-view))
  (x (:pointer :int))
  (y (:pointer :int))
  (keyboard-tip :boolean)
  (model (:pointer (g-object gtk-tree-model)))
  (path (:pointer (g-boxed-foreign gtk-tree-path)))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-icon-view-get-tooltip-context (icon-view x y keyboard-tip)
  (with-foreign-objects ((xx :int)
                         (yy :int)
                         (model-ptr :pointer)
                         (path-ptr :pointer))
    (setf (mem-ref xx :int) x
          (mem-ref yy :int) y)
    (let ((iter (make-gtk-tree-iter)))
      (when (%gtk-icon-view-get-tooltip-context icon-view
                                                xx
                                                yy
                                                keyboard-tip
                                                model-ptr
                                                path-ptr
                                                iter)
        (values (mem-ref xx :int) (mem-ref yy :int)
                (convert-from-foreign (mem-ref model-ptr :pointer)
                                      '(g-object gtk-tree-model))
                (convert-from-foreign (mem-ref path-ptr :pointer)
                                      '(g-boxed-foreign gtk-tree-path :return))
                iter)))))

(export 'gtk-icon-view-get-tooltip-context)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_tooltip_column ()
;;; 
;;; void gtk_icon_view_set_tooltip_column (GtkIconView *icon_view,
;;;                                        gint column);
;;; 
;;; If you only plan to have simple (text-only) tooltips on full items, you can
;;; use this function to have GtkIconView handle these automatically for you.
;;; column should be set to the column in icon_view's model containing the
;;; tooltip texts, or -1 to disable this feature.
;;; 
;;; When enabled, "has-tooltip" will be set to TRUE and icon_view will connect
;;; a "query-tooltip" signal handler.
;;; 
;;; Note that the signal handler sets the text with gtk_tooltip_set_markup(),
;;; so &, <, etc have to be escaped in the text.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; column :
;;; 	an integer, which is a valid column number for icon_view's model
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_tooltip_column ()
;;; 
;;; gint gtk_icon_view_get_tooltip_column (GtkIconView *icon_view);
;;; 
;;; Returns the column of icon_view's model which is being used for displaying
;;; tooltips on icon_view's rows.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	the index of the tooltip column that is currently being used, or -1 if
;;;     this is disabled.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_row ()
;;; 
;;; gint gtk_icon_view_get_item_row (GtkIconView *icon_view, GtkTreePath *path)
;;; 
;;; Gets the row in which the item path is currently displayed. Row numbers
;;; start at 0.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; path :
;;; 	the GtkTreePath of the item
;;; 
;;; Returns :
;;; 	The row in which the item is displayed
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_item_column ()
;;; 
;;; gint gtk_icon_view_get_item_column (GtkIconView *icon_view,
;;;                                     GtkTreePath *path);
;;; 
;;; Gets the column in which the item path is currently displayed. Column
;;; numbers start at 0.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; path :
;;; 	the GtkTreePath of the item
;;; 
;;; Returns :
;;; 	The column in which the item is displayed
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkIconViewDropPosition
;;; 
;;; typedef enum {
;;;   GTK_ICON_VIEW_NO_DROP,
;;;   GTK_ICON_VIEW_DROP_INTO,
;;;   GTK_ICON_VIEW_DROP_LEFT,
;;;   GTK_ICON_VIEW_DROP_RIGHT,
;;;   GTK_ICON_VIEW_DROP_ABOVE,
;;;   GTK_ICON_VIEW_DROP_BELOW
;;; } GtkIconViewDropPosition;
;;; 
;;; An enum for determining where a dropped item goes.
;;; 
;;; GTK_ICON_VIEW_NO_DROP
;;; 	no drop possible
;;; 
;;; GTK_ICON_VIEW_DROP_INTO
;;; 	dropped item replaces the item
;;; 
;;; GTK_ICON_VIEW_DROP_LEFT
;;; 	droppped item is inserted to the left
;;; 
;;; GTK_ICON_VIEW_DROP_RIGHT
;;; 	dropped item is inserted to the right
;;; 
;;; GTK_ICON_VIEW_DROP_ABOVE
;;; 	dropped item is inserted above
;;; 
;;; GTK_ICON_VIEW_DROP_BELOW
;;; 	dropped item is inserted below
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkIconViewDropPosition" gtk-icon-view-drop-position
  (:export t
   :type-initializer "gtk_icon_view_drop_position_get_type")
  (:no-drop 0)
  (:drop-into 1)
  (:drop-left 2)
  (:drop-right 3)
  (:drop-above 4)
  (:drop-below 5))

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_enable_model_drag_source ()
;;; 
;;; void gtk_icon_view_enable_model_drag_source
;;;                                          (GtkIconView *icon_view,
;;;                                           GdkModifierType start_button_mask,
;;;                                           const GtkTargetEntry *targets,
;;;                                           gint n_targets,
;;;                                           GdkDragAction actions);
;;; 
;;; Turns icon_view into a drag source for automatic DND. Calling this method
;;; sets "reorderable" to FALSE.
;;; 
;;; icon_view :
;;; 	a GtkIconTreeView
;;; 
;;; start_button_mask :
;;; 	Mask of allowed buttons to start drag
;;; 
;;; targets :
;;; 	the table of targets that the drag will support.
;;; 
;;; n_targets :
;;; 	the number of items in targets
;;; 
;;; actions :
;;; 	the bitmask of possible actions for a drag from this widget
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_enable_model_drag_dest ()
;;; 
;;; void gtk_icon_view_enable_model_drag_dest (GtkIconView *icon_view,
;;;                                            const GtkTargetEntry *targets,
;;;                                            gint n_targets,
;;;                                            GdkDragAction actions);
;;; 
;;; Turns icon_view into a drop destination for automatic DND. Calling this
;;; method sets "reorderable" to FALSE.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; targets :
;;; 	the table of targets that the drag will support.
;;; 
;;; n_targets :
;;; 	the number of items in targets
;;; 
;;; actions :
;;; 	the bitmask of possible actions for a drag to this widget
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unset_model_drag_source ()
;;; 
;;; void gtk_icon_view_unset_model_drag_source (GtkIconView *icon_view);
;;; 
;;; Undoes the effect of gtk_icon_view_enable_model_drag_source(). Calling this
;;; method sets "reorderable" to FALSE.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_unset_model_drag_dest ()
;;; 
;;; void gtk_icon_view_unset_model_drag_dest (GtkIconView *icon_view);
;;; 
;;; Undoes the effect of gtk_icon_view_enable_model_drag_dest(). Calling this
;;; method sets "reorderable" to FALSE.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_reorderable ()
;;; 
;;; void gtk_icon_view_set_reorderable (GtkIconView *icon_view,
;;;                                     gboolean reorderable);
;;; 
;;; This function is a convenience function to allow you to reorder models that
;;; support the GtkTreeDragSourceIface and the GtkTreeDragDestIface. Both
;;; GtkTreeStore and GtkListStore support these. If reorderable is TRUE, then
;;; the user can reorder the model by dragging and dropping rows. The developer
;;; can listen to these changes by connecting to the model's row_inserted and
;;; row_deleted signals. The reordering is implemented by setting up the icon
;;; view as a drag source and destination. Therefore, drag and drop can not be
;;; used in a reorderable view for any other purpose.
;;; 
;;; This function does not give you any degree of control over the order -- any
;;; reordering is allowed. If more control is needed, you should probably handle
;;; drag and drop manually.
;;; 
;;; icon_view :
;;; 	A GtkIconView.
;;; 
;;; reorderable :
;;; 	TRUE, if the list of items can be reordered.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_reorderable ()
;;; 
;;; gboolean gtk_icon_view_get_reorderable (GtkIconView *icon_view);
;;; 
;;; Retrieves whether the user can reorder the list via drag-and-drop.
;;; See gtk_icon_view_set_reorderable().
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; Returns :
;;; 	TRUE if the list can be reordered.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_set_drag_dest_item ()
;;; 
;;; void gtk_icon_view_set_drag_dest_item (GtkIconView *icon_view,
;;;                                        GtkTreePath *path,
;;;                                        GtkIconViewDropPosition pos);
;;; 
;;; Sets the item that is highlighted for feedback.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; path :
;;; 	The path of the item to highlight, or NULL.
;;; 
;;; pos :
;;; 	Specifies where to drop, relative to the item
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_drag_dest_item ()
;;; 
;;; void gtk_icon_view_get_drag_dest_item (GtkIconView *icon_view,
;;;                                        GtkTreePath **path,
;;;                                        GtkIconViewDropPosition *pos);
;;; 
;;; Gets information about the item that is highlighted for feedback.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; path :
;;; 	Return location for the path of the highlighted item, or NULL.
;;; 
;;; pos :
;;; 	Return location for the drop position, or NULL.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_get_dest_item_at_pos ()
;;; 
;;; gboolean gtk_icon_view_get_dest_item_at_pos (GtkIconView *icon_view,
;;;                                              gint drag_x,
;;;                                              gint drag_y,
;;;                                              GtkTreePath **path,
;;;                                              GtkIconViewDropPosition *pos);
;;; 
;;; Determines the destination item for a given position.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; drag_x :
;;; 	the position to determine the destination item for
;;; 
;;; drag_y :
;;; 	the position to determine the destination item for
;;; 
;;; path :
;;; 	Return location for the path of the item, or NULL.
;;; 
;;; pos :
;;; 	Return location for the drop position, or NULL.
;;; 
;;; Returns :
;;; 	whether there is an item at the given position.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_view_create_drag_icon ()
;;; 
;;; cairo_surface_t * gtk_icon_view_create_drag_icon (GtkIconView *icon_view,
;;;                                                   GtkTreePath *path);
;;; 
;;; Creates a cairo_surface_t representation of the item at path. This image is
;;; used for a drag icon.
;;; 
;;; icon_view :
;;; 	a GtkIconView
;;; 
;;; path :
;;; 	a GtkTreePath in icon_view
;;; 
;;; Returns :
;;; 	a newly-allocated surface of the drag icon.
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.icon-view.lisp -----------------------------------------
