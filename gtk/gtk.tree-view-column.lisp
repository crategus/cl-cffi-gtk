;;; ----------------------------------------------------------------------------
;;; gtk.tree-view-column.lisp
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
;;; GtkTreeViewColumn
;;; 
;;; A visible column in a GtkTreeView widget
;;; 
;;; Synopsis
;;; 
;;;     GtkTreeViewColumnSizing
;;;     GtkTreeViewColumn
;;;     
;;;     gtk_tree_view_column_new
;;;     gtk_tree_view_column_new_with_area
;;;     gtk_tree_view_column_new_with_attributes
;;;     gtk_tree_view_column_pack_start
;;;     gtk_tree_view_column_pack_end
;;;     gtk_tree_view_column_clear
;;;     gtk_tree_view_column_add_attribute
;;;     gtk_tree_view_column_set_attributes
;;;     gtk_tree_view_column_set_cell_data_func
;;;     gtk_tree_view_column_clear_attributes
;;;     gtk_tree_view_column_set_spacing
;;;     gtk_tree_view_column_get_spacing
;;;     gtk_tree_view_column_set_visible
;;;     gtk_tree_view_column_get_visible
;;;     gtk_tree_view_column_set_resizable
;;;     gtk_tree_view_column_get_resizable
;;;     gtk_tree_view_column_set_sizing
;;;     gtk_tree_view_column_get_sizing
;;;     gtk_tree_view_column_get_width
;;;     gtk_tree_view_column_get_fixed_width
;;;     gtk_tree_view_column_set_fixed_width
;;;     gtk_tree_view_column_set_min_width
;;;     gtk_tree_view_column_get_min_width
;;;     gtk_tree_view_column_set_max_width
;;;     gtk_tree_view_column_get_max_width
;;;     gtk_tree_view_column_clicked
;;;     gtk_tree_view_column_set_title
;;;     gtk_tree_view_column_get_title
;;;     gtk_tree_view_column_set_expand
;;;     gtk_tree_view_column_get_expand
;;;     gtk_tree_view_column_set_clickable
;;;     gtk_tree_view_column_get_clickable
;;;     gtk_tree_view_column_set_widget
;;;     gtk_tree_view_column_get_widget
;;;     gtk_tree_view_column_get_button
;;;     gtk_tree_view_column_set_alignment
;;;     gtk_tree_view_column_get_alignment
;;;     gtk_tree_view_column_set_reorderable
;;;     gtk_tree_view_column_get_reorderable
;;;     gtk_tree_view_column_set_sort_column_id
;;;     gtk_tree_view_column_get_sort_column_id
;;;     gtk_tree_view_column_set_sort_indicator
;;;     gtk_tree_view_column_get_sort_indicator
;;;     gtk_tree_view_column_set_sort_order
;;;     gtk_tree_view_column_get_sort_order
;;;     gtk_tree_view_column_cell_set_cell_data
;;;     gtk_tree_view_column_cell_get_size
;;;     gtk_tree_view_column_cell_get_position
;;;     gtk_tree_view_column_cell_is_visible
;;;     gtk_tree_view_column_focus_cell
;;;     gtk_tree_view_column_queue_resize
;;;     gtk_tree_view_column_get_tree_view
;;;     gtk_tree_view_column_get_x_offset
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkTreeViewColumn
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkTreeViewColumn implements GtkCellLayout and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "alignment"           gfloat                   : Read / Write
;;;   "cell-area"           GtkCellArea*             : Read / Write / Construct
;;;   "clickable"           gboolean                 : Read / Write
;;;   "expand"              gboolean                 : Read / Write
;;;   "fixed-width"         gint                     : Read / Write
;;;   "max-width"           gint                     : Read / Write
;;;   "min-width"           gint                     : Read / Write
;;;   "reorderable"         gboolean                 : Read / Write
;;;   "resizable"           gboolean                 : Read / Write
;;;   "sizing"              GtkTreeViewColumnSizing  : Read / Write
;;;   "sort-column-id"      gint                     : Read / Write
;;;   "sort-indicator"      gboolean                 : Read / Write
;;;   "sort-order"          GtkSortType              : Read / Write
;;;   "spacing"             gint                     : Read / Write
;;;   "title"               gchar*                   : Read / Write
;;;   "visible"             gboolean                 : Read / Write
;;;   "widget"              GtkWidget*               : Read / Write
;;;   "width"               gint                     : Read
;;;   "x-offset"            gint                     : Read
;;; 
;;; Signals
;;; 
;;;   "clicked"                                        : Run Last
;;; 
;;; Description
;;; 
;;; The GtkTreeViewColumn object represents a visible column in a GtkTreeView
;;; widget. It allows to set properties of the column header, and functions as
;;; a holding pen for the cell renderers which determine how the data in the
;;; column is displayed.
;;; 
;;; Please refer to the tree widget conceptual overview for an overview of all
;;; the objects and data types related to the tree widget and how they work
;;; together.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "alignment" property
;;; 
;;;   "alignment"                gfloat                : Read / Write
;;; 
;;; X Alignment of the column header text or widget.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "cell-area" property
;;; 
;;;   "cell-area"                GtkCellArea*         : Read / Write / Construct
;;; 
;;; The GtkCellArea used to layout cell renderers for this column.
;;; 
;;; If no area is specified when creating the tree view column with
;;; gtk_tree_view_column_new_with_area() a horizontally oriented GtkCellAreaBox
;;; will be used.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "clickable" property
;;; 
;;;   "clickable"                gboolean              : Read / Write
;;; 
;;; Whether the header can be clicked.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "expand" property
;;; 
;;;   "expand"                   gboolean              : Read / Write
;;; 
;;; Column gets share of extra width allocated to the widget.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "fixed-width" property
;;; 
;;;   "fixed-width"              gint                  : Read / Write
;;; 
;;; Current fixed width of the column.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 1
;;;
;;; ----------------------------------------------------------------------------
;;; The "max-width" property
;;; 
;;;   "max-width"                gint                  : Read / Write
;;; 
;;; Maximum allowed width of the column.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "min-width" property
;;; 
;;;   "min-width"                gint                  : Read / Write
;;; 
;;; Minimum allowed width of the column.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;;
;;; ----------------------------------------------------------------------------
;;; The "reorderable" property
;;; 
;;;   "reorderable"              gboolean              : Read / Write
;;; 
;;; Whether the column can be reordered around the headers.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "resizable" property
;;; 
;;;   "resizable"                gboolean              : Read / Write
;;; 
;;; Column is user-resizable.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "sizing" property
;;; 
;;;   "sizing"                   GtkTreeViewColumnSizing  : Read / Write
;;; 
;;; Resize mode of the column.
;;; 
;;; Default value: GTK_TREE_VIEW_COLUMN_GROW_ONLY
;;;
;;; ----------------------------------------------------------------------------
;;; The "sort-column-id" property
;;; 
;;;   "sort-column-id"           gint                  : Read / Write
;;; 
;;; Logical sort column ID this column sorts on when selected for sorting.
;;; Setting the sort column ID makes the column header clickable. Set to -1 to
;;; make the column unsortable.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.18
;;;
;;; ----------------------------------------------------------------------------
;;; The "sort-indicator" property
;;; 
;;;   "sort-indicator"           gboolean              : Read / Write
;;; 
;;; Whether to show a sort indicator.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "sort-order" property
;;; 
;;;   "sort-order"               GtkSortType           : Read / Write
;;; 
;;; Sort direction the sort indicator should indicate.
;;; 
;;; Default value: GTK_SORT_ASCENDING
;;;
;;; ----------------------------------------------------------------------------
;;; The "spacing" property
;;; 
;;;   "spacing"                  gint                  : Read / Write
;;; 
;;; Space which is inserted between cells.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "title" property
;;; 
;;;   "title"                    gchar*                : Read / Write
;;; 
;;; Title to appear in column header.
;;; 
;;; Default value: ""
;;;
;;; ----------------------------------------------------------------------------
;;; The "visible" property
;;; 
;;;   "visible"                  gboolean              : Read / Write
;;; 
;;; Whether to display the column.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "widget" property
;;; 
;;;   "widget"                   GtkWidget*            : Read / Write
;;; 
;;; Widget to put in column header button instead of column title.
;;;
;;; ----------------------------------------------------------------------------
;;; The "width" property
;;; 
;;;   "width"                    gint                  : Read
;;; 
;;; Current width of the column.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "x-offset" property
;;; 
;;;   "x-offset"                 gint                  : Read
;;; 
;;; Current X position of the column.
;;; 
;;; Allowed values: >= -2147483647
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "clicked" signal
;;; 
;;; void user_function (GtkTreeViewColumn *treeviewcolumn,
;;;                     gpointer           user_data)           : Run Last
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkTreeViewColumnSizing
;;; 
;;; typedef enum {
;;;   GTK_TREE_VIEW_COLUMN_GROW_ONLY,
;;;   GTK_TREE_VIEW_COLUMN_AUTOSIZE,
;;;   GTK_TREE_VIEW_COLUMN_FIXED
;;; } GtkTreeViewColumnSizing;
;;; 
;;; The sizing method the column uses to determine its width. Please note that
;;; GTK_TREE_VIEW_COLUMN_AUTOSIZE are inefficient for large views, and can make
;;; columns appear choppy.
;;; 
;;; GTK_TREE_VIEW_COLUMN_GROW_ONLY
;;; 	Columns only get bigger in reaction to changes in the model
;;; 
;;; GTK_TREE_VIEW_COLUMN_AUTOSIZE
;;; 	Columns resize to be the optimal size everytime the model changes.
;;; 
;;; GTK_TREE_VIEW_COLUMN_FIXED
;;; 	Columns are a fixed numbers of pixels wide.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkTreeViewColumnSizing" gtk-tree-view-column-sizing
  (:export t
   :type-initializer "gtk_tree_view_column_sizing_get_type")
  (:grow-only 0)
  (:autosize 1)
  (:fixed 2))

;;; ----------------------------------------------------------------------------
;;; GtkTreeCellDataFunc ()
;;; 
;;; void (*GtkTreeCellDataFunc) (GtkTreeViewColumn *tree_column,
;;;                              GtkCellRenderer *cell,
;;;                              GtkTreeModel *tree_model,
;;;                              GtkTreeIter *iter,
;;;                              gpointer data);
;;; 
;;; A function to set the properties of a cell instead of just using the
;;; straight mapping between the cell and the model. This is useful for
;;; customizing the cell renderer. For example, a function might get an integer
;;; from the tree_model, and render it to the "text" attribute of "cell" by
;;; converting it to its written equivilent. This is set by calling
;;; gtk_tree_view_column_set_cell_data_func()
;;; 
;;; tree_column :
;;;     A GtkTreeColumn
;;; 
;;; cell :
;;;     The GtkCellRenderer that is being rendered by tree_column
;;; 
;;; tree_model :
;;;     The GtkTreeModel being rendered
;;; 
;;; iter :
;;;     A GtkTreeIter of the current row rendered
;;; 
;;; data :
;;;     user data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeViewColumn
;;; 
;;; struct GtkTreeViewColumn;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTreeViewColumn" gtk-tree-view-column
  (:superclass g-initially-unowned
    :export t
    :interfaces ("GtkBuildable" "GtkCellLayout")
    :type-initializer "gtk_tree_view_column_get_type")
  ((alignment
    gtk-tree-view-column-alignment
    "alignment" "gfloat" t t)
   (clickable
    gtk-tree-view-column-clickable
    "clickable" "gboolean" t t)
   (expand
    gtk-tree-view-column-expand
    "expand" "gboolean" t t)
   (fixed-width
    gtk-tree-view-column-fixed-width
    "fixed-width" "gint" t t)
   (max-width
    gtk-tree-view-column-max-width
    "max-width" "gint" t t)
   (min-width
    gtk-tree-view-column-min-width
    "min-width" "gint" t t)
   (reorderable
    gtk-tree-view-column-reorderable
    "reorderable" "gboolean" t t)
   (resizable
    gtk-tree-view-column-resizable
    "resizable" "gboolean" t t)
   (sizing
    gtk-tree-view-column-sizing
    "sizing" "GtkTreeViewColumnSizing" t t)
   (sort-indicator
    gtk-tree-view-column-sort-indicator
    "sort-indicator" "gboolean" t t)
   (sort-order
    gtk-tree-view-column-sort-order
    "sort-order" "GtkSortType" t t)
   (spacing
    gtk-tree-view-column-spacing
    "spacing" "gint" t t)
   (title
    gtk-tree-view-column-title
    "title" "gchararray" t t)
   (visible
    gtk-tree-view-column-visible
    "visible" "gboolean" t t)
   (widget
    gtk-tree-view-column-widget
    "widget" "GtkWidget" t t)
   (width
    gtk-tree-view-column-width "width" "gint" t nil)
   (:cffi tree-view
          gtk-tree-view-column-tree-view g-object
          "gtk_tree_view_column_get_tree_view" nil)
   (:cffi sort-column-id
          gtk-tree-view-column-sort-column-id :int
          "gtk_tree_view_column_get_sort_column_id"
          "gtk_tree_view_column_set_sort_column_id")
   (:cffi cell-renderers
          gtk-tree-view-column-cell-renderers
          (g-list g-object :free-from-foreign t)
          "gtk_tree_view_column_get_cell_renderers" nil)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new ()
;;; 
;;; GtkTreeViewColumn * gtk_tree_view_column_new (void);
;;; 
;;; Creates a new GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     A newly created GtkTreeViewColumn.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new_with_area ()
;;; 
;;; GtkTreeViewColumn * gtk_tree_view_column_new_with_area (GtkCellArea *area);
;;; 
;;; Creates a new GtkTreeViewColumn using area to render its cells.
;;; 
;;; area :
;;;     the GtkCellArea that the newly created column should use to layout
;;;     cells.
;;; 
;;; Returns :
;;;     A newly created GtkTreeViewColumn.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_new_with_attributes ()
;;; 
;;; GtkTreeViewColumn * gtk_tree_view_column_new_with_attributes
;;;                                                      (const gchar *title,
;;;                                                       GtkCellRenderer *cell,
;;;                                                       ...);
;;; 
;;; Creates a new GtkTreeViewColumn with a number of default values. This is
;;; equivalent to calling gtk_tree_view_column_set_title(),
;;; gtk_tree_view_column_pack_start(), and gtk_tree_view_column_set_attributes()
;;; on the newly created GtkTreeViewColumn.
;;; 
;;; Here's a simple example:
;;; 
;;; enum { TEXT_COLUMN, COLOR_COLUMN, N_COLUMNS };
;;; ...
;;; {
;;;   GtkTreeViewColumn *column;
;;;   GtkCellRenderer   *renderer = gtk_cell_renderer_text_new ();
;;; 
;;;   column = gtk_tree_view_column_new_with_attributes
;;;                                                 ("Title",
;;;                                                  renderer,
;;;                                                  "text", TEXT_COLUMN,
;;;                                                  "foreground", COLOR_COLUMN,
;;;                                                  NULL);
;;; }
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
;;;     A newly created GtkTreeViewColumn.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_pack_start ()
;;; 
;;; void gtk_tree_view_column_pack_start (GtkTreeViewColumn *tree_column,
;;;                                       GtkCellRenderer *cell,
;;;                                       gboolean expand);
;;; 
;;; Packs the cell into the beginning of the column. If expand is FALSE, then
;;; the cell is allocated no more space than it needs. Any unused space is
;;; divided evenly between cells for which expand is TRUE.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; cell :
;;;     The GtkCellRenderer.
;;; 
;;; expand :
;;;     TRUE if cell is to be given extra space allocated to tree_column.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_pack_start" %gtk-tree-view-column-pack-start)
    :void
  (tree-column (g-object gtk-tree-view-column))
  (cell (g-object gtk-cell-renderer))
  (expand :boolean))

(defun gtk-tree-view-column-pack-start (tree-column cell &key (expand t))
  (%gtk-tree-view-column-pack-start tree-column cell expand))

(export 'gtk-tree-view-column-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_pack_end ()
;;; 
;;; void gtk_tree_view_column_pack_end (GtkTreeViewColumn *tree_column,
;;;                                     GtkCellRenderer *cell,
;;;                                     gboolean expand);
;;; 
;;; Adds the cell to end of the column. If expand is FALSE, then the cell is
;;; allocated no more space than it needs. Any unused space is divided evenly
;;; between cells for which expand is TRUE.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; cell :
;;;     The GtkCellRenderer.
;;; 
;;; expand :
;;;     TRUE if cell is to be given extra space allocated to tree_column.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_pack_end" %gtk-tree-view-column-pack-end) :void
  (tree-column (g-object gtk-tree-view-column))
  (cell (g-object gtk-cell-renderer))
  (expand :boolean))

(defun gtk-tree-view-column-pack-end (tree-column cell &key (expand t))
  (%gtk-tree-view-column-pack-end tree-column cell expand))

(export 'gtk-tree-view-column-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clear ()
;;; 
;;; void gtk_tree_view_column_clear (GtkTreeViewColumn *tree_column);
;;; 
;;; Unsets all the mappings on all renderers on the tree_column.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_clear" gtk-tree-view-column-clear) :void
  (tree-column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_add_attribute ()
;;; 
;;; void gtk_tree_view_column_add_attribute (GtkTreeViewColumn *tree_column,
;;;                                          GtkCellRenderer *cell_renderer,
;;;                                          const gchar *attribute,
;;;                                          gint column);
;;; 
;;; Adds an attribute mapping to the list in tree_column. The column is the
;;; column of the model to get a value from, and the attribute is the parameter
;;; on cell_renderer to be set from the value. So for example if column 2 of the
;;; model contains strings, you could have the "text" attribute of a
;;; GtkCellRendererText get its values from column 2.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; cell_renderer :
;;;     the GtkCellRenderer to set attributes on
;;; 
;;; attribute :
;;;     An attribute on the renderer
;;; 
;;; column :
;;;     The column position on the model to get the attribute from.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_add_attribute"
           gtk-tree-view-column-add-attribute) :void
  (tree-column (g-object gtk-tree-view-column))
  (cell-renderer (g-object gtk-cell-renderer))
  (attribute :string)
  (column :int))

(export 'gtk-tree-view-column-add-attribute)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_attributes ()
;;; 
;;; void gtk_tree_view_column_set_attributes (GtkTreeViewColumn *tree_column,
;;;                                           GtkCellRenderer *cell_renderer,
;;;                                           ...);
;;; 
;;; Sets the attributes in the list as the attributes of tree_column. The
;;; attributes should be in attribute/column order, as in
;;; gtk_tree_view_column_add_attribute(). All existing attributes are removed,
;;; and replaced with the new attributes.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; cell_renderer :
;;;     the GtkCellRenderer we're setting the attributes of
;;; 
;;; ... :
;;;     A NULL-terminated list of attributes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_cell_data_func ()
;;; 
;;; void gtk_tree_view_column_set_cell_data_func
;;;                                             (GtkTreeViewColumn *tree_column,
;;;                                              GtkCellRenderer *cell_renderer,
;;;                                              GtkTreeCellDataFunc func,
;;;                                              gpointer func_data,
;;;                                              GDestroyNotify destroy);
;;; 
;;; Sets the GtkTreeViewColumnFunc to use for the column. This function is used
;;; instead of the standard attributes mapping for setting the column value, and
;;; should set the value of tree_column's cell renderer as appropriate. func may
;;; be NULL to remove an older one.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; cell_renderer :
;;;     A GtkCellRenderer
;;; 
;;; func :
;;;     The GtkTreeViewColumnFunc to use. [allow-none]
;;; 
;;; func_data :
;;;     The user data for func.
;;; 
;;; destroy :
;;;     The destroy notification for func_data
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-cell-data-func-cb :void
    ((tree-column (g-object gtk-tree-column))
     (cell (g-object gtk-cell-renderer))
     (tree-model (g-object gtk-tree-model))
     (iter (g-boxed-foreign gtk-tree-iter))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
        (funcall fn tree-column cell tree-model iter)
      (return-from-tree-cell-data-function () nil))))

(defcallback gtk-tree-cell-data-func-destroy-cb :void ((data :pointer))
  (free-stable-pointer data))

(defcfun ("gtk_tree_view_column_set_cell_data_func"
          %gtk-tree-view-column-set-cell-data-func) :void
  (tree-column (g-object gtk-tree-view-column))
  (cell-renderer (g-object gtk-cell-renderer))
  (func :pointer)
  (func-data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-view-column-set-cell-data-function (tree-column cell-renderer
                                                                func)
  (%gtk-tree-view-column-set-cell-data-func
                                 tree-column
                                 cell-renderer
                                 (callback gtk-tree-cell-data-func-cb)
                                 (allocate-stable-pointer func)
                                 (callback gtk-tree-cell-data-func-destroy-cb)))

(export 'gtk-tree-view-column-set-cell-data-function)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clear_attributes ()
;;; 
;;; void gtk_tree_view_column_clear_attributes (GtkTreeViewColumn *tree_column,
;;;                                             GtkCellRenderer *cell_renderer)
;;; 
;;; Clears all existing attributes previously set with
;;; gtk_tree_view_column_set_attributes().
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; cell_renderer :
;;;     a GtkCellRenderer to clear the attribute mapping on.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_clear_attributes"
           gtk-tree-view-column-clear-attributes) :void
  (tree-column (g-object gtk-tree-column))
  (cell-renderer (g-object gtk-cell-renderer)))

(export 'gtk-tree-view-column-clear-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_spacing ()
;;; 
;;; void gtk_tree_view_column_set_spacing (GtkTreeViewColumn *tree_column,
;;;                                        gint spacing);
;;; 
;;; Sets the spacing field of tree_column, which is the number of pixels to
;;; place between cell renderers packed into it.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; spacing :
;;;     distance between cell renderers in pixels.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_spacing ()
;;; 
;;; gint gtk_tree_view_column_get_spacing (GtkTreeViewColumn *tree_column);
;;; 
;;; Returns the spacing of tree_column.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     the spacing of tree_column.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_visible ()
;;; 
;;; void gtk_tree_view_column_set_visible (GtkTreeViewColumn *tree_column,
;;;                                        gboolean visible);
;;; 
;;; Sets the visibility of tree_column.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; visible :
;;;     TRUE if the tree_column is visible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_visible ()
;;; 
;;; gboolean gtk_tree_view_column_get_visible (GtkTreeViewColumn *tree_column);
;;; 
;;; Returns TRUE if tree_column is visible.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     whether the column is visible or not. If it is visible, then the tree
;;;     will show the column.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_resizable ()
;;; 
;;; void gtk_tree_view_column_set_resizable (GtkTreeViewColumn *tree_column,
;;;                                          gboolean resizable);
;;; 
;;; If resizable is TRUE, then the user can explicitly resize the column by
;;; grabbing the outer edge of the column button. If resizable is TRUE and
;;; sizing mode of the column is GTK_TREE_VIEW_COLUMN_AUTOSIZE, then the sizing
;;; mode is changed to GTK_TREE_VIEW_COLUMN_GROW_ONLY.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; resizable :
;;;     TRUE, if the column can be resized
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_resizable ()
;;; 
;;; gboolean gtk_tree_view_column_get_resizable (GtkTreeViewColumn *tree_column)
;;; 
;;; Returns TRUE if the tree_column can be resized by the end user.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; Returns :
;;;     TRUE, if the tree_column can be resized.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_sizing ()
;;; 
;;; void gtk_tree_view_column_set_sizing (GtkTreeViewColumn *tree_column,
;;;                                       GtkTreeViewColumnSizing type);
;;; 
;;; Sets the growth behavior of tree_column to type.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; type :
;;;     The GtkTreeViewColumnSizing.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_sizing ()
;;; 
;;; GtkTreeViewColumnSizing gtk_tree_view_column_get_sizing (GtkTreeViewColumn
;;; *tree_column);
;;; 
;;; Returns the current type of tree_column.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     The type of tree_column.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_width ()
;;; 
;;; gint gtk_tree_view_column_get_width (GtkTreeViewColumn *tree_column);
;;; 
;;; Returns the current size of tree_column in pixels.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     The current width of tree_column.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_fixed_width ()
;;; 
;;; gint gtk_tree_view_column_get_fixed_width (GtkTreeViewColumn *tree_column);
;;; 
;;; Gets the fixed width of the column. This value is only meaning may not be
;;; the actual width of the column on the screen, just what is requested.
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; Returns :
;;;     the fixed width of the column
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_fixed_width ()
;;; 
;;; void gtk_tree_view_column_set_fixed_width (GtkTreeViewColumn *tree_column,
;;;                                            gint fixed_width);
;;; 
;;; Sets the size of the column in pixels. This is meaningful only if the sizing
;;; type is GTK_TREE_VIEW_COLUMN_FIXED. The size of the column is clamped to the
;;; min/max width for the column. Please note that the min/max width of the
;;; column doesn't actually affect the "fixed_width" property of the widget,
;;; just the actual size when displayed.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; fixed_width :
;;;     The size to set tree_column to. Must be greater than 0.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_min_width ()
;;; 
;;; void gtk_tree_view_column_set_min_width (GtkTreeViewColumn *tree_column,
;;;                                          gint min_width);
;;; 
;;; Sets the minimum width of the tree_column. If min_width is -1, then the
;;; minimum width is unset.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; min_width :
;;;     The minimum width of the column in pixels, or -1.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_min_width ()
;;; 
;;; gint gtk_tree_view_column_get_min_width (GtkTreeViewColumn *tree_column);
;;; 
;;; Returns the minimum width in pixels of the tree_column, or -1 if no minimum
;;; width is set.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     The minimum width of the tree_column.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_max_width ()
;;; 
;;; void gtk_tree_view_column_set_max_width (GtkTreeViewColumn *tree_column,
;;;                                          gint max_width);
;;; 
;;; Sets the maximum width of the tree_column. If max_width is -1, then the
;;; maximum width is unset. Note, the column can actually be wider than max
;;; width if it's the last column in a view. In this case, the column expands
;;; to fill any extra space.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; max_width :
;;;     The maximum width of the column in pixels, or -1.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_max_width ()
;;; 
;;; gint gtk_tree_view_column_get_max_width (GtkTreeViewColumn *tree_column);
;;; 
;;; Returns the maximum width in pixels of the tree_column, or -1 if no maximum
;;; width is set.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     The maximum width of the tree_column.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_clicked ()
;;; 
;;; void gtk_tree_view_column_clicked (GtkTreeViewColumn *tree_column);
;;; 
;;; Emits the "clicked" signal on the column. This function will only work if
;;; tree_column is clickable.
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_title ()
;;; 
;;; void gtk_tree_view_column_set_title (GtkTreeViewColumn *tree_column,
;;;                                      const gchar *title);
;;; 
;;; Sets the title of the tree_column. If a custom widget has been set, then
;;; this value is ignored.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; title :
;;;     The title of the tree_column.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_title ()
;;; 
;;; const gchar * gtk_tree_view_column_get_title
;;;                                             (GtkTreeViewColumn *tree_column)
;;; 
;;; Returns the title of the widget.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     the title of the column. This string should not be modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_expand ()
;;; 
;;; void gtk_tree_view_column_set_expand (GtkTreeViewColumn *tree_column,
;;;                                       gboolean expand);
;;; 
;;; Sets the column to take available extra space. This space is shared equally
;;; amongst all columns that have the expand set to TRUE. If no column has this
;;; option set, then the last column gets all extra space. By default, every
;;; column is created with this FALSE.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; expand :
;;;     TRUE if the column should take available extra space, FALSE if not
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_expand ()
;;; 
;;; gboolean gtk_tree_view_column_get_expand (GtkTreeViewColumn *tree_column);
;;; 
;;; Return TRUE if the column expands to take any available space.
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; Returns :
;;;     TRUE, if the column expands
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_clickable ()
;;; 
;;; void gtk_tree_view_column_set_clickable (GtkTreeViewColumn *tree_column,
;;;                                          gboolean clickable);
;;; 
;;; Sets the header to be active if active is TRUE. When the header is active,
;;; then it can take keyboard focus, and can be clicked.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; clickable :
;;;     TRUE if the header is active.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_clickable ()
;;; 
;;; gboolean gtk_tree_view_column_get_clickable (GtkTreeViewColumn *tree_column)
;;; 
;;; Returns TRUE if the user can click on the header for the column.
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; Returns :
;;;     TRUE if user can click the column header.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_widget ()
;;; 
;;; void gtk_tree_view_column_set_widget (GtkTreeViewColumn *tree_column,
;;;                                       GtkWidget *widget);
;;; 
;;; Sets the widget in the header to be widget. If widget is NULL, then the
;;; header button is set with a GtkLabel set to the title of tree_column.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; widget :
;;;     A child GtkWidget, or NULL. [allow-none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_widget ()
;;; 
;;; GtkWidget * gtk_tree_view_column_get_widget (GtkTreeViewColumn *tree_column)
;;; 
;;; Returns the GtkWidget in the button on the column header. If a custom widget
;;; has not been set then NULL is returned.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     The GtkWidget in the column header, or NULL. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_button ()
;;; 
;;; GtkWidget * gtk_tree_view_column_get_button (GtkTreeViewColumn *tree_column)
;;; 
;;; Returns the button used in the treeview column header
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; Returns :
;;;     The button for the column header. [transfer none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_alignment ()
;;; 
;;; void gtk_tree_view_column_set_alignment (GtkTreeViewColumn *tree_column,
;;;                                          gfloat xalign);
;;; 
;;; Sets the alignment of the title or custom widget inside the column header.
;;; The alignment determines its location inside the button -- 0.0 for left,
;;; 0.5 for center, 1.0 for right.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; xalign :
;;;     The alignment, which is between [0.0 and 1.0] inclusive.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_alignment ()
;;; 
;;; gfloat gtk_tree_view_column_get_alignment (GtkTreeViewColumn *tree_column);
;;; 
;;; Returns the current x alignment of tree_column. This value can range between
;;; 0.0 and 1.0.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     The current alignent of tree_column.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_reorderable ()
;;; 
;;; void gtk_tree_view_column_set_reorderable (GtkTreeViewColumn *tree_column,
;;;                                            gboolean reorderable);
;;; 
;;; If reorderable is TRUE, then the column can be reordered by the end user
;;; dragging the header.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; reorderable :
;;;     TRUE, if the column can be reordered.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_reorderable ()
;;; 
;;; gboolean gtk_tree_view_column_get_reorderable
;;;                                             (GtkTreeViewColumn *tree_column)
;;; 
;;; Returns TRUE if the tree_column can be reordered by the user.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; Returns :
;;;     TRUE if the tree_column can be reordered by the user.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_sort_column_id ()
;;; 
;;; void gtk_tree_view_column_set_sort_column_id
;;;                                             (GtkTreeViewColumn *tree_column,
;;;                                              gint sort_column_id);
;;; 
;;; Sets the logical sort_column_id that this column sorts on when this column
;;; is selected for sorting. Doing so makes the column header clickable.
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; sort_column_id :
;;;     The sort_column_id of the model to sort on.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_sort_column_id ()
;;; 
;;; gint gtk_tree_view_column_get_sort_column_id
;;;                                             (GtkTreeViewColumn *tree_column)
;;; 
;;; Gets the logical sort_column_id that the model sorts on when this column is
;;; selected for sorting. See gtk_tree_view_column_set_sort_column_id().
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; Returns :
;;;     the current sort_column_id for this column, or -1 if this column can't
;;;     be used for sorting.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_sort_indicator ()
;;; 
;;; void gtk_tree_view_column_set_sort_indicator
;;;                                             (GtkTreeViewColumn *tree_column,
;;;                                              gboolean setting);
;;; 
;;; Call this function with a setting of TRUE to display an arrow in the header
;;; button indicating the column is sorted. Call
;;; gtk_tree_view_column_set_sort_order() to change the direction of the arrow.
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; setting :
;;;     TRUE to display an indicator that the column is sorted
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_sort_indicator ()
;;; 
;;; gboolean gtk_tree_view_column_get_sort_indicator
;;;                                             (GtkTreeViewColumn *tree_column)
;;; 
;;; Gets the value set by gtk_tree_view_column_set_sort_indicator().
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; Returns :
;;;     whether the sort indicator arrow is displayed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_set_sort_order ()
;;; 
;;; void gtk_tree_view_column_set_sort_order (GtkTreeViewColumn *tree_column,
;;;                                           GtkSortType order);
;;; 
;;; Changes the appearance of the sort indicator.
;;; 
;;; This does not actually sort the model. Use
;;; gtk_tree_view_column_set_sort_column_id() if you want automatic sorting
;;; support. This function is primarily for custom sorting behavior, and should
;;; be used in conjunction with gtk_tree_sortable_set_sort_column() to do that.
;;; For custom models, the mechanism will vary.
;;; 
;;; The sort indicator changes direction to indicate normal sort or reverse
;;; sort. Note that you must have the sort indicator enabled to see anything
;;; when calling this function; see gtk_tree_view_column_set_sort_indicator().
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; order :
;;;     sort order that the sort indicator should indicate
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_sort_order ()
;;; 
;;; GtkSortType gtk_tree_view_column_get_sort_order
;;;                                             (GtkTreeViewColumn *tree_column)
;;; 
;;; Gets the value set by gtk_tree_view_column_set_sort_order().
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; Returns :
;;;     the sort order the sort indicator is indicating
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_set_cell_data ()
;;; 
;;; void gtk_tree_view_column_cell_set_cell_data
;;;                                             (GtkTreeViewColumn *tree_column,
;;;                                              GtkTreeModel *tree_model,
;;;                                              GtkTreeIter *iter,
;;;                                              gboolean is_expander,
;;;                                              gboolean is_expanded);
;;; 
;;; Sets the cell renderer based on the tree_model and iter. That is, for every
;;; attribute mapping in tree_column, it will get a value from the set column on
;;; the iter, and use that value to set the attribute on the cell renderer. This
;;; is used primarily by the GtkTreeView.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; tree_model :
;;;     The GtkTreeModel to to get the cell renderers attributes from.
;;; 
;;; iter :
;;;     The GtkTreeIter to to get the cell renderer's attributes from.
;;; 
;;; is_expander :
;;;     TRUE, if the row has children
;;; 
;;; is_expanded :
;;;     TRUE, if the row has visible children
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_set_cell_data"
           gtk-tree-view-column-cell-set-cell-data) :void
  (tree-column (g-object gtk-tree-view-column))
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (is-expander :boolean)
  (is-expanded :boolean))

(export 'gtk-tree-view-column-cell-set-data)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_get_size ()
;;; 
;;; void gtk_tree_view_column_cell_get_size (GtkTreeViewColumn *tree_column,
;;;                                          const GdkRectangle *cell_area,
;;;                                          gint *x_offset,
;;;                                          gint *y_offset,
;;;                                          gint *width,
;;;                                          gint *height);
;;; 
;;; Obtains the width and height needed to render the column. This is used
;;; primarily by the GtkTreeView.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; cell_area :
;;;     The area a cell in the column will be allocated, or NULL.
;;; 
;;; x_offset :
;;;     location to return x offset of a cell relative to cell_area, or NULL.
;;; 
;;; y_offset :
;;;     location to return y offset of a cell relative to cell_area, or NULL.
;;; 
;;; width :
;;;     location to return width needed to render a cell, or NULL.
;;; 
;;; height :
;;;     location to return height needed to render a cell, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_get_size"
          %gtk-tree-view-column-cell-get-size) :void
  (tree-column (g-object gtk-tree-view-column))
  (cell-area (g-boxed-foreign gdk-rectangle))
  (x-offset (:pointer :int))
  (y-offset (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun tree-view-column-cell-size (tree-column cell-area)
  (with-foreign-objects ((x :int) (y :int) (width :int) (height :int))
    (%gtk-tree-view-column-cell-get-size tree-column cell-area x y width height)
    (values (mem-ref x :int)
            (mem-ref y :int)
            (mem-ref width :int)
            (mem-ref height :int))))

(export 'gtk-tree-view-column-cell-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_get_position ()
;;; 
;;; gboolean gtk_tree_view_column_cell_get_position
;;;                                             (GtkTreeViewColumn *tree_column,
;;;                                              GtkCellRenderer *cell_renderer,
;;;                                              gint *x_offset,
;;;                                              gint *width);
;;; 
;;; Obtains the horizontal position and size of a cell in a column. If the cell
;;; is not found in the column, start_pos and width are not changed and FALSE is
;;; returned.
;;; 
;;; tree_column :
;;;     a GtkTreeViewColumn
;;; 
;;; cell_renderer :
;;;     a GtkCellRenderer
;;; 
;;; x_offset :
;;;     return location for the horizontal position of cell within tree_column,
;;;     may be NULL.
;;; 
;;; width :
;;;     return location for the width of cell, may be NULL.
;;; 
;;; Returns :
;;;     TRUE if cell belongs to tree_column.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_cell_get_position"
          %gtk-tree-view-column-cell-get-position) :boolean
  (tree-column (g-object gtk-tree-view-column))
  (cell-renderer (g-object gtk-cell-renderer))
  (start-pos (:pointer :int))
  (width (:pointer :int)))

(defun gtk-tree-view-column-cell-position (tree-column cell-renderer)
  (with-foreign-objects ((start :int) (width :int))
    (when (%gtk-tree-view-column-cell-get-position tree-column
                                                   cell-renderer
                                                   start
                                                   width)
      (list (mem-ref start :int) (mem-ref width :int)))))

(export 'gtk-tree-view-column-cell-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_cell_is_visible ()
;;; 
;;; gboolean gtk_tree_view_column_cell_is_visible
;;;                                             (GtkTreeViewColumn *tree_column)
;;; 
;;; Returns TRUE if any of the cells packed into the tree_column are visible.
;;; For this to be meaningful, you must first initialize the cells with
;;; gtk_tree_view_column_cell_set_cell_data()
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; Returns :
;;;     TRUE, if any of the cells packed into the tree_column are currently
;;;     visible
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_focus_cell ()
;;; 
;;; void gtk_tree_view_column_focus_cell (GtkTreeViewColumn *tree_column,
;;;                                       GtkCellRenderer *cell);
;;; 
;;; Sets the current keyboard focus to be at cell, if the column contains 2 or
;;; more editable and activatable cells.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; cell :
;;;     A GtkCellRenderer
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_focus_cell" gtk-tree-view-column-focus-cell)
    :void
  (tree-column (g-object gtk-tree-view-column))
  (cell-renderer (g-object gtk-cell-renderer)))

(export 'gtk-tree-view-column-focus-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_queue_resize ()
;;; 
;;; void gtk_tree_view_column_queue_resize (GtkTreeViewColumn *tree_column);
;;; 
;;; Flags the column, and the cell renderers added to this column, to have their
;;; sizes renegotiated.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_view_column_queue_resize"
          %gtk-tree-view-column-queue-resize) :void
  (tree-column (g-object gtk-tree-view-column)))

(export 'gtk-tree-view-column-queue-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_tree_view ()
;;; 
;;; GtkWidget * gtk_tree_view_column_get_tree_view
;;;                                             (GtkTreeViewColumn *tree_column)
;;; 
;;; Returns the GtkTreeView wherein tree_column has been inserted. If column is
;;; currently not inserted in any tree view, NULL is returned.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn
;;; 
;;; Returns :
;;;     The tree view wherein column has been inserted if any, NULL otherwise.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_view_column_get_x_offset ()
;;; 
;;; gint gtk_tree_view_column_get_x_offset (GtkTreeViewColumn *tree_column);
;;; 
;;; Returns the current X offset of tree_column in pixels.
;;; 
;;; tree_column :
;;;     A GtkTreeViewColumn.
;;; 
;;; Returns :
;;;     The current X offset of tree_column.
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.tree-view-column.lisp ----------------------------------
