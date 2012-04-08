;;; ----------------------------------------------------------------------------
;;; gtk.cell-layout.lisp
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
;;; GtkCellLayout
;;; 
;;; An interface for packing cells
;;; 
;;; Synopsis
;;; 
;;;     GtkCellLayout
;;;     GtkCellLayoutIface
;;;
;;;     gtk_cell_layout_pack_start
;;;     gtk_cell_layout_pack_end
;;;     gtk_cell_layout_get_area
;;;     gtk_cell_layout_get_cells
;;;     gtk_cell_layout_reorder
;;;     gtk_cell_layout_clear
;;;     gtk_cell_layout_set_attributes
;;;     gtk_cell_layout_add_attribute
;;;     gtk_cell_layout_set_cell_data_func
;;;     gtk_cell_layout_clear_attributes
;;; 
;;; Object Hierarchy
;;; 
;;;   GInterface
;;;    +----GtkCellLayout
;;; 
;;; Prerequisites
;;; 
;;; GtkCellLayout requires GObject.
;;; Known Implementations
;;; 
;;; GtkCellLayout is implemented by GtkAppChooserButton, GtkCellArea,
;;; GtkCellAreaBox, GtkCellView, GtkComboBox, GtkComboBoxText,
;;; GtkEntryCompletion, GtkIconView and GtkTreeViewColumn.
;;; 
;;; Description
;;; 
;;; GtkCellLayout is an interface to be implemented by all objects which want
;;; to provide a GtkTreeViewColumn-like API for packing cells, setting
;;; attributes and data funcs.
;;; 
;;; One of the notable features provided by implementations of GtkCellLayout are
;;; attributes. Attributes let you set the properties in flexible ways. They can
;;; just be set to constant values like regular properties. But they can also be
;;; mapped to a column of the underlying tree model with
;;; gtk_cell_layout_set_attributes(), which means that the value of the
;;; attribute can change from cell to cell as they are rendered by the cell
;;; renderer. Finally, it is possible to specify a function with
;;; gtk_cell_layout_set_cell_data_func() that is called to determine the value
;;; of the attribute for each cell that is rendered.
;;; 
;;; GtkCellLayouts as GtkBuildable
;;; 
;;; Implementations of GtkCellLayout which also implement the GtkBuildable
;;; interface (GtkCellView, GtkIconView, GtkComboBox, GtkComboBoxEntry,
;;; GtkEntryCompletion, GtkTreeViewColumn) accept GtkCellRenderer objects as
;;; <child> elements in UI definitions. They support a custom <attributes>
;;; element for their children, which can contain multiple <attribute> elements.
;;; Each <attribute> element has a name attribute which specifies a property of
;;; the cell renderer; the content of the element is the attribute value.
;;; 
;;; Example 64. A UI definition fragment specifying attributes
;;; 
;;; <object class="GtkCellView">
;;;   <child>
;;;     <object class="GtkCellRendererText"/>
;;;     <attributes>
;;;       <attribute name="text">0</attribute>
;;;     </attributes>
;;;   </child>"
;;; </object>
;;; 
;;; Furthermore for implementations of GtkCellLayout that use a GtkCellArea to
;;; lay out cells (all GtkCellLayouts in GTK+ use a GtkCellArea) cell properties
;;; can also be defined in the format by specifying the custom <cell-packing>
;;; attribute which can contain multiple <property> elements defined in the
;;; normal way.
;;; 
;;; Example 65. A UI definition fragment specifying cell properties
;;; 
;;; <object class="GtkTreeViewColumn">
;;;   <child>
;;;     <object class="GtkCellRendererText"/>
;;;     <cell-packing>
;;;       <property name="align">True</property>
;;;       <property name="expand">False</property>
;;;     </cell-packing>
;;;   </child>"
;;; </object>
;;; 
;;; Subclassing GtkCellLayout implementations
;;; 
;;; When subclassing a widget that implements GtkCellLayout like GtkIconView or
;;; GtkComboBox, there are some considerations related to the fact that these
;;; widgets internally use a GtkCellArea. The cell area is exposed as a
;;; construct-only property by these widgets. This means that it is possible
;;; to e.g. do
;;; 
;;; combo = g_object_new (GTK_TYPE_COMBO_BOX, "cell-area", my_cell_area, NULL);
;;; 
;;; to use a custom cell area with a combo box. But construct properties are
;;; only initialized after instance init() functions have run, which means that
;;; using functions which rely on the existence of the cell area in your
;;; subclass' init() function will cause the default cell area to be
;;; instantiated. In this case, a provided construct property value will be
;;; ignored (with a warning, to alert you to the problem).
;;; 
;;; static void
;;; my_combo_box_init (MyComboBox *b)
;;; {
;;;   GtkCellRenderer *cell;
;;; 
;;;   cell = gtk_cell_renderer_pixbuf_new ();
;;;   /* The following call causes the default cell area for combo boxes,
;;;    * a GtkCellAreaBox, to be instantiated
;;;    */
;;;   gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (b), cell, FALSE);
;;;   ...
;;; }
;;; 
;;; GtkWidget *
;;; my_combo_box_new (GtkCellArea *area)
;;; {
;;;   /* This call is going to cause a warning
;;;    * about area being ignored
;;;    */
;;;   return g_object_new (MY_TYPE_COMBO_BOX, "cell-area", area, NULL);
;;; }
;;; 
;;; If supporting alternative cell areas with your derived widget is not
;;; important, then this does not have to concern you. If you want to support
;;; alternative cell areas, you can do so by moving the problematic calls out
;;; of init() and into a constructor() for your class.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellLayout
;;; 
;;; typedef struct _GtkCellLayout GtkCellLayout;
;;; 
;;; struct GtkCellLayoutIface
;;; 
;;; struct GtkCellLayoutIface {
;;;   GTypeInterface g_iface;
;;; 
;;;   /* Virtual Table */
;;;   void (* pack_start)         (GtkCellLayout         *cell_layout,
;;;                                GtkCellRenderer       *cell,
;;;                                gboolean               expand);
;;;   void (* pack_end)           (GtkCellLayout         *cell_layout,
;;;                                GtkCellRenderer       *cell,
;;;                                gboolean               expand);
;;;   void (* clear)              (GtkCellLayout         *cell_layout);
;;;   void (* add_attribute)      (GtkCellLayout         *cell_layout,
;;;                                GtkCellRenderer       *cell,
;;;                                const gchar           *attribute,
;;;                                gint                   column);
;;;   void (* set_cell_data_func) (GtkCellLayout         *cell_layout,
;;;                                GtkCellRenderer       *cell,
;;;                                GtkCellLayoutDataFunc  func,
;;;                                gpointer               func_data,
;;;                                GDestroyNotify         destroy);
;;;   void (* clear_attributes)   (GtkCellLayout         *cell_layout,
;;;                                GtkCellRenderer       *cell);
;;;   void (* reorder)            (GtkCellLayout         *cell_layout,
;;;                                GtkCellRenderer       *cell,
;;;                                gint                   position);
;;;   GList* (* get_cells)        (GtkCellLayout         *cell_layout);
;;; 
;;;   GtkCellArea *(* get_area)   (GtkCellLayout         *cell_layout);
;;; };
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkCellLayout" gtk-cell-layout
  (:export t
   :type-initializer "gtk_cell_layout_get_type"))

;;; ----------------------------------------------------------------------------
;;; GtkCellLayoutDataFunc ()
;;; 
;;; void (*GtkCellLayoutDataFunc) (GtkCellLayout *cell_layout,
;;;                                GtkCellRenderer *cell,
;;;                                GtkTreeModel *tree_model,
;;;                                GtkTreeIter *iter,
;;;                                gpointer data);
;;; 
;;; A function which should set the value of cell_layout's cell renderer(s) as
;;; appropriate.
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; cell :
;;;     the cell renderer whose value is to be set
;;; 
;;; tree_model :
;;;     the model
;;; 
;;; iter :
;;;     a GtkTreeIter indicating the row to set the value for
;;; 
;;; data :
;;;     user data passed to gtk_cell_layout_set_cell_data_func()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_pack_start ()
;;; 
;;; void gtk_cell_layout_pack_start (GtkCellLayout *cell_layout,
;;;                                  GtkCellRenderer *cell,
;;;                                  gboolean expand);
;;; 
;;; Packs the cell into the beginning of cell_layout. If expand is FALSE, then
;;; the cell is allocated no more space than it needs. Any unused space is
;;; divided evenly between cells for which expand is TRUE.
;;; 
;;; Note that reusing the same cell renderer is not supported.
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; cell :
;;;     a GtkCellRenderer
;;; 
;;; expand :
;;;     TRUE if cell is to be given extra space allocated to cell_layout
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_pack_start" %gtk-cell-layout-pack-start) :void
  (cell-layout g-object)
  (cell g-object)
  (expand :boolean))

(defun gtk-cell-layout-pack-start (cell-layout cell &key (expand t))
  (%gtk-cell-layout-pack-start cell-layout cell expand))

(export 'gtk-cell-layout-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_pack_end ()
;;; 
;;; void gtk_cell_layout_pack_end (GtkCellLayout *cell_layout,
;;;                                GtkCellRenderer *cell,
;;;                                gboolean expand);
;;; 
;;; Adds the cell to the end of cell_layout. If expand is FALSE, then the cell
;;; is allocated no more space than it needs. Any unused space is divided evenly
;;; between cells for which expand is TRUE.
;;; 
;;; Note that reusing the same cell renderer is not supported.
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; cell :
;;;     a GtkCellRenderer
;;; 
;;; expand :
;;;     TRUE if cell is to be given extra space allocated to cell_layout
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_pack_end" %gtk-cell-layout-pack-end) :void
  (cell-layout g-object)
  (cell g-object)
  (expand :boolean))

(defun gtk-cell-layout-pack-end (cell-layout cell &key (expand t))
  (%gtk-cell-layout-pack-end cell-layout cell expand))

(export 'gtk-cell-layout-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_get_area ()
;;; 
;;; GtkCellArea * gtk_cell_layout_get_area (GtkCellLayout *cell_layout);
;;; 
;;; Returns the underlying GtkCellArea which might be cell_layout if called on
;;; a GtkCellArea or might be NULL if no GtkCellArea is used by cell_layout.
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; Returns :
;;;     the cell area used by cell_layout. [transfer none]
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_get_cells ()
;;; 
;;; GList * gtk_cell_layout_get_cells (GtkCellLayout *cell_layout);
;;; 
;;; Returns the cell renderers which have been added to cell_layout.
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; Returns :
;;;     a list of cell renderers. The list, but not the renderers has been
;;;     newly allocated and should be freed with g_list_free() when no longer
;;;     needed.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_get_cells" gtk-cell-layout-cells)
    (g-list g-object :free-from-foreign t)
  (cell-layout g-object))

(export 'gtk-cell-layout-cells)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_reorder ()
;;; 
;;; void gtk_cell_layout_reorder (GtkCellLayout *cell_layout,
;;;                               GtkCellRenderer *cell,
;;;                               gint position);
;;; 
;;; Re-inserts cell at position.
;;; 
;;; Note that cell has already to be packed into cell_layout for this to
;;; function properly.
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; cell :
;;;     a GtkCellRenderer to reorder
;;; 
;;; position :
;;;     new position to insert cell at
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_reorder" gtk-cell-layout-reorder) :void
  (cell-layout g-object)
  (cell g-object)
  (positin :int))

(export 'gtk-cell-layout-reorder)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_clear ()
;;; 
;;; void gtk_cell_layout_clear (GtkCellLayout *cell_layout);
;;; 
;;; Unsets all the mappings on all renderers on cell_layout and removes all
;;; renderers from cell_layout.
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_clear" gtk-cell-layout-clear) :void
  (cell-layout g-object))

(export 'gtk-cell-layout-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_set_attributes ()
;;; 
;;; void gtk_cell_layout_set_attributes (GtkCellLayout *cell_layout,
;;;                                      GtkCellRenderer *cell,
;;;                                      ...);
;;; 
;;; Sets the attributes in list as the attributes of cell_layout.
;;; 
;;; The attributes should be in attribute/column order, as in
;;; gtk_cell_layout_add_attribute(). All existing attributes are removed, and
;;; replaced with the new attributes.
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; cell :
;;;     a GtkCellRenderer
;;; 
;;; ... :
;;;     a NULL-terminated list of attributes
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_add_attribute ()
;;; 
;;; void gtk_cell_layout_add_attribute (GtkCellLayout *cell_layout,
;;;                                     GtkCellRenderer *cell,
;;;                                     const gchar *attribute,
;;;                                     gint column);
;;; 
;;; Adds an attribute mapping to the list in cell_layout.
;;; 
;;; The column is the column of the model to get a value from, and the attribute
;;; is the parameter on cell to be set from the value. So for example if column
;;; 2 of the model contains strings, you could have the "text" attribute of a
;;; GtkCellRendererText get its values from column 2.
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; cell :
;;;     a GtkCellRenderer
;;; 
;;; attribute :
;;;     an attribute on the renderer
;;; 
;;; column :
;;;     the column position on the model to get the attribute from
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_add_attribute" gtk-cell-layout-add-attribute) :void
  (cell-layout g-object)
  (cell g-object)
  (attribute (:string :free-to-foreign t))
  (column :int))

(export 'gtk-cell-layout-add-attribute)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_set_cell_data_func ()
;;; 
;;; void gtk_cell_layout_set_cell_data_func (GtkCellLayout *cell_layout,
;;;                                          GtkCellRenderer *cell,
;;;                                          GtkCellLayoutDataFunc func,
;;;                                          gpointer func_data,
;;;                                          GDestroyNotify destroy);
;;; 
;;; Sets the GtkCellLayoutDataFunc to use for cell_layout.
;;; 
;;; This function is used instead of the standard attributes mapping for setting
;;; the column value, and should set the value of cell_layout's cell renderer(s)
;;; as appropriate.
;;; 
;;; func may be NULL to remove a previously set function.
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; cell :
;;;     a GtkCellRenderer
;;; 
;;; func :
;;;     the GtkCellLayoutDataFunc to use, or NULL. [allow-none]
;;; 
;;; func_data :
;;;     user data for func
;;; 
;;; destroy :
;;;     destroy notify for func_data
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcallback gtk-cell-layout-cell-data-func-callback :void
  ((cell-layout g-object)
   (cell g-object)
   (tree-model g-object)
   (iter (g-boxed-foreign gtk-tree-iter))
   (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               cell-layout cell tree-model iter)
    (return () nil)))

(defcfun ("gtk_cell_layout_set_cell_data_func"
          %gtk-cell-layout-set-cell-data-func) :void
  (cell-layout g-object)
  (cell g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-cell-layout-set-cell-data-func (cell-layout cell func)
  (%gtk-cell-layout-set-cell-data-func
                              cell-layout
                              cell
                              (callback gtk-cell-layout-cell-data-func-callback)
                              (allocate-stable-pointer func)
                              (callback stable-pointer-free-destroy-notify-cb)))

(export 'gtk-cell-layout-set-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_clear_attributes ()
;;; 
;;; void gtk_cell_layout_clear_attributes (GtkCellLayout *cell_layout,
;;;                                        GtkCellRenderer *cell);
;;; 
;;; Clears all existing attributes previously set with
;;; gtk_cell_layout_set_attributes().
;;; 
;;; cell_layout :
;;;     a GtkCellLayout
;;; 
;;; cell :
;;;     a GtkCellRenderer to clear the attribute mapping on
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_clear_attributes" gtk-cell-layout-clear-attributes)
     :void
  (cell-layout g-object)
  (cell g-object))

(export 'gtk-cell-layout-clear-attributes)

;;; --- End of file gtk.cell-layout.lisp ---------------------------------------
