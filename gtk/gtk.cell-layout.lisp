;;; ----------------------------------------------------------------------------
;;; gtk.cell-layout.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellLayout
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkCellLayout" gtk-cell-layout
  (:export t
   :type-initializer "gtk_cell_layout_get_type"))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-layout 'type)
 "@version{2013-6-21}
  @begin{short}
    @sym{gtk-cell-layout} is an interface to be implemented by all objects which
    want to provide a @class{gtk-tree-view-column}-like API for packing cells,
    setting attributes and data funcs.
  @end{short}

  One of the notable features provided by implementations of
  @sym{gtk-cell-layout} are attributes. Attributes let you set the properties in
  flexible ways. They can just be set to constant values like regular
  properties. But they can also be mapped to a column of the underlying tree
  model with the function @fun{gtk-cell-layout-set-attributes}, which means that
  the value of the attribute can change from cell to cell as they are rendered
  by the cell renderer. Finally, it is possible to specify a function with the
  function @fun{gtk-cell-layout-set-cell-data-func} that is called to determine
  the value of the attribute for each cell that is rendered.

  @subheading{GtkCellLayouts as GtkBuildable}
    Implementations of @sym{gtk-cell-layout} which also implement the
    @class{gtk-buildable} interface (@class{gtk-cell-view},
    @class{gtk-icon-view}, @class{gtk-combo-box}, @class{gtk-combo-box-entry},
    @class{gtk-entry-completion}, @class{gtk-tree-view-column}) accept
    @class{gtk-cell-renderer} objects as @code{<child>} elements in UI
    definitions. They support a custom @code{<attributes>} element for their
    children, which can contain multiple @code{<attribute>} elements. Each
    @code{<attribute>} element has a name attribute which specifies a property
    of the cell renderer; the content of the element is the attribute value.

    @b{Example:} A UI definition fragment specifying attributes
    @begin{pre}
 <object class=\"GtkCellView\">
   <child>
     <object class=\"GtkCellRendererText\"/>
     <attributes>
       <attribute name=\"text\">0</attribute>
     </attributes>
   </child>
 </object>
    @end{pre}
    Furthermore for implementations of @sym{gtk-cell-layout} that use a
    @class{gtk-cell-area} to lay out cells (all @sym{gtk-cell-layout}'s in GTK+
    use a @class{gtk-cell-area}) cell properties can also be defined in the
    format by specifying the custom @code{<cell-packing>} attribute which can
    contain multiple @code{<property>} elements defined in the normal way.

    @b{Example:} A UI definition fragment specifying cell properties
    @begin{pre}
 <object class=\"GtkTreeViewColumn\">
   <child>
     <object class=\"GtkCellRendererText\"/>
     <cell-packing>
       <property name=\"align\">True</property>
       <property name=\"expand\">False</property>
     </cell-packing>
   </child>
 </object>
    @end{pre}
  @subheading{Subclassing GtkCellLayout implementations}
    When subclassing a widget that implements @sym{gtk-cell-layout} like
    @class{gtk-icon-view} or @class{gtk-combo-box}, there are some
    considerations related to the fact that these widgets internally use a
    @class{gtk-cell-area}. The cell area is exposed as a construct-only property
    by these widgets. This means that it is possible to e. g. do
    @begin{pre}
 combo = g_object_new (GTK_TYPE_COMBO_BOX,
                       \"cell-area\", my_cell_area, NULL);
    @end{pre}
    to use a custom cell area with a combo box. But construct properties are
    only initialized after instance @code{init()} functions have run, which
    means that using functions which rely on the existence of the cell area in
    your subclass' @code{init()} function will cause the default cell area to be
    instantiated. In this case, a provided construct property value will be
    ignored (with a warning, to alert you to the problem).
    @begin{pre}
 static void
 my_combo_box_init (MyComboBox *b)
 {
   GtkCellRenderer *cell;

   cell = gtk_cell_renderer_pixbuf_new ();
   /* The following call causes the default cell area for combo boxes,
    * a GtkCellAreaBox, to be instantiated
    */
   gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (b), cell, FALSE);
   ...
 @}
 GtkWidget *
 my_combo_box_new (GtkCellArea *area)
 {
   /* This call is going to cause a warning
    * about area being ignored
    */
   return g_object_new (MY_TYPE_COMBO_BOX, \"cell-area\", area, NULL);
 @}
    @end{pre}
    If supporting alternative cell areas with your derived widget is not
    important, then this does not have to concern you. If you want to support
    alternative cell areas, you can do so by moving the problematic calls out of
    @code{init()} and into a @code{constructor()} for your class.")

;;; ----------------------------------------------------------------------------
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_pack_start" %gtk-cell-layout-pack-start) :void
  (cell-layout g-object)
  (cell g-object)
  (expand :boolean))

(defun gtk-cell-layout-pack-start (cell-layout cell &key (expand t))
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[cell-layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[expand]{@em{true} if @arg{cell} is to be given extra space allocated
    to @arg{cell-layout}}
  @begin{short}
    Packs the @arg{cell} into the beginning of @arg{cell-layout}.
  @end{short}
  If expand is @code{nil}, then the @arg{cell} is allocated no more space than
  it needs. Any unused space is divided evenly between cells for which expand is
  @em{true}.

  Note that reusing the same cell renderer is not supported.

  Since 2.4"
  (%gtk-cell-layout-pack-start cell-layout cell expand))

(export 'gtk-cell-layout-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_pack_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_pack_end" %gtk-cell-layout-pack-end) :void
  (cell-layout g-object)
  (cell g-object)
  (expand :boolean))

(defun gtk-cell-layout-pack-end (cell-layout cell &key (expand t))
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[cell-layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[expand]{@em{true} if @arg{cell} is to be given extra space allocated
    to @arg{cell-layout}}
  @begin{short}
    Adds the @arg{cell} to the end of @arg{cell-layout}. If expand is
    @code{nil}, then the cell is allocated no more space than it needs. Any
    unused space is divided evenly between cells for which @arg{expand} is
    @em{true}.
  @end{short}

  Note that reusing the same cell renderer is not supported.

  Since 2.4"
  (%gtk-cell-layout-pack-end cell-layout cell expand))

(export 'gtk-cell-layout-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_get_area ()
;;;
;;; GtkCellArea * gtk_cell_layout_get_area (GtkCellLayout *cell_layout);
;;;
;;; Returns the underlying GtkCellArea which might be cell_layout if called on a
;;; GtkCellArea or might be NULL if no GtkCellArea is used by cell_layout.
;;;
;;; cell_layout :
;;;     a GtkCellLayout
;;;
;;; Returns :
;;;     the cell area used by cell_layout
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_get_cells ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_get_cells" gtk-cell-layout-get-cells)
    (g-list g-object :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[cell-layout]{a @class{gtk-cell-layout} object}
  @return{A list of cell renderers.}
  @begin{short}
    Returns the cell renderers which have been added to @arg{cell-layout}.
  @end{short}

  Since 2.12"
  (cell-layout (g-object gtk-cell-layout)))

(export 'gtk-cell-layout-get-cells)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_reorder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_reorder" gtk-cell-layout-reorder) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[cell-layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object to reorder}
  @argument[position]{new position to insert @arg{cell} at}
  @begin{short}
    Re-inserts @arg{cell} at position.
  @end{short}

  Note that @arg{cell} has already to be packed into @arg{cell-layout} for this
  to function properly.

  Since 2.4"
  (cell-layout g-object)
  (cell g-object)
  (positin :int))

(export 'gtk-cell-layout-reorder)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_clear" gtk-cell-layout-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[cell-layout]{a @class{gtk-cell-layout} object}
  @begin{short}
    Unsets all the mappings on all renderers on @arg{cell-layout} and removes
    all renderers from @arg{cell-layout}.
  @end{short}

  Since 2.4"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_add_attribute" gtk-cell-layout-add-attribute) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[cell-layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[attribute]{an attribute on the renderer}
  @argument[column]{the column position on the model to get the attribute from}
  @begin{short}
    Adds an attribute mapping to the list in @arg{cell-layout}.
  @end{short}

  The @arg{column} is the column of the model to get a value from, and the
  attribute is the parameter on @arg{cell} to be set from the value. So for
  example if column 2 of the model contains strings, you could have the \"text\"
  attribute of a @class{gtk-cell-renderer-text} get its values from column 2.

  Since 2.4"
  (cell-layout g-object)
  (cell g-object)
  (attribute (:string :free-to-foreign t))
  (column :int))

(export 'gtk-cell-layout-add-attribute)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_set_cell_data_func ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-cell-layout-cell-data-func-callback :void
  ((cell-layout g-object)
   (cell g-object)
   (tree-model g-object)
   (iter (g-boxed-foreign gtk-tree-iter))
   (data :pointer))
  (restart-case
      (funcall (glib::get-stable-pointer-value data)
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
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[cell-layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[func]{the @code{GtkCellLayoutDataFunc} to use, or @code{nil}}
  @begin{short}
    Sets the @code{GtkCellLayoutDataFunc} to use for @arg{cell-layout}.
  @end{short}

  This function is used instead of the standard attributes mapping for setting
  the column value, and should set the value of @arg{cell-layout}'s cell
  renderer(s) as appropriate.

  @arg{func} may be @code{nil} to remove a previously set function.

  Since 2.4"
  (%gtk-cell-layout-set-cell-data-func
      cell-layout
      cell
      (callback gtk-cell-layout-cell-data-func-callback)
      (glib::allocate-stable-pointer func)
      (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gtk-cell-layout-set-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_clear_attributes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_clear_attributes" gtk-cell-layout-clear-attributes)
     :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[cell-layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} to clear the attribute mapping on}
  @begin{short}
    Clears all existing attributes previously set with the function
    @fun{gtk-cell-layout-set-attributes}.
  @end{short}

  Since 2.4
  @see-function{gtk-cell-layout-set-attributes}"
  (cell-layout g-object)
  (cell g-object))

(export 'gtk-cell-layout-clear-attributes)

;;; --- End of file gtk.cell-layout.lisp ---------------------------------------
