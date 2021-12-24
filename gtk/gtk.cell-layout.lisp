;;; ----------------------------------------------------------------------------
;;; gtk.cell-layout.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     An interface for packing cells
;;;
;;; Types and Values
;;;
;;;     GtkCellLayout
;;;
;;; Functions
;;;
;;;     GtkCellLayoutDataFunc
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
;;;     GInterface
;;;     ╰── GtkCellLayout
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
 "@version{2021-3-13}
  @begin{short}
    The @sym{gtk-cell-layout} interface is an interface to be implemented by
    all objects which want to provide a @class{gtk-tree-view-column} object
    like API for packing cells, setting attributes and data functions.
  @end{short}

  One of the notable features provided by implementations of the
  @sym{gtk-cell-layout} interface are attributes. Attributes let you set the
  properties in flexible ways. They can just be set to constant values like
  regular properties. But they can also be mapped to a column of the underlying
  tree model with the function @fun{gtk-cell-layout-add-attribute}, which means
  that the value of the attribute can change from cell to cell as they are
  rendered by the cell renderer. Finally, it is possible to specify a function
  with the function @fun{gtk-cell-layout-set-cell-data-func} that is called to
  determine the value of the attribute for each cell that is rendered.
  @begin[GtkCellLayouts as GtkBuildable]{dictionary}
    Implementations of the @sym{gtk-cell-layout} interface which also implement
    the @class{gtk-buildable} interface accept @class{gtk-cell-renderer}
    objects as @code{<child>} elements in UI definitions. They support a custom
    @code{<attributes>} element for their children, which can contain multiple
    @code{<attribute>} elements. Each @code{<attribute>} element has a name
    attribute which specifies a property of the cell renderer. The content of
    the element is the attribute value.

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
    Furthermore for implementations of the @sym{gtk-cell-layout} interface that
    use a @class{gtk-cell-area} object to lay out cells, all
    @sym{gtk-cell-layout} objects in GTK+ use a @class{gtk-cell-area} object,
    cell properties can also be defined in the format by specifying the custom
    @code{<cell-packing>} attribute which can contain multiple @code{<property>}
    elements defined in the normal way.

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
  @end{dictionary}
  @see-class{gtk-cell-area}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_pack_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_pack_start" %gtk-cell-layout-pack-start) :void
  (layout (g-object gtk-cell-layout))
  (cell (g-object gtk-cell-renderer))
  (expand :boolean))

(defun gtk-cell-layout-pack-start (layout cell &key (expand t))
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-13}
  @argument[layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[expand]{@em{true} if @arg{cell} is to be given extra space
    allocated to @arg{layout}}
  @begin{short}
    Packs @arg{cell} into the beginning of @arg{layout}.
  @end{short}
  If the keyword argument @arg{expand} is @em{false}, then @arg{cell} is
  allocated no more space than it needs. Any unused space is divided evenly
  between cells for which @arg{expand} is @em{true}. The default value of the
  keyword argument @arg{expand} is @em{true}.

  Note that reusing the same cell renderer is not supported.
  @see-class{gtk-cell-layout}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-layout-pack-end}"
  (%gtk-cell-layout-pack-start layout cell expand))

(export 'gtk-cell-layout-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_pack_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_pack_end" %gtk-cell-layout-pack-end) :void
  (layout (g-object gtk-cell-layout))
  (cell (g-object gtk-cell-renderer))
  (expand :boolean))

(defun gtk-cell-layout-pack-end (layout cell &key (expand t))
 #+cl-cffi-gtk-documentation
 "@version{2021-3-13}
  @argument[layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[expand]{@em{true} if @arg{cell} is to be given extra space
    allocated to @arg{layout}}
  @begin{short}
    Adds @arg{cell} to the end of @arg{layout}.
  @end{short}
  If the keyword argument @arg{expand} is @em{false}, then @arg{cell} is
  allocated no more space than it needs. Any unused space is divided evenly
  between cells for which @arg{expand} is @em{true}. The default value of the
  keyword argument @arg{expand} is @em{true}.

  Note that reusing the same cell renderer is not supported.
  @see-class{gtk-cell-layout}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-layout-pack-start}"
  (%gtk-cell-layout-pack-end layout cell expand))

(export 'gtk-cell-layout-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_get_area () -> gtk-cell-layout-area
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_get_area" gtk-cell-layout-area)
    (g-object gtk-cell-area)
 "@version{2021-3-13}
  @argument[layout]{a @class{gtk-cell-layout} object}
  @return{The @class{gtk-cell-area} object used by @arg{layout}.}
  @begin{short}
    Returns the underlying cell area which might be @arg{layout} if called
    on a @class{gtk-cell-area} object or might be @code{nil} if no cell area is
    used by @arg{layout}.
  @end{short}
  @see-class{gtk-cell-layout}
  @see-class{gtk-cell-area}"
  (layout (g-object gtk-cell-layout)))

(export 'gtk-cell-layout-area)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_get_cells () -> gtk-cell-layout-cells
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_get_cells" gtk-cell-layout-cells)
    (g-list g-object :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-13}
  @argument[layout]{a @class{gtk-cell-layout} object}
  @return{A list of @class{gtk-cell-renderer} objects.}
  @begin{short}
    Returns the cell renderers which have been added to the cell layout.
  @end{short}
  @see-class{gtk-cell-layout}
  @see-class{gtk-cell-renderer}"
  (layout (g-object gtk-cell-layout)))

(export 'gtk-cell-layout-cells)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_reorder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_reorder" gtk-cell-layout-reorder) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-13}
  @argument[layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object to reorder}
  @argument[position]{an integer with the new position to insert @arg{cell} at}
  @begin{short}
    Reinserts @arg{cell} at the given position.
  @end{short}

  Note that @arg{cell} has already to be packed into @arg{layout} for this
  to function properly.
  @see-class{gtk-cell-layout}
  @see-class{gtk-cell-renderer}"
  (layout (g-object gtk-cell-layout))
  (cell (g-object gtk-cell-renderer))
  (position :int))

(export 'gtk-cell-layout-reorder)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_clear" gtk-cell-layout-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-13}
  @argument[layout]{a @class{gtk-cell-layout} object}
  @begin{short}
    Unsets all the mappings on all renderers on @arg{layout} and removes
    all renderers from @arg{layout}.
  @end{short}
  @see-class{gtk-cell-layout}"
  (layout (g-object gtk-cell-layout)))

(export 'gtk-cell-layout-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_set_attributes ()
;;; ----------------------------------------------------------------------------

(defun gtk-cell-layout-set-attributes (layout cell &rest attributes)
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-13}
  @argument[layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[attributes]{a list of attributes}
  @begin{short}
    Sets the attributes in the list as the attributes of @arg{layout}.
  @end{short}

  The attributes should be in attribute/column order, as in the function
  @fun{gtk-cell-layout-add-attribute}. All existing attributes are removed,
  and replaced with the new attributes.
  @see-class{gtk-layout}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-layout-add-attribute}"
  (loop for (attribute column) on attributes by #'cddr
        do (gtk-cell-layout-add-attribute layout cell attribute column)))

(export 'gtk-cell-layout-set-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_add_attribute ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_add_attribute" gtk-cell-layout-add-attribute) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-13}
  @argument[layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[attribute]{a string with an attribute on the renderer}
  @argument[column]{an integer with the column position on the model to get the
    attribute from}
  @begin{short}
    Adds an attribute mapping to the list in @arg{layout}.
  @end{short}

  The argument @arg{column} is the column of the model to get a value from, and
  @arg{attribute} is the parameter on @arg{cell} to be set from the value. So
  for example if column 2 of the model contains strings, you could have the
  \"text\" attribute of a @class{gtk-cell-renderer-text} object get its values
  from column 2.
  @see-class{gtk-cell-layout}
  @see-class{gtk-cell-renderer}
  @see-class{gtk-cell-renderer-text}"
  (layout (g-object gtk-cell-layout))
  (cell (g-object gtk-cell-renderer))
  (attribute (:string :free-to-foreign t))
  (column :int))

(export 'gtk-cell-layout-add-attribute)

;;; ----------------------------------------------------------------------------
;;; GtkCellLayoutDataFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-cell-layout-cell-data-func :void
  ((layout (g-object gtk-cell-layout))
   (cell (g-object gtk-cell-renderer))
   (model (g-object gtk-tree-model))
   (iter (g-boxed-foreign gtk-tree-iter))
   (data :pointer))
  (restart-case
    (funcall (get-stable-pointer-value data) layout cell model iter)
    (return () nil)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-layout-cell-data-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-cell-layout-cell-data-func atdoc:*external-symbols*)
 "@version{2021-3-13}
  @begin{short}
    A callback function which should set the value of @arg{layout}'s cell
   renderer(s) as appropriate.
  @end{short}
  @begin{pre}
 lambda (layout cell model iter)
  @end{pre}
  @begin[code]{table}
    @entry[layout]{A @class{gtk-cell-layout} object.}
    @entry[cell]{The @class{gtk-cell-renderer} object whose value is to be set.}
    @entry[model]{The @class{gtk-tree-model} object.}
    @entry[iter]{A @class{gtk-tree-iterator} iterator indicating the row to set
      the value for.}
  @end{table}
  @see-class{gtk-tree-view-column}
  @see-function{gtk-tree-view-column-set-cell-data-func}")

(export 'gtk-cell-layout-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_set_cell_data_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_set_cell_data_func"
          %gtk-cell-layout-set-cell-data-func) :void
  (layout (g-object gtk-cell-layout))
  (cell (g-object gtk-cell-renderer))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-cell-layout-set-cell-data-func (layout cell func)
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-13}
  @argument[layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object}
  @argument[func]{the @symbol{gtk-cell-layout-data-func} to use, or @code{nil}}
  @begin{short}
    Sets the callback function to use for @arg{layout}.
  @end{short}
  This function is used instead of the standard attributes mapping for setting
  the column value, and should set the value of @arg{layout}'s cell renderer(s)
  as appropriate.

  The callback function @arg{func} may be @code{nil} to remove a previously set
  function.
  @see-class{gtk-cell-layout}
  @see-class{gtk-cell-renderer}
  @see-symbol{gtk-cell-layout-cell-data-func}"
  (%gtk-cell-layout-set-cell-data-func
              layout
              cell
              (callback gtk-cell-layout-cell-data-func)
              (allocate-stable-pointer func)
              (callback stable-pointer-destroy-notify)))

(export 'gtk-cell-layout-set-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_clear_attributes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_layout_clear_attributes" gtk-cell-layout-clear-attributes)
     :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-13}
  @argument[layout]{a @class{gtk-cell-layout} object}
  @argument[cell]{a @class{gtk-cell-renderer} object to clear the attribute
    mapping on}
  @begin{short}
    Clears all existing attributes previously set with the function
    @fun{gtk-cell-layout-add-attribute}.
  @end{short}
  @see-class{gtk-cell-layout}
  @see-class{gtk-cell-renderer}
  @see-function{gtk-cell-layout-add-attribute}"
  (layout (g-object gtk-cell-layout))
  (cell (g-object gtk-cell-renderer)))

(export 'gtk-cell-layout-clear-attributes)

;;; --- End of file gtk.cell-layout.lisp ---------------------------------------
