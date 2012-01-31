;;; ----------------------------------------------------------------------------
;;; gtk.tree-store.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
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
;;;
;;; GtkTreeStore
;;; 
;;; A tree-like data structure that can be used with the GtkTreeView
;;; 
;;; Synopsis
;;; 
;;;     GtkTreeStore
;;;
;;;     gtk_tree_store_new
;;;     gtk_tree_store_newv
;;;     gtk_tree_store_set_column_types
;;;     gtk_tree_store_set_value
;;;     gtk_tree_store_set
;;;     gtk_tree_store_set_valist
;;;     gtk_tree_store_set_valuesv
;;;     gtk_tree_store_remove
;;;     gtk_tree_store_insert
;;;     gtk_tree_store_insert_before
;;;     gtk_tree_store_insert_after
;;;     gtk_tree_store_insert_with_values
;;;     gtk_tree_store_insert_with_valuesv
;;;     gtk_tree_store_prepend
;;;     gtk_tree_store_append
;;;     gtk_tree_store_is_ancestor
;;;     gtk_tree_store_iter_depth
;;;     gtk_tree_store_clear
;;;     gtk_tree_store_iter_is_valid
;;;     gtk_tree_store_reorder
;;;     gtk_tree_store_swap
;;;     gtk_tree_store_move_before
;;;     gtk_tree_store_move_after
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkTreeStore
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkTreeStore implements GtkTreeModel, GtkTreeDragSource, GtkTreeDragDest,
;;; GtkTreeSortable and GtkBuildable.
;;;
;;; Description
;;; 
;;; The GtkTreeStore object is a list model for use with a GtkTreeView widget.
;;; It implements the GtkTreeModel interface, and consequentialy, can use all
;;; of the methods available there. It also implements the GtkTreeSortable
;;; interface so it can be sorted by the view. Finally, it also implements the
;;; tree drag and drop interfaces.
;;; 
;;; GtkTreeStore as GtkBuildable
;;; The GtkTreeStore implementation of the GtkBuildable interface allows to
;;; specify the model columns with a <columns> element that may contain multiple
;;; <column> elements, each specifying one model column. The "type" attribute
;;; specifies the data type for the column.
;;; 
;;; Example 73. A UI Definition fragment for a tree store
;;; 
;;; <object class="GtkTreeStore">
;;;   <columns>
;;;     <column type="gchararray"/>
;;;     <column type="gchararray"/>
;;;     <column type="gint"/>
;;;   </columns>
;;; </object>
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeStore
;;; 
;;; struct GtkTreeStore;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTreeStore" gtk-tree-store
  (:superclass g-object
   :export t
   :interfaces ("GtkBuildable" "GtkTreeDragDest" "GtkTreeDragSource"
                "GtkTreeModel" "GtkTreeSortable")
   :type-initializer "gtk_tree_store_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_new ()
;;; 
;;; GtkTreeStore * gtk_tree_store_new (gint n_columns, ...);
;;; 
;;; Creates a new tree store as with n_columns columns each of the types passed
;;; in. Note that only types derived from standard GObject fundamental types are
;;; supported.
;;; 
;;; As an example,
;;; gtk_tree_store_new (3, G_TYPE_INT, G_TYPE_STRING, GDK_TYPE_PIXBUF); will
;;; create a new GtkTreeStore with three columns, of type int, string and
;;; GdkPixbuf respectively.
;;; 
;;; n_columns :
;;;     number of columns in the tree store
;;; 
;;; ... :
;;;     all GType types for the columns, from first to last
;;; 
;;; Returns :
;;;     a new GtkTreeStore
;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :after ((store gtk-tree-store) &rest initargs
                                       &key (column-types
                                             nil
                                             column-types-supplied-p)
                                       &allow-other-keys)
  (declare (ignore initargs))
  (when column-types-supplied-p
    (gtk-tree-store-set-column-types store column-types)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_newv ()
;;; 
;;; GtkTreeStore * gtk_tree_store_newv (gint n_columns, GType *types);
;;; 
;;; Non vararg creation function. Used primarily by language bindings.
;;; 
;;; n_columns :
;;;     number of columns in the tree store
;;; 
;;; types :
;;;     an array of GType types for the columns, from first to last
;;; 
;;; Returns :
;;;     a new GtkTreeStore Rename to: gtk_tree_store_new
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set_column_types ()
;;; 
;;; void gtk_tree_store_set_column_types (GtkTreeStore *tree_store,
;;;                                       gint n_columns,
;;;                                       GType *types);
;;; 
;;; This function is meant primarily for GObjects that inherit from
;;; GtkTreeStore, and should only be used when constructing a new GtkTreeStore.
;;; It will not function after a row has been added, or a method on the
;;; GtkTreeModel interface is called.
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; n_columns :
;;;     Number of columns for the tree store
;;; 
;;; types :
;;;     An array of GType types, one for each column.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_set_column_types"
          %gtk-tree-store-set-column-types) :void
  (tree-store (g-object gtk-tree-store))
  (n-columns :int)
  (types :pointer))

(defun gtk-tree-store-set-column-types (tree-store column-types)
  (let ((n (length column-types)))
    (with-foreign-object (types-ar 'g-type-designator n)
      (iter (for i from 0 below n)
            (for type in column-types)
            (setf (mem-aref types-ar 'g-type-designator i) type))
      (%gtk-tree-store-set-column-types tree-store n types-ar))))

(export 'gtk-tree-store-set-column-types)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set_value ()
;;; 
;;; void gtk_tree_store_set_value (GtkTreeStore *tree_store,
;;;                                GtkTreeIter *iter,
;;;                                gint column,
;;;                                GValue *value);
;;; 
;;; Sets the data in the cell specified by iter and column. The type of value
;;; must be convertible to the type of the column.
;;; 
;;; tree_store :
;;;     a GtkTreeStore
;;; 
;;; iter :
;;;     A valid GtkTreeIter for the row being modified
;;; 
;;; column :
;;;     column number to modify
;;; 
;;; value :
;;;     new value for the cell
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_set_value" %gtk-tree-store-set-value) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (column :int)
  (value :pointer))

(defun gtk-tree-store-set-value (tree-store iter column value)
  (with-foreign-object (v 'g-value)
    (set-g-value v
                 value
                 (gtk-tree-model-column-type tree-store column)
                 :zero-g-value t)
    (%gtk-tree-store-set-value tree-store iter column v)
    (g-value-unset v)
    (values)))

(defun gtk-tree-store-value (tree-store iter column)
  (gtk-tree-model-value tree-store iter column))

(defun (setf gtk-tree-store-value) (new-value tree-store iter column)
  (gtk-tree-store-set-value tree-store iter column new-value)
  new-value)

(export 'gtk-tree-store-value)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set ()
;;; 
;;; void gtk_tree_store_set (GtkTreeStore *tree_store,
;;;                          GtkTreeIter *iter,
;;;                          ...);
;;; 
;;; Sets the value of one or more cells in the row referenced by iter. The
;;; variable argument list should contain integer column numbers, each column
;;; number followed by the value to be set. The list is terminated by a -1. For
;;; example, to set column 0 with type G_TYPE_STRING to "Foo", you would write
;;; gtk_tree_store_set (store, iter, 0, "Foo", -1).
;;; 
;;; The value will be referenced by the store if it is a G_TYPE_OBJECT, and it
;;; will be copied if it is a G_TYPE_STRING or G_TYPE_BOXED.
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     A valid GtkTreeIter for the row being modified
;;; 
;;; ... :
;;;     pairs of column number and value, terminated with -1
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set_valist ()
;;; 
;;; void gtk_tree_store_set_valist (GtkTreeStore *tree_store,
;;;                                 GtkTreeIter *iter,
;;;                                 va_list var_args);
;;; 
;;; See gtk_tree_store_set(); this version takes a va_list for use by language
;;; bindings.
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     A valid GtkTreeIter for the row being modified
;;; 
;;; var_args :
;;;     va_list of column/value pairs
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set_valuesv ()
;;; 
;;; void gtk_tree_store_set_valuesv (GtkTreeStore *tree_store,
;;;                                  GtkTreeIter *iter,
;;;                                  gint *columns,
;;;                                  GValue *values,
;;;                                  gint n_values);
;;; 
;;; A variant of gtk_tree_store_set_valist() which takes the columns and values
;;; as two arrays, instead of varargs. This function is mainly intended for
;;; language bindings or in case the number of columns to change is not known
;;; until run-time.
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     A valid GtkTreeIter for the row being modified
;;; 
;;; columns :
;;;     an array of column numbers
;;; 
;;; values :
;;;     an array of GValues
;;; 
;;; n_values :
;;;     the length of the columns and values arrays
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_remove ()
;;; 
;;; gboolean gtk_tree_store_remove (GtkTreeStore *tree_store,
;;;                                 GtkTreeIter *iter);
;;; 
;;; Removes iter from tree_store. After being removed, iter is set to the next
;;; valid row at that level, or invalidated if it previously pointed to the last
;;; one.
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     A valid GtkTreeIter
;;; 
;;; Returns :
;;;     TRUE if iter is still valid, FALSE if not.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_remove" gtk-tree-store-remove) :boolean
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert ()
;;; 
;;; void gtk_tree_store_insert (GtkTreeStore *tree_store,
;;;                             GtkTreeIter *iter,
;;;                             GtkTreeIter *parent,
;;;                             gint position);
;;; 
;;; Creates a new row at position. If parent is non-NULL, then the row will be
;;; made a child of parent. Otherwise, the row will be created at the toplevel.
;;; If position is larger than the number of rows at that level, then the new
;;; row will be inserted to the end of the list. iter will be changed to point
;;; to this new row. The row will be empty after this function is called. To
;;; fill in values, you need to call gtk_tree_store_set() or
;;; gtk_tree_store_set_value().
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     An unset GtkTreeIter to set to the new row.
;;; 
;;; parent :
;;;     A valid GtkTreeIter, or NULL.
;;; 
;;; position :
;;;     position to insert the new row
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_insert" %gtk-tree-store-insert) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (position :int))

(defun gtk-tree-store-insert (tree-store parent position)
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-tree-store-insert tree-store iter parent position)
    iter))

(export 'gtk-tree-store-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_before ()
;;; 
;;; void gtk_tree_store_insert_before (GtkTreeStore *tree_store,
;;;                                    GtkTreeIter *iter,
;;;                                    GtkTreeIter *parent,
;;;                                    GtkTreeIter *sibling);
;;; 
;;; Inserts a new row before sibling. If sibling is NULL, then the row will be
;;; appended to parent 's children. If parent and sibling are NULL, then the
;;; row will be appended to the toplevel. If both sibling and parent are set,
;;; then parent must be the parent of sibling. When sibling is set, parent is
;;; optional.
;;; 
;;; iter will be changed to point to this new row. The row will be empty after
;;; this function is called. To fill in values, you need to call
;;; gtk_tree_store_set() or gtk_tree_store_set_value().
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     An unset GtkTreeIter to set to the new row.
;;; 
;;; parent :
;;;     A valid GtkTreeIter, or NULL.
;;; 
;;; sibling :
;;;     A valid GtkTreeIter, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_insert_before" %gtk-tree-store-insert-before) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (sibling (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-store-insert-before (tree-store parent sibling)
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-tree-store-insert-before tree-store iter parent sibling)
    iter))

(export 'gtk-tree-store-insert-before)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_after ()
;;; 
;;; void gtk_tree_store_insert_after (GtkTreeStore *tree_store,
;;;                                   GtkTreeIter *iter,
;;;                                   GtkTreeIter *parent,
;;;                                   GtkTreeIter *sibling);
;;; 
;;; Inserts a new row after sibling. If sibling is NULL, then the row will be
;;; prepended to parent 's children. If parent and sibling are NULL, then the
;;; row will be prepended to the toplevel. If both sibling and parent are set,
;;; then parent must be the parent of sibling. When sibling is set, parent is
;;; optional.
;;; 
;;; iter will be changed to point to this new row. The row will be empty after
;;; this function is called. To fill in values, you need to call
;;; gtk_tree_store_set() or gtk_tree_store_set_value().
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     An unset GtkTreeIter to set to the new row.
;;; 
;;; parent :
;;;     A valid GtkTreeIter, or NULL.
;;; 
;;; sibling :
;;;     A valid GtkTreeIter, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_insert_after" %gtk-tree-store-insert-after) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (sibling (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-store-insert-after (tree-store parent sibling)
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-tree-store-insert-after tree-store iter parent sibling)
    iter))

(export 'gtk-tree-store-insert-after)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_with_values ()
;;; 
;;; void gtk_tree_store_insert_with_values (GtkTreeStore *tree_store,
;;;                                         GtkTreeIter *iter,
;;;                                         GtkTreeIter *parent,
;;;                                         gint position,
;;;                                         ...);
;;; 
;;; Creates a new row at position. iter will be changed to point to this new
;;; row. If position is larger than the number of rows on the list, then the new
;;; row will be appended to the list. The row will be filled with the values
;;; given to this function.
;;; 
;;; Calling gtk_tree_store_insert_with_values (tree_store, iter, position, ...)
;;; has the same effect as calling
;;; 
;;; gtk_tree_store_insert (tree_store, iter, position);
;;; gtk_tree_store_set (tree_store, iter, ...);
;;; 
;;; with the different that the former will only emit a row_inserted signal,
;;; while the latter will emit row_inserted, row_changed and if the tree store
;;; is sorted, rows_reordered. Since emitting the rows_reordered signal
;;; repeatedly can affect the performance of the program,
;;; gtk_tree_store_insert_with_values() should generally be preferred when
;;; inserting rows in a sorted tree store.
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     An unset GtkTreeIter to set the new row, or NULL.
;;; 
;;; parent :
;;;     A valid GtkTreeIter, or NULL.
;;; 
;;; position :
;;;     position to insert the new row
;;; 
;;; ... :
;;;     pairs of column number and value, terminated with -1
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_with_valuesv ()
;;; 
;;; void gtk_tree_store_insert_with_valuesv (GtkTreeStore *tree_store,
;;;                                          GtkTreeIter *iter,
;;;                                          GtkTreeIter *parent,
;;;                                          gint position,
;;;                                          gint *columns,
;;;                                          GValue *values,
;;;                                          gint n_values);
;;; 
;;; A variant of gtk_tree_store_insert_with_values() which takes the columns and
;;; values as two arrays, instead of varargs. This function is mainly intended
;;; for language bindings.
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     An unset GtkTreeIter to set the new row, or NULL.
;;; 
;;; parent :
;;;     A valid GtkTreeIter, or NULL.
;;; 
;;; position :
;;;     position to insert the new row
;;; 
;;; columns :
;;;     an array of column numbers
;;; 
;;; values :
;;;     an array of GValues
;;; 
;;; n_values :
;;;     the length of the columns and values arrays
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_insert_with_valuesv"
          %gtk-tree-store-insert-with-valuesv) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (position :int)
  (columns :pointer)
  (values :pointer)
  (n-values :int))

(defun gtk-tree-store-insert-with-values (tree-store parent position
                                                     &rest values)
  (let ((n (length values))
        (iter (make-gtk-tree-iter)))
    (with-foreign-objects ((v-ar 'g-value n)
                           (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for type = (gtk-tree-model-column-type tree-store i))
            (setf (mem-aref columns-ar :int i) i)
            (set-g-value (mem-aref v-ar 'g-value i) value type :zero-g-value t))
      (%gtk-tree-store-insert-with-valuesv tree-store
                                           iter
                                           parent
                                           position
                                           columns-ar
                                           v-ar
                                           n)
      (iter (for i from 0 below n)
            (g-value-unset (mem-aref v-ar 'g-value i)))
      iter)))

(export 'gtk-tree-store-insert-with-values)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_prepend ()
;;; 
;;; void gtk_tree_store_prepend (GtkTreeStore *tree_store,
;;;                              GtkTreeIter *iter,
;;;                              GtkTreeIter *parent);
;;; 
;;; Prepends a new row to tree_store. If parent is non-NULL, then it will
;;; prepend the new row before the first child of parent, otherwise it will
;;; prepend a row to the top level. iter will be changed to point to this new
;;; row. The row will be empty after this function is called. To fill in values,
;;; you need to call gtk_tree_store_set() or gtk_tree_store_set_value().
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     An unset GtkTreeIter to set to the prepended row.
;;; 
;;; parent :
;;;     A valid GtkTreeIter, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_prepend" %gtk-tree-store-prepend) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter)))

(defun tree-store-prepend (tree-store parent)
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-tree-store-prepend tree-store iter parent)
    iter))

(export 'gtk-tree-store-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_append ()
;;; 
;;; void gtk_tree_store_append (GtkTreeStore *tree_store,
;;;                             GtkTreeIter *iter,
;;;                             GtkTreeIter *parent);
;;; 
;;; Appends a new row to tree_store. If parent is non-NULL, then it will append
;;; the new row after the last child of parent, otherwise it will append a row
;;; to the top level. iter will be changed to point to this new row. The row
;;; will be empty after this function is called. To fill in values, you need to
;;; call gtk_tree_store_set() or gtk_tree_store_set_value().
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     An unset GtkTreeIter to set to the appended row.
;;; 
;;; parent :
;;;     A valid GtkTreeIter, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_append" %gtk-tree-store-append) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-store-append (tree-store parent)
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-tree-store-append tree-store iter parent)
    iter))

(export 'gtk-tree-store-append)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_is_ancestor ()
;;; 
;;; gboolean gtk_tree_store_is_ancestor (GtkTreeStore *tree_store,
;;;                                      GtkTreeIter *iter,
;;;                                      GtkTreeIter *descendant);
;;; 
;;; Returns TRUE if iter is an ancestor of descendant. That is, iter is the
;;; parent (or grandparent or great-grandparent) of descendant.
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     A valid GtkTreeIter
;;; 
;;; descendant :
;;;     A valid GtkTreeIter
;;; 
;;; Returns :
;;;     TRUE, if iter is an ancestor of descendant
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_is_ancestor" gtk-tree-store-is-ancestor) :boolean
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (descendant (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-is-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_iter_depth ()
;;; 
;;; gint gtk_tree_store_iter_depth (GtkTreeStore *tree_store,
;;;                                 GtkTreeIter *iter);
;;; 
;;; Returns the depth of iter. This will be 0 for anything on the root level,
;;; 1 for anything down a level, etc.
;;; 
;;; tree_store :
;;;     A GtkTreeStore
;;; 
;;; iter :
;;;     A valid GtkTreeIter
;;; 
;;; Returns :
;;;     The depth of iter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_iter_depth" gtk-tree-store-iter-depth) :int
  (tree-store (g-object gtk-tree-store))
  (tree-iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-iter-depth)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_clear ()
;;; 
;;; void gtk_tree_store_clear (GtkTreeStore *tree_store);
;;; 
;;; Removes all rows from tree_store
;;; 
;;; tree_store :
;;;     a GtkTreeStore
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_clear" gtk-tree-store-clear) :void
  (tree-store (g-object gtk-tree-store)))

(export 'gtk-tree-store-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_iter_is_valid ()
;;; 
;;; gboolean gtk_tree_store_iter_is_valid (GtkTreeStore *tree_store,
;;;                                        GtkTreeIter *iter);
;;; 
;;; WARNING: This function is slow. Only use it for debugging and/or testing
;;; purposes.
;;; 
;;; Checks if the given iter is a valid iter for this GtkTreeStore.
;;; 
;;; tree_store :
;;;     A GtkTreeStore.
;;; 
;;; iter :
;;;     A GtkTreeIter.
;;; 
;;; Returns :
;;;     TRUE if the iter is valid, FALSE if the iter is invalid.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_iter_is_valid" gtk-tree-store-iter-is-valid) :boolean
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-iter-is-valid)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_reorder ()
;;; 
;;; void gtk_tree_store_reorder (GtkTreeStore *tree_store,
;;;                              GtkTreeIter *parent,
;;;                              gint *new_order);
;;; 
;;; Reorders the children of parent in tree_store to follow the order indicated
;;; by new_order. Note that this function only works with unsorted stores.
;;; 
;;; tree_store :
;;;     A GtkTreeStore.
;;; 
;;; parent :
;;;     A GtkTreeIter.
;;; 
;;; new_order :
;;;     an array of integers mapping the new position of each child to its old
;;;     position before the re-ordering, i.e. new_order[newpos] = oldpos.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_swap ()
;;; 
;;; void gtk_tree_store_swap (GtkTreeStore *tree_store,
;;;                           GtkTreeIter *a,
;;;                           GtkTreeIter *b);
;;; 
;;; Swaps a and b in the same level of tree_store. Note that this function only
;;; works with unsorted stores.
;;; 
;;; tree_store :
;;;     A GtkTreeStore.
;;; 
;;; a :
;;;     A GtkTreeIter.
;;; 
;;; b :
;;;     Another GtkTreeIter.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_swap" gtk-tree-store-swap) :void
  (tree-store (g-object gtk-tree-store))
  (a (g-boxed-foreign gtk-tree-iter))
  (b (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-swap)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_move_before ()
;;; 
;;; void gtk_tree_store_move_before (GtkTreeStore *tree_store,
;;;                                  GtkTreeIter *iter,
;;;                                  GtkTreeIter *position);
;;; 
;;; Moves iter in tree_store to the position before position. iter and position
;;; should be in the same level. Note that this function only works with
;;; unsorted stores. If position is NULL, iter will be moved to the end of the
;;; level.
;;; 
;;; tree_store :
;;;     A GtkTreeStore.
;;; 
;;; iter :
;;;     A GtkTreeIter.
;;; 
;;; position :
;;;     A GtkTreeIter or NULL.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_move_before" gtk-tree-store-move-before) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (position (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-move-before)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_move_after ()
;;; 
;;; void gtk_tree_store_move_after (GtkTreeStore *tree_store,
;;;                                 GtkTreeIter *iter,
;;;                                 GtkTreeIter *position);
;;; 
;;; Moves iter in tree_store to the position after position. iter and position
;;; should be in the same level. Note that this function only works with
;;; unsorted stores. If position is NULL, iter will be moved to the start of
;;; the level.
;;; 
;;; tree_store :
;;;     A GtkTreeStore.
;;; 
;;; iter :
;;;     A GtkTreeIter.
;;; 
;;; position :
;;;     A GtkTreeIter.
;;; 
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_move_after" gtk-tree-store-move-after) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (position (g-boxed-foreign gtk-tree-iter)))

(export 'tree-store-move-after)

;;; --- End of file gtk.tree-store.lisp ----------------------------------------
