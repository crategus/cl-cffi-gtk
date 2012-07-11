;;; ----------------------------------------------------------------------------
;;; gtk.list-store.lisp
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
;;; GtkListStore
;;;
;;; A list-like data structure that can be used with the GtkTreeView
;;;
;;; Synopsis
;;;
;;;     GtkListStore
;;;
;;;     gtk_list_store_new
;;;     gtk_list_store_newv
;;;     gtk_list_store_set_column_types
;;;     gtk_list_store_set
;;;     gtk_list_store_set_valist
;;;     gtk_list_store_set_value
;;;     gtk_list_store_set_valuesv
;;;     gtk_list_store_remove
;;;     gtk_list_store_insert
;;;     gtk_list_store_insert_before
;;;     gtk_list_store_insert_after
;;;     gtk_list_store_insert_with_values
;;;     gtk_list_store_insert_with_valuesv
;;;     gtk_list_store_prepend
;;;     gtk_list_store_append
;;;     gtk_list_store_clear
;;;     gtk_list_store_iter_is_valid
;;;     gtk_list_store_reorder
;;;     gtk_list_store_swap
;;;     gtk_list_store_move_before
;;;     gtk_list_store_move_after
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkListStore
;;;
;;; Implemented Interfaces
;;;
;;; GtkListStore implements GtkTreeModel, GtkTreeDragSource, GtkTreeDragDest,
;;; GtkTreeSortable and GtkBuildable.
;;;
;;; Description
;;;
;;; The GtkListStore object is a list model for use with a GtkTreeView widget.
;;; It implements the GtkTreeModel interface, and consequentialy, can use all of
;;; the methods available there. It also implements the GtkTreeSortable
;;; interface so it can be sorted by the view. Finally, it also implements the
;;; tree drag and drop interfaces.
;;;
;;; The GtkListStore can accept most GObject types as a column type, though it
;;; can't accept all custom types. Internally, it will keep a copy of data
;;; passed in (such as a string or a boxed pointer). Columns that accept
;;; GObjects are handled a little differently. The GtkListStore will keep a
;;; reference to the object instead of copying the value. As a result, if the
;;; object is modified, it is up to the application writer to call
;;; gtk_tree_model_row_changed() to emit the "row_changed" signal. This most
;;; commonly affects lists with GdkPixbufs stored.
;;;
;;; Example 71. Creating a simple list store.
;;;
;;;   enum {
;;;     COLUMN_STRING,
;;;     COLUMN_INT,
;;;     COLUMN_BOOLEAN,
;;;     N_COLUMNS
;;;   };
;;;
;;;   {
;;;     GtkListStore *list_store;
;;;     GtkTreePath *path;
;;;     GtkTreeIter iter;
;;;     gint i;
;;;
;;;     list_store = gtk_list_store_new (N_COLUMNS,
;;;                                      G_TYPE_STRING,
;;;                                      G_TYPE_INT,
;;;                                      G_TYPE_BOOLEAN);
;;;
;;;     for (i = 0; i < 10; i++)
;;;       {
;;;         gchar *some_data;
;;;
;;;         some_data = get_some_data (i);
;;;
;;;         // Add a new row to the model
;;;         gtk_list_store_append (list_store, &iter);
;;;         gtk_list_store_set (list_store, &iter,
;;;                             COLUMN_STRING, some_data,
;;;                             COLUMN_INT, i,
;;;                             COLUMN_BOOLEAN,  FALSE,
;;;                             -1);
;;;
;;;         /* As the store will keep a copy of the string internally, we
;;;          * free some_data.
;;;          */
;;;         g_free (some_data);
;;;       }
;;;
;;;     // Modify a particular row
;;;     path = gtk_tree_path_new_from_string ("4");
;;;     gtk_tree_model_get_iter (GTK_TREE_MODEL (list_store),
;;;                              &iter,
;;;                              path);
;;;     gtk_tree_path_free (path);
;;;     gtk_list_store_set (list_store, &iter,
;;;                         COLUMN_BOOLEAN, TRUE,
;;;                         -1);
;;;   }
;;;
;;;
;;; Performance Considerations
;;;
;;; Internally, the GtkListStore was implemented with a linked list with a tail
;;; pointer prior to GTK+ 2.6. As a result, it was fast at data insertion and
;;; deletion, and not fast at random data access. The GtkListStore sets the
;;; GTK_TREE_MODEL_ITERS_PERSIST flag, which means that GtkTreeIters can be
;;; cached while the row exists. Thus, if access to a particular row is needed
;;; often and your code is expected to run on older versions of GTK+, it is
;;; worth keeping the iter around.
;;;
;;; Atomic Operations
;;;
;;; It is important to note that only the methods
;;; gtk_list_store_insert_with_values() and gtk_list_store_insert_with_valuesv()
;;; are atomic, in the sense that the row is being appended to the store and the
;;; values filled in in a single operation with regard to GtkTreeModel
;;; signaling. In contrast, using e.g. gtk_list_store_append() and then
;;; gtk_list_store_set() will first create a row, which triggers the
;;; "row-inserted" signal on GtkListStore. The row, however, is still empty, and
;;; any signal handler connecting to "row-inserted" on this particular store
;;; should be prepared for the situation that the row might be empty. This is
;;; especially important if you are wrapping the GtkListStore inside a
;;; GtkTreeModelFilter and are using a GtkTreeModelFilterVisibleFunc. Using any
;;; of the non-atomic operations to append rows to the GtkListStore will cause
;;; the GtkTreeModelFilterVisibleFunc to be visited with an empty row first; the
;;; function must be prepared for that.
;;;
;;; GtkListStore as GtkBuildable
;;;
;;; The GtkListStore implementation of the GtkBuildable interface allows to
;;; specify the model columns with a <columns> element that may contain multiple
;;; <column> elements, each specifying one model column. The "type" attribute
;;; specifies the data type for the column.
;;;
;;; Additionally, it is possible to specify content for the list store in the UI
;;; definition, with the <data> element. It can contain multiple <row> elements,
;;; each specifying to content for one row of the list model. Inside a <row>,
;;; the <col> elements specify the content for individual cells.
;;;
;;; Note that it is probably more common to define your models in the code, and
;;; one might consider it a layering violation to specify the content of a list
;;; store in a UI definition, data, not presentation, and common wisdom is to
;;; separate the two, as far as possible.
;;;
;;; Example 72. A UI Definition fragment for a list store
;;;
;;;   <object class="GtkListStore">
;;;     <columns>
;;;       <column type="gchararray"/>
;;;       <column type="gchararray"/>
;;;       <column type="gint"/>
;;;     </columns>
;;;     <data>
;;;       <row>
;;;         <col id="0">John</col>
;;;         <col id="1">Doe</col>
;;;         <col id="2">25</col>
;;;       </row>
;;;       <row>
;;;         <col id="0">Johan</col>
;;;         <col id="1">Dahlin</col>
;;;         <col id="2">50</col>
;;;       </row>
;;;     </data>
;;;   </object>
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkListStore
;;;
;;; struct GtkListStore;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkListStore" gtk-list-store
  (:superclass g-object
   :export t
   :interfaces ("GtkBuildable"
                "GtkTreeDragDest"
                "GtkTreeDragSource"
                "GtkTreeModel"
                "GtkTreeSortable")
   :type-initializer "gtk_list_store_get_type")
  nil)

;;; ----------------------------------------------------------------------------

(defun call-list-store-set-column-types (list-store column-types)
  (let ((n (length column-types)))
    (with-foreign-object (types-ar 'g-type-designator n)
      (iter (for i from 0 below n)
            (for type in column-types)
            (setf (mem-aref types-ar 'g-type-designator i) type))
      (gtk-list-store-set-column-types list-store n types-ar))))

(defmethod initialize-instance :after ((list-store gtk-list-store)
                                       &rest initargs
                                       &key (column-types
                                             nil
                                             column-types-p)
                                       &allow-other-keys)
  (declare (ignore initargs))
  (when column-types-p
    (call-list-store-set-column-types list-store column-types)))

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_new ()
;;;
;;; GtkListStore * gtk_list_store_new (gint n_columns, ...);
;;;
;;; Creates a new list store as with n_columns columns each of the types passed
;;; in. Note that only types derived from standard GObject fundamental types are
;;; supported.
;;;
;;; As an example, gtk_tree_store_new (3, G_TYPE_INT, G_TYPE_STRING,
;;; GDK_TYPE_PIXBUF); will create a new GtkListStore with three columns, of type
;;; int, string and GdkPixbuf respectively.
;;;
;;; n_columns :
;;;     number of columns in the list store
;;;
;;; ... :
;;;     all GType types for the columns, from first to last
;;;
;;; Returns :
;;;     a new GtkListStore
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-list-store-new))

(defun gtk-list-store-new (&rest column-types)
  (make-instance 'gtk-list-store
                 :colums-types column-types))

(export 'gtk-list-store-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_newv ()
;;;
;;; GtkListStore * gtk_list_store_newv (gint n_columns, GType *types);
;;;
;;; Non-vararg creation function. Used primarily by language bindings.
;;;
;;; n_columns :
;;;     number of columns in the list store
;;;
;;; types :
;;;     an array of GType types for the columns, from first to last
;;;
;;; Returns :
;;;     a new GtkListStore Rename to: gtk_list_store_new
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_column_types ()
;;;
;;; void gtk_list_store_set_column_types (GtkListStore *list_store,
;;;                                       gint n_columns,
;;;                                       GType *types);
;;;
;;; This function is meant primarily for GObjects that inherit from
;;; GtkListStore, and should only be used when constructing a new GtkListStore.
;;; It will not function after a row has been added, or a method on the
;;; GtkTreeModel interface is called.
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; n_columns :
;;;     Number of columns for the list store
;;;
;;; types :
;;;     An array length n of GTypes
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_set_column_types" gtk-list-store-set-column-types)
    :void
  (list-store (g-object gtk-list-store))
  (n-columns :int)
  (types :pointer))

(export 'gtk-list-store-set-column-types)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set ()
;;;
;;; void gtk_list_store_set (GtkListStore *list_store, GtkTreeIter *iter, ...);
;;;
;;; Sets the value of one or more cells in the row referenced by iter. The
;;; variable argument list should contain integer column numbers, each column
;;; number followed by the value to be set. The list is terminated by a -1. For
;;; example, to set column 0 with type G_TYPE_STRING to "Foo", you would write
;;; gtk_list_store_set (store, iter, 0, "Foo", -1).
;;;
;;; The value will be referenced by the store if it is a G_TYPE_OBJECT, and it
;;; will be copied if it is a G_TYPE_STRING or G_TYPE_BOXED.
;;;
;;; list_store :
;;;     a GtkListStore
;;;
;;; iter :
;;;     row iterator
;;;
;;; ... :
;;;     pairs of column number and value, terminated with -1
;;; ----------------------------------------------------------------------------

(defun gtk-list-store-set (list-store iter &rest values)
  (let ((n (length values)))
    (with-foreign-objects ((value-ar 'g-value n)
                           (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for type = (gtk-tree-model-get-column-type list-store i))
            (setf (mem-aref columns-ar :int i) i)
            (set-g-value (mem-aref value-ar 'g-value i)
                         value
                         type
                         :zero-g-value t))
      (gtk-list-store-set-valuesv list-store iter columns-ar value-ar n)
      (iter (for i from 0 below n)
            (g-value-unset (mem-aref value-ar 'g-value i)))
      iter)))

(export 'gtk-list-store-set)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_valist ()
;;;
;;; void gtk_list_store_set_valist (GtkListStore *list_store,
;;;                                 GtkTreeIter *iter,
;;;                                 va_list var_args);
;;;
;;; See gtk_list_store_set(); this version takes a va_list for use by language
;;; bindings.
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     A valid GtkTreeIter for the row being modified
;;;
;;; var_args :
;;;     va_list of column/value pairs
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_value ()
;;;
;;; void gtk_list_store_set_value (GtkListStore *list_store,
;;;                                GtkTreeIter *iter,
;;;                                gint column,
;;;                                GValue *value);
;;;
;;; Sets the data in the cell specified by iter and column. The type of value
;;; must be convertible to the type of the column.
;;;
;;; list_store :
;;;     A GtkListStore
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

(defcfun ("gtk_list_store_set_value" %gtk-list-store-set-value) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (column :int)
  (value :pointer))

(defun gtk-list-store-set-value (list-store iter column value)
  (with-foreign-object (gvalue 'g-value)
    (set-g-value gvalue
                 value
                 (gtk-tree-model-get-column-type list-store column)
                 :zero-g-value t)
    (%gtk-list-store-set-value list-store iter column gvalue)
    (g-value-unset gvalue)
    (values)))

(export 'gtk-list-store-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_valuesv ()
;;;
;;; void gtk_list_store_set_valuesv (GtkListStore *list_store,
;;;                                  GtkTreeIter *iter,
;;;                                  gint *columns,
;;;                                  GValue *values,
;;;                                  gint n_values);
;;;
;;; A variant of gtk_list_store_set_valist() which takes the columns and values
;;; as two arrays, instead of varargs. This function is mainly intended for
;;; language-bindings and in case the number of columns to change is not known
;;; until run-time.
;;;
;;; list_store :
;;;     A GtkListStore
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

(defcfun ("gtk_list_store_set_valuesv"
           gtk-list-store-set-valuesv) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (columns :pointer)
  (values :pointer)
  (n-values :int))

(export 'gtk-list-store-set-valuesv)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_remove ()
;;;
;;; gboolean gtk_list_store_remove (GtkListStore *list_store, GtkTreeIter *iter)
;;;
;;; Removes the given row from the list store. After being removed, iter is set
;;; to be the next valid row, or invalidated if it pointed to the last row in
;;; list_store.
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     A valid GtkTreeIter
;;;
;;; Returns :
;;;     TRUE if iter is valid, FALSE if not.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_remove" gtk-list-store-remove) :boolean
  (list-store (g-object gtk-list-store))
  (tree-iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert ()
;;;
;;; void gtk_list_store_insert (GtkListStore *list_store,
;;;                             GtkTreeIter *iter,
;;;                             gint position);
;;;
;;; Creates a new row at position. iter will be changed to point to this new
;;; row. If position is larger than the number of rows on the list, then the new
;;; row will be appended to the list. The row will be empty after this function
;;; is called. To fill in values, you need to call gtk_list_store_set() or
;;; gtk_list_store_set_value().
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     An unset GtkTreeIter to set to the new row.
;;;
;;; position :
;;;     position to insert the new row
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert" %gtk-list-store-insert) :void
  (list-store (g-object gtk-list-store))
  (tree-iter (g-boxed-foreign gtk-tree-iter))
  (position :int))

(defun gtk-list-store-insert (list-store position)
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-insert list-store iter position)
    iter))

(export 'gtk-list-store-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_before ()
;;;
;;; void gtk_list_store_insert_before (GtkListStore *list_store,
;;;                                    GtkTreeIter *iter,
;;;                                    GtkTreeIter *sibling);
;;;
;;; Inserts a new row before sibling. If sibling is NULL, then the row will be
;;; appended to the end of the list. iter will be changed to point to this new
;;; row. The row will be empty after this function is called. To fill in values,
;;; you need to call gtk_list_store_set() or gtk_list_store_set_value().
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     An unset GtkTreeIter to set to the new row.
;;;
;;; sibling :
;;;     A valid GtkTreeIter, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert_before" %gtk-list-store-insert-before) :void
  (list-store (g-object gtk-list-store))
  (tree-iter (g-boxed-foreign gtk-tree-iter))
  (sibling (g-boxed-foreign gtk-tree-iter)))

(defun gtk-list-store-insert-before (list-store sibling)
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-insert-before list-store iter sibling)
    iter))

(export 'gtk-list-store-insert-before)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_after ()
;;;
;;; void gtk_list_store_insert_after (GtkListStore *list_store,
;;;                                   GtkTreeIter *iter,
;;;                                   GtkTreeIter *sibling);
;;;
;;; Inserts a new row after sibling. If sibling is NULL, then the row will be
;;; prepended to the beginning of the list. iter will be changed to point to
;;; this new row. The row will be empty after this function is called. To fill
;;; in values, you need to call gtk_list_store_set() or
;;; gtk_list_store_set_value().
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     An unset GtkTreeIter to set to the new row.
;;;
;;; sibling :
;;;     A valid GtkTreeIter, or NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert_after" %gtk-list-store-insert-after) :void
  (list-store (g-object gtk-list-store))
  (tree-iter (g-boxed-foreign gtk-tree-iter))
  (sibling (g-boxed-foreign gtk-tree-iter)))

(defun gtk-list-store-insert-after (list-store sibling)
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-insert-after list-store iter sibling)
    iter))

(export 'gtk-list-store-insert-after)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_with_values ()
;;;
;;; void gtk_list_store_insert_with_values (GtkListStore *list_store,
;;;                                         GtkTreeIter *iter,
;;;                                         gint position,
;;;                                         ...);
;;;
;;; Creates a new row at position. iter will be changed to point to this new
;;; row. If position is -1, or larger than the number of rows in the list, then
;;; the new row will be appended to the list. The row will be filled with the
;;; values given to this function.
;;;
;;; Calling gtk_list_store_insert_with_values (list_store, iter, position...)
;;; has the same effect as calling
;;;
;;;   gtk_list_store_insert (list_store, iter, position);
;;;   gtk_list_store_set (list_store, iter, ...);
;;;
;;; with the difference that the former will only emit a row_inserted signal,
;;; while the latter will emit row_inserted, row_changed and, if the list store
;;; is sorted, rows_reordered. Since emitting the rows_reordered signal
;;; repeatedly can affect the performance of the program,
;;; gtk_list_store_insert_with_values() should generally be preferred when
;;; inserting rows in a sorted list store.
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     An unset GtkTreeIter to set to the new row, or NULL.
;;;
;;; position :
;;;     position to insert the new row, or -1 to append after existing rows
;;;
;;; ... :
;;;     pairs of column number and value, terminated with -1
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-list-store-insert-with-values (list-store pos &rest values)
  (let ((n (length values))
        (iter (make-gtk-tree-iter)))
    (with-foreign-objects ((value-ar 'g-value n)
                           (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for type = (gtk-tree-model-get-column-type list-store i))
            (setf (mem-aref columns-ar :int i) i)
            (set-g-value (mem-aref value-ar 'g-value i)
                         value
                         type
                         :zero-g-value t))
      (gtk-list-store-insert-with-valuesv list-store
                                          iter
                                          pos
                                          columns-ar
                                          value-ar
                                          n)
      (iter (for i from 0 below n)
            (g-value-unset (mem-aref value-ar 'g-value i)))
      iter)))

(export 'gtk-list-store-insert-with-values)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_with_valuesv ()
;;;
;;; void gtk_list_store_insert_with_valuesv (GtkListStore *list_store,
;;;                                          GtkTreeIter *iter,
;;;                                          gint position,
;;;                                          gint *columns,
;;;                                          GValue *values,
;;;                                          gint n_values);
;;;
;;; A variant of gtk_list_store_insert_with_values() which takes the columns and
;;; values as two arrays, instead of varargs. This function is mainly intended
;;; for language-bindings.
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     An unset GtkTreeIter to set to the new row, or NULL.
;;;
;;; position :
;;;     position to insert the new row
;;;
;;; columns :
;;;     an array of column numbers.
;;;
;;; values :
;;;     an array of GValues.
;;;
;;; n_values :
;;;     the length of the columns and values arrays
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert_with_valuesv"
          gtk-list-store-insert-with-valuesv) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (position :int)
  (columns :pointer)
  (values :pointer)
  (n-values :int))

(export 'gtk-list-store-insert-with-valuesv)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_prepend ()
;;;
;;; void gtk_list_store_prepend (GtkListStore *list_store, GtkTreeIter *iter);
;;;
;;; Prepends a new row to list_store. iter will be changed to point to this new
;;; row. The row will be empty after this function is called. To fill in values,
;;; you need to call gtk_list_store_set() or gtk_list_store_set_value().
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     An unset GtkTreeIter to set to the prepend row.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_prepend" %gtk-list-store-prepend) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-list-store-prepend (list-store)
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-prepend list-store iter)
    iter))

(export 'gtk-list-store-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_append ()
;;;
;;; void gtk_list_store_append (GtkListStore *list_store, GtkTreeIter *iter);
;;;
;;; Appends a new row to list_store. iter will be changed to point to this new
;;; row. The row will be empty after this function is called. To fill in values,
;;; you need to call gtk_list_store_set() or gtk_list_store_set_value().
;;;
;;; list_store :
;;;     A GtkListStore
;;;
;;; iter :
;;;     An unset GtkTreeIter to set to the appended row.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_append" %gtk-list-store-append) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-list-store-append (list-store)
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-append list-store iter)
    iter))

(export 'gtk-list-store-append)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_clear ()
;;;
;;; void gtk_list_store_clear (GtkListStore *list_store);
;;;
;;; Removes all rows from the list store.
;;;
;;; list_store :
;;;     a GtkListStore.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_clear" gtk-list-store-clear) :void
  (list-store (g-object gtk-list-store)))

(export 'gtk-list-store-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_iter_is_valid ()
;;;
;;; gboolean gtk_list_store_iter_is_valid (GtkListStore *list_store,
;;;                                        GtkTreeIter *iter);
;;;
;;; Warning
;;;
;;; This function is slow. Only use it for debugging and/or testing purposes.
;;;
;;; Checks if the given iter is a valid iter for this GtkListStore.
;;;
;;; list_store :
;;;     A GtkListStore.
;;;
;;; iter :
;;;     A GtkTreeIter.
;;;
;;; Returns :
;;;     TRUE if the iter is valid, FALSE if the iter is invalid.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_iter_is_valid" gtk-list-store-iter-is-valid) :boolean
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-iter-is-valid)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_reorder ()
;;;
;;; void gtk_list_store_reorder (GtkListStore *store, gint *new_order);
;;;
;;; Reorders store to follow the order indicated by new_order. Note that this
;;; function only works with unsorted stores.
;;;
;;; store :
;;;     A GtkListStore.
;;;
;;; new_order :
;;;     an array of integers mapping the new position of each child to its old
;;;     position before the re-ordering, i.e. new_order[newpos] = oldpos.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_swap ()
;;;
;;; void gtk_list_store_swap (GtkListStore *store,
;;;                           GtkTreeIter *a,
;;;                           GtkTreeIter *b);
;;;
;;; Swaps a and b in store. Note that this function only works with unsorted
;;; stores.
;;;
;;; store :
;;;     A GtkListStore.
;;;
;;; a :
;;;     A GtkTreeIter.
;;;
;;; b :
;;;     Another GtkTreeIter.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_swap" gtk-list-store-swap) :void
  (list-store (g-object gtk-list-store))
  (a (g-boxed-foreign gtk-tree-iter))
  (b (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-swap)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_move_before ()
;;;
;;; void gtk_list_store_move_before (GtkListStore *store,
;;;                                  GtkTreeIter *iter,
;;;                                  GtkTreeIter *position);
;;;
;;; Moves iter in store to the position before position. Note that this function
;;; only works with unsorted stores. If position is NULL, iter will be moved to
;;; the end of the list.
;;;
;;; store :
;;;     A GtkListStore.
;;;
;;; iter :
;;;     A GtkTreeIter.
;;;
;;; pos :
;;;     A GtkTreeIter, or NULL.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_move_before" gtk-list-store-move-before) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (pos (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-move-before)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_move_after ()
;;;
;;; void gtk_list_store_move_after (GtkListStore *store,
;;;                                 GtkTreeIter *iter,
;;;                                 GtkTreeIter *position);
;;;
;;; Moves iter in store to the position after position. Note that this function
;;; only works with unsorted stores. If position is NULL, iter will be moved to
;;; the start of the list.
;;;
;;; store :
;;;     A GtkListStore.
;;;
;;; iter :
;;;     A GtkTreeIter.
;;;
;;; pos :
;;;     A GtkTreeIter or NULL.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_move_after" gtk-list-store-move-after) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (pos (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-move-after)

;;; --- End of file gtk.list-store.lisp ----------------------------------------
