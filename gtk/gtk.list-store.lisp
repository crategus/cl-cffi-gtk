;;; ----------------------------------------------------------------------------
;;; gtk.list-store.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkListStore
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkListStore" gtk-list-store
  (:superclass g-object
   :export t
   :interfaces ("GtkTreeModel"
                "GtkTreeDragSource"
                "GtkTreeDragDest"
                "GtkTreeSortable"
                "GtkBuildable")
   :type-initializer "gtk_list_store_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-list-store 'type)
 "@version{2013-3-28}
  @begin{short}
    The @sym{gtk-list-store} object is a list model for use with a
    @class{gtk-tree-view} widget. It implements the @class{gtk-tree-model}
    interface, and consequentialy, can use all of the methods available there.
    It also implements the @class{gtk-tree-sortable} interface so it can be
    sorted by the view. Finally, it also implements the tree drag and drop
    interfaces.
  @end{short}

  The @sym{gtk-list-store} can accept most GObject types as a column type,
  though it cannot accept all custom types. Internally, it will keep a copy of
  data passed in (such as a string or a boxed pointer). Columns that accept
  GObjects are handled a little differently. The @sym{gtk-list-store} will keep
  a reference to the object instead of copying the value. As a result, if the
  object is modified, it is up to the application writer to call the function
  @fun{gtk-tree-model-row-changed} to emit the \"row_changed\" signal. This most
  commonly affects lists with @class{gdk-pixbuf}s stored.

  @b{Example:} Creating a simple list store.
  @begin{pre}
   enum {
     COLUMN_STRING,
     COLUMN_INT,
     COLUMN_BOOLEAN,
     N_COLUMNS
   @};

   {
     GtkListStore *list_store;
     GtkTreePath *path;
     GtkTreeIter iter;
     gint i;

     list_store = gtk_list_store_new (N_COLUMNS,
                                      G_TYPE_STRING,
                                      G_TYPE_INT,
                                      G_TYPE_BOOLEAN);

     for (i = 0; i < 10; i++)
       {
         gchar *some_data;

         some_data = get_some_data (i);

         // Add a new row to the model
         gtk_list_store_append (list_store, &iter);
         gtk_list_store_set (list_store, &iter,
                             COLUMN_STRING, some_data,
                             COLUMN_INT, i,
                             COLUMN_BOOLEAN,  FALSE,
                             -1);

         /* As the store will keep a copy of the string internally, we
          * free some_data.
          */
         g_free (some_data);
       @}

     // Modify a particular row
     path = gtk_tree_path_new_from_string (\"4\");
     gtk_tree_model_get_iter (GTK_TREE_MODEL (list_store),
                              &iter,
                              path);
     gtk_tree_path_free (path);
     gtk_list_store_set (list_store, &iter,
                         COLUMN_BOOLEAN, TRUE,
                         -1);
   @}
  @end{pre}
  @subheading{Performance Considerations}
    Internally, the @sym{gtk-list-store} was implemented with a linked list with
    a tail pointer prior to GTK+ 2.6. As a result, it was fast at data insertion
    and deletion, and not fast at random data access. The @sym{gtk-list-store}
    sets the @code{GTK_TREE_MODEL_ITERS_PERSIST} flag, which means that
    @class{gtk-tree-iter} structures can be cached while the row exists. Thus,
    if access to a particular row is needed often and your code is expected to
    run on older versions of GTK+, it is worth keeping the iter around.

  @subheading{Atomic Operations}
    It is important to note that only the methods
    @fun{gtk-list-store-insert-with-values} and
    @fun{gtk-list-store-insert-with-valuesv} are atomic, in the sense that the
    row is being appended to the store and the values filled in in a single
    operation with regard to @class{gtk-tree-model} signaling. In contrast,
    using e. g. the functions @fun{gtk-list-store-append} and then
    @fun{gtk-list-store-set} will first create a row, which triggers the
    \"row-inserted\" signal on @sym{gtk-list-store}. The row, however, is still
    empty, and any signal handler connecting to \"row-inserted\" on this
    particular store should be prepared for the situation that the row might be
    empty. This is especially important if you are wrapping the
    @sym{gtk-list-store} inside a @class{gtk-tree-model-filter} and are using a
    @code{GtkTreeModelFilterVisibleFunc}. Using any of the non-atomic operations
    to append rows to the @sym{gtk-list-store} will cause the
    @code{GtkTreeModelFilterVisibleFunc} to be visited with an empty row first;
    the function must be prepared for that.

  @subheadin{GtkListStore as GtkBuildable}
    The @sym{gtk-list-store} implementation of the @class{gtk-buildable}
    interface allows to specify the model columns with a @code{<columns>}
    element that may contain multiple @code{<column>} elements, each specifying
    one model column. The \"type\" attribute specifies the data type for the
    column.

    Additionally, it is possible to specify content for the list store in the UI
    definition, with the @code{<data>} element. It can contain multiple
    @code{<row>} elements, each specifying to content for one row of the list
    model. Inside a @code{<row>}, the @code{<col>} elements specify the content
    for individual cells.

    Note that it is probably more common to define your models in the code, and
    one might consider it a layering violation to specify the content of a list
    store in a UI definition, data, not presentation, and common wisdom is to
    separate the two, as far as possible.

    @b{Example:} A UI Definition fragment for a list store
    @begin{pre}
   <object class=\"GtkListStore\">
     <columns>
       <column type=\"gchararray\"/>
       <column type=\"gchararray\"/>
       <column type=\"gint\"/>
     </columns>
     <data>
       <row>
         <col id=\"0\">John</col>
         <col id=\"1\">Doe</col>
         <col id=\"2\">25</col>
       </row>
       <row>
         <col id=\"0\">Johan</col>
         <col id=\"1\">Dahlin</col>
         <col id=\"2\">50</col>
       </row>
     </data>
   </object>
    @end{pre}")

;;; ----------------------------------------------------------------------------

(defun call-list-store-set-column-types (list-store column-types)
  (let ((n (length column-types)))
    (with-foreign-object (types-ar 'g-type n)
      (iter (for i from 0 below n)
            (for type in column-types)
            (setf (mem-aref types-ar 'g-type i) type))
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
;;; ----------------------------------------------------------------------------

;; TODO: The function does not work as expected.

(declaim (inline gtk-list-store-new))

(defun gtk-list-store-new (&rest column-types)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @argument[...]{all @class{g-type} types for the columns, from first to last}
  @return{A new @class{gtk-list-store} object.}
  @begin{short}
    Creates a new list store as with each of the types passed in. Note that only
    types derived from standard GObject fundamental types are supported.
  @end{short}

  As an example,
  @code{(gtk-tree-store-new '(G_TYPE_INT G_TYPE_STRING GDK_TYPE_PIXBUF))}
  will create a new @class{gtk-list-store} with three columns, of type
  @code{int}, @code{string} and @class{gdk-pixbuf} respectively."
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
;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation. It seems to be incorrect.

(defcfun ("gtk_list_store_set_column_types" gtk-list-store-set-column-types)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[n-columns]{number of columns for the list store}
  @argument[types]{a list of length n of @class{g-type}s}
  This function is meant primarily for GObjects that inherit from
  @class{gtk-list-store}, and should only be used when constructing a new
  @class{gtk-list-store}.
  It will not function after a row has been added, or a method on the
  @class{gtk-tree-model} interface is called."
  (list-store (g-object gtk-list-store))
  (n-columns :int)
  (types :pointer))

(export 'gtk-list-store-set-column-types)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set ()
;;; ----------------------------------------------------------------------------

;; TODO: The arguments of the function are different from the description

(defun gtk-list-store-set (list-store iter &rest values)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{a GtkListStore}
  @argument[iter]{row iterator}
  @argument[...]{pairs of column number and value, terminated with -1}
  @begin{short}
    Sets the value of one or more cells in the row referenced by iter. The
    variable argument list should contain integer column numbers, each column
    number followed by the value to be set. The list is terminated by a -1. For
    example, to set column 0 with type G_TYPE_STRING to \"Foo\", you would write
    gtk_list_store_set (store, iter, 0, \"Foo\", -1).
  @end{short}

  The value will be referenced by the store if it is a G_TYPE_OBJECT, and it
  will be copied if it is a G_TYPE_STRING or G_TYPE_BOXED."
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_set_value" %gtk-list-store-set-value) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (column :int)
  (value :pointer))

(defun gtk-list-store-set-value (list-store iter column value)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore}
  @argument[iter]{A valid GtkTreeIter for the row being modified}
  @argument[column]{column number to modify}
  @argument[value]{new value for the cell}
  Sets the data in the cell specified by iter and column. The type of value
  must be convertible to the type of the column."
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_set_valuesv"
           gtk-list-store-set-valuesv) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore}
  @argument[iter]{A valid GtkTreeIter for the row being modified}
  @argument[columns]{an array of column numbers}
  @argument[values]{an array of GValues}
  @argument[n_values]{the length of the columns and values arrays}
  @begin{short}
    A variant of gtk_list_store_set_valist() which takes the columns and values
    as two arrays, instead of varargs. This function is mainly intended for
    language-bindings and in case the number of columns to change is not known
   until run-time.
  @end{short}

  Since 2.12"
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (columns :pointer)
  (values :pointer)
  (n-values :int))

(export 'gtk-list-store-set-valuesv)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_remove" gtk-list-store-remove) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore}
  @argument[iter]{A valid GtkTreeIter}
  @return{TRUE if iter is valid, FALSE if not.}
  Removes the given row from the list store. After being removed, iter is set
  to be the next valid row, or invalidated if it pointed to the last row in
  list_store."
  (list-store (g-object gtk-list-store))
  (tree-iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert" %gtk-list-store-insert) :void
  (list-store (g-object gtk-list-store))
  (tree-iter (g-boxed-foreign gtk-tree-iter))
  (position :int))

(defun gtk-list-store-insert (list-store position)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore}
  @argument[iter]{An unset GtkTreeIter to set to the new row.}
  @argument[position]{position to insert the new row}
  Creates a new row at position. iter will be changed to point to this new
  row. If position is larger than the number of rows on the list, then the new
  row will be appended to the list. The row will be empty after this function
  is called. To fill in values, you need to call gtk_list_store_set() or
  gtk_list_store_set_value()."
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-insert list-store iter position)
    iter))

(export 'gtk-list-store-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_before ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert_before" %gtk-list-store-insert-before) :void
  (list-store (g-object gtk-list-store))
  (tree-iter (g-boxed-foreign gtk-tree-iter))
  (sibling (g-boxed-foreign gtk-tree-iter)))

(defun gtk-list-store-insert-before (list-store sibling)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore}
  @argument[iter]{An unset GtkTreeIter to set to the new row.}
  @argument[sibling]{A valid GtkTreeIter, or NULL.}
  Inserts a new row before sibling. If sibling is NULL, then the row will be
  appended to the end of the list. iter will be changed to point to this new
  row. The row will be empty after this function is called. To fill in values,
  you need to call gtk_list_store_set() or gtk_list_store_set_value()."
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-insert-before list-store iter sibling)
    iter))

(export 'gtk-list-store-insert-before)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_after ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert_after" %gtk-list-store-insert-after) :void
  (list-store (g-object gtk-list-store))
  (tree-iter (g-boxed-foreign gtk-tree-iter))
  (sibling (g-boxed-foreign gtk-tree-iter)))

(defun gtk-list-store-insert-after (list-store sibling)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore}
  @argument[iter]{An unset GtkTreeIter to set to the new row.}
  @argument[sibling]{A valid GtkTreeIter, or NULL.}
  Inserts a new row after sibling. If sibling is NULL, then the row will be
  prepended to the beginning of the list. iter will be changed to point to
  this new row. The row will be empty after this function is called. To fill
  in values, you need to call gtk_list_store_set() or
  gtk_list_store_set_value()."
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-insert-after list-store iter sibling)
    iter))

(export 'gtk-list-store-insert-after)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_with_values ()
;;; ----------------------------------------------------------------------------

(defun gtk-list-store-insert-with-values (list-store pos &rest values)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore}
  @argument[iter]{An unset GtkTreeIter to set to the new row, or NULL.}
  @argument[position]{position to insert the new row, or -1 to append after
    existing rows}
  @argument[...]{pairs of column number and value, terminated with -1}
  @begin{short}
    Creates a new row at position. iter will be changed to point to this new
    row. If position is -1, or larger than the number of rows in the list, then
    the new row will be appended to the list. The row will be filled with the
    values given to this function.
  @end{short}

  Calling gtk_list_store_insert_with_values (list_store, iter, position...)
  has the same effect as calling
  @begin{pre}
   gtk_list_store_insert (list_store, iter, position);
   gtk_list_store_set (list_store, iter, ...);
  @end{pre}
  with the difference that the former will only emit a row_inserted signal,
  while the latter will emit row_inserted, row_changed and, if the list store
  is sorted, rows_reordered. Since emitting the rows_reordered signal
  repeatedly can affect the performance of the program,
  gtk_list_store_insert_with_values() should generally be preferred when
  inserting rows in a sorted list store.

  Since 2.6"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_insert_with_valuesv"
          gtk-list-store-insert-with-valuesv) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore}
  @argument[iter]{An unset GtkTreeIter to set to the new row, or NULL.}
  @argument[position]{position to insert the new row}
  @argument[columns]{an array of column numbers.}
  @argument[values]{an array of GValues.}
  @argument[n_values]{the length of the columns and values arrays}
  @begin{short}
    A variant of gtk_list_store_insert_with_values() which takes the columns and
    values as two arrays, instead of varargs. This function is mainly intended
    for language-bindings.
  @end{short}

  Since 2.6"
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (position :int)
  (columns :pointer)
  (values :pointer)
  (n-values :int))

(export 'gtk-list-store-insert-with-valuesv)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_prepend" %gtk-list-store-prepend) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-list-store-prepend (list-store)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore}
  @argument[iter]{An unset GtkTreeIter to set to the prepend row.}
  Prepends a new row to list_store. iter will be changed to point to this new
  row. The row will be empty after this function is called. To fill in values,
  you need to call gtk_list_store_set() or gtk_list_store_set_value()."
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-prepend list-store iter)
    iter))

(export 'gtk-list-store-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_append ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_append" %gtk-list-store-append) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-list-store-append (list-store)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore}
  @argument[iter]{An unset GtkTreeIter to set to the appended row.}
  Appends a new row to list_store. iter will be changed to point to this new
  row. The row will be empty after this function is called. To fill in values,
  you need to call gtk_list_store_set() or gtk_list_store_set_value()."
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-append list-store iter)
    iter))

(export 'gtk-list-store-append)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_clear" gtk-list-store-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{a GtkListStore.}
  Removes all rows from the list store."
  (list-store (g-object gtk-list-store)))

(export 'gtk-list-store-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_iter_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_iter_is_valid" gtk-list-store-iter-is-valid) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[list_store]{A GtkListStore.}
  @argument[iter]{A GtkTreeIter.}
  @return{TRUE if the iter is valid, FALSE if the iter is invalid.}
  @subheading{Warning}
    This function is slow. Only use it for debugging and/or testing purposes.

  @begin{short}
    Checks if the given iter is a valid iter for this GtkListStore.
  @end{short}

  Since 2.2"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_swap" gtk-list-store-swap) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[store]{A GtkListStore.}
  @argument[a]{A GtkTreeIter.}
  @argument[b]{Another GtkTreeIter.}
  @begin{short}
    Swaps a and b in store. Note that this function only works with unsorted
    stores.
  @end{short}

  Since 2.2"
  (list-store (g-object gtk-list-store))
  (a (g-boxed-foreign gtk-tree-iter))
  (b (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-swap)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_move_before ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_move_before" gtk-list-store-move-before) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[store]{A GtkListStore.}
  @argument[iter]{A GtkTreeIter.}
  @argument[pos]{A GtkTreeIter, or NULL.}
  @begin{short}
    Moves iter in store to the position before position. Note that this function
    only works with unsorted stores. If position is NULL, iter will be moved to
    the end of the list.
  @end{short}

  Since 2.2"
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (pos (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-move-before)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_move_after ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_move_after" gtk-list-store-move-after) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-28}
  @argument[store]{A GtkListStore.}
  @argument[iter]{A GtkTreeIter.}
  @argument[pos]{A GtkTreeIter or NULL.}
  @begin{short}
    Moves iter in store to the position after position. Note that this function
    only works with unsorted stores. If position is NULL, iter will be moved to
    the start of the list.
  @end{short}

  Since 2.2"
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (pos (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-move-after)

;;; --- End of file gtk.list-store.lisp ----------------------------------------
