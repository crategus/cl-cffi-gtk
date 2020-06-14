;;; ----------------------------------------------------------------------------
;;; gtk.tree-store.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     A tree-like data structure that can be used with the GtkTreeView
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
;;;     GObject
;;;     ╰── GtkTreeStore
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTreeStore implements GtkTreeModel, GtkTreeDragSource,
;;;     GtkTreeDragDest, GtkTreeSortable and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeStore
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTreeStore" gtk-tree-store
  (:superclass g-object
   :export t
   :interfaces ("GtkBuildable"
                "GtkTreeDragDest"
                "GtkTreeDragSource"
                "GtkTreeModel"
                "GtkTreeSortable")
   :type-initializer "gtk_tree_store_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tree-store 'type)
 "@version{2013-9-13}
  @begin{short}
    The @sym{gtk-tree-store} object is a list model for use with a
    @class{gtk-tree-view} widget. It implements the @class{gtk-tree-model}
    interface, and consequentialy, can use all of the methods available there.
    It also implements the @class{gtk-tree-sortable} interface so it can be
    sorted by the view. Finally, it also implements the tree drag and drop
    interfaces.
  @end{short}

  @subheading{GtkTreeStore as GtkBuildable}
    The @sym{gtk-tree-store} implementation of the @class{gtk-buildable}
    interface allows to specify the model columns with a @code{<columns>}
    element that may contain multiple @code{<column>} elements, each specifying
    one model column. The @code{\"type\"} attribute specifies the data type for
    the column.

  @b{Example:} A UI Definition fragment for a tree store
  @begin{pre}
    <object class=\"GtkTreeStore\">
     <columns>
       <column type=\"gchararray\"/>
       <column type=\"gchararray\"/>
       <column type=\"gint\"/>
     </columns>
   </object>
  @end{pre}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-sortable}
  @see-class{gtk-buildable}")

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :after ((store gtk-tree-store) &rest initargs
                                       &key (column-types
                                             nil
                                             column-types-p)
                                       &allow-other-keys)
  (declare (ignore initargs))
  (when column-types-p
    (gtk-tree-store-set-column-types store column-types)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-store-new))

(defun gtk-tree-store-new (&rest column-types)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[column-types]{all @class{g-type} types for the columns, from first
    to last}
  @return{A new @class{gtk-tree-store} object.}
  @begin{short}
    Creates a new tree store as with columns of the types passed in.
  @end{short}
  Note that only types derived from standard @code{GObject} fundamental types
  are supported.

  As an example,
  @code{(gtk-tree-store-new \"gint\" \"gchararray\" \"GdkPixbuf\")}; will create
  a new @class{gtk-tree-store} with three columns, of type @code{:int},
  @code{:string} and @code{GdkPixbuf} respectively.
  @see-class{gtk-tree-store}"
  (make-instance 'gtk-tree-store
                 :column-types column-types))

(export 'gtk-tree-store-new)

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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_set_column_types"
          %gtk-tree-store-set-column-types) :void
  (tree-store (g-object gtk-tree-store))
  (n-columns :int)
  (types :pointer))

(defun gtk-tree-store-set-column-types (tree-store column-types)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-13}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[types]{a list GType types, one for each column}
  @begin{short}
    This function is meant primarily for GObjects that inherit from
    @class{gtk-tree-store}, and should only be used when constructing a new
    @class{gtk-tree-store}.
  @end{short}
  It will not function after a row has been added, or a method on the
  @class{gtk-tree-model} interface is called.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-model}"
  (let ((n (length column-types)))
    (with-foreign-object (types-ar 'g-type n)
      (iter (for i from 0 below n)
            (for type in column-types)
            (setf (mem-aref types-ar 'g-type i) type))
      (%gtk-tree-store-set-column-types tree-store n types-ar))))

(export 'gtk-tree-store-set-column-types)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set_value ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_set_value" %gtk-tree-store-set-value) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (column :int)
  (value :pointer))

(defun gtk-tree-store-set-value (tree-store iter column value)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-13}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[iter]{a valid @class{gtk-tree-iter} for the row being modified}
  @argument[column]{column number to modify}
  @argument[value]{new value for the cell}
  @begin{short}
    Sets the data in the cell specified by @arg{iter} and @arg{column}.
  @end{short}
  The type of value must be convertible to the type of the column.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-set}"
  (with-foreign-object (v '(:struct g-value))
    (set-g-value v
                 value
                 (gtk-tree-model-column-type tree-store column)
                 :zero-g-value t)
    (%gtk-tree-store-set-value tree-store iter column v)
    (g-value-unset v)
    (values)))

(export 'gtk-tree-store-set-value)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_set ()
;;; ----------------------------------------------------------------------------

(defun gtk-tree-store-set (tree-store iter &rest values)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-13}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[iter]{a valid @class{gtk-tree-iter} for the row being modified}
  @argument[values]{pairs of column number and value}
  @return{The @class{gtk-tree-iter} for the row being modified.}
  @begin{short}
    Sets the value of one or more cells in the row referenced by @arg{iter}. The
    variable argument list should contain integer column numbers, each column
    number followed by the value to be set. For example, to set column 0 with
    type @var{+g-type-string+} to \"Foo\", you would write
    @code{(gtk-tree-store-set store iter 0 \"Foo\")}.
  @end{short}

  The value will be referenced by the store if it is a @var{+g-type-object+},
  and it will be copied if it is a @var{+g-type-string+} or
  @var{+g-type-boxed+}.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-set-value}"
  (let ((n (length values)))
    (with-foreign-objects ((value-ar '(:struct g-value) n)
                           (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for type = (gtk-tree-model-column-type tree-store i))
            (setf (mem-aref columns-ar :int i) i)
            (set-g-value (mem-aptr value-ar '(:struct g-value) i)
                         value
                         type
                         :zero-g-value t))
      (gtk-tree-store-set-valuesv tree-store iter columns-ar value-ar n)
      (iter (for i from 0 below n)
            (g-value-unset (mem-aptr value-ar '(:struct g-value) i)))
      iter)))

(export 'gtk-tree-store-set)

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
;;; ----------------------------------------------------------------------------

;; This function is for internal use only and not exported.

(defcfun ("gtk_tree_store_set_valuesv"
           gtk-tree-store-set-valuesv) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-7-4}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[iter]{a valid @class{gtk-tree-iter} for the row being modified}
  @argument[columns]{an array of column numbers}
  @argument[values]{an array of @symbol{g-value}'s}
  @argument[n-values]{the length of the columns and values arrays}
  @begin{short}
    A variant of the function @fun{gtk-tree-store-set} which takes the columns
    and values as two arrays. This function is mainly intended
    for language bindings or in case the number of columns to change is not
    known until run-time.
  @end{short}
  @see-function{gtk-tree-store-set}"
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (columns :pointer)
  (values :pointer)
  (n-values :int))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_remove" gtk-tree-store-remove) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-9-13}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[iter]{a valid @class{gtk-tree-iter}}
  @return{@em{True} if @arg{iter} is still valid, @code{nil} if not.}
  @begin{short}
    Removes @arg{iter} from @arg{tree-store}.
  @end{short}
  After being removed, @arg{iter} is set to the next valid row at that level, or
  invalidated if it previously pointed to the last one.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-insert}"
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_insert" %gtk-tree-store-insert) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (position :int))

(defun gtk-tree-store-insert (tree-store parent position)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-13}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[parent]{a valid @class{gtk-tree-iter}, or @code{nil}}
  @argument[position]{position to insert the new row}
  @begin{short}
    Creates a new row at position.
  @end{short}
  If @arg{parent} is non-@code{nil}, then the row will be made a child of
  @arg{parent}. Otherwise, the row will be created at the toplevel. If position
  is larger than the number of rows at that level, then the new row will be
  inserted to the end of the list. @arg{iter} will be changed to point to this
  new row. The row will be empty after this function is called. To fill in
  values, you need to call the functions @fun{gtk-tree-store-set} or
  @fun{gtk-tree-store-set-value}.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-remove}
  @see-function{gtk-tree-store-set}
  @see-function{gtk-tree-store-set-value}"
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-tree-store-insert tree-store iter parent position)
    iter))

(export 'gtk-tree-store-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_before ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_insert_before" %gtk-tree-store-insert-before) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (sibling (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-store-insert-before (tree-store parent sibling)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-13}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[parent]{a valid @class{gtk-tree-iter}, or @code{nil}}
  @argument[sibling]{a valid @class{gtk-tree-iter}, or @code{nil}}
  @begin{short}
    Inserts a new row before sibling.
  @end{short}
  If @arg{sibling} is @code{nil}, then the row will be appended to
  @arg{parent}'s children. If @arg{parent} and @arg{sibling} are @arg{nil}, then
  the row will be appended to the toplevel. If both @arg{sibling} and
  @arg{parent} are set, then @arg{parent} must be the parent of @arg{sibling}.
  When @arg{sibling} is set, @arg{parent} is optional.

  @arg{iter} will be changed to point to this new row. The row will be empty
  after this function is called. To fill in values, you need to call the
  functions @fun{gtk-tree-store-set} or @fun{gtk-tree-store-set-value}.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-set}
  @see-function{gtk-tree-store-set-value}"
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-tree-store-insert-before tree-store iter parent sibling)
    iter))

(export 'gtk-tree-store-insert-before)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_after ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_insert_after" %gtk-tree-store-insert-after) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (sibling (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-store-insert-after (tree-store parent sibling)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-13}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[parent]{a valid @class{gtk-tree-iter}, or @code{nil}}
  @argument[sibling]{a valid @class{gtk-tree-iter}, or @code{nil}}
  @begin{short}
    Inserts a new row after sibling.
  @end{short}
  If @arg{sibling} is @code{nil}, then the row will be prepended to
  @arg{parent}'s children. If @arg{parent} and @arg{sibling} are @code{nil},
  then the row will be prepended to the toplevel. If both @arg{sibling} and
  @arg{parent} are set, then @arg{parent} must be the parent of @arg{sibling}.
  When @arg{sibling} is set, @arg{parent} is optional.

  @arg{iter} will be changed to point to this new row. The row will be empty
  after this function is called. To fill in values, you need to call the
  functions @fun{gtk-tree-store-set} or @fun{gtk-tree-store-set-value}.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-set}
  @see-function{gtk-tree-store-set-value}"
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-tree-store-insert-after tree-store iter parent sibling)
    iter))

(export 'gtk-tree-store-insert-after)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_with_values ()
;;; ----------------------------------------------------------------------------

(defun gtk-tree-store-insert-with-values (tree-store parent position
                                                     &rest values)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-13}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[parent]{a valid @class{gtk-tree-iter}, or @code{nil}}
  @argument[position]{position to insert the new row, or -1 to append after
    existing rows}
  @argument[values]{pairs of column number and value}
  @begin{short}
    Creates a new row at @arg{position}.
  @end{short}
  @arg{iter} will be changed to point to this new row. If @arg{position} is -1,
  or larger than the number of rows on the list, then the new row will be
  appended to the list. The row will be filled with the values given to this
  function.

  Calling @code{(gtk-tree-store-insert-with-values tree_store position ...)}
  has the same effect as calling
  @begin{pre}
 gtk_tree_store_insert (tree_store, iter, position);
 gtk_tree_store_set (tree_store, iter, ...);
  @end{pre}
  with the different that the former will only emit a \"row-inserted\" signal,
  while the latter will emit \"row-inserted\", \"row-changed\" and if the tree
  store is sorted, \"rows-reordered\". Since emitting the \"rows-reordered\"
  signal repeatedly can affect the performance of the program, the function
  @sym{gtk-tree-store-insert-with-values} should generally be preferred when
  inserting rows in a sorted tree store.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-insert}
  @see-function{gtk-tree-store-set}"
  (let ((n (length values))
        (iter (make-gtk-tree-iter)))
    (with-foreign-objects ((v-ar '(:struct g-value) n)
                           (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for type = (gtk-tree-model-column-type tree-store i))
            (setf (mem-aref columns-ar :int i) i)
            (set-g-value (mem-aptr v-ar '(:struct g-value) i)
                         value
                         type
                         :zero-g-value t))
      (%gtk-tree-store-insert-with-valuesv tree-store
                                           iter
                                           parent
                                           position
                                           columns-ar
                                           v-ar
                                           n)
      (iter (for i from 0 below n)
            (g-value-unset (mem-aptr v-ar '(:struct g-value) i)))
      iter)))

(export 'gtk-tree-store-insert-with-values)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_insert_with_valuesv ()
;;; ----------------------------------------------------------------------------

;; This function is for internal use and not exported.

(defcfun ("gtk_tree_store_insert_with_valuesv"
          %gtk-tree-store-insert-with-valuesv) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-27}
  @argument[tree_store]{A GtkTreeStore}
  @argument[iter]{An unset GtkTreeIter to set the new row, or NULL.}
  @argument[parent]{A valid GtkTreeIter, or NULL.}
  @argument[position]{position to insert the new row}
  @argument[columns]{an array of column numbers}
  @argument[values]{an array of GValues}
  @argument[n_values]{the length of the columns and values arrays}
  @begin{short}
    A variant of gtk_tree_store_insert_with_values() which takes the columns and
    values as two arrays, instead of varargs. This function is mainly intended
    for language bindings.
  @end{short}
  @see-class{gtk-tree-store}"
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (position :int)
  (columns :pointer)
  (values :pointer)
  (n-values :int))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_prepend" %gtk-tree-store-prepend) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-store-prepend (tree-store parent)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-14}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[parent]{a valid @class{gtk-tree-iter}, or @code{nil}}
  @begin{short}
    Prepends a new row to @arg{tree-store}.
  @end{short}
  If @arg{parent} is non-@code{nil}, then it will prepend the new row before the
  first child of parent, otherwise it will prepend a row to the top level.
  @arg{iter} will be changed to point to this new row. The row will be empty
  after this function is called. To fill in values, you need to call the
  functions @fun{gtk-tree-store-set} or @fun{gtk-tree-store-set-value}.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-set}
  @see-function{gtk-tree-store-set-value}"
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-tree-store-prepend tree-store iter parent)
    iter))

(export 'gtk-tree-store-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_append ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_append" %gtk-tree-store-append) :void
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-store-append (tree-store parent)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[parent]{a valid @class{gtk-tree-iter} structure, or @code{nil}}
  @return{The @class{gtk-tree-iter} of the appended row.}
  Appends a new row to @arg{tree-store}. If @arg{parent} is non-@code{nil}, then
  it will append the new row after the last child of @arg{parent}, otherwise it
  will append a row to the top level. The row will be empty after this function
  is called. To fill in values, you need to call the functions
  @fun{gtk-tree-store-set} or @fun{gtk-tree-store-set-value}.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-set}
  @see-function{gtk-tree-store-set-value}"
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-tree-store-append tree-store iter parent)
    iter))

(export 'gtk-tree-store-append)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_is_ancestor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_is_ancestor" gtk-tree-store-is-ancestor) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[iter]{a valid @class{gtk-tree-iter}}
  @argument[descendant]{a valid @class{gtk-tree-iter}}
  @return{@em{True}, if @arg{iter} is an ancestor of @arg{descendant}.}
  @begin{short}
    Returns @em{true} if @arg{iter} is an ancestor of @arg{descendant}.
  @end{short}
  That is, @arg{iter} is the parent, or grandparent or great-grandparent,
  of @arg{descendant}.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}"
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (descendant (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-is-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_iter_depth ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_iter_depth" gtk-tree-store-iter-depth) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[iter]{a valid @class{gtk-tree-iter}}
  @return{The depth of @arg{iter}.}
  @begin{short}
    Returns the depth of @arg{iter}.
  @end{short}
  This will be 0 for anything on the root level, 1 for anything down a level,
  etc.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}"
  (tree-store (g-object gtk-tree-store))
  (tree-iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-iter-depth)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_clear" gtk-tree-store-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  Removes all rows from @arg{tree-store}
  @see-class{gtk-tree-store}"
  (tree-store (g-object gtk-tree-store)))

(export 'gtk-tree-store-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_iter_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_iter_is_valid" gtk-tree-store-iter-is-valid) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[iter]{a @class{gtk-tree-iter}}
  @return{@em{True} if the @arg{iter} is valid, @code{nil} if the @arg{iter} is
    invalid.}
  @subheading{Warning}
    This function is slow. Only use it for debugging and/or testing purposes.

  @begin{short}
    Checks if the given @arg{iter} is a valid iter for this
    @class{gtk-tree-store}.
  @end{short}
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}"
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
;;;     position before the re-ordering, i.e. new_order[newpos] = oldpos
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_swap ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_swap" gtk-tree-store-swap) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[a]{a @class{gtk-tree-iter}}
  @argument[b]{another @class{gtk-tree-iter}}
  @begin{short}
    Swaps @arg{a} and @arg{b} in the same level of @arg{tree-store}.
  @end{short}
  Note that this function only works with unsorted stores.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}"
  (tree-store (g-object gtk-tree-store))
  (a (g-boxed-foreign gtk-tree-iter))
  (b (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-swap)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_move_before ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_move_before" gtk-tree-store-move-before) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[tree-store]{a @class{gtk-tree-store}}
  @argument[iter]{a @class{gtk-tree-iter}}
  @argument[position]{a @class{gtk-tree-iter} or @code{nil}}
  @begin{short}
    Moves @arg{iter} in @arg{tree-store} to the position before @arg{position}.
  @end{short}
  @arg{iter} and @arg{position} should be in the same level. Note that this
  function only works with unsorted stores. If @arg{position} is @code{nil},
  @arg{iter} will be moved to the end of the level.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-move-after}"
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (position (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-move-before)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_store_move_after ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_store_move_after" gtk-tree-store-move-after) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[tree-store]{a @class{gtk-tree-store} object}
  @argument[iter]{a @class{gtk-tree-iter} object}
  @argument[position]{a @class{gtk-tree-iter} object}
  @begin{short}
    Moves @arg{iter} in @arg{tree-store} to the position after @arg{position}.
  @end{short}
  @arg{iter} and @arg{position} should be in the same level. Note that this
  function only works with unsorted stores. If @arg{position} is @code{nil},
  @arg{iter} will be moved to the start of the level.
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-store-move-before}"
  (tree-store (g-object gtk-tree-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (position (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-store-move-after)

;;; --- End of file gtk.tree-store.lisp ----------------------------------------
