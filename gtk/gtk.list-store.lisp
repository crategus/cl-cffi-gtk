;;; ----------------------------------------------------------------------------
;;; gtk.list-store.lisp
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
;;; GtkListStore
;;;
;;;     A list-like data structure that can be used with the GtkTreeView
;;;
;;; Types and Values
;;;
;;;     GtkListStore
;;;
;;; Functions
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
;;;     GObject
;;;     ╰── GtkListStore
;;;
;;; Implemented Interfaces
;;;
;;;     GtkListStore implements GtkTreeModel, GtkTreeDragSource,
;;;     GtkTreeDragDest, GtkTreeSortable and GtkBuildable.
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
 "@version{2020-4-14}
  @begin{short}
    The @sym{gtk-list-store} object is a list model for use with a
    @class{gtk-tree-view} widget.
  @end{short}
  It implements the @class{gtk-tree-model} interface, and consequentialy, can
  use all of the methods available there. It also implements the
  @class{gtk-tree-sortable} interface so it can be sorted by the view. Finally,
  it also implements the tree drag and drop interfaces.

  The @sym{gtk-list-store} can accept most GObject types as a column type,
  though it cannot accept all custom types. Internally, it will keep a copy of
  data passed in, such as a string or a boxed pointer. Columns that accept
  GObjects are handled a little differently. The @sym{gtk-list-store} will keep
  a reference to the object instead of copying the value. As a result, if the
  object is modified, it is up to the application writer to call the function
  @fun{gtk-tree-model-row-changed} to emit the \"row-changed\" signal. This
  most commonly affects lists with @class{gdk-pixbuf}s stored.

  @subheading{Performance Considerations}
    Internally, the @sym{gtk-list-store} was implemented with a linked list
    with a tail pointer prior to GTK+ 2.6. As a result, it was fast at data
    insertion and deletion, and not fast at random data access. The
    @sym{gtk-list-store} sets the @code{:iters-persist} flag of type
    @symbol{gtk-tree-model-flags}, which means that @class{gtk-tree-iter}
    structures can be cached while the row exists. Thus, if access to a
    particular row is needed often and your code is expected to run on older
    versions of GTK+, it is worth keeping the iter around.

  @subheading{Atomic Operations}
    It is important to note that only the method
    @fun{gtk-list-store-insert-with-values} is atomic, in the sense that the
    row is being appended to the store and the values filled in in a single
    operation with regard to @class{gtk-tree-model} signaling. In contrast,
    using e. g. the functions @fun{gtk-list-store-append} and then
    @fun{gtk-list-store-set} will first create a row, which triggers the
    \"row-inserted\" signal on @sym{gtk-list-store}. The row, however, is still
    empty, and any signal handler connecting to \"row-inserted\" on this
    particular store should be prepared for the situation that the row might
    be empty. This is especially important if you are wrapping the
    @sym{gtk-list-store} inside a @class{gtk-tree-model-filter} and are using
    a @code{GtkTreeModelFilterVisibleFunc}. Using any of the non-atomic
    operations to append rows to the @sym{gtk-list-store} will cause the
    @code{GtkTreeModelFilterVisibleFunc} to be visited with an empty row first;
    the function must be prepared for that.
  @begin[Example]{dictionary}
    Creating a simple list store.
    @begin{pre}
(defun create-and-fill-model ()
  (let ((list-data '(\"Name1\" \"Name2\" \"Name3\" \"Name4\" \"Name5\"))
        ;; Create a new list store with three columns
        (list-store (make-instance 'gtk-list-store
                                   :column-types
                                   '(\"gint\" \"gchararray\" \"gboolean\"))))
    ;; Fill in some data
    (loop for data in list-data
          for i from 0 do
          ;; Add a new row to the model
          (gtk-list-store-set list-store
                              (gtk-list-store-append list-store)
                              i
                              data
                              nil))
    ;; Modify a particular row
    (let ((path (gtk-tree-path-new-from-string \"2\")))
      (gtk-list-store-set-value list-store
                                (gtk-tree-model-get-iter list-store path)
                                2
                                t))
    ;; Return the new list store
    list-store))
    @end{pre}
  @end{dictionary}
  @begin[GtkListStore as GtkBuildable]{dictionary}
    The @sym{gtk-list-store} implementation of the @class{gtk-buildable}
    interface allows to specify the model columns with a @code{<columns>}
    element that may contain multiple @code{<column>} elements, each specifying
    one model column. The \"type\" attribute specifies the data type for the
    column.

    Additionally, it is possible to specify content for the list store in the
    UI definition, with the @code{<data>} element. It can contain multiple
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
    @end{pre}
  @end{dictionary}
  @see-class{gtk-tree-view}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-sortable}
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-buildable}
  @see-class{gdk-pixbuf}
  @see-symbol{gtk-tree-model-flags}
  @see-function{gtk-list-store-set}
  @see-function{gtk-list-store-append}
  @see-function{gtk-tree-model-row-changed}
  @see-function{gtk-list-store-insert-with-values}")

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-list-store-new))

;; See also the function gtk-list-store-set-column-types
;; which duplicates this code. Consider to rewrite both functions.

(defun call-list-store-set-column-types (list-store column-types)
  (let ((n (length column-types)))
    (with-foreign-object (types-ar 'g-type n)
      (iter (for i from 0 below n)
            (for type in column-types)
            (setf (mem-aref types-ar 'g-type i) type))
      (%gtk-list-store-set-column-types list-store n types-ar))))

(defmethod initialize-instance :after ((list-store gtk-list-store)
                                       &rest initargs
                                       &key (column-types
                                             nil
                                             column-types-p)
                                       &allow-other-keys)
  (declare (ignore initargs))
  (when column-types-p
    (call-list-store-set-column-types list-store column-types)))

(defun gtk-list-store-new (&rest column-types)
 #+cl-cffi-gtk-documentation
 "@version{2020-4-13}
  @argument[column-types]{all @class{g-type} types for the columns, from first
    to last}
  @return{A new @class{gtk-list-store} object.}
  @begin{short}
    Creates a new list store as with each of the types passed in.
  @end{short}
  Note that only types derived from standard GObject fundamental types are
  supported.
  @begin[Example]{dictionary}
    The following example creates a new @sym{gtk-list-store} with three
    columnes, of type @codeg{gint}, @code{gchararray} and @code{GdkPixbuf}.
    @begin{pre}
 (gtk-list-store-new \"gint\" \"gchararray\" \"GdkPixbuf\")
    @end{pre}
    Note that in the Lisp binding a second implementation is
    @begin{pre}
 (make-instance 'gtk-list-store
                :column-types '(\"gint\" \"gchararray\" \"GdkPixbuf\"))
    @end{pre}
  @end{dictionary}
  @see-class{gtk-list-store}
  @see-class{g-type}"
  (make-instance 'gtk-list-store
                 :column-types column-types))

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

;;; Implementation not needed.

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set_column_types ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_set_column_types" %gtk-list-store-set-column-types)
    :void
  (list-store (g-object gtk-list-store))
  (n-columns :int)
  (types :pointer))

(defun gtk-list-store-set-column-types (list-store &rest column-types)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[column-types]{the @class{g-type}s of the columns}
  @begin{short}
    This function is meant primarily for GObjects that inherit from
    @class{gtk-list-store}, and should only be used when constructing a new
    @class{gtk-list-store}.
  @end{short}
  It will not function after a row has been added, or a method on the
  @class{gtk-tree-model} interface is called.
  @see-class{gtk-list-store}
  @see-class{g-type}
  @see-class{gtk-tree-model}"
  (let ((n (length column-types)))
    (with-foreign-object (types-ar 'g-type n)
      (iter (for i from 0 below n)
            (for type in column-types)
            (setf (mem-aref types-ar 'g-type i) type))
      (%gtk-list-store-set-column-types list-store n types-ar))))

(export 'gtk-list-store-set-column-types)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_set ()
;;; ----------------------------------------------------------------------------

;; The Lisp implementation does not support pairs of an index and a value.
;; Consider to change the implemenation.

(defun gtk-list-store-set (list-store iter &rest values)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[list_store]{a @class{gtk-list-store} object}
  @argument[iter]{row iterator}
  @argument[values]{values to set}
  @begin{short}
    Sets the value of one or more cells in the row referenced by @arg{iter}.
  @end{short}
  The variable argument list should contain the values to be set.

  @subheading{Note:}
    The Lisp implemenation does not support pairs of an index and a value, but
    only a list of values. Therefore, it is not possible to set individual
    columns.
  @see-class{gtk-list-store}"
  (let ((n (length values)))
    (with-foreign-objects ((value-ar '(:struct g-value) n)
                           (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for type = (gtk-tree-model-column-type list-store i))
            (setf (mem-aref columns-ar :int i) i)
            (set-g-value (mem-aptr value-ar '(:struct g-value) i)
                         value
                         type
                         :zero-g-value t))
      (%gtk-list-store-set-valuesv list-store iter columns-ar value-ar n)
      (iter (for i from 0 below n)
            (g-value-unset (mem-aptr value-ar '(:struct g-value) i)))
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

;; Implementation not needed

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
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[iter]{a valid @class{gtk-tree-iter} for the row being modified}
  @argument[column]{column number to modify}
  @argument[value]{new value for the cell}
  @begin{short}
    Sets the data in the cell specified by @arg{iter} and @arg{column}.
  @end{short}
  The type of @arg{value} must be convertible to the type of the @arg{column}.
  @see-class{gtk-list-store}"
  (with-foreign-object (gvalue '(:struct g-value))
    (set-g-value gvalue
                 value
                 (gtk-tree-model-column-type list-store column)
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
;;;     an array of column numbers. [array length=n_values]
;;;
;;; values :
;;;     an array of GValues. [array length=n_values]
;;;
;;; n_values :
;;;     the length of the columns and values arrays
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;; Only for internal use. Not exported.

(defcfun ("gtk_list_store_set_valuesv"
          %gtk-list-store-set-valuesv) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (columns :pointer)
  (values :pointer)
  (n-values :int))

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_remove" gtk-list-store-remove) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[iter]{a valid @class{gtk-tree-iter} object}
  @return{@em{True} if @arg{iter} is valid, @code{nil} if not.}
  @begin{short}
    Removes the given row from the list store.
  @end{short}
  After being removed, @arg{iter} is set to be the next valid row, or
  invalidated if it pointed to the last row in @arg{list-store}.
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}"
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
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[position]{position to insert the new row}
  @return{@code{iter} -- @class{gtk-tree-iter} of the new row}
  @begin{short}
    Creates a new row at @arg{position}.
  @end{short}
  @arg{iter} will point to this new row. If @arg{position} is larger than the
  number of rows on the list, then the new row will be appended to the list. The
  row will be empty after this function is called. To fill in values, you need
  to call the functions @fun{gtk-list-store-set} or
  @fun{gtk-list-store-set-value}.
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-list-store-set}
  @see-function{gtk-list-store-set-value}"
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
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[sibling]{a valid @class{gtk-tree-iter}, or @code{nil}}
  @return{@code{iter} -- a @class{gtk-tree-iter} to the new row}
  @begin{short}
    Inserts a new row before @arg{sibling}.
  @end{short}
  If @arg{sibling} is @code{nil}, then the row will be appended to the end of
  the list. @arg{iter} will point to this new row. The row will be empty after
  this function is called. To fill in values, you need to call the functions
  @fun{gtk-list-store-set} or @fun{gtk-list-store-set-value}.
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-list-store-set}
  @see-function{gtk-list-store-set-value}"
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
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[sibling]{a valid @class{gtk-tree-iter}, or @code{nil}}
  @return{@code{iter} -- a @class{gtk-tree-iter} to the new row}
  @begin{short}
    Inserts a new row after @arg{sibling}.
  @end{short}
  If @arg{sibling} is @code{nil}, then the row will be prepended to the
  beginning of the list. @arg{iter} will point to this new row. The row will be
  empty after this function is called. To fill in values, you need to call the
  functions @fun{gtk-list-store-set} or @fun{gtk-list-store-set-value}.
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-list-store-set}
  @see-function{gtk-list-store-set-value}"
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-insert-after list-store iter sibling)
    iter))

(export 'gtk-list-store-insert-after)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_insert_with_values ()
;;; ----------------------------------------------------------------------------

(defun gtk-list-store-insert-with-values (list-store position &rest values)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[position]{position to insert the new row, or -1 to append after
    existing rows}
  @argument[values]{values to store in @arg{list-store}}
  @return{@code{iter} -- @class{gtk-tree-iter} to the new row}
  @begin{short}
    Creates a new row at position.
  @end{short}
  @arg{iter} will point to this new row. If @arg{position} is -1, or larger than
  the number of rows in the list, then the new row will be appended to the list.
  The row will be filled with the values given to this function.

  Calling the function @sym{gtk-list-store-insert-with-values} has the same
  effect as calling
  @begin{pre}
 (let ((iter (gtk-list-store-insert list-store position)))
   (gtk-list-store-set list-store iter  ...)
 )
  @end{pre}
  with the difference that the former will only emit a \"row-inserted\" signal,
  while the latter will emit \"row-inserted\", \"row-changed\" and, if the list
  store is sorted, \"rows-reordered\" signals. Since emitting the
  \"rows-reordered\" signal repeatedly can affect the performance of the
  program, the function @sym{gtk-list-store-insert-with-values} should generally
  be preferred when inserting rows in a sorted list store.
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-list-store-insert}
  @see-function{gtk-list-store-set}"
  (let ((n (length values))
        (iter (make-gtk-tree-iter)))
    (with-foreign-objects ((value-ar '(:struct g-value) n)
                           (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for type = (gtk-tree-model-column-type list-store i))
            (setf (mem-aref columns-ar :int i) i)
            (set-g-value (mem-aptr value-ar '(:struct g-value) i)
                         value
                         type
                         :zero-g-value t))
      (%gtk-list-store-insert-with-valuesv list-store
                                           iter
                                           position
                                           columns-ar
                                           value-ar
                                           n)
      (iter (for i from 0 below n)
            (g-value-unset (mem-aptr value-ar '(:struct g-value) i)))
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
;;;     An unset GtkTreeIter to set to the new row, or NULL. [out][allow-none]
;;;
;;; position :
;;;     position to insert the new row, or -1 for last
;;;
;;; columns :
;;;     an array of column numbers. [array length=n_values]
;;;
;;; values :
;;;     an array of GValues. [array length=n_values]
;;;
;;; n_values :
;;;     the length of the columns and values arrays
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;; Only for internal use. Not exported.

(defcfun ("gtk_list_store_insert_with_valuesv"
          %gtk-list-store-insert-with-valuesv) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (position :int)
  (columns :pointer)
  (values :pointer)
  (n-values :int))

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_prepend" %gtk-list-store-prepend) :void
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-list-store-prepend (list-store)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @return{@code{iter} -- a @class{gtk-tree-iter} to the prepended row}
  @begin{short}
    Prepends a new row to @arg{list-store}.
  @end{short}
  @arg{iter} will point to this new row. The row will be empty after this
  function is called. To fill in values, you need to call the functions
  @fun{gtk-list-store-set} or @fun{gtk-list-store-set-value}.
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-list-store-set}
  @see-function{gtk-list-store-set-value}"
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
 "@version{2014-1-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @return{@code{iter} -- a @class{gtk-tree-iter} to the appended row}
  @begin{short}
    Appends a new row to @arg{list-store}.
  @end{short}
  @arg{iter} will point to this new row. The row will be empty after this
  function is called. To fill in values, you need to call the functions
  @fun{gtk-list-store-set} or @fun{gtk-list-store-set-value}.
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-list-store-set}
  @see-function{gtk-list-store-set-value}"
  (let ((iter (make-gtk-tree-iter)))
    (%gtk-list-store-append list-store iter)
    iter))

(export 'gtk-list-store-append)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_clear ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_clear" gtk-list-store-clear) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  Removes all rows from the list store.
  @see-class{gtk-list-store}"
  (list-store (g-object gtk-list-store)))

(export 'gtk-list-store-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_iter_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_iter_is_valid" gtk-list-store-iter-is-valid) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[iter]{a @class{gtk-tree-iter}}
  @return{@em{True} if the @arg{iter} is valid, @code{nil} if the @arg{iter} is
    invalid.}
  @subheading{Warning}
    This function is slow. Only use it for debugging and/or testing purposes.

  @begin{short}
    Checks if the given @arg{iter} is a valid iterator for this
    @class{gtk-list-store}.
  @end{short}
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}"
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
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[a]{a @class{gtk-tree-iter}}
  @argument[b]{a @class{gtk-tree-iter}}
  @begin{short}
    Swaps @arg{a} and @arg{b} in @arg{list-store}.
  @end{short}
  Note that this function only works with unsorted stores.
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}"
  (list-store (g-object gtk-list-store))
  (a (g-boxed-foreign gtk-tree-iter))
  (b (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-swap)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_move_before ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_move_before" gtk-list-store-move-before) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[iter]{a @class{gtk-tree-iter}}
  @argument[position]{a @class{gtk-tree-iter}, or @code{nil}}
  @begin{short}
    Moves @arg{iter} in @arg{list-store} to the position before @arg{position}.
  @end{short}
  Note that this function only works with unsorted stores. If position is
  @code{nil}, @arg{iter} will be moved to the end of the list.
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-list-store-move-after}"
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (position (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-move-before)

;;; ----------------------------------------------------------------------------
;;; gtk_list_store_move_after ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_store_move_after" gtk-list-store-move-after) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[list-store]{a @class{gtk-list-store} object}
  @argument[iter]{a @class{gtk-tree-iter}}
  @argument[position]{a @class{gtk-tree-iter} or @code{nil}}
  @begin{short}
    Moves @arg{iter} in @arg{list-store} to the position after @arg{position}.
  @end{short}
  Note that this function only works with unsorted stores. If @arg{position} is
  @code{nil}, @arg{iter} will be moved to the start of the list.
  @see-class{gtk-list-store}
  @see-class{gtk-tree-iter}
  @see-function{gtk-list-store-move-before}"
  (list-store (g-object gtk-list-store))
  (iter (g-boxed-foreign gtk-tree-iter))
  (position (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-list-store-move-after)

;;; --- End of file gtk.list-store.lisp ----------------------------------------
