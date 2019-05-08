;;; ----------------------------------------------------------------------------
;;; gtk.tree-sortable.lisp
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
;;; GtkTreeSortable
;;;
;;;     The interface for sortable models used by GtkTreeView
;;;
;;; Types and Values
;;;
;;;     GtkTreeSortable
;;;     GtkTreeSortableIface
;;;
;;;     GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
;;;     GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID
;;;
;;; Functions
;;;
;;;     GtkTreeIterCompareFunc
;;;
;;;     gtk_tree_sortable_sort_column_changed
;;;     gtk_tree_sortable_get_sort_column_id
;;;     gtk_tree_sortable_set_sort_column_id
;;;     gtk_tree_sortable_set_sort_func
;;;     gtk_tree_sortable_set_default_sort_func
;;;     gtk_tree_sortable_has_default_sort_func
;;;
;;; Signals
;;;
;;;     void   sort-column-changed    Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkTreeSortable
;;;
;;; Prerequisites
;;;
;;;     GtkTreeSortable requires GtkTreeModel and GObject.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeSortable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkTreeSortable" gtk-tree-sortable
  (:export t
   :type-initializer "gtk_tree_sortable_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-sortable atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-tree-sortable 'type)
 "@version{2013-6-21}
  @begin{short}
    @sym{gtk-tree-sortable} is an interface to be implemented by tree models
    which support sorting. The @class{gtk-tree-view} uses the methods provided
    by this interface to sort the model.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"sort-column-changed\" signal}
      @begin{pre}
 lambda (sortable)    : Run Last
      @end{pre}
      The \"sort-column-changed\" signal is emitted when the sort column or sort
      order of @arg{sortable} is changed. The signal is emitted before the
      contents of @arg{sortable} are resorted.
      @begin[code]{table}
        @entry[sortable]{The object on which the signal is emitted.}
      @end{table}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeSortableIface
;;;
;;; struct GtkTreeSortableIface {
;;;   GTypeInterface g_iface;
;;;
;;;   /* signals */
;;;   void     (* sort_column_changed)  (GtkTreeSortable        *sortable);
;;;
;;;   /* virtual table */
;;;   gboolean (* get_sort_column_id)   (GtkTreeSortable        *sortable,
;;;                                      gint                   *sort_column_id,
;;;                                      GtkSortType            *order);
;;;   void     (* set_sort_column_id)   (GtkTreeSortable        *sortable,
;;;                                      gint                    sort_column_id,
;;;                                      GtkSortType             order);
;;;   void     (* set_sort_func)        (GtkTreeSortable        *sortable,
;;;                                      gint                    sort_column_id,
;;;                                      GtkTreeIterCompareFunc  sort_func,
;;;                                      gpointer                user_data,
;;;                                      GDestroyNotify          destroy);
;;;   void     (* set_default_sort_func) (GtkTreeSortable        *sortable,
;;;                                       GtkTreeIterCompareFunc  sort_func,
;;;                                       gpointer                user_data,
;;;                                       GDestroyNotify          destroy);
;;;   gboolean (* has_default_sort_func) (GtkTreeSortable        *sortable);
;;; };
;;; ----------------------------------------------------------------------------

(define-vtable ("GtkTreeSortable" gtk-tree-sortable)
  (:skip parent-instance (:pointer (:struct g-type-interface)))
  ;; signal
  (:skip sort-columns-changed :pointer)
  ;; methods
  (get-sort-column-id (:boolean (sortable (g-object gtk-tree-sortable))
                                (sort-column-id (:pointer :int))
                                (order (:pointer gtk-sort-type)))
    :impl-call ((sortable)
                (multiple-value-bind (sorted-p r-sort-column-id r-order)
                    (gtk-tree-sortable-get-sort-column-id-impl sortable)
                  (unless (null-pointer-p sort-column-id)
                    (setf (mem-ref sort-column-id :int) r-sort-column-id))
                  (unless (null-pointer-p order)
                    (setf (mem-ref order 'gtk-sort-type) r-order))
                  sorted-p)))
  (set-sort-column-id (:void (sortable (g-object gtk-tree-sortable))
                             (sort-column-id :int)
                             (order gtk-sort-type)))
  (set-sort-func (:void (sortable (g-object gtk-tree-sortable))
                        (sort-column-id :int)
                        (func :pointer)
                        (data :pointer)
                        (destroy-notify :pointer)))
  (set-default-sort-func (:void (sortable (g-object gtk-tree-sortable))
                                (func :pointer)
                                (data :pointer)
                                (destroy-notify :pointer)))
  (has-default-sort-func (:boolean (sortable (g-object gtk-tree-sortable)))))

;;; ----------------------------------------------------------------------------
;;; GtkTreeIterCompareFunc ()
;;;
;;; gint (*GtkTreeIterCompareFunc) (GtkTreeModel *model,
;;;                                 GtkTreeIter *a,
;;;                                 GtkTreeIter *b,
;;;                                 gpointer user_data);
;;;
;;; A GtkTreeIterCompareFunc should return a negative integer, zero, or a
;;; positive integer if a sorts before b, a sorts with b, or a sorts after b
;;; respectively. If two iters compare as equal, their order in the sorted model
;;; is undefined. In order to ensure that the GtkTreeSortable behaves as
;;; expected, the GtkTreeIterCompareFunc must define a partial order on the
;;; model, i.e. it must be reflexive, antisymmetric and transitive.
;;;
;;; For example, if model is a product catalogue, then a compare function for
;;; the "price" column could be one which returns price_of(a) - price_of(b).
;;;
;;; model :
;;;     The GtkTreeModel the comparison is within
;;;
;;; a :
;;;     A GtkTreeIter in model
;;;
;;; b :
;;;     Another GtkTreeIter in model
;;;
;;; user_data :
;;;     Data passed when the compare func is assigned e.g. by
;;;     gtk_tree_sortable_set_sort_func()
;;;
;;; Returns :
;;;     a negative integer, zero or a positive integer depending on whether a
;;;     sorts before, with or after b
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_sort_column_changed ()
;;;
;;; void gtk_tree_sortable_sort_column_changed (GtkTreeSortable *sortable);
;;;
;;; Emits a "sort-column-changed" signal on sortable.
;;;
;;; sortable :
;;;     A GtkTreeSortable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_get_sort_column_id ()
;;; ----------------------------------------------------------------------------

;; TODO: The implementation does not look at the return value.
;;
;; See the documentation:
;; It returns TRUE unless the sort_column_id is
;; GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID or
;; GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID.
;;
;; Returns : TRUE if the sort column is not one of the special sort column ids.

(defcfun ("gtk_tree_sortable_get_sort_column_id"
          %gtk-tree-sortable-get-sort-column-id) :boolean
  (sortable (g-object gtk-tree-sortable))
  (sort-column-id (:pointer :int))
  (order (:pointer gtk-sort-type)))

(defun gtk-tree-sortable-get-sort-column-id (sortable)

 #+cl-cffi-gtk-documentation
 "@version{2019-5-6}
  @argument[sortable]{a @class{gtk-tree-sortable} object}
  @begin{return}
    @arg{sort-column-id} -- the sort column id @br{}
    @arg{order} -- the @symbol{gtk-sort-type}
  @end{return}
  @begin{short}
    Returns the current sort column and the order.
  @end{short}
  @see-class{gtk-tree-sortable}"
  (with-foreign-objects ((sort-column-id :int) (order 'gtk-sort-type))
    (%gtk-tree-sortable-get-sort-column-id sortable sort-column-id order)
    (values (mem-ref sort-column-id :int)
            (mem-ref order 'gtk-sort-type))))

(export 'gtk-tree-sortable-get-sort-column-id)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_set_sort_column_id ()
;;;
;;; void gtk_tree_sortable_set_sort_column_id (GtkTreeSortable *sortable,
;;;                                            gint sort_column_id,
;;;                                            GtkSortType order);
;;;
;;; Sets the current sort column to be sort_column_id. The sortable will resort
;;; itself to reflect this change, after emitting a "sort-column-changed"
;;; signal. sort_column_id may either be a regular column id, or one of the
;;; following special values:
;;;
;;; GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
;;;     the default sort function will be used, if it is set
;;;
;;; GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID
;;;     no sorting will occur
;;;
;;; sortable :
;;;     A GtkTreeSortable
;;;
;;; sort_column_id :
;;;     the sort column id to set
;;;
;;; order :
;;;     The sort order of the column
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_set_sort_func ()
;;;
;;; void gtk_tree_sortable_set_sort_func (GtkTreeSortable *sortable,
;;;                                       gint sort_column_id,
;;;                                       GtkTreeIterCompareFunc sort_func,
;;;                                       gpointer user_data,
;;;                                       GDestroyNotify destroy);
;;;
;;; Sets the comparison function used when sorting to be sort_func. If the
;;; current sort column id of sortable is the same as sort_column_id, then the
;;; model will sort using this function.
;;;
;;; sortable :
;;;     A GtkTreeSortable
;;;
;;; sort_column_id :
;;;     the sort column id to set the function for
;;;
;;; sort_func :
;;;     The comparison function
;;;
;;; user_data :
;;;     User data to pass to sort_func, or NULL.
;;;
;;; destroy :
;;;     Destroy notifier of user_data, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_set_default_sort_func ()
;;;
;;; void gtk_tree_sortable_set_default_sort_func
;;;                                           (GtkTreeSortable *sortable,
;;;                                            GtkTreeIterCompareFunc sort_func,
;;;                                            gpointer user_data,
;;;                                            GDestroyNotify destroy);
;;;
;;; Sets the default comparison function used when sorting to be sort_func. If
;;; the current sort column id of sortable is
;;; GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, then the model will sort using
;;; this function.
;;;
;;; If sort_func is NULL, then there will be no default comparison function.
;;; This means that once the model has been sorted, it can't go back to the
;;; default state. In this case, when the current sort column id of sortable is
;;; GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, the model will be unsorted.
;;;
;;; sortable :
;;;     A GtkTreeSortable
;;;
;;; sort_func :
;;;     The comparison function
;;;
;;; user_data :
;;;     User data to pass to sort_func, or NULL.
;;;
;;; destroy :
;;;     Destroy notifier of user_data, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_sortable_has_default_sort_func ()
;;;
;;; gboolean gtk_tree_sortable_has_default_sort_func (GtkTreeSortable *sortable)
;;;
;;; Returns TRUE if the model has a default sort function. This is used
;;; primarily by GtkTreeViewColumns in order to determine if a model can go back
;;; to the default state, or not.
;;;
;;; sortable :
;;;     A GtkTreeSortable
;;;
;;; Returns :
;;;     TRUE, if the model has a default sort function
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.tree-sortable.lisp -------------------------------------
