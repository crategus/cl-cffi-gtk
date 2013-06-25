;;; ----------------------------------------------------------------------------
;;; gtk.tree-model-sort.lisp
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
;;; GtkTreeModelSort
;;;
;;; A GtkTreeModel which makes an underlying tree model sortable
;;;
;;; Synopsis
;;;
;;;     GtkTreeModelSort
;;;
;;;     gtk_tree_model_sort_new_with_model
;;;     gtk_tree_model_sort_get_model
;;;     gtk_tree_model_sort_convert_child_path_to_path
;;;     gtk_tree_model_sort_convert_child_iter_to_iter
;;;     gtk_tree_model_sort_convert_path_to_child_path
;;;     gtk_tree_model_sort_convert_iter_to_child_iter
;;;     gtk_tree_model_sort_reset_default_sort_func
;;;     gtk_tree_model_sort_clear_cache
;;;     gtk_tree_model_sort_iter_is_valid
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeModelSort
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTreeModelSort" gtk-tree-model-sort
  (:superclass g-object
   :export t
   :interfaces ("GtkTreeModel"
                "GtkTreeSortable"
                "GtkTreeDragSource")
   :type-initializer "gtk_tree_model_sort_get_type")
  ((model
    gtk-tree-model-sort-model
    "model" "GtkTreeModel" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tree-model-sort 'type)
 "@version{2013-6-21}
  @begin{short}
    The @sym{gtk-tree-model-sort} is a model which implements the
    @class{gtk-tree-sortable} interface. It does not hold any data itself, but
    rather is created with a child model and proxies its data. It has identical
    column types to this child model, and the changes in the child are
    propagated. The primary purpose of this model is to provide a way to sort a
    different model without modifying it. Note that the sort function used by
    @sym{gtk-tree-model-sort} is not guaranteed to be stable.
  @end{short}

  The use of this is best demonstrated through an example. In the following
  sample code we create two @class{gtk-tree-view} widgets each with a view of
  the same data. As the model is wrapped here by a @sym{gtk-tree-model-sort},
  the two @class{gtk-tree-view}'s can each sort their view of the data without
  affecting the other. By contrast, if we simply put the same model in each
  widget, then sorting the first would sort the second.

  @b{Example:} Using a @sym{gtk-tree-model-sort}
  @begin{pre}
   {
     GtkTreeView *tree_view1;
     GtkTreeView *tree_view2;
     GtkTreeModel *sort_model1;
     GtkTreeModel *sort_model2;
     GtkTreeModel *child_model;

     // get the child model
     child_model = get_my_model ();

     // Create the first tree
     sort_model1 = gtk_tree_model_sort_new_with_model (child_model);
     tree_view1 = gtk_tree_view_new_with_model (sort_model1);

     // Create the second tree
     sort_model2 = gtk_tree_model_sort_new_with_model (child_model);
     tree_view2 = gtk_tree_view_new_with_model (sort_model2);

     // Now we can sort the two models independently
     gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (sort_model1),
                                           COLUMN_1, GTK_SORT_ASCENDING);
     gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (sort_model2),
                                           COLUMN_1, GTK_SORT_DESCENDING);
   @}
  @end{pre}
  To demonstrate how to access the underlying child model from the sort model,
  the next example will be a callback for the @class{gtk-tree-selection}
  \"changed\" signal. In this callback, we get a string from @code{COLUMN_1} of
  the model. We then modify the string, find the same selected row on the child
  model, and change the row there.

  @b{Example:} Accessing the child model of in a selection changed callback
  @begin{pre}
   void
   selection_changed (GtkTreeSelection *selection, gpointer data)
   {
     GtkTreeModel *sort_model = NULL;
     GtkTreeModel *child_model;
     GtkTreeIter sort_iter;
     GtkTreeIter child_iter;
     char *some_data = NULL;
     char *modified_data;

     // Get the current selected row and the model.
     if (! gtk_tree_selection_get_selected (selection,
                                            &sort_model,
                                            &sort_iter))
       return;

     /* Look up the current value on the selected row and get a new value
      * to change it to.
      */
     gtk_tree_model_get (GTK_TREE_MODEL (sort_model), &sort_iter,
                        COLUMN_1, &some_data,
                         -1);

     modified_data = change_the_data (some_data);
     g_free (some_data);

     // Get an iterator on the child model, instead of the sort model.
     gtk_tree_model_sort_convert_iter_to_child_iter
                                           (GTK_TREE_MODEL_SORT (sort_model),
                                            &child_iter,
                                            &sort_iter);

     /* Get the child model and change the value of the row.  In this
      * example, the child model is a GtkListStore.  It could be any other
      * type of model, though.
      */
     child_model = gtk_tree_model_sort_get_model
                                          (GTK_TREE_MODEL_SORT (sort_model));
     gtk_list_store_set (GTK_LIST_STORE (child_model), &child_iter,
                         COLUMN_1, &modified_data,
                         -1);
     g_free (modified_data);
   @}
  @end{pre}
  @see-slot{gtk-tree-model-sort-model}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model"
                                               'gtk-tree-model-sort) 't)
 "The @code{\"model\"} property of type @class{gtk-tree-model}
  (Read / Write / Construct) @br{}
  The model for the @sym{gtk-tree-model-sort} to sort.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model-sort-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-model-sort-model 'function)
 "@version{2013-6-21}
  Accessor of the slot @code{\"model\"} of the @class{gtk-tree-model-sort}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_new_with_model ()
;;;
;;; GtkTreeModel * gtk_tree_model_sort_new_with_model
;;;                                                 (GtkTreeModel *child_model);
;;;
;;; Creates a new GtkTreeModel, with child_model as the child model.
;;;
;;; child_model :
;;;     A GtkTreeModel
;;;
;;; Returns :
;;;     A new GtkTreeModel.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_get_model ()
;;;
;;; GtkTreeModel * gtk_tree_model_sort_get_model (GtkTreeModelSort *tree_model);
;;;
;;; Returns the model the GtkTreeModelSort is sorting.
;;;
;;; tree_model :
;;;     a GtkTreeModelSort
;;;
;;; Returns :
;;;     the "child model" being sorted
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_child_path_to_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_convert_child_path_to_path"
           gtk-tree-model-sort-convert-child-path-to-path)
    (g-boxed-foreign gtk-tree-path :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[tree-model-sort]{a @class{gtk-tree-model-sort} object}
  @argument[child-path]{a @class{gtk-tree-path} structure to convert}
  @return{A newly allocated @class{gtk-tree-path} structure, or @code{nil}.}
  @begin{short}
    Converts @arg{child-path} to a path relative to @arg{tree-model-sort}.
  @end{short}
  That is, @arg{child-path} points to a path in the child model. The returned
  path will point to the same row in the sorted model. If @arg{child-path} is
  not a valid path on the child model, then @code{nil} is returned."
  (tree-model-sort (g-object gtk-tree-model-sort))
  (child-path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-model-sort-convert-child-path-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_child_iter_to_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_convert_child_iter_to_iter"
          %gtk-tree-model-sort-convert-child-iter-to-iter) :boolean
  (tree-model-sort (g-object gtk-tree-model-sort))
  (sort-iter (g-boxed-foreign gtk-tree-iter))
  (child-iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-sort-convert-child-iter-to-iter (tree-model-sort
                                                       child-iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[tree-model-sort]{a @class{gtk-tree-model-sort} object}
  @argument[child-iter]{a valid @class{gtk-tree-iter} structure pointing to a
    row on the child model}
  @begin{return}
    @code{sort-iter} -- a valid iterator to a visible row in the child model,
    or @code{nil}
  @end{return}
  @begin{short}
    Sets @arg{sort-iter} to point to the row in @arg{tree-model-sort} that
    corresponds to the row pointed at by @arg{child-iter}. If @arg{sort-iter}
    was not set, @code{nil} is returned.
  @end{short}
  Note: A boolean is only returned since 2.14."
  (let ((sort-iter (make-gtk-tree-iter)))
    (when (%gtk-tree-model-sort-convert-child-iter-to-iter tree-model-sort
                                                           sort-iter
                                                           child-iter)
      sort-iter)))

(export 'gtk-tree-model-sort-convert-child-iter-to-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_path_to_child_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_convert_path_to_child_path"
           gtk-tree-model-sort-convert-path-to-child-path)
    (g-boxed-foreign gtk-tree-path :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[tree-model-sort]{a @class{gtk-tree-model-sort} object}
  @argument[sorted-path]{a @class{gtk-tree-path} structure to convert}
  @return{A newly allocated @class{gtk-tree-path} structure, or @code{nil}.}
  @begin{short}
    Converts @arg{sorted-path} to a path on the child model of
    @arg{tree-model-sort}.
  @end{short}
  That is, @arg{sorted-path} points to a location in @arg{tree-model-sort}. The
  returned path will point to the same location in the model not being sorted.
  If @arg{sorted-path} does not point to a location in the child model,
  @code{nil} is returned."
  (tree-model-sort (g-object gtk-tree-model-sort))
  (sorted-path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-model-sort-convert-path-to-child-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_iter_to_child_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_convert_iter_to_child_iter"
          %gtk-tree-model-sort-convert-iter-to-child-iter) :void
  (tree-model-sort (g-object tree-model-sort))
  (child-iter (g-boxed-foreign gtk-tree-iter))
  (sorted-iter (g-boxed-foreign gtk-tree-iter)))

(defun tree-model-sort-convert-iter-to-child-iter (tree-model-sort sorted-iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[tree-model-sort]{a @class{gtk-tree-model-sort} object}
  @argument[sorted-iter]{a valid @class{gtk-tree-iter} structure pointing to a
    row on @arg{tree-model-sort}}
  @begin{return}
    @code{child-iter} -- a @class{gtk-tree-iter} structure
  @end{return}
  Sets @arg{child-iter} to point to the row pointed to by @arg{sorted-iter}."
  (let ((child-iter (make-gtk-tree-iter)))
    (%gtk-tree-model-sort-convert-iter-to-child-iter tree-model-sort
                                                     child-iter
                                                     sorted-iter)
    child-iter))

(export 'gtk-tree-model-sort-convert-iter-to-child-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_reset_default_sort_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_reset_default_sort_func"
          gtk-tree-model-sort-reset-default-sort-func)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[tree-model-sort]{a @class{gtk-tree-model-sort} object}
  @begin{short}
    This resets the default sort function to be in the 'unsorted' state.
  @end{short}
  That is, it is in the same order as the child model. It will re-sort the model
  to be in the same order as the child model only if the
  @class{gtk-tree-model-sort} is in 'unsorted' state."
  (tree-model-sort (g-object gtk-tree-model-sort)))

(export 'gtk-tree-model-sort-reset-default-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_clear_cache ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_clear_cache" gtk-tree-model-sort-clear-cache)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[tree-model-sort]{a @class{gtk-tree-model-sort} object}
  @begin{short}
    This function should almost never be called. It clears the
    @arg{tree-model-sort} of any cached iterators that have not been reffed with
    the function @fun{gtk-tree-model-ref-node}.
  @end{short}
  This might be useful if the child model being sorted is static (and does not
  change often) and there has been a lot of unreffed access to nodes. As a side
  effect of this function, all unreffed iters will be invalid.
  @see-function{gtk-tree-model-ref-node}"
  (tree-model-sort (g-object gtk-tree-model-sort)))

(export 'gtk-tree-model-sort-clear-cache)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_iter_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_iter_is_valid" gtk-tree-model-sort-iter-is-valid)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-6-21}
  @argument[tree-model-sort]{a @class{gtk-tree-model-sort} object}
  @argument[iter]{a @class{gtk-tree-iter} structure}
  @return{@em{True} if the @arg{iter} is valid, @code{nil} if the @arg{iter} is
    invalid.}
  @subheading{Warning}
    This function is slow. Only use it for debugging and/or testing purposes.

  @begin{short}
    Checks if the given @arg{iter} is a valid iter for this
    @class{gtk-tree-model-sort} object.
  @end{short}

  Since 2.2"
  (tree-model-sort (g-object gtk-tree-model-sort))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-sort-iter-is-valid)

;;; --- End of file gtk.tree-model-sort.lisp -----------------------------------
