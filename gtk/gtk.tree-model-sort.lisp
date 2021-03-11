;;; ----------------------------------------------------------------------------
;;; gtk.tree-model-sort.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkTreeModelSort
;;;
;;;     A GtkTreeModel which makes an underlying tree model sortable
;;;
;;; Types and Values
;;;
;;;     GtkTreeModelSort
;;;
;;; Functions
;;;
;;;     gtk_tree_model_sort_new_with_model
;;;     gtk_tree_model_sort_get_model                      Accessor
;;;     gtk_tree_model_sort_convert_child_path_to_path
;;;     gtk_tree_model_sort_convert_child_iter_to_iter
;;;     gtk_tree_model_sort_convert_path_to_child_path
;;;     gtk_tree_model_sort_convert_iter_to_child_iter
;;;     gtk_tree_model_sort_reset_default_sort_func
;;;     gtk_tree_model_sort_clear_cache
;;;     gtk_tree_model_sort_iter_is_valid
;;;
;;; Properties
;;;
;;;     GtkTreeModel*  model    Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTreeModelSort
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTreeModelSort implements GtkTreeModel, GtkTreeSortable and
;;;     GtkTreeDragSource.
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
 "@version{2021-3-10}
  @begin{short}
    The @sym{gtk-tree-model-sort} object is a model which implements the
    @class{gtk-tree-sortable} interface.
  @end{short}
  It does not hold any data itself, but rather is created with a child model
  and proxies its data. It has identical column types to this child model, and
  the changes in the child are propagated. The primary purpose of this model is
  to provide a way to sort a different model without modifying it. Note that
  the sort function used by the @sym{gtk-tree-model-sort} object is not
  guaranteed to be stable.

  The use of this is best demonstrated through an example. In the following
  sample code we create two @class{gtk-tree-view} widgets each with a view of
  the same data. As the model is wrapped here by a @sym{gtk-tree-model-sort}
  object, the two @class{gtk-tree-view} widgets can each sort their view of the
  data without affecting the other. By contrast, if we simply put the same
  model in each widget, then sorting the first would sort the second.

  @b{Example:} Using a @sym{gtk-tree-model-sort} object
  @begin{pre}
(let* (;; Get the child model
       (child-model (gtk-my-model()))
       ;; Create the first tree view
       (sort-model1 (gtk-tree-model-sort-new-with-model child-model))
       (tree-view1 (gtk-tree-view-with-model sort-model1))
       ;; Create the second tree view
       (sort-model2 (gtk-tree-vmodel-sort-new-with-model child-model))
       (tree-view2 (gtk-tree-view-new-with-model sort-model2)))
  ;; Now we can sort the two models independently
  (setf (gtk-tree-sortable-sort-column-id sort-model1) col-1)
  (setf (gtk-tree-sortable-sort-column-id sort-model1) '(col1 :descending))
  ... )
  @end{pre}
  To demonstrate how to access the underlying child model from the sort model,
  the next example will be a callback for the @class{gtk-tree-selection}
  \"changed\" signal. In this callback, we get a string from @code{COLUMN_1} of
  the model. We then modify the string, find the same selected row on the child
  model, and change the row there.

  @b{Example:} Accessing the child model in a selection changed callback
  @begin{pre}
(defun selection-changed (selection)
  (let* ((view (gtk-tree-selection-tree-view selection))
         ;; Get the current selected row and the model
         (sort-model (gtk-tree-view-model view))
         (sort-iter (gtk-tree-selection-selected selection))
         ;; Look up the current value on the selected row and get a new value
         (value (gtk-tree-model-value sort-model sort-iter col-1))
         (new-value (change-the-value value))
         ;; Get the child model and an iterator on the child model
         (model (gtk-tree-model-sort-model sort-model))
         (iter (gtk-tree-model-sort-convert-iter-to-child-iter sort-model
                                                               sort-iter)))
    ;; Change the value of the row in the child model
    (gtk-list-store-set-value model iter col-1 new-value)))
  @end{pre}
  @see-slot{gtk-tree-model-sort-model}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-sortable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model"
                                               'gtk-tree-model-sort) 't)
 "The @code{model} property of type @class{gtk-tree-model}
  (Read / Write / Construct) @br{}
  The model to sort.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model-sort-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-model-sort-model 'function)
 "@version{2021-3-10}
  @syntax[]{(gtk-tree-model-sort-model object) => model}
  @argument[object]{a @class{gtk-tree-model-sort} object}
  @argument[model]{the @class{gtk-tree-model} child model being sorted}
  @begin{short}
    Accessor of the @slot[gtk-tree-model-sort]{model} slot of the
    @class{gtk-tree-model-sort} class.
  @end{short}

  The slot access function @sym{gtk-tree-model-sort-model} returns the model
  the @class{gtk-tree-model-sort} object is sorting.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-model-sort}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_new_with_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-model-sort-new-with-model))

(defun gtk-tree-model-sort-new-with-model (model)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-10}
  @argument[model]{a @class{gtk-tree-model} object}
  @return{A new @class{gtk-tree-model} object.}
  @begin{short}
    Creates a new tree model, with @arg{model} as the child model.
  @end{short}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-model-sort}"
  (make-instance 'gtk-tree-model-sort
                 :model model))

(export 'gtk-tree-model-sort-new-with-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_child_path_to_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_convert_child_path_to_path"
           gtk-tree-model-sort-convert-child-path-to-path)
    (g-boxed-foreign gtk-tree-path :return)
 #+cl-cffi-gtk-documentation
 "@version{2020-3-10}
  @argument[model]{a @class{gtk-tree-model-sort} object}
  @argument[path]{a @class{gtk-tree-path} instance to convert}
  @return{A @class{gtk-tree-path} instance, or @code{nil}.}
  @begin{short}
    Converts @arg{path} to a path relative to @arg{model}.
  @end{short}
  That is, @arg{path} points to a path in the child model. The returned path
  will point to the same row in the sorted model. If @arg{path} is not a valid
  path on the child model, then @code{nil} is returned.
  @see-class{gtk-tree-model-sort}
  @see-class{gtk-tree-path}"
  (model (g-object gtk-tree-model-sort))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-model-sort-convert-child-path-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_child_iter_to_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_convert_child_iter_to_iter"
          %gtk-tree-model-sort-convert-child-iter-to-iter) :boolean
  (model (g-object gtk-tree-model-sort))
  (sort-iter (g-boxed-foreign gtk-tree-iter))
  (child-iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-sort-convert-child-iter-to-iter (model iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-10}
  @argument[model]{a @class{gtk-tree-model-sort} object}
  @argument[iter]{a valid @class{gtk-tree-iter} instance pointing to a
    row on the child model}
  @begin{return}
    A valid @class{gtk-tree-iter} iterator to a visible row in the sorted model,
    or @code{nil}.
  @end{return}
  @begin{short}
    Returns the iterator to the row in @arg{model} that corresponds to the row
    pointed at by @arg{iter}.
  @end{short}
  @see-class{gtk-tree-model-sort}
  @see-class{gtk-tree-iter}"
  (let ((sort-iter (make-gtk-tree-iter)))
    (when (%gtk-tree-model-sort-convert-child-iter-to-iter model
                                                           sort-iter
                                                           iter)
      sort-iter)))

(export 'gtk-tree-model-sort-convert-child-iter-to-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_path_to_child_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_convert_path_to_child_path"
           gtk-tree-model-sort-convert-path-to-child-path)
    (g-boxed-foreign gtk-tree-path :return)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-10}
  @argument[model]{a @class{gtk-tree-model-sort} object}
  @argument[path]{a @class{gtk-tree-path} instance to convert}
  @return{A @class{gtk-tree-path} instance, or @code{nil}.}
  @begin{short}
    Converts @arg{path} to a path on the child model of @arg{model}.
  @end{short}
  That is, @arg{path} points to a location in @arg{model}. The returned path
  will point to the same location in the model not being sorted. If @arg{path}
  does not point to a location in the child model, @code{nil} is returned.
  @see-class{gtk-tree-model-sort}
  @see-class{gtk-tree-path}"
  (model (g-object gtk-tree-model-sort))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-model-sort-convert-path-to-child-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_convert_iter_to_child_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_convert_iter_to_child_iter"
          %gtk-tree-model-sort-convert-iter-to-child-iter) :void
  (model (g-object tree-model-sort))
  (child-iter (g-boxed-foreign gtk-tree-iter))
  (sorted-iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-sort-convert-iter-to-child-iter (model iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-10}
  @argument[model]{a @class{gtk-tree-model-sort} object}
  @argument[iter]{a valid @class{gtk-tree-iter} iterator pointing to a
    row on @arg{model}}
  @begin{return}
    A @class{gtk-tree-iter} iterator.
  @end{return}
  @begin{short}
  Converts @arg{iter} to point to a row on @arg{model}.
  @end{short}
  @see-class{gtk-tree-model-sort}
  @see-class{gtk-tree-iter}"
  (let ((child-iter (make-gtk-tree-iter)))
    (%gtk-tree-model-sort-convert-iter-to-child-iter model child-iter iter)
    child-iter))

(export 'gtk-tree-model-sort-convert-iter-to-child-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_reset_default_sort_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_reset_default_sort_func"
          gtk-tree-model-sort-reset-default-sort-func)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-10}
  @argument[model]{a @class{gtk-tree-model-sort} object}
  @begin{short}
    This resets the default sort function to be in the 'unsorted' state.
  @end{short}
  That is, it is in the same order as the child model. It will re-sort the
  model to be in the same order as the child model only if the
  @class{gtk-tree-model-sort} object is in 'unsorted' state.
  @see-class{gtk-tree-model-sort}"
  (model (g-object gtk-tree-model-sort)))

(export 'gtk-tree-model-sort-reset-default-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_clear_cache ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_clear_cache" gtk-tree-model-sort-clear-cache)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-10}
  @argument[model]{a @class{gtk-tree-model-sort} object}
  @begin{short}
    This function should almost never be called. It clears the @arg{model} of
    any cached iterators that have not been reffed with the function
    @fun{gtk-tree-model-ref-node}.
  @end{short}
  This might be useful if the child model being sorted is static (and does not
  change often) and there has been a lot of unreffed access to nodes. As a side
  effect of this function, all unreffed iters will be invalid.
  @see-class{gtk-tree-model-sort}
  @see-function{gtk-tree-model-ref-node}"
  (model (g-object gtk-tree-model-sort)))

(export 'gtk-tree-model-sort-clear-cache)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_sort_iter_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_sort_iter_is_valid" gtk-tree-model-sort-iter-is-valid)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-10}
  @argument[model]{a @class{gtk-tree-model-sort} object}
  @argument[iter]{a @class{gtk-tree-iter} iterator}
  @return{@em{True} if @arg{iter} is valid, @code{nil} if @arg{iter} is
    invalid.}
  @begin{short}
    Checks if the given @arg{iter} is a valid iter for this
    @class{gtk-tree-model-sort} object.
  @end{short}
  @begin[Warning]{dictionary}
    This function is slow. Only use it for debugging and/or testing purposes.
  @end{dictionary}
  @see-class{gtk-tree-model-sort}
  @see-class{gtk-tree-iter}"
  (model (g-object gtk-tree-model-sort))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-sort-iter-is-valid)

;;; --- End of file gtk.tree-model-sort.lisp -----------------------------------
