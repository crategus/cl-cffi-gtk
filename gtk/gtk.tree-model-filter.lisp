;;; ----------------------------------------------------------------------------
;;; gtk.tree-model-filter.lisp
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
;;; GtkTreeModelFilter
;;;
;;;     A GtkTreeModel which hides parts of an underlying tree model
;;;
;;; Types and Values
;;;
;;;     GtkTreeModelFilter
;;;
;;; Functions
;;;
;;;     GtkTreeModelFilterVisibleFunc
;;;     GtkTreeModelFilterModifyFunc
;;;
;;;     gtk_tree_model_filter_new
;;;     gtk_tree_model_filter_set_visible_func
;;;     gtk_tree_model_filter_set_modify_func
;;;     gtk_tree_model_filter_set_visible_column
;;;     gtk_tree_model_filter_get_model
;;;     gtk_tree_model_filter_convert_child_iter_to_iter
;;;     gtk_tree_model_filter_convert_iter_to_child_iter
;;;     gtk_tree_model_filter_convert_child_path_to_path
;;;     gtk_tree_model_filter_convert_path_to_child_path
;;;     gtk_tree_model_filter_refilter
;;;     gtk_tree_model_filter_clear_cache
;;;
;;; Properties
;;;
;;;     GtkTreeModel*  child-model     Read / Write / Construct Only
;;;      GtkTreePath*  virtual-root    Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTreeModelFilter
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTreeModelFilter implements GtkTreeModel and GtkTreeDragSource.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeModelFilter
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTreeModelFilter" gtk-tree-model-filter
  (:superclass g-object
   :export t
   :interfaces ("GtkTreeModel"
                "GtkTreeDragSource")
   :type-initializer "gtk_tree_model_filter_get_type")
  ((child-model
    gtk-tree-model-filter-child-model
    "child-model" "GtkTreeModel" t t)
   (virtual-root
    gtk-tree-model-filter-virtual-root
    "virtual-root" "GtkTreePath" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tree-model-filter 'type)
 "@version{2021-3-7}
  @begin{short}
    A @sym{gtk-tree-model-filter} object is a tree model which wraps another
    tree model.
  @end{short}
  The @sym{gtk-tree-model-filter} object can do the following things:
  @begin{itemize}
    @begin{item}
      Filter specific rows, based on data from a \"visible column\", a column
      storing booleans indicating whether the row should be filtered or not,
      or based on the return value of a \"visible function\", which gets a
      model, iterator and returns a boolean indicating whether the row should
      be filtered or not.
    @end{item}
    @begin{item}
      Modify the \"appearance\" of the model, using a modify function. This is
      extremely powerful and allows for just changing some values and also for
      creating a completely different model based on the given child model.
    @end{item}
    @begin{item}
      Set a different root node, also known as a \"virtual root\". You can pass
      in a @class{gtk-tree-path} instance indicating the root node for the
      filter at construction time.
    @end{item}
  @end{itemize}
  The basic API is similar to the @class{gtk-tree-model-sort} object. For an
  example on its usage, see the @class{gtk-tree-model-sort} documentation.

  When using the @sym{gtk-tree-model-filter} object, it is important to realize
  that the @sym{gtk-tree-model-filter} object maintains an internal cache of all
  nodes which are visible in its clients. The cache is likely to be a subtree of
  the tree exposed by the child model. The @sym{gtk-tree-model-filter} object
  will not cache the entire child model when unnecessary to not compromise the
  caching mechanism that is exposed by the reference counting scheme. If the
  child model implements reference counting, unnecessary signals may not be
  emitted because of reference counting rule 3, see the @class{gtk-tree-model}
  documentation. Note that e.g. the @class{gtk-tree-store} object does not
  implement reference counting and will always emit all signals, even when the
  receiving node is not visible.

  Because of this, limitations for possible visible functions do apply. In
  general, visible functions should only use data or properties from the node
  for which the visibility state must be determined, its siblings or its
  parents. Usually, having a dependency on the state of any child node is not
  possible, unless references are taken on these explicitly. When no such
  reference exists, no signals may be received for these child nodes. See
  reference couting rule number 3 in the @class{gtk-tree-model} documentation.

  Determining the visibility state of a given node based on the state of its
  child nodes is a frequently occurring use case. Therefore, the
  @sym{gtk-tree-model-filter} object explicitly supports this. For example, when
  a node does not have any children, you might not want the node to be visible.
  As soon as the first row is added to the node's child level, or the last row
  removed, the node's visibility should be updated.

  This introduces a dependency from the node on its child nodes. In order to
  accommodate this, the @sym{gtk-tree-model-filter} object must make sure the
  necesary signals are received from the child model. This is achieved by
  building, for all nodes which are exposed as visible nodes to
  the @sym{gtk-tree-model-filter} objects clients, the child level (if any) and
  take a reference on the first node in this level. Furthermore, for every
  \"row-inserted\", \"row-changed\" or \"row-deleted\" signal, also these which
  were not handled because the node was not cached, the
  @sym{gtk-tree-model-filter} oject will check if the visibility state of
  any parent node has changed.

  Beware, however, that this explicit support is limited to these two cases.
  For example, if you want a node to be visible only if two nodes in a child's
  child level (2 levels deeper) are visible, you are on your own. In this
  case, either rely on the @class{gtk-tree-store} object to emit all signals
  because it does not implement reference counting, or for models that do
  implement reference counting, obtain references on these child levels
  yourself.
  @see-slot{gtk-tree-model-filter-child-model}
  @see-slot{gtk-tree-model-filter-virtual-root}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-model-sort}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-tree-model-filter-child-model --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "child-model"
                                               'gtk-tree-model-filter) 't)
 "The @code{child-model} property of type @class{gtk-tree-model}
  (Read / Write / Construct) @br{}
  The model for the filter model to filter.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model-filter-child-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-model-filter-child-model 'function)
 "@version{2021-3-7}
  @syntax[]{(gtk-tree-model-filter-child-model object) => child-model}
  @syntax[]{(setf gtk-tree-model-filter-child-model object) child-model)}
  @argument[object]{a @class{gtk-tree-model-filter} object}
  @argument[child-model]{a @class{gtk-tree-model} object}
  @begin{short}
    Accessor of the @slot[gtk-tree-model-filter]{child-model} slot of the
    @class{gtk-tree-model-filter} class.
  @end{short}

  The model for the filter model to filter.
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-tree-model}")

;;; --- gtk-tree-model-filter-virtual-root -------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "virtual-root"
                                               'gtk-tree-model-filter) 't)
 "The @code{virtual-root} property of type @class{gtk-tree-path}
  (Read / Write / Construct) @br{}
  The virtual root, relative to the child model, for this filter model.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model-filter-virtual-root atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-model-filter-virtual-root 'function)
 "@version{2021-3-7}
  @syntax[]{(gtk-tree-model-filter-virtual-root object) => virtual-root}
  @syntax[]{(setf gtk-tree-model-filter-virtual-root object) virtual-root)}
  @argument[object]{a @class{gtk-tree-model-filter} object}
  @argument[virtual-root]{a @class{gtk-tree-path} instance}
  @begin{short}
    Accessor of the @slot[gtk-tree-model-filter]{virtual-root} slot of the
    @class{gtk-tree-model-filter} class.
  @end{short}

  The virtual root, relative to the child model, for this filter model.
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-tree-path}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_new" gtk-tree-model-filter-new)
    (g-object gtk-tree-model)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[child]{a @class{gtk-tree-model} object}
  @argument[root]{a @class{gtk-tree-path} instance or @code{nil}}
  @return{A new @class{gtk-tree-model} object.}
  @begin{short}
    Creates a new @class{gtk-tree-model} object, with @arg{child} as the child
    model and @arg{root} as the virtual root.
  @end{short}
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-tree-path}"
  (child (g-object gtk-tree-model))
  (root (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-model-filter-new)

;;; ----------------------------------------------------------------------------
;;; GtkTreeModelFilterVisibleFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-model-filter-visible-func :boolean
    ((model (g-object gtk-tree-model))
     (iter (g-boxed-foreign gtk-tree-iter))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn model iter)
      (return-true () t)
      (return-false () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model-filter-visible-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-tree-model-filter-visible-func atdoc:*external-symbols*)
 "@version{2021-2-3}
  @begin{short}
    A callback function which decides whether the row indicated by @arg{iter}
    is visible.
  @end{short}
  @begin{pre}
 lambda (model iter)
  @end{pre}
  @begin[code]{table}
    @entry[model]{The child model of the @class{gtk-tree-model-filter} object.}
    @entry[iter]{A @class{gtk-tree-iter} iterator pointing to the row in model
      whose visibility is determined.}
    @entry[Returns]{Whether the row indicated by @arg{iter} is visible.}
  @end{table}
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-tree-iter}
  @see-symbol{gtk-tree-model-filter-set-visible-func}")

(export 'gtk-tree-model-filter-visible-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_set_visible_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_set_visible_func"
          %gtk-tree-model-filter-set-visible-func) :void
  (filter (g-object gtk-tree-model-filter))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-model-filter-set-visible-func (filter func)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-2}
  @argument[filter]{a @class{gtk-tree-model-filter} object}
  @argument[func]{a @symbol{gtk-tree-model-filter-visible-func}, the visible
    callback function}
  @begin{short}
    Sets the visible function used when filtering the filter to be @arg{func}.
  @end{short}
  The function should return @em{true} if the given row should be visible and
  @em{false} otherwise.

  If the condition calculated by the function changes over time, e.g. because
  it depends on some global parameters, you must call the function
  @fun{gtk-tree-model-filter-refilter} to keep the visibility information of
  the model uptodate.

  Note that @arg{func} is called whenever a row is inserted, when it may still
  be empty. The visible function should therefore take special care of empty
  rows.
  @see-class{gtk-tree-model-filter}
  @see-symbol{gtk-tree-model-filter-visible-func}
  @see-function{gtk-tree-model-filter-refilter}"
  (%gtk-tree-model-filter-set-visible-func
              filter
              (callback gtk-tree-model-filter-visible-func)
              (allocate-stable-pointer func)
              (callback stable-pointer-destroy-notify)))

(export 'gtk-tree-model-filter-set-visible-func)

;;; ----------------------------------------------------------------------------
;;; GtkTreeModelFilterModifyFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-model-filter-modify-func :void
    ((model (g-object gtk-tree-model))
     (iter (g-boxed-foreign gtk-tree-iter))
     (value (:pointer (:struct g-value)))
     (column :int)
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (funcall fn model iter value column)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model-filter-modify-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-tree-model-filter-modify-func atdoc:*external-symbols*)
 "@version{2021-3-7}
  @begin{short}
    A callback function which calculates display values from raw values in the
    model.
  @end{short}
  It must fill @arg{value} with the display value for the column @arg{column}
  in the row indicated by @arg{iter}.

  Since this function is called for each data access, it is not a particularly
  efficient operation.
  @begin{pre}
 lambda (model iter value column)
  @end{pre}
  @begin[code]{table}
    @entry[model]{The @class{gtk-tree-model-filter} object.}
    @entry[iter]{A @class{gtk-tree-iter} iterator pointing to the row whose
      display values are determined.}
    @entry[value]{A @symbol{g-value} which is already initialized for with the
      correct type for the column @arg{column}.}
    @entry[column]{An integer with the column whose display value is
      determined.}
  @end{table}
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-tree-iter}
  @see-symbol{g-value}
  @see-symbol{gtk-tree-model-filter-set-modify-func}")

(export 'gtk-tree-model-filter-modify-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_set_modify_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_set_modify_func"
          %gtk-tree-model-filter-set-modify-func) :void
  (filter (g-object gtk-tree-model-filter))
  (n-columns :int)
  (types :pointer)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-model-filter-set-modify-func (filter column-types func)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[filter]{a @class{gtk-tree-model-filter} object}
  @argument[column-types]{a list of @class{g-type} types of the columns}
  @argument[func]{a @symbol{gtk-tree-model-filter-modify-func} callback
    function}
  @begin{short}
    With @arg{types} parameters, you give a list of column types for this model,
    which will be exposed to the parent model/view.
  @end{short}
  The @arg{func} parameter specifies the modify function. The modify function
  will get called for each data access, the goal of the modify function is to
  return the data which should be displayed at the location specified using the
  parameters of the modify function.
  @see-class{gtk-tree-model-filter}
  @see-symbol{gtk-tree-model-filter-modify-func}"
  (let ((n (length column-types)))
    (with-foreign-object (types-ar 'g-type n)
      (iter (for i from 0 below n)
            (for gtype in column-types)
            (setf (mem-aref types-ar 'g-type i) gtype))
      (%gtk-tree-model-filter-set-modify-func
                  filter
                  n
                  types-ar
                  (callback gtk-tree-model-filter-modify-func)
                  (allocate-stable-pointer func)
                  (callback stable-pointer-destroy-notify)))))

(export 'gtk-tree-model-filter-set-modify-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_set_visible_column ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_set_visible_column"
           gtk-tree-model-filter-set-visible-column) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[filter]{a @class{gtk-tree-model-filter} object}
  @argument[column]{an integer which is the column containing the visible
    information}
  @begin{short}
    Sets @arg{column} of the child model to be the column where @arg{filter}
    should look for visibility information.
  @end{short}
  The column should be of type \"gboolean\", where @em{true} means that a row
  is visible, and @em{false} if not.
  @see-class{gtk-tree-model-filter}"
  (filter (g-object gtk-tree-model-filter))
  (column :int))

(export 'gtk-tree-model-filter-set-visible-column)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_get_model () -> gtk-tree-model-filter-model
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_get_model" gtk-tree-model-filter-model)
    (g-object gtk-tree-model)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[filter]{a @class{gtk-tree-model-filter} object}
  @return{A @class{gtk-tree-model} object.}
  @begin{short}
    Returns the child model of the filter.
  @end{short}
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-tree-model}"
  (filter (g-object gtk-tree-model-filter)))

(export 'gtk-tree-model-filter-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_convert_child_iter_to_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_convert_child_iter_to_iter"
          %gtk-tree-model-filter-convert-child-iter-to-iter) :boolean
  (filter (g-object gtk-tree-model-filter))
  (filter-iter (g-boxed-foreign gtk-tree-iter))
  (child-iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-filter-convert-child-iter-to-iter (filter child-iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[filter]{a @class{gtk-tree-model-filter} object}
  @argument[child-iter]{a valid @class{gtk-tree-iter} iterator pointing to a
    row on the child model}
  @begin{return}
    A @class{gtk-tree-iter} iterator if @arg{child-iter} is a valid iterator
    pointing to a visible row in child model.
  @end{return}
  @begin{short}
    Returns an interator to point to the row in @arg{filter} that corresponds
    to the row pointed at by @arg{child-iter}.
  @end{short}
  If the iterator was not set, @code{nil} is returned.
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-tree-iter}"
  (let ((filter-iter (make-instance 'gtk-tree-iter)))
    (when (%gtk-tree-model-filter-convert-child-iter-to-iter filter
                                                             filter-iter
                                                             child-iter)
      filter-iter)))

(export 'gtk-tree-model-filter-convert-child-iter-to-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_convert_iter_to_child_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_convert_iter_to_child_iter"
          %gtk-tree-model-filter-convert-iter-to-child-iter) :void
  (filter (g-object gtk-tree-model-filter))
  (child-iter (g-boxed-foreign gtk-tree-iter))
  (filter-iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-filter-convert-iter-to-child-iter (filter filter-iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[filter]{a @class{gtk-tree-model-filter} object}
  @argument[filter-iter]{a valid @class{gtk-tree-iter} iterator pointing to a
    row on @arg{filter}}
  @begin{return}
    A @class{gtk-tree-iter} iterator.
  @end{return}
  @begin{short}
    Returns the iterator to point to the row pointed to by @arg{filter-iter}.
  @end{short}
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-tree-iter}"
  (let ((child-iter (make-instance 'gtk-tree-iter)))
    (%gtk-tree-model-filter-convert-iter-to-child-iter filter
                                                       child-iter
                                                       filter-iter)
    child-iter))

(export 'gtk-tree-model-filter-convert-iter-to-child-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_convert_child_path_to_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_convert_child_path_to_path"
           gtk-tree-model-filter-convert-child-path-to-path)
    (g-boxed-foreign gtk-tree-path :return)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[filter]{a @class{gtk-tree-model-filter} object}
  @argument[child-path]{a @class{gtk-tree-path} instance to convert}
  @return{A newly allocated @class{gtk-tree-path} instance, or @code{nil}.}
  @begin{short}
    Converts @arg{child-path} to a path relative to filter.
  @end{short}
  That is, @arg{child-path} points to a path in the child model. The returned
  path will point to the same row in the filtered model. If @arg{child-path} is
  not a valid path on the child model or points to a row which is not visible in
  @arg{filter}, then @code{nil} is returned.
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-tree-path}"
  (filter (g-object gtk-tree-model-sort))
  (child-path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-model-filter-convert-child-path-to-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_convert_path_to_child_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_convert_path_to_child_path"
          gtk-tree-model-filter-convert-path-to-child-path)
    (g-boxed-foreign gtk-tree-path :return)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[filter]{a @class{gtk-tree-model-filter} object}
  @argument[filter-path]{a @class{gtk-tree-path} instance to convert}
  @return{A newly allocated @class{gtk-tree-path} instance, or @code{nil}.}
  @begin{short}
    Converts @arg{filter-path} to a path on the child model of @arg{filter}.
  @end{short}
  That is, @arg{filter-path} points to a location in @arg{filter}. The returned
  path will point to the same location in the model not being filtered. If
  @arg{filter-path} does not point to a location in the child model, @code{nil}
  is returned.
  @see-class{gtk-tree-model-filter}
  @see-class{gtk-tree-path}"
  (filter (g-object gtk-tree-model-sort))
  (filter-path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-model-filter-convert-path-to-child-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_refilter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_refilter" gtk-tree-model-filter-refilter) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[filter]{a @class{gtk-tree-model-filter} object}
  @begin{short}
    Emits the signal \"row_changed\" for each row in the child model, which
    causes the filter to re-evaluate whether a row is visible or not.
  @end{short}
  @see-class{gtk-tree-model-filter}"
  (filter (g-object gtk-tree-model-filter)))

(export 'gtk-tree-model-filter-refilter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_filter_clear_cache ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_filter_clear_cache" gtk-tree-model-filter-clear-cache)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-7}
  @argument[filter]{a @class{gtk-tree-model-filter} object}
  @begin{short}
    This function clears the filter of any cached iterators that have not been
    reffed with the function @fun{gtk-tree-model-ref-node}.
  @end{short}
  This function should almost never be called.

  This might be useful if the child model being filtered is static, does not
  change often, and there has been a lot of unreffed access to nodes. As a side
  effect of this function, all unreffed iterators will be invalid.
  @see-class{gtk-tree-model-filter}
  @see-function{gtk-tree-model-ref-node}"
  (filter (g-object gtk-tree-model-filter)))

(export 'gtk-tree-model-filter-clear-cache)

;;; --- End of file gtk.tree-model-filter.lisp ---------------------------------
