;;; ----------------------------------------------------------------------------
;;; gtk.tree-model.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
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
;;; GtkTreeModel
;;;
;;;     The tree interface used by GtkTreeView
;;;
;;; Types and Values
;;;
;;;     GtkTreeModel
;;;     GtkTreeIter
;;;     GtkTreePath
;;;     GtkTreeRowReference
;;;     GtkTreeModelIface
;;;     GtkTreeModelFlags
;;;
;;; Functions
;;;
;;;     GtkTreeModelForeachFunc
;;;
;;;     gtk_tree_path_new
;;;     gtk_tree_path_new_from_string
;;;     gtk_tree_path_new_from_indices
;;;     gtk_tree_path_new_from_indicesv
;;;     gtk_tree_path_to_string
;;;     gtk_tree_path_new_first
;;;     gtk_tree_path_append_index
;;;     gtk_tree_path_prepend_index
;;;     gtk_tree_path_get_depth
;;;     gtk_tree_path_get_indices
;;;     gtk_tree_path_get_indices_with_depth
;;;     gtk_tree_path_free
;;;     gtk_tree_path_copy
;;;     gtk_tree_path_compare
;;;     gtk_tree_path_next
;;;     gtk_tree_path_prev
;;;     gtk_tree_path_up
;;;     gtk_tree_path_down
;;;     gtk_tree_path_is_ancestor
;;;     gtk_tree_path_is_descendant
;;;     gtk_tree_row_reference_new
;;;     gtk_tree_row_reference_new_proxy
;;;     gtk_tree_row_reference_get_model
;;;     gtk_tree_row_reference_get_path
;;;     gtk_tree_row_reference_valid
;;;     gtk_tree_row_reference_free
;;;     gtk_tree_row_reference_copy
;;;     gtk_tree_row_reference_inserted
;;;     gtk_tree_row_reference_deleted
;;;     gtk_tree_row_reference_reordered
;;;     gtk_tree_iter_copy
;;;     gtk_tree_iter_free
;;;     gtk_tree_model_get_flags
;;;     gtk_tree_model_get_n_columns
;;;     gtk_tree_model_get_column_type
;;;     gtk_tree_model_get_iter
;;;     gtk_tree_model_get_iter_from_string
;;;     gtk_tree_model_get_iter_first
;;;     gtk_tree_model_get_path
;;;     gtk_tree_model_get_value
;;;     gtk_tree_model_iter_next
;;;     gtk_tree_model_iter_previous
;;;     gtk_tree_model_iter_children
;;;     gtk_tree_model_iter_has_child
;;;     gtk_tree_model_iter_n_children
;;;     gtk_tree_model_iter_nth_child
;;;     gtk_tree_model_iter_parent
;;;     gtk_tree_model_get_string_from_iter
;;;     gtk_tree_model_ref_node
;;;     gtk_tree_model_unref_node
;;;     gtk_tree_model_get
;;;     gtk_tree_model_get_valist
;;;     gtk_tree_model_foreach
;;;     gtk_tree_model_row_changed
;;;     gtk_tree_model_row_inserted
;;;     gtk_tree_model_row_has_child_toggled
;;;     gtk_tree_model_row_deleted
;;;     gtk_tree_model_rows_reordered
;;;     gtk_tree_model_rows_reordered_with_length
;;;
;;; Signals
;;;
;;;     void    row-changed              Run Last
;;;     void    row-deleted              Run First
;;;     void    row-has-child-toggled    Run Last
;;;     void    row-inserted             Run First
;;;     void    rows-reordered           Run First
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ├── GtkTreeIter
;;;     ╰── GtkTreePath
;;;
;;;     GInterface
;;;     ╰── GtkTreeModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeIter
;;; ----------------------------------------------------------------------------

(define-foreign-type pointer-as-integer-foreign-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser pointer-as-integer))

(defmethod translate-to-foreign (value (type pointer-as-integer-foreign-type))
  (make-pointer value))

(defmethod translate-from-foreign (value (type pointer-as-integer-foreign-type))
  (pointer-address value))

;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct gtk-tree-iter "GtkTreeIter"
  (stamp :int :initform 0)
  (user-data pointer-as-integer :initform 0)
  (user-data-2 pointer-as-integer :initform 0)
  (user-data-3 pointer-as-integer :initform 0))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-iter atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'gtk-tree-iter 'type)
 "@version{2021-3-3}
  @begin{short}
    The @sym{gtk-tree-iter} structure is the primary structure for accessing a
    @class{gtk-tree-model} object. Models are expected to put a unique integer
    in the @arg{stamp} member, and put model specific data in the three
    @arg{user-data} members.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct gtk-tree-iter \"GtkTreeIter\"
  (stamp :int :initform 0)
  (user-data pointer-as-integer :initform 0)
  (user-data-2 pointer-as-integer :initform 0)
  (user-data-3 pointer-as-integer :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[stamp]{A unique stamp to catch invalid iterators.}
    @entry[user-data]{Model specific data.}
    @entry[user-data-2]{Model specific data.}
    @entry[user-data-3]{Model specific data.}
  @end{table}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-path}")

(export 'gtk-tree-iter)

;;; ----------------------------------------------------------------------------
;;; Accessors of the slots of the GtkTreeIter structure
;;; ----------------------------------------------------------------------------

;; not exported

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-iter-stamp atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-iter-stamp 'function)
 "@version{2020-6-8}
  @begin{short}
    Accessor of the @code{stamp} slot of the @class{gtk-tree-iter} structure.
  @end{short}
  @see-class{gtk-tree-iter}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-iter-user-data atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-iter-user-data 'function)
 "@version{2020-6-8}
  @begin{short}
    Accessor of the @code{user-data} slot of the @class{gtk-tree-iter}
    structure.
  @end{short}
  @see-class{gtk-tree-iter}")

;;; ----------------------------------------------------------------------------
;;; GtkTreePath
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "gtk_tree_path_get_type" g-size))

(define-g-boxed-opaque gtk-tree-path "GtkTreePath"
  :alloc (%gtk-tree-path-new))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-path atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'gtk-tree-path 'type)
 "@version{2021-3-4}
  @short{}
  @begin{pre}
(define-g-boxed-opaque gtk-tree-path \"GtkTreePath\"
  :alloc (%gtk-tree-path-new))
  @end{pre}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}")

(export 'gtk-tree-path)

;;; ----------------------------------------------------------------------------
;;; GtkTreeRowReference
;;; ----------------------------------------------------------------------------

(glib-init::at-init ()
  (foreign-funcall "gtk_tree_row_reference_get_type" g-size))

(define-g-boxed-opaque gtk-tree-row-reference "GtkTreeRowReference"
  :alloc (error "GtkTreeRowReference cannot be created from the Lisp side."))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-row-reference atdoc:*class-name-alias*)
      "GBoxed"
      (documentation 'gtk-tree-row-reference 'type)
 "@version{2021-3-4}
  @begin{short}
    A @sym{gtk-tree-row-reference} instance tracks model changes so that it
    always refers to the same row, a @class{gtk-tree-path} instance refers to a
    position, not a fixed row.
  @end{short}
  Create a new @sym{gtk-tree-row-reference} instance with the function
    @fun{gtk-tree-row-reference-new}.
  @begin{pre}
(define-g-boxed-opaque gtk-tree-row-reference \"GtkTreeRowReference\"
  :alloc (error \"GtkTreeRowReference cannot be created from the Lisp side.\"))
  @end{pre}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-row-reference-new}")

(export 'gtk-tree-row-reference)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_new" %gtk-tree-path-new) :pointer)

(defcfun ("gtk_tree_path_new" gtk-tree-path-new) (g-boxed-foreign gtk-tree-path)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @return{A newly created @class{gtk-tree-path} instance.}
  @short{Creates a new  tree path.}
  @see-class{gtk-tree-path}")

(export 'gtk-tree-path-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_new_from_string" gtk-tree-path-new-from-string)
    (g-boxed-foreign gtk-tree-path)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[pathstr]{the string representation of a path}
  @return{A newly created @class{gtk-tree-path}, or @code{nil}.}
  @short{Creates a tree path initialized to @arg{pathstr}.}

  The argument @arg{pathstr} is expected to be a colon separated list of
  numbers. For example, the string \"10:4:0\" would create a path of depth 3
  pointing to the 11th child of the root node, the 5th child of that 11th child,
  and the 1st child of that 5th child. If an invalid path string is passed in,
  @code{nil} is returned.
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-new}
  @see-function{gtk-tree-path-new-from-indices}
  @see-function{gtk-tree-path-to-string}"
  (pathstr :string))

(export 'gtk-tree-path-new-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_from_indices ()
;;; ----------------------------------------------------------------------------

(defun gtk-tree-path-new-from-indices (&rest indices)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[indices]{list of integers}
  @return{A newly created @class{gtk-tree-path} instance.}
  @begin{short}
    Creates a new tree path with @arg{indices} as indices.
  @end{short}
  @see-class{gtk-tree-path}"
  (gtk-tree-path-new-from-string
      (string-right-trim ":" (format nil "~{~D:~}" indices))))

(export 'gtk-tree-path-new-from-indices)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_from_indicesv ()
;;;
;;; GtkTreePath *
;;; gtk_tree_path_new_from_indicesv (gint *indices,
;;;                                  gsize length);
;;;
;;; Creates a new path with the given indices array of length .
;;;
;;; indices :
;;;     array of indices.
;;;
;;; length :
;;;     length of indices array
;;;
;;; Returns :
;;;     A newly created GtkTreePath
;;;
;;; Since 3.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_to_string" gtk-tree-path-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @return{A string with the representation of the tree path.}
  @short{Generates a string representation of the tree path.}
  This string is a ':' separated list of numbers. For example, \"4:10:0:3\"
  would be an acceptable return value for this string.
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-from-string}"
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_first ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_new_first" gtk-tree-path-new-first)
    (g-boxed-foreign gtk-tree-path)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @return{A new @class{gtk-tree-path} instance.}
  @short{Creates a new tree path.}
  The string representation of this tree path is \"0\".
  @see-class{gtk-tree-path}")

(export 'gtk-tree-path-new-first)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_append_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_append_index" %gtk-tree-path-append-index) :void
  (path (g-boxed-foreign gtk-tree-path))
  (index :int))

;; We dot not modify the argument, but return the new value.

(defun gtk-tree-path-append-index (path index)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @argument[index]{an integer with the index}
  @return{The @class{gtk-tree-path} instance.}
  @short{Appends a new index to the tree path.}
  As a result, the depth of @arg{path} is increased.
  @see-class{gtk-tree-path}"
  (let ((path-new (gtk-tree-path-copy path)))
    (%gtk-tree-path-append-index path-new index)
    path-new))

(export 'gtk-tree-path-append-index)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_prepend_index ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_prepend_index" %gtk-tree-path-prepend-index) :void
  (path (g-boxed-foreign gtk-tree-path))
  (index :int))

;; We dot not modify the argument, but return the new value.

(defun gtk-tree-path-prepend-index (path index)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @argument[index]{an integer with the index}
  @return{The @class{gtk-tree-path} instance.}
  @short{Prepends a new index to the tree path.}
  As a result, the depth of @arg{path} is increased.
  @see-class{gtk-tree-path}"
  (let ((path (gtk-tree-path-copy path)))
    (%gtk-tree-path-prepend-index path index)
    path))

(export 'gtk-tree-path-prepend-index)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_get_depth () -> gtk-tree-path-depth
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_get_depth" gtk-tree-path-depth) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @return{An integer with the depth of @arg{path}.}
  @short{Returns the current depth of the tree path.}
  @see-class{gtk-tree-path}"
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-depth)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_get_indices () -> gtk-tree-path-indices
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_get_indices" %gtk-tree-path-indices) (:pointer :int)
  (path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-indices (path)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @return{The current indices, or @code{nil}.}
  @short{Returns the current indices of the tree path.}
  This is a list of integers, each representing a node in a tree.

  The length of the list can be obtained with the function
  @fun{gtk-tree-path-depth}.
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-depth}"
  (let ((n (gtk-tree-path-depth path))
        (indices (%gtk-tree-path-indices path)))
    (loop for i from 0 below n
          collect (mem-aref indices :int i))))

(export 'gtk-tree-path-indices)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_get_indices_with_depth ()
;;;
;;; gint * gtk_tree_path_get_indices_with_depth (GtkTreePath *path, gint *depth)
;;;
;;; Returns the current indices of path.
;;;
;;; This is an array of integers, each representing a node in a tree. It also
;;; returns the number of elements in the array. The array should not be freed.
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; depth :
;;;     return location for number of elements returned in the integer array, or
;;;     NULL
;;;
;;; Returns :
;;;     The current indices, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;; not needed

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_free ()
;;; ----------------------------------------------------------------------------

;; We do not export this function.

(defcfun ("gtk_tree_path_free" gtk-tree-path-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[path]{a @class{gtk-tree-path} object}
  @short{Frees path. If path is NULL, it simply returns.}"
  (path (g-boxed-foreign gtk-tree-path)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_copy" gtk-tree-path-copy)
    (g-boxed-foreign gtk-tree-path)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @return{A new @class{gtk-tree-path} instance.}
  @short{Creates a new tree path as a copy of @arg{path}.}
  @see-class{gtk-tree-path}"
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_compare ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_compare" gtk-tree-path-compare ) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path1]{a @class{gtk-tree-path} object}
  @argument[path2]{a @class{gtk-tree-path} object to compare with}
  @return{The relative positions of @arg{path1} and @arg{path2}.}
  @short{Compares two paths.}
  If @arg{path1} appears before @arg{path2} in a tree, then -1 is returned. If
  @arg{path2} appears before @arg{path1}, then 1 is returned. If the two nodes
  are equal, then 0 is returned.
  @see-class{gtk-tree-path}"
  (path1 (g-boxed-foreign gtk-tree-path))
  (path2 (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_next ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_next" %gtk-tree-path-next) :void
  (path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-next (path)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @return{The new @class{gtk-tree-path} instance.}
  @short{Moves @arg{path} to point to the next node at the current depth.}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-prev}"
  (let ((path-new (gtk-tree-path-copy path)))
    (%gtk-tree-path-next path-new)
    path-new))

(export 'gtk-tree-path-next)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_prev ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_prev" %gtk-tree-path-prev) :boolean
  (path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-prev (path)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @return{A @class{gtk-tree-path} instance to point to the previous node,
    if it exists, otherwise @code{nil}.}
  @begin{short}
    Moves the tree path to point to the previous node at the current depth,
    if it exists.
  @end{short}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-next}"
  (let ((path-new (gtk-tree-path-copy path)))
    (when (%gtk-tree-path-prev path-new)
      path-new)))

(export 'gtk-tree-path-prev)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_up ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_up" %gtk-tree-path-up) :boolean
  (path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-up (path)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @return{A @class{gtk-tree-path} instance to point to the parent node, if it
    has a parent, otherwise @code{nil}.}
  @begin{short}
    Moves the tree path to point to its parent node, if it has a parent.
  @end{short}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-down}"
  (let ((path-new (gtk-tree-path-copy path)))
    (when (%gtk-tree-path-up path-new)
      path-new)))

(export 'gtk-tree-path-up)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_down ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_down" %gtk-tree-path-down) :void
  (path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-down (path)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @begin{short}
    Moves @arg{path} to point to the first child of the current tree path.
  @end{short}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-up}"
  (let ((path-new (gtk-tree-path-copy path)))
    (%gtk-tree-path-down path-new)
    path-new))

(export 'gtk-tree-path-down)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_is_ancestor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_is_ancestor" gtk-tree-path-is-ancestor) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @argument[descendant]{another @class{gtk-tree-path} instance}
  @return{@em{True} if @arg{descendant} is contained inside @arg{path}.}
  @begin{short}
    Returns @em{true} if @arg{descendant} is a descendant of @arg{path}.
  @end{short}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-is-descendant}"
  (path (g-boxed-foreign gtk-tree-path))
  (descendant (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-is-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_is_descendant ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_is_descendant" gtk-tree-path-is-descendant) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[path]{a @class{gtk-tree-path} instance}
  @argument[ancestor]{another @class{gtk-tree-path} instance}
  @return{@em{True} if @arg{ancestor} contains @arg{path} somewhere below it.}
  @begin{short}
    Returns @em{true} if @arg{path} is a descendant of @arg{ancestor}.
  @end{short}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-is-ancestor}"
  (path (g-boxed-foreign gtk-tree-path))
  (ancestor (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-is-descendant)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_row_reference_new" gtk-tree-row-reference-new)
    (g-boxed-foreign gtk-tree-row-reference :return)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-28}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[path]{a valid @class{gtk-tree-path} object to monitor}
  @return{A newly allocated @class{gtk-tree-row-reference}, or @code{nil}.}
  @short{Creates a row reference based on @arg{path}.}

  This reference will keep pointing to the node pointed to by @arg{path}, so
  long as it exists. Any changes that occur on @arg{model} are propagated, and
  @arg{path} is updated appropriately. If @arg{path} is not a valid path in
  @arg{model}, then @code{nil} is returned.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-row-reference}"
  (model (g-object gtk-tree-model))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-row-reference-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_new_proxy ()
;;;
;;; GtkTreeRowReference * gtk_tree_row_reference_new_proxy (GObject *proxy,
;;;                                                         GtkTreeModel *model,
;;;                                                         GtkTreePath *path);
;;;
;;; You do not need to use this function.
;;;
;;; Creates a row reference based on path.
;;;
;;; This reference will keep pointing to the node pointed to by path, so long as
;;; it exists. If path isn't a valid path in model, then NULL is returned.
;;; However, unlike references created with gtk_tree_row_reference_new(), it
;;; does not listen to the model for changes. The creator of the row reference
;;; must do this explicitly using gtk_tree_row_reference_inserted(),
;;; gtk_tree_row_reference_deleted(), gtk_tree_row_reference_reordered().
;;;
;;; These functions must be called exactly once per proxy when the corresponding
;;; signal on the model is emitted. This single call updates all row references
;;; for that proxy. Since built-in GTK objects like GtkTreeView already use
;;; this mechanism internally, using them as the proxy object will produce
;;; unpredictable results. Further more, passing the same object as model and
;;; proxy does not work for reasons of internal implementation.
;;;
;;; This type of row reference is primarily meant by structures that need to
;;; carefully monitor exactly when a row reference updates itself, and is not
;;; generally needed by most applications.
;;;
;;; proxy :
;;;     a proxy GObject
;;;
;;; model :
;;;     a GtkTreeModel
;;;
;;; path :
;;;     a valid GtkTreePath to monitor
;;;
;;; Returns :
;;;     a newly allocated GtkTreeRowReference, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_get_model () -> gtk-tree-row-reference-model
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-tree-row-reference
                              %gtk-tree-row-reference-model
  :reader "gtk_tree_row_reference_get_model"
  :type (g-object gtk-tree-model))

(declaim (inline gtk-tree-row-reference-model))

(defun gtk-tree-row-reference-model (reference)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-28}
  @argument[reference]{a @class{gtk-tree-row-reference} object}
  @return{The @class{gtk-tree-model} object.}
  @short{Returns the model that the row reference is monitoring.}
  @see-class{gtk-tree-row-reference}"
  (%gtk-tree-row-reference-model reference))

(export 'gtk-tree-row-reference-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_get_path ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-tree-row-reference
                              %gtk-tree-row-reference-path
  :reader "gtk_tree_row_reference_get_path"
  :type (g-boxed-foreign gtk-tree-path :return))

(declaim (inline gtk-tree-row-reference-path))

(defun gtk-tree-row-reference-path (reference)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-28}
  @argument[reference]{a @class{gtk-tree-row-reference} object}
  @return{A current @class{gtk-tree-path} object, or @code{nil}.}
  @begin{short}
    Returns a path that the row reference currently points to, or @code{nil} if
    the path pointed to is no longer valid.
  @end{short}
  @see-class{gtk-tree-row-reference}"
  (%gtk-tree-row-reference-path reference))

(export 'gtk-tree-row-reference-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_valid ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-tree-row-reference
                              %gtk-tree-row-reference-valid
  :reader "gtk_tree_row_reference_valid" :type :boolean)

(declaim (inline gtk-tree-row-reference-valid))

(defun gtk-tree-row-reference-valid (reference)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-28}
  @argument[reference]{a @class{gtk-tree-row-reference}, or @code{nil}}
  @return{@em{True} if @arg{reference} points to a valid path.}
  @begin{short}
    Returns @em{true} if @arg{reference} is non-@code{nil} and refers to a
    current valid path.
  @end{short}
  @see-class{gtk-tree-row-reference}"
  (%gtk-tree-row-reference-valid reference))

(export 'gtk-tree-row-reference-valid)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_free ()
;;;
;;; void gtk_tree_row_reference_free (GtkTreeRowReference *reference);
;;;
;;; Free's reference. reference may be NULL
;;;
;;; reference :
;;;     a GtkTreeRowReference, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_row_reference_copy" gtk-tree-row-reference-copy)
    (g-boxed-foreign gtk-tree-row-reference)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[reference]{a @class{gtk-tree-row-reference} instance}
  @return{A @class{gtk-tree-row-reference} instance.}
  @begin{short}
    Copies a tree row reference.
  @end{short}
  @see-class{gtk-tree-row-reference}"
  (reference (g-boxed-foreign gtk-tree-row-reference)))

(export 'gtk-tree-row-reference-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_inserted ()
;;;
;;; void gtk_tree_row_reference_inserted (GObject *proxy, GtkTreePath *path);
;;;
;;; Lets a set of row reference created by gtk_tree_row_reference_new_proxy()
;;; know that the model emitted the "row-inserted" signal.
;;;
;;; proxy :
;;;     a GObject
;;;
;;; path :
;;;     the row position that was inserted
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_deleted ()
;;;
;;; void gtk_tree_row_reference_deleted (GObject *proxy, GtkTreePath *path);
;;;
;;; Lets a set of row reference created by gtk_tree_row_reference_new_proxy()
;;; know that the model emitted the "row-deleted" signal.
;;;
;;; proxy :
;;;     a GObject
;;;
;;; path :
;;;     the path position that was deleted
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_reordered ()
;;;
;;; void gtk_tree_row_reference_reordered (GObject *proxy,
;;;                                        GtkTreePath *path,
;;;                                        GtkTreeIter *iter,
;;;                                        gint *new_order);
;;;
;;; Lets a set of row reference created by gtk_tree_row_reference_new_proxy()
;;; know that the model emitted the "rows-reordered" signal.
;;;
;;; proxy :
;;;     a GObject
;;;
;;; path :
;;;     the parent path of the reordered signal
;;;
;;; iter :
;;;     the iter pointing to the parent of the reordered
;;;
;;; new_order :
;;;     the new order of rows
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_iter_copy ()
;;;
;;; GtkTreeIter * gtk_tree_iter_copy (GtkTreeIter *iter);
;;;
;;; Creates a dynamically allocated tree iterator as a copy of iter.
;;;
;;; This function is not intended for use in applications, because you can just
;;; copy the structs by value (GtkTreeIter new_iter = iter;). You must free
;;; this iter with gtk_tree_iter_free().
;;;
;;; iter :
;;;     a GtkTreeIter
;;;
;;; Returns :
;;;     a newly-allocated copy of iter
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_iter_free ()
;;;
;;; void gtk_tree_iter_free (GtkTreeIter *iter);
;;;
;;; Frees an iterator that has been allocated by gtk_tree_iter_copy().
;;;
;;; This function is mainly used for language bindings.
;;;
;;; iter :
;;;     a dynamically allocated tree iterator
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkTreeModelFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkTreeModelFlags" gtk-tree-model-flags
  (:export t
   :type-initializer "gtk_tree_model_flags_get_type")
  (:iters-persist 1)
  (:list-only 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model-flags atdoc:*symbol-name-alias*)
      "Flags"
      (gethash 'gtk-tree-model-flags atdoc:*external-symbols*)
 "@version{2020-6-8}
  @begin{short}
    These flags indicate various properties of a @class{gtk-tree-model}.
  @end{short}

  They are returned by the function @fun{gtk-tree-model-flags}, and must be
  static for the lifetime of the object. A more complete description of
  @code{:iters-persist} can be found in the overview of this section.
  @begin{pre}
(define-g-flags \"GtkTreeModelFlags\" gtk-tree-model-flags
  (:export t
   :type-initializer \"gtk_tree_model_flags_get_type\")
  (:iters-persist 1)
  (:list-only 2))
  @end{pre}
  @begin[code]{table}
    @entry[:iters-persist]{Iterators survive all signals emitted by the tree.}
    @entry[:list-only]{The model is a list only, and never has children.}
  @end{table}
  @see-class{gtk-tree-model}
  @see-function{gtk-tree-model-flags}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeModel
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkTreeModel" gtk-tree-model
  (:export t
   :type-initializer "gtk_tree_model_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model atdoc:*class-name-alias*)
      "Interface"
      (documentation 'gtk-tree-model 'type)
 "@version{2021-2-3}
  @begin{short}
    The @sym{gtk-tree-model} interface defines a generic tree interface for use
    by the @class{gtk-tree-view} widget.
  @end{short}
  It is an abstract interface, and is designed to be usable with any appropriate
  data structure. The programmer just has to implement this interface on their
  own data type for it to be viewable by a @class{gtk-tree-view} widget.

  The model is represented as a hierarchical tree of strongly-typed, columned
  data. In other words, the model can be seen as a tree where every node has
  different values depending on which column is being queried. The type of
  data found in a column is determined by using the GType system (i.e.
  \"gint\", \"GtkButton\", \"gpointer\", etc). The types are homogeneous per
  column across all nodes. It is important to note that this interface only
  provides a way of examining a model and observing changes. The implementation
  of each individual model decides how and if changes are made.

  In order to make life simpler for programmers who do not need to write their
  own specialized model, two generic models are provided - the
  @class{gtk-tree-store} and the @class{gtk-list-store} classes. To use these,
  the developer simply pushes data into these models as necessary. These models
  provide the data structure as well as all appropriate tree interfaces. As a
  result, implementing drag and drop, sorting, and storing data is trivial. For
  the vast majority of trees and lists, these two models are sufficient.

  Models are accessed on a node/column level of granularity. One can query for
  the value of a model at a certain node and a certain column on that node.
  There are two structures used to reference a particular node in a model.
  They are the @class{gtk-tree-path} and the @class{gtk-tree-iter} structures.
  Most of the interface consists of operations on a @class{gtk-tree-iter}
  iterator.

  A path is essentially a potential node. It is a location on a model that may
  or may not actually correspond to a node on a specific model. The
  @class{gtk-tree-path} structure can be converted into either an array of
  unsigned integers or a string. The string form is a list of numbers separated
  by a colon. Each number refers to the offset at that level. Thus, the path
  '0' refers to the root node and the path '2:4' refers to the fifth child of
  the third node.

  By contrast, a @class{gtk-tree-iter} iterator is a reference to a specific
  node on a specific model. It is a generic structure with an integer and three
  generic pointers. These are filled in by the model in a model-specific way.
  One can convert a path to an iterator by calling the function
  @fun{gtk-tree-model-iter}. These iterators are the primary way of accessing a
  model and are similar to the iterators used by the @class{gtk-text-buffer}
  class. They are generally statically allocated on the stack and only used for
  a short time. The model interface defines a set of operations using them for
  navigating the model.

  It is expected that models fill in the iterator with private data. For
  example, the @class{gtk-list-store} model, which is internally a simple
  linked list, stores a list node in one of the pointers. The
  @class{gtk-tree-model-sort} class stores an array and an offset in two of the
  pointers. Additionally, there is an integer field. This field is generally
  filled with a unique stamp per model. This stamp is for catching errors
  resulting from using invalid iterators with a model.

  The lifecycle of an iterator can be a little confusing at first. Iterators
  are expected to always be valid for as long as the model is unchanged (and
  does not emit a signal). The model is considered to own all outstanding
  iterators and nothing needs to be done to free them from the user's point of
  view. Additionally, some models guarantee that an iterator is valid for as
  long as the node it refers to is valid (most notably the
  @class{gtk-tree-store} and @class{gtk-list-store} models). Although generally
  uninteresting, as one always has to allow for the case where iterators do not
  persist beyond a signal, some very important performance enhancements were
  made in the sort model. As a result, the @code{:iters-persist} flag was added
  to indicate this behavior.

  To show some common operation of a model, some examples are provided. The
  first example shows three ways of getting the iterator at the location
  '3:2:5'. While the first method shown is easier, the second is much more
  common, as you often get paths from callbacks.

  @b{Example:} Acquiring a @class{gtk-tree-iter} iterator
  @begin{pre}
;; Three ways of getting the iter pointing to the location
(let (path iter parent)
  ;; Get the iterator from a string
  (setf iter (gtk-tree-model-iter-from-string model \"3:2:5\"))

  ;; Get the iterator from a path
  (setf path (gtk-tree-path-new-from-string \"3:2:5\"))
  (setf iter (gtk-tree-model-iter model path))

  ;; Walk the tree to find the iterator
  (setf parent (gtk-tree-model-iter-nth-child model nil 3))
  (setf parent (gtk-tree-model-iter-nth-child model parent 2))
  (setf iter (gtk-tree-model-iter-nth-child model parent 5))
  ... )
  @end{pre}
  The second example shows a quick way of iterating through a list and
  getting a value from each row.

  @b{Example:} Reading data from a @class{gtk-tree-model}
  @begin{pre}
(do* ((model (gtk-tree-view-model view))             ; get the model
      (iter (gtk-tree-model-iter-first model)        ; get first iter
            (gtk-tree-model-iter-next model iter)))  ; get next iter
     ((not iter))                                    ; until iter is nil
     ;; Get a value and do something with the data
     (let ((value (gtk-tree-model-value model iter col-yearborn)))
       (gtk-list-store-set-value model iter
                                       col-yearborn
                                       (1+ value))))
  @end{pre}
  The @class{gtk-tree-model} interface contains two methods for reference
  counting: @fun{gtk-tree-model-ref-node} and @fun{gtk-tree-model-unref-node}.
  These two methods are optional to implement. The reference counting is meant
  as a way for views to let models know when nodes are being displayed. The
  @class{gtk-tree-view} widget will take a reference on a node when it is
  visible, which means the node is either in the toplevel or expanded. Being
  displayed does not mean that the node is currently directly visible to the
  user in the viewport. Based on this reference counting scheme a caching model,
  for example, can decide whether or not to cache a node based on the reference
  count. A file-system based model would not want to keep the entire file
  hierarchy in memory, but just the folders that are currently expanded in
  every current view.

  When working with reference counting, the following rules must be taken into
  account:
  @begin{itemize}
    @begin{item}
      Never take a reference on a node without owning a reference on its
      parent. This means that all parent nodes of a referenced node must be
      referenced as well.
    @end{item}
    @begin{item}
      Outstanding references on a deleted node are not released. This is not
      possible because the node has already been deleted by the time the
      row-deleted signal is received.
    @end{item}
    @begin{item}
      Models are not obligated to emit a signal on rows of which none of its
      siblings are referenced. To phrase this differently, signals are only
      required for levels in which nodes are referenced. For the root level
      however, signals must be emitted at all times (however the root level is
      always referenced when any view is attached).
    @end{item}
  @end{itemize}
  @begin[Signal Details]{dictionary}
    @subheading{The \"row-changed\" signal}
      @begin{pre}
 lambda (model path iter)    : Run Last
      @end{pre}
      This signal is emitted when a row in the model has changed.
      @begin[code]{table}
        @entry[model]{The @sym{gtk-tree-model} object on which the signal
          is emitted.}
        @entry[path]{A @class{gtk-tree-path} instance identifying the changed
          row.}
        @entry[iter]{A valid @class{gtk-tree-iter} iterator pointing to the
          changed row.}
      @end{table}
    @subheading{The \"row-deleted\" signal}
      @begin{pre}
 lambda (model path)    : Run First
      @end{pre}
      This signal is emitted when a row has been deleted. Note that no iterator
      is passed to the signal handler, since the row is already deleted. This
      should be called by models after a row has been removed. The location
      pointed to by path should be the location that the row previously was at.
      It may not be a valid location anymore.
      @begin[code]{table}
        @entry[model]{The @sym{gtk-tree-model} object on which the signal
          is emitted.}
        @entry[path]{A @class{gtk-tree-path} instance identifying the row.}
      @end{table}
    @subheading{The \"row-has-child-toggled\" signal}
      @begin{pre}
 lambda (model path iter)    : Run Last
      @end{pre}
      This signal is emitted when a row has gotten the first child row or lost
      its last child row.
      @begin[code]{table}
        @entry[model]{The @sym{gtk-tree-model} object on which the signal
          is emitted.}
        @entry[path]{A @class{gtk-tree-path} instance identifying the row.}
        @entry[iter]{A valid @class{gtk-tree-iter} iterator pointing to the
          row.}
      @end{table}
    @subheading{The \"row-inserted\" signal}
      @begin{pre}
 lambda (model path iter)    : Run First
      @end{pre}
      This signal is emitted when a new row has been inserted in the model.
      Note that the row may still be empty at this point, since it is a common
      pattern to first insert an empty row, and then fill it with the desired
      values.
      @begin[code]{table}
        @entry[model]{The @sym{gtk-tree-model} object on which the signal is
          emitted.}
        @entry[path]{A @class{gtk-tree-path} instance identifying the new row.}
        @entry[iter]{A valid @class{gtk-tree-iter} iterator pointing to the new
          row.}
      @end{table}
    @subheading{The \"rows-reordered\" signal}
      @begin{pre}
 lambda (model path iter new-order)    : Run First
      @end{pre}
      This signal is emitted when the children of a node in the
      @class{gtk-tree-model} object have been reordered. Note that this signal
      is not emitted when rows are reordered by DND, since this is implemented
      by removing and then reinserting the row.
      @begin[code]{table}
        @entry[model]{The @sym{gtk-tree-model} object on which the signal
          is emitted.}
        @entry[path]{A @class{gtk-tree-path} instance identifying the tree node
          whose children have been reordered.}
        @entry[iter]{A valid @class{gtk-tree-iter} iterator pointing to the
         node whose children have been reordered.}
        @entry[new-order]{An array of integers mapping the current position of
          each child to its old position before the re-ordering, i.e.
          @code{@arg{new-order}[newpos] = oldpos}.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-tree-view}
  @see-class{gtk-list-store}
  @see-class{gtk-tree-store}
  @see-class{gtk-tree-sortable}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_flags () -> gtk-tree-model-flags
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_flags" gtk-tree-model-flags)
    gtk-tree-model-flags
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @return{The @symbol{gtk-model-flags} flags supported by this interface.}
  @begin{short}
    Returns a set of flags supported by this interface.
  @end{short}
  The flags are a bitwise combination of @symbol{gtk-tree-model-flags} flags.
  The flags supported should not change during the lifetime of the model.
  @see-class{gtk-tree-model}
  @see-symbol{gtk-tree-model-flags}"
  (model (g-object gtk-tree-model)))

(export 'gtk-tree-model-flags)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_n_columns () -> gtk-tree-model-n-columns
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_n_columns" gtk-tree-model-n-columns) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @return{An integer with the number of columns.}
  @begin{short}
    Returns the number of columns supported by @arg{model}.
  @end{short}
  @see-class{gtk-tree-model}
  @see-function{gtk-tree-model-column-type}"
  (model (g-object gtk-tree-model)))

(export 'gtk-tree-model-n-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_column_type () -> gtk-tree-model-column-type
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_column_type" gtk-tree-model-column-type) g-type
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[index]{an integer with the column index}
  @return{The @class{g-type} type of the column.}
  @begin{short}
    Returns the type of the column.
  @end{short}
  @see-class{gtk-tree-model}
  @see-function{gtk-tree-model-n-columns}"
  (model (g-object gtk-tree-model))
  (index :int))

(export 'gtk-tree-model-column-type)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter () -> gtk-tree-model-iter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_iter" %gtk-tree-model-iter) :boolean
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-model-iter (model path)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[path]{the @class{gtk-tree-path} instance}
  @return{The @class{gtk-tree-iter} iterator or @code{nil}, if the iterator is
    not set.}
  @begin{short}
    Returns a valid iterator pointing to @arg{path}.
  @end{short}
  If @arg{path} does not exist, @code{nil} is returned.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-class{gtk-tree-path}"
  (let ((iter (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter model iter path)
      iter)))

(export 'gtk-tree-model-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter_from_string () -> gtk-tree-model-iter-from-string
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_iter_from_string"
          %gtk-tree-model-iter-from-string) :boolean
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (pathstr :string))

(defun gtk-tree-model-iter-from-string (model pathstr)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[pathstr]{a string representation of a @class{gtk-tree-path} object}
  @return{The @class{gtk-tree-iter} iterator.}
  @begin{short}
    Returns a valid iterator pointing to @arg{pathstr}, if it exists.
  @end{short}
  Otherwise, @code{nil} is returned.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-class{gtk-tree-path}"
  (let ((iter (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-from-string model iter pathstr)
      iter)))

(export 'gtk-tree-model-iter-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter_first () -> gtk-tree-model-iter-first
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_iter_first" %gtk-tree-model-iter-first)
    :boolean
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-first (model)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @return{A @class{gtk-tree-iter} iterator.}
  @begin{short}
    Returns the first iterator in the tree, the one at the path \"0\".
  @end{short}
  Returns @code{nil} if the tree is empty.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-iter-next}"
  (let ((iter (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-first model iter)
      iter)))

(export 'gtk-tree-model-iter-first)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_path () -> gtk-tree-model-path
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_path" gtk-tree-model-path)
     (g-boxed-foreign gtk-tree-path :return)
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[iter]{a @class{gtk-tree-iter} iterator}
  @return{A newly created @class{gtk-tree-path} instance.}
  @begin{short}
    Returns a tree path referenced by the given iterator.
  @end{short}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-class{gtk-tree-path}"
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_value () -> gtk-tree-model-value
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_value" %gtk-tree-model-value) :void
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (column :int)
  (value (:pointer (:struct g-value))))

(defun gtk-tree-model-value (model iter column)
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-13}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[iter]{the @class{gtk-tree-iter} iterator}
  @argument[column]{an integer with the column to lookup the value at}
  @return{The value at @arg{column}.}
  @begin{short}
    Returns the value at @arg{column}.
  @end{short}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (with-foreign-object (value '(:struct g-value))
    (g-value-init value)
    (%gtk-tree-model-value model iter column value)
    (prog1
      (parse-g-value value)
      (g-value-unset value))))

(export 'gtk-tree-model-value)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_next ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_next" %gtk-tree-model-iter-next) :boolean
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-next (model iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[iter]{a @class{gtk-tree-iter} iterator}
  @return{A @class{gtk-tree-iter} iterator.}
  @begin{short}
    Returns the iterator to the node following @arg{iter} at the current level.
  @end{short}
  If there is no next iterator, @code{nil} is returned.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-iter-first}
  @see-function{gtk-tree-model-iter-previous}"
  (let ((next (copy-gtk-tree-iter iter)))
    (when (%gtk-tree-model-iter-next model next)
      next)))

(export 'gtk-tree-model-iter-next)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_previous ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_previous" %gtk-tree-model-iter-previous) :boolean
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-previous (model iter)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[iter]{a @class{gtk-tree-iter} iterator}
  @return{A @class{gtk-tree-iter} iterator.}
  @begin{short}
    Returns the iterator to the previous node at the current level.
  @end{short}
  If there is no previous iterator, @code{nil} is returned.
  @see-class{gtk-tree-model}
  @see-function{gtk-tree-model-iter-next}"
  (let ((prev (copy-gtk-tree-iter iter)))
    (when (%gtk-tree-model-iter-previous model prev)
      prev)))

(export 'gtk-tree-model-iter-previous)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_children" %gtk-tree-model-iter-children) :boolean
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-children (model parent)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[parent]{a @class{gtk-tree-iter} iterator, or @code{nil}}
  @return{A @class{gtk-tree-iter} iterator.}
  @begin{short}
    Returns the iterator to the first child of @arg{parent}.
  @end{short}
  If @arg{parent} has no children, @code{nil} is returned. The @arg{parent}
  iterator will remain a valid node after this function has been called.

  If @arg{parent} is @code{nil} returns the first node. This is equivalent to:
  @begin{pre}
(gtk-tree-model-iter-first model)
  @end{pre}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-iter-parent}
  @see-function{gtk-tree-model-iter-n-children}
  @see-function{gtk-tree-model-iter-nth-child}"
  (let ((child (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-children model child parent)
      child)))

(export 'gtk-tree-model-iter-children)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_has_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_has_child" gtk-tree-model-iter-has-child)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[iter]{the @class{gtk-tree-iter} iterator to test for children}
  @return{@em{True} if @arg{iter} has children.}
  @begin{short}
    Returns @em{true} if @arg{iter} has children, @code{nil} otherwise.
  @end{short}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-iter-has-child)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_n_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_n_children" gtk-tree-model-iter-n-children) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[iter]{a @class{gtk-tree-iter} iterator, or @code{nil}}
  @return{The number of children of @arg{iter}.}
  @begin{short}
    Returns the number of children that @arg{iter} has.
  @end{short}
  As a special case, if @arg{iter} is @code{nil}, then the number of toplevel
  nodes is returned.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-iter-n-children)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_nth_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_nth_child" %gtk-tree-model-iter-nth-child)
    :boolean
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (n :int))

(defun gtk-tree-model-iter-nth-child (model parent n)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[parent]{a @class{gtk-tree-iter} iterator to get the child from, or
    @code{nil}}
  @argument[n]{an integer with the index of the desired child}
  @return{The @class{gtk-tree-iter} iterator to the nth child.}
  @begin{short}
    Returns the iterator to the child of @arg{parent}, using the given index.
  @end{short}
  The first index is 0. If @arg{n} is too big, or @arg{parent} has no children,
  @code{nil} is returned. The @arg{parent} iterator will remain a valid node
  after this function has been called. As a special case, if @arg{parent} is
  @code{nil}, then the nth root node is returned.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (let ((child (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-nth-child model child parent n)
      child)))

(export 'gtk-tree-model-iter-nth-child)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_parent" %gtk-tree-model-iter-parent) :boolean
  (model g-object)
  (iter (g-boxed-foreign gtk-tree-iter))
  (child (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-parent (model child)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[child]{a @class{gtk-tree-iter} iterator}
  @return{A @class{gtk-tree-iter} iterator to the parent.}
  @begin{short}
    Returns the iterator to the parent of @arg{child}.
  @end{short}
  If @arg{child} is at the toplevel, and does not have a parent, then
  @code{nil} is returned. The @arg{child} iterator will remain a valid node
  after this function has been called.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-iter-children}"
  (let ((parent (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-parent model parent child)
      parent)))

(export 'gtk-tree-model-iter-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_string_from_iter () -> gtk-tree-model-string-from-iter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_string_from_iter"
           gtk-tree-model-string-from-iter) (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[iter]{a @class{gtk-tree-iter} instance}
  @return{A string representation of @arg{iter}.}
  @begin{short}
    Generates a string representation of the iterator.
  @end{short}
  This string is a ':' separated list of numbers. For example, \"4:10:0:3\"
  would be an acceptable return value for this string.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-string-from-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_ref_node ()
;;; ----------------------------------------------------------------------------

;; not exported

(defcfun ("gtk_tree_model_ref_node" gtk-tree-model-ref-node) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[iter]{the @class{gtk-tree-iter} iterator}
  @begin{short}
    Lets the tree ref the node.
  @end{short}

  This is an optional method for models to implement. To be more specific,
  models may ignore this call as it exists primarily for performance reasons.

  This function is primarily meant as a way for views to let caching models
  know when nodes are being displayed, and hence, whether or not to cache that
  node. Being displayed means a node is in an expanded branch, regardless of
  whether the node is currently visible in the viewport. For example, a
  file-system based model would not want to keep the entire file-hierarchy in
  memory, just the sections that are currently being displayed by every
  current view.

  A model should be expected to be able to get an iter independent of its
  reffed state.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_unref_node ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_unref_node" gtk-tree-model-unref-node) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-28}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[iter]{the @class{gtk-tree-iter} iterator}
  @begin{short}
    Lets the tree unref the node.
  @end{short}

  This is an optional method for models to implement. To be more specific,
  models may ignore this call as it exists primarily for performance reasons.
  For more information on what this means, see the function
  @fun{gtk-tree-model-ref-node}.

  Please note that nodes that are deleted are not unreffed.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-ref-node}"
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get ()
;;; ----------------------------------------------------------------------------

;; TODO: Consider to return all values of the row. This would be more consistent
;; to the function gtk-tree-model-set.

(defun gtk-tree-model-get (model iter &rest colums)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[iter]{a @class{gtk-tree-iter} iterator to a row}
  @argument[columns]{a list of integers with column numbers}
  @return{A list of values for the columns.}
  @begin{short}
    Gets the value of one or more cells in the row referenced by @arg{iter}.
  @end{short}
  The variable argument list should contain integer column numbers.
  For example, to get a value from columns 1 and 3, you would write:
  @begin{pre}
(gtk-tree-model-get model iter 1 3)
  @end{pre}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-value}"
  (let ((result nil))
    (dolist (column colums)
      (setf result
            (cons (gtk-tree-model-value model iter column) result)))
    (reverse result)))

(export 'gtk-tree-model-get)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_valist ()
;;;
;;; void gtk_tree_model_get_valist (GtkTreeModel *tree_model,
;;;                                 GtkTreeIter *iter,
;;;                                 va_list var_args);
;;;
;;; See gtk_tree_model_get(), this version takes a va_list for language
;;; bindings
;;; to use.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     a row in tree_model
;;;
;;; var_args :
;;;     va_list of column/return location pairs
;;; ----------------------------------------------------------------------------

;; not needed

;;; ----------------------------------------------------------------------------
;;; GtkTreeModelForeachFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-model-foreach-func :boolean
    ((model g-object)
     (path (g-boxed-foreign gtk-tree-path))
     (iter (g-boxed-foreign gtk-tree-iter))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn model path iter)
      (stop-tree-model-iteration () t)
      (skip-tree-model-current () nil))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model-foreach-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-tree-model-foreach-func atdoc:*external-symbols*)
 "@version{2021-3-4}
  @begin{short}
    Type of the callback function passed to the funcion
    @fun{gtk-tree-model-foreach} to iterate over the rows in a tree model.
  @end{short}
  @begin{pre}
 lambda (model path iter)
  @end{pre}
  @begin[code]{table}
    @entry[model]{The @class{gtk-tree-model} object being iterated.}
    @entry[path]{The current @class{gtk-tree-path} instance.}
    @entry[iter]{The current @class{gtk-tree-iter} iterator.}
    @entry[Returns]{@em{True} to stop iterating, @em{false} to continue.}
  @end{table}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-foreach}")

(export 'gtk-tree-model-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_foreach" %gtk-tree-model-foreach) :void
  (model g-object)
  (func :pointer)
  (data :pointer))

(defun gtk-tree-model-foreach (model func)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[func]{a @symbol{gtk-tree-model-foreach-func} callback function to
    be called on each row}
  @begin{short}
    Calls @arg{func} on each node in @arg{model} in a depth-first fashion.
  @end{short}
  If @arg{func} returns @em{true}, then the tree ceases to be walked, and the
  function @sym{gtk-tree-model-foreach} returns.
  @see-class{gtk-tree-model}
  @see-symbol{gtk-tree-model-foreach-func}"
  (with-stable-pointer (ptr func)
    (%gtk-tree-model-foreach model
                             (callback gtk-tree-model-foreach-func)
                             ptr)))

(export 'gtk-tree-model-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_row_changed" gtk-tree-model-row-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[path]{a @class{gtk-tree-path} instance pointing to the changed row}
  @argument[iter]{a valid @class{gtk-tree-iter} iterator pointing to the
    changed row}
  @begin{short}
    Emits the \"row-changed\" signal on @arg{model}.
  @end{short}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-iter}"
  (model (g-object gtk-tree-model))
  (path (g-boxed-foreign gtk-tree-path))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-row-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_inserted ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_row_inserted" gtk-tree-model-row-inserted) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[path]{a @class{gtk-tree-path} instance pointing to the inserted row}
  @argument[iter]{a valid @class{gtk-tree-iter} iterator pointing to the
    inserted row}
  @begin{short}
    Emits the \"row-inserted\" signal on @arg{model}.
  @end{short}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-iter}"
  (model (g-object gtk-tree-model))
  (path (g-boxed-foreign gtk-tree-path))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-row-inserted)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_has_child_toggled ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_row_has_child_toggled"
           gtk-tree-model-row-has-child-toggled) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[path]{a @class{gtk-tree-path} instance pointing to the changed row}
  @argument[iter]{a valid @class{gtk-tree-iter} iterator pointing to the
    changed row}
  @begin{short}
    Emits the \"row-has-child-toggled\" signal on @arg{model}.
  @end{short}
  This should be called by models after the child state of a node changes.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-iter}"
  (model (g-object gtk-tree-model))
  (path (g-boxed-foreign gtk-tree-path))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-row-has-child-toggled)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_deleted ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_row_deleted" gtk-tree-model-row-deleted) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[path]{a @class{gtk-tree-path} instance pointing to the previous
    location of the deleted row}
  @begin{short}
    Emits the \"row-deleted\" signal on @arg{model}.
  @end{short}

  This should be called by models after a row has been removed. The location
  pointed to by path should be the location that the row previously was at. It
  may not be a valid location anymore.

  Nodes that are deleted are not unreffed, this means that any outstanding
  references on the deleted node should not be released.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-path}"
  (model (g-object gtk-tree-model))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-model-row-deleted)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_rows_reordered ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_rows_reordered_with_length"
          %gtk-tree-model-rows-reordered) :void
  (model (g-object gtk-tree-model))
  (path (g-boxed-foreign gtk-tree-path))
  (iter (g-boxed-foreign gtk-tree-iter))
  (order (:pointer :int))
  (length :int))

(defun gtk-tree-model-rows-reordered (model path iter order)
 #+cl-cffi-gtk-documentation
 "@version{2021-3-4}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[path]{a @class{gtk-tree-path} instance pointing to the tree node
    whose children have been reordered}
  @argument[iter]{a valid @class{gtk-tree-iter} iterator pointing to the node
    whose children have been reordered, or @code{nil} if the depth of path is 0}
  @argument[order]{a list of integers mapping the current position of each
    child to its old position before the re-ordering}
  @begin{short}
    Emits the \"rows-reordered\" signal on @arg{model}.
  @end{short}
  This should be called by models when their rows have been reordered.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-iter}"
  (with-foreign-object (order-ar :int (length order))
    (dolist (i order)
      (mem-aref order-ar :int i))
    (%gtk-tree-model-rows-reordered model path iter order-ar (length order))))

(export 'gtk-tree-model-rows-reordered)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_rows_reordered_with_length ()
;;;
;;; void
;;; gtk_tree_model_rows_reordered_with_length
;;;                                (GtkTreeModel *tree_model,
;;;                                 GtkTreePath *path,
;;;                                 GtkTreeIter *iter,
;;;                                 gint *new_order,
;;;                                 gint length);
;;;
;;; Emits the “rows-reordered” signal on tree_model .
;;;
;;; This should be called by models when their rows have been reordered.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; path :
;;;     a GtkTreePath pointing to the tree node whose children have been
;;;     reordered
;;;
;;; iter :
;;;     a valid GtkTreeIter pointing to the node whose children have been
;;;     reordered, or NULL if the depth of path is 0.
;;;
;;; new_order :
;;;     an array of integers mapping the current position of each child to its
;;;     old position before the re-ordering, i.e. new_order [newpos] = oldpos.
;;;
;;; length :
;;;     length of new_order array
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;; not needed

;;; --- End of file gtk.tree-model.lisp ----------------------------------------
