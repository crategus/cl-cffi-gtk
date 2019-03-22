;;; ----------------------------------------------------------------------------
;;; gtk.tree-model.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
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
;;; GtkTreeModel
;;;
;;; The tree interface used by GtkTreeView
;;;
;;; Synopsis
;;;
;;;     GtkTreeModel
;;;     GtkTreeIter
;;;     GtkTreePath
;;;     GtkTreeRowReference
;;;     GtkTreeModelIface
;;;     GtkTreeModelFlags
;;;
;;;     gtk_tree_path_new
;;;     gtk_tree_path_new_from_string
;;;     gtk_tree_path_new_from_indices
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeModel
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkTreeModel" gtk-tree-model
  (:export t
   :type-initializer "gtk_tree_model_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-tree-model 'type)
 "@version{2013-5-12}
  @begin{short}
    The @sym{gtk-tree-model} interface defines a generic tree interface for use
    by the @class{gtk-tree-view} widget. It is an abstract interface, and is
    designed to be usable with any appropriate data structure. The programmer
    just has to implement this interface on their own data type for it to be
    viewable by a @class{gtk-tree-view} widget.
  @end{short}

  The model is represented as a hierarchical tree of strongly-typed, columned
  data. In other words, the model can be seen as a tree where every node has
  different values depending on which column is being queried. The type of
  data found in a column is determined by using the GType system (i. e.
  @code{G_TYPE_INT}, @code{GTK_TYPE_BUTTON}, @code{G_TYPE_POINTER}, etc). The
  types are homogeneous per column across all nodes. It is important to note
  that this interface only provides a way of examining a model and observing
  changes. The implementation of each individual model decides how and if
  changes are made.

  In order to make life simpler for programmers who do not need to write their
  own specialized model, two generic models are provided - the
  @class{gtk-tree-store} and the @class{gtk-list-store}. To use these, the
  developer simply pushes data into these models as necessary. These models
  provide the data structure as well as all appropriate tree interfaces. As a
  result, implementing drag and drop, sorting, and storing data is trivial. For
  the vast majority of trees and lists, these two models are sufficient.

  Models are accessed on a node/column level of granularity. One can query for
  the value of a model at a certain node and a certain column on that node.
  There are two structures used to reference a particular node in a model.
  They are the @class{gtk-tree-path} and the @class{gtk-tree-iter}[4]. Most of
  the interface consists of operations on a @class{gtk-tree-iter}.

  A path is essentially a potential node. It is a location on a model that may
  or may not actually correspond to a node on a specific model. The
  @class{gtk-tree-path} struct can be converted into either an array of unsigned
  integers or a string. The string form is a list of numbers separated by a
  colon. Each number refers to the offset at that level. Thus, the path '0'
  refers to the root node and the path '2:4' refers to the fifth child of the
  third node.

  By contrast, a @class{gtk-tree-iter} is a reference to a specific node on a
  specific model. It is a generic struct with an integer and three generic
  pointers. These are filled in by the model in a model-specific way. One can
  convert a path to an iterator by calling the function
  @fun{gtk-tree-model-get-iter}. These iterators are the primary way of
  accessing a model and are similar to the iterators used by
  @class{gtk-text-buffer}. They are generally statically allocated on the stack
  and only used for a short time. The model interface defines a set of
  operations using them for navigating the model.

  It is expected that models fill in the iterator with private data. For
  example, the @class{gtk-list-store} model, which is internally a simple linked
  list, stores a list node in one of the pointers. The
  @class{gtk-tree-model-sort} stores an array and an offset in two of the
  pointers. Additionally, there is an integer field. This field is generally
  filled with a unique stamp per model. This stamp is for catching errors
  resulting from using invalid iterators with a model.

  The lifecycle of an iterator can be a little confusing at first. Iterators
  are expected to always be valid for as long as the model is unchanged (and
  does not emit a signal). The model is considered to own all outstanding
  iterators and nothing needs to be done to free them from the user's point of
  view. Additionally, some models guarantee that an iterator is valid for as
  long as the node it refers to is valid (most notably the
  @class{gtk-tree-store} and @class{gtk-list-store}). Although generally
  uninteresting, as one always has to allow for the case where iterators do not
  persist beyond a signal, some very important performance enhancements were
  made in the sort model. As a result, the @code{GTK_TREE_MODEL_ITERS_PERSIST}
  flag was added to indicate this behavior.

  To help show some common operation of a model, some examples are provided.
  The first example shows three ways of getting the iter at the location
  '3:2:5'. While the first method shown is easier, the second is much more
  common, as you often get paths from callbacks.

  @b{Example:} Acquiring a @class{gtk-tree-iter} object
  @begin{pre}
   /* Three ways of getting the iter pointing to the location */
   GtkTreePath *path;
   GtkTreeIter iter;
   GtkTreeIter parent_iter;

   /* get the iterator from a string */
   gtk_tree_model_get_iter_from_string (model, &iter, \"3:2:5\");

   /* get the iterator from a path */
   path = gtk_tree_path_new_from_string (\"3:2:5\");
   gtk_tree_model_get_iter (model, &iter, path);
   gtk_tree_path_free (path);

   /* walk the tree to find the iterator */
   gtk_tree_model_iter_nth_child (model, &iter, NULL, 3);
   parent_iter = iter;
   gtk_tree_model_iter_nth_child (model, &iter, &parent_iter, 2);
   parent_iter = iter;
   gtk_tree_model_iter_nth_child (model, &iter, &parent_iter, 5);
  @end{pre}
  This second example shows a quick way of iterating through a list and
  getting a string and an integer from each row. The @code{populate_model}
  function used below is not shown, as it is specific to the
  @class{gtk-list-store}. For information on how to write such a function, see
  the @class{gtk-list-store} documentation.

  @b{Example:} Reading data from a @class{gtk-tree-model}
  @begin{pre}
   enum
   {
     STRING_COLUMN,
     INT_COLUMN,
     N_COLUMNS
   @};

   ...

   GtkTreeModel *list_store;
   GtkTreeIter iter;
   gboolean valid;
   gint row_count = 0;

   /* make a new list_store */
   list_store = gtk_list_store_new (N_COLUMNS, G_TYPE_STRING, G_TYPE_INT);

   /* Fill the list store with data */
   populate_model (list_store);

   /* Get the first iter in the list */
   valid = gtk_tree_model_get_iter_first (list_store, &iter);

   while (valid)
    {
      /* Walk through the list, reading each row */
      gchar *str_data;
      gint   int_data;

      /* Make sure you terminate calls to gtk_tree_model_get()
       * with a '-1' value
       */
      gtk_tree_model_get (list_store, &iter,
                          STRING_COLUMN, &str_data,
                          INT_COLUMN, &int_data,
                          -1);

      /* Do something with the data */
      g_print (\"Row %d: (%s,%d)\n\", row_count, str_data, int_data);
      g_free (str_data);

      row_count++;
      valid = gtk_tree_model_iter_next (list_store, &iter);
    @}
  @end{pre}
  The @class{gtk-tree-model} interface contains two methods for reference
  counting: @fun{gtk-tree-model-ref-node} and @fun{gtk-tree-model-unref-node}.
  These two methods are optional to implement. The reference counting is meant
  as a way for views to let models know when nodes are being displayed.
  @class{gtk-tree-view} will take a reference on a node when it is visible,
  which means the node is either in the toplevel or expanded. Being displayed
  does not mean that the node is currently directly visible to the user in the
  viewport. Based on this reference counting scheme a caching model, for
  example, can decide whether or not to cache a node based on the reference
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
 lambda (tree-model path iter)   : Run Last
      @end{pre}
      This signal is emitted when a row in the model has changed.
      @begin[code]{table}
        @entry[tree-model]{The @class{gtk-tree-model} on which the signal is
          emitted.}
        @entry[path]{A @class{gtk-tree-path} identifying the changed row.}
        @entry[iter]{A valid @class{gtk-tree-iter} pointing to the changed row.}
      @end{table}
    @subheading{The \"row-deleted\" signal}
      @begin{pre}
 lambda (tree-model path)   : Run First
      @end{pre}
      This signal is emitted when a row has been deleted.
      Note that no iterator is passed to the signal handler, since the row is
      already deleted.
      This should be called by models after a row has been removed. The location
      pointed to by path should be the location that the row previously was at.
      It may not be a valid location anymore.
      @begin[code]{table}
        @entry[tree-model]{The @class{gtk-tree-model} on which the signal is
          emitted.}
        @entry[path]{A @class{gtk-tree-path} identifying the row.}
      @end{table}
    @subheading{The \"row-has-child-toggled\" signal}
      @begin{pre}
 lambda (tree-model path iter)   : Run Last
      @end{pre}
      This signal is emitted when a row has gotten the first child row or lost
      its last child row.
      @begin[code]{table}
        @entry[tree-model]{The @class{gtk-tree-model} on which the signal is
          emitted.}
        @entry[path]{A @class{gtk-tree-path} identifying the row.}
        @entry[iter]{A valid @class{gtk-tree-iter} pointing to the row.}
      @end{table}
    @subheading{The \"row-inserted\" signal}
      @begin{pre}
 lambda (tree-model path iter)   : Run First
      @end{pre}
      This signal is emitted when a new row has been inserted in the model.
      Note that the row may still be empty at this point, since it is a common
      pattern to first insert an empty row, and then fill it with the desired
      values.
      @begin[code]{table}
        @entry[tree-model]{The @class{gtk-tree-model} on which the signal is
          emitted.}
        @entry[path]{A @class{gtk-tree-path} identifying the new row.}
        @entry[iter]{A valid @class{gtk-tree-iter} pointing to the new row.}
      @end{table}
    @subheading{The \"rows-reordered\" signal}
      @begin{pre}
 lambda (tree-model path iter new-order)   : Run First
      @end{pre}
      This signal is emitted when the children of a node in the
      @class{gtk-tree-model} have been reordered.
      Note that this signal is not emitted when rows are reordered by DND, since
      this is implemented by removing and then reinserting the row.
      @begin[code]{table}
        @entry[tree-model]{The @class{gtk-tree-model} on which the signal is
          emitted.}
        @entry[path]{A @class{gtk-tree-path} identifying the tree node whose
          children have been reordered.}
        @entry[iter]{A valid @class{gtk-tree-iter} pointing to the node whose.}
        @entry[new-order]{An array of integers mapping the current position of
          each child to its old position before the re-ordering, i. e.
          @code{@arg{new-order}[newpos] = oldpos}.}
      @end{table}
  @end{dictionary}")

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
(setf (gethash 'gtk-tree-iter atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-tree-iter 'type)
 "@version{2013-9-18}
  @begin{short}
    The @sym{gtk-tree-iter} structure is the primary structure for accessing a
    @class{gtk-tree-model}. Models are expected to put a unique integer in the
    @arg{stamp} member, and put model specific data in the three @arg{user-data}
    members.
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
;;;
;;; Accessors of slots of the GtkTreeIter structure
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-iter-stamp atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-iter-stamp 'function)
 "@version{2013-9-18}
  Accessor of the slot @code{stamp} of the @class{gtk-tree-iter} structure.
  @see-class{gtk-tree-iter}")

(export 'gtk-tree-iter-stamp)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-iter-user-data atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-iter-user-data 'function)
 "@version{2013-9-18}
  Accessor of the slot @code{user-data} of the @class{gtk-tree-iter}
  structure.
  @see-class{gtk-tree-iter}")

(export 'gtk-tree-iter-user-data)

;;; ----------------------------------------------------------------------------
;;; GtkTreePath
;;; ----------------------------------------------------------------------------

(glib-init::at-init () (foreign-funcall "gtk_tree_path_get_type" :int))

(define-g-boxed-opaque gtk-tree-path "GtkTreePath"
  :alloc (%gtk-tree-path-new))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-path atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-tree-path 'type)
 "@version{2013-9-18}
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

(glib-init::at-init () (foreign-funcall "gtk_tree_row_reference_get_type" :int))

(define-g-boxed-opaque gtk-tree-row-reference "GtkTreeRowReference"
  :alloc (lambda () (error "")))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-row-reference atdoc:*class-name-alias*) "CStruct"
      (documentation 'gtk-tree-row-reference 'type)
 "@version{2013-9-18}
  @begin{short}
    A @sym{gtk-tree-row-reference} tracks model changes so that it always refers
    to the same row, a @class{gtk-tree-path} refers to a position, not a fixed
    row. Create a new @sym{gtk-tree-row-reference} with the function
    @fun{gtk-tree-row-reference-new}.
  @end{short}
  @begin{pre}
(define-g-boxed-opaque gtk-tree-row-reference \"GtkTreeRowReference\"
  :alloc (lambda () (error \"\")))
  @end{pre}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-row-reference-new}")

(export 'gtk-tree-row-reference)

;;; ----------------------------------------------------------------------------
;;; enum GtkTreeModelFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkTreeModelFlags" gtk-tree-model-flags
  (:export t
   :type-initializer "gtk_tree_model_flags_get_type")
  (:iters-persist 1)
  (:list-only 2))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-model-flags atdoc:*symbol-name-alias*) "Flags"
      (gethash 'gtk-tree-model-flags atdoc:*external-symbols*)
 "@version{2013-5-12}
  @begin{short}
    These flags indicate various properties of a @class{gtk-tree-model}.
  @end{short}

  They are returned by the function @fun{gtk-tree-model-get-flags}, and must be
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
  @see-function{gtk-tree-model-get-flags}")

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeModelIface
;;;
;;; struct GtkTreeModelIface {
;;;   GTypeInterface g_iface;
;;;
;;;   /* Signals */
;;;   void         (* row_changed)           (GtkTreeModel *tree_model,
;;;                                           GtkTreePath  *path,
;;;                                           GtkTreeIter  *iter);
;;;   void         (* row_inserted)          (GtkTreeModel *tree_model,
;;;                                           GtkTreePath  *path,
;;;                                           GtkTreeIter  *iter);
;;;   void         (* row_has_child_toggled) (GtkTreeModel *tree_model,
;;;                                           GtkTreePath  *path,
;;;                                           GtkTreeIter  *iter);
;;;   void         (* row_deleted)           (GtkTreeModel *tree_model,
;;;                                           GtkTreePath  *path);
;;;   void         (* rows_reordered)        (GtkTreeModel *tree_model,
;;;                                           GtkTreePath  *path,
;;;                                           GtkTreeIter  *iter,
;;;                                           gint         *new_order);
;;;
;;;   /* Virtual Table */
;;;   GtkTreeModelFlags (* get_flags)  (GtkTreeModel *tree_model);
;;;
;;;   gint         (* get_n_columns)   (GtkTreeModel *tree_model);
;;;   GType        (* get_column_type) (GtkTreeModel *tree_model,
;;;                                     gint          index_);
;;;   gboolean     (* get_iter)        (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter,
;;;                                     GtkTreePath  *path);
;;;   GtkTreePath *(* get_path)        (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   void         (* get_value)       (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter,
;;;                                     gint          column,
;;;                                     GValue       *value);
;;;   gboolean     (* iter_next)       (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   gboolean     (* iter_previous)   (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   gboolean     (* iter_children)   (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter,
;;;                                     GtkTreeIter  *parent);
;;;   gboolean     (* iter_has_child)  (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   gint         (* iter_n_children) (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   gboolean     (* iter_nth_child)  (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter,
;;;                                     GtkTreeIter  *parent,
;;;                                     gint          n);
;;;   gboolean     (* iter_parent)     (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter,
;;;                                     GtkTreeIter  *child);
;;;   void         (* ref_node)        (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   void         (* unref_node)      (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;; };
;;; ----------------------------------------------------------------------------

(define-vtable ("GtkTreeModel" gtk-tree-model)
  (:skip parent-instance (:pointer (:struct g-type-interface)))
  ;; some signals
  (:skip tree-model-row-changed :pointer)
  (:skip tree-model-row-inserted :pointer)
  (:skip tree-model-row-has-child-toggled :pointer)
  (:skip tree-model-row-deleted :pointer)
  (:skip tree-model-rows-reordered :pointer)
  ;; methods
  (get-flags (gtk-tree-model-flags (tree-model g-object)))
  (get-n-columns (:int (tree-model g-object)))
  (get-column-type (g-type (tree-model g-object) (index :int)))
  (get-iter (:boolean
             (tree-model g-object)
             (iter (g-boxed-foreign gtk-tree-iter))
             (path (g-boxed-foreign gtk-tree-path))))
  (get-path ((g-boxed-foreign gtk-tree-path :return)
             (tree-model g-object)
             (iter (g-boxed-foreign gtk-tree-iter))))
  (get-value (:void
              (tree-model g-object)
              (iter (g-boxed-foreign gtk-tree-iter))
              (n :int)
              (value (:pointer (:struct g-value))))
             :impl-call
             ((tree-model iter n)
              (multiple-value-bind (v type)
                  (gtk-tree-model-get-value-impl tree-model iter n)
                (set-g-value value v type))))
  (iter-next (:boolean
              (tree-model g-object)
              (iter (g-boxed-foreign gtk-tree-iter))))
  (iter-previous (:boolean
                  (tree-model g-object)
                  (iter (g-boxed-foreign gtk-tree-iter))))
  (iter-children (:boolean
                  (tree-model g-object)
                  (iter (g-boxed-foreign gtk-tree-iter))
                  (parent (g-boxed-foreign gtk-tree-iter))))
  (iter-has-child (:boolean
                   (tree-model g-object)
                   (iter (g-boxed-foreign gtk-tree-iter))))
  (iter-n-children (:int
                    (tree-model g-object)
                    (iter (g-boxed-foreign gtk-tree-iter))))
  (iter-nth-child (:boolean
                   (tree-model g-object)
                   (iter (g-boxed-foreign gtk-tree-iter))
                   (parent (g-boxed-foreign gtk-tree-iter))
                   (n :int)))
  (iter-parent (:boolean
                (tree-model g-object)
                (iter (g-boxed-foreign gtk-tree-iter))
                (child (g-boxed-foreign gtk-tree-iter))))
  (ref-node (:void
             (tree-model g-object)
             (iter (g-boxed-foreign gtk-tree-iter))))
  (unref-node (:void
               (tree-model g-object)
                (iter (g-boxed-foreign gtk-tree-iter)))))

;;; ----------------------------------------------------------------------------
;;; GtkTreeModelForeachFunc ()
;;;
;;; gboolean (*GtkTreeModelForeachFunc) (GtkTreeModel *model,
;;;                                      GtkTreePath *path,
;;;                                      GtkTreeIter *iter,
;;;                                      gpointer data);
;;;
;;; Type of the callback passed to gtk_tree_model_foreach() to iterate over the
;;; rows in a tree model.
;;;
;;; model :
;;;     the GtkTreeModel being iterated
;;;
;;; path :
;;;     the current GtkTreePath
;;;
;;; iter :
;;;     the current GtkTreeIter
;;;
;;; data :
;;;     The user data passed to gtk_tree_model_foreach()
;;;
;;; Returns :
;;;     TRUE to stop iterating, FALSE to continue
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-model-foreach-cb :boolean
    ((model g-object)
     (path (g-boxed-foreign gtk-tree-path))
     (iter (g-boxed-foreign gtk-tree-iter))
     (data :pointer))
  (let ((fn (glib::get-stable-pointer-value data)))
    (restart-case
      (funcall fn model path iter)
      (stop-tree-model-iteration () t)
      (skip-tree-model-current () nil))))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_new" %gtk-tree-path-new) :pointer)

(defcfun ("gtk_tree_path_new" gtk-tree-path-new)
    (g-boxed-foreign gtk-tree-path)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @return{A newly created @class{gtk-tree-path} object.}
  @short{Creates a new @class{gtk-tree-path}. This structure refers to a row.}")

(export 'gtk-tree-path-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_new_from_string" gtk-tree-path-new-from-string)
    (g-boxed-foreign gtk-tree-path)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[path]{the string representation of a path}
  @return{A newly created @class{gtk-tree-path}, or @code{nil}.}
  @short{Creates a new @class{gtk-tree-path} initialized to @arg{path}.}

  @arg{path} is expected to be a colon separated list of numbers. For example,
  the string \"10:4:0\" would create a path of depth 3 pointing to the 11th
  child of the root node, the 5th child of that 11th child, and the 1st child of
  that 5th child. If an invalid path string is passed in, @code{nil} is
  returned.
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-new}
  @see-function{gtk-tree-path-new-from-indices}
  @see-function{gtk-tree-path-to-string}"
  (path :string))

(export 'gtk-tree-path-new-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_from_indices ()
;;; ----------------------------------------------------------------------------

(defun gtk-tree-path-new-from-indices (&rest indices)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[indices]{list of integers terminated by -1}
  @return{A newly created @class{gtk-tree-path} object.}
  @begin{short}
    Creates a new path with @arg{indices} as indices.
  @end{short}

  Since 2.2"
  (gtk-tree-path-new-from-string
    (string-right-trim ":" (format nil "~{~D:~}" indices))))

(export 'gtk-tree-path-new-from-indices)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_to_string" gtk-tree-path-to-string) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[path]{a @class{gtk-tree-path} object}
  @return{A string.}
  @short{Generates a string representation of the path.}

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
 "@version{2013-5-12}
  @return{A new @class{gtk-tree-path} object.}
  @short{Creates a new @class{gtk-tree-path}.}

  The string representation of this path is \"0\".")

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
 "@version{2013-5-12}
  @argument[path]{a @class{gtk-tree-path} object}
  @argument[index]{the index}
  @return{The new path.}
  @short{Appends a new @arg{index} to a @arg{path}.}

  As a result, the depth of the @arg{path} is increased."
  (let ((path (gtk-tree-path-copy path)))
    (%gtk-tree-path-append-index path index)
    path))

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
 "@version{2013-5-12}
  @argument[path]{a @class{gtk-tree-path} object}
  @argument[index]{the index}
  @return{The new path.}
  @short{Prepends a new @arg{index} to a @arg{path}.}

  As a result, the depth of the @arg{path} is increased."
  (let ((path (gtk-tree-path-copy path)))
    (%gtk-tree-path-prepend-index path index)
    path))

(export 'gtk-tree-path-prepend-index)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_get_depth ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_get_depth" gtk-tree-path-get-depth) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[path]{a @class{gtk-tree-path} object}
  @return{The depth of @arg{path}.}
  @short{Returns the current depth of @arg{path}.}
  @see-class{gtk-tree-path}"
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-get-depth)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_get_indices ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_get_indices" %gtk-tree-path-get-indices)
    (:pointer :int)
  (path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-get-indices (path)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[path]{a @class{gtk-tree-path} object}
  @return{The current indices, or @code{nil}.}
  @short{Returns the current indices of @arg{path}.}

  This is a list of integers, each representing a node in a tree.

  The length of the list can be obtained with the function
  @fun{gtk-tree-path-get-depth}.
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-get-depth}"
  (let ((n (gtk-tree-path-get-depth path))
        (indices (%gtk-tree-path-get-indices path)))
    (loop
      for i from 0 below n
      collect (mem-aref indices :int i))))

(export 'gtk-tree-path-get-indices)

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

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_free" gtk-tree-path-free) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[path]{a @class{gtk-tree-path} object}
  @short{Frees path. If path is NULL, it simply returns.}"
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-free)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_copy" gtk-tree-path-copy)
    (g-boxed-foreign gtk-tree-path)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[path]{a @class{gtk-tree-path} object}
  @return{A new @class{gtk-tree-path} object.}
  @short{Creates a new @class{gtk-tree-path} object as a copy of @arg{path}.}
  @see-class{gtk-tree-path}"
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_compare ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_compare" gtk-tree-path-compare ) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[a]{a @class{gtk-tree-path} object}
  @argument[b]{a @class{gtk-tree-path} object to compare with}
  @return{The relative positions of @arg{a} and @arg{b}.}
  @short{Compares two paths.}

  If @arg{a} appears before @arg{b} in a tree, then -1 is returned. If @arg{b}
  appears before @arg{a}, then 1 is returned. If the two nodes are equal, then
  0 is returned."
  (tree-path-1 (g-boxed-foreign gtk-tree-path))
  (tree-path-2 (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_next ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_next" %gtk-tree-path-next) :void
  (tree-path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-next (path)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[path]{a @class{gtk-tree-path} object}
  @return{The new path.}
  @short{Moves the @arg{path} to point to the next node at the current depth.}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-prev}"
  (%gtk-tree-path-next path)
  path)

(export 'gtk-tree-path-next)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_prev ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_prev" %gtk-tree-path-prev) :boolean
  (tree-path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-prev (path)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[path]{a @class{gtk-tree-path} object}
  @return{@em{True} if @arg{path} has a previous node, and the move was made.}
  Moves the @arg{path} to point to the previous node at the current depth, if it
  exists.
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-next}"
  (when (%gtk-tree-path-prev path)
    path))

(export 'gtk-tree-path-prev)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_up ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_up" %gtk-tree-path-up) :boolean
  (tree-path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-up (path)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[path]{a @class{gtk-tree-path} object}
  @return{@em{True} if @arg{path} has a parent, and the move was made.}
  Moves the @arg{path} to point to its parent node, if it has a parent.
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-down}"
  (when (%gtk-tree-path-up path)
    path))

(export 'gtk-tree-path-up)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_down ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_down" %gtk-tree-path-down) :void
  (tree-path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-down (path)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[path]{a @class{gtk-tree-path} object}
  Moves @arg{path} to point to the first child of the current @arg{path}.
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-up}"
  (%gtk-tree-path-down path)
  path)

(export 'gtk-tree-path-down)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_is_ancestor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_is_ancestor" gtk-tree-path-is-ancestor) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[path]{a @class{gtk-tree-path} object}
  @argument[descendant]{another @class{gtk-tree-path} object}
  @return{@em{True} if @arg{descendant} is contained inside @arg{path}.}
  Returns @em{true} if @arg{descendant} is a descendant of @arg{path}.
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
 "@version{2013-10-15}
  @argument[path]{a @class{gtk-tree-path} object}
  @argument[ancestor]{another @class{gtk-tree-path} object}
  @return{@em{True} if @arg{ancestor} contains @arg{path} somewhere below it.}
  Returns @em{true} if @arg{path} is a descendant of @arg{ancestor}.
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-is-ancestor}"
  (tree-path (g-boxed-foreign gtk-tree-path))
  (ancestor (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-is-descendant)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_row_reference_new" gtk-tree-row-reference-new)
    (g-boxed-foreign gtk-tree-row-reference :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[path]{a valid @class{gtk-tree-path} object to monitor}
  @return{A newly allocated @class{gtk-tree-row-reference}, or @code{nil}.}
  @short{Creates a row reference based on @arg{path}.}

  This reference will keep pointing to the node pointed to by @arg{path}, so
  long as it exists. Any changes that occur on @arg{model} are propagated, and
  the @arg{path} is updated appropriately. If @arg{path} is not a valid
  @arg{path} in @arg{model}, then @code{nil} is returned.
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
;;; for that proxy. Since built-in GTK+ objects like GtkTreeView already use
;;; this mechanism internally, using them as the proxy object will produce
;;; unpredictable results. Further more, passing the same object as model and
;;; proxy doesn't work for reasons of internal implementation.
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
;;; gtk_tree_row_reference_get_model ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-tree-row-reference
                              %gtk-tree-row-reference-get-model
  :reader "gtk_tree_row_reference_get_model"
  :type (g-object gtk-tree-model))

(declaim (inline gtk-tree-row-reference-get-model))

(defun gtk-tree-row-reference-get-model (reference)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[reference]{a @class{gtk-tree-row-reference} object}
  @return{The model.}
  @short{Returns the model that the row reference is monitoring.}

  Since 2.8"
  (%gtk-tree-row-reference-get-model reference))

(export 'gtk-tree-row-reference-get-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_get_path ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-tree-row-reference
                              %gtk-tree-row-reference-get-path
  :reader "gtk_tree_row_reference_get_path"
  :type (g-boxed-foreign gtk-tree-path :return))

(declaim (inline gtk-tree-row-reference-get-path))

(defun gtk-tree-row-reference-get-path (reference)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[reference]{a @class{gtk-tree-row-reference} object}
  @return{A current path, or @code{nil}.}
  Returns a path that the row reference currently points to, or @code{nil} if
  the path pointed to is no longer valid.
  @see-class{gtk-tree-row-reference}"
  (%gtk-tree-row-reference-get-path reference))

(export 'gtk-tree-row-reference-get-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_valid ()
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-tree-row-reference
                              %gtk-tree-row-reference-valid
  :reader "gtk_tree_row_reference_valid" :type :boolean)

(declaim (inline gtk-tree-row-reference-valid))

(defun gtk-tree-row-reference-valid (reference)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[reference]{a @class{gtk-tree-row-reference}, or @code{nil}}
  @return{@em{True} if reference points to a valid path.}
  Returns @em{true} if the @arg{reference} is non-@code{nil} and refers to a
  current valid path.
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
;;;
;;; GtkTreeRowReference * gtk_tree_row_reference_copy
;;;                                            (GtkTreeRowReference *reference);
;;;
;;; Copies a GtkTreeRowReference.
;;;
;;; reference :
;;;     a GtkTreeRowReference
;;;
;;; Returns :
;;;     a copy of reference
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

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
;;; copy the structs by value (GtkTreeIter new_iter = iter;). You must free this
;;; iter with gtk_tree_iter_free().
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
;;; gtk_tree_model_get_flags ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_flags" gtk-tree-model-get-flags)
    gtk-tree-model-flags
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @return{The flags supported by this interface.}
  @begin{short}
    Returns a set of flags supported by this interface.
  @end{short}

  The flags are a bitwise combination of @symbol{gtk-tree-model-flags}. The
  flags supported should not change during the lifetime of the
  @arg{tree-model}.
  @see-class{gtk-tree-model}
  @see-symbol{gtk-tree-model-flags}"
  (tree-model (g-object gtk-tree-model)))

(export 'gtk-tree-model-get-flags)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_n_columns ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_n_columns" gtk-tree-model-get-n-columns) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @return{The number of columns.}
  Returns the number of columns supported by @arg{tree-model}.
  @see-class{gtk-tree-model}
  @see-function{gtk-tree-model-get-column-type}"
  (tree-model (g-object gtk-tree-model)))

(export 'gtk-tree-model-get-n-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_column_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_column_type" gtk-tree-model-get-column-type)
    g-type
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[index]{the column index}
  @return{The type of the column.}
  Returns the type of the column.
  @see-class{gtk-tree-model}
  @see-function{gtk-tree-model-get-n-columns}"
  (tree-model (g-object gtk-tree-model))
  (index :int))

(export 'gtk-tree-model-get-column-type)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_iter" %gtk-tree-model-get-iter) :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign gtk-tree-iter))
  (path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-model-get-iter (tree-model path)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[path]{the @class{gtk-tree-path} object}
  @return{The @class{gtk-tree-iter} or @code{nil}, if iter is not set.}
  Returns a valid iterator pointing to @arg{path}. If @arg{path} does not
  exist, @code{nil} is returned.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-class{gtk-tree-path}"
  (let ((iter (make-gtk-tree-iter)))
    (when (%gtk-tree-model-get-iter tree-model iter path)
      iter)))

(export 'gtk-tree-model-get-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_iter_from_string"
          %gtk-tree-model-get-iter-from-string)
    :boolean
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (path-string :string))

(defun gtk-tree-model-get-iter-from-string (tree-model path-string)
 #+cl-cffi-gtk-documentation
 "@version{2013-12-15}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[path-string]{a string representation of a @class{gtk-tree-path}
    object}
  @begin{return}
    @code{iter} -- the @class{gtk-tree-iter} object
  @end{return}
  @begin{short}
    Returns @arg{iter} a valid iterator pointing to @arg{path-string}, if it
    exists.
  @end{short}
  Otherwise, @code{nil} is returned.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (let ((iter (make-gtk-tree-iter)))
    (if (%gtk-tree-model-get-iter-from-string tree-model iter path-string)
        iter
        nil)))

(export 'gtk-tree-model-get-iter-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter_first ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_iter_first" %gtk-tree-model-get-iter-first)
    :boolean
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-get-iter-first (tree-model)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @return{@code{iter} -- the @class{gtk-tree-iter} object}
  Initializes @arg{iter} with the first iterator in the tree, the one at the
  path \"0\". Returns @code{nil} if the tree is empty.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-get-iter-next}"
  (let ((iter (make-gtk-tree-iter)))
    (if (%gtk-tree-model-get-iter-first tree-model iter)
        iter
        nil)))

(export 'gtk-tree-model-get-iter-first)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_path" gtk-tree-model-get-path)
     (g-boxed-foreign gtk-tree-path :return)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[iter]{the @class{gtk-tree-iter} object}
  @return{A newly-created @class{gtk-tree-path} object.}
  @begin{short}
    Returns a newly-created @class{gtk-tree-path} referenced by @arg{iter}.
  @end{short}

  This path should be freed with the function @fun{gtk-tree-path-free}.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-class{gtk-tree-path}
  @see-function{gtk-tree-path-free}"
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-get-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_value ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_value" %gtk-tree-model-get-value) :void
  (model g-object)
  (iter (g-boxed-foreign gtk-tree-iter))
  (column :int)
  (value (:pointer (:struct g-value))))

(defun gtk-tree-model-get-value (tree-model iter column)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-18}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[iter]{the @class{gtk-tree-iter}}
  @argument[column]{the column to lookup the value at}
  @return{The value at @arg{column}.}
  Returns the value at @arg{column}.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (with-foreign-object (v '(:struct g-value))
    (g-value-zero v)
    (%gtk-tree-model-get-value tree-model iter column v)
    (prog1
      (parse-g-value v)
      (g-value-unset v))))

(export 'gtk-tree-model-get-value)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_next ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_next" %gtk-tree-model-iter-next) :boolean
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-next (tree-model iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @return{@code{iter} -- the new @class{gtk-tree-iter} object}
  @begin{short}
    Sets @arg{iter} to point to the node following it at the current level.
  @end{short}

  If there is no next @arg{iter}, @code{nil} is returned and @arg{iter} is set
  to be invalid.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-iter-first}"
  (let ((iter-new (copy-gtk-tree-iter iter)))
    (when (%gtk-tree-model-iter-next tree-model iter-new)
      iter-new)))

(export 'gtk-tree-model-iter-next)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_previous ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_previous" %gtk-tree-model-iter-previous) :boolean
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-previous (tree-model iter)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @return{@code{iter} -- the @class{gtk-tree-iter} object}
  @begin{short}
    Sets @arg{iter} to point to the previous node at the current level.
  @end{short}

  If there is no previous @arg{iter}, @code{nil} is returned and @arg{iter} is
  set to be invalid.

  Since 3.0"
  (let ((iter-new (copy-gtk-tree-iter iter)))
    (when (%gtk-tree-model-iter-previous tree-model iter-new)
      iter-new)))

(export 'gtk-tree-model-iter-previous)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_children" %gtk-tree-model-iter-children) :boolean
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-children (tree-model parent)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[parent]{the @class{gtk-tree-iter}, or @code{nil}}
  @return{@code{iter} -- the new @class{gtk-tree-iter} to be set to the child}
  @begin{short}
    Sets @arg{iter} to point to the first child of @arg{parent}.
  @end{short}

  If @arg{parent} has no children, @code{nil} is returned and @arg{iter} is set
  to be invalid. @arg{parent} will remain a valid node after this function has
  been called.

  If @arg{parent} is @code{nil} returns the first node, equivalent to
  @code{(gtk-tree-model-get-iter-first tree-model iter)}
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-iter-parent}
  @see-function{gtk-tree-model-iter-n-children}
  @see-function{gtk-tree-model-iter-nth-child}"
  (let ((iter (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-children tree-model iter parent)
      iter)))

(export 'gtk-tree-model-iter-children)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_has_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_has_child" gtk-tree-model-iter-has-child)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[iter]{the @class{gtk-tree-iter} to test for children}
  @return{@em{True} if @arg{iter} has children.}
  Returns @em{true} if @arg{iter} has children, @code{nil} otherwise."
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-iter-has-child)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_n_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_n_children" gtk-tree-model-iter-n-children) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[iter]{the @class{gtk-tree-iter}, or @code{nil}}
  @return{The number of children of @arg{iter}.}
  @begin{short}
    Returns the number of children that @arg{iter} has.
  @end{short}

  As a special case, if @arg{iter} is @code{nil}, then the number of toplevel
  nodes is returned."
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-iter-n-children)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_nth_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_nth_child" %gtk-tree-model-iter-nth-child)
    :boolean
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (n :int))

(defun gtk-tree-model-iter-nth-child (tree-model parent n)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-18}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[parent]{the @class{gtk-tree-iter} to get the child from, or
    @code{nil}}
  @argument[n]{the index of the desired child}
  @return{@code{iter} -- the @class{gtk-tree-iter} to set to the nth child}
  @begin{short}
    Sets @arg{iter} to be the child of @arg{parent}, using the given index.
  @end{short}

  The first index is 0. If @arg{n} is too big, or @arg{parent} has no children,
  @code{nil} is returned. @arg{parent} will remain a valid node after this
  function has been called. As a special case, if @arg{parent} is @code{nil},
  then the nth root node is set.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}"
  (let ((iter (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-nth-child tree-model iter parent n)
      iter)))

(export 'gtk-tree-model-iter-nth-child)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_parent" %gtk-tree-model-iter-parent) :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign gtk-tree-iter))
  (child (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-parent (tree-model child)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[child]{the @class{gtk-tree-iter} object}
  @return{@code{parent} -- the new @class{gtk-tree-iter} to set to the parent}
  @begin{short}
    Sets @arg{parent} to be the parent of @arg{child}.
  @end{short}

  If @arg{child} is at the toplevel, and does not have a parent, then
  @arg{parent} is set to an invalid iterator and @code{nil} is returned.
  @arg{child} will remain a valid node after this function has been called.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-iter}
  @see-function{gtk-tree-model-iter-children}"
  (let ((parent (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-parent tree-model parent child)
      parent)))

(export 'gtk-tree-model-iter-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_string_from_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_string_from_iter"
           gtk-tree-model-get-string-from-iter)
    (g-string :free-from-foreign t)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[iter]{a @class{gtk-tree-iter} object}
  @return{A newly-allocated string.}
  @begin{short}
    Generates a string representation of the @arg{iter}.
  @end{short}

  This string is a ':' separated list of numbers. For example, \"4:10:0:3\"
  would be an acceptable return value for this string.

  Since 2.2"
  (tree-model g-object)
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-get-string-from-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_ref_node ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_ref_node" gtk-tree-model-ref-node) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[iter]{the @class{gtk-tree-iter} object}
  @begin{short}
    Lets the tree ref the node.
  @end{short}

  This is an optional method for models to implement. To be more specific,
  models may ignore this call as it exists primarily for performance reasons.

  This function is primarily meant as a way for views to let caching models
  know when nodes are being displayed (and hence, whether or not to cache that
  node). Being displayed means a node is in an expanded branch, regardless of
  whether the node is currently visible in the viewport. For example, a
  file-system based model would not want to keep the entire file-hierarchy in
  memory, just the sections that are currently being displayed by every
  current view.

  A model should be expected to be able to get an iter independent of its
  reffed state."
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-ref-node)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_unref_node ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_unref_node" gtk-tree-model-unref-node) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-12}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[iter]{the @class{gtk-tree-iter} object}
  @begin{short}
    Lets the tree unref the node.
  @end{short}

  This is an optional method for models to implement. To be more specific,
  models may ignore this call as it exists primarily for performance reasons.
  For more information on what this means, see the function
  @fun{gtk-tree-model-ref-node}.

  Please note that nodes that are deleted are not unreffed.
  @see-function{gtk-tree-model-ref-node}"
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-unref-node)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get ()
;;; ----------------------------------------------------------------------------

(defun gtk-tree-model-get (tree-model iter &rest colums)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[iter]{a row in @arg{tree-model}}
  @argument[columns]{column numbers}
  @return{A list of values for the columns.}
  @begin{short}
    Gets the value of one or more cells in the row referenced by @arg{iter}.
  @end{short}
  The variable argument list should contain integer column numbers.
  For example, to get a value from columns 1 and 3, you would write:
  @code{(gtk-tree-model-get model iter 1 3)}.
  @see-class{gtk-tree-model}
  @see-function{gtk-tree-model-get-value}"
  (let ((result nil))
    (dolist (column colums)
      (setf result
            (cons (gtk-tree-model-get-value tree-model iter column) result)))
    (reverse result)))

(export 'gtk-tree-model-get)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_valist ()
;;;
;;; void gtk_tree_model_get_valist (GtkTreeModel *tree_model,
;;;                                 GtkTreeIter *iter,
;;;                                 va_list var_args);
;;;
;;; See gtk_tree_model_get(), this version takes a va_list for language bindings
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

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_foreach" %gtk-tree-model-foreach) :void
  (model g-object)
  (func :pointer)
  (data :pointer))

(defun gtk-tree-model-foreach (model func)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-18}
  @argument[model]{a @class{gtk-tree-model} object}
  @argument[func]{a function to be called on each row}
  @begin{short}
    Calls @arg{func} on each node in @arg{model} in a depth-first fashion.
  @end{short}

  If @arg{func} returns @em{true}, then the tree ceases to be walked, and the
  function @sym{gtk-tree-model-foreach} returns.
  @see-class{gtk-tree-model}"
  (with-stable-pointer (ptr func)
    (%gtk-tree-model-foreach model (callback gtk-tree-model-foreach-cb) ptr)))

(export 'gtk-tree-model-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_row_changed" gtk-tree-model-row-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-22}
  @argument[tree-model]{a @class{gtk-tree-model} object}
  @argument[path]{a @class{gtk-tree-path} pointing to the changed row}
  @argument[iter]{a valid @class{gtk-tree-iter} pointing to the changed row}
  Emits the \"row-changed\" signal on @arg{tree-model}.
  @see-class{gtk-tree-model}
  @see-class{gtk-tree-path}
  @see-class{gtk-tree-iter}"
  (tree-model (g-object gtk-tree-model))
  (path (g-boxed-foreign gtk-tree-path))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-row-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_inserted ()
;;;
;;; void gtk_tree_model_row_inserted (GtkTreeModel *tree_model,
;;;                                   GtkTreePath *path,
;;;                                   GtkTreeIter *iter);
;;;
;;; Emits the "row-inserted" signal on tree_model.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; path :
;;;     a GtkTreePath pointing to the inserted row
;;;
;;; iter :
;;;     a valid GtkTreeIter pointing to the inserted row
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_has_child_toggled ()
;;;
;;; void gtk_tree_model_row_has_child_toggled (GtkTreeModel *tree_model,
;;;                                            GtkTreePath *path,
;;;                                            GtkTreeIter *iter);
;;;
;;; Emits the "row-has-child-toggled" signal on tree_model. This should be
;;; called by models after the child state of a node changes.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; path :
;;;     a GtkTreePath pointing to the changed row
;;;
;;; iter :
;;;     a valid GtkTreeIter pointing to the changed row
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_deleted ()
;;;
;;; void gtk_tree_model_row_deleted (GtkTreeModel *tree_model,
;;;                                  GtkTreePath *path);
;;;
;;; Emits the "row-deleted" signal on tree_model.
;;;
;;; This should be called by models after a row has been removed. The location
;;; pointed to by path should be the location that the row previously was at. It
;;; may not be a valid location anymore.
;;;
;;; Nodes that are deleted are not unreffed, this means that any outstanding
;;; references on the deleted node should not be released.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; path :
;;;     a GtkTreePath pointing to the previous location of the deleted row
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_rows_reordered ()
;;;
;;; void gtk_tree_model_rows_reordered (GtkTreeModel *tree_model,
;;;                                     GtkTreePath *path,
;;;                                     GtkTreeIter *iter,
;;;                                     gint *new_order);
;;;
;;; Emits the "rows-reordered" signal on tree_model.
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
;;;     reordered, or NULL if the depth of path is 0
;;;
;;; new_order :
;;;     an array of integers mapping the current position of each child to its
;;;     old position before the re-ordering, i.e. new_order[newpos] = oldpos
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.tree-model.lisp ----------------------------------------
