;;; ----------------------------------------------------------------------------
;;; gtk.tree-model.lisp
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
;;;
;;; Object Hierarchy
;;;
;;;   GInterface
;;;    +----GtkTreeModel
;;;
;;;   GBoxed
;;;    +----GtkTreeIter
;;;
;;;   GBoxed
;;;    +----GtkTreePath
;;;
;;; Prerequisites
;;;
;;; GtkTreeModel requires GObject.
;;;
;;; Known Derived Interfaces
;;;
;;; GtkTreeModel is required by GtkTreeSortable.
;;;
;;; Known Implementations
;;;
;;; GtkTreeModel is implemented by GtkListStore, GtkTreeModelFilter,
;;; GtkTreeModelSort and GtkTreeStore.
;;;
;;; Signals
;;;
;;;   "row-changed"                                    : Run Last
;;;   "row-deleted"                                    : Run First
;;;   "row-has-child-toggled"                          : Run Last
;;;   "row-inserted"                                   : Run First
;;;   "rows-reordered"                                 : Run First
;;;
;;; Description
;;;
;;; The GtkTreeModel interface defines a generic tree interface for use by the
;;; GtkTreeView widget. It is an abstract interface, and is designed to be
;;; usable with any appropriate data structure. The programmer just has to
;;; implement this interface on their own data type for it to be viewable by a
;;; GtkTreeView widget.
;;;
;;; The model is represented as a hierarchical tree of strongly-typed, columned
;;; data. In other words, the model can be seen as a tree where every node has
;;; different values depending on which column is being queried. The type of
;;; data found in a column is determined by using the GType system (ie.
;;; G_TYPE_INT, GTK_TYPE_BUTTON, G_TYPE_POINTER, etc). The types are homogeneous
;;; per column across all nodes. It is important to note that this interface
;;; only provides a way of examining a model and observing changes. The
;;; implementation of each individual model decides how and if changes are made.
;;;
;;; In order to make life simpler for programmers who do not need to write their
;;; own specialized model, two generic models are provided - the GtkTreeStore
;;; and the GtkListStore. To use these, the developer simply pushes data into
;;; these models as necessary. These models provide the data structure as well
;;; as all appropriate tree interfaces. As a result, implementing drag and drop,
;;; sorting, and storing data is trivial. For the vast majority of trees and
;;; lists, these two models are sufficient.
;;;
;;; Models are accessed on a node/column level of granularity. One can query for
;;; the value of a model at a certain node and a certain column on that node.
;;; There are two structures used to reference a particular node in a model.
;;; They are the GtkTreePath and the GtkTreeIter[4]. Most of the interface
;;; consists of operations on a GtkTreeIter.
;;;
;;; A path is essentially a potential node. It is a location on a model that may
;;; or may not actually correspond to a node on a specific model. The
;;; GtkTreePath struct can be converted into either an array of unsigned
;;; integers or a string. The string form is a list of numbers separated by a
;;; colon. Each number refers to the offset at that level. Thus, the path '0'
;;; refers to the root node and the path '2:4' refers to the fifth child of the
;;; third node.
;;;
;;; By contrast, a GtkTreeIter is a reference to a specific node on a specific
;;; model. It is a generic struct with an integer and three generic pointers.
;;; These are filled in by the model in a model-specific way. One can convert a
;;; path to an iterator by calling gtk_tree_model_get_iter(). These iterators
;;; are the primary way of accessing a model and are similar to the iterators
;;; used by GtkTextBuffer. They are generally statically allocated on the stack
;;; and only used for a short time. The model interface defines a set of
;;; operations using them for navigating the model.
;;;
;;; It is expected that models fill in the iterator with private data. For
;;; example, the GtkListStore model, which is internally a simple linked list,
;;; stores a list node in one of the pointers. The GtkTreeModelSort stores an
;;; array and an offset in two of the pointers. Additionally, there is an
;;; integer field. This field is generally filled with a unique stamp per model.
;;; This stamp is for catching errors resulting from using invalid iterators
;;; with a model.
;;;
;;; The lifecycle of an iterator can be a little confusing at first. Iterators
;;; are expected to always be valid for as long as the model is unchanged (and
;;; doesn't emit a signal). The model is considered to own all outstanding
;;; iterators and nothing needs to be done to free them from the user's point of
;;; view. Additionally, some models guarantee that an iterator is valid for as
;;; long as the node it refers to is valid (most notably the GtkTreeStore and
;;; GtkListStore). Although generally uninteresting, as one always has to allow
;;; for the case where iterators do not persist beyond a signal, some very
;;; important performance enhancements were made in the sort model. As a result,
;;; the GTK_TREE_MODEL_ITERS_PERSIST flag was added to indicate this behavior.
;;;
;;; To help show some common operation of a model, some examples are provided.
;;; The first example shows three ways of getting the iter at the location
;;; '3:2:5'. While the first method shown is easier, the second is much more
;;; common, as you often get paths from callbacks.
;;;
;;; Example 59. Acquiring a GtkTreeIter
;;;
;;;   /* Three ways of getting the iter pointing to the location */
;;;   GtkTreePath *path;
;;;   GtkTreeIter iter;
;;;   GtkTreeIter parent_iter;
;;;
;;;   /* get the iterator from a string */
;;;   gtk_tree_model_get_iter_from_string (model, &iter, "3:2:5");
;;;
;;;   /* get the iterator from a path */
;;;   path = gtk_tree_path_new_from_string ("3:2:5");
;;;   gtk_tree_model_get_iter (model, &iter, path);
;;;   gtk_tree_path_free (path);
;;;
;;;   /* walk the tree to find the iterator */
;;;   gtk_tree_model_iter_nth_child (model, &iter, NULL, 3);
;;;   parent_iter = iter;
;;;   gtk_tree_model_iter_nth_child (model, &iter, &parent_iter, 2);
;;;   parent_iter = iter;
;;;   gtk_tree_model_iter_nth_child (model, &iter, &parent_iter, 5);
;;;
;;;
;;; This second example shows a quick way of iterating through a list and
;;; getting a string and an integer from each row. The populate_model function
;;; used below is not shown, as it is specific to the GtkListStore. For
;;; information on how to write such a function, see the GtkListStore
;;; documentation.
;;;
;;; Example 60. Reading data from a GtkTreeModel
;;;
;;;   enum
;;;   {
;;;     STRING_COLUMN,
;;;     INT_COLUMN,
;;;     N_COLUMNS
;;;   };
;;;
;;;   ...
;;;
;;;   GtkTreeModel *list_store;
;;;   GtkTreeIter iter;
;;;   gboolean valid;
;;;   gint row_count = 0;
;;;
;;;   /* make a new list_store */
;;;   list_store = gtk_list_store_new (N_COLUMNS, G_TYPE_STRING, G_TYPE_INT);
;;;
;;;   /* Fill the list store with data */
;;;   populate_model (list_store);
;;;
;;;   /* Get the first iter in the list */
;;;   valid = gtk_tree_model_get_iter_first (list_store, &iter);
;;;
;;;   while (valid)
;;;    {
;;;      /* Walk through the list, reading each row */
;;;      gchar *str_data;
;;;      gint   int_data;
;;;
;;;      /* Make sure you terminate calls to gtk_tree_model_get()
;;;       * with a '-1' value
;;;       */
;;;      gtk_tree_model_get (list_store, &iter,
;;;                          STRING_COLUMN, &str_data,
;;;                          INT_COLUMN, &int_data,
;;;                          -1);
;;;
;;;      /* Do something with the data */
;;;      g_print ("Row %d: (%s,%d)\n", row_count, str_data, int_data);
;;;      g_free (str_data);
;;;
;;;      row_count++;
;;;      valid = gtk_tree_model_iter_next (list_store, &iter);
;;;    }
;;;
;;;
;;; The GtkTreeModel interface contains two methods for reference counting:
;;; gtk_tree_model_ref_node() and gtk_tree_model_unref_node(). These two methods
;;; are optional to implement. The reference counting is meant as a way for
;;; views to let models know when nodes are being displayed. GtkTreeView will
;;; take a reference on a node when it is visible, which means the node is
;;; either in the toplevel or expanded. Being displayed does not mean that the
;;; node is currently directly visible to the user in the viewport. Based on
;;; this reference counting scheme a caching model, for example, can decide
;;; whether or not to cache a node based on the reference count. A file-system
;;; based model would not want to keep the entire file hierarchy in memory, but
;;; just the folders that are currently expanded in every current view.
;;;
;;; When working with reference counting, the following rules must be taken into
;;; account:
;;;
;;;     Never take a reference on a node without owning a reference on its
;;;     parent. This means that all parent nodes of a referenced node must be
;;;     referenced as well.
;;;
;;;     Outstanding references on a deleted node are not released. This is not
;;;     possible because the node has already been deleted by the time the
;;;     row-deleted signal is received.
;;;
;;;     Models are not obligated to emit a signal on rows of which none of its
;;;     siblings are referenced. To phrase this differently, signals are only
;;;     required for levels in which nodes are referenced. For the root level
;;;     however, signals must be emitted at all times (however the root level is
;;;     always referenced when any view is attached).
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-changed" signal
;;;
;;; void user_function (GtkTreeModel *tree_model,
;;;                     GtkTreePath  *path,
;;;                     GtkTreeIter  *iter,
;;;                     gpointer      user_data)       : Run Last
;;;
;;; This signal is emitted when a row in the model has changed.
;;;
;;; tree_model :
;;;     the GtkTreeModel on which the signal is emitted
;;;
;;; path :
;;;     a GtkTreePath identifying the changed row
;;;
;;; iter :
;;;     a valid GtkTreeIter pointing to the changed row
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-deleted" signal
;;;
;;; void user_function (GtkTreeModel *tree_model,
;;;                     GtkTreePath  *path,
;;;                     gpointer      user_data)       : Run First
;;;
;;; This signal is emitted when a row has been deleted.
;;;
;;; Note that no iterator is passed to the signal handler, since the row is
;;; already deleted.
;;;
;;; This should be called by models after a row has been removed. The location
;;; pointed to by path should be the location that the row previously was at. It
;;; may not be a valid location anymore.
;;;
;;; tree_model :
;;;     the GtkTreeModel on which the signal is emitted
;;;
;;; path :
;;;     a GtkTreePath identifying the row
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-has-child-toggled" signal
;;;
;;; void user_function (GtkTreeModel *tree_model,
;;;                     GtkTreePath  *path,
;;;                     GtkTreeIter  *iter,
;;;                     gpointer      user_data)       : Run Last
;;;
;;; This signal is emitted when a row has gotten the first child row or lost its
;;; last child row.
;;;
;;; tree_model :
;;;     the GtkTreeModel on which the signal is emitted
;;;
;;; path :
;;;     a GtkTreePath identifying the row
;;;
;;; iter :
;;;     a valid GtkTreeIter pointing to the row
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-inserted" signal
;;;
;;; void user_function (GtkTreeModel *tree_model,
;;;                     GtkTreePath  *path,
;;;                     GtkTreeIter  *iter,
;;;                     gpointer      user_data)       : Run First
;;;
;;; This signal is emitted when a new row has been inserted in the model.
;;;
;;; Note that the row may still be empty at this point, since it is a common
;;; pattern to first insert an empty row, and then fill it with the desired
;;; values.
;;;
;;; tree_model :
;;;     the GtkTreeModel on which the signal is emitted
;;;
;;; path :
;;;     a GtkTreePath identifying the new row
;;;
;;; iter :
;;;     a valid GtkTreeIter pointing to the new row
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "rows-reordered" signal
;;;
;;; void user_function (GtkTreeModel *tree_model,
;;;                     GtkTreePath  *path,
;;;                     GtkTreeIter  *iter,
;;;                     gpointer      new_order,
;;;                     gpointer      user_data)       : Run First
;;;
;;; This signal is emitted when the children of a node in the GtkTreeModel have
;;; been reordered.
;;;
;;; Note that this signal is not emitted when rows are reordered by DND, since
;;; this is implemented by removing and then reinserting the row.
;;;
;;; tree_model :
;;;     the GtkTreeModel on which the signal is emitted
;;;
;;; path :
;;;     a GtkTreePath identifying the tree node whose children have been
;;;     reordered
;;;
;;; iter :
;;;     a valid GtkTreeIter pointing to the node whose
;;;
;;; new_order :
;;;     an array of integers mapping the current position of each child to its
;;;     old position before the re-ordering, i.e. new_order[newpos] = oldpos
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeModel
;;;
;;; typedef struct _GtkTreeModel GtkTreeModel;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkTreeModel" gtk-tree-model
  (:export t
   :type-initializer "gtk_tree_model_get_type"))

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeIter
;;;
;;; struct GtkTreeIter {
;;;   gint stamp;
;;;   gpointer user_data;
;;;   gpointer user_data2;
;;;   gpointer user_data3;
;;; };
;;;
;;; The GtkTreeIter is the primary structure for accessing a GtkTreeModel.
;;; Models are expected to put a unique integer in the stamp member, and put
;;; model-specific data in the three user_data members.
;;;
;;; gint stamp;
;;;     a unique stamp to catch invalid iterators
;;;
;;; gpointer user_data;
;;;     model-specific data
;;;
;;; gpointer user_data2;
;;;     model-specific data
;;;
;;; gpointer user_data3;
;;;     model-specific data
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

(export 'gtk-tree-iter)
(export 'gtk-tree-iter-stamp)
(export 'gtk-tree-iter-user-data)

;;; ----------------------------------------------------------------------------
;;; GtkTreePath
;;;
;;; typedef struct _GtkTreePath GtkTreePath;
;;; ----------------------------------------------------------------------------

(defctype gtk-tree-path :pointer)

(define-g-boxed-opaque gtk-tree-path "GtkTreePath"
  :alloc (%gtk-tree-path-new))

;;; ----------------------------------------------------------------------------
;;; GtkTreeRowReference
;;;
;;; typedef struct _GtkTreeRowReference GtkTreeRowReference;
;;; ----------------------------------------------------------------------------

(at-init ()
  (gobject::type-initializer-call "gtk_tree_row_reference_get_type"))

(define-g-boxed-opaque gtk-tree-row-reference "GtkTreeRowReference"
  :alloc (lambda () (error "")))

(export 'gtk-tree-row-reference)

;;; ----------------------------------------------------------------------------
;;; enum GtkTreeModelFlags
;;;
;;; typedef enum {
;;;   GTK_TREE_MODEL_ITERS_PERSIST = 1 << 0,
;;;   GTK_TREE_MODEL_LIST_ONLY     = 1 << 1
;;; } GtkTreeModelFlags;
;;;
;;; These flags indicate various properties of a GtkTreeModel.
;;;
;;; They are returned by gtk_tree_model_get_flags(), and must be static for the
;;; lifetime of the object. A more complete description of
;;; GTK_TREE_MODEL_ITERS_PERSIST can be found in the overview of this section.
;;;
;;; GTK_TREE_MODEL_ITERS_PERSIST
;;;     iterators survive all signals emitted by the tree
;;;
;;; GTK_TREE_MODEL_LIST_ONLY
;;;     the model is a list only, and never has children
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkTreeModelFlags" gtk-tree-model-flags
  (:export t
   :type-initializer "gtk_tree_model_flags_get_type")
  (:iters-persist 1)
  (:list-only 2))

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
  (:skip parent-instance g-type-interface)
  ;; some signals
  (:skip tree-model-row-changed :pointer)
  (:skip tree-model-row-inserted :pointer)
  (:skip tree-model-row-has-child-toggled :pointer)
  (:skip tree-model-row-deleted :pointer)
  (:skip tree-model-rows-reordered :pointer)
  ;; methods
  (get-flags (gtk-tree-model-flags (tree-model g-object)))
  (get-n-columns (:int (tree-model g-object)))
  (get-column-type (g-type-designator (tree-model g-object) (index :int)))
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
              (value (:pointer g-value)))
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
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
      (funcall fn model path iter)
      (stop-tree-model-iteration () t)
      (skip-tree-model-current () nil))))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new ()
;;;
;;; GtkTreePath * gtk_tree_path_new (void);
;;;
;;; Creates a new GtkTreePath. This structure refers to a row.
;;;
;;; Returns :
;;;     A newly created GtkTreePath.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_new" %gtk-tree-path-new) :pointer)

(defcfun ("gtk_tree_path_new" gtk-tree-path-new)
    (g-boxed-foreign gtk-tree-path))

(export 'gtk-tree-path-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_from_string ()
;;;
;;; GtkTreePath * gtk_tree_path_new_from_string (const gchar *path);
;;;
;;; Creates a new GtkTreePath initialized to path.
;;;
;;; path is expected to be a colon separated list of numbers. For example, the
;;; string "10:4:0" would create a path of depth 3 pointing to the 11th child of
;;; the root node, the 5th child of that 11th child, and the 1st child of that
;;; 5th child. If an invalid path string is passed in, NULL is returned.
;;;
;;; path :
;;;     The string representation of a path
;;;
;;; Returns :
;;;     A newly-created GtkTreePath, or NULL
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_new_from_string" gtk-tree-path-new-from-string)
    (g-boxed-foreign gtk-tree-path)
  (path :string))

(export 'gtk-tree-path-new-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_from_indices ()
;;;
;;; GtkTreePath * gtk_tree_path_new_from_indices (gint first_index, ...);
;;;
;;; Creates a new path with first_index and varargs as indices.
;;;
;;; first_index :
;;;     first integer
;;;
;;; ... :
;;;     list of integers terminated by -1
;;;
;;; Returns :
;;;     A newly created GtkTreePath
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defun gtk-tree-path-new-from-indices (&rest indices)
  (gtk-tree-path-new-from-string
    (string-right-trim ":" (format nil "~{~D:~}" indices))))

(export 'gtk-tree-path-new-from-indices)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_to_string ()
;;;
;;; gchar * gtk_tree_path_to_string (GtkTreePath *path);
;;;
;;; Generates a string representation of the path.
;;;
;;; This string is a ':' separated list of numbers. For example, "4:10:0:3"
;;; would be an acceptable return value for this string.
;;;
;;; path :
;;;     A GtkTreePath
;;;
;;; Returns :
;;;     A newly-allocated string. Must be freed with g_free().
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_to_string" gtk-tree-path-to-string) :string
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_new_first ()
;;;
;;; GtkTreePath * gtk_tree_path_new_first (void);
;;;
;;; Creates a new GtkTreePath.
;;;
;;; The string representation of this path is "0".
;;;
;;; Returns :
;;;     A new GtkTreePath
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_new_first" gtk-tree-path-new-first)
    (g-boxed-foreign gtk-tree-path))

(export 'gtk-tree-path-new-first)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_append_index ()
;;;
;;; void gtk_tree_path_append_index (GtkTreePath *path, gint index_);
;;;
;;; Appends a new index to a path.
;;;
;;; As a result, the depth of the path is increased.
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; index_ :
;;;     the index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_append_index" %gtk-tree-path-append-index) :void
  (path (g-boxed-foreign gtk-tree-path))
  (index :int))

;; We dot not modify the argument, but return the new value

(defun gtk-tree-path-append-index (path index)
  (let ((path (gtk-tree-path-copy path)))
    (%gtk-tree-path-append-index path index)
    path))

(export 'gtk-tree-path-append-index)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_prepend_index ()
;;;
;;; void gtk_tree_path_prepend_index (GtkTreePath *path, gint index_);
;;;
;;; Prepends a new index to a path.
;;;
;;; As a result, the depth of the path is increased.
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; index_ :
;;;     the index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_prepend_index" %gtk-tree-path-prepend-index) :void
  (path (g-boxed-foreign gtk-tree-path))
  (index :int))

;; We dot not modify the argument, but return the new value

(defun gtk-tree-path-prepend-index (path index)
  (let ((path (gtk-tree-path-copy path)))
    (%gtk-tree-path-prepend-index path index)
    path))

(export 'gtk-tree-path-prepend-index)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_get_depth ()
;;;
;;; gint gtk_tree_path_get_depth (GtkTreePath *path);
;;;
;;; Returns the current depth of path.
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; Returns :
;;;     The depth of path
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_get_depth" gtk-tree-path-get-depth) :int
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-get-depth)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_get_indices ()
;;;
;;; gint * gtk_tree_path_get_indices (GtkTreePath *path);
;;;
;;; Returns the current indices of path.
;;;
;;; This is an array of integers, each representing a node in a tree. This value
;;; should not be freed.
;;;
;;; The length of the array can be obtained with gtk_tree_path_get_depth().
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; Returns :
;;;     The current indices, or NULL
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_get_indices" %gtk-tree-path-get-indices)
    (:pointer :int)
  (path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-get-indices (path)
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
;;;
;;; void gtk_tree_path_free (GtkTreePath *path);
;;;
;;; Frees path. If path is NULL, it simply returns.
;;;
;;; path :
;;;     a GtkTreePath
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_free" gtk-tree-path-free) :void
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-free)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_copy ()
;;;
;;; GtkTreePath * gtk_tree_path_copy (const GtkTreePath *path);
;;;
;;; Creates a new GtkTreePath as a copy of path.
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; Returns :
;;;     a new GtkTreePath
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_copy" gtk-tree-path-copy)
    (g-boxed-foreign gtk-tree-path)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_compare ()
;;;
;;; gint gtk_tree_path_compare (const GtkTreePath *a, const GtkTreePath *b);
;;;
;;; Compares two paths.
;;;
;;; If a appears before b in a tree, then -1 is returned. If b appears before a,
;;; then 1 is returned. If the two nodes are equal, then 0 is returned.
;;;
;;; a :
;;;     a GtkTreePath
;;;
;;; b :
;;;     a GtkTreePath to compare with
;;;
;;; Returns :
;;;     the relative positions of a and b
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_compare" gtk-tree-path-compare ) :int
  (tree-path-1 (g-boxed-foreign gtk-tree-path))
  (tree-path-2 (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_next ()
;;;
;;; void gtk_tree_path_next (GtkTreePath *path);
;;;
;;; Moves the path to point to the next node at the current depth.
;;;
;;; path :
;;;     a GtkTreePath
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_next" %gtk-tree-path-next) :void
  (tree-path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-next (path)
  (%gtk-tree-path-next path)
  path)

(export 'gtk-tree-path-next)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_prev ()
;;;
;;; gboolean gtk_tree_path_prev (GtkTreePath *path);
;;;
;;; Moves the path to point to the previous node at the current depth, if it
;;; exists.
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; Returns :
;;;     TRUE if path has a previous node, and the move was made
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_prev" %gtk-tree-path-prev) :boolean
  (tree-path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-prev (path)
  (when (%gtk-tree-path-prev path)
    path))

(export 'gtk-tree-path-prev)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_up ()
;;;
;;; gboolean gtk_tree_path_up (GtkTreePath *path);
;;;
;;; Moves the path to point to its parent node, if it has a parent.
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; Returns :
;;;     TRUE if path has a parent, and the move was made
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_up" %gtk-tree-path-up) :boolean
  (tree-path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-up (path)
  (when (%gtk-tree-path-up path)
    path))

(export 'gtk-tree-path-up)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_down ()
;;;
;;; void gtk_tree_path_down (GtkTreePath *path);
;;;
;;; Moves path to point to the first child of the current path.
;;;
;;; path :
;;;     a GtkTreePath
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_down" %gtk-tree-path-down) :void
  (tree-path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-path-down (path)
  (%gtk-tree-path-down path)
  path)

(export 'gtk-tree-path-down)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_is_ancestor ()
;;;
;;; gboolean gtk_tree_path_is_ancestor (GtkTreePath *path,
;;;                                     GtkTreePath *descendant);
;;;
;;; Returns TRUE if descendant is a descendant of path.
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; descendant :
;;;     another GtkTreePath
;;;
;;; Returns :
;;;     TRUE if descendant is contained inside path
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_is_ancestor" gtk-tree-path-is-ancestor) :boolean
  (tree-path (g-boxed-foreign gtk-tree-path))
  (descendant (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-is-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_path_is_descendant ()
;;;
;;; gboolean gtk_tree_path_is_descendant (GtkTreePath *path,
;;;                                       GtkTreePath *ancestor);
;;;
;;; Returns TRUE if path is a descendant of ancestor.
;;;
;;; path :
;;;     a GtkTreePath
;;;
;;; ancestor :
;;;     another GtkTreePath
;;;
;;; Returns :
;;;     TRUE if ancestor contains path somewhere below it
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_path_is_descendant" gtk-tree-path-is-descendant) :boolean
  (tree-path (g-boxed-foreign gtk-tree-path))
  (ancestor (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-path-is-descendant)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_new ()
;;;
;;; GtkTreeRowReference * gtk_tree_row_reference_new (GtkTreeModel *model,
;;;                                                   GtkTreePath *path);
;;;
;;; Creates a row reference based on path.
;;;
;;; This reference will keep pointing to the node pointed to by path, so long as
;;; it exists. Any changes that occur on model are propagated, and the path is
;;; updated appropriately. If path isn't a valid path in model, then NULL is
;;; returned.
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

(defcfun ("gtk_tree_row_reference_new" gtk-tree-row-reference-new)
    (g-boxed-foreign gtk-tree-row-reference :return)
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
;;;
;;; GtkTreeModel * gtk_tree_row_reference_get_model
;;;                                            (GtkTreeRowReference *reference);
;;;
;;; Returns the model that the row reference is monitoring.
;;;
;;; reference :
;;;     a GtkTreeRowReference
;;;
;;; Returns :
;;;     the model
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-tree-row-reference
                              gtk-tree-row-reference-model
  :reader "gtk_tree_row_reference_get_model"
  :type (g-object gtk-tree-model))

(export 'gtk-tree-row-reference-model)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_get_path ()
;;;
;;; GtkTreePath * gtk_tree_row_reference_get_path
;;;                                            (GtkTreeRowReference *reference);
;;;
;;; Returns a path that the row reference currently points to, or NULL if the
;;; path pointed to is no longer valid.
;;;
;;; reference :
;;;     a GtkTreeRowReference
;;;
;;; Returns :
;;;     a current path, or NULL
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-tree-row-reference gtk-tree-row-reference-path
  :reader "gtk_tree_row_reference_get_path"
  :type (g-boxed-foreign gtk-tree-path :return))

(export 'gtk-tree-row-reference-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_row_reference_valid ()
;;;
;;; gboolean gtk_tree_row_reference_valid (GtkTreeRowReference *reference);
;;;
;;; Returns TRUE if the reference is non-NULL and refers to a current valid
;;; path.
;;;
;;; reference :
;;;     a GtkTreeRowReference, or NULL
;;;
;;; Returns :
;;;     TRUE if reference points to a valid path
;;; ----------------------------------------------------------------------------

(define-boxed-opaque-accessor gtk-tree-row-reference
                              gtk-tree-row-reference-valid
  :reader "gtk_tree_row_reference_valid" :type :boolean)

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
;;;
;;; GtkTreeModelFlags gtk_tree_model_get_flags (GtkTreeModel *tree_model);
;;;
;;; Returns a set of flags supported by this interface.
;;;
;;; The flags are a bitwise combination of GtkTreeModelFlags. The flags
;;; supported should not change during the lifetime of the tree_model.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; Returns :
;;;     the flags supported by this interface
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_flags" gtk-tree-model-get-flags)
    gtk-tree-model-flags
  (tree-model (g-object gtk-tree-model)))

(export 'gtk-tree-model-get-flags)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_n_columns ()
;;;
;;; gint gtk_tree_model_get_n_columns (GtkTreeModel *tree_model);
;;;
;;; Returns the number of columns supported by tree_model.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; Returns :
;;;     the number of columns
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_n_columns" gtk-tree-model-get-n-columns) :int
  (tree-model (g-object gtk-tree-model)))

(export 'gtk-tree-model-get-n-columns)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_column_type ()
;;;
;;; GType gtk_tree_model_get_column_type (GtkTreeModel *tree_model, gint index_)
;;;
;;; Returns the type of the column.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; index_ :
;;;     the column index
;;;
;;; Returns :
;;;     the type of the column
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_column_type" gtk-tree-model-get-column-type)
    g-type-designator
  (tree-model (g-object gtk-tree-model))
  (index :int))

(export 'gtk-tree-model-get-column-type)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter ()
;;;
;;; gboolean gtk_tree_model_get_iter (GtkTreeModel *tree_model,
;;;                                   GtkTreeIter *iter,
;;;                                   GtkTreePath *path);
;;;
;;; Sets iter to a valid iterator pointing to path. If path does not exist, iter
;;; is set to an invalid iterator and FALSE is returned.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the uninitialized GtkTreeIter
;;;
;;; path :
;;;     the GtkTreePath
;;;
;;; Returns :
;;;     TRUE, if iter was set
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_iter" %gtk-tree-model-get-iter) :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign gtk-tree-iter))
  (path (g-boxed-foreign gtk-tree-path)))

(defun gtk-tree-model-get-iter (tree-model path)
  (let ((iter (make-gtk-tree-iter)))
    (if (%gtk-tree-model-get-iter tree-model iter path)
        iter
        nil)))

(export 'gtk-tree-model-get-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter_from_string ()
;;;
;;; gboolean gtk_tree_model_get_iter_from_string (GtkTreeModel *tree_model,
;;;                                               GtkTreeIter *iter,
;;;                                               const gchar *path_string);
;;;
;;; Sets iter to a valid iterator pointing to path_string, if it exists.
;;; Otherwise, iter is left invalid and FALSE is returned.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     an uninitialized GtkTreeIter
;;;
;;; path_string :
;;;     a string representation of a GtkTreePath
;;;
;;; Returns :
;;;     TRUE, if iter was set
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_iter_from_string"
          %gtk-tree-model-get-iter-from-string)
    :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign gtk-tree-iter))
  (path-string :string))

(defun gtk-tree-model-get-iter-from-string (tree-model path-string)
  (let ((iter (make-gtk-tree-iter)))
    (if (%gtk-tree-model-get-iter-from-string tree-model iter path-string)
        iter
        nil)))

(export 'gtk-tree-model-get-iter-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_iter_first ()
;;;
;;; gboolean gtk_tree_model_get_iter_first (GtkTreeModel *tree_model,
;;;                                         GtkTreeIter *iter);
;;;
;;; Initializes iter with the first iterator in the tree (the one at the path
;;; "0") and returns TRUE. Returns FALSE if the tree is empty.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the uninitialized GtkTreeIter
;;;
;;; Returns :
;;;     TRUE, if iter was set
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_iter_first" %gtk-tree-model-get-iter-first)
    :boolean
  (model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-get-iter-first (tree-model)
  (let ((iter (make-gtk-tree-iter)))
    (if (%gtk-tree-model-get-iter-first tree-model iter)
        iter
        nil)))

(export 'gtk-tree-model-get-iter-first)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_path ()
;;;
;;; GtkTreePath * gtk_tree_model_get_path (GtkTreeModel *tree_model,
;;;                                        GtkTreeIter *iter);
;;;
;;; Returns a newly-created GtkTreePath referenced by iter.
;;;
;;; This path should be freed with gtk_tree_path_free().
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the GtkTreeIter
;;;
;;; Returns :
;;;     a newly-created GtkTreePath
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_path" gtk-tree-model-get-path)
     (g-boxed-foreign gtk-tree-path :return)
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-get-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_value ()
;;;
;;; void gtk_tree_model_get_value (GtkTreeModel *tree_model,
;;;                                GtkTreeIter *iter,
;;;                                gint column,
;;;                                GValue *value);
;;;
;;; Initializes and sets value to that at column.
;;;
;;; When done with value, g_value_unset() needs to be called to free any
;;; allocated memory.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the GtkTreeIter
;;;
;;; column :
;;;     the column to lookup the value at
;;;
;;; value :
;;;     an empty GValue to set
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_value" %gtk-tree-model-get-value) :void
  (model g-object)
  (iter (g-boxed-foreign gtk-tree-iter))
  (column :int)
  (value (:pointer g-value)))

(defun gtk-tree-model-get-value (tree-model iter column)
  (with-foreign-object (v 'g-value)
    (g-value-zero v)
    (%gtk-tree-model-get-value tree-model iter column v)
    (prog1
      (parse-g-value v)
      (g-value-unset v))))

(export 'gtk-tree-model-get-value)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_next ()
;;;
;;; gboolean gtk_tree_model_iter_next (GtkTreeModel *tree_model,
;;;                                    GtkTreeIter *iter);
;;;
;;; Sets iter to point to the node following it at the current level.
;;;
;;; If there is no next iter, FALSE is returned and iter is set to be invalid.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the GtkTreeIter
;;;
;;; Returns :
;;;     TRUE if iter has been changed to the next node
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_next" %gtk-tree-model-iter-next) :boolean
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-next (tree-model iter)
  (let ((iter-new (copy-gtk-tree-iter iter)))
    (when (%gtk-tree-model-iter-next tree-model iter-new)
      iter-new)))

(export 'gtk-tree-model-iter-next)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_previous ()
;;;
;;; gboolean gtk_tree_model_iter_previous (GtkTreeModel *tree_model,
;;;                                        GtkTreeIter *iter);
;;;
;;; Sets iter to point to the previous node at the current level.
;;;
;;; If there is no previous iter, FALSE is returned and iter is set to be
;;; invalid.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the GtkTreeIter
;;;
;;; Returns :
;;;     TRUE if iter has been changed to the previous node
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_previous" %gtk-tree-model-iter-previous) :boolean
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-previous (tree-model iter)
  (let ((iter-new (copy-gtk-tree-iter iter)))
    (when (%gtk-tree-model-iter-previous tree-model iter-new)
      iter-new)))

(export 'gtk-tree-model-iter-previous)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_children ()
;;;
;;; gboolean gtk_tree_model_iter_children (GtkTreeModel *tree_model,
;;;                                        GtkTreeIter *iter,
;;;                                        GtkTreeIter *parent);
;;;
;;; Sets iter to point to the first child of parent.
;;;
;;; If parent has no children, FALSE is returned and iter is set to be invalid.
;;; parent will remain a valid node after this function has been called.
;;;
;;; If parent is NULL returns the first node, equivalent to
;;; gtk_tree_model_get_iter_first (tree_model, iter);
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the new GtkTreeIter to be set to the child
;;;
;;; parent :
;;;     the GtkTreeIter, or NULL
;;;
;;; Returns :
;;;     TRUE, if child has been set to the first child
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_children" %gtk-tree-model-iter-children) :boolean
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-children (tree-model parent)
  (let ((iter (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-children tree-model iter parent)
      iter)))

(export 'gtk-tree-model-iter-children)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_has_child ()
;;;
;;; gboolean gtk_tree_model_iter_has_child (GtkTreeModel *tree_model,
;;;                                         GtkTreeIter *iter);
;;;
;;; Returns TRUE if iter has children, FALSE otherwise.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the GtkTreeIter to test for children
;;;
;;; Returns :
;;;     TRUE if iter has children
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_has_child" gtk-tree-model-iter-has-child)
    :boolean
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-iter-has-child)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_n_children ()
;;;
;;; gint gtk_tree_model_iter_n_children (GtkTreeModel *tree_model,
;;;                                      GtkTreeIter *iter);
;;;
;;; Returns the number of children that iter has.
;;;
;;; As a special case, if iter is NULL, then the number of toplevel nodes is
;;; returned.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the GtkTreeIter, or NULL
;;;
;;; Returns :
;;;     the number of children of iter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_n_children" gtk-tree-model-iter-n-children) :int
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-iter-n-children)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_nth_child ()
;;;
;;; gboolean gtk_tree_model_iter_nth_child (GtkTreeModel *tree_model,
;;;                                         GtkTreeIter *iter,
;;;                                         GtkTreeIter *parent,
;;;                                         gint n);
;;;
;;; Sets iter to be the child of parent, using the given index.
;;;
;;; The first index is 0. If n is too big, or parent has no children, iter is
;;; set to an invalid iterator and FALSE is returned. parent will remain a valid
;;; node after this function has been called. As a special case, if parent is
;;; NULL, then the nth root node is set.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the GtkTreeIter to set to the nth child
;;;
;;; parent :
;;;     the GtkTreeIter to get the child from, or NULL
;;;
;;; n :
;;;     the index of the desired child
;;;
;;; Returns :
;;;     TRUE, if parent has an nth child
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_nth_child" %gtk-tree-model-iter-nth-child)
    :boolean
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter))
  (parent (g-boxed-foreign gtk-tree-iter))
  (n :int))

(defun gtk-tree-model-iter-nth-child (tree-model parent n)
  (let ((iter (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-nth-child tree-model iter parent n)
      iter)))

(export 'gtk-tree-model-iter-nth-child)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_iter_parent ()
;;;
;;; gboolean gtk_tree_model_iter_parent (GtkTreeModel *tree_model,
;;;                                      GtkTreeIter *iter,
;;;                                      GtkTreeIter *child);
;;;
;;; Sets iter to be the parent of child.
;;;
;;; If child is at the toplevel, and doesn't have a parent, then iter is set to
;;; an invalid iterator and FALSE is returned. child will remain a valid node
;;; after this function has been called.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the new GtkTreeIter to set to the parent
;;;
;;; child :
;;;     the GtkTreeIter
;;;
;;; Returns :
;;;     TRUE, if iter is set to the parent of child
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_iter_parent" %gtk-tree-model-iter-parent) :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign gtk-tree-iter))
  (child (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-model-iter-parent (tree-model child)
  (let ((parent (make-gtk-tree-iter)))
    (when (%gtk-tree-model-iter-parent tree-model parent child)
      parent)))

(export 'gtk-tree-model-iter-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get_string_from_iter ()
;;;
;;; gchar * gtk_tree_model_get_string_from_iter (GtkTreeModel *tree_model,
;;;                                              GtkTreeIter *iter);
;;;
;;; Generates a string representation of the iter.
;;;
;;; This string is a ':' separated list of numbers. For example, "4:10:0:3"
;;; would be an acceptable return value for this string.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     a GtkTreeIter
;;;
;;; Returns :
;;;     a newly-allocated string. Must be freed with g_free().
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_get_string_from_iter"
           gtk-tree-model-get-string-from-iter)
    (g-string :free-from-foreign t)
  (tree-model g-object)
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-get-string-from-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_ref_node ()
;;;
;;; void gtk_tree_model_ref_node (GtkTreeModel *tree_model, GtkTreeIter *iter);
;;;
;;; Lets the tree ref the node.
;;;
;;; This is an optional method for models to implement. To be more specific,
;;; models may ignore this call as it exists primarily for performance reasons.
;;;
;;; This function is primarily meant as a way for views to let caching models
;;; know when nodes are being displayed (and hence, whether or not to cache that
;;; node). Being displayed means a node is in an expanded branch, regardless of
;;; whether the node is currently visible in the viewport. For example, a
;;; file-system based model would not want to keep the entire file-hierarchy in
;;; memory, just the sections that are currently being displayed by every
;;; current view.
;;;
;;; A model should be expected to be able to get an iter independent of its
;;; reffed state.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the GtkTreeIter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_ref_node" gtk-tree-model-ref-node) :void
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-ref-node)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_unref_node ()
;;;
;;; void gtk_tree_model_unref_node (GtkTreeModel *tree_model,
;;;                                 GtkTreeIter *iter);
;;;
;;; Lets the tree unref the node.
;;;
;;; This is an optional method for models to implement. To be more specific,
;;; models may ignore this call as it exists primarily for performance reasons.
;;; For more information on what this means, see gtk_tree_model_ref_node().
;;;
;;; Please note that nodes that are deleted are not unreffed.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     the GtkTreeIter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_unref_node" gtk-tree-model-unref-node) :void
  (tree-model (g-object gtk-tree-model))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-model-unref-node)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_get ()
;;;
;;; void gtk_tree_model_get (GtkTreeModel *tree_model,
;;;                          GtkTreeIter *iter,
;;;                          ...);
;;;
;;; Gets the value of one or more cells in the row referenced by iter. The
;;; variable argument list should contain integer column numbers, each column
;;; number followed by a place to store the value being retrieved. The list is
;;; terminated by a -1. For example, to get a value from column 0 with type
;;; G_TYPE_STRING, you would write:
;;; gtk_tree_model_get (model, iter, 0, &place_string_here, -1), where
;;; place_string_here is a gchar* to be filled with the string.
;;;
;;; Returned values with type G_TYPE_OBJECT have to be unreferenced, values with
;;; type G_TYPE_STRING or G_TYPE_BOXED have to be freed. Other values are passed
;;; by value.
;;;
;;; tree_model :
;;;     a GtkTreeModel
;;;
;;; iter :
;;;     a row in tree_model
;;;
;;; ... :
;;;     pairs of column number and value return locations, terminated by -1
;;; ----------------------------------------------------------------------------

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
;;;
;;; void gtk_tree_model_foreach (GtkTreeModel *model,
;;;                              GtkTreeModelForeachFunc func,
;;;                              gpointer user_data);
;;;
;;; Calls func on each node in model in a depth-first fashion.
;;;
;;; If func returns TRUE, then the tree ceases to be walked, and
;;; gtk_tree_model_foreach() returns.
;;;
;;; model :
;;;     a GtkTreeModel
;;;
;;; func :
;;;     a function to be called on each row
;;;
;;; user_data :
;;;     user data to passed to func
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_model_foreach" %gtk-tree-model-foreach) :void
  (model g-object)
  (func :pointer)
  (data :pointer))

(defun gtk-tree-model-foreach (model func)
  (with-stable-pointer (ptr func)
    (%gtk-tree-model-foreach model (callback gtk-tree-model-foreach-cb) ptr)))

(export 'gk-tree-model-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_model_row_changed ()
;;;
;;; void gtk_tree_model_row_changed (GtkTreeModel *tree_model,
;;;                                  GtkTreePath *path,
;;;                                  GtkTreeIter *iter);
;;;
;;; Emits the "row-changed" signal on tree_model.
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

;;; ----------------------------------------------------------------------------

;; Implementation of array-list-store

(defclass array-list-store (gtk-tree-model)
  ((items :initform (make-array 0 :adjustable t :fill-pointer t)
          :reader store-items)
   (columns-getters :initform (make-array 0 :adjustable t :fill-pointer t)
                    :reader store-getters)
   (columns-types :initform (make-array 0 :adjustable t :fill-pointer t)
                  :reader store-types))
  (:metaclass gobject-class)
  (:g-type-name . "LispArrayListStore"))

(export 'array-list-store)

(register-object-type-implementation "LispArrayListStore"
                                     array-list-store
                                     "GObject"
                                     ("GtkTreeModel")
                                     nil)

(defun store-items-count (store)
  (length (store-items store)))

(export 'store-items-count)

(defun store-item (store index)
  (aref (store-items store) index))

(export 'store-item)

(defun store-add-item (store item)
  (vector-push-extend item (store-items store))
  (let* ((path (make-instance 'gtk-tree-path))
         (iter (make-gtk-tree-iter)))
    (setf (gtk-tree-path-indices path) (list (1- (length (store-items store)))))
    (setf (gtk-tree-iter-stamp iter)
          0
          (gtk-tree-iter-user-data iter)
          (1- (length (store-items store))))
    (g-signal-emit store "row-inserted" path iter)))

(export 'store-add-item)

(defun store-remove-item (store item &key (test 'eq))
  (with-slots (items) store
    (let ((index (position item items :test test)))
      (unless index (error "No such item~%~A~%in list-store~%~A" item store))
      (setf items (delete item items :test test))
      (let ((path (make-instance 'gtk-tree-path)))
        (setf (gtk-tree-path-indices path) (list index))
        (g-signal-emit store "row-deleted" path)))))

(export 'store-remove-item)

(defun store-add-column (store type getter)
  (vector-push-extend type (store-types store))
  (vector-push-extend getter (store-getters store))
  (1- (length (store-types store))))

(export 'store-add-column)

(defmethod gtk-tree-model-get-flags-impl ((model array-list-store))
  '(:list-only))

(defmethod gtk-tree-model-get-n-columns-impl ((model array-list-store))
  (length (store-types model)))

(defmethod gtk-tree-model-get-column-type-impl
    ((tree-model array-list-store) index)
  (aref (store-types tree-model) index))

(defmethod gtk-tree-model-get-iter-impl ((model array-list-store) iter path)
  (let ((indices (gtk-tree-path-indices path)))
    (when (and (= 1 (length indices))
               (< (first indices) (length (store-items model))))
      (setf (gtk-tree-iter-stamp iter)
            0
            (gtk-tree-iter-user-data iter)
            (first indices))
      t)))

(defmethod gtk-tree-model-ref-node-impl ((model array-list-store) iter)
  (declare (ignorable model iter)))

(defmethod gtk-tree-model-unref-node-impl ((model array-list-store) iter)
  (declare (ignorable model iter)))

(defmethod gtk-tree-model-iter-next-impl ((model array-list-store) iter)
  (let ((n (gtk-tree-iter-user-data iter)))
    (when (< n (1- (length (store-items model))))
      (setf (gtk-tree-iter-user-data iter) (1+ n))
      t)))

(defmethod gtk-tree-model-iter-nth-child-impl
    ((model array-list-store) iter parent n)
  (declare (ignorable parent))
  (setf (gtk-tree-iter-stamp iter) 0
        (gtk-tree-iter-user-data iter) n)
  t)

(defmethod gtk-tree-model-iter-children-impl
    ((model array-list-store) iter parent)
  (declare (ignore iter parent))
  nil)

(defmethod gtk-tree-model-iter-n-children-impl ((model array-list-store) iter)
  (if (null iter)
      (length (store-items model))
      0))

(defmethod gtk-tree-model-get-path-impl ((model array-list-store) iter)
  (let ((path (make-instance 'gtk-tree-path)))
    (setf (gtk-tree-path-indices path)
          (list (gtk-tree-iter-user-data iter)))
    path))

(defmethod gtk-tree-model-iter-has-child-impl ((model array-list-store) iter)
  (declare (ignorable iter))
  nil)

(defgeneric gtk-tree-model-item (model iter-or-path))

(defmethod gtk-tree-model-item ((model array-list-store) (iter gtk-tree-iter))
  (let ((n-row (gtk-tree-iter-user-data iter)))
    (aref (store-items model) n-row)))

(defmethod gtk-tree-model-item ((model array-list-store) (path gtk-tree-path))
  (let ((n-row (first (gtk-tree-path-indices path))))
    (aref (store-items model) n-row)))

(export 'gtk-tree-model-item)

(defmethod gtk-tree-model-get-value-impl ((model array-list-store) iter n)
  (let ((n-row (gtk-tree-iter-user-data iter)))
    (values (funcall (aref (store-getters model) n)
                     (aref (store-items model) n-row))
            (aref (store-types model) n))))

;;; ----------------------------------------------------------------------------

(defun array-insert-at (array element index)
  (assert (adjustable-array-p array))
  (adjust-array array (1+ (length array)) :fill-pointer t)
  (iter (for i from (1- (length array)) above index)
        (setf (aref array i)
              (aref array (1- i))))
  (setf (aref array index) element)
  array)

(defun array-remove-at (array index)
  (assert (adjustable-array-p array))
  (iter (for i from index below (1- (length array)))
        (setf (aref array i)
              (aref array (1+ i))))
  (adjust-array array (1- (length array)) :fill-pointer t)
  array)

(defstruct tree-node
  (tree nil)
  (parent nil)
  (id nil)
  (item nil)
  (children (make-array 0 :element-type 'tree-node
                          :adjustable t
                          :fill-pointer t)))

(defclass tree-lisp-store (gtk-tree-model)
  ((columns-getters :initform (make-array 0 :adjustable t :fill-pointer t)
                    :reader tree-lisp-store-getters)
   (columns-types :initform (make-array 0 :adjustable t :fill-pointer t)
                  :reader tree-lisp-store-types)
   (root :initform (make-tree-node)
         :reader tree-lisp-store-root)
   (id-map :initform (make-hash-table)
           :reader tree-lisp-store-id-map)
   (next-id-value :initform 0
                  :accessor tree-lisp-store-next-id-value))
  (:metaclass gobject-class)
  (:g-type-name . "LispTreeStore"))

(defmethod initialize-instance :after ((object tree-lisp-store) &key
                                       &allow-other-keys)
  (setf (tree-node-tree (tree-lisp-store-root object)) object))

(register-object-type-implementation "LispTreeStore"
                                     tree-lisp-store
                                     "GObject"
                                     ("GtkTreeModel")
                                     nil)

(defun map-subtree (node fn)
  (funcall fn node)
  (iter (for child in-vector (tree-node-children node))
        (map-subtree child fn)))

(defun clear-id (node)
  (map-subtree node
               (lambda (n)
                 (when (and (tree-node-id n)
                            (tree-node-tree n))
                   (remhash (tree-node-id n)
                            (tree-lisp-store-id-map (tree-node-tree n))))
                 (setf (tree-node-id n) nil))))

(defun set-node-tree (node tree)
  (map-subtree node
               (lambda (n)
                 (setf (tree-node-tree n) tree))))

(defun tree-node-insert-at (node child index)
  (assert (null (tree-node-parent child)))
  (clear-id child)
  (setf (tree-node-parent child) node)
  (set-node-tree child (tree-node-tree node))
  (array-insert-at (tree-node-children node) child index)
  (notice-tree-node-insertion (tree-node-tree node) node child index)
  node)

(defun tree-node-child-at (node index)
  (aref (tree-node-children node) index))

(defun tree-node-remove-at (node index)
  (assert (<= 0 index (1- (length (tree-node-children node)))))
  (let ((child (tree-node-child-at node index)))
    (clear-id child)
    (setf (tree-node-parent child) nil)
    (set-node-tree child nil)
    (array-remove-at (tree-node-children node) index)
    (notice-tree-node-removal (tree-node-tree node) node child index)))

(defun tree-lisp-store-add-column (store column-type column-getter)
  (vector-push-extend column-getter (tree-lisp-store-getters store))
  (vector-push-extend column-type (tree-lisp-store-types store)))

(defmethod gtk-tree-model-get-flags-impl ((store tree-lisp-store))
  nil)

(defmethod gtk-tree-model-get-n-columns-impl ((store tree-lisp-store))
  (length (tree-lisp-store-getters store)))

(defmethod gtk-tree-model-get-column-type-impl ((store tree-lisp-store) index)
  (aref (tree-lisp-store-types store) index))

(defun get-node-by-indices (root indices)
  (if indices
      (get-node-by-indices (tree-node-child-at root (first indices))
                           (rest indices))
      root))

(defun get-node-by-path (tree path)
  (let ((indices (gtk-tree-path-indices path)))
    (get-node-by-indices (tree-lisp-store-root tree) indices)))

(defun get-node-path (node)
  (iter (with z = nil)
        (for parent = (tree-node-parent node))
        (while parent)
        (for index = (position node (tree-node-children parent)))
        (push index z)
        (setf node parent)
        (finally (return z))))

(defun tree-lisp-store-get-next-id (tree)
  (incf (tree-lisp-store-next-id-value tree)))

(defun tree-lisp-store-add-id-map (tree id node)
  (setf (gethash id (tree-lisp-store-id-map tree)) node))

(defun get-assigned-id (tree node)
  (or (tree-node-id node)
      (let ((id (tree-lisp-store-get-next-id tree)))
        (tree-lisp-store-add-id-map tree id node)
        (setf (tree-node-id node) id)
        id)))

(defun get-node-by-id (tree id)
  (gethash id (tree-lisp-store-id-map tree)))

(defmethod gtk-tree-model-get-iter-impl ((store tree-lisp-store) iter path)
  (let* ((node (get-node-by-path store path))
         (node-idx (get-assigned-id store node)))
    (setf (gtk-tree-iter-stamp iter) 0
          (gtk-tree-iter-user-data iter) node-idx)))

(defun get-node-by-iter (tree iter)
  (get-node-by-id tree (gtk-tree-iter-user-data iter)))

(defmethod gtk-tree-model-get-path-impl ((store tree-lisp-store) iter)
  (let* ((path (make-instance 'gtk-tree-path))
         (node (get-node-by-iter store iter))
         (indices (get-node-path node)))
    (setf (gtk-tree-path-indices path) indices)
    path))

(defmethod gtk-tree-model-get-value-impl ((store tree-lisp-store) iter n)
  (let* ((node (get-node-by-iter store iter))
         (getter (aref (tree-lisp-store-getters store) n))
         (type (aref (tree-lisp-store-types store) n)))
    (values (funcall getter (tree-node-item node))
            type)))

(defmethod gtk-tree-model-iter-next-impl ((store tree-lisp-store) iter)
  (let* ((node (get-node-by-iter store iter))
         (parent (tree-node-parent node))
         (index (position node (tree-node-children parent))))
    (when (< (1+ index) (length (tree-node-children parent)))
      (setf (gtk-tree-iter-stamp iter)
            0
            (gtk-tree-iter-user-data iter)
            (get-assigned-id store (tree-node-child-at parent (1+ index))))
      t)))

(defmethod gtk-tree-model-iter-children-impl ((store tree-lisp-store) iter parent)
  (let* ((node (if parent
                   (get-node-by-iter store parent)
                   (tree-lisp-store-root store))))
    (when (plusp (length (tree-node-children node)))
      (setf (gtk-tree-iter-stamp iter)
            0
            (gtk-tree-iter-user-data iter)
            (get-assigned-id store (tree-node-child-at node 0)))
      t)))

(defmethod gtk-tree-model-iter-has-child-impl ((store tree-lisp-store) iter)
  (let ((node (get-node-by-iter store iter)))
    (plusp (length (tree-node-children node)))))

(defmethod gtk-tree-model-iter-n-children-impl ((store tree-lisp-store) iter)
  (let* ((node (if iter
                   (get-node-by-iter store iter)
                   (tree-lisp-store-root store))))
    (length (tree-node-children node))))

(defmethod gtk-tree-model-iter-nth-child-impl
    ((store tree-lisp-store) iter parent n)
  (let* ((node (if parent
                   (get-node-by-iter store parent)
                   (tree-lisp-store-root store)))
         (requested-node (tree-node-child-at node n)))
    (setf (gtk-tree-iter-stamp iter)
          0
          (gtk-tree-iter-user-data iter)
          (get-assigned-id store requested-node))
    t))

(defmethod gtk-tree-model-iter-parent-impl ((store tree-lisp-store) iter child)
  (let ((node (get-node-by-iter store child)))
    (when (tree-node-parent node)
      (setf (gtk-tree-iter-stamp iter)
            0
            (gtk-tree-iter-user-data iter)
            (get-assigned-id store (tree-node-parent node))))))

(defmethod gtk-tree-model-ref-node-impl ((store tree-lisp-store) iter)
  (declare (ignorable iter)))

(defmethod gtk-tree-model-unref-node-impl ((store tree-lisp-store) iter)
  (declare (ignorable iter)))

;;; ----------------------------------------------------------------------------

(defun notice-tree-node-insertion (tree node child index)
  (declare (ignore node index))
  (when tree
    (let* ((path (make-instance 'gtk-tree-path))
           (iter (make-gtk-tree-iter)))
      (setf (gtk-tree-path-indices path) (get-node-path child)
            (gtk-tree-iter-stamp iter) 0
            (gtk-tree-iter-user-data iter) (get-assigned-id tree child))
      (g-signal-emit tree "row-inserted" path iter)
      (when (plusp (length (tree-node-children child)))
        (g-signal-emit tree "row-has-child-toggled" path iter)))))

(defun notice-tree-node-removal (tree node child index)
  (declare (ignore child))
  (when tree
    (let ((path (make-instance 'gtk-tree-path)))
      (setf (gtk-tree-path-indices path)
            (nconc (get-node-path node) (list index)))
      (g-signal-emit tree "row-deleted" path))
    (when (zerop (length (tree-node-children node)))
      (let* ((path (make-instance 'gtk-tree-path))
             (iter (make-gtk-tree-iter)))
        (setf (gtk-tree-path-indices path)
              (get-node-path node)
              (gtk-tree-iter-stamp iter)
              0
              (gtk-tree-iter-user-data iter)
              (get-assigned-id tree node))
        (g-signal-emit tree "row-has-child-toggled" path iter)))))

;;; --- End of file gtk.tree-model.lisp ----------------------------------------
