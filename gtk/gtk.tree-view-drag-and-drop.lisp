;;; ----------------------------------------------------------------------------
;;; gtk.tree-view-drag-and-drop.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkTreeView drag-and-drop
;;; 
;;; Interfaces for drag-and-drop support in GtkTreeView
;;; 	
;;; Synopsis
;;; 
;;;     GtkTreeDragSource
;;;     GtkTreeDragSourceIface
;;;
;;;     gtk_tree_drag_source_drag_data_delete
;;;     gtk_tree_drag_source_drag_data_get
;;;     gtk_tree_drag_source_row_draggable
;;;
;;;     GtkTreeDragDest
;;;     GtkTreeDragDestIface
;;;
;;;     gtk_tree_drag_dest_drag_data_received
;;;     gtk_tree_drag_dest_row_drop_possible
;;;     gtk_tree_set_row_drag_data
;;;     gtk_tree_get_row_drag_data
;;; 
;;; Object Hierarchy
;;; 
;;;   GInterface
;;;    +----GtkTreeDragSource
;;; 
;;;   GInterface
;;;    +----GtkTreeDragDest
;;; 
;;; Known Implementations
;;; 
;;; GtkTreeDragSource is implemented by GtkListStore, GtkTreeModelFilter,
;;; GtkTreeModelSort and GtkTreeStore.
;;; 
;;; GtkTreeDragDest is implemented by GtkListStore and GtkTreeStore.
;;; Description
;;; 
;;; GTK+ supports Drag-and-Drop in tree views with a high-level and a low-level
;;; API.
;;; 
;;; The low-level API consists of the GTK+ DND API, augmented by some treeview
;;; utility functions: gtk_tree_view_set_drag_dest_row(),
;;; gtk_tree_view_get_drag_dest_row(), gtk_tree_view_get_dest_row_at_pos(),
;;; gtk_tree_view_create_row_drag_icon(), gtk_tree_set_row_drag_data() and
;;; gtk_tree_get_row_drag_data(). This API leaves a lot of flexibility, but
;;; nothing is done automatically, and implementing advanced features like
;;; hover-to-open-rows or autoscrolling on top of this API is a lot of work.
;;; 
;;; On the other hand, if you write to the high-level API, then all the
;;; bookkeeping of rows is done for you, as well as things like hover-to-open
;;; and auto-scroll, but your models have to implement the GtkTreeDragSource
;;; and GtkTreeDragDest interfaces.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeDragSource
;;; 
;;; typedef struct _GtkTreeDragSource GtkTreeDragSource;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkTreeDragSource" gtk-tree-drag-source
  (:export t
   :type-initializer "gtk_tree_drag_source_get_type"))

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeDragSourceIface
;;; 
;;; struct GtkTreeDragSourceIface {
;;;   GTypeInterface g_iface;
;;; 
;;;   /* VTable - not signals */
;;; 
;;;   gboolean (* row_draggable)    (GtkTreeDragSource *drag_source,
;;;                                  GtkTreePath       *path);
;;; 
;;;   gboolean (* drag_data_get)    (GtkTreeDragSource *drag_source,
;;;                                  GtkTreePath       *path,
;;;                                  GtkSelectionData  *selection_data);
;;; 
;;;   gboolean (* drag_data_delete) (GtkTreeDragSource *drag_source,
;;;                                  GtkTreePath       *path);
;;; };
;;; ----------------------------------------------------------------------------

(define-vtable ("GtkTreeDragSource" gtk-tree-drag-source)
  (:skip parent-instance g-type-interface)
  ;;methods
  (row-draggable (:boolean
		  (tree-drag-source g-object)
		  (path (g-boxed-foreign gtk-tree-path))))
  (drag-data-get (:boolean
		  (tree-drag-source g-object)
		  (path (g-boxed-foreign gtk-tree-path))
		  (selection-data (g-boxed-foreign gtk-selection-data))))
  (drag-data-delete (:boolean
		     (tree-drag-source g-object)
                      (path (g-boxed-foreign gtk-tree-path)))))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_drag_source_drag_data_delete ()
;;; 
;;; gboolean gtk_tree_drag_source_drag_data_delete
;;;                                             (GtkTreeDragSource *drag_source,
;;;                                              GtkTreePath *path);
;;; 
;;; Asks the GtkTreeDragSource to delete the row at path, because it was moved
;;; somewhere else via drag-and-drop. Returns FALSE if the deletion fails
;;; because path no longer exists, or for some model-specific reason. Should
;;; robustly handle a path no longer found in the model!
;;; 
;;; drag_source :
;;; 	a GtkTreeDragSource
;;; 
;;; path :
;;; 	row that was being dragged
;;; 
;;; Returns :
;;; 	TRUE if the row was successfully deleted
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_drag_source_drag_data_get ()
;;; 
;;; gboolean gtk_tree_drag_source_drag_data_get
;;;                                           (GtkTreeDragSource *drag_source,
;;;                                            GtkTreePath *path,
;;;                                            GtkSelectionData *selection_data)
;;; 
;;; Asks the GtkTreeDragSource to fill in selection_data with a representation
;;; of the row at path. selection_data->target gives the required type of the
;;; data. Should robustly handle a path no longer found in the model!
;;; 
;;; drag_source :
;;; 	a GtkTreeDragSource
;;; 
;;; path :
;;; 	row that was dragged
;;; 
;;; selection_data :
;;; 	a GtkSelectionData to fill with data from the dragged row. [out]
;;; 
;;; Returns :
;;; 	TRUE if data of the required type was provided
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_drag_source_row_draggable ()
;;; 
;;; gboolean gtk_tree_drag_source_row_draggable (GtkTreeDragSource *drag_source,
;;;                                              GtkTreePath *path);
;;; 
;;; Asks the GtkTreeDragSource whether a particular row can be used as the
;;; source of a DND operation. If the source doesn't implement this interface,
;;; the row is assumed draggable.
;;; 
;;; drag_source :
;;; 	a GtkTreeDragSource
;;; 
;;; path :
;;; 	row on which user is initiating a drag
;;; 
;;; Returns :
;;; 	TRUE if the row can be dragged
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkTreeDragDest
;;; 
;;; typedef struct _GtkTreeDragDest GtkTreeDragDest;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkTreeDragDest" gtk-tree-drag-dest
  (:export t
   :type-initializer "gtk_tree_drag_dest_get_type"))

;;; ----------------------------------------------------------------------------
;;; struct GtkTreeDragDestIface
;;; 
;;; struct GtkTreeDragDestIface {
;;;   GTypeInterface g_iface;
;;; 
;;;   /* VTable - not signals */
;;; 
;;;   gboolean (* drag_data_received) (GtkTreeDragDest   *drag_dest,
;;;                                    GtkTreePath       *dest,
;;;                                    GtkSelectionData  *selection_data);
;;; 
;;;   gboolean (* row_drop_possible)  (GtkTreeDragDest   *drag_dest,
;;;                                    GtkTreePath       *dest_path,
;;; 				       GtkSelectionData  *selection_data);
;;; };
;;; ----------------------------------------------------------------------------

(define-vtable ("GtkTreeDragDest" gtk-tree-drag-dest)
  (:skip parent-instance g-type-interface)
  ;;methods
  (drag-data-received (:boolean
		       (tree-drag-dest g-object)
		       (path (g-boxed-foreign gtk-tree-path))
		       (selection-data (g-boxed-foreign gtk-selection-data))))
  (row-drop-possible (:boolean
		      (tree-drag-dest g-object)
		      (path (g-boxed-foreign gtk-tree-path))
                       (selection-data (g-boxed-foreign gtk-selection-data)))))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_drag_dest_drag_data_received ()
;;; 
;;; gboolean gtk_tree_drag_dest_drag_data_received
;;;                                          (GtkTreeDragDest *drag_dest,
;;;                                           GtkTreePath *dest,
;;;                                           GtkSelectionData *selection_data);
;;; 
;;; Asks the GtkTreeDragDest to insert a row before the path dest, deriving the
;;; contents of the row from selection_data. If dest is outside the tree so that
;;; inserting before it is impossible, FALSE will be returned. Also, FALSE may
;;; be returned if the new row is not created for some model-specific reason.
;;; Should robustly handle a dest no longer found in the model!
;;; 
;;; drag_dest :
;;; 	a GtkTreeDragDest
;;; 
;;; dest :
;;; 	row to drop in front of
;;; 
;;; selection_data :
;;; 	data to drop
;;; 
;;; Returns :
;;; 	whether a new row was created before position dest
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_drag_dest_row_drop_possible ()
;;; 
;;; gboolean gtk_tree_drag_dest_row_drop_possible
;;;                                           (GtkTreeDragDest *drag_dest,
;;;                                            GtkTreePath *dest_path,
;;;                                            GtkSelectionData *selection_data)
;;; 
;;; Determines whether a drop is possible before the given dest_path, at the
;;; same depth as dest_path. i.e., can we drop the data in selection_data at
;;; that location. dest_path does not have to exist; the return value will
;;; almost certainly be FALSE if the parent of dest_path doesn't exist, though.
;;; 
;;; drag_dest :
;;; 	a GtkTreeDragDest
;;; 
;;; dest_path :
;;; 	destination row
;;; 
;;; selection_data :
;;; 	the data being dragged
;;; 
;;; Returns :
;;; 	TRUE if a drop is possible before dest_path
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_set_row_drag_data ()
;;; 
;;; gboolean gtk_tree_set_row_drag_data (GtkSelectionData *selection_data,
;;;                                      GtkTreeModel *tree_model,
;;;                                      GtkTreePath *path);
;;; 
;;; Sets selection data of target type GTK_TREE_MODEL_ROW. Normally used in a
;;; drag_data_get handler.
;;; 
;;; selection_data :
;;; 	some GtkSelectionData
;;; 
;;; tree_model :
;;; 	a GtkTreeModel
;;; 
;;; path :
;;; 	a row in tree_model
;;; 
;;; Returns :
;;; 	TRUE if the GtkSelectionData had the proper target type to allow us to
;;;     set a tree row
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tree_get_row_drag_data ()
;;; 
;;; gboolean gtk_tree_get_row_drag_data (GtkSelectionData *selection_data,
;;;                                      GtkTreeModel **tree_model,
;;;                                      GtkTreePath **path);
;;; 
;;; Obtains a tree_model and path from selection data of target type
;;; GTK_TREE_MODEL_ROW. Normally called from a drag_data_received handler. This
;;; function can only be used if selection_data originates from the same process
;;; that's calling this function, because a pointer to the tree model is being
;;; passed around. If you aren't in the same process, then you'll get memory
;;; corruption. In the GtkTreeDragDest drag_data_received handler, you can
;;; assume that selection data of type GTK_TREE_MODEL_ROW is in from the current
;;; process. The returned path must be freed with gtk_tree_path_free().
;;; 
;;; selection_data :
;;; 	a GtkSelectionData
;;; 
;;; tree_model :
;;; 	a GtkTreeModel
;;; 
;;; path :
;;; 	row in tree_model
;;; 
;;; Returns :
;;; 	TRUE if selection_data had target type GTK_TREE_MODEL_ROW and is
;;;     otherwise valid
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.tree-view-drag-and-drop.lisp ---------------------------
