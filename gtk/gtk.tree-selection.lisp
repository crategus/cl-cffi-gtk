;;; ----------------------------------------------------------------------------
;;; gtk.tree-selection.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkTreeSelection
;;;
;;; The selection object for GtkTreeView
;;;
;;; Synopsis
;;;
;;;     GtkTreeSelection
;;;
;;;     gtk_tree_selection_set_mode
;;;     gtk_tree_selection_get_mode
;;;     gtk_tree_selection_set_select_function
;;;     gtk_tree_selection_get_select_function
;;;     gtk_tree_selection_get_user_data
;;;     gtk_tree_selection_get_tree_view
;;;     gtk_tree_selection_get_selected
;;;     gtk_tree_selection_selected_foreach
;;;     gtk_tree_selection_get_selected_rows
;;;     gtk_tree_selection_count_selected_rows
;;;     gtk_tree_selection_select_path
;;;     gtk_tree_selection_unselect_path
;;;     gtk_tree_selection_path_is_selected
;;;     gtk_tree_selection_select_iter
;;;     gtk_tree_selection_unselect_iter
;;;     gtk_tree_selection_iter_is_selected
;;;     gtk_tree_selection_select_all
;;;     gtk_tree_selection_unselect_all
;;;     gtk_tree_selection_select_range
;;;     gtk_tree_selection_unselect_range
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeSelection
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTreeSelection" gtk-tree-selection
  (:superclass g-object
    :export t
    :interfaces nil
    :type-initializer "gtk_tree_selection_get_type")
  ((mode
    gtk-tree-selection-mode
    "mode" "GtkSelectionMode" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tree-selection 'type)
 "@version{2013-3-10}
  @begin{short}
    The GtkTreeSelection object is a helper object to manage the selection for a
    GtkTreeView widget. The GtkTreeSelection object is automatically created
    when a new GtkTreeView widget is created, and cannot exist independentally
    of this widget. The primary reason the GtkTreeSelection objects exists is
    for cleanliness of code and API. That is, there is no conceptual reason all
    these functions could not be methods on the GtkTreeView widget instead of a
    separate function.
  @end{short}

  The GtkTreeSelection object is gotten from a GtkTreeView by calling
  gtk_tree_view_get_selection(). It can be manipulated to check the selection
  status of the tree, as well as select and deselect individual rows. 
  Selection is done completely view side. As a result, multiple views of the
  same model can have completely different selections. Additionally, you
  cannot change the selection of a row on the model that is not currently
  displayed by the view without expanding its parents first.

  One of the important things to remember when monitoring the selection of a
  view is that the \"changed\" signal is mostly a hint. That is,it may only emit
  one signal when a range of rows is selected. Additionally, it may on
  occasion emit a \"changed\" signal when nothing has happened (mostly as a
  result of programmers calling select_row on an already selected row).
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      Emitted whenever the selection has (possibly) changed. Please note that
      this signal is mostly a hint. It may only be emitted once when a range of
      rows are selected, and it may occasionally be emitted when nothing has
      happened.
      @begin{pre}
 void user_function (GtkTreeSelection *treeselection,
                     gpointer          user_data)          : Run First
      @end{pre}
      @begin[code]{table}
        @entry[treeselection]{the object which received the signal.}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-tree-selection-mode}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mode" 'gtk-tree-selection) 't)
 "The @code{\"mode\"} property of type @symbol{gtk-selection-mode}
  (Read / Write)@br{}
  Selection mode. See gtk_tree_selection_set_mode() for more information on
  this property.@br{}
  Default value: GTK_SELECTION_SINGLE@br{}
  Since 3.2")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-selection-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-selection-mode 'function)
 "@version{2013-3-10}
  @begin{short}
    Accessor of the slot @code{\"mode\"} of the @class{gtk-tree-selection}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; GtkTreeSelectionFunc ()
;;;
;;; gboolean (*GtkTreeSelectionFunc) (GtkTreeSelection *selection,
;;;                                   GtkTreeModel *model,
;;;                                   GtkTreePath *path,
;;;                                   gboolean path_currently_selected,
;;;                                   gpointer data);
;;;
;;; A function used by gtk_tree_selection_set_select_function() to filter
;;; whether or not a row may be selected. It is called whenever a row's state
;;; might change. A return value of TRUE indicates to selection that it is okay
;;; to change the selection.
;;;
;;; selection :
;;;     A GtkTreeSelection
;;;
;;; model :
;;;     A GtkTreeModel being viewed
;;;
;;; path :
;;;     The GtkTreePath of the row in question
;;;
;;; path_currently_selected :
;;;     TRUE, if the path is currently selected
;;;
;;; data :
;;;     user data
;;;
;;; Returns :
;;;     TRUE, if the selection state of the row can be toggled
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-selection-select-function-cb :boolean
    ((selection g-object)
     (model g-object)
     (path (g-boxed-foreign gtk-tree-path))
     (path-currently-selected :boolean)
     (data :pointer))
  (let ((fn (glib::get-stable-pointer-value data)))
    (restart-case
        (funcall fn selection model path path-currently-selected)
      (return-true () t)
      (return-false () nil))))

;;; ----------------------------------------------------------------------------
;;; GtkTreeSelectionForeachFunc ()
;;;
;;; void (*GtkTreeSelectionForeachFunc) (GtkTreeModel *model,
;;;                                      GtkTreePath *path,
;;;                                      GtkTreeIter *iter,
;;;                                      gpointer data);
;;;
;;; A function used by gtk_tree_selection_selected_foreach() to map all selected
;;; rows. It will be called on every selected row in the view.
;;;
;;; model :
;;;     The GtkTreeModel being viewed
;;;
;;; path :
;;;     The GtkTreePath of a selected row
;;;
;;; iter :
;;;     A GtkTreeIter pointing to a selected row
;;;
;;; data :
;;;     user data
;;; ----------------------------------------------------------------------------

(defcallback gtk-tree-selection-foreach-cb :void
    ((model g-object)
     (path (g-boxed-foreign gtk-tree-path))
     (iter (g-boxed-foreign gtk-tree-iter))
     (data :pointer))
  (let ((fn (glib::get-stable-pointer-value data)))
    (funcall fn model path iter)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_set_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-selection-set-mode))

(defun gtk-tree-selection-set-mode (selection type)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[type]{The selection mode}
  @begin{short}
    Sets the selection mode of the selection. If the previous type was
    GTK_SELECTION_MULTIPLE, then the anchor is kept selected, if it was
    previously selected.
  @end{short}"
  (setf (gtk-tree-selection-mode selection) type))

(export 'gtk-tree-selection-set-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_mode ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tree-selection-get-mode))

(defun gtk-tree-selection-get-mode (selection type)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{a GtkTreeSelection}
  @return{the current selection mode}
  @short{Gets the selection mode for selection.}
  See gtk_tree_selection_set_mode()."
  (gtk-tree-selection-mode selection))

(export 'gtk-tree-selection-get-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_set_select_function ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_set_select_function"
          %gtk-tree-selection-set-select-function) :void
  (selection g-object)
  (select-function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-tree-selection-set-select-function (selection fn)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[func]{The selection function. May be NULL}
  @argument[data]{The selection function's data. May be NULL}
  @argument[destroy]{The destroy function for user data. May be NULL}
  @short{Sets the selection function.}

  If set, this function is called before any node is selected or unselected,
  giving some control over which nodes are selected. The select function
  should return TRUE if the state of the node may be toggled, and FALSE if the
  state of the node should be left unchanged."
  (%gtk-tree-selection-set-select-function
                                selection
                                (callback gtk-tree-selection-select-function-cb)
                                (glib::allocate-stable-pointer fn)
                                (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gtk-tree-selection-set-select-function)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_select_function ()
;;; ----------------------------------------------------------------------------

;; The Lisp select-function is stored in user-data

(defun gtk-tree-selection-get-select-function (selection)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @return{The function.}
  @short{Returns the current selection function.}

  Since 2.14"
  (let ((ptr (%gtk-tree-selection-get-user-data selection)))
    (unless (null-pointer-p ptr)
      (glib::get-stable-pointer-value ptr))))

(export 'gtk-tree-selection-get-select-function)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_user_data ()
;;;
;;; gpointer gtk_tree_selection_get_user_data (GtkTreeSelection *selection);
;;;
;;; Returns the user data for the selection function.
;;;
;;; selection :
;;;     A GtkTreeSelection.
;;;
;;; Returns :
;;;     The user data.
;;; ----------------------------------------------------------------------------

;; The function is not exported, because we do not store user-data, but a
;; pointer to the select function.

(defcfun ("gtk_tree_selection_get_user_data"
          %gtk-tree-selection-get-user-data) :pointer
  (selection (g-object gtk-tree-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_tree_view ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_get_tree_view" gtk-tree-selection-get-tree-view)
    (g-object gtk-tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection}
  @return{A GtkTreeView}
  @short{Returns the tree view associated with selection.}"
  (selection (g-object gtk-tree-selection)))

(export 'gtk-tree-selection-get-tree-view)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_get_selected"
          %gtk-tree-selection-get-selected) :boolean
  (selection g-object)
  (model :pointer)
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-selection-get-selected (selection)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[model]{A pointer to set to the GtkTreeModel, or NULL.}
  @argument[iter]{The GtkTreeIter, or NULL.}
  @return{TRUE, if there is a selected node.}
  @begin{short}
    Sets iter to the currently selected node if selection is set to
    GTK_SELECTION_SINGLE or GTK_SELECTION_BROWSE. iter may be NULL if you just
    want to test if selection has any selected nodes. model is filled with the
    current model as a convenience. This function will not work if you use
    selection is GTK_SELECTION_MULTIPLE.
  @end{short}"
  (let ((iter (make-instance 'gtk-tree-iter)))
    (when (%gtk-tree-selection-get-selected selection (null-pointer) iter)
      iter)))

(export 'gtk-tree-selection-get-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_selected_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_selected_foreach"
          %gtk-tree-selection-selected-foreach) :void
  (selection g-object)
  (func :pointer)
  (data :pointer))

(defun gtk-tree-selection-selected-foreach (selection fn)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[func]{The function to call for each selected node.}
  @argument[data]{user data to pass to the function.}
  @begin{short}
    Calls a function for each selected node. Note that you cannot modify the
    tree or selection from within this function. As a result,
    gtk_tree_selection_get_selected_rows() might be more useful.
  @end{short}"
  (with-stable-pointer (ptr fn)
    (%gtk-tree-selection-selected-foreach
                                        selection
                                        (callback gtk-tree-selection-foreach-cb)
                                        ptr)))

(export 'gtk-tree-selection-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_selected_rows ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_get_selected_rows"
          %gtk-tree-selection-get-selected-rows)
    (g-list (g-boxed-foreign gtk-tree-path) :free-from-foreign t)
  (selection g-object)
  (model :pointer))

(defun gtk-tree-selection-get-selected-rows (selection)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[model]{A pointer to set to the GtkTreeModel, or NULL.}
  @return{A GList containing a GtkTreePath for each selected row.}
  @begin{short}
    Creates a list of path of all selected rows. Additionally, if you are
    planning on modifying the model after calling this function, you may want to
    convert the returned list into a list of GtkTreeRowReferences. To do this,
    you can use gtk_tree_row_reference_new().
  @end{short}

  To free the return value, use:
  @code{g_list_free_full (list, (GDestroyNotify) gtk_tree_path_free);}

  Since 2.2"
  (%gtk-tree-selection-get-selected-rows selection (null-pointer)))

(export 'gtk-tree-selection-get-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_count_selected_rows ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_count_selected_rows"
           gtk-tree-selection-count-selected-rows) :int
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @return{The number of rows selected.}
  @short{Returns the number of rows that have been selected in tree.}

  Since 2.2"
  (selection g-object))

(export 'gtk-tree-selection-count-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_path" gtk-tree-selection-select-path)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[path]{The GtkTreePath to be selected.}
  @short{Select the row at path.}"
  (selection g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-selection-select-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_path" gtk-tree-selection-unselect-path)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[path]{The GtkTreePath to be unselected.}
  @short{Unselects the row at path.}"
  (selection g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-selection-unselect-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_path_is_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_path_is_selected"
           gtk-tree-selection-path-is-selected) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[path]{A GtkTreePath to check selection on.}
  @return{TRUE if path is selected.}
  @begin{short}
    Returns TRUE if the row pointed to by path is currently selected. If path
    does not point to a valid location, FALSE is returned
  @end{short}"
  (selection g-object)
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-selection-path-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_iter" gtk-tree-selection-select-iter) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[iter]{The GtkTreeIter to be selected.}
  @short{Selects the specified iterator.}"
  (selection g-object)
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-selection-select-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_iter" gtk-tree-selection-unselect-iter)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[iter]{The GtkTreeIter to be unselected.}
  @short{Unselects the specified iterator.}"
  (selection g-object)
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-selection-unselect-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_iter_is_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_iter_is_selected"
           gtk-tree-selection-iter-is-selected) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection}
  @argument[iter]{A valid GtkTreeIter}
  @return{TRUE, if iter is selected}
  @short{Returns TRUE if the row at iter is currently selected.}"
  (selection g-object)
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-selection-iter-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_all" gtk-tree-selection-select-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @begin{short}
    Selects all the nodes. selection must be set to GTK_SELECTION_MULTIPLE mode.
  @end{short}"
  (selection g-object))

(export 'gtk-tree-selection-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_all" gtk-tree-selection-unselect-all)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @short{Unselects all the nodes.}"
  (selection g-object))

(export 'gtk-tree-selection-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_range" gtk-tree-selection-select-range)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[start_path]{The initial node of the range.}
  @argument[end_path]{The final node of the range.}
  @begin{short}
    Selects a range of nodes, determined by start_path and end_path inclusive.
    selection must be set to GTK_SELECTION_MULTIPLE mode.
  @end{short}"
  (selection g-object)
  (start-path (g-boxed-foreign gtk-tree-path))
  (end-path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-selection-select-range)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_range" gtk-tree-selection-unselect-range)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-3-10}
  @argument[selection]{A GtkTreeSelection.}
  @argument[start_path]{The initial node of the range.}
  @argument[end_path]{The initial node of the range.}
  @begin{short}
    Unselects a range of nodes, determined by start_path and end_path inclusive.
  @end{short}

  Since 2.2"
  (selection g-object)
  (start-path (g-boxed-foreign gtk-tree-path))
  (end-path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-selection-unselect-range)

;;; --- End of file gtk.tree-selection.lisp ------------------------------------
