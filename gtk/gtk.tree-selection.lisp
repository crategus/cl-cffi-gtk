;;; ----------------------------------------------------------------------------
;;; gtk.tree-selection.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     The selection object for GtkTreeView
;;;
;;; Types and Values
;;;
;;;     GtkTreeSelection
;;;
;;; Functions
;;;
;;;     GtkTreeSelectionFunc
;;;     GtkTreeSelectionForeachFunc
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
;;;
;;; Properties
;;;
;;;     GtkSelectionMode    mode       Read / Write
;;;
;;; Signals
;;;
;;;                 void    changed    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTreeSelection
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tree-selection 'type)
 "@version{2020-6-28}
  @begin{short}
    The @sym{gtk-tree-selection} object is a helper object to manage the
    selection for a @class{gtk-tree-view} widget.
  @end{short}
  The @sym{gtk-tree-selection} object is automatically created when a new
  @class{gtk-tree-view} widget is created, and cannot exist independentally of
  this widget. The primary reason the @sym{gtk-tree-selection} objects exists
  is for cleanliness of code and API. That is, there is no conceptual reason
  all these functions could not be methods on the @class{gtk-tree-view} widget
  instead of a separate function.

  The @sym{gtk-tree-selection} object is gotten from a @class{gtk-tree-view} by
  calling the function @fun{gtk-tree-view-selection}. It can be manipulated
  to check the selection status of the tree, as well as select and deselect
  individual rows. Selection is done completely view side. As a result, multiple
  views of the same model can have completely different selections.
  Additionally, you cannot change the selection of a row on the model that is
  not currently displayed by the view without expanding its parents first.

  One of the important things to remember when monitoring the selection of a
  view is that the \"changed\" signal is mostly a hint. That is, it may only
  emit one signal when a range of rows is selected. Additionally, it may on
  occasion emit a \"changed\" signal when nothing has happened (mostly as a
  result of programmers calling @code{select_row} on an already selected row).
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (selection)    : Run First
      @end{pre}
      Emitted whenever the selection has (possibly) changed. Please note that
      this signal is mostly a hint. It may only be emitted once when a range of
      rows are selected, and it may occasionally be emitted when nothing has
      happened.
      @begin[code]{table}
        @entry[selection]{The @sym{gtk-tree-selection} object which received
        the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-tree-selection-mode}
  @see-class{gtk-tree-view}
  @see-function{gtk-tree-view-selection}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "mode" 'gtk-tree-selection) 't)
 "The @code{mode} property of type @symbol{gtk-selection-mode} (Read / Write)
  @br{}
  Selection mode. See the function @fun{gtk-tree-selection-mode} for more
  information on this property. @br{}
  Default value: @code{:single}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tree-selection-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tree-selection-mode 'function)
 "@version{2020-6-28}
  @syntax[]{(gtk-tree-selection-mode object) => mode}
  @syntax[]{(setf (gtk-tree-selection-mode object) mode)}
  @argument[object]{a @class{gtk-tree-selection} object}
  @argument[mode]{a @symbol{gtk-selection-mode} value}
  @begin{short}
    Accessor of the @slot[gtk-tree-selection]{mode} slot of the
    @class{gtk-tree-selection} class.
  @end{short}

  The slot access function @sym{gtk-tree-selection-mode} gets the current
  selection mode. The slot access function @sym{(setf gtk-tree-selection-mode)}
  sets the selection mode of the selection. If the previous type was
  @code{:multiple}, then the anchor is kept selected, if it was previously
  selected.
  @see-class{gtk-tree-selection}
  @see-symbol{gtk-selection-mode}")

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
  (let ((fn (get-stable-pointer-value data)))
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
  (let ((fn (get-stable-pointer-value data)))
    (funcall fn model path iter)))

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
 "@version{2020-6-28}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[func]{the selection function, may be @code{nil}}
  @short{Sets the selection function.}

  If set, this function is called before any node is selected or unselected,
  giving some control over which nodes are selected. The select function
  should return @em{true} if the state of the node may be toggled, and
  @code{nil} if the state of the node should be left unchanged.
  @see-class{gtk-tree-selection}"
  (%gtk-tree-selection-set-select-function
                               selection
                               (callback gtk-tree-selection-select-function-cb)
                               (allocate-stable-pointer fn)
                               (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-tree-selection-set-select-function)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_select_function ()
;;; ----------------------------------------------------------------------------

;; The Lisp select-function is stored in user-data

(defun gtk-tree-selection-get-select-function (selection)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-28}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @return{The function.}
  @short{Returns the current selection function.}
  @see-class{gtk-tree-selection}"
  (let ((ptr (%gtk-tree-selection-get-user-data selection)))
    (unless (null-pointer-p ptr)
      (get-stable-pointer-value ptr))))

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
;;; gtk_tree_selection_get_tree_view () -> gtk-tree-selection-tree-view
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_get_tree_view" gtk-tree-selection-tree-view)
    (g-object gtk-tree-view)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-28}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @return{A @class{gtk-tree-view} object.}
  @begin{short}
    Returns the tree view associated with @arg{selection}.
  @end{short}
  @see-class{gtk-tree-selection}"
  (selection (g-object gtk-tree-selection)))

(export 'gtk-tree-selection-tree-view)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_get_selected" %gtk-tree-selection-selected)
    :boolean
  (selection (g-object gtk-tree-selection))
  (model :pointer)
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-tree-selection-selected (selection)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-28}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @begin{return}
    The @class{gtk-tree-iter} object of the selected node, or @code{nil}
    if there is no selected node.
  @end{return}
  @begin{short}
    Sets iter to the currently selected node if @arg{selection} is set to
    @code{:single} or @code{:browse}.
  @end{short}
  This function will not work if you use selection is @code{:multiple}.
  @see-class{gtk-tree-selection}
  @see-class{gtk-tree-iter}"
  (let ((iter (make-instance 'gtk-tree-iter)))
    (when (%gtk-tree-selection-selected selection (null-pointer) iter)
      iter)))

(export 'gtk-tree-selection-selected)

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
 "@version{2020-6-28}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[func]{the function to call for each selected node}
  @begin{short}
    Calls a function for each selected node.
  @end{short}
  Note that you cannot modify the tree or selection from within this function.
  As a result, the functin @fun{gtk-tree-selection-selected-rows} might be more
  useful.
  @see-class{gtk-tree-selection}
  @see-function{gtk-tree-selection-selected-rows}"
  (with-stable-pointer (ptr fn)
    (%gtk-tree-selection-selected-foreach
                                        selection
                                        (callback gtk-tree-selection-foreach-cb)
                                        ptr)))

(export 'gtk-tree-selection-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_get_selected_rows () -> gtk-tree-selection-selected-rows
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_get_selected_rows"
          %gtk-tree-selection-selected-rows)
    (g-list (g-boxed-foreign gtk-tree-path) :free-from-foreign t)
  (selection g-object)
  (model :pointer))

(defun gtk-tree-selection-selected-rows (selection)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-20}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[model]{a pointer to set to the @class{gtk-tree-model} object,
    or @code{nil}}
  @return{A list containing a @class{gtk-tree-path} for each selected row.}
  @begin{short}
    Creates a list of path of all selected rows.
  @end{short}
  Additionally, if you are planning on modifying the model after calling this
  function, you may want to convert the returned list into a list of
  @class{gtk-tree-row-reference}'s. To do this, you can use the function
  @fun{gtk-tree-row-reference-new}.

  To free the return value, use:
  @code{g_list_free_full (list, (GDestroyNotify) gtk_tree_path_free);}
  @see-class{gtk-tree-selection}
  @see-function{gtk-tree-row-reference-new}"
  (%gtk-tree-selection-selected-rows selection (null-pointer)))

(export 'gtk-tree-selection-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_count_selected_rows ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_count_selected_rows"
           gtk-tree-selection-count-selected-rows) :int
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @return{A @code{:int} with the number of rows selected.}
  @short{Returns the number of rows that have been selected in the tree.}
  @see-class{gtk-tree-selection}"
  (selection (g-object gtk-tree-selection)))

(export 'gtk-tree-selection-count-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_path" gtk-tree-selection-select-path) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[path]{the @class{gtk-tree-path} to be selected}
  @begin{short}
    Select the row at @arg{path}.
  @end{short}
  @see-class{gtk-tree-selection}
  @see-class{gtk-tree-path}"
  (selection (g-object gtk-tree-selection))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-selection-select-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_path ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_path" gtk-tree-selection-unselect-path)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[path]{the @class{gtk-tree-path} to be unselected}
  @begin{short}
    Unselects the row at @arg{path}.
  @end{short}
  @see-class{gtk-tree-selection}
  @see-class{gtk-tree-path}"
  (selection (g-object gtk-tree-selection))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-selection-unselect-path)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_path_is_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_path_is_selected"
           gtk-tree-selection-path-is-selected) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[path]{a @class{gtk-tree-path} object to check selection on}
  @return{@em{True} if @arg{path} is selected.}
  @begin{short}
    Returns @em{true} if the row pointed to by @arg{path} is currently selected.
    If @arg{path} does not point to a valid location, @code{nil} is returned.
  @end{short}
  @see-class{gtk-tree-selection}
  @see-class{gtk-tree-path}"
  (selection (g-object gtk-tree-selection))
  (path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-selection-path-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_iter" gtk-tree-selection-select-iter) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[iter]{the @class{gtk-tree-iter} object to be selected}
  @begin{short}
    Selects the specified iterator.
  @end{short}
  @see-class{gtk-tree-selection}
  @see-class{gtk-tree-iter}"
  (selection (g-object gtk-tree-selection))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-selection-select-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_iter" gtk-tree-selection-unselect-iter)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[iter]{the @class{gtk-tree-iter} object to be unselected}
  @begin{short}
    Unselects the specified iterator.
  @end{short}
  @see-class{gtk-tree-selection}
  @see-class{gtk-tree-iter}"
  (selection (g-object gtk-tree-selection))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-selection-unselect-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_iter_is_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_iter_is_selected"
           gtk-tree-selection-iter-is-selected) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[iter]{a valid @class{gtk-tree-iter} object}
  @return{@em{True}, if @arg{iter} is selected.}
  @begin{short}
    Returns @em{true} if the row at @arg{iter} is currently selected.
  @end{short}
  @see-class{gtk-tree-selection}
  @see-class{gtk-tree-iter}"
  (selection (g-object gtk-tree-selection))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-tree-selection-iter-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_all" gtk-tree-selection-select-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @begin{short}
    Selects all the nodes.
  @end{short}
  @arg{selection} must be set to @code{:multiple} mode.
  @see-class{gtk-tree-selection}"
  (selection (g-object gtk-tree-selection)))

(export 'gtk-tree-selection-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_all" gtk-tree-selection-unselect-all)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @short{Unselects all the nodes.}
  @see-class{gtk-tree-selection}"
  (selection (g-object gtk-tree-selection)))

(export 'gtk-tree-selection-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_select_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_select_range" gtk-tree-selection-select-range)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[start-path]{the initial @class{gtk-tree-path} node of the range}
  @argument[end-path]{the final @class{gtk-tree-path} node of the range}
  @begin{short}
    Selects a range of nodes, determined by @arg{start-path} and @arg{end-path}
    inclusive.
  @end{short}
  @arg{selection} must be set to @code{:multiple} mode.
  @see-class{gtk-tree-selection}
  @see-class{gtk-tree-path}"
  (selection (g-object gtk-tree-selection))
  (start-path (g-boxed-foreign gtk-tree-path))
  (end-path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-selection-select-range)

;;; ----------------------------------------------------------------------------
;;; gtk_tree_selection_unselect_range ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tree_selection_unselect_range" gtk-tree-selection-unselect-range)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-9-11}
  @argument[selection]{a @class{gtk-tree-selection} object}
  @argument[start-path]{the initial @class{gtk-tree-path} node of the range}
  @argument[end-path]{the initial @class{gtk-tree-path} node of the range}
  @begin{short}
    Unselects a range of nodes, determined by @arg{start-path} and
    @arg{end-path} inclusive.
  @end{short}
  @see-class{gtk-tree-selection}
  @see-class{gtk-tree-path}"
  (selection (g-object gtk-tree-selection))
  (start-path (g-boxed-foreign gtk-tree-path))
  (end-path (g-boxed-foreign gtk-tree-path)))

(export 'gtk-tree-selection-unselect-range)

;;; --- End of file gtk.tree-selection.lisp ------------------------------------
