;;; ----------------------------------------------------------------------------
;;; gtk.list-box.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2021 Dieter Kaiser
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
;;; GtkListBox
;;;
;;;     A list container
;;;
;;; Types and Values
;;;
;;;     GtkListBox
;;;     GtkListBoxRow
;;;
;;; Functions
;;;
;;;     GtkListBoxFilterFunc
;;;     GtkListBoxSortFunc
;;;     GtkListBoxUpdateHeaderFunc
;;;
;;;     gtk_list_box_new
;;;     gtk_list_box_prepend
;;;     gtk_list_box_insert
;;;     gtk_list_box_select_row
;;;     gtk_list_box_unselect_row
;;;     gtk_list_box_select_all
;;;     gtk_list_box_unselect_all
;;;     gtk_list_box_get_selected_row
;;;
;;;     GtkListBoxForeachFunc
;;;     gtk_list_box_selected_foreach
;;;
;;;     gtk_list_box_get_selected_rows
;;;     gtk_list_box_set_selection_mode                    Accessor
;;;     gtk_list_box_get_selection_mode                    Accessor
;;;     gtk_list_box_set_activate_on_single_click          Accessor
;;;     gtk_list_box_get_activate_on_single_click          Accessor
;;;     gtk_list_box_get_adjustment
;;;     gtk_list_box_set_adjustment
;;;     gtk_list_box_set_placeholder
;;;     gtk_list_box_get_row_at_index
;;;     gtk_list_box_get_row_at_y
;;;     gtk_list_box_invalidate_filter
;;;     gtk_list_box_invalidate_headers
;;;     gtk_list_box_invalidate_sort
;;;
;;;     gtk_list_box_set_filter_func
;;;
;;;     gtk_list_box_set_header_func
;;;     gtk_list_box_set_sort_func
;;;     gtk_list_box_drag_highlight_row
;;;     gtk_list_box_drag_unhighlight_row
;;;
;;;     GtkListBoxCreateWidgetFunc
;;;
;;;     gtk_list_box_bind_model
;;;     gtk_list_box_row_new
;;;     gtk_list_box_row_changed
;;;     gtk_list_box_row_is_selected
;;;     gtk_list_box_row_get_header
;;;     gtk_list_box_row_set_header
;;;     gtk_list_box_row_get_index
;;;     gtk_list_box_row_set_activatable                   Accessor
;;;     gtk_list_box_row_get_activatable                   Accessor
;;;     gtk_list_box_row_set_selectable                    Accessor
;;;     gtk_list_box_row_get_selectable                    Accessor
;;;
;;; Properties
;;;
;;;             gboolean    activate-on-single-click    Read/Write
;;;     GtkSelectionMode    selection-mode              Read/Write
;;;             gboolean    activatable                 Read/Write
;;;             gboolean    selectable                  Read/Write
;;;
;;; Signals
;;;
;;;                 void    activate-cursor-row      Action
;;;                 void    move-cursor              Action
;;;                 void    row-activated            Run Last
;;;                 void    row-selected             Run Last
;;;                 void    select-all               Action
;;;                 void    selected-rows-changed    Run First
;;;                 void    toggle-cursor-row        Action
;;;                 void    unselect-all             Action
;;;                 void    activate                 Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ├── GtkBin
;;;                 │   ╰── GtkListBoxRow
;;;                 ╰── GtkListBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkListBox implements AtkImplementorIface and GtkBuildable.
;;;
;;;     GtkListBoxRow implements AtkImplementorIface, GtkBuildable and
;;;     GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxRow
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkListBoxRow" gtk-list-box-row
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable")
   :type-initializer "gtk_list_box_row_get_type")
  ((activatable
    gtk-list-box-row-activatable
    "activatable" "gboolean" t t)
   (selectable
    gtk-list-box-row-selectable
    "selectable" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-list-box-row 'type)
 "@version{2021-4-30}
  @begin{short}
    The @sym{gkt-list-box-row} widget is a child widget for the
    @class{gtk-list-box} widget.
  @end{short}

  The @sym{gtk-list-box-row} widget can be marked as activatable or selectable.
  If a row is activatable, the \"row-activated\" signal will be emitted for it
  when the user tries to activate it. If it is selectable, the row will be
  marked as selected when the user tries to select it.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (listboxrow)    :action
      @end{pre}
      This is a keybinding signal, which will cause this row to be activated.
      If you want to be notified when the user activates a row (by key or not),
      use the \"row-activated\" signal on the row’s parent @class{gtk-list-box}
      widget.
      @begin[code]{table}
        @entry[listboxrow]{The @sym{gtk-list-box-row} widget on which the
        signal is emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-list-box-row-activatable}
  @see-slot{gtk-list-box-row-selectable}
  @see-class{gtk-list-box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-list-box-row-activatable -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "activatable"
                                               'gtk-list-box-row) 't)
 "The @code{activatable} property of type @code{:boolean} (Read / Write) @br{}
  The property determines whether the \"row-activated\" signal will be emitted
  for this row. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-row-activatable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-list-box-row-activatable 'function)
 "@version{2021-4-30}
  @syntax[]{(gtk-list-box-row-activatable object) => activatable}
  @syntax[]{(setf (gtk-list-box-row-activatable object) activatable)}
  @argument[object]{a @class{gtk-list-box-row} widget}
  @argument[activatable]{@em{true} to mark the row as activatable}
  @begin{short}
    Accessor of the @slot[gtk-list-box-row]{activatable} slot of the
    @class{gtk-list-box-row} class.
  @end{short}

  The slot access function @sym{gtk-list-box-row-activatable} gets the value of
  the @slot[gtk-list-box-row]{activatable} property for this row. The slot
  access function @sym{(setf gtk-list-box-row-activatable)} sets the property.
  @see-class{gtk-list-box-row}")

;;; --- gtk-list-box-row-selectable --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selectable"
                                               'gtk-list-box-row) 't)
 "The @code{selectable} property of type @code{:boolean} (Read / Write) @br{}
  The property determines whether this row can be selected. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-row-selectable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-list-box-row-selectable 'function)
 "@version{2021-4-30}
  @syntax[]{(gtk-list-box-row-selectable object) => selectable}
  @syntax[]{(setf (gtk-list-box-row-selectable object) selectable)}
  @argument[object]{a @class{gtk-list-box-row} widget}
  @argument[selectable]{@em{true} to mark the row as selectable}
  @begin{short}
    Accessor of the @slot[gtk-list-box-row]{selectable} slot of the
    @class{gtk-list-box} class.
  @end{short}

  The slot access function @sym{gtk-list-box-row-selectable} gets the value of
  the @slot[gtk-list-box-row]{selectable} property for this row. The slot access
  function @sym{(setf gtk-list-box-row-selectable)} sets the property.
  @see-class{gtk-list-box-row}")

;;; ----------------------------------------------------------------------------
;;; GtkListBox
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkListBox" gtk-list-box
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_list_box_get_type")
  ((activate-on-single-click
    gtk-list-box-activate-on-single-click
    "activate-on-single-click" "gboolean" t t)
   (selection-mode
    gtk-list-box-selection-mode
    "selection-mode" "GtkSelectionMode" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-list-box 'type)
 "@version{2021-4-30}
  @begin{short}
    The @sym{gtk-list-box} widget is a vertical container that contains
    @class{gtk-list-box-row} children.
  @end{short}
  These rows can by dynamically sorted and filtered, and headers can be added
  dynamically depending on the row content. It also allows keyboard and mouse
  navigation and selection like a typical list.

  @image[list-box]{}

  Using the @sym{gtk-list-box} widget is often an alternative to the
  @class{gtk-tree-view} widget, especially when the list contents has a more
  complicated layout than what is allowed by a @class{gtk-cell-renderer}
  object, or when the contents is interactive, i.e. has a button in it.

  Although a @sym{gtk-list-box} widget must have only @classl{gtk-list-box-row}
  children you can add any kind of widget to it via the function
  @fun{gtk-container-add}, and a @class{gtk-list-box-row} widget will
  automatically be inserted between the list and the widget.

  The @class{gtk-list-box-row} widget can be marked as activatable or
  selectable. If a row is activatable, the \"row-activated\" signal will be
  emitted for it when the user tries to activate it. If it is selectable, the
  row will be marked as selected when the user tries to select it.
  @begin[CSS Nodes]{dictionary}
    @begin{pre}
 list
  ╰── row[.activatable]
    @end{pre}
    The @sym{gtk-list-box} widget uses a single CSS node named @code{list}.
    Each @class{gtk-list-box-row} widget uses a single CSS node named
    @code{row}. The row nodes get the @code{.activatable} style class added
    when appropriate.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-cursor-row\" signal}
      @begin{pre}
 lambda (listbox)    :action
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
 lambda (listbox step count)    :action
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} widget on which the signal is
          emitted.}
        @entry[step]{A @symbol{gtk-movement-step} value.}
        @entry[count]{An integer.}
      @end{table}
    @subheading{The \"row-activated\" signal}
      @begin{pre}
 lambda (listbox row)    :run-last
      @end{pre}
      The signal is emitted when a row has been activated by the user.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} widget on which the signal is
          emitted.}
        @entry[row]{The activated @class{gtk-list-box-row} widget.}
      @end{table}
    @subheading{The \"row-selected\" signal}
      @begin{pre}
 lambda (listbox row)    :run-last
      @end{pre}
      The signal is emitted when a new row is selected, or when the selection
      is cleared. When the box is using the selection mode @code{:multiple},
      this signal will not give you the full picture of selection changes, and
      you should use the \"selected-rows-changed\" signal instead.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} widget on which the signal is
          emitted.}
        @entry[row]{The selected @class{gtk-list-box-row} widget.}
      @end{table}
    @subheading{The \"select-all\" signal}
      @begin{pre}
 lambda (listbox)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to select all
      children of the box, if the selection mode permits it. The default
      bindings for this signal is the @kbd{Ctrl-a} key.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"selected-rows-changed\" signal}
      @begin{pre}
 lambda (listbox)    :run-first
      @end{pre}
      The signal is emitted when the set of selected rows changes.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"toggle-cursor-row\" signal}
      @begin{pre}
 lambda (listbox)    :action
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"unselect-all\" signal}
      @begin{pre}
 lambda (listbox)    :action
      @end{pre}
        The signal is a keybinding signal which gets emitted to unselect all
        children of the box, if the selection mode permits it. The default
        bindings for this signal is the @kbd{Ctrl-Shift-a} key.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} widget on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-list-box-activate-on-single-click}
  @see-slot{gtk-list-box-selection-mode}
  @see-class{gtk-list-box-row}
  @see-class{gtk-tree-view}
  @see-class{gtk-cell-renderer}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-list-box-activate-on-single-click ----------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "activate-on-single-click"
                                               'gtk-list-box) 't)
 "The @code{activate-on-single-click} property of type @code{:boolean}
  (Read / Write) @br{}
  Activate row on a single click. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-activate-on-single-click
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-list-box-activate-on-single-click 'function)
 "@version{2021-4-30}
  @syntax[]{(gtk-list-box-acivate-on-click object) => single}
  @syntax[]{(setf (gtk-list-box-activate-on-click object) single)}
  @argument[object]{a @class{gtk-list-box} widget}
  @argument[single]{a boolean whether to activate the row on a single click}
  @begin{short}
    Accessor of the @slot[gtk-list-box]{activate-on-single-click} slot of the
    @class{gtk-list-box} class.
  @end{short}

  The slot access function @sym{gtk-list-box-activate-on-single-click} returns
  whether rows activate on single clicks. The slot access function
  @sym{(setf gtk-list-box-activate-on-single-click)} sets whether rows activate
  on single clicks.

  If @arg{single} is @em{true}, rows will be activated when you click on them,
  otherwise you need to double-click.
  @see-class{gtk-list-box}")

;;; --- gtk-list-box-selection-mode --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selection-mode"
                                               'gtk-list-box) 't)
 "The @code{selection-mode} property of type @symbol{gtk-selection-mode}
  (Read / Write) @br{}
  The selection mode. @br{}
  Default value: @code{:single}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-selection-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-list-box-selection-mode 'function)
 "@version{2021-4-30}
  @syntax[]{(gtk-list-box-selection-mode object) => mode}
  @syntax[]{(setf (gtk-list-box-selection-mode object) mode)}
  @argument[object]{a @class{gtk-list-box} widget}
  @argument[mode]{a value of the @symbol{gtk-selection-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-list-box]{selection-mode} slot of the
    @class{gtk-list-box} class.
  @end{short}

  The slot access function @sym{gtk-list-box-selection-mode} gets the selection
  mode of the listbox. The slot access function
  @sym{(setf gtk-list-box-selection-mode)} sets the selection mode. See the
  @symbol{gtk-selection-mode} enumeration for details.
  @see-class{gtk-list-box}
  @see-symbol{gtk-selection-mode}")

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-list-box-new))

(defun gtk-list-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @return{A new @class{gtk-list-box} widget.}
  @begin{short}
    Creates a new list box container.
  @end{short}
  @see-class{gtk-list-box}"
  (make-instance 'gtk-list-box))

(export 'gtk-list-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_prepend" gtk-list-box-prepend) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[child]{a @class{gtk-widget} child widet to add}
  @begin{short}
    Prepend a child widget to the list.
  @end{short}
  If a sort function is set, the child widget will actually be inserted at the
  calculated position and this function has the same effect as the function
  @fun{gtk-container-add}.
  @see-class{gtk-list-box}
  @see-class{gtk-widget}
  @see-function{gtk-list-box-insert}
  @see-function{gtk-container-add}"
  (listbox (g-object gtk-list-box))
  (child (g-object gtk-widget)))

(export 'gtk-list-box-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_insert" gtk-list-box-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[child]{a @class{gtk-widget} child widget to add}
  @argument[position]{an integer with the position to insert the child widget
    in}
  @begin{short}
    Insert the child widget into the list box at the given position.
  @end{short}
  If a sort function is set, the child widget will actually be inserted at the
  calculated position and this function has the same effect as the function
  @fun{gtk-container-add}.

  If the position is -1, or larger than the total number of items in the list
  box, then the child widget will be appended to the end.
  @see-class{gtk-list-box}
  @see-class{gtk-widget}
  @see-function{gtk-list-box-prepend}
  @see-function{gtk-container-add}"
  (listbox (g-object gtk-list-box))
  (child (g-object gtk-widget))
  (position :int))

(export 'gtk-list-box-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_select_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_select_row" gtk-list-box-select-row) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[row]{a @class{gtk-list-box-row} widget}
  @begin{short}
    Make @arg{row} the currently selected row.
  @end{short}
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}"
  (listbox (g-object gtk-list-box))
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-select-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_unselect_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_unselect_row" gtk-list-box-unselect-row) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[row]{a @class{gtk-list-box-row} widget}
  @begin{short}
    Unselects a single row of the list box, if the selection mode allows it.
  @end{short}
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}"
  (listbox (g-object gtk-list-box))
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-unselect-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_select_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_select_all" gtk-list-box-select-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @begin{short}
    Select all children of the list box, if the selection mode allows it.
  @end{short}
  @see-class{gtk-list-box}"
  (listbox (g-object gtk-list-box)))

(export 'gtk-list-box-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_unselect_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_unselect_all" gtk-list-box-unselect-all) :void
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @begin{short}
    Unselect all children of the list box, if the selection mode allows it.
  @end{short}
  @see-class{gtk-list-box}"
  (listbox (g-object gtk-list-box)))

(export 'gtk-list-box-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_selected_row () -> gtk-list-box-selected-row
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_get_selected_row" gtk-list-box-selected-row)
    (g-object gtk-list-box-row)
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @return{The selected @class{gtk-list-box-row} widget.}
  @begin{short}
    Gets the selected row.
  @end{short}

  Note that the list box may allow multiple selection, in which case you should
  use the function @fun{gtk-list-box-selected-foreach} to find all selected
  rows.
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}
  @see-function{gtk-list-box-selected-foreach}"
  (listbox (g-object gtk-list-box)))

(export 'gtk-list-box-selected-row)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxForeachFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-list-box-foreach-func :void
    ((listbox (g-object gtk-list-box))
     (row (g-object gtk-list-box-row))
     (data :pointer))
  (restart-case
    (let ((ptr (get-stable-pointer-value data)))
      (funcall ptr listbox row))
    (return () :report "Error in GtkListBoxForeachFunc callback." nil)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-foreach-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-list-box-foreach-func atdoc:*external-symbols*)
 "@version{2021-4-30}
  @begin{short}
    A callback function used by the function
    @fun{gtk-list-box-selected-foreach}.
  @end{short}
  It will be called on every selected child of the list box.
  @begin{pre}
 lambda (listbox row)
  @end{pre}
  @begin[code]{table}
    @entry[listbox]{A @class{gtk-list-box} widget.}
    @entry[row]{A @class{gtk-list-box-row} widget.}
  @end{table}
  @see-class{gtk-list-box}
  @see-function{gtk-list-box-selected-foreach}")

(export 'gtk-list-box-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_selected_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_selected_foreach" %gtk-list-box-selected-foreach) :void
  (listbox (g-object gtk-list-box))
  (func :pointer)
  (data :pointer))

(defun gtk-list-box-selected-foreach (listbox func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[func]{a @symbol{gtk-list-box-foreach-func} function which is passed
    as a callback function}
  @begin{short}
    Calls a function for each selected child.
  @end{short}

  Note that the selection cannot be modified from within this function.
  @see-class{gtk-list-box}
  @see-symbol{gtk-list-box-foreach-func}"
  (with-stable-pointer (ptr func)
    (%gtk-list-box-selected-foreach listbox
                                    (callback gtk-list-box-foreach-func)
                                    ptr)))

(export 'gtk-list-box-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_selected_rows () -> gtk-list-box-selected-rows
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_get_selected_rows" gtk-list-box-selected-rows)
    (g-list (g-object gtk-widget))
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @return{A list containing the @class{gtk-widget} object for each selected
    row.}
  @begin{short}
    Creates a list of all selected rows in the list box.
  @end{short}
  @see-class{gtk-list-box}
  @see-class{gtk-widget}"
  (listbox (g-object gtk-list-box)))

(export 'gtk-list-box-selected-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_adjustment ()
;;; gtk_list_box_set_adjustment () -> gtk-list-box-adjustment
;;; ----------------------------------------------------------------------------

(defun (setf gtk-list-box-adjustment) (adjustment listbox)
  (foreign-funcall "gtk_list_box_set_adjustment"
                   (g-object gtk-list-box) listbox
                   (g-object gtk-adjustment) adjustment
                   :void)
  adjustment)

(defcfun ("gtk_list_box_get_adjustment" gtk-list-box-adjustment)
    (g-object gtk-adjustment)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @syntax[]{(gtk-list-box-adjustment listbox) => adjustment}
  @syntax[]{(setf (gtk-list-box-adjustment listbox) adjustment)}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[adjustment]{a @class{gtk-adjustment} object}
  @begin{short}
    Accessor of the adjustment the list box uses for vertical scrolling.
  @end{short}

  The function @sym{gtk-list-box-adjustment} gets the adjustment (if any) that
  the list box uses to for vertical scrolling. The function
  @sym{(setf gtk-list-box-adjustment)} sets the adjustment.

  For instance, this is used to get the page size for PageUp/Down key handling.
  In the normal case when the list box is packed inside a
  @class{gtk-scrolled-window} widget the adjustment from that will be picked up
  automatically, so there is no need to manually do that.
  @see-class{gtk-list-box}
  @see-class{gtk-adjustment}
  @see-class{gtk-scrolled-window}"
  (listbox (g-object gtk-list-box)))

(export 'gtk-list-box-adjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_placeholder ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_set_placeholder" gtk-list-box-set-placeholder) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[placeholder]{a @class{gtk-widget} object}
  @begin{short}
    Sets the placeholder widget that is shown in the list when it does not
    display any visible children.
  @end{short}
  @see-class{gtk-list-box}
  @see-class{gtk-widget}"
  (listbox (g-object gtk-list-box))
  (placeholder (g-object gtk-widget)))

(export 'gtk-list-box-set-placeholder)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_row_at_index () -> gtk-list-box-row-at-index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_get_row_at_index" gtk-list-box-row-at-index)
    (g-object gtk-list-box-row)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[index]{an integer with the index of the row}
  @return{The @class{gtk-list-box-row} widget at @arg{index}.}
  @begin{short}
    Gets the n-th child in the list box (not counting headers).
  @end{short}
  If @arg{index} is negative or larger than the number of items in the list box,
  @code{nil} is returned.
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}"
  (listbox (g-object gtk-list-box))
  (index :int))

(export 'gtk-list-box-row-at-index)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_row_at_y () -> gtk-list-box-row-at-y
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_get_row_at_y" gtk-list-box-row-at-y)
    (g-object gtk-list-box-row)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[y]{an integer with the position of the row}
  @return{The @class{gtk-list-box-row} widget at position @arg{y}.}
  @begin{short}
    Gets the row at the y position in the list box.
  @end{short}
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}"
  (listbox (g-object gtk-list-box))
  (y :int))

(export 'gtk-list-box-row-at-y)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_filter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_invalidate_filter" gtk-list-box-invalidate-filter) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @begin{short}
    Update the filtering for all rows.
  @end{short}
  Call this when the result of the filter function on the list box is changed
  due to an external factor. For instance, this would be used if the filter
  function just looked for a specific search string and the entry with the
  search string has changed.
  @see-class{gtk-list-box}
  @see-function{gtk-list-box-set-filter-func}"
  (listbox (g-object gtk-list-box)))

(export 'gtk-list-box-invalidate-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_headers ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_invalidate_headers" gtk-list-box-invalidate-headers)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @begin{short}
    Update the separators for all rows.
  @end{short}
  Call this when the result of the header function on the box is changed due to
  an external factor.
  @see-class{gtk-list-box}
  @see-function{gtk-list-box-set-header-func}"
  (listbox (g-object gtk-list-box)))

(export 'gtk-list-box-invalidate-headers)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_sort ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_invalidate_sort" gtk-list-box-invalidate-sort) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @begin{short}
    Update the sorting for all rows.
  @end{short}
  Call this when the result of the sort function on the list box is changed due
  to an external factor.
  @see-class{gtk-list-box}
  @see-function{gtk-list-box-set-sort-func}"
  (listbox (g-object gtk-list-box)))

(export 'gtk-list-box-invalidate-sort)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxFilterFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-list-box-filter-func :boolean
    ((row (g-object gtk-list-box-row))
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr row)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-filter-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-list-box-filter-func atdoc:*external-symbols*)
 "@version{2021-4-30}
  @begin{short}
    Will be called whenever the row changes or is added and lets you control
    if the row should be visible or not.
  @end{short}
  @begin{pre}
 lambda (row)
  @end{pre}
  @begin[code]{table}
    @entry[row]{A @class{gtk-list-box-row} widget that may be filtered.}
    @entry[Returns]{@em{True} if the row should be visible, @em{false}
      otherwise.}
  @end{table}
  @see-class{gtk-list-box-row}
  @see-function{gtk-list-box-set-filter-func}")

(export 'gtk-list-box-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_filter_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_set_filter_func" %gtk-list-box-set-filter-func) :void
  (listbox (g-object gtk-list-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun gtk-list-box-set-filter-func (listbox func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[func]{a @symbol{gtk-list-box-filter-func} callback that lets you
    filter which rows to show}
  @begin{short}
    By setting a filter function on the list box one can decide dynamically
    which of the rows to show.
  @end{short}
  For instance, to implement a search function on a list that filters the
  original list to only show the matching rows.

  The function @arg{func} will be called for each row after the call, and it
  will continue to be called each time a row changes (via the function
  @fun{gtk-list-box-row-changed}) or when the function
  @fun{gtk-list-box-invalidate-filter} is called.

  Note that using a filter function is incompatible with using a model,
  see the function @fun{gtk-list-box-bind-model}.
  @see-class{gtk-list-box}
  @see-function{gtk-list-box-row-changed}
  @see-function{gtk-list-box-invalidate-filter}
  @see-function{gtk-list-box-bind-model}"
  (%gtk-list-box-set-filter-func listbox
                                 (callback gtk-list-box-filter-func)
                                 (allocate-stable-pointer func)
                                 (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-list-box-set-filter-func)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxUpdateHeaderFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-list-box-update-header-func :void
    ((row (g-object gtk-list-box-row))
     (before (g-object gtk-list-box-row))
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr row before)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-update-header-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-list-box-update-header-func atdoc:*external-symbols*)
 "@version{2021-4-30}
  @begin{short}
    Whenever @arg{row} changes or which row is before @arg{row} changes this is
    called, which lets you update the header on @arg{row}.
  @end{short}
  You may remove or set a new one via the function
  @fun{gtk-list-box-row-set-header-func} or just change the state of the current
  header widget.
  @begin{pre}
 lambda (row before)
  @end{pre}
  @begin[code]{table}
    @entry[row]{A @class{gtk-list-box-row} widget with the row to update.}
    @entry[before]{A @class{gtk-list-box-row} widget before @arg{row}, or
      @code{nil} if it is the first row.}
  @end{table}
  @see-class{gtk-list-box-row}
  @see-function{gtk-list-box-set-header-func}")

(export 'gtk-list-box-update-header-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_header_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_set_header_func" %gtk-list-box-set-header-func) :void
  (listbox (g-object gtk-list-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun gtk-list-box-set-header-func (listbox func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[func]{a @symbol{gtk-list-box-update-header-func} callback that lets
  you add row headers}
  @begin{short}
    By setting a header function on the list box one can dynamically add headers
    in front of rows, depending on the contents of the row and its position in
    the list.
  @end{short}
  For instance, one could use it to add headers in front of the first item of a
  new kind, in a list sorted by the kind.

  The @arg{func} callback can look at the current header widget using the
  function @fun{gtk-list-box-row-header} and either update the state of the
  widget as needed, or set a new one using the function
  @sym{(setf gtk-list-box-row-header)}. If no header is needed, set the header
  to @code{nil}.

  Note that you may get many calls of the @arg{func} callback to this for a
  particular row when e.g. changing things that do not affect the header. In
  this case it is important for performance to not blindly replace an existing
  header with an identical one.

  The @arg{func} callback will be called for each row after the call, and it
  will continue to be called each time a row changes (via the function
  @fun{gtk-list-box-row-changed}) and when the row before changes (either by
  the function @fun{gtk-list-box-row-changed} on the previous row, or when the
  previous row becomes a different row). It is also called for all rows when
  the function @fun{gtk-list-box-invalidate-headers} is called.
  @see-class{gtk-list-box}
  @see-symbol{gtk-list-box-update-header-func}
  @see-function{gtk-list-box-row-header}
  @see-function{gtk-list-box-row-changed}
  @see-function{gtk-list-box-invalidate-headers}"
  (%gtk-list-box-set-header-func listbox
                                 (callback gtk-list-box-update-header-func)
                                 (allocate-stable-pointer func)
                                 (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-list-box-set-header-func)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxSortFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-list-box-sort-func :int
    ((row1 (g-object gtk-list-box-row))
     (row2 (g-object gtk-list-box-row))
     (data :pointer))
  (let ((ptr (get-stable-pointer-value data)))
    (funcall ptr row1 row2)))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-sort-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-list-box-sort-func atdoc:*external-symbols*)
 "@version{2021-4-30}
  @begin{short}
    The type of the callback function that compares two rows to determine which
    should be first.
  @end{short}
  @begin{pre}
 lambda (row1 row2)
  @end{pre}
  @begin[code]{table}
    @entry[row1]{A @class{gtk-list-box-row} widget with the first row.}
    @entry[row2]{A @class{gtk-list-box-row} widget with the second row.}
    @entry[Return]{An integer which is < 0 if @arg{row1} should be before
      @arg{row2}, 0 if they are equal and > 0 otherwise.}
  @end{table}
  @see-class{gtk-list-box-row}
  @see-function{gtk-list-box-set-sort-func}")

(export 'gtk-list-box-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_sort_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_set_sort_func" %gtk-list-box-set-sort-func) :void
  (listbox (g-object gtk-list-box))
  (sort-func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defun gtk-list-box-set-sort-func (listbox func)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[func]{a @symbol{gtk-list-box-sort-func} callback for the sort
   function}
  @begin{short}
    By setting a sort function on the list box one can dynamically reorder the
    rows of the list, based on the contents of the rows.
  @end{short}

  The @arg{func} callback will be called for each row after the call, and will
  continue to be called each time a row changes (via the function
  @fun{gtk-list-box-row-changed}) and when the function
  @fun{gtk-list-box-invalidate-sort} is called.

  Note that using a sort function is incompatible with using a model. See the
  function @fun{gtk-list-box-bind-model}.
  @see-class{gtk-list-box}
  @see-symbol{gtk-list-box-sort-func}
  @see-function{gtk-list-box-row-changed}
  @see-function{gtk-list-box-invalidate-sort}
  @see-function{gtk-list-box-bind-model}"
  (%gtk-list-box-set-sort-func listbox
                               (callback gtk-list-box-sort-func)
                               (allocate-stable-pointer func)
                               (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-list-box-set-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_drag_highlight_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_drag_highlight_row" gtk-list-box-drag-highlight-row)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @argument[row]{a @class{gtk-list-box-row} widget}
  @begin{short}
    This is a helper function for implementing DnD onto a list box.
  @end{short}
  The passed in row will be highlighted via the function
  @fun{gtk-drag-highlight}, and any previously highlighted row will be
  unhighlighted.

  The row will also be unhighlighted when the widget gets a drag leave event.
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}
  @see-function{gtk-drag-highlight}"
  (listbox (g-object gtk-list-box))
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-drag-highlight-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_drag_unhighlight_row ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_drag_unhighlight_row" gtk-list-box-drag-unhighlight-row)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[listbox]{a @class{gtk-list-box} widget}
  @begin{short}
    If a row has previously been highlighted via the function
    @fun{gtk-list-box-drag-highlight-row} it will have the highlight removed.
  @end{short}
  @see-class{gtk-list-box}
  @see-function{gtk-list-box-drag-highlight-row}"
  (listbox (g-object gtk-list-box)))

(export 'gtk-list-box-drag-unhighlight-row)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxCreateWidgetFunc ()
;;;
;;; GtkWidget * (*GtkListBoxCreateWidgetFunc) (gpointer item,
;;;                                            gpointer user_data);
;;;
;;; Called for list boxes that are bound to a GListModel with
;;; gtk_list_box_bind_model() for each item that gets added to the model.
;;;
;;; Versions of GTK prior to 3.18 called gtk_widget_show_all() on the rows
;;; created by the GtkListBoxCreateWidgetFunc, but this forced all widgets
;;; inside the row to be shown, and is no longer the case. Applications should
;;; be updated to show the desired row widgets.
;;;
;;; item :
;;;     the item from the model for which to create a widget for.
;;;
;;; user_data :
;;;     user data.
;;;
;;; Returns :
;;;     a GtkWidget that represents item .
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_bind_model ()
;;;
;;; void gtk_list_box_bind_model (GtkListBox *box,
;;;                               GListModel *model,
;;;                               GtkListBoxCreateWidgetFunc create_widget_func,
;;;                               gpointer user_data,
;;;                               GDestroyNotify user_data_free_func);
;;;
;;; Binds model to box .
;;;
;;; If box was already bound to a model, that previous binding is destroyed.
;;;
;;; The contents of box are cleared and then filled with widgets that represent
;;; items from model . box is updated whenever model changes. If model is NULL,
;;; box is left empty.
;;;
;;; It is undefined to add or remove widgets directly (for example, with
;;; gtk_list_box_insert() or gtk_container_add()) while box is bound to a model.
;;;
;;; Note that using a model is incompatible with the filtering and sorting
;;; functionality in GtkListBox. When using a model, filtering and sorting
;;; should be implemented by the model.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; model :
;;;     the GListModel to be bound to box .
;;;
;;; create_widget_func :
;;;     a function that creates widgets for items or NULL in case you also
;;;     passed NULL as model .
;;;
;;; user_data :
;;;     user data passed to create_widget_func
;;;
;;; user_data_free_func :
;;;     function for freeing user_data
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;; TODO: GListModel ist not implemented

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-list-box-row-new))

(defun gtk-list-box-row-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @return{A new @class{gtk-list-box-row} widget.}
  @begin{short}
    Creates a new list box row, to be used as a child of a list box.
  @end{short}
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}"
  (make-instance 'gtk-list-box-row))

(export 'gtk-list-box-row-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_row_changed" gtk-list-box-row-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[row]{a @class{gtk-list-box-row} widget}
  @begin{short}
    Marks @arg{row} as changed, causing any state that depends on this to be
    updated.
  @end{short}
  This affects sorting, filtering and headers.

  Note that calls to this method must be in sync with the data used for the row
  functions. For instance, if the list box is mirroring some external data set,
  and *two* rows changed in the external data set then when you call the
  function @sym{gtk-list-box-row-changed} on the first row the sort function
  must only read the new data for the first of the two changed rows, otherwise
  the resorting of the rows will be wrong.

  This generally means that if you do not fully control the data model you have
  to duplicate the data that affects the list box row functions into the row
  widgets themselves. Another alternative is to call the function
  @fun{gtk-list-box-invalidate-sort} on any model change, but that is more
  expensive.
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}
  @see-function{gtk-list-box-invalidate-sort}"
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-row-changed)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_is_selected ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_row_is_selected" gtk-list-box-row-is-selected) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-7-13}
  @argument[row]{a @class{gtk-list-box-row} widget}
  @begin{short}
    Returns a boolean whether the child is currently selected in its list box
    container.
  @end{short}
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}"
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-row-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_get_header ()
;;; gtk_list_box_row_set_header () -> gtk-list-box-row-header
;;; ----------------------------------------------------------------------------

(defun (setf gtk-list-box-row-header) (header row)
  (foreign-funcall "gtk_list_box_row_set_header"
                   (g-object gtk-list-box-row) row
                   (g-object gtk-widget) header
                   :void)
  header)

(defcfun ("gtk_list_box_row_get_header" gtk-list-box-row-header)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @syntax[]{(gtk-list-box-row-header row) => header}
  @syntax[]{(setf (gtk-list-box-row-header row) header)}
  @argument[row]{a @class{gtk-list-box-row} widget}
  @argument[header]{a @class{gtk-widget} object}
  @begin{short}
    Accessor of the header widget of a list box row.
  @end{short}

  The function @sym{gtk-list-box-row-header} returns the current header of the
  list box row. This can be used in a @symbol{gtk-list-box-update-header-func}
  callback to see if there is a header set already, and if so to update the
  state of it.

  The function @sym{(setf gtk-list-box-row-header)} sets the current header of
  the list box row. This is only allowed to be called from a
  @symbol{gtk-list-box-update-header-func} callback. It will replace any
  existing header in the row, and be shown in front of the row in the list box.
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}
  @see-class{gtk-widget}
  @see-symbol{gtk-list-box-update-header-func}"
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-row-header)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_get_index () -> gtk-list-box-row-index
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_row_get_index" gtk-list-box-row-index) :int
 #+cl-cffi-gtk-documentation
 "@version{2021-4-30}
  @argument[row]{a @class{gtk-list-box-row} widget}
  @return{An integer with the index of the row in the list box, or -1 if the
    row is not in the list box.}
  @begin{short}
    Gets the current index of the row in its list box container.
  @end{short}
  @see-class{gtk-list-box}
  @see-class{gtk-list-box-row}"
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-row-index)

;;; --- End of file gtk.list-box.lisp ------------------------------------------
