;;; ----------------------------------------------------------------------------
;;; gtk.list-box.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 Dieter Kaiser
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
;;;     (*GtkListBoxFilterFunc)
;;;     (*GtkListBoxSortFunc)
;;;     (*GtkListBoxUpdateHeaderFunc)
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
;;;     (*GtkListBoxForeachFunc)
;;;
;;;     gtk_list_box_selected_foreach
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
;;;     gtk_list_box_set_filter_func
;;;     gtk_list_box_set_header_func
;;;     gtk_list_box_set_sort_func
;;;     gtk_list_box_drag_highlight_row
;;;     gtk_list_box_drag_unhighlight_row
;;;
;;;     (*GtkListBoxCreateWidgetFunc)
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
;;;     activate-on-single-click
;;;     selection-mode
;;;     activatable
;;;     selectable
;;;
;;; Signals
;;;
;;;     activate-cursor-row
;;;     move-cursor
;;;     row-activated
;;;     row-selected
;;;     select-all
;;;     selected-rows-changed
;;;     toggle-cursor-row
;;;     unselect-all
;;;     activate
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
  nil)

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
    "selection-mode" "GtkSelectionMode" t t)
   (activatable
    gtk-list-box-activatable
    "activatable" "gboolean" t t)
   (selectable
    gtk-list-box-selectable
    "selectable" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-list-box 'type)
 "@version{2019-3-10}
  @begin{short}
    A @sym{gtk-list-box} is a vertical container that contains
    @class{gtk-list-box-row} children.
  @end{short}
  These rows can by dynamically sorted and filtered, and headers can be added
  dynamically depending on the row content. It also allows keyboard and mouse
  navigation and selection like a typical list.

  @image[list-box]{}

  Using @sym{gtk-list-box} is often an alternative to @class{gtk-tree-view},
  especially when the list contents has a more complicated layout than what is
  allowed by a @class{gtk-cell-renderer}, or when the contents is interactive
  (i. e. has a button in it).

  Although a @sym{gtk-list-box} must have only @classl{gtk-list-box-row}
  children you can add any kind of widget to it via the function
  @fun{gtk-container-add}, and a @class{gtk-list-box-row} widget will
  automatically be inserted between the list and the widget.

  @class{gtk-list-box-rows} can be marked as activatable or selectable. If a row
  is activatable, \"row-activated\" will be emitted for it when the user tries
  to activate it. If it is selectable, the row will be marked as selected when
  the user tries to select it.

  Since 3.10
  @begin[CSS Nodes]{dictionary}
    @begin{pre}
  list
   ╰── row[.activatable]
    @end{pre}
    @sym{gtk-list-box} uses a single CSS node named @code{list}. Each
    @class{gtk-list-box-row} uses a single CSS node named @code{row}. The row
    nodes get the @code{.activatable} style class added when appropriate.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-cursor-row\" signal}
      @begin{pre}
 lambda (listbox)
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} on which the signal is emitted.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
  lambda (listbox arg1 arg2)
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} on which the signal is emitted.}
        @entry[arg1]{A @symbol{gtk-movement-step}}
        @entry[arg2]{A integer}
      @end{table}
    @subheading{The \"row-activated\" signal}
      @begin{pre}
  lambda (listbox row)
      @end{pre}
      The \"row-activated\" signal is emitted when a row has been activated by
      the user.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} on which the signal is emitted.}
        @entry[row]{The activated row.}
      @end{table}
    @subheading{The \"row-selected\" signal}
      @begin{pre}
  lambda (listbox row)
      @end{pre}
      The \"row-selected\" signal is emitted when a new row is selected, or
      (with a @code{nil} row ) when the selection is cleared.
      When the box is using the selection mode @code{:multiple}, this signal
      will not give you the full picture of selection changes, and you should
      use the \"selected-rows-changed\" signal instead.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} on which the signal is emitted.}
        @entry[row]{The selected row.}
      @end{table}
    @subheading{The \"select-all\" signal}
      @begin{pre}
  lambda (listbox)
      @end{pre}
      The \"select-all\" signal is a keybinding signal which gets emitted to
      select all children of the box, if the selection mode permits it.
      The default bindings for this signal is Ctrl-a.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} on which the signal is emitted.}
      @end{table}
      Since 3.14

    @subheading{The \"selected-rows-changed\" signal}
      @begin{pre}
  lambda (listbox)
      @end{pre}
      The \"selected-rows-changed\" signal is emitted when the set of selected
      rows changes.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} on which the signal is emitted.}
      @end{table}
      Since 3.14

    @subheading{The \"toggle-cursor-row\" signal}
      @begin{pre}
  lambda (listbox)
      @end{pre}
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} on which the signal is emitted.}
      @end{table}
    @subheading{The \"unselect-all\" signal}
      @begin{pre}
  lambda (listbox)
      @end{pre}
        The \"unselect-all\" signal is a keybinding signal which gets emitted to
        unselect all children of the box, if the selection mode permits it.
        The default bindings for this signal is Ctrl-Shift-a.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} on which the signal is emitted.}
      @end{table}
      Since 3.14

    @subheading{The \"activate\" signal}
      @begin{pre}
  lambda (listbox)
      @end{pre}
      This is a keybinding signal, which will cause this row to be activated.
      If you want to be notified when the user activates a row (by key or not),
      use the \"row-activated\" signal on the row’s parent @class{gtk-list-box}.
      @begin[code]{table}
        @entry[listbox]{The @sym{gtk-list-box} on which the signal is emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-list-box-activate-on-single-click}
  @see-slot{gtk-list-box-selection-mode}
  @see-slot{gtk-list-box-activatable}
  @see-slot{gtk-list-box-selectable}
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
  Activate row on a single click.@br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-activate-on-single-click
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-list-box-activate-on-single-click 'function)
 "@version{2019-3-10}
  @argument[object]{a @class{gtk-list-box} widget}
  @argument[single]{a boolean}
  @syntax[]{(gtk-list-box-acivate-on-click object) => single}
  @syntax[]{(setf (gtk-list-box-activate-on-click object) single)}
  @begin{short}
    Accessor of the slot @slot[gtk-list-box]{activate-on-single-click} of the
    @class{gtk-list-box} class.
  @end{short}

  The generic function @sym{gtk-list-box-activate-on-single-click} returns
  whether rows activate on single clicks.

  If single is @em{true}, rows will be activated when you click on them,
  otherwise you need to double-click.
  @see-class{gtk-list-box}")

;;; --- gtk-list-box-selection-mode --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selection-mode"
                                               'gtk-list-box) 't)
 "The @code{selection-mode} property of type @symbol{gtk-selection-mode}
  (Read / Write) @br{}
  The selection mode.
  Default value: @code{:single}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-selection-mode atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-list-box-selection-mode 'function)
 "@version{2019-3-10}
  @argument[object]{a @class{gtk-list-box} widget}
  @argument[mode]{a @symbol{gtk-selection-mode}}
  @syntax[]{(gtk-list-box-selection-mode object) => mode}
  @syntax[]{(setf (gtk-list-box-selection-mode object) mode)}
  @begin{short}
    Accessor of the slot @slot[gtk-list-box]{selection-mode} of the
    @class{gtk-list-box} class.
  @end{short}

  The generic function @sym{gtk-list-box-selection-mode} gets the selection mode
  of the listbox.

  The generic function @sym{(setf gtk-list-box-selection-mode)} sets how
  selection works in the listbox. See @symbol{gtk-selection-mode} for details.
  @see-class{gtk-list-box}
  @see-symbol{gtk-selection-mode}")

;;; --- gtk-list-box-activatable -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "activatable"
                                               'gtk-list-box) 't)
 "The @code{activatable} property of type @code{:boolean}
  (Read / Write) @br{}
  The property determines whether the \"row-activated\" signal will be emitted
  for this row. @br{}
  Since 3.14@br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-activatable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-list-box-activatable 'function)
 "@version{2019-3-10}
  @argument[object]{a @class{gtk-list-box} widget}
  @argument[activatable]{@em{true} to mark the row as activatable}
  @syntax[]{(gtk-list-box-activatable object) => activatable}
  @syntax[]{(setf (gtk-list-box-activatable object) activatable)}
  @begin{short}
    Accessor of the slot @slot[gtk-list-box]{activatable} of the
    @class{gtk-list-box} class.
  @end{short}

  The generic function @sym{gtk-list-box-activatable} gets the value of the
  @code{activatable} property for this row.

  The generic function @sym{(setf gtk-list-box-activatable)} set the
  @code{activatable} property for this row.

  Since 3.14
  @see-class{gtk-list-box}")

;;; --- gtk-list-box-selectable ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "selectable"
                                               'gtk-list-box) 't)
 "The @code{selectable} property of type @code{:boolean}
  (Read / Write) @br{}
  The property determines whether this row can be selected. @br{}
  Since 3.14 @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-list-box-selectable atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-list-box-selectable 'function)
 "@version{2019-3-10}
  @argument[object]{a @class{gtk-list-box} widget}
  @argument[selectable]{@em{true} to mark the row as selectable}
  @syntax[]{(gtk-list-box-selectable object) => selectable}
  @syntax[]{(setf (gtk-list-box-selectable object) selectable)}
  @begin{short}
    Accessor of the slot @slot[gtk-list-box]{selectable} of the
    @class{gtk-list-box} class.
  @end{short}

  The generic function @sym{gtk-list-box-selectable} gets the value of the
  @code{selectable} property for this row.

  The generic function @sym{(setf gtk-list-box-selectable)} set the
  @code{selectable} property for this row.

  Since 3.14
  @see-class{gtk-list-box}")

;;; ----------------------------------------------------------------------------
;;; GtkListBoxFilterFunc ()
;;;
;;; gboolean (*GtkListBoxFilterFunc) (GtkListBoxRow *row,
;;;                                   gpointer user_data);
;;;
;;; Will be called whenever the row changes or is added and lets you control
;;; if the row should be visible or not.
;;;
;;; row :
;;;     the row that may be filtered
;;;
;;; user_data :
;;;     user data.
;;;
;;; Returns :
;;;    TRUE if the row should be visible, FALSE otherwise
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkListBoxSortFunc ()
;;;
;;; gint (*GtkListBoxSortFunc) (GtkListBoxRow *row1,
;;;                             GtkListBoxRow *row2,
;;;                             gpointer user_data);
;;;
;;; Compare two rows to determine which should be first.
;;;
;;; row1 :
;;;     the first row
;;;
;;; row2 :
;;;     the second row
;;;
;;; user_data :
;;;     user data.
;;;
;;; Returns :
;;;     < 0 if row1 should be before row2 , 0 if they are equal and > 0
;;;     otherwise
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkListBoxUpdateHeaderFunc ()
;;;
;;; void (*GtkListBoxUpdateHeaderFunc) (GtkListBoxRow *row,
;;;                                     GtkListBoxRow *before,
;;;                                     gpointer user_data);
;;;
;;; Whenever row changes or which row is before row changes this is called,
;;; which lets you update the header on row . You may remove or set a new one
;;; via gtk_list_box_row_set_header() or just change the state of the current
;;; header widget.
;;;
;;; row :
;;;     the row to update
;;;
;;; before :
;;;     the row before row , or NULL if it is first.
;;;
;;; user_data :
;;;     user data.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_new ()
;;;
;;; GtkWidget * gtk_list_box_new (void);
;;;
;;; Creates a new GtkListBox container.
;;;
;;; Returns :
;;;     a new GtkListBox
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-list-box-new))

(defun gtk-list-box-new ()
  (make-instance 'gtk-list-box))

(export 'gtk-list-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_prepend ()
;;;
;;; void gtk_list_box_prepend (GtkListBox *box, GtkWidget *child);
;;;
;;; Prepend a widget to the list. If a sort function is set, the widget will
;;; actually be inserted at the calculated position and this function has the
;;; same effect of gtk_container_add().
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; child
;;;     the GtkWidget to add
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_prepend" gtk-list-box-prepend) :void
  (box (g-object gtk-list-box))
  (child (g-object gtk-widget)))

(export 'gtk-list-box-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_insert ()
;;;
;;; void gtk_list_box_insert (GtkListBox *box,
;;;                           GtkWidget *child,
;;;                           gint position);
;;;
;;; Insert the child into the box at position . If a sort function is set, the
;;; widget will actually be inserted at the calculated position and this
;;; function has the same effect of gtk_container_add().
;;;
;;; If position is -1, or larger than the total number of items in the box ,
;;; then the child will be appended to the end.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; child :
;;;     the GtkWidget to add
;;;
;;; position :
;;;     the position to insert child in
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_insert" gtk-list-box-insert) :void
  (box (g-object gtk-list-box))
  (child (g-object gtk-widget))
  (position :int))

(export 'gtk-list-box-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_select_row ()
;;;
;;; void gtk_list_box_select_row (GtkListBox *box, GtkListBoxRow *row);
;;;
;;; Make row the currently selected row.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; row :
;;;     The row to select or NULL.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_select_row" gtk-list-box-select-row) :void
  (box (g-object gtk-list-box))
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-select-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_unselect_row ()
;;;
;;; void gtk_list_box_unselect_row (GtkListBox *box,
;;;                                 GtkListBoxRow *row);
;;;
;;; Unselects a single row of box , if the selection mode allows it.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; row :
;;;     the row to unselected
;;;
;;; Since: 3.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_unselect_row" gtk-list-box-unselect-row) :void
  (box (g-object gtk-list-box))
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-unselect-row)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_select_all ()
;;;
;;; void gtk_list_box_select_all (GtkListBox *box);
;;;
;;; Select all children of box , if the selection mode allows it.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; Since: 3.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_select_all" gtk-list-box-select-all) :void
  (box (g-object gtk-list-box)))

(export 'gtk-list-box-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_unselect_all ()
;;;
;;; void gtk_list_box_unselect_all (GtkListBox *box);
;;;
;;; Unselect all children of box , if the selection mode allows it.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; Since: 3.14
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_unselect_all" gtk-list-box-unselect-all) :void
  (box (g-object gtk-list-box)))

(export 'gtk-list-box-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_selected_row ()
;;;
;;; GtkListBoxRow * gtk_list_box_get_selected_row (GtkListBox *box);
;;;
;;; Gets the selected row.
;;;
;;; Note that the box may allow multiple selection, in which case you should use
;;; gtk_list_box_selected_foreach() to find all selected rows.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; Returns :
;;;     the selected row.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_get_selected_row" gtk-list-box-get-selected-row)
    (g-object gtk-list-box-row)
  (box (g-object gtk-list-box)))

(export 'gtk-list-bet-get-selected-row)

;;; ----------------------------------------------------------------------------
;;; GtkListBoxForeachFunc ()
;;;
;;; void (*GtkListBoxForeachFunc) (GtkListBox *box,
;;;                                GtkListBoxRow *row,
;;;                                gpointer user_data);
;;;
;;; A function used by gtk_list_box_selected_foreach(). It will be called on
;;; every selected child of the box .
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; row :
;;;     a GtkListBoxRow
;;;
;;; user_data :
;;;     user data.
;;;
;;; Since: 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_selected_foreach ()
;;;
;;; void gtk_list_box_selected_foreach (GtkListBox *box,
;;;                                     GtkListBoxForeachFunc func,
;;;                                     gpointer data);
;;;
;;; Calls a function for each selected child.
;;;
;;; Note that the selection cannot be modified from within this function.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; func :
;;;     the function to call for each selected child.
;;;
;;; data :
;;;     user data to pass to the function
;;;
;;; Since: 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_selected_rows ()
;;;
;;; GList * gtk_list_box_get_selected_rows (GtkListBox *box);
;;;
;;; Creates a list of all selected children.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; Returns :
;;;     A GList containing the GtkWidget for each selected child. Free with
;;;     g_list_free() when done.
;;;
;;; Since: 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_adjustment ()
;;;
;;; GtkAdjustment * gtk_list_box_get_adjustment (GtkListBox *box);
;;;
;;; Gets the adjustment (if any) that the widget uses to for vertical scrolling.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; Returns :
;;;     the adjustment.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_get_adjustment" gtk-list-box-get-adjustment)
    (g-object gtk-adjustment)
  (box (g-object gtk-list-box)))

(export 'gtk-list-box-get-adjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_adjustment ()
;;;
;;; void gtk_list_box_set_adjustment (GtkListBox *box,
;;;                                   GtkAdjustment *adjustment);
;;;
;;; Sets the adjustment (if any) that the widget uses to for vertical scrolling.
;;; For instance, this is used to get the page size for PageUp/Down key
;;; handling.
;;;
;;; In the normal case when the box is packed inside a GtkScrolledWindow the
;;; adjustment from that will be picked up automatically, so there is no need to
;;; manually do that.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; adjustment
;;;     the adjustment, or NULL.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_set_adjustment" gtk-list-box-set-adjustment) :void
  (box (g-object gtk-list-box))
  (adjustment (g-object gtk-adjustment)))

(export 'gtk-list-box-set-adjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_placeholder ()
;;;
;;; void gtk_list_box_set_placeholder (GtkListBox *box,
;;;                                    GtkWidget *placeholder);
;;;
;;; Sets the placeholder widget that is shown in the list when it doesn't
;;; display any visible children.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; placeholder :
;;;     a GtkWidget or NULL.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_set_placeholder" gtk-list-box-set-placeholder) :void
  (box (g-object gtk-list-box))
  (placeholder (g-object gtk-widget)))

(export 'gtk-list-box-set-placeholder)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_row_at_index ()
;;;
;;; GtkListBoxRow * gtk_list_box_get_row_at_index (GtkListBox *box, gint index_)
;;;
;;; Gets the n-th child in the list (not counting headers). If _index is
;;; negative or larger than the number of items in the list, NULL is returned.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; index_ :
;;;     the index of the row
;;;
;;; Returns :
;;;     the child GtkWidget or NULL.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_get_row_at_y ()
;;;
;;; GtkListBoxRow * gtk_list_box_get_row_at_y (GtkListBox *box, gint y);
;;;
;;; Gets the row at the y position.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; y :
;;;     position
;;;
;;; Returns :
;;;     the row or NULL in case no row exists for the given y coordinate.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_filter ()
;;;
;;; void gtk_list_box_invalidate_filter (GtkListBox *box);
;;;
;;; Update the filtering for all rows. Call this when result of the filter
;;; function on the box is changed due to an external factor. For instance, this
;;; would be used if the filter function just looked for a specific search
;;; string and the entry with the search string has changed.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_headers ()
;;;
;;; void gtk_list_box_invalidate_headers (GtkListBox *box);
;;;
;;; Update the separators for all rows. Call this when result of the header
;;; function on the box is changed due to an external factor.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_invalidate_sort ()
;;;
;;; void gtk_list_box_invalidate_sort (GtkListBox *box);
;;;
;;; Update the sorting for all rows. Call this when result of the sort function
;;; on the box is changed due to an external factor.
;;;
;;; box:
;;;     a GtkListBox
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_filter_func ()
;;;
;;; void gtk_list_box_set_filter_func (GtkListBox *box,
;;;                                    GtkListBoxFilterFunc filter_func,
;;;                                    gpointer user_data,
;;;                                    GDestroyNotify destroy);
;;;
;;; By setting a filter function on the box one can decide dynamically which of
;;; the rows to show. For instance, to implement a search function on a list
;;; that filters the original list to only show the matching rows.
;;;
;;; The filter_func will be called for each row after the call, and it will
;;; continue to be called each time a row changes
;;; (via gtk_list_box_row_changed()) or when gtk_list_box_invalidate_filter()
;;; is called.
;;;
;;; Note that using a filter function is incompatible with using a model
;;; (see gtk_list_box_bind_model()).
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; filter_func :
;;;     callback that lets you filter which rows to show.
;;;
;;; user_data :
;;;     user data passed to filter_func
;;;
;;; destroy :
;;;     destroy notifier for user_data
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_header_func ()
;;;
;;; void gtk_list_box_set_header_func (GtkListBox *box,
;;;                                    GtkListBoxUpdateHeaderFunc update_header,
;;;                                    gpointer user_data,
;;;                                    GDestroyNotify destroy);
;;;
;;; By setting a header function on the box one can dynamically add headers in
;;; front of rows, depending on the contents of the row and its position in the
;;; list. For instance, one could use it to add headers in front of the first
;;; item of a new kind, in a list sorted by the kind.
;;;
;;; The update_header can look at the current header widget using
;;; gtk_list_box_row_get_header() and either update the state of the widget as
;;; needed, or set a new one using gtk_list_box_row_set_header(). If no header
;;; is needed, set the header to NULL.
;;;
;;; Note that you may get many calls update_header to this for a particular row
;;; when e.g. changing things that don’t affect the header. In this case it is
;;; important for performance to not blindly replace an existing header with an
;;; identical one.
;;;
;;; The update_header function will be called for each row after the call, and
;;; it will continue to be called each time a row changes (via
;;; gtk_list_box_row_changed()) and when the row before changes (either by
;;; gtk_list_box_row_changed() on the previous row, or when the previous row
;;; becomes a different row). It is also called for all rows when
;;; gtk_list_box_invalidate_headers() is called.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; update_header :
;;;     callback that lets you add row headers.
;;;
;;; user_data :
;;;     user data passed to update_header
;;;
;;; destroy :
;;;     destroy notifier for user_data
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_set_sort_func ()
;;;
;;; void gtk_list_box_set_sort_func (GtkListBox *box,
;;;                                  GtkListBoxSortFunc sort_func,
;;;                                  gpointer user_data,
;;;                                  GDestroyNotify destroy);
;;;
;;; By setting a sort function on the box one can dynamically reorder the rows
;;; of the list, based on the contents of the rows.
;;;
;;; The sort_func will be called for each row after the call, and will continue
;;; to be called each time a row changes (via gtk_list_box_row_changed()) and
;;; when gtk_list_box_invalidate_sort() is called.
;;;
;;; Note that using a sort function is incompatible with using a model (see
;;; gtk_list_box_bind_model()).
;;;
;;; box:
;;;     a GtkListBox
;;;
;;; sort_func :
;;;     the sort function.
;;;
;;; user_data :
;;;     user data passed to sort_func
;;;
;;; destroy :
;;;     destroy notifier for user_data
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_drag_highlight_row ()
;;;
;;; void gtk_list_box_drag_highlight_row (GtkListBox *box,
;;;                                       GtkListBoxRow *row);
;;;
;;; This is a helper function for implementing DnD onto a GtkListBox. The passed
;;; in row will be highlighted via gtk_drag_highlight(), and any previously
;;; highlighted row will be unhighlighted.
;;;
;;; The row will also be unhighlighted when the widget gets a drag leave event.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; row :
;;;     a GtkListBoxRow
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_drag_unhighlight_row ()
;;;
;;; void gtk_list_box_drag_unhighlight_row (GtkListBox *box);
;;;
;;; If a row has previously been highlighted via
;;; gtk_list_box_drag_highlight_row() it will have the highlight removed.
;;;
;;; box :
;;;     a GtkListBox
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkListBoxCreateWidgetFunc ()
;;;
;;; GtkWidget * (*GtkListBoxCreateWidgetFunc) (gpointer item,
;;;                                            gpointer user_data);
;;;
;;; Called for list boxes that are bound to a GListModel with
;;; gtk_list_box_bind_model() for each item that gets added to the model.
;;;
;;; Versions of GTK+ prior to 3.18 called gtk_widget_show_all() on the rows
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
;;; Since: 3.16
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
;;; Since: 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_new ()
;;;
;;; GtkWidget * gtk_list_box_row_new (void);
;;;
;;; Creates a new GtkListBoxRow, to be used as a child of a GtkListBox.
;;;
;;; Returns :
;;;     a new GtkListBoxRow
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_changed ()
;;;
;;; void gtk_list_box_row_changed (GtkListBoxRow *row);
;;;
;;; Marks row as changed, causing any state that depends on this to be updated.
;;; This affects sorting, filtering and headers.
;;;
;;; Note that calls to this method must be in sync with the data used for the
;;; row functions. For instance, if the list is mirroring some external data
;;; set, and *two* rows changed in the external data set then when you call
;;; gtk_list_box_row_changed() on the first row the sort function must only read
;;; the new data for the first of the two changed rows, otherwise the resorting
;;; of the rows will be wrong.
;;;
;;; This generally means that if you don’t fully control the data model you have
;;; to duplicate the data that affects the listbox row functions into the row
;;; widgets themselves. Another alternative is to call
;;; gtk_list_box_invalidate_sort() on any model change, but that is more
;;; expensive.
;;;
;;; row :
;;;     a GtkListBoxRow
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_is_selected ()
;;;
;;; gboolean gtk_list_box_row_is_selected (GtkListBoxRow *row);
;;;
;;; Returns whether the child is currently selected in its GtkListBox container.
;;;
;;; row :
;;;     a GtkListBoxRow
;;;
;;; Returns :
;;;     TRUE if row is selected
;;;
;;; Since: 3.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_get_header ()
;;;
;;; GtkWidget * gtk_list_box_row_get_header (GtkListBoxRow *row);
;;;
;;; Returns the current header of the row . This can be used in a
;;; GtkListBoxUpdateHeaderFunc to see if there is a header set already, and if
;;; so to update the state of it.
;;;
;;; row :
;;;     a GtkListBoxRow
;;;
;;; Returns :
;;;     the current header, or NULL if none.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_row_get_header" gtk-list-box-row-get-header)
    (g-object gtk-widget)
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-row-get-header)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_set_header ()
;;;
;;; void gtk_list_box_row_set_header (GtkListBoxRow *row, GtkWidget *header);
;;;
;;; Sets the current header of the row . This is only allowed to be called from
;;; a GtkListBoxUpdateHeaderFunc. It will replace any existing header in the
;;; row, and be shown in front of the row in the listbox.
;;;
;;; row :
;;;     a GtkListBoxRow
;;;
;;; header :
;;;     the header, or NULL.
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_row_set_header" gtk-list-box-row-set-header) :void
  (row (g-object gtk-list-box-row))
  (header (g-object gtk-widget)))

(export 'gtk-list-box-row-set-header)

;;; ----------------------------------------------------------------------------
;;; gtk_list_box_row_get_index ()
;;;
;;; gint gtk_list_box_row_get_index (GtkListBoxRow *row);
;;;
;;; Gets the current index of the row in its GtkListBox container.
;;;
;;; row :
;;;     a GtkListBoxRow
;;;
;;; Returns :
;;;     the index of the row , or -1 if the row is not in a listbox
;;;
;;; Since: 3.10
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_list_box_row_get_index" gtk-list-box-row-get-index) :int
  (row (g-object gtk-list-box-row)))

(export 'gtk-list-box-row-get-index)

;;; --- End of file gtk.list-box.lisp ------------------------------------------
