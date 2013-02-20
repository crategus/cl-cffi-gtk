;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-combo.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See >http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkCellRendererCombo
;;;
;;; Renders a combobox in a cell
;;;
;;; Synopsis
;;;
;;;     GtkCellRendererCombo
;;;
;;;     gtk_cell_renderer_combo_new
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererCombo
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererCombo" gtk-cell-renderer-combo
  (:superclass gtk-cell-renderer-text
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_combo_get_type")
  ((has-entry
    gtk-cell-renderer-combo-has-entry
    "has-entry" "gboolean" t t)
   (model
    gtk-cell-renderer-combo-model
    "model" "GtkTreeModel" t t)
   (text-column
    gtk-cell-renderer-combo-text-column
    "text-column" "gint" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-combo 'type)
 "@version{2013-2-20}
  @begin{short}
    GtkCellRendererCombo renders text in a cell like GtkCellRendererText from
    which it is derived.
  @end{short}
  But while GtkCellRendererText offers a simple entry to edit the text,
  GtkCellRendererCombo offers a GtkComboBox or GtkComboBoxEntry widget to edit
  the text. The values to display in the combo box are taken from the tree model
  specified in the @code{\"model\"} property.

  The combo cell renderer takes care of adding a text cell renderer to the
  combo box and sets it to display the column specified by its
  @code{\"text-column\"} property. Further properties of the comnbo box can be
  set in a handler for the \"editing-started\" signal.

  The GtkCellRendererCombo cell renderer was added in GTK+ 2.6.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      This signal is emitted each time after the user selected an item in the
      combo box, either by using the mouse or the arrow keys. Contrary to
      GtkComboBox, GtkCellRendererCombo::changed is not emitted for changes made
      to a selected item in the entry. The argument new_iter corresponds to the
      newly selected item in the combo box and it is relative to the
      GtkTreeModel set via the model property on GtkCellRendererCombo.

      Note that as soon as you change the model displayed in the tree view, the
      tree view will immediately cease the editing operating. This means that
      you most probably want to refrain from changing the model until the combo
      cell renderer emits the edited or editing_canceled signal.
      @begin{pre}
 void user_function (GtkCellRendererCombo *combo,
                     gchar                *path_string,
                     GtkTreeIter          *new_iter,
                     gpointer              user_data)        : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[combo]{the object on which the signal is emitted}
        @entry[path_string]{a string of the path identifying the edited cell
          (relative to the tree view model)}
        @entry[new_iter]{the new iter selected in the combo box (relative to the
          combo box model)}
        @entry[user_data]{user data set when the signal handler was connected.}
       @end{table}
       Since 2.14
  @end{dictionary}
  @see-slot{gtk-cell-renderer-combo-has-entry}
  @see-slot{gtk-cell-renderer-combo-model}
  @see-slot{gtk-cell-renderer-combo-text-column}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-entry" 'gtk-cell-renderer-combo) 't)
 "The @code{\"has-entry\"} property of type @code{gboolean} (Read / Write)@br{}
  If TRUE, the cell renderer will include an entry and allow to enter values
  other than the ones in the popup list.@br{}
  Default value: TRUE@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model" 'gtk-cell-renderer-combo) 't)
 "The @code{\"model\"} property of type @class{gtk-tree-model}
  (Read / Write)@r{}
  Holds a tree model containing the possible values for the combo box. Use the
  text_column property to specify the column holding the values.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model" 'gtk-cell-renderer-combo) 't)
 "The @code{\"text-column\"} property of type @code{gint} (Read / Write)@br{}
  Specifies the model column which holds the possible values for the combo
  box. Note that this refers to the model specified in the model property, not
  the model backing the tree view to which this cell renderer is attached.
  GtkCellRendererCombo automatically adds a text cell renderer for this column
  to its combo box.@br{}
  Allowed values: >= G_MAXULONG@br{}
  Default value: -1@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-renderer-combo-has-entry --------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-combo-has-entry atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-combo-has-entry 'function)
 "@version{2013-2-20}
  @begin{short}
    Accessor of the slot @code{\"has-entry\"} of the
    @class{gtk-cell-renderer-combo} class.
  @end{short}")

;;; --- gtk-cell-renderer-combo-model ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-combo-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-combo-model 'function)
 "@version{2013-2-20}
  @begin{short}
    Accessor of the slot @code{\"model\"} of the
    @class{gtk-cell-renderer-combo} class.
  @end{short}")

;;; --- gtk-cell-renderer-combo-text-column ------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-combo-text-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-combo-text-column 'function)
 "@version{2013-2-20}
  @begin{short}
    Accessor of the slot @code{\"text-column\"} of the
    @class{gtk-cell-renderer-combo} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_combo_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-combo-new))

(defun gtk-cell-renderer-combo-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-2-20}
  @return{the new cell renderer}
  @begin{short}
    Creates a new GtkCellRendererCombo.
  @end{short}
  Adjust how text is drawn using object properties. Object properties can be set
  globally (with g_object_set()). Also, with GtkTreeViewColumn, you can bind a
  property to a value in a GtkTreeModel. For example, you can bind the \"text\"
  property on the cell renderer to a string value in the model, thus rendering
  a different string in each row of the GtkTreeView.

  Since 2.6"
  (make-instance 'gtk-cell-renderer-combo))

(export 'gtk-cell-renderer-combo-new)

;;; --- End of file gtk.cell-renderer-combo.lisp -------------------------------
