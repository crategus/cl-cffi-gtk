;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-combo.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkCellRendererCombo
;;; 
;;; Renders a combobox in a cell
;;; 
;;; Synopsis
;;; 
;;;     GtkCellRendererCombo
;;;
;;;     gtk_cell_renderer_combo_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkCellRenderer
;;;                +----GtkCellRendererText
;;;                      +----GtkCellRendererCombo
;;; 
;;; Properties
;;; 
;;;   "has-entry"                gboolean              : Read / Write
;;;   "model"                    GtkTreeModel*         : Read / Write
;;;   "text-column"              gint                  : Read / Write
;;; 
;;; Signals
;;; 
;;;   "changed"                                        : Run Last
;;; 
;;; Description
;;; 
;;; GtkCellRendererCombo renders text in a cell like GtkCellRendererText from
;;; which it is derived. But while GtkCellRendererText offers a simple entry to
;;; edit the text, GtkCellRendererCombo offers a GtkComboBox or GtkComboBoxEntry
;;; widget to edit the text. The values to display in the combo box are taken
;;; from the tree model specified in the "model" property.
;;; 
;;; The combo cell renderer takes care of adding a text cell renderer to the
;;; combo box and sets it to display the column specified by its "text-column"
;;; property. Further properties of the comnbo box can be set in a handler for
;;; the "editing-started" signal.
;;; 
;;; The GtkCellRendererCombo cell renderer was added in GTK+ 2.6.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-entry" property
;;; 
;;;   "has-entry"                gboolean              : Read / Write
;;; 
;;; If TRUE, the cell renderer will include an entry and allow to enter values
;;; other than the ones in the popup list.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "model" property
;;; 
;;;   "model"                    GtkTreeModel*         : Read / Write
;;; 
;;; Holds a tree model containing the possible values for the combo box. Use
;;; the text_column property to specify the column holding the values.
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "text-column" property
;;; 
;;;   "text-column"              gint                  : Read / Write
;;; 
;;; Specifies the model column which holds the possible values for the combo
;;; box.
;;; 
;;; Note that this refers to the model specified in the model property, not the
;;; model backing the tree view to which this cell renderer is attached.
;;; 
;;; GtkCellRendererCombo automatically adds a text cell renderer for this
;;; column to its combo box.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "changed" signal
;;; 
;;; void user_function (GtkCellRendererCombo *combo,
;;;                     gchar                *path_string,
;;;                     GtkTreeIter          *new_iter,
;;;                     gpointer              user_data)        : Run Last
;;; 
;;; This signal is emitted each time after the user selected an item in the
;;; combo box, either by using the mouse or the arrow keys. Contrary to
;;; GtkComboBox, GtkCellRendererCombo::changed is not emitted for changes made
;;; to a selected item in the entry. The argument new_iter corresponds to the
;;; newly selected item in the combo box and it is relative to the GtkTreeModel
;;; set via the model property on GtkCellRendererCombo.
;;; 
;;; Note that as soon as you change the model displayed in the tree view, the
;;; tree view will immediately cease the editing operating. This means that you
;;; most probably want to refrain from changing the model until the combo cell
;;; renderer emits the edited or editing_canceled signal.
;;; 
;;; combo :
;;;     the object on which the signal is emitted
;;; 
;;; path_string :
;;;     a string of the path identifying the edited cell (relative to the tree
;;;     view model)
;;; 
;;; new_iter :
;;;     the new iter selected in the combo box (relative to the combo box model)
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererCombo
;;; 
;;; struct GtkCellRendererCombo;
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
;;; gtk_cell_renderer_combo_new ()
;;; 
;;; GtkCellRenderer * gtk_cell_renderer_combo_new (void);
;;; 
;;; Creates a new GtkCellRendererCombo. Adjust how text is drawn using object
;;; properties. Object properties can be set globally (with g_object_set()).
;;; Also, with GtkTreeViewColumn, you can bind a property to a value in a
;;; GtkTreeModel. For example, you can bind the "text" property on the cell
;;; renderer to a string value in the model, thus rendering a different string
;;; in each row of the GtkTreeView.
;;; 
;;; Returns :
;;;     the new cell renderer
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-combo-new ()
  (make-instance 'gtk-cell-renderer-combo))

(export 'gtk-cell-renderer-combo-new)

;;; --- End of file gtk.cell-renderer-combo.lisp -------------------------------
