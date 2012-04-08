;;; ----------------------------------------------------------------------------
;;; gtk.cell-editable.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
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
;;; GtkCellEditable
;;; 
;;; Interface for widgets which can are used for editing cells
;;; 	
;;; Synopsis
;;; 
;;;     GtkCellEditable
;;;     GtkCellEditableIface
;;;
;;;     gtk_cell_editable_start_editing
;;;     gtk_cell_editable_editing_done
;;;     gtk_cell_editable_remove_widget
;;; 
;;; Object Hierarchy
;;; 
;;;   GInterface
;;;    +----GtkCellEditable
;;; 
;;; Prerequisites
;;; 
;;; GtkCellEditable requires GtkWidget.
;;; Known Implementations
;;; 
;;; GtkCellEditable is implemented by GtkAppChooserButton, GtkComboBox,
;;; GtkComboBoxText, GtkEntry and GtkSpinButton.
;;;
;;; Properties
;;; 
;;;   "editing-canceled"         gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "editing-done"                                   : Run Last
;;;   "remove-widget"                                  : Run Last
;;; 
;;; Description
;;; 
;;; The GtkCellEditable interface must be implemented for widgets to be usable
;;; when editing the contents of a GtkTreeView cell.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "editing-canceled" property
;;; 
;;;   "editing-canceled"         gboolean              : Read / Write
;;; 
;;; Indicates whether editing on the cell has been canceled.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.20
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "editing-done" signal
;;; 
;;; void user_function (GtkCellEditable *cell_editable,
;;;                     gpointer         user_data)          : Run Last
;;; 
;;; This signal is a sign for the cell renderer to update its value from the
;;; cell_editable.
;;; 
;;; Implementations of GtkCellEditable are responsible for emitting this signal
;;; when they are done editing, e.g. GtkEntry is emitting it when the user
;;; presses Enter.
;;; 
;;; gtk_cell_editable_editing_done() is a convenience method for emitting
;;; "editing-done".
;;; 
;;; cell_editable :
;;; 	the object on which the signal was emitted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "remove-widget" signal
;;; 
;;; void user_function (GtkCellEditable *cell_editable,
;;;                     gpointer         user_data)          : Run Last
;;; 
;;; This signal is meant to indicate that the cell is finished editing, and the
;;; widget may now be destroyed.
;;; 
;;; Implementations of GtkCellEditable are responsible for emitting this signal
;;; when they are done editing. It must be emitted after the "editing-done"
;;; signal, to give the cell renderer a chance to update the cell's value
;;; before the widget is removed.
;;; 
;;; gtk_cell_editable_remove_widget() is a convenience method for emitting
;;; "remove-widget".
;;; 
;;; cell_editable :
;;; 	the object on which the signal was emitted
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellEditable
;;; 
;;; typedef struct _GtkCellEditable GtkCellEditable;
;;; 
;;; struct GtkCellEditableIface
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkCellEditable" gtk-cell-editable
  (:export t
   :type-initializer "gtk_cell_editable_get_type"))

;;; ----------------------------------------------------------------------------
;;; struct GtkCellEditableIface {
;;;   GTypeInterface g_iface;
;;; 
;;;   /* signals */
;;;   void (* editing_done)  (GtkCellEditable *cell_editable);
;;;   void (* remove_widget) (GtkCellEditable *cell_editable);
;;; 
;;;   /* virtual table */
;;;   void (* start_editing) (GtkCellEditable *cell_editable,
;;; 			  GdkEvent        *event);
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_start_editing ()
;;; 
;;; void gtk_cell_editable_start_editing (GtkCellEditable *cell_editable,
;;;                                       GdkEvent *event);
;;; 
;;; Begins editing on a cell_editable. event is the GdkEvent that began the
;;; editing process. It may be NULL, in the instance that editing was initiated
;;; through programatic means.
;;; 
;;; cell_editable :
;;; 	A GtkCellEditable
;;; 
;;; event :
;;; 	A GdkEvent, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_editing_done ()
;;; 
;;; void gtk_cell_editable_editing_done (GtkCellEditable *cell_editable);
;;; 
;;; Emits the "editing-done" signal.
;;; 
;;; cell_editable :
;;; 	A GtkTreeEditable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_remove_widget ()
;;; 
;;; void gtk_cell_editable_remove_widget (GtkCellEditable *cell_editable);
;;; 
;;; Emits the "remove-widget" signal.
;;; 
;;; cell_editable :
;;; 	A GtkTreeEditable
;;; ----------------------------------------------------------------------------


;;; --- gtk.cell-editable.lisp -------------------------------------------------
