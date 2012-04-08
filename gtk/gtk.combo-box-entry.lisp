;;; ----------------------------------------------------------------------------
;;; gtk.combo-box-entry.lisp
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
;;; GtkComboBoxEntry
;;; 
;;; A text entry field with a dropdown list
;;; 
;;; Synopsis
;;; 
;;;     GtkComboBoxEntry
;;;
;;;     gtk_combo_box_entry_new
;;;     gtk_combo_box_entry_new_with_model
;;;     gtk_combo_box_entry_new_text
;;;     gtk_combo_box_entry_set_text_column
;;;     gtk_combo_box_entry_get_text_column
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkObject
;;;                +----GtkWidget
;;;                      +----GtkContainer
;;;                            +----GtkBin
;;;                                  +----GtkComboBox
;;;                                        +----GtkComboBoxEntry
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkComboBoxEntry implements AtkImplementorIface, GtkBuildable, GtkCellLayout
;;; and GtkCellEditable.
;;;
;;; Properties
;;; 
;;;   "text-column"              gint                  : Read / Write
;;; 
;;; Description
;;; 
;;; A GtkComboBoxEntry is a widget that allows the user to choose from a list
;;; of valid choices or enter a different value. It is very similar to a
;;; GtkComboBox, but it displays the selected value in an entry to allow
;;; modifying it.
;;; 
;;; In contrast to a GtkComboBox, the underlying model of a GtkComboBoxEntry
;;; must always have a text column (see gtk_combo_box_entry_set_text_column()),
;;; and the entry will show the content of the text column in the selected row.
;;; To get the text from the entry, use gtk_combo_box_get_active_text().
;;; 
;;; The changed signal will be emitted while typing into a GtkComboBoxEntry, as
;;; well as when selecting an item from the GtkComboBoxEntry's list. Use
;;; gtk_combo_box_get_active() or gtk_combo_box_get_active_iter() to discover
;;; whether an item was actually selected from the list.
;;; 
;;; Connect to the activate signal of the GtkEntry (use gtk_bin_get_child()) to
;;; detect when the user actually finishes entering text.
;;; 
;;; The convenience API to construct simple text-only GtkComboBoxes can also be
;;; used with GtkComboBoxEntrys which have been constructed with
;;; gtk_combo_box_entry_new_text().
;;; 
;;; If you have special needs that go beyond a simple entry (e.g. input
;;; validation), it is possible to replace the child entry by a different widget
;;; using gtk_container_remove() and gtk_container_add().
;;;
;;; GtkComboBoxEntry as GtkBuildable
;;; 
;;; Beyond the <attributes> support that is shared by all GtkCellLayout
;;; implementation, GtkComboBoxEntry makes the entry available in UI definitions
;;; as an internal child with name "entry".
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "text-column" property
;;; 
;;;   "text-column"              gint                  : Read / Write
;;; 
;;; A column in the data source model to get the strings from.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkComboBoxEntry
;;; 
;;; struct GtkComboBoxEntry;
;;; 
;;; Warning
;;; 
;;; GtkComboBoxEntry is deprecated and should not be used in newly-written code.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkComboBoxEntry" gtk-combo-box-entry
  (:superclass gtk-combo-box
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                "GtkCellLayout")
   :type-initializer "gtk_combo_box_entry_get_type")
  ((text-column
    gtk-combo-box-entry-text-column
    "text-column" "gint" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_entry_new ()
;;; 
;;; GtkWidget * gtk_combo_box_entry_new (void);
;;; 
;;; Warning
;;; 
;;; gtk_combo_box_entry_new has been deprecated since version 2.24 and should
;;; not be used in newly-written code. Use gtk_combo_box_new_with_entry()
;;; instead
;;; 
;;; Creates a new GtkComboBoxEntry which has a GtkEntry as child. After
;;; construction, you should set a model using gtk_combo_box_set_model() and a
;;; text column using gtk_combo_box_entry_set_text_column().
;;; 
;;; Returns :
;;;     A new GtkComboBoxEntry.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_entry_new_with_model ()
;;; 
;;; GtkWidget * gtk_combo_box_entry_new_with_model (GtkTreeModel *model,
;;;                                                 gint text_column);
;;; 
;;; Warning
;;; 
;;; gtk_combo_box_entry_new_with_model has been deprecated since version 2.24
;;; and should not be used in newly-written code. Use
;;; gtk_combo_box_new_with_model_and_entry() instead
;;; 
;;; Creates a new GtkComboBoxEntry which has a GtkEntry as child and a list of
;;; strings as popup. You can get the GtkEntry from a GtkComboBoxEntry using
;;; GTK_ENTRY (GTK_BIN (combo_box_entry)->child). To add and remove strings from
;;; the list, just modify model using its data manipulation API.
;;; 
;;; model :
;;;     A GtkTreeModel.
;;; 
;;; text_column :
;;;     A column in model to get the strings from.
;;; 
;;; Returns :
;;;     A new GtkComboBoxEntry.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_entry_new_text ()
;;; 
;;; GtkWidget * gtk_combo_box_entry_new_text (void);
;;; 
;;; Warning
;;; 
;;; gtk_combo_box_entry_new_text is deprecated and should not be used in
;;; newly-written code.
;;; 
;;; Convenience function which constructs a new editable text combo box, which
;;; is a GtkComboBoxEntry just displaying strings. If you use this function to
;;; create a text combo box, you should only manipulate its data source with the
;;; following convenience functions: gtk_combo_box_append_text(),
;;; gtk_combo_box_insert_text(), gtk_combo_box_prepend_text() and
;;; gtk_combo_box_remove_text().
;;; 
;;; Returns :
;;;     A new text GtkComboBoxEntry.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_entry_set_text_column ()
;;; 
;;; void gtk_combo_box_entry_set_text_column (GtkComboBoxEntry *entry_box,
;;;                                           gint text_column);
;;; 
;;; Warning
;;; 
;;; gtk_combo_box_entry_set_text_column has been deprecated since version 2.24
;;; and should not be used in newly-written code. Use
;;; gtk_combo_box_set_entry_text_column() instead
;;; 
;;; Sets the model column which entry_box should use to get strings from to be
;;; text_column.
;;; 
;;; entry_box :
;;;     A GtkComboBoxEntry.
;;; 
;;; text_column :
;;;     A column in model to get the strings from.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_entry_get_text_column ()
;;; 
;;; gint gtk_combo_box_entry_get_text_column (GtkComboBoxEntry *entry_box)
;;; 
;;; Warning
;;; 
;;; gtk_combo_box_entry_get_text_column has been deprecated since version 2.24
;;; and should not be used in newly-written code. Use
;;; gtk_combo_box_get_entry_text_column() instead
;;; 
;;; Returns the column which entry_box is using to get the strings from.
;;; 
;;; entry_box :
;;;     A GtkComboBoxEntry.
;;; 
;;; Returns :
;;;     A column in the data source model of entry_box.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.combo-box-entry.lisp -----------------------------------
