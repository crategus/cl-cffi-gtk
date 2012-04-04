;;; ----------------------------------------------------------------------------
;;; gtk.combo-box-text.lisp
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
;;; GtkComboBoxText
;;;
;;; A simple, text-only combo box
;;;	
;;; Synopsis
;;; 
;;;     GtkComboBoxText
;;;
;;;     gtk_combo_box_text_new
;;;     gtk_combo_box_text_new_with_entry
;;;     gtk_combo_box_text_append
;;;     gtk_combo_box_text_prepend
;;;     gtk_combo_box_text_insert
;;;     gtk_combo_box_text_append_text
;;;     gtk_combo_box_text_prepend_text
;;;     gtk_combo_box_text_insert_text
;;;     gtk_combo_box_text_remove
;;;     gtk_combo_box_text_remove_all
;;;     gtk_combo_box_text_get_active_text
;;;
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkComboBox
;;;                                  +----GtkComboBoxText
;;;
;;; Implemented Interfaces
;;; 
;;; GtkComboBoxText implements AtkImplementorIface, GtkBuildable, GtkCellLayout
;;; and GtkCellEditable.
;;;
;;; Description
;;;
;;; A GtkComboBoxText is a simple variant of GtkComboBox that hides the
;;; model-view complexity for simple text-only use cases.
;;;
;;; To create a GtkComboBoxText, use gtk_combo_box_text_new() or
;;; gtk_combo_box_text_new_with_entry().
;;;
;;; You can add items to a GtkComboBoxText with
;;; gtk_combo_box_text_append_text(), gtk_combo_box_text_insert_text() or
;;; gtk_combo_box_text_prepend_text() and remove options with
;;; gtk_combo_box_text_remove().
;;;
;;; If the GtkComboBoxText contains an entry (via the 'has-entry' property),
;;; its contents can be retrieved using gtk_combo_box_text_get_active_text().
;;; The entry itself can be accessed by calling gtk_bin_get_child() on the
;;; combo box.
;;;
;;; GtkComboBoxText as GtkBuildable
;;;
;;; The GtkComboBoxText implementation of the GtkBuildable interface supports
;;; adding items directly using the <items> element and specifying <item>
;;; elements for each item. Each <item> element supports the regular translation
;;; attributes "translatable", "context" and "comments".
;;;
;;; Example 74. A UI definition fragment specifying GtkComboBoxText items
;;;
;;; <object class="GtkComboBoxText">
;;;   <items>
;;;     <item translatable="yes">Factory</item>
;;;     <item translatable="yes">Home</item>
;;;     <item translatable="yes">Subway</item>
;;;   </items>
;;; </object>
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkComboBoxText
;;;
;;; struct GtkComboBoxText;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkComboBoxText" gtk-combo-box-text
  (:superclass gtk-combo-box
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                "GtkCellLayout")
   :type-initializer "gtk_combo_box_text_get_type")
  nil)
          
;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_new ()
;;;
;;; GtkWidget * gtk_combo_box_text_new
;;;
;;; Creates a new GtkComboBoxText, which is a GtkComboBox just displaying
;;; strings. See gtk_combo_box_entry_new_with_text().
;;;
;;; Returns :
;;;     A new GtkComboBoxText
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defun gtk-combo-box-text-new ()
  (make-instance 'gtk-combo-box-text))

(export 'gtk-combo-box-text-new)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_new_with_entry ()
;;;
;;; GtkWidget * gtk_combo_box_text_new_with_entry (void);
;;;
;;; Creates a new GtkComboBoxText, which is a GtkComboBox just displaying
;;; strings. The combo box created by this function has an entry.
;;;
;;; Returns :
;;;     a new GtkComboBoxText
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;; TODO: A ComboBoxText with an entry seems not to work as expected.

(defun gtk-combo-box-text-with-entry ()
  (make-instance 'gtk-combo-box-text
                 :has-entry t))

(export 'gtk-combo-box-text-with-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_append ()
;;;
;;; void gtk_combo_box_text_append (GtkComboBoxText *combo_box,
;;;                                 const gchar *id,
;;;                                 const gchar *text);
;;;
;;; Appends text to the list of strings stored in combo_box. If id is non-NULL
;;; then it is used as the ID of the row.
;;;
;;; This is the same as calling gtk_combo_box_text_insert() with a position
;;; of -1.
;;;
;;; combo_box :
;;;     A GtkComboBoxText
;;;
;;; id :
;;;     a string ID for this value, or NULL
;;;
;;; text :
;;;     A string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;; TODO: gtk_combo_box_text_append is not available on Ubuntu with GTK+ 2.24.

(defcfun ("gtk_combo_box_text_append" gtk-combo-box-text-append) :void
  (combo-box (g-object gtk-combo-box-text))
  (id :string)
  (text :string))
  
(export 'gtk-combo-box-text-append)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_prepend ()
;;;
;;; void gtk_combo_box_text_prepend (GtkComboBoxText *combo_box,
;;;                                  const gchar *id,
;;;                                  const gchar *text);
;;;
;;; Prepends text to the list of strings stored in combo_box. If id is non-NULL
;;; then it is used as the ID of the row.
;;;
;;; This is the same as calling gtk_combo_box_text_insert() with a position
;;; of 0.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; id :
;;;     a string ID for this value, or NULL
;;;
;;; text :
;;;     a string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;; TODO: gtk_combo_box_text_prepend is not available on Ubuntu with GTK+ 2.24.

(defcfun ("gtk_combo_box_text_prepend" gtk-combo-box-text-prepend) :void
  (combo-box (g-object gtk-combo-box-text))
  (id :string)
  (text :string))
  
(export 'gtk-combo-box-text-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_insert ()
;;;
;;; void gtk_combo_box_text_insert (GtkComboBoxText *combo_box,
;;;                                 gint position,
;;;                                 const gchar *id,
;;;                                 const gchar *text);
;;;
;;; Inserts text at position in the list of strings stored in combo_box. If id
;;; is non-NULL then it is used as the ID of the row. See "id-column".
;;;
;;; If position is negative then text is appended.
;;;
;;; combo_box :
;;;     A GtkComboBoxText
;;;
;;; position :
;;;     An index to insert text
;;;
;;; id :
;;;     a string ID for this value, or NULL
;;;
;;; text :
;;;     A string to display
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_append_text ()
;;;
;;; void gtk_combo_box_text_append_text (GtkComboBoxText *combo_box,
;;;                                      const gchar *text);
;;;
;;; Appends text to the list of strings stored in combo_box.
;;;
;;; This is the same as calling gtk_combo_box_text_insert_text() with a
;;; position of -1.
;;;
;;; combo_box :
;;;     A GtkComboBoxText
;;;
;;; text :
;;;     A string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_append_text" gtk-combo-box-text-append-text) :void
  (combo-box (g-object gtk-combo-box-text))
  (text :string))
  
(export 'gtk-combo-box-text-append-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_prepend_text ()
;;;
;;; void gtk_combo_box_text_prepend_text (GtkComboBoxText *combo_box,
;;;                                       const gchar *text);
;;;
;;; Prepends text to the list of strings stored in combo_box.
;;;
;;; This is the same as calling gtk_combo_box_text_insert_text() with a
;;; position of 0.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; text :
;;;     A string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_prepend_text" gtk-combo-box-text-prepend-text)
    :void
  (combo-box (g-object gtk-combo-box-text))
  (text :string))
  
(export 'gtk-combo-box-text-prepend-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_insert_text ()
;;;
;;; void gtk_combo_box_text_insert_text (GtkComboBoxText *combo_box,
;;;                                      gint position,
;;;                                      const gchar *text);
;;;
;;; Inserts text at position in the list of strings stored in combo_box.
;;;
;;; If position is negative then text is appended.
;;;
;;; This is the same as calling gtk_combo_box_text_insert() with a NULL
;;; ID string.
;;;
;;; combo_box :
;;;     A GtkComboBoxText
;;;
;;; position :
;;;     An index to insert text
;;;
;;; text :
;;;     A string
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_insert_text" gtk-combo-box-text-insert-text) :void
  (combo-box (g-object gtk-combo-box-text))
  (position :int)
  (text :string))
  
(export 'gtk-combo-box-text-insert-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_remove ()
;;;
;;; void gtk_combo_box_text_remove (GtkComboBoxText *combo_box,
;;;                                 gint position);
;;;
;;; Removes the string at position from combo_box.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; position :
;;;     index of the item to remove
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_remove" gtk-combo-box-text-remove) :void
  (combo-box (g-object gtk-combo-box-text))
  (position :int))

(export 'gtk-combo-box-text-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_remove_all ()
;;;
;;; void gtk_combo_box_text_remove_all (GtkComboBoxText *combo_box);
;;;
;;; Removes all the text entries from the combo box.
;;; 
;;; combo_box :
;;;     A GtkComboBoxText
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_get_active_text ()
;;;
;;; gchar * gtk_combo_box_text_get_active_text (GtkComboBoxText *combo_box);
;;;
;;; Returns the currently active string in combo_box, or NULL if none is
;;; selected. If combo_box contains an entry, this function will return its
;;; contents (which will not necessarily be an item from the list).
;;;
;;; combo_box :
;;;     a GtkComboBoxText
;;;
;;; Returns :
;;;     a newly allocated string containing the currently active text. Must be
;;;     freed with g_free().
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_get_active_text"
           gtk-combo-box-text-get-active-text) :string
  (combo-box g-object))
  
(export 'gtk-combo-box-text-get-active-text)

;;; --- End of file gtk.combo-box-text.lisp ------------------------------------
