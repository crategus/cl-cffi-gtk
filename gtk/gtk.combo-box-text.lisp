;;; ----------------------------------------------------------------------------
;;; gtk.combo-box-text.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkComboBoxText
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkComboBoxText" gtk-combo-box-text
  (:superclass gtk-combo-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkCellEditable"
                "GtkCellLayout")
   :type-initializer "gtk_combo_box_text_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-combo-box-text 'type)
 "@version{2013-2-26}
  @begin{short}
    A GtkComboBoxText is a simple variant of GtkComboBox that hides the
    model-view complexity for simple text-only use cases.
  @end{short}

  To create a GtkComboBoxText, use gtk_combo_box_text_new() or
  gtk_combo_box_text_new_with_entry().

  You can add items to a GtkComboBoxText with
  gtk_combo_box_text_append_text(), gtk_combo_box_text_insert_text() or
  gtk_combo_box_text_prepend_text() and remove options with
  gtk_combo_box_text_remove().

  If the GtkComboBoxText contains an entry (via the 'has-entry' property), its
  contents can be retrieved using gtk_combo_box_text_get_active_text(). The
  entry itself can be accessed by calling gtk_bin_get_child() on the combo
  box.

  You should not call gtk_combo_box_set_model() or attempt to pack more cells
  into this combo box via its GtkCellLayout interface.
 
  GtkComboBoxText as GtkBuildable

  The GtkComboBoxText implementation of the GtkBuildable interface supports
  adding items directly using the <items> element and specifying <item>
  elements for each item. Each <item> element supports the regular translation
  attributes \"translatable\", \"context\" and \"comments\".

  Example 74. A UI definition fragment specifying GtkComboBoxText items
  @begin{pre}
 <object class=\"GtkComboBoxText\">
   <items>
     <item translatable=\"yes\">Factory</item>
     <item translatable=\"yes\">Home</item>
     <item translatable=\"yes\">Subway</item>
   </items>
 </object>
  @end{pre}")

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-text-new))

(defun gtk-combo-box-text-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-2-26}
  @return{A new GtkComboBoxText}
  @begin{short}
    Creates a new GtkComboBoxText, which is a GtkComboBox just displaying
    strings.
  @end{short}

  Since 2.24"
  (make-instance 'gtk-combo-box-text))

(export 'gtk-combo-box-text-new)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_new_with_entry ()
;;; ----------------------------------------------------------------------------

;; TODO: A ComboBoxText with an entry seems not to work as expected.

(defun gtk-combo-box-text-with-entry ()
 #+cl-cffi-gtk-documentation
 "@version{2013-2-26}
  @return{a new GtkComboBoxText}
  @begin{short}
    Creates a new GtkComboBoxText, which is a GtkComboBox just displaying
    strings. The combo box created by this function has an entry.
  @end{short}

  Since 2.24"
  (make-instance 'gtk-combo-box-text
                 :has-entry t))

(export 'gtk-combo-box-text-with-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_append ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_append" gtk-combo-box-text-append) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-26}
  @argument[combo-box]{A GtkComboBoxText}
  @argument[id]{a string ID for this value, or NULL}
  @argument[text]{A string}
  @begin{short}
    Appends text to the list of strings stored in combo_box. If id is non-NULL
    then it is used as the ID of the row.
  @end{short}

  This is the same as calling gtk_combo_box_text_insert() with a position of
  -1.

  Since 2.24"
  (combo-box (g-object gtk-combo-box-text))
  (id :string)
  (text :string))

(export 'gtk-combo-box-text-append)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_prepend" gtk-combo-box-text-prepend) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-26}
  @argument[combo-box]{A GtkComboBox}
  @argument[id]{a string ID for this value, or NULL}
  @argument[text]{a string}
  @begin{short}
    Prepends text to the list of strings stored in combo_box. If id is non-NULL
    then it is used as the ID of the row.
  @end{short}

  This is the same as calling gtk_combo_box_text_insert() with a position of 0.

  Since 2.24"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_append_text" gtk-combo-box-text-append-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-26}
  @argument[combo-box]{A GtkComboBoxText}
  @argument[text]{A string}
  @begin{short}
    Appends text to the list of strings stored in combo_box.
  @end{short}

  This is the same as calling gtk_combo_box_text_insert_text() with a position
  of -1.

  Since 2.24"
  (combo-box (g-object gtk-combo-box-text))
  (text :string))
  
(export 'gtk-combo-box-text-append-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_prepend_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_prepend_text" gtk-combo-box-text-prepend-text)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-26}
  @argument[combo-box]{A GtkComboBox}
  @argument[text]{A string}
  @begin{short}
    Prepends text to the list of strings stored in combo_box.
  @end{short}

  This is the same as calling gtk_combo_box_text_insert_text() with a position
  of 0.

  Since 2.24"
  (combo-box (g-object gtk-combo-box-text))
  (text :string))
  
(export 'gtk-combo-box-text-prepend-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_insert_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_insert_text" gtk-combo-box-text-insert-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-26}
  @argument[combo-box]{A GtkComboBoxText}
  @argument[position]{An index to insert text}
  @argument[text]{A string}
  @begin{short}
    Inserts text at position in the list of strings stored in combo_box.
  @end{short}

  If position is negative then text is appended.

  This is the same as calling gtk_combo_box_text_insert() with a NULL ID
  string.

  Since 2.24"
  (combo-box (g-object gtk-combo-box-text))
  (position :int)
  (text :string))
  
(export 'gtk-combo-box-text-insert-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_remove" gtk-combo-box-text-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-26}
  @argument[combo-box]{A GtkComboBox}
  @argument[position]{Index of the item to remove}
  @begin{short}
    Removes the string at position from combo_box.
  @end{short}

  Since 2.24"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_get_active_text"
           gtk-combo-box-text-get-active-text) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-2-26}
  @argument[combo-box]{A GtkComboBoxText}
  @begin{return}
    a newly allocated string containing the currently active text. Must be
    freed with g_free().
  @end{return}
  @begin{short}
    Returns the currently active string in combo_box, or NULL if none is
    selected.
  @end{short}
  If combo_box contains an entry, this function will return its
  contents (which will not necessarily be an item from the list).

  Since 2.24"
  (combo-box (g-object gtk-combo-box-text)))
  
(export 'gtk-combo-box-text-get-active-text)

;;; --- End of file gtk.combo-box-text.lisp ------------------------------------
