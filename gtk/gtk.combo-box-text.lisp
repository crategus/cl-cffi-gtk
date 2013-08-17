;;; ----------------------------------------------------------------------------
;;; gtk.combo-box-text.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-combo-box-text 'type)
 "@version{2013-8-15}
  @begin{short}
    A @sym{gtk-combo-box-text} is a simple variant of @class{gtk-combo-box} that
    hides the model-view complexity for simple text-only use cases.
  @end{short}

  To create a @sym{gtk-combo-box-text}, use the functions
  @fun{gtk-combo-box-text-new} or @fun{gtk-combo-box-text-new-with-entry}.

  You can add items to a @sym{gtk-combo-box-text} with the functions
  @fun{gtk-combo-box-text-append-text}, @fun{gtk-combo-box-text-insert-text} or
  @fun{gtk-combo-box-text-prepend-text} and remove options with the function
  @fun{gtk-combo-box-text-remove}.

  If the @sym{gtk-combo-box-text} contains an entry via the
  @code{\"has-entry\"} property, its contents can be retrieved using
  the function @fun{gtk-combo-box-text-get-active-text}. The entry itself can be
  accessed by calling the function @fun{gtk-bin-get-child} on the combo box.

  You should not call the function @fun{gtk-combo-box-set-model} or attempt to
  pack more cells into this combo box via its @class{gtk-cell-layout} interface.

  @subheading{GtkComboBoxText as GtkBuildable}
    The @sym{gtk-combo-box-text} implementation of the @class{gtk-buildable}
    interface supports adding items directly using the <items> element and
    specifying <item> elements for each item. Each <item> element supports the
    regular translation attributes \"translatable\", \"context\" and
    \"comments\".

    @b{Example:} A UI definition fragment specifying @sym{gtk-combo-box-text}
    items
  @begin{pre}
 <object class=\"GtkComboBoxText\">
   <items>
     <item translatable=\"yes\">Factory</item>
     <item translatable=\"yes\">Home</item>
     <item translatable=\"yes\">Subway</item>
   </items>
 </object>
  @end{pre}
  @see-function{gtk-combo-box-text-new}
  @see-function{gtk-combo-box-text-new-with-entry}
  @see-function{gtk-combo-box-text-append-text}
  @see-function{gtk-combo-box-text-insert-text}
  @see-function{gtk-combo-box-text-prepend-text}
  @see-function{gtk-combo-box-text-remove}
  @see-function{gtk-combo-box-text-get-active-text}
  @see-function{gtk-combo-box-set-model}
  @see-function{gtk-bin-get-child}")

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-text-new))

(defun gtk-combo-box-text-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @return{A new @class{gtk-combo-box-text} widget.}
  @begin{short}
    Creates a new @class{gtk-combo-box-text} widget, which is a
    @class{gtk-combo-box} just displaying strings.
  @end{short}

  Since 2.24
  @see-class{gtk-combo-box-text}
  @see-function{gtk-combo-box-text-new-with-entry}"
  (make-instance 'gtk-combo-box-text))

(export 'gtk-combo-box-text-new)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_new_with_entry ()
;;; ----------------------------------------------------------------------------

;; TODO: A ComboBoxText with an entry seems not to work as expected.

(defun gtk-combo-box-text-new-with-entry ()
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @return{A new @class{gtk-combo-box-text} widget.}
  @begin{short}
    Creates a new @class{gtk-combo-box-text} widget, which is a
    @class{gtk-combo-box} just displaying strings. The combo box created by
    this function has an entry.
  @end{short}

  Since 2.24
  @see-class{gtk-combo-box-text}
  @see-function{gtk-combo-text-new}"
  (make-instance 'gtk-combo-box-text
                 :has-entry t))

(export 'gtk-combo-box-text-new-with-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_append ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_append" %gtk-combo-box-text-append) :void
  (combo-box (g-object gtk-combo-box-text))
  (id :string)
  (text :string))

(defun gtk-combo-box-text-append (combo-box id text)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @argument[combo-box]{a @class{gtk-combo-box-text} widget}
  @argument[id]{a string ID for this value, or @code{nil}}
  @argument[text]{a string}
  @begin{short}
    Appends @arg{text} to the list of strings stored in @arg{combo-box}. If ID
    is non-@code{nil} then it is used as the ID of the row.
  @end{short}

  This is the same as calling the function @fun{gtk-combo-box-text-insert} with
  a position of -1.

  Since 2.24
  @see-class{gtk-combo-box-text}
  @see-function{gtk-combo-box-text-insert}
  @see-function{gtk-combo-box-text-prepend}"
  (%gtk-combo-box-text-append combo-box
                              (if id id (null-pointer))
                              text))

(export 'gtk-combo-box-text-append)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_prepend" %gtk-combo-box-text-prepend) :void
  (combo-box (g-object gtk-combo-box-text))
  (id :string)
  (text :string))

(defun gtk-combo-box-text-prepend (combo-box id text)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[id]{a string ID for this value, or @code{nil}}
  @argument[text]{a string}
  @begin{short}
    Prepends text to the list of strings stored in @arg{combo-box}. If ID is
    non-@code{nil} then it is used as the ID of the row.
  @end{short}

  This is the same as calling the function @fun{gtk-combo-box-text-insert} with
  a position of 0.

  Since 2.24
  @see-class{gtk-combo-box-text}
  @see-function{gtk-combo-box-text-insert}"
  (%gtk-combo-box-text-prepend combo-box
                               (if id id (null-pointer))
                               text))

(export 'gtk-combo-box-text-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_insert" %gtk-combo-box-text-insert) :void
  (combo-box (g-object gtk-combo-box-text))
  (position :int)
  (id :string)
  (text :string))

(defun gtk-combo-box-text-insert (combo-box position id text)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @argument[combo-box]{a @class{gtk-combo-box-text} widget}
  @argument[position]{an index to insert text}
  @argument[id]{a string ID for this value, or @code{nil}}
  @argument[text]{a string to display}
  @begin{short}
    Inserts @arg{text} at @arg{position} in the list of strings stored in
    @arg{combo-box}. If @arg{id} is non-@code{nil} then it is used as the ID of
    the row.
  @end{short}
  See the property @code{\"id-column\"} of the @class{gtk-combo-box}.

  If @arg{position} is negative then @arg{text} is appended.

  Since 3.0
  @see-class{gtk-combo-box-text}"
  (%gtk-combo-box-text-insert combo-box
                              position
                              (if id id (null-pointer))
                              text))

(export 'gtk-combo-box-text-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_append_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_append_text" gtk-combo-box-text-append-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @argument[combo-box]{a @class{gtk-combo-box-text} widget}
  @argument[text]{a string}
  @begin{short}
    Appends @arg{text} to the list of strings stored in @arg{combo-box}.
  @end{short}

  This is the same as calling the function @fun{gtk-combo-box-text-insert-text}
  with a position of -1.

  Since 2.24
  @see-class{gtk-combo-box-text}
  @see-function{gtk-combo-box-text-insert-text}
  @see-function{gtk-combo-box-text-prepend}"
  (combo-box (g-object gtk-combo-box-text))
  (text :string))

(export 'gtk-combo-box-text-append-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_prepend_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_prepend_text" gtk-combo-box-text-prepend-text)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[text]{a string}
  @begin{short}
    Prepends @arg{text} to the list of strings stored in @arg{combo-box}.
  @end{short}

  This is the same as calling the function @fun{gtk-combo-box-text-insert-text}
  with a position of 0.

  Since 2.24
  @see-class{gtk-box-combo-text}
  @see-function{gtk-combo-box-text-insert-text}
  @see-function{gtk-combo-box-text-append-text}"
  (combo-box (g-object gtk-combo-box-text))
  (text :string))

(export 'gtk-combo-box-text-prepend-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_insert_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_insert_text" gtk-combo-box-text-insert-text) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @argument[combo-box]{a @class{gtk-combo-box-text} widget}
  @argument[position]{an index to insert text}
  @argument[text]{a string}
  @begin{short}
    Inserts @arg{text} at @arg{position} in the list of strings stored in
    @arg{combo-box}.
  @end{short}

  If @arg{position} is negative then @arg{text} is appended.

  This is the same as calling the function @fun{gtk-combo-box-text-insert} with
  a @code{nil} ID string.

  Since 2.24
  @see-class{gtk-combo-text}
  @see-function{gtk-combo-box-text-insert}"
  (combo-box (g-object gtk-combo-box-text))
  (position :int)
  (text :string))

(export 'gtk-combo-box-text-insert-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_remove" gtk-combo-box-text-remove) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[position]{index of the item to remove}
  @begin{short}
    Removes the string at position from @arg{combo-box}.
  @end{short}

  Since 2.24
  @see-class{gtk-combo-text}
  @see-function{gtk-combo-text-remove-all}"
  (combo-box (g-object gtk-combo-box-text))
  (position :int))

(export 'gtk-combo-box-text-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_remove_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_text_remove_all" gtk-combo-text-remove-all) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-15}
  @argument[combo-box]{a @class{gtk-combo-box-text} widget}
  @begin{short}
    Removes all the text entries from the combo box.
  @end{short}

  Since 3.0
  @see-class{gtk-combo-box-text}
  @see-function{gtk-combo-text-remove}"
  (combo-box (g-object gtk-combo-text)))

(export 'gtk-combo-box-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_get_active_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_get_active_text"
           gtk-combo-box-text-get-active-text) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-7-15}
  @argument[combo-box]{a @class{gtk-combo-box-text} widget}
  @begin{return}
    A string containing the currently active text.
  @end{return}
  @begin{short}
    Returns the currently active string in @arg{combo-box}, or @code{nil} if
    none is selected.
  @end{short}
  If @arg{combo-box} contains an entry, this function will return its
  contents which will not necessarily be an item from the list.

  Since 2.24
  @see-class{gtk-combo-text}"
  (combo-box (g-object gtk-combo-box-text)))

(export 'gtk-combo-box-text-get-active-text)

;;; --- End of file gtk.combo-box-text.lisp ------------------------------------
