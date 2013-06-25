;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-combo.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-combo 'type)
 "@version{2013-6-22}
  @begin{short}
    @sym{gtk-cell-renderer-combo} renders text in a cell like
    @class{gtk-cell-renderer-text} from which it is derived.
  @end{short}
  But while @class{gtk-cell-renderer-text} offers a simple entry to edit the
  text, @sym{gtk-cell-renderer-combo} offers a @class{gtk-combo-box} or
  @class{gtk-combo-box-entry} widget to edit the text. The values to display in
  the combo box are taken from the tree model specified in the @code{\"model\"}
  property.

  The combo cell renderer takes care of adding a text cell renderer to the
  combo box and sets it to display the column specified by its
  @code{\"text-column\"} property. Further properties of the comnbo box can be
  set in a handler for the \"editing-started\" signal.

  The @sym{gtk-cell-renderer-combo} cell renderer was added in GTK+ 2.6.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (combo path-string new-iter)   : Run Last
      @end{pre}
      This signal is emitted each time after the user selected an item in the
      combo box, either by using the mouse or the arrow keys. Contrary to
      @class{gtk-combo-box}, the \"changed\" signal is not emitted for changes
      made to a selected item in the entry. The argument @arg{new-iter}
      corresponds to the newly selected item in the combo box and it is relative
      to the @class{gtk-tree-model} set via the model property on
      @sym{gtk-cell-renderer-combo}.
      Note that as soon as you change the model displayed in the tree view, the
      tree view will immediately cease the editing operating. This means that
      you most probably want to refrain from changing the model until the combo
      cell renderer emits the edited or \"editing-canceled\" signal.
      @begin[code]{table}
        @entry[combo]{The object on which the signal is emitted.}
        @entry[path-string]{A string of the path identifying the edited cell
          (relative to the tree view model).}
        @entry[new-iter]{The new iter selected in the combo box (relative to the
          combo box model).}
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
(setf (documentation (atdoc:get-slot-from-name "has-entry"
                                               'gtk-cell-renderer-combo) 't)
 "The @code{\"has-entry\"} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, the cell renderer will include an entry and allow to enter values
  other than the ones in the popup list. @br{}
  Default value: @em{true} @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model"
                                               'gtk-cell-renderer-combo) 't)
 "The @code{\"model\"} property of type @class{gtk-tree-model}
  (Read / Write) @r{}
  Holds a tree model containing the possible values for the combo box. Use the
  @code{\"text-column\"} property to specify the column holding the
  values. @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text-column"
                                               'gtk-cell-renderer-combo) 't)
 "The @code{\"text-column\"} property of type @code{:int} (Read / Write) @br{}
  Specifies the model column which holds the possible values for the combo
  box. Note that this refers to the model specified in the model property, not
  the model backing the tree view to which this cell renderer is attached.
  @sym{gtk-cell-renderer-combo} automatically adds a text cell renderer for this
  column to its combo box. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-combo-has-entry atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-combo-has-entry 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"has-entry\"} of the
  @class{gtk-cell-renderer-combo} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-combo-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-combo-model 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"model\"} of the
  @class{gtk-cell-renderer-combo} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-combo-text-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-combo-text-column 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"text-column\"} of the
  @class{gtk-cell-renderer-combo} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_combo_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-combo-new))

(defun gtk-cell-renderer-combo-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @return{The new cell renderer.}
  @begin{short}
    Creates a new @class{gtk-cell-renderer-combo} object.
  @end{short}
  Adjust how text is drawn using object properties. Object properties can be set
  globally (with the function @fun{g-object-set}). Also, with
  @class{gtk-tree-view-column}, you can bind a property to a value in a
  @class{gtk-tree-model}. For example, you can bind the \"text\" property on the
  cell renderer to a string value in the model, thus rendering a different
  string in each row of the @class{gtk-tree-view}.

  Since 2.6"
  (make-instance 'gtk-cell-renderer-combo))

(export 'gtk-cell-renderer-combo-new)

;;; --- End of file gtk.cell-renderer-combo.lisp -------------------------------
