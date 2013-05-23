;;; ----------------------------------------------------------------------------
;;; gtk.cell-editable.lisp
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellEditable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkCellEditable" gtk-cell-editable
  (:export t
   :type-initializer "gtk_cell_editable_get_type")
  (editing-canceled
   gtk-cell-editable-editing-canceld
   "editing-canceled" "gboolean" t t))

;;; ----------------------------------------------------------------------------

(setf (documentation 'gtk-cell-editable 'type)
 "@version{2013-2-18}
  @begin{short}
    The GtkCellEditable interface must be implemented for widgets to be usable
    when editing the contents of a GtkTreeView cell.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"editing-done\" signal}
      This signal is a sign for the cell renderer to update its value from the
      cell_editable.
      Implementations of GtkCellEditable are responsible for emitting this signal
      when they are done editing, e.g. GtkEntry is emitting it when the user
      presses Enter.
      gtk_cell_editable_editing_done() is a convenience method for emitting
      \"editing-done\".
      @begin{pre}
 void user_function (GtkCellEditable *cell_editable,
                     gpointer         user_data)          : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[cell_editable]{the object on which the signal was emitted}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}

    @subheading{The \"remove-widget\" signal}
      @begin{pre}
 void user_function (GtkCellEditable *cell_editable,
                     gpointer         user_data)          : Run Last
      @end{pre}
      This signal is meant to indicate that the cell is finished editing, and
      the widget may now be destroyed.
      Implementations of GtkCellEditable are responsible for emitting this
      signal when they are done editing. It must be emitted after the
      \"editing-done\" signal, to give the cell renderer a chance to update the
      cell's value before the widget is removed.
      gtk_cell_editable_remove_widget() is a convenience method for emitting
      \"remove-widget\".
      @begin[code]{table}
        @entry[cell_editable]{the object on which the signal was emitted}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-cell-editable-editing-canceled}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editing-canceled"
                                               'gtk-cell-editable) 't)
 "The @code{\"editing-canceled\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Indicates whether editing on the cell has been canceled.@br{}
  Default value: @code{nil}@br{}
  Since 2.20")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-editable-editing-canceled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-editable-editing-canceled 'function)
 "@version{2013-5-22}
  Accessor of the slot @code{\"editing-canceled\"} of the
  @class{gtk-cell-editable} class.")

;;; ----------------------------------------------------------------------------
;;; struct GtkCellEditableIface
;;;
;;; struct GtkCellEditableIface {
;;;   GTypeInterface g_iface;
;;;
;;;   /* signals */
;;;   void (* editing_done)  (GtkCellEditable *cell_editable);
;;;   void (* remove_widget) (GtkCellEditable *cell_editable);
;;;
;;;   /* virtual table */
;;;   void (* start_editing) (GtkCellEditable *cell_editable,
;;;                           GdkEvent        *event);
;;; };
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_start_editing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_start_editing" gtk-cell-editable-start-editing)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-19}
  @argument[cell-editable]{A GtkCellEditable}
  @argument[event]{A GdkEvent, or NULL.}
  @begin{short}
    Begins editing on a cell_editable. event is the GdkEvent that began the
    editing process. It may be NULL, in the instance that editing was initiated
    through programatic means.
  @end{short}"
  (cell-editable (g-object gtk-cell-editable))
    (event (g-boxed-foreign gdk-event)))

(export 'gtk-cell-editable-start-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_editing_done ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_editing_done" gtk-cell-editable-editing-done) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-19}
  @argument[cell_editable]{A GtkTreeEditable}
  @short{Emits the \"editing-done\" signal.}"
  (cell-editable (g-object gtk-cell-editable)))

(export 'gtk-cell-editable-editing-done)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_remove_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_remove_widget" gtk-cell-editable-remove-widget)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-2-19}
  @argument[cell_editable]{A GtkTreeEditable}
  @short{Emits the \"remove-widget\" signal.}"
  (cell-editable (g-object gtk-cell-editable)))

(export 'gtk-cell-editable-remove-widget)

;;; --- End of file gtk.cell-editable.lisp -------------------------------------
