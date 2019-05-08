;;; ----------------------------------------------------------------------------
;;; gtk.cell-editable.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;;     Interface for widgets which can are used for editing cells
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
;;; Properties
;;;
;;;     gboolean   editing-canceled    Read / Write
;;;
;;; Signals
;;;
;;;         void   editing-done        Run Last
;;;         void   remove-widget       Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkCellEditable
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-editable atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-cell-editable 'type)
 "@version{2013-6-22}
  @begin{short}
    The @sym{gtk-cell-editable} interface must be implemented for widgets to be
    usable when editing the contents of a @class{gtk-tree-view} cell.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"editing-done\" signal}
      @begin{pre}
 lambda (cell-editable)    : Run Last
      @end{pre}
      This signal is a sign for the cell renderer to update its value from the
      @arg{cell-editable}.
      Implementations of @sym{gtk-cell-editable} are responsible for emitting
      this signal when they are done editing, e. g. @class{gtk-entry} is
      emitting it when the user presses Enter.
      The function @fun{gtk-cell-editable-editing-done} is a convenience method
      for emitting the \"editing-done\" signal.
      @begin[code]{table}
        @entry[cell-editable]{The object on which the signal was emitted.}
      @end{table}
    @subheading{The \"remove-widget\" signal}
      @begin{pre}
 lambda (cell-editable)    : Run Last
      @end{pre}
      This signal is meant to indicate that the cell is finished editing, and
      the widget may now be destroyed.
      Implementations of @sym{gtk-cell-editable} are responsible for emitting
      this signal when they are done editing. It must be emitted after the
      \"editing-done\" signal, to give the cell renderer a chance to update the
      cell's value before the widget is removed.
      The function @fun{gtk-cell-editable-remove-widget} is a convenience method
      for emitting the \"remove-widget\" signal.
      @begin[code]{table}
        @entry[cell-editable]{The object on which the signal was emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-cell-editable-editing-canceled}
  @see-function{gtk-cell-editable-editing-done}
  @see-function{gtk-cell-editable-remove-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "editing-canceled"
                                               'gtk-cell-editable) 't)
 "The @code{editing-canceled} property of type @code{:boolean}
  (Read / Write) @br{}
  Indicates whether editing on the cell has been canceled. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-editable-editing-canceled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-editable-editing-canceled 'function)
 "@version{2013-6-22}
  Accessor of the @slot[gtk-cell-editable]{editing-canceled} slot of the
  @class{gtk-cell-editable} class.
  @see-class{gtk-editable}")

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
 "@version{2013-6-22}
  @argument[cell-editable]{a @class{gtk-cell-editable} object}
  @argument[event]{a @class{gdk-event}, or @code{nil}}
  Begins editing on a @arg{cell-editable}. @arg{event} is the @class{gdk-event}
  that began the editing process. It may be @code{nil}, in the instance that
  editing was initiated through programatic means."
  (cell-editable (g-object gtk-cell-editable))
    (event (g-boxed-foreign gdk-event)))

(export 'gtk-cell-editable-start-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_editing_done ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_editing_done" gtk-cell-editable-editing-done) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @argument[cell-editable]{a @class{gtk-cell-editable} object}
  Emits the \"editing-done\" signal."
  (cell-editable (g-object gtk-cell-editable)))

(export 'gtk-cell-editable-editing-done)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_remove_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_remove_widget" gtk-cell-editable-remove-widget)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @argument[cell-editable]{a @class{gtk-cell-editable} object}
  Emits the \"remove-widget\" signal."
  (cell-editable (g-object gtk-cell-editable)))

(export 'gtk-cell-editable-remove-widget)

;;; --- End of file gtk.cell-editable.lisp -------------------------------------
