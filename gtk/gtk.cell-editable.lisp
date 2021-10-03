;;; ----------------------------------------------------------------------------
;;; gtk.cell-editable.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     gboolean    editing-canceled    Read / Write
;;;
;;; Signals
;;;
;;;         void    editing-done        Run Last
;;;         void    remove-widget       Run Last
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
 "@version{2020-6-20}
  @begin{short}
    The @sym{gtk-cell-editable} interface must be implemented for widgets to be
    usable when editing the contents of a @class{gtk-tree-view} cell.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"editing-done\" signal}
      @begin{pre}
 lambda (cell-editable)    :run-last
      @end{pre}
      This signal is a sign for the cell renderer to update its value from the
      @arg{cell-editable} argument. Implementations of the
      @sym{gtk-cell-editable} class are responsible for emitting this signal
      when they are done editing, e.g. the @class{gtk-entry} widget is emitting
      it when the user presses the @kbd{Enter} key. The
      @fun{gtk-cell-editable-editing-done} function is a convenience method for
      emitting the \"editing-done\" signal.
      @begin[code]{table}
        @entry[cell-editable]{The @sym{gtk-cell-editable} object on which the
          signal was emitted.}
      @end{table}
    @subheading{The \"remove-widget\" signal}
      @begin{pre}
 lambda (cell-editable)    : Run Last
      @end{pre}
      This signal is meant to indicate that the cell is finished editing, and
      the widget may now be destroyed. Implementations of
      @sym{gtk-cell-editable} are responsible for emitting this signal when they
      are done editing. It must be emitted after the \"editing-done\" signal, to
      give the cell renderer a chance to update the cell's value before the
      widget is removed. The function @fun{gtk-cell-editable-remove-widget} is
      a convenience method for emitting the \"remove-widget\" signal.
      @begin[code]{table}
        @entry[cell-editable]{The @sym{gtk-cell-editable} object on which the
          signal was emitted.}
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
 "The @code{editing-canceled} property of type @code{:boolean} (Read / Write)
  @br{}
  Indicates whether editing on the cell has been canceled. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-editable-editing-canceled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-editable-editing-canceled 'function)
 "@version{2020-6-20}
  @syntax[]{(gtk-cell-editable-editing-canceled object) => canceled}
  @syntax[]{(setf (gtk-cell-editable-editing-canceled object) canceled)}
  @argument[object]{a @class{gtk-cell-editable} object}
  @argument[canceled]{a boolean whether editing on the cell has been canceled}
  @begin{short}
    Accessor of the @slot[gtk-cell-editable]{editing-canceled} slot of the
    @class{gtk-cell-editable} class.
  @end{short}

  Indicates whether editing on the cell has been canceled.
  @see-class{gtk-editable}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_start_editing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_start_editing" gtk-cell-editable-start-editing)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-20}
  @argument[cell-editable]{a @class{gtk-cell-editable} object}
  @argument[event]{a @class{gdk-event}, or @code{nil}}
  @begin{short}
    Begins editing on a cell editable.
  @end{short}
  @arg{event} is the @class{gdk-event} that began the editing process. It may
  be @code{nil}, in the instance that editing was initiated through programatic
  means.
  @see-class{gtk-cell-editable}"
  (cell-editable (g-object gtk-cell-editable))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-cell-editable-start-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_editing_done ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_editing_done" gtk-cell-editable-editing-done) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-20}
  @argument[cell-editable]{a @class{gtk-cell-editable} object}
  @begin{short}
    Emits the \"editing-done\" signal.
  @end{short}
  @see-class{gtk-cell-editable}"
  (cell-editable (g-object gtk-cell-editable)))

(export 'gtk-cell-editable-editing-done)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_remove_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_remove_widget" gtk-cell-editable-remove-widget)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2020-6-20}
  @argument[cell-editable]{a @class{gtk-cell-editable} object}
  @begin{short}
    Emits the \"remove-widget\" signal.
  @end{short}
  @see-class{gtk-cell-editable}"
  (cell-editable (g-object gtk-cell-editable)))

(export 'gtk-cell-editable-remove-widget)

;;; --- End of file gtk.cell-editable.lisp -------------------------------------
