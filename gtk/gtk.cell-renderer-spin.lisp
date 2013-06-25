;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-spin.lisp
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
;;; GtkCellRendererSpin
;;;
;;; Renders a spin button in a cell
;;;
;;; Synopsis
;;;
;;; GtkCellRendererSpin
;;;
;;; gtk_cell_renderer_spin_new
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererSpin
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererSpin" gtk-cell-renderer-spin
  (:superclass gtk-cell-renderer-text
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_spin_get_type")
  ((adjustment
    gtk-cell-renderer-spin-adjustment
    "adjustment" "GtkAdjustment" t t)
   (climb-rate
    gtk-cell-renderer-spin-climb-rate
    "climb-rate" "gdouble" t t)
   (digits
    gtk-cell-renderer-spin-digits
    "digits" "guint" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-spin 'type)
 "@version{2013-6-22}
  @begin{short}
    @sym{gtk-cell-renderer-spin} renders text in a cell like
    @class{gtk-cell-renderer-text} from which it is derived.
  @end{short}
  But while @class{gtk-cell-renderer-text} offers a simple entry to edit the
  text, @sym{gtk-cell-renderer-spin} offers a @class{gtk-spin-button} widget. Of
  course, that means that the text has to be parseable as a floating point
  number.

  The range of the spin button is taken from the adjustment property of the
  cell renderer, which can be set explicitly or mapped to a column in the tree
  model, like all properties of cell renders. @sym{gtk-cell-renderer-spin} also
  has properties for the \"climb-rate\" and the number of \"digits\" to display.
  Other @class{gtk-spin-button} properties can be set in a handler for the
  \"editing-started\" signal.

  The @sym{gtk-cell-renderer-spin} cell renderer was added in GTK+ 2.10.
  @see-slot{gtk-cell-renderer-spin-adjustment}
  @see-slot{gtk-cell-renderer-spin-climb-rate}
  @see-slot{gtk-cell-renderer-spin-digits}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "adjustment"
                                               'gtk-cell-renderer-spin) 't)
 "The @code{\"adjustment\"} property of type @class{gtk-adjustment}
  (Read / Write) @br{}
  The adjustment that holds the value of the spin button. This must be
  non-@code{nil} for the cell renderer to be editable. @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "climb-rate"
                                               'gtk-cell-renderer-spin) 't)
 "The @code{\"climb-rate\"} property of type @code{:double} (Read / Write) @br{}
  The acceleration rate when you hold down a button. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "digits"
                                               'gtk-cell-renderer-spin) 't)
 "The @code{\"digits\"} property of type @code{:uint} (Read / Write) @br{}
  The number of decimal places to display. @br{}
  Allowed values: <= 20 @br{}
  Default value: 0 @br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-spin-adjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-spin-adjustment 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"adjustment\"} of the
  @class{gtk-cell-renderer-spin} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-spin-climb-rate atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-spin-climb-rate 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"climb-rate\"} of the
  @class{gtk-cell-renderer-spin} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-spin-digits atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-spin-digits 'function)
 "@version{2013-6-22}
  Accessor of the slot @code{\"digits\"} of the
  @class{gtk-cell-renderer-spin} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_spin_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-spin-new))

(defun gtk-cell-renderer-spin-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @return{A new @class{gtk-cell-renderer-spin} object.}
  @short{Creates a new @class{gtk-cell-renderer-spin} object.}

  Since 2.10"
  (make-instance 'gtk-cell-renderer-spin))

(export 'gtk-cell-renderer-spin-new)

;;; --- End of file gtk.cell-renderer-spin.lisp --------------------------------
