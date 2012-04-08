;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-spin.lisp
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
;;; GtkCellRendererSpin
;;; 
;;; Renders a spin button in a cell
;;; 
;;; Synopsis
;;; 
;;;     GtkCellRendererSpin
;;;
;;;     gtk_cell_renderer_spin_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkCellRenderer
;;;                +----GtkCellRendererText
;;;                      +----GtkCellRendererSpin
;;; 
;;; Properties
;;; 
;;;   "adjustment"               GtkAdjustment*        : Read / Write
;;;   "climb-rate"               gdouble               : Read / Write
;;;   "digits"                   guint                 : Read / Write
;;; 
;;; Description
;;; 
;;; GtkCellRendererSpin renders text in a cell like GtkCellRendererText from
;;; which it is derived. But while GtkCellRendererText offers a simple entry to
;;; edit the text, GtkCellRendererSpin offers a GtkSpinButton widget. Of course,
;;; that means that the text has to be parseable as a floating point number.
;;; 
;;; The range of the spinbutton is taken from the adjustment property of the
;;; cell renderer, which can be set explicitly or mapped to a column in the tree
;;; model, like all properties of cell renders. GtkCellRendererSpin also has
;;; properties for the "climb-rate" and the number of "digits" to display. Other
;;; GtkSpinButton properties can be set in a handler for the "editing-started"
;;; signal.
;;; 
;;; The GtkCellRendererSpin cell renderer was added in GTK+ 2.10.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "adjustment" property
;;; 
;;;   "adjustment"               GtkAdjustment*        : Read / Write
;;; 
;;; The adjustment that holds the value of the spinbutton. This must be non-NULL
;;; for the cell renderer to be editable.
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "climb-rate" property
;;; 
;;;   "climb-rate"               gdouble               : Read / Write
;;; 
;;; The acceleration rate when you hold down a button.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "digits" property
;;; 
;;;   "digits"                   guint                 : Read / Write
;;; 
;;; The number of decimal places to display.
;;; 
;;; Allowed values: <= 20
;;; 
;;; Default value: 0
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererSpin
;;; 
;;; struct GtkCellRendererSpin;
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

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_spin_new ()
;;; 
;;; GtkCellRenderer * gtk_cell_renderer_spin_new (void);
;;; 
;;; Creates a new GtkCellRendererSpin.
;;; 
;;; Returns :
;;;     a new GtkCellRendererSpin
;;; 
;;; Since 2.10
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-spin-new ()
  (make-instance 'gtk-cell-renderer-spin))

(export 'gtk-cell-renderer-spin-new)

;;; --- End of file gtk.cell-renderer-spin.lisp --------------------------------
