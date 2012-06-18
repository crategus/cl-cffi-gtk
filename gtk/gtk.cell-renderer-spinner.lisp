;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-spinner.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; GtkCellRendererSpinner
;;; 
;;; Renders a spinning animation in a cell
;;;     
;;; Synopsis
;;; 
;;;     GtkCellRendererSpinner
;;;
;;;     gtk_cell_renderer_spinner_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkCellRenderer
;;;                +----GtkCellRendererSpinner
;;; 
;;; Properties
;;; 
;;;   "active"                   gboolean              : Read / Write
;;;   "pulse"                    guint                 : Read / Write
;;;   "size"                     GtkIconSize           : Read / Write
;;; 
;;; Description
;;; 
;;; GtkCellRendererSpinner renders a spinning animation in a cell, very similar
;;; to GtkSpinner. It can often be used as an alternative to a
;;; GtkCellRendererProgress for displaying indefinite activity, instead of
;;; actual progress.
;;; 
;;; To start the animation in a cell, set the "active" property to TRUE and
;;; increment the "pulse" property at regular intervals. The usual way to set
;;; the cell renderer properties for each cell is to bind them to columns in
;;; your tree model using e.g. gtk_tree_view_column_add_attribute().
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "active" property
;;; 
;;;   "active"                   gboolean              : Read / Write
;;; 
;;; Whether the spinner is active (ie. shown) in the cell.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "pulse" property
;;; 
;;;   "pulse"                    guint                 : Read / Write
;;; 
;;; Pulse of the spinner. Increment this value to draw the next frame of the
;;; spinner animation. Usually, you would update this value in a timeout.
;;; 
;;; By default, the GtkSpinner widget draws one full cycle of the animation,
;;; consisting of 12 frames, in 750 milliseconds.
;;; 
;;; Default value: 0
;;; 
;;; Since 2.20
;;;
;;; ----------------------------------------------------------------------------
;;; The "size" property
;;; 
;;;   "size"                     GtkIconSize           : Read / Write
;;; 
;;; The GtkIconSize value that specifies the size of the rendered spinner.
;;; 
;;; Default value: GTK_ICON_SIZE_MENU
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererSpinner
;;; 
;;; struct GtkCellRendererSpinner;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererSpinner" gtk-cell-renderer-spinner
  (:superclass gtk-cell-renderer
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_spinner_get_type")
  ((active
    gtk-cell-renderer-spinner-active
    "active" "gboolean" t t)
   (pulse
    gtk-cell-renderer-spinner-pulse
    "pulse" "guint" t t)
   (size
    gtk-cell-renderer-spinner-size
    "size" "GtkIconSize" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_spinner_new ()
;;; 
;;; GtkCellRenderer * gtk_cell_renderer_spinner_new (void);
;;; 
;;; Returns a new cell renderer which will show a spinner to indicate activity.
;;; 
;;; Returns :
;;;     a new GtkCellRenderer
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-spinner-new))

(defun gtk-cell-renderer-spinner-new ()
  (make-instance 'gtk-cell-renderer-spinner))

(export 'gtk-cell-renderer-spinner-new)

;;; --- End of file gtk.cell-renderer-spinner.lisp -----------------------------
