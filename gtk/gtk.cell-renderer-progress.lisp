;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-progress.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkCellRendererProgress
;;; 
;;; Renders numbers as progress bars
;;; 
;;; Synopsis
;;; 
;;;     GtkCellRendererProgress
;;;
;;;     gtk_cell_renderer_progress_new
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkCellRenderer
;;;                +----GtkCellRendererProgress
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkCellRendererProgress implements GtkOrientable.
;;;
;;; Properties
;;; 
;;;   "inverted"                 gboolean              : Read / Write
;;;   "pulse"                    gint                  : Read / Write
;;;   "text"                     gchar*                : Read / Write
;;;   "text-xalign"              gfloat                : Read / Write
;;;   "text-yalign"              gfloat                : Read / Write
;;;   "value"                    gint                  : Read / Write
;;; 
;;; Description
;;; 
;;; GtkCellRendererProgress renders a numeric value as a progress par in a cell.
;;; Additionally, it can display a text on top of the progress bar.
;;; 
;;; The GtkCellRendererProgress cell renderer was added in GTK+ 2.6.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "inverted" property
;;; 
;;;   "inverted"                 gboolean              : Read / Write
;;; 
;;; Invert the direction in which the progress bar grows.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "pulse" property
;;; 
;;;   "pulse"                    gint                  : Read / Write
;;; 
;;; Setting this to a non-negative value causes the cell renderer to enter
;;; "activity mode", where a block bounces back and forth to indicate that some
;;; progress is made, without specifying exactly how much.
;;; 
;;; Each increment of the property causes the block to move by a little bit.
;;; 
;;; To indicate that the activity has not started yet, set the property to zero.
;;; To indicate completion, set the property to G_MAXINT.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "text" property
;;; 
;;;   "text"                     gchar*                : Read / Write
;;; 
;;; The "text" property determines the label which will be drawn over the
;;; progress bar. Setting this property to NULL causes the default label to be
;;; displayed. Setting this property to an empty string causes no label to be
;;; displayed.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "text-xalign" property
;;; 
;;;   "text-xalign"              gfloat                : Read / Write
;;; 
;;; The "text-xalign" property controls the horizontal alignment of the text in
;;; the progress bar. Valid values range from 0 (left) to 1 (right). Reserved
;;; for RTL layouts.
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "text-yalign" property
;;; 
;;;   "text-yalign"              gfloat                : Read / Write
;;; 
;;; The "text-yalign" property controls the vertical alignment of the text in
;;; the progress bar. Valid values range from 0 (top) to 1 (bottom).
;;; 
;;; Allowed values: [0,1]
;;; 
;;; Default value: 0.5
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "value" property
;;; 
;;;   "value"                    gint                  : Read / Write
;;; 
;;; The "value" property determines the percentage to which the progress bar
;;; will be "filled in".
;;; 
;;; Allowed values: [0,100]
;;; 
;;; Default value: 0
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererProgress
;;; 
;;; struct GtkCellRendererProgress;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererProgress" gtk-cell-renderer-progress
  (:superclass gtk-cell-renderer
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_progress_get_type")
  ((orientation
    gtk-cell-renderer-progress-orientation
    "orientation" "GtkProgressBarOrientation" t t)
   (pulse
    gtk-cell-renderer-progress-pulse
    "pulse" "gint" t t)
   (text
    gtk-cell-renderer-progress-text
    "text" "gchararray" t t)
   (text-xalign
    gtk-cell-renderer-progress-text-xalign
    "text-xalign" "gfloat" t t)
   (text-yalign
    gtk-cell-renderer-progress-text-yalign
    "text-yalign" "gfloat" t t)
   (value
    gtk-cell-renderer-progress-value
    "value" "gint" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_progress_new ()
;;; 
;;; GtkCellRenderer * gtk_cell_renderer_progress_new (void);
;;; 
;;; Creates a new GtkCellRendererProgress.
;;; 
;;; Returns :
;;;     the new cell renderer
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defun gtk-cell-renderer-progress-new ()
  (make-instance 'gtk-cell-renderer-progress-new))

(export 'gtk-cell-renderer-progress-new)

;;; --- End of file gtk.cell-renderer-progress.lisp ----------------------------
