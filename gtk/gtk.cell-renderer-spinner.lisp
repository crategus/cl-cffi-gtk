;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-spinner.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See >http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererSpinner
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-spinner 'type)
 "@version{2013-2-23}
  @begin{short}
    GtkCellRendererSpinner renders a spinning animation in a cell, very similar
    to GtkSpinner.
  @end{short}
  It can often be used as an alternative to a GtkCellRendererProgress for
  displaying indefinite activity, instead of actual progress.

  To start the animation in a cell, set the @code{\"active\"} property to TRUE
  and increment the @code{\"pulse\"} property at regular intervals. The usual
  way to set the cell renderer properties for each cell is to bind them to
  columns in your tree model using e.g. gtk_tree_view_column_add_attribute().
  @see-slot{gtk-cell-renderer-spinner-active}
  @see-slot{gtk-cell-renderer-spinner-pulse}
  @see-slot{gtk-cell-renderer-spinner-size}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-cell-renderer-spinner) 't)
 "The @code{\"active\"} property of type @code{gboolean} (Read / Write)@br{}
  Whether the spinner is active (ie. shown) in the cell.@br{}
  Default value: FALSE")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pulse" 'gtk-cell-renderer-spinner) 't)
 "The @code{\"pulse\"} property of type @code{guint} (Read / Write)@br{}
  Pulse of the spinner. Increment this value to draw the next frame of the
  spinner animation. Usually, you would update this value in a timeout.@br{}
  By default, the GtkSpinner widget draws one full cycle of the animation,
  consisting of 12 frames, in 750 milliseconds.@br{}
  Default value: 0@br{}
  Since 2.20")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size" 'gtk-cell-renderer-spinner) 't)
 "The @code{\"size\"} property of type @code{GtkIconSize} (Read / Write)@br{}
  The GtkIconSize value that specifies the size of the rendered spinner.@br{}
  Default value: GTK_ICON_SIZE_MENU@br{}
  Since 2.20")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-renderer-spinner-active ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-spinner-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-spinner-active 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"active\"} of the
    @class{gtk-cell-renderer-spinner} class.
  @end{short}")

;;; --- gtk-cell-renderer-spinner-pulse ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-spinner-pulse atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-spinner-pulse 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"pulse\"} of the
    @class{gtk-cell-renderer-spinner} class.
  @end{short}")

;;; --- gtk-cell-renderer-spinner-size -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-spinner-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-spinner-size 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the slot @code{\"size\"} of the
    @class{gtk-cell-renderer-spinner} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_spinner_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-spinner-new))

(defun gtk-cell-renderer-spinner-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-2-23}
  @return{A new GtkCellRenderer.}
  @begin{short}
    Returns a new cell renderer which will show a spinner to indicate activity.
  @end{short}

  Since 2.20"
  (make-instance 'gtk-cell-renderer-spinner))

(export 'gtk-cell-renderer-spinner-new)

;;; --- End of file gtk.cell-renderer-spinner.lisp -----------------------------
