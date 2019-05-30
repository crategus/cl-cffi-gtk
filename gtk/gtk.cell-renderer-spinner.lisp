;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-spinner.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2019 Dieter Kaiser
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
;;;     Renders a spinning animation in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererSpinner
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_spinner_new
;;;
;;; Properties
;;;
;;;        gboolean   active    Read / Write
;;;           guint   pulse     Read / Write
;;;     GtkIconSize   size      Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererSpinner
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-spinner 'type)
 "@version{2013-6-22}
  @begin{short}
    @sym{gtk-cell-renderer-spinner} renders a spinning animation in a cell,
    very similar to @class{gtk-spinner}.
  @end{short}
  It can often be used as an alternative to a @class{gtk-cell-renderer-progress}
  for displaying indefinite activity, instead of actual progress.

  To start the animation in a cell, set the @code{active} property to
  @em{true} and increment the @code{pulse} property at regular intervals.
  The usual way to set the cell renderer properties for each cell is to bind
  them to columns in your tree model using e. g. the function
  @fun{gtk-tree-view-column-add-attribute}.
  @see-slot{gtk-cell-renderer-spinner-active}
  @see-slot{gtk-cell-renderer-spinner-pulse}
  @see-slot{gtk-cell-renderer-spinner-size}
  @see-function{gtk-tree-view-column-add-attribute}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-renderer-spinner-active ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active"
                                               'gtk-cell-renderer-spinner) 't)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the spinner is active (i. e. shown) in the cell. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-spinner-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-spinner-active 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-spinner]{active} slot of the
    @class{gtk-cell-renderer-spinner} class.
  @end{short}
  @see-class{gtk-cell-renderer-spinner}")

;;; --- gtk-cell-renderer-spinner-pulse ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pulse"
                                               'gtk-cell-renderer-spinner) 't)
 "The @code{pulse} property of type @code{:uint} (Read / Write) @br{}
  Pulse of the spinner. Increment this value to draw the next frame of the
  spinner animation. Usually, you would update this value in a timeout.
  By default, the @class{gtk-spinner} widget draws one full cycle of the
  animation, consisting of 12 frames, in 750 milliseconds. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-spinner-pulse atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-spinner-pulse 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-spinner]{pulse} slot of the
    @class{gtk-cell-renderer-spinner} class.
  @end{short}
  @see-class{gtk-cell-renderer-spinner}")

;;; --- gtk-cell-renderer-spinner-size -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "size"
                                               'gtk-cell-renderer-spinner) 't)
 "The @code{size} property of type @symbol{gtk-icon-size} (Read / Write) @br{}
  The @symbol{gtk-icon-size} value that specifies the size of the rendered
  spinner. @br{}
  Default value: @code{:menu}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-spinner-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-spinner-size 'function)
 "@version{2013-2-23}
  @begin{short}
    Accessor of the @slot[gtk-cell-renderer-spinner]{size} slot of the
    @class{gtk-cell-renderer-spinner} class.
  @end{short}
  @see-class{gtk-cell-renderer-spinner}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_spinner_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-spinner-new))

(defun gtk-cell-renderer-spinner-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @return{A new @class{gtk-cell-renderer} object.}
  @begin{short}
    Returns a new cell renderer which will show a spinner to indicate activity.
  @end{short}
  @see-class{gtk-cell-renderer-spinner}"
  (make-instance 'gtk-cell-renderer-spinner))

(export 'gtk-cell-renderer-spinner-new)

;;; --- End of file gtk.cell-renderer-spinner.lisp -----------------------------
