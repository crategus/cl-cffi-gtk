;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-progress.lisp
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
;;; GtkCellRendererProgress
;;;
;;;     Renders numbers as progress bars
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererProgress
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_progress_new
;;;
;;; Properties
;;;
;;;     gboolean   inverted       Read / Write
;;;         gint   pulse          Read / Write
;;;        gchar*  text           Read / Write
;;;       gfloat   text-xalign    Read / Write
;;;       gfloat   text-yalign    Read / Write
;;;         gint   value          Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererProgress
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererProgress
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererProgress" gtk-cell-renderer-progress
  (:superclass gtk-cell-renderer
   :export t
   :interfaces ("GtkOrientable")
   :type-initializer "gtk_cell_renderer_progress_get_type")
  ((inverted
    gtk-cell-renderer-progress-inverted
    "inverted" "gboolean" t t)
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-cell-renderer-progress 'type)
 "@version{2013-6-22}
  @begin{short}
    @sym{gtk-cell-renderer-progress} renders a numeric value as a progress par
    in a cell. Additionally, it can display a text on top of the progress bar.
  @end{short}

  The @sym{gtk-cell-renderer-progress} cell renderer was added in GTK+ 2.6.
  @see-slot{gtk-cell-renderer-progress-inverted}
  @see-slot{gtk-cell-renderer-progress-pulse}
  @see-slot{gtk-cell-renderer-progress-text}
  @see-slot{gtk-cell-renderer-progress-text-xalign}
  @see-slot{gtk-cell-renderer-progress-text-yalign}
  @see-slot{gtk-cell-renderer-progress-value}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-cell-renderer-progress-inverted ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inverted"
                                               'gtk-cell-renderer-progress) 't)
 "The @code{inverted} property of type @code{:boolean} (Read / Write) @br{}
  Invert the direction in which the progress bar grows. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-progress-inverted atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-progress-inverted 'function)
 "@version{2013-6-22}
  Accessor of the @slotgtk-cell-renderer-progress[]{inverted} slot of the
  @class{gtk-cell-renderer-progress} class.
  @see-class{gtk-cell-renderer-progress}")

;;; --- gtk-cell-renderer-progress-pulse ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pulse"
                                               'gtk-cell-renderer-progress) 't)
 "The @code{pulse} property of type @code{:int} (Read / Write) @br{}
  Setting this to a non-negative value causes the cell renderer to enter
  \"activity mode\", where a block bounces back and forth to indicate that some
  progress is made, without specifying exactly how much.
  Each increment of the property causes the block to move by a little bit.
  To indicate that the activity has not started yet, set the property to zero.
  To indicate completion, set the property to @code{G_MAXINT}. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-progress-pulse atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-progress-pulse 'function)
 "@version{2013-6-22}
  Accessor of the @slot[gtk-cell-renderer-progress]{pulse} slot of the
  @class{gtk-cell-renderer-progress} class.
  @see-class{gtk-cell-renderer-progress}")

;;; --- gtk-cell-renderer-progress-text ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text"
                                               'gtk-cell-renderer-progress) 't)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The @code{text} property determines the label which will be drawn over the
  progress bar. Setting this property to @code{nil} causes the default label to
  be displayed. Setting this property to an empty string causes no label to be
  displayed. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-progress-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-progress-text 'function)
 "@version{2013-6-22}
  Accessor of the @slot[gtk-cell-renderer-progress]{text} slot of the
  @class{gtk-cell-renderer-progress} class.
  @see-class{gtk-cell-renderer-progress}")

;;; --- gtk-cell-renderer-progress-text-xalign ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text-xalign"
                                               'gtk-cell-renderer-progress) 't)
 "The @code{text-xalign} property of type @code{:float} (Read / Write) @br{}
  The @code{text-xalign} property controls the horizontal alignment of the
  text in the progress bar. Valid values range from 0 (left) to 1 (right).
  Reserved for RTL layouts. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-progress-text-xalign
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-progress-text-xalign 'function)
 "@version{2013-6-22}
  Accessor of the @slot[gtk-cell-renderer-progress]{text-xalign} slot of the
  @class{gtk-cell-renderer-progress} class.
  @see-class{gtk-cell-renderer-progress}")

;;; --- gtk-cell-renderer-progress-text-yalign ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text-yalign"
                                               'gtk-cell-renderer-progress) 't)
 "The @code{text-yalign} property of type @code{:float} (Read / Write) @br{}
  The @code{text-yalign} property controls the vertical alignment of the
  text in the progress bar. Valid values range from 0 (top) to 1 (bottom). @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-progress-text-yalign
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-progress-text-yalign 'function)
 "@version{2013-6-22}
  Accessor of the @slot[gtk-cell-renderer-progress]{text-yalign} slot of the
  @class{gtk-cell-renderer-progress} class.
  @see-class{gtk-cell-renderer-progress}")

;;; --- gtk-cell-renderer-progress-value ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value"
                                               'gtk-cell-renderer-progress) 't)
 "The @code{value} property of type @code{:int} (Read / Write) @br{}
  The @code{value} property determines the percentage to which the progress
  bar will be \"filled in\". @br{}
  Allowed values: [0,100] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-cell-renderer-progress-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-cell-renderer-progress-value 'function)
 "@version{2013-6-22}
  Accessor of the @slot[gtk-cell-renderer-progress]{value} slot of the
  @class{gtk-cell-renderer-progress} class.
  @see-class{gtk-cell-renderer-progress}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_progress_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-cell-renderer-progress-new))

(defun gtk-cell-renderer-progress-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-22}
  @return{The new cell renderer.}
  @begin{short}
    Creates a new @sym{gtk-cell-renderer-progress} object.
  @end{short}
  @see-class{gtk-cell-renderer-progress}"
  (make-instance 'gtk-cell-renderer-progress))

(export 'gtk-cell-renderer-progress-new)

;;; --- End of file gtk.cell-renderer-progress.lisp ----------------------------
