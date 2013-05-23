;;; ----------------------------------------------------------------------------
;;; gtk.overlay.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
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
;;; GtkOverlay
;;; 
;;; A container which overlays widgets on top of each other
;;;     
;;; Synopsis
;;; 
;;;     GtkOverlay
;;;     
;;;     gtk_overlay_new
;;;     gtk_overlay_add_overlay
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkOverlay
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkOverlay" gtk-overlay
  (:superclass gtk-bin
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_overlay_get_type")
  nil)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-overlay 'type)
 "@version{2013-5-20}
  @begin{short}
    @sym{gtk-overlay} is a container which contains a single main child, on top
    of which it can place overlay widgets. The position of each overlay widget
    is determined by its @code{\"halign\"} and @code{\"valign\"} properties.
    E. g. a widget with both alignments set to @code{:start} will be placed at
    the top left corner of the main widget, whereas an overlay with
    @code{\"halign\"} set to @code{:center} and @code{\"valign\"} set to
    @code{:end} will be placed a the bottom edge of the main widget,
    horizontally centered. The position can be adjusted by setting the margin
    properties of the child to non-zero values.
  @end{short}

  More complicated placement of overlays is possible by connecting to the
  \"get-child-position\" signal.

  @subheading{GtkOverlay as GtkBuildable}
    The @sym{gtk-overlay} implementation of the @class{gtk-buildable} interface
    supports placing a child as an overlay by specifying \"overlay\" as the
    \"type\" attribute of a <child> element.
  @begin[Signal Details]{dictionary}
    @subheading{The \"get-child-position\" signal}
      @begin{pre}
 lambda (overlay widget allocation)   : Run Last
      @end{pre}
      The \"get-child-position\" signal is emitted to determine the position and
      size of any overlay child widgets. A handler for this signal should fill
      allocation with the desired position and size for widget, relative to the
      'main' child of overlay.
      The default handler for this signal uses the widget's @code{\"halign\"}
      and @code{\"valign\"} properties to determine the position and gives the
      widget its natural size (except that an alignment of @code{:fill} will
      cause the overlay to be full-width/height). If the main child is a
      @class{gtk-scrolled-window}, the overlays are placed relative to its
      contents.
      @begin[code]{table}
        @entry[overlay]{The @class{gtk-overlay}.}
        @entry[widget]{The child widget to position.}
        @entry[allocation]{Return location for the allocation.}
        @entry[Return]{@em{True} if the allocation has been filled.}
      @end{table}
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-overlay-new))

(defun gtk-overlay-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @return{A new @class{gtk-overlay} container.}
  @short{Creates a new @class{gtk-overlay} container.}

  Since 3.2"
  (make-instance 'gtk-overlay))

(export 'gtk-overlay-new)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_add_overlay ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_overlay_add_overlay" gtk-overlay-add-overlay) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-20}
  @argument[overlay]{a @class{gtk-overlay} container}
  @argument[widget]{a @class{gtk-widget} to be added to the container}
  @short{Adds @arg{widget} to @arg{overlay}.}

  The widget will be stacked on top of the main widget added with the function
  @fun{gtk-container-add}.

  The position at which @arg{widget} is placed is determined from its
  @code{\"halign\"} and @code{\"valign\"} properties.

  Since 3.2"
  (overlay (g-object gtk-overlay))
  (widget (g-object gtk-widget)))

(export 'gtk-overlay-add-overlay)

;;; --- End of file gtk.overlay.lisp -------------------------------------------
