;;; ----------------------------------------------------------------------------
;;; gtk.orientable.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; GtkOrientable
;;;
;;;     An interface for flippable widgets
;;;
;;; Types and Values
;;;
;;;     GtkOrientable
;;;
;;; Functions
;;;
;;;     gtk_orientable_get_orientation                     Accessor
;;;     gtk_orientable_set_orientation                     Accessor
;;;
;;; Properties
;;;
;;;     GtkOrientation    orientation    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkOrientable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkOrientable" gtk-orientable
  (:export t
   :type-initializer "gtk_orientable_get_type")
  (orientation
   gtk-orientable-orientation
   "orientation" "GtkOrientation" t t))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-orientable atdoc:*class-name-alias*)
      "Interface"
      (documentation 'gtk-orientable 'type)
 "@version{2021-10-31}
  @begin{short}
    An interface for flippable widgets.
  @end{short}

  The @sym{gtk-orientable} interface is implemented by all widgets that can be
  oriented horizontally or vertically. Historically, such widgets have been
  realized as subclasses of a common base class, e.g. GtkBox, GtkHBox, GtkVBox.
  The @sym{gtk-orientable} interface is more flexible in that it allows the
  orientation to be changed at runtime, allowing the widgets to \"flip\".
  @see-slot{gtk-orientable-orientation}
  @see-class{gtk-box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-orientable-orientation ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "orientation"
                                               'gtk-orientable) 't)
 "The @code{orientation} property of type @symbol{gtk-orientation}
    (Read / Write) @br{}
  The orientation of the orientable widget. @br{}
  Default value: @code{:horizontal}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-orientable-orientation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-orientable-orientation 'function)
 "@version{*2021-10-31}
  @syntax[]{(gtk-orientable-orientation object) => orientation}
  @syntax[]{(setf (gtk-orientable-orientation object) orientation)}
  @argument[object]{a @class{gtk-orientable} widget}
  @argument[orientation]{a value of the @symbol{gtk-orientation} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-orientable]{orientation} slot of the
    @class{gtk-orientable} interface.
  @end{short}

  The @sym{gtk-orientable-orientation} slot access function returns the
  orientation of the orientable widget. The
  @sym{(setf gtk-orientable-orientation)} slot access function sets the
  orientation.
  @see-class{gtk-orientable}
  @see-symbol{gtk-orientation}")

;;; --- End of file gtk.orientable.lisp ----------------------------------------
