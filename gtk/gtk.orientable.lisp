;;; ----------------------------------------------------------------------------
;;; gtk.orientable.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.

;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;; An interface for flippable widgets
;;;
;;; Synopsis
;;;
;;;     GtkOrientable
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
(setf (gethash 'gtk-orientable atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-orientable 'type)
 "@version{2013-3-30}
  @short{An interface for flippable widgets.}

  The @sym{gtk-orientable} interface is implemented by all widgets that can be
  oriented horizontally or vertically. Historically, such widgets have been
  realized as subclasses of a common base class (e. g. @class{gtk-box},
  @class{gtk-hbox}, @class{gtk-vbox} and @class{gtk-scale}, @class{gtk-hscale},
  or @class{gtk-vscale}). @sym{gtk-orientable} is more flexible in that it
  allows the orientation to be changed at runtime, allowing the widgets to
  \"flip\".

  @sym{gtk-orientable} was introduced in GTK+ 2.16.
  @see-slot{gtk-orientable-orientation}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-orientable-orientation ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "orientation"
                                               'gtk-orientable) 't)
 "The @code{\"orientation\"} property of type @symbol{gtk-orientation}
  (Read / Write) @br{}
  The orientation of the orientable. @br{}
  Default value: @code{:horizontal}
  Since 2.16")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-orientable-orientation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-orientable-orientation 'function)
 "@version{2014-2-21}
  @argument[object]{a @class{gtk-orientable} widet}
  @syntax[]{(gtk-orientable-orientation object) >= orientation}
  @syntax[]{(setf (gtk-orientable-orientation object) orientation)}
  @begin{short}
    Accessor of the slot @slot[gtk-orientable]{orientation} of the
    @class{gtk-orientable} interface.
  @end{short}

  The generic function @sym{gtk-orientable-orientation} returns the orientation
  of the orientable.

  The generic function @sym{(setf gtk-orientable-orientation)} sets the
  orientation of the orientable.

  Since 2.16
  @see-class{gtk-orientable}")

;;; --- End of file gtk.orientable.lisp ----------------------------------------
