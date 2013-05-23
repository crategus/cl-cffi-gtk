;;; ----------------------------------------------------------------------------
;;; gtk.orientable.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkOrientable
;;; 
;;; An interface for flippable widgets
;;;     
;;; Synopsis
;;; 
;;;     GtkOrientable
;;;     
;;;     gtk_orientable_get_orientation
;;;     gtk_orientable_set_orientation
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

;;; ----------------------------------------------------------------------------

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
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "orientation"
                                               'gtk-orientable) 't)
 "The @code{\"orientation\"} property of type @symbol{gtk-orientation}
  (Read / Write)@br{}
  The orientation of the orientable.@br{}
  Default value: @code{:horizontal}
  Since 2.16")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-orientable-orientation atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-orientable-orientation 'function)
 "@version{2013-3-30}
  Accessor of the slot @arg{\"orientation\"} of the @class{gtk-orientable}
  interface.")

;;; ----------------------------------------------------------------------------
;;; gtk_orientable_get_orientation ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-orientable-get-orientation))

(defun gtk-orientable-get-orientation (orientable)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[orientable]{a @class{gtk-orientable} instance}
  @return{The orientation of the @arg{orientable}.}
  @short{Retrieves the orientation of the @arg{orientable}.}

  Since 2.16"
  (gtk-orientable-orientation orientable))

(export 'gtk-orientable-get-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_orientable_set_orientation ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-orientable-set-orientation))

(defun gtk-orientable-set-orientation (orientable orientation)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-30}
  @argument[orientable]{a @class{gtk-orientable} instance}
  @argument[orientation]{the @arg{orientable}'s new @arg{orientation}}
  @short{Sets the @arg{orientation} of the @arg{orientable}.}

  Since 2.16"
  (setf (gtk-orientable-orientation orientable) orientation))

(export 'gtk-orientable-set-orientation)

;;; --- End of file gtk.orientable.lisp ----------------------------------------
