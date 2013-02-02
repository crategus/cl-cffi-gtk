;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.orientable.lisp
;;;
;;; Documentation strings for the library GTK+.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(in-package :gtk)

;;; --- gtk-orientable ---------------------------------------------------------

(setf (gethash 'gtk-orientable atdoc:*class-name-alias*) "Interface")
(setf (documentation 'gtk-orientable 'type)
 "@version{2013-1-20}
  @short{An interface for flippable widgets.}

  The @sym{gtk-orientable} interface is implemented by all widgets that can be
  oriented horizontally or vertically. Historically, such widgets have been
  realized as subclasses of a common base class (e.g. @class{gtk-box} /
  @class{gtk-hbox} / @class{gtk-vbox} or @class{gtk-scale} / @class{gtk-hscale} 
  / @class{gtk-vscale}). @sym{gtk-orientable} is more flexible in that it
  allows the orientation to be changed at runtime, allowing the widgets to
  \"flip\".
 
  @sym{gtk-orientable} was introduced in GTK+ 2.16.
  @see-slot{gtk-orientable-orientation}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "orientation" 'gtk-orientable) 't)
 "The @arg{\"orientation\"} property of type @symbol{gtk-orientation}
  (Read / Write)@br{}
  The orientation of the orientable.@b{}
  Default value: @code{:horizontal}
  Since 2.16")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-orientable-orientation ---------------------------------------------

(setf (gethash 'gtk-orientable-orientation atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-orientable-orientation 'function)
 "@version{2013-1-20}
  @begin{short}
    Accessor of the slot @arg{\"orientation\"} of the @class{gtk-orientable}
    interface.
  @end{short}
  @see-function{gtk-orientable-get-orientation}
  @see-function{gtk-orientable-set-orientation}")

;;; --- gtk-orientable-get-orientation -----------------------------------------

(setf (documentation 'gtk-orientable-get-orientation 'function)
 "@version{2013-1-20}
  @argument[orientable]{a @class{gtk-orientable} instance}
  @return{The orientation of the @arg{orientable}.}
  @short{Retrieves the orientation of the @arg{orientable}.}

  Since 2.16")

;;; --- gtk-orientable-set-orientation -----------------------------------------

(setf (documentation 'gtk-orientable-set-orientation 'function)
 "@version{2013-1-20}
  @argument[orientable]{a @class{gtk-orientable} instance}
  @argument[orientation]{the @arg{orientable}'s new @arg{orientation}.}
  @short{Sets the @arg{orientation} of the @arg{orientable}.}

  Since 2.16")

;;; --- End of file atdoc-gtk.orientable.lisp ----------------------------------
