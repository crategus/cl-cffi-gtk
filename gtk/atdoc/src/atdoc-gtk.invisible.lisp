;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.invisible.lisp
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

;;; --- gtk-invisible ----------------------------------------------------------

(setf (documentation 'gtk-invisible 'type)
 "@version{2013-1-30}
  @begin{short}
    The @sym{gtk-invisible} widget is used internally in GTK+, and is probably
    not very useful for application developers.
  @end{short}

  It is used for reliable pointer grabs and selection handling in the code for
  drag-and-drop.
  @see-slot{gtk-invisible-screen}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-invisible) 't)
 "The \"screen\" property of type @class{gdk-screen} (Read / Write)@br{}
  The screen where this window will be displayed.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-invisible-screen ---------------------------------------------------

(setf (gethash 'gtk-invisible-screen atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-invisible-screen 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"screen\"} of the @class{gtk-invisible}
    class.
  @end{short}")

;;; --- gtk-invisible-new ------------------------------------------------------

(setf (documentation 'gtk-invisible-new 'function)
 "@version{2013-1-30}
  @return{A new @class{gtk-invisible} widget.}
  @begin{short}
    Creates a new @class{gtk-invisible}.
  @end{short}")

;;; --- gtk-invisible-new-for-screen -------------------------------------------

(setf (documentation 'gtk-invisible-new-for-screen 'function)
 "@version{2013-1-30}
  @argument[screen]{a @class{gdk-screen} which identifies on which the new
    @class{gtk-invisible} will be created.}
  @return{A newly created @class{gtk-invisible} object.}
  @begin{short}
    Creates a new @class{gtk-invisible} object for a specified @arg{screen}.
  @end{short}

  Since 2.2")

;;; --- gtk-invisible-set-screen -----------------------------------------------

(setf (documentation 'gtk-invisible-set-screen 'function)
 "@version{2013-1-30}
  @argument[invisible]{a @class{gtk-invisible}.}
  @argument[screen]{a @class{gdk-screen}.}
  @begin{short}
    Sets the @class{gdk-screen} where the @class{gtk-invisible} object will be
    displayed.
  @end{short}

  Since 2.2")

;;; --- gtk-invisible-get-screen -----------------------------------------------

(setf (documentation 'gtk-invisible-get-screen 'function)
 "@version{2013-1-30}
  @argument[invisible]{a @class{gtk-invisible} widget.}
  @return{The associated @class{gdk-screen}.}
  @begin{short}
    Returns the @class{gdk-screen} object associated with invisible.
  @end{short}
  Since 2.2")

;;; --- End of file atdoc-gtk.invisible.lisp -----------------------------------