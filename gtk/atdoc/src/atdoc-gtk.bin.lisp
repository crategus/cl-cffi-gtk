;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.bin.lisp
;;;
;;; Documentation strings for the library GTK.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See http://www.gtk.org.
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

;;; --- gtk-bin ----------------------------------------------------------------

(setf (documentation 'gtk-bin 'type)
 "@version{2014-1-5}
  @short{The GtkBin widget is a container with just one child.}
  It is not very useful itself, but it is useful for deriving subclasses, since
  it provides common code needed for handling a single child widget.

 Many GTK+ widgets are subclasses of GtkBin, including GtkWindow, GtkButton,
 GtkFrame, GtkHandleBox or GtkScrolledWindow.")

;;; --- gtk-bin-get-child ------------------------------------------------------

(setf (documentation 'gtk-bin-get-child 'function)
 "@version{2013-1-5}
  @argument[bin]{a GtkBin}
  @return{pointer to child of the GtkBin}
  @begin{short}
    Gets the child of the GtkBin, or NULL if the bin contains no child widget.
  @end{short}
  The returned widget does not have a reference added, so you do not need to
  unref it.")

;;; --- End of file atdoc-gtk.bin.lisp -----------------------------------------
