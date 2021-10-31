;;; ----------------------------------------------------------------------------
;;; gtk.invisible.lisp
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
;;; GtkInvisible
;;;
;;;     A widget which is not displayed
;;;
;;; Synopsis
;;;
;;;     GtkInvisible
;;;
;;;     gtk_invisible_new
;;;     gtk_invisible_new_for_screen
;;;     gtk_invisible_set_screen                           Accessor
;;;     gtk_invisible_get_screen                           Accessor
;;;
;;; Properties
;;;
;;;     GdkScreen*  screen    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkInvisible
;;;
;;; Implemented Interfaces
;;;
;;;     GtkInvisible implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkInvisible
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkInvisible" gtk-invisible
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_invisible_get_type")
  ((screen
    gtk-invisible-screen
    "screen" "GdkScreen" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-invisible 'type)
 "@version{2021-10-27}
  @begin{short}
    The @sym{gtk-invisible} widget is used internally in GTK, and is probably
    not very useful for application developers.
  @end{short}
  It is used for reliable pointer grabs and selection handling in the code for
  drag and drop.
  @see-slot{gtk-invisible-screen}
  @see-class{gdk-screen}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-invisible) 't)
 "The @code{screen} property of type @class{gdk-screen} (Read / Write) @br{}
  The screen where this window will be displayed.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-invisible-screen atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-invisible-screen 'function)
 "@version{2021-10-27}
  @syntax[]{(gtk-invisible-screen object) => screen}
  @syntax[]{(setf (gtk-invisible-screen object) screen)}
  @argument[object]{a @class{gtk-invisible} widget}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Accessor of the @slot[gtk-invisible]{screen} slot of the
    @class{gtk-invisible} class.
  @end{short}

  The @sym{gtk-invisible-screen} slot access function returns the screen
  associated with the invisible widget. The @sym{(setf gtk-invisible-screen)}
  slot access function sets the screen where the invisible widget will be
  displayed.
  @see-class{gtk-invisible}
  @see-class{gdk-screen}")

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-invisible-new))

(defun gtk-invisible-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @return{A new @class{gtk-invisible} widget.}
  @begin{short}
    Creates a new invisible widget.
  @end{short}
  The @slot[gtk-invisible]{screen} slot is initialized to the default screen.
  See the @fun{gtk-invisible-new-for-screen} function to create a new invisible
  widget for a specified screen.
  @see-class{gtk-invisible}
  @see-class{gdk-screen}
  @see-function{gtk-invisible-new-for-screen}
  @see-function{gtk-invisible-screen}"
  (make-instance 'gtk-invisible))

(export 'gtk-invisible-new)

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_new_for_screen ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-invisible-new-for-screen))

(defun gtk-invisible-new-for-screen (screen)
 #+cl-cffi-gtk-documentation
 "@version{2021-10-27}
  @argument[screen]{a @class{gdk-screen} object which identifies on which the
    @class{gtk-invisible} widget will be created}
  @return{A newly created @class{gtk-invisible} widget.}
  @begin{short}
    Creates a new invisible widget for a specified screen.
  @end{short}
  See also the @fun{gtk-invisible-new} function to create a new invisible widget
  for the default screen.
  @see-class{gtk-invisible}
  @see-class{gdk-screen}
  @see-function{gtk-invisible-new}"
  (make-instance 'gtk-invisible
                 :screen screen))

(export 'gtk-invisible-new-for-screen)

;;; --- End of file gtk.invisible.lisp -----------------------------------------
