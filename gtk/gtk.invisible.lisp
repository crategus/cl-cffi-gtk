;;; ----------------------------------------------------------------------------
;;; gtk.invisible.lisp
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
;;; GtkInvisible
;;;
;;; A widget which is not displayed
;;;
;;; Synopsis
;;;
;;;     GtkInvisible
;;;
;;;     gtk_invisible_new
;;;     gtk_invisible_new_for_screen
;;;     gtk_invisible_set_screen
;;;     gtk_invisible_get_screen
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
 "@version{2013-4-16}
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

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "screen" 'gtk-invisible) 't)
 "The @code{\"screen\"} property of type @class{gdk-screen} (Read / Write)@br{}
  The screen where this window will be displayed.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-invisible-screen atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-invisible-screen 'function)
 "@version{2013-7-1}
  Accessor of the slot @code{\"screen\"} of the @class{gtk-invisible} class.
  See the accessor functions @fun{gtk-invisible-get-screen} and
  @fun{gtk-invisible-set-screen} for more information.
  @see-function{gtk-invisible-get-screen}
  @see-function{gtk-invisible-set-screen}")

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-invisible-new))

(defun gtk-invisible-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-4-16}
  @return{A new @class{gtk-invisible} widget.}
  Creates a new @class{gtk-invisible} widget."
  (make-instance 'gtk-invisible))

(export 'gtk-invisible-new)

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_new_for_screen ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-invisible-new-for-screen))

(defun gtk-invisible-new-for-screen (screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-16}
  @argument[screen]{a @class{gdk-screen} object which identifies on which the
    new @class{gtk-invisible} widget will be created}
  @return{A newly created @class{gtk-invisible} widget.}
  @begin{short}
    Creates a new @class{gtk-invisible} widget for a specified @arg{screen}.
  @end{short}

  Since 2.2"
  (make-instance 'gtk-invisible
                 :screen screen))

(export 'gtk-invisible-new-for-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_set_screen ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-invisible-set-screen))

(defun gtk-invisible-set-screen (invisible screen)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-1}
  @argument[invisible]{a @class{gtk-invisible} widget}
  @argument[screen]{a @class{gdk-screen} object}
  @begin{short}
    Sets the @class{gdk-screen} object where the @class{gtk-invisible} widget
    will be displayed.
  @end{short}

  Since 2.2
  @see-function{gtk-invisible-get-screen}"
  (setf (gtk-invisible-screen invisible) screen))

(export 'gtk-invisible-set-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_get_screen ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-invisible-get-screen))

(defun gtk-invisible-get-screen (invisible)
 #+cl-cffi-gtk-documentation
 "@version{2013-7-1}
  @argument[invisible]{a @class{gtk-invisible} widget}
  @return{The associated @class{gdk-screen} object.}
  @begin{short}
    Returns the @class{gdk-screen} object associated with invisible.
  @end{short}

  Since 2.2
  @see-function{gtk-invisible-set-screen}"
  (gtk-invisible-screen invisible))

(export 'gtk-invisible-get-screen)

;;; --- End of file gtk.invisible.lisp -----------------------------------------
