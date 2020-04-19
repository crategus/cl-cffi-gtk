;;; ----------------------------------------------------------------------------
;;; gtk.spinner.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2020 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkSpinner
;;;
;;; Functions
;;;
;;;     gtk_spinner_new
;;;     gtk_spinner_start
;;;     gtk_spinner_stop
;;;
;;; Properties
;;;
;;;     gboolean  active    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkSpinner
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSpinner implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; Class gtk-spinner
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSpinner" gtk-spinner
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_spinner_get_type")
  ((active
    gtk-spinner-active
    "active" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-spinner 'type)
 "@version{2020-4-17}
  @begin{short}
    A @sym{gtk-spinner} widget displays an icon size spinning animation.
  @end{short}
  It is often used as an alternative to a @class{gtk-progress-bar} widget for
  displaying indefinite activity, instead of actual progress.

  To start the animation, use the function @fun{gtk-spinner-start}, to stop it
  use the function @fun{gtk-spinner-stop}.
  @begin[CSS nodes]{dictionary}
    @sym{gtk-spinner} has a single CSS node with the name @code{spinner}. When
    the animation is active, the @code{:checked} pseudoclass is added to this
    node.
  @end{dictionary}
  @see-slot{gtk-spinner-active}
  @see-class{gtk-cell-renderer-spinner}
  @see-class{gtk-progress-bar}
  @see-function{gtk-spinner-start}
  @see-function{gtk-spinner-stop}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-spinner) 't)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the spinner is active. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spinner-active atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-spinner-active 'function)
 "@version{2020-4-17}
  @syntax[]{(gtk-spinner-active object) => active}
  @syntax[]{(setf (gtk-spinner-active object) active)}
  @argument[object]{a @class{gtk-spinner} widget}
  @argument[active]{a boolean wether the spinner is active}
  @begin{short}
    Accessor of the @slot[gtk-spinner]{active} slot of the @class{gtk-spinner}
    class.
  @end{short}
  @see-class{gtk-spinner}")

;;; ----------------------------------------------------------------------------
;;; gtk-spinner-new
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spinner-new))

(defun gtk-spinner-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @return{A new @class{gtk-spinner} widget.}
  @short{Returns a new spinner widget. Not yet started.}
  @see-class{gtk-spinner}
  @see-class{gtk-spinner-start}"
  (make-instance 'gtk-spinner))

(export 'gtk-spinner-new)

;;; ----------------------------------------------------------------------------
;;; gtk-spinner-start
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spinner_start" gtk-spinner-start) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[spinner]{a @class{gtk-spinner} widget}
  @short{Starts the animation of the spinner.}
  @see-class{gtk-spinner}
  @see-function{gtk-spinner-stop}"
  (spinner (g-object gtk-spinner)))

(export 'gtk-spinner-start)

;;; ----------------------------------------------------------------------------
;;; gtk-spinner-stop
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spinner_stop" gtk-spinner-stop) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-4-17}
  @argument[spinner]{a @class{gtk-spinner} widget}
  @short{Stops the animation of the spinner.}
  @see-class{gtk-spinner}
  @see-function{gtk-spinner-start}"
  (spinner (g-object gtk-spinner)))

(export 'gtk-spinner-stop)

;;; --- End of file gtk.spinner.lisp -------------------------------------------
