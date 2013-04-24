;;; ----------------------------------------------------------------------------
;;; gtk.spinner.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-spinner 'type)
 "@version{2013-4-24}
  @begin{short}
    A @sym{gtk-spinner} widget displays an icon size spinning animation. It is
    often used as an alternative to a @class{gtk-progress-bar} widget for
    displaying indefinite activity, instead of actual progress.
  @end{short}

  To start the animation, use the function @fun{gtk-spinner-start}, to stop it
  use the function @fun{gtk-spinner-stop}.
  @see-slot{gtk-spinner-active}")

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-spinner) 't)
 "The @code{\"active\"} property of type @code{:boolean} (Read / Write)@br{}
  Whether the spinner is active.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Properties
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-spinner-active atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-spinner-active 'function)
 "@version{2013-2-4}
  @begin{short}
    Accessor of the slot \"active\" of the @class{gtk-spinner} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk-spinner-new
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spinner-new))

(defun gtk-spinner-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-4-24}
  @return{A new @class{gtk-spinner} widget.}
  @short{Returns a new spinner widget. Not yet started.}

  Since 2.20"
  (make-instance 'gtk-spinner))

(export 'gtk-spinner-new)

;;; ----------------------------------------------------------------------------
;;; gtk-spinner-start
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spinner_start" gtk-spinner-start) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-24}
  @argument[spinner]{a @class{gtk-spinner} widget}
  @short{Starts the animation of the spinner.}

  Since 2.20"
  (spinner (g-object gtk-spinner)))

(export 'gtk-spinner-start)

;;; ----------------------------------------------------------------------------
;;; gtk-spinner-stop
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spinner_stop" gtk-spinner-stop) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-24}
  @argument[spinner]{a @class{gtk-spinner} widget}
  @short{Stops the animation of the spinner.}

  Since 2.20"
  (spinner (g-object gtk-spinner)))

(export 'gtk-spinner-stop)

;;; --- End of file gtk.spinner.lisp -------------------------------------------
