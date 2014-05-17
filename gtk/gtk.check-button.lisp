;;; ----------------------------------------------------------------------------
;;; gtk.check-button.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.9 and modified to document the Lisp binding to the GTK library.
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

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; Class gtk-check-button
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCheckButton" gtk-check-button
  (:superclass gtk-toggle-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_check_button_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-check-button 'type)
 "@version{2013-4-26}
  @begin{short}
    A @sym{gtk-check-button} places a discrete @class{gtk-toggle-button} next to
    a widget, (usually a @class{gtk-label}). See the section on
    @class{gtk-toggle-button} widgets for more information about toggle/check
    buttons.
  @end{short}

  The important signal \"toggled\" is also inherited from
  @class{gtk-toggle-button}.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"indicator-size\" style property}
      @code{\"indicator-size\"} of type @code{:int} (Read)@br{}
      Size of check or radio indicator.@br{}
      Allowed values: >= 0 @br{}
      Default value: 16

    @subheading{The \"indicator-spacing\" style property}
      @code{\"indicator-spacing\"} of type @code{:int} (Read)@br{}
      Spacing around check or radio indicator.@br{}
      Allowed values: >= 0@br{}
      Default value: 2
  @end{dictionary}")

;;; ----------------------------------------------------------------------------
;;; gtk-check-button-new
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-button-new))

(defun gtk-check-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-4-26}
  @return{A @class{gtk-check-button} widget.}
  @short{Creates a new @class{gtk-check-button} widget.}"
  (make-instance 'gtk-check-button))

(export 'gtk-check-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk-check-button-new-with-label
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-check-button-new-with-label))

(defun gtk-check-button-new-with-label (label)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-26}
  @argument[label]{the text for the check button}
  @return{A @class{gtk-check-button} widget.}
  @begin{short}
    Creates a new @class{gtk-check-button} widget with a @class{gtk-label}
    widget to the right of it.
  @end{short}"
  (make-instance 'gtk-check-button
                 :label label))

(export 'gtk-check-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk-check-button-new-with-mnemonic
;;; ----------------------------------------------------------------------------

;; TODO: Rewrite the implementation in terms of the function make-instance

(defcfun ("gtk_check_button_new_with_mnemonic"
           gtk-check-button-new-with-mnemonic)
    (g-object gtk-widget)
#+cl-cffi-gtk-documentation
 "@version{2013-4-26}
  @argument[label]{the text of the button, with an underscore in front of the
    mnemonic character}
  @return{A @class{gtk-check-button} widget.}
  @short{Creates a new @class{gtk-check-button} widget containing a label.}
  The label will be created using the function
  @fun{gtk-label-new-with-mnemonic}, so underscores in label indicate the
  mnemonic for the check button.
  @see-function{gtk-label-new-with-mnemonic}"
  (label :string))

(export 'gtk-check-button-new-with-mnemonic)

;;; --- End of file gtk.check-button.lisp --------------------------------------
