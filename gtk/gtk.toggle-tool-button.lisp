;;; ----------------------------------------------------------------------------
;;; gtk.toggle-tool-button.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkToggleToolButton
;;;
;;; A GtkToolItem containing a toggle button
;;;
;;; Synopsis
;;;
;;;     GtkToggleToolButton
;;;
;;;     gtk_toggle_tool_button_new
;;;     gtk_toggle_tool_button_new_from_stock
;;;     gtk_toggle_tool_button_set_active
;;;     gtk_toggle_tool_button_get_active
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToggleToolButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkToggleToolButton" gtk-toggle-tool-button
  (:superclass gtk-tool-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable"
                "GtkActionable")
   :type-initializer "gtk_toggle_tool_button_get_type")
  ((active
    gtk-toggle-tool-button-active
    "active" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-toggle-tool-button 'type)
 "@version{2013-8-23}
  @begin{short}
    A @sym{gtk-toggle-tool-button} is a @class{gtk-tool-item} that contains a
    toggle button.
  @end{short}

  Use the @fun{gtk-toggle-tool-button-new} function to create a new
  @sym{gtk-toggle-tool-button}. Use the
  @fun{gtk-toggle-tool-button-new-from-stock} to create a new
  @sym{gtk-toggle-tool-button} containing a stock item.
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
 lambda (button)   : Run First
      @end{pre}
      Emitted whenever the toggle tool button changes state.
      @begin[code]{table}
        @entry[button]{The object that emitted the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-toggle-tool-button-active}
  @see-class{gtk-tool-item}
  @see-function{gtk-toggle-tool-button-new}
  @see-function{gtk-toggle-tool-button-new-from-stock}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active"
                                               'gtk-toggle-tool-button) 't)
 "The @code{\"active\"} property of type @code{:boolean} (Read / Write) @br{}
  If the toggle tool button should be pressed in. @br{}
  Default value: @code{nil} @br{}
  Since 2.8")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toggle-tool-button-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toggle-tool-button-active 'function)
 "@version{2013-8-23}
  Accessor of the slot \"active\" of the @class{gtk-toggle-tool-button} class.
  @see-class{gtk-toggle-tool-button}
  @see-function{gtk-toggle-tool-button-get-active}
  @see-function{gtk-toggle-tool-button-set-active}")

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toggle-tool-button-new))

(defun gtk-toggle-tool-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @return{A newly created @class{gtk-toggle-tool-button} widget}
  @begin{short}
    Returns a new @class{gtk-toggle-tool-button} widget.
  @end{short}

  Since 2.4
  @see-class{gtk-toggle-tool-button}
  @see-function{gtk-toggle-tool-button-new-from-stock}"
  (make-instance 'gtk-toggle-tool-button))

(export 'gtk-toggle-tool-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_new_from_stock ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toggle-tool-button-new-from-stock))

(defun gtk-toggle-tool-button-new-from-stock (stock-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[stock-id]{the name of the stock item}
  @return{A new @class{gtk-toggle-tool-button} widget.}
  @begin{short}
    Creates a new @class{gtk-toggle-tool-button} widget containing the image and
    text from a stock item.
  @end{short}

  It is an error if @arg{stock-id} is not a name of a stock item.

  Since 2.4
  @see-class{gtk-toggle-tool-button}
  @see-function{gtk-toggle-tool-button-new}"
  (make-instance 'gtk-toggle-tool-button
                 :stock-id stock-id))

(export 'gtk-toggle-tool-button-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_set_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toggle-tool-button-set-active))

(defun gtk-toggle-tool-button-set-active (button is-active)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-toggle-tool-button} widget}
  @argument[is-active]{whether @arg{button} should be active}
  @begin{short}
    Sets the status of the toggle tool button.
  @end{short}
  Set to @em{true} if you want the @class{gtk-toggle-button} widget to be
  'pressed in', and @code{nil} to raise it. This action causes the toggled
  signal to be emitted.

  Since 2.4
  @see-class{gtk-toggle-tool-button}
  @see-function{gtk-toggle-tool-button-get-active}"
  (setf (gtk-toggle-tool-button-active button) is-active))

(export 'gtk-toggle-tool-button-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_get_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toggle-tool-button-get-active))

(defun gtk-toggle-tool-button-get-active (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-toggle-tool-button} widget}
  @return{@em{True} if the toggle tool button is pressed in, @code{nil} if not.}
  @begin{short}
    Queries a @class{gtk-toggle-tool-button} and returns its current state.
  @end{short}
  Returns @em{true} if the toggle button is pressed in and @code{nil} if it
  is raised.

  Since 2.4
  @see-class{gtk-toggle-tool-button}
  @see-function{gtk-toggle-tool-button-set-active}"
  (gtk-toggle-tool-button-active button))

(export 'gtk-toggle-tool-button-get-active)

;;; --- End of file gtk.toggle-tool-button.lisp --------------------------------
