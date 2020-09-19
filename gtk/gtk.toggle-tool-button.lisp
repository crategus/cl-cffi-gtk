;;; ----------------------------------------------------------------------------
;;; gtk.toggle-tool-button.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     A GtkToolItem containing a toggle button
;;;
;;; Types and Values
;;;
;;;     GtkToggleToolButton
;;;
;;; Functions
;;;
;;;     gtk_toggle_tool_button_new
;;;     gtk_toggle_tool_button_new_from_stock
;;;     gtk_toggle_tool_button_set_active                  Accessor
;;;     gtk_toggle_tool_button_get_active                  Accessor
;;;
;;; Properties
;;;
;;;     gboolean    active     Read / Write
;;;
;;; Signals
;;;
;;;         void    toggled    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkToolItem
;;;                         ╰── GtkToolButton
;;;                             ╰── GtkToggleToolButton
;;;                                 ╰── GtkRadioToolButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToggleToolButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
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
 "@version{2020-9-5}
  @begin{short}
    A @sym{gtk-toggle-tool-button} is a @class{gtk-tool-item} that contains a
    toggle button.
  @end{short}

  Use the function @fun{gtk-toggle-tool-button-new} to create a new toggle tool
  button.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-toggle-tool-button} class has a single CSS node with name
    @code{togglebutton}.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
 lambda (button)    : Run First
      @end{pre}
      Emitted whenever the toggle tool button changes state.
      @begin[code]{table}
        @entry[button]{The @sym{gtk-toggle-tool-button} widget that emitted the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-toggle-tool-button-active}
  @see-class{gtk-tool-item}
  @see-function{gtk-toggle-tool-button-new}
  @see-function{gtk-toggle-tool-button-new-from-stock}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active"
                                               'gtk-toggle-tool-button) 't)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  If the toggle tool button should be pressed in. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-toggle-tool-button-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-toggle-tool-button-active 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-toggle-tool-button-active object) => is-active}
  @syntax[]{(setf (gtk-toggle-tool-button-active object) is-active)}
  @argument[object]{a @class{gtk-toggle-tool-button} widget}
  @argument[is-active]{a @code{:boolean} whether @arg{button} should be active}
  @begin{short}
    Accessor of the @slot[gtk-toggle-tool-button]{active} slot of the
    @class{gtk-toggle-tool-button} class.
  @end{short}

  The slot access function @sym{gtk-toggle-tool-button-active} queries a toggle
  tool button and returns its current state. The slot access function
  @sym{(setf gtk-toggle-tool-button-active)} sets the status of the toggle tool
  button.

  Set to @em{true} if you want the toggle tool button to be 'pressed in',
  and @em{false} to raise it. This action causes the toggled signal to be
  emitted.
  @see-class{gtk-toggle-tool-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-toggle-tool-button-new))

(defun gtk-toggle-tool-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-9-5}
  @return{A newly created @class{gtk-toggle-tool-button} widget.}
  @begin{short}
    Returns a new toggle tool button.
  @end{short}
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
 "@version{2020-9-5}
  @argument[stock-id]{a @code{:string} with the name of the stock item}
  @return{A new @class{gtk-toggle-tool-button} widget.}
  @begin{short}
    Creates a new toggle tool button containing the image and text from a stock
    item.
  @end{short}

  It is an error if @arg{stock-id} is not a name of a stock item.
  @begin[Warning]{dictionary}
    The function @sym{gtk-toggle-tool-button-new-from-stock} has been deprecated
    since version 3.10 and should not be used in newly-written code. Use the
    function @fun{gtk-toggle-tool-button-new} instead.
  @end{dictionary}
  @see-class{gtk-toggle-tool-button}
  @see-function{gtk-toggle-tool-button-new}"
  (make-instance 'gtk-toggle-tool-button
                 :stock-id stock-id))

(export 'gtk-toggle-tool-button-new-from-stock)

;;; --- End of file gtk.toggle-tool-button.lisp --------------------------------
