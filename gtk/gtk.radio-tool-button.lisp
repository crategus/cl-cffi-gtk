;;; ----------------------------------------------------------------------------
;;; gtk.radio-tool-button.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkRadioToolButton
;;;
;;;     A toolbar item that contains a radio button
;;;
;;; Types and Values
;;;
;;;     GtkRadioToolButton
;;;
;;; Functions
;;;
;;;     gtk_radio_tool_button_new
;;;     gtk_radio_tool_button_new_from_stock
;;;     gtk_radio_tool_button_new_from_widget
;;;     gtk_radio_tool_button_new_with_stock_from_widget
;;;     gtk_radio_tool_button_get_group
;;;     gtk_radio_tool_button_set_group
;;;
;;; Properties
;;;
;;;     GtkRadioToolButton*  group    Write
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
;;;     GtkRadioToolButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRadioToolButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRadioToolButton" gtk-radio-tool-button
  (:superclass gtk-toggle-tool-button
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActivatable"
                 "GtkActionable")
    :type-initializer "gtk_radio_tool_button_get_type")
  ((group
    gtk-radio-tool-button-group
    "group" "GtkRadioToolButton" nil t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-radio-tool-button 'type)
 "@version{2013-4-3}
  @begin{short}
    A @sym{gtk-radio-tool-button} is a @class{gtk-tool-item} that contains a
    radio button, that is, a button that is part of a group of toggle buttons
    where only one button can be active at a time.
  @end{short}

  Use the @fun{gtk-radio-tool-button-new} function to create a new
  @sym{gtk-radio-tool-button}. Use the
  @fun{gtk-radio-tool-button-new-from-widget} function to create a new
  @sym{gtk-radio-tool-button} that is part of the same group as an existing
  @sym{gtk-radio-tool-button}. Use the
  @fun{gtk-radio-tool-button-new-from-stock} or
  @fun{gtk-radio-tool-button-new-with-stock-from-widget} functions create a new
  @sym{gtk-radio-tool-button} containing a stock item.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-radio-tool-button} has a single CSS node with name
    @code{toolbutton}.
  @end{dictionary}
  @see-slot{gtk-radio-tool-button-group}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group"
                                               'gtk-radio-tool-button) 't)
 "The @code{group} property of type @code{gtk-radio-tool-button}
  (Write) @br{}
  Sets a new group for a radio tool button.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-tool-button-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-tool-button-group 'function)
 "@version{2019-4-6}
  @begin{short}
    Accessor of the slot @slot[gtk-radio-tool-button]{group} of the
    @class{gtk-radio-tool-button} class.
  @end{short}
  @see-class{gtk-radio-tool-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_tool_button_new" gtk-radio-tool-button-new)
    (g-object gtk-tool-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[group]{an existing radio button group, or @code{nil} if you are
    creating a new group}
  @return{The new @class{gtk-radio-tool-button}.}
  @begin{short}
    Creates a new @class{gtk-radio-tool-button}, adding it to @arg{group}.
  @end{short}
  @see-class{gtk-radio-tool-button}"
  (group (g-slist (g-object gtk-radio-button))))

(export 'gtk-radio-tool-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_tool_button_new_from_stock"
           gtk-radio-tool-button-new-from-stock) (g-object gtk-tool-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-14}
  @argument[group]{an existing radio button group, or @code{nil} if you are
    creating a new group}
  @argument[stock-id]{the name of a stock item}
  @return{The new @class{gtk-radio-tool-button}.}
  @begin{short}
    Creates a new @class{gtk-radio-tool-button}, adding it to @arg{group}. The
    new @class{gtk-radio-tool-button} will contain an icon and label from the
    stock item indicated by @arg{stock-id}.
  @end{short}
  @see-class{gtk-radio-tool-button}"
  (group (g-slist (g-object gtk-radio-button)))
  (stock-id :string))

(export 'gtk-radio-tool-button-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_tool_button_new_from_widget"
           gtk-radio-tool-button-new-from-widget)
    (g-object gtk-tool-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[group]{an existing @class{gtk-radio-tool-button}, or @code{nil}}
  @return{The new @class{gtk-radio-tool-button}.}
  @begin{short}
    Creates a new @class{gtk-radio-tool-button} adding it to the same group as
    @arg{group}.
  @end{short}
  @see-class{gtk-radio-tool-button}"
  (group (g-object gtk-radio-tool-button)))

(export 'gtk-radio-tool-button-new-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new_with_stock_from_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_tool_button_new_with_stock_from_widget"
           gtk-radio-tool-button-new-with-stock-from-widget)
    (g-object gtk-tool-item)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[group]{an existing @class{gtk-radio-tool-button} widget}
  @argument[stock-id]{the name of a stock item}
  @return{A new @class{gtk-radio-tool-button}.}
  @begin{short}
    Creates a new @class{gtk-radio-tool-button} adding it to the same group as
    @arg{group}. The new @class{gtk-radio-tool-button} will contain an icon and
    label from the stock item indicated by @arg{stock-id}.
  @end{short}
  @see-class{gtk-radio-tool-button}"
  (group (g-object gtk-radio-tool-button))
  (stock-id :string))

(export 'gtk-radio-tool-button-new-with-stock-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_get_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_tool_button_get_group" gtk-radio-tool-button-get-group)
    (g-slist (g-object gtk-radio-button) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[button]{a @class{gtk-radio-tool-button} widget}
  @return{The group @arg{button} belongs to.}
  @short{Returns the radio button group @arg{button} belongs to.}
  @see-class{gtk-radio-tool-button}"
  (button (g-object gtk-radio-tool-button)))

(export 'gtk-radio-tool-button-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_set_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_tool_button_set_group" gtk-radio-tool-button-set-group)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-4-3}
  @argument[button]{a @class{gtk-radio-tool-button} widget}
  @argument[group]{an existing radio button group}
  @begin{short}
    Adds @arg{button} to @arg{group}, removing it from the group it belonged to
    before.
  @end{short}
  @see-class{gtk-radio-tool-button}"
  (button (g-object gtk-radio-tool-button))
  (group (g-slist (g-object gtk-radio-button))))

(export 'gtk-radio-tool-button-set-group)

;;; --- End of file gtk.radio-tool-button.lisp ---------------------------------
