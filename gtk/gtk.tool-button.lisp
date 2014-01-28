;;; ----------------------------------------------------------------------------
;;; gtk.tool-button.lisp
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
;;; GtkToolButton
;;;
;;; A GtkToolItem subclass that displays buttons
;;;
;;; Synopsis
;;;
;;;     GtkToolButton
;;;
;;;     gtk_tool_button_new
;;;     gtk_tool_button_new_from_stock
;;;     gtk_tool_button_set_label
;;;     gtk_tool_button_get_label
;;;     gtk_tool_button_set_use_underline
;;;     gtk_tool_button_get_use_underline
;;;     gtk_tool_button_set_stock_id
;;;     gtk_tool_button_get_stock_id
;;;     gtk_tool_button_set_icon_name
;;;     gtk_tool_button_get_icon_name
;;;     gtk_tool_button_set_icon_widget
;;;     gtk_tool_button_get_icon_widget
;;;     gtk_tool_button_set_label_widget
;;;     gtk_tool_button_get_label_widget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToolButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkToolButton" gtk-tool-button
  (:superclass gtk-tool-item
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActivatable")
    :type-initializer "gtk_tool_button_get_type")
  ((icon-name
    gtk-tool-button-icon-name
    "icon-name" "gchararray" t t)
   (icon-widget
    gtk-tool-button-icon-widget
    "icon-widget" "GtkWidget" t t)
   (label
    gtk-tool-button-label
    "label" "gchararray" t t)
   (label-widget
    gtk-tool-button-label-widget
    "label-widget" "GtkWidget" t t)
   (stock-id
    gtk-tool-button-stock-id
    "stock-id" "gchararray" t t)
   (use-underline
    gtk-tool-button-use-underline
    "use-underline" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tool-button 'type)
 "@version{2014-1-26}
  @begin{short}
    @sym{gtk-tool-button}'s are @class{gtk-tool-item}'s containing buttons.
  @end{short}

  Use the function @fun{gtk-tool-button-new} to create a new
  @sym{gtk-tool-button}. Use the function @fun{gtk-tool-button-new-from-stock}
  to create a @sym{gtk-tool-button} containing a stock item.

  The label of a @sym{gtk-tool-button} is determined by the properties
  @code{\"label-widget\"}, @code{\"label\"}, and @code{\"stock-id\"}. If
  @code{\"label-widget\"} is non-@code{nil}, then that widget is used as the
  label. Otherwise, if @code{\"label\"} is non-@code{nil}, that string is used
  as the label. Otherwise, if @code{\"stock-id\"} is non-@code{nil}, the label
  is determined by the stock item. Otherwise, the button does not have a label.

  The icon of a @sym{gtk-tool-button} is determined by the properties
  @code{\"icon-widget\"} and @code{\"stock-id\"}. If @code{\"icon-widget\"} is
  non-@code{nil}, then that widget is used as the icon. Otherwise, if
  @code{\"stock-id\"} is non-@code{nil}, the icon is determined by the stock
  item. Otherwise, the button does not have a icon.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"icon-spacing\" style property}
      @code{\"icon-spacing\"} of type @code{:int} (Read / Write) @br{}
      Spacing in pixels between the icon and label. @br{}
      Allowed values: >= 0 @br{}
      Default value: 3
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"clicked\" signal}
      @begin{pre}
 lambda (toolbutton)   : Action
      @end{pre}
      This signal is emitted when the tool button is clicked with the mouse or
      activated with the keyboard.
      @begin[code]{table}
        @entry[toolbutton]{The object that emitted the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-tool-button-icon-name}
  @see-slot{gtk-tool-button-icon-widget}
  @see-slot{gtk-tool-button-label}
  @see-slot{gtk-tool-button-label-widget}
  @see-slot{gtk-tool-button-stock-id}
  @see-slot{gtk-tool-button-use-underline}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-name" 'gtk-tool-button) 't)
 "The @code{\"icon-name\"} property of type @code{:string} (Read / Write) @br{}
  The name of the themed icon displayed on the item. This property only has an
  effect if not overridden by @code{\"label\"}, @code{\"icon-widget\"} or
  @code{\"stock-id\"} properties. @br{}
  Default value: @code{nil}
  Since 2.8")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon-widget"
                                               'gtk-tool-button) 't)
 "The @code{\"icon-widget\"} property of type @class{gtk-widget}
  (Read / Write) @br{}
  Icon widget to display in the item.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-tool-button) 't)
 "The @code{\"label\"} property of type @code{:string} (Read / Write) @br{}
  Text to show in the item. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-widget"
                                               'gtk-tool-button) 't)
 "The @code{\"label-widget\"} property of type @class{gtk-widget}
  (Read / Write) @br{}
  Widget to use as the item label.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "stock-id" 'gtk-tool-button) 't)
 "The @code{\"stock-id\"} property of type @code{:string} (Read / Write) @br{}
  The stock icon displayed on the item. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-underline"
                                               'gtk-tool-button) 't)
 "The @code{\"use-underline\"} property of type @code{:boolean}
  (Read / Write) @br{}
  If set, an underline in the label property indicates that the next character
  should be used for the mnemonic accelerator key in the overflow menu. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-button-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-button-icon-name 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{\"icon-name\"} of the @class{gtk-tool-button}
  class.
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-icon-name}
  @see-function{gtk-tool-button-set-icon-name}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-button-icon-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-button-icon-widget 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{\"icon-widget\"} of the @class{gtk-tool-button}
  class.
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-icon-widget}
  @see-function{gtk-tool-button-set-icon-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-button-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-button-label 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{\"label\"} of the @class{gtk-tool-button} class.
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-label}
  @see-function{gtk-tool-button-set-label}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-button-label-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-button-label-widget 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{\"label-widget\"} of the @class{gtk-tool-button}
  class.
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-label-widget}
  @see-function{gtk-tool-button-set-label-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-button-stock-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-button-stock-id 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{\"stock-id\"} of the @class{gtk-tool-button} class.
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-stock-id}
  @see-function{gtk-tool-button-set-stock-id}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-button-use-underline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-button-use-underline 'function)
 "@version{2013-8-23}
  Accessor of the slot @code{\"use-underline\"} of the @class{gtk-tool-button}
  class.
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-use-underline}
  @see-function{gtk-tool-button-set-use-underline}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-tool-button-new (icon-widget label)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[label]{a string that will be used as label, or @code{nil}}
  @argument[icon-widget]{a widget that will be used as the button contents,
    or @code{nil}}
  @return{A new @class{gtk-tool-button} widget}
  @begin{short}
    Creates a new @class{gtk-tool-button} using @arg{icon-widget} as contents
    and @arg{label} as label.
  @end{short}

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-new-from-stock}"
  (let ((tool-button (make-instance 'gtk-tool-button)))
    (when icon-widget
      (setf (gtk-tool-button-icon-widget tool-button) icon-widget))
    (when label
      (setf (gtk-tool-button-label tool-button) label))
    tool-button))

(export 'gtk-tool-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defun gtk-tool-button-new-from-stock (stock-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[stock-id]{the name of the stock item}
  @return{A new @class{gtk-tool-button} widget.}
  @begin{short}
    Creates a new @class{gtk-tool-button} containing the image and text from a
    stock item.
  @end{short}

  It is an error if @arg{stock-id} is not a name of a stock item.

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-new}"
  (make-instance 'gtk-tool-button
                 :stock-id stock-id))

(export 'gtk-tool-button-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_set_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-set-label))

(defun gtk-tool-button-set-label (button label)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @argument[label]{a string that will be used as label, or @code{nil}}
  @begin{short}
    Sets @arg{label} as the label used for the tool button.
  @end{short}
  The @code{\"label\"} property only has an effect if not overridden by a
  non-@code{nil} @code{\"label-widget\"} property. If both the
  @code{\"label-widget\"} and @code{\"label\"} properties are @code{nil}, the
  label is determined by the @code{\"stock-id\"} property. If the
  @code{\"stock-id\"} property is also @code{nil}, @arg{button} will not have a
  label.

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-label}"
  (setf (gtk-tool-button-label button) label))

(export 'gtk-tool-button-set-label)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_get_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-get-label))

(defun gtk-tool-button-get-label (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @return{The label, or @code{nil}.}
  @begin{short}
    Returns the label used by the tool button, or @code{nil} if the tool button
    does not have a label or uses a label from a stock item.
  @end{short}

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-set-label}"
  (gtk-tool-button-label button))

(export 'gtk-tool-button-get-label)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_set_use_underline ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-set-use-underline))

(defun gtk-tool-button-set-use-underline (button use-underline)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @argument[use-underline]{whether the button label has the form \"_Open\"}
  @begin{short}
    If set, an underline in the label property indicates that the next character
    should be used for the mnemonic accelerator key in the overflow menu.
  @end{short}
  For example, if the label property is \"_Open\" and @arg{use-underline} is
  @em{true}, the label on the tool button will be \"Open\" and the item on the
  overflow menu will have an underlined 'O'.

  Labels shown on tool buttons never have mnemonics on them; this property
  only affects the menu item on the overflow menu.

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-use-underline}"
  (setf (gtk-tool-button-use-underline button) use-underline))

(export 'gtk-tool-button-set-use-underline)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_get_use_underline ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-get-use-underline))

(defun gtk-tool-button-get-use-underline (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @begin{return}
    @em{True} if underscores in the label property are used as mnemonics on menu
    items on the overflow menu.
  @end{return}
  @begin{short}
    Returns whether underscores in the label property are used as mnemonics on
    menu items on the overflow menu.
  @end{short}
  See the function @fun{gtk-tool-button-set-use-underline}.

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-set-use-underline}"
  (gtk-tool-button-use-underline button))

(export 'gtk-tool-button-get-use-underline)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_set_stock_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-set-stock-id))

(defun gtk-tool-button-set-stock-id (button stock-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @argument[stock-id]{a name of a stock item, or @code{nil}}
  @begin{short}
    Sets the name of the stock item.
  @end{short}
  See the function @fun{gtk-tool-button-new-from-stock}. The @code{\"stock-id\"}
  property only has an effect if not overridden by non-@code{nil}
  @code{\"label\"} and @code{\"icon-widget\"} properties.

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-stock-id}"
  (setf (gtk-tool-button-stock-id button) stock-id))

(export 'gtk-tool-button-set-stock-id)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_get_stock_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-get-stock-id))

(defun gtk-tool-button-get-stock-id (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @return{The name of the stock item for @arg{button}.}
  @begin{short}
    Returns the name of the stock item.
  @end{short}
  See the function @fun{gtk-tool-button-set-stock-id}.

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-set-stock-id}"
  (gtk-tool-button-stock-id button))

(export 'gtk-tool-button-get-stock-id)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_set_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-set-icon-name))

(defun gtk-tool-button-set-icon-name (button icon-name)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @argument[icon-name]{the name of the themed icon}
  @begin{short}
    Sets the icon for the tool button from a named themed icon.
  @end{short}
  See the docs for @class{gtk-icon-theme} for more details. The
  @code{\"icon-name\"} property only has an effect if not overridden by
  non-@code{nil} @code{\"label\"}, @code{\"icon-widget\"} and
  @code{\"stock-id\"} properties.

  Since 2.8
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-icon-name}"
  (setf (gtk-tool-button-icon-name button) icon-name))

(export 'gtk-tool-button-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_get_icon_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-get-icon-name))

(defun gtk-tool-button-get-icon-name (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @return{The icon name or @code{nil} if the tool button has no themed icon.}
  @begin{short}
    Returns the name of the themed icon for the tool button.
  @end{short}
  See the function @fun{gtk-tool-button-set-icon-name}.

  Since 2.8
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-set-icon-name}"
  (gtk-tool-button-icon-name button))

(export 'gtk-tool-button-get-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_set_icon_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-set-icon-widget))

(defun gtk-tool-button-set-icon-widget (button icon-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @argument[icon-widget]{the widget used as icon, or @code{nil}}
  @begin{short}
    Sets icon as the widget used as icon on @arg{button}.
  @end{short}
  If @arg{icon-widget} is @code{nil} the icon is determined by the
  @code{\"stock-id\"} property. If the @code{\"stock-id\"} property is also
  @code{nil}, @arg{button} will not have an icon.

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-icon-widget}"
  (setf (gtk-tool-button-icon-widget button) icon-widget))

(export 'gtk-tool-button-set-icon-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_get_icon_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-get-icon-widget))

(defun gtk-tool-button-get-icon-widget (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @return{The widget used as icon on @arg{button}, or @code{nil}.}
  @begin{short}
    Return the widget used as icon widget on @arg{button}.
  @end{short}
  See the function @fun{gtk-tool-button-set-icon-widget}.

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-set-icon-widget}"
  (gtk-tool-button-icon-widget button))

(export 'gtk-tool-button-get-icon-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_set_label_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-set-label-widget))

(defun gtk-tool-button-set-label-widget (button label-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @argument[label-widget]{the widget used as label, or @code{nil}}
  @begin{short}
    Sets @arg{label-widget} as the widget that will be used as the label for
    @arg{button}.
  @end{short}
  If @arg{label-widget} is @code{nil} the @code{\"label\"} property is used as
  label. If @code{\"label\"} is also @code{nil}, the label in the stock item
  determined by the @code{\"stock-id\"} property is used as label. If
  @code{\"stock-id\"} is also @code{nil}, @arg{button} does not have a label.

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-get-label-widget}"
  (setf (gtk-tool-button-label-widget button) label-widget))

(export 'gtk-tool-button-set-label-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_get_label_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-button-get-label-widget))

(defun gtk-tool-button-get-label-widget (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-23}
  @argument[button]{a @class{gtk-tool-button} widget}
  @return{The widget used as label on @arg{button}, or @code{nil}.}
  @begin{short}
    Returns the widget used as label on @arg{button}.
  @end{short}
  See the function @fun{gtk-tool-button-set-label-widget}.

  Since 2.4
  @see-class{gtk-tool-button}
  @see-function{gtk-tool-button-set-label-widget}"
  (gtk-tool-button-label-widget button))

(export 'gtk-tool-button-get-label-widget)

;;; --- End of file gtk.tool-button.lisp ---------------------------------------
