;;; ----------------------------------------------------------------------------
;;; gtk.radio-tool-button.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkRadioToolButton
;;; 
;;; A toolbar item that contains a radio button
;;;     
;;; Synopsis
;;; 
;;;     GtkRadioToolButton
;;;
;;;     gtk_radio_tool_button_new
;;;     gtk_radio_tool_button_new_from_stock
;;;     gtk_radio_tool_button_new_from_widget
;;;     gtk_radio_tool_button_new_with_stock_from_widget
;;;     gtk_radio_tool_button_get_group
;;;     gtk_radio_tool_button_set_group
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRadioToolButton
;;; 
;;; struct GtkRadioToolButton;
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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-radio-tool-button 'type)
 "@version{2013-3-18}
  @begin{short}
    A @class{gtk-radio-tool-button} is a @class{gtk-tool-item} that contains a
    radio button, that is, a button that is part of a group of toggle buttons
    where only one button can be active at a time.
  @end{short}

  Use @fun{gtk-radio-tool-button-new} to create a new
  @sym{gtk-radio-tool-button}. Use @fun{gtk-radio-tool-button-new-from-widget}
  to create a new @sym{gtk-radio-tool-button} that is part of the same group as
  an existing @sym{gtk-radio-tool-button}. Use
  @fun{gtk-radio-tool-button-new-from-stock} or
  @fun{gtk-radio-tool-button-new-with-stock-from-widget} create a new
  @sym{gtk-radio-tool-button} containing a stock item.
  @see-slot{gtk-radio-tool-button-group}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group"
                                               'gtk-radio-tool-button) 't)
 "The @code{\"group\"} property of type @code{gtk-radio-tool-button}
  (Write)@br{}
  Sets a new group for a radio tool button.@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-tool-button-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-tool-button-group 'function)
 "@version{2013-3-18}
  Accessor of the slot @code{\"group\"} of the @class{gtk-radio-tool-button}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new ()
;;; 
;;; GtkToolItem * gtk_radio_tool_button_new (GSList *group);
;;; 
;;; Creates a new GtkRadioToolButton, adding it to group.
;;; 
;;; group :
;;;     An existing radio button group, or NULL if you are creating a new group.
;;; 
;;; Returns :
;;;     The new GtkRadioToolButton
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new_from_stock ()
;;; 
;;; GtkToolItem * gtk_radio_tool_button_new_from_stock (GSList *group,
;;;                                                     const gchar *stock_id);
;;; 
;;; Creates a new GtkRadioToolButton, adding it to group. The new
;;; GtkRadioToolButton will contain an icon and label from the stock item
;;; indicated by stock_id.
;;; 
;;; group :
;;;     an existing radio button group, or NULL if you are creating a new group.
;;; 
;;; stock_id :
;;;     the name of a stock item
;;; 
;;; Returns :
;;;     The new GtkRadioToolItem
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new_from_widget ()
;;; 
;;; GtkToolItem * gtk_radio_tool_button_new_from_widget
;;;                                                 (GtkRadioToolButton *group);
;;; 
;;; Creates a new GtkRadioToolButton adding it to the same group as group
;;; 
;;; group :
;;;     An existing GtkRadioToolButton, or NULL
;;; 
;;; Returns :
;;;     The new GtkRadioToolButton
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new_with_stock_from_widget ()
;;; 
;;; GtkToolItem * gtk_radio_tool_button_new_with_stock_from_widget
;;;                                                  (GtkRadioToolButton *group,
;;;                                                   const gchar *stock_id);
;;; 
;;; Creates a new GtkRadioToolButton adding it to the same group as group. The
;;; new GtkRadioToolButton will contain an icon and label from the stock item
;;; indicated by stock_id.
;;; 
;;; group :
;;;     An existing GtkRadioToolButton
;;; 
;;; stock_id :
;;;     the name of a stock item
;;; 
;;; Returns :
;;;     A new GtkRadioToolButton
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_get_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_tool_button_get_group" gtk-radio-tool-button-get-group)
    (g-slist (g-object gtk-radio-tool-button) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-18}
  @argument[button]{a @class{gtk-radio-tool-button} widget}
  @return{The group button belongs to.}
  @short{Returns the radio button group button belongs to.}

  Since 2.4"
  (button (g-object gtk-radio-tool-button)))

(export 'gtk-radio-tool-button-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_set_group ()
;;; 
;;; void gtk_radio_tool_button_set_group (GtkRadioToolButton *button,
;;;                                       GSList *group);
;;; 
;;; Adds button to group, removing it from the group it belonged to before.
;;; 
;;; button :
;;;     a GtkRadioToolButton
;;; 
;;; group :
;;;     an existing radio button group
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.radio-tool-button.lisp ---------------------------------
