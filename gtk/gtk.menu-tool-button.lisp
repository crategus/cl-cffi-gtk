;;; ----------------------------------------------------------------------------
;;; gtk.menu-tool-button.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; GtkMenuToolButton
;;; 
;;; A GtkToolItem containing a button with an additional dropdown menu
;;; 
;;; Synopsis
;;; 
;;;     GtkMenuToolButton
;;;
;;;     gtk_menu_tool_button_new
;;;     gtk_menu_tool_button_new_from_stock
;;;     gtk_menu_tool_button_set_menu
;;;     gtk_menu_tool_button_get_menu
;;;     gtk_menu_tool_button_set_arrow_tooltip_text
;;;     gtk_menu_tool_button_set_arrow_tooltip_markup
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkToolItem
;;;                                  +----GtkToolButton
;;;                                        +----GtkMenuToolButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkMenuToolButton implements AtkImplementorIface, GtkBuildable and
;;; GtkActivatable.
;;;
;;; Properties
;;; 
;;;   "menu"                     GtkMenu*              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "show-menu"                                      : Run First
;;; 
;;; Description
;;; 
;;; A GtkMenuToolButton is a GtkToolItem that contains a button and a small
;;; additional button with an arrow. When clicked, the arrow button pops up a
;;; dropdown menu.
;;; 
;;; Use gtk_menu_tool_button_new() to create a new GtkMenuToolButton.
;;; Use gtk_menu_tool_button_new_from_stock() to create a new GtkMenuToolButton
;;; containing a stock item.
;;; 
;;; GtkMenuToolButton as GtkBuildable
;;; 
;;; The GtkMenuToolButton implementation of the GtkBuildable interface supports
;;; adding a menu by specifying "menu" as the "type" attribute of a <child>
;;; element.
;;; 
;;; Example 79. A UI definition fragment with menus
;;; 
;;; <object class="GtkMenuToolButton">
;;;   <child type="menu">
;;;     <object class="GtkMenu"/>
;;;   </child>
;;; </object>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "menu" property
;;; 
;;;   "menu"                     GtkMenu*              : Read / Write
;;; 
;;; The dropdown menu.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-menu" signal
;;; 
;;; void user_function (GtkMenuToolButton *button,
;;;                     gpointer           user_data)      : Run First
;;; 
;;; The ::show-menu signal is emitted before the menu is shown.
;;; 
;;; It can be used to populate the menu on demand, using
;;; gtk_menu_tool_button_get_menu().
;;; 
;;; Note that even if you populate the menu dynamically in this way, you must
;;; set an empty menu on the GtkMenuToolButton beforehand, since the arrow is
;;; made insensitive if the menu is not set.
;;; 
;;; button :
;;;     the object on which the signal is emitted
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuToolButton
;;; 
;;; struct GtkMenuToolButton;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuToolButton" gtk-menu-tool-button
  (:superclass gtk-tool-button
   :export t
   :interfaces ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
   :type-initializer "gtk_menu_tool_button_get_type")
  ((menu
    gtk-menu-tool-button-menu
    "menu" "GtkMenu" t t)
   (:cffi arrow-tooltip-text
          gtk-menu-tool-button-arrow-tooltip-text :string
          nil "gtk_menu_tool_button_set_arrow_tooltip_text")
   (:cffi arrow-tooltip-markup
          gtk-menu-tool-button-arrow-tooltip-markup :string
          nil "gtk_menu_tool_button_set_arrow_tooltip_markup")))

;;; ----------------------------------------------------------------------------
;;; gtk_menu_tool_button_new ()
;;; 
;;; GtkToolItem * gtk_menu_tool_button_new (GtkWidget *icon_widget,
;;;                                         const gchar *label);
;;; 
;;; Creates a new GtkMenuToolButton using icon_widget as icon and label as
;;; label.
;;; 
;;; icon_widget :
;;;     a widget that will be used as icon widget, or NULL
;;; 
;;; label :
;;;     a string that will be used as label, or NULL
;;; 
;;; Returns :
;;;     the new GtkMenuToolButton
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_tool_button_new_from_stock ()
;;; 
;;; GtkToolItem * gtk_menu_tool_button_new_from_stock (const gchar *stock_id)
;;; 
;;; Creates a new GtkMenuToolButton. The new GtkMenuToolButton will contain an
;;; icon and label from the stock item indicated by stock_id.
;;; 
;;; stock_id :
;;;     the name of a stock item
;;; 
;;; Returns :
;;;     the new GtkMenuToolButton
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_tool_button_set_menu ()
;;; 
;;; void gtk_menu_tool_button_set_menu (GtkMenuToolButton *button,
;;;                                     GtkWidget *menu);
;;; 
;;; Sets the GtkMenu that is popped up when the user clicks on the arrow. If
;;; menu is NULL, the arrow button becomes insensitive.
;;; 
;;; button :
;;;     a GtkMenuToolButton
;;; 
;;; menu :
;;;     the GtkMenu associated with GtkMenuToolButton
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_tool_button_get_menu ()
;;; 
;;; GtkWidget * gtk_menu_tool_button_get_menu (GtkMenuToolButton *button)
;;; 
;;; Gets the GtkMenu associated with GtkMenuToolButton.
;;; 
;;; button :
;;;     a GtkMenuToolButton
;;; 
;;; Returns :
;;;     the GtkMenu associated with GtkMenuToolButton. [transfer none]
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_tool_button_set_arrow_tooltip_text ()
;;; 
;;; void gtk_menu_tool_button_set_arrow_tooltip_text (GtkMenuToolButton *button,
;;;                                                   const gchar *text);
;;; 
;;; Sets the tooltip text to be used as tooltip for the arrow button which pops
;;; up the menu. See gtk_tool_item_set_tooltip_text() for setting a tooltip on
;;; the whole GtkMenuToolButton.
;;; 
;;; button :
;;;     a GtkMenuToolButton
;;; 
;;; text :
;;;     text to be used as tooltip text for button's arrow button
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_menu_tool_button_set_arrow_tooltip_markup ()
;;; 
;;; void gtk_menu_tool_button_set_arrow_tooltip_markup
;;;                                                  (GtkMenuToolButton *button,
;;;                                                   const gchar *markup);
;;; 
;;; Sets the tooltip markup text to be used as tooltip for the arrow button
;;; which pops up the menu. See gtk_tool_item_set_tooltip_text() for setting
;;; a tooltip on the whole GtkMenuToolButton.
;;; 
;;; button :
;;;     a GtkMenuToolButton
;;; 
;;; markup :
;;;     markup text to be used as tooltip text for button's arrow button
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.menu-tool-button.lisp ----------------------------------
