;;; ----------------------------------------------------------------------------
;;; gtk.toggle-tool-button.lisp
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
;;;                                        +----GtkToggleToolButton
;;;                                              +----GtkRadioToolButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkToggleToolButton implements AtkImplementorIface, GtkBuildable
;;; and GtkActivatable.
;;;
;;; Properties
;;; 
;;;   "active"                   gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "toggled"                                        : Run First
;;; 
;;; Description
;;; 
;;; A GtkToggleToolButton is a GtkToolItem that contains a toggle button.
;;; 
;;; Use gtk_toggle_tool_button_new() to create a new GtkToggleToolButton. Use
;;; gtk_toggle_tool_button_new_from_stock() to create a new GtkToggleToolButton
;;; containing a stock item.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "active" property
;;; 
;;;   "active"                   gboolean              : Read / Write
;;; 
;;; If the toggle tool button should be pressed in.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.8
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggled" signal
;;; 
;;; void user_function (GtkToggleToolButton *toggle_tool_button,
;;;                     gpointer             user_data)              : Run First
;;; 
;;; Emitted whenever the toggle tool button changes state.
;;; 
;;; toggle_tool_button :
;;;     the object that emitted the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToggleToolButton
;;; 
;;; struct GtkToggleToolButton;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkToggleToolButton" gtk-toggle-tool-button
  (:superclass gtk-tool-button
   :export t
   :interfaces ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
   :type-initializer "gtk_toggle_tool_button_get_type")
  ((active gtk-toggle-tool-button-active
    "active" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_new ()
;;; 
;;; GtkToolItem * gtk_toggle_tool_button_new (void);
;;; 
;;; Returns a new GtkToggleToolButton
;;; 
;;; Returns :
;;;     a newly created GtkToggleToolButton
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_new_from_stock ()
;;; 
;;; GtkToolItem * gtk_toggle_tool_button_new_from_stock (const gchar *stock_id);
;;; 
;;; Creates a new GtkToggleToolButton containing the image and text from a stock
;;; item. Some stock ids have preprocessor macros like
;;; GTK_STOCK_OK and GTK_STOCK_APPLY.
;;; 
;;; It is an error if stock_id is not a name of a stock item.
;;; 
;;; stock_id :
;;;     the name of the stock item
;;; 
;;; Returns :
;;;     A new GtkToggleToolButton
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_set_active ()
;;; 
;;; void gtk_toggle_tool_button_set_active (GtkToggleToolButton *button,
;;;                                         gboolean is_active);
;;; 
;;; Sets the status of the toggle tool button. Set to TRUE if you want the
;;; GtkToggleButton to be 'pressed in', and FALSE to raise it. This action
;;; causes the toggled signal to be emitted.
;;; 
;;; button :
;;;     a GtkToggleToolButton
;;; 
;;; is_active :
;;;     whether button should be active
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_get_active ()
;;; 
;;; gboolean gtk_toggle_tool_button_get_active (GtkToggleToolButton *button);
;;; 
;;; Queries a GtkToggleToolButton and returns its current state. Returns TRUE if
;;; the toggle button is pressed in and FALSE if it is raised.
;;; 
;;; button :
;;;     a GtkToggleToolButton
;;; 
;;; Returns :
;;;     TRUE if the toggle tool button is pressed in, FALSE if not
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.toggle-tool-button.lisp --------------------------------
