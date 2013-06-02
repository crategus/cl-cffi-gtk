;;; ----------------------------------------------------------------------------
;;; gtk.toggle-tool-button.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
 "@version{2013-6-1}
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
 lambda (toggle-tool-button)   : Run First
      @end{pre}
      Emitted whenever the toggle tool button changes state.
      @begin[code]{table}
        @entry[toggle-tool-button]{The object that emitted the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-toggle-tool-button-active}")

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
 "@version{2013-3-25}
  Accessor of the slot \"active\" of the @class{gtk-toggle-tool-button} class.")

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
;;; item. Some stock ids have preprocessor macros like GTK_STOCK_OK and
;;; GTK_STOCK_APPLY.
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
