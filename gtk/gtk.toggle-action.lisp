;;; ----------------------------------------------------------------------------
;;; gtk.toggle-action.lisp
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
;;; GtkToggleAction
;;; 
;;; An action which can be toggled between two states
;;; 	
;;; Synopsis
;;; 
;;;     GtkToggleAction
;;;     
;;;     gtk_toggle_action_new
;;;     gtk_toggle_action_toggled
;;;     gtk_toggle_action_set_active
;;;     gtk_toggle_action_get_active
;;;     gtk_toggle_action_set_draw_as_radio
;;;     gtk_toggle_action_get_draw_as_radio
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GtkAction
;;;          +----GtkToggleAction
;;;                +----GtkRadioAction
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkToggleAction implements GtkBuildable.
;;; Properties
;;; 
;;;   "active"                   gboolean              : Read / Write
;;;   "draw-as-radio"            gboolean              : Read / Write
;;; 
;;; Signals
;;; 
;;;   "toggled"                                        : Run First
;;; 
;;; Description
;;; 
;;; A GtkToggleAction corresponds roughly to a GtkCheckMenuItem. It has an
;;; "active" state specifying whether the action has been checked or not.
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
;;; Whether the toggle action should be active.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "draw-as-radio" property
;;; 
;;;   "draw-as-radio"            gboolean              : Read / Write
;;; 
;;; Whether the proxies for this action look like radio action proxies.
;;; 
;;; This is an appearance property and thus only applies if
;;; "use-action-appearance" is TRUE.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "toggled" signal
;;; 
;;; void user_function (GtkToggleAction *toggleaction,
;;;                     gpointer         user_data)         : Run First
;;; 
;;; Should be connected if you wish to perform an action whenever the
;;; GtkToggleAction state is changed.
;;; 
;;; toggleaction :
;;;     the object which received the signal.
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToggleAction
;;; 
;;; struct GtkToggleAction;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkToggleAction" gtk-toggle-action
  (:superclass gtk-action
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_toggle_action_get_type")
  ((active gtk-toggle-action-active
    "active" "gboolean" t t)
   (draw-as-radio gtk-toggle-action-draw-as-radio
    "draw-as-radio" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_new ()
;;; 
;;; GtkToggleAction * gtk_toggle_action_new (const gchar *name,
;;;                                          const gchar *label,
;;;                                          const gchar *tooltip,
;;;                                          const gchar *stock_id);
;;; 
;;; Creates a new GtkToggleAction object. To add the action to a GtkActionGroup
;;; and set the accelerator for the action, call
;;; gtk_action_group_add_action_with_accel().
;;; 
;;; name :
;;;     A unique name for the action
;;; 
;;; label :
;;;     The label displayed in menu items and on buttons, or NULL.
;;; 
;;; tooltip :
;;;     A tooltip for the action, or NULL.
;;; 
;;; stock_id :
;;;     The stock icon to display in widgets representing the action, or NULL.
;;; 
;;; Returns :
;;;     a new GtkToggleAction
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_toggled ()
;;; 
;;; void gtk_toggle_action_toggled (GtkToggleAction *action);
;;; 
;;; Emits the "toggled" signal on the toggle action.
;;; 
;;; action :
;;;     the action object
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_set_active ()
;;; 
;;; void gtk_toggle_action_set_active (GtkToggleAction *action,
;;;                                    gboolean is_active);
;;; 
;;; Sets the checked state on the toggle action.
;;; 
;;; action :
;;;     the action object
;;; 
;;; is_active :
;;;     whether the action should be checked or not
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_get_active ()
;;; 
;;; gboolean gtk_toggle_action_get_active (GtkToggleAction *action);
;;; 
;;; Returns the checked state of the toggle action.
;;; 
;;; action :
;;;     the action object
;;; 
;;; Returns :
;;;     the checked state of the toggle action
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_set_draw_as_radio ()
;;; 
;;; void gtk_toggle_action_set_draw_as_radio (GtkToggleAction *action,
;;;                                           gboolean draw_as_radio);
;;; 
;;; Sets whether the action should have proxies like a radio action.
;;; 
;;; action :
;;;     the action object
;;; 
;;; draw_as_radio :
;;;     whether the action should have proxies like a radio action
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_get_draw_as_radio ()
;;; 
;;; gboolean gtk_toggle_action_get_draw_as_radio (GtkToggleAction *action);
;;; 
;;; Returns whether the action should have proxies like a radio action.
;;; 
;;; action :
;;;     the action object
;;; 
;;; Returns :
;;;     whether the action should have proxies like a radio action.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.toggle-action.lisp -------------------------------------
