;;; ----------------------------------------------------------------------------
;;; gtk.radio-action.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;;﻿
;;; GtkRadioAction
;;;
;;; An action of which only one in a group can be active
;;;
;;; Synopsis
;;;
;;;     GtkRadioAction
;;;
;;;     gtk_radio_action_new
;;;     gtk_radio_action_get_group
;;;     gtk_radio_action_set_group
;;;     gtk_radio_action_join_group
;;;     gtk_radio_action_get_current_value
;;;     gtk_radio_action_set_current_value
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
;;; GtkRadioAction implements GtkBuildable.
;;;
;;; Properties
;;;
;;;   "current-value"            gint                  : Read / Write
;;;   "group"                    GtkRadioAction*       : Write
;;;   "value"                    gint                  : Read / Write
;;;
;;; Signals
;;;
;;;   "changed"                                        : No Recursion
;;;
;;; Description
;;;
;;; A GtkRadioAction is similar to GtkRadioMenuItem. A number of radio actions
;;; can be linked together so that only one may be active at any one time.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "current-value" property
;;;
;;;   "current-value"            gint                  : Read / Write
;;;
;;; The value property of the currently active member of the group to which this
;;; action belongs.
;;;
;;; Default value: 0
;;;
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "group" property
;;;
;;;   "group"                    GtkRadioAction*       : Write
;;;
;;; Sets a new group for a radio action.
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "value" property
;;;
;;;   "value"                    gint                  : Read / Write
;;;
;;; The value is an arbitrary integer which can be used as a convenient way to
;;; determine which action in the group is currently active in an ::activate or
;;; ::changed signal handler. See gtk_radio_action_get_current_value() and
;;; GtkRadioActionEntry for convenient ways to get and set this property.
;;;
;;; Default value: 0
;;;
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "changed" signal
;;;
;;; void user_function (GtkRadioAction *action,
;;;                     GtkRadioAction *current,
;;;                     gpointer        user_data)      : No Recursion
;;;
;;; The ::changed signal is emitted on every member of a radio group when the
;;; active member is changed. The signal gets emitted after the ::activate
;;; signals for the previous and current active members.
;;;
;;; action :
;;;     the action on which the signal is emitted
;;;
;;; current :
;;;     the member of actions group which has just been activated
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRadioAction
;;;
;;; struct GtkRadioAction;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRadioAction" gtk-radio-action
  (:superclass gtk-toggle-action
   :export t
   :interfaces ("GtkBuildable")
   :type-initializer "gtk_radio_action_get_type")
  ((current-value
    gtk-radio-action-current-value
    "current-value" "gint" t t)
   (group
    gtk-radio-action-group
    "group" "GtkRadioAction" nil t)
   (value
    gtk-radio-action-value
    "value" "gint" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_new ()
;;;
;;; GtkRadioAction * gtk_radio_action_new (const gchar *name,
;;;                                        const gchar *label,
;;;                                        const gchar *tooltip,
;;;                                        const gchar *stock_id,
;;;                                        gint value);
;;;
;;; Creates a new GtkRadioAction object. To add the action to a GtkActionGroup
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
;;;     A tooltip for this action, or NULL.
;;;
;;; stock_id :
;;;     The stock icon to display in widgets representing this action, or NULL.
;;;
;;; value :
;;;     The value which gtk_radio_action_get_current_value() should return if
;;;     this action is selected.
;;;
;;; Returns :
;;;     a new GtkRadioAction
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_get_group ()
;;;
;;; GSList * gtk_radio_action_get_group (GtkRadioAction *action);
;;;
;;; Returns the list representing the radio group for this object. Note that the
;;; returned list is only valid until the next change to the group.
;;;
;;; A common way to set up a group of radio group is the following:
;;;
;;;   GSList *group = NULL;
;;;   GtkRadioAction *action;
;;;
;;;   while (/* more actions to add */)
;;;     {
;;;        action = gtk_radio_action_new (...);
;;;
;;;        gtk_radio_action_set_group (action, group);
;;;        group = gtk_radio_action_get_group (action);
;;;     }
;;;
;;; action :
;;;     the action object
;;;
;;; Returns :
;;;     the list representing the radio group for this object
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_action_get_group" gtk-radio-action-get-group)
    (g-slist (g-object gtk-radio-action) :free-from-foreign nil)
  (action (g-object gtk-radio-action)))

(export 'gtk-radio-action-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_set_group ()
;;;
;;; void gtk_radio_action_set_group (GtkRadioAction *action, GSList *group);
;;;
;;; Sets the radio group for the radio action object.
;;;
;;; action :
;;;     the action object
;;;
;;; group :
;;;     a list representing a radio group. [element-type GtkRadioAction]
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_join_group ()
;;;
;;; void gtk_radio_action_join_group (GtkRadioAction *action,
;;;                                   GtkRadioAction *group_source);
;;;
;;; Joins a radio action object to the group of another radio action object.
;;;
;;; Use this in language bindings instead of the gtk_radio_action_get_group()
;;; and gtk_radio_action_set_group() methods
;;;
;;; A common way to set up a group of radio actions is the following:
;;;
;;;   GtkRadioAction *action;
;;;   GtkRadioAction *last_action;
;;;
;;;   while (/* more actions to add */)
;;;     {
;;;        action = gtk_radio_action_new (...);
;;;
;;;        gtk_radio_action_join_group (action, last_action);
;;;        last_action = action;
;;;     }
;;;
;;; action :
;;;     the action object
;;;
;;; group_source :
;;;     a radio action object whos group we are joining, or NULL to remove the
;;;     radio action from its group
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_get_current_value ()
;;;
;;; gint gtk_radio_action_get_current_value (GtkRadioAction *action);
;;;
;;; Obtains the value property of the currently active member of the group to
;;; which action belongs.
;;;
;;; action :
;;;     a GtkRadioAction
;;;
;;; Returns :
;;;     The value of the currently active group member
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_set_current_value ()
;;;
;;; void gtk_radio_action_set_current_value (GtkRadioAction *action,
;;;                                          gint current_value);
;;;
;;; Sets the currently active group member to the member with value property
;;; current_value.
;;;
;;; action :
;;;     a GtkRadioAction
;;;
;;; current_value :
;;;     the new value
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.radio-action.lisp --------------------------------------
