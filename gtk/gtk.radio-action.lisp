;;; ----------------------------------------------------------------------------
;;; gtk.radio-action.lisp
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
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRadioAction
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-radio-action 'type)
 "@version{2013-6-2}
  @begin{short}
    A @sym{gtk-radio-action} is similar to @class{gtk-radio-menu-item}. A number
    of radio actions can be linked together so that only one may be active at
    any one time.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (action current)   : No Recursion
      @end{pre}
      The \"changed\" signal is emitted on every member of a radio group when
      the active member is changed. The signal gets emitted after the
      \"activate\" signals for the previous and current active members.
      @begin[code]{table}
        @entry[action]{the action on which the signal is emitted}
        @entry[current]{the member of actions group which has just been
          activated}
      @end{table}
      Since 2.4
  @end{dictionary}
  @see-slot{gtk-radio-action-current-value}
  @see-slot{gtk-radio-action-group}
  @see-slot{gtk-radio-action-value}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-value"
                                               'gtk-radio-action) 't)
 "The @code{\"current-value\"} property of type @code{:int} (Read / Write) @br{}
  The value property of the currently active member of the group to which this
  action belongs. @br{}
  Default value: 0 @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group" 'gtk-radio-action) 't)
 "The @code{\"group\"} property of type @class{gtk-radio-action} (Write) @br{}
  Sets a new group for a radio action. @br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value" 'gtk-radio-action) 't)
 "The @code{\"value\"} property of type @code{:int} (Read / Write) @br{}
  The value is an arbitrary integer which can be used as a convenient way to
  determine which action in the group is currently active in an \"activate\" or
  \"changed\" signal handler. See the @fun{gtk-radio-action-get-current-value}
  function and @class{gtk-radio-action-entry} for convenient ways to get and
  set this property. @br{}
  Default value: 0 @br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-action-current-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-action-current-value 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"current-value\"} of the @class{gtk-radio-action}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-action-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-action-group 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"group\"} of the @class{gtk-radio-action}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-action-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-action-value 'function)
 "@version{2013-3-17}
  Accessor of the slot @code{\"value\"} of the @class{gtk-radio-action}
  class.")

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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_action_get_group" gtk-radio-action-get-group)
    (g-slist (g-object gtk-radio-action) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-2}
  @argument[action]{the action object}
  @return{The list representing the radio group for this object.}
  @begin{short}
    Returns the list representing the radio group for this object. Note that the
    returned list is only valid until the next change to the group.
  @end{short}

  A common way to set up a group of radio group is the following:
  @begin{pre}
   GSList *group = NULL;
   GtkRadioAction *action;

   while (/* more actions to add */)
     {
        action = gtk_radio_action_new (...);

        gtk_radio_action_set_group (action, group);
        group = gtk_radio_action_get_group (action);
     @}
  @end{pre}
  Since 2.4"
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
