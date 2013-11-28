;;; ----------------------------------------------------------------------------
;;; gtk.radio-action.lisp
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
 "@version{2013-11-28}
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
        @entry[action]{The action on which the signal is emitted.}
        @entry[current]{The member of actions group which has just been
          activated.}
      @end{table}
      Since 2.4
  @end{dictionary}
  @see-slot{gtk-radio-action-current-value}
  @see-slot{gtk-radio-action-group}
  @see-slot{gtk-radio-action-value}
  @see-class{gtk-radio-menu-item}
  @see-class{gtk-radio-action-entry}
  @see-function{gtk-radio-action-get-current-value}")

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
 "@version{2013-11-28}
  Accessor of the slot @code{\"current-value\"} of the @class{gtk-radio-action}
  class.
  @see-class{gtk-radio-action}
  @see-function{gtk-radio-action-get-current-value}
  @see-function{gtk-radio-action-set-current-value}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-action-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-action-group 'function)
 "@version{2013-11-28}
  Accessor of the slot @code{\"group\"} of the @class{gtk-radio-action}
  class.
  @see-class{gtk-radio-action}
  @see-function{gtk-radio-action-get-group}
  @see-function{gtk-radio-action-set-group}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-action-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-action-value 'function)
 "@version{2013-11-28}
  Accessor of the slot @code{\"value\"} of the @class{gtk-radio-action}
  class.
  @see-class{gtk-radio-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_action_new" gtk-radio-action-new)
    (g-object gtk-radio-action)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-28}
  @argument[name]{a unique name for the action}
  @argument[label]{the label displayed in menu items and on buttons,
    or @code{nil}}
  @argument[tooltip]{a tooltip for this action, or @code{nil}}
  @argument[stock-id]{the stock icon to display in widgets representing this
    action, or @code{nil}}
  @argument[value]{the value which the function
    @fun{gtk-radio-action-get-current-value} should return if this action is
    selected.}
  @return{A new @class{gtk-radio-action} object}
  @begin{short}
    Creates a new @class{gtk-radio-action} object.
  @end{short}
  To add the action to a @class{gtk-action-group} and set the accelerator for
  the action, call the function @fun{gtk-action-group-add-action-with-accel}.

  Since 2.4
  @see-class{gtk-radio-action}
  @see-class{gtk-action-group}
  @see-function{gtk-radio-action-get-current-value}
  @see-function{gtk-action-group-add-action-with-accel}"
  (name :string)
  (label :string)
  (tooltip :string)
  (stock-id :string)
  (value :int))

(export 'gtk-radio-action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_get_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_action_get_group" gtk-radio-action-get-group)
    (g-slist (g-object gtk-radio-action) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-28}
  @argument[action]{the action object}
  @return{The list representing the radio group for this object.}
  @begin{short}
    Returns the list representing the radio group for this object.
  @end{short}
  Note that the returned list is only valid until the next change to the group.

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
  Since 2.4
  @see-class{gtk-radio-action}
  @see-function{gtk-radio-action-set-group}"
  (action (g-object gtk-radio-action)))

(export 'gtk-radio-action-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_set_group ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-radio-action-set-group))

(defun gtk-radio-action-set-group (action group)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-28}
  @argument[action]{the action object}
  @argument[group]{a list representing a radio group}
  @begin{short}
    Sets the radio group for the radio action object.
  @end{short}

  Since 2.4
  @see-class{gtk-radio-action}
  @see-function{gtk-radio-action-get-group}"
  (setf (gtk-radio-action-group action) group))

(export 'gtk-radio-action-set-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_join_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_action_join_group" gtk-radio-action-join-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-28}
  @argument[action]{the action object}
  @argument[group-source]{a radio action object whos group we are joining, or
    @code{nil} to remove the radio action from its group}
  @begin{short}
    Joins a radio action object to the group of another radio action object.
  @end{short}

  Use this in language bindings instead of the the functions
  @fun{gtk-radio-action-get-group} and @fun{gtk-radio-action-set-group}.

  A common way to set up a group of radio actions is the following:
  @begin{pre}
   GtkRadioAction *action;
   GtkRadioAction *last_action;

   while (/* more actions to add */)
     {
        action = gtk_radio_action_new (...);

        gtk_radio_action_join_group (action, last_action);
        last_action = action;
     @}
  @end{pre}

  Since 3.0
  @see-class{gtk-radio-action}
  @see-function{gtk-radio-action-get-group}
  @see-function{gtk-radio-action-set-group}"
  (action (g-object gtk-radio-action))
  (group-source (g-slist (g-object radio-acttion))))

(export 'gtk-radio-action-join-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_get_current_value ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-radio-action-get-current-value))

(defun gtk-radio-action-get-current-value (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-28}
  @argument[action]{a @class{gtk-radio-action} object}
  @return{The value of the currently active group member.}
  @begin{short}
    Obtains the value property of the currently active member of the group to
    which @arg{action} belongs.
  @end{short}

  Since 2.4
  @see-class{gtk-radio-action}
  @see-function{gtk-radio-action-set-current-value}"
  (gtk-radio-action-get-current-value action))

(export 'gtk-radio-action-get-current-value)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_set_current_value ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-radio-action-set-current-value))

(defun gtk-radio-action-set-current-value (context current-value)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-28}
  @argument[action]{a @class{gtk-radio-action} object}
  @argument[current-value]{the new value}
  @begin{short}
    Sets the currently active group member to the member with value property
    @arg{current-value}.
  @end{short}

  Since 2.10
  @see-class{gtk-radio-action}
  @see-function{gtk-radio-action-get-current-value}"
  (setf (gtk-radio-action-current-value context) current-value))

(export 'gtk-radio-action-set-current-value)

;;; --- End of file gtk.radio-action.lisp --------------------------------------
