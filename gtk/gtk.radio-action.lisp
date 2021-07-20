;;; ----------------------------------------------------------------------------
;;; gtk.radio-action.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     An action of which only one in a group can be active
;;;
;;; Types and Values
;;;
;;;     GtkRadioAction
;;;
;;; Functions
;;;
;;;     gtk_radio_action_new
;;;     gtk_radio_action_get_group
;;;     gtk_radio_action_set_group
;;;     gtk_radio_action_join_group
;;;     gtk_radio_action_get_current_value
;;;     gtk_radio_action_set_current_value
;;;
;;; Properties
;;;
;;;               gint    current-value    Read / Write
;;;     GtkRadioAction*   group            Write
;;;               gint    value            Read / Write
;;;
;;; Signals
;;;
;;;               void    changed          No Recursion
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkAction
;;;         ╰── GtkToggleAction
;;;             ╰── GtkRadioAction
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRadioAction implements GtkBuildable.
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
 "@version{2021-7-20}
  @begin{short}
    The @sym{gtk-radio-action} object is similar to the
    @class{gtk-radio-menu-item} widget.
  @end{short}
  A number of radio actions can be linked together so that only one may be
  active at any one time.
  @begin[Warning]{dictionary}
    The @sym{gtk-radio-action} class has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (action current)    :no-recurse
      @end{pre}
      The signal is emitted on every member of a radio group when the active
      member is changed. The signal gets emitted after the \"activate\" signals
      for the previous and current active members.
      @begin[code]{table}
        @entry[action]{The @sym{gtk-radio-action} object on which the signal
          is emitted.}
        @entry[current]{The @sym{gtk-radio-action} member of the action group
          which has just been activated.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-radio-action-current-value}
  @see-slot{gtk-radio-action-group}
  @see-slot{gtk-radio-action-value}
  @see-class{gtk-action}
  @see-class{gtk-radio-menu-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-radio-action-current-value -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "current-value"
                                               'gtk-radio-action) 't)
 "The @code{current-value} property of type @code{:int} (Read / Write) @br{}
  The value property of the currently active member of the group to which this
  action belongs. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-action-current-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-action-current-value 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-radio-action-current-value object) => current-value}
  @syntax[]{(setf (gtk-radio-action-current-value object) current-value)}
  @argument[object]{a @class{gtk-radio-action} object}
  @argument[current-value]{an integer with the value}
  @begin{short}
    Accessor of the @slot[gtk-radio-acton]{current-value} slot of the
    @class{gtk-radio-action} class.
  @end{short}

  The slot access function @sym{gtk-radio-action-current-value} obtains the
  value property of the currently active member of the group to which the radio
  action belongs. The slot access function
  @sym{(setf gtk-radio-action-current-value)} sets the currently active group
  member.
  @begin[Warning]{dictionary}
    The function @sym{gtk-radio-action-current-value} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-radio-action}")

;;; --- gtk-radio-action-group -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "group" 'gtk-radio-action) 't)
 "The @code{group} property of type @class{gtk-radio-action} (Write) @br{}
  Sets a new group for a radio action.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-action-group atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-action-group 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-radio-action-group object) => group}
  @syntax[]{(setf (gtk-radio-action-group object) group)}
  @argument[object]{a @class{gtk-radio-action} object}
  @argument[group]{a list of @class{gtk-radion-action} objects representing a
    radio group}
  @begin{short}
    Accessor of the @slot[gtk-radio-action]{group} slot of the
    @class{gtk-radio-action} class.
  @end{short}

  The slot access function @sym{gtk-radio-action-group} returns the list
  representing the radio group for this object. The slot access function
  @sym{(setf gtk-radio-action-group)} sets the radio group.

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
  @begin[Warning]{dictionary}
    The function @sym{gtk-radio-action-group} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-radio-action}")

;;; --- gtk-radio-action-value -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "value" 'gtk-radio-action) 't)
 "The @code{value} property of type @code{:int} (Read / Write) @br{}
  The value is an integer which can be used as a convenient way to determine
  which action in the group is currently active in an \"activate\" or
  \"changed\" signal handler. @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-radio-action-value atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-radio-action-value 'function)
 "@version{2021-7-20}
  @syntax[]{(gtk-radio-action-value object) => value}
  @syntax[]{(setf (gtk-radio-action-value object) value)}
  @argument[object]{a @class{gtk-radio-action} object}
  @argument[value]{an integer with the value which can be used to determine
    which action is active}
  @begin{short}
    Accessor of the @slot[gtk-radio-action]{value} slot of the
    @class{gtk-radio-action} class.
  @end{short}

  The value is an integer which can be used as a convenient way to determine
  which action in the group is currently active in an \"activate\" or
  \"changed\" signal handler.
  @begin[Warning]{dictionary}
    The function @sym{gtk-radio-action-value} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-radio-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_action_new" gtk-radio-action-new)
    (g-object gtk-radio-action)
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[name]{a string with the unique name for the action}
  @argument[label]{a string with the label displayed in menu items and on
    buttons, or @code{nil}}
  @argument[tooltip]{a string with the tooltip for this action, or @code{nil}}
  @argument[stock-id]{a string with the stock icon to display in widgets
    representing this action, or @code{nil}}
  @argument[value]{an integer with the value which the function
    @fun{gtk-radio-action-current-value} should return if this action is
    selected}
  @return{A new @class{gtk-radio-action} object.}
  @begin{short}
    Creates a new radio action.
  @end{short}
  To add the action to a @class{gtk-action-group} object and set the accelerator
  for the action, call the function @fun{gtk-action-group-add-action}.
  @begin[Warning]{dictionary}
    The function @sym{gtk-radio-action-new} has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-radio-action}
  @see-class{gtk-action-group}
  @see-function{gtk-radio-action-current-value}
  @see-function{gtk-action-group-add-action}"
  (name :string)
  (label :string)
  (tooltip :string)
  (stock-id :string)
  (value :int))

(export 'gtk-radio-action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_join_group ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_radio_action_join_group" gtk-radio-action-join-group) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-7-20}
  @argument[action]{a @class{gtk-radio-action} object}
  @argument[source]{a @class{gtk-radio-action} object whose group we are
    joining, or @code{nil} to remove the radio action from its group}
  @begin{short}
    Joins a radio action object to the group of another radio action object.
  @end{short}
  Use this in language bindings instead of the the slot access function
  @fun{gtk-radio-action-group}.

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
  @begin[Warning]{dictionary}
    The function @sym{gtk-radio-action-join-group} has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk-radio-action}
  @see-function{gtk-radio-action-group}"
  (action (g-object gtk-radio-action))
  (source (g-object gtk-radio-action)))

(export 'gtk-radio-action-join-group)

;;; --- End of file gtk.radio-action.lisp --------------------------------------
