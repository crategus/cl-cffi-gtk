;;; ----------------------------------------------------------------------------
;;; gtk.actionable.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; GtkActionable
;;; 
;;; An interface for widgets that can be associated with actions
;;;     
;;; Synopsis
;;; 
;;;     GtkActionable
;;;     GtkActionableInterface
;;;     
;;;     gtk_actionable_get_action_name
;;;     gtk_actionable_set_action_name
;;;     gtk_actionable_get_action_target_value
;;;     gtk_actionable_set_action_target_value
;;;     gtk_actionable_set_action_target
;;;     gtk_actionable_set_detailed_action_name
;;; 
;;; Object Hierarchy
;;; 
;;;   GInterface
;;;    +----GtkActionable
;;; 
;;; Prerequisites
;;; 
;;; GtkActionable requires GtkWidget.
;;;
;;; Known Implementations
;;; 
;;; GtkActionable is implemented by GtkButton, GtkCheckButton, GtkColorButton,
;;; GtkFontButton, GtkLinkButton, GtkLockButton, GtkMenuToolButton,
;;; GtkRadioButton, GtkRadioToolButton, GtkScaleButton, GtkSwitch,
;;; GtkToggleButton, GtkToggleToolButton, GtkToolButton and GtkVolumeButton.
;;;
;;; Properties
;;; 
;;;   "action-name"              gchar*                : Read / Write
;;;   "action-target"            GVariant*             : Read / Write
;;; 
;;; Description
;;; 
;;; This interface provides a convenient way of associating widgets with actions
;;; on a GtkApplicationWindow or GtkApplication.
;;; 
;;; It primarily consists of two properties: "action-name" and "action-target".
;;; There are also some convenience APIs for setting these properties.
;;; 
;;; This interface is presently only meaningful if used on a widget that is (or
;;; will be) located inside of a GtkApplicationWindow and can only be used to
;;; associate the widget with actions on that window, or its associated
;;; GtkApplication.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "action-name" property
;;; 
;;;   "action-name"              gchar*                : Read / Write
;;; 
;;; The name of the associated action, like 'app.quit'.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "action-target" property
;;; 
;;;   "action-target"            GVariant*             : Read / Write
;;; 
;;; The parameter for action invocations.
;;; 
;;; Allowed values: GVariant<*>
;;; 
;;; Default value: NULL
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkActionable
;;; 
;;; typedef struct _GtkActionable GtkActionable;
;;; 
;;; An opaque pointer type.
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkActionable" gtk-actionable
  (:export t
   :type-initializer "gtk_actionable_get_type")
  (action-name
   gtk-actionable-action-name
   "action-name" "gchar" t t)
  (action-target
   gtk-actionable-action-target
   "action-target" "GVariant" t t))

;;; ----------------------------------------------------------------------------
;;; struct GtkActionableInterface
;;; 
;;; struct GtkActionableInterface {
;;;   GTypeInterface g_iface;
;;; 
;;;   const gchar * (* get_action_name)         (GtkActionable *actionable);
;;;   void          (* set_action_name)         (GtkActionable *actionable,
;;;                                              const gchar   *action_name);
;;;   GVariant *    (* get_action_target_value) (GtkActionable *actionable);
;;;   void          (* set_action_target_value) (GtkActionable *actionable,
;;;                                              GVariant *action_target_value);
;;; };
;;; 
;;; The interface vtable for GtkActionable.
;;; 
;;; GTypeInterface g_iface;
;;; 
;;; get_action_name ()
;;;     virtual pointer for gtk_actionable_get_action_name()
;;; 
;;; set_action_name ()
;;;     virtual pointer for gtk_actionable_set_action_name()
;;; 
;;; get_action_target_value ()
;;;     virtual pointer for gtk_actionable_get_action_target_value()
;;; 
;;; set_action_target_value ()
;;;     virtual pointer for gtk_actionable_set_action_target_value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_actionable_get_action_name ()
;;; 
;;; const gchar * gtk_actionable_get_action_name (GtkActionable *actionable);
;;; 
;;; Gets the action name for actionable.
;;; 
;;; See gtk_actionable_set_action_name() for more information.
;;; 
;;; actionable :
;;;     a GtkActionable widget
;;; 
;;; Returns :
;;;     the action name, or NULL if none is set
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-actionable-get-action-name))

(defun gtk-actionable-get-action-name (actionable)
  (gtk-actionable-action-name actionable))

(export 'gtk-actionable-get-action-name)

;;; ----------------------------------------------------------------------------
;;; gtk_actionable_set_action_name ()
;;; 
;;; void gtk_actionable_set_action_name (GtkActionable *actionable,
;;;                                      const gchar *action_name);
;;; 
;;; Specifies the name of the action with which this widget should be
;;; associated. If action_name is NULL then the widget will be unassociated from
;;; any previous action.
;;; 
;;; Usually this function is used when the widget is located (or will be
;;; located) within the hierarchy of a GtkApplicationWindow.
;;; 
;;; Names are of the form "win.save" or "app.quit" for actions on the containing
;;; GtkApplicationWindow or its associated GtkApplication, respectively. This is
;;; the same form used for actions in the GMenu associated with the window.
;;; 
;;; actionable :
;;;     a GtkActionable widget
;;; 
;;; action_name :
;;;     an action name, or NULL
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-actionable-set-action-name))

(defun gtk-actionable-set-action-name (actionable action-name)
  (setf (gtk-actionable-action-name actionable) action-name))

(export 'gtk-actionable-set-action-name)

;;; ----------------------------------------------------------------------------
;;; gtk_actionable_get_action_target_value ()
;;; 
;;; GVariant * gtk_actionable_get_action_target_value
;;;                                                  (GtkActionable *actionable)
;;; 
;;; Gets the current target value of actionable.
;;; 
;;; See gtk_actionable_set_target_value() for more information.
;;; 
;;; actionable :
;;;     a GtkActionable widget
;;; 
;;; Returns :
;;;     the current target value
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-actionable-get-action-target-value))

(defun gtk-actionable-get-action-target-value (actionable)
  (gtk-actionable-action-target actionable))

(export 'gtk-actionable-get-action-target-value)

;;; ----------------------------------------------------------------------------
;;; gtk_actionable_set_action_target_value ()
;;; 
;;; void gtk_actionable_set_action_target_value (GtkActionable *actionable,
;;;                                              GVariant *target_value);
;;; 
;;; Sets the target value of an actionable widget.
;;; 
;;; If target_value is NULL then the target value is unset.
;;; 
;;; The target value has two purposes. First, it is used as the parameter to
;;; activation of the action associated with the GtkActionable widget. Second,
;;; it is used to determine if the widget should be rendered as "active" - the
;;; widget is active if the state is equal to the given target.
;;; 
;;; Consider the example of associating a set of buttons with a GAction with
;;; string state in a typical "radio button" situation. Each button will be
;;; associated with the same action, but with a different target value for that
;;; action. Clicking on a particular button will activate the action with the
;;; target of that button, which will typically cause the action's state to
;;; change to that value. Since the action's state is now equal to the target
;;; value of the button, the button will now be rendered as active (and the
;;; other buttons, with different targets, rendered inactive).
;;; 
;;; actionable :
;;;     a GtkActionable widget
;;; 
;;; target_value :
;;;     a GVariant to set as the target value, or NULL
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-actionable-set-action-target-value))

(defun gtk-actionable-set-action-target-value (actionable target-value)
  (setf (gtk-actionable-action-target actionable) target-value))

(export 'gtk-actionable-set-action-target-value)

;;; ----------------------------------------------------------------------------
;;; gtk_actionable_set_action_target ()
;;; 
;;; void gtk_actionable_set_action_target (GtkActionable *actionable,
;;;                                        const gchar *format_string,
;;;                                        ...);
;;; 
;;; Sets the target of an actionable widget.
;;; 
;;; This is a convenience function that calls g_variant_new() for format_string
;;; and uses the result to call gtk_actionable_set_action_target_value().
;;; 
;;; If you are setting a string-valued target and want to set the action name at
;;; the same time, you can use gtk_actionable_set_detailed_action_name().
;;; 
;;; actionable :
;;;     a GtkActionable widget
;;; 
;;; format_string :
;;;     a GVariant format string
;;; 
;;; ... :
;;;     arguments appropriate for format_string
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_actionable_set_detailed_action_name ()
;;; 
;;; void gtk_actionable_set_detailed_action_name
;;;                                          (GtkActionable *actionable,
;;;                                           const gchar *detailed_action_name)
;;; 
;;; Sets the action-name and associated string target value of an actionable
;;; widget.
;;; 
;;; This allows for the effect of both gtk_actionable_set_action_name() and
;;; gtk_actionable_set_target() in the common case that the target is
;;; string-valued.
;;; 
;;; detailed_action_name is a string of the form "action::target" where action
;;; is the action name and target is the string to use as the target.
;;; 
;;; actionable :
;;;     a GtkActionable widget
;;; 
;;; detailed_action_name :
;;;     the detailed action name
;;; 
;;; Since 3.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_actionable_set_detailed_action_name"
           gtk-actionable-set-detailed-action-name) :void
  (actionable (g-object gtk-actionable))
  (detailed-action-name :string))

(export 'gtk-actionable-set-detailed-action-name)

;;; --- End of file gtk.actionable.lisp ----------------------------------------
