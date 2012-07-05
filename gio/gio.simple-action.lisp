;;; ----------------------------------------------------------------------------
;;; gio.simple-action.lisp
;;;
;;; The documentation has been copied from the GIO Reference Manual
;;; for GIO 2.32.3. The latest version of this documentation can be found
;;; on-line at http://library.gnome.org/devel/gio/unstable/.
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
;;; GSimpleAction
;;;
;;; A simple GAction implementation
;;;
;;; Synopsis
;;;
;;;     GSimpleAction
;;;
;;;     g_simple_action_new
;;;     g_simple_action_new_stateful
;;;
;;;     g_simple_action_set_enabled
;;;     g_simple_action_set_state
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GSimpleAction
;;;
;;; Implemented Interfaces
;;;
;;; GSimpleAction implements GAction.
;;;
;;; Properties
;;;
;;;   "enabled"                  gboolean             : Read / Write
;;;   "name"                     gchar*               : Read / Write / Construct
;;;   "parameter-type"           GVariantType*        : Read / Write / Construct
;;;   "state"                    GVariant*            : Read / Write
;;;   "state-type"               GVariantType*        : Read
;;;
;;; Signals
;;;
;;;   "activate"                                      : Run Last
;;;   "change-state"                                  : Run Last
;;;
;;; Description
;;;
;;; A GSimpleAction is the obvious simple implementation of the GAction
;;; interface. This is the easiest way to create an action for purposes of
;;; adding it to a GSimpleActionGroup.
;;;
;;; See also GtkAction.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "enabled" property
;;;
;;;   "enabled"                  gboolean              : Read / Write
;;;
;;; If action is currently enabled.
;;;
;;; If the action is disabled then calls to g_action_activate() and
;;; g_action_change_state() have no effect.
;;;
;;; Default value: TRUE
;;;
;;; Since 2.28
;;;
;;; ----------------------------------------------------------------------------
;;; The "name" property
;;;
;;;   "name"                     gchar*               : Read / Write / Construct
;;;
;;; The name of the action. This is mostly meaningful for identifying the action
;;; once it has been added to a GSimpleActionGroup.
;;;
;;; Default value: NULL
;;;
;;; Since 2.28
;;;
;;; ----------------------------------------------------------------------------
;;; The "parameter-type" property
;;;
;;;   "parameter-type"           GVariantType*        : Read / Write / Construct
;;;
;;; The type of the parameter that must be given when activating the action.
;;;
;;; Since 2.28
;;;
;;; ----------------------------------------------------------------------------
;;; The "state" property
;;;
;;;   "state"                    GVariant*             : Read / Write
;;;
;;; The state of the action, or NULL if the action is stateless.
;;;
;;; Allowed values: GVariant<*>
;;;
;;; Default value: NULL
;;;
;;; Since 2.28
;;;
;;; ----------------------------------------------------------------------------
;;; The "state-type" property
;;;
;;;   "state-type"               GVariantType*         : Read
;;;
;;; The GVariantType of the state that the action has, or NULL if the action is
;;; stateless.
;;;
;;; Since 2.28
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate" signal
;;;
;;; void user_function (GSimpleAction *simple,
;;;                     GVariant      *parameter,
;;;                     gpointer       user_data)      : Run Last
;;;
;;; Indicates that the action was just activated.
;;;
;;; parameter will always be of the expected type. In the event that an
;;; incorrect type was given, no signal will be emitted.
;;;
;;; simple :
;;;     the GSimpleAction
;;;
;;; parameter :
;;;     the parameter to the activation
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.28
;;;
;;; ----------------------------------------------------------------------------
;;; The "change-state" signal
;;;
;;; void user_function (GSimpleAction *simple,
;;;                     GVariant      *value,
;;;                     gpointer       user_data)      : Run Last
;;;
;;; Indicates that the action just received a request to change its state.
;;;
;;; value will always be of the correct state type. In the event that an
;;; incorrect type was given, no signal will be emitted.
;;;
;;; If no handler is connected to this signal then the default behaviour is to
;;; call g_simple_action_set_state() to set the state to the requested value. If
;;; you connect a signal handler then no default action is taken. If the state
;;; should change then you must call g_simple_action_set_state() from the
;;; handler.
;;;
;;; Example 21. Example 'change-state' handler
;;;
;;;   static void
;;;   change_volume_state (GSimpleAction *action,
;;;                        GVariant      *value,
;;;                        gpointer       user_data)
;;;   {
;;;     gint requested;
;;;     requested = g_variant_get_int32 (value);
;;;
;;;     // Volume only goes from 0 to 10
;;;     if (0 <= requested && requested <= 10)
;;;       g_simple_action_set_state (action, value);
;;;   }
;;;
;;;
;;; The handler need not set the state to the requested value. It could set it
;;; to any value at all, or take some other action.
;;;
;;; simple :
;;;     the GSimpleAction
;;;
;;; value :
;;;     the requested value for the state
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GSimpleAction
;;;
;;; typedef struct _GSimpleAction GSimpleAction;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GSimpleAction" g-simple-action
  (:superclass g-object
   :export t
   :interfaces ("GAction")
   :type-initializer "g_simple_action_get_type")
  nil)
;  ((enabled
;    g-simple-action-enabled
;    "enabled" "gboolean" t t)
;   (name
;    g-simple-action-name
;    "name" "gchar" t t)
;   (parameter-type
;    g-simple-action-parameter-type
;    "parameter-type" "GVariantType" t t)
;   (state
;    g-simple-action-state
;    "state" "GVariant" t t)
;   (state-type
;    g-simple-action-state-type
;    "state-type" "GVariantType" t t)))

;;; ----------------------------------------------------------------------------
;;; g_simple_action_new ()
;;;
;;; GSimpleAction * g_simple_action_new (const gchar *name,
;;;                                      const GVariantType *parameter_type);
;;;
;;; Creates a new action.
;;;
;;; The created action is stateless. See g_simple_action_new_stateful().
;;;
;;; name :
;;;     the name of the action
;;;
;;; parameter_type :
;;;     the type of parameter to the activate function
;;;
;;; Returns :
;;;     a new GSimpleAction
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defun g-simple-action-new (name parameter-type)
  (make-instance 'g-simple-action
                 :name name
                 :parameter-type parameter-type))

(export 'g-simple-action-new)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_new_stateful ()
;;;
;;; GSimpleAction * g_simple_action_new_stateful
;;;                                         (const gchar *name,
;;;                                          const GVariantType *parameter_type,
;;;                                          GVariant *state);
;;;
;;; Creates a new stateful action.
;;;
;;; state is the initial state of the action. All future state values must have
;;; the same GVariantType as the initial state.
;;;
;;; If the state GVariant is floating, it is consumed.
;;;
;;; name :
;;;     the name of the action
;;;
;;; parameter_type :
;;;     the type of the parameter to the activate function
;;;
;;; state :
;;;     the initial state of the action
;;;
;;; Returns :
;;;     a new GSimpleAction
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-simple-action-new-stateful))

(defun g-simple-action-new-stateful (name parameter-type state)
  (make-instance 'g-simple-action-new-stateful
                 :name name
                 :parameter-type parameter-type
                 :state state
                 :state-type (g-variant-get-type state)))

(export 'g-simple-action-new-stateful)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_set_enabled ()
;;;
;;; void g_simple_action_set_enabled (GSimpleAction *simple, gboolean enabled);
;;;
;;; Sets the action as enabled or not.
;;;
;;; An action must be enabled in order to be activated or in order to have its
;;; state changed from outside callers.
;;;
;;; This should only be called by the implementor of the action. Users of the
;;; action should not attempt to modify its enabled flag.
;;;
;;; simple :
;;;     a GSimpleAction
;;;
;;; enabled :
;;;     whether the action is enabled
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-simple-action-set-enabled))

(defun g-simple-action-set-enabled (action enabled)
  (setf (g-action-enabled action) enabled))

(export 'g-simple-action-set-enabled)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_set_state ()
;;;
;;; void g_simple_action_set_state (GSimpleAction *simple, GVariant *value);
;;;
;;; Sets the state of the action.
;;;
;;; This directly updates the 'state' property to the given value.
;;;
;;; This should only be called by the implementor of the action. Users of the
;;; action should not attempt to directly modify the 'state' property. Instead,
;;; they should call g_action_change_state() to request the change.
;;;
;;; simple :
;;;     a GSimpleAction
;;;
;;; value :
;;;     the new GVariant for the state
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

(declaim (inline g-simple-action-set-state))

(defun g-simple-action-set-state (action value)
  (setf (g-action-state action) value))

(export 'g-simple-action-set-state)

;;; --- End of file gio.simple-action.lisp -------------------------------------
