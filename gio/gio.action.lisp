;;; ----------------------------------------------------------------------------
;;; gio.action.lisp
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
;;; GAction
;;;
;;; An action interface
;;;
;;; Synopsis
;;;
;;;     GAction
;;;     GActionInterface
;;;
;;;     g_action_get_name
;;;     g_action_get_parameter_type
;;;     g_action_get_state_type
;;;     g_action_get_state_hint
;;;
;;;     g_action_get_enabled
;;;     g_action_get_state
;;;
;;;     g_action_change_state
;;;     g_action_activate
;;;
;;; Object Hierarchy
;;;
;;;   GInterface
;;;    +----GAction
;;;
;;; Prerequisites
;;;
;;; GAction requires GObject.
;;;
;;; Known Implementations
;;;
;;; GAction is implemented by GSimpleAction.
;;;
;;; Properties
;;;
;;;   "enabled"                  gboolean              : Read
;;;   "name"                     gchar*                : Read
;;;   "parameter-type"           GVariantType*         : Read
;;;   "state"                    GVariant*             : Read
;;;   "state-type"               GVariantType*         : Read
;;;
;;; Description
;;;
;;; GAction represents a single named action.
;;;
;;; The main interface to an action is that it can be activated with
;;; g_action_activate(). This results in the 'activate' signal being emitted. An
;;; activation has a GVariant parameter (which may be NULL). The correct type
;;; for the parameter is determined by a static parameter type (which is given
;;; at construction time).
;;;
;;; An action may optionally have a state, in which case the state may be set
;;; with g_action_change_state(). This call takes a GVariant. The correct type
;;; for the state is determined by a static state type (which is given at
;;; construction time).
;;;
;;; The state may have a hint associated with it, specifying its valid range.
;;;
;;; GAction is merely the interface to the concept of an action, as described
;;; above. Various implementations of actions exist, including GSimpleAction and
;;; GtkAction.
;;;
;;; In all cases, the implementing class is responsible for storing the name of
;;; the action, the parameter type, the enabled state, the optional state type
;;; and the state and emitting the appropriate signals when these change. The
;;; implementor responsible for filtering calls to g_action_activate() and
;;; g_action_change_state() for type safety and for the state being enabled.
;;;
;;; Probably the only useful thing to do with a GAction is to put it inside of a
;;; GSimpleActionGroup.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "enabled" property
;;;
;;;   "enabled"                  gboolean              : Read
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
;;;   "name"                     gchar*                : Read
;;;
;;; The name of the action. This is mostly meaningful for identifying the action
;;; once it has been added to a GActionGroup.
;;;
;;; Default value: NULL
;;;
;;; Since 2.28
;;;
;;; ----------------------------------------------------------------------------
;;; The "parameter-type" property
;;;
;;;   "parameter-type"           GVariantType*         : Read
;;;
;;; The type of the parameter that must be given when activating the action.
;;;
;;; Since 2.28
;;;
;;; ----------------------------------------------------------------------------
;;; The "state" property
;;;
;;;   "state"                    GVariant*             : Read
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
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GAction
;;;
;;; typedef struct _GAction GAction;
;;; ----------------------------------------------------------------------------

(define-g-interface "GAction" g-action
  (:export t
   :type-initializer "g_action_get_type")
  (enabled
   g-action-enabled
   "enabled" "gboolean" t nil)
  (name
   g-action-name
   "name" "gchar" t nil)
  (parameter-type
   g-action-parameter-type
   "parameter-type" "GVariantType" t nil)
  (state
   g-action-state
   "state" "GVariant" t nil)
  (state-type
   g-action-state-type
   "state-type" "GVariantType" t nil))

;;; ----------------------------------------------------------------------------
;;; struct GActionInterface
;;;
;;; struct GActionInterface {
;;;   GTypeInterface g_iface;
;;;
;;;   /* virtual functions */
;;;   const gchar *        (* get_name)             (GAction  *action);
;;;   const GVariantType * (* get_parameter_type)   (GAction  *action);
;;;   const GVariantType * (* get_state_type)       (GAction  *action);
;;;   GVariant *           (* get_state_hint)       (GAction  *action);
;;;
;;;   gboolean             (* get_enabled)          (GAction  *action);
;;;   GVariant *           (* get_state)            (GAction  *action);
;;;
;;;   void                 (* change_state)         (GAction  *action,
;;;                                                  GVariant *value);
;;;   void                 (* activate)             (GAction  *action,
;;;                                                  GVariant *parameter);
;;; };
;;;
;;; The virtual function table for GAction.
;;;
;;; GTypeInterface g_iface;
;;;
;;; get_name ()
;;;     the virtual function pointer for g_action_get_name()
;;;
;;; get_parameter_type ()
;;;     the virtual function pointer for g_action_get_parameter_type()
;;;
;;; get_state_type ()
;;;     the virtual function pointer for g_action_get_state_type()
;;;
;;; get_state_hint ()
;;;     the virtual function pointer for g_action_get_state_hint()
;;;
;;; get_enabled ()
;;;     the virtual function pointer for g_action_get_enabled()
;;;
;;; get_state ()
;;;     the virtual function pointer for g_action_get_state()
;;;
;;; change_state ()
;;;     the virtual function pointer for g_action_change_state()
;;;
;;; activate ()
;;;     the virtual function pointer for g_action_activate(). Note that GAction
;;;     does not have an 'activate' signal but that implementations of it may
;;;     have one.
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_action_get_name ()
;;;
;;; const gchar * g_action_get_name (GAction *action);
;;;
;;; Queries the name of action.
;;;
;;; action :
;;;     a GAction
;;;
;;; Returns :
;;;     the name of the action
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-action-get-name))

(defun g-action-get-name (action)
  (g-action-name action))

(export 'g-action-get-name)

;;; ----------------------------------------------------------------------------
;;; g_action_get_parameter_type ()
;;;
;;; const GVariantType * g_action_get_parameter_type (GAction *action);
;;;
;;; Queries the type of the parameter that must be given when activating action.
;;;
;;; When activating the action using g_action_activate(), the GVariant given to
;;; that function must be of the type returned by this function.
;;;
;;; In the case that this function returns NULL, you must not give any GVariant,
;;; but NULL instead.
;;;
;;; action :
;;;     a GAction
;;;
;;; Returns :
;;;     the parameter type
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-action-get-parameter-type))

(defun g-action-get-parameter-type (action)
  (g-action-parameter-type action))

(export 'g-action-get-parameter-type)

;;; ----------------------------------------------------------------------------
;;; g_action_get_state_type ()
;;;
;;; const GVariantType * g_action_get_state_type (GAction *action);
;;;
;;; Queries the type of the state of action.
;;;
;;; If the action is stateful (e.g. created with g_simple_action_new_stateful())
;;; then this function returns the GVariantType of the state. This is the type
;;; of the initial value given as the state. All calls to
;;; g_action_change_state() must give a GVariant of this type and
;;; g_action_get_state() will return a GVariant of the same type.
;;;
;;; If the action is not stateful (e.g. created with g_simple_action_new()) then
;;; this function will return NULL. In that case, g_action_get_state() will
;;; return NULL and you must not call g_action_change_state().
;;;
;;; action :
;;;     a GAction
;;;
;;; Returns :
;;;     the state type, if the action is stateful
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-action-get-state-type))

(defun g-action-get-state-type (action)
  (g-action-state-type action))

(export 'g-action-get-state-type)

;;; ----------------------------------------------------------------------------
;;; g_action_get_state_hint ()
;;;
;;; GVariant * g_action_get_state_hint (GAction *action);
;;;
;;; Requests a hint about the valid range of values for the state of action.
;;;
;;; If NULL is returned it either means that the action is not stateful or that
;;; there is no hint about the valid range of values for the state of the
;;; action.
;;;
;;; If a GVariant array is returned then each item in the array is a possible
;;; value for the state. If a GVariant pair (ie: two-tuple) is returned then the
;;; tuple specifies the inclusive lower and upper bound of valid values for the
;;; state.
;;;
;;; In any case, the information is merely a hint. It may be possible to have a
;;; state value outside of the hinted range and setting a value within the range
;;; may fail.
;;;
;;; The return value (if non-NULL) should be freed with g_variant_unref() when
;;; it is no longer required.
;;;
;;; action :
;;;     a GAction
;;;
;;; Returns :
;;;     the state range hint
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_get_state_hint" g-action-get-state-hint)
    (:pointer g-variant)
  (action (g-object g-action)))

(export 'g-action-get-state-hint)

;;; ----------------------------------------------------------------------------
;;; g_action_get_enabled ()
;;;
;;; gboolean g_action_get_enabled (GAction *action);
;;;
;;; Checks if action is currently enabled.
;;;
;;; An action must be enabled in order to be activated or in order to have its
;;; state changed from outside callers.
;;;
;;; action :
;;;     a GAction
;;;
;;; Returns :
;;;     whether the action is enabled
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-action-get-enabled))

(defun g-action-get-enabled (action)
  (g-action-enabled action))

(export 'g-action-get-enabled)

;;; ----------------------------------------------------------------------------
;;; g_action_get_state ()
;;;
;;; GVariant * g_action_get_state (GAction *action);
;;;
;;; Queries the current state of action.
;;;
;;; If the action is not stateful then NULL will be returned. If the action is
;;; stateful then the type of the return value is the type given by
;;; g_action_get_state_type().
;;;
;;; The return value (if non-NULL) should be freed with g_variant_unref() when
;;; it is no longer required.
;;;
;;; action :
;;;     a GAction
;;;
;;; Returns :
;;;     the current state of the action
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(declaim (inline g-action-get-state))

(defun g-action-get-state (action)
  (g-action-state action))

(export 'g-action-get-state)

;;; ----------------------------------------------------------------------------
;;; g_action_change_state ()
;;;
;;; void g_action_change_state (GAction *action, GVariant *value);
;;;
;;; Request for the state of action to be changed to value.
;;;
;;; The action must be stateful and value must be of the correct type. See
;;; g_action_get_state_type().
;;;
;;; This call merely requests a change. The action may refuse to change its
;;; state or may change its state to something other than value. See
;;; g_action_get_state_hint().
;;;
;;; If the value GVariant is floating, it is consumed.
;;;
;;; action :
;;;     a GAction
;;;
;;; value :
;;;     the new state
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_change_state" g-action-change-state) :void
  (action (g-object g-action))
  (value (:pointer g-variant)))

(export 'g-action-change-state)

;;; ----------------------------------------------------------------------------
;;; g_action_activate ()
;;;
;;; void g_action_activate (GAction *action, GVariant *parameter);
;;;
;;; Activates the action.
;;;
;;; parameter must be the correct type of parameter for the action (ie: the
;;; parameter type given at construction time). If the parameter type was NULL
;;; then parameter must also be NULL.
;;;
;;; action :
;;;     a GAction
;;;
;;; parameter :
;;;     the parameter to the activation
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_activate" %g-action-activate) :void
  (action :pointer)
  (parameter :pointer))

(defun g-action-activate (action parameter)
  (%g-action-activate (pointer action) parameter))

(export 'g-action-activate)

;;; --- End of file gio.action.lisp --------------------------------------------
