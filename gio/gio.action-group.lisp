;;; ----------------------------------------------------------------------------
;;; gio.action-group.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;; GActionGroup
;;;
;;;     A group of actions
;;;
;;; Types and Values
;;;
;;;     GActionGroup
;;;
;;; Functions
;;;
;;;     g_action_group_list_actions
;;;     g_action_group_query_action
;;;     g_action_group_has_action
;;;     g_action_group_get_action_enabled
;;;     g_action_group_get_action_parameter_type
;;;     g_action_group_get_action_state_type
;;;     g_action_group_get_action_state_hint
;;;     g_action_group_get_action_state
;;;     g_action_group_change_action_state
;;;     g_action_group_activate_action
;;;     g_action_group_action_added
;;;     g_action_group_action_removed
;;;     g_action_group_action_enabled_changed
;;;     g_action_group_action_state_changed
;;;
;;; Signals
;;;
;;;     void    action-added              Has Details
;;;     void    action-enabled-changed    Has Details
;;;     void    action-removed            Has Details
;;;     void    action-state-changed      Has Details
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GActionGroup
;;;
;;; Prerequisites
;;;
;;;     GActionGroup requires GObject.
;;;
;;; Known Derived Interfaces
;;;
;;;     GActionGroup is required by GRemoteActionGroup.
;;;
;;; Known Implementations
;;;
;;;     GActionGroup is implemented by GApplication, GDBusActionGroup and
;;;     GSimpleActionGroup.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GActionGroup
;;; ----------------------------------------------------------------------------

(define-g-interface "GActionGroup" g-action-group
  (:export t
   :type-initializer "g_action_group_get_type"))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-group atdoc:*class-name-alias*)
      "Interface"
      (documentation 'g-action-group 'type)
 "@version{2021-9-8}
  @begin{short}
    The @sym{g-action-group} interface represents a group of actions.
  @end{short}
  Actions can be used to expose functionality in a structured way, either from
  one part of a program to another, or to the outside world. Action groups are
  often used together with a @class{g-menu-model} object that provides
  additional representation data for displaying the actions to the user, e.g.
  in a menu.

  The main way to interact with the actions in a @sym{g-action-group} object is
  to activate them with the @fun{g-action-group-activate-action} function.
  Activating an action may require a @type{g-variant} parameter. The required
  type of the parameter can be inquired with the
  @fun{g-action-group-action-parameter-type} function. Actions may be disabled,
  see the @fun{g-action-group-action-enabled} function. Activating a disabled
  action has no effect.

  Actions may optionally have a state in the form of a @type{g-variant}
  parameter. The current state of an action can be inquired with the
  @fun{g-action-group-action-state} function. Activating a stateful action may
  change its state, but it is also possible to set the state by calling
  the @fun{g-action-group-change-action-state} function.

  As typical example, consider a text editing application which has an option
  to change the current font to 'bold'. A good way to represent this would be
  a stateful action, with a boolean state. Activating the action would toggle
  the state.

  Each action in the group has a unique name which is a string. All method
  calls, except the @fun{g-action-group-list-actions} function take the name
  of an action as an argument.

  The @sym{g-action-group} API is meant to be the 'public' API to the action
  group. The calls here are exactly the interaction that 'external forces',
  e.g. UI, incoming D-Bus messages, etc., are supposed to have with actions.
  'Internal' APIs, i.e. ones meant only to be accessed by the action group
  implementation, are found on subclasses. This is why you will find - for
  example - the @fun{g-action-group-action-enabled} function but not an
  equivalent @code{(setf g-action-group-action-enabled)} function.

  Signals are emitted on the action group in response to state changes on
  individual actions.

  Implementations of the @sym{g-action-group} interface should provide
  implementations for the @fun{g-action-group-list-actions} and
  @fun{g-action-group-query-action} virtual functions. The other virtual
  functions should not be implemented - their \"wrappers\" are actually
  implemented with calls to the @fun{g-action-group-query-action} function.
  @begin[Signal Details]{dictionary}
    @subheading{The \"action-added\" signal}
      @begin{pre}
 lambda (group name)    :detailed
      @end{pre}
      Signals that a new action was just added to the group. This signal is
      emitted after the action has been added and is now visible.
      @begin[code]{table}
        @entry[group]{The @sym{g-action-group} object that changed.}
        @entry[name]{A string with the name of the action.}
      @end{table}
    @subheading{The \"action-enabled-changed\" signal}
      @begin{pre}
 lambda (group name enabled)    :detailed
      @end{pre}
      Signals that the enabled status of the named action has changed.
      @begin[code]{table}
        @entry[group]{The @sym{g-action-group} object that changed.}
        @entry[name]{A string with the name of the action.}
        @entry[enabled]{A boolean whether the action is enabled or not.}
      @end{table}
    @subheading{The \"action-removed\" signal}
      @begin{pre}
 lambda (group name)    :detailed
      @end{pre}
      Signals that an action is just about to be removed from the group. This
      signal is emitted before the action is removed, so the action is still
      visible and can be queried from the signal handler.
      @begin[code]{table}
        @entry[group]{The @sym{g-action-group} object that changed.}
        @entry[name]{A string with the name of the action.}
      @end{table}
    @subheading{The \"action-state-changed\" signal}
      @begin{pre}
 lambda (group name parameter)    :detailed
      @end{pre}
      Signals that the state of the named action has changed.
      @begin[code]{table}
        @entry[group]{The @sym{g-action-group} object that changed.}
        @entry[name]{A string with the name of the action.}
        @entry[parameter]{The new @type{g-variant} parameter of the state.}
      @end{table}
  @end{dictionary}
  @see-class{g-simple-action-group}")

;;; ----------------------------------------------------------------------------
;;; g_action_group_list_actions ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_list_actions" g-action-group-list-actions) g-strv
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @return{A list of strings with the names of the actions in the group.}
  @begin{short}
    Lists the actions contained within the action group.
  @end{short}
  @see-class{g-action-group}"
  (group (g-object g-action-group)))

(export 'g-action-group-list-actions)

;;; ----------------------------------------------------------------------------
;;; g_action_group_query_action ()
;;;
;;; gboolean g_action_group_query_action (GActionGroup *action_group,
;;;                                       const gchar *action_name,
;;;                                       gboolean *enabled,
;;;                                       const GVariantType **parameter_type,
;;;                                       const GVariantType **state_type,
;;;                                       GVariant **state_hint,
;;;                                       GVariant **state);
;;;
;;; Queries all aspects of the named action within an action_group.
;;;
;;; This function acquires the information available from
;;;
;;;   g_action_group_has_action(),
;;;   g_action_group_get_action_enabled(),
;;;   g_action_group_get_action_parameter_type(),
;;;   g_action_group_get_action_state_type(),
;;;   g_action_group_get_action_state_hint() and
;;;   g_action_group_get_action_state()
;;;
;;; with a single function call.
;;;
;;; This provides two main benefits.
;;;
;;; The first is the improvement in efficiency that comes with not having to
;;; perform repeated lookups of the action in order to discover different things
;;; about it. The second is that implementing GActionGroup can now be done by
;;; only overriding this one virtual function.
;;;
;;; The interface provides a default implementation of this function that calls
;;; the individual functions, as required, to fetch the information. The
;;; interface also provides default implementations of those functions that call
;;; this function. All implementations, therefore, must override either this
;;; function or all of the others.
;;;
;;; If the action exists, TRUE is returned and any of the requested fields (as
;;; indicated by having a non-NULL reference passed in) are filled. If the
;;; action does not exist, FALSE is returned and the fields may or may not have
;;; been modified.
;;;
;;; action_group :
;;;     a GActionGroup
;;;
;;; action_name :
;;;     the name of an action in the group
;;;
;;; enabled :
;;;     if the action is presently enabled
;;;
;;; parameter_type :
;;;     the parameter type, or NULL if none needed
;;;
;;; state_type :
;;;     the state type, or NULL if stateless
;;;
;;; state_hint :
;;;     the state hint, or NULL if none
;;;
;;; state :
;;;     the current state, or NULL if stateless
;;;
;;; Returns :
;;;     TRUE if the action exists, else FALSE
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_action_group_has_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_has_action" g-action-group-has-action) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of the action to check for}
  @return{A boolean whether the named action exists.}
  @begin{short}
    Checks if the named action exists within the action group.
  @end{short}
  @see-class{g-action-group}"
  (group (g-object g-action-group))
  (name :string))

(export 'g-action-group-has-action)

;;; ----------------------------------------------------------------------------
;;; g_action_group_get_action_enabled () -> g-action-group-action-enabled
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_get_action_enabled" g-action-group-action-enabled)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of the action to query}
  @return{A boolean whether or not the action is currently enabled.}
  @begin{short}
    Checks if the named action within the action group is currently enabled.
  @end{short}

  An action must be enabled in order to be activated or in order to have its
  state changed from outside callers.
  @see-class{g-action-group}"
  (group (g-object g-action-group))
  (name :string))

(export 'g-action-group-action-enabled)

;;; ----------------------------------------------------------------------------
;;; g_action_group_get_action_parameter_type ()
;;; -> g-action-group-action-parameter-type
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_get_action_parameter_type"
           g-action-group-action-parameter-type)
    (g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of the action to query}
  @return{The @class{g-variant-type} parameter type.}
  @begin{short}
    Queries the type of the parameter that must be given when activating the
    named action within the action group.
  @end{short}

  When activating the action using the @fun{g-action-group-activate-action}
  function, the @class{g-variant-type} parameter type given to that function
  must be of the type returned by this function.

  In the case that this function returns @code{nil}, you must not give any
  @type{g-variant} parameter, but @code{nil} instead.

  The parameter type of a particular action will never change but it is
  possible for an action to be removed and for a new action to be added with
  the same name but a different parameter type.
  @begin[Note]{dictionary}
    This function does not return @code{nil} as expected for an action with
    no @class{g-variant-type} parameter type. Your will get a correct return
    value using the @fun{g-action-map-lookup-action} function to get the action
    from the action group and the @fun{g-action-parameter-type} function to get
    the parameter type.
  @end{dictionary}
  @see-class{g-action-group}
  @see-class{g-variant-type}
  @see-type{g-variant}
  @see-function{g-action-group-activate-action}
  @see-function{g-action-map-lookup-action}
  @see-function{g-action-parameter-type}"
  (group (g-object g-action-group))
  (name :string))

(export 'g-action-group-action-parameter-type)

;;; ----------------------------------------------------------------------------
;;; g_action_group_get_action_state_type () -> g-action-group-action-state-type
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_get_action_state_type"
           g-action-group-action-state-type) (g-boxed-foreign g-variant-type)
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of the action to query}
  @return{The @class{g-variant-type} state type, if the action is stateful.}
  @begin{short}
    Queries the type of the state of the named action within the action group.
  @end{short}

  If the action is stateful then this function returns the
  @class{g-variant-type} state type of the state. All calls to the
  @fun{g-action-group-change-action-state} function must give a @type{g-variant}
  parameter of this type and the @fun{g-action-group-action-state} function
  will return a @type{g-variant} parameter of the same type.

  If the action is not stateful then this function will return @code{nil}. In
  that case, the @fun{g-action-group-action-state} function will return
  @code{nil} and you must not call the @fun{g-action-group-change-action-state}
  function.

  The state type of a particular action will never change but it is possible
  for an action to be removed and for a new action to be added with the same
  name but a different state type.
  @begin[Note]{dictionary}
    This function does not return @code{nil} as expected for an action with
    no @class{g-variant-type} state type. Your will get a correct return
    value using the @fun{g-action-map-lookup-action} function to get the action
    from the action group and the @fun{g-action-state-type} function to get
    the parameter type.
  @end{dictionary}
  @see-class{g-action-group}
  @see-class{g-variant-type}
  @see-type{g-variant}
  @see-function{g-action-group-change-action-state}
  @see-function{g-action-group-action-state}
  @see-function{g-action-map-lookup-action}
  @see-function{g-action-state-type}"
  (group (g-object g-action-group))
  (name :string))

(export 'g-action-group-action-state-type)

;;; ----------------------------------------------------------------------------
;;; g_action_group_get_action_state_hint () -> g-action-group-action-state-hint
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_get_action_state_hint"
           g-action-group-action-state-hint) (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of the action to query}
  @return{The @type{g-variant} state range hint, or a @code{null-pointer}.}
  @begin{short}
    Requests a hint about the valid range of values for the state of the named
    action within the action group.
  @end{short}

  If a @code{null-pointer} is returned it either means that the action is not
  stateful or that there is no hint about the valid range of values for the
  state of the action.

  If a @type{g-variant} parameter array is returned then each item in the array
  is a possible value for the state. If a @type{g-variant} parameter pair, i.e.
  two-tuple, is returned then the tuple specifies the inclusive lower and upper
  bound of valid values for the state.

  In any case, the information is merely a hint. It may be possible to have a
  state value outside of the hinted range and setting a value within the range
  may fail.
  @see-class{g-action-group}
  @see-type{g-variant}"
  (group (g-object g-action-group))
  (name :string))

(export 'g-action-group-action-state-hint)

;;; ----------------------------------------------------------------------------
;;; g_action_group_get_action_state () -> g-action-group-action-state
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_get_action_state" g-action-group-action-state)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of the action to query}
  @return{The current @type{g-variant} state of the action, or a
    @code{null-pointer}.}
  @begin{short}
    Queries the current state of the named action within the action group.
  @end{short}

  If the action is not stateful then a @code{null-pointer} will be returned. If
  the action is stateful then the type of the return value is the type given by
  the @fun{g-action-group-action-state-type} function.
  @see-class{g-action-group}
  @see-type{g-variant}
  @see-function{g-action-group-action-state-type}"
  (group (g-object g-action-group))
  (name :string))

(export 'g-action-group-action-state)

;;; ----------------------------------------------------------------------------
;;; g_action_group_change_action_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_change_action_state"
           g-action-group-change-action-state) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of the action to request the change on}
  @argument[parameter]{the new @type{g-variant} state}
  @begin{short}
    Request for the state of the named action within the action group to be
    changed to the @arg{parameter} argument.
  @end{short}
  The action must be stateful and @arg{parameter} must be of the correct type,
  see the @fun{g-action-group-action-state-type} function. This call merely
  requests a change. The action may refuse to change its state or may change
  its state to something other than value, see the
  @fun{g-action-group-action-state-hint} function.
  @see-class{g-action-group}
  @see-type{g-variant}
  @see-function{g-action-group-action-state-type}
  @see-function{g-action-group-action-state-hint}"
  (group (g-object g-action-group))
  (name :string)
  (parameter (:pointer (:struct g-variant))))

(export 'g-action-group-change-action-state)

;;; ----------------------------------------------------------------------------
;;; g_action_group_activate_action ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_activate_action" g-action-group-activate-action) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of the action to activate}
  @argument[parameter]{the @type{g-variant} parameter to the activation}
  @begin{short}
    Activate the named action within the action group.
  @end{short}
  If the action is expecting a parameter, then the correct type of the parameter
  must be given as @arg{parameter}. If the action is expecting no parameters
  then the @arg{parameter} argument must be a @code{null-pointer}, see the
  @fun{g-action-group-action-parameter-type} function.
  @see-class{g-action-group}
  @see-function{g-action-group-action-parameter-type}"
  (group (g-object g-action-group))
  (name :string)
  (parameter :pointer))

(export 'g-action-group-activate-action)

;;; ----------------------------------------------------------------------------
;;; g_action_group_action_added ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_action_added" g-action-group-action-added) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of an action in the group}
  @begin{short}
    Emits the \"action-added\" signal on the action group.
  @end{short}
  This function should only be called by @class{g-action-group} implementations.
  @see-class{g-action-group}"
  (group (g-object g-action-group))
  (name :string))

(export 'g-action-group-action-added)

;;; ----------------------------------------------------------------------------
;;; g_action_group_action_removed ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_action_removed" g-action-group-action-removed) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of an action in the action group}
  @begin{short}
    Emits the \"action-removed\" signal on the action group.
  @end{short}
  This function should only be called by @class{g-action-group} implementations.
  @see-class{g-action-group}"
  (group (g-object g-action-group))
  (name :string))

(export 'g-action-group-action-removed)

;;; ----------------------------------------------------------------------------
;;; g_action_group_action_enabled_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_action_enabled_changed"
           g-action-group-action-enabled-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of an action in the action group}
  @argument[enabled]{a boolean whether or not the action is now enabled}
  @begin{short}
    Emits the \"action-enabled-changed\" signal on the action group.
  @end{short}
  This function should only be called by @class{g-action-group}
  implementations.
  @see-class{g-action-group}"
  (group (g-object g-action-group))
  (name :string)
  (enabled :boolean))

(export 'g-action-group-action-enabled-changed)

;;; ----------------------------------------------------------------------------
;;; g_action_group_action_state_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_group_action_state_changed"
           g-action-group-action-state-changed) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-9-8}
  @argument[group]{a @class{g-action-group} object}
  @argument[name]{a string with the name of an action in the group}
  @argument[state]{the new @type{g-variant} state of the named action}
  @begin{short}
    Emits the \"action-state-changed\" signal on the action group.
  @end{short}
  This function should only be called by @class{g-action-group} implementations.
  @see-class{g-action-group}
  @see-type{g-variant}"
  (group (g-object g-action-group))
  (name :string)
  (state (:pointer (:struct g-variant))))

(export 'g-action-group-action-state-changed)

;;; --- End of file gio.action-group.lisp --------------------------------------
