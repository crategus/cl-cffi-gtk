;;; ----------------------------------------------------------------------------
;;; gio.action.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.62 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2020 Dieter Kaiser
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
;;;     An action interface
;;;
;;; Types and Values
;;;
;;;     GAction
;;;
;;; Functions
;;;
;;;     g_action_name_is_valid
;;;     g_action_get_name                                  Accessor
;;;     g_action_get_parameter_type                        Accessor
;;;     g_action_get_state_type                            Accessor
;;;     g_action_get_state_hint
;;;     g_action_get_enabled                               Accessor
;;;     g_action_get_state                                 Accessor
;;;     g_action_change_state
;;;     g_action_activate
;;;     g_action_parse_detailed_name
;;;     g_action_print_detailed_name
;;;
;;; Properties
;;;
;;;         gboolean    enabled           Read
;;;            gchar*   name              Read
;;;     GVariantType*   parameter-type    Read
;;;         GVariant*   state             Read
;;;     GVariantType*   state-type        Read
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GAction
;;;
;;; Prerequisites
;;;
;;;     GAction requires GObject.
;;;
;;; Known Implementations
;;;
;;;     GAction is implemented by GPropertyAction and GSimpleAction.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GAction
;;; ----------------------------------------------------------------------------

(define-g-interface "GAction" g-action
  (:export t
   :type-initializer "g_action_get_type")
  (enabled
   g-action-enabled
   "enabled" "gboolean" t nil)
  (name
   g-action-name
   "name" "gchararray" t nil)
  (parameter-type
   g-action-parameter-type
   "parameter-type" "GVariantType" t nil)
  (state
   g-action-state
   "state" "GVariant" t nil)
  (state-type
   g-action-state-type
   "state-type" "GVariantType" t nil))

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action atdoc:*class-name-alias*) "Interface"
      (documentation 'g-action 'type)
 "@version{2020-2-8}
  @begin{short}
    The @sym{g-action} interface represents a single named action.
  @end{short}

  The main interface to an action is that it can be activated with the function
  @fun{g-action-activate}. This results in the \"activate\" signal being
  emitted. An activation has a @type{g-variant} parameter, which may be
  @code{nil}. The correct type for the parameter is determined by a static
  parameter type, which is given at construction time.

  An action may optionally have a state, in which case the state may be set
  with the function @fun{g-action-change-state}. This call takes a
  @type{g-variant} parameter. The correct type for the state is determined by
  a static state type, which is given at construction time.

  The state may have a hint associated with it, specifying its valid range.

  @sym{g-action} is merely the interface to the concept of an action, as
  described above. Various implementations of actions exist, including
  @class{g-simple-action}.

  In all cases, the implementing class is responsible for storing the name of
  the action, the parameter type, the enabled state, the optional state type
  and the state and emitting the appropriate signals when these change. The
  implementor responsible for filtering calls to the functions
  @fun{g-action-activate} and @fun{g-action-change-state} for type safety and
  for the state being enabled.

  Probably the only useful thing to do with a @sym{g-action} is to put it inside
  of a @class{g-simple-action-group}.
  @see-slot{g-action-enabled}
  @see-slot{g-action-name}
  @see-slot{g-action-parameter-type}
  @see-slot{g-action-state}
  @see-slot{g-action-state-type}
  @see-class{g-simple-action}
  @see-class{g-simple-action-group}
  @see-function{g-action-activate}
  @see-function{g-action-change-state}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g-action-enabled -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enabled" 'g-action) 't)
 "The @code{enabled} property of type @code{:boolean} (Read) @br{}
  If the action is currently enabled. If the action is disabled then calls to
  the functions @fun{g-action-activate} and @fun{g-action-change-state} have no
  effect. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-enabled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-enabled 'function)
 "@version{2020-2-8}
  @syntax[]{(g-action-enabled object) => enabled}
  @argument[object]{a @class{g-action} object}
  @argument[enabled]{a boolean whether the @arg{action} is enabled}
  @begin{short}
    Accessor of the @slot[g-action]{enabled} slot of the @class{g-action} class.
  @end{short}

  Checks if the action is currently enabled. An action must be enabled in order
  to be activated or in order to have its state changed from outside callers.
  @see-class{g-action}")

;;; --- g-action-name ----------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'g-action) 't)
 "The @code{name} property of type @code{:string} (Read) @br{}
  The name of the action. This is mostly meaningful for identifying the action
  once it has been added to a @class{g-action-group} object. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-name 'function)
 "@version{*2021-5-11}
  @syntax[]{(g-action-name object) => name}
  @argument[action]{a @class{g-action} object}
  @argument[name]{a string with the name of the action}
  @begin{short}
    Accessor of the @slot[g-action]{name} slot of the @class{g-action} class.
  @end{short}
  Queries the name of the action.
  @see-class{g-action}")

;;; --- g-action-parameter-type ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "parameter-type" 'g-action) 't)
 "The @code{parameter-type} property of type @class{g-variant-type}
  (Read) @br{}
  The type of the parameter that must be given when activating the action.
  @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-parameter-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-parameter-type 'function)
 "@version{2020-2-8}
  @syntax[]{(g-action-parameter-type object) => type}
  @argument[object]{a @class{g-action} object}
  @argument[type]{the @class{g-variant-type} parameter type}
  @begin{short}
    Accessor of the @slot[g-action]{parameter-type} slot of the
    @class{g-action} class.
  @end{short}

  Queries the type of the parameter that must be given when activating the
  action.

  When activating the the action using the function @fun{g-action-activate},
  the @type{g-variant} given to that function must be of the type returned by
  this function.

  In the case that this function returns @code{nil}, you must not give any
  @type{g-variant}, but @code{nil} instead.
  @see-class{g-action}")

;;; --- g-application-state ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state" 'g-action) 't)
 "The @code{state} property of type @type{g-variant} (Read) @br{}
  The state of the action, or @code{nil} if the action is stateless. @br{}
  Allowed values: @code{GVariant<*>} @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-state 'function)
 "@version{*2021-5-11}
  @syntax[]{(g-action-state object) => state}
  @argument[object]{a @class{g-action} object}
  @argument[state]{the current @type{g-variant} state of the action}
  @begin{short}
    Accessor of the @slot[g-action]{state} slot of the @class{g-action} class.
  @end{short}

  Queries the current state of the action.

  If the action is not stateful then @code{nil} will be returned. If the
  action is stateful then the type of the return value is the type given
  by the function @fun{g-action-state-type}.
  @see-class{g-action}
  @see-function{g-action-state-type}")

;;; --- g-action-state-type ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state-type" 'g-action) 't)
 "The @code{state-type} property of type @class{g-variant-type} (Read) @br{}
  The @class{g-variant-type} of the state that the action has, or @code{nil} if
  the action is stateless. @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-state-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-state-type 'function)
 "@version{2020-2-8}
  @argument[object]{a @class{g-action} object}
  @argument[state-type]{the @class{g-variant-type} state type, if the action is
    stateful}
  @begin{short}
    Accessor of the @slot[g-application]{state-type} slot of the
    @class{g-action} class.
  @end{short}

  Queries the type of the state of the action.

  If the action is stateful, e.g. created with the function
  @fun{g-simple-action-new-stateful}, then this function returns the
  @class{g-variant-type} of the state. This is the type of the initial value
  given as the state. All calls to the function @fun{g-action-change-state} must
  give a @type{g-variant} of this type and the function @fun{g-action-state}
  will return a @type{g-variant} of the same type.

  If the action is not stateful, e.g. created with the function
  @fun{g-simple-action-new}, then this function will return @code{nil}. In that
  case, the function @fun{g-action-state} will return @code{nil} and you
  must not call the function @fun{g-action-change-state}.
  @see-class{g-action}")

;;; ----------------------------------------------------------------------------
;;; g_action_name_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_name_is_valid" g-action-name-is-valid) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-2-8}
  @argument[name]{a @code{:string} with a action name}
  @return{@var{True} if @arg{name} is a valid action name.}
  @begin{short}
    Checks if the action name is valid.
  @end{short}
  The action name is valid if it consists only of alphanumeric characters, plus
  '-' and '.'. The empty string is not a valid action name.

  It is an error to call this function with a non-utf8 action name.
  @arg{action-name} must not be @code{nil}.
  @see-class{g-action}"
  (name :string))

(export 'g-action-name-is-valid)

;;; ----------------------------------------------------------------------------
;;; g_action_get_state_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_get_state_hint" g-action-get-state-hint)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2020-2-8}
  @argument[action]{a @class{g-action} object}
  @return{The @type{g-variant} state range hint.}
  @begin{short}
    Requests a hint about the valid range of values for the state of the action.
  @end{short}

  If @code{nil} is returned it either means that the action is not stateful or
  that there is no hint about the valid range of values for the state of the
  action.

  If a @type{g-variant} array is returned then each item in the array is a
  possible value for the state. If a @type{g-variant} pair, i.e. two-tuple,
  is returned then the tuple specifies the inclusive lower and upper bound of
  valid values for the state.

  In any case, the information is merely a hint. It may be possible to have a
  state value outside of the hinted range and setting a value within the range
  may fail.
  @see-class{g-action}
  @see-type{g-variant}"
  (action (g-object g-action)))

(export 'g-action-get-state-hint)

;;; ----------------------------------------------------------------------------
;;; g_action_change_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_change_state" g-action-change-state) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-2-8}
  @argument[action]{a @class{g-action} object}
  @argument[value]{the new @type{g-variant} state}
  @begin{short}
    Request for the state of the action to be changed to @arg{value}.
  @end{short}

  The the action must be stateful and @arg{value} must be of the correct type.
  See the function @fun{g-action-state-type}.

  This call merely requests a change. The action may refuse to change its
  state or may change its state to something other than @arg{value}. See the
  function @fun{g-action-get-state-hint}.
  @see-class{g-action}
  @see-type{g-variant}
  @see-function{g-action-state-type}
  @see-function{g-action-get-state-hint}"
  (action (g-object g-action))
  (value (:pointer (:struct g-variant))))

(export 'g-action-change-state)

;;; ----------------------------------------------------------------------------
;;; g_action_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_activate" %g-action-activate) :void
  (action :pointer)
  (parameter :pointer))

(defun g-action-activate (action parameter)
 #+cl-cffi-gtk-documentation
 "@version{2020-2-8}
  @argument[action]{a @class{g-action} object}
  @argument[parameter]{the @class{g-variant} parameter to the activation}
  @short{Activates the action.}

  @arg{parameter} must be the correct type of the parameter for the action,
  i.e. the parameter type given at construction time. If the parameter type
  was @code{nil} then @arg{parameter} must also be @code{nil}.
  @see-class{g-action}"
  (%g-action-activate (pointer action) parameter))

(export 'g-action-activate)

;;; ----------------------------------------------------------------------------
;;; g_action_parse_detailed_name ()
;;; ----------------------------------------------------------------------------

;; TODO: More work is needed. The implementation seems not to be fully workable.

(defcfun ("g_action_parse_detailed_name" %g-action-parse-detailed-name) :boolean
  (detailed-name :string)
  (action-name :string)
  (target-value (:pointer (:struct g-variant)))
  (error :pointer))

(defun g-action-parse-detailed-name (detailed-name action-name target-value)
 #+cl-cffi-gtk-documentation
 "@version{#2020-2-8}
  @argument[detailed-name]{a @code{:string} with a detailed action name}
  @argument[action-name]{a @code{:string} with the action name}
  @argument[target-value]{a @type{g-variant} target value, or @code{nil} for
    no target}
  @return{@arg{True} if successul, else @arg{false}.}
  @begin{short}
    Parses a detailed action name into its separate name and target components.
  @end{short}
  Detailed action names can have three formats.

  The first format is used to represent an action name with no target value and
  consists of just an action name containing no whitespace nor the characters
  ':', '(' or ')'. For example: \"app.action\".

  The second format is used to represent an action with a target value that is
  a non-empty string consisting only of alphanumerics, plus '-' and '.'. In that
  case, the action name and target value are separated by a double colon
  (\"::\"). For example: \"app.action::target\".

  The third format is used to represent an action with any type of target value,
  including strings. The target value follows the action name, surrounded in
  parens. For example: \"app.action(42)\". The target value is parsed using
  the function @fun{g-variant-parse}. If a tuple-typed value is desired, it must
  be specified in the same way, resulting in two sets of parens, for example:
  \"app.action((1,2,3))\". A string target can be specified this way as well:
  \"app.action('target')\". For strings, this third format must be used if *
  target value is empty or contains characters other than alphanumerics,
  '-' and '.'.
  @see-class{g-action}"
  (with-g-error (err)
    (%g-action-parse-detailed-name detailed-name
                                   action-name
                                   target-value
                                   err)))

(export 'g-action-parse-detailed-name)

;;; ----------------------------------------------------------------------------
;;; g_action_print_detailed_name ()
;;; ----------------------------------------------------------------------------

;; TODO: More work is needed. The implementation seems not to be fully workable.

(defcfun ("g_action_print_detailed_name" g-action-print-detailed-name) :string
 #+cl-cffi-gtk-documentation
 "@version{#2020-2-8}
  @argument[action-name]{a @code{:string} with a valid action name}
  @argument[target-value]{a @type{g-variant} target value, or @code{nil}}
  @begin{short}
    Formats a detailed action name from an action name and a target value.
  @end{short}

  It is an error to call this function with an invalid action name. This
  function is the opposite of the function @fun{g-action-parse-detailed-name}.
  It will produce a string that can be parsed back to the action name and
  target value by that function. See that function for the types of strings
  that will be printed by this function.
  @see-class{g-action}
  @see-function{g-action-parse-detailed-name}"
  (action-name :string)
  (target-value (:pointer (:struct g-variant))))

(export 'g-action-print-detailed-name)

;;; --- End of file gio.action.lisp --------------------------------------------
