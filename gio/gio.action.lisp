;;; ----------------------------------------------------------------------------
;;; gio.action.lisp
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
(setf (gethash 'g-action atdoc:*class-name-alias*)
      "Interface"
      (documentation 'g-action 'type)
 "@version{2021-8-1}
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

  The @sym{g-action} interface is merely the interface to the concept of an
  action, as described above. Various implementations of actions exist,
  including the @class{g-simple-action} class.

  In all cases, the implementing class is responsible for storing the name of
  the action, the parameter type, the enabled state, the optional state type
  and the state and emitting the appropriate signals when these change. The
  implementor responsible for filtering calls to the functions
  @fun{g-action-activate} and @fun{g-action-change-state} for type safety and
  for the state being enabled.

  Probably the only useful thing to do with a @sym{g-action} is to put it inside
  of a @class{g-simple-action-group} object.
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
  Whether the action is currently enabled. If the action is disabled then calls
  to the functions @fun{g-action-activate} and @fun{g-action-change-state} have
  no effect. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-enabled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-enabled 'function)
 "@version{2021-8-1}
  @syntax[]{(g-action-enabled object) => enabled}
  @argument[object]{a @class{g-action} object}
  @argument[enabled]{a boolean whether @arg{action} is enabled}
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
 "@version{*2021-10-8}
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
 "The @code{parameter-type} property of type @class{g-variant-type} (Read) @br{}
  The type of the parameter that must be given when activating the action.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-parameter-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-parameter-type 'function)
 "@version{2021-9-8}
  @syntax[]{(g-action-parameter-type object) => vtype}
  @argument[object]{a @class{g-action} object}
  @argument[vtype]{the @class{g-variant-type} parameter type}
  @begin{short}
    Accessor of the @slot[g-action]{parameter-type} slot of the
    @class{g-action} class.
  @end{short}

  Queries the type of the parameter that must be given when activating the
  action.

  When activating the action using the @fun{g-action-activate} function, the
  @type{g-variant} parameter given to that function must be of the type returned
  by this function.

  In the case that this function returns a @code{nil}, you must not give any
  @type{g-variant} parameter, but a @code{null-pointer} instead.
  @see-class{g-action}
  @see-class{g-variant-type}
  @see-type{g-variant}
  @see-function{g-action-activate}")

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
 "@version{*2021-10-8}
  @syntax[]{(g-action-state object) => state}
  @argument[object]{a @class{g-action} object}
  @argument[state]{the current @type{g-variant} state of the action}
  @begin{short}
    Accessor of the @slot[g-action]{state} slot of the @class{g-action} class.
  @end{short}

  Queries the current state of the action. If the action is not stateful then a
  @code{NULL} pointer will be returned. If the action is stateful then the type
  of the return value is the type given by the @fun{g-action-state-type}
  function.
  @see-class{g-action}
  @see-type{g-variant}
  @see-function{g-action-state-type}")

;;; --- g-action-state-type ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state-type" 'g-action) 't)
 "The @code{state-type} property of type @class{g-variant-type} (Read) @br{}
  The @class{g-variant-type} parameter type of the state that the action has,
  or @code{nil} if the action is stateless. @br{}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-state-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-state-type 'function)
 "@version{2021-9-9}
  @syntax[]{(g-action-state-type object) => vtype}
  @argument[object]{a @class{g-action} object}
  @argument[vtype]{the @class{g-variant-type} state type, if the action is
    stateful}
  @begin{short}
    Accessor of the @slot[g-application]{state-type} slot of the
    @class{g-action} class.
  @end{short}

  Queries the type of the state of the action.

  If the action is stateful, e.g. created with the
  @fun{g-simple-action-new-stateful} function, then this function returns the
  @class{g-variant-type} parameter type of the state. This is the type of the
  initial value given as the state. All calls to the @fun{g-action-change-state}
  function must give a @type{g-variant} parameter of this type and the
  @fun{g-action-state} function will return a @type{g-variant} parameter of the
  same type.

  If the action is not stateful, e.g. created with the
  @fun{g-simple-action-new} function, then this function will return @code{nil}.
  In that case, the @fun{g-action-state} function will return a
  @code{null-pointer} and you must not call the @fun{g-action-change-state}
  function.
  @see-class{g-action}
  @see-class{g-variant-type}
  @see-type{g-variant}
  @see-function{g-simple-action-new}
  @see-function{g-simple-action-new-stateful}
  @see-function{g-action-change-state}
  @see-function{g-action-state}")

;;; ----------------------------------------------------------------------------
;;; g_action_name_is_valid ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_name_is_valid" g-action-name-is-valid) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-8-1}
  @argument[name]{a string with an action name}
  @return{@em{True} if @arg{name} is a valid action name.}
  @begin{short}
    Checks if the action name is valid.
  @end{short}
  The action name is valid if it consists only of alphanumeric characters, plus
  '-' and '.'. The empty string is not a valid action name.

  It is an error to call this function with a non-utf8 action name.
  @begin[Example]{dictionary}
    @begin{pre}
(g-action-name-is-valid \"action\") => T
(g-action-name-is-valid \"win.action\") => T
(g-action-name-is-valid \"win-action\") => T
(g-action-name-is-valid \"win-action!\") NIL
(g-action-name-is-valid \"\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g-action}"
  (name :string))

(export 'g-action-name-is-valid)

;;; ----------------------------------------------------------------------------
;;; g_action_get_state_hint () -> g-action-state-hint
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_get_state_hint" g-action-state-hint)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2021-9-9}
  @argument[action]{a @class{g-action} object}
  @return{The @type{g-variant} state range hint.}
  @begin{short}
    Requests a hint about the valid range of values for the state of the action.
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
  @see-class{g-action}
  @see-type{g-variant}"
  (action (g-object g-action)))

(export 'g-action-state-hint)

;;; ----------------------------------------------------------------------------
;;; g_action_change_state ()
;;; ----------------------------------------------------------------------------

;; TODO: Is the implementation correct? See g-action-activate.

(defcfun ("g_action_change_state" g-action-change-state) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-1}
  @argument[action]{a @class{g-action} object}
  @argument[value]{the new @type{g-variant} state}
  @begin{short}
    Request for the state of the action to be changed to @arg{value}.
  @end{short}

  The the action must be stateful and @arg{value} must be of the correct type.
  See the function @fun{g-action-state-type}.

  This call merely requests a change. The action may refuse to change its
  state or may change its state to something other than @arg{value}. See the
  function @fun{g-action-state-hint}.
  @see-class{g-action}
  @see-type{g-variant}
  @see-function{g-action-state-type}
  @see-function{g-action-state-hint}"
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
 "@version{2021-8-1}
  @argument[action]{a @class{g-action} object}
  @argument[parameter]{the @class{g-variant} parameter to the activation}
  @short{Activates the action.}

  The argument @arg{parameter} must be the correct type of the parameter for the
  action, i.e. the parameter type given at construction time. If the parameter
  type was a @code{NULL} pointer then @arg{parameter} must also be a
  @code{NULL} pointer.
  @see-class{g-action}
  @see-type{g-variant}"
  (%g-action-activate (pointer action) parameter))

(export 'g-action-activate)

;;; ----------------------------------------------------------------------------
;;; g_action_parse_detailed_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_parse_detailed_name" %g-action-parse-detailed-name) :boolean
  (detailed :string)
  (name :string)
  (value (:pointer (:struct g-variant)))
  (err :pointer))

(defun g-action-parse-detailed-name (detailed)
 #+cl-cffi-gtk-documentation
 "@version{2021-8-1}
  @argument[detailed]{a string with a detailed action name}
  @begin{return}
     @code{name} - a string with the action name @br{}
     @code{value} - a @type{g-variant} target value, or a @code{NULL} pointer
     for no target
  @end{return}
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
  @begin[Examples]{dictionary}
    @begin{pre}
;; First format
(g-action-parse-detailed-name \"app.action\")
=> \"app.action\"
=> #.(SB-SYS:INT-SAP #X00000000)
;; Second format
(g-action-parse-detailed-name \"app.action::target\")
=> \"app.action\"
=> #.(SB-SYS:INT-SAP #X7F5B7000E8D0)
(g-variant-string
    (second (multiple-value-list
        (g-action-parse-detailed-name \"app.action::target\"))))
=> \"target\"
;; Third format
(g-action-parse-detailed-name \"app.action(42)\")
=> \"app.action\"
=> #.(SB-SYS:INT-SAP #X7F5B7000E870)
(g-variant-int32
    (second (multiple-value-list
        (g-action-parse-detailed-name \"app.action(42)\"))))
=> 42
    @end{pre}
  @end{dictionary}
  @see-class{g-action}
  @see-type{g-variant}
  @see-function{g-variant-parse}"
  (with-g-error (err)
    (with-foreign-objects ((name :string)
                           (value '(:pointer (:struct g-variant))))
      (when (%g-action-parse-detailed-name detailed name value err)
        (values (mem-ref name :string)
                (mem-ref value '(:pointer (:struct g-variant))))))))

(export 'g-action-parse-detailed-name)

;;; ----------------------------------------------------------------------------
;;; g_action_print_detailed_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_print_detailed_name" %g-action-print-detailed-name) :string
  (name :string)
  (value (:pointer (:struct g-variant))))

(defun g-action-print-detailed-name (name &optional (value nil))
 #+cl-cffi-gtk-documentation
 "@version{2021-8-1}
  @argument[name]{a string with a valid action name}
  @argument[value]{an optional @type{g-variant} target value}
  @begin{short}
    Formats a detailed action name from an action name and a target value.
  @end{short}

  It is an error to call this function with an invalid action name. This
  function is the opposite of the function @fun{g-action-parse-detailed-name}.
  It will produce a string that can be parsed back to the action name and
  target value by that function. See that function for the types of strings
  that will be printed by this function.
  @begin[Examples]{dictionary}
    @begin{pre}
(g-action-print-detailed-name \"action\")
=> \"action\"
(g-action-print-detailed-name \"action\" (g-variant-new-boolean \"t\"))
=> \"action(true)\"
(g-action-print-detailed-name \"action\" (g-variant-new-int32 42))
=> \"action(42)\"
    @end{pre}
  @end{dictionary}
  @see-class{g-action}
  @see-type{g-variant}
  @see-function{g-action-parse-detailed-name}"
  (%g-action-print-detailed-name name
                                 (if value
                                     value
                                     (null-pointer))))

(export 'g-action-print-detailed-name)

;;; --- End of file gio.action.lisp --------------------------------------------
