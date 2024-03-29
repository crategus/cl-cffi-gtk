;;; ----------------------------------------------------------------------------
;;; gio.simple-action.lisp
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
;;; GSimpleAction
;;;
;;;     A simple GAction implementation
;;;
;;; Types and Values
;;;
;;;     GSimpleAction
;;;
;;; Functions
;;;
;;;     g_simple_action_new
;;;     g_simple_action_new_stateful
;;;
;;;     g_simple_action_set_enabled
;;;     g_simple_action_set_state
;;;     g_simple_action_set_state_hint
;;;
;;; Properties
;;;
;;;       gboolean    enabled           Read / Write
;;;          gchar*   name              Read / Write / Construct Only
;;;   GVariantType*   parameter-type    Read / Write / Construct Only
;;;       GVariant*   state             Read / Write / Construct
;;;   GVariantType*   state-type        Read
;;;
;;; Signals
;;;
;;;           void    activate          Run Last
;;;           void    change-state      Run Last
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GSimpleAction
;;;
;;; Implemented Interfaces
;;;
;;; GSimpleAction implements GAction.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; GSimpleAction
;;; ----------------------------------------------------------------------------

(define-g-object-class "GSimpleAction" g-simple-action
  (:superclass g-object
   :export t
   :interfaces ("GAction")
   :type-initializer "g_simple_action_get_type")
  ((enabled
    g-simple-action-enabled
    "enabled" "gboolean" t t)
   (name
    g-simple-action-name
    "name" "gchararray" t nil)
   (parameter-type
    g-simple-action-parameter-type
    "parameter-type" "GVariantType" t nil)
   (state
    g-simple-action-state
    "state" "GVariant" t t)
   (state-type
    g-simple-action-state-type
    "state-type" "GVariantType" t nil)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-simple-action 'type)
 "@version{2021-8-1}
  @begin{short}
    A @sym{g-simple-action} object is the obvious simple implementation of the
    @class{g-action} interface.
  @end{short}
  This is the easiest way to create an action for purposes of adding it to a
  @class{g-simple-action-group} object.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (action parameter)    :run-last
      @end{pre}
      Indicates that the action was just activated. The argument @arg{parameter}
      will always be of the expected type. In the event that an incorrect type
      was given, no signal will be emitted.
      @begin[code]{table}
        @entry[action]{The @sym{g-simple-action} object.}
        @entry[parameter]{The @type{g-variant} parameter to the activation.}
      @end{table}
    @subheading{The \"change-state\" signal}
      @begin{pre}
 lambda (action value)    :run-last
      @end{pre}
      Indicates that the action just received a request to change its state.
      The argument @arg{value} will always be of the correct state type. In the
      event that an incorrect type was given, no signal will be emitted.

      If no handler is connected to this signal then the default behaviour is
      to call the function @fun{g-simple-action-state} to set the state to the
      requested value. If you connect a signal handler then no default action
      is taken. If the state should change then you must call the function
      @fun{g-simple-action-state} from the handler.

      @b{Example:} Implementation of a \"change-state\" handler
      @begin{pre}
(g-signal-connect action \"change-state\"
                  (lambda (action value)
                    (let ((requested (g-variant-int32 value)))
                      ;; Volume only goes from 0 to 10
                      (when (and (>= requested 0) (<= requested 10))
                        (setf (g-simple-action-state action) value)))))
      @end{pre}
      The handler need not set the state to the requested value. It could set
      it to any value at all, or take some other action.
      @begin[code]{table}
        @entry[action]{The @sym{g-simple-action} object.}
        @entry[value]{The requested @type{g-variant} value for the state.}
      @end{table}
  @end{dictionary}
  @see-slot{g-simple-action-enabled}
  @see-slot{g-simple-action-name}
  @see-slot{g-simple-action-parameter-type}
  @see-slot{g-simple-action-state}
  @see-slot{g-simple-action-state-type}
  @see-class{g-action}
  @see-class{g-simple-action-group}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g-simple-action-enabled ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "enabled" 'g-simple-action) 't)
 "The @code{enabled} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action is currently enabled. If the action is disabled then calls
  to the functions @fun{g-action-activate} and @fun{g-action-change-state} have
  no effect. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-simple-action-enabled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-simple-action-enabled 'function)
 "@version{2021-8-1}
  @syntax[]{(g-simple-action-enabled object) => enabled}
  @syntax[]{(setf (g-simple-action-enabled object) enabled)}
  @argument[object]{a @class{g-simple-action} object}
  @argument[enabled]{a boolean whether the action is enabled}
  @begin{short}
    Accessor of the @slot[g-simple-action]{enabled} slot of the
    @class{g-simple-action} class.
  @end{short}

  Sets the action as enabled or not. An action must be enabled in order to be
  activated or in order to have its state changed from outside callers.

  This should only be called by the implementor of the action. Users of the
  action should not attempt to modify its enabled flag.
  @see-class{g-simple-action}")

;;; --- g-simple-action-name ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'g-simple-action) 't)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  The name of the action. This is mostly meaningful for identifying the action
  once it has been added to a @class{g-simple-action-group} object. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-simple-action-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-simple-action-name 'function)
 "@version{2021-8-1}
  @syntax[]{(g-simple-action-name object) => name}
  @argument[object]{a @class{g-simple-action} object}
  @argument[name]{a string with the name of the action}
  @begin{short}
    Accessor of the @slot[g-simple-action]{name} slot of the
    @class{g-simple-action} class.
  @end{short}

  The name of the action. This is mostly meaningful for identifying the action
  once it has been added to a @class{g-simple-action-group} object.
  @see-class{g-simple-action}
  @see-class{g-simple-action-group}")

;;; --- g-simple-action-parameter-type -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "parameter-type"
                                               'g-simple-action) 't)
 "The @code{parameter-type} property of type @class{g-variant-type}
  (Read / Write / Construct Only) @br{}
  The type of the parameter that must be given when activating the action.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-simple-action-parameter-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-simple-action-parameter-type 'function)
 "@version{2021-8-1}
  @syntax[]{(g-simple-action-parameter-type object) => vtype}
  @argument[object]{a @class{g-simple-action} object}
  @argument[vtype]{a @class{g-variant-type} instance}
  @begin{short}
    Accessor of the @slot[g-simple-action]{parameter-type} slot of the
    @class{g-simple-action} class.
  @end{short}

  The type of the parameter that must be given when activating the action.
  @see-class{g-simple-action}
  @see-class{g-variant-type}")

;;; --- g-simple-action-state --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state" 'g-simple-action) 't)
 "The @code{state} property of type @type{g-variant} (Read / Write / Construct)
  @br{}
  The state of the action, or @code{nil} if the action is stateless. @br{}
  Allowed values: @code{GVariant<*>} @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-simple-action-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-simple-action-state 'function)
 "@version{2021-8-1}
  @syntax[]{(g-simple-action-state object) => value}
  @syntax[]{(setf (g-simple-action-state object) value)}
  @argument[object]{a @class{g-simple-action} object}
  @argument[value]{the @type{g-variant} value for the state}
  @begin{short}
    Accessor of the @slot[g-simple-action]{state} slot of the
    @class{g-simple-action} class.
  @end{short}

  Sets the state of the action to @arg{value}. This directly updates the
  @slot[g-simple-action]{state} property to the given @arg{value}.

  This should only be called by the implementor of the action. Users of the
  action should not attempt to directly modify the @slot[g-simple-action]{state}
  property. Instead, they should call the function @fun{g-action-change-state}
  to request the change.
  @see-class{g-simple-action}
  @see-type{g-variant}
  @see-function{g-action-change-state}")

;;; ---g-simple-action-state-type ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state-type"
                                               'g-simple-action) 't)
 "The @code{state-type} property of type @class{g-variant-type} (Read) @br{}
  The type of the state that the action has, or @code{nil} if the action is
  stateless.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-simple-action-state-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-simple-action-state-type 'function)
 "@version{2021-8-1}
  @syntax[]{(g-simple-action-state-type object) => vtype}
  @argument[object]{a @class{g-simple-action} object}
  @argument[vtype]{a @class{g-variant-type} instance}
  @begin{short}
    Accessor of the @slot[g-simple-action]{state-type} slot of the
    @class{g-simple-action} class.
  @end{short}

  The type of the state that the action has, or @code{nil} if the action is
  stateless.
  @see-class{g-simple-action}
  @see-class{g-variant-type}")

;;; ----------------------------------------------------------------------------
;;; g_simple_action_new ()
;;; ----------------------------------------------------------------------------

(defun g-simple-action-new (name vtype)
 #+cl-cffi-gtk-documentation
 "@version{*2021-10-31}
  @argument[name]{a string with the name of the action}
  @argument[vtype]{the @class{g-variant-type} type or a type string for the
    parameter to the activate function}
  @return{A new @class{g-simple-action} object.}
  @begin{short}
    Creates a new action.
  @end{short}
  The created action is stateless. See the @fun{g-simple-action-new-stateful}
  function for a stateful action.
  @begin[Note]{dictionary}
    A type string for the @arg{vtype} argument is converted to the
    @class{g-variant-type} type with the @fun{g-variant-type-new} function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    A simple action with no parameter type.
    @begin{pre}
(defvar action (g-simple-action-new \"clear\" nil)) => ACTION
(g-action-name action) => \"clear\"
(g-action-enabled action) => T
(g-action-parameter-type action) => NIL
(g-action-state action) => #.(SB-SYS:INT-SAP #X00000000)
(g-action-state-type action) => NIL
    @end{pre}
    A simple action with a string parameter type.
    @begin{pre}
(setf action (g-simple-action-new \"check\" \"s\"))
=> #<G-SIMPLE-ACTION {10022B4AF3@}>
(g-action-name action) => \"check\"
(g-action-enabled action) => T
(g-action-parameter-type action) => #<G-VARIANT-TYPE {10022B5AB3@}>
(g-action-state action) => #.(SB-SYS:INT-SAP #X00000000)
(g-action-state-type action) => NIL
    @end{pre}
  @end{dictionary}
  @see-class{g-simple-action}
  @see-class{g-variant-type}
  @see-function{g-simple-action-new-stateful}
  @see-function{g-variant-type-new}"
  (if (stringp vtype)
      (let ((vtype1 (g-variant-type-new vtype)))
        (unwind-protect
          (make-instance 'g-simple-action
                         :name name
                         :parameter-type vtype1)
          (g-variant-type-free vtype1)))
      (make-instance 'g-simple-action
                     :name name
                     :parameter-type vtype)))

(export 'g-simple-action-new)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_new_stateful ()
;;; ----------------------------------------------------------------------------

(defun g-simple-action-new-stateful (name vtype state)
 #+cl-cffi-gtk-documentation
 "@version{*2021-10-31}
  @argument[name]{a string with the name of the action}
  @argument[vtype]{the @class{g-variant-type} type or a type string of the
    parameter to the activate function}
  @argument[state]{the initial @symbol{g-variant} state of the action}
  @return{A new @class{g-simple-action} object.}
  @begin{short}
    Creates a new stateful action.
  @end{short}
  The @arg{state} argument is the initial state of the action. All future state
  values must have the same @class{g-variant-type} type as the initial state.
  @begin[Note]{dictionary}
    A type string for the @arg{vtype} argument is converted to the
    @class{g-variant-type} type with the @fun{g-variant-type-new} function.
  @end{dictionary}
  @see-class{g-simple-action}
  @see-type{g-variant}
  @see-class{g-variant-type}
  @see-function{g-simple-action-new}
  @see-function{g-variant-type-new}"
  (if (stringp vtype)
      (let ((vtype1 (g-variant-type-new vtype)))
        (unwind-protect
          (make-instance 'g-simple-action
                         :name name
                         :parameter-type vtype1
                         :state state)
          (g-variant-type-free vtype1)))
      (make-instance 'g-simple-action
                     :name name
                     :parameter-type vtype
                     :state state)))

(export 'g-simple-action-new-stateful)

;;; ----------------------------------------------------------------------------
;;; g_simple_action_set_state_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_simple_action_set_state_hint" g-simple-action-set-state-hint) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-8-1}
  @argument[action]{a @class{g-simple-action} object}
  @argument[hint]{a @type{g-variant} value representing the state hint}
  @begin{short}
    Sets the state hint for the action.
  @end{short}
  See the function @fun{g-action-state-hint} for more information about action
  state hints.

  Since 2.44
  @see-class{g-simple-action}
  @see-function{g-action-state-hint}"
  (action (g-object g-simple-action))
  (hint (:pointer (:struct g-variant))))

(export 'g-simple-action-set-state-hint)

;;; --- End of file gio.simple-action.lisp -------------------------------------
