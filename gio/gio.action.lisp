;;; ----------------------------------------------------------------------------
;;; gio.action.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.36.4 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;;     g_action_get_enabled
;;;     g_action_get_state
;;;
;;;     g_action_change_state
;;;     g_action_activate
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
 "@version{2013-8-10}
  @begin{short}
    @sym{g-action} represents a single named action.
  @end{short}

  The main interface to an action is that it can be activated with the function
  @fun{g-action-activate}. This results in the 'activate' signal being emitted.
  An activation has a @type{g-variant} parameter, which may be @code{nil}.
  The correct type for the parameter is determined by a static parameter type,
  which is given at construction time.

  An action may optionally have a state, in which case the state may be set
  with the function @fun{g-action-change-state}. This call takes a
  @type{g-variant}. The correct type for the state is determined by a static
  state type, which is given at construction time.

  The state may have a hint associated with it, specifying its valid range.

  @sym{g-action} is merely the interface to the concept of an action, as
  described above. Various implementations of actions exist, including
  @class{g-simple-action} and @class{gtk-action}.

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
  @see-class{gtk-action}
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
  If action is currently enabled.
  If the action is disabled then calls to the functions @fun{g-action-activate}
  and @fun{g-action-change-state} have no effect. @br{}
  Default value: @em{true} @br{}
  Since 2.28")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-enabled atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-enabled 'function)
 "@version{2013-8-10}
  Accessor of the @slot[g-action]{enabled} slot of the @class{g-action} class.
  @see-class{g-action}
  @see-function{g-action-get-enabled}")

;;; --- g-action-name ----------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "name" 'g-action) 't)
 "The @code{name} property of type @code{:string} (Read) @br{}
  The name of the action. This is mostly meaningful for identifying the action
  once it has been added to a @class{g-action-group}. @br{}
  Default value: @code{nil} @br{}
  Since 2.28")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-name 'function)
 "@version{2013-8-10}
  Accessor of the @slot[g-action]{name} slot of the @class{g-action} class.
  @see-class{g-action}
  @see-function{g-action-get-name}")

;;; --- g-action-parameter-type ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "parameter-type" 'g-action) 't)
 "The @code{parameter-type} property of type @class{g-variant-type}
  (Read) @br{}
  The type of the parameter that must be given when activating the action. @br{}
  Since 2.28")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-parameter-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-parameter-type 'function)
 "@version{2013-8-10}
  Accessor of the @slot[g-action]{parameter-type} slot of the
  @class{g-action} class.
  @see-class{g-action}
  @see-function{g-action-get-parameter-type}")

;;; --- g-application-state ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state" 'g-action) 't)
 "The @code{state} property of type @type{g-variant} (Read) @br{}
  The state of the action, or @code{nil} if the action is stateless. @br{}
  Allowed values: @code{GVariant<*>} @br{}
  Default value: @code{nil} @br{}
  Since 2.28")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-state atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-state 'function)
 "@version{2013-8-10}
  Accessor of the @slot[g-action]{state} slot of the @class{g-action} class.
  @see-class{g-action}
  @see-function{g-action-get-state}")

;;; --- g-action-state-type ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "state-type" 'g-action) 't)
 "The @code{state-type} property of type @class{g-variant-type}
  (Read) @br{}
  The @class{g-variant-type} of the state that the action has, or @code{nil} if
  the action is stateless. @br{}
  Since 2.28")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-action-state-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-action-state-type 'function)
 "@version{2013-8-10}
  Accessor of the @slot[g-application]{state-type} slot of the @class{g-action}
  class.
  @see-class{g-action}
  @see-function{g-action-get-state-type}")

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
;;; ----------------------------------------------------------------------------

(declaim (inline g-action-get-name))

(defun g-action-get-name (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-10}
  @argument[action]{a @class{g-action} object}
  @return{The name of the @arg{action}.}
  @short{Queries the name of @arg{action}.}

  Since 2.28
  @see-class{g-action}"
  (g-action-name action))

(export 'g-action-get-name)

;;; ----------------------------------------------------------------------------
;;; g_action_get_parameter_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-action-get-parameter-type))

(defun g-action-get-parameter-type (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-10}
  @argument[action]{a @class{g-action} object}
  @return{The parameter type.}
  @begin{short}
    Queries the type of the parameter that must be given when activating
    @arg{action}.
  @end{short}

  When activating the @arg{action} using the function @fun{g-action-activate},
  the @type{g-variant} given to that function must be of the type returned by
  this function.

  In the case that this function returns @code{nil}, you must not give any
  @type{g-variant}, but @code{nil} instead.

  Since 2.28
  @see-class{g-action}
  @see-type{g-variant}
  @see-function{g-action-activate}"
  (g-action-parameter-type action))

(export 'g-action-get-parameter-type)

;;; ----------------------------------------------------------------------------
;;; g_action_get_state_type ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-action-get-state-type))

(defun g-action-get-state-type (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-10}
  @argument[action]{a @class{g-action} object}
  @return{The state type, if the @arg{action} is stateful.}
  @begin{short}
    Queries the type of the state of @arg{action}.
  @end{short}

  If the @arg{action} is stateful, e. g. created with the function
  @fun{g-simple-action-new-stateful}, then this function returns the
  @class{g-variant-type} of the state. This is the type of the initial value
  given as the state. All calls to the function @fun{g-action-change-state} must
  give a @type{g-variant} of this type and the function
  @fun{g-action-get-state} will return a @type{g-variant} of the same type.

  If the action is not stateful, e. g. created with the function
  @fun{g-simple-action-new}, then this function will return @code{nil}. In that
  case, the function @fun{g-action-get-state} will return @code{nil} and you
  must not call the function @fun{g-action-change-state}.

  Since 2.28
  @see-class{g-action}
  @see-type{g-variant}
  @see-class{g-variant-type}
  @see-function{g-simple-action-new-stateful}
  @see-function{g-action-change-state}
  @see-function{g-action-get-state}
  @see-function{g-simple-action-new}"
  (g-action-state-type action))

(export 'g-action-get-state-type)

;;; ----------------------------------------------------------------------------
;;; g_action_get_state_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_get_state_hint" g-action-get-state-hint)
    (:pointer (:struct g-variant))
 #+cl-cffi-gtk-documentation
 "@version{2013-8-10}
  @argument[action]{a @class{g-action} object}
  @return{The state range hint.}
  @begin{short}
    Requests a hint about the valid range of values for the state of
    @arg{action}.
  @end{short}

  If @code{nil} is returned it either means that the @arg{action} is not
  stateful or that there is no hint about the valid range of values for the
  state of the @arg{action}.

  If a @type{g-variant} array is returned then each item in the array is a
  possible value for the state. If a @type{g-variant} pair, i. e. two-tuple,
  is returned then the tuple specifies the inclusive lower and upper bound of
  valid values for the state.

  In any case, the information is merely a hint. It may be possible to have a
  state value outside of the hinted range and setting a value within the range
  may fail.

  The return value, if non-@code{nil}), should be freed with the function
  @fun{g-variant-unref} when it is no longer required.

  Since 2.28
  @see-class{g-action}
  @see-type{g-variant}
  @see-function{g-variant-unref}"
  (action (g-object g-action)))

(export 'g-action-get-state-hint)

;;; ----------------------------------------------------------------------------
;;; g_action_get_enabled ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-action-get-enabled))

(defun g-action-get-enabled (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-10}
  @argument[action]{a @class{g-action} object}
  @return{Whether the @arg{action} is enabled.}
  @begin{short}
    Checks if @arg{action} is currently enabled.
  @end{short}

  An action must be enabled in order to be activated or in order to have its
  state changed from outside callers.

  Since 2.28
  @see-class{g-action}"
  (g-action-enabled action))

(export 'g-action-get-enabled)

;;; ----------------------------------------------------------------------------
;;; g_action_get_state ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-action-get-state))

(defun g-action-get-state (action)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-10}
  @argument[action]{a @class{g-action} object}
  @return{The current state of the @arg{action}.}
  @begin{short}
    Queries the current state of @arg{action}.
  @end{short}

  If the @arg{action} is not stateful then @code{nil} will be returned. If the
  @arg{action} is stateful then the type of the return value is the type given
  by the function @fun{g-action-get-state-type}.

  The return value, if non-@code{nil}, should be freed with the function
  @fun{g-variant-unref} when it is no longer required.

  Since 2.28
  @see-class{g-action}
  @see-function{g-action-get-state-type}
  @see-function{g-variant-unref}"
  (g-action-state action))

(export 'g-action-get-state)

;;; ----------------------------------------------------------------------------
;;; g_action_change_state ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_action_change_state" g-action-change-state) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-10}
  @argument[action]{a @class{g-action} object}
  @argument[value]{the new state}
  @begin{short}
    Request for the state of @arg{action} to be changed to @arg{value}.
  @end{short}

  The @arg{action} must be stateful and @arg{value} must be of the correct type.
  See the function @fun{g-action-get-state-type}.

  This call merely requests a change. The action may refuse to change its
  state or may change its state to something other than value. See the function
  @fun{g-action-get-state-hint}.

  If the value @type{g-variant} is floating, it is consumed.

  Since 2.30
  @see-class{g-action}
  @see-type{g-variant}
  @see-function{g-action-get-state-type}
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
 "@version{2013-8-10}
  @argument[action]{a @class{g-action} object}
  @argument[parameter]{the parameter to the activation}
  @short{Activates the @arg{action}.}

  @arg{parameter} must be the correct type of the parameter for the
  @arg{action}, i. e. the parameter type given at construction time. If the
  parameter type was @code{nil} then @arg{parameter} must also be @code{nil}.

  Since 2.28
  @see-class{g-action}"
  (%g-action-activate (pointer action) parameter))

(export 'g-action-activate)

;;; --- End of file gio.action.lisp --------------------------------------------
